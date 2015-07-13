//
// com.ironcorelabs.FreeGrammarTestWithSerializer
//
// Copyright (c) 2015 IronCore Labs
//
package com.ironcorelabs

import scalaz._, Scalaz._
import scala.util.Try
import scala.language.implicitConversions
import MyFreeGrammar._

object FreeGrammarTestWithSerializer {

  abstract trait DBDocument[A] {
    val data: A
    val hashver: Option[HashVerString]
    val key: Option[Key]
    def genKey: Throwable \/ Key
    def dataJson: Throwable \/ JsonString
    def fromJson(s: JsonString): Throwable \/ A
    def modify(f: A => A): DBDocument[A]
  }

  // For simplicity in this example, we'll assume even new Accounts already have an id
  case class Account(id: Int, userName: String, fullName: String)

  case class DBAccount(key: Option[Key], data: Account, hashver: Option[HashVerString]) extends DBDocument[Account] {
    def genKey: Throwable \/ Key = Key("account_" + data.id).right
    def dataJson: Throwable \/ JsonString = JsonString(s"""
        {
          "id": "${data.id}",
          "userName": "${data.userName}",
          "fullName": "${data.fullName}"
        }
      """).right

    def fromJson(js: JsonString) = DBAccount.fromJson(js)
    def modify(f: Account => Account): DBAccount = DBAccount(key, f(data), hashver)
  }
  object DBAccount {
    def fromJson(js: JsonString): Throwable \/ Account = try {
      import scala.util.parsing.json._
      val json: Option[Any] = JSON.parseFull(js.s)
      json match {
        case Some(m) => {
          val map = m.asInstanceOf[Map[String, Any]]
          (for {
            id <- map.get("id").map((is: Any) => Integer.parseInt(is.toString))
            user <- map.get("userName").map(_.toString)
            name <- map.get("fullName").map(_.toString)
          } yield Account(id, user, name).right) getOrElse (new Exception("Json parse failure")).left
        }
        case _ => (new Exception("Json parse failure")).left
      }
    } catch {
      case e: Exception => e.left
    }
  }

  /*
   * Type definitions for convenience:
   *
   * type DBFree[A] = Free[DBOps, A]
   * type DBProg[A] = EitherT[DBFree, Throwable, A]
   *
   * getDoc(k) returns DBProg[DbValue]
   */
  def createAccount(account: DBAccount): DBProg[Unit] = {
    if (account.key.isEmpty && account.hashver.isEmpty) {
      val doc: Throwable \/ DBProg[Unit] = for {
        json <- account.dataJson
        key <- account.genKey
      } yield createDoc(key, json)
      doc.fold((e: Throwable) => dbProgFail(e), (dbp: DBProg[Unit]) => dbp)
    } else {
      dbProgFail(new Exception("Creating account with existing hashver or key"))
    }
  }

  def updateAccount(account: DBAccount): DBProg[Unit] = {
    val doc = for {
      json <- account.dataJson
      key <- account.genKey
      hashver <- account.hashver \/> (new Exception("No hashver"))
    } yield updateDoc(key, json, hashver)
    doc.fold((e: Throwable) => dbProgFail(e), (dbp: DBProg[Unit]) => dbp)
  }

  def getAccount(k: Key): DBProg[DBAccount] = for {
    t <- getDoc(k)
    (jsonString, hashVersion) = t
    account <- liftIntoDBProg(DBAccount.fromJson(jsonString))
  } yield DBAccount(Some(k), account, Some(hashVersion))

  def modifyAccount(k: Key, f: Account => Account): DBProg[DBAccount] = for {
    t <- getDoc(k)
    (jsonString, hashVersion) = t
    account <- liftIntoDBProg(DBAccount.fromJson(jsonString))
    newjson <- liftIntoDBProg(DBAccount(Some(k), f(account), Some(hashVersion)).dataJson)
    res <- updateDoc(k, newjson, hashVersion)
    t2 <- getDoc(k)
    (jsonString2, hashVersion2) = t2
    account2 <- liftIntoDBProg(DBAccount.fromJson(jsonString2))
  } yield DBAccount(Some(k), account2, Some(hashVersion2))

  def getAccount2(k: Key): DBProg[DBAccount] = getDoc(k).flatMapF {
    case (jsonString, hashVersion) =>
      Monad[DBFree].point(
        DBAccount.fromJson(jsonString).map(account =>
          DBAccount(Some(k), account, Some(hashVersion)))
      )
  }

}
