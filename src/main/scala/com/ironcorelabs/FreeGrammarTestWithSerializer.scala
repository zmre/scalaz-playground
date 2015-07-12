//
// com.ironcorelabs.FreeGrammarTestWithSerializer
//
// Copyright (c) 2015 IronCore Labs
//
package com.ironcorelabs

import scalaz._
import Scalaz._
import scala.util.Try
import scala.language.implicitConversions

object FreeGrammarTestWithSerializer {
  import MyFreeGrammar._

  abstract trait DBDocument[A] {
    val record: A
    val hashver: Option[HashVerString]
    val key: Option[Key]
    def genKey: Throwable \/ Key
    def toJson: Throwable \/ JsonString
    def fromJson(s: JsonString): Throwable \/ A
    def modify(f: A => A): DBDocument[A]
  }

  // For simplicity in this example, we'll assume even new Accounts already have an id
  case class Account(id: Int, userName: String, fullName: String)

  case class DBAccount(key: Option[Key], record: Account, hashver: Option[HashVerString]) extends DBDocument[Account] {
    def genKey: Throwable \/ Key = Key("account_" + record.id).right
    def toJson: Throwable \/ JsonString = JsonString(s"""
        {
          "id": "${genKey}",
          "userName": "${record.userName}",
          "fullName": "${record.fullName}"
        }
      """).right

    def fromJson(js: JsonString) = DBAccount.fromJson(js)
    def modify(f: Account => Account): DBAccount = DBAccount(key, f(record), hashver)
  }
  object DBAccount {
    def fromJson(js: JsonString): Throwable \/ Account = try {
      import scala.util.parsing.json._
      val json: Option[Any] = JSON.parseFull(js.s)
      json match {
        case Some(m) => {
          val map = m.asInstanceOf[Map[String, Any]]
          (for {
            id <- map.get("id").map(_.asInstanceOf[Int])
            user <- map.get("userName").map(_.toString)
            name <- map.get("fullName").map(_.toString)
          } yield Account(id, user, name).right
          ) getOrElse (new Exception("Json parse failure")).left
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

  def getAccount(k: Key): DBProg[DBAccount] = for {
    t <- getDoc(k)
    (jsonString, hashVersion) = t
    account <- liftIntoDBFree(DBAccount.fromJson(jsonString))
  } yield DBAccount(Some(k), account, Some(hashVersion))
}
