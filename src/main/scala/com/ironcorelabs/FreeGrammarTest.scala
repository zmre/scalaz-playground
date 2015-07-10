//
// com.ironcorelabs.FreeGrammarTest
//
// Copyright (c) 2015 IronCore Labs
//
package com.ironcorelabs

import scalaz._
import Scalaz._
import scala.util.Try
import scala.language.implicitConversions

object MyFreeGrammar {

  final case class Key(k: String) extends AnyVal
  final case class JsonString(s: String) extends AnyVal
  final case class HashVerString(s: String) extends AnyVal
  type DbValue = (JsonString, HashVerString)
  type DBFree[A] = Free[DBOps, A]
  type EDBProg[A] = EitherT[DBFree, Throwable, A]

  def genHashVer(s: JsonString): HashVerString =
    HashVerString(scala.util.hashing.MurmurHash3.stringHash(s.s).toString)

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

    def fromJson(js: JsonString): Throwable \/ Account = try {
      import scala.util.parsing.json._
      val json: Option[Any] = JSON.parseFull(js.s)
      json match {
        case Some(map: Map[String, Any]) => (for {
          id <- map.get("id").map(_.asInstanceOf[Int])
          user <- map.get("userName").map(_.toString)
          name <- map.get("fullName").map(_.toString)
        } yield Account(id, user, name).right) getOrElse (new Exception("Json parse failure")).left

        case _ => (new Exception("Json parse failure")).left
      }
    } catch {
      case e: Exception => e.left
    }

    def modify(f: Account => Account): DBAccount = DBAccount(key, f(record), hashver)
  }



  // 1. ADT
  sealed trait DBOps[+Next]
  case class GetDoc[Next](key: Key, nextF: Throwable \/ DbValue => Next) extends DBOps[Next]
  case class CreateDoc[Next, B](key: Key, doc: JsonString, nextF: Throwable \/ Unit => Next) extends DBOps[Next]
  case class UpdateDoc[Next, B](key: Key, doc: JsonString, hashver: HashVerString, next: Next) extends DBOps[Next]
  case class RemoveKey[Next](key: Key, next: Next) extends DBOps[Next]
  // case class GetCounter[A](key: String, next: A) extends DBOps[A]
  // case class IncrementCounter[A](key: String, delta: Long, next: A) extends DBOps[A]

  // 2. Functor for ADT
  // implicit object DBOpsFunctor extends Functor[DBOps] {
  implicit val dbOps2Functor: scalaz.Functor[DBOps] = new scalaz.Functor[DBOps] {
    def map[A,B](a: DBOps[A])(f: A => B): DBOps[B] = a match {
      // case GetDoc(k, nextF) => GetDoc(k, nextF andThen f)
      case GetDoc(k, nextF) => GetDoc(k, nextF andThen f)
      case CreateDoc(k, doc, nextF) => CreateDoc(k, doc, nextF andThen f)
      case UpdateDoc(k, doc, hashver, next) => UpdateDoc(k, doc, hashver, f(next))
      case RemoveKey(k, next) => RemoveKey(k, f(next))
    }
  }

  // 3. Lifting functions / free monad magic
  def liftToFreeEitherT[A](a: DBOps[Throwable \/ A]): EDBProg[A] = {
    val free: DBFree[Throwable \/ A] = Free.liftF(a)
    EitherT.eitherT(free)
  }
  def liftToEitherT[A](a: DBFree[Throwable \/ A]): EDBProg[A] = EitherT.eitherT(a)
  // def getAccount(key: String): DBProg[Throwable \/ DBAccount] = Free.liftF(
    // GetDoc(key, (doc: Throwable \/ DBAccount) => doc)
    // GetDoc(key, identity)
  // )
  // def createAccount(doc: DBAccount): DBProg[Throwable \/ DBAccount] = Free.liftF(CreateDoc(doc, ().right))
  // def getDoc[A](key: Key): DBProg[DbFetchResult] = Free.liftF(GetDoc(key, (d:DbFetchResult) => d.map(())))
  def getDoc(key: Key): EDBProg[DbValue] = liftToFreeEitherT(GetDoc(key, d => d))
  def createDoc(k: Key, doc: JsonString): EDBProg[Unit] = liftToFreeEitherT(CreateDoc(k, doc, d => d))
  def updateDoc(k: Key, doc: JsonString, hashver: HashVerString): EDBProg[Unit]  =
    liftToFreeEitherT(UpdateDoc(k, doc, hashver, ().right))
  def removeKey(k: Key): EDBProg[Unit]  = liftToFreeEitherT(RemoveKey(k, ().right))
  def createAndRead(k: Key, v: JsonString): EDBProg[DbValue] = for {
    _ <- createDoc(k, v)
    d <- getDoc(k)
  } yield d

  def modifyDoc(k: Key, f: JsonString => JsonString): EDBProg[Unit] = for {
    t <- getDoc(k)
    (jsonString, hashVersion) = t
    res <- updateDoc(k, f(jsonString), hashVersion)
  } yield res

  type KVMap = Map[Key, JsonString]
  object DBInterpreterMemory {
    def apply[A](prog: EDBProg[A], m: KVMap = Map()): Throwable \/ A = run(prog, m)._2
    def run[A](prog: EDBProg[A], m: KVMap = Map()): (KVMap, Throwable \/ A) = prog.run.foldRun(m) { (m, op) => op match {
        case GetDoc(k, nextF) => {
          val jsonO:Option[JsonString] = m.get(k)
          val ret = jsonO match {
            case Some(j) => ((j, genHashVer(j))).right
            case None => (new Exception(s"No value found for key '${k.k}'")).left
          }
          (m, nextF(ret))
        }
        case UpdateDoc(k, doc, hashver, cont) => {
          // TODO: Should fetch value and compare hashver - fail if data not in map
          ((m + (k -> doc)), cont)
        }
        case CreateDoc(k, doc, nextF) => m.get(k) match {
          case None => ((m + (k -> doc), nextF(().right)))
          case Some(_) => (m, nextF((new Exception(s"Can't create since '${k.k}' already exists")).left))
        }
        case RemoveKey(k, cont) => (m - k, cont)
      }
    }
  }
}
