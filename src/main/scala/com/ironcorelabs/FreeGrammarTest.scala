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
  type DBProg[A] = EitherT[DBFree, Throwable, A]

  def genHashVer(s: JsonString): HashVerString =
    HashVerString(scala.util.hashing.MurmurHash3.stringHash(s.s).toString)

  // 1. ADT
  sealed trait DBOps[+Next]
  case class GetDoc[Next](key: Key, nextF: Throwable \/ DbValue => Next) extends DBOps[Next]
  case class CreateDoc[Next, B](key: Key, doc: JsonString, nextF: Throwable \/ Unit => Next) extends DBOps[Next]
  case class UpdateDoc[Next, B](key: Key, doc: JsonString, hashver: HashVerString, nextF: Throwable \/ Unit => Next) extends DBOps[Next]
  case class RemoveKey[Next](key: Key, nextF: Throwable \/ Unit => Next) extends DBOps[Next]
  // case class GetCounter[A](key: String, next: A) extends DBOps[A]
  // case class IncrementCounter[A](key: String, delta: Long, next: A) extends DBOps[A]

  // 2. Functor for ADT
  implicit val dbOps2Functor: scalaz.Functor[DBOps] = new scalaz.Functor[DBOps] {
    def map[A,B](a: DBOps[A])(f: A => B): DBOps[B] = a match {
      // case GetDoc(k, nextF) => GetDoc(k, nextF andThen f)
      case GetDoc(k, nextF) => GetDoc(k, nextF andThen f)
      case CreateDoc(k, doc, nextF) => CreateDoc(k, doc, nextF andThen f)
      case UpdateDoc(k, doc, hashver, nextF) => UpdateDoc(k, doc, hashver, nextF andThen f)
      case RemoveKey(k, nextF) => RemoveKey(k, nextF andThen f)
    }
  }

  // 3. Lifting functions / free monad magic
  def liftToFreeEitherT[A](a: DBOps[Throwable \/ A]): DBProg[A] = {
    val free: DBFree[Throwable \/ A] = Free.liftF(a)
    EitherT.eitherT(free)
  }

  def getDoc(key: Key): DBProg[DbValue] = liftToFreeEitherT(GetDoc(key, d => d))
  def createDoc(k: Key, doc: JsonString): DBProg[Unit] = liftToFreeEitherT(CreateDoc(k, doc, d => d))
  def updateDoc(k: Key, doc: JsonString, hashver: HashVerString): DBProg[Unit]  =
    liftToFreeEitherT(UpdateDoc(k, doc, hashver, d => d))
  def removeKey(k: Key): DBProg[Unit]  = liftToFreeEitherT(RemoveKey(k, d => d))
  def createAndRead(k: Key, v: JsonString): DBProg[DbValue] = for {
    _ <- createDoc(k, v)
    d <- getDoc(k)
  } yield d

  def modifyDoc(k: Key, f: JsonString => JsonString): DBProg[Unit] = for {
    t <- getDoc(k)
    (jsonString, hashVersion) = t
    res <- updateDoc(k, f(jsonString), hashVersion)
  } yield res

  type KVMap = Map[Key, JsonString]
  object DBInterpreterMemory {
    def apply[A](prog: DBProg[A], m: KVMap = Map()): Throwable \/ A = run(prog, m)._2
    def run[A](prog: DBProg[A], m: KVMap = Map()): (KVMap, Throwable \/ A) = prog.run.foldRun(m) { (m, op) => op match {
        case GetDoc(k, nextF) => m.get(k).fold {
            (m, nextF(error(s"No value found for key '${k.k}'")))
          } { j: JsonString =>
            (m, nextF((j, genHashVer(j)).right))
          }
        case UpdateDoc(k, doc, hashver, nextF) => m.get(k).fold {
            (m, nextF(error(s"No value found for key '${k.k}'")))
          } { j: JsonString =>
            val storedhashver = genHashVer(j)
            if (hashver == storedhashver) {
              ((m + (k -> doc)), nextF(().right))
            } else {
              (m, nextF(error("Someone else updated this doc first")))
            }
          }
        case CreateDoc(k, doc, nextF) => m.get(k).fold {
            ((m + (k -> doc), nextF(().right)))
          } { a:JsonString =>
            (m, nextF(error(s"Can't create since '${k.k}' already exists")))
          }
        case RemoveKey(k, nextF) => m.get(k).fold {
            (m, nextF(error("Can't remove non-existent document")))
          } { a: JsonString =>
            (m - k, nextF(().right))
          }
      }
    }
    private def error(s: String) = (new Exception(s)).left
  }
}
