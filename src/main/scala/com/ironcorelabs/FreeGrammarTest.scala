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
import scala.language.higherKinds

object MyFreeGrammar {

  final case class Key(k: String) extends AnyVal
  final case class JsonString(s: String) extends AnyVal
  final case class HashVerString(s: String) extends AnyVal
  type DbValue = (JsonString, HashVerString)
  type DBFree[A] = Free.FreeC[DBOps, A]
  type DBProg[A] = EitherT[DBFree, Throwable, A]
  implicit val MonadDBFree: Monad[DBFree] = Free.freeMonad[({ type l[a] = Coyoneda[DBOps, a] })#l]
  def genHashVer(s: JsonString): HashVerString =
    HashVerString(scala.util.hashing.MurmurHash3.stringHash(s.s).toString)

  // 1. ADT
  sealed trait DBOps[+Next]
  case class GetDoc[Next](key: Key) extends DBOps[Throwable \/ DbValue]
  case class CreateDoc[Next, B](key: Key, doc: JsonString) extends DBOps[Throwable \/ Unit]
  case class UpdateDoc[Next, B](key: Key, doc: JsonString, hashver: HashVerString) extends DBOps[Throwable \/ Unit]
  case class RemoveKey[Next](key: Key) extends DBOps[Throwable \/ Unit]
  // case class GetCounter[A](key: String, next: A) extends DBOps[A]
  // case class IncrementCounter[A](key: String, delta: Long, next: A) extends DBOps[A]

  // 3. Lifting functions / free monad magic
  def liftIntoDBProg[A](either: Throwable \/ A): DBProg[A] = EitherT.eitherT(Monad[DBFree].point(either))
  def liftToFreeEitherT[A](a: DBOps[Throwable \/ A]): DBProg[A] = {
    val free: DBFree[Throwable \/ A] = Free.liftFC(a)
    EitherT.eitherT(free)
  }
  def dbProgFail[A](e: Throwable): DBProg[A] = liftIntoDBProg(e.left)

  def getDoc(key: Key): DBProg[DbValue] = liftToFreeEitherT(GetDoc(key))
  def createDoc(k: Key, doc: JsonString): DBProg[Unit] = liftToFreeEitherT(CreateDoc(k, doc))
  def updateDoc(k: Key, doc: JsonString, hashver: HashVerString): DBProg[Unit] = liftToFreeEitherT(UpdateDoc(k, doc, hashver))
  def removeKey(k: Key): DBProg[Unit] = liftToFreeEitherT(RemoveKey(k))
  def createAndRead(k: Key, v: JsonString): DBProg[DbValue] = for {
    _ <- createDoc(k, v)
    d <- getDoc(k)
  } yield d

  def modifyDoc(k: Key, f: JsonString => JsonString): DBProg[Unit] = for {
    t <- getDoc(k)
    (jsonString, hashVersion) = t
    res <- updateDoc(k, f(jsonString), hashVersion)
  } yield res

  // 4. Interpret the monads and render the result
  type KVMap = Map[Key, JsonString]
  object DBInterpreterMemory {
    type KVState[A] = State[KVMap, A]

    def modifyState(s: KVMap): (KVMap, Throwable \/ Unit) = s -> ().right
    val toKVState: DBOps ~> KVState = new (DBOps ~> KVState) {
      def apply[A](op: DBOps[A]): KVState[A] = {
        op match {
          case GetDoc(k) => State.get.map { _.get(k).map(j => (j -> genHashVer(j)).right).getOrElse(error(s"No value found for key '${k.k}'")) }
          case UpdateDoc(k, doc, hashver) => State { m: KVMap =>
            m.get(k).map { j =>
              val storedhashver = genHashVer(j)
              if (hashver == storedhashver) {
                modifyState(m + (k -> doc))
              } else {
                m -> error("Someone else updated this doc first")
              }
            }.getOrElse(m -> error(s"No value found for key '${k.k}'"))
          }
          case CreateDoc(k, doc) => State { m: KVMap =>
            m.get(k).map(_ => m -> error(s"Can't create since '${k.k}' already exists")).getOrElse(modifyState(m + (k -> doc)))
          }
          case RemoveKey(k) => State { m: KVMap =>
            val keyOrError = m.get(k).map(_ => k.right).getOrElse(error("Can't remove non-existent document"))
            keyOrError.fold(t => m -> t.left, key => modifyState(m - k))
          }
        }
      }
    }

    def apply[A](prog: DBProg[A], m: KVMap = Map()): Throwable \/ A = run(prog, m)._2
    def run[A](prog: DBProg[A], m: KVMap = Map()): (KVMap, Throwable \/ A) = {
      Free.runFC[DBOps, KVState, Throwable \/ A](prog.run)(toKVState).apply(m)
    }
    private def error[A](s: String): Throwable \/ A = (new Exception(s)).left
  }
}
