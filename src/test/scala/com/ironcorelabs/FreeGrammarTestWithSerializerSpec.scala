//
// com.ironcorelabs.FreeGrammarTestWithSerializerSpec
//
// Copyright (c) 2015 IronCore Labs
//
package com.ironcorelabs

import org.scalatest.{ WordSpec, Matchers }
import org.typelevel.scalatest._
import DisjunctionValues._
import scalaz._, Scalaz._
import scala.language.postfixOps

class FreeGrammarTestWithSerializerSpec extends WordSpec with Matchers with DisjunctionMatchers {
  "FreeGrammarTestWithSerializer accounts" should {
    import com.ironcorelabs.MyFreeGrammar._
    import com.ironcorelabs.FreeGrammarTestWithSerializer._
    val a = Account(1, "user1", "User One")
    val b = Account(1, "user2", "User Two")
    val dbanew = DBAccount(None, a, None)
    val k = dbanew.genKey.toOption
    val dbaexist = DBAccount(k, a, dbanew.dataJson.map(genHashVer(_)).toOption)
    val emptyData: KVMap = Map()

    "persist a new account to the database" in {
      val testCreate = createAccount(dbanew)
      val (data, res) = DBInterpreterMemory.run(testCreate)
      res should be(right)
      data should !==(emptyData)
      data.get(k.get) should ===(dbaexist.dataJson.toOption)
    }
    "fail persisting account to db if key or hash exist" in {
      // create should fail if a key or hashver is passed in
      val testCreate = createAccount(dbaexist)
      val (data, res) = DBInterpreterMemory.run(testCreate)
      res should be(left)
    }
    "fail if adding an account that already exists" in {
      val testCreate = createAccount(dbanew)
      val (data1, res1) = DBInterpreterMemory.run(testCreate)
      val (data2, res2) = DBInterpreterMemory.run(testCreate, data1)
      res2 should be(left)
    }
    "update an account in the db" in {
      // Below will take the DBAccount wrapping account `a` and update it to `b`
      // so that user2 is in place.
      val testUpdate: DBProg[Unit] = for {
        _ <- createAccount(dbanew)
        upd <- updateAccount(dbaexist.copy(data = b))
      } yield upd
      val (data, res) = DBInterpreterMemory.run(testUpdate)
      res should be(right)
      DBAccount.fromJson(data.get(k.get).get) should ===(b.right)
    }
    "fail update if value not in db" in {
      val testUpdate = updateAccount(dbaexist)
      val (data, res) = DBInterpreterMemory.run(testUpdate)
      res should be(left)
    }
    "fail update if missing hashver" in {
      val testUpdate: DBProg[Unit] = updateAccount(dbaexist.copy(hashver = None))
      val (data, res) = DBInterpreterMemory.run(testUpdate)
      res should be(left)
    }
    "fail update if missing key" in {
      val testUpdate = updateAccount(dbaexist.copy(key = None))
      val (data, res) = DBInterpreterMemory.run(testUpdate)
      res should be(left)
    }
    "get an account that exists" in {
      val testUpdate: DBProg[DBAccount] = for {
        _ <- createAccount(dbanew)
        account <- getAccount(k.get)
      } yield account
      // DBInterpreterMemory.print(testUpdate)
      val (data, res) = DBInterpreterMemory.run(testUpdate)
      res should equal(dbaexist.right)
    }
    "fail to get an account that doesn't exist" in {
      val testUpdate: DBProg[DBAccount] = getAccount(k.get)
      val (data, res) = DBInterpreterMemory.run(testUpdate)
      res should be(left)
    }
    "modify an account that exists" in {
      val testUpdate: DBProg[DBAccount] = for {
        _ <- createAccount(dbanew)
        upd <- modifyAccount(k.get, (account) => b)
      } yield upd
      val (data, res) = DBInterpreterMemory.run(testUpdate)
      res should be(right)
      DBAccount.fromJson(data.get(k.get).get) should ===(b.right)
    }
    "fail to modify an account that does not exists" in {
      val testUpdate: DBProg[DBAccount] = modifyAccount(k.get, (account) => b)
      val (data, res) = DBInterpreterMemory.run(testUpdate)
      res should be(left)
      data should equal(emptyData)
    }
  }
}
