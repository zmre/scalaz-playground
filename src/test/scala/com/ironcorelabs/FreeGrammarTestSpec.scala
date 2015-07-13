//
// com.ironcorelabs.FreeGrammarTestSpec
//
// Copyright (c) 2015 IronCore Labs
//
package com.ironcorelabs

import org.scalatest.{ WordSpec, Matchers }
import org.typelevel.scalatest._
import DisjunctionValues._
import scala.language.postfixOps
import scalaz._
import Scalaz._

class FreeGrammarTestSpec extends WordSpec with Matchers with DisjunctionMatchers {
  "FreeGrammarTest" should {
    import com.ironcorelabs.MyFreeGrammar._
    val k = Key("test")
    val v = JsonString("value")
    val newvalue = JsonString("some other value")
    val hv = genHashVer(v)
    val seedData: KVMap = Map(k -> v)
    val emptyData: KVMap = Map()

    "get a doc that exists" in {
      val testRead = getDoc(k)
      val res = DBInterpreterMemory(testRead, seedData)
      res.value._1 should ===(v)
    }
    "fail fetching a doc that doesn't exist" in {
      val testRead = getDoc(k)
      val res = DBInterpreterMemory(testRead)
      res should be(left)
    }
    "create a doc that doesn't exist" in {
      val testCreate = createDoc(k, v)
      val (data, res) = DBInterpreterMemory.run(testCreate)
      res should be(right)
      data should equal(seedData)
    }
    "fail to create a doc if it already exists" in {
      val testCreate = createDoc(k, newvalue)
      val (data, res) = DBInterpreterMemory.run(testCreate, seedData)
      res should be(left)
      data should equal(seedData)
    }
    "update a doc that exists with correct hashver" in {
      val testUpdate = updateDoc(k, newvalue, hv)
      val (data, res) = DBInterpreterMemory.run(testUpdate, seedData)
      res should be(right)
      data.get(k) should ===(Some(newvalue))
    }
    "fail updating a doc that doesn't exist" in {
      val testUpdate = updateDoc(k, newvalue, hv)
      val (data, res) = DBInterpreterMemory.run(testUpdate)
      res should be(left)
      data should ===(emptyData)
    }
    "fail updating a doc when using incorrect hashver" in {
      val testUpdate = updateDoc(k, newvalue, HashVerString("badver"))
      val (data, res) = DBInterpreterMemory.run(testUpdate, seedData)
      res should be(left)
      data should ===(seedData)
    }
    "remove a key that exists" in {
      val testRemove = removeKey(k)
      val (data, res) = DBInterpreterMemory.run(testRemove, seedData)
      res should be(right)
      data should ===(emptyData)
    }
    "fail removing a key that doesn't exist" in {
      val testRemove = removeKey(k)
      val (data, res) = DBInterpreterMemory.run(testRemove)
      res should be(left)
      data should ===(emptyData)
    }
    "create and read" in {
      val testCreateAndRead = createAndRead(k, v)
      val (data, res) = DBInterpreterMemory.run(testCreateAndRead)
      res.value._1 should ===(v)
      data should equal(seedData)
    }
    "create and read something that already exists should fail" in {
      val testCreateAndRead = createAndRead(k, v)
      val (data, res) = DBInterpreterMemory.run(testCreateAndRead, seedData)
      res should be(left)
    }
    "modify map" in {
      val testModify = modifyDoc(k, j => newvalue)
      val (data, res) = DBInterpreterMemory.run(testModify, seedData)
      res should be(right)
      data.get(k) should ===(Some(newvalue))
    }
    "modify map fails if key is not in db" in {
      val testModify = modifyDoc(k, j => newvalue)
      val (data, res) = DBInterpreterMemory.run(testModify)
      res should be(left)
    }
  }
}
