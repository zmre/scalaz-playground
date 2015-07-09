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
    val seedData: KVMap = Map(k -> v)

    "get a doc that exists" in {
      val testRead = getDoc(k)
      val res = DBInterpreterMemory(testRead, seedData)
      res.value._1 should === (v)
    }
    "fail fetching a doc that doesn't exist" in {
      val testRead = getDoc(k)
      val res:DbFetchResult = DBInterpreterMemory(testRead)
      res should be (left)
    }
    "create a doc that doesn't exist" in {
      val testCreate = createDoc(k, v)
      val (data, res) = DBInterpreterMemory.run(testCreate)
      res should be (right)
      data should equal (seedData)
    }
    "fail to create a doc if it already exists" in {
      val testCreate = createDoc(k, JsonString("some other value"))
      val (data, res) = DBInterpreterMemory.run(testCreate, seedData)
      res should be (left)
      data should equal (seedData)
    }

  }
}
