/*
 * This file is part of COMP3000 assignment 1.
 *
 * Copyright (C) 2021 Kym Haines, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

// https://www.scala-lang.org/api/2.12.8/scala/util/matching/Regex.html
// https://www.scala-lang.org/api/2.12.8/scala/collection/immutable/List.html
// https://www.scala-lang.org/api/2.12.8/scala/collection/immutable/StringLike.html
// https://www.scala-lang.org/api/2.12.8/scala/collection/immutable/Map.html

package org.mq.interpret

import scala.util.matching._

object Interpret {

  sealed abstract class LObject
  case class LSymbol(sym:String) extends LObject
  case class LNumber(num:Int) extends LObject
  case class LList(head:LObject, tail:LObject) extends LObject

  val nil = LSymbol("nil")
  val T = LSymbol("t")
  val error = LSymbol("ERROR")

  def resetEnv:Unit =
  {
    // TO DO: reset your functions/variables environment
  }

  val pat = "[a-z]*-?[0-9]+|[+/-]*".r //regex for all

  val patnum = "-?[0-9]+".r //find all numbers including negative integers

  val patname = "[a-zA-Z][a-zA-Z0-9]*".r //find name

  def matchPat(line:String):List[String] =
                              pat.findAllIn(line.toLowerCase).toList

  def strToLObj(s:String):LObject = 
  {
      s match {
        case patnum() => LNumber(s.toInt)
        case patname() => LSymbol(s)
        case pat() => LSymbol(s)
        
        }
    }

  def tokensToLObjs(a:List[String]):Option[LObject] = 
  {
    // TO DO: convert a list of token strings to an LObject
    // NOTE: anywhere () is seen, it should be interpreted as nil
    None
  }

  // for testing
  def lineToLObj(line:String):LObject = tokensToLObjs(matchPat(line)) match
  {
  case Some(s) => s
  case None    => error
  }

  def setValue(varName:String, value:LObject):Unit =
  {
    // TO DO: assign a value to a variable
    None
  }

  def getValue(varName:String):LObject =
  {
    // TO DO: get the value of a variable; or error if variable not defined
    error
  }

  def add(a:LObject, b:LObject):LObject = {
    error
    

  }
    

  def sub(a:LObject, b:LObject):LObject = error    // TO DO

  def mul(a:LObject, b:LObject):LObject = error    // TO DO

  def div(a:LObject, b:LObject):LObject = error    // TO DO

  def car(a:LObject):LObject = 
  {
    a match {
      case LList(head, _) => head
      case _ => error
    }
  }

  def cdr(a:LObject):LObject = 
  {
   a match {
     case LList(_, tail) => tail
     case _ => error
   } 
  }

  def cons(a:LObject, b:LObject):LObject = error   // TO DO

  def eeqq(a:LObject, b:LObject):LObject = error   // TO DO

  def setq(v:String, b:LObject):LObject = error    // TO DO

  def iiff(cond:LObject, ifThen:LObject, ifElse:LObject):LObject = error //TO DO

  def defun(name:String, arg:String, body:LObject):LObject =
  {
    // TO DO: define a function
    // the function definition source would look like:
    //      (def name (arg) body)
    LSymbol(name)
  }

  def funCall(name:String, arg:LObject):LObject = error    // TO DO

  def eval(a:LObject):LObject = a match
  {
  case LSymbol("nil") => nil
  case LSymbol("t") => T
  // TO DO: add cases for all the other possibilities in the eval table
  //        in the spec
  case _          => error
  }

  def showLine(s:LObject):Unit = { show(s);  println() }

  def show(s:LObject):Unit = s match
  {
  case LList(h, t)  => print("(")
                       show(h)
                       showList(t)
                       print(")")
  case LSymbol(a)   => print(a)
  case LNumber(a)   => print(a)
  }

  def showList(s:LObject):Unit = s match
  {
  case LSymbol("nil") =>
  case a:LList => print(" ")
                  show(a.head)
                  a.tail match
                  {
                  case b:LList => showList(b)
                  case _ =>
                  }
  case _ => print(" . ")
            show(s)
  }

}
