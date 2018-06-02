/*
 * Copyright 2011-2017 Chris de Vreeze
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package eu.cdevreeze.xpathparser.clientapp

import scala.reflect.classTag

import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.annotation.JSExportTopLevel

import org.scalajs.dom.document
import org.scalajs.dom.Node
import org.scalajs.dom.raw.HTMLLIElement
import org.scalajs.dom.raw.HTMLTextAreaElement
import org.scalajs.dom.raw.HTMLUListElement

import eu.cdevreeze.xpathparser.ast.FunctionCall
import eu.cdevreeze.xpathparser.ast.XPathExpr
import eu.cdevreeze.xpathparser.parse.XPathParser
import eu.cdevreeze.xpathparser.util.VariableBindingUtil
import fastparse.core.Parsed

/**
 * Program that checks the syntax of an XPath expression, showing if it can be successfully parsed.
 *
 * It is tightly integrated with the corresponding HTML page.
 *
 * @author Chris de Vreeze
 */
@JSExportTopLevel("XPathSyntaxChecker")
object XPathSyntaxChecker {

  private lazy val xpathTextArea = getXPathTextArea()

  private val liCls = "list-group-item"

  @JSExport("checkSyntax")
  def checkSyntax(xpathString: String): Unit = {
    val parseResult: Parsed[XPathExpr, Char, String] = XPathParser.xpathExpr.parse(xpathString.trim)

    parseResult.fold(
      (parser, pos, extra) => showFailure(parseResult.asInstanceOf[Parsed.Failure[Char, String]]),
      (expr, pos) => showSuccess(parseResult.asInstanceOf[Parsed.Success[XPathExpr, Char, String]]))
  }

  @JSExport("clear")
  def clear(): Unit = {
    xpathTextArea.style.color = "black"
    xpathTextArea.value = ""

    val freeVariablesUList = getFreeVariablesUList()
    val boundVariablesUList = getBoundVariablesUList()
    val calledFunctionsUList = getCalledFunctionsUList()

    freeVariablesUList.innerHTML = ""
    boundVariablesUList.innerHTML = ""
    calledFunctionsUList.innerHTML = ""
  }

  @JSExport("clearColor")
  def clearColor(): Unit = {
    xpathTextArea.style.color = "black"
  }

  private def showSuccess(parseResult: Parsed.Success[XPathExpr, Char, String]): Unit = {
    xpathTextArea.style.color = "green"

    val freeVariablesUList = getFreeVariablesUList()
    val boundVariablesUList = getBoundVariablesUList()
    val calledFunctionsUList = getCalledFunctionsUList()

    freeVariablesUList.innerHTML = ""
    boundVariablesUList.innerHTML = ""
    calledFunctionsUList.innerHTML = ""

    val freeVariables = VariableBindingUtil.findAllFreeVariables(parseResult.value)
    val boundVariables = VariableBindingUtil.findAllBoundVariables(parseResult.value)
    val calledFunctions = parseResult.value.findAllElemsOrSelfOfType(classTag[FunctionCall])

    freeVariables.map(_.varName).distinct.foreach { freeVar =>
      addNewReadonlyListItem(freeVariablesUList, freeVar.toString, liCls)
    }

    boundVariables.map(_.varName).distinct.foreach { boundVar =>
      addNewReadonlyListItem(boundVariablesUList, boundVar.toString, liCls)
    }

    calledFunctions.map(_.functionName).distinct.foreach { funcCall =>
      addNewReadonlyListItem(calledFunctionsUList, funcCall.toString, liCls)
    }
  }

  private def showFailure(parseResult: Parsed.Failure[Char, String]): Unit = {
    xpathTextArea.style.color = "red"

    val freeVariablesUList = getFreeVariablesUList()
    val boundVariablesUList = getBoundVariablesUList()
    val calledFunctionsUList = getCalledFunctionsUList()

    freeVariablesUList.innerHTML = ""
    boundVariablesUList.innerHTML = ""
    calledFunctionsUList.innerHTML = ""
  }

  private def getXPathTextArea(): HTMLTextAreaElement = {
    document.getElementById("xpathInput").ensuring(_ != null).asInstanceOf[HTMLTextAreaElement]
  }

  private def getFreeVariablesUList(): HTMLUListElement = {
    document.getElementById("freeVariables").ensuring(_ != null).asInstanceOf[HTMLUListElement]
  }

  private def getBoundVariablesUList(): HTMLUListElement = {
    document.getElementById("boundVariables").ensuring(_ != null).asInstanceOf[HTMLUListElement]
  }

  private def getCalledFunctionsUList(): HTMLUListElement = {
    document.getElementById("functions").ensuring(_ != null).asInstanceOf[HTMLUListElement]
  }

  private def addNewReadonlyListItem(targetNode: Node, text: String, cssClass: String): Unit = {
    val liElem = document.createElement("li").asInstanceOf[HTMLLIElement]
    liElem.className = cssClass
    val textNode = document.createTextNode(text)
    liElem.appendChild(textNode)
    targetNode.appendChild(liElem)
  }
}
