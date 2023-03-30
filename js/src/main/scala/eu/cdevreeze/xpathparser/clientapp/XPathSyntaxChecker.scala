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
import org.scalajs.dom.HTMLPreElement
import org.scalajs.dom.HTMLLIElement
import org.scalajs.dom.HTMLTextAreaElement
import org.scalajs.dom.HTMLUListElement

import eu.cdevreeze.xpathparser.ast.FunctionCall
import eu.cdevreeze.xpathparser.ast.XPathExpr
import eu.cdevreeze.xpathparser.parse.XPathParser
import eu.cdevreeze.xpathparser.util.VariableBindingUtil
import cats.parse.{Parser => P}

/**
 * Program that checks the syntax of an XPath expression, showing if it can be successfully parsed.
 *
 * It is tightly integrated with the corresponding HTML page.
 *
 * @author
 *   Chris de Vreeze
 */
@JSExportTopLevel("XPathSyntaxChecker")
object XPathSyntaxChecker:

  private lazy val xpathTextArea = getXPathTextArea()

  private val liCls = "list-group-item"

  @JSExport("checkSyntax")
  def checkSyntax(xpathString: String): Unit =
    val parseResult: Either[P.Error, XPathExpr] = XPathParser.xpathExpr.parseAll(xpathString)

    parseResult.fold(
      { case parseError @ P.Error(offset, expectations) => showFailure(parseError) },
      { xpathExpr =>
        showSuccess(xpathExpr)
      }
    )

  @JSExport("clear")
  def clear(): Unit =
    xpathTextArea.style.color = "black"
    xpathTextArea.value = ""

    val freeVariablesUList = getFreeVariablesUList()
    val boundVariablesUList = getBoundVariablesUList()
    val calledFunctionsUList = getCalledFunctionsUList()

    freeVariablesUList.innerHTML = ""
    boundVariablesUList.innerHTML = ""
    calledFunctionsUList.innerHTML = ""

    getAstPreElement().firstElementChild.innerHTML = ""

  @JSExport("clearColor")
  def clearColor(): Unit =
    xpathTextArea.style.color = "black"

  private def showSuccess(xpathExpr: XPathExpr): Unit =
    xpathTextArea.style.color = "green"

    val freeVariablesUList = getFreeVariablesUList()
    val boundVariablesUList = getBoundVariablesUList()
    val calledFunctionsUList = getCalledFunctionsUList()

    freeVariablesUList.innerHTML = ""
    boundVariablesUList.innerHTML = ""
    calledFunctionsUList.innerHTML = ""

    val freeVariables = VariableBindingUtil.findAllFreeVariables(xpathExpr)
    val boundVariables = VariableBindingUtil.findAllBoundVariables(xpathExpr)
    val calledFunctions = xpathExpr.findAllElemsOrSelfOfType(classTag[FunctionCall])

    freeVariables.map(_.varName).distinct.foreach { freeVar =>
      addNewReadonlyListItem(freeVariablesUList, freeVar.toString, liCls)
    }

    boundVariables.map(_.varName).distinct.foreach { boundVar =>
      addNewReadonlyListItem(boundVariablesUList, boundVar.toString, liCls)
    }

    calledFunctions.map(_.functionName).distinct.foreach { funcCall =>
      addNewReadonlyListItem(calledFunctionsUList, funcCall.toString, liCls)
    }

    val codeElement = getAstPreElement().firstElementChild

    val codeString: String = pprint.apply(xpathExpr, height = 2000).plainText

    codeElement.innerHTML = ""
    addCodeString(codeElement, "\n" + codeString)

  private def showFailure(parseError: P.Error): Unit =
    xpathTextArea.style.color = "red"

    val freeVariablesUList = getFreeVariablesUList()
    val boundVariablesUList = getBoundVariablesUList()
    val calledFunctionsUList = getCalledFunctionsUList()

    freeVariablesUList.innerHTML = ""
    boundVariablesUList.innerHTML = ""
    calledFunctionsUList.innerHTML = ""

    getAstPreElement().firstElementChild.innerHTML = ""

  private def getXPathTextArea(): HTMLTextAreaElement =
    document.getElementById("xpathInput").ensuring(_ != null).asInstanceOf[HTMLTextAreaElement]

  private def getFreeVariablesUList(): HTMLUListElement =
    document.getElementById("freeVariables").ensuring(_ != null).asInstanceOf[HTMLUListElement]

  private def getBoundVariablesUList(): HTMLUListElement =
    document.getElementById("boundVariables").ensuring(_ != null).asInstanceOf[HTMLUListElement]

  private def getCalledFunctionsUList(): HTMLUListElement =
    document.getElementById("functions").ensuring(_ != null).asInstanceOf[HTMLUListElement]

  private def getAstPreElement(): HTMLPreElement =
    document.getElementById("astPre").ensuring(_ != null).asInstanceOf[HTMLPreElement]

  private def addNewReadonlyListItem(targetNode: Node, text: String, cssClass: String): Unit =
    val liElem = document.createElement("li").asInstanceOf[HTMLLIElement]
    liElem.className = cssClass
    val textNode = document.createTextNode(text)
    liElem.appendChild(textNode)
    targetNode.appendChild(liElem)

  private def addCodeString(targetNode: Node, text: String): Unit =
    val textNode = document.createTextNode(text)
    targetNode.appendChild(textNode)
