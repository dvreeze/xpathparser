=========
CHANGELOG
=========


0.5.0
=====

This release has the following changes, compared to version 0.4.0:

* Made parsers for parts of XPath expressions publicly available, via object ``XPathElemParser``
* Now ``Expr`` inherits from ``XPathExpr``, which is a breaking change
* Parser ``XPathParser.xpathExpr`` still consumes the entire XPath string but now also ignores leading whitespace
* Added ``ExtractXPathTest`` demonstrating how to obtain XPath strings of parts of XPath expressions

By making all XPath element parsers publicly available, it is now far more feasible to add customized parsers
(outside the xpathparser code base) for custom parsing scenarios, such as extracting parts of XPath expressions
as XPath strings. That is the reason for this release. Desired functionality such as serializing AST elements
to strings has not yet been implemented.


0.4.0
=====

This release has the following changes, compared to version 0.3.2:

* Scala.js as second target platform
* A far better AST type hierarchy

  * For example, variable references indirectly inherit from expression, because they ARE expressions
  * This prevents an explosion of AST object graphs due to repeated deep object composition, which plagued previous releases
  * The AST type hierarchy can still be changed in the future, but it is far better than before

* Fixed bug in querying of free and bound variables
