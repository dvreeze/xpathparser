=========
CHANGELOG
=========


0.6.0
=====

This release moves the project to FastParse 2, and it cross-builds to Scala 2.13 and Scala 2.12, dropping support for Scala 2.11.
Obviously, this release is not backward compatible with version 0.5.1.


0.5.1
=====

This release is like version 0.5.0, but adds ``EQNameUtil``. This utility can help in finding namespace prefixes
occurring in XPath expressions.


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
