=========
CHANGELOG
=========


0.8.0
=====

Johan Walters (github user fourth44) discovered a regression in the AST. The larger problem was that the operator
associativity was not represented correctly in the AST, so he made a pull request with a fix. Ultimately a fix
was committed that resembles the pull request, but keeps recursion in some of the AST data structures. The result has
been checked against the XPath 3.1 spec, in particular the table of operator precedence and associativity, so
Johan Walters' findings have led to a substantial improvement in the AST.

Some of the changes in the AST are breaking. These are (found with xpathparserJVM/*:mimaReportBinaryIssues):

* static method apply(eu.cdevreeze.xpathparser.ast.UnionExpr,eu.cdevreeze.xpathparser.ast.MultiplicativeOp,eu.cdevreeze.xpathparser.ast.MultiplicativeExpr)eu.cdevreeze.xpathparser.ast.CompoundMultiplicativeExpr in class eu.cdevreeze.xpathparser.ast.CompoundMultiplicativeExpr's type is different in current version, where it is (eu.cdevreeze.xpathparser.ast.MultiplicativeExpr,eu.cdevreeze.xpathparser.ast.MultiplicativeOp,eu.cdevreeze.xpathparser.ast.UnionExpr)eu.cdevreeze.xpathparser.ast.CompoundMultiplicativeExpr instead of (eu.cdevreeze.xpathparser.ast.UnionExpr,eu.cdevreeze.xpathparser.ast.MultiplicativeOp,eu.cdevreeze.xpathparser.ast.MultiplicativeExpr)eu.cdevreeze.xpathparser.ast.CompoundMultiplicativeExpr
  filter with: ProblemFilters.exclude[IncompatibleMethTypeProblem]("eu.cdevreeze.xpathparser.ast.CompoundMultiplicativeExpr.apply")
* method firstUnionExpr()eu.cdevreeze.xpathparser.ast.UnionExpr in class eu.cdevreeze.xpathparser.ast.CompoundMultiplicativeExpr does not have a correspondent in current version
  filter with: ProblemFilters.exclude[DirectMissingMethodProblem]("eu.cdevreeze.xpathparser.ast.CompoundMultiplicativeExpr.firstUnionExpr")
* method remainder()eu.cdevreeze.xpathparser.ast.MultiplicativeExpr in class eu.cdevreeze.xpathparser.ast.CompoundMultiplicativeExpr does not have a correspondent in current version
  filter with: ProblemFilters.exclude[DirectMissingMethodProblem]("eu.cdevreeze.xpathparser.ast.CompoundMultiplicativeExpr.remainder")
* method copy(eu.cdevreeze.xpathparser.ast.UnionExpr,eu.cdevreeze.xpathparser.ast.MultiplicativeOp,eu.cdevreeze.xpathparser.ast.MultiplicativeExpr)eu.cdevreeze.xpathparser.ast.CompoundMultiplicativeExpr in class eu.cdevreeze.xpathparser.ast.CompoundMultiplicativeExpr's type is different in current version, where it is (eu.cdevreeze.xpathparser.ast.MultiplicativeExpr,eu.cdevreeze.xpathparser.ast.MultiplicativeOp,eu.cdevreeze.xpathparser.ast.UnionExpr)eu.cdevreeze.xpathparser.ast.CompoundMultiplicativeExpr instead of (eu.cdevreeze.xpathparser.ast.UnionExpr,eu.cdevreeze.xpathparser.ast.MultiplicativeOp,eu.cdevreeze.xpathparser.ast.MultiplicativeExpr)eu.cdevreeze.xpathparser.ast.CompoundMultiplicativeExpr
  filter with: ProblemFilters.exclude[IncompatibleMethTypeProblem]("eu.cdevreeze.xpathparser.ast.CompoundMultiplicativeExpr.copy")
* synthetic method copy$default$1()eu.cdevreeze.xpathparser.ast.UnionExpr in class eu.cdevreeze.xpathparser.ast.CompoundMultiplicativeExpr has a different result type in current version, where it is eu.cdevreeze.xpathparser.ast.MultiplicativeExpr rather than eu.cdevreeze.xpathparser.ast.UnionExpr
  filter with: ProblemFilters.exclude[IncompatibleResultTypeProblem]("eu.cdevreeze.xpathparser.ast.CompoundMultiplicativeExpr.copy$default$1")
* synthetic method copy$default$3()eu.cdevreeze.xpathparser.ast.MultiplicativeExpr in class eu.cdevreeze.xpathparser.ast.CompoundMultiplicativeExpr has a different result type in current version, where it is eu.cdevreeze.xpathparser.ast.UnionExpr rather than eu.cdevreeze.xpathparser.ast.MultiplicativeExpr
  filter with: ProblemFilters.exclude[IncompatibleResultTypeProblem]("eu.cdevreeze.xpathparser.ast.CompoundMultiplicativeExpr.copy$default$3")
* method this(eu.cdevreeze.xpathparser.ast.UnionExpr,eu.cdevreeze.xpathparser.ast.MultiplicativeOp,eu.cdevreeze.xpathparser.ast.MultiplicativeExpr)Unit in class eu.cdevreeze.xpathparser.ast.CompoundMultiplicativeExpr's type is different in current version, where it is (eu.cdevreeze.xpathparser.ast.MultiplicativeExpr,eu.cdevreeze.xpathparser.ast.MultiplicativeOp,eu.cdevreeze.xpathparser.ast.UnionExpr)Unit instead of (eu.cdevreeze.xpathparser.ast.UnionExpr,eu.cdevreeze.xpathparser.ast.MultiplicativeOp,eu.cdevreeze.xpathparser.ast.MultiplicativeExpr)Unit
  filter with: ProblemFilters.exclude[IncompatibleMethTypeProblem]("eu.cdevreeze.xpathparser.ast.CompoundMultiplicativeExpr.this")
* method apply(eu.cdevreeze.xpathparser.ast.StepExpr,eu.cdevreeze.xpathparser.ast.StepOp,eu.cdevreeze.xpathparser.ast.RelativePathExpr)eu.cdevreeze.xpathparser.ast.CompoundRelativePathExpr in object eu.cdevreeze.xpathparser.ast.CompoundRelativePathExpr in current version does not have a correspondent with same parameter signature among (eu.cdevreeze.xpathparser.ast.RelativePathExpr,eu.cdevreeze.xpathparser.ast.StepOp,eu.cdevreeze.xpathparser.ast.StepExpr)eu.cdevreeze.xpathparser.ast.CompoundRelativePathExpr, (java.lang.Object,java.lang.Object,java.lang.Object)java.lang.Object
  filter with: ProblemFilters.exclude[IncompatibleMethTypeProblem]("eu.cdevreeze.xpathparser.ast.CompoundRelativePathExpr.apply")
* method apply(eu.cdevreeze.xpathparser.ast.UnionExpr,eu.cdevreeze.xpathparser.ast.MultiplicativeOp,eu.cdevreeze.xpathparser.ast.MultiplicativeExpr)eu.cdevreeze.xpathparser.ast.CompoundMultiplicativeExpr in object eu.cdevreeze.xpathparser.ast.CompoundMultiplicativeExpr in current version does not have a correspondent with same parameter signature among (eu.cdevreeze.xpathparser.ast.MultiplicativeExpr,eu.cdevreeze.xpathparser.ast.MultiplicativeOp,eu.cdevreeze.xpathparser.ast.UnionExpr)eu.cdevreeze.xpathparser.ast.CompoundMultiplicativeExpr, (java.lang.Object,java.lang.Object,java.lang.Object)java.lang.Object
  filter with: ProblemFilters.exclude[IncompatibleMethTypeProblem]("eu.cdevreeze.xpathparser.ast.CompoundMultiplicativeExpr.apply")
* method apply(eu.cdevreeze.xpathparser.ast.MultiplicativeExpr,eu.cdevreeze.xpathparser.ast.AdditionOp,eu.cdevreeze.xpathparser.ast.AdditiveExpr)eu.cdevreeze.xpathparser.ast.CompoundAdditiveExpr in object eu.cdevreeze.xpathparser.ast.CompoundAdditiveExpr in current version does not have a correspondent with same parameter signature among (eu.cdevreeze.xpathparser.ast.AdditiveExpr,eu.cdevreeze.xpathparser.ast.AdditionOp,eu.cdevreeze.xpathparser.ast.MultiplicativeExpr)eu.cdevreeze.xpathparser.ast.CompoundAdditiveExpr, (java.lang.Object,java.lang.Object,java.lang.Object)java.lang.Object
  filter with: ProblemFilters.exclude[IncompatibleMethTypeProblem]("eu.cdevreeze.xpathparser.ast.CompoundAdditiveExpr.apply")
* static method apply(eu.cdevreeze.xpathparser.ast.InstanceOfExpr,eu.cdevreeze.xpathparser.ast.IntersectExceptOp,eu.cdevreeze.xpathparser.ast.IntersectExceptExpr)eu.cdevreeze.xpathparser.ast.CompoundIntersectExceptExpr in class eu.cdevreeze.xpathparser.ast.CompoundIntersectExceptExpr's type is different in current version, where it is (eu.cdevreeze.xpathparser.ast.IntersectExceptExpr,eu.cdevreeze.xpathparser.ast.IntersectExceptOp,eu.cdevreeze.xpathparser.ast.InstanceOfExpr)eu.cdevreeze.xpathparser.ast.CompoundIntersectExceptExpr instead of (eu.cdevreeze.xpathparser.ast.InstanceOfExpr,eu.cdevreeze.xpathparser.ast.IntersectExceptOp,eu.cdevreeze.xpathparser.ast.IntersectExceptExpr)eu.cdevreeze.xpathparser.ast.CompoundIntersectExceptExpr
  filter with: ProblemFilters.exclude[IncompatibleMethTypeProblem]("eu.cdevreeze.xpathparser.ast.CompoundIntersectExceptExpr.apply")
* method firstInstanceOfExpr()eu.cdevreeze.xpathparser.ast.InstanceOfExpr in class eu.cdevreeze.xpathparser.ast.CompoundIntersectExceptExpr does not have a correspondent in current version
  filter with: ProblemFilters.exclude[DirectMissingMethodProblem]("eu.cdevreeze.xpathparser.ast.CompoundIntersectExceptExpr.firstInstanceOfExpr")
* method remainder()eu.cdevreeze.xpathparser.ast.IntersectExceptExpr in class eu.cdevreeze.xpathparser.ast.CompoundIntersectExceptExpr does not have a correspondent in current version
  filter with: ProblemFilters.exclude[DirectMissingMethodProblem]("eu.cdevreeze.xpathparser.ast.CompoundIntersectExceptExpr.remainder")
* method copy(eu.cdevreeze.xpathparser.ast.InstanceOfExpr,eu.cdevreeze.xpathparser.ast.IntersectExceptOp,eu.cdevreeze.xpathparser.ast.IntersectExceptExpr)eu.cdevreeze.xpathparser.ast.CompoundIntersectExceptExpr in class eu.cdevreeze.xpathparser.ast.CompoundIntersectExceptExpr's type is different in current version, where it is (eu.cdevreeze.xpathparser.ast.IntersectExceptExpr,eu.cdevreeze.xpathparser.ast.IntersectExceptOp,eu.cdevreeze.xpathparser.ast.InstanceOfExpr)eu.cdevreeze.xpathparser.ast.CompoundIntersectExceptExpr instead of (eu.cdevreeze.xpathparser.ast.InstanceOfExpr,eu.cdevreeze.xpathparser.ast.IntersectExceptOp,eu.cdevreeze.xpathparser.ast.IntersectExceptExpr)eu.cdevreeze.xpathparser.ast.CompoundIntersectExceptExpr
  filter with: ProblemFilters.exclude[IncompatibleMethTypeProblem]("eu.cdevreeze.xpathparser.ast.CompoundIntersectExceptExpr.copy")
* synthetic method copy$default$1()eu.cdevreeze.xpathparser.ast.InstanceOfExpr in class eu.cdevreeze.xpathparser.ast.CompoundIntersectExceptExpr has a different result type in current version, where it is eu.cdevreeze.xpathparser.ast.IntersectExceptExpr rather than eu.cdevreeze.xpathparser.ast.InstanceOfExpr
  filter with: ProblemFilters.exclude[IncompatibleResultTypeProblem]("eu.cdevreeze.xpathparser.ast.CompoundIntersectExceptExpr.copy$default$1")
* synthetic method copy$default$3()eu.cdevreeze.xpathparser.ast.IntersectExceptExpr in class eu.cdevreeze.xpathparser.ast.CompoundIntersectExceptExpr has a different result type in current version, where it is eu.cdevreeze.xpathparser.ast.InstanceOfExpr rather than eu.cdevreeze.xpathparser.ast.IntersectExceptExpr
  filter with: ProblemFilters.exclude[IncompatibleResultTypeProblem]("eu.cdevreeze.xpathparser.ast.CompoundIntersectExceptExpr.copy$default$3")
* method this(eu.cdevreeze.xpathparser.ast.InstanceOfExpr,eu.cdevreeze.xpathparser.ast.IntersectExceptOp,eu.cdevreeze.xpathparser.ast.IntersectExceptExpr)Unit in class eu.cdevreeze.xpathparser.ast.CompoundIntersectExceptExpr's type is different in current version, where it is (eu.cdevreeze.xpathparser.ast.IntersectExceptExpr,eu.cdevreeze.xpathparser.ast.IntersectExceptOp,eu.cdevreeze.xpathparser.ast.InstanceOfExpr)Unit instead of (eu.cdevreeze.xpathparser.ast.InstanceOfExpr,eu.cdevreeze.xpathparser.ast.IntersectExceptOp,eu.cdevreeze.xpathparser.ast.IntersectExceptExpr)Unit
  filter with: ProblemFilters.exclude[IncompatibleMethTypeProblem]("eu.cdevreeze.xpathparser.ast.CompoundIntersectExceptExpr.this")
* static method apply(eu.cdevreeze.xpathparser.ast.StepExpr,eu.cdevreeze.xpathparser.ast.StepOp,eu.cdevreeze.xpathparser.ast.RelativePathExpr)eu.cdevreeze.xpathparser.ast.CompoundRelativePathExpr in class eu.cdevreeze.xpathparser.ast.CompoundRelativePathExpr's type is different in current version, where it is (eu.cdevreeze.xpathparser.ast.RelativePathExpr,eu.cdevreeze.xpathparser.ast.StepOp,eu.cdevreeze.xpathparser.ast.StepExpr)eu.cdevreeze.xpathparser.ast.CompoundRelativePathExpr instead of (eu.cdevreeze.xpathparser.ast.StepExpr,eu.cdevreeze.xpathparser.ast.StepOp,eu.cdevreeze.xpathparser.ast.RelativePathExpr)eu.cdevreeze.xpathparser.ast.CompoundRelativePathExpr
  filter with: ProblemFilters.exclude[IncompatibleMethTypeProblem]("eu.cdevreeze.xpathparser.ast.CompoundRelativePathExpr.apply")
* method firstStepExpr()eu.cdevreeze.xpathparser.ast.StepExpr in class eu.cdevreeze.xpathparser.ast.CompoundRelativePathExpr does not have a correspondent in current version
  filter with: ProblemFilters.exclude[DirectMissingMethodProblem]("eu.cdevreeze.xpathparser.ast.CompoundRelativePathExpr.firstStepExpr")
* method remainder()eu.cdevreeze.xpathparser.ast.RelativePathExpr in class eu.cdevreeze.xpathparser.ast.CompoundRelativePathExpr does not have a correspondent in current version
  filter with: ProblemFilters.exclude[DirectMissingMethodProblem]("eu.cdevreeze.xpathparser.ast.CompoundRelativePathExpr.remainder")
* method copy(eu.cdevreeze.xpathparser.ast.StepExpr,eu.cdevreeze.xpathparser.ast.StepOp,eu.cdevreeze.xpathparser.ast.RelativePathExpr)eu.cdevreeze.xpathparser.ast.CompoundRelativePathExpr in class eu.cdevreeze.xpathparser.ast.CompoundRelativePathExpr's type is different in current version, where it is (eu.cdevreeze.xpathparser.ast.RelativePathExpr,eu.cdevreeze.xpathparser.ast.StepOp,eu.cdevreeze.xpathparser.ast.StepExpr)eu.cdevreeze.xpathparser.ast.CompoundRelativePathExpr instead of (eu.cdevreeze.xpathparser.ast.StepExpr,eu.cdevreeze.xpathparser.ast.StepOp,eu.cdevreeze.xpathparser.ast.RelativePathExpr)eu.cdevreeze.xpathparser.ast.CompoundRelativePathExpr
  filter with: ProblemFilters.exclude[IncompatibleMethTypeProblem]("eu.cdevreeze.xpathparser.ast.CompoundRelativePathExpr.copy")
* synthetic method copy$default$1()eu.cdevreeze.xpathparser.ast.StepExpr in class eu.cdevreeze.xpathparser.ast.CompoundRelativePathExpr has a different result type in current version, where it is eu.cdevreeze.xpathparser.ast.RelativePathExpr rather than eu.cdevreeze.xpathparser.ast.StepExpr
  filter with: ProblemFilters.exclude[IncompatibleResultTypeProblem]("eu.cdevreeze.xpathparser.ast.CompoundRelativePathExpr.copy$default$1")
* synthetic method copy$default$3()eu.cdevreeze.xpathparser.ast.RelativePathExpr in class eu.cdevreeze.xpathparser.ast.CompoundRelativePathExpr has a different result type in current version, where it is eu.cdevreeze.xpathparser.ast.StepExpr rather than eu.cdevreeze.xpathparser.ast.RelativePathExpr
  filter with: ProblemFilters.exclude[IncompatibleResultTypeProblem]("eu.cdevreeze.xpathparser.ast.CompoundRelativePathExpr.copy$default$3")
* method this(eu.cdevreeze.xpathparser.ast.StepExpr,eu.cdevreeze.xpathparser.ast.StepOp,eu.cdevreeze.xpathparser.ast.RelativePathExpr)Unit in class eu.cdevreeze.xpathparser.ast.CompoundRelativePathExpr's type is different in current version, where it is (eu.cdevreeze.xpathparser.ast.RelativePathExpr,eu.cdevreeze.xpathparser.ast.StepOp,eu.cdevreeze.xpathparser.ast.StepExpr)Unit instead of (eu.cdevreeze.xpathparser.ast.StepExpr,eu.cdevreeze.xpathparser.ast.StepOp,eu.cdevreeze.xpathparser.ast.RelativePathExpr)Unit
  filter with: ProblemFilters.exclude[IncompatibleMethTypeProblem]("eu.cdevreeze.xpathparser.ast.CompoundRelativePathExpr.this")
* method apply(eu.cdevreeze.xpathparser.ast.InstanceOfExpr,eu.cdevreeze.xpathparser.ast.IntersectExceptOp,eu.cdevreeze.xpathparser.ast.IntersectExceptExpr)eu.cdevreeze.xpathparser.ast.CompoundIntersectExceptExpr in object eu.cdevreeze.xpathparser.ast.CompoundIntersectExceptExpr in current version does not have a correspondent with same parameter signature among (eu.cdevreeze.xpathparser.ast.IntersectExceptExpr,eu.cdevreeze.xpathparser.ast.IntersectExceptOp,eu.cdevreeze.xpathparser.ast.InstanceOfExpr)eu.cdevreeze.xpathparser.ast.CompoundIntersectExceptExpr, (java.lang.Object,java.lang.Object,java.lang.Object)java.lang.Object
  filter with: ProblemFilters.exclude[IncompatibleMethTypeProblem]("eu.cdevreeze.xpathparser.ast.CompoundIntersectExceptExpr.apply")
* static method apply(eu.cdevreeze.xpathparser.ast.MultiplicativeExpr,eu.cdevreeze.xpathparser.ast.AdditionOp,eu.cdevreeze.xpathparser.ast.AdditiveExpr)eu.cdevreeze.xpathparser.ast.CompoundAdditiveExpr in class eu.cdevreeze.xpathparser.ast.CompoundAdditiveExpr's type is different in current version, where it is (eu.cdevreeze.xpathparser.ast.AdditiveExpr,eu.cdevreeze.xpathparser.ast.AdditionOp,eu.cdevreeze.xpathparser.ast.MultiplicativeExpr)eu.cdevreeze.xpathparser.ast.CompoundAdditiveExpr instead of (eu.cdevreeze.xpathparser.ast.MultiplicativeExpr,eu.cdevreeze.xpathparser.ast.AdditionOp,eu.cdevreeze.xpathparser.ast.AdditiveExpr)eu.cdevreeze.xpathparser.ast.CompoundAdditiveExpr
  filter with: ProblemFilters.exclude[IncompatibleMethTypeProblem]("eu.cdevreeze.xpathparser.ast.CompoundAdditiveExpr.apply")
* method firstMultiplicativeExpr()eu.cdevreeze.xpathparser.ast.MultiplicativeExpr in class eu.cdevreeze.xpathparser.ast.CompoundAdditiveExpr does not have a correspondent in current version
  filter with: ProblemFilters.exclude[DirectMissingMethodProblem]("eu.cdevreeze.xpathparser.ast.CompoundAdditiveExpr.firstMultiplicativeExpr")
* method remainder()eu.cdevreeze.xpathparser.ast.AdditiveExpr in class eu.cdevreeze.xpathparser.ast.CompoundAdditiveExpr does not have a correspondent in current version
  filter with: ProblemFilters.exclude[DirectMissingMethodProblem]("eu.cdevreeze.xpathparser.ast.CompoundAdditiveExpr.remainder")
* method copy(eu.cdevreeze.xpathparser.ast.MultiplicativeExpr,eu.cdevreeze.xpathparser.ast.AdditionOp,eu.cdevreeze.xpathparser.ast.AdditiveExpr)eu.cdevreeze.xpathparser.ast.CompoundAdditiveExpr in class eu.cdevreeze.xpathparser.ast.CompoundAdditiveExpr's type is different in current version, where it is (eu.cdevreeze.xpathparser.ast.AdditiveExpr,eu.cdevreeze.xpathparser.ast.AdditionOp,eu.cdevreeze.xpathparser.ast.MultiplicativeExpr)eu.cdevreeze.xpathparser.ast.CompoundAdditiveExpr instead of (eu.cdevreeze.xpathparser.ast.MultiplicativeExpr,eu.cdevreeze.xpathparser.ast.AdditionOp,eu.cdevreeze.xpathparser.ast.AdditiveExpr)eu.cdevreeze.xpathparser.ast.CompoundAdditiveExpr
  filter with: ProblemFilters.exclude[IncompatibleMethTypeProblem]("eu.cdevreeze.xpathparser.ast.CompoundAdditiveExpr.copy")
* synthetic method copy$default$1()eu.cdevreeze.xpathparser.ast.MultiplicativeExpr in class eu.cdevreeze.xpathparser.ast.CompoundAdditiveExpr has a different result type in current version, where it is eu.cdevreeze.xpathparser.ast.AdditiveExpr rather than eu.cdevreeze.xpathparser.ast.MultiplicativeExpr
  filter with: ProblemFilters.exclude[IncompatibleResultTypeProblem]("eu.cdevreeze.xpathparser.ast.CompoundAdditiveExpr.copy$default$1")
* synthetic method copy$default$3()eu.cdevreeze.xpathparser.ast.AdditiveExpr in class eu.cdevreeze.xpathparser.ast.CompoundAdditiveExpr has a different result type in current version, where it is eu.cdevreeze.xpathparser.ast.MultiplicativeExpr rather than eu.cdevreeze.xpathparser.ast.AdditiveExpr
  filter with: ProblemFilters.exclude[IncompatibleResultTypeProblem]("eu.cdevreeze.xpathparser.ast.CompoundAdditiveExpr.copy$default$3")
* method this(eu.cdevreeze.xpathparser.ast.MultiplicativeExpr,eu.cdevreeze.xpathparser.ast.AdditionOp,eu.cdevreeze.xpathparser.ast.AdditiveExpr)Unit in class eu.cdevreeze.xpathparser.ast.CompoundAdditiveExpr's type is different in current version, where it is (eu.cdevreeze.xpathparser.ast.AdditiveExpr,eu.cdevreeze.xpathparser.ast.AdditionOp,eu.cdevreeze.xpathparser.ast.MultiplicativeExpr)Unit instead of (eu.cdevreeze.xpathparser.ast.MultiplicativeExpr,eu.cdevreeze.xpathparser.ast.AdditionOp,eu.cdevreeze.xpathparser.ast.AdditiveExpr)Unit
  filter with: ProblemFilters.exclude[IncompatibleMethTypeProblem]("eu.cdevreeze.xpathparser.ast.CompoundAdditiveExpr.this")


0.7.0
=====

This release drops support for Scala 2.12, does cross-compile to Scala 3 and 2.13.6, and uses cats-parse
instead of FastParse. Note that FastParse uses macros and does not support Scala 3 (at the moment?).
Also, cats-parse offers more type-safety, and uses opt-in instead of opt-out backtracking.

With the use of cats-parse, cats-core is also a dependency. That came in handy, in order to use its
non-empty collections in the AST classes, thus making them more type-safe.

Due to the move from FastParse to cats-parse, there are too many breaking changes to mention.
In other words, this release is not at all backwards compatible with previous releases.
Having said that, most compilation errors when using release 0.7.0 instead of 0.6.1 should be
relatively easy and quick to fix.


0.6.1
=====

This release upgrades dependencies, including scalajs, which is now version 1.0.1.


0.6.0
=====

This release moves the project to FastParse 2, and it cross-builds to Scala 2.13 and Scala 2.12, dropping support for Scala 2.11.
Obviously, this release is not backward compatible with version 0.5.1. On the other hand, this release is at the source level as compatible as
possible with version 0.5.1, given that FastParse 2 has now been used (instead of FastParse 1).


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
