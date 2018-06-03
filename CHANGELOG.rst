=========
CHANGELOG
=========


0.4.0
=====

This release has the following changes, compared to version 0.3.2:

* Scala.js as second target platform
* A far better AST type hierarchy

  * For example, variable references indirectly inherit from expression, because they ARE expressions
  * This prevents an explosion of AST object graphs due to repeated deep object composition, which plagued previous releases
  * The AST type hierarchy can still be changed in the future, but it is far better than before

* Fixed querying of free and bound variables
