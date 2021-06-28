============
XPath Parser
============

XPath parser, for static analysis of XPath. A use case could be static analysis of XPath expressions in XBRL formula
or table linkbases. For example, used custom functions could be found, or syntax errors in XPath could be found at an
early stage.

At this point only the AST that results from parsing can be queried. This AST has no knowledge other than the
structure of the parsed XPath expression.

This XPath parser makes use of the excellent `cats-parse`_ library for the generation of the XPath parser.

Older versions of this project (before 0.7.0) used the also excellent `FastParse`_ library. Support for Scala 3 was the reason
to move to cats-parse. Increased type safety and opt-in backtracking were other reasons for this move.

XPath 3.1 is the supported XPath version.

.. _`cats-parse`: https://github.com/typelevel/cats-parse
.. _`FastParse`: https://github.com/com-lihaoyi/fastparse
