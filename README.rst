============
XPath Parser
============

XPath parser, for static analysis of XPath. A use case could be static analysis of XPath expressions in XBRL formula
or table linkbases. For example, used custom functions could be found, or syntax errors in XPath could be found at an
early stage.

This XPath parser makes use of the excellent `FastParse`_ library for the generation of the XPath parser.

XPath 3.1 is the supported XPath version.

.. _`FastParse`: http://www.lihaoyi.com/fastparse/
