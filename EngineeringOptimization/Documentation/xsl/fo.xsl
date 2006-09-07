<?xml version='1.0'?>
<xsl:stylesheet
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.0">
	<xsl:import href="http://docbook.sourceforge.net/release/xsl/current/fo/docbook.xsl"/>
	<xsl:import href="common.xsl"/>
	<!--xsl:param name="ulink.show">0</xsl:param-->
	<!--xsl:param name="alignment">left</xsl:param-->
	<!--xsl:param name="fop1.extensions">1</xsl:param-->
	<!--<xsl:param name="fop.extensions">1</xsl:param>-->
	<xsl:param name="xep.extensions">1</xsl:param>
	<xsl:attribute-set name="monospace.verbatim.properties">
		<xsl:attribute name="font-size">8pt</xsl:attribute>
	</xsl:attribute-set>
</xsl:stylesheet>
