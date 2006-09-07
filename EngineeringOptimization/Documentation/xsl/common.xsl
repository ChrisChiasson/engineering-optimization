<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
	<!--xsl:param name="chapter.autolabel">0</xsl:param-->
	<xsl:param name="label.from.part">1</xsl:param>
	<xsl:param name="component.label.includes.part.label">1</xsl:param>
	<xsl:param name="section.autolabel">1</xsl:param>
	<!--follow xml:base-->
	<xsl:param name="keep.relative.image.uris">0</xsl:param>
	<!--olinking on-->
	<xsl:param name="collect.xref.targets">no</xsl:param>
	<xsl:param name="target.database.document">target.xml</xsl:param>
	<xsl:param name="current.docid">self</xsl:param>
	<!--draft mode off-->
	<xsl:param name="draft.watermark.image">draft.png</xsl:param>
	<xsl:param name="draft.mode">no</xsl:param>
</xsl:stylesheet>
