<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
	<!--numbering or lettering of chapters (1 is that default)-->
	<xsl:param name="chapter.autolabel">1</xsl:param>
	<!--restart chapter numbering at each part? (0 is the default)-->
	<xsl:param name="label.from.part">0</xsl:param>
	<!--indication of part number in hot link automatic text (0 default)-->
	<xsl:param name="component.label.includes.part.label">1</xsl:param>
	<!--section numbering (so it will be used in hot text) (0 default)-->
	<xsl:param name="section.autolabel">1</xsl:param>
	<!--follow xml:base-->
	<xsl:param name="keep.relative.image.uris">0</xsl:param>
	<!--olinking on (defaluts to no)-->
	<xsl:param name="collect.xref.targets">yes</xsl:param>
	<!--the single entry in target.xml points to target.db-->
	<xsl:param name="target.database.document">target.xml</xsl:param>
	<!--set the doc id to self to match target.xml-->
	<xsl:param name="current.docid">self</xsl:param>
	<!--if draft mode is on, use a local image-->
	<xsl:param name="draft.watermark.image">draft.png</xsl:param>
	<!--draft mode off (defaults to maybe)-->
	<xsl:param name="draft.mode">no</xsl:param>
</xsl:stylesheet>
