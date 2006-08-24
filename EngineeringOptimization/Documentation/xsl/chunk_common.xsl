<?xml version='1.0'?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
		xmlns="http://www.w3.org/1999/xhtml" version="1.0">
<!-- ^^^ like the same statement in ml_common, the namespace here is
     kind of a hack - the html elements in the file should really be
     in the xhtml and html xsl files - but it works like this too-->

<xsl:param name="use.id.as.filename">1</xsl:param>
<xsl:param name="generate.manifest">1</xsl:param>
<xsl:param name="manifest">-</xsl:param>
<!--<xsl:param name="chunker.output.encoding">UTF-8</xsl:param>-->

<xsl:template name="chunk-element-content">
  <xsl:param name="prev"/>
  <xsl:param name="next"/>
  <xsl:param name="nav.context"/>
  <xsl:param name="content">
    <xsl:apply-imports/>
  </xsl:param>

  <xsl:call-template name="user.preroot"/>

  <html>
    <xsl:call-template name="html.head">
      <xsl:with-param name="prev" select="$prev"/>
      <xsl:with-param name="next" select="$next"/>
    </xsl:call-template>

    <body>
      <xsl:call-template name="body.attributes"/>

      <!-- this tucks the output of the document into 2 divs so that
           css can put it in the appropriate columns -->

      <div id="wrapper">
	<div id="block_1">

	  <xsl:call-template name="user.header.navigation"/>

	  <xsl:call-template name="header.navigation">
	    <xsl:with-param name="prev" select="$prev"/>
	    <xsl:with-param name="next" select="$next"/>
	    <xsl:with-param name="nav.context" select="$nav.context"/>
	  </xsl:call-template>

	  <xsl:call-template name="user.header.content"/>

	  <xsl:copy-of select="$content"/>

	  <xsl:call-template name="user.footer.content"/>

	  <xsl:call-template name="footer.navigation">
	    <xsl:with-param name="prev" select="$prev"/>
	    <xsl:with-param name="next" select="$next"/>
	    <xsl:with-param name="nav.context" select="$nav.context"/>
	  </xsl:call-template>

	  <xsl:call-template name="user.footer.navigation"/>

	  <!-- end of block_1 div -->
	</div>

	<!--call website sidebar-->
	<xsl:call-template name="site.sidebar"/>

	<!-- end of wrapper div -->
      </div>

    </body>
  </html>
</xsl:template>
</xsl:stylesheet>