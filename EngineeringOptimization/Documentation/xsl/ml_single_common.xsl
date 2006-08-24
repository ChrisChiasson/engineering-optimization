<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
		xmlns="http://www.w3.org/1999/xhtml" version="1.0">
<!-- ^^^ like the same statement in ml_common, the namespace here is
     kind of a hack - the html elements in the file should really be
     in the xhtml and html xsl files - but it works like this too-->

<xsl:template match="*" mode="process.root">
  <xsl:variable name="doc" select="self::*"/>

  <xsl:call-template name="user.preroot"/>
  <xsl:call-template name="root.messages"/>

  <html>
    <head>
      <xsl:call-template name="system.head.content">
        <xsl:with-param name="node" select="$doc"/>
      </xsl:call-template>
      <xsl:call-template name="head.content">
        <xsl:with-param name="node" select="$doc"/>
      </xsl:call-template>
      <xsl:call-template name="user.head.content">
        <xsl:with-param name="node" select="$doc"/>
      </xsl:call-template>
    </head>
    <body>
      <xsl:call-template name="body.attributes"/>   

      <!-- this tucks the output of the document into 2 divs so that
           css can put it in the appropriate columns -->

      <div id="wrapper">
	<div id="block_1">

	  <xsl:call-template name="user.header.content">
	    <xsl:with-param name="node" select="$doc"/>
	  </xsl:call-template>

	  <xsl:apply-templates select="."/>

	  <xsl:call-template name="user.footer.content">
	    <xsl:with-param name="node" select="$doc"/>
	  </xsl:call-template>

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