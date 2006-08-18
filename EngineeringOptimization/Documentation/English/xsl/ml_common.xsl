<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
		xmlns="http://www.w3.org/1999/xhtml" version="1.0">
<xsl:param name="html.stylesheet">/css/alpha/ml.css</xsl:param>
<xsl:param name="link.mailto.url">mailto:chris@chiasson.name</xsl:param>
<xsl:template match="author" mode="titlepage.mode">
  <div class="{name(.)}">
    <h3 class="{name(.)}">
      <a class="{name(.)}">
	<xsl:attribute name="href">mailto:<xsl:value-of select="./email"/></xsl:attribute>
	<xsl:call-template name="person.name"/>
      </a>
    </h3>
    <xsl:apply-templates mode="titlepage.mode" select="./contrib"/>
    <xsl:apply-templates mode="titlepage.mode" select="./affiliation"/>
    <xsl:apply-templates mode="titlepage.mode" select="./address"/>
  </div>
</xsl:template>
<!--xsl:template match="author" mode="titlepage.mode">
  <div class="{name(.)}">
    <h3 class="{name(.)}"><xsl:call-template name="person.name"/></h3>
    <xsl:apply-templates mode="titlepage.mode" select="./contrib"/>
    <xsl:apply-templates mode="titlepage.mode" select="./affiliation"/>
    <xsl:apply-templates mode="titlepage.mode" select="./email"/>
  </div>
</xsl:template-->

<xsl:template name="user.head.content">
  <link rel="alternate" type="application/atom+xml" href="/news.atom.xml" />
</xsl:template>

<xsl:param name="generate.toc">
appendix  toc,title
article/appendix  nop
article   nop
book      toc,title,figure,table,example,equation
chapter   nop
part      toc
preface   nop
qandadiv  nop
qandaset  nop
reference toc,title
sect1     nop
sect2     nop
sect3     nop
sect4     nop
sect5     nop
section   nop
set       toc,title
</xsl:param>
</xsl:stylesheet>
