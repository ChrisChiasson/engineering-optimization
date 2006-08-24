<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns="http://www.w3.org/1999/xhtml" version="1.0">

<!--the condition paramter mathml should not be necessary after conversion to DocBook 5-->

<xsl:param name="profile.condition">mathml;video</xsl:param>

<!--xhtml strict conformance parameters-->

<xsl:param name="css.decoration">0</xsl:param>
<xsl:param name="html.longdesc">0</xsl:param>
<xsl:param name="ulink.target"></xsl:param>
<xsl:param name="use.viewport">0</xsl:param>

<!--end xhtml strict conformance parameters-->

<!--apparently, there is no way to turn off the DTD,
	and the XHTML+MathML+SVG DTD doesn't actually match
	the stylesheet's output (at least with db4 and the
	way I had things setup)-->
	
<xsl:output method="xml"
            encoding="UTF-8"
            indent="no"
            doctype-public=""
            doctype-system=""/>

<xsl:template name="site.sidebar">
  <div id="block_2">
        <div id="side_navigation_links">
	  <p><a href="/">Go to this website's root page.</a></p>
	</div>

        <!-- SiteSearch Google -->

        <form method="get" action="http://www.google.com/custom" target="_top">
        <p><a href="http://www.google.com/"><img src="http://www.google.com/logos/Logo_25wht.gif" border="0" alt="Google"/></a> search this site:</p>
        <input type="hidden" name="domains" value="chiasson.name"></input>
        <input type="text" name="q" size="20" maxlength="255" value=""></input>
        <input type="submit" name="sa" value="Go"></input>
        <input type="hidden" name="sitesearch" value="chiasson.name"/>
        <input type="hidden" name="client" value="pub-9913151546120299"></input>
        <input type="hidden" name="forid" value="1"></input>
        <input type="hidden" name="ie" value="UTF-8"></input>
        <input type="hidden" name="oe" value="UTF-8"></input>
        <input type="hidden" name="cof" value="GALT:#008000;GL:1;DIV:#336699;VLC:663399;AH:center;BGC:FFFFFF;LBGC:336699;ALC:0000FF;LC:0000FF;T:000000;GFNT:0000FF;GIMP:0000FF;FORID:1;"></input>
        <input type="hidden" name="hl" value="en"></input>
        </form><br/>

        <!-- SiteSearch Google -->

    <object type="text/html" data="html/related_links.html" width="180px" height="150px"/><br/><br/>
    <object type="text/html" data="html/adsense.html" width="160px" height="600px"/><br/><br/>
    <object type="text/html" data="html/get_firefox_with_google_toolbar.html" width="120px" height="240px"/><br/><br/>
    <script type="text/javascript" src="http://www.google-analytics.com/urchin.js"/>
    <script type="text/javascript" src="js/urchin_tracker.js"/>
  </div>
</xsl:template>

</xsl:stylesheet>
