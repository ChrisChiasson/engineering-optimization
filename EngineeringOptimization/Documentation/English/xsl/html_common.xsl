<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:param name="profile.condition">nonmathml;video</xsl:param>
<xsl:output method="html"
            media-type="text/html"
            encoding="ISO-8859-1"
            indent="no"
            doctype-public="-//W3C//DTD HTML 4.01 Transitional//EN"
            doctype-system="http://www.w3.org/TR/html4/loose.dtd"/>

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
    <script type="text/javascript" src="/js/alpha/related_links.js"/>
    <script type="text/javascript" src="http://relcontent.googlesyndication.com/relcontent/show_rc.js"/><br/><br/>
    <script type="text/javascript" src="/js/alpha/adsense_content.js"/>
    <script type="text/javascript" src="http://pagead2.googlesyndication.com/pagead/show_ads.js"/><br/><br/>
    <script type="text/javascript" src="/js/alpha/get_firefox_with_google_toolbar.js"/>
    <script type="text/javascript" src="http://pagead2.googlesyndication.com/pagead/show_ads.js"/>
    <script type="text/javascript" src="http://www.google-analytics.com/urchin.js"/>
    <script type="text/javascript" src="/js/alpha/urchin_tracker.js"/>
  </div>
</xsl:template>

</xsl:stylesheet>