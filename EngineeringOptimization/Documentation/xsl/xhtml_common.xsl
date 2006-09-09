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


	<!--this is the xhtml imagedata template - it is patched to handle svg and mathml-->
	<xsl:template match="imagedata" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:stext="http://nwalsh.com/xslt/ext/com.nwalsh.saxon.TextFactory" xmlns:simg="http://nwalsh.com/xslt/ext/com.nwalsh.saxon.ImageIntrinsics" xmlns:ximg="xaln://com.nwalsh.xalan.ImageIntrinsics" xmlns:xtext="com.nwalsh.xalan.Text" xmlns:lxslt="http://xml.apache.org/xslt" xmlns="http://www.w3.org/1999/xhtml" exclude-result-prefixes="xlink stext xtext lxslt simg ximg" extension-element-prefixes="stext xtext">
		<xsl:choose>
			<xsl:when xmlns:svg="http://www.w3.org/2000/svg" test="svg:*">
				<xsl:apply-templates/>
			</xsl:when>
			<xsl:when xmlns:mml="http://www.w3.org/1998/Math/MathML" test="mml:*">
				<xsl:apply-templates/>
			</xsl:when>
			<xsl:otherwise>
				<xsl:variable name="filename">
					<xsl:call-template name="mediaobject.filename">
						<xsl:with-param name="object" select=".."/>
					</xsl:call-template>
				</xsl:variable>
				
				<xsl:choose>
					<xsl:when test="@format='linespecific'">
						<xsl:choose>
							<xsl:when test="$use.extensions != '0'                         and $textinsert.extension != '0'">
								<xsl:choose>
									<xsl:when test="element-available('stext:insertfile')">
										<stext:insertfile href="{$filename}" encoding="{$textdata.default.encoding}"/>
									</xsl:when>
									<xsl:when test="element-available('xtext:insertfile')">
										<xtext:insertfile href="{$filename}"/>
									</xsl:when>
									<xsl:otherwise>
										<xsl:message terminate="yes">
											<xsl:text>No insertfile extension available.</xsl:text>
										</xsl:message>
									</xsl:otherwise>
								</xsl:choose>
							</xsl:when>
							<xsl:otherwise>
								<a xlink:type="simple" xlink:show="embed" xlink:actuate="onLoad" href="{$filename}"/>
							</xsl:otherwise>
						</xsl:choose>
					</xsl:when>
					<xsl:otherwise>
						<xsl:variable name="longdesc.uri">
							<xsl:call-template name="longdesc.uri">
								<xsl:with-param name="mediaobject" select="ancestor::imageobject/parent::*"/>
							</xsl:call-template>
						</xsl:variable>
						
						<xsl:variable name="phrases" select="ancestor::mediaobject/textobject[phrase]                             |ancestor::inlinemediaobject/textobject[phrase]                             |ancestor::mediaobjectco/textobject[phrase]"/>
						
						<xsl:call-template name="process.image">
							<xsl:with-param name="alt">
								<xsl:apply-templates select="$phrases[not(@role) or @role!='tex'][1]"/>
							</xsl:with-param>
							<xsl:with-param name="longdesc">
								<xsl:call-template name="write.longdesc">
									<xsl:with-param name="mediaobject" select="ancestor::imageobject/parent::*"/>
								</xsl:call-template>
							</xsl:with-param>
						</xsl:call-template>
						
						<xsl:if test="$html.longdesc != 0 and $html.longdesc.link != 0                     and ancestor::imageobject/parent::*/textobject[not(phrase)]">
							<xsl:call-template name="longdesc.link">
								<xsl:with-param name="longdesc.uri" select="$longdesc.uri"/>
							</xsl:call-template>
						</xsl:if>
					</xsl:otherwise>
				</xsl:choose>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

</xsl:stylesheet>
