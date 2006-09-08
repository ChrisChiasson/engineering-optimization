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
	
	<!--template where the iftest for the first tgroup has been changed-->
	<xsl:template match="tgroup" name="tgroup">
		<xsl:if test="not(@cols)">
			<xsl:message terminate="yes">
				<xsl:text>Error: CALS tables must specify the number of columns.</xsl:text>
			</xsl:message>
		</xsl:if>
		
		<xsl:variable name="table.width">
			<xsl:call-template name="table.width"/>
		</xsl:variable>
		
		<xsl:variable name="colspecs">
			<xsl:choose>
				<xsl:when test="$use.extensions != 0
					and $tablecolumns.extension != 0">
					<xsl:call-template name="generate.colgroup.raw">
						<xsl:with-param name="cols" select="@cols"/>
					</xsl:call-template>
				</xsl:when>
				<xsl:otherwise>
					<xsl:call-template name="generate.colgroup">
						<xsl:with-param name="cols" select="@cols"/>
					</xsl:call-template>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:variable>
		
		<xsl:if test="count(preceding-sibling::tgroup) = 0">
			<!-- If this is the first tgroup, output the width attribute for the -->
			<!-- surrounding fo:table. (If this isn't the first tgroup, trying   -->
			<!-- to output the attribute will cause an error.)                   -->
			<xsl:attribute name="width">
				<xsl:value-of select="$table.width"/>
			</xsl:attribute>
		</xsl:if>
		
		<xsl:choose>
			<xsl:when test="$use.extensions != 0
				and $tablecolumns.extension != 0">
				<xsl:choose>
					<xsl:when test="function-available('stbl:adjustColumnWidths')">
						<xsl:copy-of select="stbl:adjustColumnWidths($colspecs)"/>
					</xsl:when>
					<xsl:when test="function-available('xtbl:adjustColumnWidths')">
						<xsl:copy-of select="xtbl:adjustColumnWidths($colspecs)"/>
					</xsl:when>
					<xsl:when test="function-available('ptbl:adjustColumnWidths')">
						<xsl:copy-of select="ptbl:adjustColumnWidths($colspecs)"/>
					</xsl:when>
					<xsl:otherwise>
						<xsl:message terminate="yes">
							<xsl:text>No adjustColumnWidths function available.</xsl:text>
						</xsl:message>
					</xsl:otherwise>
				</xsl:choose>
			</xsl:when>
			<xsl:otherwise>
				<xsl:copy-of select="$colspecs"/>
			</xsl:otherwise>
		</xsl:choose>
		
		<xsl:apply-templates select="thead"/>
		<xsl:apply-templates select="tfoot"/>
		<xsl:apply-templates select="tbody"/>
	</xsl:template>
</xsl:stylesheet>
