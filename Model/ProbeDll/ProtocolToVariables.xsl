<?xml version="1.0"?>
<xsl:stylesheet version = '1.0' xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

<xsl:output method="xml"/>
<xsl:template match="describecomp">
      <xsl:element name="{class}">
         <variables>
   <xsl:apply-templates select="property"/>
         </variables>
         <events>
   <xsl:apply-templates select="event"/>
         </events>
      </xsl:element>

</xsl:template>

<xsl:template match="property">
	<xsl:if test="@access='read'"><variable name="{@name}" description="{type/@description}" read="T" write="F" kind="{type/@kind}" array="{type/@array}" units="{type/@unit}"/></xsl:if>
	<xsl:if test="@access='write'"><variable name="{@name}" description="{type/@description}" read="F" write="T" kind="{type/@kind}" array="{type/@array}" units="{type/@unit}"/></xsl:if>
	<xsl:if test="@access='both'"><variable name="{@name}" description="{type/@description}" read="T" write="T" kind="{type/@kind}" array="{type/@array}" units="{type/@unit}"/></xsl:if>
  
</xsl:template>
<xsl:template match="event">
      <xsl:if test="@kind='published'"><event name="{@name}" description="{type/@description}"/></xsl:if>
</xsl:template>

</xsl:stylesheet>
