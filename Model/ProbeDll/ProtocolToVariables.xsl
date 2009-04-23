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
      <variable name="{@name}" description="{type/@description}" array="{type/@array}" units="{type/@unit}"/>
</xsl:template>
<xsl:template match="event">
      <xsl:if test="@kind='published'"><event name="{@name}" description="{type/@description}"/></xsl:if>
</xsl:template>

</xsl:stylesheet>
