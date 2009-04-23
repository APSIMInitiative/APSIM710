<?xml version="1.0"?>
<xsl:stylesheet version = '1.0' xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>
<xsl:output method="html"/>

<!-- ============================================================================
     Match the top level node and setup the document.
     ============================================================================ -->
<xsl:template match="/">
   <html>
   <link rel="stylesheet" href="../ApsimWebStyle.css" type="text/css"/>
   <body>
   <xsl:apply-templates select="//variables"/>
   <xsl:apply-templates select="//events"/>
   </body>
   </html>
</xsl:template>


<!-- ============================================================================
     Matches all 'variables' elements.
     ============================================================================ -->
<xsl:template match="variables">
   <h2>Variables owned by <xsl:value-of select="name(..)"/></h2>
   <table>
   <tr>
   <td><b>Name</b></td>
   <td><b>Description</b></td>
   <td><b>Units</b></td>
   </tr>

   <xsl:apply-templates select="variable">
      <xsl:sort select="@name"/>
   </xsl:apply-templates>
   </table>
</xsl:template>


<!-- ============================================================================
     Matches all 'variable' elements. Simply output variable details.
     ============================================================================ -->
<xsl:template match="variable">
   <tr>
   <td><xsl:value-of select="@name"/></td>
   <td><xsl:value-of select="@description"/></td>
   <td><xsl:value-of select="@units"/></td>
   </tr>
</xsl:template>


<!-- ============================================================================
     Matches all 'events' elements.
     ============================================================================ -->
<xsl:template match="events">
   <h2>Events published by <xsl:value-of select="name(..)"/></h2>
   <table>
   <tr>
   <td><b>Name</b></td>
   <td><b>Description</b></td>
   </tr>

   <xsl:apply-templates select="event">
      <xsl:sort select="@name"/>
   </xsl:apply-templates>
   </table>
</xsl:template>


<!-- ============================================================================
     Matches all 'variable' elements. Simply output variable details.
     ============================================================================ -->
<xsl:template match="event">
   <tr>
   <td><xsl:value-of select="@name"/></td>
   </tr>
</xsl:template>


</xsl:stylesheet>
