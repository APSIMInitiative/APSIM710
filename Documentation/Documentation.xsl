<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:template match="/">
   <html>
   <link rel="stylesheet" href="ApsimWebStyle.css" type="text/css"/>
   <body>
   <h1>APSIM Documentation</h1>
   <xsl:apply-templates/>
   </body>
   </html>
</xsl:template>

<xsl:template match="Documentation">
   <p>
   <a href="{.}"><xsl:value-of select="@name"/></a>
   </p>

</xsl:template>


</xsl:stylesheet>

