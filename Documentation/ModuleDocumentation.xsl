<?xml version="1.0"?>
<xsl:stylesheet version = '1.0' xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>
<xsl:output method="html"/>

<!-- ============================================================================
     Match the top level node and apply 'variables' template twice, once for
     a table of contents and once for the body of the document.
     ============================================================================ -->
<xsl:template match="/">
   <html>
   <link rel="stylesheet" href="../docs/shared/apsimwebstyle.css" type="text/css"/>
   <body>
   <h2>Module documentation</h2>

   <!-- Create a table of contents -->
   <p>
   <xsl:apply-templates select="//UItype">
      <xsl:with-param name="table-of-contents" select="'yes'"/>
      <xsl:sort select="name(..)"/>
   </xsl:apply-templates>
   </p>

   <xsl:apply-templates select="//UItype">
      <xsl:with-param name="table-of-contents" select="'no'"/>
      <xsl:sort select="@name"/>
   </xsl:apply-templates>

   </body>
   </html>
</xsl:template>

<!-- ============================================================================
     Matches all 'UItype' elements. Called twice, once for a table of contents
     and once for the actual variables
     ============================================================================ -->
<xsl:template match="UItype">
   <xsl:param name="table-of-contents" select="'no'"/>
   <xsl:if test="../Documentation">
      <xsl:choose>
         <xsl:when test="$table-of-contents='yes'">
            <a href="#{name(..)}"><xsl:value-of select="name(..)"/></a><br/><xsl:text>&#160;</xsl:text>
         </xsl:when>

         <xsl:otherwise>
            <H2><a name="{name(..)}"><xsl:value-of select="name(..)"/></a></H2>
            <xsl:apply-templates select="../Documentation"/>

            <xsl:if test="../variables">
              <xsl:choose>
                  <xsl:when test="boolean(../variables/@link) = false">
                     <table>
                     <tr><td><b>Variables available for reporting</b></td></tr>
                     <tr>
                     <td><b>Name</b></td>
                     <td><b>Description</b></td>
                     </tr>
                        <xsl:apply-templates select="../variables">
                           <xsl:sort select="@name"/>
                        </xsl:apply-templates>
                     </table>
                  </xsl:when>
                  <xsl:otherwise>
                     <p><a href="ComponentDescription.xml#{../variables/@link}">Link to variables for outputting</a></p>
                  </xsl:otherwise>
              </xsl:choose>
           </xsl:if>

         </xsl:otherwise>
      </xsl:choose>
   </xsl:if>
</xsl:template>


<!-- ============================================================================
     Matches all 'documentation' elements.
     ============================================================================ -->
<xsl:template match="Documentation">
   <p><a href="{.}"><xsl:value-of select="@name"/></a></p>
</xsl:template>


<!-- ============================================================================
     Matches all 'variable' elements. Simply output variable details.
     ============================================================================ -->
<xsl:template match="variable">
   <tr>
   <td><xsl:value-of select="@name"/></td>
   <td><xsl:value-of select="@description"/></td>
   </tr>
</xsl:template>

</xsl:stylesheet>
