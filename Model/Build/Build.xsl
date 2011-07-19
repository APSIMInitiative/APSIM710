<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

   <xsl:template match="/">
      <html>
         <body>
            <xsl:apply-templates/>
         </body>
      </html>
   </xsl:template>

   <xsl:template match="Folder">
      <table border="0">
         <tr>
            <xsl:call-template name="PrintFolder">
               <xsl:with-param name="Node" select="." />
            </xsl:call-template>
         </tr>
         <tr>
            <td/>
            <td>
               <xsl:apply-templates/>
            </td>
         </tr>
      </table>
   </xsl:template>


   <xsl:template match="StartTime">
   </xsl:template>
   
   <xsl:template match="Job">
      <xsl:choose>
         <xsl:when test="@status='Pass'">
            <img src="check2.png"/>
         </xsl:when>
         <xsl:when test="@status='Fail'">
            <img src="delete2.png"/>
         </xsl:when>
         <xsl:otherwise>
            <img src="unknown.png"/>
         </xsl:otherwise>
      </xsl:choose>
      <img src="document.png" />
      <xsl:value-of select="@name"/>(<xsl:value-of select="@ElapsedTime"/> sec)
      <xsl:if test="ExitCode!=0">
         <p>
         <textarea cols="80" rows="10">StdErr: <xsl:value-of select="StdErr"/>
StdOut: <xsl:value-of select="StdOut"/></textarea>
         </p>
      </xsl:if>
      <br/> 
   </xsl:template>



   <xsl:template name="PrintFolder">
      <xsl:param name="Node" />
      <td>
         <xsl:choose>
            <xsl:when test="$Node/@status='Pass'">
               <img src="check2.png"/>
            </xsl:when>
            <xsl:when test="$Node/@status='Fail'">
               <img src="delete2.png"/>
            </xsl:when>
            <xsl:otherwise>
               <img src="unknown.png"/>
            </xsl:otherwise>
         </xsl:choose>
      </td>
      <td>
         <img src="folder_closed.png" />
         <b>
         <xsl:value-of select="$Node/@name"/> (<xsl:value-of select="$Node/@ElapsedTime"/> sec)</b>
      </td>
   </xsl:template>


</xsl:stylesheet>

