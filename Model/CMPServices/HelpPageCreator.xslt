<?xml version="1.0" encoding="UTF-8" ?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output encoding="utf-8" method="html"/>
    
    <!-- the length of the assmbly name; is added to each type and member name; we will cut it off -->
    <xsl:variable name="AssemblyNameLength" select="string-length(//assembly/name)"/>

    <!-- assembly: build the help table with the title of the assembly -->
    <xsl:template match="assembly">
        <LINK REL="STYLESHEET" TYPE="TEXT/CSS" HREF="HelpStyleSheet.css"/>
        <TABLE CLASS="AsemblyTable" CELLPADDING="2" CELLSPACING="0">
            <TR CLASS="AssemblyHeader">
                <TD COLSPAN="3">
                    <xsl:text>Assembly </xsl:text>
                    <FONT CLASS="Title"><xsl:value-of select="name"/></FONT>
                </TD>
            </TR>
            <xsl:apply-templates select="//member[contains(@name,'T:')]"/>
        </TABLE>
    </xsl:template>

    <!-- process all types listed in the XML file -->
    <xsl:template match="//member[contains(@name,'T:')]">

        <!-- the value of the currently processed type; without the assembly name and member identifier -->
        <xsl:variable name="MemberValue" select="substring(@name, $AssemblyNameLength + 4, string-length(@name) - $AssemblyNameLength - 3)"/>

        <!-- get the member identifier and assembly name which is before the member name -->
        <xsl:variable name="IdentifierAndAssemblyName" select="substring-before(@name, $MemberValue)"/>
        
        <!-- get the member filter; which queries all members of the currently processed type -->
        <xsl:variable name="MemberFilter" select="concat(concat(substring($IdentifierAndAssemblyName, 3, string-length($IdentifierAndAssemblyName) - 2), $MemberValue), '.')"/>

        <TR CLASS="AssemblyBody">
            <TD>
                <xsl:text>  </xsl:text>
            </TD>
            <TD>
                <TABLE CLASS="TypeTable" CELLPADDING="2" CELLSPACING="0">
                    <TR CLASS="TypeHeader">
                        <TD COLSPAN="2">
                            <xsl:text>Type </xsl:text>
                            <FONT CLASS="Title"><xsl:value-of select="$MemberValue"/></FONT>

                            <!-- output the type description if present -->
                            <xsl:if test="string-length(summary) &gt; 0">
                                <xsl:text> - </xsl:text>
                                <xsl:value-of select="summary"/>
                            </xsl:if>
                         </TD>
                    </TR>

                    <!-- call the template to process the type members and pass along all the members -->
                    <xsl:call-template name="ProcessTypeMembers">
                        <xsl:with-param name="Members" select="//member[contains(@name, $MemberFilter)]"/>
                        <xsl:with-param name="TypeName" select="concat($MemberValue, '.')"/>
                    </xsl:call-template>
                </TABLE>
            </TD>
            <TD>
                <xsl:text>  </xsl:text>
            </TD>
        </TR>
    </xsl:template>

    <!-- process all members of the currently processed type; gets passed along all members -->
    <xsl:template name="ProcessTypeMembers">
        <xsl:param name="Members"/>
        <xsl:param name="TypeName"/>
        
        <!-- loop through all the members -->
        <xsl:for-each select="$Members">

            <!-- the value of the currently processed type; without the assembly name and member identifier -->
            <xsl:variable name="MemberValue" select="substring(@name, $AssemblyNameLength + 4, string-length(@name) - $AssemblyNameLength - 3)"/>

            <xsl:choose>

                <!-- we found a constructor -->
                <xsl:when test="contains(@name,'#ctor')">
                    <TR CLASS="TypeBody">
                        <TD nowrap="nowrap">
                            <FONT CLASS="Text">
                                <xsl:text>Constructor </xsl:text>
                            </FONT>
                        </TD>
                        <TD WIDTH="100%">
                            <FONT CLASS="Member">
                                <!-- if the type name is included in the member name then we exclude it -->
                                <xsl:value-of select="substring($TypeName, 1, string-length($TypeName) - 1)"/>
                                <xsl:text>()</xsl:text>
                            </FONT>

                            <!-- if we have summary to the constructor then show them -->
                            <xsl:if test="string-length(summary) &gt; 0">
                                - <xsl:value-of select="summary"/>
                            </xsl:if>
                            
                            <!-- if we have parameters then list them -->
                            <xsl:if test="count(param) &gt; 0">
                                <BR/>
                                <TABLE CLASS="ParamTable" CELLPADDING="2" CELLSPACING="0">
                                    <xsl:for-each select="param">
                                        <TR CLASS="ParamBody">
                                            <TD >
                                                <FONT CLASS="Param">
                                                    <xsl:value-of select="@name"/>
                                                </FONT>
                                            </TD>
                                            <TD>
                                                <xsl:value-of select="."/>
                                            </TD>
                                        </TR>
                                    </xsl:for-each>
                                </TABLE>
                                
                                <!-- if we have remarks section to the method then show them -->
                                <xsl:if test="string-length(remarks) &gt; 0">
                                   <FONT CLASS="Remarks">
                                      <xsl:value-of select="remarks"/>
                                   </FONT>   
                                </xsl:if>
                                
                            </xsl:if>                            
                        </TD>
                    </TR>
                </xsl:when>

                <!-- we found a member -->
                <xsl:when test="contains(@name,'M:')">
                    <TR CLASS="TypeBody">
                        <TD nowrap="nowrap">
                            <FONT CLASS="Text">
                                <xsl:if test="count(permission) &gt; 0">
                                   <xsl:value-of select="permission"/>
                                   <br><xsl:text></xsl:text></br>
                                </xsl:if>       
                                <xsl:text>Method </xsl:text>
                            </FONT>
                        </TD>
                        <TD WIDTH="100%">
                            <FONT CLASS="Member">
                            
                                <!-- if the type name is included in the member name then we exclude it -->
                                <xsl:if test="contains($MemberValue, $TypeName)">
                                    <xsl:variable name="MethodNameWithArguments" select="substring-after($MemberValue,$TypeName)"/>

                                    <!-- if we have parameters then cut them off -->
                                    <xsl:if test="contains($MethodNameWithArguments,'(')">
                                       <xsl:value-of select="substring-before($MethodNameWithArguments,'(')"/>
                                    </xsl:if>

                                    <!-- for the case where we have no parameters -->
                                    <xsl:if test="contains($MethodNameWithArguments,'(') = false">
                                       <xsl:value-of select="$MethodNameWithArguments"/>
                                    </xsl:if>

                                    <xsl:text>()</xsl:text>
                                </xsl:if>
                                
                                <xsl:if test="contains($MemberValue,$TypeName) = false">
                                    <!-- if we have parameters then cut them off -->
                                    <xsl:if test="contains($MemberValue,'(')">
                                        <xsl:value-of select="substring-before($MemberValue,'(')"/>
                                    </xsl:if>

                                    <!-- for the case where we have no parameters -->
                                    <xsl:if test="contains($MemberValue,'(') = false">
                                        <xsl:value-of select="$MemberValue"/>
                                    </xsl:if>

                                    <xsl:text>()</xsl:text>
                                </xsl:if>
                            </FONT>

                            <!-- if we have summary to the method then show them -->
                            <xsl:if test="string-length(summary) &gt; 0">
                                - <xsl:value-of select="summary"/>
                            </xsl:if>

                            <!-- if we have parameters or return values then list them -->
                            <xsl:if test="(count(param) &gt; 0) or (count(returns) &gt; 0)">
                                <BR/>
                                <TABLE CLASS="ParamTable" CELLPADDING="2" CELLSPACING="0">
                                    <xsl:for-each select="param">
                                        <TR CLASS="ParamBody">
                                            <TD >
                                                <FONT CLASS="Param">
                                                    <xsl:value-of select="@name"/>
                                                </FONT>
                                            </TD>
                                            <TD>
                                                <xsl:value-of select="."/>
                                            </TD>
                                        </TR>
                                    </xsl:for-each>
                                    
                                    <!-- check if we have a return value for the method -->
                                    <xsl:if test="count(returns) &gt; 0">
                                        <TR CLASS="ParamBody">
                                            <TD WIDTH="10%">
                                                <FONT CLASS="Param">
                                                    <b><xsl:text>Returns</xsl:text></b>
                                                </FONT>
                                            </TD>
                                            <TD>
                                                <xsl:value-of select="returns"/>
                                            </TD>
                                        </TR>
                                    </xsl:if>
                                </TABLE>
                                
                                <!-- if we have remarks section to the method then show them -->
                                <xsl:if test="string-length(remarks) &gt; 0">
                                   <FONT CLASS="Remarks">
                                      <xsl:value-of select="remarks"/>
                                   </FONT>   
                                </xsl:if>
 
                            </xsl:if>
                        </TD>
                    </TR>
                </xsl:when>

                <!-- we found a field -->
                <xsl:when test="contains(@name,'F:')">
                    <TR CLASS="TypeBody">
                        <TD nowrap="nowrap">
                            <xsl:if test="count(permission) &gt; 0">
                               <xsl:value-of select="permission"/>
                               <xsl:text> </xsl:text>
                            </xsl:if>       
                            <FONT CLASS="Text">
                                <xsl:text>Field </xsl:text>
                            </FONT>
                        </TD>
                        <TD WIDTH="100%">
                            <FONT CLASS="Member">
                                <!-- if the type name is included in the member name then we exclude it -->
                                <xsl:if test="contains($MemberValue, $TypeName)">
                                    <xsl:value-of select="substring-after($MemberValue,$TypeName)"/>
                                </xsl:if>
                                <xsl:if test="contains($MemberValue,$TypeName) = false">
                                    <xsl:value-of select="$MemberValue"/>
                                </xsl:if>
                            </FONT>

                            <!-- if we have summary remarks to the field then show them -->
                            <xsl:if test="string-length(summary) &gt; 0">
                                - <xsl:value-of select="summary"/>
                            </xsl:if>
                        </TD>
                    </TR>
                </xsl:when>

                <!-- we found a property -->
                <xsl:when test="contains(@name,'P:')">
                    <TR CLASS="TypeBody">
                        <TD nowrap="nowrap">
                            <xsl:if test="count(permission) &gt; 0">
                               <xsl:value-of select="permission"/>
                               <xsl:text> </xsl:text>
                            </xsl:if>       
                            <FONT CLASS="Text">
                                <xsl:text>Property </xsl:text>
                            </FONT>
                        </TD>
                        <TD WIDTH="100%">
                            <FONT CLASS="Member">
                                <!-- if the type name is included in the member name then we exclude it -->
                                <xsl:if test="contains($MemberValue, $TypeName)">
                                    <xsl:value-of select="substring-after($MemberValue,$TypeName)"/>
                                </xsl:if>
                                <xsl:if test="contains($MemberValue,$TypeName) = false">
                                    <xsl:value-of select="$MemberValue"/>
                                </xsl:if>
                            </FONT>

                            <!-- if we have remarks to the constructor then show them -->
                            <xsl:if test="string-length(summary) &gt; 0">
                                - <xsl:value-of select="summary"/>
                            </xsl:if>
                        </TD>
                    </TR>
                </xsl:when>

                <!-- we found the type itself; which we ignore -->
                <xsl:when test="contains(@name,'T:')">

                </xsl:when>

                <!-- we found a unknown member identifier -->
                <xsl:otherwise>
                    <TR CLASS="TypeBody">
                        <TD nowrap="nowrap">
                            <FONT CLASS="Text">
                                <xsl:text>Unknown </xsl:text>
                            </FONT>
                        </TD>
                        <TD WIDTH="100%">
                            <FONT CLASS="Member">
                                <!-- if the type name is included in the member name then we exclude it -->
                                <xsl:if test="contains($MemberValue, $TypeName)">
                                    <xsl:value-of select="substring-after($MemberValue,$TypeName)"/>
                                </xsl:if>
                                <xsl:if test="contains($MemberValue,$TypeName) = false">
                                    <xsl:value-of select="$MemberValue"/>
                                </xsl:if>
                            </FONT>

                            <!-- if we have remarks to the constructor then show them -->
                            <xsl:if test="string-length(summary) &gt; 0">
                                - <xsl:value-of select="summary"/>
                            </xsl:if>
                        </TD>
                    </TR>
                </xsl:otherwise>
            </xsl:choose>
        </xsl:for-each>
    </xsl:template>
    
    <!-- root node: calls the assembly match which then calls the members match -->
    <xsl:template match="/">
        <xsl:apply-templates select="//assembly"/>
    </xsl:template>

</xsl:stylesheet>
