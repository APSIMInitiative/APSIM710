# Writes build info to apsim.xml

if {[catch {set msg [exec svn log -q -r BASE ../..]} emsg] } {
  set releaseNumber $emsg
} else {
  set msg [lindex [split $msg "\n"] 1]
  set releaseNumber [lindex [split $msg "|"] 0]
}

set buildTime [clock format [clock seconds] -format "%d-%b-%Y"]

set fp [open ../../ApsimTemplate.xml r]; set text [read -nonewline $fp]; close $fp

regsub -all -line "\<builddate\>.*\</builddate\>$" $text "\<builddate\>$buildTime\</builddate\>" text
regsub -all -line "\<buildnumber\>.*\</buildnumber\>$" $text "\<buildnumber\>$releaseNumber\</buildnumber\>" text

set fp [open ../../Apsim.xml w]
puts -nonewline $fp $text
close $fp

# extract the current major and minor version numbers from ApsimTemplate.xml
if ([regexp -line "\<apsim\>.*\</apsim\>$" $text version]) {
  set info [lindex [split $version "<>"] 2]
  set major [lindex [split $info "."] 0]
  set minor [lindex [split $info "."] 1]
# get rid of the leading "r" of the release number
  set releaseNumber [lindex [split $releaseNumber " r"] 1]
# write out these values so we can access them from the Windows shell  
  puts "$major $minor $releaseNumber"
}

set fp [open VersionInfo.cs w]
puts $fp "using System.Reflection;"
puts $fp "\[assembly: AssemblyVersion(\"$major.$minor.0.0\")\]"
puts $fp "\[assembly: AssemblyFileVersion(\"$major.$minor.$releaseNumber.0\")\]"
close $fp

set fp [open VersionInfo.vb w]
puts $fp "Imports System.Reflection"
puts $fp "<Assembly: AssemblyVersion(\"$major.$minor.0.0\")>"
puts $fp "<Assembly: AssemblyFileVersion(\"$major.$minor.$releaseNumber.0\")>"
close $fp
exit