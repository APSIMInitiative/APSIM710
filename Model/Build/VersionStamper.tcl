# Writes build info to apsim.xml

set buildTime [clock format [clock seconds] -format "%d-%b-%Y"]

set fp [open ../../Apsim.xml r]; set text [read -nonewline $fp]; close $fp

# extract the current major and minor version numbers from ApsimTemplate.xml
if ([regexp -line "\<apsim\>.*\</apsim\>$" $text version]) {
  set info [lindex [split $version "<>"] 2]
  set major [lindex [split $info "."] 0]
  set minor [lindex [split $info "."] 1]
  regexp -line "\<buildnumber\>.*\</buildnumber\>$" $text releaseNumber
  set releaseNumber [lindex [split $releaseNumber "<>"] 2]
  puts $releaseNumber
  
# get rid of the leading "r" of the release number
  set releaseNumber [lindex [split $releaseNumber " r"] 1]

  set fp [open VersionInfo.bat w]
  puts $fp "@echo off"
  puts $fp "SET MAJOR_VERSION=$major"
  puts $fp "SET MINOR_VERSION=$minor"
  puts $fp "SET BUILD_NUMBER=$releaseNumber"
  close $fp

  set fp [open VersionInfo.sh w]
  puts $fp "#! /bin/sh"
  puts $fp "export MAJOR_VERSION=$major"
  puts $fp "export MINOR_VERSION=$minor"
  puts $fp "export BUILD_NUMBER=$releaseNumber"
  close $fp

  set fp [open VersionInfo.cs w]
  puts $fp "using System.Reflection;"
#  puts $fp "\[assembly: AssemblyVersion(\"3.0.0.0\")\]"
  puts $fp "\[assembly: AssemblyFileVersion(\"$major.$minor.$releaseNumber.0\")\]" 
  close $fp

  set fp [open VersionInfo.vb w]
  puts $fp "Imports System.Reflection"
#  puts $fp "<Assembly: AssemblyVersion(\"3.0.0.0\")>"
  puts $fp "<Assembly: AssemblyFileVersion(\"$major.$minor.$releaseNumber.0\")>"
  close $fp
}
exit
