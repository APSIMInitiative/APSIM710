# Set the APSIM version number
set major 7
set minor 4

# Get the revision number
if {[catch {set msg [exec svn log -q -r BASE ../..]} emsg] } {
  puts $emsg
  set releaseNumber "r0"
} else {
  set msg [lindex [split $msg "\n"] 1]
  set releaseNumber [lindex [split $msg "|"] 0]
}

# get rid of the leading "r" of the release number
set releaseNumber [lindex [split $releaseNumber " r"] 1]

# If Bob is running this then increment the revision number.
# Explanation: Because Bob does a commit at the end of
# a build (causing the revision number to increment by 1), Bob writes the revision number + 1
# to the apsim.xml file in anticipation of the pending commit.
puts $argv
if {$argv == "Increment"} {
    puts "in"
    incr releaseNumber
}

# Get the build time.
set buildTime [clock format [clock seconds] -format "%d-%b-%Y"]

# Write VersionInfo.bat
set fp [open VersionInfo.bat w]
puts $fp "@echo off"
puts $fp "SET MAJOR_VERSION=$major"
puts $fp "SET MINOR_VERSION=$minor"
puts $fp "SET BUILD_NUMBER=$releaseNumber"
close $fp

# Write VersionInfo.sh
set fp [open VersionInfo.sh w]
puts $fp "#! /bin/sh"
puts $fp "export MAJOR_VERSION=$major"
puts $fp "export MINOR_VERSION=$minor"
puts $fp "export BUILD_NUMBER=$releaseNumber"
close $fp

# Write VersionInfo.cs
set fp [open VersionInfo.cs w]
puts $fp "using System.Reflection;"
puts $fp "\[assembly: AssemblyFileVersion(\"$major.$minor.$releaseNumber.0\")\]" 
close $fp

# Write VersionInfo.vb
set fp [open VersionInfo.vb w]
puts $fp "Imports System.Reflection"
puts $fp "<Assembly: AssemblyFileVersion(\"$major.$minor.$releaseNumber.0\")>"
close $fp

# Write VersionInfo.cpp
set fp [open VersionInfo.cpp w]
puts $fp "std::string EXPORT getApsimVersion(void) {return \"$major.$minor\";}"
puts $fp "std::string EXPORT getApsimBuildNumber(void) {return \"r$releaseNumber\";}"
close $fp  

