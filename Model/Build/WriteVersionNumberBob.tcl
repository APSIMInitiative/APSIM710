# Writes build info to apsim.xml - Bob style 
# Explanation: Because Bob does a commit at the end of
# a build (causing the revision number to increment by 1), Bob writes the revision number + 1
# to the apsim.xml file in anticipation of the pending commit.

if {[catch {set msg [exec svn log -q -r BASE ../..]} emsg] } {
  puts $emsg
  set releaseNumber "r0"
} else {
  set msg [lindex [split $msg "\n"] 1]
  set releaseNumber [string range [lindex [split $msg "|"] 0] 1 end]
  incr releaseNumber 
  set releaseNumber r$releaseNumber
}

set buildTime [clock format [clock seconds] -format "%d-%b-%Y"]

set fp [open ../../ApsimTemplate.xml r]; set text [read -nonewline $fp]; close $fp

regsub -all -line "\<builddate\>.*\</builddate\>$" $text "\<builddate\>$buildTime\</builddate\>" text
regsub -all -line "\<buildnumber\>.*\</buildnumber\>$" $text "\<buildnumber\>$releaseNumber\</buildnumber\>" text

set fp [open ../../Apsim.xml w]
puts -nonewline $fp $text
close $fp

exit
