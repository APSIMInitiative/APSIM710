# Simplified from VersionStamper
# All we're trying to do is get the current version and build numbers

if {[catch {set msg [exec svn log -q -r BASE ../..]} emsg] } {
  set releaseNumber $emsg
} else {
  set msg [lindex [split $msg "\n"] 1]
  set releaseNumber [lindex [split $msg "|"] 0]
}

set fp [open ../../ApsimTemplate.xml r]; set text [read -nonewline $fp]; close $fp

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

exit