# Writes build info to apsim.xml

catch {set msg [exec svn log -q -r HEAD ./]} msg
set msg [lindex [split $msg "\n"] 1]

set fp [open UnitTest.tmp r]; set text [read -nonewline $fp]; close $fp

regsub -all -line " time: .*$" $text "" text
regsub -all -line " Duration: .*$" $text "" text

set fp [open UnitTest.out w]
puts -nonewline $fp $text
close $fp

exit