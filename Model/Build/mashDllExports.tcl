#! tclsh

# Add aliases to a dll with existing (MS style) stdcall exports (_XXX@yyyy -> XXX) 

set dll [lindex $argv 0]

if {![file exists $dll]} {puts stderr "Usage:\n[info script] <dll>"; exit}

set text [exec dumpbin /exports $dll]

set pastheader 0
puts "LIBRARY [file tail $dll]"
puts "EXPORTS"
foreach line [split $text "\n"] {
   if {[scan $line "%s %s %s %s" ordinal hint RVA longName] == 4} {
      if {$pastheader} {
         if {[scan $longName "_%\[A-Za-z0-9_\]@" name] == 1} {
            puts "  $name = $longName"
         }
      }
      if {"$ordinal" == "ordinal"} {set pastheader 1}
   }
}

