#! C:/development/apsim/FarmMachinery/CashBookUI.tcl
trace remove variable XMLDoc read setXML

package require Tk
package require BWidget
package require tdom

catch {destroy .w}
set w [frame .w]

## Decode the XML string for this applet
set doc [dom parse $XMLDoc]
set docroot [$doc documentElement]

set balance [[$docroot selectNodes //balance] text]
set outputfilename [[$docroot selectNodes //outputfilename] text]
set summaryfilename [[$docroot selectNodes //summaryfilename] text]

label $w.of  -text "Filename"
entry $w.ofe -width 30 -textvariable outputfilename
button $w.ofb -text Browse -command chooseOutputFilename

label $w.sf  -text "Summary Filename"
entry $w.sfe -width 30 -textvariable summaryfilename
button $w.sfb -text Browse -command chooseSummaryFilename

label $w.ob  -text "Opening Balance"
entry $w.obe -width 12 -justify right -validate key -vcmd {string is int %P} -textvariable balance
label $w.obu  -text "\$"

grid $w.of  -row 1 -column 1 -sticky w   -padx 5 -pady 15
grid $w.ofe -row 1 -column 2 -sticky ew  -padx 5 -columnspan 2
grid $w.ofb -row 1 -column 4             -padx 5

grid $w.sf  -row 2 -column 1 -sticky w   -padx 5 -pady 15
grid $w.sfe -row 2 -column 2 -sticky ew  -padx 5 -columnspan 2
grid $w.sfb -row 2 -column 4             -padx 5

grid $w.ob  -row 3 -column 1 -sticky w   -padx 5 -pady 5
grid $w.obe -row 3 -column 2 -sticky ew  -padx 5
grid $w.obu -row 3 -column 3 -sticky w   -padx 5

grid rowconf    $w 5 -weight 1
grid columnconf $w 5 -weight 1

proc chooseOutputFilename {} {
   global outputfilename
   set types {{{Comma Separated Variable Files}   {.csv}}
              {{All Files}                        *}}
   set filename [tk_getSaveFile -filetypes $types -initialfile $outputfilename]
   if {$filename != ""} {
      set outputfilename $filename
   }
}

proc chooseSummaryFilename {} {
   global summaryfilename
   set types {{{Comma Separated Variable Files}   {.csv}}
              {{All Files}                        *}}
   set filename [tk_getSaveFile -filetypes $types -initialfile $outputfilename]
   if {$filename != ""} {
      set summaryfilename $filename
   }
}

grid forget .
grid $w -row 0 -column 0 -sticky nwe
grid columnconf . 0 -weight 1
grid rowconf    . 0 -weight 1

trace add variable XMLDoc read setXML
proc setXML {name1 name2 op} {
   global XMLDoc doc docroot balance outputfilename summaryfilename
   catch {
     foreach var {balance outputfilename summaryfilename} {
        set new [$doc createElement $var]
        $new appendChild [$doc createTextNode [set $var]]
        set old [$docroot selectNodes //$var] 
        [$old parentNode] appendChild $new
        $old delete
     }
   } msg
   if {$msg != ""} {
     tk_messageBox -title "Error" -message $msg -type ok
   } else {
     set XMLDoc [$doc asXML]
   }
}

