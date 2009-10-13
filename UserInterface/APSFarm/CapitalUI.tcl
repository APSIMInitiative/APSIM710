#! C:/development/apsim/Economics/OverheadsUI.tcl
trace remove variable XMLDoc read setXML

package require Tk
package require BWidget
package require tdom
catch {destroy .w}
set w [frame .w]

proc getValue {id thing} {
   foreach node [$id childNodes] {
      if {[string equal -nocase [$node nodeName] $thing]} {
         return [$node text]
      }
   }
}

foreach v {name desc amount units} {
  catch {unset $v}
}

## Decode the XML string for this applet
set doc [dom parse $XMLDoc]
set docroot [$doc documentElement]
;#set gdocroot [[dom parse $GlobalXMLDoc] documentElement]


label $w.title -text "Initial Capital Assets"
grid $w.title  -row 1 -column 1 -sticky w  -padx 5 ;#-columnspan 3

set node [$docroot selectNodes //category]
if {$node == {}} {tk_messageBox -title "Error" -message "Missing category in XML" -type ok; return}
set category [$node text]
set node [lindex [$docroot selectNodes //$category] 0]

set names {}
set row 2
foreach {name description units} { \
    ivalue       "Loan Value of Initial Investment" "\$" \
    loanRate     "Loan repayment rate"              "%" \
    loanDuration "Loan duration"                    "Years"} {

   lappend names $name
   set $name [getValue $node $name]
  
   label $w.l$name -text $description
   entry $w.e$name -textvariable $name -width 10 -vcmd {string is int %P} -justify right
   label $w.u$name -text $units

   grid $w.l$name  -row $row -column 1 -sticky w  -pady 3
   grid $w.e$name  -row $row -column 2 -sticky ew -pady 3
   grid $w.u$name  -row $row -column 3 -sticky w  -pady 3
   incr row
}

grid columnconf $w 4 -weight 1
grid rowconf    $w $row -weight 1

grid forget .
grid $w -row 0 -column 0 -sticky nwe
grid columnconf . 0 -weight 1
grid rowconf    . 0 -weight 1


proc setXML {name1 name2 op} {
   global XMLDoc doc docroot category names 
   set catroot [lindex [$docroot selectNodes //$category] 0]
   catch {
      foreach name $names {
         global $name
         set new [$doc createElement $name]
         $new appendChild [$doc createTextNode [set $name]]
         foreach tnode [$catroot childNodes] {
            if {[string equal -nocase [$tnode nodeName] $name]} {
               $tnode delete
            }
         }
         $catroot appendChild $new
      }
   } msg
   if {$msg != ""} {
      global errorInfo; 
      tk_messageBox -title "Error" -message "$msg:\n$errorInfo" -type ok
   } else {
      set XMLDoc [$doc asXML]
   }
}
trace add variable XMLDoc read setXML
