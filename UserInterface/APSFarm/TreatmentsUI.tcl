#! 
# TreatmentsUI. 
trace remove variable XMLDoc read setXML

package require Tk
package require tdom

catch {unset config}
catch {unset check}

proc getValue {id thing} {
   foreach node [$id childNodes] {
      if {[string equal -nocase [$node nodeName] $thing]} {
         return [$node text]
      }
   }
}

## Decode the XML string for this applet into an array
set doc [dom parse $XMLDoc]
set docroot [$doc documentElement]

set config(docroot) [$docroot nodeName]
set config(docrootName) [$docroot getAttribute name]
set config(uiscript) [[$docroot selectNodes //uiscript] text]
set config(category) [[$docroot selectNodes //category] text]
foreach node [$docroot selectNodes //$config(category)] {
   set variable [getValue $node name]; if {$variable == ""} {set variable "undefined"}
   set config(name,$variable)    $variable
   set config(desc,$variable)    [getValue $node desc]
   set config(units,$variable)   [getValue $node units]
   set config(updated,$variable) [getValue $node updated]
   set config(price,$variable)   [getValue $node price]
   lappend config(variables) $variable
}
$doc delete
foreach {name value} [array get config] {set check($name) $value}

# Build the UI
catch {destroy .w}
set w [frame .w]
label $w.cat -text "Category $config(category)"
grid $w.cat -row 0 -column 1 -padx 3  -sticky w

label $w.t0 -text Description
label $w.t1 -text "Apsim Name"
label $w.t2 -text Price
label $w.t3 -text Units
label $w.t4 -text "Last Updated"
grid $w.t0 -row 1 -column 1 -padx 3
grid $w.t1 -row 1 -column 2 -padx 3
grid $w.t2 -row 1 -column 3 -padx 3
grid $w.t3 -row 1 -column 4 -padx 3
grid $w.t4 -row 1 -column 5 -padx 3

set row 2
foreach variable $config(variables) {
   label $w.l$row -text   $config(desc,$variable)
   label $w.name$row -text $config(name,$variable)
   entry $w.price$row -width 8 -textvariable config(price,$variable) -vcmd {string is int %P} 
   label $w.u$row   -text $config(units,$variable)
   label $w.up$row  -text  $config(updated,$variable)

   grid $w.l$row     -row $row -column 1 -pady 1 -sticky nw -padx 2
   grid $w.name$row  -row $row -column 2 -pady 1 -sticky ne -padx 2
   grid $w.price$row -row $row -column 3 -pady 1 -sticky new
   grid $w.u$row     -row $row -column 4 -pady 1 -padx 2 -sticky nw
   grid $w.up$row    -row $row -column 5 -pady 1 -padx 2 -sticky nw
   incr row
}

# ..Bits to support creating a new price thingy
button $w.r$row -text "Add another" -command "addParameter $w $row"
grid $w.r$row -row $row -column 1 -columnspan 2 -sticky w -pady 5  -padx 5

proc addParameter {w row} {
   destroy $w.r${row}

   set config(additionalDesc) ""; set config(additionalName) ""
   entry $w.r${row}Dentry -textvariable config(additionalDesc)
   entry $w.r${row}Aentry -textvariable config(additionalName)
   grid $w.r${row}Dentry -row $row -column 1 -sticky we
   grid $w.r${row}Aentry -row $row -column 2 -sticky we

   bind $w.r${row}Aentry <Key-Return> "acceptNewParameter $w $row"
   bind $w.r${row}Dentry <Key-Return> "acceptNewParameter $w $row"
}

proc acceptNewParameter {w row} {
   global config check

   if {$config(additionalDesc) == "" || $config(additionalName) == ""} {return}

   destroy $w.r${row}Aentry
   destroy $w.r${row}Dentry

   set variable $config(additionalName)
   lappend config(variables) $variable
   set config(desc,$variable) $config(additionalDesc)
   set config(name,$variable) $config(additionalName)
   set config(updated,$variable) [clock format [clock seconds] -format %d/%m/%Y]
   set check(price,$variable)    ""

   label $w.r${row}Dlabel -text $config(additionalDesc)
   label $w.r${row}Nlabel -text $config(additionalName)
   entry $w.r${row}entry -textvariable config(price,$variable)
   entry $w.r${row}uentry -textvariable config(units,$variable)
   label $w.r${row}uplabel -textvariable config(updated,$variable)
   
   grid $w.r${row}Dlabel -row $row -column 1 -pady 1 -sticky nw -padx 3
   grid $w.r${row}Nlabel -row $row -column 2 -pady 1 -sticky ne -padx 3
   grid $w.r${row}entry -row $row  -column 3 -sticky new
   grid $w.r${row}uentry -row $row -column 4 -sticky nw
   grid $w.r${row}uplabel -row $row -column 5 -sticky nw

   incr row
   button $w.r$row -text "Add another" -command "addParameter $w $row"
   grid $w.r$row -row $row -column 1 -columnspan 2 -sticky w -pady 5  -padx 5
}

## All done!
grid rowconf    $w $row -weight 1
grid columnconf $w 5    -weight 1

grid forget .
grid .w -row 0 -column 0 -sticky nwe
grid columnconf . 0 -weight 1
grid rowconf    . 0 -weight 1

# When the UI asks for XMLDoc, recreate the ascii form from the xml tree
trace add variable XMLDoc read setXML
proc setXML {name1 name2 op} {
   global XMLDoc config check
   catch {
      set newDoc [dom createDocument $config(docroot)]
      set root [$newDoc documentElement]
      $root setAttribute name $config(docrootName)
      foreach name [list uiscript category] {
         set new [$newDoc createElement $name]
         $new appendChild [$newDoc createTextNode $config($name)]
         $root appendChild $new
      }

      foreach variable $config(variables) {
         set varNode [$newDoc createElement $config(category)]
         if { ![string equal $check(price,$variable) $config(price,$variable)] } {
            set config(updated,$variable)  [clock format [clock seconds] -format %d/%m/%Y]
         }
         foreach attr [list desc name price units updated] { 
            set new [$newDoc createElement $attr]
            $new appendChild [$newDoc createTextNode $config($attr,$variable)]
            $varNode appendChild $new
         }
         $root appendChild $varNode
      }
   } msg
   if {$msg != ""} {
      tk_messageBox -title "Error" -message $msg -type ok
   } else {
      set XMLDoc [$newDoc asXML]
#      set fp [open c:/tmp/z.xml w]
#      puts $fp [$newDoc asXML]
#      close $fp
      $newDoc delete
   }  
}

