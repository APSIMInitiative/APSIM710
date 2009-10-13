#!
# Annual farm overheads UI

trace remove variable XMLDoc read setXML

package require Tk
package require BWidget
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
   set config(description,$variable)    [getValue $node description]
   set config(value,$variable)   [getValue $node value]
   set config(updated,$variable) [getValue $node updated]
   lappend config(variables) $variable
}
$doc delete
foreach {name value} [array get config] {set check($name) $value}

# Lay out the UI
catch {destroy .w}
set w [frame .w]

label $w.title -text "Annual Farm Overhead expenses"
grid $w.title  -row 1 -column 1 -sticky w  -padx 5 -columnspan 3

set row 2
foreach variable $config(variables) {
  label $w.l$row -text $config(description,$variable)
  entry $w.e$row -textvariable config(value,$variable) -width 10 -vcmd {string is int %P} -justify right

  grid $w.l$row  -row $row -column 1 -sticky w  -padx 5
  grid $w.e$row  -row $row -column 2 -sticky ew -padx 5
  trace add variable config(value,$variable) write updateSum
  incr row
}

label $w.tsum  -text "Total"
label $w.sum  -textvariable config(sum)
grid  $w.tsum -row $row -column 1 -sticky e -padx 5 -sticky e
grid  $w.sum  -row $row -column 2 -sticky ew -padx 5 -sticky e
incr row

proc updateSum {a b c} {
   global config
   set config(sum)  "??"
   catch {
     set sum 0.0
     foreach variable $config(variables) {
        set sum [expr $sum + $config(value,$variable)]
     }
     set config(sum) "\$[format %.02f $sum]"
   }
}

# ..Bits to support creating a new price thingy
button $w.r$row -text "Add another" -command "addParameter $w $row"
grid $w.r$row -row $row -column 1 -columnspan 2 -sticky w -pady 5  -padx 5
incr row

proc addParameter {w row} {
   destroy $w.r${row}

   entry $w.r${row}entry -textvariable config(additional)
   grid $w.r${row}entry -row $row -column 1 -sticky ew
   bind $w.r${row}entry <Key-Return> "acceptNewParameter $w $row"
}

proc acceptNewParameter {w row} {
   global config check
   destroy $w.r${row}entry

   set variable v$row
   set config(name,$variable)           $variable
   set config(description,$variable)    $config(additional)
   set config(value,$variable)          "0"
   set check(value,$variable)           ""
   lappend config(variables) $variable

   label $w.r${row}label -text $config(description,$variable)
   entry $w.r${row}entry -textvariable config(value,$variable) -justify right
   trace add variable config(value,$variable) write updateSum

   grid $w.r${row}label -row $row -column 1 -padx 5 -sticky nw
   grid $w.r${row}entry -row $row -column 2 -padx 5 -sticky ew 
   grid  $w.tsum -row [expr $row+1] -column 1 -sticky e -padx 5 -sticky e
   grid  $w.sum  -row [expr $row+1] -column 2 -sticky ew -padx 5 -sticky e

   unset config(additional)
   set row [expr $row+2]
   button $w.r$row -text "Add another" -command "addParameter $w $row"
   grid $w.r$row -row $row -column 1 -columnspan 2 -sticky w -pady 5  -padx 5
}

updateSum junk junk junk

grid columnconf $w 3 -weight 1
grid rowconf    $w $row -weight 1

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
         if { ![string equal $check(value,$variable) $config(value,$variable)] } {
            set config(updated,$variable)  [clock format [clock seconds] -format %d/%m/%Y]
         }
         foreach attr [list name value description updated] { 
            set new [$newDoc createElement $attr]
            $new appendChild [$newDoc createTextNode $config($attr,$variable)]
            $varNode appendChild $new
         }
         $root appendChild $varNode
      }
   } msg
   if {$msg != ""} {
      global errorInfo; tk_messageBox -title "Error" -message "$msg:\n$errorInfo" -type ok
   } else {
     set XMLDoc [$newDoc asXML]
#      set fp [open c:/tmp/z.xml w]
#      puts $fp [$newDoc asXML]
#      close $fp
     $newDoc delete
   }  
}
trace add variable XMLDoc read setXML

grid forget .
grid $w -row 0 -column 0 -sticky nwe
grid columnconf . 0 -weight 1
grid rowconf    . 0 -weight 1
