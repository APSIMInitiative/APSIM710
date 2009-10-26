#! 
# PricesUI. Prices for crops. Special treatment for crops that have protein premium
# 
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
   set config(desc,$variable)    [getValue $node desc]
   set config(units,$variable)   [getValue $node units]
   set config(updated,$variable) [getValue $node updated]
   set config(price,$variable)   [getValue $node price]
   set config(protein,$variable) [getValue $node protein]
   if {[llength $config(protein,$variable)] > 1} {
       for {set i 0} {$i < [llength $config(protein,$variable)]} {incr i} {
          set config(price,$variable,$i) [lindex $config(price,$variable) $i]
          set config(protein,$variable,$i) [lindex $config(protein,$variable) $i]
       }
   }
   set config(grain_moisture,$variable) [getValue $node grain_moisture]
   lappend config(variables) $variable
}
$doc delete
foreach {name value} [array get config] {set check($name) $value}

# Lay out the UI
catch {destroy .w}
set w [frame .w]
set row 1
label $w.t0 -text Description
label $w.t1 -text Price
label $w.t2 -text Units
label $w.t3 -text "Grain moisture\ncontent (%)"
label $w.t4 -text "Last Updated"
grid $w.t0 -row 0 -column 1 -padx 3 -sticky n
grid $w.t1 -row 0 -column 2 -padx 3 -sticky n
grid $w.t2 -row 0 -column 3 -padx 3 -sticky n
grid $w.t3 -row 0 -column 4 -padx 3 -sticky n
grid $w.t4 -row 0 -column 5 -padx 3 -sticky n

foreach variable $config(variables) {
   label $w.l$row -text   $config(desc,$variable)
   if {[info exists config(protein,$variable,0)]} {
      set f [frame $w.price$row]
      label $f.h1 -text "Protein (%)"
      label $f.h2 -text "Price (\$/tonne)"
      grid $f.h1 -row 1 -column 1; grid $f.h2 -row 1 -column 2
      set n [llength $config(protein,$variable)]
      for {set i 0} {$i < $n} {incr i} {
          entry $f.pt$i -width 8 -textvariable config(protein,$variable,$i) -vcmd {string is int %P} 
          entry $f.pr$i -width 8 -textvariable config(price,$variable,$i) -vcmd {string is int %P} 
          grid $f.pt$i -row [expr $i+2] -column 1; grid $f.pr$i -row [expr $i+2] -column 2
      }
      label $w.u$row 
   } else {
      entry $w.price$row -width 8 -textvariable config(price,$variable) -vcmd {string is int %P} 
      label $w.u$row -text $config(units,$variable)
   }   
   entry $w.gmc$row -width 5 -textvariable config(grain_moisture,$variable) -vcmd {string is int %P} 
   label $w.up$row -text  $config(updated,$variable)

   grid $w.l$row     -row $row -column 1 -pady 3 -sticky nw -padx 5
   grid $w.price$row -row $row -column 2 -pady 3 -sticky new
   grid $w.u$row     -row $row -column 3 -pady 3 -padx 5 -sticky nw
   grid $w.gmc$row   -row $row -column 4 -pady 3 -padx 5 -sticky nw
   grid $w.up$row    -row $row -column 5 -pady 3 -padx 5 -sticky nw
   incr row
}

# ..Bits to support creating a new price thingy
button $w.r$row -text "Add another" -command "addParameter $w $row"
grid $w.r$row -row $row -column 1 -columnspan 2 -sticky w -pady 5  -padx 5
incr row

proc addParameter {w row} {
   destroy $w.r${row}

   entry $w.r${row}entry -textvariable config(additional)
   grid $w.r${row}entry -row $row -column 1 -sticky e

   bind $w.r${row}entry <Key-Return> "acceptNewParameter $w $row"
}

proc acceptNewParameter {w row} {
   global config check
   destroy $w.r${row}entry

   label $w.r${row}label -text $config(additional)
   entry $w.r${row}entry -textvariable config(price,$config(additional))
   entry $w.r${row}uentry -textvariable config(units,$config(additional))
   label $w.r${row}uplabel -textvariable config(updated,$config(additional))
   lappend config(variables) $config(additional)
   set config(name,$config(additional)) $config(additional)
   set config(updated,$config(additional)) [clock format [clock seconds] -format %d/%m/%Y]
   set config(protein,$config(additional)) ""
   set check(price,$config(additional))    ""
   set check(protein,$config(additional))  ""
   
   grid $w.r${row}label -row $row -column 1 -pady 3 -sticky nw -padx 5
   grid $w.r${row}entry -row $row -column 2 -sticky new
   grid $w.r${row}uentry -row $row -column 3 -sticky nw
   grid $w.r${row}uplabel -row $row -column 4 -sticky nw
   incr row

   unset config(additional)
   button $w.r$row -text "Add another" -command "addParameter $w $row"
   grid $w.r$row -row $row -column 1 -columnspan 2 -sticky w -pady 5  -padx 5
}

#Some handy URLs
set f [frame $w.urls]; set fc 0
foreach {name url} {
"ABARE economics"        "http://www.abareconomics.com/"
"ABARE market monitor"   "http://www.abareconomics.com/publications_html/market_monitor/market_monitor/emarketmonitor.xls"
"ASX Grain futures"      "http://www.asx.com.au/investor/futures/grain/index.htm"
"Chicago Board of Trade" "http://www.cbot.com/"} {
  label $f.f$fc -text $name -background SystemButtonFace -foreground SystemButtonText -fg blue
  grid  $f.f$fc -sticky w
  bind  $f.f$fc <Enter> "%W configure -background SystemHighlight -foreground SystemHighlightText"
  bind  $f.f$fc <Leave> "%W configure -background SystemButtonFace -foreground SystemButtonText -fg blue"
  bind  $f.f$fc <Button-1> "exec cmd.exe /c start $url &"
  incr fc
}
grid $w.urls   -row $row -column 1 -columnspan 5 -sticky w -pady 5  -padx 5

# All done!
incr row
grid rowconf    $w $row -weight 1
grid columnconf $w 5    -weight 1

grid forget .
grid $w -row 0 -column 0 -sticky nwe
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
         foreach attr [list protein price] {
            if {[info exists config($attr,$variable,0)]} {
               set config($attr,$variable) {}
               for {set i 0} {[info exists config($attr,$variable,$i)]} {incr i} {
                  lappend config($attr,$variable) $config($attr,$variable,$i)
               }
            }
         }
         if { ![string equal $check(price,$variable) $config(price,$variable)] } {
            set config(updated,$variable)  [clock format [clock seconds] -format %d/%m/%Y]
         }
         foreach attr [list desc name price protein units updated] { 
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
