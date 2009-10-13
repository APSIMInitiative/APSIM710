#! C:/development/apsim/FarmMachinery/MachineryUI.tcl
trace remove variable XMLDoc read setXML

package require Tk
package require tdom
package require BWidget
catch {destroy .w}
set w [frame .w]

proc getValue {id thing} {
   foreach node [$id childNodes] {
      if {[string equal -nocase [$node nodeName] $thing]} {
         return [$node text]
      }
   }
}

proc getAttr {id thing tag} {
   foreach node [$id childNodes] {
      if {[string equal -nocase [$node nodeName] $thing] &&
          [$node hasAttribute $tag]} {
         return [$node getAttribute $tag]
      }
   }
}

foreach v {name variables what description units row image fuelrate  workrate  hoursperday} {
  catch {unset $v}
}
foreach name [image names] {image delete $name}


## Decode the XML string for this applet. First, remove any trace that may be set
set doc [dom parse $XMLDoc]
set docroot [$doc documentElement]

# We will also need a list of all implements used in the simulation
set gdocroot [[dom parse $GlobalXMLDoc] documentElement]

set node [$docroot selectNodes //category]
if {$node == {}} {tk_messageBox -title "Error" -message "Missing category in XML" -type ok; return}
set category [$node text]

set catroot [$docroot selectNodes //$category]
if {$catroot == {}} {tk_messageBox -title "Error" -message "Missing $category in XML" -type ok; return}

# Deal with the pretty piccie
set variables image
set image [getValue $catroot image]
regsub -all "%apsuite" $image $apsuite imageFile
if {![catch {set img [image create photo -file $imageFile]}]} {
   label $w.img  -image $img
} else  {
   button $w.img -text "Set Image" -command "changeImage $w"
}  

if {[winfo exists .changeImageMenu]} {destroy .changeImageMenu}
menu .changeImageMenu -tearoff 0
.changeImageMenu add command -label "Change Image" -command "changeImage $w"
bind $w.img <3> "tk_popup .changeImageMenu %X %Y"

proc changeImage {w} {
   set types {
       {{GIF Files}  .gif }
       {{JPEG Files} .jpg }
       {{All Files} * }
   }
   set newFile [tk_getOpenFile -filetypes $types -multiple 0 -title "Choose file"]
   if {$newFile != ""} {
      catch {destroy $w.img}
      if {![catch {set img [image create photo -file $newFile]}]} {
         label $w.img  -image $img
         global image
         set image $newFile
      } else  {
         button $w.img -text "Set Image" -command "changeImage $w"
      }  
   }
}

# Set up the entry boxes
set row 1
foreach {what description units tractorOnly} {
    name            "Apsim Name"           ""                                 0 \
    newPrice        "New Price"            "($)"                              0 \
    tradeInValue    "Trade In Value"       "(% of new)"                       0 \
    lifeOfEquipment "Life of Equipment"    "(hrs)"                            0 \
    insuranceRate    "Insurance cost"      "($/ $1000 insured)"               0 \
    repairs          "Repairs & Maintenance" "(% of new value over lifetime)" 0 \
    oil              "Oil & Tyre costs"    "(%age of fuel costs)"             1 \
    loanInterestRate "Loan Interest Rate"  "(%)"                              0 \
    loanDuration     "Loan Duration"       "(years)"                          0 \
    age              "Age at start of simulation" "(hrs)"                     0  } {
      lappend variables $what
      catch {unset $what}
      set $what [getValue $catroot $what]
      label $w.l$row -text $description
      if {$what != "name"} {
         entry $w.e$row -textvariable $what -vcmd {string is int %P} -width 10
      } else {
         entry $w.e$row -textvariable $what -vcmd {string is alpha %P} -width 15
      }  
      label $w.u$row -text $units
      grid $w.l$row -row $row -column 1 -sticky w     -pady 3  
      grid $w.e$row -row $row -column 2 -sticky e     -pady 3  
      grid $w.u$row -row $row -column 3 -sticky w     -pady 3  
      incr row
}

grid $w.img -row 1 -column 4 -sticky n  -pady 3  -rowspan $row
grid columnconf $w 3    -weight 1
grid rowconf    $w $row -weight 1

incr row
if {$category == "tractor"} {
   frame $w.f
   grid $w.f -row $row -column 1 -sticky nw -columnspan 4

   label $w.f.in -text "Implement"
   label $w.f.fr -text "Fuel Rate (lts/hour)"
   label $w.f.wr -text "Work Rate (ha/hour)"
   label $w.f.hr -text "Daily Hours (hours)"
   grid $w.f.in -row 1 -column 1  -pady 3 -padx 5
   grid $w.f.fr -row 1 -column 2  -pady 3 -padx 5 
   grid $w.f.wr -row 1 -column 3  -pady 3 -padx 5
   grid $w.f.hr -row 1 -column 4  -pady 3 -padx 5

   set row 2
   set implements {}

   # Go through all implements in the current simulation and find a fuel/work rate for them.
   foreach node [$gdocroot selectNodes //implement] {
      set implementName [getValue $node name]
      label $w.f.in$row -text $implementName
      set fuelrate($node) {}
      foreach tnode [$catroot childNodes] {
         if {[string equal -nocase [$tnode nodeName] "fuelrate"] && 
             [string equal -nocase [$tnode getAttribute implement] $implementName]} {
            set fuelrate($node) [$tnode text]
         }  
      }
      entry $w.f.fr$row -textvariable fuelrate($node) -width 6

      set workrate($node) {}
      foreach tnode [$catroot childNodes] {
         if {[string equal -nocase [$tnode nodeName] "workrate"] && 
             [string equal -nocase [$tnode getAttribute implement] $implementName]} {
            set workrate($node) [$tnode text]
         }  
      }
      entry $w.f.wr$row -textvariable workrate($node) -width 6

      set hoursperday($node) {}
      foreach tnode [$catroot childNodes] {
         if {[string equal -nocase [$tnode nodeName] "hoursperday"] && 
             [string equal -nocase [$tnode getAttribute implement] $implementName]} {
            set hoursperday($node) [$tnode text]
         }  
      }
      entry $w.f.hr$row -textvariable hoursperday($node) -width 6
   
      lappend implements $node
      grid $w.f.in$row -row $row -column 1 -sticky e -pady 3 -padx 5
      grid $w.f.fr$row -row $row -column 2 -sticky w -pady 3 
      grid $w.f.wr$row -row $row -column 3 -sticky w -pady 3 
      grid $w.f.hr$row -row $row -column 4 -sticky w -pady 3 
      incr row
   }
}

grid rowconf $w 100 -weight 1

grid forget .
grid $w -row 0 -column 0 -sticky nwe
grid columnconf . 0 -weight 1
grid rowconf    . 0 -weight 1

proc setXML {name1 name2 op} {
   global XMLDoc doc docroot catroot category variables
   catch {
     foreach var $variables {
        global $var
        set new [$doc createElement $var]
        $new appendChild [$doc createTextNode [set $var]]
        foreach tnode [$catroot childNodes] {
           if {[string equal -nocase [$tnode nodeName] $var]} {
              $tnode delete
           }
        }
        $catroot appendChild $new
        ;#tk_messageBox -title "Set" -message "var=$var;value=[set $var]" -type ok
     }
     if {$category == "tractor"} {
        global workrate fuelrate hoursperday implements
        foreach node $implements {
           set implementName [getValue $node name]
           set new [$doc createElement workrate]
           $new setAttribute implement $implementName
           $new appendChild [$doc createTextNode $workrate($node)]
           foreach tnode [$catroot childNodes] {
              if {[string equal -nocase [$tnode nodeName] workrate] &&
                  [string equal -nocase [$tnode getAttribute implement] $implementName]} {
                 $tnode delete
              }
           }   
           $catroot appendChild $new

           set new [$doc createElement fuelrate]
           $new setAttribute implement $implementName
           $new appendChild [$doc createTextNode $fuelrate($node)]
           foreach tnode [$catroot childNodes] {
              if {[string equal -nocase [$tnode nodeName] fuelrate] &&
                  [string equal -nocase [$tnode getAttribute implement] $implementName]} {
                 $tnode delete
              }
           }   
           $catroot appendChild $new

           set new [$doc createElement hoursperday]
           $new setAttribute implement $implementName
           $new appendChild [$doc createTextNode $hoursperday($node)]
           foreach tnode [$catroot childNodes] {
              if {[string equal -nocase [$tnode nodeName] hoursperday] &&
                  [string equal -nocase [$tnode getAttribute implement] $implementName]} {
                 $tnode delete
              }
           }   
           $catroot appendChild $new
        }
     }
   } msg
   if {$msg != ""} {
      global errorInfo; tk_messageBox -title "Error" -message "$msg:\n$errorInfo" -type ok
   } else {
      set XMLDoc [$doc asXML]
   }
}
trace add variable XMLDoc read setXML
