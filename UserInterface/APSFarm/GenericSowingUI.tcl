#! Sowing UI without detailed knowledge of crop args
#
trace remove variable XMLDoc read setXML

package require Tk

catch {unset config}

package require BWidget
package require tdom
package require Img

# Data support procedures

# Find a list of states that the management UI knows about
proc findStates {} {
   global config
   set knownStates {}

   foreach node [$config(gdocroot) selectNodes //node] {
      set name [getValue $node name]
      if {$name != "" && [lsearch $knownStates $name] < 0} {
         lappend knownStates $name
      }   
   }
   return $knownStates
}

proc findMachinery {} {
   global config
   foreach thing [list tractor implement] {
      set config($thing) "none"
      foreach node [$config(gdocroot) selectNodes //$thing] {
         set name [getValue $node name]
         if {$name != "" && [lsearch $config($thing) $name] < 0} {
            lappend config($thing) $name
         }   
      }
   }
}

# Pull together fertiliser information
proc findFertilisers {} {
   global config apsuite
   set config(fertilisers) {}
   
   # Fertiliser names from module
   set fp [open $apsuite/Model/Fertiliser.xml r]
   set doc      [dom parse [read $fp]]
   close $fp
   set docroot  [$doc documentElement]
   foreach node [$docroot childNodes] {
     set name [$node nodeName]
     foreach what {full_name components fraction} {
        set config($name,$what) [getValue $node $what]
     }     
     lappend config(fertilisers) $name
   }
   $doc delete

   # Fertiliser costs from economics
   foreach node [$config(gdocroot) selectNodes //fertilisercost] {
     set name [getValue $node name] 
     if {[lsearch $config(fertilisers) $name] >= 0} {
        set config($name,fert_cost) [getValue $node price]
     } else {
        set config($name,fert_cost) "<unknown>"
     }
   }

}

# Return a list of crops that are plugged into this simulation
proc findCrops {} {
   global config
   set result {}
   set knownCrops {}
   foreach node [[$config(typesDoc) documentElement] selectNodes Type/MetaData/IsCrop ] {
      if {[string equal -nocase [$node text] "yes"]} {
          lappend knownCrops [[[$node parent] parent] getAttribute name]
      }
   }

   foreach crop $knownCrops {
      if {[llength [$config(gdocroot) selectNodes //$crop]] > 0} {
          lappend result $crop
      }
   }
   return $result
}

# Return a list of cultivars for this crop
proc getCultivars {crop} {
   global config

   set cvs {}
   foreach node [[$config(typesDoc) documentElement] selectNodes Type\[@name='$crop'\]/Model ] {
      foreach child [$node childNodes] {
        catch {
           if {[$child getAttribute cultivar] == "yes"} {
                 set name [$child nodeName]
                 if {[lsearch $cvs $name] < 0} {lappend cvs $name}
           }
        }
      }
   }
   if {$cvs == {}} {tk_messageBox -title "Error" -message "No cultivars found for $crop" -type ok}
   return $cvs
}

# return the filename of a photo of this crop
proc getImage {crop}     { 
   global apsuite config

   foreach node [[$config(typesDoc) documentElement] selectNodes Type\[@name='$crop'\]/MetaData ] {
      foreach child [$node childNodes] {
             if {[string equal -nocase [$child nodeName] "Image"]} {
                 set name [$child text]
                 regsub {%apsuite} $name $apsuite name
                 regsub {%apsim%} $name $apsuite name
                 return $name
             }
      }
   }

   return ""
}

# Get the value of one of our parameters
proc getValue {id thing} {
   foreach node [$id childNodes] {
      if {[string equal -nocase [$node nodeName] $thing]} {
         return [$node text]
      }
   }
   return ""
}

# Get the values of one of our parameters
proc getValues {id thing} {
   set result {}
   foreach node [$id childNodes] {
      if {[string equal -nocase [$node nodeName] $thing]} {
         lappend result [$node text]
      }
   }
   return $result
}


proc addVariable {name} {
   global config
   if {![info exists config(variables)]} {set config(variables) $name}
   if {[lsearch $config(variables) $name] < 0} {lappend config(variables) $name}
}

##########UI procedures
proc CropUI  {w} {
   global config

   label $w.l1 -text "State"
   ComboBox $w.s -editable 1 -textvariable config(state) \
                   -values $config(states) \
                   -helptext "The state (node) these rules allow transitions to" \
                   -width 10
   message $w.m1 -text {Rotations consist of states and transitions 
between states. This component provides both rules and actions; 
rules that represent the feasibility of changing to a state, and 
actions that are taken when making a transition to a state. } -width 450

   label $w.l2 -text "Crop"
   ComboBox $w.c -editable 1 -textvariable config(crop) \
                   -values $config(crops) \
                   -modifycmd "changeCropImage" \
                   -helptext "The underlying crop module for this state" \
                   -width 10
   message $w.m2 -width 450 -text {The underlying APSIM crop module that this 
state represents.}

   grid $w.l1 -row 1 -column 1 -pady 5
   grid $w.s  -row 1 -column 2
   grid $w.m1 -row 2 -column 1 -columnspan 3 -sticky w

   grid $w.l2 -row 3 -column 1 -pady 5
   grid $w.c  -row 3 -column 2
   grid $w.m2 -row 4 -column 1 -columnspan 3 -sticky w

   label $w.img -image cropImage
   grid $w.img -row 1 -column 4 -rowspan 5 -sticky ne

   grid columnconf $w 3 -weight 1
   grid rowconf    $w 5 -weight 1

   addVariable state
   addVariable crop

   set filename [getImage $config(crop)]
   if {[file exists $filename]} {cropImage config -file $filename}
}

proc changeCropImage {} { 
   global config
   set filename [getImage $config(crop)]
   if {[file exists $filename]} {cropImage config -file $filename}
}

proc plantingWindowUI {w} {

   set row 1
   foreach {name desc units} {
       date1 "Start of planting window" "(dd-mmm)"\
       date2 "End of planting window" "(dd-mmm)"\
       raincrit "Rainfall required for planting" "(mm)" \
       rainnumdays "Number of rainfall days" ""\
       esw_amount "Minimum soil water" "(mm)"} {
      label $w.${name}label -text $desc
      Entry $w.${name}entry -textvariable config($name) -width 8
      label $w.${name}units -text $units
      grid $w.${name}label  -row $row -column 1 -sticky e -pady 3
      grid $w.${name}entry -row $row -column 2
      grid $w.${name}units -row $row -column 3 -sticky w
      incr row
      addVariable $name
   }
   checkbutton $w.must_sow -text "Must Sow" -onvalue yes -offvalue no -variable config(must_sow)
   DynamicHelp::add $w.must_sow -text "Must sow crop at end of planting window"
   grid $w.must_sow -row $row -column 1 -columnspan 2 -sticky w -pady 3
   addVariable must_sow
   incr row

   checkbutton $w.wait_machinery -text "Wait for Machinery" -onvalue yes -offvalue no -variable config(wait_machinery)
   DynamicHelp::add $w.wait_machinery -text "Whether planting should be delayed until machinery is available"
   grid $w.wait_machinery -row $row -column 1 -columnspan 2 -sticky w -pady 3
   addVariable must_sow
   incr row

   message $w.info -aspect 200 -justify left -text \
{Here the time of planting and soil water criteria are described.
Planting is considered in the time window between the two dates specified.
If a planting rainfall event occurs, soil water content (relative to 15bar lower limit)
is above this threshold, and machinery is available, planting this crop is feasible
subject to farm level constraints. If no opportunity has presented
at the end of the window, planting can be forced if the "must sow" flag is set.}
   grid $w.info -row $row -column 1 -columnspan 3 -sticky w -pady 3
   incr row

   grid columnconf $w 5 -weight 1
   grid rowconf    $w $row -weight 1
}


proc sowingParametersUI {w} {
   global config

   set row 1
                        
   set cultivars [getCultivars $config(crop)]
   label $w.cultivarLabel -text "$config(crop) Cultivar"
   ComboBox $w.cultivarCbx -editable 0 \
                -textvariable config(cultivar) \
                -values  $cultivars \
                -helptext "Cultivar of crop"  -width 12
   set index [lsearch $cultivars $config(cultivar)]
   if {$index < 0} {set index 0}
   $w.cultivarCbx setvalue @$index
   grid $w.cultivarLabel  -row $row -column 1 -sticky e -pady 3
   grid $w.cultivarCbx -row $row -column 2 -sticky w
   addVariable cultivar
   incr row

   foreach {name desc units} {
       plants       "Established sowing density"  "(plants/m2)" \
       sowing_depth "Sowing depth"                "(mm)" \
       row_spacing  "Space between rows"          "(mm)" } {

      label $w.${name}label -text $desc
      Entry $w.${name}entry -textvariable config($name) -width 12
      label $w.${name}units -text $units
      grid  $w.${name}label -row $row -column 1 -sticky e  -pady 3
      grid  $w.${name}entry -row $row -column 2 -sticky w
      grid  $w.${name}units -row $row -column 3 -sticky w
      addVariable $name
      incr row
   }

   label $w.sowCostlabel -text "Sowing cost"
   Entry $w.sowCostentry -textvariable config(sowing_costs) -helptext "Cost of sowing" -width 12
   label $w.sowCostunits -text "(\$/ha)"

   grid $w.sowCostlabel -row $row -column 1 -sticky e -pady 5
   grid $w.sowCostentry -row $row -column 2 -sticky w
   grid $w.sowCostunits -row $row -column 3 -sticky w
   addVariable sowing_costs
   incr row

   foreach {name type desc } {
      sow_tractor   tractor   "Tractor for Sowing"  
      sow_implement implement "Implement for Sowing"  } {

      label $w.${name}label -text $desc
      ComboBox $w.${name}Cbx -editable 0 \
                -textvariable config($name) \
                -values  $config($type) \
                -helptext $desc -width 12
      grid  $w.${name}label -row $row -column 1 -sticky e -pady 5
      grid  $w.${name}Cbx   -row $row -column 2 -sticky w
      addVariable $name
      incr row
   }
   
   label $w.fert_typelabel -text "Type of fertiliser"
   ComboBox $w.fert_typeCbx -textvariable config(fert_type) \
                            -width 12 -values $config(fertilisers) \
                            -modifycmd setFert
   grid  $w.fert_typelabel -row $row -column 1 -sticky e -pady 3
   grid  $w.fert_typeCbx -row $row -column 2 -sticky w
   addVariable fert_type
   incr row

   label $w.fert_costlabel -text "Cost of Fertiliser"
   label $w.fert_costentry -textvariable config(fert_cost) -width 12 
   label $w.fert_costunits -text "(\$/kg)"
   grid  $w.fert_costlabel -row $row -column 1 -sticky e -pady 3
   grid  $w.fert_costentry -row $row -column 2 -sticky w
   grid  $w.fert_costunits -row $row -column 3 -sticky w
   addVariable fert_cost
   incr row
   setFert

   label $w.target_nlabel -text "Target N in soil"
   Entry $w.target_nentry -textvariable config(target_n) -width 12
   label $w.target_nunits -text "(kg/ha)"
   grid  $w.target_nlabel -row $row -column 1 -sticky e -pady 3
   grid  $w.target_nentry -row $row -column 2 -sticky w
   grid  $w.target_nunits -row $row -column 3 -sticky w
   addVariable target_n
   incr row

   message $w.info -width 450 -text \
{Here the details of the crop are described. }

   grid $w.info -row $row -column 1 -columnspan 3 -sticky w -pady 3
   incr row

   grid columnconf $w 5 -weight 1
   grid rowconf    $w $row -weight 1
}

proc setFert {} {
   global config
   set name $config(fert_type)
   if {[info exists config($name,fert_cost)]} {
     set config(fert_cost) $config($name,fert_cost)
   } else {
     set config(fert_cost) "???"
   }  
}

proc harvestParametersUI {w} {
   global config

   set row 1

   foreach {name desc units} {
       harvest_costs  "Cost of harvesting"      "($/ha)" \
       price          "Price of crop"           "($/t wet)" \
       moisture       "Grain moisture content"  "(%)"} {
      label $w.${name}label -text $desc
      Entry $w.${name}entry -textvariable config($name) -helptext $desc -width 12
      label $w.${name}units -text $units
      grid  $w.${name}label  -row $row -column 1 -sticky e
      grid  $w.${name}entry -row $row -column 2 -sticky w
      grid  $w.${name}units -row $row -column 3 -sticky w
      addVariable $name
      incr row
   }

   foreach {name type desc } {
      harv_tractor   tractor   "Tractor for Harvesting"  
      harv_implement implement "Implement for Harvesting"  } {

      label $w.${name}label -text $desc
      ComboBox $w.${name}Cbx -editable 0 \
                -textvariable config($name) \
                -values  $config($type) \
                -helptext $desc -width 12
      grid  $w.${name}label -row $row -column 1 -sticky e -pady 5
      grid  $w.${name}Cbx   -row $row -column 2 -sticky w
      addVariable $name
      incr row
   }
   
   grid columnconf $w 5 -weight 1
   grid rowconf    $w $row -weight 1
}

proc editRulesUI {w} {
   global config

   ScrolledWindow $w.sw
   text $w.sw.t -height 30 -wrap none -font courier
   $w.sw setwidget $w.sw.t

   global config
   set config(editWindow)  $w.sw.t

   $w.sw.t insert end [$config(rule) text]

   grid $w.sw -row 1 -column 1 -sticky nswe
   focus $w.sw.t

   $w.sw.t tag configure variables -foreground blue
   $w.sw.t tag configure subst     -foreground red -background gray

   markVariables
   showVariables
   
   bind $w.sw.t <Any-KeyRelease> "+markVariables"

   grid columnconf $w 1 -weight 1
   grid rowconf    $w 1 -weight 1

}

proc markVariables {}  {
   global config

   set t $config(editWindow)

   foreach {a b} [$t tag ranges subst] {$t tag remove subst $a $b}
   set lc 1
   foreach line [split [$t get 0.0 end] "\n"] {
      foreach v $config(variables) {
         if {[set s [string first "\[$v\]" $line]] >= 0} {
             $t tag add subst $lc.$s $lc.[expr $s+[string length $v]+2]
         }
      }
      incr lc
   }
}

# Highlight the variables at the end of the document
proc showVariables {}  {
   global config
   foreach {a b} [$config(editWindow) tag range variables] {$config(editWindow) delete $a $b}
   $config(editWindow) insert end "#Current variables: (not saved - do not edit)\n" variables
   foreach v $config(variables) {
      $config(editWindow) insert end "#$v=$config($v)\n" variables
   }
}

proc additionalUI {w} {
   global config

   set row 1

   foreach node [$config(docroot) childNodes] {
      set nodeName [$node nodeName]
      if {$nodeName != "uiscript" && 
          $nodeName != "rule" &&
          $nodeName != "ruletemplate" &&
          [lsearch $config(variables) $nodeName] < 0} {
         label $w.r${row}label -text $nodeName
         entry $w.r${row}entry -textvariable config($nodeName)

         grid $w.r${row}label -row $row -column 1 -sticky e
         grid $w.r${row}entry -row $row -column 2 -sticky w
         addVariable $nodeName
         incr row
      }
   }

   Button $w.r$row -text "Add parameter" -command "addParameter $w $row" -helptext "Add an editable parameter to this UI" 
   grid $w.r$row -row $row -column 1 -sticky w -pady 5  -padx 5

   grid columnconf $w 3 -weight 1
   grid rowconf    $w [expr $row+5]  -weight 1
}

proc addParameter {w row} {
   destroy $w.r${row}

   entry $w.r${row}entry -textvariable config(additional)
   Button $w.r${row}button -text "Accept" -command "acceptNewParameter $w $row" -helptext "Add this parameter" 
   grid $w.r${row}entry -row $row -column 1 -sticky e
   grid $w.r${row}button -row $row -column 2 -sticky e

   bind $w.r${row}entry <Key-Return> "acceptNewParameter $w $row"
}

proc acceptNewParameter {w row} {
   global config
   destroy $w.r${row}entry
   destroy $w.r${row}button

   label $w.r${row}label -text $config(additional)
   entry $w.r${row}entry -textvariable config($config(additional))
   lappend config(variables) $config(additional)
   grid $w.r${row}label -row $row -column 1 -sticky e
   grid $w.r${row}entry -row $row -column 2 -sticky w

   $config(editWindow) insert end "#$config(additional)=\n" variables

   incr row
   unset config(additional)
   Button $w.r$row -text "Add parameter" -command "addParameter $w $row" -helptext "Add an editable parameter to this UI" 
   grid $w.r$row -row $row -column 1 -sticky w -pady 5  -padx 5
}


proc MainUI  {w} {
   global config
   image create photo cropImage

   NoteBook $w.nb
   CropUI              [$w.nb insert end crop -text "What"]
   plantingWindowUI    [$w.nb insert end pw   -text "Planting Criteria"]
   sowingParametersUI  [$w.nb insert end est  -text "Establishment"]
   harvestParametersUI [$w.nb insert end harv -text "Harvesting"]
   additionalUI        [$w.nb insert end add  -text "Additional"]
   editRulesUI         [$w.nb insert end edit -text "Code" -raisecmd "markVariables; showVariables"]

   grid $w.nb -in $w -row 0 -column 0 -sticky nsew -padx 2 -pady 2
   grid columnconf $w 0 -weight 1
   grid rowconf    $w 0 -weight 1

   $w.nb raise crop
   return $w
}

proc processRule {node} {
   global config
   set template [$node text]
   foreach v $config(variables) {
      regsub -all "\\\[$v\\\]" $template $config($v) template
   }
   return $template
}

###################Code starts here#########
## Decode the XML string for this applet
set config(doc)      [dom parse $XMLDoc]
set config(docroot)  [$config(doc) documentElement]
set config(gdocroot) [[dom parse $GlobalXMLDoc] documentElement]

set allTypes "<root>"
foreach f [glob -nocomplain -dir $apsuite/Model *.xml] {
  if {![string match -nocase farpoint* [file tail $f]] && ![string match -nocase teechart* [file tail $f]]} {
     set fp [open $f r]
     append allTypes [read $fp]
     close $fp
  }  
}
append allTypes "</root>\n"

set config(typesDoc) [dom parse $allTypes]

set config(crops)  [findCrops] 
set config(states) [findStates] 
findMachinery
findFertilisers

set config(rule) {}
foreach node [$config(docroot) childNodes] {
   if {[$node nodeName] == "ruletemplate" } {
      set config(rule) $node
   } elseif {[$node nodeName] != "rule" } {
      set config([$node nodeName]) [$node text]
   }
}

addVariable uiscript

catch {destroy .w}
grid forget .
grid [MainUI [frame .w]] -row 0 -column 0 -sticky nwe
grid columnconf . 0 -weight 1
grid rowconf    . 0 -weight 1


proc setXML {name1 name2 op} {
   global XMLDoc config myName 
   catch {
      set newDoc [dom createDocument [$config(docroot) nodeName]]
      set root [$newDoc documentElement]
      $root setAttribute name [$config(docroot) getAttribute name]
      
      # grab the rule from the text window, replace it in the XML tree
      set new [$config(doc) createElement ruletemplate]
      foreach child [$config(rule) childNodes] {$child delete}
      foreach {a b} [$config(editWindow) tag range variables] {$config(editWindow) delete $a $b}
      $config(rule) appendChild [$config(doc) createTextNode [$config(editWindow) get 0.0 end]]
      
      # now the variable substitutions on that tree
      set new [$newDoc createElement rule]
      $new appendChild [$newDoc createTextNode [processRule $config(rule)]]
      $new setAttribute name      "$myName - [$config(rule) getAttribute name]"
      $new setAttribute condition [$config(rule) getAttribute condition]
      $new setAttribute invisible yes
      $root appendChild $new

      # Keep a copy of the template
      set new [$newDoc createElement ruletemplate]
      $new setAttribute name [$config(rule) getAttribute name]
      $new setAttribute condition [$config(rule) getAttribute condition]
      $new appendChild [$newDoc createTextNode [$config(rule) text]]
      $root appendChild $new
      
      # Finally the variables
      foreach variable $config(variables) {
         set new [$newDoc createElement $variable]
         $new appendChild [$newDoc createTextNode $config($variable)]
         $root appendChild $new
      }
   } msg
   if {$msg != ""} {
      tk_messageBox -title "Error" -message $msg -type ok
   } else {
      set XMLDoc [$newDoc asXML]
#      set fp [open c:/tmp/z.xml w]
#      puts $fp [$newDoc asXML]
#      close $fp
   }   
}

trace add variable XMLDoc read setXML
