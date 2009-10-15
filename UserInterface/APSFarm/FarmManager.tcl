
########### Support procedures

proc max {a b} { return [expr {($a > $b) ? $a : $b}] }
proc min {a b} { return [expr {($a > $b) ? $b : $a}] }

# Return the current state of the system
proc currentState {paddock} {
   global state
   return $state($paddock)
}

# Gather arguments (eg sowing depth, fert rate etc} from defaults array
# $action: the operation (sow, fert ...
# $what: the module (wheat, sorghum ...
proc gatherArgs {action what} {
   global defaults config

   # 1. "sow,..."
   foreach {index value} [array get defaults $action,*] {
      set args([lindex [split $index ","] end]) $value
   }
   # 2. override above with any "wheat,sow,..."
   foreach {index value} [array get defaults $what,$action,*] {
      set args([lindex [split $index ","] end]) $value
   }
   # 3. override above with any "SpringWheat,sow,..."
   if {[info exists config($what,alias)]} {
      foreach {index value} [array get defaults $config($what,alias),$action,*] {
         set args([lindex [split $index ","] end]) $value
      }
   }
   return [array get args]
}

# Mash a list into name/value pairs to pass via the apsim message system
proc mash {list} {
   set result {}
   foreach {name value} $list {lappend result [list $name $value]}
   return $result
}

# Sum a list
proc lsum {list} {
   set result 0.0
   foreach value $list {set result [expr $result + $value]}
   return $result
}

# Linear interpolation
proc linint {x_coords y_coords value} {
   set y  0.0

   if {[llength $x_coords] <= 0 || [llength $y_coords] <= 0 || [llength $x_coords] != [llength $y_coords]} {
      error "bad coordinates in linear_interp_real"
   }

   for {set indx 0} {$indx < [llength $x_coords]} {incr indx} {
      if {$value <= [lindex $x_coords $indx]} {
         if {$indx == 0} {
            set y [lindex $y_coords $indx]
         } else {
            if {abs($value - [lindex $x_coords $indx]) < 1.0E-4} {
               set y [lindex $y_coords $indx]
            } else  {
               set y_part [expr [lindex $y_coords $indx] - [lindex $y_coords [expr $indx-1]] ]
               set x_part [expr [lindex $x_coords $indx] - [lindex $x_coords [expr $indx-1]] ]
               set y [expr ($y_part/$x_part) * ($value - [lindex $x_coords [expr $indx - 1]]) + [lindex $y_coords [expr $indx- 1]] ]
            }
         break;
         }
      } elseif {$indx == [lindex $x_coords $indx]-1} {
         set y  [lindex $y_coords $indx]
      } else {
         set y  0.0
      }
   }
   return $y
}

#############################
# Generic sowing/harvesting procedures. These rely on a "defaults" array for crop specifics
#############################
# Change to a new state (utility procedure). Sows crop and fertilises it
proc sowCrop {paddock crop} {
   global defaults config

   # See if this is an alias
   if {[info exists config($crop,alias)]} {
      set realCrop $config($crop,alias)
   } else {
      set realCrop $crop
   }
   array set sow [gatherArgs sow $crop]

   eval apsimSendMessage .masterpm.$paddock.$realCrop sow [mash [array get sow]]

   # machinery operation
   eval apsimSendMessage economics operate \
                   [mash [concat [array get sow] area $config($paddock,area)  \
                   paddock $paddock costtype incrop_cost ]]

   # Sowing costs
   # {{category seedcost} {name cotton} {rate 3.5} {Comment "blah blah"}} ...
   array set costs [array get defaults $realCrop,sowingCosts]
   array set costs [array get defaults $crop,sowingCosts]
   foreach {junk items}  [array get costs]  {
      foreach item $items {
         eval apsimSendMessage economics expenditure \
                    [mash [concat $item area $config($paddock,area)  \
                    paddock $paddock incrop_cost {} ]]
      }
   }

   # Work out what to do with fertiliser stuff
   array set fert [gatherArgs fertiliser $crop]
   if {$fert(calcMethod) == "constant_rate"} {
      eval apsimSendMessage .masterpm.$paddock.fertiliser apply [mash [array get fert]]
      apsimSendMessage economics expenditure {category fertilisercost} "name  $fert(type)" \
               "rate $fert(amount)" "area $config($paddock,area)"  "paddock $paddock" "fertiliser_type $fert(type)" \
               "fertiliser_rate $fert(amount)" {incrop_cost {}} "crop $crop" \
               "SW_state [apsimGet $config($paddock,watBal).esw]" \
               "NO3_state [apsimGet $config($paddock,nModule).no3()]" \
               "comment Fertiliser" 

   } elseif {$fert(calcMethod) == "targetN"} {
      set n [apsimGet $config($paddock,nModule).no3()]
      set deficit [expr $fert(amount) - $n]
      if {$deficit > 0.0} {
         set fert(amount) $deficit
         eval apsimSendMessage .masterpm.$paddock.fertiliser apply [mash [array get fert]]
         apsimSendMessage economics expenditure {category fertilisercost} "name  $fert(type)" \
                       "rate $fert(amount)" "area $config($paddock,area)"  "paddock $paddock"  "crop $crop" \
                       "fertiliser_type $fert(type)" "fertiliser_rate $fert(amount)" \
                       {incrop_cost {}} \
                       "SW_state [apsimGet $config($paddock,watBal).esw]" \
                       "NO3_state [apsimGet $config($paddock,nModule).no3()]" \
                       "comment Fertiliser"

      }                                 
   } elseif {$fert(calcMethod) == "howard"} {
      set N  [apsimGet $config($paddock,nModule).no3()]      
      set sw [apsimGet esw]             
      set deficit [expr $sw - $N]
      if {$deficit > 0.0} {
         set fert(amount) $deficit
         eval apsimSendMessage $paddock.fertiliser apply [mash [array get fert]]
         apsimSendMessage economics expenditure {category fertilisercost} "name  $fert(type)" \
               "rate $fert(amount)" "area $config($paddock,area)"  "paddock $paddock"  "crop $crop" \
               "fertiliser_type $fert(type)" "fertiliser_rate $fert(amount)" \
               "SW_state [apsimGet $config($paddock,watBal).esw]" \
               "NO3_state [apsimGet $config($paddock,nModule).no3()]" \
               {incrop_cost {}} "comment Fertiliser"
      }
   }

   # Finally, see if there's a helper function defined
   if {[info commands sow$realCrop] != {}} {
      extraSow$realCrop
   } elseif {[info commands sow$crop] != {}} {
      extraSow$crop
   }
}

# See whether an apsim crop can be harvested (utility procedure)
proc canHarvestCrop {paddock crop} {
   global config

   if {[info exists config($crop,alias)]} {set crop $config($crop,alias)}

   if {$crop == "wheat" || $crop == "sorghum" || $crop == "maize" || $crop == "chickpea" || $crop == "weed"} {
      set stageName [apsimGet $paddock.$crop.StageName]
      set plant_status [apsimGet $paddock.$crop.plant_status]
      if {$stageName == "harvest_ripe" || $plant_status == "dead"} {
         return 1
      }
      return 0
   } else {
      error "canHarvestCrop: Don't know harvest a crop called $crop"
   }
}

# Harvest and end a crop
proc harvestAndEndCrop {paddock crop} {
   global config daysSinceLastHarvest defaults

   if {[info exists config($crop,alias)]} {
      set realCrop $config($crop,alias)
   } else {
      set realCrop $crop
   }

   if {[apsimGet .masterpm.$paddock.$realCrop.plant_status] != "dead"} {
      if {$realCrop == "wheat"} {
         apsimSendMessage economics income {category cropprice} "name $realCrop"  \
                          "yield [expr [apsimGet .masterpm.$paddock.$realCrop.yield]/1000.0]" \
                          "protein [apsimGet .masterpm.$paddock.$realCrop.grain_protein]" \
                          "area $config($paddock,area)" "paddock $paddock" "crop $realCrop" \
                          "SW_state [apsimGet $config($paddock,watBal).esw]" \
                          "NO3_state [apsimGet $config($paddock,nModule).no3()]" 
                          
      } else {
         apsimSendMessage economics income {category cropprice} "name $realCrop"  \
                          "yield [expr [apsimGet .masterpm.$paddock.$realCrop.yield]/1000.0]" \
                          "area $config($paddock,area)" "paddock $paddock" "crop $realCrop" \
                          "SW_state [apsimGet $config($paddock,watBal).esw]" \
                          "NO3_state [apsimGet $config($paddock,nModule).no3()]" 
                          
      }  
      # Harvesting costs
      # {{category seedcost} {name cotton} {rate 3.5} {Comment "blah blah"}} ...
      array set costs [array get defaults $realCrop,harvestingCosts]
      array set costs [array get defaults $crop,harvestingCosts]
      foreach {junk items} [array get costs] {
         foreach item $items {
            eval apsimSendMessage economics expenditure \
                       [mash [concat $item area $config($paddock,area)  \
                       paddock $paddock incrop_cost {} crop $crop ]]
         }
      }

      reportEvent $paddock "harvest-crop=$crop"
   } else {
      reportEvent $paddock "fail-crop=$crop"
   }
   apsimSendMessage .masterpm.$paddock.$realCrop harvest
   apsimSendMessage .masterpm.$paddock.$realCrop kill_crop
   apsimSendMessage .masterpm.$paddock.$realCrop end_crop
   set daysSinceLastHarvest($paddock) 0
}

proc calcPAWC {paddock} {
  set dul [apsimGet $paddock.dul_dep]
  set ll [apsimGet $paddock.ll15_dep]
  set pawc 0.0
  for {set i 0} {$i < [llength $dul]} {incr i} {
      set pawc [expr $pawc + ([lindex $dul $i] - [lindex $ll $i])]
  }
  return $pawc
}

proc genericCosts {args} {
   # {{category seedcost} {name cotton} {rate 3.5} {Comment "blah blah"}} ...
   foreach {junk items} [concat [array get defaults $crop,sowCosts] [array get defaults $realCrop,sowCosts]]  {
      foreach item $items {
         eval apsimSendMessage economics expenditure \
                    [mash [concat $item area $area  \
                    paddock $paddock incrop_cost {} 
      }
   }
}


proc dateWithin {t0 t1} {
   set d0 [date2day $t0]
   set d1 [date2day $t1]
   return [dayWithin $d0 $d1]
}

proc dateIs {t} {
   set d [date2day $t]
   return [dayIs $d]
}

proc dayWithin {t0 t1} {
   set t [apsimGet day]
   if {$t0 > $t1} {
      return [expr $t >= $t0 || $t <= $t1]
   } else {
      return [expr $t >= $t0 && $t <= $t1]
   }
   # notreached
}

proc dayIs {t} {
   return [expr $t == [apsimGet day]]
}

proc date2day {d} {
   set year [apsimGet year]
   return [string trimleft [clock format [clock scan "$d-$year"] -format %j] "0"]
}


# Return the sum of the last n days rainfall
proc sumLastRain {n} {
   global rainList
   return  [expr [join [lrange $rainList end-[expr $n-1] end] "+"]]
}

# Maintain a list of rainfall amounts.
proc accumRain {} {
   global rainList
   set rainList [concat [lrange $rainList 1 end] [apsimGet rain]]
}

# Return the fraction planted under a crop
proc areaPlanted {what} {
   global config
   set sum 0; set n 0
   foreach paddock $config(cropPaddocks) {
      set state [currentState $paddock]
      if {[info exists config($state,alias)]} {set state $config($state,alias)}
      if {"$state" == "$what"} {
        incr sum
      }
      incr n
   }
   return [expr $sum/$n.0 ]
}

proc machineryAvailable {what} {
   # Arrays are returned as stringified lists
   eval array set result [apsimGet available($what)]
   return $result($what)
}

# simple logging to record farm activities in summary file
proc setupLogging {logFileName} {
   global paddocks colour
   apsimWriteToSummaryFile "initialised logging:paddocks=$paddocks"
}
proc closeLogging {} {
   apsimWriteToSummaryFile "Finished"
}
proc log {msg} {
   apsimWriteToSummaryFile $msg
}


# Set up a report CSV file
set reportFp {}
proc setupReport {} {
   global reportFp crops paddocks 
   global simName 
   
   set reportFp [open $simName.csv w]
   puts -nonewline $reportFp "day,year,source,event,rain"

   foreach paddock $paddocks {
     if {$paddock != "toplevel"} {
       foreach variable {runoff drain soil_loss es} {
          puts -nonewline $reportFp ",$paddock.$variable"
       }
       foreach crop $crops {
          puts -nonewline $reportFp ",$paddock.$crop.yield"
       }
     } 
   }
   puts $reportFp "livestock_number,livestock_class,livestock_weight"
}

# Write a line to the CSV report file
proc reportCSV {from event args} {
   global reportVariables reportFp config paddocks crops accRain
   set line "[apsimGet day],[apsimGet year],$from,$event"
   foreach arg [split $args ","] {
      foreach {name value} [split $arg "="] {break}
      set info($name) $value
   }
   if {[info exists info(crop)]} {set crop $info(crop)} else {set crop ""}
   if {[info exists config($crop,alias)]} {set crop $config($crop,alias)}

   if {$event == "endYear"} {
      append line ",$accRain"
   } else {
      append line ","
   }
   
   foreach paddock $paddocks {
      if {$paddock != "toplevel"} {
         foreach variable {runoff drain soil_loss es} {
            if {$event == "endYear"} {
               global $paddock.$variable
               append line ",[set $paddock.$variable]"
            } else {
               append line ","
            }
         }  
         foreach crop2 $crops {
            set value ""
            if {$paddock == $from && $crop == $crop2 && $event == "harvest"} {
              set value [apsimGetOptional .masterpm.$paddock.$crop2.yield]
            }
            append line ",$value"
         }
      }
   }
   foreach what {livestock_number livestock_class livestock_weight} {
      if {[info exists info($what)]} {
        append line ",$info($what)"
      } else {
        append line ","
      }
   }
   puts $reportFp $line
   flush $reportFp
}

proc closeReporting {} {
   global reportFp
   if {$reportFp != {}} {
      close $reportFp
   }
}

# pass events to various output files
proc reportEvent {from what} {
   foreach {event args} [split $what "-"] {break}
   reportCSV $from $event $args
}


#############################
############### The decision routine(s).
#############################
# Return the score for planting "what"
proc checkRules {thisPaddock} {
  global config

  uplevel #0 set paddock $thisPaddock
  set bestScore -1.0; set bestTarget {}; set bestArc {}; set bestGraph {}
  foreach graph $config($thisPaddock,graphNames) {
     foreach arc [$graph arcs -out [currentState $thisPaddock]] {
        set expr {}
        set target [$graph arc target $arc]
        foreach rule [$graph arc get $arc rules] {
           set value [uplevel #0 expr $rule]
           lappend expr $value
           if {$value == 0} {break} ;# no need to continue evaluating subsequent rules
        }
        if {[llength $expr] > 0} {
           set score [expr [join $expr "*"]]
           if {$score > $bestScore} {
              set bestScore  $score
              set bestTarget $target
              set bestArc    $arc
              set bestGraph  $graph
           }
        }
     }
  }   
  return [list  $bestScore $bestGraph $bestTarget $bestArc]
}

# A place holder function to monitor state transitions
proc logState {graph paddock state} {}

# Change state to another
proc changeState {paddock graph arc} {
   global state
   uplevel #0 set paddock $paddock
   foreach action [$graph arc get $arc actions] { uplevel #0 $action }
   set state($paddock) [$graph arc target $arc]
   logState $graph $paddock $state($paddock)
}

# Do daily process at top level
proc doProcess {} {
   global date paddocks config daysSinceLastHarvest
   
   set date "[apsimGet day],[apsimGet year]"

   set more 1
   while {$more} {
      set more 0
      # 1. Find out what's possible
      set bestPaddock {}; set bestScore -1.0
      foreach paddock $paddocks {
         foreach {score graph target arc} [checkRules $paddock] {break}
         if {$score > $bestScore} {
            set bestPaddock $paddock; set bestScore $score; set bestGraph $graph; set bestTarget $target; set bestArc $arc
         }
      }
      if {$bestScore > 0.0} {
         changeState $bestPaddock $bestGraph $bestArc
         set more 1
      }
   }
   
   foreach paddock $paddocks {
      incr daysSinceLastHarvest($paddock)
   }   
}


