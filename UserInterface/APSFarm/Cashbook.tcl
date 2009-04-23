# Apsim/Economics Cashbook (sub)module

# accepts messages:
#  cashbook income {amount 64000.0} {comment "description here"}
#  cashbook income {category cropprice} {name wheat} {yield 4000} {protein 12.3} {comment "description here"}
#  cashbook income {category cropprice} {name sorghum} {yield 6000} {comment "description here"}
#  cashbook expenditure {cost 42.0} {comment "description here"}
#  cashbook expenditure {category seed} {name wheat} {rate 120} {comment "description here"}
# Keeps a balance, writes a log.

########################## Apsim interface code here
source $apsuite/Model/TclLink/CIDataTypes.tcl

set balance [expr 0.0]
apsimRegisterGetSet balance
apsimRegisterEvent  income      "cashbook:incomeHandler"
apsimRegisterEvent  expenditure "cashbook:expenditureHandler"
apsimRegisterEvent  process     "cashbook:processHandler"
apsimRegisterEvent  end_simulation "cashbook:exitHandler"

# An event handler. 
#  cashbook income {amount 64000.0}                                              {comment "description here"}
#  cashbook income {category cropprice} {name wheat} {yield 4000} {protein 12.3} {comment "description here"}
#  cashbook income {category cropprice} {name sorghum} {yield 6000}              {comment "description here"}
proc cashbook:incomeHandler {args} {
   global incomingApsimVariant balance

   set names {}
   foreach {_name _value} [unpack_Variant $incomingApsimVariant] {
      set $_name $_value
      lappend names $_name
   }
   if {![info exists comment]} { set comment "" }
   if {![info exists paddock]} { set paddock "" }
   if {![info exists protein]} { set protein "" }
   
   if {[info exists amount] && [string is double -strict $amount]} {
      set balance [expr $balance + $amount]
      cashbook:log income amount $amount comment $comment paddock $paddock

   } elseif {[info exists category] && [info exists name] && 
             [info exists yield] && [info exists area]} {
      set amount "NA"
      set price  "NA"
      global docroot
      foreach node [$docroot selectNodes //$category] {
         if {[string equal -nocase [getValue $node name] $name]} {
             set grain_moisture [getValue $node grain_moisture]
             if {![string is double -strict $grain_moisture]} {
                apsimWriteToSummaryFile "Warning!!!!\nNo grain moisture found for $name. Using 12%."
                set grain_moisture 12.0
             }
             if {$name != "wheat"} {
                set price [getValue $node price]
             } else {
                # oddball wheat pricing system..
                set proteins [getValue $node protein]
                set prices [getValue $node price]
                set price [lindex $prices 0]
                for {set i 0} {($i < [llength $proteins]) && 
                               ($protein > [lindex $proteins $i])} {incr i} {
                   set price [lindex $prices $i]
                }
             }  
             set wetYield [expr ($yield * 100.0) / (100.0 - $grain_moisture)]
             set amount [expr  $price * $wetYield * $area]
         }
      }
      set comment "$category ($name)"
      apsimWriteToSummaryFile "Priced $name $category (price='$price', yield='$wetYield' wet) over '$area' ha = '$amount'"
      set balance [expr $balance + $amount]
      cashbook:log income amount $amount comment $comment \
                          income $amount crop_income $amount \
                          paddock $paddock area $area \
                          yield $wetYield protein $protein crop $name
   } else {
      error "cashbook:income: Must specify a either an numeric amount, or <category, name, price and area>."
   }
}

proc cashbook:expenditureHandler {args} {
  global incomingApsimVariant
  set _names {}

  set comment ""
  foreach {_name _value} [unpack_Variant $incomingApsimVariant] { 
     set $_name $_value 
     lappend _names $_name
  }

  if {[lsearch ${_names} comment] < 0} {lappend _names comment}

  if {[info exists cost]} {
     # Simple 
     global balance
     set balance [expr $balance - $cost]
  } elseif {[info exists category] && [info exists name] && [info exists rate] && [info exists area]} {
     # lookup this expense in our table of costs

     set cost "NA"
     global docroot
     foreach node [$docroot selectNodes //$category] {
        if {[string equal -nocase [getValue $node name] $name]} {
            set price [getValue $node price]
            set cost [expr $price * $rate * $area]
        }
     }
     if {![string is double -strict $cost]} {error "No price info for $category/$name"}
     if {[lsearch ${_names} price] < 0} {lappend _names price}

     set comment "$category ($name)"
     apsimWriteToSummaryFile "Costed $name $category (rate=$rate, price=$price) over $area ha = $cost"
     
     global balance
     set balance [expr $balance - $cost]
  } else {
     error "cashbook:expenditure: Must specify a either a cost or a (category + name + rate + area)."
  }
  if {[info exists fallow_cost] && ![string is double -strict $fallow_cost]} {set fallow_cost $cost}
  if {[info exists incrop_cost] && ![string is double -strict $incrop_cost]} {set incrop_cost $cost}
  set cmd "cashbook:log expenditure cost $cost" 
  foreach _name ${_names} { lappend cmd ${_name} [set ${_name}] }
  eval $cmd
}

# Add up the annual farm overheads
proc cashbook:doFarmOverheads {} {
   global docroot balance
   set sum 0.0
   foreach node [$docroot selectNodes //overhead] {
      set sum [expr $sum + [getValue $node value]]
   }
      
   set balance [expr $balance - $sum]
   cashbook:log expenditure cost $sum comment "Farm Overheads"
}

# Work our repayments on initial investment
proc cashbook:doInitialCapital {} {
   global docroot balance
   set sum 0.0

   # Set the period of initial loan
   setValue [lindex [$docroot selectNodes //icapital] 0] loanPeriod 1

   set node [lindex [$docroot selectNodes //icapital] 0]
   if {[getValue $node loanRate] > 0.0} {
      if {[getValue $node loanPeriod] <=  [getValue $node loanDuration]} {
         #A = P(i(1+i)^n)/((1+i)^n - 1)
         set P [getValue $node ivalue] 
         set i [expr [getValue $node loanRate]/100.0]
         set n [getValue $node loanDuration]
         
         set A [expr $P * ($i*pow(1+$i,$n))/(pow(1.0+$i,$n) - 1.0) ]
      
         set balance [expr $balance - $A]
         cashbook:log expenditure cost $A \
                                  interest_paid $A \
                                  comment "Loan repayments for initial capital outlay"
         
         setValue $node loanPeriod [expr 1 + [getValue $node loanPeriod]]
         if { [getValue $node loanPeriod] >  [getValue $node loanDuration] } {
             apsimWriteToSummaryFile "Loan for initial capital outlay is finished"
         }    
      }
   }
}
     


# Send an "end financial year" message when needed
proc cashbook:processHandler {args} {
  if {[apsimGet day] == 181} {
     cashbook:doFarmOverheads
     cashbook:doInitialCapital
     apsimSendMessage "" end_financial_year
  }
}

proc cashbook:log {what args} {
   global balance cashbook:outputfp
   set date [apsimGet dd/mmm/yyyy]

   set _names {}
   foreach {_name _value} $args { 
      set ${_name} ${_value} 
      lappend _names $_name
   }

   if {${cashbook:outputfp} != {}} {
      if {$what == "income"} {
         puts ${cashbook:outputfp} "$date,$amount,,$balance,$comment"
      } elseif {$what == "expenditure"} {
         puts ${cashbook:outputfp} "$date,,$cost,$balance,$comment"
      } else {
         error "Unknown cashbook operation '$what'"
      }
   }

   if {$what == "income"} {
      set cmd "cashbook:summary date $date $what $amount"
   } elseif {$what == "expenditure"} {
      set cmd "cashbook:summary date $date $what $cost "
   } else {
      error "Unknown cashbook operation '$what'"
   }
   foreach _name ${_names} { lappend cmd ${_name} [set ${_name}] }
   eval $cmd
}

proc cashbook:summary {args} {
   global balance cashbook:summaryfp
   
   if { ${cashbook:summaryfp} != {}} {
      
      foreach {_name _value} $args {set ${_name} ${_value} }

      foreach v {date paddock area crop yield protein "fertiliser_type" \
               "fertiliser_rate" "interim_rainfall" "interim_runoff" "interim_drainage" \
               "interim_soil_loss" "NO3_state" "fallow_cost" "incrop_cost" "crop_income" \
               "interest_paid" "expenditure" "income" "comment"} {
         if {[info exists $v]} {
            puts -nonewline ${cashbook:summaryfp} "[set $v],"
         } else {
            puts -nonewline ${cashbook:summaryfp} ","
         }     
      }
      puts ${cashbook:summaryfp} ""
   }
}
# Annual summaries. Re-read the event file we have written into a single array and
# write a cash flow summary
proc cashbook:exitHandler {} {
   global docroot cashbook:summaryfilename cashbook:outputfp cashbook:summaryfp

   puts "Cashbook: Writing summaries"

   if { ${cashbook:outputfp} != {}} { close ${cashbook:outputfp} }
   if { ${cashbook:summaryfp} != {}} { close ${cashbook:summaryfp} }

   if { ${cashbook:summaryfilename} == {}} { return }

   #1. Read and store event data
   set fp [open ${cashbook:summaryfilename} r]
   gets $fp header
   set columns [split $header ","]
   
   set minyear 3000; set maxyear 0; 
   set years {}; set paddocks {}; catch {unset A}
   
   while {[gets $fp line] >= 0} {
      foreach what {incrop_cost fallow_cost crop_income income expenditure interest_paid interest_earned} {catch {unset $what}}
      foreach $columns [split $line ","] {break}
      regsub -all "/" $date "-" date
      set now [clock scan $date]
      set endYear [clock scan "30-june-[clock format $now -format %Y]"]
   
      if {$now > $endYear} {
         set year [expr [clock format $now -format %Y]+1]
      } else {
         set year [clock format $now -format %Y]
      }
   
      if {$year < $minyear} {set minyear $year}
      if {$year > $maxyear} {set maxyear $year}
      if {[lsearch $years $year] < 0} {lappend years $year}
      if {$paddock != {} && $paddock != "livestock" && [lsearch $paddocks $paddock] < 0} {lappend paddocks $paddock}
      foreach what {incrop_cost fallow_cost crop_income} {
         if {[string is double -strict [set $what]]} {
            if {![info exists A($year,$paddock,$what)]} {
               set A($year,$paddock,$what) [set $what]
            } else {
               set A($year,$paddock,$what) [expr $A($year,$paddock,$what) + [set $what]]
            }
         }
      }
      
      foreach what {income expenditure interest_paid interest_earned} {
         if {$paddock == {} && 
             [info exists $what] && 
             [string is double -strict [set $what]]} {
            if {![info exists A($year,farm_$what)]} {
               set A($year,farm_$what) [set $what]
            } else {
               set A($year,farm_$what) [expr $A($year,farm_$what) + [set $what]]
            }
         }

         if {$paddock == "livestock" && 
             [info exists $what] && 
             [string is double -strict [set $what]]} {
            if {![info exists A($year,livestock_$what)]} {
               set A($year,livestock_$what) [set $what]
            } else {
               set A($year,livestock_$what) [expr $A($year,livestock_$what) + [set $what]]
            }
         }

      }
   }
   close $fp
   
   #2. Write annual cash flow summary
   set fp [open "[file root ${cashbook:summaryfilename}].Annual.csv" w]

   foreach year $years {puts -nonewline $fp ",Year $year"}
   puts $fp "\nCrop income"
   foreach paddock $paddocks {
      puts -nonewline $fp "Paddock $paddock"
      foreach year $years {
         if {[info exists A($year,$paddock,crop_income)]} {
            puts -nonewline $fp ",[format "%.0f" $A($year,$paddock,crop_income)]"
         } else {
            puts -nonewline $fp ","
         }
      }
      puts $fp ""
   }
   
   puts -nonewline $fp "Livestock income"
   foreach year $years {
      if {[info exists A($year,livestock_income)]} {
         puts -nonewline $fp ",[format "%.0f" $A($year,livestock_income)]"
      } else {
         puts -nonewline $fp ","
      }
   }
   puts $fp ""
   puts -nonewline $fp "Other farm income"
   foreach year $years {
      if {[info exists A($year,farm_income)]} {
         puts -nonewline $fp ",[format "%.0f" $A($year,farm_income)]"
      } else {
         puts -nonewline $fp ","
      }
   }
   puts $fp ""
   puts -nonewline $fp "Interest Earned"
   foreach year $years {
      if {[info exists A($year,farm_interest_earned)]} {
         puts -nonewline $fp ",[format "%.0f" $A($year,farm_interest_earned)]"
      } else {
         puts -nonewline $fp ","
      }
   }
   puts $fp ""
   
   puts -nonewline $fp "Total income"
   foreach year $years {
      set sum 0.0
      foreach {name value} [array get A $year,*income] {
         set sum [expr $sum + $value]
      }   
      foreach {name value} [array get A $year,farm_interest_earned] {
         set sum [expr $sum + $value]
      }   
      puts -nonewline $fp ",[format %.0f $sum]"
      set A($year,total_income) $sum
   }
   puts $fp "\n"
   
   puts $fp "Variable expenses"
   foreach what {incrop_cost fallow_cost} {
      foreach paddock $paddocks {
         puts -nonewline $fp "Paddock $paddock $what"
         foreach year $years {
            if {[info exists A($year,$paddock,$what)]} {
               puts -nonewline $fp ",[format "%.0f" $A($year,$paddock,$what)]"
            } else {
               puts -nonewline $fp ","
            }
         }
         puts $fp ""
      }
   }
   puts -nonewline $fp "Livestock expenses"
   foreach year $years {
      if {[info exists A($year,livestock_expenditure)]} {
         puts -nonewline $fp ",[format "%.0f" $A($year,livestock_expenditure)]"
      } else {
         puts -nonewline $fp ","
      }
   }
   puts $fp ""
   puts -nonewline $fp "Other expenses"
   foreach year $years {
      if {[info exists A($year,farm_expenditure)]} {
         puts -nonewline $fp ",[format "%.0f" $A($year,farm_expenditure)]"
      } else {
         puts -nonewline $fp ","
      }
   }
   puts $fp ""
   
   puts -nonewline $fp "Interest Paid"
   foreach year $years {
      if {[info exists A($year,farm_interest_paid)]} {
         puts -nonewline $fp ",[format "%.0f" $A($year,farm_interest_paid)]"
      } else {
         puts -nonewline $fp ","
      }
   }
   puts $fp ""
   
   puts -nonewline $fp "Total expenses"
   foreach year $years {
      set sum 0.0
      foreach {name value} [array get A $year,farm_expenditure] {
         set sum [expr $sum + $value]
      }   
      foreach {name value} [array get A $year,farm_interest_paid] {
         set sum [expr $sum + $value]
      }   
      foreach {name value} [array get A $year,*cost] {
         set sum [expr $sum + $value]
      }   
      puts -nonewline $fp ",[format %.0f $sum]"
      set A($year,total_expenses) $sum
   }
   puts $fp "\n"

   puts -nonewline $fp "Annual Surplus/Deficit"
   foreach year $years {
      set sum [expr $A($year,total_income) - $A($year,total_expenses)]
      set A($year,surplus) $sum
      puts -nonewline $fp ",[format %.0f $sum]"
   }
   puts $fp "\n"

   puts -nonewline $fp "Cumulative Cash Flow"
   set year [expr [lindex $years 0]-1]
   set A($year,balance) [[$docroot selectNodes //balance] text]
   foreach year $years {
      set A($year,balance) [expr $A([expr $year-1],balance) + $A($year,surplus)]
      puts -nonewline $fp ",[format %.0f $A($year,balance)]"
   }
   puts $fp "\n"

   close $fp
}


########################## End apsim interface code
## Read our initial conditions

package require tdom
set doc [dom parse [apsimGetComponentXML]]
set docroot [$doc documentElement]

set node [lindex [$docroot selectNodes //balance] 0]
if {$node != {}} {
   set balance [$node text]
}

set node [lindex [$docroot selectNodes //outputfilename] 0]
set cashbook:outputfp {}
set cashbook:outputfilename {}
if {$node != {}} {
  set cashbook:outputfilename [$node text]
}
 
set node [lindex [$docroot selectNodes //summaryfilename] 0]
set cashbook:summaryfp {}
set cashbook:summaryfilename {}
if {$node != {}} {
  set cashbook:summaryfilename [$node text]
}  

if {${cashbook:outputfilename} != {}} {
   set cashbook:outputfp [open ${cashbook:outputfilename} w]
   puts ${cashbook:outputfp} "date,income,expenditure,balance,comment"
}

if {${cashbook:summaryfilename} != {}} {
   set cashbook:summaryfp [open ${cashbook:summaryfilename} w]
   puts ${cashbook:summaryfp} "date,paddock,area,crop,yield,protein,fertiliser_type,fertiliser_rate,\
interim_rainfall,interim_runoff,interim_drainage,\
interim_soil_loss,NO3_state,fallow_cost,incrop_cost,crop_income,interest_paid,expenditure,income,comment"
   file delete -force "[file root ${cashbook:summaryfilename}].Annual.csv"
}

puts "     Cashbook initialised."
if {${cashbook:outputfilename} != {}} {
  puts "     Simple output: ${cashbook:outputfilename}"
}
if {${cashbook:summaryfilename} != {}} {
  puts "     Summary : ${cashbook:summaryfilename}"
  puts "     Annual  : [file root ${cashbook:summaryfilename}].Annual.csv"
}

puts "     Initial Balance = $balance"
puts ""
