# Apsim Machinery (sub)Module
# Contains Tractors, Implements & Labour.

# Operations: 
# bool available(<config>)    ; See if a configuration can be used today (read)
# operate <config> <area>     ; Operate a configuration over an area until finished (event)

# <config> is:
#  tractor=<name>,implement=<name>

#########################################################################
# support procedures. 
package require tdom

########################## Apsim interface code here
source $apsuite/Model/TclLink/CIDataTypes.tcl

# A trace handler for our 'fake' array. This handler is called whenever another module 
# asks us whether machinery is available.
#    NB. Careful! Errors in trace routines are not caught!!!
proc machinery:maProc {name1 name2 op} {
   if {$op == "read" && [string length $name2] > 0} {
      global $name1
      array set $name1 [list $name2 0]                   ;# off by default
      foreach nv [split $name2 ","] {
         foreach {name value} [split $nv "="] {
            regsub -all "'" $name {} name
            regsub -all "'" $value {} value
            set $name $value
         }
      }
      # Do stuff with args to determine what the value should be..
      if {![info exists tractor] || ![info exists implement]} {
         set msg "You must specify a tractor and implement operate with. (got $name2)"
         apsimWriteToSummaryFile $msg; error $msg
      }
      
      # Check they are known to us
      if {[catch {set tid [getTractorId $tractor]; set iid [getImplementId $implement]}]} {
         global errorInfo
         apsimWriteToSummaryFile "$msg\n$errorInfo"; error $msg
      }
      
      # Now see if they are busy
      set avail 1
      catch {
         global machinery:jobs
         foreach job ${machinery:jobs} {
            foreach {tid iid area paddock costtype} [split $job ","] {break}
            if {$tractor == [getName $tid] || $implement == [getName $iid]} {set avail 0}
         }
      } msg
      if {$msg != ""} {apsimWriteToSummaryFile $msg; error $msg}
           
      # Set the variable - this is what is returned to apsim.
      array set $name1 [list $name2 $avail]
   }  
}

# An event handler. 
proc machinery:operateHandler {args} {
  global incomingApsimVariant
  foreach {name value} [unpack_Variant $incomingApsimVariant] {
     regsub -all "'" $name {} name
     regsub -all "'" $value {} value
     set [string tolower $name] [string tolower $value]
  }
  if {![info exists tractor] || ![info exists implement] || ![info exists area]} {
     error "Must specify a tractor, implement, and area to operate over."
  }
  if { ![info exists paddock] } {set paddock {}}
  if { ![info exists costtype] } {set costtype unknown}
  machinery:operate $tractor $implement $area $paddock $costtype
}

array set available {}

# Now tell apsim we own it (unnecessary)
apsimRegisterGetSet available

# The trace variable we use to trigger events when the variable is read
trace add variable available {array read} "machinery:maProc"

# Now register the event handlers. 
apsimRegisterEvent operate            "machinery:operateHandler"
apsimRegisterEvent process            "machinery:process"
apsimRegisterEvent end_financial_year "machinery:end_year"
########################## End apsim interface code

########################## Machinery configuration code
# All machinery has an "id" which is just a node in the XML tree.
# Read in our configuration to an XML tree
proc machinery:initialise {} {
   global config
   set config(xmldoc) [dom parse [apsimGetComponentXML]]
   set config(docroot) [$config(xmldoc) documentElement]
} 

# Get the value of an objects 'thing'
proc getValue {id thing} {
   foreach node [$id childNodes] {
      if {[string equal -nocase [$node nodeName] $thing]} {
         return [string tolower [$node text]]
      }
   }
#   apsimWriteToSummaryFile "Object $id doesn't have a $thing node:"
#   foreach node [$id childNodes] {
#      apsimWriteToSummaryFile "name=[$node nodeName],value=[$node text]"
#   }
   return ""
}

# Set the value of an objects 'thing'
proc setValue {id thing what} {
   global config
   foreach node [$id childNodes] {
      if {[string equal -nocase [$node nodeName] $thing]} {
         $id removeChild $node
      }
   }
   set node [$config(xmldoc) createElement $thing]
   $node appendChild [$config(xmldoc) createTextNode $what]
   $id appendChild $node
}

# Return the name of an object, given id.
proc getName {id} {
   return [getValue $id "name"]
}

# Return a list of tractors we know about
proc getTractorIds {} {
   global config
   return [$config(docroot) selectNodes //tractor]
}

proc getTractorNames {} {
   set result {}
   foreach id [getTractorIds] {lappend result [string tolower [getName $id]]}
   return $result
}

# Return the ID of a tractor we know about
proc getTractorId {name} {
   global config
   foreach fn [$config(docroot) selectNodes //tractor/name] {
      if {[string compare -nocase "[$fn text]" "$name"] == 0} {return [$fn parent]}
   }
   error "Tractor $name isn't in database (should be one of \"[join [getTractorNames] \",\"]\")"
}
proc getImplementIds {} {
   global config
   return [$config(docroot) selectNodes //implement]
}
proc getImplementNames {} {
   set result {}
   foreach id [getImplementIds] {lappend result [string tolower [getName $id]]}
   return $result
}
proc getImplementId {name} {
   global config
   foreach fn [$config(docroot) selectNodes //implement/name] { 
      if {[string compare -nocase "[$fn text]" "$name"] == 0} {return [$fn parent]}
   }
   error "Implement $name isn't in database (should be one of \"[join [getImplementNames] \",\"]\")"
}

# return the work rate for this combo can cover per hour
proc getCombo {tid iid thing} {
   set implement [getName $iid]
   foreach rn [$tid childNodes] {
     if {[string compare -nocase [$rn nodeName] $thing] == 0 &&
         [string compare -nocase [$rn getAttribute implement] "$implement"] == 0} {
           return [$rn text]
       }
   }
   error "No work rate for [[$tid child 1] text] + [[$iid child 1] text] specified"
}

proc getRate {tid iid} {
   return [getCombo $tid $iid fuelrate]
}

# Return the cost of fuel per liter
proc fuelCost {} {
   global config
   set node [lindex [$config(docroot) selectNodes //fuelcosts] 0]
   return [getValue $node "price"]
}

# return the fuel cost this combo uses per hour 
proc getFuelCost {tid iid} {
   return [expr [getCombo $tid $iid fuelrate] * [fuelCost]]
}

# Return the number of hours worked per day
proc getHoursPerDay {tid iid} {
   set implement [getName $iid]
   foreach rn [$tid selectNodes hoursperday] {
       if {[string compare -nocase [$rn getAttribute implement] "$implement"] == 0} {
          set hoursperday [$rn text]
          return $hoursperday
       }
   }
   error "No hours per day for [getName $tid] + [getName $iid] specified"
}

##############
# Operate a configuration over an area. Just add it to the job queue 
# and let process look after it.
proc machinery:operate {tractorName implementName area paddock costtype} {
   set tid [getTractorId $tractorName]
   set iid [getImplementId $implementName]
   if {![string is double -strict $area]} {
      error "Area should be a number (not $area)"
   }
   global machinery:jobs
   lappend machinery:jobs $tid,$iid,$area,$paddock,$costtype
   apsimWriteToSummaryFile "Machinery job '[getName $tid] + [getName $iid]' is queued"
}

# The daily process routine. Manages the job queue
proc machinery:process {} {
   global machinery:jobs
   set tomorrowsJobs {}
   # Go through each job. If an item is in use in any prior job, we can't do it today. 
   for {set ijob 0} {$ijob < [llength ${machinery:jobs}]} {incr ijob} {
      set job [lindex ${machinery:jobs} $ijob]
      foreach {tid iid area paddock costtype} [split $job ","] {break}
      set inuse 0
      for {set j 0} {$j < $ijob} {incr j} {
         foreach {Ttid Tiid Tarea Tpaddock Tcosttype} [split [lindex ${machinery:jobs} $j] ","] {break}
         if {$tid == $Ttid || $iid == $Tiid} {set inuse 1}
      }
      if {!$inuse} {
        # The job is running today. Work out how many hours, and then the costs
        set maxHours [getHoursPerDay $tid $iid]
        set rate [getRate $tid $iid]

        if {$maxHours * $rate <= $area} {
           set hours $maxHours
           set areaToday [expr $maxHours * $rate]
        } else {   
           set hours [expr $area / $rate]
           set areaToday [expr $hours * $rate]
        }   
        #apsimWriteToSummaryFile "hours='$hours', rate='$rate',cost='[getFuelCost $tid $iid]',oil='[getValue $tid oil]',paddock='$paddock',costtype=$costtype"
        set cost [expr $hours * [getFuelCost $tid $iid] * (1.0 + [getValue $tid oil]/100.0)]
        apsimSendMessage "" expenditure [list cost $cost] [list comment "fuel & oil costs of [getName $tid] + [getName $iid]"] [list paddock $paddock] [list area $areaToday] [list $costtype {}]

        set cost [expr $hours * [getValue $tid newPrice] * ([getValue $tid repairs]/100.0)/ [getValue $tid lifeOfEquipment]] 
        apsimSendMessage "" expenditure [list cost $cost] [list comment "Repairs & maintenance of [getName $tid]"] [list paddock $paddock] [list area $areaToday] [list $costtype {}]

        set cost [expr $hours * [getValue $iid newPrice] * ([getValue $iid repairs]/100.0)/ [getValue $iid lifeOfEquipment]] 
        apsimSendMessage "" expenditure [list cost $cost] [list comment "Repairs & maintenance of [getName $iid]"] [list paddock $paddock] [list area $areaToday] [list $costtype {}]

        set rate [expr $hours * [getRate $tid $iid]]
        set area [expr $area - $rate]
        setValue $tid age [expr $hours + [getValue $tid age]]
        setValue $iid age [expr $hours + [getValue $iid age]]

        if {$area > 0} {
           lappend tomorrowsJobs $tid,$iid,$area,$paddock,$costtype
        } else {
           apsimWriteToSummaryFile "Machinery job '[getName $tid] + [getName $iid]' in $paddock has finished"
        }
      } else {
        lappend tomorrowsJobs $job
      } 
   }
   set machinery:jobs $tomorrowsJobs
}

# The "end_year" routine. Do loan payments and replacement
proc machinery:end_year {} {
   foreach id [concat [getTractorIds] [getImplementIds]] {   
     if {[getValue $id loanInterestRate] > 0.0} {
        if {[getValue $id loanDuration] > 0} {
           if {[getValue $id age] >= [getValue $id lifeOfEquipment]} {
              set newPrice [getValue $id newPrice]
              set salvage [expr [getValue $id tradeInValue]/100.0 * $newPrice]
              set loanValue [expr $newPrice - $salvage]
              apsimWriteToSummaryFile "Establishing new loan of \$$loanValue for [getName $id] (new price \$$newPrice, salvage \$$salvage)"
              setValue $id loanValue $loanValue
              setValue $id age 0
              setValue $id loanPeriod 1
           } else {
              if {[getValue $id loanPeriod] <=  [getValue $id loanDuration]} {
                 #A = P(i(1+i)^n)/((1+i)^n - 1)
                 set P [getValue $id newPrice] 
                 set i [expr [getValue $id loanInterestRate]/100.0]
                 set n [getValue $id loanDuration]
                 set A [expr $P * ($i*pow(1+$i,$n))/(pow(1.0+$i,$n) - 1.0) ]
                 apsimSendMessage "" expenditure [list cost $A] [list interest_paid $A] [list comment "Loan repayments for [getName $id]"]
                 
                 setValue $id loanPeriod [expr 1 + [getValue $id loanPeriod]]
                 if { [getValue $id loanPeriod] >  [getValue $id loanDuration] } {
                     apsimWriteToSummaryFile "Loan for [getName $id] is finished"
                 }
              }
              
           }
        }
     }    
   }  
}

# Finally, load our configuration database and initialise states
machinery:initialise 
set machinery:jobs {}
apsimWriteToSummaryFile "Machinery:\nTractors=[getTractorNames]\nImplements=[getImplementNames]"

# Set the current period of the loan to 1 past its end
foreach id [concat [getTractorIds] [getImplementIds]] {
   setValue $id loanPeriod [expr 1 + [getValue $id loanDuration]]
}
