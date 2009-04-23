#! C:\development\apsim\manager\lib\RotationInitialisation.tcl
# Support procedures for (single paddock) rotations

# Return the current state of the system
proc currentState {} {
   global state 
   return $state   
}

# Default log option is to do nothing. May be overridden later.
proc log {msg} { }

############### The decision routine. 
# Return the score for planting "what"
proc checkRules {} {
  log "State is [currentState]. (esw=[format %.0f [apsimGet esw]])"

  set bestScore -1.0; set bestTarget {}
  foreach arc [stateGraph arcs -out [currentState]] {
     set expr {}
     set target [stateGraph arc target $arc]
     foreach rule [stateGraph arc get $arc rules] {
        set value [uplevel #0 expr $rule]
        log "target=$target,rule=$rule, value=$value"
        lappend expr $value
     }  
     set score [expr [join $expr "*"]]
     if {$score > $bestScore} {
        set bestScore $score
        set bestTarget $target
     }
  }
  return [list $bestTarget $bestScore]
}

# Change state to another
proc changeState {newState} {
   global state
   set arcs {}
   foreach arc [stateGraph arcs -out [currentState]] {
      if {[stateGraph arc target $arc] == "$newState" } {
         lappend arcs $arc
      }
   }   
   if {[llength $arcs] == 0} {error "No paths from [currentState] to $newState!"}
   if {[llength $arcs] > 1} {
      # More than 1 path - evaluate best option
      set bestScore -1.0; set bestArc {}
      foreach arc $arcs {
         set expr {}
         foreach rule [stateGraph arc get $arc rules] {
            set value [uplevel #0 expr $rule]
            lappend expr $value
         }
         set score [expr [join $expr "*"]]
         if {$score > $bestScore} {
            set bestScore $score
            set bestArc $arc
         }
      }  
      if {$bestScore > 0.0} {
         apsimWriteToSummaryFile "Taking arc $bestArc from [currentState] to $newState"
         set arc $bestArc
      } else {
         error "No viable paths from [currentState] to $newState! (candidates are $arcs)"
      }
   } else {
      # Only 1 path - take it.
      set arc $arcs
   }
   foreach action [stateGraph arc get $arc actions] { uplevel #0 $action }
   set state $newState
   log "changeState $newState"
}

proc doProcess {} {
   log "[apsimGet day],[apsimGet year]"
   set more 1
   while {$more} {
      set more 0
      foreach  {target score} [checkRules] {break}
      if {$score > 0} {changeState $target; set more 1}
   }
}

# A flag for initialisation procedures.
# NB. some funny business here. Delay opening report file until the 1st day of 
# simulation as it's hard to know how many paddocks are present at init2 time.
set setupDone 0
proc checkSetup {} {
   global setupDone
   if {!$setupDone} {
     log "changeState [currentState]"
     set setupDone 1
  }
}
