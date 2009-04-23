#! C:\development\apsim\tcllink\lib\RotationInitialisationUI.tcl
trace remove variable XMLDoc read setXML

package require Tk
package require tdom
package require BWidget

catch {destroy .w}
set w [frame .w]

catch {unset config}
catch {unset check}


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

proc appletInit {} {
   global XMLDoc GlobalXMLDoc config
   set config(gdoc) [dom parse $GlobalXMLDoc]
   set config(gdocroot) [$config(gdoc) documentElement]
   set config(doc) [dom parse $XMLDoc]
   set config(docroot) [$config(doc) documentElement]
   set config(uiscript) [[$config(docroot) selectNodes //uiscript] text]
   set config(initialState) [[$config(docroot) selectNodes //initialState] text]
   set config(simpleLogging) [[$config(docroot) selectNodes //simpleLogging] text]

   set config(states) {}
   foreach node [$config(gdocroot) selectNodes //node] {
      lappend config(states) [getValue $node name]
   }
}   

proc uiInit {w} {
   global config
   label $w.t0 -text Initialisation
   label $w.t1 -text "Initial State"
   
   ComboBox $w.c -editable 0 -textvariable config(initialState) \
                      -values $config(states) \
                      -helptext "Which state to start in" 

   checkbutton $w.sl -text "Log to summary file" -onvalue 1 -offvalue 0 -variable config(simpleLogging)
   DynamicHelp::add $w.sl -text "Log evaluation of management rules to summary file"
   
   grid $w.t0 -row 0 -column 0 -padx 3            -sticky w
   grid $w.t1 -row 1 -column 0 -padx 3 -pady 5    -sticky w
   grid $w.c -row 1 -column 1 -padx 3             -sticky w
   grid $w.sl -row 2 -column 0 -columnspan 2 -padx 3 -sticky w

   grid columnconf $w 2 -weight 1
   grid rowconf    $w 3 -weight 1
}

appletInit 
uiInit $w

grid forget .
grid $w -row 0 -column 0 -sticky nwe
grid columnconf . 0 -weight 1
grid rowconf    . 0 -weight 1

proc setXML {name1 name2 op} {
   global XMLDoc config myName apsuite
   catch {
      set newDoc [dom createDocument [$config(docroot) nodeName]]
      set root [$newDoc documentElement]
      $root setAttribute name [$config(docroot) getAttribute name]

      set new [$newDoc createElement rule]
      set text "source \$apsuite/UserInterface/APSFarm/RotationInitialisation.tcl\nset state \"$config(initialState)\"\n"
      if {$config(simpleLogging)} {
         append text "proc log {msg} {puts \$msg}\n"
      }
      $new appendChild [$newDoc createTextNode $text]
      $new setAttribute name      "$myName - Rotation Initialisation UI1"
      $new setAttribute condition "init"
      $new setAttribute invisible yes
      $root appendChild $new

      set new [$newDoc createElement rule]
      $new appendChild [$newDoc createTextNode "checkSetup\ndoProcess\n"]
      $new setAttribute name      "$myName - Rotation Initialisation UI2"
      $new setAttribute condition "process"
      $new setAttribute invisible yes
      $root appendChild $new

      set new [$newDoc createElement rule]
      $new appendChild [$newDoc createTextNode "exit\n"]
      $new setAttribute name      "$myName - Rotation Initialisation UI3"
      $new setAttribute condition "exit"
      $new setAttribute invisible yes
      $root appendChild $new

      foreach variable [list uiscript initialState simpleLogging] {
         set new [$newDoc createElement $variable]
         $new appendChild [$newDoc createTextNode $config($variable)]
         $root appendChild $new
      }
   } msg
   if {$msg != ""} {
      global errorInfo; tk_messageBox -title "Error" -message "$msg:\n$errorInfo" -type ok
   } else {
      set XMLDoc [$newDoc asXML]
   }
}
trace add variable XMLDoc read setXML
