#! /usr/bin/tclsh

# from https://wiki.tcl.tk/15244

 package require uri
 package require base64
 package require html
 
 proc HTTPD {port certfile keyfile userpwds realm handler} {
  if {![llength [info commands Log]]} { proc Log {args} { puts $args } }
  namespace eval httpd [list set handlers $handler]
  namespace eval httpd [list set realm $realm]
  foreach up $userpwds { namespace eval httpd [list lappend auths [base64::encode $up]] }
  namespace eval httpd {
    proc respond {sock code body {head ""}} {
      puts -nonewline $sock "HTTP/1.0 $code ???\nContent-Type: text/html; charset=ISO-8859-1\nConnection: close\nContent-length: [string length $body]\n$head\n$body"
    }
    proc checkauth {sock ip auth} {
      variable auths
      variable realm
      if {[info exist auths] && [lsearch -exact $auths $auth]==-1} {
        respond $sock 401 Unauthorized "WWW-Authenticate: Basic realm=\"$realm\"\n"
        error "Unauthorized from $ip"
      }
    }
    proc handler {sock ip reqstring auth} {
      variable auths
      variable handlers
      checkauth $sock $ip $auth
      array set req $reqstring
      switch -glob $req(path) [concat $handlers [list default { respond $sock 404 "Error" }]]
    }
    proc accept {sock ip port} {
      if {[catch {
        gets $sock line
        set auth ""
        for {set c 0} {[gets $sock temp]>=0 && $temp ne "\r" && $temp ne ""} {incr c} {
          regexp {Authorization: Basic ([^\r\n]+)} $temp -- auth
          if {$c == 30} { error "Too many lines from $ip" }
        }
        if {[eof $sock]} { error "Connection closed from $ip" }
        foreach {method url version} $line { break }
        switch -exact $method {
          GET { handler $sock $ip [uri::split $url] $auth }
          default { error "Unsupported method '$method' from $ip" }
        }
      } msg]} {
        Log "Error: $msg"
      }
      close $sock
    }
  }
  if {$certfile ne ""} {
  package require tls
  ::tls::init \
    -certfile $certfile \
    -keyfile  $keyfile \
    -ssl2 1 \
    -ssl3 1 \
    -tls1 0 \
    -require 0 \
    -request 0
  ::tls::socket -server httpd::accept $port
  } else {
  socket -server httpd::accept $port
  }
 }
 # Generating SSL key is very easy, just use these two commands:
 #  openssl genrsa -out server-private.pem 1024
 #  openssl req -new -x509 -key server-private.pem -out server-public.pem -days 365 
 # Or just don't specify the key files to use HTTP instead of HTTPS
 # HTTPD 9005 "" "" {mike:pwd} {AuthRealm} {
 #HTTPD 9005 server-public.pem server-private.pem {mike:pwd you:yourpwd} {AuthRealm} {
 #       "" {
 #         respond $sock 200 {Want to know the <a href="/time">time</a>?}
 #       }
 #       "time" {
 #         respond $sock 200 "Time: [clock format [clock seconds]]" "Refresh: 6;URL=/\n"
 #       }
 #}
 
exec echo Idle > /tmp/buildStatus
exec echo "" >  /tmp/buildLog
proc readFile filename {
    set fp [open $filename]
    set res [read $fp [file size $filename]]
    close $fp
    return [string trim $res] 
}

HTTPD 2500 "" "" {} {AuthRealm} {
        "build*" {
          if {[readFile /tmp/buildStatus] == "Idle"} {
            set rev [lindex [split $req(path) /] 1]
            if {![string is int $rev]} {set rev ""}
            if {[ catch {exec /usr/local/bin/BuildApsim.sh $rev  > /tmp/buildLog 2>@1 &} msg ]} {
               respond $sock 200 "Error - $msg"
            } else {
               respond $sock 200 {OK}
            }   
          } else {
            respond $sock 200 {Busy}
          }
        }
        "log" {
          respond $sock 200 [readFile /tmp/buildLog]
        }
        "status" {
          respond $sock 200 [readFile /tmp/buildStatus]
        }
        "time" {
          respond $sock 200 "Time: [clock format [clock seconds]]" "Refresh: 6;URL=/\n"
        }
 }
 
 vwait forever
