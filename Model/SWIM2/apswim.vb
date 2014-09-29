module apswimmodule
use registrations

Dim calc_section as string * ( as string * )
parameter (calc_section

Dim crop_section as string * ( as string * )
parameter (crop_section

Dim climate_section as string * ( as string * )
parameter (climate_section =

Dim init_section as string * ( as string * )
parameter (init_section

Dim runoff_section as string * ( as string * )
parameter (runoff_section =

Dim solute_section as string * ( as string * )
parameter (solute_section =

Dim top_boundary_section as string * ( as string * )
parameter (top_boundary_section = "top_

Dim bottom_boundary_section as string * ( as string * )
parameter (bottom_boundary_section = "bottom_

Dim interp_key as string * ( as string * )
parameter (interp_

Dim bypass_flow_section as string * ( as string * )
parameter (bypass_flow_section = "byp

Dim drain_section as string * ( as string * )
parameter (drain_section

Dim m as integer
paramet

Dim mp as integer
parameter (mp=25) ' origin

Dim mv as integer
paramet

Dim swimlogsize as integer
parameter (swimlogsi

Dim nsol as integer
parameter (

Dim effpar as double
parameter (effpar

Dim psi_ll15 as double
parameter (psi_ll15 =
Dim psi_dul as double
parameter (psi_dul

Dim max_table as integer
parameter (max

Dim strsize as integer
parameter (str


' =====================================================================
'     apswim globals
' =====================================================================
type apswimglobals
sequence
Dim potet  ' from met file as double
Dim rain   ' from met file as double
Dim mint as double
Dim maxt as double
Dim radn as double

Dim swimrainnumpairs as integer
Dim swimevapnumpairs as integer
Dim swimsolnumpairs(nsol) as integer

Dim swimraintime (swimlogsize) as double
Dim swimrainamt (swimlogsize) as double
Dim swimeqraintime (swimlogsize) as double
Dim swimeqrainamt (swimlogsize) as double
Dim swimevaptime (swimlogsize) as double
Dim swimevapamt (swimlogsize) as double
Dim swimsoltime (nsol,swimlogsize) as double
Dim swimsolamt (nsol,swimlogsize) as double

Dim td_runoff as double
Dim td_rain as double
Dim td_evap as double
Dim td_pevap as double
Dim td_bypass as double
Dim td_drain as double
Dim td_subsurface_drain as double
Dim td_soldrain(nsol) as double
Dim td_slssof(nsol) as double


Dim t as double
Dim dt as double

Dim won as double
Dim woff as double
Dim wes as double
Dim wesp as double
Dim wex as double
Dim wbp as double
Dim winf as double
Dim h0 as double
Dim wp as double
Dim wp0 as double
Dim wdrn as double

Dim h as double
Dim hold as double
Dim ron as double
Dim roff as double
Dim res as double
Dim resp as double
Dim rex as double
Dim rssf as double

Dim slon (nsol) as double
Dim sloff (nsol) as double
Dim slex (nsol) as double
Dim slbp (nsol) as double
Dim slinf (nsol) as double
Dim slp (nsol) as double
Dim sldrn (nsol) as double

Dim dc(nsol,m) as double
Dim slsur (nsol) as double
Dim cslsur (nsol) as double
Dim rslon (nsol) as double
Dim rsloff (nsol) as double
Dim rslex (nsol) as double

Dim demand_is_met(mv,nsol) as boolean

Dim solute_owners (nsol) as integer

Dim work as double
Dim slwork as double

Dim hmin as double
Dim gsurf as double

Dim day as integer
Dim year as integer
Dim apsim_timestep as double
Dim start_day as integer
Dim start_year as integer
Dim apsim_time as string * 10
Dim run_has_started as boolean


Dim psim as double
Dim psimin(mv) as double
Dim rtp(mv) as double
Dim rt(mv) as double
Dim ctp(mv) as double
Dim ct(mv) as double
Dim slup(mv,nsol) ' this seems a silly declaration as double
'                                    ! from what i see it makes no difference
'                                    ! because it is not used.

Dim crop_names (mv) as string * (strsize)
Dim crop_owners (mv) as integer
Dim num_crops as integer
Dim nveg as integer
Dim root_radius(mv) as double
Dim root_conductance(mv) as double
Dim pep(mv) as double
Dim solute_demand (mv,nsol) as double

Dim crop_cover as double
Dim residue_cover as double
Dim cover_green_sum as double

Dim hbp as double
Dim hbpold as double
Dim qbp as double
Dim qbpd as double
'cnh      double precision slbp0
Dim qslbp (nsol) as double

Dim gf as double

Dim swta(m) as double



Dim crops_found as boolean
Dim psix(mv) as double

end type apswimglobals
' =====================================================================
'     apswim parameters
' =====================================================================
type apswimparameters
sequence
Dim specification_type  ' type of soil specs to be used as integer
Dim evap_source as string * 50
Dim echo_directives as string * 5
Dim salb as double

Dim slmin as double
Dim slmax as double

Dim ll15(0:m) as double
Dim dul(0:m) as double
Dim sat(0:m) as double
Dim a(0:m) as double
Dim  b(0:m) as double
Dim  c(0:m) as double
Dim  psii(0:m) as double

Dim ivap as integer
Dim isbc as integer
Dim itbc as integer
Dim ibbc as integer

Dim solute_names (nsol) as string * (strsize)
Dim extra_solute_supply_flag as string * (strsize)
Dim solute_exclusion_flag as string * (strsize)
Dim num_solutes as integer

Dim dw as double
Dim dtmin as double
Dim dtmax as double
Dim dtmax_sol as double
Dim isol as integer

Dim ersoil as double
Dim ernode as double
Dim errex as double
Dim dppl as double
Dim dpnl as double
Dim slcerr as double

Dim swt as double
Dim slswt as double

'cnh added xbp 19-9-1994
Dim gbp as double
Dim sbp as double
Dim xbp as double
Dim ibp as integer

Dim hm0 as double
Dim hm1 as double
Dim hrc as double
Dim roff0 as double
Dim roff1 as double
Dim g0 as double
Dim g1 as double
Dim grc as double

Dim cslgw(nsol) as double
Dim slupf (nsol) as double
Dim slos (nsol) as double
Dim slsci (nsol) as double
Dim slscr (nsol) as double
Dim dcon (nsol) as double
Dim dthc (nsol) as double
Dim dthp (nsol) as double
Dim disp (nsol) as double

Dim constant_gradient as double
Dim constant_potential as double
Dim crop_table_name (mv) as string * (strsize)
Dim crop_table_psimin(mv) as double
Dim crop_table_root_radius(mv) as double
Dim crop_table_root_con(mv) as double

Dim n as integer

Dim subsurface_drain as string * 3
Dim drain_depth as double
Dim drain_spacing as double
Dim klat as double
Dim drain_radius as double
Dim imperm_depth as double

Dim water_table_depth as double
Dim water_table_conductance as double

end type apswimparameters
' =====================================================================
'     apswim constants
' =====================================================================
type apswimconstants
sequence

Dim min_crit_temp as double
Dim max_crit_temp as double
Dim max_albedo as double
Dim residue_albedo as double

Dim max_bitesize as double
Dim supply_fraction as double
Dim a_to_evap_fact as double
Dim canopy_eos_coef as double

Dim cover_effects as string * 5

Dim negative_conc_warn as double
Dim negative_conc_fatal as double

Dim max_iterations as integer

Dim min_total_root_length as double
Dim default_rain_duration        ' default duration of rainfall (min) as double
Dim default_evap_duration        ' default duration of evaporation (min) as double

end type apswimconstants

'     ! instance variables.
common /instancepointers/ id,g,p,c
save instancepointers
type (apswimglobals),pointer   g
global const p
type (apswimconstants),pointer   c
type (idstype),pointer   id

' =====================================================================



* =
sub  apswim_reset ()
* =
use infrastructure
'implicit none

*+  purpose
*      initialise apswim module

*+  changes
*     <insert here>

*+  constant values
Dim myname as string * ( as string * )
parameter (myname = "apsw

*+  local variables
Dim event_string as string * 40       ' string to output
Dim iost                  ' iostat variable as integer

*- implementation section ----------------------------------
call push_routine (myname)


call apswim_zero_variables ()

call apswim_get_other_variables ()

'     ! notify system that we have initialised

event_string = "initialising "
open report.txt for append as #1
call write_string #1,

'     ! get all constants from constants file
call apswim_read_constants ()

'     ! get all parameters from parameter file

call apswim_read_param ()

call apswim_read_solute_params()
call apswim_read_solsoil_params()
call apswim_read_crop_params()

call apswim_register_solute_outputs()

'     ! set swim defaults - params that are not to be set by user
call apswim_init_defaults ()

'     ! calculate anything swim needs from input parameters
call apswim_init_calc ()

'     ! check all inputs for errors
call apswim_check_inputs()

'     ! initialise solute information
'     !call apswim_init_solute()

call apswim_new_profile_event()

call pop_routine (myname)
close #1:exit sub
close #1
end sub



* =
sub  apswim_read_param ()
* =

use infrastructure
'implicit none

*+  purpose
*      read in all parameters from parameter file.

*+  changes
*     <insert here>

*+  constant values
Dim myname as string * ( as string * )
parameter (myname = "apswim_re

*+  local variables
Dim bypass_flag as string * 5         ' flag for bypass flow (yes/no)
Dim ivap_switch as string * 5         ' flag for vapour flow (yes/no)
Dim node                    ' node counter variable as integer
Dim num_nodes               ' number of specified nodes as integer
Dim numvals                 ' number of values read from file as integer
Dim num_sl as integer
Dim num_psi as integer
Dim num_theta as integer
Dim point as integer
Dim temp_sl  (mp) as double
Dim temp_hkl (mp) as double
Dim temp_hkld (mp) as double
Dim temp_wc (mp) as double
Dim temp_wcd(mp) as double

*- implementation section ----------------------------------
call push_routine (myname)

'        ! ------------- initial soil profile info ---------------


call read_integer_var_optional(
if (numvals = 0) then
    p%specification_type = 1
end if

'        ! read in vol. soil water from parameter file

c      call read_double_array (

'        ! read in minimum log suction from parameter file

call read_double_var (

'        ! read in maximum log suction from parameter file

call read_double_var (

'        ! read in node depths from parameter file

call read_double_array (

p%n = num_nodes - 1

'        ! read in water content as either volumetric content or
'        !                matric potential.

call read_double_array_optional (

call read_double_array_optional (

if ((num_theta > 0) and (num_psi > 0))then
call fatal_error (err_user,

elseif ((num_theta = 0) and (num_psi = 0)) then
call fatal_error (err_user,
else
'        ! one of the two has been supplied - ok
end if


'        ! read in soil type water from parameter file

call read_char_array (

'        ! ---------------- configuration information --------------

'        ! read in bypass flow flag from parameter file

call read_char_var (

'        ! read in runoff flag from parameter file
'        ! ---------------------------------------

call read_integer_var (

'        ! read in surface boundary conditions flag from parameter file
'        ! ------------------------------------------------------------
call read_integer_var (

'        ! read in bottom boundary conditions flag from parameter file
'        ! -----------------------------------------------------------
call read_integer_var (

'        ! read in vapour conductivity flag from parameter file
'        ! ----------------------------------------------------
call read_char_var (
if (ivap_switch = "on") then
    p%ivap = 1
    else
    p%ivap = 0
end if

call read_char_array (

if ((p%num_solutes = 1) and (p%solute_names(1) = "none")) then
'        ! user wants no solutes
    p%num_solutes = 0
    p%solute_names(1) = " "
end if

'     ! read in flag for extra_solute_supply
call read_char_var_optional (

'     ! read in flag for extra_solute_supply
call read_char_var_optional (

'     ! read in flag for echoing incoming messages
call read_char_var_optional (

'     ! read in flag for subsurface drainage
call read_char_var_optional (
if (numvals = 0) then
    p%subsurface_drain = "off"
end if


if (p%specification_type  =  1) then
'        ! read in soil water characteristics for each node
'        !            from parameter file
    for node=0 to  p%n

        if (p%soil_type(node)  <>  interp_key) then

            call read_double_array (

            call read_double_array (

            call read_double_array (

            call read_double_array (

            call read_double_array (

            for point=1 to num_sl
                p%sl(node,point) = temp_sl(point)
                p%wc(node,point) = temp_wc(point)
                p%wcd(node,point) = temp_wcd(point)
                p%hkl(node,point) = temp_hkl(point)
                p%hkld(node,point) = temp_hkld(point)
L90:
                continue
            next point
        next i

        else
    end if

L100:
    continue
next node
elseif (p%specification_type  =  2) then
call read_double_array (
call read_double_array (
call read_double_array (
call read_double_array (

else
call fatal_error(err_user, "unknown specification type")
end if

'        ! ------------- swim calculation parameters -------------

'        ! read in p%dtmin from parameter file

call read_double_var (

'        ! read in p%dtmax from parameter file

call read_double_var (

call read_double_var_optional (

if (numvals <=0) then
'        ! not read in - use p%dtmax as default.
    p%dtmax_sol = p%dtmax
    else
'        ! it was read in - ok
end if

'        ! read in p%ersoil from parameter file

call read_double_var (

'        ! read in p%ernode from parameter file

call read_double_var (

'        ! read in p%errex from parameter file

call read_double_var (

'        ! read in p%dppl from parameter file

call read_double_var (

'        ! read in p%dpnl from parameter file

call read_double_var (

'        ! read in max water increment from parameter file

call read_double_var (

call read_double_var(

call read_double_var(

call read_double_var(


'        ! ------------------ climate information ------------------

call read_char_var (

'        ! read in soil albedo from parameter file

call read_real_var (


if (bypass_flag = "on") then
    p%ibp = 1
'           ! read in bypass flow depth from parameter file

    call read_double_var (

'           ! read in bypass flow conductance from parameter file

    call read_double_var (

'           ! read in bypass flow storage from parameter file

    call read_double_var (
    end sub
