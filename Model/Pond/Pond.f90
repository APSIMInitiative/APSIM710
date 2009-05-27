module PondModule
   use ComponentInterfaceModule
   use registrations
   use DataTypes
     
!  ====================================================================
!  Pond constants
!  ====================================================================

!  Short Description:
!  Constant values

!  Notes:

!  attributes:

 
!  ----------------------- Declaration section ------------------------

   Use ConstantsModule

   integer    max_residues                ! Maximum number of residues
   parameter (max_residues = 100)

   integer    max_solutes       ! Maximum number of solutes in the soil
   parameter (max_solutes = 20)

   integer    max_coeffs       ! Maximum number of coefficients in a table
   parameter (max_coeffs = 10)

   integer    module_name_size      ! maximum length of module name
   parameter (module_name_size = 30)

   integer    max_sources      ! maximum number of water sources allowable
   parameter (max_sources = 10)

   integer    max_layer                   ! Maximum number of layers
   parameter (max_layer = 100)
   
   integer    max_part              ! number of plant parts
   parameter (max_part = 5)
   

!  Global variables

   type PondGlobals
      sequence
      real    pond_digital                         ! 1-0 equivalent of g%pond_active
      real    rain                                 ! precipitation (mm/d)
      real    radn                                 ! solar radiation (mj/m^2/day)
      real    mint                                 ! minimum air temperature (oC)
      real    maxt                                 ! maximum air temperature (oC)
      real    pond_depth                           ! depth of ponded layer today (mm)
      real    pond_depth_yest                      ! depth of ponded layer yesterday (mm)
      real    pond_evap                            ! evaporation from pond today (mm)
      real    ampef                                ! hourly floodwater evaporation, initialised as 0.38 * total daily evap
      real    pond_no3                             ! mineral N as nitrate in pond (kg/ha)
      real    pond_nh4                             ! mineral N as ammonium in pond (kg/ha)
      real    pond_nh3                             ! mineral N as ammonia in pond (kg/ha)
      real    pond_no3_conc                        ! concentration of mineral N as nitrate in pond ()
      real    pond_nh4_conc                        ! concentration of mineral N as ammonium in pond ()
      real    pond_nh3_conc                        ! concentration of mineral N as ammonia in pond, able to be volatilised ()
      real    pond_urea                            ! urea in pond (kg/ha)
      real    pond_hum_C                           ! humic C in pond (kg/ha)
      real    pond_biom_C                          ! biom C in pond (kg/ha)
      real    pab_mass                             ! mass of algae in pond (kg/ha) (capped at 500kg/ha)
      real    algal_turnover                       ! mass of algae turned over each day and added into surfaceom pool (kg/ha)
      real    rlai                                 ! rice leaf area index ()
      real    rep                                  ! rice evapotranspiration (mm)
      real    ftmax                                ! maximum pond temperature today (oC)
      real    ftmin                                ! minimum pond temperature today (oC)
      real    ftmean                               ! mean pond temperature today (oC)
      real    ftmin_yest                           ! maximum pond temperature yesterday (oC)
      real    ftmax_yest                           ! minimum pond temperature yesterday (oC)
      real    ftmean_yest                          ! mean pond temperature yesterday (oC)
      real    oc1                                  ! organic carbon in soil layer 1 (%)
      real    dlayer1                              ! depth of soil layer 1 (mm)
      real    bd1                                  ! bulk density of soil layer 1 (g/cm3)
      real    algact                               ! pond algal activity (0-1 factor)
      real    fni                                  ! nitrogen factor affecting algal growth (0-1)
      real    fpi                                  ! phosphorus factor affecting algal growth (0-1)
      real    ali                                  ! light factor affefcting algal growth
      real    fti                                  ! temperature factor affecting algal growth
      real    yalga                                ! 
      real    algfix                               !
      real    tmpfon                               ! pond organic nitrogen (kg/ha)
      real    fuhydr                               ! floodwater urea hydrolysis this timestep
      real    totuh                                ! cumulative urea hydrolised over the 12 daily timesteps
      real    fph                                  ! floodwater ph for timestep 
      real    pot_hydrolysis                       ! maximum potential rate of pond urea hydrolysis, soil OC limited 
      real    totaml                               ! total ammonia loss from pond (kg/ha)
      real    amloss                               ! daily ammonia loss from pond (kg/ha)
      real    runoff                               ! runoff over the bund (mm)
      real    infiltration                         ! daily infiltration into the top layer of the soil (mm)
      real    no3_infiltrated(max_layer)
      real    nh4_infiltrated(max_layer)
      real    urea_infiltrated(max_layer)
      
            
      integer year                                 ! year
      integer day                                  ! day of year
      integer num_residues
      integer ponded_days                          ! consecutive ponded days (ie g%pond_depth > 0)
      integer kill                                 !
      character   pond_active*10                   ! variable = yes or no depending on whether a pond is present in simulation
      character   residue_name(max_residues)*(Max_module_name_size)
      character   residue_type(max_residues)*(Max_module_name_size)
                                                   ! list of module types
      real         pot_C_decomp(max_residues)      ! Potential residue C decomposition (kg/ha)
      real         pot_N_decomp(max_residues)      ! Potential residue N decomposition (kg/ha)
      real         pot_P_decomp(max_residues)      ! Potential residue P decomposition (kg/ha)
      real         dlt_res_C_decomp(max_residues)  ! residue C decomposition (kg/ha)
      real         dlt_res_N_decomp(max_residues)  ! residue N decomposition (kg/ha)
      real         dlt_res_nh4_min                 ! Net Residue NH4 mineralisation (kg/ha)
      real         dlt_res_no3_min                 ! Net Residue NO3 mineralisation (kg/ha)
      real         dlt_res_c_atm(max_residues)     ! carbon from residues lost to atmosphere (kg/ha)
      real         dlt_res_c_biom(max_residues)    ! carbon from residues to biomass
      real         dlt_res_c_hum(max_residues)     ! carbon from residues to humic
                                          
      logical immobil                              ! is there immobilisation of N by algae in pond
      logical akill                                ! algal kill
                                          

   end type PondGlobals
   !  ====================================================================
   type PondParameters
      sequence
         real    mwcon (max_solutes)                    ! impermeable soil layer indicator

   end type PondParameters
   !  ====================================================================
   type PondConstants
      sequence
      real         ef_res                 ! fraction of residue C mineralized
                                          ! retained in system (0-1)
      real         fr_res_biom            ! fraction of retained residue C
                                          ! transferred to biomass (0-1)
      real         mcn                    ! C:N ratio of microbes ()
      real         pond_cn                ! c:N ratio in pond (used as the soil_cn is used in SoilN2)
      real         maxrate_pab            ! maximum rate at which Phostosynthetic Algal Biomass (pab)can accumulate
                                          ! in the floodwater (kg/ha/day)
      real         pab_p_index            ! a modifying factor for algal production based on availability of P
                                          ! Godwin & Singh (1998) suggested two values 1.0 for no limits, 0.5 for limited
      real         water_albedo           ! Albedo of pond water                                          
      integer      algae_present          ! an indicator of whether algae is present in environment, 
                                          ! hence whether plays a role in pond processes
      real         amlos_fudge_factor     ! a factor applied to the calculated amlos (amonia loss) 
      real         solute_infiltration_rate  !  the fraction of solutes (urea nh4) which moves from pond to soil daily

   end type PondConstants
   !  ====================================================================

   ! instance variables.
   common /InstancePointers/ ID,g,p,c
   save InstancePointers
   type (PondGlobals),pointer :: g
   type (PondParameters),pointer :: p
   type (PondConstants),pointer :: c
   type (IDsType), pointer :: ID


   contains

!  ===========================================================
subroutine Pond_read_parameters ()
!  ===========================================================

   Use Infrastructure
   implicit none
 
!+ Purpose
!  input initial values from parameter file.

!+ Mission Statement
!  Read Parameters

!+ Changes
!  dsg 150603  built

!+ Constant Values
   character  my_name*(*)            ! name of this module
   parameter (my_name = 'Pond_read_parameters')
!
    character  section_name*(*)
    parameter (section_name = 'parameters')

!+ Local Variables
   integer    i                     ! simple counter
   integer    numvals               ! number of values returned
   character  source_type*100       ! local variable for source type
   character  dummy*100             ! first half of solute concatenation
   character  default_name*100      ! concatenated parameter name for initial solute concentration

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   call write_string (new_line//'   - Reading Pond Parameters')


   call pop_routine (my_name)
   return
end subroutine


!  ====================================================================
subroutine Pond_read_constants ()
!  ====================================================================
   Use Infrastructure
   implicit none

!+ Purpose
!  Read in all coefficients from coefficient file.

!+ Mission Statement
!  Read Constants from Ini file

!+ Changes
!  dsg 150603  built

!+ Constant Values
   character  my_name*(*)          ! name of this procedure
   parameter (my_name = 'Pond_read_constants')
   character  section_name*(*)
   parameter (section_name = 'constants')

!+ Local Variables
   integer numvals                 ! number of values read from file
   character string*100            ! message string
   real cooper

!- Implementation Section ----------------------------------
   call push_routine (my_name)

   call write_string (new_line//'    - Reading Pond constants')

!   call read_real_var(section_name, 'mcn', '()', c%mcn, numvals, 1.0, 50.0)

!   call read_real_var (section_name, 'ef_res', '()', c%ef_res, numvals, 0.0, 1.0)

!   call read_real_var (section_name, 'fr_res_biom', '()', c%fr_res_biom, numvals, 0.0, 1.0)
   
!   call read_real_var (section_name, 'pond_cn', '()', c%pond_cn, numvals, 5.0, 30.0)
   
!   call read_real_var (section_name, 'maxrate_pab', '()', c%maxrate_pab, numvals, 0.0, 100.0)

!   call read_real_var (section_name, 'pab_p_index', '()', c%pab_p_index, numvals, 0.0, 100.0)

!   call read_real_var (section_name, 'water_albedo', '()', c%water_albedo, numvals, 0.0, 100.0)

!   call read_integer_var (section_name, 'algae_present', '()', c%algae_present, numvals, 0, 1)

!   call read_real_var (section_name, 'amlos_fudge_factor', '()', c%amlos_fudge_factor, numvals, 0.0, 100.0)

!   call read_real_var (section_name, 'solute_infiltration_rate', '()', c%solute_infiltration_rate, numvals, 0.0, 100.0)

   c%ef_res = 0.4        ! fraction of residue C mineralized
                    ! retained in system (0-1)

   c%fr_res_biom = 0.9   ! fraction of retained residue C
                    ! transferred to biomass (0-1)

   c%mcn = 8.0           ! C:N ratio of microbes ()

   c%pond_cn = 30        ! ???guess pond carbon-nitrogen ratio

   c%maxrate_PAB = 20    ! maximum rate of algae growth in kg/ha/day 
                    ! -according to Roger (1996) PAB should reach a max of 500 kg/ha

   c%PAB_p_index = 1.0   ! a modifying factor for algal production based on availability of P
                    ! Godwin & Singh (1998) suggested two values 1.0 for no limits, 0.5 for limited 
                    
   c%water_albedo = 0.05  ! albedo of pond water  

   c%algae_present = 1    ! an indicator of whether algae is present in environment, 

   c%amlos_fudge_factor = 0.5 !1.0    !  a factor applied to the daily calculated amlos (ammonia (NH3-) loss) 

   c%solute_infiltration_rate = 0.3


   call pop_routine (my_name)
   return
end subroutine




!  ===========================================================
subroutine Pond_ONprocess ()
!  ===========================================================
   Use Infrastructure
   implicit none
 
!+ Purpose
!  Daily calculations

!+ Mission Statement
!  Perform all APSIM Timestep calculations

!+ Changes
!  dsg 150603  built

!+ Calls

!+ Constant Values
   character  my_name*(*)           ! this subroutine name
   parameter (my_name = 'Pond_process')

!+ Local Variables
    integer i, k                      ! simple counters
    integer timestep                           ! hourly timestep counter
    character*200  err_string
    real htmfac                                ! hourly floodwater temperature factor
    real ftemp                                 ! floodwater temperature for the given timestep

!- Implementation Section ----------------------------------

   call push_routine (my_name)
  
   
   if (g%pond_active.eq.'yes') then

       call Pond_get_daily_variables ()


!  Mineralise some residues in the pond
       call Pond_min_residues (g%dlt_res_C_decomp,g%dlt_res_N_decomp,g%dlt_res_c_biom,g%dlt_res_c_hum,g%dlt_res_c_atm,g%dlt_res_nh4_min,g%dlt_res_no3_min)


       ! these are getable by soiln2 - we will not maintain pond humic & biom pools, 
       ! they'll go straight into top soil layer
       g%pond_hum_c = sum (g%dlt_res_c_hum(:))

       g%pond_biom_c = sum (g%dlt_res_c_biom(:))


       ! update pond mineral N

       g%pond_nh4 = g%pond_nh4 + g%dlt_res_nh4_min
       g%pond_no3 = g%pond_no3 + g%dlt_res_no3_min
   
! on a daily basis the pond module needs to 
!     - hydrolise urea
!     - volatilise some amonia (includes calculating pond temperature & ph with several timesteps)
!     - move solutes (NH4, NO3, urea) between pond and soil, or vice-versa
!     - grow some Photosynthetic Algal Biomass (pab)


       call Pond_temperature_balance()
       
       call Pond_calculate_daily_variables()     ! including pab growth factors
       
       call Pond_grow_pab ()
       
       call Pond_check_runoff_N ()


! Do the following steps using 12 daily timesteps....
   
       do timestep = 1, 12

           k    = 7 - timestep
           if (timestep .gt. 6) then
             k = timestep - 6
           endif

           ! dsg 291008
           ! ********** calculate a few bits and pieces needed for the subsequent subroutines *************
              !  apply a sinusoidal curve to g%ftmin & g%ftmax, and others to caluclate the values for each timestep as f(timestep)
              !  Calculate ph with each timestep

           ! hourly floodwater temperature factor (Godwin & Singh 1998, CERES-Rice)
                htmfac = 0.931 + 0.114*k - 0.0703*k**2 + 0.0053*k**3
           !
           ! first approximation of floodwater temperature for this timestep
                ftemp = g%ftmin + htmfac*(g%ftmax + 2.0 - g%ftmin)
                if (timestep .eq. 13) then
                   ftemp = g%ftmin
                endif
            

            call Pond_hydrolise_urea(ftemp)
            
       
            call Pond_ph_balance(timestep)
       

            call Pond_volatilise_ammonia(timestep, ftemp)
       
            
      end do           

      call Pond_move_solutes_down()     ! move solutes into soil by the processes of mass-flow and adsorption
            
            
       
 
   else
    !   there is no pond, do nothing during process
   endif
       

   call pop_routine (my_name)
   return
end subroutine




!  ====================================================================
subroutine Pond_send_my_variable (variable_name)
!  ====================================================================
   Use Infrastructure
   implicit none

!+ Sub-Program Arguments
   character variable_name*(*)      ! (input) variable name to search for

!+ Purpose
!  return the value of a variable in return_string.  used to return
!  values of variables requested by other modules.

!+ Notes
!  a flag is set if any of the totals is requested.  the totals are
!  reset during the next process phase when this happens.

!+ Mission Statement
!  Send Value of Requested Variable

!+ Changes
!  dsg 150603  built

!+ Constant Values
   character  my_name*(*)           ! name of subroutine
   parameter (my_name = 'Pond_send_my_variable')

!+ Local Variables
   integer    solnum           !   solute number

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   if (variable_name .eq. 'pond_active') then
       call respond2get_char_var (variable_name,'()', g%pond_active)

   elseif (variable_name .eq. 'pond_digital') then
      call respond2get_real_var (variable_name,'()', g%pond_digital)

   elseif (variable_name .eq. 'consecutive_ponded_days') then
      call respond2get_integer_var (variable_name,'()', g%ponded_days)

   elseif (variable_name .eq. 'pond_no3') then
      call respond2get_real_var (variable_name,'()', g%pond_no3)

   elseif (variable_name .eq. 'pond_nh4') then
      call respond2get_real_var (variable_name,'()', g%pond_nh4)

   elseif (variable_name .eq. 'pond_urea') then
      call respond2get_real_var (variable_name,'()', g%pond_urea)

   elseif (variable_name .eq. 'pond_nh3') then
      call respond2get_real_var (variable_name,'()', g%pond_nh3)

   elseif (variable_name .eq. 'pond_no3_conc') then
      call respond2get_real_var (variable_name,'()', g%pond_no3_conc)

   elseif (variable_name .eq. 'pond_nh4_conc') then
      call respond2get_real_var (variable_name,'()', g%pond_nh4_conc)

   elseif (variable_name .eq. 'pond_nh3_conc') then
      call respond2get_real_var (variable_name,'()', g%pond_nh3_conc)

   elseif (variable_name .eq. 'pab_mass') then
      call respond2get_real_var (variable_name,'()', g%pab_mass)

   elseif (variable_name .eq. 'algal_turnover') then
      call respond2get_real_var (variable_name,'()', g%algal_turnover)

   elseif (variable_name .eq. 'pond_hum_c') then
      call respond2get_real_var (variable_name,'()', g%pond_hum_c)

   elseif (variable_name .eq. 'pond_biom_c') then
      call respond2get_real_var (variable_name,'()', g%pond_biom_c)

   elseif (variable_name .eq. 'ftmax') then
      call respond2get_real_var (variable_name,'()', g%ftmax)

   elseif (variable_name .eq. 'ftmin') then
      call respond2get_real_var (variable_name,'()', g%ftmin)

   elseif (variable_name .eq. 'ftmean') then
      call respond2get_real_var (variable_name,'()', g%ftmean)

   elseif (variable_name .eq. 'ftmax_yest') then
      call respond2get_real_var (variable_name,'()', g%ftmax_yest)

   elseif (variable_name .eq. 'ftmin_yest') then
      call respond2get_real_var (variable_name,'()', g%ftmin_yest)

   elseif (variable_name .eq. 'ftmean_yest') then
      call respond2get_real_var (variable_name,'()', g%ftmean_yest)

   elseif (variable_name .eq. 'pond_depth') then
      call respond2get_real_var (variable_name,'()', g%pond_depth)

   elseif (variable_name .eq. 'pond_evap') then
      call respond2get_real_var (variable_name,'()', g%pond_evap)

   elseif (variable_name .eq. 'rlai_from_pondmodule') then
      call respond2get_real_var (variable_name,'()', g%rlai)

   elseif (variable_name .eq. 'rep') then
      call respond2get_real_var (variable_name,'()', g%rep)

   elseif (variable_name .eq. 'oc1') then
      call respond2get_real_var (variable_name,'()', g%oc1)

   elseif (variable_name .eq. 'dlayer1') then
      call respond2get_real_var (variable_name,'()', g%dlayer1)

   elseif (variable_name .eq. 'bd1') then
      call respond2get_real_var (variable_name,'()', g%bd1)

   elseif (variable_name .eq. 'algact') then
      call respond2get_real_var (variable_name,'()', g%algact)

   elseif (variable_name .eq. 'fni') then
      call respond2get_real_var (variable_name,'()', g%fni)

   elseif (variable_name .eq. 'fpi') then
      call respond2get_real_var (variable_name,'()', g%fpi)

   elseif (variable_name .eq. 'fti') then
      call respond2get_real_var (variable_name,'()', g%fti)

   elseif (variable_name .eq. 'ali') then
      call respond2get_real_var (variable_name,'()', g%ali)

   elseif (variable_name .eq. 'maxrate_pab') then
      call respond2get_real_var (variable_name,'()', c%maxrate_pab)

   elseif (variable_name .eq. 'yalga') then
      call respond2get_real_var (variable_name,'()', g%yalga)

   elseif (variable_name .eq. 'algfix') then
      call respond2get_real_var (variable_name,'()', g%algfix)

   elseif (variable_name .eq. 'tmpfon') then
      call respond2get_real_var (variable_name,'()', g%tmpfon)

   elseif (variable_name .eq. 'totuh') then
      call respond2get_real_var (variable_name,'()', g%totuh)

   elseif (variable_name .eq. 'fph') then
      call respond2get_real_var (variable_name,'()', g%fph)

   elseif (variable_name .eq. 'pot_hydrolysis') then
      call respond2get_real_var (variable_name,'()', g%pot_hydrolysis)

   elseif (variable_name .eq. 'totaml') then
      call respond2get_real_var (variable_name,'()', g%totaml)

   elseif (variable_name .eq. 'amloss') then
      call respond2get_real_var (variable_name,'()', g%amloss)

   elseif (variable_name .eq. 'dlt_res_nh4_min') then
      call respond2get_real_var (variable_name,'()', g%dlt_res_nh4_min)

   elseif (variable_name .eq. 'dlt_res_no3_min') then
      call respond2get_real_var (variable_name,'()', g%dlt_res_no3_min)

   elseif (variable_name .eq. 'kill') then
      call respond2get_integer_var (variable_name,'()', g%kill)

   elseif (variable_name .eq. 'dlt_res_C_decomp') then
      call respond2get_real_array (variable_name,'(kg/ha)', g%dlt_res_C_decomp, g%num_residues)

   elseif (variable_name .eq. 'dlt_res_N_decomp') then
      call respond2get_real_array (variable_name,'(kg/ha)', g%dlt_res_N_decomp, g%num_residues)
      
   elseif (variable_name .eq. 'dlt_res_C_atm') then
      call respond2get_real_array (variable_name,'(kg/ha)', g%dlt_res_C_atm, g%num_residues)

   elseif (variable_name .eq. 'dlt_res_C_biom') then
      call respond2get_real_array (variable_name,'(kg/ha)', g%dlt_res_C_biom, g%num_residues)

   elseif (variable_name .eq. 'dlt_res_C_hum') then
      call respond2get_real_array (variable_name,'(kg/ha)', g%dlt_res_C_hum, g%num_residues)

   elseif (variable_name .eq. 'infiltration_pond_calc') then
      call respond2get_real_var (variable_name,'()', g%infiltration)

   elseif (variable_name .eq. 'no3_infiltrated') then
      call respond2get_real_array (variable_name,'(kg/ha)', g%no3_infiltrated, max_layer)

   elseif (variable_name .eq. 'nh4_infiltrated') then
      call respond2get_real_array (variable_name,'(kg/ha)', g%nh4_infiltrated, max_layer)

   elseif (variable_name .eq. 'urea_infiltrated') then
      call respond2get_real_array (variable_name,'(kg/ha)', g%urea_infiltrated, max_layer)

   else
      call message_unused ()
   endif


   call pop_routine (my_name)
   return
end subroutine


!  ===========================================================
subroutine Pond_zero_variables ()
!  ===========================================================
   Use Infrastructure
   implicit none

!+ Purpose
!  zero variables & arrays

!+ Mission Statement
!  Zero Variables

!+ Changes
!  dsg 150603  built

!+ Constant Values
   character  my_name*(*)           ! module name
   parameter (my_name  = 'Pond_zero_variables')

!- Implementation Section ----------------------------------

   call push_routine (my_name)

!  ====================================================================
! Globals
      g%pond_active = 'no'
      g%pond_digital = 0.0
      g%pond_no3 = 0.0
      g%pond_nh4 = 0.0
      g%pond_urea = 0.0
      g%rlai = 0.0
      g%ponded_days = 0.0
      g%ftmax = 0.0
      g%ftmin = 0.0
      g%ftmean = 0.0
      g%ftmax_yest = 0.0
      g%ftmin_yest = 0.0
      g%ftmean_yest = 0.0
! ====================================================================
! Parameters

! ====================================================================
! Constants


! =====================================================================


   call pop_routine (my_name)
   return
end subroutine


!  ===========================================================
subroutine Pond_zero_event_data ()
!  ===========================================================
   Use Infrastructure
   implicit none

!+ Purpose
!  Zero information describing event data from other modules

!+ Mission Statement
!  Zero information describing event data from other modules

!+ Changes
!  dsg 150603  built

!+ Constant Values
   character  my_name*(*)           ! module name
   parameter (my_name  = 'Pond_zero_event_data')

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   g%year = 0                           ! year
   g%day  = 0                           ! day of year
   g%rain = 0.0                         ! precipitation (mm/d)
   g%radn = 0.0                         ! solar radiation (mj/m^2/day)
   g%mint = 0.0                         ! minimum air temperature (oC)
   g%maxt = 0.0                         ! maximum air temperature (oC)

   call pop_routine (my_name)
   return
end subroutine

!  ===========================================================
subroutine Pond_zero_daily_variables ()
!  ===========================================================
   Use Infrastructure
   implicit none

!+ Purpose
!  zero variables & arrays

!+ Mission Statement
!  Zero Daily Variables

!+ Changes
!  dsg 150603  built

!+ Constant Values
   character  my_name*(*)           ! module name
   parameter (my_name  = 'Pond_zero_daily_variables')

!+ Local Variables
   integer layer                    ! soil layer number counter
   integer solnum                   ! solute number counter

!- Implementation Section ----------------------------------

   call push_routine (my_name)

     g%dlt_res_C_decomp(:)  = 0.0
     g%dlt_res_N_decomp(:)  = 0.0
     g%dlt_res_nh4_min      = 0.0
     g%dlt_res_no3_min      = 0.0
     g%dlt_res_c_atm(:)     = 0.0
     g%dlt_res_c_biom(:)    = 0.0
     g%dlt_res_c_hum(:)     = 0.0
     g%pond_hum_c           = 0.0
     g%pond_biom_c          = 0.0
     g%fni = 0.0
     g%ali = 0.0
     g%fti = 0.0
     g%fpi = 0.0
     g%algact = 0.0
     g%totuh = 0.0
     g%amloss = 0.0

   call pop_routine (my_name)
   return
end subroutine


!  ====================================================================
subroutine Pond_sum_report ()
!  ====================================================================
   Use Infrastructure
   implicit none

!+ Purpose
!  <insert here>

!+ Mission Statement
!  Report Pond module summary details

!+ Changes
!  dsg 150603  built

!+ Constant Values
   character  my_name*(*)           ! name of current procedure
   parameter (my_name = 'Pond_sum_report')

!+ Local Variables
   integer    i                     ! simple counter
   character  line*100              ! temp output record
   character  report_string*200     ! string message sent to summary file

!- Implementation Section ----------------------------------
   call push_routine (my_name)

   call write_string (new_line)

   write(report_string,*)'Pond Module Operational'
   call write_string (report_string)


   call pop_routine (my_name)
   return
end subroutine


!  ===========================================================
subroutine Pond_init ()
!  ===========================================================
   Use Infrastructure
   implicit none

!+ Purpose
!  input initial values from soil water parameter files.

!+ Mission Statement
!  

!+ Changes
!  dsg 150603  built

!+ Constant Values
   character  my_name*(*)           ! name of subroutine
   parameter (my_name  = 'Pond_init')

!- Implementation Section ----------------------------------

   call push_routine (my_name)
   ! zero pools

   ! Get all coefficients from file

   call Pond_read_parameters ()
   call Pond_read_constants ()

   call pop_routine (my_name)
   return
end subroutine


!  ===========================================================
subroutine Pond_season_init ()
!  ===========================================================
   Use Infrastructure
   implicit none

!+ Purpose
!  input initial values for the start of each ponded season.

!+ Mission Statement
!  

!+ Changes
!  dsg 150603  built

!+ Constant Values
   character  my_name*(*)           ! name of subroutine
   parameter (my_name  = 'Pond_season_init')

!+ Local variables
      real        foci              !

!- Implementation Section ----------------------------------

   call push_routine (my_name)

      g%fni     = 0.1
      g%fpi     = 0.5
      g%immobil = .false.
      g%akill   = .false.
      
      g%pab_mass = 0.0
      g%yalga   = 0.1
      g%algfix  = 0.0
      g%kill = 0

      ! Initialize Algal Activity
      foci   = g%oc1 / 3.0
      foci   = min(foci, 1.0)
      g%algact = min(0.2, foci)


   call pop_routine (my_name)
   return
end subroutine

!  ===========================================================
subroutine Pond_ONtick (variant)
!  ===========================================================
   Use Infrastructure
   implicit none

   integer, intent(in) :: variant

!+ Purpose
!  Update internal time record and reset daily state variables.

!+ Mission Statement
!  Update internal time record and reset daily state variables.

!+ Changes
!  dsg 150603  built

!+ Local Variables
   type(timeType) :: tick

!+ Constant Values
   character*(*) myname               ! name of current procedure
   parameter (myname = 'Pond_ONtick')

!- Implementation Section ----------------------------------
   call push_routine (myname)

   call unpack_time(variant, tick)
   call jday_to_day_of_year(dble(tick%startday), g%day, g%year)

   call pop_routine (myname)
   return
end subroutine
!  ===========================================================
subroutine Pond_ONnewmet (variant)
!  ===========================================================
   Use Infrastructure
   implicit none

   integer, intent(in) :: variant
!+ Purpose
!  Get new met data

!+ Mission Statement
!  Get new met data

!+ Changes
!  dsg 150603  built

!+ Local Variables
   type(newmetType) :: newmet
   integer numvals

!+ Constant Values
   character*(*) myname               ! name of current procedure
   parameter (myname = 'Pond_ONnewmet')

!- Implementation Section ----------------------------------
   call push_routine (myname)

   call unpack_newmet(variant, newmet)

   g%radn = newmet%radn
   g%maxt = newmet%maxt
   g%mint = newmet%mint
   g%rain = newmet%rain

   call pop_routine (myname)
   return
end subroutine

!     ===========================================================
subroutine Pond_ONPotentialResidueDecompositionCalculated(variant)
!     ===========================================================
   Use Infrastructure
   implicit none
!+  Purpose
!     Get information of potential residue decomposition

!+  Mission Statement
!     Get information of potential residue decomposition

!+  Sub-Program Arguments
   integer, intent(in out) :: variant

!+  Local Variables
   character*200  message, err_string
   integer numvals
   integer residue                ! simple som counter
   integer num_som                ! number of som elements provided in event
   type (SurfaceOrganicMatterDecompType)::SurfaceOrganicMatterDecomp

!+  Constant Values
   character*(*) myname               ! name of current procedure
   parameter (myname ='Pond_ONPotentialResidueDecompositionCalculated')

!- Implementation Section ----------------------------------
   call push_routine (myname)
   call unpack_SurfaceOrganicMatterDecomp(variant,SurfaceOrganicMatterDecomp)

   do residue = 1,g%num_residues
      g%residue_name(residue)=SurfaceOrganicMatterDecomp%pool(residue)%name
      g%residue_type(residue)=SurfaceOrganicMatterDecomp%pool(residue)%OrganicMatterType
      g%pot_C_decomp(residue)=SurfaceOrganicMatterDecomp%pool(residue)%FOM%C
      g%pot_n_decomp(residue)=SurfaceOrganicMatterDecomp%pool(residue)%FOM%N
   ! this P decomposition is needed to formulate data required by SOILP - struth, this is very ugly
      g%pot_p_decomp(residue)=SurfaceOrganicMatterDecomp%pool(residue)%FOM%P
   end do



   call pop_routine (myname)
   return
end subroutine


!     ===========================================================
subroutine pond_ActiveCheck ()
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Purpose
!      Get the values of 'pond' from water-balance module to check wether POND is active

!+  Mission Statement
!     G

!+  Constant Values
   character  my_name*(*)
   parameter (my_name='pond_ActiveCheck')

!+  Local Variables
   character  err_string*80         ! Error message string
   integer      numvals             ! number of values returned
   real         temp, temp2
   real         pond_yesterday 
   real         old_pab  
   real         count_dry           ! number of consecutive days with no ponding    

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   call get_real_var (unknown_module, 'pond', '(mm)', temp, numvals, 0.0, 1000.0)
   call get_real_var (unknown_module, 'pond_evap', '(mm)', temp2, numvals, 0.0, 1000.0)

   if(numvals.eq.0) then
      err_string = 'Cannot find any module who owns "pond"'
      call FATAL_ERROR (ERR_user, err_string)
   endif

   if (temp.gt.0.0) then
        g%pond_active = 'yes'
        g%pond_digital = 1.0
        g%ponded_days = g%ponded_days + 1
        count_dry = 0
   else 
        g%pond_active = 'no'
        g%pond_digital = 0.0
!        g%ftmax = 0.0
!        g%ftmin = 0.0
!        g%ftmean = 0.0
        g%ftmax_yest = 0.0
        g%ftmin_yest = 0.0
        g%ftmean_yest = 0.0
   endif

   if (g%ponded_days.eq.1) then
       call Pond_season_init
   endif    

   if (temp.le.0.0) then
       count_dry = count_dry + 1
   endif

   if (count_dry.ge.5.and.g%pab_mass.ge.0.0) then
!     Dryup, so add all algae to surface organic matter - only after it has been sitting dry for (arbitrary) 5 days
      call Pond_send_cropchopped_event(g%pab_mass)
      g%pab_mass = 0.0
      g%ponded_days = 0
   endif      

   g%pond_depth = temp
   g%pond_evap = temp2

   call pop_routine (my_name)
   return
end subroutine

!     ===========================================================
subroutine Pond_set_my_variable (variable_name)
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Sub-Program Arguments
   character  variable_name*(*)     ! (INPUT) Variable name to
                                    ! search for

!+  Purpose
!     Set one of our variables altered by some other module

!+  Mission Statement
!     Set Variable as Requested

!+  Calls


!+  Constant Values
   character  my_name*(*)
   parameter (my_name='pond_set_my_variable')

!+  Local Variables
   integer      numvals             ! number of values received
   real         temp                ! temporary values of dlt's sent through
   character    err_string*100    ! error string

!- Implementation Section ----------------------------------
   call push_routine (my_name)

   if (variable_name .eq. 'pond_no3') then
      call collect_real_var(variable_name,'(kg/ha)', g%pond_no3, numvals, 0.0, 1000.0)


   elseif (variable_name .eq. 'pond_nh4') then
      call collect_real_var(variable_name,'(kg/ha)', g%pond_nh4, numvals, 0.0, 1000.0)


   elseif (variable_name .eq. 'pond_urea') then
      call collect_real_var(variable_name,'(kg/ha)', g%pond_urea, numvals, 0.0, 1000.0)


   elseif (variable_name .eq. 'dlt_pond_no3') then
      call collect_real_var(variable_name,'(kg/ha)', temp, numvals, -1000.0, 1000.0)
      g%pond_no3 = g%pond_no3 + temp


   elseif (variable_name .eq. 'dlt_pond_nh4') then
      call collect_real_var(variable_name,'(kg/ha)', temp, numvals, -1000.0, 1000.0)
      g%pond_nh4 = g%pond_nh4 + temp


   elseif (variable_name .eq. 'dlt_pond_urea') then
      call collect_real_var(variable_name,'(kg/ha)', temp, numvals, -1000.0, 1000.0)
      g%pond_urea = g%pond_urea + temp


   else
         ! Don't know this variable name
      call message_unused ()
   endif

   call pop_routine (my_name)
   return
end subroutine

!     ===========================================================
subroutine Pond_min_residues (dlt_C_decomp, dlt_N_decomp, dlt_c_biom, dlt_c_hum, dlt_c_atm, dlt_nh4_min, dlt_no3_min)
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Sub-Program Arguments
      real dlt_C_decomp(max_residues)   ! C decomposed for each residue (Kg/ha)
      real dlt_N_decomp(max_residues)   ! N decomposed for each residue (Kg/ha)
      real dlt_c_atm(max_residues)      ! (OUTPUT) carbon to atmosphere (kg/ha)
      real dlt_c_biom(max_residues)     ! (OUTPUT) carbon to biomass (kg/ha)
      real dlt_c_hum(max_residues)      ! (OUTPUT) carbon to humic (kg/ha)
      real dlt_nh4_min                  ! (OUTPUT) N to NH4 (kg/ha)
      real dlt_no3_min                  ! (OUTPUT) N to NO3 (kg/ha)

!+  Purpose
!       Test to see whether adequate mineral nitrogen is available
!       to sustain potential rate of decomposition of surface residues
!       and calculate net rate of nitrogen mineralization/immobilization

!+  Mission Statement
!     Calculate rate of nitrogen mineralization/immobilization

!+  Constant Values
   character  my_name*(*)           ! name of subroutine
   parameter (my_name = 'Pond_min_residues')

!+  Local Variables
   real       nit_tot               ! total N avaliable for immobilization
                                    !    (kg/ha)
   real       dlt_c_biom_tot(max_residues) ! C mineralized converted to biomass
   real       dlt_c_hum_tot(max_residues)  ! C mineralized converted to humic
   real       n_demand              ! potential N immobilization
   real       n_avail               ! available N for immobilization
   real       scale_of              ! factor to reduce mineralization
                                    ! rates if insufficient N available
   real       part_fraction         ! partitioning fraction
   real       dlt_n_min             ! net N mineralized (kg/ha)
   integer    residue               ! count of residue
   character  err_string*200

!+  Initial Data Values

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   ! get total available mineral N in pond

      nit_tot = nit_tot + g%pond_no3 + g%pond_nh4


       ! potential decomposition rates of residue C and N
       ! are obtained from SurfaceOM module during a 
       !'PotentialResidueDecompositionCalculated' Event


       ! calculate potential transfers to biom and humic pools

   dlt_c_biom_tot(:) = g%pot_c_decomp(:) * c%ef_res * c%fr_res_biom
   dlt_c_hum_tot(:)  = g%pot_c_decomp(:) * c%ef_res* (1.0 - c%fr_res_biom)



       ! test whether adequate N available to meet immobilization demand


   n_demand = divide (sum (dlt_c_biom_tot(:)), c%mcn, 0.0)+ divide (sum (dlt_c_hum_tot(:)), c%pond_cn, 0.0)

   n_avail = nit_tot + sum (g%pot_n_decomp(:))

   if (n_demand.gt.n_avail) then
      scale_of = divide (nit_tot, (n_demand - sum (g%pot_n_decomp(:))), 0.0)
      scale_of = bound (scale_of, 0.0, 1.0)
   else
      scale_of = 1.0   ! supply exceeds demand
   endif


      do residue = 1, g%num_residues

         ! now adjust carbon transformations etc.
         dlt_c_decomp(residue) = g%pot_c_decomp(residue)* scale_of 
         dlt_n_decomp(residue) = g%pot_n_decomp(residue)* scale_of 

         dlt_c_hum(residue) = dlt_c_hum_tot(residue)* scale_of 
         dlt_c_biom(residue) = dlt_c_biom_tot(residue)* scale_of 
         dlt_c_atm(residue) = dlt_c_decomp(residue)- dlt_c_hum(residue)- dlt_c_biom(residue)
      end do

   

   dlt_NH4_min = 0.0
   dlt_n_min = sum (dlt_n_decomp(:)) - n_demand * scale_of

   if (dlt_n_min .gt. 0.0) then
         ! we have mineralisation into NH4
         dlt_nh4_min = dlt_n_min 

   else if (dlt_n_min .lt. 0.0) then
      ! Now soak up any N required for immobilisation from NH4 then NO3

         dlt_nh4_min =  - min(g%pond_nh4,abs(dlt_n_min))
         dlt_n_min = dlt_n_min - dlt_nh4_min

         dlt_no3_min =  - min(g%pond_no3,abs(dlt_n_min))
         dlt_n_min = dlt_n_min - dlt_no3_min
      ! There should now be no remaining immobilization demand
!      call bound_check_real_var (dlt_n_min,-0.001, 0.001,'remaining imobilization')
!    ********** check this out later and fix - was giving warning errrors here
   else
      ! no N transformation
   endif

   call pop_routine (my_name)
   return
end subroutine

!     ===========================================================
subroutine Pond_sendActualResidueDecompositionCalculated()
!     ===========================================================
   use ComponentInterfaceModule
   Use infrastructure
   implicit none

!+  Purpose
!     Send the equivalent of the old 'Do_Decompose' action to residue/manure and soilP

!+  Mission Statement
!     Send the equivalent of the old 'Do_Decompose' event to residue/manure and soilP

!+  Constant Values
   character*(*) my_name               ! name of current procedure
   parameter (my_name ='Pond_sendActualResidueDecompositionCalculated')
!+  Local Variables
    character*200  message, err_string
    integer   residue                     ! residue number
    real      dlt_res_c_decomp            ! amount of c to decompose into pond for each residue
    real      dlt_res_n_decomp            ! amount of n to decompose into pond for each residue
    real      c_summed                    ! total amount of c to decompose
    real      n_summed                    ! total amount of n to decompose
    type (SurfaceOrganicMatterDecompType)::SurfaceOrganicMatterDecomp


!- Implementation Section ----------------------------------
   call push_routine (my_name)
   c_summed = 0.0
   n_summed = 0.0

   ! Potential decomposition was given to this module by a residue
   ! module.  We now explicitly tell the residue module to decompose
   ! some of its residue now.  If we have been unable to decompose the
   ! potential it gave us, the value returned belows will have been
   ! reduced to the actual value.

   do residue = 1, g%num_residues

       c_summed = c_summed + g%dlt_res_c_hum(residue)+ g%dlt_res_c_biom(residue)+ g%dlt_res_c_atm(residue)


       n_summed = n_summed + g%dlt_res_n_decomp(residue)

       ! dsg 131103  Now, pack up the structure to return decompositions to SurfaceOrganicMatter

       SurfaceOrganicMatterDecomp%pool(residue)%name = g%residue_name(residue)

       SurfaceOrganicMatterDecomp%pool(residue)%OrganicMatterType =g%residue_type(residue)

       !   dsg 131103   The 'amount' value will not be used by SurfaceOrganicMatter, so send zero as default
       SurfaceOrganicMatterDecomp%pool(residue)%FOM%amount = 0.0

       SurfaceOrganicMatterDecomp%pool(residue)%FOM%C = c_summed

       SurfaceOrganicMatterDecomp%pool(residue)%FOM%N = n_summed

       !   dsg 131103   The 'P' value will not be collected by SurfaceOrganicMatter, so send zero as default.
       SurfaceOrganicMatterDecomp%pool(residue)%FOM%P = 0.0
       SurfaceOrganicMatterDecomp%pool(residue)%FOM%AshAlk = 0.0
       c_summed = 0.0
       n_summed = 0.0
   end do
 
   call publish_SurfaceOrganicMatterDecomp(id%ActualResidueDecompositionCalculated, SurfaceOrganicMatterDecomp)


   call pop_routine (my_name)
   return
end subroutine


!     ===========================================================
subroutine Pond_temperature_balance ()
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Sub-Program Arguments

!+  Purpose
!       

!+  Mission Statement
!     

!+  Constant Values
   character  my_name*(*)           ! name of subroutine
   parameter (my_name = 'Pond_temperature_balance')

!+  Local Variables
      integer i                      ! counter 
      integer numvals                ! counter                                                        
      real tmfac1(8)                 ! temperature factor                                              
      real fdepth                    ! pond depth in cms                                               
      real totevap_cm                ! pond evaporation + rice transpiration in cms                    
      real tot_tmp                   ! summation variable                                              
      real tot_fwt                   ! summation variable                                              
      real ttmp                      ! temporary variable                                              
      real tmean                     ! temporary variable                                              
      real albedo                    ! albedo of ponded rice field                                     
      real ftmax_diff                ! difference between max and mean pond temperatures               
      real atmax_diff                ! difference between max and mean atmospheric temperatures        
      real ftmin_diff                ! difference between min and mean pond temperatures               
      real atmin_diff                ! difference between min and mean atmospheric temperatures        
      real fdepth_tmax               ! pond depth factor on maximum pond temp                          
      real fdepth_tmin               ! pond depth factor on minimum pond temp                          
      real fevap_effect              ! evaporative cooling effect factor                               
      real netradindex               ! crop albedo effect on net radiation reaching floodwater surface 
      real rad_effect                ! crop albedo effect on net radiation reaching floodwater surface 
      real soiltemp_layer1           ! soil temperature in layer 1                                     
      real fwtmp                     ! temporary floodwater summation variable                         
      real soiltemp_layer(max_layer) ! average soil temperature in layer 1   
      character*200  err_string
         
!+  Initial Data Values

!- Implementation Section ----------------------------------

   call push_routine (my_name)

!  dsg 231008    do season init stuff if appropriate

	  if (g%ponded_days.eq.1) then     ! FIRST DAY AFTER FLOODING
	      do i = 1, 8
                 tmfac1(i) = 0.931+0.114*i-0.0703*i**2+0.0053*i**3   ! from CERES Rice
              end do
              g%ftmin_yest = g%mint
	      g%ftmax_yest = g%maxt
	      g%ftmean_yest= (g%maxt+g%mint)/2.0
	  endif

      fdepth = g%pond_depth/10.     
      totevap_cm  = g%pond_evap/10. + g%rep/10. 	! from CERES Rice :-  'ef' is floodwater evaporation, 'et' must be rice transpiration

      tot_tmp=0.0
      tot_fwt=0.0
      do i = 1, 8
         tmfac1(i) = 0.931+0.114*i-0.0703*i**2+0.0053*i**3   ! from CERES Rice
         ttmp = g%mint + tmfac1(i)*(g%maxt-g%mint)
	 tot_tmp = tot_tmp + ttmp
      end do
      tmean = tot_tmp/8.

      albedo       = 0.23-(0.23-c%water_albedo)*exp(-0.75*g%rlai)

      
      ftmax_diff = g%ftmax_yest - g%ftmean_yest  
      atmax_diff = g%maxt - g%mint
      ftmin_diff = g%ftmean_yest - g%ftmin_yest  
      atmin_diff = tmean - g%mint	

!c    what is intended here is:
!c   if ambient is above ftempmax then raise ftemp by fdepth effect
!c    if ftempmax is above ambient lower ftemp by fdeptheffect
!
      fdepth_tmax = (ftmax_diff/atmax_diff)*0.45 + ((40.-fdepth)*0.025)*0.65


      fdepth_tmin = (ftmin_diff/atmin_diff)*0.45 + min(((40.-fdepth)*0.025)*0.9, 1.0)


!c  ***** now allow for evaporative cooling effect on floodwater max 
!c    this is greatest in shallow water since a higher proportion of the
!c     water is lost - so latent heat is lost        
! 
      fevap_effect= min(1.0, (totevap_cm/fdepth)*6.0)   
!       
!
!c*** crop albedo effect on net radiation reaching floodwater surface
!c
!c      when crop is present then rad_effect will reduce 
!c      floodwater temperature. without crop rad_effect = 0 (no effect)
!
      netradindex = (1.0-albedo)/(1.0-c%water_albedo)
      rad_effect  = 1.0 - netradindex   

! dsg 231008  We need to get the soil temperature :- soiltemp_layer1

   call get_real_array (unknown_module, 'ave_soil_temp', max_layer, '(oC)', soiltemp_layer, numvals, -100.0, 100.0)


! dsg 241008   ***** incorporating all the effects on pond temperature *****

      g%ftmax = soiltemp_layer(1) + atmax_diff * (fdepth_tmax - fevap_effect - rad_effect)
      g%ftmin = soiltemp_layer(1) - atmin_diff * (fdepth_tmin + rad_effect)



      
!      the floodwater temperature calculations are based on salus soil
!      temperature routine.
	
      do i = 1, 8
          fwtmp = g%ftmin + tmfac1(i)*(g%ftmax-g%ftmin)
	  tot_fwt = tot_fwt + fwtmp
      end do

      g%ftmean     = tot_fwt/8.
      g%ftmin_yest = g%ftmin
      g%ftmax_yest = g%ftmax
      g%ftmean_yest= g%ftmean



   call pop_routine (my_name)
   return
end subroutine



!     ===========================================================
subroutine Pond_hydrolise_urea (ftemp)
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Sub-Program Arguments
    real  ftemp          ! floodwater temperature for this timestep

!+  Purpose
!       

!+  Mission Statement
!     

!+  Constant Values
   character  my_name*(*)           ! name of subroutine
   parameter (my_name = 'Pond_hydrolise_urea')

!+  Local Variables
    real tempfu                        ! temperature effect on urea hydrolysis (0-0.9)
    real ualgct                       !
    character*200  err_string
    
!+  Initial Data Values

!- Implementation Section ----------------------------------

   call push_routine (my_name)



      ! Urea hydrolysis function of surface layer oc or biological activity, whichever is greater.
      !
      tempfu = 0.04 * ftemp - 0.2
      tempfu = min (tempfu, 0.9)
      ualgct = 0.1*g%algact
      g%fuhydr = max (g%pot_hydrolysis, ualgct) * tempfu * g%pond_urea
      g%pond_urea   = g%pond_urea  - g%fuhydr
      g%pond_nh4  = g%pond_nh4 + g%fuhydr

!     cumulative urea hydrolised
      g%totuh  = g%totuh + g%fuhydr


   call pop_routine (my_name)
   return
end subroutine


!     ===========================================================
subroutine Pond_move_solutes_down ()
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Sub-Program Arguments

!+  Purpose
!       

!+  Mission Statement
!     

!+  Constant Values
   character  my_name*(*)           ! name of subroutine
   parameter (my_name = 'Pond_move_solutes_down')

!+  Local Variables
    integer   numvals,i                    !   simple counter
    real      pond_depth_yest             !   yesterday's pond depth for the purpose of calculations in this routine (because it is collected afterward
    real      pond_evap                   !   today's pond evaporation
    real      irrigation                  !   today's irrigation
    real      infiltration                !   today's infiltration
    integer   deepest_layer
    real      temp(max_layer)             ! temporary variable
    real      infiltration_frac           ! fraction of the pond which infiltrates today
    real      no3_infiltrated(max_layer)
    real      nh4_infiltrated(max_layer)
    real      urea_infiltrated(max_layer)
    character*200  err_string
    
    
!+  Initial Data Values

!- Implementation Section ----------------------------------

   call push_routine (my_name)

! dsg 311008   Move solutes from the pond into the soil by the processes of mass-flow and adsorption



!  infiltration = yesterday_pond - today_pond - evaporation + irrigation_applied + rain
      call get_real_var (unknown_module, 'pond_evap', '(mm)', pond_evap, numvals, 0.0, 1000.0)

      call get_real_var (unknown_module, 'irrigation', '(mm)', irrigation, numvals, 0.0, 1000.0)
   
      call get_real_var (unknown_module, 'infiltration', '(mm)', infiltration, numvals, 0.0, 1000.0)

!      g%infiltration = g%pond_depth_yest + g%pond_depth - pond_evap - irrigation + g%rain
       g%infiltration = infiltration      


!  Calculate fraction of pond water infiltrating
      infiltration_frac = divide(g%infiltration,g%pond_depth,0.0)
      infiltration_frac = max(0.0, min(1.0, infiltration_frac))
!  dsg 050309 this line was put in to test for a user-specified solute infiltration rate     infiltration_frac = c%solute_infiltration_rate

!   dsg 011108  put the infiltrated solutes into the soil
       call get_real_array (unknown_module, 'dlayer', max_layer, '(mm)', temp, numvals, 0.0, 10000.0)
          if (numvals.eq.0) then
              err_string = 'Cannot find variable "dlayer" for counting'
              call FATAL_ERROR (ERR_user, err_string)
          endif
       deepest_layer = count_of_real_vals (temp, max_layer)
       
       do i = 1,deepest_layer
          g%no3_infiltrated(i) = 0.0
          g%nh4_infiltrated(i) = 0.0
          g%urea_infiltrated(i) = 0.0
       end do


       
       g%no3_infiltrated(1) = g%pond_no3 * infiltration_frac
       g%nh4_infiltrated(1) = g%pond_nh4 * infiltration_frac
       g%urea_infiltrated(1) = g%pond_urea * infiltration_frac
      
!     dsg & as 040209    Now increment pond mineral N pools
      g%pond_no3 = g%pond_no3 * (1 - infiltration_frac)
      g%pond_nh4 = g%pond_nh4 * (1 - infiltration_frac)
      g%pond_urea = g%pond_urea * (1 - infiltration_frac)
   

          
       call set_real_array (unknown_module, 'dlt_no3','(kg/ha)', g%no3_infiltrated, deepest_layer)
       call set_real_array (unknown_module, 'dlt_nh4','(kg/ha)', g%nh4_infiltrated, deepest_layer)
       call set_real_array (unknown_module, 'dlt_urea','(kg/ha)', g%urea_infiltrated, deepest_layer)


   g%pond_depth_yest = g%pond_depth


   call pop_routine (my_name)
   return
end subroutine


!     ===========================================================
subroutine Pond_calculate_daily_variables ()
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Sub-Program Arguments

!+  Purpose
!       

!+  Mission Statement
!     

!+  Constant Values
   character  my_name*(*)           ! name of subroutine
   parameter (my_name = 'Pond_calculate_daily_variables')

!+  Local Variables
      real        walb              ! water albedo
      real        frad              ! solar radiation reduced by lai
      real        elag              !
      real        fldni             ! total mineral N available in floodwater (kg/ha)
      real        tmpfon            ! 
      real        algaln
      real        temp_algact       ! tester value for g%algalct
      character   err_string*1000      
!+  Initial Data Values

!- Implementation Section ----------------------------------

   call push_routine (my_name)

      
!     Calculate potential hydrolysis, AK - CERES-Rice
      !
      !  dsg 311008 The following g%oc1 is the % Organic carbon in layer 1.  Is this correct for the equation below?
      !                                Or is it in kg/ha?? or ppm ????? 
      g%pot_hydrolysis    = 0.008 + 0.005 * g%oc1
      g%pot_hydrolysis    = min(g%pot_hydrolysis,1.0)


!     Calculatefloodwater evaporation (ef) initialization at begin of day
      !
      ! ampef(hr) is 0.2*ef(daily)  inc to .38
      !
      g%ampef   = 0.38*g%pond_evap 
      
         
!     Compute concentrations of N in floodwater.
      g%pond_nh4_conc = g%pond_nh4  * 100.0 / g%pond_depth
      g%pond_no3_conc = g%pond_no3  * 100.0 / g%pond_depth


!     Calculate n factor affecting algal growth
      g%fni = (g%pond_nh4_conc + g%pond_no3_conc)/15.0+0.10          ! 15->10
      g%fni = min (g%fni,1.0)

!     dsg 311008  Phosphorus factor for algal growth will be assumed as 0.5 (ie no phosphorus fertilisers added)
!                 CERES Rice use fpi= 1.0 when P fertilisers have been added
!      g%fpi = 0.5  : this is already set in Pond_season_init
!   dsg 031108  
      g%fpi     = c%pab_p_index


!     Calculate light factor affecting algal growth, ali
      algaln = 0.0
      walb     = 0.1
      frad     = g%radn*(1.0-walb)*exp(-0.65*g%rlai)      ! .85->.65
      g%ali      = 1.0 - exp(-frad/5.0)

!     dsg 311008  Calculate temperature factor for algal growth
      if (g%ftmean .lt. 30.0) then
         g%fti = (g%ftmean-15.0)*0.1
       elseif (g%ftmean .ge. 30.0) then
         g%fti = 1.0-(g%ftmean-30.0)*0.05
      endif

      g%fti = min (g%fti,1.0)


!     Calculate biological activity of floodwater, algact (0-1 value)
      !
      elag   = min (g%fni,g%fpi,g%ali,g%fti)
      temp_algact = elag*(3.0-g%algact)*g%algact       ! 3->3.5

      if (g%rlai .gt. 1.0) then
         temp_algact = min (temp_algact,g%ali)
      endif
      temp_algact = min(temp_algact,1.00)
      temp_algact = max(temp_algact,0.10)

!  dsg fudge 031108 - take out N-factor
      g%algact   = min (g%fpi,g%ali,g%fti)
      if(g%algact.lt.0.0) then
        g%algact = 0.0
      endif


      
!  dsg 130209  - if no algae present (as specified by user) then make g%algact always equal to zero 
      if (c%algae_present.eq.0) then
         g%algact = 0.0
      endif     


      fldni = g%pond_nh4 + g%pond_no3

      if (.not. g%akill) then
         if (.not. g%immobil) then
            if (algaln .gt. fldni) then
                g%algfix = algaln
            endif
            algaln = fldni
            algaln = max (algaln,g%algfix)
            !
      ! trying to determine amount of n in algae at peak algal growth
      ! maximum n fixed by algae = 5 kg n/ha
            !
            if (algaln .ge. g%algfix .and. algaln .gt. 0.8) then
               algaln = min (algaln,8.0)
               g%pond_no3  = g%pond_no3 - algaln * g%pond_no3 / fldni
               g%pond_nh4  = g%pond_nh4 - algaln * g%pond_nh4 / fldni
               g%immobil  = .true.
               g%algfix = algaln
             else
               if (g%kill .eq. 1 .and. fldni .gt. 0.0) then
                  g%pond_no3  = g%pond_no3 - algaln * g%pond_no3 / fldni
                  g%pond_nh4  = g%pond_nh4 - algaln * g%pond_nh4 / fldni
                  g%immobil  = .true.
                  g%algfix = algaln
               endif
            endif
         endif

         if (g%yalga .le. g%algact) then
            g%kill = 0
          else
            g%kill = g%kill + 1
            if (g%kill .eq. 2) then
               g%akill  = .true.
               g%tmpfon = g%tmpfon + g%algfix
               g%algfix = 0.0
            endif
         endif
      endif
      g%yalga = g%algact

!   dsg 291008  The section below was used in CERES-Rice to determine whether to calculate Pond dynamics on 1 or 12 daily timesteps
!               If daily is 'true' then only one timestep, if 'false' then 12 timesteps per day.
!    For the time being, we will always do 12 timesteps per day
      
!      if (algact .ge. 0.2 .or. yrdoy .lt. lfd10) then
!!         ihd = 2
!        daily = .false.
!      else
!        daily = .true.
!      endif


   call pop_routine (my_name)
   return
end subroutine



!     ===========================================================
subroutine Pond_get_daily_variables ()
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Sub-Program Arguments

!+  Purpose
!       

!+  Mission Statement
!     

!+  Constant Values
   character  my_name*(*)           ! name of subroutine
   parameter (my_name = 'Pond_get_daily_variables')

!+  Local Variables
    integer numvals                      ! simple counters
    real temp(max_layer)                 ! temporary variable
    character*200  err_string
      
!+  Initial Data Values

!- Implementation Section ----------------------------------

   call push_routine (my_name)

! determine the runoff over the bund, needed by subroutine Pond_check_N_runoff
       call get_real_var (unknown_module, 'runoff', '()', g%runoff, numvals, 0.0, 1000.0)
      
! determine the rice lai - required by the pond_pab_rcf function to determine growth factor for algae
       call get_real_var_optional (unknown_module, 'rlai', '()', g%rlai, numvals, 0.0, 30.0)
          if (numvals.eq.0) then
              g%rlai = 0.0
          endif
      
! determine the actual rice evapotranspiration - required by the pond_temperature_balance subroutine 
       call get_real_var_optional (unknown_module, 'trw', '()', g%rep, numvals, 0.0, 20.0)
          if (numvals.eq.0) then
              g%rep = 0.0
          endif

! determine the organic carbon in soil layer 1 - required to calculate potential urea hydrolysis in pond (kg/ha) 
       call get_real_array (unknown_module, 'oc%', max_layer, '(%)', temp, numvals, 0.0, 100.0)
          if (numvals.eq.0) then
              err_string = 'Cannot find variable "oc%"'
              call FATAL_ERROR (ERR_user, err_string)
          endif
          g%oc1 = temp(1)          

! determine the dlayer(1) -  
       call get_real_array (unknown_module, 'dlayer', max_layer, '(mm)', temp, numvals, 0.0, 10000.0)
          if (numvals.eq.0) then
              err_string = 'Cannot find variable "dlayer"'
              call FATAL_ERROR (ERR_user, err_string)
          endif
          g%dlayer1 = temp(1)          

! determine the bd(1) -  
       call get_real_array (unknown_module, 'bd', max_layer, '(g/cm3)', temp, numvals, 0.0, 100.0)
          if (numvals.eq.0) then
              err_string = 'Cannot find variable "bd"'
              call FATAL_ERROR (ERR_user, err_string)
          endif
          g%bd1 = temp(1)          
      


   call pop_routine (my_name)
   return
end subroutine



!     ===========================================================
subroutine Pond_ph_balance (timestep)
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Sub-Program Arguments
    integer  timestep !  (1-12)
!+  Purpose
!       

!+  Mission Statement
!     

!+  Constant Values
   character  my_name*(*)           ! name of subroutine
   parameter (my_name = 'Pond_ph_balance')

!+  Local Variables
     real phshift                        ! change in ph due to algal activity
     real fuhydrc                        ! floodwater urea hydrolised (concentration)
     real fuhydrm                        ! 
     real phfuhyd                        !
   character  Err_string*400      ! Event message string
 
!+  Initial Data Values

!- Implementation Section ----------------------------------

   call push_routine (my_name)

         !
         ! Calculate indices for biological activity
         !
         phshift = 0.5 + 2.0*g%algact                  ! 2->2.5


         if (timestep .le. 6 .or. timestep .gt. 8) then
            g%fph = 7.0 + phshift*sin(3.1412*float(timestep)/12.0)
            !
            ! add effects of urea hydrolysis on floodwater ph here
            !
            if (g%fuhydr .gt. 0.05) then
               fuhydrc = g%fuhydr*100.0/g%pond_depth
               fuhydrm = fuhydrc*0.001/14.0
               phfuhyd = min(10.0,-log10(fuhydrm))
               g%fph     = g%fph + g%ali*(10.0-phfuhyd)/10.0
            endif
         endif

         g%fph  = amax1 (g%fph,7.0)



   call pop_routine (my_name)
   return
end subroutine


!     ===========================================================
subroutine Pond_volatilise_ammonia (timestep, ftemp)
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Sub-Program Arguments
    integer  timestep    ! timestep number (2hourly intervals) (1-12)
    real     ftemp       ! floodwater temperature this timestep

!+  Purpose
!       

!+  Mission Statement
!     

!+  Constant Values
   character  my_name*(*)           ! name of subroutine
   parameter (my_name = 'Pond_volatilise_ammonia')

!+  Local Variables
     real hef         ! Hourly Floodwater Evaporation (mm/hr)
     real tk          ! floodwater temperature in kelvin (K)
     real wind        ! windspeed
     real aloghk      !
     real hk          !
     real fnh3m       ! 
     real fnh3p       ! floodwater partial pressure of ammonia
     real amlos1      ! ammonia loss from the pond (kg N/ha/hr) 
     character err_string *200
     
!+  Initial Data Values

!- Implementation Section ----------------------------------

   call push_routine (my_name)


         ! Calculate Hourly Floodwater Evaporation (HEF)
         !
!         if (timestep .le. 6 .or. timestep .gt. 9) then
            hef = g%ampef*sin(3.141593*float(timestep)/12.0) + 0.08  ! 10->9
!         endif
         hef = abs(hef)


         ! ammonia loss routine ... calculate floodwater nh3
         !
         tk     = ftemp + 273.
         g%pond_nh4_conc = g%pond_nh4 * 100.0 / g%pond_depth
         g%pond_no3_conc = g%pond_no3 * 100.0 / g%pond_depth
         if (timestep .le. 6 .or. timestep .gt. 9 .or. g%pond_nh3_conc .ge. g%pond_nh4_conc) then
            g%pond_nh3_conc = g%pond_nh4_conc/(1.0+10.0**(0.09018+2729.92/tk-g%fph))
         endif
         g%pond_nh3  = g%pond_nh3_conc * g%pond_depth * 0.01
     
         wind   = 7.15 * hef                           ! 7.15->5.75
         if (g%rlai .gt. 1.0) then
            wind = 5.75*hef/(g%rlai*1.5)
         endif

  ! dsg 180209   detemine partial pressure of ammonia (reference Freney et al 1981, AJAR 32:37-45)
         aloghk = 155.559 - 8621.06/tk - 25.6767*alog(tk) + 0.035388*tk
         hk     = exp(aloghk)
         fnh3m  = g%pond_nh3_conc*0.001/14.0
         fnh3p  = max (0.0,(10.0*fnh3m/hk))       ! 1.552
     
  !   dsg 180209  Now calculate ammonia N loss - reference: Godwin Singh Buresh and DeDatta (1990) pages 320-325 in Transactions of the 
  !                                              14th International Congress of Soil Science, "Modelling of N dynamics in relation to rice
  !                                              growth and yield", vol iv, Commission iv, Kyoto, japan 
         if (fnh3p .gt. 0.0) then
         amlos1 = 0.036 * fnh3p + 0.0082 * wind + 0.000036 * fnh3p**2 * wind * g%pond_depth
         endif

         if (fnh3p .le. 0.0) then
            amlos1 = 0.0
         endif
         if (g%pond_nh3_conc .le. 0.0 .or. amlos1 .le. 0.0) then
            amlos1 = 0.0
         endif
!  dsg 031108 comment this out for time being - don't understand         g%pond_nh3  = min (g%pond_no3,g%pond_nh4)
         amlos1 = min (amlos1,g%pond_nh3)
         
!  dsg fudge factor for testing with Ahmad Suriadi
         amlos1 = amlos1 * c%amlos_fudge_factor

         g%pond_nh4  = g%pond_nh4  - amlos1
         g%amloss = amlos1
         g%totaml = g%totaml + amlos1
         g%pond_nh4_conc = g%pond_nh4*100.0/g%pond_depth



   call pop_routine (my_name)
   return
end subroutine



!     ===========================================================
subroutine Pond_grow_pab ()
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Sub-Program Arguments

!+  Purpose
!       

!+  Mission Statement
!     

!+  Constant Values
   character  my_name*(*)           ! name of subroutine
   parameter (my_name = 'Pond_grow_pab')

!+  Local Variables
!    real pab_tf         ! temperature factor affecting algal growth (0-1)
!    real pab_rcf        ! radiation & cover factor affecting algal growth (0-1)
    real dlt_pab        ! increment in algal growth (kg/ha)
   character  Err_string*400      ! Event message string
 
!+  Initial Data Values

!- Implementation Section ----------------------------------

   call push_routine (my_name)

!   pab_tf = pond_pab_tf() 
!   pab_rcf = pond_pab_rcf()
   g%algal_turnover = 0.0     

   dlt_pab  = c%maxrate_pab * g%algact
   g%pab_mass = g%pab_mass + dlt_pab
   


   if (g%pab_mass.gt.500.0) then
         g%algal_turnover = g%pab_mass - 500.0
         g%pab_mass = 500.0
   endif
   
   if (g%algal_turnover.gt.0.0) then  
!   **** send a cropped chopped event here to add the 'turnover' to the surfaceom pool  ********** 
       call Pond_send_cropchopped_event (g%algal_turnover)
   endif     
   
   ! what does the function of temperature look like???? - use Arrhenius equation - read about in 
   ! "A Kinetic Approach to the effect of temperature on algal growth" Goldman et al, Limnology and Oceanography
   ! Vol 19, No5, Sep 1974, pp 756-766

  

   call pop_routine (my_name)
   return
end subroutine


!     ===========================================================
subroutine Pond_send_cropchopped_event (amount)
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Sub-Program Arguments
    real amount        ! the amount of algae being sent to residue pool (kg/ha)
!+  Purpose
!       

!+  Mission Statement
!     

!+  Constant Values
   character  my_name*(*)           ! name of subroutine
   parameter (my_name = 'Pond_send_cropchopped_event')

!+  Local Variables
      integer i                          ! simple counter
      real dlt_crop_dm(max_part)         ! algae dry matter added to residue pool (kg/ha)
      real dlt_dm_n(max_part)            ! algae N added to residue pool (kg/ha)
      real fraction_to_residue(max_part) ! fraction of total going to residue pool (0-1.0)
      character dm_type(max_part)*10     ! type of material

!+  Initial Data Values

!- Implementation Section ----------------------------------

   call push_routine (my_name)


      do i = 1,max_part
         dlt_crop_dm(i) = 0.0
         dlt_dm_n(i) = 0.0
         fraction_to_residue(i) = 1.0
         dm_type = 'algae'
      end do
 
 ! dsg 31/10/08  put algae data into component 2, as per green leaves in other crops        
      dlt_crop_dm(2) = amount        
      dlt_dm_n(2) = amount * 0.4 / 8.0  ! assuming algae is 40% C, with C:N of 8.0 (Roger 1996)
      fraction_to_residue(2) = 1.0
      
      
      call new_postbox ()

      call post_char_var   (DATA_crop_type,'()', 'algae')
      call post_char_array (DATA_dm_type,'()', dm_type, max_part)
      call post_real_array (DATA_dlt_crop_dm,'(kg/ha)', dlt_crop_dm, max_part)
      call post_real_array (DATA_dlt_dm_n,'(kg/ha)', dlt_dm_n, max_part)
      call post_real_array (DATA_fraction_to_Residue,'()', fraction_to_residue, max_part)

      call event_send (unknown_module, EVENT_Crop_Chopped)

      call delete_postbox ()

   
   call pop_routine (my_name)
   return
end subroutine


!     ===========================================================
subroutine Pond_check_runoff_N ()
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Sub-Program Arguments

!+  Purpose
!       

!+  Mission Statement
!     

!+  Constant Values
   character  my_name*(*)           ! name of subroutine
   parameter (my_name = 'Pond_check_runoff_N)')

!+  Local Variables

    real    loss_fraction             !  fraction of pond lost as runoff ()

!+  Initial Data Values

!- Implementation Section ----------------------------------

   call push_routine (my_name)

!   dsg 011108   Loose N in runoff at the same fraction of water lost from pond

    loss_fraction = divide(g%runoff,g%pond_depth,0.0)

    loss_fraction = max(0.0, min(1.0, loss_fraction))

    g%pond_no3 = g%pond_no3 * (1 - loss_fraction)
    g%pond_nh4 = g%pond_nh4 * (1 - loss_fraction)
    g%pond_nh3 = g%pond_nh3 * (1 - loss_fraction)
    g%pond_urea = g%pond_urea * (1 - loss_fraction)
   
   call pop_routine (my_name)
   return
end subroutine

!================================================================
   real function pond_pab_tf ()
!================================================================
   use infrastructure
   implicit none

!+  Purpose
!   Calculate temperature factor for algal growth (0-1).

!+  Notes
!   Use Arrhenius equation

!+  Constant Values
   character*(*) my_name             !  name of current procedure
   parameter (my_name = 'pond_pab_tf')

!+  Local Variables
   real       ave_temp              ! today's average air temp (oC)
   real       tf                    ! temperature factor

!- Implementation Section ----------------------------------
   call push_routine (my_name)

   tf = 0.0
   ave_temp = 20.0 ! default for time being, will need to be sent with function call according to timestep...

! use the temperature factor suggested by Godwin & Singh 1998
!
!            1 |            /\
!              |           / .\    
!              |          /  . \   
!              |         /   .  \  
!              |        /    .   \
!            0 ------------------------------ temp
!              0       15    30   45   


   if (ave_temp .lt. 15.0) then
      tf = 0.0

   else if (ave_temp.ge.15.0.and.ave_temp.le.30.0) then
      tf = divide (ave_temp, 15.0, 0.0)- 1.0
      tf = bound (tf, 0.0, 1.0)

   else if (ave_temp.gt.30.0) then
      tf = divide (ave_temp, -15.0, 0.0)+ 3.0
      tf = bound (tf, 0.0, 1.0)

   else
   endif

   pond_pab_tf = tf

   call pop_routine (my_name)
   return
end function

!================================================================
   real function pond_pab_rcf ()
!================================================================
   use infrastructure
   implicit none

!+  Purpose
!   Calculate radiation and temperature factor for algal growth (0-1).

!+  Notes
!   Use Arrhenius equation ...??

!+  Constant Values
   character*(*) my_name             !  name of current procedure
   parameter (my_name = 'pond_pab_rcf')

!+  Local Variables
   real       ave_temp              ! today's average air temp (oC)
   real       rcf                   ! radiation and cover factor
   real       WALB                  ! constant from CERES Rice
   real       FRAD                  ! radiation fraction from CERES Rice (Godwin & Singh 1998)
   real       ALI                   ! shading factor against algae growth from CERES rice

!- Implementation Section ----------------------------------
   call push_routine (my_name)
 
      WALB = 0.1

      FRAD     = g%radn*(1.0-WALB)*EXP(-0.65*g%rlai)      ! .85->.65
      ALI      = 1.0 - EXP(-FRAD/5.0)


      pond_pab_rcf = ALI

   call pop_routine (my_name)
   return
end function



!  ====================================================================
subroutine Pond_create ()
!  ====================================================================
   Use Infrastructure
   implicit none

!+ Purpose
!  Create

!+ Mission statement
!  Create

!+ Changes
!  dsg 150603  built

!+ Constant Values
   character*(*) myname               ! name of current procedure
   parameter (myname = 'Pond_create')

!+ Local Variables

!- Implementation Section ----------------------------------
   call push_routine (myname)

   call Pond_zero_variables ()
   call Pond_zero_event_data ()


   call pop_routine (myname)
   return
end subroutine
end module PondModule
   
!* ====================================================================
subroutine doInit1()
!* ====================================================================
   use PondModule
   Use infrastructure
   implicit none
   ml_external doInit1
   integer dummy

   call doRegistrations(id)

   dummy = add_registration_with_units(getVariableReg, 'runoff', floatTypeDDML, 'mm')
   dummy = add_registration_with_units(getVariableReg, 'crop_area', floatTypeDDML, 'ha')
   dummy = add_registration_with_units(getVariableReg, 'day', intTypeDDML, '')
   dummy = add_registration_with_units(respondToGetReg, 'rain_capture', floatTypeDDML, 'ML')
   dummy = add_registration_with_units(respondToGetReg, 'available_water', floatTypeDDML, 'ML')
   dummy = add_registration_with_units(respondToGetReg, 'evaporation', floatTypeDDML, 'ML')
   dummy = add_registration_with_units(respondToGetReg, 'seepage', floatTypeDDML, 'ML')
   dummy = add_registration_with_units(respondToGetReg, 'overflow', floatTypeDDML, 'ML')
   dummy = add_registration_with_units(respondToGetReg, 'runoff_input', floatTypeDDML, 'ML')
   dummy = add_registration_with_units(respondToGetReg, 'irrig_water_supplied', floatTypeDDML, 'ML')
   dummy = add_registration_with_units(respondToGetReg, 'available_depth', floatTypeDDML, 'm')
   dummy = add_registration_with_units(respondToGetReg, 'max_available_water', floatTypeDDML, 'Ml')
   dummy = add_registration_with_units(respondToGetReg, 'min_volume', floatTypeDDML, 'Ml')
   dummy = add_registration_with_units(respondToGetReg, 'max_pump', floatTypeDDML, 'Ml/day')
   dummy = add_registration_with_units(respondToGetReg, 'annual_allocation', floatTypeDDML, 'ML')
   dummy = add_registration_with_units(respondToGetReg, 'allocation_renewal_day', intTypeDDML, '')
   dummy = add_registration_with_units(respondToGetReg, 'full', intTypeDDML, '')
   dummy = add_registration_with_units(respondToGetReg, 'filling_event', intTypeDDML, '')
   dummy = add_registration_with_units(respondToGetReg, 'storage_*', floatTypeDDML, 'ppm')

end subroutine

!  ===========================================================
subroutine alloc_dealloc_instance(doAllocate)
!  ===========================================================
   use PondModule
   implicit none
   ml_external alloc_dealloc_instance

!+ Sub-Program Arguments
   logical, intent(in) :: doAllocate

!+ Purpose
!  Module instantiation routine.

!- Implementation Section ----------------------------------

   if (doAllocate) then
      allocate(id)
      allocate(g)
      allocate(p)
      allocate(c)
   else
      deallocate(id)
      deallocate(g)
      deallocate(p)
      deallocate(c)
   end if

   return
end subroutine



!  ====================================================================
subroutine Main (action, data_string)
!  ====================================================================
   Use Infrastructure
   use PondModule
   implicit none
   ml_external Main

!+ Sub-Program Arguments
   character action*(*)             ! (input) action to perform
   character data_string*(*)        ! (input) data for action

!+ Purpose
!  ???

!+ Mission Statement
!  Handles communications for Pond

!+ Changes
!  ?????

!+ Constant Values
   character  my_name*(*)           ! name of this module
   parameter (my_name = 'Pond')

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   if (action.eq.ACTION_get_variable) then
     ! respond to request for variable values - from modules
      call Pond_send_my_variable (Data_string)

   else if (action .eq. ACTION_set_variable) then
      call Pond_set_my_variable (data_string)

   else if (action.eq.ACTION_prepare) then
      call Pond_ActiveCheck ()

   else if (action.eq.ACTION_process) then
      call Pond_ONprocess ()
      if (g%pond_active.eq.'yes') then 
        call Pond_sendActualResidueDecompositionCalculated()
      endif

   else if (action.eq.ACTION_init) then
      call Pond_init ()
      call Pond_sum_report ()

   else if (action.eq.ACTION_create) then
      call doRegistrations(id)
      call Pond_create()


   else
      ! don't use message
      call Message_unused ()
   endif

   call pop_routine (my_name)
   return
end subroutine


!  ====================================================================
!  This routine is the event handler for all events
!  ====================================================================
subroutine respondToEvent(fromID, eventID, variant)
   use PondModule
   Use infrastructure
   implicit none
   ml_external respondToEvent

   integer, intent(in) :: fromID
   integer, intent(in) :: eventID
   integer, intent(in out) :: variant

   if (eventID .eq. id%tick) then
      call Pond_zero_daily_variables ()
      call Pond_ONtick(variant)
   elseif (eventID .eq. id%newmet) then
      call Pond_ONnewmet(variant)
   elseif (eventID .eq.id%PotentialResidueDecompositionCalculated) then
       if (g%pond_active.eq.'yes') then
          !  only decompose the residues if a pond is present, otherwise soiln2 will should pick this event up 
          call Pond_ONPotentialResidueDecompositionCalculated(variant)
       endif
   endif
   return
end subroutine respondToEvent

