module Soiln2Module
   use DataTypes

! ====================================================================
!      SoilN variables
! ====================================================================

!   Short description:
!      globals, parameters and constants

!   Notes:
!      none

! ----------------------- Declaration section ------------------------

   Use ConstantsModule

   integer    max_wf_values               ! maximum no. of index/values pairs for
   parameter (max_wf_values = 10)         ! specifying water factor on
                                       ! mineralization
   integer    max_pHf_values              ! maximum no. of index/values pairs for
   parameter (max_pHf_values = 10)        ! specifying pH factor on
                                       ! nitrification
   integer    max_layer                   ! Maximum number of layers
   parameter (max_layer = 100)

   integer    max_residues                ! Maximum number of residues
   parameter (max_residues = 100)

   integer    max_fom_type                ! Maximum number of fom types
   parameter (max_fom_type = 10)

   integer     residue_dim                ! dimension number of residues
   parameter   (residue_dim = 2)

   integer     layer_dim                  ! dimension number of layers
   parameter   (layer_dim = 1)

   integer    nfract                      ! number of fractions of fresh organic
   parameter (nfract = 3)                 ! matter

   integer    days_in_year_max            ! Maximum number of days in a g_year
   parameter (days_in_year_max = 366)

   real C_in_fom                          ! fraction weight of C in carbohydrate
   parameter (C_in_fom = 0.4)

   real       g_urea_min                  ! minimum urea allowed (kg/ha)
   parameter (g_urea_min = 0.0)

   integer on
   parameter (on = 1)

   integer off
   parameter (off = 0)
! ====================================================================
   type Soiln2Globals
      sequence
      ! COEFFICIENTS
      integer      num_fom_types          ! number of fom types read
      integer       fom_type              ! integer representing type of fom


      ! CLIMATE
      real         amp                    ! annual amplitude in mean monthly
                                          ! temperature (oC)
      real         latitude               ! latitude (degrees, negative for
                                          ! southern hemisphere)
      real         maxt                   ! maximum air temperature (oC)
      real         mint                   ! minimum air temperature (oC)
      real         radn                   ! solar radiation (MJ/m^2/day)
      real         salb                   ! bare soil albedo (unitless)
      real         surf_temp(days_in_year_max)
                                          ! actual soil surface temperatures (oC)
      real         ave_temp               ! annual average ambient temperature(oC)
      real         dlt_soil_loss          ! todays soil loss (t/ha)

      ! DATE
      integer      day_of_year            ! day of year
      integer      year                   ! year

      ! INITIAL
      real         root_CN                ! initial C:N ratio of roots ()
      real         root_CN_pool(nfract)   ! initial C:N ratio of each of the three root composition pools (carbohydrate, cellulose, and lignin)
      real         root_wt                ! initial root weight
      real         root_depth             ! initial depth over which roots are distributed (mm)
      real         soil_CN                ! soil C:N ratio ()

      ! POOLS
      real         biom_C(max_layer)      ! biomass carbon   (kg/ha)
      real         biom_N(max_layer)      ! biomass nitrogen (kg/ha)
      real         fom_N(max_layer)       ! nitrogen in FOM  (kg/ha)
      real         fom_c_pool(nfract, max_layer)
                                          ! FOM C in each fraction (kg/ha)
      real         dlt_fom_c_pool1(max_layer) ! delta fom C pool in fraction 1 (kg/ha)
      real         dlt_fom_c_pool2(max_layer) ! delta fom C pool in fraction 2 (kg/ha)
      real         dlt_fom_c_pool3(max_layer) ! delta fom C pool in fraction 3 (kg/ha)
      real         fom_n_pool(nfract, max_layer)
                                          ! FOM N in each fraction (kg/ha)
      real         hum_C(max_layer)       ! Humic C (kg/ha)
      real         hum_N(max_layer)       ! Humic N (kg/ha)
      real         inert_C(max_layer)     ! humic C that is not subject to
                                          ! mineralization (kg/ha)
      real         NH4(max_layer)         ! ammonium nitrogen(kg/ha)
      real         NO3(max_layer)         ! nitrate nitrogen (kg/ha)
      real         NH4_yesterday(max_layer) ! yesterday's ammonium nitrogen(kg/ha)
      real         NO3_yesterday(max_layer) ! yesterday's nitrate nitrogen (kg/ha)
      real         urea(max_layer)        ! Urea nitrogen   (kg/ha)
      integer      num_residues           ! number of residues decomposing (0-100)

      ! TRANSFORMATIONS
      real         dlt_biom_C_atm(max_layer)
                                          ! biomass C lost to atmosphere
                                          ! (kg/ha)
      real         dlt_biom_C_hum(max_layer)
                                          ! biomass C converted to humic
                                          ! (kg/ha)
      real         dlt_biom_N_min(max_layer)
                                          ! net biomass N mineralized (kg/ha)
      real         dlt_C_decomp(max_layer,max_residues)
                                          ! Residue C decomposed (kg/ha)
      real         dlt_C_incorp(max_layer)! Residue C incorporated into FOM
                                          ! (kg/ha)
      real         dlt_NO3_dnit(max_layer)! N denitrified (kg/ha)
      real         dlt_fom_C_atm(nfract,max_layer)
                                          ! fom C lost to atmosphere
                                          ! (kg/ha)
      real         dlt_fom_C_biom(nfract,max_layer)
                                          ! fom C converted to biomass
                                          ! (kg/ha)
      real         dlt_fom_C_hum(nfract,max_layer)
                                          ! fom C converted to humic
                                          ! (kg/ha)
      real         dlt_fom_n(nfract,max_layer)
                                          ! fom N mineralised in each fraction
                                          ! (kg/ha)
      real         dlt_fom_N_min(max_layer)
                                          ! net fom N mineralized (kg/ha)
                                          ! (negative for immobilization)
      real         dlt_hum_C_atm(max_layer)
                                          ! humic C lost to atmosphere (kg/ha)
      real         dlt_hum_C_biom(max_layer)
                                          ! humic C converted to biomass
                                          ! (kg/ha)
      real         dlt_hum_N_min(max_layer)
                                          ! net humic N mineralized (kg/ha)
      real         dlt_N_decomp(max_residues)
                                          ! Residue N decomposed (kg/ha)
      real         dlt_fom_N_incorp(max_layer)
                                          ! Residue N incorporated into FOM

       real         dlt_res_c_atm(max_layer, max_residues)
                                          ! carbon from residues lost
                                          ! to atmosphere (kg/ha)
      real         dlt_res_c_biom(max_layer, max_residues)
                                          ! carbon from residues to biomass
      real         dlt_res_c_hum(max_layer, max_residues)
                                          ! carbon from residues to humic
      real         soilp_dlt_res_c_atm(max_layer) ! carbon from all residues to atmosphere in each layer (kg/ha) for 'get' by soilp
      real         soilp_dlt_res_c_hum(max_layer) ! carbon from all residues to humic in each layer (kg/ha) for 'get' by soilp
      real         soilp_dlt_res_c_biom(max_layer)! carbon from all residues to biom in each layer (kg/ha) for 'get' by soilp
      real         soilp_dlt_org_p(max_layer)     ! variable needed by soilp in its calculations


      real         pot_C_decomp(max_residues)
                                          ! Potential residue C decomposition (kg/ha)
      real         pot_N_decomp(max_residues)
                                          ! Potential residue N decomposition (kg/ha)
      real         pot_P_decomp(max_residues)
                                          ! Potential residue P decomposition (kg/ha)
      real         dlt_res_C_decomp(max_layer, max_residues)
                                          ! residue C decomposition (kg/ha)
      real         dlt_res_N_decomp(max_layer, max_residues)
                                          ! residue N decomposition (kg/ha)
      real         dlt_res_nh4_min (max_layer)
                                          ! Net Residue NH4 mineralisation (kg/ha)
      real         dlt_res_no3_min (max_layer)
                                          ! Net Residue NO3 mineralisation (kg/ha)
      real         NO3_transform_net(max_layer)
                                          ! net NO3 transformation today
      real         NH4_transform_net(max_layer)
                                          ! net NH4 transformation today
      real         dlt_NO3_net(max_layer) ! net NO3 change today
      real         dlt_NH4_net(max_layer) ! net NH4 change today

      ! PROFILE
      real         bd(max_layer)          ! moist bulk density of soil (g/cm^3)
      real         dlayer(max_layer)      ! thickness of soil layer  (mm)
      real         dul_dep(max_layer)     ! drained upper limit soil water content
                                          ! (mm)
      real         fr_biom_C(max_layer)   ! initial ratio of biomass-C to
                                          ! mineralizable humic-C (0-1)
      real         fr_inert_C(max_layer)  ! initial proportion of total soil C
                                          ! that is not subject to mineralization
                                          ! (0-1)
      real         ll15_dep(max_layer)    ! lower limit (@15bar) of soil water
                                          ! content (mm)
      real         oc(max_layer)          ! organic carbon concentration (%)
      real         ph(max_layer)          ! pH of soil in a 1:1 soil-water slurry
      real         sat_dep(max_layer)     ! saturated water content (mm)
      real         NH4_min(max_layer)     ! minimum allowable NH4 (kg/ha)
      real         NO3_min(max_layer)     ! minimum allowable NO3 (kg/ha)
      real         soil_temp(max_layer)   ! soil temperature (oC)
      real         sw_dep(max_layer)      ! soil water content (mm)
      real         dlt_N_sed              !  total N / C losses
      real         dlt_C_loss_sed
      integer    p_N_reduction            ! (on or off)
      real       dlt_rntrf(max_layer)     ! nitrogen moved by nitrification
                                          !    (kg/ha)
      real       dlt_urea_hydrol(max_layer) ! nitrogen moved by hydrolysis (kg/ha)
      real       excess_nh4(max_layer)      ! excess N required above NH4 supply

      real oldC
      real oldN

      ! CHARACTER
      character   soiltype*32               ! soil type spec used to determine mineralisation parameters.
      character   pond_active*10            ! parameter to indicate whether the soil is under flooded & ponded conditions
      character   residue_module*(Max_module_name_size)
                                          ! list of modules  replying
      character   residue_name(max_residues)*(Max_module_name_size)
      character   residue_type(max_residues)*(Max_module_name_size)
                                          ! list of module types
      character   fom_types(max_fom_type)*32
                                          ! list of fom types

      ! LOGICAL
      logical   use_external_st           ! flag for soil temperature
      logical   use_external_tav_amp      ! flag for soil ph
      logical   use_external_ph           ! flag for soil ph
      logical   use_organic_solutes       ! flag for FOM leaching

   end type Soiln2Globals
! ====================================================================
!      type Soiln2Parameters
!      end type Soiln2Parameters
! ====================================================================
   type Soiln2Constants
      sequence
      real         CNrf_coeff             ! coeff. to determine the magnitude
                                          ! of C:N effects on decomposition of
                                          ! FOM ()
      real         CNrf_optCN             ! C:N above which decomposition rate
                                          ! of FOM declines ()
      real         fom_min                ! minimum allowable FOM (kg/ha)
      real         fr_fom(nfract,max_fom_type)
                                          ! carbohydrate, cellulose & lignin
                                          ! fractions of FOM (0-1)
      real         OC2OM_factor           ! conversion from OC to OM
      real         opt_temp(2)            ! Soil temperature above which there
                                          ! is no further effect on mineralisation
                                          ! and nitrification (oC)
      real         NH4ppm_min             ! minimum allowable NH4 (ppm)
      real         no3ppm_min             ! minimum allowable NO3 (ppm)
      real         wf_min_index(max_wf_values)
                                          ! index specifying water content
                                          ! for water factor for mineralization
      real         wf_min_values(max_wf_values)
                                          ! value of water factor(mineralization)
                                          ! function at given index values
      real         wf_nit_index(max_wf_values)
                                          ! index specifying water content
                                          ! for water factor for nitrification
      real         wf_nit_values(max_wf_values)
                                          ! value of water factor(nitrification)
                                          ! function at given index values
      real         enr_a_coeff            ! enrichment equation coefficients
      real         enr_b_coeff
      real         nitrification_pot      ! Potential nitrification by soil (ppm)
      real         nh4_at_half_pot        ! nh4 conc at half potential (ppm)
      real         pHf_nit_pH(max_pHf_values)
                                          ! pH values for specifying
                                          ! pH factor for nitrification
      real         pHf_nit_values(max_pHf_values)
                                          ! value of pH factor(nitrification)
                                          ! function for given pH values
      real         dnit_rate_coeff        ! denitrification rate coefficient (kg/mg)
      real         dnit_wf_power          ! denitrification water factor power term

      ! MINERALISATION CONSTANTS
      real         ef_biom                ! fraction of biomass C mineralized
                                          ! retained in system (0-1)
      real         ef_fom                 ! fraction of FOM C mineralized
                                          ! retained in system (0-1)
      real         ef_hum                 ! fraction of humic C mineralized
                                          ! retained in system (0-1)
      real         ef_res                 ! fraction of residue C mineralized
                                          ! retained in system (0-1)
      real         fr_biom_biom           ! fraction of retained biomass C
                                          ! returned to biomass (0-1)
      real         fr_fom_biom            ! fraction of retained FOM C
                                          ! transferred to biomass (0-1)
      real         fr_res_biom            ! fraction of retained residue C
                                          ! transferred to biomass (0-1)
      real         mCN                    ! C:N ratio of microbes ()
      real         min_depth              ! depth from which mineral N can be
                                          ! immobilized by decomposing residues
                                          ! (mm)
      real         rd_biom(2)             ! potential rate of soil biomass
                                          ! mineralization (per day)
      real         rd_fom(nfract,2)       ! maximum rate constants for
                                          ! decomposition of FOM pools (0-1)
      real         rd_hum(2)              ! potential rate of humus
                                          ! mineralization (per day)
   end type Soiln2Constants
! ====================================================================
   type IDsType
      sequence
      integer :: ExternalMassFlow
      integer :: new_solute
      integer :: n_balance
      integer :: c_balance
      integer :: actualresiduedecompositioncalculated
      integer :: reset
      integer :: sum_report
      integer :: IncorpFOM
      integer :: tick
      integer :: newmet
      integer :: potentialresiduedecompositioncalculated
      integer :: IncorpFOMPool
      integer :: new_profile
      integer :: process
      integer :: NitrogenChanged
   end type IDsType


   ! instance variables.
   common /InstancePointers/ ID,g,p,c
   save InstancePointers
   type (Soiln2Globals),pointer :: g
   type (Soiln2Constants),pointer :: c
   type (IDsType), pointer :: id


   contains




!     ===========================================================
subroutine soiln2_reset ()
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Purpose
!      Initialise SoilN module

!+  Mission Statement
!      (Re)Initialise SoilN module

!+  Constant Values
    character  my_name*(*)
    parameter (my_name='soiln2_reset')

!- Implementation Section ----------------------------------
   call push_routine (my_name)

   ! Save State
   call soiln2_save_state ()

   ! Zero internal State Variables
   call soiln2_zero_variables ()

   ! Check data dependencies with other modules
   call soiln2_check_data_supply ()

   ! Get information specific to the site
   call soiln2_get_site_variables()

   ! Get information which may vary through time
   call soiln2_get_other_variables ()

   ! Get all parameters from parameter file
   call soiln2_read_param ()

   ! Get all coefficients from parameter file
   call soiln2_read_constants ()

   ! Perform initial calculations from inputs
   call soiln2_init_calc()

   ! Change of State
   call soiln2_delta_state ()

   call pop_routine (my_name)
   return
end subroutine



!     ===========================================================
subroutine soiln2_save_state ()
!     ===========================================================
   Use Infrastructure
   implicit none


!+  Sub-Program Arguments

!+  Purpose
!     Calculate Organic Carbon Percentage

!+  Mission Statement
!     Calculate Organic Carbon Percentage

!+  Calls


!+  Local Variables

!+  Constant Values
   character*(*) myname               ! name of current procedure
   parameter (myname = 'soiln2_save_state')

!- Implementation Section ----------------------------------
   call push_routine (myname)

   g%oldN = soiln2_total_n()
   g%oldC = soiln2_total_c()

   call pop_routine (myname)
   return
end subroutine

!     ===========================================================
subroutine soiln2_delta_state ()
!     ===========================================================
   Use Infrastructure
   implicit none


!+  Sub-Program Arguments

!+  Purpose
!     Calculate Organic Carbon Percentage

!+  Mission Statement
!     Calculate Organic Carbon Percentage

!+  Calls


!+  Local Variables
      real       dltN
      real       newN
      real       dltC
      real       newC

!+  Constant Values
   character*(*) myname               ! name of current procedure
   parameter (myname = 'soiln2_delta_state')

!- Implementation Section ----------------------------------
   call push_routine (myname)

   newN = soiln2_total_n()
   newC = soiln2_total_c()

   dltN = newN - g%oldN
   dltC = newC - g%oldC

   call soilN2_ExternalMassFlow (dltN)
   call soilN2_ExternalMassFlowC (dltC)


   call pop_routine (myname)
   return
end subroutine

!     ===========================================================
real function soiln2_total_c ()
!     ===========================================================
   Use Infrastructure
   implicit none
   integer    num_layers
   integer    layer                 ! layer number
   real       fom_c (max_layer)     ! fresh organic C (kg/ha)
   real       carbon_tot  ! total carbon in soil(kg/ha)

!- Implementation Section ----------------------------------

      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call fill_real_array (fom_c, 0.0, max_layer)

      do layer = 1, num_layers
        fom_c(layer) = sum_real_array (g%fom_c_pool(1,layer), nfract)
      end do

      carbon_tot = 0.0
      do layer = 1, num_layers
         carbon_tot = carbon_tot + fom_c(layer)+ g%hum_c(layer)+ g%biom_c(layer)
      end do

   soiln2_total_c = carbon_tot

   return
end function

!     ===========================================================
real function soiln2_total_n ()
!     ===========================================================
   Use Infrastructure
   implicit none
   integer    num_layers

!- Implementation Section ----------------------------------

   num_layers = count_of_real_vals (g%dlayer, max_layer)
   soiln2_total_n = sum(g%fom_n(1:num_layers)) + sum(g%hum_n(1:num_layers)) + sum(g%biom_n(1:num_layers)) &
                  + sum(g%no3(1:num_layers)) + sum(g%nh4(1:num_layers)) + sum(g%urea(1:num_layers))

   return
end function

!     ===========================================================
subroutine soiln2_read_param ()
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Purpose
!      Read in all parameters from parameter file.

!+  Mission Statement
!     Read Parameters from par file

!+  Calls

!+  Constant Values
   character*(*) section_name       !
   parameter (section_name = 'parameters')
!
   character*(*) my_name
   parameter (my_name='soiln2_read_param')

!+  Local Variables
   integer    i                     ! counter
   integer    layer                 ! Soil layer number
   real       no3(max_layer)        ! soil nitrate(ppm)
   real       nh4(max_layer)        ! soil ammonium(ppm)
   integer    numvals               ! number of values read from file
   real       ureappm(max_layer)    ! soil g_urea(ppm)
   character  string*80

!+  Initial Data Values

   call fill_real_array (no3, 0.0, max_layer)
   call fill_real_array (nh4, 0.0, max_layer)
   call fill_real_array (ureappm,0.0, max_layer)

!- Implementation Section ----------------------------------
   call push_routine (my_name)

   call write_string (  new_line//'   - Reading Parameters')

   ! read in setting for soil type which is used to determine the mineralisation
   ! model parameters section from the ini file.
   call read_char_var_optional (section_name, 'soiltype', '()', g%soiltype, numvals)
   if (numvals.le.0) g%soiltype = 'standard'

      ! Get parameter file name from control file and open it
   if (.not. g%use_external_st) then
      ! only need to read these if soil temp is not external
      call get_real_var_optional ( unknown_module, 'amp','(oC)',g%amp,numvals,0.0,50.0)

      if (numvals .ne.1) then
         g%use_external_tav_amp = .false.
         call read_real_var (section_name, 'amp', '(oC)', g%amp, numvals, 0.0, 50.0)

      else
         ! got amp from system ok
         g%use_external_tav_amp = .true.
      endif

      call get_real_var_optional (unknown_module,'tav','(oC)',g%ave_temp,numvals,0.0,50.0)

      if (numvals .ne.1) then
         if (g%use_external_tav_amp) then
            call fatal_error (err_internal, 'Default AMP with external TAV not permitted.')
         else
         endif
         call read_real_var (section_name, 'tav', '(oC)', g%ave_temp, numvals, 0.0, 50.0)

      else
         ! got tav from system ok
         if (g%use_external_tav_amp) then
         else
            call fatal_error (err_internal, 'External AMP with default TAV not permitted.')
         endif
      endif

   endif

   call read_real_var (section_name, 'root_cn', '()', g%root_cn, numvals, 0.0, 500.0)


   !dsg   Optionally read in CN ratio in each of the fractions
   call read_real_array_optional (section_name, 'root_cn_pool', 3, '()', g%root_cn_pool, numvals, 0.0, 1000.0)

   !dsg    Check if all values supplied.  If not use average C:N ratio in all pools
   if (numvals.lt.3) then
     do  i = 1,3
       g%root_cn_pool(i)=g%root_cn
     end do
   endif


   call read_real_var (section_name, 'root_wt', '(kg/ha)', g%root_wt, numvals, 0.0, 100000.0)

   call read_real_var_optional (section_name, 'root_depth', '(kg/ha)', g%root_depth, numvals, 0.0, 10000.0)
     ! dsg 180604 if 'root_depth' not provided, assume that 'root_wt' is distributed over whole profile
     if (numvals.eq.0) then
        g%root_depth = sum_real_array (g%dlayer, max_layer)
     endif

   call read_real_var (section_name, 'soil_cn', '()', g%soil_cn, numvals, 5.0, 30.0)

   call read_real_array (section_name, 'oc', max_layer, '(%)', g%oc, numvals, 0.01, 4.0)

   if (.not. g%use_external_ph) then
      call read_real_array (section_name, 'ph', max_layer, '()', g%ph, numvals, 3.5, 11.0)
   endif

   call read_real_array (section_name, 'fbiom', max_layer, '()', g%fr_biom_c, numvals, 0.0, 1.0)

   call read_real_array ( &
              section_name, 'finert', max_layer, '()', g%fr_inert_c, numvals, 0.0, 1.0)

   call read_real_array (section_name, 'no3ppm', max_layer, '(ppm)', no3, numvals, 0.0, 300.0)

   call read_real_array (section_name, 'nh4ppm', max_layer, '(ppm)', nh4, numvals, 0.0, 300.0)

   call read_real_array_optional (section_name, 'ureappm', max_layer, '(ppm)', ureappm, numvals, 0.0, 600.0)

   call read_real_var (section_name, 'enr_a_coeff', '()', c%enr_a_coeff, numvals, 1.0, 20.0)

   call read_real_var (section_name, 'enr_b_coeff', '()', c%enr_b_coeff, numvals, 0.0, 20.0)

   call read_char_var (section_name, 'profile_reduction', '()', string, numvals)

   if (string(1:2) .eq. 'on') then
      g%p_n_reduction = on

   else
      g%p_n_reduction = off

   endif

   string = '  '
   call read_char_var_optional (section_name, 'use_organic_solutes', '()', string, numvals)
   if (string(1:2) .eq. 'on') then
      g%use_organic_solutes = .true.
   else
      g%use_organic_solutes = .false.
   endif

   do layer=1,max_layer
      g%nh4(layer) = divide (nh4(layer), soiln2_fac (layer), 0.0)
      g%no3(layer) = divide (no3(layer), soiln2_fac (layer), 0.0)
      g%urea(layer) = divide (ureappm(layer), soiln2_fac (layer), 0.0)
   end do

   call pop_routine (my_name)
   return
end subroutine



!     ===========================================================
subroutine soiln2_zero_all_globals ()
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Purpose
!       Zero all global variables & arrays

!+  Mission statement
!       Zero all global variables and arrays

!+  Constant Values
   character  my_name*(*)           ! name of procedure
   parameter (my_name  = 'soiln2_zero_all_globals')

!- Implementation Section ----------------------------------

   call push_routine (my_name)

       ! Globals

   g%num_fom_types    = 0
   g%fom_type         = 0
   g%amp              = 0.0
   g%latitude         = 0.0
   g%maxt             = 0.0
   g%mint             = 0.0
   g%radn             = 0.0
   g%salb             = 0.0
   g%surf_temp(:)     = 0.0
   g%ave_temp         = 0.0
   g%dlt_soil_loss    = 0.0
   g%day_of_year      = 0
   g%year             = 0
   g%root_CN          = 0.0
   g%root_wt          = 0.0
   g%soil_CN          = 0.0
   g%biom_C(:)        = 0.0
   g%biom_N(:)        = 0.0
   g%fom_N(:)         = 0.0
   g%fom_c_pool(:, :)   = 0.0
   g%fom_n_pool(:, :) = 0.0
   g%dlt_fom_c_pool1(:) = 0.0
   g%dlt_fom_c_pool2(:) = 0.0
   g%dlt_fom_c_pool3(:) = 0.0
   g%hum_C(:)         = 0.0
   g%hum_N(:)         = 0.0
   g%inert_C(:)       = 0.0
   g%NH4(:)           = 0.0
   g%NO3(:)           = 0.0
   g%NH4_yesterday(:) = 0.0
   g%NO3_yesterday(:) = 0.0
   g%urea(:)          = 0.0
   g%num_residues     = 0
   g%dlt_biom_C_atm(:)= 0.0
   g%dlt_biom_C_hum(:)    = 0.0
   g%dlt_biom_N_min(:)    = 0.0
   g%dlt_C_decomp(:,:)    = 0.0
   g%dlt_C_incorp(:)      = 0.0
   g%dlt_NO3_dnit(:)      = 0.0
   g%dlt_fom_C_atm(:,:)   = 0.0
   g%dlt_fom_C_biom(:,:)  = 0.0
   g%dlt_fom_C_hum(:,:)   = 0.0
   g%dlt_fom_N_min(:)     = 0.0
   g%dlt_hum_C_atm(:)     = 0.0
   g%dlt_hum_C_biom(:)    = 0.0
   g%dlt_hum_N_min(:)     = 0.0
   g%dlt_N_decomp(:)      = 0.0
   g%dlt_fom_N_incorp(:)    = 0.0
   g%dlt_fom_n(:,:)         = 0.0
   g%dlt_res_c_atm(:, :)    = 0.0
   g%dlt_res_c_biom(:, :)   = 0.0
   g%dlt_res_c_hum(:, :)    = 0.0
   g%pot_C_decomp(:)        = 0.0
   g%pot_N_decomp(:)        = 0.0
   g%dlt_res_C_decomp(:, :) = 0.0
   g%dlt_res_N_decomp(:, :) = 0.0
   g%dlt_res_nh4_min (:)    = 0.0
   g%dlt_res_no3_min (:)    = 0.0
   g%NO3_transform_net(:)   = 0.0
   g%NH4_transform_net(:)   = 0.0
   g%dlt_NO3_net(:)         = 0.0
   g%dlt_NH4_net(:)         = 0.0
   g%bd(:)                  = 0.0
   g%dlayer(:)              = 0.0
   g%dul_dep(:)             = 0.0
   g%fr_biom_C(:)           = 0.0
   g%fr_inert_C(:)          = 0.0
   g%ll15_dep(:)            = 0.0
   g%oc(:)                  = 0.0
   g%ph(:)                  = 0.0
   g%sat_dep(:)             = 0.0
   g%NH4_min(:)             = 0.0
   g%NO3_min(:)             = 0.0
   g%soil_temp(:)           = 0.0
   g%sw_dep(:)              = 0.0
   g%dlt_N_sed              = 0.0
   g%dlt_C_loss_sed         = 0.0
   g%p_N_reduction    = 0
   g%oldC = 0.0
   g%oldN = 0.0
   g%use_organic_solutes = .false.
   g%residue_name(:)    = blank
   g%residue_type(:)     = blank
   g%fom_types(:)         = blank
   g%pond_active = 'no'

   ! I think that this needs to be true by default
   ! for comms to work - NIH
   g%use_external_st      = .true.
   ! Not sure about these - NIH
   g%use_external_tav_amp = .false.
   g%use_external_ph      = .false.

   g%dlt_rntrf(:)         = 0.0
   g%dlt_urea_hydrol(:)   = 0.0
   g%excess_nh4(:)        = 0.0

      ! Constants
   c%CNrf_coeff            = 0.0
   c%CNrf_optCN            = 0.0
   c%fom_min               = 0.0
   c%fr_fom(:,:)           = 0.0
   c%OC2OM_factor          = 0.0
   c%opt_temp(2)           = 0.0
   c%NH4ppm_min            = 0.0
   c%no3ppm_min            = 0.0
   c%wf_min_index(:)       = 0.0
   c%wf_min_values(:)      = 0.0
   c%wf_nit_index(:)       = 0.0
   c%wf_nit_values(:)      = 0.0
   c%enr_a_coeff           = 0.0
   c%enr_b_coeff           = 0.0
   c%nitrification_pot     = 0.0
   c%nh4_at_half_pot       = 0.0
   c%pHf_nit_pH(:)         = 0.0
   c%pHf_nit_values(:)     = 0.0
   c%dnit_rate_coeff       = 0.0
   c%dnit_wf_power         = 0.0
   c%ef_biom               = 0.0
   c%ef_fom                = 0.0
   c%ef_hum                = 0.0
   c%ef_res                = 0.0
   c%fr_biom_biom          = 0.0
   c%fr_fom_biom           = 0.0
   c%fr_res_biom           = 0.0
   c%mCN                   = 0.0
   c%min_depth             = 0.0
   c%rd_biom(:)            = 0.0
   c%rd_fom(:,:)           = 0.0
   c%rd_hum(:)             = 0.0

   call pop_routine (my_name)
   return
end subroutine
!     ===========================================================
subroutine soiln2_zero_variables ()
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Purpose
!       Zero soil nitrogen variables

!+  Mission Statement
!     Zero variables

!+  Constant Values
   character  my_name*(*)           ! subroutine name
   parameter (my_name = 'soiln2_zero_variables')


!- Implementation Section ----------------------------------

   call push_routine (my_name)

   g%amp            = 0.0
   g%latitude       = 0.0
   g%salb           = 0.0
   g%surf_temp      = 0.0
   g%ave_temp       = 0.0
   g%dlt_soil_loss  = 0.0

   g%root_CN = 0.0
   g%root_wt = 0.0
   g%soil_CN = 0.0
   g%root_CN_pool(:) = 0.0

   g%dlt_biom_C_atm = 0.0
   g%dlt_biom_C_hum = 0.0
   g%dlt_biom_N_min = 0.0
   g%dlt_C_decomp = 0.0
   g%dlt_C_incorp = 0.0
   g%dlt_NO3_dnit = 0.0
   g%dlt_fom_C_atm = 0.0
   g%dlt_fom_C_biom = 0.0
   g%dlt_fom_C_hum = 0.0
   g%dlt_fom_N_min = 0.0
   g%dlt_hum_C_atm = 0.0
   g%dlt_hum_C_biom = 0.0
   g%dlt_hum_N_min = 0.0
   g%dlt_N_decomp = 0.0
   g%dlt_fom_N_incorp = 0.0
   g%dlt_fom_n(:,:) = 0.0
   g%dlt_res_c_biom = 0.0
   g%dlt_res_c_hum = 0.0
   g%dlt_res_c_atm = 0.0
   g%pot_C_decomp(:) = 0.0
   g%pot_N_decomp(:) = 0.0
   g%pot_P_decomp(:) = 0.0
   g%dlt_res_C_decomp = 0.0
   g%dlt_res_N_decomp = 0.0
   g%dlt_res_nh4_min = 0.0
   g%dlt_res_no3_min = 0.0
   g%NH4_transform_net = 0.0
   g%NO3_transform_net = 0.0
   g%dlt_NH4_net       = 0.0
   g%dlt_rntrf(:)         = 0.0
   g%dlt_urea_hydrol(:)   = 0.0
   g%excess_nh4(:)        = 0.0
   g%dlt_NO3_net       = 0.0
   g%dlt_fom_n(:,:) = 0.0

   c%fom_min = 0.0
   c%no3ppm_min = 0.0
   c%nh4ppm_min = 0.0

   g%biom_C(:)          = 0.0
   g%biom_N(:)          = 0.0
   g%fom_N(:)           = 0.0
   g%fom_c_pool(:,:)    = 0.0
   g%fom_n_pool(:,:)    = 0.0
   g%hum_C(:)           = 0.0
   g%hum_N(:)           = 0.0
   g%inert_C(:)         = 0.0
   g%NH4(:)             = 0.0
   g%NO3(:)             = 0.0
   g%NH4_yesterday(:)   = 0.0
   g%NO3_yesterday(:)   = 0.0
   g%urea(:)            = 0.0
   g%num_residues = 0

   g%fr_biom_C      = 0.0
   g%fr_inert_C     = 0.0
   g%oc             = 0.0
   g%ph             = 0.0
   g%NO3_min        = 0.0
   g%NH4_min        = 0.0
   g%soil_temp      = 0.0
   g%sw_dep         = 0.0
   g%dlt_N_sed      = 0.0
   g%dlt_C_loss_sed = 0.0
   g%p_n_reduction    = 0
   g%use_organic_solutes = .false.

   c%oc2om_factor      = 0.0
   c%CNrf_coeff        = 0.0
   c%CNrf_optCN        = 0.0
   c%fom_min           = 0.0
   c%fr_fom            = 0.0
   c%NH4ppm_min        = 0.0
   c%no3ppm_min        = 0.0
   c%opt_temp(:)       = 0.0
   c%wf_min_index      = 0.0
   c%wf_min_values     = 0.0
   c%wf_nit_index      = 0.0
   c%wf_nit_values     = 0.0
   c%enr_a_coeff       = 0.0
   c%enr_b_coeff       = 0.0
   c%nitrification_pot = 0.0
   c%nh4_at_half_pot   = 0.0
   c%pHf_nit_pH        = 0.0
   c%pHf_nit_values    = 0.0
   c%dnit_rate_coeff   = 0.0
   c%dnit_wf_power     = 0.0
   g%num_fom_types     = 0

   c%ef_biom           = 0.0
   c%ef_fom            = 0.0
   c%ef_hum            = 0.0
   c%ef_res            = 0.0
   c%fr_biom_biom      = 0.0
   c%fr_fom_biom       = 0.0
   c%fr_res_biom       = 0.0
   c%mCN               = 0.0
   c%min_depth         = 0.0
   c%rd_biom(:)        = 0.0
   c%rd_fom(:,:)       = 0.0
   c%rd_hum(:)         = 0.0

   g%residue_name = ' '
   g%residue_type  = ' '
   g%fom_types      = ' '

   ! I think that this needs to be true by default
   ! for comms to work - NIH
   g%use_external_st      = .true.
   ! Not sure about these - NIH
   g%use_external_ph = .false.
   g%use_external_tav_amp = .false.

   call pop_routine (my_name)
   return
end subroutine



!     ===========================================================
subroutine soiln2_get_other_variables ()
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Purpose
!      Get the values of variables from other modules

!+  Mission Statement
!     Get Other Variables

!+  Constant Values
   character  my_name*(*)
   parameter (my_name='soiln2_get_other_variables')

!+  Local Variables
   integer      numvals             ! number of values returned

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   call get_real_array (unknown_module, 'sw_dep', max_layer, '(mm)', g%sw_dep, numvals, 0.00001, 1000.0)
   call get_real_array (unknown_module, 'dlayer', max_layer, '(mm)', g%dlayer, numvals, 0.0, 10000.0)

    if (g%p_n_reduction.eq.on) then
      ! ONLY need soil loss if profile reduction is on

      call get_real_var_optional (unknown_module,'soil_loss','(t/ha)',g%dlt_soil_loss,numvals,0.0,1000.0)
   endif

   if (g%use_external_ph) then
      g%ph  = 0.0

      call get_real_array (unknown_module, 'ph', max_layer, '()', g%ph, numvals, 3.5, 11.0)

   endif

   call soiln2_check_pond()


   call pop_routine (my_name)
   return
end subroutine


!     ===========================================================
subroutine soiln2_check_pond ()
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Purpose
!      Get the values of variables from other modules

!+  Mission Statement
!     Get Other Variables

!+  Constant Values
   character  my_name*(*)
   parameter (my_name='soiln2_check_pond')

!+  Local Variables
   integer      numvals             ! number of values returned

!- Implementation Section ----------------------------------

   call push_routine (my_name)

! dsg 180508 check for the presence of a pond
      call get_char_var_optional (Unknown_module,'pond_active','',g%pond_active,numvals)
     
      if (numvals.eq.0) then
          g%pond_active = 'no'
      endif


   call pop_routine (my_name)
   return
end subroutine


!     ===========================================================
subroutine Soiln2_sendActualResidueDecompositionCalculated()
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
   parameter (my_name ='Soiln2_sendActualResidueDecompositionCalculated')
!+  Local Variables
    character*200  message
    integer   residue                     ! residue number
    integer   num_layers                  ! number of layers in profile
    real      dlt_res_c_decomp(max_layer) ! amount of c to decompose into each layer for each residue
    real      dlt_res_n_decomp(max_layer) ! amount of n to decompose into each layer for each residue
    real      c_summed                    ! total amount of c to decompose
    real      c_summed_layer(max_layer)   ! total amount of c to decompose into each layer
    real      n_summed                    ! total amount of n to decompose
    type (SurfaceOrganicMatterDecompType)::SurfaceOrganicMatterDecomp


!- Implementation Section ----------------------------------
   call push_routine (my_name)

   ! dsg 131004 these are variables calculated so that soilp can 'get' them  - yukko
   g%soilp_dlt_res_c_atm(:) = 0.0
   g%soilp_dlt_res_c_hum(:) = 0.0
   g%soilp_dlt_res_c_biom(:) = 0.0
   g%soilp_dlt_org_p(:) = 0.0
   c_summed_layer(:) = 0.0

   num_layers = count_of_real_vals (g%dlayer, max_layer)

   ! Potential decomposition was given to this module by a residue
   ! module.  We now explicitly tell the residue module to decompose
   ! some of its residue now.  If we have been unable to decompose the
   ! potential it gave us, the value returned belows will have been
   ! reduced to the actual value.

   do residue = 1, g%num_residues

       dlt_res_c_decomp(:) = g%dlt_res_c_hum(:,residue)+ g%dlt_res_c_biom(:,residue)+ g%dlt_res_c_atm(:,residue)

       c_summed = sum(dlt_res_c_decomp)

       dlt_res_n_decomp(:) = g%dlt_res_n_decomp(:,residue)
       n_summed = sum(dlt_res_n_decomp)

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

     ! dsg 131004 soilp needs some stuff - very ugly process - needs to be streamlined
     !    create some variables which soilp can "get" - layer based arrays independant of residues

       g%soilp_dlt_res_c_atm(:) = g%soilp_dlt_res_c_atm(:) +  g%dlt_res_c_atm(:,residue)
       g%soilp_dlt_res_c_hum(:) = g%soilp_dlt_res_c_hum(:) +  g%dlt_res_c_hum(:,residue)
       g%soilp_dlt_res_c_biom(:) = g%soilp_dlt_res_c_biom(:) +  g%dlt_res_c_biom(:,residue)
       c_summed_layer(:) = c_summed_layer(:) + dlt_res_c_decomp(:)
   end do
   SurfaceOrganicMatterDecomp%num_Pool = g%num_residues

     ! dsg 131004  calculate the old dlt_org_p (from the old Decomposed event sent by residue2) for getting by soilp
       g%soilp_dlt_org_p(:) = c_summed_layer(:) * divide (sum(g%pot_P_decomp), sum(g%pot_C_decomp), 0.0)


   call publish_SurfaceOrganicMatterDecomp(id%ActualResidueDecompositionCalculated, SurfaceOrganicMatterDecomp)


   call pop_routine (my_name)
   return
end subroutine



!     ===========================================================
subroutine soiln2_send_my_variable (variable_name)
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Sub-Program Arguments
   character  variable_name*(*)     ! (INPUT) Variable name to
                                    ! search for

!+  Purpose
!      Return the value of one of our variables to caller

!+  Mission Statement
!     Send Value of Requested Variable

!+  Calls


!+  Constant Values
   character  my_name*(*)
   parameter (my_name='soiln2_Send_my_variable')

!+  Local Variables
   integer    layer                 ! layer number
   integer    indx                 ! index - 1 for aerobic and 2 for anaerobic conditions
   real       fom_c (max_layer)     ! fresh organic C (kg/ha)
   real       fom_c_pool1(max_layer) ! fresh organic C in pool 1 (kg/ha)
   real       fom_c_pool2(max_layer) ! fresh organic C in pool 2 (kg/ha)
   real       fom_c_pool3(max_layer) ! fresh organic C in pool 3 (kg/ha)
   real       fom_n (max_layer)     ! fresh organic N (kg/ha)
   real       fom_n_pool1(max_layer) ! fresh organic N in pool 1 (kg/ha)
   real       fom_n_pool2(max_layer) ! fresh organic N in pool 1 (kg/ha)
   real       fom_n_pool3(max_layer) ! fresh organic N in pool 1 (kg/ha)
   real       dlt_net_n_min(max_layer) ! net mineralisation (kg/ha)
   real       nh4ppm(max_layer)     ! soil ammonium conc (ppm)
   real       no3ppm(max_layer)     ! soil nitrate conc (ppm)
   integer    num_layers            ! number of soil layers
   real       carbon_tot (max_layer) ! total carbon in soil(kg/ha)
   real       nit_tot (max_layer)   ! total N in soil (kg/ha)
   real       org_c (max_layer)     ! organic c
   real       org_n (max_layer)     ! organic n
   real       oc_percent(max_layer) ! organic c %
   real       temp(max_layer)
   character   err_string*80
!- Implementation Section ----------------------------------
   call push_routine (my_name)

   if (variable_name .eq. 'no3') then
   !                       ----
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name,'(kg/ha)', g%no3, num_layers)

   elseif (variable_name .eq. 'dlt_no3_net') then
   !                           ----
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name,'(kg/ha)', g%dlt_no3_net, num_layers)


   elseif (variable_name .eq. 'no3ppm') then
   !                           ------
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call fill_real_array (no3ppm,0.0,max_layer)

      do layer=1,num_layers
         no3ppm(layer) = g%no3(layer)*soiln2_fac (layer)
      end do

      call respond2get_real_array (variable_name,'(ppm)', no3ppm, num_layers)

   elseif (variable_name .eq. 'no3_min') then
   !                           --------
      num_layers = count_of_real_vals (g%dlayer, max_layer)

      call respond2get_real_array (variable_name,'(kg/ha)', g%no3_min, num_layers)

   elseif (variable_name .eq. 'nh4') then
   !                           ----
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name,'(kg/ha)', g%nh4, num_layers)


   elseif (variable_name .eq. 'dlt_nh4_net') then
   !                           ----
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name,'(kg/ha)', g%dlt_nh4_net, num_layers)


   elseif (variable_name .eq. 'nh4ppm') then
   !                           ------
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call fill_real_array (nh4ppm,0.0,max_layer)

      do layer=1,num_layers
         nh4ppm(layer) = g%nh4(layer)*soiln2_fac (layer)
      end do

      call respond2get_real_array (variable_name,'(ppm)', nh4ppm, num_layers)

   elseif (variable_name .eq. 'nh4_min') then
   !                           --------
      num_layers = count_of_real_vals (g%dlayer, max_layer)

      call respond2get_real_array (variable_name,'(kg/ha)', g%nh4_min, num_layers)


   elseif (variable_name .eq. 'urea') then
   !                           ----
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name,'(kg/ha)', g%urea, num_layers)


   elseif (variable_name .eq. 'ureappm') then
   !                           ------
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call fill_real_array (ureappm,0.0,max_layer)

      do layer=1,num_layers
         ureappm(layer) = g%urea(layer)*soiln2_fac (layer)
      end do

      call respond2get_real_array (variable_name,'(ppm)', ureappm, num_layers)

   elseif (variable_name .eq. 'dlt_rntrf') then
   !                           ----
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name,'(kg/ha)', g%dlt_rntrf, num_layers)

   elseif (variable_name .eq. 'dlt_urea_hydrol') then
   !                           ----
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name,'(kg/ha)', g%dlt_urea_hydrol, num_layers)

   elseif (variable_name .eq. 'excess_nh4') then
   !                           ----
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name,'(kg/ha)', g%excess_nh4, num_layers)

    elseif (variable_name .eq. 'fom_n') then
   !                           -----
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name,'(kg/ha)', g%fom_n, num_layers)


   elseif (variable_name .eq. 'fom_n_pool1') then
   !                           -----
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call fill_real_array (fom_n_pool1, 0.0, max_layer)

      do layer = 1, num_layers
        fom_n_pool1(layer) = g%fom_n_pool(1,layer)
      end do

      call respond2get_real_array (variable_name,'(kg/ha)', fom_n_pool1, num_layers)

   elseif (variable_name .eq. 'fom_n_pool2') then
   !                           -----
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call fill_real_array (fom_n_pool2, 0.0, max_layer)

      do layer = 1, num_layers
        fom_n_pool2(layer) = g%fom_n_pool(2,layer)
      end do

      call respond2get_real_array (variable_name,'(kg/ha)', fom_n_pool2, num_layers)

   elseif (variable_name .eq. 'fom_n_pool3') then
   !                           -----
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call fill_real_array (fom_n_pool3, 0.0, max_layer)

      do layer = 1, num_layers
        fom_n_pool3(layer) = g%fom_n_pool(3,layer)
      end do

      call respond2get_real_array (variable_name,'(kg/ha)', fom_n_pool3, num_layers)


   elseif (variable_name .eq. 'hum_n') then
   !                           -----
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name,'(kg/ha)', g%hum_n, num_layers)

   elseif (variable_name .eq. 'biom_n') then
   !                           ------
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name,'(kg/ha)', g%biom_n, num_layers)


   elseif (variable_name .eq. 'fom_c') then
   !                           -----
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call fill_real_array (fom_c, 0.0, max_layer)

      do layer = 1, num_layers
        fom_c(layer) = g%fom_c_pool(1,layer) + g%fom_c_pool(2,layer) +g%fom_c_pool(3,layer)
      end do

      call respond2get_real_array (variable_name,'(kg/ha)', fom_c, num_layers)

!dsg   the following 4 variables added so that soilp can 'get' fractional information

   elseif (variable_name .eq. 'num_fom_types') then
   !                           -------------
      call respond2get_integer_var (variable_name,'()', g%num_fom_types)


   elseif (variable_name .eq. 'fr_carb') then
   !                           -------------
      call respond2get_real_var (variable_name,'()', c%fr_fom(1,g%fom_type))


   elseif (variable_name .eq. 'fr_cell') then
   !                           -------------
      call respond2get_real_var (variable_name,'()', c%fr_fom(2,g%fom_type))


   elseif (variable_name .eq. 'fr_lign') then
   !                           -------------
      call respond2get_real_var (variable_name,'()', c%fr_fom(3,g%fom_type))



   elseif (variable_name .eq. 'fom_c_pool1') then
   !                           -----
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call fill_real_array (fom_c_pool1, 0.0, max_layer)

      do layer = 1, num_layers
        fom_c_pool1(layer) = g%fom_c_pool(1,layer)
      end do

      call respond2get_real_array (variable_name,'(kg/ha)', fom_c_pool1, num_layers)


   elseif (variable_name .eq. 'fom_c_pool2') then
   !                           -----
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call fill_real_array (fom_c_pool2, 0.0, max_layer)

      do layer = 1, num_layers
        fom_c_pool2(layer) = g%fom_c_pool(2,layer)
      end do

      call respond2get_real_array (variable_name,'(kg/ha)', fom_c_pool2, num_layers)


   elseif (variable_name .eq. 'fom_c_pool3') then
   !                           -----
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call fill_real_array (fom_c_pool3, 0.0, max_layer)

      do layer = 1, num_layers
        fom_c_pool3(layer) = g%fom_c_pool(3,layer)
      end do

      call respond2get_real_array (variable_name,'(kg/ha)', fom_c_pool3, num_layers)


   elseif (variable_name .eq. 'hum_c') then
   !                           -----
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name,'(kg/ha)', g%hum_c, num_layers)

   elseif (variable_name .eq. 'biom_c') then
   !                           ------
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name,'(kg/ha)', g%biom_c, num_layers)



   elseif (variable_name .eq. 'carbon_tot') then
   !                           -----
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call fill_real_array (carbon_tot,0.0,max_layer)
      call fill_real_array (fom_c, 0.0, max_layer)

      do layer = 1, num_layers
        fom_c(layer) = sum_real_array (g%fom_c_pool(1,layer), nfract)
      end do

      do layer = 1, num_layers
         carbon_tot(layer) = fom_c(layer)+ g%hum_c(layer)+ g%biom_c(layer)
      end do

      call respond2get_real_array (variable_name,'(kg/ha)', carbon_tot, num_layers)

   elseif (variable_name .eq. 'oc%') then
   !                           ------
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call fill_real_array (oc_percent,0.0,max_layer)
      call soiln2_oc_percent(oc_percent)
      call respond2get_real_array (variable_name,'(%)', oc_percent, num_layers)


   elseif (variable_name .eq. 'nh4_transform_net') then
   !                           ----
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name,'(kg/ha)', g%NH4_transform_net, num_layers)

   elseif (variable_name .eq. 'no3_transform_net') then
   !                           ----
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name,'(kg/ha)', g%NO3_transform_net, num_layers)

   elseif (variable_name .eq. 'dlt_res_nh4_min') then
   !                           ----
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name,'(kg/ha)', g%dlt_res_NH4_min, num_layers)

   elseif (variable_name .eq. 'dlt_fom_n_min') then
   !                           ----
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name,'(kg/ha)', g%dlt_fom_N_min, num_layers)

   elseif (variable_name .eq. 'dlt_biom_n_min') then
   !                           ----
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name,'(kg/ha)', g%dlt_biom_N_min, num_layers)

   elseif (variable_name .eq. 'dlt_hum_n_min') then
   !                           ----
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name,'(kg/ha)', g%dlt_hum_N_min, num_layers)

   elseif (variable_name .eq. 'dlt_res_no3_min') then
   !                           ----
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name,'(kg/ha)', g%dlt_res_NO3_min, num_layers)

   elseif (variable_name .eq. 'dlt_no3_dnit') then
   !                           ----
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name,'(kg/ha)', g%dlt_NO3_dnit, num_layers)

   elseif (variable_name .eq. 'nit_tot') then
   !                           -----
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call fill_real_array (nit_tot,0.0,max_layer)

      do layer = 1, num_layers
         nit_tot(layer) = g%fom_n(layer)+ g%hum_n(layer)+ g%biom_n(layer)+ g%no3(layer)+ g%nh4(layer)+ g%urea(layer)
      end do

      call respond2get_real_array (variable_name,'(kg/ha)', nit_tot, num_layers)
   elseif (variable_name .eq. 'dlt_n_min') then
   !                       ----
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call fill_real_array (dlt_net_n_min, 0.0, max_layer)

      do layer = 1, num_layers
         dlt_net_n_min(layer) = g%dlt_hum_n_min(layer)+ g%dlt_biom_n_min(layer)+ g%dlt_fom_n_min(layer)
      end do

      call respond2get_real_array (variable_name,'(kg/ha)', dlt_net_n_min, num_layers)

   elseif (variable_name .eq. 'dlt_n_min_res') then
   !                       ----
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call fill_real_array (dlt_net_n_min, 0.0, max_layer)

      do layer = 1, num_layers
         dlt_net_n_min(layer) = g%dlt_res_no3_min(layer)+ g%dlt_res_nh4_min(layer)
      end do

      call respond2get_real_array (variable_name,'(kg/ha)', dlt_net_n_min, num_layers)

   elseif (variable_name .eq. 'dlt_n_min_tot') then
   !                       ----
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call fill_real_array (dlt_net_n_min, 0.0, max_layer)

      do layer = 1, num_layers
         dlt_net_n_min(layer) = g%dlt_hum_n_min(layer)+ g%dlt_biom_n_min(layer)+ g%dlt_fom_n_min(layer)+ g%dlt_res_no3_min(layer)+ g%dlt_res_nh4_min(layer)
      end do

      call respond2get_real_array (variable_name,'(kg/ha)', dlt_net_n_min, num_layers)


   elseif (variable_name .eq. 'dnit') then
   !                           ----
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name,'(kg/ha)', g%dlt_no3_dnit, num_layers)

   elseif (variable_name .eq. 'dlt_c_loss_in_sed') then
   !                           -------------
      call respond2get_real_var (variable_name,'(kg)', g%dlt_c_loss_sed)

   elseif (variable_name .eq. 'dlt_n_loss_in_sed') then
   !                           -------------
      call respond2get_real_var (variable_name,'(kg)', g%dlt_n_sed)

   elseif (variable_name .eq. 'st'.and. .not.g%use_external_st) then
   !                           --
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name,'(oC)', g%soil_temp, num_layers)


   elseif (index(variable_name,'org_c_pool') .eq. 1) then
   !                           -----
      call fill_real_array (org_c,0.0,max_layer)

      num_layers = count_of_real_vals (g%dlayer, max_layer)

      call respond2get_real_array (variable_name,'(kg/ha)', org_c, num_layers)

   elseif (variable_name .eq. 'org_n') then
   !                           -----
      call fill_real_array (org_n,0.0,max_layer)

      num_layers = count_of_real_vals (g%dlayer, max_layer)

      call respond2get_real_array (variable_name,'(kg/ha)', org_n, num_layers)


   elseif (variable_name .eq. 'dlt_fom_c_hum') then
   !                           -------------
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call fill_real_array (fom_c, 0.0, max_layer)

      do layer = 1, num_layers
         fom_c(layer) = sum_real_array (g%dlt_fom_c_hum(1,layer),nfract)
      end do

      call respond2get_real_array (variable_name,'(kg/ha)', fom_c, num_layers)

   elseif (variable_name .eq. 'dlt_fom_c_biom') then
   !                           --------------
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call fill_real_array (fom_c, 0.0, max_layer)

      do layer = 1, num_layers
         fom_c(layer) = sum_real_array (g%dlt_fom_c_biom(1,layer),nfract)
      end do

      call respond2get_real_array (variable_name,'(kg/ha)', fom_c, num_layers)

   elseif (variable_name .eq. 'dlt_fom_c_atm') then
   !                           -------------
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call fill_real_array (fom_c, 0.0, max_layer)

      do layer = 1, num_layers
         fom_c(layer) = sum_real_array (g%dlt_fom_c_atm(1,layer),nfract)
      end do

      call respond2get_real_array (variable_name,'(kg/ha)', fom_c, num_layers)

   elseif (variable_name .eq. 'dlt_hum_c_biom') then
   !                           --------------
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name,'(kg/ha)', g%dlt_hum_c_biom, num_layers)

   elseif (variable_name .eq. 'dlt_hum_c_atm') then
   !                           -------------
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name,'(kg/ha)', g%dlt_hum_c_atm, num_layers)

   elseif (variable_name .eq. 'dlt_biom_c_hum') then
   !                           --------------
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name,'(kg/ha)', g%dlt_biom_c_hum, num_layers)

   elseif (variable_name .eq. 'dlt_biom_c_atm') then
   !                           --------------
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name,'(kg/ha)', g%dlt_biom_c_atm, num_layers)

   elseif (variable_name .eq. 'dlt_res_c_biom') then
   !                           --------------
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name,'(kg/ha)', sum (g%dlt_res_c_biom(:,:), dim=residue_dim), num_layers)

   elseif (variable_name .eq. 'dlt_res_c_hum') then
   !                           -------------
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name,'(kg/ha)', sum (g%dlt_res_c_hum(:,:), dim=residue_dim), num_layers)

   elseif (variable_name .eq. 'dlt_res_c_atm') then
   !                           -------------
      call respond2get_real_var (variable_name,'(kg/ha)', sum(g%dlt_res_c_atm(:,:)))

!dsg The following 3 dlt's are added so that soilp can 'get' them
   elseif (variable_name .eq. 'dlt_fom_c_pool1') then
   !                           --------------
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name,'(kg/ha)', g%dlt_fom_c_pool1, num_layers)

   elseif (variable_name .eq. 'dlt_fom_c_pool2') then
   !                           --------------
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name,'(kg/ha)', g%dlt_fom_c_pool2, num_layers)

   elseif (variable_name .eq. 'dlt_fom_c_pool3') then
   !                           --------------
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name,'(kg/ha)', g%dlt_fom_c_pool3, num_layers)

   elseif (variable_name .eq. 'soilp_dlt_res_c_atm') then
   !                           --------------
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name,'(kg/ha)', g%soilp_dlt_res_c_atm, num_layers)

   elseif (variable_name .eq. 'soilp_dlt_res_c_hum') then
   !                           --------------
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name,'(kg/ha)', g%soilp_dlt_res_c_hum, num_layers)

   elseif (variable_name .eq. 'soilp_dlt_res_c_biom') then
   !                           --------------
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name,'(kg/ha)', g%soilp_dlt_res_c_biom, num_layers)

   elseif (variable_name .eq. 'soilp_dlt_org_p') then
   !                           --------------
      num_layers = count_of_real_vals (g%dlayer, max_layer)
      call respond2get_real_array (variable_name,'(kg/ha)', g%soilp_dlt_org_p, num_layers)

   elseif (variable_name .eq. 'tf') then
   !                           --------------
      num_layers = count_of_real_vals (g%dlayer, max_layer)
             ! dsg 200508  use different values for some constants when anaerobic conditions dominate
             if (g%pond_active.eq.'no') then
                 indx = 1
             else if (g%pond_active.eq.'yes') then
                 indx = 2
             else
             endif
      do layer = 1, num_layers
         if (g%soiltype.eq.'rothc') then
            temp(layer) = rothc_tf (layer,indx)
         else
            temp(layer) = soiln2_tf (layer,indx)
         endif
      end do
      call respond2get_real_array (variable_name,'()', temp, num_layers)

   else
      call message_unused ()
   endif

   call pop_routine (my_name)
   return
end subroutine

!     ===========================================================
      subroutine soilN2_ExternalMassFlow (dltN)
!     ===========================================================
      Use Infrastructure
      implicit none

      real, intent(in) :: dltN

!+  Purpose
!     Update internal time record and reset daily state variables.

!+  Mission Statement
!     Update internal time record and reset daily state variables.

!+  Changes
!        260899 nih

!+  Local Variables
      type (ExternalMassFlowType) :: massBalanceChange

!+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'soilN2_ExternalMassFlow')

!- Implementation Section ----------------------------------
      call push_routine (myname)

      if (dltN >= 0.0) then
         massBalanceChange%FlowType = "gain"
      else
         massBalanceChange%FlowType = "loss"
      endif
         massBalanceChange%PoolClass = "soil"
         massBalanceChange%DM = 0.0
         massBalanceChange%C  = 0.0
         massBalanceChange%N  = abs(dltN)
         massBalanceChange%P  = 0.0
         massBalanceChange%SW = 0.0

         call publish_ExternalMassFlow(ID%ExternalMassFlow, massBalanceChange)


      call pop_routine (myname)
      return
      end subroutine


!     ===========================================================
      subroutine soilN2_ExternalMassFlowC (dltC)
!     ===========================================================
      Use Infrastructure
      implicit none

      real, intent(in) :: dltC

!+  Purpose
!     Update internal time record and reset daily state variables.

!+  Mission Statement
!     Update internal time record and reset daily state variables.

!+  Changes
!        260899 nih

!+  Local Variables
      type (ExternalMassFlowType) :: massBalanceChange

!+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'soilN2_ExternalMassFlowC')

!- Implementation Section ----------------------------------
      call push_routine (myname)

      if (dltC >= 0.0) then
         massBalanceChange%FlowType = "gain"
      else
         massBalanceChange%FlowType = "loss"
      endif
         massBalanceChange%PoolClass = "soil"
         massBalanceChange%DM = 0.0
         massBalanceChange%C  = abs(dltC)
         massBalanceChange%N  = 0.0
         massBalanceChange%P  = 0.0
         massBalanceChange%SW = 0.0

         call publish_ExternalMassFlow(ID%ExternalMassFlow, massBalanceChange)


      call pop_routine (myname)
      return
      end subroutine



!     ===========================================================
subroutine soiln2_set_my_variable (variable_name)
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
   parameter (my_name='soiln2_set_my_variable')

!+  Local Variables
   integer      layer               ! soil layer number
   real         nh4(max_layer)      ! ammonium(ppm)
   real         no3(max_layer)      ! nitrate (ppm)
   integer      numvals             ! number of values received
   real         tarray(max_layer)   ! temporary array
   character*80 string              ! tmp
   integer      pool_num            ! pool number
   character    error_string*100    ! error string
   character    err_string*80
      real       dltN
      real       oldN(max_layer)       ! temporary array

!- Implementation Section ----------------------------------
   call push_routine (my_name)

   if (variable_name .eq. 'no3') then
      oldN = g%no3
      call collect_real_array(variable_name, max_layer, '(kg/ha)', no3, numvals, 0.0, 1000.0)

      do layer = 1, numvals
         g%no3(layer) = no3(layer)
         call bound_check_real_var (g%no3(layer), g%no3_min(layer), g%no3(layer), 'g%NO3(layer)')
      end do
      dltN = sum(g%no3) - sum(oldN)
      call soilN2_ExternalMassFlow (dltN)

   elseif (variable_name .eq. 'no3ppm') then

      oldN = g%no3
      call fill_real_array (no3,0.0,max_layer)

      call collect_real_array(variable_name, max_layer, '(ppm)', no3, numvals, c%no3ppm_min, 1000.0)

      do layer=1,numvals
         g%no3(layer) = divide (no3(layer), soiln2_fac (layer), 0.0)
      end do
      dltN = sum(g%no3) - sum(oldN)
      call soilN2_ExternalMassFlow (dltN)

   elseif (variable_name .eq. 'nh4') then

      oldN = g%nh4
      call collect_real_array(variable_name, max_layer, '(kg/ha)', nh4, numvals, 0.0, 1000.0)

      do layer = 1, numvals
         g%nh4(layer) = nh4(layer)
         call bound_check_real_var (g%nh4(layer), g%nh4_min(layer), g%nh4(layer), 'g%NH4(layer)')
      end do
      dltN = sum(g%nh4) - sum(oldN)
      call soilN2_ExternalMassFlow (dltN)

   elseif (variable_name .eq. 'nh4ppm') then

      oldN = g%nh4
      call fill_real_array (nh4, 0.0, max_layer)

      call collect_real_array(variable_name, max_layer, '(ppm)', nh4, numvals, c%nh4ppm_min, 1000.0)

      do layer=1,numvals
         g%nh4(layer) = divide (nh4(layer), soiln2_fac (layer), 0.0)
      end do
      dltN = sum(g%nh4) - sum(oldN)
      call soilN2_ExternalMassFlow (dltN)

   elseif (variable_name .eq. 'urea') then

      oldN = g%urea
      call collect_real_array(variable_name, max_layer, '(kg/ha)', g%urea, numvals, g_urea_min, 1000.0)

      dltN = sum(g%urea) - sum(oldN)
      call soilN2_ExternalMassFlow (dltN)

   elseif (variable_name .eq. 'dlt_no3') then

      call collect_real_array(variable_name, max_layer, '(kg/ha)', tarray, numvals,-1000.0, 1000.0)

      do layer = 1, numvals
         g%no3(layer) = g%no3(layer) + tarray(layer)
         call bound_check_real_var (g%no3(layer), g%no3_min(layer), g%no3(layer), 'g%NO3(layer)')
      end do

   elseif (variable_name .eq. 'dlt_nh4') then

      call collect_real_array(variable_name, max_layer, '(kg/ha)', tarray, numvals,-1000.0, 1000.0)

      do layer = 1, numvals
         g%nh4(layer) = g%nh4(layer) + tarray(layer)
         call bound_check_real_var (g%nh4(layer), g%nh4_min(layer), g%nh4(layer), 'g%NH4(layer)')
      end do

   elseif (variable_name .eq. 'dlt_no3ppm') then

      call collect_real_array(variable_name, max_layer, '(ppm)', tarray, numvals,-1000.0, 1000.0)

      do layer = 1, numvals
         g%no3(layer) = g%no3(layer) + divide (tarray(layer), soiln2_fac (layer), 0.0)
         call bound_check_real_var (g%no3(layer), g%no3_min(layer), g%no3(layer), 'g%NO3(layer)')
      end do

   elseif (variable_name .eq. 'dlt_nh4ppm') then

      call collect_real_array(variable_name, max_layer, '(ppm)', tarray, numvals,-1000.0, 1000.0)

      do layer = 1, numvals
         g%nh4(layer) = g%nh4(layer)+ divide (tarray(layer), soiln2_fac (layer), 0.0)
         call bound_check_real_var (g%nh4(layer), g%nh4_min(layer), g%nh4(layer), 'g%NH4(layer)')
      end do

   elseif (variable_name .eq. 'dlt_urea') then

      call collect_real_array(variable_name, max_layer, '(kg/ha)', tarray, numvals,-1000.0, 1000.0)

      do layer = 1, numvals
         g%urea(layer) = g%urea(layer) + tarray(layer)
         call bound_check_real_var (g%urea(layer), g_urea_min, g%urea(layer), 'g%urea(layer)')
      end do

   elseif (variable_name .eq. 'n_reduction') then

      call collect_char_var (variable_name, '()', string, numvals)
      if (string(1:2) .eq. 'on') then
         g%p_n_reduction = on
      else
         g%p_n_reduction = off
      endif

   elseif ((variable_name .eq. 'dlt_org_n').or.(variable_name .eq. 'org_n')) then
   ! NIH - This works because there is no logical difference
   ! between the pool and it delta

      call collect_real_array(variable_name, max_layer, '(kg/ha)', tarray, numvals, 0.0, 1000.0)

      do layer = 1, numvals
         g%fom_n(layer) = g%fom_n(layer) + tarray(layer)
      end do


   elseif ((index(variable_name, 'dlt_org_c_pool') .eq. 1).or.(index(variable_name, 'org_c_pool') .eq. 1)) then

   ! NIH - This works because there is no logical difference
   ! between the pool and it delta

      call string_to_integer_var(variable_name (index(variable_name,'pool')+4:),pool_num, numvals)

      if (numvals .eq. 1) then

         call collect_real_array(variable_name, max_layer, '(kg/ha)', tarray, numvals, 0.0, 1000.0)

         do layer = 1, numvals
            g%fom_c_pool(pool_num,layer) =g%fom_c_pool(pool_num,layer) + tarray(layer)
         end do

      else
         !couldn't read pool number
         write (error_string,'(2a)')'cannot read pool number from ', variable_name

         call fatal_error (Err_User,error_string)
      endif

   else
         ! Don't know this variable name
      call message_unused ()
   endif

   call pop_routine (my_name)
   return
end subroutine



!     ===========================================================
subroutine soiln2_read_constants ()
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Purpose
!      Read in all parameters from parameter file.

!+  Mission Statement
!     Read Constants from Ini file

!+  Constant Values
   character*(*) my_name
   parameter (my_name = 'soiln2_read_constants')

!+  Local Variables
   integer    numvals               ! number of values read from file
   character  string*80
   character section_name*32

!- Implementation Section ----------------------------------
   call push_routine (my_name)

   call write_string (new_line//'   - Reading Constants')

   section_name = g%soiltype

   call read_real_var_optional (section_name, 'mcn', '()', c%mcn, numvals, 1.0, 50.0)
   if (numvals.le.0) then
      call write_string ('Using standard soil mineralisation for soil type '//g%soiltype)
      section_name = 'standard'
      call read_real_var(section_name, 'mcn', '()', c%mcn, numvals, 1.0, 50.0)
   else
      if (g%soiltype.ne.'standard') then
         call write_string ('Using soil mineralisation specification for '//g%soiltype)
      endif
   endif

   call read_real_var (section_name, 'ef_fom', '()', c%ef_fom, numvals, 0.0, 1.0)

   call read_real_var (section_name, 'fr_fom_biom', '()', c%fr_fom_biom, numvals, 0.0, 1.0)

   call read_real_var (section_name, 'ef_biom', '()', c%ef_biom, numvals, 0.0, 1.0)

   call read_real_var (section_name, 'fr_biom_biom', '()', c%fr_biom_biom, numvals, 0.0, 1.0)

   call read_real_var (section_name, 'ef_hum', '()', c%ef_hum, numvals, 0.0, 1.0)

   call read_real_array (section_name, 'rd_biom', 2, '()', c%rd_biom(:), numvals, 0.0, 1.0)

   call read_real_array (section_name, 'rd_hum', 2, '()', c%rd_hum(:), numvals, 0.0, 1.0)

   call read_real_var (section_name, 'ef_res', '()', c%ef_res, numvals, 0.0, 1.0)

   call read_real_var (section_name, 'fr_res_biom', '()', c%fr_res_biom, numvals, 0.0, 1.0)

   call read_real_array (section_name, 'rd_carb', 2, '()', c%rd_fom(1,:), numvals, 0.0, 1.0)

   call read_real_array (section_name, 'rd_cell', 2, '()', c%rd_fom(2,:), numvals, 0.0, 1.0)

   call read_real_array (section_name, 'rd_lign', 2, '()', c%rd_fom(3,:), numvals, 0.0, 1.0)

   call read_char_array (section_name, 'fom_type', max_fom_type, '()', g%fom_types, g%num_fom_types)

   call read_real_array (section_name, 'fr_carb', max_fom_type, '()', c%fr_fom(1,:), numvals, 0.0, 1.0)

   If (numvals.ne.g%num_fom_types) then
         ! We have an mismatch of type and data

      string = 'Number of "fr_carb" different to "fom_type"'
      call FATAL_ERROR (ERR_user, string)
   else
   endif

   call read_real_array (section_name, 'fr_cell', max_fom_type, '()', c%fr_fom(2,:), numvals, 0.0, 1.0)

   If (numvals.ne.g%num_fom_types) then
         ! We have an mismatch of type and data

      string = 'Number of "fr_cell" different to "fom_type"'
      call FATAL_ERROR (ERR_user, string)
   else
   endif

   call read_real_array (section_name, 'fr_lign', max_fom_type, '()', c%fr_fom(3,:), numvals, 0.0, 1.0)

   If (numvals.ne.g%num_fom_types) then
         ! We have an mismatch of type and data

      string = 'Number of "fr_lign" different to "fom_type"'
      call FATAL_ERROR (ERR_user, string)
   else
   endif

   call read_real_var (section_name, 'oc2om_factor', '()', c%oc2om_factor, numvals, 0.0, 3.0)

   call read_real_var (section_name, 'fom_min', '()', c%fom_min, numvals, 0.0, 1.0)

   call read_real_var (section_name, 'no3ppm_min', '()', c%no3ppm_min, numvals, 0.0, 1.0)

   call read_real_var (section_name, 'nh4ppm_min', '()', c%nh4ppm_min, numvals, 0.0, 1.0)

   call read_real_var (section_name, 'min_depth', '(mm)', c%min_depth, numvals, 0.0, 1000.0)

   call read_real_var (section_name, 'cnrf_coeff', '(mm)', c%cnrf_coeff, numvals, 0.0, 10.0)

   call read_real_var (section_name, 'cnrf_optcn', '(mm)', c%cnrf_optcn, numvals, 5.0, 100.0)

   call read_real_array (section_name, 'opt_temp', 2, '(mm)', c%opt_temp(:), numvals, 5.0, 100.0)

   call read_real_array (section_name, 'wfmin_index', max_wf_values, '()', c%wf_min_index, numvals, 0.0, 2.0)

   call read_real_array (section_name, 'wfmin_values', max_wf_values, '(0-1)', c%wf_min_values, numvals, 0.0, 1.0)

   call read_real_array (section_name, 'wfnit_index', max_wf_values, '()', c%wf_nit_index, numvals, 0.0, 2.0)

   call read_real_array (section_name, 'wfnit_values', max_wf_values, '(0-1)', c%wf_nit_values, numvals, 0.0, 1.0)

   call read_real_var (section_name, 'nitrification_pot', '(ppm)', c%nitrification_pot, numvals, 0.0, 100.0)

   call read_real_var (section_name, 'nh4_at_half_pot', '(ppm)', c%nh4_at_half_pot, numvals, 0.0, 200.0)


   call read_real_array (section_name, 'phf_nit_ph', max_phf_values, '()', c%phf_nit_ph, numvals, 0.0, 14.)

   call read_real_array (section_name, 'pHf_nit_values', max_phf_values, '(0-1)', c%phf_nit_values, numvals, 0.0, 1.0)

   call read_real_var (section_name, 'dnit_rate_coeff', '(kg/mg)', c%dnit_rate_coeff, numvals, 0.0, 1.0)

   call read_real_var (section_name, 'dnit_wf_power', '()', c%dnit_wf_power, numvals, 0.0, 5.0)


   call pop_routine (my_name)
   return
end subroutine



!     ===========================================================
subroutine soiln2_init_calc ()
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Purpose
!       Initialize soil nitrogen factors and variables

!+  Mission Statement
!     Initialize soil nitrogen factors and variables

!+  Calls

!+  Constant Values
   character  my_name*(*)
   parameter (my_name = 'soiln2_init_calc')
!
   real       ppm                   ! factor to convert parts to parts per
   parameter (ppm = 1000000.0)      !    million

!+  Local Variables
   real       cum_depth             ! cumulative depth of profile (mm)
   real       deepest_layer         ! deepest layer number to which initial root_wt is distributed
   real       previous_cum_depth    ! previous value of cum_depth as layer loop is stepped through (mm)
   real       factor                ! factor representing the proportion of layer into which the roots have delved
   real       fom                   ! fresh organic matter in layer (kg/ha)
   integer    layer                 ! layer number in loop ()
   integer    i                     ! counter
   integer    num_layers            ! number of layers used ()
   real       oc_ppm                ! soil organic carbon (ppm)
   real       root_distrib(max_layer) ! root distribution weighting over
                                    ! profile (0-1)
   real       ave_temp              ! averave temperature (oC)
   real       carbon_tot            ! total soil carbon in layer (kg/ha)
   real       root_distrib_tot      ! total root distribution weighting ()

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   ave_temp = (g%maxt + g%mint) *0.5
   call fill_real_array (g%surf_temp, ave_temp, days_in_year_max)
   call soiln2_soil_temp (g%soil_temp)

   call fill_real_array (root_distrib, 0.0, max_layer)

   num_layers = count_of_real_vals (g%dlayer, max_layer)
   deepest_layer = get_cumulative_index_real (g%root_depth, g%dlayer, max_layer)

   cum_depth = 0.0
   previous_cum_depth = 0.0

   do layer = 1,deepest_layer
      cum_depth = cum_depth + g%dlayer(layer)
      factor=min(1.0,divide((g%root_depth - previous_cum_depth),g%dlayer(layer),0.0))
      root_distrib(layer) = exp (-3.0*min(1.0,divide(cum_depth,g%root_depth,0.0)))*factor
      previous_cum_depth = cum_depth
   end do

   root_distrib_tot = sum_real_array (root_distrib, num_layers)


   do layer = 1,num_layers

      g%nh4_min(layer) = divide (c%nh4ppm_min, soiln2_fac (layer), 0.0)
      g%no3_min(layer) = divide (c%no3ppm_min, soiln2_fac (layer), 0.0)


      if (g%no3(layer) .lt. g%no3_min(layer) ) then
         call warning_error (err_user,'Attempt to initialise NO3 below lower limit')
         g%no3(layer) = g%no3_min(layer)
      else
      endif

      if (g%nh4(layer) .lt. g%nh4_min(layer) ) then
         call warning_error (err_user,'Attempt to initialise NH4 below lower limit')
         g%nh4(layer) = g%nh4_min(layer)
      else
      endif

          ! calculate total soil C
      oc_ppm = g%oc(layer)*pcnt2fract * ppm
      carbon_tot = divide (oc_ppm, soiln2_fac (layer), 0.0)
          ! calculate inert soil C
      g%inert_c(layer) = g%fr_inert_c(layer) * carbon_tot

          ! g%fr_biom_C is ratio of biomass-c to humic-c that is subject
          ! to mineralization

      g%biom_c(layer) = divide((carbon_tot - g%inert_c(layer)) * g%fr_biom_c(layer), (1.0 + g%fr_biom_c(layer)), 0.0)
      g%biom_n(layer) = divide (g%biom_c(layer), c%mcn, 0.0)

      g%hum_c(layer) = carbon_tot - g%biom_c(layer)
      g%hum_n(layer) = divide (g%hum_c(layer), g%soil_cn, 0.0)

      fom = divide (g%root_wt * root_distrib(layer), root_distrib_tot, 0.0)

      g%fom_c_pool(1,layer) = fom*c%fr_fom(1,1)*C_in_fom
      g%fom_c_pool(2,layer) = fom*c%fr_fom(2,1)*C_in_fom
      g%fom_c_pool(3,layer) = fom*c%fr_fom(3,1)*C_in_fom

      !Calculate the N in each pool in each layer, g%fom_n_pool(nfract,layer)
      g%fom_n_pool(1,layer)=divide(g%fom_c_pool(1,layer),g%root_cn_pool(1), 0.0)
      g%fom_n_pool(2,layer)=divide(g%fom_c_pool(2,layer),g%root_cn_pool(2), 0.0)
      g%fom_n_pool(3,layer)=divide(g%fom_c_pool(3,layer),g%root_cn_pool(3), 0.0)

      !dsg    compile g%fom_n(layer)
      do i=1,3
         g%fom_n(layer) = g%fom_n(layer) + g%fom_n_pool(i,layer)
      end do

      g%no3_yesterday(layer) = g%no3(layer)
      g%nh4_yesterday(layer) = g%nh4(layer)

   end do

   call pop_routine (my_name)
   return
end subroutine



!     ===========================================================
subroutine soiln2_sum_report ()
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Purpose
!     Report SoilN Module Status

!+  Mission Statement
!     Report SoilN Module Status

!+  Constant Values
   character*(*) my_name            ! name of current procedure
   parameter (my_name = 'soiln2_sum_report')

!+  Local Variables
   integer    layer                 ! layer number
   integer    num_layers            ! number of soil profile layers
   character*80 string              ! output string
   real       total_fom_c           ! total C in all fpools

!- Implementation Section ----------------------------------
   call push_routine (my_name)

   call write_string (new_line//new_line)

   if (g%use_external_st) then
      string = '      Soil temperature supplied externally'
      call write_string (string)
   endif

   if (g%use_external_tav_amp) then
      string = '      TAV and AMP supplied externally'
      call write_string (string)
   endif

   if (g%use_external_ph) then
      string = '      Soil pH supplied externally'
      call write_string (string)
   endif

   call write_string (new_line//new_line)

   string = '                 Soil Profile Properties'
   call write_string (string)

   string = '     ------------------------------------------------'
   call write_string (string)

   string = '      Layer    pH    OC     NO3     NH4    Urea'
   call write_string (string)

   string = '                    (%) (kg/ha) (kg/ha) (kg/ha)'
   call write_string (string)

   string = '     ------------------------------------------------'
   call write_string (string)

   num_layers = count_of_real_vals(g%dlayer, max_layer)

   do layer = 1,num_layers
      write (string, '(5x, i4, 5x, f4.2, 2x, f4.2, 3(2x, f6.2))')layer, g%ph(layer), g%oc(layer), g%no3(layer), g%nh4(layer), g%urea(layer)
      call write_string (string)
   end do

   string = '     ------------------------------------------------'
   call write_string (string)

   write (string, '(6x, ''Totals'', 12x, 3(2x, f6.2))')sum_real_array (g%no3, max_layer), sum_real_array (g%nh4, max_layer), sum_real_array (g%urea, max_layer)
   call write_string (string)

   string = '     ------------------------------------------------'
   call write_string (string)

   call write_string (new_line//new_line)
   call write_string (new_line//new_line)

   string = '             Initial Soil Organic Matter Status'
   call write_string (string)

   string ='     ---------------------------------------------------------'
   call write_string (string)

   string ='      Layer      Hum-C   Hum-N  Biom-C  Biom-N   FOM-C   FOM-N'
   call write_string (string)

   string ='               (kg/ha) (kg/ha) (kg/ha) (kg/ha) (kg/ha) (kg/ha)'
   call write_string (string)

   string ='     ---------------------------------------------------------'
   call write_string (string)

   num_layers = count_of_real_vals(g%dlayer,max_layer)
   do layer = 1,num_layers
      write (string, '(5x, i4, 3x, f10.1, 5f8.1)')layer, g%hum_c(layer), g%hum_n(layer), g%biom_c(layer), g%biom_n(layer), sum_real_array (g%fom_c_pool(1,layer), nfract), g%fom_n(layer)
      call write_string (string)
   end do

   string ='     ---------------------------------------------------------'
   call write_string (string)

   total_fom_c = 0.0
   do layer = 1,num_layers
      total_fom_c = total_fom_c+ sum_real_array (g%fom_c_pool(1,layer), nfract)
   end do

   write (string, '(6x, ''Totals'', f10.1, 5f8.1)')sum_real_array (g%hum_c, max_layer), sum_real_array (g%hum_n, max_layer), sum_real_array (g%biom_c, max_layer), sum_real_array (g%biom_n, max_layer), total_fom_c, sum_real_array (g%fom_n, max_layer)
   call write_string (string)

   string ='     ---------------------------------------------------------'
   call write_string (string)

   call pop_routine (my_name)
   return
end subroutine



!     ===========================================================
subroutine soiln2_OnIncorpFOMPool(variant)
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Purpose
!       Add roots into fom pools

!+  Mission Statement
!     Add roots into fom pools

!+  Sub-Program Arguments
   integer, intent(in out) :: variant

!+  Constant Values
   character  my_name*(*)           ! name of subroutine
   parameter (my_name = 'soiln2_OnIncorpFOMPool')

!+  Local Variables
   character*200   message          !
   integer    layer                 ! layer number in loop ()
   type (FOMPoolType)::FPoolProfileLayer


!- Implementation Section ----------------------------------

   call push_routine (my_name)

   call unpack_FOMPool(variant,FPoolProfileLayer)


   !NOW INCREMENT THE POOLS with the unpacked deltas

   do layer = 1, FPoolProfileLayer%num_layer

      g%fom_c_pool(1,layer) = g%fom_c_pool(1,layer)+ FPoolProfileLayer%layer(layer)%pool(1)%C
      g%fom_c_pool(2,layer) = g%fom_c_pool(2,layer)+ FPoolProfileLayer%layer(layer)%pool(2)%C
      g%fom_c_pool(3,layer) = g%fom_c_pool(3,layer)+ FPoolProfileLayer%layer(layer)%pool(3)%C


      g%fom_n_pool(1,layer) = g%fom_n_pool(1,layer)+ FPoolProfileLayer%layer(layer)%pool(1)%N
      g%fom_n_pool(2,layer) = g%fom_n_pool(2,layer)+ FPoolProfileLayer%layer(layer)%pool(2)%N
      g%fom_n_pool(3,layer) = g%fom_n_pool(3,layer)+ FPoolProfileLayer%layer(layer)%pool(3)%N



      !dsg    add up fom_n in each layer by adding up each of the pools
      g%fom_n(layer) = g%fom_n_pool(1,layer)+ g%fom_n_pool(2,layer)+ g%fom_n_pool(3,layer)

      g%no3(layer) = g%no3(layer) + FPoolProfileLayer%layer(layer)%no3
      g%nh4(layer) = g%nh4(layer) + FPoolProfileLayer%layer(layer)%nh4

  end do

  call pop_routine (my_name)
  return
end subroutine

!     ===========================================================
subroutine OnIncorpFOM (variant)
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Purpose
!       Add roots into fom pools

   integer, intent(in out) :: variant
   integer    layer                 ! layer number in loop ()
   type(FOMLayerType) IncorpFOM
   logical    NSpecified            ! was any N specified in the FOM?

!- Implementation Section ----------------------------------

   !     We partition the C and N into fractions in each layer.
   !     We will do this by assuming that the CN ratios
   !     of all fractions are equal
   call unpack_FOMLayer(variant, IncorpFOM)

   ! If the caller specified CNR values then use them to calculate N from Amount.
   do layer = 1, IncorpFOM%num_layer
      if (IncorpFOM%layer(layer)%CNR .gt. 0) then
         IncorpFOM%Layer(layer)%FOM%N = divide (IncorpFOM%Layer(layer)%FOM%Amount* C_in_fom, IncorpFOM%Layer(Layer)%CNR, 0.0)
      endif
   end do

   ! Was any N specified?
   NSpecified = .false.
   do layer = 1, IncorpFOM%num_layer
      NSpecified = (NSpecified .or. IncorpFOM%Layer(layer)%FOM%N .gt. 0)
   end do

   if (NSpecified) then
      g%fom_type = Find_string_in_array (IncorpFOM%Type, g%fom_types, g%num_fom_types)

      if (g%fom_type.le.0) then
         ! fom type not found - use default
         g%fom_type = 1
      endif

      ! Now convert the IncorpFOM%DeltaWt and the IncorpFOM%DeltaN arrays to
      ! include fraction information and add to pools.
      do layer = 1, IncorpFOM%num_layer
         g%fom_c_pool(1,layer) = g%fom_c_pool(1,layer)+ IncorpFOM%Layer(layer)%FOM%Amount * c%fr_fom(1,g%fom_type) * C_in_fom
         g%fom_c_pool(2,layer) = g%fom_c_pool(2,layer)+ IncorpFOM%Layer(layer)%FOM%Amount * c%fr_fom(2,g%fom_type) * C_in_fom
         g%fom_c_pool(3,layer) = g%fom_c_pool(3,layer)+ IncorpFOM%Layer(layer)%FOM%Amount * c%fr_fom(3,g%fom_type) * C_in_fom

         g%fom_n_pool(1,layer) = g%fom_n_pool(1,layer)+ IncorpFOM%Layer(layer)%FOM%N * c%fr_fom(1,g%fom_type)
         g%fom_n_pool(2,layer) = g%fom_n_pool(2,layer)+ IncorpFOM%Layer(layer)%FOM%N * c%fr_fom(2,g%fom_type)
         g%fom_n_pool(3,layer) = g%fom_n_pool(3,layer)+ IncorpFOM%Layer(layer)%FOM%N * c%fr_fom(3,g%fom_type)

         !dsg    add up fom_n in each layer by adding up each of the pools
         g%fom_n(layer) = g%fom_n_pool(1,layer)+ g%fom_n_pool(2,layer)+ g%fom_n_pool(3,layer)
      end do
   endif

   return
end subroutine


!     ===========================================================
real function soiln2_fac (layer)
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Sub-Program Arguments
   integer    layer                 ! (INPUT) soil layer counter

!+  Purpose
!      Convert kg/ha to ppm of soil in a given layer

!+  Mission Statement
!     Conversion factor (kg/ha to ppm)

!+  Constant Values
   character  my_name*(*)           ! name of subroutine
   parameter (my_name = 'soiln2_fac')

!- Implementation Section ----------------------------------

   call push_routine (my_name)

      ! calculate conversion factor from kg/ha to ppm (mg/kg)

   soiln2_fac = divide (100.0, g%bd(layer)*g%dlayer(layer), 0.0)

   call pop_routine (my_name)
   return
end function



!     ===========================================================
subroutine soiln2_soil_temp (soil_temp)
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Sub-Program Arguments
   real       soil_temp(*)          ! (OUTPUT) temperature of each layer
                                    !    in profile

!+  Purpose
!           Calculates average soil temperature at the centre of each layer
!           based on the soil temperature model of EPIC (Williams et al 1984)

!+  Mission Statement
!     Calculate soil temperature

!+  Calls

!+  Constant Values
   character  my_name*(*)           ! subroutine name
   parameter (my_name = 'soiln2_Soil_Temp')
!
   real days_in_year                ! no of days in one year
   parameter (days_in_year = 365.25)
!
   real       nth_solst             ! day of year of nthrn summer solstice
   parameter (nth_solst = 173.0)
!
   real       temp_delay            ! delay from solstice to warmest day
   parameter (temp_delay = 27.0)    !   (days)
!
   real       pi                    ! pi
   parameter (pi = 3.14159)
!
   real       nth_hot               ! warmest day of year of nth hemisphere
   parameter (nth_hot = nth_solst + temp_delay)
!
   real       sth_solst             ! day of year of sthrn summer solstice
   parameter (sth_solst = nth_solst + days_in_year/2.0)
!
   real       sth_hot               ! warmest day of year of sth hemisphere
   parameter (sth_hot = sth_solst + temp_delay )
!
   real       ang                   ! length of one day in radians
   parameter (ang = (2.0*pi) /days_in_year) ! factor to convert day of
                                            ! year to radian fraction of year

!+  Local Variables
   real       alx                   ! time in radians of year from hottest
                                    !  instance to current day of year as a
                                    !  radian fraction of one year for soil
                                    !  temperature calculations
   real       cum_depth             ! cumulative depth in profile
   real       damp                  ! temperature damping depth
                                    !   (mm depth/radians time)
   real       dlt_temp              ! change in soil temperature
   real       depth_lag             ! temperature lag factor in radians
                                    !    for depth
   integer    layer                 ! layer counter
   integer    num_layers            ! number of layers
   integer    numvals               ! number of values returned

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   if (g%use_external_st) then
      ! another module is supplying soil temperature
      g%soil_temp = 0.0

      call get_real_array (unknown_module, 'ave_soil_temp', max_layer, '(oC)', g%soil_temp, numvals, -20.0, 80.0)


   else
      ! Get a factor to calculate "normal" soil temperature from the
      ! day of g%year assumed to have the warmest average soil temperature
      ! of the g%year.  The normal soil temperature varies as a cosine
      ! function of alx.  This is the number of radians (time) of a
      ! g%year today is from the warmest soil temp.


      ! check for nth/sth hemisphere
      if (g%latitude.ge.0) then
         alx = ang* (offset_day_of_year (g%year, g%day_of_year, int(-nth_hot)))

      else
         alx = ang* (offset_day_of_year (g%year, g%day_of_year, int(-sth_hot)))
      endif

      call bound_check_real_var (alx, 0.0, 6.31, 'alx')

        ! get change in soil temperature since hottest day. deg c.

      dlt_temp = soiln2_soiltemp_dt(alx)

       ! get temperature damping depth. (mm per radian of a g%year)

      damp = soiln2_soiltemp_dampdepth ()

      cum_depth = 0.0

       ! Now get the average soil temperature for each layer.
       ! The difference in temperature between surface and subsurface
       ! layers ( exp(zd)) is an exponential function of the ratio of
       ! the depth to the bottom of the layer and the temperature
       ! damping depth of the soil.

      call fill_real_array (soil_temp, 0.0, max_layer)
      num_layers = count_of_real_vals (g%dlayer, max_layer)

      do layer = 1,num_layers

          ! get the cumulative depth to bottom of current layer

          cum_depth = cum_depth + g%dlayer(layer)

          ! get the lag factor for depth. This reduces changes in
          ! soil temperature with depth. (radians of a g%year)

          depth_lag = divide (cum_depth, damp, 0.0)

          ! allow subsurface temperature changes to lag behind
          ! surface temperature changes

          soil_temp(layer) = soiln2_layer_temp(depth_lag, alx, dlt_temp)

          call bound_check_real_var (soil_temp(layer),-20.0, 80.0, 'soil_temp')

     end do

   endif

   call pop_routine (my_name)
   return
end subroutine



!     ===========================================================
real function soiln2_layer_temp (depth_lag, alx, dlt_temp)
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Sub-Program Arguments
   real       alx                   ! (INPUT) time in radians of a g%year
                                    ! from hottest instance
   real       dlt_temp              ! (INPUT) change in surface soil
                                    ! temperature since hottest day (deg c)
   real       depth_lag             ! (INPUT) lag factor for depth
                                    ! (radians)

!+  Purpose
!       Get subsoil temperature.

!+  Mission Statement
!     Soil temperature using %1, %2, %3

!+  Constant Values
   character  my_name*(*)           ! subroutine name
   parameter (my_name = 'soiln2_Layer_Temp')

!- Implementation Section ----------------------------------

   call push_routine (my_name)

      ! Now get the average soil temperature for the layer.
      ! The difference in temperature between surface and subsurface
      ! layers ( exp(-depth_lag)) is an exponential function of the ratio of
      ! the depth to the bottom of the layer and the temperature
      ! damping depth of the soil.

   soiln2_layer_temp = g%ave_temp+ (g%amp/2.0*cos (alx - depth_lag) + dlt_temp)* exp (-depth_lag)

   call pop_routine (my_name)
   return
end function



!     ===========================================================
real function soiln2_soiltemp_dt (alx)
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Sub-Program Arguments
   real       alx                   ! (INPUT) time of g%year in radians
                                    !     from warmest instance

!+  Purpose
!           Calculates  the rate of change in soil surface temperature
!           with time.
!           jngh 24-12-91.  I think this is actually a correction to adjust
!           today's normal sinusoidal soil surface temperature to the
!           current temperature conditions.

!+  Mission Statement
!     Rate of change in soil surface temperature with time

!+  Calls


!+  Constant Values
   character  my_name*(*)           ! subroutine name
   parameter (my_name = 'soiln2_SoilTemp_dt')
!
   integer    ndays                 ! number of days for moving average
   parameter (ndays = 5)

!+  Local Variables
   real       ave_temp_0            ! moving average surface temperature
                                    !    of ndays (oC)
   integer    day                   ! loop counter
   integer    day_of_year           ! day of g%year for moving average
   real       temp_0(10)            ! store of last few days surface
                                    !    temperatures (oC)
   real       temp_a                ! today's surface layer temperature
                                    ! (oC)
   real       ave_temp              ! mean ambient air temperature (oC)
   integer    yesterday             ! yesterday's day of g%year number

!- Implementation Section ----------------------------------

   call push_routine (my_name)

      ! Get today's top layer temp from yesterdays temp and today's
      ! weather conditions.
      ! The actual soil surface temperature is affected by current
      ! weather conditions.

   yesterday = offset_day_of_year (g%year, g%day_of_year, -1)
   ave_temp = (g%maxt + g%mint) * 0.5

   !jh need to name these constants
   g%surf_temp(g%day_of_year) = (1.0 - g%salb)* (ave_temp + (g%maxt - ave_temp)*sqrt (g%radn*23.8846/800.0))+ g%salb * g%surf_temp(yesterday)

   ! get last few days soil surface temperature for moving average

   do day = 1, ndays
      day_of_year = offset_day_of_year (g%year, g%day_of_year, 1 - day)
      temp_0(day) = g%surf_temp(day_of_year)
   end do

   ave_temp_0 = divide (sum_real_array (temp_0, ndays), real(ndays), 0.0)

   ! Get today's normal surface soil temperature
   ! There is no depth lag, being the surface, and there
   ! is no adjustment for the current temperature conditions
   ! as we want the "normal" sinusoidal temperature for this
   ! time of g%year.

   temp_a = soiln2_layer_temp (0.0, alx, 0.0)

   ! Get the rate of change in soil surface temperature with time.
   ! This is the difference between a five-day moving average and
   ! today's normal surface soil temperature.

   soiln2_soiltemp_dt = ave_temp_0 - temp_a

   ! check output

   call bound_check_real_var(soiln2_soiltemp_dt, -100.0, 100.0, 'soiln2_SoilTemp_dt')

   call pop_routine (my_name)
   return
end function



!     ===========================================================
real function soiln2_soiltemp_dampdepth ()
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Purpose
!           Now get the temperature damping depth. This is a function of the
!             average bulk density of the soil and the amount of water above
!             the lower limit. I think the damping depth units are
!             mm depth/radian of a g_year

!+  Notes
!       241091 consulted Brian Wall.  For soil temperature an estimate of
!       the water content of the total profile is required, not the plant
!       extractable soil water.  Hence the method used here - difference
!       total lower limit and total soil water instead of sum of differences
!       constrained to and above.  Here the use of lower limit is of no
!       significance - it is merely a reference point, just as 0.0 could
!       have been used.  jngh

!+  Mission Statement
!     Temperature damping depth

!+  Constant Values
   real       sw_avail_tot_min      ! minimum available sw (mm water)
   parameter (sw_avail_tot_min = 0.01)
!
   character  my_name*(*)           ! name of subroutine
   parameter (my_name = 'soiln2_SoilTemp_DampDepth')

!+  Local Variables
   real       ave_bd                ! average bulk density over layers
                                    !    (g/cc soil)
   real       sw_avail_tot          ! amount of sw above lower limit
                                    !    (mm water)
   real       b                     ! intermediate variable
   real       cum_depth             ! cumulative depth in profile (mm)
   real       damp_depth_max        ! maximum damping depth (potential)
                                    !  (mm soil/radian of a g_year (58 days))
   real       f                     ! fraction of potential damping depth
                                    !   discounted by water content of
                                    !   soil (0-1)
   real       favbd                 ! a function of average bulk density
   real       wcf                   ! a function of water content (0-1)
   integer    num_layers            ! number of layers in profile
   real       bd_tot                ! total bulk density over profile
                                    !    (g/cc soil)
   real       ll_tot                ! total lower limit over profile
                                    !    (mm water)
   real       sw_dep_tot            ! total soil water over profile
                                    !    (mm water)
   real       wc                    ! water content of profile (0-1)
   real       ww                    ! potential sw above lower limit
                                    !    (mm water/mm soil)

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   num_layers = count_of_real_vals (g%dlayer, max_layer)

       ! get average bulk density

   bd_tot = sum_products_real_array (g%bd, g%dlayer, num_layers)
   cum_depth = sum_real_array (g%dlayer, num_layers)
   ave_bd = divide (bd_tot, cum_depth, 0.0)

       ! favbd ranges from almost 0 to almost 1
       ! damp_depth_max ranges from 1000 to almost 3500
       ! It seems damp_depth_max is the damping depth potential.

   favbd = divide (ave_bd, (ave_bd + 686.0*exp (-5.63*ave_bd)), 0.0)
   damp_depth_max = 1000.0 + 2500.0*favbd
   damp_depth_max = l_bound (damp_depth_max, 0.0)

       ! Potential sw above lower limit - mm water/mm soil depth
       ! note that this function says that average bulk density
       ! can't go above 2.47222, otherwise potential becomes negative.
       ! This function allows potential (ww) to go from 0 to .356

   ww = 0.356 - 0.144*ave_bd
   ww = l_bound (ww, 0.0)


       ! calculate amount of soil water, using lower limit as the
       ! reference point.

   ll_tot = sum_real_array (g%ll15_dep, num_layers)
   sw_dep_tot = sum_real_array (g%sw_dep, num_layers)
   sw_avail_tot = sw_dep_tot - ll_tot
   sw_avail_tot = l_bound (sw_avail_tot, sw_avail_tot_min)

       ! get fractional water content -

       ! wc can range from 0 to 1 while
       ! wcf ranges from 1 to 0

   wc = divide (sw_avail_tot, (ww*cum_depth), 1.0)
   wc = bound (wc, 0.0, 1.0)
   wcf = divide((1.0  - wc),(1.0 + wc),0.0)

       ! Here b can range from -.69314 to -1.94575
       ! and f ranges from 1 to  0.142878
       ! When wc is 0, wcf=1 and f=500/damp_depth_max
       ! and soiln2_SoilTemp_DampDepth=500
       ! When wc is 1, wcf=0 and f=1
       ! and soiln2_SoilTemp_DampDepth=damp_depth_max
       ! and that damp_depth_max is the maximum.


   b = alog (divide (500.0, damp_depth_max, 1.0e10))

   f = exp (b* wcf**2)

       ! Get the temperature damping depth. (mm soil/radian of a g_year)
       ! discount the potential damping depth by the soil water deficit.
       ! Here soiln2_SoilTemp_DampDepth ranges from 500 to almost
       ! 3500 mm/58 days.

   soiln2_soiltemp_dampdepth = f*damp_depth_max

   call pop_routine (my_name)
   return
end function



!     ===========================================================
subroutine soiln2_urea_hydrolysis (layer, dlt_urea_hydrol)
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Sub-Program Arguments
   integer    layer               ! (INPUT) soil layer
   real       dlt_urea_hydrol     ! (OUTPUT) g_urea hydrolysed (kg/ha)
!
!mep  urea is hydrolysed to ammonium_N

!+  Purpose
!       Hydrolyse g_urea.
!       NOTE - not tested as not in old code - from CM V2.

!+  Mission Statement
!     Hydrolyse urea in %1

!+  Calls


!+  Constant Values
   character  my_name*(*)           ! name of subroutine
   parameter (my_name = 'soiln2_Urea_hydrolysis')

!+  Local Variables
   integer    index                 ! index - 1 for aerobic and 2 for anaerobic conditions
   real       ak                    ! potential fraction of g_urea to be
                                    !    hydrolysed
   real       swf                   ! sw factor limitimg hydrolysis (0-1)
   real       tf                    ! soil temp factor limiting hydrolysis
                                    !    (0-1)

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   ! dsg 200508  use different values for some constants when anaerobic conditions dominate
   if (g%pond_active.eq.'no') then
       index = 1
   else if (g%pond_active.eq.'yes') then
       index = 2
   else
   endif


   if (g%urea(layer).gt.0.0) then

         ! do some g%urea hydrolysis.

      if (g%urea(layer).lt.0.1) then
         dlt_urea_hydrol = g%urea(layer)

      else

             ! get soil water factor

         swf = soiln2_wf (layer,index) + 0.20
         swf = bound (swf, 0.0, 1.0)

             ! get soil temperature factor

         tf = (g%soil_temp(layer)/40.0) + 0.20
         tf = bound (tf, 0.0, 1.0)

             ! get potential fraction of g%urea for hydrolysis
             ! note (jngh) g%oc & g%ph are not updated during simulation

             !mep    following equation would be better written in terms of hum_C and biom_C
             !mep    oc(layer) = (hum_C(layer) + biom_C(layer))*soiln2_fac (layer)*10000.

         ak = -1.12+ 1.31*g%oc(layer) + 0.203*g%ph(layer)- 0.155*g%oc(layer)*g%ph(layer)

         ak = bound (ak, 0.25, 1.0)

             ! get amount hydrolysed

         dlt_urea_hydrol = ak*g%urea(layer) * min(swf, tf)
         dlt_urea_hydrol = bound (dlt_urea_hydrol, 0.0, g%urea(layer))
      endif

   else
      dlt_urea_hydrol = 0.0
   endif

   call pop_routine (my_name)
   return
end subroutine


!     ===========================================================
subroutine soiln2_min_residues (dlt_C_decomp, dlt_N_decomp, dlt_c_biom, dlt_c_hum, dlt_c_atm, dlt_nh4_min, dlt_no3_min)
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Sub-Program Arguments
      real dlt_C_decomp(max_layer, max_residues)   ! C decomposed for each residue (Kg/ha)
      real dlt_N_decomp(max_layer, max_residues)   ! N decomposed for each residue (Kg/ha)
      real dlt_c_atm(max_layer, max_residues)      ! (OUTPUT) carbon to atmosphere (kg/ha)
      real dlt_c_biom(max_layer, max_residues)     ! (OUTPUT) carbon to biomass (kg/ha)
      real dlt_c_hum(max_layer, max_residues)      ! (OUTPUT) carbon to humic (kg/ha)
      real dlt_nh4_min(max_layer)      ! (OUTPUT) N to NH4 (kg/ha)
      real dlt_no3_min(max_layer)      ! (OUTPUT) N to NO3 (kg/ha)

!+  Purpose
!       Test to see whether adequate mineral nitrogen is available
!       to sustain potential rate of decomposition of surface residues
!       and calculate net rate of nitrogen mineralization/immobilization

!+  Mission Statement
!     Calculate rate of nitrogen mineralization/immobilization

!+  Constant Values
   character  my_name*(*)           ! name of subroutine
   parameter (my_name = 'soiln2_min_residues')

!+  Local Variables
   real       avail_no3(max_layer)  ! no3 available for mineralisation
   real       avail_nh4(max_layer)  ! nh4 available for mineralisation
   real       nit_tot               ! total N avaliable for immobilization
                                    !    (kg/ha)
   real       dlt_c_biom_tot(max_residues) ! C mineralized converted to biomass
   real       dlt_c_hum_tot(max_residues)  ! C mineralized converted to humic
   real       fraction(max_layer)   ! fraction of a layer
   integer    layer                 ! soil layer counter variable
   integer    min_layer             ! soil layer to which N is available
                                    ! for mineralisation.
   real       min_layer_top         ! depth of the top of min_layer
   real       n_demand              ! potential N immobilization
   real       n_avail               ! available N for immobilization
   integer    nlayrs                ! number of soil layers
   real       scale_of              ! factor to reduce mineralization
                                    ! rates if insufficient N available
   real       part_fraction         ! partitioning fraction
   real       dlt_n_min             ! net N mineralized (kg/ha)
   integer    residue               ! count of residue

!+  Initial Data Values
   call fill_real_array (avail_no3, 0.0, max_layer)
   call fill_real_array (avail_nh4, 0.0, max_layer)
   call fill_real_array (dlt_c_hum, 0.0, max_layer)
   call fill_real_array (dlt_c_biom, 0.0, max_layer)
   call fill_real_array (dlt_nh4_min, 0.0, max_layer)
   call fill_real_array (dlt_no3_min, 0.0, max_layer)

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   ! get total available mineral N in surface soil

   nlayrs = count_of_real_vals (g%dlayer, max_layer)
   min_layer = find_layer_no (c%min_depth, g%dlayer, nlayrs)

   if (min_layer .eq. 1) then
      ! I am not sure that sum real array handles this case - NIH
      min_layer_top = 0.0
   else
      min_layer_top = sum_real_array (g%dlayer, min_layer - 1)
   endif

   nit_tot = 0.0
   do layer = 1, min_layer

      avail_no3(layer) = (g%no3(layer) - g%no3_min(layer))
      avail_no3(layer) = l_bound (avail_no3(layer), 0.0)

      avail_nh4(layer) = (g%nh4(layer) - g%nh4_min(layer))
      avail_nh4(layer) = l_bound (avail_nh4(layer), 0.0)

      if (layer .eq. min_layer) then
         fraction(layer) = divide (c%min_depth - min_layer_top,g%dlayer(layer),0.0)
         fraction(layer) = bound (fraction(layer), 0.0, 1.0)
      else
         fraction(layer) = 1.0
      endif

      avail_no3(layer) = avail_no3(layer) * fraction(layer)
      avail_nh4(layer) = avail_nh4(layer) * fraction(layer)

      nit_tot = nit_tot + avail_no3(layer) + avail_nh4(layer)

   end do

       ! get potential decomposition rates of residue C and N
       !      from residue module


       ! jpd
       ! determine potential decomposition rates of residue C and N
       ! in relation to Avail N, for residue and manure surface material
       ! i.e. Do Scaling for avail N, combining residue&manure decomposition


       ! calculate potential transfers to biom and humic pools

   dlt_c_biom_tot(:) = g%pot_c_decomp(:) * c%ef_res * c%fr_res_biom
   dlt_c_hum_tot(:)  = g%pot_c_decomp(:) * c%ef_res* (1.0 - c%fr_res_biom)


       ! test whether adequate N available to meet immobilization demand

   n_demand = divide (sum (dlt_c_biom_tot(:)), c%mcn, 0.0)+ divide (sum (dlt_c_hum_tot(:)), g%soil_cn, 0.0)

   n_avail = nit_tot + sum (g%pot_n_decomp(:))

   if (n_demand.gt.n_avail) then
      scale_of = divide (nit_tot, (n_demand - sum (g%pot_n_decomp(:))), 0.0)
      scale_of = bound (scale_of, 0.0, 1.0)
   else
      scale_of = 1.0   ! supply exceeds demand
   endif

   ! Partition Additions of C and N to layers

   do layer = 1, min_layer
      part_fraction = divide(g%dlayer(layer)*fraction(layer),c%min_depth,0.0)
      do residue = 1, g%num_residues

         ! now adjust carbon transformations etc.
         dlt_c_decomp(layer, residue) = g%pot_c_decomp(residue)* scale_of * part_fraction
         dlt_n_decomp(layer, residue) = g%pot_n_decomp(residue)* scale_of * part_fraction

         dlt_c_hum(layer, residue) = dlt_c_hum_tot(residue)* scale_of * part_fraction
         dlt_c_biom(layer, residue) = dlt_c_biom_tot(residue)* scale_of * part_fraction
         dlt_c_atm(layer, residue) = dlt_c_decomp(layer, residue)- dlt_c_hum(layer, residue)- dlt_c_biom(layer, residue)

      end do
   end do

   dlt_NH4_min = 0.0
   dlt_n_min = sum (dlt_n_decomp(:,:)) - n_demand * scale_of

   if (dlt_n_min .gt. 0.0) then
         ! we have mineralisation into NH4
         ! distribute it over the layers
      do layer = 1, min_layer
         part_fraction = divide(g%dlayer(layer)*fraction(layer),c%min_depth,0.0)
         dlt_nh4_min(layer) = dlt_n_min * part_fraction
      end do

   else if (dlt_n_min .lt. 0.0) then
      ! Now soak up any N required for immobilisation from NH4 then NO3

      do layer = 1, min_layer
         dlt_nh4_min(layer) =  - min(avail_nh4(layer),abs(dlt_n_min))
         dlt_n_min = dlt_n_min - dlt_nh4_min(layer)
      end do

      do layer = 1, min_layer
         dlt_no3_min(layer) =  - min(avail_no3(layer),abs(dlt_n_min))
         dlt_n_min = dlt_n_min - dlt_no3_min(layer)
      end do
      ! There should now be no remaining immobilization demand
      call bound_check_real_var (dlt_n_min,-0.001, 0.001,'remaining imobilization')

   else
      ! no N transformation
   endif

   call pop_routine (my_name)
   return
end subroutine



!     ===========================================================
subroutine soiln2_process ()
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Purpose
!        This routine performs the soil N balance.
!
!        It calculates hydrolysis of g_urea, mineralisation of organic
!        matter and immobilization of mineral nitrogen due to crop
!        residue and soil organic matter decomposition, denitrification
!        and nitrification.

!+  Mission Statement
!     Perform all APSIM Timestep calculations

!+  Constant Values
   character  my_name*(*)           ! name of subroutine
   parameter (my_name = 'soiln2_process')

!+  Local Variables
      character  error_string *80      ! error message if inadequate N
                                    ! for immobilization
   integer    layer                 ! soil layer count
   integer    numvals
   integer    num_layers            ! number of soil layers used
   integer    fract                 ! number of fpools for fom
   real       dlt_rntrf             ! nitrogen moved by nitrification
                                    !    (kg/ha)
   real       dlt_urea_hydrol       ! nitrogen moved by hydrolysis (kg/ha)
   real       excess_nh4            ! excess N required above NH4 supply
   real       fom_c                 ! total fom carbon
   real       fom_cn                ! CN ratio of fom
   real       dlt_pond_c_hum        ! humic material from breakdown of residues in pond (if present)
   real       dlt_pond_c_biom       ! biom material from breakdown of residues in pond (if present)
   character  err_string*120

!- Implementation Section ----------------------------------

   call push_routine (my_name)


       ! update soil temperature

   call soiln2_soil_temp (g%soil_temp)

        ! get number of layers

   num_layers = count_of_real_vals (g%dlayer, max_layer)


   if (g%pond_active.eq.'no') then
        ! decompose surface residues

       ! dsg 010508 If there is no pond, then mineralise residues into top soil layer 
       !     as done previously.  If there is a pond, then we need to mineralise directly into the pond water, calculating the 
       !     immobilisation demand using mineral N in the pond also.  If 'pond_active' = 'yes' then this will be done in the 'pond' module.
       !     SoilN2 would get some of the N back from the Pond module via a combination of mass flow and adsorption.
       call soiln2_min_residues (g%dlt_res_C_decomp,g%dlt_res_N_decomp,g%dlt_res_c_biom,g%dlt_res_c_hum,g%dlt_res_c_atm,g%dlt_res_nh4_min,g%dlt_res_no3_min)


       g%hum_c = g%hum_c+ sum (g%dlt_res_c_hum(:,:), dim = residue_dim)

       g%biom_c = g%biom_c+ sum (g%dlt_res_c_biom(:,:), dim = residue_dim)

       do layer = 1, num_layers

          g%hum_n(layer) = divide (g%hum_c(layer), g%soil_cn, 0.0)
          g%biom_n(layer) = divide (g%biom_c(layer), c%mcn, 0.0)

          ! update soil mineral N

          g%nh4(layer) = g%nh4(layer) + g%dlt_res_nh4_min(layer)
          g%no3(layer) = g%no3(layer) + g%dlt_res_no3_min(layer)

       end do

   else
     !  dsg 190508,  there is a pond, so POND module will decompose residues - not SoilN2
     !  dsg 110708   Get the biom & hum C decomposed in the pond and add to soil - on advice of MEP
 
       call get_real_var (unknown_module, 'pond_biom_C', 'kg/ha', dlt_pond_c_biom, numvals, -1000.0, 1000.0)
       call get_real_var (unknown_module, 'pond_hum_C', 'kg/ha', dlt_pond_c_hum, numvals, -1000.0, 1000.0)

       ! increment the soiln2 hum and biom C pools in top soil layer        
       g%hum_c(1) = g%hum_c(1) + dlt_pond_c_hum
       g%biom_c(1) = g%biom_c(1) + dlt_pond_c_biom
      
       g%hum_n(1) = divide (g%hum_c(1), g%soil_cn, 0.0)
       g%biom_n(1) = divide (g%biom_c(1), c%mcn, 0.0)

   endif

      ! now take each layer in turn

   do layer=1,num_layers

        ! hydrolyse some g_urea

      call soiln2_urea_hydrolysis (layer, dlt_urea_hydrol)

      g%nh4(layer) = g%nh4(layer) + dlt_urea_hydrol
      g%urea(layer) = g%urea(layer) - dlt_urea_hydrol

         ! denitrification of nitrate-N

      call soiln2_denitrification (layer, g%dlt_no3_dnit(layer))

      g%no3(layer) = g%no3(layer) - g%dlt_no3_dnit(layer)

          ! Calculate transformations of soil organic matter and
          ! organic nitrogen.

          ! get action from humic pool

      call soiln2_min_humic (layer, g%dlt_hum_c_biom(layer), g%dlt_hum_c_atm(layer), g%dlt_hum_n_min(layer))

          ! get action from biom pool

      call soiln2_min_biomass (layer, g%dlt_biom_c_hum(layer), g%dlt_biom_c_atm(layer), g%dlt_biom_n_min(layer))

          ! get action from fom pool

      call soiln2_min_fom (layer, g%dlt_fom_c_biom(1,layer), g%dlt_fom_c_hum(1,layer), g%dlt_fom_c_atm(1,layer), g%dlt_fom_n(1,layer), g%dlt_fom_n_min(layer))

       ! update pools

      g%hum_c(layer) = g%hum_c(layer)- g%dlt_hum_c_biom(layer)- g%dlt_hum_c_atm(layer)+ g%dlt_biom_c_hum(layer)+ sum_real_array (g%dlt_fom_c_hum(1,layer), nfract)

      g%hum_n(layer) = divide (g%hum_c(layer), g%soil_cn, 0.0)

      g%biom_c(layer) = g%biom_c(layer)- g%dlt_biom_c_hum(layer)- g%dlt_biom_c_atm(layer)+ g%dlt_hum_c_biom(layer)+ sum_real_array (g%dlt_fom_c_biom(1,layer), nfract)

      g%biom_n(layer) = divide (g%biom_c(layer), c%mcn, 0.0)


      do fract= 1,nfract
        g%fom_c_pool(fract, layer) = g%fom_c_pool(fract, layer)- g%dlt_fom_c_hum(fract, layer)- g%dlt_fom_c_biom(fract, layer)- g%dlt_fom_c_atm(fract, layer)
        !dsg  also perform calculation for n
        g%fom_n_pool(fract, layer) = g%fom_n_pool(fract, layer)- g%dlt_fom_n(fract,layer)

      end do

       !dsg  these 3 dlts are calculated for the benefit of soilp which needs to 'get' them
       g%dlt_fom_c_pool1(layer) = g%dlt_fom_c_hum(1,layer)+g%dlt_fom_c_biom(1,layer)+g%dlt_fom_c_atm(1,layer)
       g%dlt_fom_c_pool2(layer) = g%dlt_fom_c_hum(2,layer)+g%dlt_fom_c_biom(2,layer)+g%dlt_fom_c_atm(2,layer)
       g%dlt_fom_c_pool3(layer) = g%dlt_fom_c_hum(3,layer)+g%dlt_fom_c_biom(3,layer)+g%dlt_fom_c_atm(3,layer)

       fom_c = g%fom_c_pool(1,layer)+ g%fom_c_pool(2,layer)+ g%fom_c_pool(3,layer)

       !dsg    add up fom_n in each layer in each of the pools
       g%fom_n(layer) = g%fom_n_pool(1,layer)+ g%fom_n_pool(2,layer)+ g%fom_n_pool(3,layer)


       ! update soil mineral N
       g%nh4(layer) = g%nh4(layer)+ g%dlt_hum_n_min(layer)+ g%dlt_biom_n_min(layer)+ g%dlt_fom_n_min(layer)

       ! now check if too much NH4 immobilized
       if (g%nh4(layer).lt.g%nh4_min(layer)) then
         excess_nh4 = g%nh4_min(layer) - g%nh4(layer)
         g%nh4(layer) = g%nh4_min(layer)
       else
         excess_nh4 = 0.0
       endif

       g%no3(layer) = g%no3(layer) - excess_nh4

       ! note soiln2_min_fom tests for adequate mineral N for
       ! immobilization so that g_NO3 should not go below g_NO3_min

       g%no3(layer) = l_bound (g%no3(layer), g%no3_min(layer))

       ! nitrification of some ammonium-N

       call soiln2_nitrification (layer, dlt_rntrf)

       g%no3(layer) = g%no3(layer) + dlt_rntrf
       g%nh4(layer) = g%nh4(layer) - dlt_rntrf

       call bound_check_real_var (g%no3(layer), g%no3_min(layer), 9000.0, 'NO3(layer)')
       call bound_check_real_var (g%nh4(layer), g%nh4_min(layer), 9000.0, 'NH4(layer)')
       call bound_check_real_var (g%urea(layer), 0.0, 9000.0, 'urea(layer)')

       g%NH4_transform_net(layer) = g%dlt_res_NH4_min(layer)+ g%dlt_fom_N_min(layer)+ g%dlt_biom_N_min(layer)+ g%dlt_hum_N_min(layer)- dlt_rntrf+ dlt_urea_hydrol+ excess_nh4

       g%NO3_transform_net(layer) = g%dlt_res_NO3_min(layer)- g%dlt_NO3_dnit(layer)+ dlt_rntrf- excess_NH4

       g%dlt_rntrf(layer)         = dlt_rntrf
       g%dlt_urea_hydrol(layer)   = dlt_urea_hydrol
       g%excess_nh4(layer)        = excess_NH4

       g%dlt_NH4_net(layer) = g%NH4(layer) - g%NH4_yesterday(layer)
       g%dlt_NO3_net(layer) = g%NO3(layer) - g%NO3_yesterday(layer)

       g%NH4_yesterday(layer) = g%NH4(layer)
       g%NO3_yesterday(layer) = g%NO3(layer)
   end do


   call pop_routine (my_name)
   return
end subroutine



!     ===========================================================
subroutine soiln2_min_humic (layer, dlt_c_biom, dlt_c_atm, dlt_n_min)
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Sub-Program Arguments
   integer    index                 ! index - 1 for aerobic and 2 for anaerobic conditions
   integer    layer                 ! (INPUT) layer count
   real       dlt_n_min             ! (OUTPUT) net humic N mineralized
                                    ! (kg/ha)
   real       dlt_c_biom            ! (OUTPUT) carbon to biomass pool (kg/ha)
   real       dlt_c_atm             ! (OUTPUT) carbon to atmosphere (kg/ha)

!+  Purpose
!       Mineralise some humic material.  Calculates the
!       daily rate of decomposition and net nitrogen mineralisation from
!       the humic pool.

!+  Assumptions
!       There is an g_inert_C component of the humic pool that is not
!       subject to mineralization

!+  Notes
!       Net mineralisation can be negative if
!         soil_CN > mCN/ef_hum

!+  Mission Statement
!     Calculate the humic rate of decomposition and nitrogen mineralisation

!+  Calls


!+  Constant Values
   character  my_name*(*)           ! subroutine name
   parameter (my_name = 'soiln2_min_humic')

!+  Local Variables
   real       mf                    ! moisture factor
   real       tf                    ! temperature factor
   real       dlt_c_min_tot         !  humic C mineralized kg/ha
   real       dlt_n_min_tot         !  humic N mineralized kg/ha

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   ! dsg 200508  use different values for some constants when there's a pond and anaerobic conditions dominate
   if (g%pond_active.eq.'no') then
       index = 1
   else if (g%pond_active.eq.'yes') then
       index = 2
   else
   endif


   if (g%soiltype.eq.'rothc') then
      tf = rothc_tf (layer,index)
   else
      tf = soiln2_tf (layer,index)
   endif

   mf = soiln2_wf(layer,index)

   ! get the rate of mineralization of N from the humic pool

   dlt_c_min_tot = (g%hum_c(layer) - g%inert_c(layer))* c%rd_hum(index) * tf * mf
   dlt_n_min_tot = divide (dlt_c_min_tot, g%soil_cn, 0.0)

   dlt_c_biom = dlt_c_min_tot * c%ef_hum
   dlt_c_atm = dlt_c_min_tot *(1.0 - c%ef_hum)
   dlt_n_min = dlt_n_min_tot - divide (dlt_c_biom, c%mcn, 0.0)

   call pop_routine (my_name)
   return
end subroutine



!     ===========================================================
subroutine soiln2_min_biomass (layer,dlt_c_hum,dlt_c_atm,dlt_n_min)
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Sub-Program Arguments
   integer    layer           ! (INPUT) layer count
   real       dlt_n_min       ! (OUTPUT) net biomass N mineralized (kg/ha)
   real       dlt_c_hum       ! (OUTPUT) carbon to humic pool (kg/ha)
   real       dlt_c_atm       ! (OUTPUT) carbon to atmosphere (kg/ha)

!+  Purpose
!       Mineralise some soil biomass material.  Calculates the
!       daily rate of decomposition and net nitrogen mineralisation
!       from the biomass pool

!+  Mission Statement
!     Calculate the biomass rate of decomposition and nitrogen mineralisation

!+  Calls

!+  Constant Values
   character  my_name*(*)           ! subroutine name
   parameter (my_name = 'soiln2_min_biomass')

!+  Local Variables
   integer    index                 ! index - 1 for aerobic and 2 for anaerobic conditions
   real       mf                    ! moisture factor
   real       tf                    ! temperature factor
   real       dlt_c_min_tot         ! biomass C mineralized kg/ha
   real       dlt_n_min_tot         ! biomass N mineralized kg/ha

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   ! dsg 200508  use different values for some constants when anaerobic conditions dominate
   if (g%pond_active.eq.'no') then
       index = 1
   else if (g%pond_active.eq.'yes') then
       index = 2
   else
   endif

   if (g%soiltype.eq.'rothc') then
      tf = rothc_tf (layer,index)
   else
      tf = soiln2_tf (layer,index)
   endif
   mf = soiln2_wf(layer,index)

   ! get the rate of mineralization of C & N from the biomass pool

   dlt_n_min_tot = g%biom_n(layer) * c%rd_biom(index) * tf * mf
   dlt_c_min_tot = dlt_n_min_tot * c%mcn

   dlt_c_hum = dlt_c_min_tot * c%ef_biom * (1.0 - c%fr_biom_biom)
   dlt_c_atm = dlt_c_min_tot *(1.0 - c%ef_biom)

   ! calculate net mineralization

   dlt_n_min = dlt_n_min_tot- divide (dlt_c_hum, g%soil_cn, 0.0)- divide ((dlt_c_min_tot - dlt_c_atm - dlt_c_hum), c%mcn, 0.0)

   call pop_routine (my_name)
   return
end subroutine



!     ===========================================================
subroutine soiln2_min_fom (layer, dlt_c_biom, dlt_c_hum, dlt_c_atm, dlt_fom_n, dlt_n_min)
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Sub-Program Arguments
   integer    layer           ! (INPUT) soil layer number
   real       dlt_c_atm (*)   ! (OUTPUT) carbon to atmosphere (kg/ha)
   real       dlt_c_biom (*)  ! (OUTPUT) carbon to biomass (kg/ha)
   real       dlt_c_hum (*)   ! (OUTPUT) carbon to humic (kg/ha)
   real       dlt_fom_n(*)    ! (OUTPUT) amount of N mineralized (kg/ha)
                              !  from each pool
   real       dlt_n_min       ! (OUTPUT) net N mineralized (kg/ha)

!+  Purpose
!       Mineralise some fresh organic matter.
!       Calculates the daily rate of decomposition, partitions the carbon
!       in to soil pools and loss to atmosphere,
!       and the net nitrogen mineralisation (negative if N immobilized) from
!       the fresh organic matter pool.

!+  Mission Statement
!     Calculate the fresh organic matter rate of decomposition and nitrogen mineralisation

!+  Calls

!+  Constant Values
   character  my_name*(*)           ! name of subroutine
   parameter (my_name = 'soiln2_min_fom')

!+  Local Variables
   real       cnr                   ! ratio C in fresh OM to N available
                                    !    for decay
   real       cnrf                  ! C:N ratio factor
   real       drate                 ! fraction of fom used in
                                    !    mineralization
   real       fom_c                 ! fresh organic carbon (kg/ha)
   real       fom_n                 ! fresh organic nitrogen (kg/ha)
   integer    fractn                ! fraction number
   integer    index                 ! index = 1 for aerobic conditions, 2 for anaerobic conditions
   real       grcm                  ! gross amount of fresh organic carbon
                                    !    mineralized (kg/ha)
   real       grnm                  ! gross amount of N released from fresh
                                    !    organic matter (kg/ha)
   real       mf                    ! moisture limiting factor
   real       tf                    ! temperature limitimg factor
   real       nit_tot               ! total N avaliable for mineralization
                                    !    (kg/ha)
   real       dlt_c_min_tot(nfract) ! amount of C mineralized (kg/ha)
                                    !  from each pool
   real       dlt_fom_c_min_tot     ! total C mineralized (kg/ha)
                                    !  summed across fpools
   real       dlt_fom_n_min_tot     ! amount of fresh organic
                                    !    N mineralized across fpools (kg/ha)
   real       dlt_c_biom_tot        ! C mineralized converted to biomass
   real       dlt_c_hum_tot         ! C mineralized converted to humic
   real       dlt_n_min_tot(nfract) ! amount of fresh organic
                                    !    N mineralized in each pool (kg/ha)
   real       n_demand              ! potential N immobilization
   real       n_avail               ! available N for immobilization
   real       scale_of              ! factor to reduce mineralization
                                    ! rates if insufficient N to meet
                                    ! immobilization demand
   real       fom_cn                ! CN ratio of fom pool
   character  err_string*80
!- Implementation Section ----------------------------------

   call push_routine (my_name)

   ! dsg 200508  use different values for some constants when anaerobic conditions dominate
   if (g%pond_active.eq.'no') then
       index = 1
   else if (g%pond_active.eq.'yes') then
       index = 2
   else
   endif

   ! get total available mineral N
   nit_tot = (g%no3(layer) - g%no3_min(layer))+ (g%nh4(layer) - g%nh4_min(layer))
   nit_tot = l_bound (nit_tot, 0.0)

   fom_c = sum_real_array (g%fom_c_pool(1,layer), nfract)

   !dsg   the fom nitrogen must now also be totalled
   fom_n = sum_real_array (g%fom_n_pool(1,layer), nfract)

   ! calculate a C:N ratio that includes mineral-N in the lay
   cnr = divide(fom_c, fom_n + nit_tot, 0.0)

   ! calculate the C:N ratio factor
   cnrf = exp (-c%cnrf_coeff* (cnr - c%cnrf_optcn) /c%cnrf_optcn)
   cnrf = bound (cnrf, 0.0, 1.0)

   ! get temperature & moisture factors for the layer
   if (g%soiltype.eq.'rothc') then
      tf = rothc_tf (layer,index)
   else
      tf = soiln2_tf (layer,index)
   endif
   mf = soiln2_wf (layer,index)

   ! calulate gross amount of C & N released due to mineralization
   ! of the fresh organic matter.

   if (fom_c.ge.c%fom_min) then

      ! take the decomposition of carbohydrate-like,
      ! cellulose-like and lignin-like fractions (fpools)
      ! of the residue in turn.

      dlt_fom_n_min_tot = 0.0
      dlt_fom_c_min_tot =0.0
      dlt_n_min_tot(:) = 0.0
      dlt_c_min_tot(:) = 0.0

      fom_cn = divide (fom_c, fom_n, 0.0)


      do fractn = 1,nfract
         drate = c%rd_fom(fractn,index) * cnrf * tf * mf

         ! calculate the amounts of carbon and nitrogen mineralized
         grcm = drate * g%fom_c_pool(fractn,layer)
         !dsg calculate grnm using same method as grcm
         grnm = drate * g%fom_n_pool(fractn,layer)

         dlt_fom_n_min_tot = dlt_fom_n_min_tot + grnm
         dlt_c_min_tot(fractn) = grcm
         dlt_n_min_tot(fractn) = grnm
         dlt_fom_c_min_tot = dlt_fom_c_min_tot + grcm
       end do


      ! calculate potential transfers to biom and humic pools

      dlt_c_biom_tot = dlt_fom_c_min_tot * c%ef_fom * c%fr_fom_biom
      dlt_c_hum_tot = dlt_fom_c_min_tot * c%ef_fom* (1.0 - c%fr_fom_biom)

      ! test whether adequate N available to meet immobilization demand
      n_demand = divide (dlt_c_biom_tot, c%mcn, 0.0)+ divide (dlt_c_hum_tot, g%soil_cn, 0.0)
      n_avail = nit_tot + dlt_fom_n_min_tot

      if (n_demand.gt.n_avail) then
         ! rate of mineralization must be scaled back so that immobilization can be satisfied

         scale_of = divide (nit_tot, n_demand - dlt_fom_n_min_tot, 0.0)
         scale_of = bound (scale_of, 0.0, 1.0)
      else
         scale_of = 1.0   ! supply exceeds demand
      endif

      ! now adjust carbon transformations etc.
      !dsg   and similarly for npools

      do fractn = 1, nfract
         dlt_c_hum (fractn) = dlt_c_min_tot(fractn)* c%ef_fom* (1.0 - c%fr_fom_biom)* scale_of

         dlt_c_biom(fractn) = dlt_c_min_tot(fractn)* c%ef_fom* c%fr_fom_biom* scale_of

         dlt_c_atm (fractn) = dlt_c_min_tot(fractn)* (1.0 - c%ef_fom)* scale_of

         dlt_fom_n(fractn)=dlt_n_min_tot(fractn)*scale_of

         dlt_c_hum(fractn) = round_to_zero (dlt_c_hum(fractn))
         dlt_c_biom(fractn) = round_to_zero (dlt_c_biom(fractn))
         dlt_c_atm(fractn) = round_to_zero (dlt_c_atm(fractn))
         dlt_fom_n(fractn) = round_to_zero (dlt_fom_n(fractn))
      end do

      dlt_n_min = (dlt_fom_n_min_tot - n_demand) * scale_of

   else
      call fill_real_array (dlt_c_hum, 0.0, nfract)
      call fill_real_array (dlt_c_biom, 0.0, nfract)
      call fill_real_array (dlt_c_atm, 0.0, nfract)
      call fill_real_array (dlt_fom_n, 0.0, nfract)

      dlt_n_min = 0.0

   endif

   call pop_routine (my_name)
   return
end subroutine



!     ===========================================================
subroutine soiln2_nitrification (layer, dlt_rntrf)
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Sub-Program Arguments
   integer    layer                 ! (INPUT) soil layer count
   real       dlt_rntrf             ! (OUTPUT) actual rate of nitrification
                                    !    (kg/ha)

!+  Purpose
!           Calculates nitrification of NH4 in a given soil layer.

!+  Notes
!        This routine is much simplified from original CERES code
!        g_ph effect on nitrification is not invoked

!+  Mission Statement
!     Calculate nitrification of NH4 in %1

!+  Calls


!+  Constant Values
   character  my_name*(*)           ! name of subroutine
   parameter (my_name = 'soiln2_Nitrification')

!+  Local Variables
   integer    index                 ! index - 1 for aerobic and 2 for anaerobic conditions
   real       opt_rate_ppm          ! rate of nitrification
                                    ! under optimum conditions (ppm)
   real       opt_rate              ! rate of nitrification
                                    ! under optimum conditions (kg/ha)
   real       phf                   ! g_ph factor
   real       pni                   ! potential nitrification index (0-1)
   real       nh4_avail             ! available ammonium (kg/ha)
   real       nh4ppm                ! ammonium in soil (ppm)
   real       tf                    ! temperature factor (0-1)
   real       wfd                   ! water factor (0-1)

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   ! dsg 200508  use different values for some constants when anaerobic conditions dominate
   if (g%pond_active.eq.'no') then
       index = 1
   else if (g%pond_active.eq.'yes') then
       index = 2
   else
   endif


   phf = soiln2_phf_nitrf (layer)

   ! get a 0-1 water factor for nitrification
   wfd = soiln2_wf_nitrf (layer,index)

   ! get a 0-1 temperature factor from soil temperature
   tf = soiln2_tf (layer,index)

   ! use a combined index to adjust rate of nitrification
   ! NOTE phn removed to match CERES v1
   pni = min (wfd, tf, phf)

   ! get actual rate of nitrification for layer
   nh4ppm = g%nh4(layer)*soiln2_fac (layer)
   opt_rate_ppm = divide((c%nitrification_pot* nh4ppm),(nh4ppm + c%nh4_at_half_pot),0.0)
   opt_rate = divide(opt_rate_ppm,soiln2_fac(layer),0.0)
   dlt_rntrf = pni * opt_rate
   nh4_avail = l_bound (g%nh4(layer) - g%nh4_min(layer), 0.0)
   dlt_rntrf = bound (dlt_rntrf, 0.0, nh4_avail)

   call pop_routine (my_name)
   return
end subroutine



!     ===========================================================
subroutine soiln2_denitrification (layer, dlt_n_atm)
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Sub-Program Arguments
   real       dlt_n_atm             ! (OUTPUT) denitrification rate
                                    !    - kg/ha/day
   integer    layer                 ! (INPUT) soil layer counter

!+  Purpose
!           Calculates denitrification whenever the soil water in the
!           layer > the drained upper limit (Godwin et al., 1984),
!           the NO3 nitrogen concentration > 1 mg N/kg soil,
!           and the soil temperature >= a minimum temperature.
!           NOTE denitrification routine not validated

!+  Assumptions
!       That there is a root system present.  Rolston et al. say that the
!       denitrification rate coeffficient (dnit_rate_coeff) of non-cropped
!       plots was 0.000168 and for cropped plots 3.6 times more
!       (dnit_rate_coeff = 0.0006). The larger rate coefficient was required
!       to account for the effects of the root system in consuming oxygen
!       and in adding soluble organic C to the soil.

!+  Notes
!       Reference: Rolston DE, Rao PSC, Davidson JM, Jessup RE.
!       "Simulation of denitrification losses of Nitrate fertiliser applied
!        to uncropped, cropped, and manure-amended field plots".
!        Soil Science April 1984 Vol 137, No 4, pp 270-278.
!
!       Reference for Carbon availability factor -
!       Reddy KR, Khaleel R, Overcash MR. "Carbon transformations in land
!       areas receiving organic wastes in relation to nonpoint source
!       pollution: A conceptual model".  J.Environ. Qual. 9:434-442.

!+  Mission Statement
!     Calculate denitrification in %1

!+  Calls


!+  Constant Values
   character  my_name*(*)           ! name of subroutine
   parameter (my_name = 'soiln2_Denitrification')

!+  Local Variables
   real       active_c              ! water extractable organic carbon as
                                    ! "available" C conc. (mg C/kg soil)
   real       tf                    ! temperature factor affecting
                                    !    denitrification rate (0-1)
   real       wf                    ! soil moisture factor affecting
                                    !    denitrification rate (0-1)
   real       no3_avail             ! soil nitrate available (kg/ha)
   real       hum_c_conc            ! carbon conc. of humic pool
                                    !    (mg C/kg soil)
   real       fom_c_conc            ! carbon conc. of fresh organic pool
                                    !    (mg C/kg soil)

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   if (g%no3(layer).ge.g%no3_min(layer)) then

      hum_c_conc = g%hum_c(layer) *soiln2_fac (layer)

      fom_c_conc = sum_real_array (g%fom_c_pool(1,layer), nfract)* soiln2_fac (layer)

      ! get available carbon concentration from soil organic
      ! carbon concentration (24.5+)

      ! Note CM V2 had active_c = fom_C_conc + 0.0031*hum_C_conc + 24.5
      active_c = 0.0031*(fom_c_conc + hum_c_conc) + 24.5

      ! Get water factor (0-1)
      wf = soiln2_wf_denit (layer)

      ! get temperature factor from soil temperature (0-1)
      ! This is an empirical dimensionless function to account for
      ! the effect of temperature.
      ! The upper limit of 1.0 means that optimum denitrification
      ! temperature is 50 oC and above.  At 0 oC it is 0.1 of optimum,
      ! and at -20 oC is about 0.04.
      tf = 0.1* exp (0.046*g%soil_temp(layer))
      tf = bound (tf, 0.0, 1.0)

      ! calculate denitrification rate  - kg/ha
      dlt_n_atm = c%dnit_rate_coeff*active_c*wf*tf*g%no3(layer)

      ! prevent NO3 - N concentration from falling below NO3_min
      no3_avail = g%no3(layer) - g%no3_min(layer)
      dlt_n_atm = bound (dlt_n_atm, 0.0, no3_avail)

   else
      dlt_n_atm = 0.0
   endif

   call pop_routine (my_name)
   return
end subroutine



!     ===========================================================
real function soiln2_wf_nitrf (layer,index)
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Sub-Program Arguments
   integer    layer                 ! (INPUT) layer number
   integer    index                 ! index = 1 for aerobic conditions, 2 for anaerobic

!+  Purpose
!       Calculates a 0-1 water factor for nitrification.

!+  Assumptions
!       1 < layer < num_layers

!+  Mission Statement
!     Water factor for nitrification in %1

!+  Constant Values
   character  my_name*(*)           ! name of subroutine
   parameter (my_name = 'soiln2_wf_nitrf')

!+  Local Variables
   real       wfd                   ! temporary water factor (0-1)

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   if (g%sw_dep(layer).gt.g%dul_dep(layer)) then

      ! saturated
      wfd = 1.0 + divide (g%sw_dep(layer) - g%dul_dep(layer), g%sat_dep(layer) - g%dul_dep(layer), 0.0)
      wfd = bound (wfd, 1.0, 2.0)

   else

      ! unsaturated
      ! assumes rate of mineralization is at optimum rate
      ! until soil moisture midway between dul and ll15

      wfd = divide (g%sw_dep(layer) - g%ll15_dep(layer), (g%dul_dep(layer) - g%ll15_dep(layer)), 0.0)
      wfd = bound (wfd, 0.0, 1.0)

   endif

   if (index.eq.1) then
        soiln2_wf_nitrf =linear_interp_real (wfd, c%wf_nit_index, c%wf_nit_values, max_wf_values)
   else if (index.eq.2) then
        ! if pond is active, and aerobic conditions dominate, assume wf_nitrf = 0
        soiln2_wf_nitrf = 0.0
   else
   endif     

   call pop_routine (my_name)
   return
end function



!     ===========================================================
real function soiln2_wf_denit (layer)
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Sub-Program Arguments
   integer    layer                 ! (INPUT) layer number

!+  Purpose
!       Calculates a 0-1 water factor for denitrification

!+  Assumptions
!       1 < layer < num_layers

!+  Mission Statement
!     Water factor for denitrification in %1

!+  Constant Values
   character  my_name*(*)           ! name of subroutine
   parameter (my_name = 'soiln2_wf_denit')

!+  Local Variables
   real       wfd                   ! temporary water factor (0-1)

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   if (g%sw_dep(layer).gt.g%dul_dep(layer)) then

     ! saturated
      wfd = divide (g%sw_dep(layer) - g%dul_dep(layer), g%sat_dep(layer) - g%dul_dep(layer), 0.0)**c%dnit_wf_power
   else

     ! unsaturated
      wfd = 0
   endif

   soiln2_wf_denit = bound (wfd, 0.0, 1.0)

   call pop_routine (my_name)
   return
end function



!     ===========================================================
real function soiln2_wf (layer,index)
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Sub-Program Arguments
   integer    layer                 ! (INPUT) layer number
   integer    index                 ! index = 1 for aerobic conditions, 2 for anaerobic

!+  Purpose
!       Calculates a 0-1 water factor for mineralisation.

!+  Assumptions
!       1 < layer < num_layers

!+  Mission Statement
!     Water factor for mineralisation in %1

!+  Constant Values
   character  my_name*(*)           ! name of subroutine
   parameter (my_name = 'soiln2_wf')

!+  Local Variables
   real       wfd                   ! temporary water factor (0-1)

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   if (g%sw_dep(layer).gt.g%dul_dep(layer)) then

      ! saturated
      wfd = 1.0 + divide (g%sw_dep(layer) - g%dul_dep(layer), g%sat_dep(layer) - g%dul_dep(layer), 0.0)
      wfd = bound (wfd, 1.0, 2.0)

   else

      ! unsaturated

      ! assumes rate of mineralization is at optimum rate
      !    until soil moisture midway between dul and ll15

      wfd = divide (g%sw_dep(layer) - g%ll15_dep(layer), (g%dul_dep(layer) - g%ll15_dep(layer)), 0.0)
      wfd = bound (wfd, 0.0, 1.0)

   endif

   if (index.eq.1) then
        soiln2_wf =linear_interp_real (wfd, c%wf_min_index, c%wf_min_values, max_wf_values)
   else if (index.eq.2) then
        ! if pond is active, and liquid conditions dominate, assume wf = 1
        soiln2_wf = 1.0
   else
   endif     
   

   call pop_routine (my_name)
   return
end function



!     ===========================================================
real function soiln2_tf (layer, index)
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Sub-Program Arguments
   integer    layer                 ! (INPUT) layer number
   integer    index                 ! index = 1 for aerobic conditions, 2 for anaerobic

!+  Purpose
!       Calculate a temperature factor, based on the soil temperature
!       of the layer, for nitrification and mineralisation

!+  Notes
!           - the layer l < 1 or > num_layers
!           - the soil temperature falls outside of lower to upper

!+  Mission Statement
!     Nitrification and mineralisation soil temperature factor in %1

!+  Constant Values
   character  my_name*(*)           ! name of subroutine
   parameter (my_name = 'soiln2_tf')

!+  Local Variables
   real tf                          ! temporary temperature factor

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   ! Alternate version from CM
   !      tf = (g%soil_temp(layer) - 5.0) /30.0
   ! because tf is bound between 0 and 1, the effective
   ! temperature (g%soil_temp) lies between 5 to 35.

   ! alternative quadratic temperature function is preferred
   !  with optimum temperature (CM - used 32 deg)
   if (g%soil_temp(layer).gt.0.0) then
      tf = divide (g%soil_temp(layer)*g%soil_temp(layer), c%opt_temp(index)**2.0, 0.0)
      tf = bound (tf, 0.0, 1.0)
   else
      ! Soil is too cold for mineralisation
      tf = 0.0
   endif
   soiln2_tf = tf


   call pop_routine (my_name)
   return
end function

!     ===========================================================
real function rothc_tf (layer,index)
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Sub-Program Arguments
   integer    layer                 ! (INPUT) layer number
   integer    index                 ! index = 1 for aerobic conditions, 2 for anaerobic

!+  Purpose
!       Calculate a temperature factor, based on the soil temperature
!       of the layer, for nitrification and mineralisation

!+  Notes
!           - the layer l < 1 or > num_layers
!           - the soil temperature falls outside of lower to upper

!+  Mission Statement
!     Nitrification and mineralisation soil temperature factor in %1

!+  Constant Values
   character  my_name*(*)           ! name of subroutine
   parameter (my_name = 'soiln2_tf')

!+  Local Variables
   real tf                          ! temporary temperature factor
   real t
!- Implementation Section ----------------------------------

   t = min(g%soil_temp(layer),c%opt_temp(index))
   tf = 47.9/(1+exp(106/(t+18.3)))
   rothc_tf = tf


   return
end function


!     ===========================================================
subroutine soiln2_check_profile (new_profile)
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Sub-Program Arguments
   real       new_profile(*)

!+  Purpose
!     See if soil profile has changed since yesterday or whether some
!     soil has eroded. If so, move pools around to cater for it.

!+  Mission Statement
!     Allow for any changes in soil profile since yesterday

!+  Constant Values
   character  my_name*(*)           ! name of subroutine
   parameter (my_name = 'soiln2_check_profile')

!+  Local Variables
   real       loss                  ! temporary
   real       fom_c_pool(max_layer)   ! temporary
   integer    pool                  ! index
   integer    layer                 ! index

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   g%dlt_n_sed = 0.0
   g%dlt_c_loss_sed = 0.0

   ! How to decide:
   ! if bedrock is lower than lowest  profile depth, we won't see
   ! any change in profile, even if there is erosion. Ideally we
   ! should test both soil_loss and dlayer for changes to cater for
   ! manager control. But, the latter means we have to fudge enr for the
   ! loss from top layer.

   if (g%dlt_soil_loss.gt.0.0 .and. g%p_n_reduction.eq.on) then

      ! move pools
      call soiln2_move_layers (g%nh4, new_profile, loss)
      g%dlt_n_sed = g%dlt_n_sed + loss

      call soiln2_move_layers (g%inert_c, new_profile, loss)
      g%dlt_c_loss_sed = g%dlt_c_loss_sed + loss

      call soiln2_move_layers (g%biom_c, new_profile, loss)
      g%dlt_c_loss_sed = g%dlt_c_loss_sed + loss

      call soiln2_move_layers (g%biom_n, new_profile, loss)
      g%dlt_n_sed = g%dlt_n_sed + loss

      call soiln2_move_layers (g%hum_c, new_profile, loss)
      g%dlt_c_loss_sed = g%dlt_c_loss_sed + loss

      call soiln2_move_layers (g%hum_n, new_profile, loss)
      g%dlt_n_sed = g%dlt_n_sed + loss

      call soiln2_move_layers (g%fom_n, new_profile, loss)
      g%dlt_n_sed = g%dlt_n_sed + loss

      do pool = 1, nfract
         do layer = 1, max_layer
            fom_c_pool(layer) = g%fom_c_pool(pool, layer)
         end do

         call soiln2_move_layers (fom_c_pool, new_profile, loss)
         g%dlt_c_loss_sed = g%dlt_c_loss_sed + loss

         do layer = 1, max_layer
            g%fom_c_pool(pool, layer) = fom_c_pool(layer)
         end do
      end do

   else
      ! nothing
   endif

   ! update g_dlayer

   do layer = 1, max_layer
      g%dlayer(layer) = new_profile(layer)
   end do

   call pop_routine (my_name)
   return
end subroutine



!     ================================================================
subroutine soiln2_move_layers (variable,new_profile,profile_loss)
!     ================================================================
   Use Infrastructure
   implicit none

!+  Sub-Program Arguments
   real       variable(*)           ! (INPUT) variable to change
   real       new_profile(*)        ! (INPUT) new profile
   real       profile_loss          ! (OUTPUT) kgs lost from layer 1

!+  Purpose
!     move a layer

!+  Assumptions
!     bedrock always in lowest profile

!+  Notes
!     - does a mass balance check.

!+  Mission Statement
!     Move %1 to %2 with a loss of %3

!+  Calls

!+  Constant Values
   character  my_name*(*)
   parameter (my_name = 'soiln2_move_layers')

!+  Local Variables
   real       enr                   ! enrichment ratio
   real       layer_gain            ! gain to each layer
   real       layer_loss            ! loss to each layer
   real       yesterdays_n
   real       todays_n
   integer    lowest_layer
   integer    new_lowest_layer
   real       profile_depth         ! current profile depth (mm)
   real       profile_gain          ! gain to profile
   real       new_profile_depth     ! new profile depth (mm)
   integer    layer

!- Implementation Section ----------------------------------
   call push_routine(my_name)

   profile_loss = 0.0
   layer_loss = 0.0
   layer_gain = 0.0

   lowest_layer = count_of_real_vals (g%dlayer, max_layer)
   new_lowest_layer = count_of_real_vals (new_profile, max_layer)

   ! for mass balance later
   yesterdays_n = sum_real_array (variable, lowest_layer)

   ! initialise layer loss from below profile same as bottom layer

   profile_depth = sum_real_array (g%dlayer, lowest_layer)
   new_profile_depth = sum_real_array (new_profile, new_lowest_layer)

   if (reals_are_equal (profile_depth, new_profile_depth))then
      ! move from below bottom layer - assume it has same properties
      ! as bottom layer
      layer_loss = variable(lowest_layer)* soiln2_layer_fract (lowest_layer)

   else
      ! we're going into bedrock
      layer_loss = 0.0

      ! now see if bottom layers have been merged.

      if (lowest_layer .gt. new_lowest_layer .and.lowest_layer .gt. 1) then

         ! merge the layers..
         do layer = lowest_layer, new_lowest_layer+1, -1
            variable(layer-1) = variable(layer-1) + variable(layer)
            variable(layer) = 0.0
         end do

      else
         ! they haven't been merged yet
      endif

   endif
   profile_gain = layer_loss

   ! now move from bottom layer to top

   do layer = new_lowest_layer, 1, -1
      ! this layer gains what the lower layer lost
      layer_gain = layer_loss
      layer_loss = variable(layer) * soiln2_layer_fract (layer)
      variable(layer) = variable(layer) + layer_gain - layer_loss
   end do

   ! now adjust top layer for enrichment
   enr = c%enr_a_coeff* (t2kg * g%dlt_soil_loss) **(-1.0 * c%enr_b_coeff)
   enr = bound (enr, 1.0, c%enr_a_coeff)

   profile_loss = layer_loss * enr
   variable(1) = variable(1) + (layer_loss - profile_loss)
   variable(1) = l_bound (variable(1), 0.0)

   ! check mass balance
   todays_n = sum_real_array (variable, new_lowest_layer)

   call bound_check_real_var(yesterdays_n  + profile_gain - profile_loss, todays_n, todays_n, ' N mass balance out')

   call pop_routine (my_name)
   return
end subroutine



!     ===========================================================
real function soiln2_layer_fract (layer)
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Sub-Program Arguments
   integer    layer                 ! (INPUT) layer number

!+  Purpose
!     fraction of layer moved

!+  Mission Statement
!     Fraction of %1 moved

!+  Calls


!+  Constant Values
   character my_name*(*)
   parameter (my_name = 'soiln2_layer_fract')

!+  Local Variables
   real       layer_fract           ! fraction of layer moved
   character  string*200

!- Implementation Section ----------------------------------
   call push_routine (my_name)

   layer_fract = g%dlt_soil_loss * soiln2_fac (layer) / 1000.0
   if (layer_fract .gt. 1.0) then
      write (string, '(a, i3, a, f6.1,a)')'Soil loss is greater than depth of layer(', layer, ') by ', layer_fract*100.0, '%.'// new_line// 'Constrained to this layer. '// 'Re-mapping of SoilN pools will be incorrect.'
      call warning_error (err_user, string)
   else
   endif
   soiln2_layer_fract = bound (layer_fract, 0.0, 1.0)

   call pop_routine (my_name)
   return
end function



!     ===========================================================
real function soiln2_phf_nitrf (layer)
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Sub-Program Arguments
   integer    layer                 ! (INPUT) layer number

!+  Purpose
!       Calculates a 0-1 pH factor for nitrification.

!+  Assumptions
!       1 < layer < num_layers

!+  Mission Statement
!     Calculate pH factor for nitrification

!+  Constant Values
   character  my_name*(*)           ! name of subroutine
   parameter (my_name = 'soiln2_pHf_nitrf')

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   soiln2_phf_nitrf =linear_interp_real (g%ph(layer),c%phf_nit_ph,c%phf_nit_values, max_phf_values)

   call pop_routine (my_name)
   return
end function


!     ===========================================================
subroutine soiln2_check_data_supply ()
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Purpose
!       Check what variables are being supplied by other modules.

!+  Mission Statement
!       Check what variables are being supplied by other modules.

!+  Constant Values
   character  my_name*(*)           ! name of subroutine
   parameter (my_name = 'soiln2_check_data_supply')

!+  Local Variables
   real    temp_var    ! temporary variable
   integer numvals                  ! number of values returned

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   call get_real_var_optional (unknown_module, 'ave_soil_temp()', '(oC)', temp_var, numvals, -20.0, 80.0)

   if(numvals .gt. 0.0)then
      ! another module owns soil temperature
      g%use_external_st = .true.
   else
      ! I will own it
      g%use_external_st = .false.
   endif

   call get_real_var_optional (unknown_module, 'ph()', '()', temp_var, numvals, 3.5, 11.0)

   if(numvals .gt. 0.0)then
      ! another module owns soil ph
      g%use_external_ph = .true.
   endif

   call pop_routine (my_name)
   return
end subroutine


! ====================================================================
subroutine soiln2_notification ()
! ====================================================================
   Use Infrastructure
   implicit none

!+  Purpose
!      Notify all interested modules about this module's ownership
!      of solute information.

!+  Mission Statement
!     Notify other modules of ownership of solute information

!+  Constant Values

   character*(*) myname               ! name of current procedure
   parameter (myname = 'soiln2_notification')

!+  Local Variables
   character  solute_names(7)*32    ! list of soilN solutes ()
   integer    numsolutes
!- Implementation Section ----------------------------------
   call push_routine (myname)

   call new_postbox()

   solute_names(1) = 'no3'
   solute_names(2) = 'nh4'
   solute_names(3) = 'urea'
   solute_names(4) = 'org_c_pool1'
   solute_names(5) = 'org_c_pool2'
   solute_names(6) = 'org_c_pool3'
   solute_names(7) = 'org_n'

   if (g%use_organic_solutes .eq. .true.) then
      ! publish all the solutes including the organic ones
      numsolutes = 7
   else
      ! unless the user states they need them - don't publish the organic solutes
      numsolutes = 3
   endif

   call post_char_array (DATA_new_solute_names, '()', solute_names, numsolutes)

   call event_send (unknown_module, EVENT_new_solute)

   call delete_postbox()


   call pop_routine (myname)
   return
end subroutine


!     ===========================================================
subroutine soiln2_ONtick (variant)
!     ===========================================================
   Use Infrastructure
   implicit none

   integer, intent(in) :: variant
!+  Purpose
!     Update internal time record and reset daily state variables.

!+  Mission Statement
!     Update internal time record and reset daily state variables.

!+  Local Variables
   type(timeType) :: tick

!+  Constant Values
   character*(*) myname               ! name of current procedure
   parameter (myname = 'soiln2_ONtick')

!- Implementation Section ----------------------------------
   call push_routine (myname)

   call unpack_time(variant, tick)
   call jday_to_day_of_year(dble(tick%startday), g%day_of_year,g%year)

   ! Reset Potential Decomposition Register
   g%num_residues = 0
   g%pot_c_decomp(:) = 0.0
   g%pot_n_decomp(:) = 0.0


   call pop_routine (myname)
   return
end subroutine

!     ===========================================================
subroutine soiln2_ONnewmet (variant)
!     ===========================================================
   Use Infrastructure
   implicit none

   integer, intent(in) :: variant

!+  Purpose
!     Get new met data

!+  Mission Statement
!     Get new met data

!+  Local Variables
   type(newmetType) :: newmet
   integer numvals

!+  Constant Values
   character*(*) myname               ! name of current procedure
   parameter (myname = 'soiln2_ONnewmet')

!- Implementation Section ----------------------------------
   call push_routine (myname)

   call unpack_newmet(variant, newmet)
   g%radn = newmet%radn
   g%maxt = newmet%maxt
   g%mint = newmet%mint
   call pop_routine (myname)
   return
end subroutine

!     ===========================================================
subroutine soiln2_get_site_variables ()
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Purpose
!      Get the values of site-specific variables from other modules

!+  Mission Statement
!     Get Site-Specific Variables

!+  Constant Values
   character  my_name*(*)
   parameter (my_name='soiln2_get_site_variables')

!+  Local Variables
   integer      numvals             ! number of values returned

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   call get_real_var (unknown_module, 'latitude', '(deg)', g%latitude, numvals, -60.0,  60.0)

   call get_real_var (unknown_module, 'salb', '()', g%salb, numvals, 0.00001, 1.0)

   call pop_routine (my_name)
   return
end subroutine


!     ===========================================================
subroutine soiln2_ONPotentialResidueDecompositionCalculated(variant)
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
   character*200  message
   integer numvals
   integer residue                ! simple som counter
   integer num_som                ! number of som elements provided in event
   type (SurfaceOrganicMatterDecompType)::SurfaceOrganicMatterDecomp

!+  Constant Values
   character*(*) myname               ! name of current procedure
   parameter (myname ='soiln2_ONPotentialResidueDecompositionCalculated')

!- Implementation Section ----------------------------------
   call push_routine (myname)

   call unpack_SurfaceOrganicMatterDecomp(variant,SurfaceOrganicMatterDecomp)
   g%num_residues = SurfaceOrganicMatterDecomp%num_pool

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
subroutine soiln2_OC_percent (oc_percent)
!     ===========================================================
   Use Infrastructure
   implicit none


!+  Sub-Program Arguments
   real    oc_percent(*)            ! (OUTPUT)

!+  Purpose
!     Calculate Organic Carbon Percentage

!+  Mission Statement
!     Calculate Organic Carbon Percentage

!+  Calls


!+  Local Variables
   integer num_layers
   integer layer
   real    oc_ppm                   !organic carbon (ppm)

!+  Constant Values
   real      ppm2fract                      !parts per million convert to fraction
   parameter (ppm2fract= 1.0/1000000.0)

   character*(*) myname               ! name of current procedure
   parameter (myname = 'soiln2_OC_percent')

!- Implementation Section ----------------------------------
   call push_routine (myname)

   num_layers = count_of_real_vals (g%dlayer, max_layer)

   do layer = 1, num_layers
      oc_ppm = (g%hum_c(layer) + g%biom_c(layer))* soiln2_fac (layer)
      oc_percent(layer) = oc_ppm * ppm2fract * fract2pcnt
   end do

   call pop_routine (myname)
   return
end subroutine


! ====================================================================
subroutine soiln2_Send_Nbalance_Event ()
! ====================================================================
   Use Infrastructure
   implicit none

!+  Purpose
!     Notify other modules of completion of N balance.

!+  Mission Statement
!     Notify other modules of completion of N balance.

!+  Local Variables
   integer    num_layers            ! number of soil layers

!+  Constant Values
   character*(*) myname               ! name of current procedure
   parameter (myname = 'soiln2_Send_Nbalance_Event')

!- Implementation Section ----------------------------------
   call push_routine (myname)

   call new_postbox ()

   num_layers = count_of_real_vals (g%dlayer, max_layer)

   call post_real_array   (DATA_nh4_transform_net,'(kg/ha)', g%NH4_transform_net, num_layers)
   call post_real_array   (DATA_no3_transform_net,'(kg/ha)', g%NO3_transform_net, num_layers)

   call post_real_array   (DATA_dlt_nh4_net,'(kg/ha)', g%dlt_NH4_net, num_layers)
   call post_real_array   (DATA_dlt_no3_net,'(kg/ha)', g%dlt_NO3_net, num_layers)

   call event_send (unknown_module, EVENT_N_balance)

   call delete_postbox ()


   call pop_routine (myname)
   return
end subroutine

! ====================================================================
subroutine soiln2_Send_Cbalance_Event ()
! ====================================================================
   Use Infrastructure
   implicit none

!+  Purpose
!     Notify other modules of completion of C balance.

!+  Mission Statement
!     Notify other modules of completion of C balance.

!+  Local Variables
   integer    num_layers            ! number of soil layers
   integer    layer                 ! simple layer counter
   integer    fraction              ! simple FOM fraction counter
   integer    residue               ! simple residue source counter
   real       c_from_FOM            ! change in OC from FOM flows
   real       c_from_residues       ! change in OC from residue flows
   real       c_to_atmosphere       ! loss of OC to atmosphere
   real       dlt_oc(max_layer)     ! change in organic carbon
   real       dlt_om(max_layer)     ! change in organic matter

!+  Constant Values
   character*(*) myname               ! name of current procedure
   parameter (myname = 'soiln2_Send_Cbalance_Event')

!- Implementation Section ----------------------------------
   call push_routine (myname)

   call new_postbox ()

   num_layers = count_of_real_vals (g%dlayer, max_layer)

   do layer = 1, num_layers

      ! OC can increase by flows from FOM
      ! =================================
      c_from_FOM = 0.0

      do fraction = 1,nfract
         c_from_FOM = c_from_FOM+ g%dlt_fom_c_biom(fraction,layer)+ g%dlt_fom_c_hum(fraction,layer)
      end do

      ! OC can increase by flows from residues
      ! ======================================
      c_from_residues = 0.0

      do residue = 1, g%num_residues
         c_from_residues = c_from_residues+ g%dlt_res_c_biom(layer,residue)+ g%dlt_res_c_hum(layer,residue)
      end do

      ! OC can decrease by loss to atmosphere
      ! =====================================
      c_to_atmosphere = g%dlt_hum_c_atm(layer)+ g%dlt_biom_c_atm(layer)


      ! Now calculate net changes to OC
      ! ===============================
      dlt_oc(layer) = c_from_FOM+ c_from_residues- c_to_atmosphere

      ! Change in OM is related by a fixed factor
      ! =========================================
      dlt_om(layer) = dlt_oc(layer) * c%OC2OM_factor

   end do

   call post_real_array   (DATA_dlt_OC,'(kg/ha)', dlt_OC, num_layers)
   call post_real_array   (DATA_dlt_OM,'(kg/ha)', dlt_OM, num_layers)

   call event_send (unknown_module, EVENT_C_balance)

   call delete_postbox ()


   call pop_routine (myname)
   return
end subroutine


!     ===========================================================
subroutine Soiln2_ONNew_Profile (variant)
!     ===========================================================
   Use Infrastructure
   implicit none

!+  Purpose
!     Update internal soil layer structure with new data

!+  Mission Statement
!     Update internal soil layer structure with new data
   integer variant

!+  Local Variables
   type(NewProfileType) :: newProfile

!+  Constant Values
   character*(*) myname               ! name of current procedure
   parameter (myname = 'SoilN2_ONNew_Profile')

!- Implementation Section ----------------------------------
   call push_routine (myname)

!   g%ll15_dep(:) = 0.0
!   g%dul_dep(:) = 0.0
!   g%sat_dep(:) = 0.0
!   g%sw_dep(:) = 0.0
!   g%bd(:) = 0.0

   call unpack_newProfile(variant, newProfile)

   g%ll15_dep(:) = newProfile%ll15_dep
   g%dul_dep(:) = newProfile%dul_dep
   g%sat_dep(:) = newProfile%sat_dep
   g%sw_dep(:) = newProfile%sw_dep
   g%bd(:) = newProfile%bd

   call soiln2_check_profile (newProfile%dlayer)

   call pop_routine (myname)
   return
end subroutine





!  ===========================================================
subroutine OnNitrogenChanged (variant)
!  ===========================================================
   Use Infrastructure
   implicit none

   ! Another module wants to change our nitrogen

   integer, intent(in) :: variant
   type(NitrogenChangedType) :: NitrogenChanged
   integer :: layer

   call unpack_NitrogenChanged(variant, NitrogenChanged)
   do layer = 1,NitrogenChanged%num_DeltaNO3
      g%no3(layer) = g%no3(layer) + NitrogenChanged%DeltaNO3(layer)
      g%nh4(layer) = g%nh4(layer) + NitrogenChanged%DeltaNH4(layer)
      call bound_check_real_var (g%no3(layer), g%no3_min(layer), g%no3(layer), 'g%NO3(layer)')
      call bound_check_real_var (g%nh4(layer), g%nh4_min(layer), g%nh4(layer), 'g%NH4(layer)')
   enddo
   return
end subroutine

end module Soiln2Module

!     ===========================================================
subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
   use Soiln2Module
   implicit none
   ml_external alloc_dealloc_instance

!+  Sub-Program Arguments
   logical, intent(in) :: doAllocate

!+  Purpose
!      Module instantiation routine.

!- Implementation Section ----------------------------------

   if (doAllocate) then
      allocate(g)
      allocate(c)
      allocate(id)
   else
      deallocate(g)
      deallocate(c)
      deallocate(id)
   end if
   return
end subroutine



!     ===========================================================
subroutine Main (action, data_string)
!     ===========================================================
   Use Infrastructure
   Use SoilN2Module
   implicit none
   ml_external Main

!+  Sub-Program Arguments
    character  action*(*)           ! Message action to perform
    character  data_string*(*)      ! Message data

!+  Purpose
!      This routine is the interface between the main system and the
!      SoilN module.

!+  Mission Statement
!     SoilN2

!+  Constant Values
   character  my_name*(*)
   parameter (my_name='APSIM_SoilN2')
   character  err_string*120         ! Error message string

!- Implementation Section ----------------------------------

   call push_routine (my_name)

   if (action.eq.ACTION_get_variable) then
      call soiln2_send_my_variable (data_string)

   else if (action .eq. ACTION_set_variable) then
      call soiln2_set_my_variable (data_string)

   else if (action.eq.ACTION_process) then
      call soiln2_get_other_variables ()
      call soiln2_process ()
      call soiln2_send_Nbalance_Event ()
      call soiln2_send_Cbalance_Event ()
      if (g%pond_active.eq.'no') then 
        call Soiln2_sendActualResidueDecompositionCalculated()
      endif

   else if ((action.eq.ACTION_reset).or.(action.eq.ACTION_user_init)) then
      call soiln2_reset ()

   else if (action.eq.ACTION_sum_report) then
      call soiln2_sum_report()

   elseif (action.eq.ACTION_init) then
      call soiln2_reset ()
      call soiln2_notification()
      call soiln2_sum_report()

   else
      ! Don't use message
      call message_unused ()
 
   endif

   call pop_routine (my_name)
   return
end subroutine


! ====================================================================
! Do 1st stage initialisation
! ====================================================================
subroutine doInit1()
   use Soiln2Module
   Use infrastructure
   implicit none
   ml_external doInit1
   integer dummy

   ! events published
   id%ExternalMassFlow = add_registration(eventReg, 'ExternalMassFlow', ExternalMassFlowTypeDDML, '')
   id%new_solute = add_registration(eventReg, 'new_solute', ApsimVariantTypeDDML, '')
   id%n_balance = add_registration(eventReg, 'n_balance', ApsimVariantTypeDDML, '')
   id%c_balance = add_registration(eventReg, 'c_balance', ApsimVariantTypeDDML, '')
   id%actualresiduedecompositioncalculated = add_registration(eventReg, 'actualresiduedecompositioncalculated', SurfaceOrganicMatterDecompTypeDDML, '')

   ! events subscribed to
   id%process = add_registration(respondToEventReg, 'process', nullTypeDDML, '')
   id%reset = add_registration(respondToEventReg, 'reset', nullTypeDDML, '')
   id%sum_report = add_registration(respondToEventReg, 'sum_report', nullTypeDDML, '')
   id%IncorpFOM = add_registration(respondToEventReg, 'IncorpFOM', FOMLayerTypeDDML, '')
   id%tick = add_registration(respondToEventReg, 'tick', timeTypeDDML, '')
   id%newmet = add_registration(respondToEventReg, 'newmet', newmetTypeDDML, '')
   id%potentialresiduedecompositioncalculated = add_registration(respondToEventReg, 'potentialresiduedecompositioncalculated', SurfaceOrganicMatterDecompTypeDDML, '')
   id%IncorpFOMPool = add_registration(respondToEventReg, 'IncorpFOMPool', FOMPoolTypeDDML, '')
   id%new_profile = add_registration(respondToEventReg, 'new_profile', newprofileTypeDDML, '')
   id%NitrogenChanged = add_registration(respondToEventReg, 'NitrogenChanged', NitrogenChangedTypeDDML, '')

   ! variables we get from other modules.
   dummy = add_registration_with_units(getVariableReg, 'amp', floatTypeDDML, 'oC')
   dummy = add_registration_with_units(getVariableReg, 'tav', floatTypeDDML, 'oC')
   dummy = add_registration_with_units(getVariableReg, 'sw_dep', floatarrayTypeDDML, 'mm')
   dummy = add_registration_with_units(getVariableReg, 'dlayer', floatarrayTypeDDML, 'mm')
   dummy = add_registration_with_units(getVariableReg, 'soil_loss', floatarrayTypeDDML, 't/ha')
   dummy = add_registration_with_units(getVariableReg, 'ph', floatarrayTypeDDML, '')
   dummy = add_registration_with_units(getVariableReg, 'sw_dep', floatarrayTypeDDML, 'mm')
   dummy = add_registration_with_units(getVariableReg, 'ave_soil_temp', floatarrayTypeDDML, 'oC')
   dummy = add_registration_with_units(getVariableReg, 'latitude', floatTypeDDML, 'deg')
   dummy = add_registration_with_units(getVariableReg, 'salb', floatTypeDDML, '')

   ! variables we own and make available to other modules.
   dummy = add_registration_with_units(respondToGetSetReg, 'no3', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'dlt_no3_net', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetSetReg, 'no3ppm', floatarrayTypeDDML, 'mg/kg')
   dummy = add_registration_with_units(respondToGetReg, 'no3_min', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetSetReg, 'nh4', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'dlt_nh4_net', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetSetReg, 'nh4ppm', floatarrayTypeDDML, 'mg/kg')
   dummy = add_registration_with_units(respondToGetReg, 'nh4_min', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetSetReg, 'urea', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'dlt_urea_hydrol', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'excess_nh4', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'fom_n', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'fom_n_pool1', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'fom_n_pool2', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'fom_n_pool3', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'hum_n', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'biom_n', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'fom_c', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'num_fom_types', intTypeDDML, '')
   dummy = add_registration_with_units(respondToGetReg, 'fr_carb', floatTypeDDML, '')
   dummy = add_registration_with_units(respondToGetReg, 'fr_cell', floatTypeDDML, '')
   dummy = add_registration_with_units(respondToGetReg, 'fr_lign', floatTypeDDML, '')
   dummy = add_registration_with_units(respondToGetReg, 'fom_c_pool1', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'fom_c_pool2', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'fom_c_pool3', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'hum_c', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'biom_c', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'carbon_tot', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'oc%', floatarrayTypeDDML, '%')
   dummy = add_registration_with_units(respondToGetReg, 'nh4_transform_net', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'no3_transform_net', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'dlt_res_nh4_min', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'dlt_fom_n_min', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'dlt_biom_n_min', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'dlt_hum_n_min', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'dlt_res_no3_min', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'dlt_no3_dnit', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'nit_tot', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'dlt_n_min', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'dlt_n_min_res', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'dlt_n_min_tot', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'dnit', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'dlt_c_loss_in_sed', floatTypeDDML, 'kg')
   dummy = add_registration_with_units(respondToGetReg, 'dlt_n_loss_in_sed', floatTypeDDML, 'kg')
   dummy = add_registration_with_units(respondToGetReg, 'st', floatarrayTypeDDML, 'oC')
   dummy = add_registration_with_units(respondToGetSetReg, 'org_c_pool', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetSetReg, 'org_n', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'dlt_fom_c_hum', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'dlt_fom_c_biom', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'dlt_fom_c_atm', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'dlt_hum_c_biom', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'dlt_hum_c_atm', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'dlt_biom_c_hum', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'dlt_biom_c_atm', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'dlt_res_c_biom', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'dlt_res_c_hum', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'dlt_res_c_atm', floatTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'dlt_fom_c_pool1', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'dlt_fom_c_pool2', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'dlt_fom_c_pool3', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'soilp_dlt_res_c_atm', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'soilp_dlt_res_c_hum', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'soilp_dlt_res_c_biom', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToGetReg, 'soilp_dlt_org_p', floatarrayTypeDDML, 'kg/ha')

   ! settable variables
   dummy = add_registration_with_units(respondToSetReg, 'dlt_no3', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToSetReg, 'dlt_nh4', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToSetReg, 'dlt_no3ppm', floatarrayTypeDDML, 'mg/kg')
   dummy = add_registration_with_units(respondToSetReg, 'dlt_nh4ppm', floatarrayTypeDDML, 'mg/kg')
   dummy = add_registration_with_units(respondToSetReg, 'dlt_urea', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToSetReg, 'n_reduction', stringTypeDDML, '')
   dummy = add_registration_with_units(respondToSetReg, 'dlt_org_n', floatarrayTypeDDML, 'kg/ha')
   dummy = add_registration_with_units(respondToSetReg, 'dlt_org_c_pool', floatarrayTypeDDML, 'kg/ha')

   call soiln2_zero_all_globals ()

end subroutine doInit1

! ====================================================================
! This routine is the event handler for all events
! ====================================================================
subroutine respondToEvent(fromID, eventID, variant)
   use Soiln2Module
   Use infrastructure
   implicit none
   ml_external respondToEvent

   integer, intent(in) :: fromID
   integer, intent(in) :: eventID
   integer, intent(in out) :: variant

   if (eventID .eq. id%tick) then
      call Soiln2_ONtick(variant)
   else if (eventID .eq. id%newmet) then
      call Soiln2_ONnewmet(variant)

   elseif (eventID .eq.id%PotentialResidueDecompositionCalculated) then
      if (g%pond_active.eq.'no') then
           ! only do this if there is no pond.  If there is a pond, POND module will do this decomposition
           call soiln2_ONPotentialResidueDecompositionCalculated(variant)
      endif

   elseif (eventID .eq.id%IncorpFOMPool) then
      call soiln2_OnIncorpFOMPool(variant)

   elseif (eventID .eq.id%IncorpFOM) then
      call OnIncorpFOM(variant)

   elseif (eventID .eq.id%new_profile) then
      call soiln2_ONnew_profile(variant)

   else if (eventID .eq. id%NitrogenChanged) then
      call OnNitrogenChanged(variant)
   endif
   return
end subroutine respondToEvent

