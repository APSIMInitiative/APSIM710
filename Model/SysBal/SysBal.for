      module SysBalModule
      use ComponentInterfaceModule
      use Registrations
! ====================================================================
!     sysbal constants
! ====================================================================

!   Short description:
!      sysbal module constants

!   Notes:
!      none

!   Attributes:
!      Version:         Any hardware/Fortran77
!      Extensions:      Long names <= 20 chars.
!                       Lowercase
!                       Underscore
!                       Inline comments

!   Changes:
!      201093 jngh programmed

! ----------------------- Declaration section ------------------------

!   Constant values
      integer    max_layer             ! Maximum number of layers in soil
      parameter (max_layer = 100)

      integer    max_modules                    ! maximum number of modules in at once
      parameter (max_modules = 20)

      integer    module_name_size             ! maximum length of module name
      parameter (module_name_size = 32)

      real       fraction_C_FOM             ! Fraction Cin FOM
      parameter (fraction_C_FOM = 0.4)

      integer    MaxStringSize           ! max length of the crop type.
      parameter (MaxStringSize = 100)

      integer    MaxArraySize                 ! maximum number of dry matter types
      parameter (MaxArraySize = 10)

      type SysBalGlobals
         sequence
         type(ExternalMassFlowType) :: massBalanceChange
         integer    sysbal_index(max_modules) ! index to sorted sysbal height ()
         real       height(max_modules)       ! sysbal height of modules (mm)
         integer    num_modules               ! number of modules ()

         real Nloss_system
         real Ngain_system
         real Ndlt_system
         real Ndlt_surface
         real Ndlt_crop
         real Ndlt_soil
         real Nerror_system
         real Ncum_error_system
         real Nstate_system_yest
         real Nstate_surface_yest
         real Nstate_crop_yest
         real Nstate_soil_yest
         real Ploss_system
         real Pgain_system
         real Pdlt_system
         real Pdlt_surface
         real Pdlt_crop
         real Pdlt_soil
         real Perror_system
         real Pcum_error_system
         real Pstate_system_yest
         real Pstate_surface_yest
         real Pstate_crop_yest
         real Pstate_soil_yest
         real Closs_system
         real Cgain_system
         real Cdlt_system
         real Cdlt_surface
         real Cdlt_crop
         real Cdlt_soil
         real Cerror_system
         real Ccum_error_system
         real Cstate_system_yest
         real Cstate_surface_yest
         real Cstate_crop_yest
         real Cstate_soil_yest
         real DMloss_system
         real DMgain_system
         real DMdlt_system
         real DMdlt_surface
         real DMdlt_crop
         real DMdlt_soil
         real DMerror_system
         real DMcum_error_system
         real DMstate_system_yest
         real DMstate_surface_yest
         real DMstate_crop_yest
         real DMstate_soil_yest
         real SWloss_system
         real SWgain_system
         real SWdlt_system
         real SWdlt_surface
         real SWdlt_crop
         real SWdlt_soil
         real SWerror_system
         real SWcum_error_system
         real SWstate_system_yest
         real SWstate_surface_yest
         real SWstate_crop_yest
         real SWstate_soil_yest

         real irrigation
         real       dm_removed          ! amount of residue added (kg/ha)
         real       N_removed        ! amount of residue N removed (kg/ha)
         real       P_removed       ! amount of residue N removed (kg/ha)
         real       dm_added          ! amount of residue added (kg/ha)
         real       N_added        ! amount of residue N removed (kg/ha)
         real       P_added       ! amount of residue N removed (kg/ha)

         type(ExternalMassFlowType) :: removedCrop
         type(ExternalMassFlowType) :: addedCrop

         type(ExternalMassFlowType) :: removedSoil
         type(ExternalMassFlowType) :: addedSoil

         type(ExternalMassFlowType) :: removedSurface
         type(ExternalMassFlowType) :: addedSurface

         logical    phosphorus_aware

      end type SysBalGlobals
! ====================================================================
      ! instance variables.
      common /InstancePointers/ ID,g,p,c
      save InstancePointers
      type (SysBalGlobals),pointer :: g
      type (IDsType),pointer :: id

      contains


!     ===========================================================
      subroutine sysbal_init ()
!     ===========================================================

      Use Infrastructure
      implicit none

!+  Purpose
!      Initialise sysbal module. Output mesage and get list from control file.

!+  Changes
!     201093 jngh specified and programmed
!     210395 jngh changed from unknown_section to a defined section
!     280999 sdb removed version reference


!+  Constant Values
      character  my_name*(*)           ! procedure name
      parameter (my_name  = 'sysbal_init')
!
      character  section_name*(*)      ! name of parameter section
      parameter (section_name = 'parameters')

!+  Local Variables
      integer    num_modules           ! number of module names in list
      character  line*200              ! message
      integer    i                     ! loop counter

!- Implementation Section ----------------------------------

      call push_routine (my_name)

            ! initialisation message

!      call Write_string (' Initialising')
!
!            ! now get  list from control file
!
!      call read_char_array_optional (section_name
!     :                   , 'intermodule', max_modules, '()'
!     :                   , g%intermodule_list, num_modules)
!
!      call bound_check_integer_var (num_modules, 0, max_modules
!     :                            , 'num_modules')
!
!         ! now report initial conditions
!
!      if (num_modules.gt.1) then
!         write (line, '(a)')  ' Module rotation for intermoduleping :'
!         call write_string (line)
!
!         write (line, '(100a)')  (g%intermodule_list(i), i=1, num_modules)
!         call write_string (line)
!
!      else
!         ! no swapping required
!         write (line,'(a)')
!     :             ' No module rotation for intermoduleping'
!         call write_string (line)
!      endif

      call pop_routine (my_name)
      return
      end subroutine





!     ===========================================================
      subroutine sysbal_zero_all_variables ()
!     ===========================================================

      Use Infrastructure
      implicit none

!+  Purpose
!     Set all variables in this module to zero.

!+  Changes
!      201093 jngh specified and programmed

!+  Constant Values
      character  my_name*(*)           ! procedure name
      parameter (my_name  = 'sysbal_zero_all_variables')

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      g%sysbal_index(:) = 0
      g%height(:)       = 0.0
      g%num_modules = 0

      g%Nloss_system         = 0.0
      g%Ngain_system         = 0.0
      g%Ndlt_system          = 0.0
      g%Ndlt_surface         = 0.0
      g%Ndlt_crop          = 0.0
      g%Ndlt_soil            = 0.0
      g%Nerror_system        = 0.0
      g%Ncum_error_system    = 0.0
      g%Nstate_system_yest   = 0.0
      g%Nstate_surface_yest  = 0.0
      g%Nstate_crop_yest   = 0.0
      g%Nstate_soil_yest     = 0.0
      g%Ploss_system         = 0.0
      g%Pgain_system         = 0.0
      g%Pdlt_system          = 0.0
      g%Pdlt_surface         = 0.0
      g%Pdlt_crop          = 0.0
      g%Pdlt_soil            = 0.0
      g%Perror_system        = 0.0
      g%Pcum_error_system    = 0.0
      g%Pstate_system_yest   = 0.0
      g%Pstate_surface_yest  = 0.0
      g%Pstate_crop_yest   = 0.0
      g%Pstate_soil_yest     = 0.0
      g%Closs_system         = 0.0
      g%Cgain_system         = 0.0
      g%Cdlt_system          = 0.0
      g%Cdlt_surface         = 0.0
      g%Cdlt_crop          = 0.0
      g%Cdlt_soil            = 0.0
      g%Cerror_system        = 0.0
      g%Ccum_error_system    = 0.0
      g%Cstate_system_yest   = 0.0
      g%Cstate_surface_yest  = 0.0
      g%Cstate_crop_yest   = 0.0
      g%Cstate_soil_yest     = 0.0
      g%DMloss_system        = 0.0
      g%DMgain_system        = 0.0
      g%DMdlt_system         = 0.0
      g%DMdlt_surface        = 0.0
      g%DMdlt_crop         = 0.0
      g%DMdlt_soil           = 0.0
      g%DMerror_system       = 0.0
      g%DMcum_error_system   = 0.0
      g%DMstate_system_yest  = 0.0
      g%DMstate_surface_yest = 0.0
      g%DMstate_crop_yest  = 0.0
      g%DMstate_soil_yest    = 0.0
      g%SWloss_system        = 0.0
      g%SWgain_system        = 0.0
      g%SWdlt_system         = 0.0
      g%SWdlt_surface        = 0.0
      g%SWdlt_crop         = 0.0
      g%SWdlt_soil           = 0.0
      g%SWerror_system       = 0.0
      g%SWcum_error_system   = 0.0
      g%SWstate_system_yest  = 0.0
      g%SWstate_surface_yest = 0.0
      g%SWstate_crop_yest  = 0.0
      g%SWstate_soil_yest    = 0.0

      g%dm_removed           = 0.0
      g%N_removed            = 0.0
      g%P_removed            = 0.0

      g%dm_added           = 0.0
      g%N_added            = 0.0
      g%P_added            = 0.0


      g%irrigation           = 0.0
      g%phosphorus_aware     = .false.

      g%massBalanceChange%PoolClass = ' '
      g%massBalanceChange%FlowType = ' '
      g%massBalanceChange%C = 0.0
      g%massBalanceChange%N = 0.0
      g%massBalanceChange%P = 0.0
      g%massBalanceChange%DM = 0.0
      g%massBalanceChange%SW = 0.0

      g%removedCrop%PoolClass = ' '
      g%removedCrop%FlowType = ' '
      g%removedCrop%C = 0.0
      g%removedCrop%N = 0.0
      g%removedCrop%P = 0.0
      g%removedCrop%DM = 0.0
      g%removedCrop%SW = 0.0

      g%addedCrop%PoolClass = ' '
      g%addedCrop%FlowType = ' '
      g%addedCrop%C = 0.0
      g%addedCrop%N = 0.0
      g%addedCrop%P = 0.0
      g%addedCrop%DM = 0.0
      g%addedCrop%SW = 0.0

      g%removedSurface%PoolClass = ' '
      g%removedSurface%FlowType = ' '
      g%removedSurface%C = 0.0
      g%removedSurface%N = 0.0
      g%removedSurface%P = 0.0
      g%removedSurface%DM = 0.0
      g%removedSurface%SW = 0.0

      g%addedSurface%PoolClass = ' '
      g%addedSurface%FlowType = ' '
      g%addedSurface%C = 0.0
      g%addedSurface%N = 0.0
      g%addedSurface%P = 0.0
      g%addedSurface%DM = 0.0
      g%addedSurface%SW = 0.0

      g%removedSoil%PoolClass = ' '
      g%removedSoil%FlowType = ' '
      g%removedSoil%C = 0.0
      g%removedSoil%N = 0.0
      g%removedSoil%P = 0.0
      g%removedSoil%DM = 0.0
      g%removedSoil%SW = 0.0

      g%addedSoil%PoolClass = ' '
      g%addedSoil%FlowType = ' '
      g%addedSoil%C = 0.0
      g%addedSoil%N = 0.0
      g%addedSoil%P = 0.0
      g%addedSoil%DM = 0.0
      g%addedSoil%SW = 0.0

      call pop_routine (my_name)
      return
      end subroutine

!     ===========================================================
      subroutine sysbal_zero_variables ()
!     ===========================================================

      Use Infrastructure
      implicit none

!+  Purpose
!     Set all variables in this module to zero.

!+  Changes
!      201093 jngh specified and programmed

!+  Constant Values
      character  my_name*(*)           ! procedure name
      parameter (my_name  = 'sysbal_zero_variables')

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      call fill_integer_array (g%sysbal_index, 0, max_modules)
      call fill_real_array (g%height, 0.0, max_modules)

      g%num_modules = 0

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine sysbal_get_N_variables (
     :                                    state_crop
     :                                  , state_soil
     :                                  , state_surface
     :                                  , loss_soil
     :                                  , loss_crop
     :                                  , loss_surface
     :                                  , loss_discontinuity
     :                                  , gain_soil
     :                                  , gain_crop
     :                                  , gain_surface
     :                                  , gain_discontinuity
     :                                  )
!     ===========================================================

      Use Infrastructure
      implicit none

      real       state_crop
      real       state_soil
      real       state_surface
      real       loss_soil
      real       loss_crop
      real       loss_surface
      real       loss_discontinuity
      real       gain_soil
      real       gain_crop
      real       gain_surface
      real       gain_discontinuity

!+  Purpose
!      Get the values of variables from other modules

!+  Changes
!      201093 jngh specified and programmed
!      261196 jngh tested incoming cover for 1. Set log to 100.0 if it is.

!+  Constant Values
!
      character  my_name*(*)           ! procedure name
      parameter (my_name='sysbal_get_N_variables')

      character  mm*10
      parameter (mm = '(mm)')

      character  gm2*10
      parameter (gm2 = '(g/m2)')

      character  kgha*10
      parameter (kgha = '(kg/ha)')

      character  n_green*30
      parameter (n_green = 'n_green')

      character  GreenN*30
      parameter (GreenN = 'greenn')

      character  n_senesced*30
      parameter (n_senesced = 'n_senesced')

      character  SenescedN*30
      parameter (SenescedN = 'senescedn')

      character  n_dead*30
      parameter (n_dead = 'n_dead')

      character  nit_tot*30
      parameter (nit_tot = 'nit_tot()')

      character  surfaceom_n*30
      parameter (surfaceom_n = 'surfaceom_n')

      character  surfaceom_no3*30
      parameter (surfaceom_no3 = 'surfaceom_no3')

      character  surfaceom_nh4*30
      parameter (surfaceom_nh4 = 'surfaceom_nh4')

      character  dlt_no3_dnit*30
      parameter (dlt_no3_dnit = 'dlt_no3_dnit()')

      character  leach_NO3*30
      parameter (leach_NO3 = 'leach_no3')

      character  leach_NH4*30
      parameter (leach_NH4 = 'leach_nh4')

      character  dlt_n_fixed*30
      parameter (dlt_n_fixed = 'dlt_n_fixed')

      character  dlayer*30
      parameter (dlayer = 'dlayer')

!+  Local Variables
      character*200   message          !

!- Implementation Section ----------------------------------

      call push_routine (my_name)

  ! define system states
  !---------------

      state_crop =  sysbal_get_variable('TotalN', gm2) * gm2kg/sm2ha

      if (state_crop.eq.0) then
         state_crop =
     :      sysbal_get_variable(n_green, gm2) * gm2kg/sm2ha
     :    + sysbal_get_variable(GreenN, gm2) * gm2kg/sm2ha
     :    + sysbal_get_variable(n_senesced, gm2) * gm2kg/sm2ha
     :    + sysbal_get_variable(SenescedN, gm2) * gm2kg/sm2ha
     :    + sysbal_get_variable(n_dead, gm2) * gm2kg/sm2ha
      endif
     
      state_soil = sysbal_get_variable(nit_tot, kgha)
      state_surface = sysbal_get_variable(surfaceom_n, kgha)
     :    + sysbal_get_variable(surfaceom_no3, kgha)
     :    + sysbal_get_variable(surfaceom_nh4, kgha)

  ! define system losses
  !--------------
      loss_soil = sysbal_get_variable(dlt_no3_dnit, kgha)
     :          + sysbal_get_variable(leach_NO3, kgha)
     :          + sysbal_get_variable(leach_NH4, kgha)
     :          + g%removedSoil%N

      loss_crop    = g%N_removed + g%removedCrop%N
      loss_surface = g%removedSurface%N

  ! define system gains
  !-------------
      gain_soil    = g%addedSoil%N
      gain_crop    = sysbal_get_variable(dlt_n_fixed, gm2) * gm2kg/sm2ha
     :             + g%addedCrop%N
      gain_surface = g%N_added + g%addedSurface%N

      call pop_routine (my_name)
      return
      end subroutine

!     ===========================================================
      subroutine sysbal_get_P_variables (
     :                                    state_crop
     :                                  , state_soil
     :                                  , state_surface
     :                                  , loss_soil
     :                                  , loss_crop
     :                                  , loss_surface
     :                                  , loss_discontinuity
     :                                  , gain_soil
     :                                  , gain_crop
     :                                  , gain_surface
     :                                  , gain_discontinuity
     :                                  )
!     ===========================================================

      Use Infrastructure
      implicit none

      real       state_crop
      real       state_soil
      real       state_surface
      real       loss_soil
      real       loss_crop
      real       loss_surface
      real       loss_discontinuity
      real       gain_soil
      real       gain_crop
      real       gain_surface
      real       gain_discontinuity

!+  Purpose
!      Get the values of variables from other crops

!+  Changes
!      201093 jngh specified and programmed
!      261196 jngh tested incoming cover for 1. Set log to 100.0 if it is.

!+  Constant Values
!
      character  my_name*(*)           ! procedure name
      parameter (my_name='sysbal_get_P_variables')

      character  gm2*10
      parameter (gm2 = '(g/m2)')

      character  kgha*10
      parameter (kgha = '(kg/ha)')

      character  p_green*30
      parameter (p_green = 'p_green')

      character  GreenP*30
      parameter (GreenP = 'greenp')

      character  p_senesced*30
      parameter (p_senesced = 'p_senesced')

      character  SenescedP*30
      parameter (SenescedP = 'senescedp')

      character  p_dead*30
      parameter (p_dead = 'p_dead')

      character  fom_p*30
      parameter (fom_p = 'fom_p()')

      character  hum_p*30
      parameter (hum_p = 'hum_p()')

      character  biom_p*30
      parameter (biom_p = 'biom_p()')

      character  rock_p*30
      parameter (rock_p = 'rock_p()')

      character  banded_p*30
      parameter (banded_p = 'banded_p()')

      character  unavail_p*30
      parameter (unavail_p = 'unavail_p()')

      character  labile_p*30
      parameter (labile_p = 'labile_p()')

      character  surfaceom_p*30
      parameter (surfaceom_p = 'surfaceom_p')

      character  surfaceom_labile_p*30
      parameter (surfaceom_labile_p = 'surfaceom_labile_p')

!+  Local Variables
      character*200   message          !

!- Implementation Section ----------------------------------

      call push_routine (my_name)

  ! define system states
  !---------------

      state_crop =  sysbal_get_variable('TotalP', gm2) * gm2kg/sm2ha

      if (state_crop.eq.0) then

         state_crop =
     :      sysbal_get_variable(p_green, gm2) * gm2kg/sm2ha
     :    + sysbal_get_variable(GreenP, gm2) * gm2kg/sm2ha
     :    + sysbal_get_variable(p_senesced, gm2) * gm2kg/sm2ha
     :    + sysbal_get_variable(SenescedP, gm2) * gm2kg/sm2ha
     :    + sysbal_get_variable(p_dead, gm2) * gm2kg/sm2ha

      endif

      state_soil = sysbal_get_variable(fom_p, kgha)
     :           + sysbal_get_variable(hum_p, kgha)
     :           + sysbal_get_variable(biom_p, kgha)
     :           + sysbal_get_variable(rock_p, kgha)
     :           + sysbal_get_variable(banded_p, kgha)
     :           + sysbal_get_variable(unavail_p, kgha)
     :           + sysbal_get_variable(labile_p, kgha)
      state_surface = sysbal_get_variable(surfaceom_p, kgha)
     :           + sysbal_get_variable(surfaceom_labile_p, kgha)

  ! define system losses
  !--------------
      loss_soil = g%removedSoil%P
      loss_crop    = g%P_removed + g%removedCrop%P
      loss_surface = g%removedSurface%P

  ! define system gains
  !-------------
      gain_soil    = g%addedSoil%P
      gain_crop    = g%addedCrop%P
      gain_surface = g%P_added + g%addedSurface%P

      call pop_routine (my_name)
      return
      end subroutine

!     ===========================================================
      subroutine sysbal_get_C_variables (
     :                                    state_crop
     :                                  , state_soil
     :                                  , state_surface
     :                                  , loss_soil
     :                                  , loss_crop
     :                                  , loss_surface
     :                                  , loss_discontinuity
     :                                  , gain_soil
     :                                  , gain_crop
     :                                  , gain_surface
     :                                  , gain_discontinuity
     :                                  )
!     ===========================================================

      Use Infrastructure
      implicit none

      real       state_crop
      real       state_soil
      real       state_surface
      real       loss_soil
      real       loss_crop
      real       loss_surface
      real       loss_discontinuity
      real       gain_soil
      real       gain_crop
      real       gain_surface
      real       gain_discontinuity

!+  Purpose
!      Get the values of variables from other modules

!+  Changes
!      201093 jngh specified and programmed
!      261196 jngh tested incoming cover for 1. Set log to 100.0 if it is.

!+  Constant Values
!
      character  my_name*(*)           ! procedure name
      parameter (my_name='sysbal_get_C_variables')

      character  gm2*10
      parameter (gm2 = '(g/m2)')

      character  kgha*10
      parameter (kgha = '(kg/ha)')

      character  dm_green*30
      parameter (dm_green = 'dm_green')

      character  GreenWt*30
      parameter (GreenWt = 'greenwt')

      character  dm_senesced*30
      parameter (dm_senesced = 'dm_senesced')

      character  SenescedWt*30
      parameter (SenescedWt = 'senescedwt')

      character  dm_dead*30
      parameter (dm_dead = 'dm_dead')

      character  carbon_tot*30
      parameter (carbon_tot = 'carbon_tot()')

      character  surfaceom_c*30
      parameter (surfaceom_c = 'surfaceom_c')

      character  dlt_fom_c_atm*30
      parameter (dlt_fom_c_atm = 'dlt_fom_c_atm()')

      character  dlt_hum_c_atm*30
      parameter (dlt_hum_c_atm = 'dlt_hum_c_atm()')

      character  dlt_biom_c_atm*30
      parameter (dlt_biom_c_atm = 'dlt_biom_c_atm()')

      character  dlt_dm_oil_conv*30
      parameter (dlt_dm_oil_conv = 'dlt_dm_oil_conv')

      character  dlt_dm_oil_conv_retrans*30
      parameter (dlt_dm_oil_conv_retrans = 'dlt_dm_oil_conv_retrans')

      character  dlt_res_c_atm*30
      parameter (dlt_res_c_atm = 'dlt_res_c_atm')

      character  dlt_dm_green*30
      parameter (dlt_dm_green = 'dlt_dm_green')

      character  GrowthWt*30
      parameter (GrowthWt = 'growthwt')


!+  Local Variables
      character*200   message          !

!- Implementation Section ----------------------------------

      call push_routine (my_name)

  ! define system states
  !---------------

      state_crop =  sysbal_get_variable('TotalC', gm2) * gm2kg/sm2ha

      if (state_crop.eq.0) then

         state_crop=sysbal_get_variable(dm_green, gm2)*gm2kg/sm2ha*0.4
     :       + sysbal_get_variable(GreenWt, gm2) * gm2kg/sm2ha * 0.4
     :       + sysbal_get_variable(dm_senesced, gm2) * gm2kg/sm2ha *0.4
     :       + sysbal_get_variable(SenescedWt, gm2) * gm2kg/sm2ha *0.4
     :       + sysbal_get_variable(dm_dead, gm2) * gm2kg/sm2ha *0.4

      endif  

      state_soil = sysbal_get_variable(carbon_tot, kgha)
      state_surface = sysbal_get_variable(surfaceom_c, kgha)

  ! define system losses
  !--------------

      loss_soil = sysbal_get_variable(dlt_fom_c_atm, kgha)
     :          + sysbal_get_variable(dlt_hum_c_atm, kgha)
     :          + sysbal_get_variable(dlt_biom_c_atm, kgha)
     :          + g%removedSoil%C

      loss_crop    =
!     :   sysbal_get_variable(dlt_dm_oil_conv, gm2) * gm2kg/sm2ha * 0.4     ! conversion is not included in the dlt_dm_green
     : + sysbal_get_variable(dlt_dm_oil_conv_retrans, gm2)
     :   * gm2kg/sm2ha * 0.4
     :   + g%dm_removed * 0.4
     :   + g%removedCrop%DM * 0.4
      loss_surface = sysbal_get_variable(dlt_res_c_atm, kgha)
     :             + g%removedSurface%C

  ! define system gains
  !-------------

      gain_soil = g%addedSoil%C
      gain_crop = sysbal_get_variable(dlt_dm_green, gm2)
     :          * gm2kg/sm2ha * 0.4
     :          + sysbal_get_variable(GrowthWt, gm2)
     :          * gm2kg/sm2ha * 0.4
     :          + g%addedCrop%DM * 0.4
      gain_surface = g%dm_added * 0.4
     :             + g%addedSurface%C

      call pop_routine (my_name)
      return
      end subroutine

!     ===========================================================
      subroutine sysbal_get_DM_variables (
     :                                    state_crop
     :                                  , state_soil
     :                                  , state_surface
     :                                  , loss_soil
     :                                  , loss_crop
     :                                  , loss_surface
     :                                  , loss_discontinuity
     :                                  , gain_soil
     :                                  , gain_crop
     :                                  , gain_surface
     :                                  , gain_discontinuity
     :                                  )
!     ===========================================================

      Use Infrastructure
      implicit none

      real       state_crop
      real       state_soil
      real       state_surface
      real       loss_soil
      real       loss_crop
      real       loss_surface
      real       loss_discontinuity
      real       gain_soil
      real       gain_crop
      real       gain_surface
      real       gain_discontinuity

!+  Purpose
!      Get the values of variables from other modules

!+  Changes
!      201093 jngh specified and programmed
!      261196 jngh tested incoming cover for 1. Set log to 100.0 if it is.

!+  Constant Values
!
      character  my_name*(*)           ! procedure name
      parameter (my_name='sysbal_get_DM_variables')

      character  gm2*10
      parameter (gm2 = '(g/m2)')

      character  kgha*10
      parameter (kgha = '(kg/ha)')

      character  dm_green*30
      parameter (dm_green = 'dm_green')

      character  GreenWt*30
      parameter (GreenWt = 'greenwt')

      character  dm_senesced*30
      parameter (dm_senesced = 'dm_senesced')

      character  SenescedWt*30
      parameter (SenescedWt = 'senescedwt')

      character  dm_dead*30
      parameter (dm_dead = 'dm_dead')

      character  carbon_tot*30
      parameter (carbon_tot = 'carbon_tot()')

      character  surfaceom_wt*30
      parameter (surfaceom_wt = 'surfaceom_wt')

      character  dlt_fom_c_atm*30
      parameter (dlt_fom_c_atm = 'dlt_fom_c_atm()')

      character  dlt_hum_c_atm*30
      parameter (dlt_hum_c_atm = 'dlt_hum_c_atm()')

      character  dlt_biom_c_atm*30
      parameter (dlt_biom_c_atm = 'dlt_biom_c_atm()')

      character  dlt_dm_oil_conv*30
      parameter (dlt_dm_oil_conv = 'dlt_dm_oil_conv')

      character  dlt_dm_oil_conv_retrans*30
      parameter (dlt_dm_oil_conv_retrans = 'dlt_dm_oil_conv_retrans')

      character  dlt_res_c_atm*30
      parameter (dlt_res_c_atm = 'dlt_res_c_atm')

      character  dlt_dm_green*30
      parameter (dlt_dm_green = 'dlt_dm_green')

      character  GrowthWt*30
      parameter (GrowthWt = 'growthwt')

!+  Local Variables
      character*200   message          !

!- Implementation Section ----------------------------------

      call push_routine (my_name)

  ! define system states
  !---------------

      state_crop = sysbal_get_variable(dm_green, gm2) * gm2kg/sm2ha
     :           + sysbal_get_variable(GreenWt, gm2) * gm2kg/sm2ha
     :           + sysbal_get_variable(dm_senesced, gm2) * gm2kg/sm2ha
     :           + sysbal_get_variable(SenescedWt, gm2) * gm2kg/sm2ha
     :           + sysbal_get_variable(dm_dead, gm2) * gm2kg/sm2ha

     

      state_soil = sysbal_get_variable(carbon_tot, kgha) / 0.4
      state_surface = sysbal_get_variable(surfaceom_wt, kgha)

  ! define system losses
  !--------------
      loss_soil = sysbal_get_variable(dlt_fom_c_atm, kgha) / 0.4
     :          + sysbal_get_variable(dlt_hum_c_atm, kgha) / 0.4
     :          + sysbal_get_variable(dlt_biom_c_atm, kgha) / 0.4
     :          + g%removedSoil%C / 0.4
      loss_crop    =
!     :   sysbal_get_variable(dlt_dm_oil_conv, gm2) * gm2kg/sm2ha          ! conversion is not included in the dlt_dm_green
     : + sysbal_get_variable(dlt_dm_oil_conv_retrans, gm2) * gm2kg/sm2ha
     :   + g%dm_removed
     :   + g%removedCrop%DM
      loss_surface = sysbal_get_variable(dlt_res_c_atm, kgha) / 0.4
     :             + g%removedSurface%DM

  ! define system gains
  !-------------
      gain_soil = g%addedSoil%C / 0.4
      gain_crop = sysbal_get_variable(dlt_dm_green, gm2) * gm2kg/sm2ha
     :          + sysbal_get_variable(GrowthWt, gm2) * gm2kg/sm2ha
     :          + g%addedCrop%DM
      gain_surface = g%dm_added + g%addedSurface%DM

      call pop_routine (my_name)
      return
      end subroutine

!     ===========================================================
      subroutine sysbal_get_SW_variables (
     :                                    state_crop
     :                                  , state_soil
     :                                  , state_surface
     :                                  , loss_soil
     :                                  , loss_crop
     :                                  , loss_surface
     :                                  , loss_discontinuity
     :                                  , gain_soil
     :                                  , gain_crop
     :                                  , gain_surface
     :                                  , gain_discontinuity
     :                                  )
!     ===========================================================

      Use Infrastructure
      implicit none

      real       state_crop
      real       state_soil
      real       state_surface
      real       loss_soil
      real       loss_crop
      real       loss_surface
      real       loss_discontinuity
      real       gain_soil
      real       gain_crop
      real       gain_surface
      real       gain_discontinuity

!+  Purpose
!      Get the values of variables from other modules

!+  Changes
!      201093 jngh specified and programmed
!      261196 jngh tested incoming cover for 1. Set log to 100.0 if it is.

!+  Constant Values
!
      character  my_name*(*)           ! procedure name
      parameter (my_name='sysbal_get_SW_variables')

      character  mm*10
      parameter (mm = '(mm)')

      character  sw_dep*30
      parameter (sw_dep = 'sw_dep()')

      character  pond*30
      parameter (pond = 'pond')

      character  es*30
      parameter (es = 'es')

      character  drain*30
      parameter (drain = 'drain')

      character  ep*30
      parameter (ep = 'ep')

      character  runoff*30
      parameter (runoff = 'runoff')

      character  rain*30
      parameter (rain = 'rain')

!+  Local Variables
      character*200   message          !

!- Implementation Section ----------------------------------

      call push_routine (my_name)

  ! define system states
  !---------------

      state_crop = 0.0
      state_soil = sysbal_get_variable(sw_dep, mm)
      state_surface = sysbal_get_variable(pond, mm)

  ! define system losses
  !--------------
      loss_soil = sysbal_get_variable(es, mm)
     :          + sysbal_get_variable(drain, mm)
     :          + g%removedSoil%SW
      loss_crop    = sysbal_get_variable(ep, mm)
     :             + g%removedCrop%SW
      loss_surface = sysbal_get_variable(runoff, mm)
     :             + g%removedSurface%SW

  ! define system gains
  !-------------
      gain_soil    = g%addedSoil%SW
      gain_crop    = g%addedCrop%SW
      gain_surface = sysbal_get_variable(rain, mm)
     :             + g%irrigation + g%addedSurface%SW

      call pop_routine (my_name)
      return
      end subroutine


!     ===========================================================
      real function sysbal_get_variable (var_name, units)
!     ===========================================================

      Use Infrastructure
      implicit none

      character var_name*(*)
      character units*(*)

!+  Purpose
!      Get the values of variables from other modules

!+  Changes
!      201093 jngh specified and programmed
!      261196 jngh tested incoming cover for 1. Set log to 100.0 if it is.

!+  Constant Values
      character  my_name*(*)           ! procedure name
      parameter (my_name='sysbal_get_variable')

      integer maxvals
      parameter (maxvals=200)

!+  Local Variables
      integer    module                  ! index for modules
      real       tempsum               !
      integer    numvals               ! number of values in string
      integer    owner_module          ! owner module of variable
      real       value
      character  name*30
      character  unit*30
      character  string*400            ! output string
      real        values(maxvals)
      integer    i
      character * 100 str

!- Implementation Section ----------------------------------

      call push_routine (my_name)


      module = 0
      value = 0.0
      name = var_name
      unit = units
1000  continue
         call get_real_arrays (module+1, name, maxvals, unit
     :                              , values, numvals
     :                              , -1e-4, 1e6)

         if (numvals.ne.0) then
            if (module+1.le.max_modules) then
               module = module + 1

               tempsum = 0.0
               do i = 1, numvals
                  tempsum = tempsum + values(i)
               enddo

               value = value + tempsum
               goto 1000
            else
               call fatal_error (err_user
     :            , 'Too many modules with '//trim(var_name))
            endif
         else
         endif

!         if (module .eq. 0) then
!         write (string, *)
!     :                   'No values returned for '//trim(var_name)
!         call Write_string (string)
!
!         else
!         endif

      sysbal_get_variable = value

      call pop_routine (my_name)
      return
      end function

!     ===========================================================
      integer function sysbal_get_last_index (var_name, units)
!     ===========================================================

      Use Infrastructure
      implicit none

      character var_name*(*)
      character units*(*)

!+  Purpose
!      Get the last index of an array

!+  Changes
!      201093 jngh specified and programmed
!      261196 jngh tested incoming cover for 1. Set log to 100.0 if it is.

!+  Constant Values
      character  my_name*(*)           ! procedure name
      parameter (my_name='sysbal_get_variable')

!+  Local Variables
      integer    module                  ! index for modules
      real       temp                  !
      integer    numvals               ! number of values in string
      integer    owner_module          ! owner module of variable
      real       array(max_layer)

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      array(:) = 0.0
      call get_real_array (unknown_module, var_name, max_layer
     :                                    , units
     :                                    , array, numvals
     :                                    , 0.0, 1e6)


      sysbal_get_last_index = count_of_real_vals (array, max_layer)

      call pop_routine (my_name)
      return
      end function



!     ===========================================================
      subroutine sysbal_send_my_variable (Variable_name)
!     ===========================================================

      Use Infrastructure
      implicit none

!+  Sub-Program Arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for

!+  Purpose
!      Return the value of one of our variables to caller

!+  Changes
!      201093 jngh specified and programmed
!      011195 jngh  added call to message_unused
!      010896 jngh changed method of getting module name for gets
!      120996 jngh removed print statement
!      021199 jngh added export of cover_tot_all and cover_height_all arrays

!+  Calls
!      integer    sysbal_module_number    ! function

!+  Constant Values
      character  my_name*(*)           ! procedure name
      parameter (my_name='sysbal_send_my_variable')
!+  Local Variables
      real       cover                 ! temporary cover variable
      integer    module                ! module counter
      character  module_string*(max_module_name_size) ! module name
      real       cover_tot_all(max_modules)   ! total cover of each module (0-1)
      real       cover_green_all(max_modules) ! green cover of each module (0-1)
      integer    moduleID
      integer    numvals
      logical    found

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (variable_name.eq.'n_loss_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Nloss_system)

      else if (variable_name.eq.'n_gain_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Ngain_system)

      else if (variable_name.eq.'n_dlt_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Ndlt_system)

      else if (variable_name.eq.'n_dlt_surface') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :
     :                            , g%Ndlt_surface)

      else if (variable_name.eq.'n_dlt_crop') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Ndlt_crop)

      else if (variable_name.eq.'n_dlt_soil') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Ndlt_soil)

      else if (variable_name.eq.'n_error_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Nerror_system)

      else if (variable_name.eq.'n_cum_error_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Ncum_error_system)

      else if (variable_name.eq.'n_state_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Nstate_system_yest)

      else if (variable_name.eq.'n_state_surface') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Nstate_surface_yest)

      else if (variable_name.eq.'n_state_crop') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Nstate_crop_yest)

      else if (variable_name.eq.'n_state_soil') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Nstate_soil_yest)

      else if (variable_name.eq.'p_loss_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Ploss_system)

      else if (variable_name.eq.'p_gain_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Pgain_system)

      else if (variable_name.eq.'p_dlt_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Pdlt_system)

      else if (variable_name.eq.'p_dlt_surface') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Pdlt_surface)

      else if (variable_name.eq.'p_dlt_crop') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Pdlt_crop)

      else if (variable_name.eq.'p_dlt_soil') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Pdlt_soil)

      else if (variable_name.eq.'p_error_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Perror_system)

      else if (variable_name.eq.'p_cum_error_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Pcum_error_system)

      else if (variable_name.eq.'p_state_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Pstate_system_yest)

      else if (variable_name.eq.'p_state_surface') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Pstate_surface_yest)

      else if (variable_name.eq.'p_state_crop') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Pstate_crop_yest)

      else if (variable_name.eq.'p_state_soil') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Pstate_soil_yest)

      else if (variable_name.eq.'c_loss_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Closs_system)

      else if (variable_name.eq.'c_gain_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Cgain_system)

      else if (variable_name.eq.'c_dlt_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Cdlt_system)

      else if (variable_name.eq.'c_dlt_surface') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Cdlt_surface)

      else if (variable_name.eq.'c_dlt_crop') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Cdlt_crop)

      else if (variable_name.eq.'c_dlt_soil') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Cdlt_soil)

      else if (variable_name.eq.'c_error_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Cerror_system)

      else if (variable_name.eq.'c_cum_error_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Ccum_error_system)

      else if (variable_name.eq.'c_state_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Cstate_system_yest)

      else if (variable_name.eq.'c_state_surface') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Cstate_surface_yest)

      else if (variable_name.eq.'c_state_crop') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Cstate_crop_yest)

      else if (variable_name.eq.'c_state_soil') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%Cstate_soil_yest)

      else if (variable_name.eq.'dm_loss_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%DMloss_system)

      else if (variable_name.eq.'dm_gain_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%DMgain_system)

      else if (variable_name.eq.'dm_dlt_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%DMdlt_system)

      else if (variable_name.eq.'dm_dlt_surface') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%DMdlt_surface)

      else if (variable_name.eq.'dm_dlt_crop') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%DMdlt_crop)

      else if (variable_name.eq.'dm_dlt_soil') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%DMdlt_soil)

      else if (variable_name.eq.'dm_error_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%DMerror_system)

      else if (variable_name.eq.'dm_cum_error_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%DMcum_error_system)

      else if (variable_name.eq.'dm_state_system') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%DMstate_system_yest)

      else if (variable_name.eq.'dm_state_surface') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%DMstate_surface_yest)

      else if (variable_name.eq.'dm_state_crop') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%DMstate_crop_yest)

      else if (variable_name.eq.'dm_state_soil') then
         call respond2get_real_var (variable_name, '(kg/ha)'
     :                            , g%DMstate_soil_yest)

      else if (variable_name.eq.'sw_loss_system') then
         call respond2get_real_var (variable_name, '(mm)'
     :                            , g%SWloss_system)

      else if (variable_name.eq.'sw_gain_system') then
         call respond2get_real_var (variable_name, '(mm)'
     :                            , g%SWgain_system)

      else if (variable_name.eq.'sw_dlt_system') then
         call respond2get_real_var (variable_name, '(mm)'
     :                            , g%SWdlt_system)

      else if (variable_name.eq.'sw_dlt_surface') then
         call respond2get_real_var (variable_name, '(mm)'
     :                            , g%SWdlt_surface)

      else if (variable_name.eq.'sw_dlt_crop') then
         call respond2get_real_var (variable_name, '(mm)'
     :                            , g%SWdlt_crop)

      else if (variable_name.eq.'sw_dlt_soil') then
         call respond2get_real_var (variable_name, '(mm)'
     :                            , g%SWdlt_soil)

      else if (variable_name.eq.'sw_error_system') then
         call respond2get_real_var (variable_name, '(mm)'
     :                            , g%SWerror_system)

      else if (variable_name.eq.'sw_cum_error_system') then
         call respond2get_real_var (variable_name, '(mm)'
     :                            , g%SWcum_error_system)

      else if (variable_name.eq.'sw_state_system') then
         call respond2get_real_var (variable_name, '(mm)'
     :                            , g%SWstate_system_yest)

      else if (variable_name.eq.'sw_state_surface') then
         call respond2get_real_var (variable_name, '(mm)'
     :                            , g%SWstate_surface_yest)

      else if (variable_name.eq.'sw_state_crop') then
         call respond2get_real_var (variable_name, '(mm)'
     :                            , g%SWstate_crop_yest)

      else if (variable_name.eq.'sw_state_soil') then
         call respond2get_real_var (variable_name, '(mm)'
     :                            , g%SWstate_soil_yest)

      else
            ! don't own the variable
         call Message_unused ()
      endif

      call pop_routine (my_name)
      return
      end subroutine





!     ===========================================================
      subroutine sysbal_prepare ()
!     ===========================================================

      Use Infrastructure
      implicit none

!+  Purpose
!     Perform calculations before the current timestep. This is the main
!     processing for the arbitrator

!+  Changes
!      201093 jngh specified and programmed

!+  Constant Values
      character  my_name*(*)           ! procedure name
      parameter (my_name='sysbal_prepare')

!- Implementation Section ----------------------------------

      call push_routine (my_name)

            ! determine modules now

      call sysbal_modules_present (g%sysbal_index, g%num_modules)

      if (g%num_modules.gt.0) then

!               ! get light transmitted through each layer
!
!         call sysbal_top_layer_light (g%top_layer_light)
!
!               ! get light intercepted by each module sysbal
!
!         call sysbal_intc_light (g%intc_light)

      else
            ! no modules present
      endif

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine sysbal_modules_present (sysbal_index, num_modules)
!     ===========================================================

      Use Infrastructure
      implicit none

!+  Sub-Program Arguments
      integer    sysbal_index(*)       ! (OUTPUT) presence of sysbal and order
      integer    num_modules          ! (OUTPUT) number of modules present

!+  Purpose
!     Determine which modules are present and their order from top down.

!+  Changes
!      201093 jngh specified and programmed

!+  Constant Values
      character  my_name*(*)           ! procedure name
      parameter (my_name='sysbal_modules_present')

!+  Local Variables
      real       temp(max_modules)       ! temporary height array for sorting
      real       temp1(max_modules)      ! temporary height array for counting

!- Implementation Section ----------------------------------

      call push_routine (my_name)

            ! determine modules with modules now

            ! We put the heights into a temporary array as negative numbers,
            ! sort that into ascending order, with a key to their original
            ! position before sortine.  This gives us an index to the
            ! height array in descending order of height.

      call fill_real_array (temp, 0.0, max_modules)
      call subtract_real_array (g%height, temp, max_modules)
      call fill_integer_array (sysbal_index, 0, max_modules)

            ! determine order of modules from top down

      call shell_sort_real (temp, -max_modules, sysbal_index)

      call fill_real_array (temp1, 0.0, max_modules)
      call subtract_real_array (temp, temp1, max_modules)
      num_modules = count_of_real_vals (temp1, max_modules)

      call pop_routine (my_name)
      return
      end subroutine


!     ===========================================================
      subroutine sysbal_post ()
!     ===========================================================

      Use Infrastructure
      implicit none

!+  Purpose
!     Perform calculations after the current timestep.

!+  Changes
!      201093 jngh specified and programmed

!+  Constant Values
      character  my_name*(*)           ! procedure name
      parameter (my_name='sysbal_post')

!+  Local Variables
      integer    num_in_list           ! number of names in module list
      character  string*400            ! output string

      real       error_threshold
      Parameter (error_threshold = 0.1)

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      call sysbal_Bal (sysbal_get_N_variables
     :               , g%Nloss_system
     :               , g%Ngain_system
     :               , g%Ndlt_system
     :               , g%Ndlt_surface
     :               , g%Ndlt_crop
     :               , g%Ndlt_soil
     :               , g%Nerror_system
     :               , g%Ncum_error_system
     :               , g%Nstate_system_yest
     :               , g%Nstate_surface_yest
     :               , g%Nstate_crop_yest
     :               , g%Nstate_soil_yest
     :               )


      if (g%Nerror_system .gt. error_threshold*0.1) then
         write (string, '(a,f13.6)')
     :                   '**** N balance - unaccounted gain (kg/ha) = '
     :                  , g%Nerror_system
         call Write_string (string)

      elseif (g%Nerror_system .lt. -error_threshold*0.1) then
         write (string, '(a,f13.6)')
     :                   '**** N balance - unaccounted loss (kg/ha) = '
     :                  , g%Nerror_system
         call Write_string (string)

      else
         ! balance is ok
      endif

      if ( g%phosphorus_aware ) then
         call sysbal_Bal (sysbal_get_P_variables
     :               , g%Ploss_system
     :               , g%Pgain_system
     :               , g%Pdlt_system
     :               , g%Pdlt_surface
     :               , g%Pdlt_crop
     :               , g%Pdlt_soil
     :               , g%Perror_system
     :               , g%Pcum_error_system
     :               , g%Pstate_system_yest
     :               , g%Pstate_surface_yest
     :               , g%Pstate_crop_yest
     :               , g%Pstate_soil_yest
     :               )

         if (g%Perror_system .gt. error_threshold*0.1/8.0) then
            write (string, '(a,f13.6)')
     :                   '**** P balance - unaccounted gain (kg/ha) = '
     :                  , g%Perror_system
            call Write_string (string)

         elseif (g%Perror_system .lt. -error_threshold*0.1/8.0) then
            write (string, '(a,f13.6)')
     :                   '**** P balance - unaccounted loss (kg/ha) = '
     :                  , g%Perror_system
            call Write_string (string)

         else
            ! balance is ok
         endif

      else
         ! no P
      endif


      call sysbal_Bal (sysbal_get_C_variables
     :               , g%Closs_system
     :               , g%Cgain_system
     :               , g%Cdlt_system
     :               , g%Cdlt_surface
     :               , g%Cdlt_crop
     :               , g%Cdlt_soil
     :               , g%Cerror_system
     :               , g%Ccum_error_system
     :               , g%Cstate_system_yest
     :               , g%Cstate_surface_yest
     :               , g%Cstate_crop_yest
     :               , g%Cstate_soil_yest
     :               )

      if (g%Cerror_system .gt. error_threshold) then
         write (string, '(a,f13.6)')
     :                   '**** C balance - unaccounted gain (kg/ha) = '
     :                  , g%Cerror_system
         call Write_string (string)
         write (string, '(a,f13.6)')
     :                   '****        equivalent DM as FOM  (kg/ha) = '
     :                  , g%Cerror_system / 0.4
         call Write_string (string)

      elseif (g%Cerror_system .lt. -error_threshold) then
         write (string, '(a,f13.6)')
     :                   '**** C balance - unaccounted loss (kg/ha) = '
     :                  , g%Cerror_system
         call Write_string (string)
         write (string, *)
     :                   '****        equivalent DM as FOM  (kg/ha) = '
     :                  , g%Cerror_system / 0.4
         call Write_string (string)

      else
         ! balance is ok
      endif


      call sysbal_Bal (sysbal_get_DM_variables
     :               , g%DMloss_system
     :               , g%DMgain_system
     :               , g%DMdlt_system
     :               , g%DMdlt_surface
     :               , g%DMdlt_crop
     :               , g%DMdlt_soil
     :               , g%DMerror_system
     :               , g%DMcum_error_system
     :               , g%DMstate_system_yest
     :               , g%DMstate_surface_yest
     :               , g%DMstate_crop_yest
     :               , g%DMstate_soil_yest
     :               )

!      if (g%DMerror_system .gt. error_threshold) then
!         write (string, *)
!     :                   '**** DM balance - unaccounted gain (kg/ha) = '
!     :                  , g%DMerror_system
!         call Write_string (string)
!
!      elseif (g%DMerror_system .lt. -error_threshold) then
!         write (string, *)
!     :                   '**** DM balance - unaccounted loss (kg/ha) = '
!     :                  , g%DMerror_system
!         call Write_string (string)
!
!      else
!         ! balance is ok
!      endif

      call sysbal_Bal (sysbal_get_SW_variables
     :               , g%SWloss_system
     :               , g%SWgain_system
     :               , g%SWdlt_system
     :               , g%SWdlt_surface
     :               , g%SWdlt_crop
     :               , g%SWdlt_soil
     :               , g%SWerror_system
     :               , g%SWcum_error_system
     :               , g%SWstate_system_yest
     :               , g%SWstate_surface_yest
     :               , g%SWstate_crop_yest
     :               , g%SWstate_soil_yest
     :               )

      if (g%SWerror_system .gt. error_threshold) then
         write (string, '(a,f13.6)')
     :                   '**** SW balance - unaccounted gain (mm) = '
     :                  , g%SWerror_system
         call Write_string (string)

      elseif (g%SWerror_system .lt. -error_threshold) then
         write (string, '(a,f13.6)')
     :                   '**** SW balance - unaccounted loss (mm) = '
     :                  , g%SWerror_system
         call Write_string (string)

      else
         ! balance is ok
      endif

         ! Zero event data
      g%irrigation = 0.0

      g%dm_removed           = 0.0
      g%N_removed            = 0.0
      g%P_removed            = 0.0

      g%dm_added           = 0.0
      g%N_added            = 0.0
      g%P_added            = 0.0

      g%massBalanceChange%PoolClass = ' '
      g%massBalanceChange%FlowType = ' '
      g%massBalanceChange%C = 0.0
      g%massBalanceChange%N = 0.0
      g%massBalanceChange%P = 0.0
      g%massBalanceChange%DM = 0.0
      g%massBalanceChange%SW = 0.0

      g%removedCrop%PoolClass = ' '
      g%removedCrop%FlowType = ' '
      g%removedCrop%C = 0.0
      g%removedCrop%N = 0.0
      g%removedCrop%P = 0.0
      g%removedCrop%DM = 0.0
      g%removedCrop%SW = 0.0

      g%addedCrop%PoolClass = ' '
      g%addedCrop%FlowType = ' '
      g%addedCrop%C = 0.0
      g%addedCrop%N = 0.0
      g%addedCrop%P = 0.0
      g%addedCrop%DM = 0.0
      g%addedCrop%SW = 0.0

      g%removedSurface%PoolClass = ' '
      g%removedSurface%FlowType = ' '
      g%removedSurface%C = 0.0
      g%removedSurface%N = 0.0
      g%removedSurface%P = 0.0
      g%removedSurface%DM = 0.0
      g%removedSurface%SW = 0.0

      g%addedSurface%PoolClass = ' '
      g%addedSurface%FlowType = ' '
      g%addedSurface%C = 0.0
      g%addedSurface%N = 0.0
      g%addedSurface%P = 0.0
      g%addedSurface%DM = 0.0
      g%addedSurface%SW = 0.0

      g%removedSoil%PoolClass = ' '
      g%removedSoil%FlowType = ' '
      g%removedSoil%C = 0.0
      g%removedSoil%N = 0.0
      g%removedSoil%P = 0.0
      g%removedSoil%DM = 0.0
      g%removedSoil%SW = 0.0

      g%addedSoil%PoolClass = ' '
      g%addedSoil%FlowType = ' '
      g%addedSoil%C = 0.0
      g%addedSoil%N = 0.0
      g%addedSoil%P = 0.0
      g%addedSoil%DM = 0.0
      g%addedSoil%SW = 0.0

      call pop_routine (my_name)
      return
      end subroutine

!     ===========================================================
      subroutine sysbal_Bal ( fun
     :                      , loss_system
     :                      , gain_system
     :                      , dlt_system
     :                      , dlt_surface
     :                      , dlt_crop
     :                      , dlt_soil
     :                      , error_system
     :                      , cum_error_system
     :                      , state_system_yest
     :                      , state_surface_yest
     :                      , state_crop_yest
     :                      , state_soil_yest
     :                      )
!     ===========================================================

      Use Infrastructure
      implicit none


!      real fun
      real  loss_system
      real  gain_system

      real  dlt_system
      real  dlt_surface
      real  dlt_crop
      real  dlt_soil

      real  error_system
      real  cum_error_system

      real  state_system_yest
      real  state_surface_yest
      real  state_crop_yest
      real  state_soil_yest

!+  Purpose
!     Perform calculations after the current timestep.

!+  Changes
!      201093 jngh specified and programmed

!+  Constant Values
      character  my_name*(*)           ! procedure name
      parameter (my_name='sysbal_bal')

!+  Local Variables
      real  state_soil
      real  state_surface
      real  state_crop
      real  state_system

      real  loss_soil
      real  loss_surface
      real  loss_crop
      real  loss_discontinuity

      real  gain_soil
      real  gain_surface
      real  gain_crop
      real  gain_discontinuity


!- Implementation Section ----------------------------------

      call push_routine (my_name)

      call fun (
     :          state_crop
     :        , state_soil
     :        , state_surface
     :        , loss_soil
     :        , loss_crop
     :        , loss_surface
     :        , loss_discontinuity
     :        , gain_soil
     :        , gain_crop
     :        , gain_surface
     :        , gain_discontinuity
     :        )

      loss_system = loss_soil + loss_crop + loss_surface
     :            + loss_discontinuity
      gain_system = gain_soil + gain_crop + gain_surface
     :            + gain_discontinuity

      state_system = state_surface + state_soil + state_crop
      dlt_system = state_system - state_system_yest

      error_system =  dlt_system + loss_system - gain_system

      if (state_system_yest >= 0.00001) then
         cum_error_system = cum_error_system + error_system

      else
         error_system = 0.0
      endif

      dlt_soil    =  state_soil    - state_soil_yest
      dlt_surface =  state_surface - state_surface_yest
      dlt_crop    =  state_crop    - state_crop_yest

      state_system_yest  = state_system
      state_soil_yest    = state_soil
      state_surface_yest = state_surface
      state_crop_yest    = state_crop


      call pop_routine (my_name)
      return
      end subroutine

! ====================================================================
       subroutine sysbal_ONirrigated ()
! ====================================================================
      Use Infrastructure
      implicit none

!+  Purpose
!     <insert here>

!+  Mission Statement
!     Add Water

!+  Changes
!   neilh - 18-08-1995 - Programmed and Specified
!   neilh - 07-06-1996 - removed data_String from argument list
!                      - changed extract calls to collect calls
!   neilh - 30-08-1999 - routine name changed to ONirrigated

!+  Local Variables
       real             amount           ! amount of irrigation (mm)
       integer          numvals          ! no. of values read from string
       real             solconc          ! solute conc in water(kg/ha)
       integer          solnum           ! solute no. counter variable

!- Implementation Section ----------------------------------

      call collect_real_var (DATA_irrigate_amount
     :                      ,'(mm)'
     :                      ,amount
     :                      ,numvals
     :                      ,0.0
     :                      ,1000.)

      g%irrigation = g%irrigation + amount

!      do 100 solnum = 1, g%num_solutes
!
!         call collect_real_var_optional (
!     :                         g%solute_names(solnum)
!     :                        ,'(kg/ha)'
!     :                        ,solconc
!     :                        ,numvals
!     :                        ,0.0
!     :                        ,1000.)
!
!        if (numvals.gt.0) then
!           g%irrigation_solute(solnum) = g%irrigation_solute(solnum)
!     :                                 + solconc
!        else
!        endif
!  100 continue

      return
      end subroutine

!===========================================================
      subroutine sysbal_ON_Crop_chopped ()
!===========================================================
      Use Infrastructure
      implicit none

!+  Purpose
!     Get information on surfom added from the crops

!+  Local Variables
      real      dm_removed
      real      N_removed
      real      P_removed
      real      dlt_crop_dm(MaxArraySize)
      real      dlt_dm_N(MaxArraySize)
      real      dlt_dm_P(MaxArraySize)
      real      fraction_to_Residue(MaxArraySize)
      real      fraction_removed(MaxArraySize)
      character  Event_string*400      ! Event message string
      character  flag*5                ! p data flag
      integer    NumVals               ! number of values read from file
      integer    NumVal_dm             ! number of values read from file
      integer    NumVal_N              ! number of values read from file
      integer    NumVal_P              ! number of values read from file
      integer    SOMNo     ! system number of the surface organic matter added
      integer    residue               ! system surfom counter
      integer    max_part              ! number of plant parts
      parameter (max_part = 10)
      integer    root_num
      integer    counter

      character*20 part_name(max_part)

!- Implementation Section ----------------------------------

      fraction_to_Residue(:) = 0.0
      call collect_real_array (DATA_fraction_to_Residue, MaxArraySize
     :                      , '()', fraction_to_Residue, numvals
     :                      , 0.0, 100000.0)

      call collect_char_array (DATA_dm_type, max_part
     :                      , '()', part_name, numvals)

      do  counter = 1, numvals
         if (part_name(counter)(1:4) .eq. 'root') then
            root_num = counter
            exit
         else
            root_num = 0
         endif
      end do

         ! Find the amount of surfom to be removed today
      dlt_crop_dm(:) = 0.0
      call collect_real_array (DATA_dlt_crop_dm, MaxArraySize, '()'
     :                      , dlt_crop_dm, numval_dm, 0.0, 100000.0)
      fraction_removed(:numvals) = 1.0 - fraction_to_Residue(:numvals)
         ! assume roots are not removed from system but end up in soil.
      if(root_num .gt. 0) fraction_removed(root_num) = 0.0

      dm_removed = sum(dlt_crop_dm(:numvals)
     :           * fraction_removed(:numvals))

      if (dm_removed .gt. 0.0) then

          ! Find the amount of N removed in surfom today
         dlt_dm_N(:) = 0.0
         call collect_real_array(DATA_dlt_dm_n, MaxArraySize, '(kg/ha)'
     :                      , dlt_dm_n, numval_n, -10000.0, 10000.0)
         N_removed = sum(dlt_dm_N(:numvals)
     :             *  fraction_removed(:numvals))

             ! Find the amount of P removed in surfom today, if phosphorus aware

         if ( g%phosphorus_aware ) then
            dlt_dm_P(:) = 0.0
            call collect_real_array_optional (DATA_dlt_dm_p
     :                        , MaxArraySize
     :                        , '(kg/ha)'
     :                        , dlt_dm_p
     :                        , numval_p
     :                        , -10000.0
     :                        , 10000.0)
            P_removed = sum(dlt_dm_P(:numvals)
     :                * fraction_removed(:numvals))
         else
            ! Not phosphorus aware
            dlt_dm_P(:) = 0.0
            P_removed = 0.0
         endif

         g%dm_removed = g%dm_removed + dm_removed
         g%N_removed = g%N_removed + N_removed
         g%P_removed = g%P_removed + P_removed
      else
            !nothing to add
      endif

      return
      end subroutine

!===========================================================
      subroutine sysbal_ONBiomassRemoved (variant)
!===========================================================
      Use Infrastructure
      implicit none

!+  Purpose
!     Get information on surfom added from the crops

      integer, intent(in) :: variant
      type(BiomassRemovedType) :: BiomassRemoved
      integer root_num
      real    fraction_removed(MaxArraySize)
      integer numvals
      real    dm_removed
      real    n_removed
      real    p_removed
      integer counter

      call unpack_BiomassRemoved(variant, BiomassRemoved)

      do  counter = 1, numvals
         if (BiomassRemoved%dm_type(counter)(1:4) .eq. 'root') then
            root_num = counter
            exit
         else
            root_num = 0
         endif
      end do

      ! Find the amount of surfom to be removed today
      numvals = BiomassRemoved%num_dm_type
      fraction_removed(:numvals) = 1.0 -
     :      BiomassRemoved%fraction_to_Residue(:numvals)

      ! assume roots are not removed from system but end up in soil.
      if(root_num .gt. 0)fraction_removed(root_num)=  0.0

      dm_removed = sum(BiomassRemoved%dlt_crop_dm(:numvals)
     :           * fraction_removed(:numvals))

      if (dm_removed .gt. 0.0) then

         ! Find the amount of N removed in surfom today
         N_removed = sum(BiomassRemoved%dlt_dm_N(:numvals)
     :             *  fraction_removed(:numvals))

         ! Find the amount of P removed in surfom today, if phosphorus aware
         P_removed = sum(BiomassRemoved%dlt_dm_P(:numvals)
     :                * fraction_removed(:numvals))

         g%dm_removed = g%dm_removed + dm_removed
         g%N_removed = g%N_removed + N_removed
         g%P_removed = g%P_removed + P_removed
      endif

      return
      end subroutine

!===========================================================
      subroutine sysbal_ON_Residue_removed ()
!===========================================================
      Use Infrastructure
      implicit none

!+  Purpose
!     Get information on surfom added from the crops

!+  Local Variables
      real      dm_removed
      real      N_removed
      real      P_removed
      real      dlt_crop_dm(MaxArraySize)
      real      dlt_dm_N(MaxArraySize)
      real      dlt_dm_P(MaxArraySize)
      real      fraction_to_Residue(MaxArraySize)
      real      fraction_removed(MaxArraySize)
      character  Event_string*400      ! Event message string
      character  flag*5                ! p data flag
      integer    NumVals               ! number of values read from file
      integer    NumVal_dm             ! number of values read from file
      integer    NumVal_N              ! number of values read from file
      integer    NumVal_P              ! number of values read from file
      integer    SOMNo     ! system number of the surface organic matter added
      integer    residue               ! system surfom counter
      integer    max_part              ! number of plant parts
      parameter (max_part = 10)
      integer    root_num
      integer    counter

      character*20 part_name(max_part)

!- Implementation Section ----------------------------------

      fraction_to_Residue(:) = 0.0
      call collect_real_array (DATA_fraction_to_Residue, MaxArraySize
     :                      , '()', fraction_to_Residue, numvals
     :                      , 0.0, 100000.0)

      max_part(:) = blank
      call collect_char_array (DATA_dm_type, max_part
     :                      , '()', part_name, numvals)

      do  counter = 1, numvals
         if (part_name(counter)(1:4) .eq. 'root') then
            root_num = counter
            exit
         else
            root_num = 0
         endif
      end do

         ! Find the amount of surfom to be removed today
      dlt_crop_dm(:) = 0.0
      call collect_real_array (DATA_dlt_crop_dm, MaxArraySize, '()'
     :                      , dlt_crop_dm, numval_dm, 0.0, 100000.0)
      fraction_removed(:numvals) = 1.0 - fraction_to_Residue(:numvals)
         ! assume roots are not removed from system but end up in soil.
      if(root_num .gt. 0) fraction_removed(root_num) = 0.0

      dm_removed = sum(dlt_crop_dm(:numvals)
     :           * fraction_removed(:numvals))

      if (dm_removed .gt. 0.0) then

          ! Find the amount of N removed in surfom today
         dlt_dm_N(:) = 0.0
         call collect_real_array(DATA_dlt_dm_n, MaxArraySize, '(kg/ha)'
     :                      , dlt_dm_n, numval_n, -10000.0, 10000.0)
         N_removed = sum(dlt_dm_N(:numvals)
     :             *  fraction_removed(:numvals))

             ! Find the amount of P removed in surfom today, if phosphorus aware

         if ( g%phosphorus_aware ) then
            dlt_dm_P(:) = 0.0
            call collect_real_array_optional (DATA_dlt_dm_p
     :                        , MaxArraySize
     :                        , '(kg/ha)'
     :                        , dlt_dm_p
     :                        , numval_p
     :                        , -10000.0
     :                        , 10000.0)
            P_removed = sum(dlt_dm_P(:numvals)
     :                * fraction_removed(:numvals))
         else
            ! Not phosphorus aware
            dlt_dm_P(:) = 0.0
            P_removed = 0.0
         endif

         g%dm_removed = g%dm_removed + dm_removed
         g%N_removed = g%N_removed + N_removed
         g%P_removed = g%P_removed + P_removed
      else
            !nothing to add
      endif

      return
      end subroutine

!================================================================
      subroutine sysbal_add_surfom ()
!================================================================
      Use Infrastructure
      implicit none

!+  Purpose
!   Calculates surfom addition as a result of add_surfom message

!
!!+  Local Variables
      character  Err_string*300         ! Error message string
      integer    numvals               ! number of values read from file
      integer    numval_n              ! number of N values read from file
      integer    numval_cnr            ! number of cnr values read from file
      integer    numval_p              ! number of N values read from file
      integer    numval_cpr            ! number of cnr values read from file
      integer    residue               ! residue counter
      integer    SOMNo     ! specific system number for this residue name
      character  surfom_name*30        ! unique 'name' of residue to be added ()
      character  surfom_type*30        ! 'type' of residue to be added ()
      real       surfom_added          ! Mass of new surfom added (kg/ha)
      real       surfom_n_added        ! N added in new material (kg/ha)
      real       surfom_cnr_added      ! C:N ratio of new material
      real       surfom_p_added        ! P added in new material (kg/ha)
      real       surfom_cpr_added      ! C:P ratio of new material
      real       added_wt

   !- Implementation Section ----------------------------------

      ! Get Mass of material added
      call collect_real_var ('mass', '(kg/ha)', surfom_added, numvals
     :                        , -100000.0, 100000.0)

      if (surfom_added .gt. 0.0) then
         ! Get N content of material added
         call collect_real_var_optional ('n', '(kg/ha)', surfom_N_added
     :                        , numval_n, -10000.0, 10000.0)
!         if (numval_n.eq.0) then
!            call collect_real_var_optional ('cnr', '()'
!     :                        , surfom_cnr_added, numval_cnr
!     :                        , 0.0, 10000.0)
!            surfom_N_added = divide ((surfom_added * 0.08) !c%C_fract(SOMNo))
!     :                               , surfom_cnr_added, 0.0)
!
!         else
!         endif

         ! collect P information from this new member
         surfom_p_added = 0.0
         call collect_real_var_optional ('p', '(kg/ha)', surfom_p_added
     :                        , numval_p, -10000.0, 10000.0)
!         if (numval_p.eq.0) then
!            surfom_cpr_added = 0.0
!            call collect_real_var_optional ('cpr', '()'
!     :                        , surfom_cpr_added, numval_cpr
!     :                        , 0.0, 10000.0)
!            surfom_P_added = divide ((surfom_added* c%C_fract(SOMNo))
!     :                        , surfom_cpr_added, 0.0)
!            ! If no P info provided, and no cpr info provided then
!            ! use default cpr and throw warning error to notify user
!            If (numval_CPr .eq. 0) then
!               surfom_p_added = divide ((surfom_added*c%C_fract(SOMNo))
!     :                        ,c%default_cpr,0.0)
!         Endif
         g%dm_added = g%dm_added + surfom_added
         g%N_added = g%N_added + surfom_N_added
         g%P_added = g%P_added + surfom_p_added

      else
      endif

      return
      end subroutine

!================================================================
      subroutine sysbal_set_phosphorus_aware ()
!================================================================
      Use infrastructure
      implicit none

!+  Purpose
!      Check that soil phosphorus is in system

!+  Local Variables
      integer   numvals
      real labile_p(max_layer)      ! labile p from soil phosphorous

!- Implementation Section ----------------------------------

      numvals = 0
      call Get_real_array_optional(unknown_module, 'labile_p'
     :                      , max_layer, '(kg/ha)', labile_p
     :                      , numvals, 0.5, 1000.0)

      if(numvals .gt. 0) then
         ! is p aware
         g%phosphorus_aware = .true.
      else
         g%phosphorus_aware = .false.

      endif

      return
      end subroutine

! ====================================================================
       subroutine sysbal_create ()
! ====================================================================
      Use Infrastructure
      implicit none

!+  Purpose
!     Create

!+  Mission statement
!     Create

!+  Changes
!   neilh - 04-01-2002 - Programmed and Specified


!+  Local Variables

!- Implementation Section ----------------------------------


         call sysbal_zero_all_variables ()
         call sysbal_init ()

      return
      end subroutine

! ====================================================================
       subroutine sysbal_ONExternalMassFlow (variant)
! ====================================================================
      Use Infrastructure
      implicit none

      integer, intent(in) :: variant
!+  Purpose
!     catch Mass balance discontinuites

!+  Mission statement
!     Create

!+  Changes
!   neilh - 04-01-2002 - Programmed and Specified


!+  Local Variables
      type(ExternalMassFlowType) :: massBalanceChange
      character*200   message          !

!- Implementation Section ----------------------------------


      call unpack_ExternalMassFlow(variant, massBalanceChange)

      if (massBalanceChange%PoolClass == 'soil') then
         if (massBalanceChange%FlowType == 'gain') then
            g%addedSoil%C = g%addedSoil%C
     :                    + massBalanceChange%C
            g%addedSoil%N = g%addedSoil%N
     :                    + massBalanceChange%N
            g%addedSoil%P = g%addedSoil%P
     :                    + massBalanceChange%P
            g%addedSoil%DM = g%addedSoil%DM
     :                     + massBalanceChange%DM
            g%addedSoil%SW = g%addedSoil%SW
     :                     + massBalanceChange%SW
         elseif (massBalanceChange%FlowType == 'loss') then
            g%removedSoil%C = g%removedSoil%C
     :                      + massBalanceChange%C
            g%removedSoil%N = g%removedSoil%N
     :                      + massBalanceChange%N
            g%removedSoil%P = g%removedSoil%P
     :                      + massBalanceChange%P
            g%removedSoil%DM = g%removedSoil%DM
     :                       + massBalanceChange%DM
            g%removedSoil%SW = g%removedSoil%SW
     :                       + massBalanceChange%SW
         else
            ! unknown Flow Type
            write (message, *) 'Unknown External Mass Flow Type: '
     :           , trim(massBalanceChange%FlowType)
     :           , ': Must be gain or loss'
         endif
      elseif (massBalanceChange%PoolClass == 'crop') then
         if (massBalanceChange%FlowType == 'gain') then
            g%addedCrop%C = g%addedCrop%C
     :                    + massBalanceChange%C
            g%addedCrop%N = g%addedCrop%N
     :                    + massBalanceChange%N
            g%addedCrop%P = g%addedCrop%P
     :                    + massBalanceChange%P
            g%addedCrop%DM = g%addedCrop%DM
     :                     + massBalanceChange%DM
            g%addedCrop%SW = g%addedCrop%SW
     :                     + massBalanceChange%SW
         elseif (massBalanceChange%FlowType == 'loss') then
            g%removedCrop%C = g%removedCrop%C
     :                      + massBalanceChange%C
            g%removedCrop%N = g%removedCrop%N
     :                      + massBalanceChange%N
            g%removedCrop%P = g%removedCrop%P
     :                       + massBalanceChange%P
            g%removedCrop%DM = g%removedCrop%DM
     :                       + massBalanceChange%DM
            g%removedCrop%SW = g%removedCrop%SW
     :                       + massBalanceChange%SW
         else
            ! unknown Flow Type
            write (message, *) 'Unknown External Mass Flow Type: '
     :           , trim(massBalanceChange%FlowType)
     :           , ': Must be gain or loss'
         endif
      elseif (massBalanceChange%PoolClass == 'surface') then
         if (massBalanceChange%FlowType == 'gain') then
            g%addedSurface%C = g%addedSurface%C
     :                       + massBalanceChange%C
            g%addedSurface%N = g%addedSurface%N
     :                       + massBalanceChange%N
            g%addedSurface%P = g%addedSurface%P
     :                       + massBalanceChange%P
            g%addedSurface%DM = g%addedSurface%DM
     :                        + massBalanceChange%DM
            g%addedSurface%SW = g%addedSurface%SW
     :                        + massBalanceChange%SW
         elseif (massBalanceChange%FlowType == 'loss') then
            g%removedSurface%C = g%removedSurface%C
     :                         + massBalanceChange%C
            g%removedSurface%N = g%removedSurface%N
     :                         + massBalanceChange%N
            g%removedSurface%P = g%removedSurface%P
     :                         + massBalanceChange%P
            g%removedSurface%DM = g%removedSurface%DM
     :                          + massBalanceChange%DM
            g%removedSurface%SW = g%removedSurface%SW
     :                          + massBalanceChange%SW
         else
            ! unknown Flow Type
            write (message, *) 'Unknown External Mass Flow Type: '
     :           , trim(massBalanceChange%FlowType)
     :           , ': Must be gain or loss'
         endif
      else
         ! unknown Pool Class
            write (message, *) 'Unknown External Mass Pool Type: '
     :           , trim(massBalanceChange%PoolClass)
     :           , ': Must be surface, crop or soil'
      endif

      return
      end subroutine


      end module SysBalModule

!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use SysBalModule
      implicit none
      ml_external alloc_dealloc_instance

!+  Sub-Program Arguments
      logical, intent(in) :: doAllocate

!+  Purpose
!      Module instantiation routine.

!- Implementation Section ----------------------------------

      if (doAllocate) then
         allocate(g)
         allocate(id)
      else
         deallocate(g)
         deallocate(id)
      end if
      return
      end subroutine




!     ===========================================================
      subroutine Main (Action, Data_string)
!     ===========================================================

      Use Infrastructure
      use SysBalModule
      implicit none
      ml_external Main

!+  Sub-Program Arguments
      character  Action*(*)            ! (INPUT) Message action to perform
      character  Data_string*(*)       ! (INPUT) Message data

!+  Purpose
!      This routine is the interface between the main system and the
!      sysbal module.

!+  Changes
!      201093 jngh specified and programmed
!      011195 jngh  added call to message_unused
!      090299 jngh removed find modules and get other variables from init
!      100299 jngh added find modules back in
!      280999 sdb removed version reference

!+  Constant Values
      character  my_name*(*)
      parameter (my_name = 'sysbal_main')

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Action.eq.ACTION_Get_variable) then
            ! respond to requests from other modules
         call sysbal_send_my_variable (Data_string)

      else if (action .eq. EVENT_irrigated) then
               ! respond to addition of irrigation
         call sysbal_ONirrigated ()

      elseif (Action .eq. EVENT_Crop_Chopped) then
         call sysbal_ON_Crop_Chopped ()

!      else if (Action.eq.'add_surfaceom') then
!         call sysbal_Add_surfom ()

      elseif (Action .eq. EVENT_Residue_removed) then
         call sysbal_ON_Residue_removed ()

      elseif (Action .eq. ACTION_Prepare) then
         call sysbal_zero_variables ()
!         call sysbal_get_other_variables ()
!         call sysbal_prepare ()
          call sysbal_set_phosphorus_aware ()

      else if (Action .eq. ACTION_Post) then
!         call sysbal_get_other_variables ()
         call sysbal_post ()

      else if (Action.eq.ACTION_Init) then
         call sysbal_zero_all_variables ()
         call sysbal_init ()
!         call sysbal_get_other_variables ()

      else
            ! Don't use message

         call Message_unused ()

      endif

      call pop_routine (my_name)
      return
      end subroutine

      ! ====================================================================
      ! do first stage initialisation stuff.
      ! ====================================================================
      subroutine doInit1 ()
      use infrastructure
      use SysbalModule

      ml_external doInit1

      call doRegistrations(id)
      end subroutine

 ! ====================================================================
! This routine is the event handler for all events
! ====================================================================
      subroutine respondToEvent(fromID, eventID, variant)
      Use infrastructure
      Use SysBalModule
      implicit none
      ml_external respondToEvent

      integer, intent(in) :: fromID
      integer, intent(in) :: eventID
      integer, intent(in) :: variant

      if (eventID .eq. id%ExternalMassFlow) then
         call sysbal_ONExternalMassFlow(variant)
      else if (eventID .eq. id%BiomassRemoved) then
         call sysbal_ONBiomassRemoved(variant)
      endif
      return
      end subroutine respondToEvent
