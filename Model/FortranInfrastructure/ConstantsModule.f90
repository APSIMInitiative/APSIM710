! ====================================================================
!      Constant definitions
! ====================================================================

!   Short description:
!      Globally used constant definitions

!   Notes:

!   Changes:
!      DPH 2/07/92
!      DPH 15/02/93 Added MES_Get_variable and MES_Ret_variable
!      JNGH 11/02/94 increased Mes_data_size to from 500 to 600.
!      JNGH 4/8/94 included file_not_found flag
!                    changed function string length to = mes data size
!      JNGH 29/9/94 added harvest, sow and no_section
!      DPH 7/11/94 Removed var_delimiter.  Modified Mes_delimiter to ','
!                  Changed names of Global ... module specifiers.
!                  Added MES_End_crop and MES_Kill_crop messages
!      DPH 20/10/95 Changed MES_Data_size to 50 characters.
!                   Removed MES_Ret_variable.
!      JNGH 26/2/96 added MES_INITIATE_CROP
!      DPH 20/5/96  MES_intertime_step, Max_variable_name_size, MES_report
!      JNGH 22/06/96 reduced function string length to 600 from 1000
!      dph 24/2/97  added lu_log_file, MES_start, MES_pause, MES_continue,
!                   MES_idle
!      nih 19/8/97  added MES_reset and MES_Sum_Report
!      igh 18/08/98 Changed MES_Till from 'till' to 'tillage'
!      sb 1/9/98 Added max_year and min_year.
!      jngh 21/10/98 added MNP messages
!      nih 09/09/99 changed MES_init contents
!      dph 22/9/99  changed new_line to char(10)
!      dph 23/9/99  moved MES_ constants to action.inc
!      dph 27/9/99  removed all lu_ constants
!      jngh 15/12/00 added close_enough_to_zero
!      dph 22/6/01 renamed file to componentdefinitions.f90 and converted
!                  to a F90 module.

! ----------------------- Declaration section ------------------------

module ConstantsModule
   implicit none

   ! blank character.
   character (len=*), parameter :: Blank = ' '

   ! Size of a message's address char. string
   integer, parameter :: Mes_address_size = 8


   ! Size of action part of message
   integer, parameter :: Mes_Action_size = 20

   ! Size of data part of message
   integer, parameter :: Mes_Data_size = 50

   ! Message delimiter
   character (len=*), parameter :: Mes_delimiter = ','

   ! Len. of string returned by char. fun.
   integer, parameter :: Function_string_len = 600

   ! minimum year for APSIM simulations.
   integer, parameter :: min_year = 1583  ! See clock manual wrt gregorian calendar

   ! maximum year for APSIM simulations.
   integer, parameter :: max_year = 4200

   character (len=*), parameter :: All_active_modules ='act_mods'
   integer, parameter           :: Unknown_module = 0
   character (len=*), parameter :: First_active_module = 'unk_mod'

   ! Smallest number considered to be zero
   real, parameter :: close_enough_to_zero = 1.0e-15

   ! maximum character size
   integer, parameter :: max_line_size = 2000

   ! maximum allowed size for filenames
   integer, parameter :: Max_file_name_size = 100

   ! maximum allowed size for module names (can be FQ eg: .masterPM.xyz.abc)
   integer, parameter :: Max_module_name_size = 512

   ! maximum allowed size for module names
   integer, parameter :: Max_inst_name_size = max_module_name_size+3

   ! Maximum allowed size for section names
   integer, parameter :: Max_section_name_size = 50

   ! maximum size of a variable name.
   integer, parameter :: Max_variable_name_size = 35

   ! maximum size for title
   integer, parameter :: Max_title_size = 100

   ! maximum soil layers
   integer, parameter :: crop_max_layer = 100


   ! New line delimiter
   character (len=*), parameter :: New_line = char(10)

   ! identifier for no section returned
   character (len=*), parameter :: no_section = ' '


   character (len=*), parameter :: ACTION_Create        = 'create'
   character (len=*), parameter :: ACTION_Init          = 'init2'
   character (len=*), parameter :: ACTION_Get_variable = 'get'
   character (len=*), parameter :: ACTION_Set_variable = 'set'
   character (len=*), parameter :: ACTION_Prepare      = 'prepare'
   character (len=*), parameter :: ACTION_Process      = 'process'
   character (len=*), parameter :: ACTION_Post         = 'post'
   character (len=*), parameter :: ACTION_Start        = 'start'
   character (len=*), parameter :: ACTION_Pause        = 'pause'
   character (len=*), parameter :: ACTION_Continue     = 'continue'
   character (len=*), parameter :: ACTION_Finish       = 'finish'
   character (len=*), parameter :: ACTION_End_Run      = 'end_run'
   character (len=*), parameter :: ACTION_Report       = 'report'
   character (len=*), parameter :: ACTION_Idle         = 'idle'
   character (len=*), parameter :: ACTION_Reset        = 'reset'
   character (len=*), parameter :: ACTION_Sum_Report   = 'sum_report'
   character (len=*), parameter :: ACTION_Till         = 'tillage'
   character (len=*), parameter :: ACTION_Sow          = 'sow'
   character (len=*), parameter :: ACTION_Harvest      = 'harvest'
   character (len=*), parameter :: ACTION_End_Crop     = 'end_crop'
   character (len=*), parameter :: ACTION_Kill_Crop    = 'kill_crop'
   character (len=*), parameter :: ACTION_Add_Residue   = 'add_residue'
   character (len=*), parameter :: ACTION_Do_Decompose  = 'do_decompose'
   character (len=*), parameter :: ACTION_Decomposed    = 'decomposed'
   character (len=*), parameter :: ACTION_Initiate_Crop = 'initiate_crop'
   character (len=*), parameter :: ACTION_Add_Residue_P = 'add_residue_p'
   character (len=*), parameter :: ACTION_User_Init = 'init'
   character (len=*), parameter :: ACTION_gimme_water  = 'gimme_water'
   character (len=*), parameter :: ACTION_Top_up  = 'top_up'

   integer, parameter :: err_internal = 1
   integer, parameter :: err_user = 2

! ====================================================================
!      COMMON EVENT CONSTANTS
! ====================================================================

!   Short description:
!      Globally used constants for defining names for events within APSIM

!   Notes:

!   Changes:
!      NIH 17/05/99

! ----------------------- Declaration section ------------------------

!   Global variables
!      none

!   Constant values

      character DATA_sender* (*)         ! keyname used to identify the sender
      parameter (DATA_sender='sender')   ! of an event.  This data is included
                                          ! with every event.

      character DATA_sender_ID*(*)        ! ID used to identify the sender
      parameter (DATA_sender_ID='sender_id')   ! of an event.  This data is included
                                          ! with every event.

! ====================================================================
!      CLOCK TICK EVENT
! ====================================================================

!   Short description:
!      To notify all interested modules in a change in the simulation
!      system time and the duration of the new time step.

!   Notes:

!   Changes:
!      NIH 25/08/99

! ----------------------- Declaration section ------------------------

!   Global variables
!      none

!   Event Name

      character EVENT_tick* (*)
      parameter (EVENT_tick='tick')

!   Event Data

      character DATA_day *(*)      ! data name for day of year
      parameter (DATA_day = 'day')
      character DATA_year *(*)      ! data name for year
      parameter (DATA_year = 'year')
      character DATA_time *(*)      ! data name for time of day
      character DATA_jday *(*)      ! data name for julian day number
      parameter (DATA_jday = 'jday')

      parameter (DATA_time = 'time')
      character DATA_timestep *(*)  ! data name for timestep
      parameter (DATA_timestep = 'timestep')


! ====================================================================
!      NEW SOLUTE EVENT
! ====================================================================

!   Short description:
!      To notify all interested modules in the ownership of solutes by
!      an individual module.
!      For example, Soil water modules may need to know what solutes will
!      need to be redistributed with movement of soil water.

!   Notes:

!   Changes:
!      NIH 17/05/99

! ----------------------- Declaration section ------------------------

!   Global variables
!      none

!   Event Name

      character EVENT_new_solute* (*)
      parameter (EVENT_new_solute='new_solute')

!   Event Data

      character DATA_new_solute_names *(*)               ! name of character array of data containing
      parameter (DATA_new_solute_names = 'solute_names') ! a list of solutes owned by the sender


! ====================================================================
!      NEW MET EVENT
! ====================================================================

!   Short description:
!      To notify all interested modules in a change in the simulation
!      met data for the current time step

!   Notes:

!   Changes:
!      NIH 27/08/99

! ----------------------- Declaration section ------------------------

!   Global variables
!      none

!   Event Name

      character EVENT_newmet* (*)
      parameter (EVENT_newmet='newmet')

!   Event Data

      character DATA_radn *(*)
      parameter (DATA_radn = 'radn')
      character DATA_maxt *(*)
      parameter (DATA_maxt = 'maxt')
      character DATA_mint *(*)
      parameter (DATA_mint = 'mint')
      character DATA_rain *(*)
      parameter (DATA_rain = 'rain')
      character DATA_vp *(*)
      parameter (DATA_vp = 'vp')

! ====================================================================
!      IRRIGATED EVENT
! ====================================================================

!   Short description:
!      To notify all interested modules in an irrigation application
!      which may/maynot include solutes

!   Notes:

!   Changes:
!      NIH 30/08/99

! ----------------------- Declaration section ------------------------

!   Global variables
!      none

!   Event Name

      character EVENT_irrigated* (*)
      parameter (EVENT_irrigated='irrigated')

!   Event Data

      character DATA_irrigate_amount *(*)
      parameter (DATA_irrigate_amount = 'amount')
      character DATA_irrigate_time *(*)
      parameter (DATA_irrigate_time = 'time')
      character DATA_irrigate_duration *(*)
      parameter (DATA_irrigate_duration = 'duration')



! ====================================================================
!      POTENTIAL RESIDUE DECOMPOSITION EVENT
! ====================================================================

!   Short description:
!      To notify all interested modules in a potential residue decomposition

!   Notes:

!   Changes:
!      NIH 31/08/99

! ----------------------- Declaration section ------------------------

!   Global variables
!      none

!   Event Name

      character EVENT_Pot_Res_decomp*(*)
      parameter (EVENT_Pot_Res_decomp='pot_decomp')

!   Event Data

      character DATA_Pot_C_decomp *(*)
      parameter (DATA_Pot_C_decomp = 'pot_c_decomp')
      character DATA_Pot_N_decomp *(*)
      parameter (DATA_Pot_N_decomp = 'pot_n_decomp')
      character DATA_Pot_P_decomp *(*)
      parameter (DATA_Pot_P_decomp = 'pot_p_decomp')

! ====================================================================
!      NITROGEN BALANCE EVENT
! ====================================================================

!   Short description:
!      To notify all interested modules in the completion of the
!      soil N calculations, including all N transformation processes
!      for soil organic and inorganic pools.

!   Notes:

!   Changes:
!      NIH 08/09/99

! ----------------------- Declaration section ------------------------

!   Global variables
!      none

!   Event Name

      character EVENT_N_Balance*(*)
      parameter (EVENT_N_Balance='n_balance')

!   Event Data

      character DATA_NH4_transform_net *(*)
      parameter (DATA_NH4_transform_net = 'nh4_transform_net')
      character DATA_NO3_transform_net *(*)
      parameter (DATA_NO3_transform_net = 'no3_transform_net')

      character DATA_dlt_NH4_net *(*)
      parameter (DATA_dlt_NH4_net = 'dlt_nh4_net')
      character DATA_dlt_NO3_net *(*)
      parameter (DATA_dlt_NO3_net = 'dlt_no3_net')

! ====================================================================
!      CARBON BALANCE EVENT
! ====================================================================

!   Short description:
!      To notify all interested modules in the completion of the
!      soil C calculations, including all C transformation processes
!      for soil organic pools.

!   Notes:

!   Changes:
!      NIH 08/09/99

! ----------------------- Declaration section ------------------------

!   Global variables
!      none

!   Event Name

      character EVENT_C_Balance*(*)
      parameter (EVENT_C_Balance='c_balance')

!   Event Data

      character DATA_dlt_OC *(*)
      parameter (DATA_dlt_OC = 'dlt_oc')
      character DATA_dlt_OM *(*)
      parameter (DATA_dlt_OM = 'dlt_om')

! ====================================================================
!      RESIDUE ADDED EVENT
! ====================================================================

!   Short description:
!      To notify all interested modules of the addition of residue to the
!        residue pool.

!   Notes:

!   Changes:
!      230999 jngh

! ----------------------- Declaration section ------------------------

!   Global variables
!      none

!   Event Name

      character EVENT_Residue_added*(*)
      parameter (EVENT_Residue_added='residue_added')

!   Event Data

      character DATA_residue_type *(*)
      parameter (DATA_residue_type = 'dlt_residue_type')
      character DATA_dlt_residue_wt *(*)
      parameter (DATA_dlt_residue_wt = 'dlt_residue_wt')

! ====================================================================
!      RESIDUE REMOVED EVENT
! ====================================================================

!   Short description:
!      To notify all interested modules of the removal of residue from the
!        residue pool.

!   Notes:

!   Changes:
!      230999 jngh

! ----------------------- Declaration section ------------------------

!   Global variables
!      none

!   Event Name

      character EVENT_Residue_removed*(*)
      parameter (EVENT_Residue_removed='residue_removed')

!   Event Data

      character DATA_residue_removed_action *(*)
      parameter (DATA_residue_removed_action = 'residue_removed_action')
      character DATA_dlt_residue_fraction *(*)
      parameter (DATA_dlt_residue_fraction = 'dlt_residue_fraction')
      character DATA_residue_incorp_fraction *(*)
      parameter (DATA_residue_incorp_fraction = 'residue_incorp_fract')

! ====================================================================
!      SURFACEOM REMOVED EVENT
! ====================================================================

!   Short description:
!      To notify all interested modules of the removal of residue from the
!        residue pool, other than by fraction.

!   Notes:

!   Changes:
!      230999 jngh

! ----------------------- Declaration section ------------------------

!   Global variables
!      none

!   Event Name

      character EVENT_SurfaceOM_removed*(*)
      parameter (EVENT_SurfaceOM_removed='surfaceom_removed')

!   Event Data

      character DATA_SurfaceOM_type *(*)
      parameter (DATA_SurfaceOM_type = 'surfaceom_type')
      character DATA_SurfaceOM_dm_type *(*)
      parameter (DATA_SurfaceOM_dm_type = 'dm_type')
      character DATA_dlt_SurfaceOM_wt *(*)
      parameter (DATA_dlt_SurfaceOM_wt = 'dlt_surfaceom_wt')
      character DATA_SurfaceOM_dlt_dm_n *(*)
      parameter (DATA_SurfaceOM_dlt_dm_n = 'dlt_dm_n')
      character DATA_SurfaceOM_dlt_dm_p *(*)
      parameter (DATA_SurfaceOM_dlt_dm_p = 'dlt_dm_p')

! ====================================================================
!      Crop Chopped EVENT
! ====================================================================

!   Short description:
!      To notify all interested modules of the removal of dry matter
!        from a crop.

!   Notes:

!   Changes:
!      230999 jngh

! ----------------------- Declaration section ------------------------

!   Global variables
!      none

!   Event Name

      character EVENT_Crop_chopped*(*)
      parameter (EVENT_Crop_chopped='crop_chopped')

!   Event Data

      character DATA_crop_type *(*)
      parameter (DATA_crop_type = 'crop_type')
      character DATA_dm_type *(*)
      parameter (DATA_dm_type = 'dm_type')
      character DATA_dlt_crop_dm *(*)
      parameter (DATA_dlt_crop_dm = 'dlt_crop_dm')
      character DATA_fraction_to_residue *(*)
      parameter (DATA_fraction_to_residue = 'fraction_to_residue')
      character DATA_dlt_dm_n *(*)
      parameter (DATA_dlt_dm_n = 'dlt_dm_n')
      character DATA_dlt_dm_cnr *(*)
      parameter (DATA_dlt_dm_cnr = 'dlt_dm_cnr')
      character DATA_dlt_dm_p *(*)
      parameter (DATA_dlt_dm_p = 'dlt_dm_p')
      character DATA_dlt_dm_cpr *(*)
      parameter (DATA_dlt_dm_cpr = 'dlt_dm_cpr')


! ====================================================================
!      New Profile Event
! ====================================================================

!   Short description:
!      To notify all interested modules of the change in soil water
!        profile characteristic information

!   Notes:

!   Changes:
!      150600 nih

! ----------------------- Declaration section ------------------------

!   Global variables
!      none

!   Event Name

      character EVENT_New_Profile*(*)
      parameter (EVENT_New_Profile='new_profile')

!   Event Data

      character DATA_dlayer *(*)
      parameter (DATA_dlayer = 'dlayer')
      character DATA_air_dry_dep *(*)
      parameter (DATA_air_dry_dep = 'air_dry_dep')
      character DATA_ll15_dep *(*)
      parameter (DATA_ll15_dep = 'll15_dep')
      character DATA_dul_dep *(*)
      parameter (DATA_dul_dep = 'dul_dep')
      character DATA_sat_dep *(*)
      parameter (DATA_sat_dep = 'sat_dep')
      character DATA_sw_dep *(*)
      parameter (DATA_sw_dep = 'sw_dep')
      character DATA_bd *(*)
      parameter (DATA_bd = 'bd')


! ====================================================================
!      Water Given EVENT
! ====================================================================

!   Short description:
!      To notify all interested modules of the transfer of water between sources

!   Notes:

!   Changes:
!      100603  dsg

! ----------------------- Declaration section ------------------------

!   Global variables
!      none

!   Event Name

      character EVENT_Water_given*(*)
      parameter (EVENT_Water_given='water_given')

!   Event Data

      character DATA_water_supplied *(*)
      parameter (DATA_water_supplied = 'water_supplied')

!     Solute information will also be included in this event at a future point



end module ConstantsModule

