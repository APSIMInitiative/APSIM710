*     ========================================
      module MicrometModule
*     ========================================
      use Registrations

      real       svp_A
      parameter (svp_A = 6.106)            ! Teten coefficients

      real       svp_B
      parameter (svp_B = 17.27)            ! Teten coefficients

      real       svp_C
      parameter (svp_C = 237.3)            ! Teten coefficients

      real       abs_temp              ! 0 C in Kelvin (g_k)
      parameter (abs_temp = 273.16)

      real       r_gas                 ! universal gas constant (J/mol/K)
      parameter (r_gas = 8.3143)

      real       mwh2o                 ! molecular weight water (kg/mol)
      parameter (mwh2o = 0.018016)

      real       mwair                 ! molecular weight air (kg/mol)
      parameter (mwair = 0.02897)

      real       molef                 ! molecular fraction of water to air ()
      parameter (molef = mwh2o/mwair)

      real       Cp                    ! Specific heat of air at constant pressure
      parameter (Cp = 1010.0)          ! (J/kg/K)

      real        stef_boltz              !Stefan-Boltzman constant
      parameter   (stef_boltz = 5.67e-8)

      real        pi                   !value of pi
      parameter   (pi = 3.1459)

      real        c_cloud              !constant for cloud effect on longwave radiation
      parameter   (c_cloud = 0.1)

      real        Deg2Rad              !convert degrees to radians
      parameter   (Deg2Rad = 3.1458 / 180.)

      real RhoW
      parameter (RhoW = 998.0) !kg/m3

      real svp_fract                   !weights vpd towards vpd at maximum temperature
      parameter (svp_fract =  0.66)

      integer max_components
      parameter (max_components = 10)

      integer max_layer
      parameter (max_layer = 2*max_components - 1)

      integer max_table
      parameter (max_table = 10)

      real SunSetAngle
      parameter (SunSetAngle = 0.0)

*     ========================================
      Type MicrometGlobals
         sequence
         integer NumComponents
         character ComponentName(max_components)*32
         character ComponentType(max_components)*32
         real      ComponentLAI(max_components)
         real      ComponentLAItot(max_components)
         real      ComponentCoverGreen(max_components)
         real      ComponentCoverTot(max_components)
         real      ComponentKtot(max_components)
         real      ComponentK(max_components)
         real      ComponentHeight(max_components)
         real      ComponentDepth(max_components)
         real      ComponentAlbedo(max_components)
         real      ComponentEmissivity(max_components)
         real      ComponentGsmax(max_components)
         real      ComponentR50(max_components)
         real      ComponentFrgr(max_components)

         real      DeltaZ(max_layer)
         integer   NumLayers

         real      LayerKtot(max_layer)

         real      LAI(max_layer,max_components)
         real      LAItot(max_layer,max_components)
         real      Ftot(max_layer,max_components)
         real      Fgreen(max_layer,max_components)
         real      Rs(max_layer,max_components)
         real      Rl(max_layer,max_components)
         real      Rsoil(max_layer,max_components)
         real      Gc(max_layer,max_components)
         real      Ga(max_layer,max_components)
         real      PET(max_layer,max_components)
         real      PETr(max_layer,max_components)
         real      PETa(max_layer,max_components)
         real      Omega(max_layer,max_components)
         real      Interception(max_layer,max_components)
         real      albedo
         real      Emissivity


         real      radn
         real      maxt
         real      mint
         real      rain
         real      vp
         real      windspeed

         integer   day
         integer   year
         real      latitude
         real      AverageT
         real      SunshineHours
         real    FractionClearSky
         real      DayLength
         real      DayLengthLight
         real    Net_Long_Wave
         real    SoilHeat
         real    DryLeafFraction

      end type MicrometGlobals
*     ========================================
      Type MicrometParameters
         sequence
         real      soil_albedo

         real      A_interception
         real      B_interception
         real      C_interception
         real      D_interception
      end type MicrometParameters
*     ========================================
      Type MicrometConstants
         sequence
         real air_pressure
         real soil_emissivity
         real sun_angle
         real soil_heat_flux_fraction
         real night_interception_fraction
         real windspeed_default
         real RefHeight

      end type MicrometConstants
*     ========================================
      ! instance variables.
      common /InstancePointers/ ID,g,p,c
      save InstancePointers
      type (MicrometGlobals),pointer :: g
      type (MicrometParameters),pointer :: p
      type (MicrometConstants),pointer :: c
      type (IDsType), pointer :: id


      contains



* ====================================================================
       subroutine Micromet_Create ()
* ====================================================================
      Use Infrastructure
      implicit none


*+  Purpose
*      Zero everything

*+  Mission Statement
*     Zero Everything

*+  Changes
*     NIH 12/3/03 Specified

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_create')

*+  Local Variables


*- Implementation Section ----------------------------------

      call push_routine (myname)

      call Micromet_zero_variables ()

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine Micromet_Init ()
* ====================================================================

      Use Infrastructure
      implicit none


*+  Purpose
*      Initialise Micromet module

*+  Mission Statement
*     Initialise all internal state variables

*+  Changes
*     NIH 28/3/00 Specified

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_init')

*+  Local Variables
       character Event_string*40       ! String to output
       integer   numvals               ! number of values returned

*- Implementation Section ----------------------------------

      call push_routine (myname)

      ! Notify system that we have initialised

      Event_string = 'Initialising'
      call Write_string (Event_string)

      ! Get all parameters from parameter file

      call Micromet_read_constants ()

      call Micromet_read_param ()

      call get_real_var (unknown_module, 'latitude', '(deg)', 
     : g%latitude, numvals, -60.0,  60.0)

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine Micromet_zero_variables ()
* ====================================================================

      Use Infrastructure
      implicit none

*+  Purpose
*     Set all variables to initial state.  i.e. zero or blank.

*+  Mission Statement
*     Set internal state variables to zero

*+  Changes
*     NIH 28/3/00 Specified

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_zero_variables')

*+  Local Variables

*- Implementation Section ----------------------------------

      call push_routine (myname)

      g%NumComponents = 0
      g%ComponentName(:) = ' '
      g%ComponentType(:) = ' '
      g%ComponentLAI(:) = 0.0
      g%ComponentLAItot(:) = 0.0
      g%ComponentCoverGreen(:) = 0.0
      g%ComponentCoverTot(:) = 0.0
      g%ComponentK(:) = 0.0
      g%ComponentHeight(:) = 0.0
      g%ComponentDepth(:) = 0.0
      g%ComponentGsmax(:) = 0.0
      g%ComponentR50(:) = 0.0
      g%ComponentAlbedo(:) = 0.0
      g%ComponentEmissivity(:) = 0.0
      g%ComponentFrgr(:) = 0.0

      g%DeltaZ(:) = 0.0
      g%NumLayers = 0

      g%LayerKtot(:) = 0.0

      g%LAI(:,:) = 0.0
      g%LAItot(:,:) = 0.0
      g%Ftot(:,:) = 0.0
      g%Fgreen(:,:) = 0.0
      g%Rs(:,:) = 0.0
      g%Rl(:,:) = 0.0
      g%Rsoil(:,:) = 0.0
      g%Gc(:,:) = 0.0
      g%Ga(:,:) = 0.0
      g%PET(:,:) = 0.0
      g%PETr(:,:) = 0.0
      g%PETa(:,:) = 0.0
      g%Omega(:,:) = 0.0
      g%Interception(:,:) = 0.0
      g%albedo = 0.0
      g%Emissivity = 0.0
      g%Net_Long_Wave = 0.0

      g%radn = 0.0
      g%maxt = 0.0
      g%mint = 0.0
      g%rain = 0.0
      g%vp = 0.0

      g%latitude = 0.0
      g%day = 0
      g%year = 0
      g%AverageT = 0.0
      g%SunshineHours = 0.0
      g%DayLength = 0.0
      g%DryLeafFraction = 0.0


      c%air_pressure = 0.0
      c%soil_emissivity = 0.0

      p%soil_albedo = 0.0

      p%A_interception = 0.0
      p%B_interception = 0.0
      p%C_interception = 0.0
      p%D_interception = 0.0

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine Micromet_Send_my_variable (Variable_name)
* ====================================================================

      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
       character Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Purpose
*       Return the value of one of our variables to caller.

*+  Mission Statement
*     Supply information to requesting module

*+  Changes
*     NIH 28/3/00 Specified

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_send_my_variable')

*+  Local Variables
      real total_lai
      real total_interception
      integer i
      integer j
      real    net_radn
      
      integer ComponentNo

*- Implementation Section ----------------------------------

      call push_routine (myname)



CCC  VOS added 4 April 2011 - needed as part of system to allow crops to change their gsmax for climate change work
CCC
      if (index(Variable_name,'gsmax_').eq.1) then
         ComponentNo = position_in_char_array
     :                   (Variable_name(7:)
     :                   ,g%ComponentName
     :                   ,g%NumComponents)

         if (ComponentNo.eq.0) then
            call fatal_Error(ERR_Internal
     :                 ,'Unknown Canopy Component: '//Variable_name)
         else
            call respond2get_real_var (
     :               Variable_name,
     :               'm/s',
     :               g%ComponentGsmax(ComponentNo))
         endif
CCC End new code

      elseif (Variable_name.eq.'interception') then

      Total_Interception = 0.0

      do 200 i = 1, g%NumLayers
         do 100 j = 1, g%NumComponents
            Total_Interception = Total_Interception
     :                         + g%Interception(i,j)
  100    continue
  200 continue

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'mm'                ! variable units
     :              ,Total_Interception) ! variable

!      elseif
!
!      call collect_char_var (DATA_sender
!     :                      ,'()'
!     :                      ,sender
!     :                      ,numvals)
!
!      ComponentNo = position_in_char_array
!     :                   (sender
!     :                   ,g%ComponentName
!     :                   ,g%NumComponents)
!
!      if (ComponentNo.eq.0) then
!         call fatal_Error(ERR_Internal
!     :                   ,'Unknown Canopy Component: '//sender)
!

      elseif (Variable_name.eq.'gc') then

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'()'              ! variable units
     :              ,g%gc(1,1)) ! variable

      elseif (Variable_name.eq.'ga') then

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'()'              ! variable units
     :              ,g%ga(1,1)) ! variable

      elseif (Variable_name.eq.'petr') then

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'()'              ! variable units
     :              ,sum(g%petr(:,:))) ! variable

      elseif (Variable_name.eq.'peta') then

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'()'              ! variable units
     :              ,sum(g%peta(:,:))) ! variable

      elseif (Variable_name.eq.'net_radn') then

         net_radn = g%radn * (1.0 - g%albedo)
     :            + g%Net_Long_Wave

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'()'              ! variable units
     :              ,net_radn) ! variable

      elseif (Variable_name.eq.'net_rs') then

         net_radn = g%radn * (1.0 - g%albedo)

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'()'              ! variable units
     :              ,net_radn) ! variable

      elseif (Variable_name.eq.'net_rl') then

         net_radn = g%Net_Long_Wave

         call respond2get_real_var (
     :               variable_name       ! variable name
     :              ,'()'              ! variable units
     :              ,net_radn) ! variable

      elseif (Variable_name.eq.'soil_heat') then

         call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'()'              ! variable units
     :              ,g%SoilHeat)       ! variable

      elseif (Variable_name.eq.'dryleaffraction') then

         call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'()'              ! variable units
     :              ,g%DryLeafFraction)       ! variable

      else
         call Message_Unused ()
      endif

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine Micromet_set_my_variable (Variable_name)
* ====================================================================

      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name to search for

*+  Purpose
*     Set one of our variables altered by some other module

*+  Changes
*    040411  VOS added to allow user to reset gsmaxfor crop for climate change simulations

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'Micromet_set_my_variable')

*+  Local Variables
      integer numvals                  ! number of values returned
      integer ComponentNo              ! component number
      real*4 ub                        ! upper bound for the change
      real*4 lb                        ! lower bound for the change
      real*4 dlt_gsmax                 ! temporary variable to hold the change

*- Implementation Section ----------------------------------
      call push_routine (myname)


      if (index(Variable_name,'dlt_gsmax_').eq.1) then
         ComponentNo = position_in_char_array
     :                   (Variable_name(11:)
     :                   ,g%ComponentName
     :                   ,g%NumComponents)

         if (ComponentNo.eq.0) then
            call fatal_Error(ERR_Internal
     :                 ,'Unknown Canopy Component: '//Variable_name)
         else
            lb = -0.9 * g%ComponentGsmax(ComponentNo)
            ub = 0.9 * g%ComponentGsmax(ComponentNo)
            call collect_real_var (
     :             variable_name        ! variable name
     :            ,'m/s'                ! units
     :            ,dlt_gsmax            ! variable
     :            ,numvals              ! number of elements returned
     :            ,lb                   ! lower limit for bound check
     :            ,ub)                  ! upper limit for bound check
            if (dlt_gsmax .gt. ub) then    !need to cause a fatal not warning error
               call Fatal_Error (err_internal,'dlt_gsmax too high')
            elseif (dlt_gsmax .lt. lb) then
               call Fatal_Error (err_internal,'dlt_gsmax too low')
            endif
            g%ComponentGsmax(ComponentNo) = 
     :                       g%ComponentGsmax(ComponentNo) + dlt_gsmax
         endif

      else
         ! Don't know this variable name
         call Message_Unused ()
      endif

      call pop_routine (myname)
      return
      end subroutine


*     ===========================================================
      subroutine Micromet_read_param ()
*     ===========================================================

      Use Infrastructure
      implicit none

*+  Purpose
*       Read in all parameters from parameter file.

*+  Mission Statement
*     Read parameters from parameter file

*+  Changes
*     NIH 28/3/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_read_param')
*
      character section_name*(*)
      parameter (section_name = 'parameters')

*+  Local Variables
      integer    numvals               ! number of values read

*- Implementation Section ----------------------------------

      call push_routine (myname)

      call write_string (new_line//'   - Reading Parameters')

      call read_real_var (
     :        section_name,          ! Section header
     :        'soil_albedo',         ! Keyword
     :        '()',                  ! Units
     :        p%soil_albedo,         ! Parameter
     :        numvals,               ! Number of values returned
     :        0.0,                   ! Lower Limit for bound checking
     :        1.0)                   ! Upper Limit for bound checking


      call read_real_var_optional (
     :        section_name,          ! Section header
     :        'a_interception',      ! Keyword
     :        'mm/mm',               ! Units
     :        p%a_interception,      ! Parameter
     :        numvals,               ! Number of values returned
     :        0.0,                   ! Lower Limit for bound checking
     :        10.0)                  ! Upper Limit for bound checking
      if(numvals.eq.0) then
         p%a_interception = 0.0
      else
      endif

      call read_real_var_optional (
     :        section_name,          ! Section header
     :        'b_interception',      ! Keyword
     :        '()',                  ! Units
     :        p%b_interception,      ! Parameter
     :        numvals,               ! Number of values returned
     :        0.0,                   ! Lower Limit for bound checking
     :        5.0)                   ! Upper Limit for bound checking
      if(numvals.eq.0) then
         p%b_interception = 1.0
      else
      endif

      call read_real_var_optional (
     :        section_name,          ! Section header
     :        'c_interception',      ! Keyword
     :        'mm',                  ! Units
     :        p%c_interception,      ! Parameter
     :        numvals,               ! Number of values returned
     :        0.0,                   ! Lower Limit for bound checking
     :        10.0)                  ! Upper Limit for bound checking
      if(numvals.eq.0) then
         p%c_interception = 0.0
      else
      endif

      call read_real_var_optional (
     :        section_name,          ! Section header
     :        'd_interception',      ! Keyword
     :        'mm',                  ! Units
     :        p%d_interception,      ! Parameter
     :        numvals,               ! Number of values returned
     :        0.0,                   ! Lower Limit for bound checking
     :        20.0)                  ! Upper Limit for bound checking
      if(numvals.eq.0) then
         p%d_interception = 0.0
      else
      endif

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine Micromet_read_constants ()
* ====================================================================

      Use Infrastructure
      implicit none

*+  Purpose
*      Read in all constants from ini file.

*+  Mission Statement
*     Read constants from ini file

*+  Changes
*     NIH 28/3/00 Specified

*+  Constant Values
      character*(*) section_name
      parameter (section_name = 'constants')
*
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Micromet_read_constants')

*+  Local Variables
      integer    numvals               ! number of values read from file

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call write_string (new_line//'   - Reading Constants')

      call read_real_var (
     :           section_name         ! Section header
     :         , 'air_pressure'       ! Keyword
     :         , 'hPa'  	             ! Units
     :         , c%air_pressure       ! Variable
     :         , numvals              ! Number of values returned
     :         , 900.                 ! Lower Limit for bound checking
     :         , 1100.)               ! Upper Limit for bound checking

      call read_real_var (
     :           section_name         ! Section header
     :         , 'soil_emissivity'    ! Keyword
     :         , '()'                 ! Units
     :         , c%soil_emissivity    ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.9                  ! Lower Limit for bound checking
     :         , 1.0)                 ! Upper Limit for bound checking

      call read_real_var (
     :           section_name         ! Section header
     :         , 'sun_angle'    ! Keyword
     :         , '()'                 ! Units
     :         , c%sun_angle    ! Variable
     :         , numvals              ! Number of values returned
     :         , -20.0                  ! Lower Limit for bound checking
     :         , 20.0)                 ! Upper Limit for bound checking

      call read_real_var (
     :           section_name         ! Section header
     :         , 'soil_heat_flux_fraction'    ! Keyword
     :         , '()'                 ! Units
     :         , c%soil_heat_flux_fraction    ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1.0)                ! Upper Limit for bound checking

      call read_real_var (
     :           section_name         ! Section header
     :         , 'night_interception_fraction'    ! Keyword
     :         , '()'                 ! Units
     :         , c%night_interception_fraction    ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1.0)                ! Upper Limit for bound checking

      call read_real_var (
     :           section_name         ! Section header
     :         , 'windspeed_default'    ! Keyword
     :         , 'm/s'                  ! Units
     :         , c%windspeed_default    ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 10.0)                ! Upper Limit for bound checking

      call read_real_var (
     :           section_name         ! Section header
     :         , 'refheight'    ! Keyword
     :         , 'm'                  ! Units
     :         , c%RefHeight    ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 10.0)                ! Upper Limit for bound checking

      call pop_routine (myname)
      return
      end subroutine


*     ===========================================================
      subroutine Micromet_OnNewCrop (variant)
*     ===========================================================

      Use Infrastructure
      implicit none

      integer, intent(in) :: variant

*+  Purpose
*       Register presence of a new crop

*+  Mission Statement
*       Register presence of a new crop

*+  Changes
*     NIH 30/3/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_OnNewCrop')

*+  Local Variables
      type(NewCropType) :: new_crop

      integer    numvals               ! number of values read
      integer    i
      integer    senderIdx
*- Implementation Section ----------------------------------

      call push_routine (myname)

      call unpack_newcrop(variant, new_crop)

      ! See if we know of it
      senderIdx = 0
      do 100 i = 1, g%NumComponents
        if (g%ComponentName(i).eq.Trim(new_crop%sender))
     :            senderIdx = i
 100  continue

      ! If sender is unknown, add it to the list
      if (senderIdx.eq.0) then
        g%NumComponents = g%NumComponents + 1
        if (g%NumComponents.gt.max_components) then
          call fatal_Error(ERR_Internal
     :                   ,'Too many canopy components in system')
        else
          g%ComponentName(g%NumComponents) = Trim(new_crop%sender)
          g%ComponentType(g%NumComponents) = new_crop%crop_type
          i = add_registration_with_units(respondToGetReg, 
     :        'gsmax_'//new_crop%sender, floatTypeDDML, 'm/s')
          i = add_registration_with_units(respondToSetReg, 
     :        'dlt_gsmax_'//new_crop%sender, floatTypeDDML, 'm/s')

!      print*,'there'
          ! Read Component Specific Constants
          ! ---------------------------------
          call micromet_component_constants(g%NumComponents)
        endif
      else ! update crop type information
        if (g%ComponentType(senderIdx).ne.new_crop%crop_type) then
          g%ComponentType(senderIdx) = new_crop%crop_type
          call micromet_component_constants(senderIdx)		  
        endif
      endif

!      print*,'end'
      call pop_routine (myname)
      return
      end subroutine

*     ===========================================================
      subroutine Micromet_OnNewCanopy (variant)
*     ===========================================================

      Use Infrastructure
      implicit none
      integer, intent(in) :: variant

*+  Purpose
*       Obtain updated information about a plant canopy

*+  Mission Statement
*       Obtain updated information about a plant canopy

*+  Changes
*     NIH 30/3/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_OnNewCanopy')

*+  Local Variables
      type(newcanopyType) :: newcanopy
      integer    numvals               ! number of values read
      character  sender*32
      integer    ComponentNo

*- Implementation Section ----------------------------------

      call push_routine (myname)

      call unpack_newcanopy(variant, newcanopy)

      ComponentNo = position_in_char_array
     :                   (newcanopy%sender
     :                   ,g%ComponentName
     :                   ,g%NumComponents)

      if (ComponentNo.eq.0) then
         call fatal_Error(ERR_Internal
     :                 ,'Unknown Canopy Component: '//newcanopy%sender)

      else

        g%ComponentLAI(ComponentNo) = newcanopy%lai
        g%ComponentLAItot(ComponentNo) = newcanopy%lai_tot
        g%ComponentCoverGreen(ComponentNo) = newcanopy%cover
        g%ComponentCoverTot(ComponentNo) = newcanopy%cover_tot
        g%ComponentHeight(ComponentNo) = newcanopy%height
        g%ComponentHeight(ComponentNo) = g%ComponentHeight(ComponentNo)
     :                         / 1000.  ! to convert from mm to m
        g%ComponentDepth(ComponentNo) = newcanopy%depth
        g%ComponentDepth(ComponentNo) = g%ComponentDepth(ComponentNo)
     :                         / 1000.  ! to convert from mm to m

      endif
      !call Micromet_Canopy_Compartments ()

      call pop_routine (myname)
      return
      end subroutine

*     ===========================================================
      subroutine Micromet_Prepare ()
*     ===========================================================

      Use Infrastructure
      implicit none

*+  Purpose
*       Perform Prepare Phase Calculations

*+  Mission Statement
*       Perform Prepare Phase Calculations

*+  Changes
*     NIH 30/3/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_Prepare')

*+  Local Variables

*- Implementation Section ----------------------------------

      call push_routine (myname)

      call Micromet_Met_Variables ()
      call Micromet_Canopy_Compartments ()
      call Micromet_Canopy_Energy_Balance ()

      call pop_routine (myname)
      return
      end subroutine

*     ===========================================================
      subroutine Micromet_Process ()
*     ===========================================================

      Use Infrastructure
      implicit none

*+  Purpose
*       Perform Process Phase Calculations

*+  Mission Statement
*       Perform Process Phase Calculations

*+  Changes
*     NIH 30/3/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_Process')

*+  Local Variables

*- Implementation Section ----------------------------------

      call push_routine (myname)

      call Micromet_Calculate_Gc ()
      call Micromet_Calculate_Ga ()
      call Micromet_Calculate_Interception ()
      call Micromet_Calculate_PM()
      call Micromet_Calculate_Omega()


      call Micromet_Energy_Balance_Event()
      call Micromet_Water_Balance_Event()

      call pop_routine (myname)
      return
      end subroutine

*     ===========================================================
      subroutine Micromet_Canopy_Compartments ()
*     ===========================================================

      Use Infrastructure
      implicit none

*+  Purpose
*       Break the combined Canopy into functional compartments

*+  Mission Statement
*       Break the combined Canopy into functional compartments

*+  Changes
*     NIH 30/3/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_Canopy_Compartments')

*+  Local Variables

*- Implementation Section ----------------------------------

      call push_routine (myname)

      Call Micromet_Define_Layers ()

      Call Micromet_Divide_Components ()

      Call Micromet_Light_Extinction()

      call pop_routine (myname)
      return
      end subroutine

*     ===========================================================
      subroutine Micromet_Define_Layers ()
*     ===========================================================

      Use Infrastructure
      implicit none

*+  Purpose
*       Break the combined Canopy into layers

*+  Mission Statement
*       Break the combined Canopy into layers

*+  Changes
*     NIH 30/3/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_Define_Layers')

*+  Local Variables
      real nodes(2*max_components - 1)
      integer NumNodes
      integer ComponentNo
      integer Node
      real    CanopyBase
      integer key(2*max_components - 1)

*- Implementation Section ----------------------------------

      call push_routine (myname)

      nodes(:) = 0.0
      NumNodes = 1

      do 100 ComponentNo = 1, g%NumComponents
         if (position_in_real_array(g%ComponentHeight(ComponentNo)
     :                             ,Nodes
     :                             ,NumNodes)
     :       .eq.0) then
            NumNodes = NumNodes + 1
            Nodes(NumNodes) = g%ComponentHeight(ComponentNo)

         else
            ! it is already there - ignore it
         endif

         CanopyBase = g%ComponentHeight(ComponentNo)
     :              - g%ComponentDepth(ComponentNo)

         if (position_in_real_array(CanopyBase
     :                             ,Nodes
     :                             ,NumNodes)
     :       .eq.0) then
            NumNodes = NumNodes + 1
            Nodes(NumNodes) = CanopyBase
         else
            ! it is already there - ignore it
         endif

  100 continue

      ! Sort into Ascending order
      call shell_sort_real (Nodes,NumNodes,key)

      g%NumLayers = NumNodes - 1

      do 200 Node = 1, NumNodes - 1
         g%DeltaZ(Node) = Nodes(Node+1) - Nodes(Node)
  200 continue

      call pop_routine (myname)
      return
      end subroutine


*     ===========================================================
      subroutine Micromet_Divide_Components ()
*     ===========================================================

      Use Infrastructure
      implicit none

*+  Purpose
*       Break the components into layers

*+  Mission Statement
*       Break the components into layers

*+  Changes
*     NIH 30/3/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_Divide_Components')

*+  Local Variables
      real Ld (max_components)
      real top
      real bottom
      integer i
      integer j

*- Implementation Section ----------------------------------

      call push_routine (myname)

      g%LAI(:,:) = 0.0
      g%LAItot(:,:) = 0.0

      do 100 j = 1, g%NumComponents
         Ld(j) = divide(g%ComponentLAItot(j)
     :                 ,g%ComponentDepth(j)
     :                 ,0.0)
  100 continue

      top = 0.0
      bottom = 0.0
      do 300 i = 1, g%NumLayers
         bottom = top
         top = top + g%DeltaZ(i)

         ! Calculate LAI for layer i and component j
         ! =========================================

         do 200 j = 1, g%NumComponents
            if ((g%ComponentHeight(j).gt.bottom)
     :                 .and.
     :          (g%ComponentHeight(j)-g%ComponentDepth(j).lt.top))then
               g%LAItot(i,j) = Ld(j) * g%DeltaZ(i)
               g%LAI(i,j) = g%LAItot(i,j)
     :             * divide(g%ComponentLAI(j)
     :                     ,g%ComponentLAItot(j)
     :                     ,0.0)
            else
               ! This component is not in this layer
            endif

  200    continue

         ! Calculate fractional contribution for layer i and component j
         ! =============================================================

         do 250 j = 1, g%NumComponents
            g%Ftot(i,j) = divide(g%LAItot(i,j)
     :                       ,sum(g%LAItot(i,1:g%NumComponents))
     :                       ,0.0)
            ! Note: Sum of Fgreen will be < 1 as it is green over total
            g%Fgreen(i,j) = divide(g%LAI(i,j)
     :                       ,sum(g%LAItot(i,1:g%NumComponents))
     :                       ,0.0)
  250    continue

  300 continue

      call pop_routine (myname)
      return
      end subroutine

*     ===========================================================
      subroutine Micromet_Table (Title,Array)
*     ===========================================================

      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
      character Title*(*)
      real      Array(1:max_layer,1:max_components)

*+  Purpose
*       Print out a 2-Dimensional table for a given state variable

*+  Mission Statement
*       Print out a 2-Dimensional table for a given state variable

*+  Changes
*     NIH 30/3/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_Table')

*+  Local Variables
      integer i
      integer j
      character string*200
      character line_rule*200
      real      top
      real      bottom

*- Implementation Section ----------------------------------

      call push_routine (myname)

      write(line_rule,'(5x,70(''-''))')


      call write_string('Table for '//Title)

      call write_string(Line_Rule)

      write(string,'(5x,a,4x,11a10)')
     :       'Canopy Layer Height'
     :      ,(g%ComponentName(j),j=1,g%NumComponents)
     :      ,'Total'
      call write_string(String)

      call write_string(Line_Rule)

      do 100 i = g%NumLayers, 1, -1
         top = sum(g%DeltaZ(1:i))
         if (i.eq.1) then
            bottom = 0.0
         else
            bottom = top - g%DeltaZ(i)
         endif

         write(string,'(x,f7.3,'' - '',f7.3,5x,11f10.3)')
     :              bottom
     :           ,  top
     :           , (array(i,j),j=1,g%NumComponents)
     :           , sum(array(i,1:g%NumComponents))

         call write_string(String)

  100 continue

      call write_string(Line_Rule)

         write(string,'(x,''       Total     '',5x,11f10.3)')
     :            (sum(array(1:g%NumLayers,j)),j=1,g%NumComponents)
     :            ,sum(array(:,:))
         call write_string(String)

      call write_string(Line_Rule)


      call pop_routine (myname)
      return
      end subroutine

*     ===========================================================
      subroutine Micromet_Light_Extinction ()
*     ===========================================================

      Use Infrastructure
      implicit none

*+  Purpose
*       Calculate light extinction parameters

*+  Mission Statement
*       Calculate light extinction parameters

*+  Changes
*     NIH 30/3/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_Light_Extinction')

*+  Local Variables
      integer i
      integer j

*- Implementation Section ----------------------------------

      call push_routine (myname)

         ! Calculate effective K from LAI and Cover
         ! ========================================


      do 100 j = 1, g%NumComponents
         if (reals_are_equal(g%ComponentCoverGreen(j), 
     :                       1.0, 0.00001)) then
            call fatal_Error(ERR_Internal
     :  ,'Unrealistically high cover value in Micromet i.e. > 0.9999')
         endif
         g%ComponentK(j) = divide(-log(1.-g%ComponentCoverGreen(j))
     :                           ,g%ComponentLAI(j)
     :                           ,0.0)
         g%ComponentKtot(j) = divide(-log(1.-g%ComponentCoverTot(j))
     :                           ,g%ComponentLAItot(j)
     :                           ,0.0)
  100 continue

         ! Calculate extinction for individual layers
         ! ==========================================


      do 200 i = 1, g%NumLayers

         g%LayerKtot(i) = Sum(g%Ftot(i,1:g%NumComponents)
     :                     * g%ComponentKtot(1:g%NumComponents))

  200 continue

      call pop_routine (myname)
      return
      end subroutine

*     ===========================================================
      subroutine Micromet_Canopy_Energy_Balance ()
*     ===========================================================

      Use Infrastructure
      implicit none

*+  Purpose
*       Perform the Overall Canopy Energy Balance

*+  Mission Statement
*       Perform the Overall Canopy Energy Balance

*+  Changes
*     NIH 30/3/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_Canopy_Energy_Balance')

*+  Local Variables

*- Implementation Section ----------------------------------

      call push_routine (myname)

      Call Micromet_short_wave_radiation ()
      Call Micromet_Energy_Terms ()
      Call Micromet_Long_Wave_Radiation ()
      Call Micromet_SoilHeat_radiation ()

      call pop_routine (myname)
      return
      end subroutine

*     ===========================================================
      subroutine Micromet_short_wave_radiation ()
*     ===========================================================

      Use Infrastructure
      implicit none

*+  Purpose
*       Calculate interception of short wave by canopy compartments

*+  Mission Statement
*       Calculate interception of short wave by canopy compartments

*+  Changes
*     NIH 30/3/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_short_wave_radiation')

*+  Local Variables
      integer i
      integer j
      real    Rin
      real    Rint
*- Implementation Section ----------------------------------

      call push_routine (myname)

         ! Perform Top-Down Light Balance
         ! ==============================

      Rin = g%Radn

      do 200 i = g%NumLayers,1,-1

         Rint = Rin
     :        * (1. - exp(-g%LayerKtot(i)
     :                    *sum(g%LAItot(i,1:g%NumComponents))))

         do 100 j = 1, g%NumComponents
            g%Rs(i,j) = Rint
     :                * divide(g%Ftot(i,j)*g%ComponentKtot(j)
     :                        ,g%LayerKtot(i)
     :                        ,0.0)
  100    continue

         Rin = Rin - Rint

  200 continue


      call pop_routine (myname)
      return
      end subroutine


*     ===========================================================
      subroutine Micromet_OnNewMet (variant)
*     ===========================================================

      Use Infrastructure
      implicit none

      integer, intent(in) :: variant

*+  Purpose
*       Obtain all relevant met data

*+  Mission Statement
*       Obtain all relevant met data

*+  Changes
*     NIH 30/3/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_OnNewMet')

*+  Local Variables
      type(newmetType) :: newmet


*- Implementation Section ----------------------------------

      call push_routine (myname)

      call unpack_newmet(variant, newmet)
      g%radn = newmet%radn
      g%maxt = newmet%maxt
      g%mint = newmet%mint
      g%rain = newmet%rain
      g%vp   = newmet%vp

      call pop_routine (myname)
      return
      end subroutine

*     ===========================================================
      subroutine Micromet_Long_Wave_Radiation ()
*     ===========================================================

      Use Infrastructure
      implicit none

*+  Purpose
*       Calculate Net Long Wave Radiation Balance

*+  Mission Statement
*       Calculate Net Long Wave Radiation Balance

*+  Changes
*     NIH 30/3/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_Long_Wave_Radiation')

*+  Local Variables
      integer i
      integer j


*- Implementation Section ----------------------------------

      call push_routine (myname)



      g%Net_Long_Wave = micromet_longwave(g%AverageT
     :                                   ,g%FractionClearSky
     :                                   ,g%Emissivity)
     :              * g%DayLength * hr2s / 1.0e6  ! W to MJ


         ! Long Wave Balance Proportional to Short Wave Balance
         ! ====================================================

      do 200 i = g%NumLayers,1,-1


         do 100 j = 1, g%NumComponents
            g%Rl(i,j) = divide(g%Rs(i,j)
     :                        ,g%Radn
     :                        ,0.0)
     :                * g%Net_Long_Wave
  100    continue


  200 continue

      call pop_routine (myname)
      return
      end subroutine

*     ===========================================================
      subroutine Micromet_SoilHeat_Radiation ()
*     ===========================================================

      Use Infrastructure
      implicit none

*+  Purpose
*       Calculate Radiation loss to soil heating

*+  Mission Statement
*       Calculate Radiation loss to soil heating

*+  Changes
*     NIH 30/3/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_SoilHeat_Radiation')

*+  Local Variables
      integer i
      integer j

      real radnint ! Intercepted SW radiation
*- Implementation Section ----------------------------------

      call push_routine (myname)

      RadnInt = sum(g%Rs(:,:))

      g%SoilHeat = micromet_Soil_Heat_Flux(g%radn
     :                                  ,RadnInt
     :                                  ,c%soil_heat_flux_fraction)

!      g%SoilHeat = -0.1
!     :           * ((1. - g%Albedo) * g%Radn
!     :               + g%Net_Long_Wave)

         ! SoilHeat Balance Proportional to Short Wave Balance
         ! ====================================================

      do 200 i = g%NumLayers,1,-1


         do 100 j = 1, g%NumComponents
            g%Rsoil(i,j) = divide(g%Rs(i,j)
     :                          ,g%Radn
     :                          ,0.0)
     :                * g%SoilHeat
  100    continue


  200 continue

      call pop_routine (myname)
      return
      end subroutine

*     ===========================================================
      subroutine Micromet_Met_Variables ()
*     ===========================================================

      Use Infrastructure
      implicit none

*+  Purpose
*       Calculate Daily Met Variables

*+  Mission Statement
*       Calculate Daily Met Variables

*+  Changes
*     NIH 30/3/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_Met_Variables')


*+  Local Variables
      real DayLengthLight

*- Implementation Section ----------------------------------

      call push_routine (myname)

      !g%AverageT = (g%maxt + g%mint)/2.0
      g%AverageT = micromet_AverageT(g%mint,g%maxt)

      ! This is the length of time within the day during which
      ! Evaporation will take place
      g%Daylength = Micromet_DayLength(g%latitude,g%day,c%Sun_Angle)



      ! This is the length of time within the day during which
      ! the sun is above the horizon
      g%DaylengthLight
     :         = Micromet_DayLength(g%latitude,g%day,SunSetAngle)


      g%SunshineHours = Micromet_Sunshine_Hours(g%Radn
     :                                         ,g%DayLengthLight
     :                                         ,g%latitude
     :                                         ,g%day)


      g%FractionClearSky = divide(g%SunshineHours
     :                         ,g%DayLengthLight
     :                         ,0.0)

      call pop_routine (myname)
      return
      end subroutine

*     ===========================================================
      subroutine micromet_ONtick (variant)
*     ===========================================================

      Use Infrastructure
      implicit none

      integer, intent(in) :: variant

*+  Purpose
*     Update internal time record and reset daily state variables.

*+  Mission Statement
*     Update internal time record and reset daily state variables.

*+  Changes
*        140400 nih

*+  Local Variables
      type(timeType) :: tick

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'micromet_ONtick')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      call unpack_time(variant, tick)
      call jday_to_day_of_year(dble(tick%startday), g%day, g%year)

      call pop_routine (myname)
      return
      end subroutine

*     ===========================================================
      subroutine Micromet_Energy_Terms ()
*     ===========================================================

      Use Infrastructure
      implicit none

*+  Purpose
*       Calculate the overall system energy terms

*+  Mission Statement
*       Calculate the overall system energy terms

*+  Changes
*     NIH 14/4/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_Energy_Terms')

*+  Local Variables
      integer i
      integer j

*- Implementation Section ----------------------------------

      call push_routine (myname)

      ! Each term is a radiation weighted average of component terms
      ! ============================================================

      g%albedo = 0.0
      g%Emissivity = 0.0

      do 200 i = g%NumLayers,1,-1

         do 100 j = 1, g%NumComponents
            g%albedo = g%albedo
     :               + divide(g%Rs(i,j)
     :                       ,g%Radn
     :                       ,0.0)
     :                   * g%ComponentAlbedo(j)
            g%Emissivity = g%Emissivity
     :               + divide(g%Rs(i,j)
     :                       ,g%Radn
     :                       ,0.0)
     :                   * g%ComponentEmissivity(j)

  100    continue

  200 continue

      g%albedo = g%albedo
     :         + (1. - divide(sum(g%Rs(:,:))
     :                       ,g%Radn
     :                       ,0.0))
     :             * p%Soil_Albedo

      g%Emissivity = g%Emissivity
     :         + (1. - divide(sum(g%Rs(:,:))
     :                       ,g%Radn
     :                       ,0.0))
     :             * c%Soil_Emissivity


      call pop_routine (myname)
      return
      end subroutine

*     ===========================================================
      subroutine Micromet_component_constants (Cno)
*     ===========================================================

      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
      integer Cno ! Component Number

*+  Purpose
*       Read constants for a given canopy component

*+  Mission Statement
*       Read constants for a given canopy component

*+  Changes
*     NIH 30/3/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_component_constants')

*+  Local Variables
      integer    numvals               ! number of values read
      character  search_order(max_table)*32 ! sections to search
      integer    num_sections          ! number of sections to search

*- Implementation Section ----------------------------------

      call push_routine (myname)

      ! Find search order for component constants
      ! -----------------------------------------

      call read_char_array ('constants'
     :                     , g%ComponentType(Cno)
     :                     , max_table
     :                     , '()'
     :                     , search_order
     :                     , num_sections)


         ! Read Component Specific Constants
         ! ---------------------------------
         ! (should be in dedicated routine)

         call search_read_real_var (
     :           search_order
     :         , num_sections
     :         , 'albedo'             ! Keyword
     :         , '(0-1)'              ! Units
     :         , g%ComponentAlbedo(Cno)  ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1.0)                 ! Upper Limit for bound checking

         call search_read_real_var (
     :           search_order
     :         , num_sections
     :         , 'emissivity'         ! Keyword
     :         , '(0-1)'              ! Units
     :         , g%ComponentEmissivity(Cno)  ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.9                  ! Lower Limit for bound checking
     :         , 1.0)                 ! Upper Limit for bound checking

         call search_read_real_var (
     :           search_order
     :         , num_sections
     :         , 'gsmax'              ! Keyword
     :         , 'm/s'                ! Units
     :         , g%ComponentGsmax(Cno)  ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1.0)                 ! Upper Limit for bound checking

         call search_read_real_var (
     :           search_order
     :         , num_sections
     :         , 'r50'                ! Keyword
     :         , 'W/m2'               ! Units
     :         , g%ComponentR50(Cno)  ! Variable
     :         , numvals              ! Number of values returned
     :         , 0.0                  ! Lower Limit for bound checking
     :         , 1e3)                 ! Upper Limit for bound checking


      call pop_routine (myname)
      return
      end subroutine

*     ===========================================================
      subroutine Micromet_Calculate_Gc ()
*     ===========================================================

      Use Infrastructure
      implicit none

*+  Purpose
*       Calculate the canopy conductance for system compartments

*+  Mission Statement
*       Calculate the canopy conductance for system compartments

*+  Changes
*     NIH 19/4/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_Calculate_Gc')

*+  Local Variables
      integer i
      integer j
      real    layerLAItot
      real    Rin
      real    Rint
      real    Rflux

*- Implementation Section ----------------------------------

      call push_routine (myname)

      Rin = g%Radn

      do 200 i = g%NumLayers,1,-1
         LayerLAItot = sum(g%LAItot(i,:))

         Rflux = Rin * 10**6 / (g%DayLength *3600.0) ! should use convert.inc
     :         * (1. - g%albedo)

         do 100 j = 1, g%NumComponents
            g%Gc(i,j) = micromet_CanopyConductance
     :                    (g%ComponentGsmax(j)
     :                    ,g%ComponentR50(j)
     :                    ,g%ComponentFrgr(j)
     :                    ,g%Fgreen(i,j)
     :                    ,g%LayerKtot(i)
     :                    ,LayerLAItot
     :                    ,Rflux)

  100    continue

         ! Calculate Rin for next layer down
         Rint = sum(g%Rs(i,:))
         Rin = Rin - Rint

  200 continue

      call pop_routine (myname)
      return
      end subroutine

*     ===========================================================
      subroutine Micromet_Calculate_Ga ()
*     ===========================================================

      Use Infrastructure
      implicit none

*+  Purpose
*       Calculate the aerodynamic conductance for system compartments

*+  Mission Statement
*       Calculate the aerodynamic conductance for system compartments

*+  Changes
*     NIH 30/5/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_Calculate_Ga')

*+  Local Variables
      integer i
      integer j
      real    TotalGa
      integer numvals

*- Implementation Section ----------------------------------

      call push_routine (myname)

      call get_real_var_optional (unknown_module, 'windspeed'
     :                                    , 'm/s'
     :                                    , g%windspeed, numvals
     :                                    , 0.0, 10.0)

      if (numvals.le.0) then
         ! no windspeed information
         g%windspeed = c%windspeed_default
      endif

      TotalGa = micromet_AerodynamicConductanceFAO(
     :                         g%WindSpeed          !windspeed
     :                       , c%RefHeight
     :                       , sum(g%DeltaZ(:))     !Top Height
     :                       , sum(g%LAItot(:,:)) ) ! Total LAI


      do 200 i = 1, g%NumLayers

         do 100 j = 1, g%NumComponents
            g%Ga(i,j) = TotalGa
     :                * divide(g%Rs(i,j)
     :                        ,sum(g%Rs(:,:))
     :                        ,0.0)

  100    continue

  200 continue

      call pop_routine (myname)
      return
      end subroutine

*     ===========================================================
      subroutine Micromet_Calculate_Interception ()
*     ===========================================================

      Use Infrastructure
      implicit none

*+  Purpose
*       Calculate the interception loss of water from the canopy

*+  Mission Statement
*       Calculate the interception loss of water from the canopy

*+  Changes
*     NIH 23/8/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_Calculate_Interception')

*+  Local Variables
      real Total_LAI
      real Total_Interception
      integer i
      integer j

*- Implementation Section ----------------------------------

      call push_routine (myname)

      Total_LAI = sum(g%LAItot(:,:))

      Total_Interception = p%A_interception * g%rain**p%B_interception
     :               + p%C_interception * Total_LAI
     :               + p%D_interception

      Total_Interception = bound(Total_Interception, 0.0, 0.99*g%Rain)

      do 200 i = 1, g%NumLayers
         do 100 j = 1, g%NumComponents

            g%Interception(i,j) = divide(g%LAI(i,j)
     :                                  ,sum(g%LAI(:,:))
     :                                  ,0.0)
     :                          * Total_Interception

  100    continue
  200 continue
      call pop_routine (myname)
      return
      end subroutine


*     ===========================================================
      subroutine Micromet_Calculate_Omega ()
*     ===========================================================

      Use Infrastructure
      implicit none

*+  Purpose
*       Calculate the aerodynamic decoupling for system compartments

*+  Mission Statement
*       Calculate the aerodynamic decoupling for system compartments

*+  Changes
*     NIH 30/5/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_Calculate_Omega')

*+  Local Variables
      integer i
      integer j

*- Implementation Section ----------------------------------

      call push_routine (myname)

      do 200 i = 1, g%NumLayers
         do 100 j = 1, g%NumComponents

            g%Omega(i,j) = micromet_omega(g%mint
     :                                   ,g%maxt
     :                                   ,c%air_pressure
     :                                   ,g%Ga(i,j)
     :                                   ,g%Gc(i,j)
     :                                   )

  100    continue

  200 continue

      call pop_routine (myname)
      return
      end subroutine

*====================================================================
      subroutine micromet_calculate_PM ()
*====================================================================

      Use Infrastructure
      implicit none

*+  Sub-Program Arguments

*+  Purpose
*     Calculate the Penman-Monteith water demand

*+  Notes

*+  Changes
*       270500 - NIH specified and programmed

*+  Calls


*+  Local Variables
 !     REAL AverageT
 !     REAL Lambda
      REAL NetRadiation       ! J
      INTEGER i
      INTEGER j
      REAL Free_Evap
      REAL Free_Evap_Ga
      REAL Free_Evap_Gc

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_calculate_PM')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      ! zero a few things
      g%PET(:,:) = 0.0
      g%PETr(:,:) = 0.0
      g%PETa(:,:) = 0.0


      NetRadiation = ((1. - g%Albedo) * sum(g%Rs(:,:))
     :             + sum(g%Rl(:,:))
     :             + sum(g%Rsoil(:,:)))
     :             * 1e6        ! MJ/J
      NetRadiation = l_bound(NetRadiation, 0.0)


      Free_Evap_Ga = sum(g%Ga(:,:))
      Free_Evap_Gc = Free_Evap_Ga * 1e6  !=infinite surface conductance

      Free_Evap = micromet_Penman_Monteith
     :              (
     :               NetRadiation
     :              ,g%mint
     :              ,g%maxt
     :              ,g%vp
     :              ,c%Air_Pressure
     :              ,g%daylength
     :              ,Free_Evap_Ga
     :              ,Free_Evap_Gc
     :              )


      g%DryLeafFraction = 1.0
     :                  -  divide (sum(g%Interception(:,:))
     :                             *(1.0-c%night_interception_fraction)
     :                             ,Free_Evap
     :                             ,0.0)

!      if (g%rain.gt.0) then
!        print*,
!     :               NetRadiation
!     :              ,g%mint
!     :              ,g%maxt
!     :              ,g%vp
!     :              ,c%Air_Pressure
!     :              ,g%daylength
!     :              ,Free_Evap_Ga
!     :              ,Free_Evap_Gc
!
!      print*,dry_leaf_fraction, Free_evap,c%night_interception_fraction
!      pause
!      endif

      if (g%DryLeafFraction.lt.0.0) then
!         call Warning_Error(Err_User,
!     :            'Interception volume > max free water evaporation')
         g%DryLeafFraction = 0.0
      else
      endif

!      averageT = (g%mint + g%maxt)/2.0
!      Lambda = micromet_Lambda (AverageT)

      do 200 i = 1, g%NumLayers
         do 100 j = 1, g%NumComponents

            NetRadiation = ((1. - g%Albedo) * g%Rs(i,j)
     :                   + g%Rl(i,j)
     :                   + g%Rsoil(i,j))
     :             * 1e6        ! MJ/J
            NetRadiation = l_bound(NetRadiation, 0.0)

            g%PETr(i,j) = micromet_PETr
     :              (
     :               NetRadiation * g%DryLeafFraction
     :              ,g%mint
     :              ,g%maxt
     :              ,c%Air_Pressure
     :              ,g%Ga(i,j)
     :              ,g%Gc(i,j)
     :              )

            g%PETa(i,j) = micromet_PETa
     :                 (
     :                  g%mint
     :                 ,g%maxt
     :                 ,g%vp
     :                 ,c%Air_Pressure
     :                 ,g%daylength * g%DryLeafFraction
     :                 ,g%Ga(i,j)
     :                 ,g%Gc(i,j)
     :                 )

            g%PET(i,j) = g%PETr(i,j) + g%PETa(i,j)

  100    continue
  200 continue

      call pop_routine (myname)

      return
      end subroutine

*====================================================================
      real function micromet_Penman_Monteith
     :              (
     :               Rn
     :              ,mint
     :              ,maxt
     :              ,vp
     :              ,Air_Pressure
     :              ,daylength
     :              ,Ga
     :              ,Gc
     :              )
*====================================================================

      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
      real Rn
      real mint
      real maxt
      real vp
      real Air_Pressure
      real daylength
      real Ga
      real Gc

*+  Purpose
*     Calculate the Penman-Monteith water demand

*+  Notes

*+  Changes
*       270500 - NIH specified and programmed

*+  Calls


*+  Local Variables
      REAL Non_dQs_dT
      REAL Lambda             ! J/kg
      REAL denominator         !of the Penman-Monteith equation
      REAL RhoA
      REAL SpecificVPD
      REAL averageT
      REAL PETa
      REAL PETr

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_Penman_Monteith')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      averageT = Micromet_AverageT(mint,maxt)
      Non_dQs_dT = micromet_Non_dQs_dT (averageT ,Air_Pressure)
      RhoA = micromet_RhoA (averageT, Air_Pressure)
      Lambda = micromet_Lambda (AverageT)

      SpecificVPD = micromet_SpecificVPD ( vp
     :                                   , mint
     :                                   , maxt
     :                                   , Air_Pressure)

      Denominator = Non_dQs_dT
     :            + Divide( Ga, Gc, 0.0)
     :            + 1.0                        ! unitless

      PETr = Divide( Non_dQs_dT * Rn
     :             , Denominator, 0.0)      ! J
     :             * 1000.0                 ! mm/m3  ????
     :             / Lambda                 ! J/kg
     :             / RhoW                   ! kg/m3


      PETa = Divide( RhoA * SpecificVPD * Ga
     :             , Denominator, 0.0)      ! kg/m3.kg/kg.m/s =
     :             * 1000.0                 ! m to mm ?
     :             * (DayLength *3600.0)    ! s
     :             / RhoW                   ! kg/m3

      micromet_penman_monteith = PETr + PETa

      call pop_routine (myname)
      return
      end function

*====================================================================
      real function micromet_PETr
     :              (
     :               Rn
     :              ,minT
     :              ,maxT
     :              ,Air_Pressure
     :              ,Ga
     :              ,Gc
     :              )
*====================================================================

      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
      real Rn
      real MinT
      real MaxT
      real Air_Pressure
      real Ga
      real Gc

*+  Purpose
*     Calculate the radiation-driven term for the Penman-Monteith
*     water demand

*+  Notes

*+  Changes
*       270500 - NIH specified and programmed

*+  Calls



*+  Local Variables
      REAL AverageT
      REAL Non_dQs_dT
      REAL Lambda             ! J/kg
      REAL denominator         !of the Penman-Monteith equation

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_PETr')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      averageT = micromet_averageT(mint,maxt)

      Non_dQs_dT = micromet_Non_dQs_dT (averageT ,Air_Pressure)

      Lambda = micromet_Lambda (AverageT)

      Denominator = Non_dQs_dT
     :            + Divide(Ga, Gc, 0.0)
     :            + 1.0                        ! unitless

      micromet_PETr = Divide( Non_dQs_dT * Rn
     :                      , Denominator, 0.0)      ! J
     :                      * 1000.0                 ! mm/m3  ????
     :                      / Lambda                 ! J/kg
     :                      / RhoW                   ! kg/m3

      call pop_routine (myname)

      return
      end function

*====================================================================
      real function micromet_PETa
     :              (
     :               mint
     :              ,maxt
     :              ,vp
     :              ,Air_Pressure
     :              ,daylength
     :              ,Ga
     :              ,Gc
     :              )
*====================================================================

      Use Infrastructure
      implicit none

*+  Sub-Program Arguments
      real mint
      real maxt
      real vp
      real Air_Pressure
      real daylength
      real Ga
      real Gc

*+  Purpose
*     Calculate the aerodynamically-driven term for the Penman-Monteith
*     water demand

*+  Notes

*+  Changes
*       270500 - NIH specified and programmed

*+  Calls



*+  Local Variables
      REAL Non_dQs_dT
      REAL Lambda             ! J/kg
      REAL denominator         !of the Penman-Monteith equation
      REAL RhoA
      REAL SpecificVPD
      REAL averageT

*+  Constant Values
      character  myname*(*)            ! name of procedure
      parameter (myname = 'micromet_PETa')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      averageT = Micromet_AverageT(mint,maxt)
      Non_dQs_dT = micromet_Non_dQs_dT (averageT ,Air_Pressure)
      RhoA = micromet_RhoA (averageT, Air_Pressure)
      Lambda = micromet_Lambda (AverageT)

      SpecificVPD = micromet_SpecificVPD ( vp
     :                                   , mint
     :                                   , maxt
     :                                   , Air_Pressure)



      Denominator = Non_dQs_dT
     :            + Divide( Ga, Gc, 0.0)
     :            + 1.0                        ! unitless

      micromet_PETa = Divide( RhoA * SpecificVPD * Ga
     :                      , Denominator, 0.0)      ! kg/m3.kg/kg.m/s =
     :                      * 1000.0                 ! m to mm ?
     :                      * (DayLength *3600.0)    ! s
     :                      / RhoW                   ! kg/m3

!      print*,SpecificVPD, Ga, Gc, RhoA,Daylength, RhoW
!      print*,Denominator, Non_dQs_dT,vp,air_pressure
!      print*,'spec hum of 3kpa=', micromet_specifichumidity(3.0,101.0)
!      pause

      call pop_routine (myname)

      return
      end function

*     ===========================================================
      subroutine Micromet_Energy_Balance_Event ()
*     ===========================================================

      Use Infrastructure
      implicit none

*+  Purpose
*       Send an energy balance event

*+  Mission Statement
*       Send an energy balance event

*+  Changes
*     NIH 30/5/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_Energy_Balance_Event')

*+  Local Variables
      integer i,j
      real int_radn_tot
      real int_radn_green
      type(CanopyEnergyBalanceType) :: LightProfile


*- Implementation Section ----------------------------------
      call push_routine (myname)


      LightProfile%num_Interception = g%NumComponents

      do 100 j=1,g%NumComponents

         LightProfile%Interception(j)%name =g%ComponentName(j)
         LightProfile%Interception(j)%croptype =g%ComponentType(j)
         LightProfile%Interception(j)%Num_layer = g%NumLayers
         do 50 i=1,g%NumLayers
            LightProfile%Interception(j)%Layer(i)%thickness
     :              = g%DeltaZ(i)
            LightProfile%Interception(j)%Layer(i)%amount
     :              = g%Rs(i,j)*Micromet_radn_green_fraction(j)
   50    continue

  100 continue

      call publish_CanopyEnergyBalance(id%canopy_energy_balance
     :                               , LightProfile)

      call pop_routine (myname)
      return
      end subroutine

*     ===========================================================
      real function Micromet_radn_green_fraction (j)
*     ===========================================================

      Use Infrastructure
      implicit none
*+  Sub-Program Arguments
      integer j

*+  Purpose
*       Calculate the proportion of light intercepted by
*       a given component that corresponds to green leaf

*+  Mission Statement
*       Calculate the proportion of light intercepted by
*       a given component that corresponds to green leaf

*+  Changes
*

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_Radn_Green_fraction')

*+  Local Variables
      real kl_green
      real kl_tot

*- Implementation Section ----------------------------------
      call push_routine (myname)

      kl_green = -log(1.0-g%ComponentCoverGreen(j))
      kl_tot = -log(1.0-g%ComponentCoverTot(j))

      Micromet_radn_green_fraction =
     :             divide (kl_green,kl_tot,0.0)

      call pop_routine (myname)
      return
      end function


*     ===========================================================
      subroutine Micromet_Water_Balance_Event ()
*     ===========================================================

      Use Infrastructure
      implicit none

*+  Purpose
*       Send a canopy water balance event

*+  Mission Statement
*       Send a canopy water balance event

*+  Changes
*     NIH 30/5/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_Water_Balance_Event')

*+  Local Variables
      integer j
      type(CanopyWaterBalanceType) :: CanopyWaterBalance

*- Implementation Section ----------------------------------
      call push_routine (myname)


      do 100 j=1,g%NumComponents
         CanopyWaterBalance%canopy(j)%name = g%ComponentName(j)
         CanopyWaterBalance%canopy(j)%CropType = g%ComponentType(j)
         CanopyWaterBalance%canopy(j)%PotentialEp
     :                       = sum(g%PET(1:g%NumLayers,j))
  100 continue
      CanopyWaterBalance%num_canopy = g%NumComponents

      CanopyWaterBalance%eo = 0.0 ! need to implement this later
      CanopyWaterBalance%interception =
     :       sum(g%Interception(1:max_layer,1:max_components))

      call publish_CanopyWaterBalance(id%canopy_water_balance
     :                               , CanopyWaterBalance)

      call pop_routine (myname)
      return
      end subroutine

*     ===========================================================
      subroutine Micromet_OnNewPotGrowth (variant)
*     ===========================================================

      Use Infrastructure
      implicit none
      integer, intent(in) :: variant

*+  Purpose
*       Obtain updated information about a plant's growth capacity

*+  Mission Statement
*       Obtain updated information about a plant's growth capacity

*+  Changes
*     NIH 1/6/00 Specified

*+  Calls


*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet_OnNewPotGrowth')

*+  Local Variables
      type(NewPotentialGrowthType) :: NewPotentialGrowth

      integer    ComponentNo

*- Implementation Section ----------------------------------

      call push_routine (myname)

      call unpack_NewPotentialGrowth(variant, NewPotentialGrowth)


      ComponentNo = position_in_char_array
     :                   (NewPotentialGrowth%sender
     :                   ,g%ComponentName
     :                   ,g%NumComponents)

      if (ComponentNo.eq.0) then
         call fatal_Error(ERR_Internal
     :       ,'Unknown Canopy Component: '//NewPotentialGrowth%sender)

      else
         g%ComponentFrgr(ComponentNo) = NewPotentialGrowth%Frgr

      endif

      call pop_routine (myname)
      return
      end subroutine


      include 'MicroScience.for'

      end module MicrometModule


!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      Use MicrometModule
      Use Infrastructure
      implicit none
      ml_external alloc_dealloc_instance

!+  Sub-Program Arguments
      logical, intent(in) :: doAllocate

!+  Purpose
!      Module instantiation routine.

!- Implementation Section ----------------------------------

      if (doAllocate) then
         allocate(g)
         allocate(p)
         allocate(c)
         allocate(id)
      else
         deallocate(g)
         deallocate(p)
         deallocate(c)
         deallocate(id)
      end if
      return
      end subroutine


* ====================================================================
       subroutine Main (Action, Data_string)
* ====================================================================
      Use MicrometModule
      Use Infrastructure
      implicit none
      ml_external Main



*+  Sub-Program Arguments
       character Action*(*)            ! Message action to perform
       character Data_string*(*)       ! Message data

*+  Purpose
*      This routine is the interface between the main system and the
*      Micromet module.

*+  Mission Statement
*     Apsim Micromet

*+  Changes
*     NIH 28/3/00 Specified

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'Micromet Main')

*- Implementation Section ----------------------------------

      call push_routine (myname)

      if (Action.eq.ACTION_Init) then
         call Micromet_Init ()

      elseif (Action.eq.ACTION_Prepare) then
         call Micromet_Prepare ()

      elseif (Action.eq.ACTION_Process) then
         call Micromet_Process ()

      else if (Action.eq.ACTION_Get_variable) then
         call Micromet_Send_my_variable (Data_string)

      else if (Action.eq.ACTION_Set_variable) then      !VOS added 040411
         call Micromet_Set_my_variable (data_string)

      else if (Action.eq.'lai_table') then
         call Micromet_table ('LAI',g%LAI)

      else if (Action.eq.'ftot_table') then
         call Micromet_table ('Ftot',g%Ftot)

      else if (Action.eq.'Fgreen_table') then
         call Micromet_table ('Fgreen',g%Fgreen)

      else if (Action.eq.'rs_table') then
         call Micromet_table ('Rs',g%Rs)

      else if (Action.eq.'rl_table') then
         call Micromet_table ('Rl',g%Rl)
      else if (Action.eq.'gc_table') then
         call Micromet_table ('Gc',g%Gc)
      else if (Action.eq.'ga_table') then
         call Micromet_table ('Ga',g%Ga)
      else if (Action.eq.'pet_table') then
         call Micromet_table ('PET',g%PET)
      else if (Action.eq.'petr_table') then
         call Micromet_table ('PETr',g%PETr)
      else if (Action.eq.'peta_table') then
         call Micromet_table ('PETa',g%PETa)
      else if (Action.eq.'omega_table') then
         call Micromet_table ('Omega',g%Omega)

      else
         ! Don't use message
         call Message_Unused ()
      endif

      call pop_routine (myname)
      return
      end
      
      ! ====================================================================
      ! do first stage initialisation stuff.
      ! ====================================================================
      subroutine doInit1 ()
      use infrastructure
      use MicroMetModule
      
      ml_external doInit1
      
      call doRegistrations(id)
      call MicroMet_create()
      end subroutine
      
! ====================================================================
! This routine is the event handler for all events
! ====================================================================
      subroutine respondToEvent(fromID, eventID, variant)
      use MicroMetModule
      Use infrastructure
      implicit none
      ml_external respondToEvent

      integer, intent(in) :: fromID
      integer, intent(in) :: eventID
      integer, intent(in) :: variant

      if (eventID .eq. id%tick) then
         call MicroMet_ONtick(variant)
      else if (eventID .eq. id%newmet) then
         call MicroMet_ONnewmet(variant)
      else if (eventID .eq. id%newcrop) then
         call Micromet_OnNewCrop (variant)
      else if (eventID .eq. id%new_canopy) then
         call Micromet_OnNewCanopy (variant)
      else if (eventID .eq. id%NewPotentialGrowth) then
         call Micromet_OnNewPotGrowth (variant)
      endif
      return
      end subroutine respondToEvent

