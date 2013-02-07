


* ====================================================================
       subroutine PlantP_zero_variables (param_init)
* ====================================================================
      implicit none

*+  Argument Values
      logical param_init  !indicate whether model constants and parameters need to be initialised

*+  Purpose
*     Set all variables in this module to zero.

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'PlantP_zero_variables')

*+  Local Variables

*- Implementation Section ----------------------------------
      call push_routine (myname)
      call print_routine (myname)

      ! Parameters
      ! ==========

c      p%crop_type = ' '       ! Characters


      ! Globals
      ! =======


      call PlantP_zero_daily_variables ()


      g%plantPfact_photo = 1.0
      g%plantPfact_expansion = 1.0
      g%plantPfact_pheno = 1.0
      g%plantPfact_grain = 1.0

      if (param_init) then
         g%phosphorus_aware    = .false.
         g%growth_Stage = 0.0      ! Reals
         g%part_p_green(:) = 0.0
         g%dlt_part_p_green(:) = 0.0
         g%part_p_sen(:) = 0.0
         g%part_p_dead(:) = 0.0
         g%dlt_part_p_dead(:) = 0.0
         g%dlt_part_p_sen(:) = 0.0
         g%dlt_part_p_det(:) = 0.0
         g%dlt_part_p_retrans(:) = 0.0

         g%part_demand(:) = 0.0

         g%part_names(:) = ' '     ! Characters

         g%num_parts = 0           ! Integers

      ! Constants
      ! =========

         c%stress_determinants(:) = ' '      ! Characters

         c%x_p_stage_code(:) = 0.0           ! Reals
         c%y_p_conc_max (:,:) = 0.0
         c%y_p_conc_min (:,:) = 0.0
         c%y_p_conc_sen (:,:) = 0.0

         c%pfact_photo_slope = 0.0

         c%pfact_expansion_slope = 0.0
         c%pfact_pheno_slope = 0.0

         c%num_x_p_stage_code = 0             ! Integers
      else
      endif

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine PlantP_get_other_variables ()
* ====================================================================
      implicit none

*+  Purpose
*      Get the values of variables from other modules

*+  Changes
*     <insert here>


*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'PlantP_get_other_variables')

*+  Local Variables
       integer numvals              ! number of values returned

*- Implementation Section ----------------------------------
      call push_routine (myname)
      call print_routine (myname)

c      call Get_real_array (
c     :      unknown_module  ! Module that responds (Not Used)
c     :     ,'dlayer'        ! Variable Name
c     :     ,max_layer       ! Array Size
c     :     ,'(mm)'          ! Units                (Not Used)
c     :     ,g%dlayer        ! Variable
c     :     ,numvals         ! Number of values returned
c     :     ,0.0             ! Lower Limit for bound checking
c     :     ,1000.)          ! Upper Limit for bound checking

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       logical function PlantP_Send_my_variable (Variable_name)
* ====================================================================

      implicit none

*+  Sub-Program Arguments
       character Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Purpose
*      Return the value of one of our variables to caller

*+  Changes

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'PlantP_send_my_variable')

*+  Local Variables
      real       biomass_p             ! total above-ground biomass P (g/m^2)
      real       apt_P_up              ! N uptake by stover (g/m^2)
      real       p_conc
      real       stress


*- Implementation Section ----------------------------------
      call push_routine (myname)
      call print_routine (myname)

      PlantP_Send_my_variable = .true.

      if (variable_name .eq. 'greenp') then
          call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(g/m^2)'          ! variable units
     :              ,sum_real_array(g%part_p_green, max_parts))

      elseif (variable_name .eq. 'p_sen') then
          call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(g/m^2)'          ! variable units
     :              ,sum_real_array(g%part_p_sen, max_parts))

!      elseif (variable_name .eq. 'p_dead') then
!          call respond2get_real_array (
!     :               variable_name     ! variable name
!     :              ,'(g/m2)'          ! variable units
!     :              ,g%part_p_dead          ! variable
!     :              ,max_parts)      ! Array size

      elseif (variable_name .eq. 'p_demand') then
          call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'(g/m^2)'         ! variable units
     :              ,sum(g%part_demand)) ! variable

      elseif (variable_name .eq. 'pfact_photo') then
          call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'()'              ! variable units
     :              ,g%plantPfact_photo) ! variable

      elseif (variable_name .eq. 'pfact_pheno') then
          call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'()'              ! variable units
     :              ,g%plantPfact_pheno) ! variable

      elseif ((variable_name .eq. 'pfact_expansion')
     :   .or. (variable_name .eq. 'pfact_expan')) then
          call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'()'              ! variable units
     :              ,g%plantPfact_expansion) ! variable

      elseif (variable_name .eq. 'pfact_grain') then
          call respond2get_real_var (
     :               variable_name     ! variable name
     :              ,'()'              ! variable units
     :              ,g%PlantPfact_grain) ! variable

      elseif (variable_name .eq. 'PStressPhoto') then
         if (g%plantPfact_photo .gt. 0.0) then
            stress = 1.0 - g%plantPfact_photo
         else
            stress = 0.0
         endif
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , stress)

      elseif (variable_name .eq. 'p_stress_fact_pheno') then
         if (g%plantPfact_pheno .gt. 0.0) then
            stress = 1.0 - g%plantPfact_pheno
         else
            stress = 0.0
         endif
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , stress)

      elseif (variable_name .eq. 'p_stress_fact_expan') then
         if (g%plantPfact_expansion .gt. 0.0) then
            stress = 1.0 - g%plantPfact_expansion
         else
            stress = 0.0
         endif
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , stress)

      elseif (variable_name .eq. 'p_stress_fact_grain') then
         if (g%plantPfact_grain .gt. 0.0) then
            stress = 1.0 - g%plantPfact_grain
         else
            stress = 0.0
         endif
         call respond2get_real_var (variable_name
     :                             , '()'
     :                             , stress)



      elseif ((variable_name .eq. 'biomass_p')
     :   .or. (variable_name .eq. 'p_uptake')) then
         biomass_p = (sum_real_array (g%part_p_green, max_part)
     :             - g%part_p_green(root)
     :             + sum_real_array (g%part_p_sen, max_part)
     :             - g%part_p_sen(root)
     :             + sum_real_array (g%part_p_dead, max_part)
     :             - g%part_p_dead(root))

         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , biomass_p)

      elseif (variable_name .eq. 'green_biomass_p') then
         biomass_p = (sum_real_array (g%part_p_green, max_part)
     :                 - g%part_p_green(root))

         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , biomass_p)

      elseif (variable_name .eq. 'grain_p') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%part_p_green(grain))

      elseif (variable_name .eq. 'leafgreenp') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%part_p_green(leaf))

      elseif (variable_name .eq. 'stemgreenp') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%part_p_green(stem))

      elseif (variable_name .eq. 'rootgreenp') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%part_p_green(root))

      elseif (variable_name .eq. 'deadleaf_p') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%part_p_sen(leaf))

      elseif (variable_name .eq. 'flower_p') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%part_p_green(flower))

      elseif (variable_name .eq. 'head_p') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%part_p_green(flower)
     :                               + g%part_p_green(grain))

      elseif (variable_name .eq. 'senescedp') then
         call respond2get_real_var (variable_name
     :                     , '(g/m^2)'
     :                     , sum_real_array (g%part_p_sen, max_part))

      elseif (variable_name .eq. 'p_dead') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g%part_p_dead
     :                             , max_part)

      elseif (variable_name .eq. 'dlt_p_green') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g%dlt_part_p_green
     :                             , max_part)

      elseif (variable_name .eq. 'dlt_p_retrans') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g%dlt_part_p_retrans
     :                             , max_part)

      elseif (variable_name .eq. 'dlt_p_detached') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g%dlt_part_p_det
     :                             , max_part)

      elseif (variable_name .eq. 'dlt_p_dead') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g%dlt_part_p_dead
     :                             , max_part)

      elseif (variable_name .eq. 'dlt_p_sen') then
         call respond2get_real_array (variable_name
     :                             , '(g/m^2)'
     :                             , g%dlt_part_p_sen
     :                             , max_part)

!      elseif (variable_name .eq. 'dlt_p_dead_detached') then
!         call respond2get_real_array (variable_name
!     :                             , '(g/m^2)'
!     :                             , g%dlt_p_dead_detached
!     :                             , max_part)



            ! plant phosphorus

      elseif (variable_name .eq. 'p_conc_stover') then
         p_conc = divide ((g%part_p_green(leaf)
     :                    + g%part_p_green(stem)
     :                    + g%part_p_green(flower))
     :                  , (g%dm_green(leaf)
     :                    + g%dm_green(stem)
     :                    + g%dm_green(flower))
     :                  , 0.0) * 100.0
         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , p_conc)

      elseif (variable_name .eq. 'leafgreenpconc') then
         p_conc = divide (g%part_p_green(leaf)
     :                  , g%dm_green(leaf)
     :                  , 0.0) * 100.0
         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , p_conc)

      elseif (variable_name .eq. 'stemgreenpconc') then
         p_conc = divide (g%part_p_green(stem)
     :                  , g%dm_green(stem)
     :                  , 0.0) * 100.0
         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , p_conc)

      elseif (variable_name .eq. 'graingreenpconc'
     :   .or. variable_name .eq. 'p_grain_pcnt') then
         p_conc = divide (g%part_p_green(grain)
     :                  , g%dm_green(grain)
     :                  , 0.0) * 100.0
         call respond2get_real_var (variable_name
     :                             , '(%)'
     :                             , p_conc)

      elseif (variable_name .eq. 'p_uptake_stover') then
         apt_p_up = (g%part_p_green(leaf)
     :            + g%part_p_green(stem)
     :            + g%part_p_green(flower))
cih     :            *gm2kg /sm2ha
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , apt_p_up)

!      elseif (variable_name .eq. 'p_demand') then
!         p_demand = sum_real_array (g%p_demand, max_part)
!         call respond2get_real_var (variable_name
!     :                             , '(g/m^2)'
!     :                             , p_demand)

      elseif (variable_name .eq. 'grain_p_demand') then
         call respond2get_real_var (variable_name
     :                             , '(g/m^2)'
     :                             , g%part_demand(grain))


      else
         PlantP_Send_my_variable = .false.
      endif

      call pop_routine (myname)
      return
      end function


*     ===========================================================
      subroutine PlantP_read_param ()
*     ===========================================================
      implicit none

*+  Purpose
*       Read all module parameters.

*+  Changes
*     <insert here>

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'PlantP_read_param')
*
      character section_name*(*)
      parameter (section_name = 'parameters')

*+  Local Variables
      integer    numvals               ! number of values read

*- Implementation Section ----------------------------------

      call push_routine (myname)
      call print_routine (myname)


c      call read_char_var (
c     :           section_name         ! Section header
c     :          ,'crop_type'          ! Keyword
c     :          ,'()'                 ! Units
c     :          ,p%crop_type          ! Array
c     :          ,numvals)             ! Number of values returned

c      call read_real_array (
c     :           section_name         ! Section header
c     :          ,'rlv'                ! Keyword
c     :          ,max_layer            ! array size
c     :          ,'(mm/cmm)'           ! Units
c     :          ,p%rlv                ! Array
c     :          ,numvals              ! Number of values returned
c     :          ,0.0                  ! Lower Limit for bound check
c     :          ,1.0)                 ! Upper Limit for bound check

      call pop_routine  (myname)
      return
      end subroutine


*     ===========================================================
      subroutine PlantP_read_constants ()
*     ===========================================================

      implicit none


*+  Sub-Program Arguments


*+  Purpose
*       Read all module constants.

*+  Changes
*

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'PlantP_read_constants')

*+  Local Variables
      integer    numvals               ! number of values read
      character  search_order(max_table)*32 ! sections to search
      integer    num_sections          ! number of sections to search
      integer    part
      real       temp(max_table)
      character  keyword*32

*- Implementation Section ----------------------------------

      call push_routine (myname)
      call print_routine (myname)

      call read_char_array ('constants'
     :                     , 'stress_determinants'
     :                     , max_parts, '()'
     :                     , c%stress_determinants
     :                     , numvals)

      call read_char_array ('constants'
     :                     , 'yield_parts'
     :                     , max_parts, '()'
     :                     , c%yield_parts
     :                     , numvals)

      call read_char_array ('constants'
     :                     , 'retrans_parts'
     :                     , max_parts, '()'
     :                     , c%retrans_parts
     :                     , numvals)

      call read_real_var  ('constants'
     :                     , 'pfact_photo_slope'
     :                     , '()'
     :                     , c%pfact_photo_slope
     :                     , numvals
     :                     , 1.0
     :                     , 100.0)

      call read_real_var  ('constants'
     :                     , 'pfact_expansion_slope'
     :                     , '()'
     :                     , c%pfact_expansion_slope
     :                     , numvals
     :                     , 1.0
     :                     , 100.0)

      call read_real_var  ('constants'
     :                     , 'pfact_pheno_slope'
     :                     , '()'
     :                     , c%pfact_pheno_slope
     :                     , numvals
     :                     , 1.0
     :                     , 100.0)

      call read_real_var  ('constants'
     :                     , 'pfact_grain_slope'
     :                     , '()'
     :                     , c%pfact_grain_slope
     :                     , numvals
     :                     , 1.0
     :                     , 100.0)

!!      FIXME - make this optional for now until MEP has ok'd the its use.
      call read_real_var_optional  ('constants'
     :                     , 'p_uptake_factor'
     :                     , '()'
     :                     , c%p_uptake_factor
     :                     , numvals
     :                     , 1.0
     :                     , 10.0)
      call read_real_array ('constants'
     :                     , 'x_p_stage_code'
     :                     , max_table, '()'
     :                     , c%x_p_stage_code
     :                     , c%num_x_p_stage_code
     :                     , 0.0
     :                     , 100.0)

      do 100 part = 1, g%num_parts

        keyword = 'y_p_conc_max_'//trim(g%part_names(part))

      call read_real_array ('constants'
     :                     , keyword
     :                     , max_table, '(g/g)'
     :                     , temp
     :                     , c%num_x_p_stage_code
     :                     , 0.0
     :                     , 1.0)
         c%y_p_conc_max(part,1:c%num_x_p_stage_code)
     :       = temp(1:c%num_x_p_stage_code)

        keyword = 'y_p_conc_sen_'//trim(g%part_names(part))

      call read_real_array ('constants'
     :                     , keyword
     :                     , max_table, '(g/g)'
     :                     , temp
     :                     , c%num_x_p_stage_code
     :                     , 0.0
     :                     , 1.0)
         c%y_p_conc_sen(part,1:c%num_x_p_stage_code)
     :       = temp(1:c%num_x_p_stage_code)

        keyword = 'y_p_conc_min_'//trim(g%part_names(part))

      call read_real_array ('constants'
     :                     , keyword
     :                     , max_table, '(g/g)'
     :                     , temp
     :                     , c%num_x_p_stage_code
     :                     , 0.0
     :                     , 1.0)
         c%y_p_conc_min(part,1:c%num_x_p_stage_code)
     :       = temp(1:c%num_x_p_stage_code)


        keyword = 'p_conc_init_'//trim(g%part_names(part))

      call read_real_var   ('constants'
     :                     , keyword
     :                     , '(g/g)'
     :                     , c%p_conc_init(part)
     :                     , numvals
     :                     , 0.0
     :                     , 1.0)

  100 continue

      call pop_routine  (myname)
      return
      end subroutine



* ====================================================================
       subroutine PlantP_prepare (growth_stage
     :                           ,part_wts
     :                           ,dlt_dm_pot)
* ====================================================================
       implicit none

*+  Sub-Program Arguments
      real part_wts(*)
      real growth_stage
      real dlt_dm_pot

*+  Purpose
*     <insert here>

*+  Changes
*

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'PlantP_prepare')
      character  string*200            ! output string

*- Implementation Section ----------------------------------
      call push_routine (myname)
      call print_routine (myname)

      call PlantP_zero_daily_variables()

      call PlantP_get_other_variables ()
      call PlantP_demand(growth_stage, part_wts, dlt_dm_pot)
      call PlantP_Stress(growth_stage, part_wts)

      call pop_routine (myname)
      return
      end subroutine


* ====================================================================
       subroutine PlantP_process (growth_stage,dm_green,dm_senesced
     :                           ,dlt_dm_senesced
     :                           ,dlt_dm_detached)
* ====================================================================
      implicit none

*+  Sub-Program Arguments
       real growth_stage
       real dm_green(*)
       real dm_senesced(*)
       real dlt_dm_senesced(*)
       real dlt_dm_detached(*)

*+  Purpose
*     <insert here>

*+  Changes
*

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'PlantP_process')

*+  Local Variables
      real layered_p_uptake(max_layer)
      integer numvals
      integer part
      real p_uptake
      real total_demand


*- Implementation Section ----------------------------------
      call push_routine (myname)
      call print_routine (myname)
      call PlantP_init_pools(dm_green)

      call PlantP_partition()
      call PlantP_senescence(growth_stage
     :                      ,dm_green
     :                      ,dlt_dm_senesced)
      call PlantP_detachment(dm_senesced,dlt_dm_detached)

      g%part_p_green(:) = g%part_p_green(:)
     :                  + g%dlt_part_p_green(:)
     :                  - g%dlt_part_p_sen(:)

      g%part_p_sen(:) = g%part_p_sen(:)
     :                + g%dlt_part_p_sen(:)
     :                - g%dlt_part_p_det(:)


      ! Now do any retranslocation to try and keep pools
      ! at target concentrations.

      call PlantP_retrans(growth_stage,dm_green)

      g%part_p_green(:) = g%part_p_green(:)
     :                  + g%dlt_part_p_retrans(:)

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine PlantP_partition ()
* ====================================================================
      implicit none

*+  Sub-Program Arguments

*+  Purpose
*     <insert here>

*+  Changes
*


*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'PlantP_partition')

*+  Local Variables
      real layered_p_uptake(max_layer)
      integer numvals
      integer part
      real p_uptake
      real total_demand
      character keyword*32
      character*200 string


*- Implementation Section ----------------------------------
      call push_routine (myname)
      call print_routine (myname)

      call fill_real_array (layered_p_uptake,0.0,max_layer)

      keyword = 'uptake_p_'//trim(g%crop_type)

      call get_real_array_Optional
     :                        (unknown_module
     :                       ,keyword
     :                       ,max_layer
     :                       ,'()'
     :                       ,layered_p_uptake
     :                       ,numvals
     :                       ,0.0
     :                       ,100.)
      if (numvals.gt.0) then
         p_uptake = sum(layered_p_uptake(1:numvals))
     :            * kg2gm/ha2sm

      else
         p_uptake = sum(g%part_demand(1:g%num_parts))

      endif

      total_demand = sum(g%part_demand(1:g%num_parts))

      do 100 part=1,g%num_parts
         g%dlt_part_p_green(part) = p_uptake
     :                            * divide(g%part_demand(part)
     :                                     ,total_demand
     :                                     ,0.0)
!!            write (string,*) 'part, total_demand, g%part_demand(part), '
!!     :                 //'p_uptake, g%dlt_part_p_green(part), '
!!     :                 //'g%part_p_green(part)'
!!            call Write_string (string)
!!            write (string,*) part, total_demand, g%part_demand(part),
!!     :                 p_uptake, g%dlt_part_p_green(part),
!!     :                 g%part_p_green(part)
!!            call Write_string (string)
  100 continue

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine PlantP_senescence (growth_stage, part_wts
     :                              , dlt_part_sen)
* ====================================================================
      implicit none

*+  Sub-Program Arguments
      real part_wts(*)
      real dlt_part_sen(*)
      real growth_stage


*+  Purpose
*     <insert here>

*+  Changes
*

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'PlantP_senescence')

*+  Local Variables
      integer part
      real p_conc_green
      real p_conc_sen

*- Implementation Section ----------------------------------
      call push_routine (myname)
      call print_routine (myname)

      g%dlt_part_p_sen(:) = 0.0

      do 100 part=1,g%num_parts
         p_conc_green = divide (g%part_p_green(part)
     :                         ,part_wts(part)
     :                         ,0.0)
         p_conc_sen = linear_interp_real
     :                   (growth_stage
     :                   ,c%x_p_stage_code
     :                   ,c%y_p_conc_sen(part,1:c%num_x_p_stage_code)
     :                   ,c%num_x_p_stage_code)

         g%dlt_part_p_sen(part) = min(p_conc_green,p_conc_sen)
     :                          * dlt_part_sen(part)

  100 continue

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine PlantP_detachment (dm_senesced, dlt_dm_detached)
* ====================================================================
      implicit none

*+  Sub-Program Arguments
      real dm_senesced(*)
      real dlt_dm_detached(*)

*+  Purpose
*     <insert here>

*+  Changes
*

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'PlantP_detachment')

*+  Local Variables
      integer part
      real    sen_detach_frac

*- Implementation Section ----------------------------------
      call push_routine (myname)
      call print_routine (myname)

      g%dlt_part_p_det(:) = 0.0

      do 100 part=1,g%num_parts
         sen_detach_frac = divide(dlt_dm_detached(part)
     :                           ,dm_senesced(part)
     :                           ,0.0)

         g%dlt_part_p_det(part) = g%part_p_sen(part)
     :                            * sen_detach_frac
  100 continue

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine PlantP_zero_daily_variables ()
* ====================================================================
      implicit none

*+  Purpose
*     <insert here>

*+  Changes
*

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'PlantP_zero_daily_variables')

*- Implementation Section ----------------------------------
      call push_routine (myname)
      call print_routine (myname)

      g%dlt_part_p_green(:) = 0.0
      g%part_demand(:) = 0.0

      call pop_routine (myname)
      return
      end subroutine


* ====================================================================
       subroutine PlantP_Create ()
* ====================================================================
      implicit none

*+  Sub-Program Arguments

*+  Purpose
*      Create PlantP module

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'PlantP_create')

*+  Local Variables

*- Implementation Section ----------------------------------
      call push_routine (myname)
      call print_routine (myname)

      call PlantP_zero_variables (.true.)

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine PlantP_Init (crop_type, part_names, num_parts)
* ====================================================================
      implicit none

*+  Sub-Program Arguments
      character crop_type*(*)
      character part_names(*)*(*)
      Integer num_parts

*+  Purpose
*      Initialise PlantP module

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'PlantP_init')

*+  Local Variables

*- Implementation Section ----------------------------------
      call push_routine (myname)
      call print_routine (myname)

      call PlantP_get_other_variables ()

      call PlantP_set_phosphorus_aware ()

      if (g%phosphorus_aware) then
         g%crop_type = crop_type

         call PlantP_Set_Up_Parts (part_names, num_parts)

         call PlantP_read_param ()

         call PlantP_read_constants ()
      else
      endif

      call pop_routine (myname)
      return
      end subroutine


* ====================================================================
       subroutine PlantP_Set_Up_Parts (part_names, num_parts)
* ====================================================================
      implicit none

*+  Sub-Program Arguments
      character part_names(*)*(*)
      integer   num_parts

*+  Purpose
*      Initialise PlantP parts arrays

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'PlantP_set_up_parts')

*+  Local Variables

*- Implementation Section ----------------------------------
      call push_routine (myname)
      call print_routine (myname)

      g%num_parts = num_parts
      g%part_names(1:num_parts) = part_names(1:num_parts)

      ! Put further setup stuff here.....

      call pop_routine (myname)
      return
      end subroutine

*     ================================================================
      subroutine PlantP_set_phosphorus_aware ()
*     ================================================================
      implicit none

*+  Purpose
*      Check that soil phosphorus is in system

*+  Mission statement
*     Check the phosphorus awareness of the system

*+  Changes
*     121198 jngh programmed
*     170599 jngh added include 'write.pub'

*+  Constant Values
      character*(*) myname
      parameter (myname = 'PlantP_set_phosphorus_aware')

*+  Local Variables
      integer   numvals
      real labile_p(max_layer)      ! labile p from soil phosphorus

*- Implementation Section ----------------------------------

      call push_routine (myname)
      call print_routine (myname)

      call Get_real_array_optional(
     :      unknown_module  ! Module that responds (Not Used)
     :    , 'labile_p'      ! Variable Name
     :    , max_layer       ! size of array
     :    , '(kg/ha)'       ! Units                (Not Used)
     :    , labile_p       ! Variable
     :    , numvals         ! Number of values returned
     :    , 1.0             ! Lower Limit for bound checking
     :    , 1000.0)         ! Upper Limit for bound checking

      if(numvals .gt. 0) then
         !module is p aware
         g%phosphorus_aware = .true.
         call write_string (
     :                  new_line
     :                  //'    - Module is set phosphorus aware')
      else
         g%phosphorus_aware = .false.

      endif

      call pop_routine (myname)
      return
      end subroutine


* ====================================================================
       subroutine PlantP_demand (growth_stage, parts_wt, dlt_dm_pot)
* ====================================================================
      implicit none

*+  Sub-Program Arguments
      real parts_wt(*)
      real growth_stage
      real dlt_dm_pot            ! potential dm increase (g/m2)

*+  Purpose
*      Calculate plant P demands

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'PlantP_demand')

*+  Local Variables
      real       deficit               ! deficit of total plant p (g/m2)
      integer part
      real p_conc_max
      integer counter
      integer num_yield_parts
      real    rel_growth_rate
      real       p_demand_max          ! maximum P demand (g/m2)
      real       P_demand_new          ! demand for P by new growth
                                       ! (g/m^2)
      real       P_demand_old          ! demand for P by old biomass
                                       ! (g/m^2)
      real dlt_dm_pot_part

      character*200 string
*- Implementation Section ----------------------------------
      call push_routine (myname)
      call print_routine (myname)

      g%Part_demand(:) = 0.0
      g%growth_stage = growth_stage
      num_yield_parts = count_of_char_vals(c%yield_parts
     :                                     ,max_parts)

      rel_growth_rate = divide(dlt_dm_pot
     :                        ,sum(parts_wt(1:g%num_parts))
     :                        ,0.0)

      do 100 part = 1, g%num_parts

         dlt_dm_pot_part = dlt_dm_pot
     :                   * (1.0 + c%ratio_root_shoot(int(growth_stage)))
     :                   * divide(g%dlt_dm_green(part)
     :                           , sum(g%dlt_dm_green(1:g%num_parts))
     :                           , 0.0)

         ! Find if this part is a yield part

         counter = position_in_char_array
     :             (g%part_names(part)
     :             ,c%yield_parts
     :             ,num_yield_parts)


         if (counter .eq. 0) then
            ! Not a yield part - therefore it contributes to demand

!!            write (string,*) 'dlt_dm_pot_part, dlt_dm_pot, '
!!     :                 //'sum(g%dlt_dm_green(1:g%num_parts)), '
!!     :                 //'g%dlt_dm_green(part), growth_stage '
!!     :                 //'c%ratio_root_shoot(growth_stage)'
!!            call Write_string (string)
!!            write (string,*) dlt_dm_pot_part, dlt_dm_pot
!!     :                 , sum(g%dlt_dm_green(1:g%num_parts))
!!     :                 , g%dlt_dm_green(part), growth_stage
!!     :                 , c%ratio_root_shoot(growth_stage)
!!            call Write_string (string)

            p_conc_max = linear_interp_real
     :                   (g%growth_stage
     :                   ,c%x_p_stage_code
     :                   ,c%y_p_conc_max(part,1:c%num_x_p_stage_code)
     :                   ,c%num_x_p_stage_code)

            ! scale up to include potential new growth
            ! assuming partitioning today similar to current
            ! plant form - a rough approximation

!!            deficit = p_conc_max
!!     :                   * parts_wt(part)
!!     :                   * (1. + rel_growth_rate)
!!     :              - g%part_p_green(part)
!!            g%Part_demand(part) = l_bound(deficit, 0.0)

!!     FIXME - use original method for dlt_dm_pot_part for now
            dlt_dm_pot_part = parts_wt(part) * rel_growth_rate
            P_demand_new = dlt_dm_pot_part * P_conc_max
            P_demand_old = (parts_wt(part) * P_conc_max)
     :                   - g%part_p_green(part)
!!     FIXME - don't constrain demand for old for now
!!            P_demand_old = l_bound (P_demand_old, 0.0)

            deficit = P_demand_old + P_demand_new
            deficit = l_bound (deficit, 0.0)

            p_demand_max = P_demand_new * c%p_uptake_factor
!!            p_demand_max = g%Part_demand(part) * c%p_uptake_factor
!!            write (string,*) 'deficit, p_demand_max, rel_growth_rate, '
!!     :                 //'dlt_dm_pot, p_conc_max, P_demand_new, '
!!     :                 //'P_demand_old, g%part_p_green(part), '
!!     :                 //'parts_wt(part)'
!!            call Write_string (string)
!!            write (string,*) deficit, p_demand_max, rel_growth_rate
!!     :                 , dlt_dm_pot, p_conc_max, P_demand_new
!!     :                 , P_demand_old, g%part_p_green(part)
!!     :                 , parts_wt(part)
!!            call Write_string (string)

!!       FIXME - don't constrain demand until uptake factor is ok'd by MEP.
!!            g%Part_demand(part) = u_bound (deficit, p_demand_max)
            g%Part_demand(part) = deficit

         else
            ! A yield part - does not contribute to soil demand
            g%Part_demand(part) = 0.0
         endif

  100 continue


      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       real function PlantP_Pfact_photo ()
* ====================================================================
      implicit none

*+  Sub-Program Arguments

*+  Purpose
*      Provide value of P factor for photosynthesis

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'PlantP_Pfact_photo')

*+  Local Variables

*- Implementation Section ----------------------------------
      call push_routine (myname)
      call print_routine (myname)

      if (g%phosphorus_aware) then
         PlantP_Pfact_photo = g%plantPfact_photo
      else
         PlantP_Pfact_photo = 1.0
      endif

      call pop_routine (myname)
      return
      end function

* ====================================================================
       real function PlantP_Pfact_grain ()
* ====================================================================
      implicit none

*+  Sub-Program Arguments

*+  Purpose
*      Provide value of P factor for grain filling

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'PlantP_Pfact_grain')

*+  Local Variables

*- Implementation Section ----------------------------------
      call push_routine (myname)
      call print_routine (myname)

      if (g%phosphorus_aware) then
         PlantP_Pfact_grain = g%plantPfact_grain
      else
         PlantP_Pfact_grain = 1.0
      endif

      call pop_routine (myname)
      return
      end function
* ====================================================================
       real function PlantP_Pfact_expansion ()
* ====================================================================
      implicit none

*+  Sub-Program Arguments

*+  Purpose
*      Provide value of P factor for cell expansion

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'PlantP_Pfact_expansion')

*+  Local Variables

*- Implementation Section ----------------------------------
      call push_routine (myname)
      call print_routine (myname)

      if (g%phosphorus_aware) then
         PlantP_Pfact_expansion = g%plantPfact_expansion
      else
         PlantP_Pfact_expansion = 1.0
      endif

      call pop_routine (myname)
      return
      end function
* ====================================================================
       real function PlantP_Pfact_pheno ()
* ====================================================================
      implicit none

*+  Sub-Program Arguments

*+  Purpose
*      Provide value of P factor for phenology

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'PlantP_Pfact_pheno')

*+  Local Variables

*- Implementation Section ----------------------------------
      call push_routine (myname)
      call print_routine (myname)

      if (g%phosphorus_aware) then
         PlantP_Pfact_pheno = g%plantPfact_pheno
      else
         PlantP_Pfact_pheno = 1.0
      endif

      call pop_routine (myname)
      return
      end function

* ====================================================================
       subroutine PlantP_add_residue (chop_fr_green, chop_fr_sen
     :                        ,chop_fr_dead, fraction_to_residue
     :                        )
* ====================================================================
      implicit none

*+  Sub-Program Arguments
      real chop_fr_green(*)
      real chop_fr_sen(*)
      real chop_fr_dead(*)
      real fraction_to_residue(*)
!      integer EventInterface

*+  Purpose
*      Send P to soil or residues when plants are 'chopped'

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'PlantP_add_residue')

*+  Local Variables
      real    dlt_residue_p

*- Implementation Section ----------------------------------
      call push_routine (myname)
      call print_routine (myname)

      dlt_residue_p = sum(chop_fr_green(1:g%num_parts)
     :                      * g%part_p_green(1:g%num_parts)
     :                      * fraction_to_residue(1:g%num_parts)
     :                   +
     :                    chop_fr_sen(1:g%num_parts)
     :                      * g%part_p_sen(1:g%num_parts)
     :                      * fraction_to_residue(1:g%num_parts)
     :                   +
     :                    chop_fr_dead(1:g%num_parts)
     :                      * g%part_p_dead(1:g%num_parts)
     :                      * fraction_to_residue(1:g%num_parts)
     :                    )
     :              * gm2kg/sm2ha

      g%part_p_green(1:g%num_parts) = g%part_p_green(1:g%num_parts)
     :                              * (1.-chop_fr_green(1:g%num_parts))
      g%part_p_sen(1:g%num_parts) = g%part_p_sen(1:g%num_parts)
     :                              * (1.-chop_fr_sen(1:g%num_parts))
      g%part_p_dead(1:g%num_parts) = g%part_p_dead(1:g%num_parts)
     :                              * (1.-chop_fr_dead(1:g%num_parts))

      call New_postbox ()

      call post_real_var ('dlt_residue_p'
     :                   ,'(kg/ha)'
     :                   , dlt_residue_P)

       call event_send(unknown_module, ACTION_add_residue_p)

!      call EI_BroadcastAction     (EventInterface
!     :                            ,ACTION_add_residue_p
!     :                            ,Blank
!     :                            )

      call Delete_postbox ()

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine PlantP_residue_chopped (chop_fr_green, chop_fr_sen
     :                        ,chop_fr_dead, fraction_to_residue
     :                        , dlt_residue_p, dlt_dm_P
     :                        )
* ====================================================================
      implicit none

*+  Sub-Program Arguments
      real chop_fr_green(*)
      real chop_fr_sen(*)
      real chop_fr_dead(*)
      real fraction_to_residue(*)
      real dlt_dm_P(*)
      real    dlt_residue_p
!      integer EventInterface

*+  Purpose
*      Send P to soil or residues when plants are 'chopped'

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'PlantP_residue_chopped')

*+  Local Variables

*- Implementation Section ----------------------------------
      call push_routine (myname)
      call print_routine (myname)

      if (g%phosphorus_aware) then
         dlt_dm_P(1:g%num_parts) = (chop_fr_green(1:g%num_parts)
     :                      * g%part_p_green(1:g%num_parts)
     :                      * fraction_to_residue(1:g%num_parts)
     :                   +
     :                    chop_fr_sen(1:g%num_parts)
     :                      * g%part_p_sen(1:g%num_parts)
     :                      * fraction_to_residue(1:g%num_parts)
     :                   +
     :                    chop_fr_dead(1:g%num_parts)
     :                      * g%part_p_dead(1:g%num_parts)
     :                      * fraction_to_residue(1:g%num_parts))
     :              * gm2kg/sm2ha

         dlt_residue_p = sum(dlt_dm_P(1:g%num_parts))

         g%part_p_green(1:g%num_parts) = g%part_p_green(1:g%num_parts)
     :                               * (1.-chop_fr_green(1:g%num_parts))
         g%part_p_sen(1:g%num_parts) = g%part_p_sen(1:g%num_parts)
     :                               * (1.-chop_fr_sen(1:g%num_parts))
         g%part_p_dead(1:g%num_parts) = g%part_p_dead(1:g%num_parts)
     :                                * (1.-chop_fr_dead(1:g%num_parts))

      else
         dlt_dm_P(1:g%num_parts)   = 0.0
         dlt_residue_p = 0.0
      endif

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine PlantP_incorp_fom (incorp_fr_green, incorp_fr_sen
     :                        ,incorp_fr_dead, dlayer
     :                        ,root_length,root_depth
     :                        , p_incorporated
     :                        )
* ====================================================================
      implicit none

*+  Sub-Program Arguments
      real incorp_fr_green(*)
      real incorp_fr_sen(*)
      real incorp_fr_dead(*)
      real dlayer(*)
      real root_length(*)
      real root_depth
!      integer EventInterface

*+  Purpose
*      Send P to soil or residues when plants are 'chopped'

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'PlantP_incorp_fom')

*+  Local Variables
      type(FOMLayerType) :: IncorpFOM
      real    dlt_fom_p(max_layer)
      real    p_incorporated
      integer layer

*- Implementation Section ----------------------------------
      call push_routine (myname)
      call print_routine (myname)

      p_incorporated = sum(incorp_fr_green(1:g%num_parts)
     :                      * g%part_p_green(1:g%num_parts)
     :                   +
     :                    incorp_fr_sen(1:g%num_parts)
     :                      * g%part_p_sen(1:g%num_parts)
     :                   +
     :                    incorp_fr_dead(1:g%num_parts)
     :                      * g%part_p_dead(1:g%num_parts)
     :                    )
     :               * gm2kg /sm2ha


      g%part_p_green(1:g%num_parts) = g%part_p_green(1:g%num_parts)
     :                           * (1.-incorp_fr_green(1:g%num_parts))
      g%part_p_sen(1:g%num_parts) = g%part_p_sen(1:g%num_parts)
     :                           * (1.-incorp_fr_sen(1:g%num_parts))
      g%part_p_dead(1:g%num_parts) = g%part_p_dead(1:g%num_parts)
     :                           * (1.-incorp_fr_dead(1:g%num_parts))


      call crop_root_dist
     :               (
     :                dlayer
     :              , root_length
     :              , root_depth
     :              , dlt_fom_p
     :              , p_incorporated
     :              , max_layer
     :               )

      IncorpFOM%Type = c%crop_type
      IncorpFOM%num_layer = count_of_real_vals(root_length,max_layer)
      do layer = 1, IncorpFOM%num_layer
         IncorpFOM%layer(layer)%FOM%Amount = 0.0
         IncorpFOM%layer(layer)%FOM%N = 0.0
         IncorpFOM%layer(layer)%FOM%P = dlt_fom_P(layer)
         IncorpFOM%layer(layer)%CNR = 0.0
         IncorpFOM%layer(layer)%LabileP = 0.0
      enddo
      call publish_FOMLayer(id%Incorp_FOM, IncorpFOM)

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       real function PlantP_Pfact (growth_stage, dm_green)
* ====================================================================
      implicit none

*+  Sub-Program Arguments
      real growth_stage
      real dm_green(*)

*+  Purpose
*      Provide value of generic P factor

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'PlantP_Pfact')

*+  Local Variables
      integer determinant
      integer num_determinants
      real    p_conc_max
      real    p_conc_min
      integer part
      real    max_p
      real    min_p
      real    act_p
      real    max_p_conc
      real    min_p_conc
      real    act_p_conc
      real    determinants_wt
      real    pfact
      character*200 msg

*- Implementation Section ----------------------------------
      call push_routine (myname)
      call print_routine (myname)

      num_determinants = count_of_char_vals(c%stress_determinants
     :                                     ,max_parts)

      act_p = 0.0
      min_p = 0.0
      max_p = 0.0
      determinants_wt = 0.0

      do 100 determinant = 1, num_determinants
         part = position_in_char_array
     :             (c%stress_determinants(determinant)
     :             ,g%part_names
     :             ,g%num_parts)

         act_p = act_p + g%part_p_green(part)

         p_conc_max = linear_interp_real
     :                   (growth_stage
     :                   ,c%x_p_stage_code
     :                   ,c%y_p_conc_max(part,1:c%num_x_p_stage_code)
     :                   ,c%num_x_p_stage_code)
         max_p = max_p + p_conc_max * dm_green(part)

         p_conc_min = linear_interp_real
     :                   (growth_stage
     :                   ,c%x_p_stage_code
     :                   ,c%y_p_conc_min(part,1:c%num_x_p_stage_code)
     :                   ,c%num_x_p_stage_code)
         min_p = min_p + p_conc_min * dm_green(part)


         determinants_wt = determinants_wt
     :                   + dm_green(part)
  100 continue

      act_p_conc = divide(act_p, determinants_wt, 1.0)
      max_p_conc = divide(max_p, determinants_wt, 1.0)
      min_p_conc = divide(min_p, determinants_wt, 1.0)

      if ((determinants_wt.le.1.0e-5).or. (act_p.le.1.0e-5)) then
         ! appears that things are not yet initialised
         pfact = 1.0

      else
         pfact = divide(act_p_conc - min_p_conc
     :                 ,max_p_conc - min_p_conc
     :                 ,1.0)
      endif

      PlantP_Pfact = bound(pfact,0.0,1.0)

      call pop_routine (myname)
      return
      end function

* ====================================================================
       subroutine PlantP_Stress (growth_stage, dm_green)
* ====================================================================
      implicit none

*+  Sub-Program Arguments
      real growth_stage
      real dm_green(*)

*+  Purpose
*      Provide value of  P stress factors

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'PlantP_Stress')

*+  Local Variables
      real    pfact

*- Implementation Section ----------------------------------
      call push_routine (myname)
      call print_routine (myname)

      if (g%phosphorus_aware) then
         pfact = PlantP_Pfact(growth_stage,dm_green)

         g%plantPfact_photo = pfact * c%pfact_photo_slope
         g%plantPfact_photo = bound(g%plantPfact_photo,0.0,1.0)

         g%plantPfact_expansion = pfact * c%pfact_expansion_slope
         g%plantPfact_expansion = bound(g%plantPfact_expansion,0.0,1.0)

         g%plantPfact_pheno = pfact * c%pfact_pheno_slope
         g%plantPfact_pheno = bound(g%plantPfact_pheno,0.0,1.0)

         g%plantPfact_grain = pfact * c%pfact_grain_slope
         g%plantPfact_grain = bound(g%plantPfact_grain,0.0,1.0)
      else
         g%plantPfact_photo = 1.0
         g%plantPfact_expansion = 1.0
         g%plantPfact_pheno = 1.0
         g%plantPfact_grain = 1.0
      endif

      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine PlantP_init_pools (dm_green)
* ====================================================================
      implicit none

*+  Sub-Program Arguments
      real dm_green(*)

*+  Purpose
*      Initialise Plant P Pools

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'PlantP_init_pools')

*+  Local Variables

      type (ExternalMassFlowType) :: massBalanceChange


*- Implementation Section ----------------------------------
      call push_routine (myname)
      call print_routine (myname)

      if ((sum(dm_green(1:g%num_parts)).gt.1.0e-5)
     :            .and.
     :   (sum(g%part_p_green(1:g%num_parts)).le.1.0e-5)) then

         ! biomass has been initialised but the p pools have not
         g%part_p_green(1:g%num_parts) = dm_green(1:g%num_parts)
     :                                 * c%p_conc_init(1:g%num_parts)

         if (on_day_of (emerg, g%current_stage, g%days_tot)) then
             ! seedling has just emerged.
            massBalanceChange%PoolClass = "crop"
            massBalanceChange%FlowType = "gain"
            massBalanceChange%DM = 0.0
            massBalanceChange%C  = 0.0
            massBalanceChange%N  = 0.0
            massBalanceChange%P = sum(g%part_p_green(1:g%num_parts))
     :                          * gm2kg/sm2ha
            massBalanceChange%SW = 0.0

            call publish_ExternalMassFlow(ID%ExternalMassFlow
     :                                 , massBalanceChange)
         else
               !do nothing
         endif

      else
         ! Do nothing
      endif


      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine PlantP_retrans (growth_stage,dm_green)
* ====================================================================
      implicit none

*+  Sub-Program Arguments
      real growth_stage
      real dm_green(*)

*+  Purpose
*      Calculate retranslocation between pools

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'PlantP_retrans')

*+  Local Variables
      real supply(max_parts)
      real demand(max_parts)

      integer counter
      integer num_supplies
      integer num_yield_parts
      integer part
      real    p_conc_min
      real    min_p
      real    p_conc_max
      real    max_p
      real    fraction

*- Implementation Section ----------------------------------
      call push_routine (myname)
      call print_routine (myname)

      g%dlt_part_p_retrans(:) = 0.0
      supply(:) = 0.0
      demand(:) = 0.0

      num_supplies = count_of_char_vals(c%retrans_parts
     :                                     ,max_parts)
      num_yield_parts = count_of_char_vals(c%yield_parts
     :                                     ,max_parts)

      do 100 counter = 1, num_supplies

         part = position_in_char_array
     :             (c%retrans_parts(counter)
     :             ,g%part_names
     :             ,g%num_parts)

         p_conc_min = linear_interp_real
     :                   (growth_stage
     :                   ,c%x_p_stage_code
     :                   ,c%y_p_conc_min(part,1:c%num_x_p_stage_code)
     :                   ,c%num_x_p_stage_code)
         min_p = p_conc_min * dm_green(part)
         supply(part) = max(g%part_p_green(part) - min_p,0.0)

  100 continue

      do 200 counter = 1, num_yield_parts

         part = position_in_char_array
     :             (c%yield_parts(counter)
     :             ,g%part_names
     :             ,g%num_parts)

         p_conc_max = linear_interp_real
     :                   (growth_stage
     :                   ,c%x_p_stage_code
     :                   ,c%y_p_conc_max(part,1:c%num_x_p_stage_code)
     :                   ,c%num_x_p_stage_code)
         max_p = p_conc_max * dm_green(part)
         demand(part) = max(max_p - g%part_p_green(part), 0.0)

  200 continue

      do 300 part = 1, g%num_parts

         if (supply(part).gt.0.0) then
            fraction = divide(sum(demand),sum(supply),0.0)
            fraction = bound(fraction,0.0,1.0)
            g%dlt_part_p_retrans(part) = -supply(part)*fraction

         elseif (demand(part).gt.0.0) then
            fraction = divide(sum(supply),sum(demand),0.0)
            fraction = bound(fraction,0.0,1.0)
            g%dlt_part_p_retrans(part) = demand(part)*fraction

         else
            ! this part is not involved
         endif
  300 continue


      call pop_routine (myname)
      return
      end subroutine

* ====================================================================
       subroutine PlantP_summary ()
* ====================================================================
      implicit none

*+  Purpose
*      Calculate retranslocation between pools

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'PlantP_summary')

*+  Local Variables
      character  string*200            ! message
      real       P_grain               ! total grain P uptake (kg/ha)
      real       P_dead                ! above ground dead plant P (kg/ha)
      real       P_green               ! above ground green plant P (kg/ha)
      real       P_senesced            ! above ground senesced plant P (kg/ha)
      real       P_stover              ! nitrogen content of stover (kg\ha)
      real       P_total               ! total gross nitrogen content (kg/ha)
      real       P_grain_conc_percent  ! grain nitrogen %

*- Implementation Section ----------------------------------          g%part_p_green(1:g%num_parts)
      call push_routine (myname)
      call print_routine (myname)

      if (g%phosphorus_aware) then

         P_grain_conc_percent = divide (g%part_p_green(grain)
     :                              + g%part_p_dead(grain)
     :                            , g%dm_green(grain)
     :                              + g%dm_dead(grain)
     :                            , 0.0)
     :                     * fract2pcnt

         P_grain = (g%part_p_green(grain) + g%part_p_dead(grain))
     :           * gm2kg/sm2ha

         P_green = (sum_real_array (g%part_p_green, max_parts)
     :           - g%part_p_green(root) - g%part_p_green(grain))
     :           * gm2kg / sm2ha

         P_senesced = (sum_real_array (g%part_p_sen, max_parts)
     :              - g%part_p_sen(root) - g%part_p_sen(grain))
     :             * gm2kg / sm2ha

         P_dead = (sum_real_array (g%part_p_dead, max_parts)
     :          - g%part_p_dead(root) - g%part_p_dead(grain))
     :          * gm2kg / sm2ha

         P_stover = P_green + P_senesced + P_dead
         P_total = P_grain + P_stover

         write (string, '(a,f10.2,t55,a,f10.2)')
     :            ' grain P percent            =', P_grain_conc_percent
     :          , ' total P content (kg/ha)    =', P_total
         call write_string ( string)

         write (string, '(a,f10.2,t55,a,f10.2)')
     :            ' grain P uptake (kg/ha)     =', P_grain
     :          , ' senesced P content (kg/ha) =', P_senesced

         call write_string ( string)

         write (string, '(a,f10.2,t55,a,f10.2)')
     :            ' green P content (kg/ha)    =', P_green
     :          , ' dead P content (kg/ha)     =', P_dead
         call write_string ( string)

      else
            ! phosphorus not in
      endif

      call pop_routine (myname)
      return
      end subroutine


