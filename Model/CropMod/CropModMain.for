!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!! This file is only used in CropMod.dll which is only used by sunflower
!!!!!!!!! MAIZE has it's own CropModMain.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use CropModModule
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

*=====================================================================
      include 'doInit1.for'
*=====================================================================

*=====================================================================
      subroutine Main (action, data_string)
*=====================================================================
      use CropLibrary
      Use infrastructure
      Use CropModModule
      implicit none
      ml_external Main


*+  Sub-Program Arguments
      character  action*(*)            ! (INPUT) Message action to perform
      character  data_string*(*)       ! (INPUT) Message data

*+  Purpose
*     This module performs crop growth simulation simulates crop phenological
*     development, growth of root, leaf, head, stem and grain,
*     Water and  nitrogen uptake, leaf and root senescense.

*+  Changes
*      271198 ew
*      250894 sc    specified and programmed
*      011195 jngh  added call to message_unused

*+  Calls

*+  Constant Values
      character  my_name*(*)         ! name of this procedure
      parameter (my_name='CropMod')


      LOGICAL TestTrue
      REAL    daylength
      REAL    radn_ext
      REAL    rue_max
      REAL    diff_radn_frac
      REAL    rue_diff_radn_modifier
      character  module_name*(max_module_name_size)

*- Implementation Section ----------------------------------
      call push_routine (my_name)
      call print_routine (my_name)

      if (action.eq.ACTION_init) then

         !Zero pools inlcuding contants and parameters
         call Zero_Variables (.true.)

         !Read the crop specific contants from ini files
         call CropMod_Initialisation ()

         !Request and receive variables from owner-modules
         call Get_Other_Variables ()

         call get_name (module_name)

         if (module_name .eq. 'sorghum' .OR.
     :       module_name .eq. 'maize') then
             call Fatal_error (ERR_user,
     :            'Please use standalone crop module')
         endif

         !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c         if (c%crop_type .eq. 'wheat') then
c             TestTrue = .TRUE.
c         else
c             TestTrue = .FALSE.
c         endif
c
c             TestTrue = .FALSE.
c
c         if (TestTrue) open (1, FILE='test.dat')
         !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


      elseif (action.eq.ACTION_set_variable) then

         ! Respond to request to reset variable values of variables from other modules
         call Set_My_Variable (data_string)

      elseif (action.eq.ACTION_get_variable) then

         !Respond to request for variable values - from other modules
          call Send_My_Variable (Data_string)

      elseif (action.eq.ACTION_prepare) then !this happens each time step (daily)

         if (g%plant_status.ne.status_out) then

            !Zero all daily rate variables
            call Zero_Daily_Variables ()

            !Request and receive variables from owner-modules
            call Get_Other_Variables ()

            !Do crop processes prepare
            call Simulation_Prepare ()

         else
            ! Crop not in the field, do nothing
            call Zero_Variables (.false.)
         endif

      elseif (action.eq.ACTION_process) then


         if (g%plant_status.ne.status_out) then

            !request and receive variables from owner-modules
            call Get_Other_Variables ()

            !Crop processes - Dynamic prcess sub-modules
            call Crop_Process ()

            !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c           if (TestTrue) then

c               call Photoperiod_Wang (
c     :                 g%day_of_year
c     :               , g%latitude
c     :               , c%twilight
c     :               , daylength )

c               call ExtraTerrestrialRadiationDailyTotal (
c     :                 g%day_of_year
c     :               , g%latitude
c     :               , radn_ext )
c
c               call  Diffuse_Radiation_fraction
c     :                 (
c     :                 g%radn,
c     :                 radn_ext,
c     :                 diff_radn_frac
c     :                 )

c              call RUE_Diffuse_Radiation_Modifier (
c     :                 diff_radn_frac,
c     :                 rue_diff_radn_modifier
c     :                 )

c     .          g%tiller_area_max,
c     .          p%tiller_curve,
c     .          p%tiller_tt_infl,
c     .          g%tiller_area_pot,

c               WRITE(1,FMT="(i6,2x,10f7.2,
c     :                       10f7.2,
c     :                       10f7.2,
c     :                       10f7.2
c     :                       )")
c     :                       g%day_of_year
c     :                      ,g%tiller_area_max(1:10)
c     :                      ,p%tiller_curve(1:10)
c     :                      ,p%tiller_tt_infl(1:10)
c     :                      ,g%tiller_area_pot(1:10)
c
c               WRITE(1,FMT="(i6,  3x,
c     :                       f6.2,3x,
c     :                       f6.2,3x,
c     :                       f6.2,3x,
c     :                       f6.2,3x,
c     :                       f6.2,3x,
c     :                       )")
c     :                  g%day_of_year
c     :                 ,daylength
c     :                 ,radn_ext
c     :                 ,g%radn
c     :                 ,c%RUE_Max*rue_diff_radn_modifier
c     :                 ,1.34

c               PRINT *, '-------------------------------------'
c               PRINT *, 'DayOfYear =', g%day_of_year
c               PRINT *, 'day_len   =', daylength
c               PRINT *, 'radn_ext  =', radn_ext
c               PRINT *, 'radn_act  =', g%radn
c               PRINT *, 'RUE_max   =', RUE_Max*0.48
c               PRINT *, 'RUE_act   =', 1.34

c            endif

            !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


            !Send changes of other variables to owner-modules
            call Set_Other_Variables ()

         else
            !crop not in, do nothing
         endif

      elseif (action.eq.ACTION_end_crop) then

         !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c        if (TestTrue)   close (1)
         !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

         !end crop - turn the stover into residue
         call End_Crop ()

!            !Zero all the globals, but not the contants and parameters
!            call Zero_Variables (.false.)

         !Set plant status to status_out and stage to plant_end subroutine
         if (g%plant_status.ne.status_out) then
             g%plant_status  = status_out
             g%current_stage = real (plant_end)
         end if

         elseif (action.eq.ACTION_kill_crop) then
            ! kill crop - died, but biomass remain in field
            call Kill_Crop (
     .          g%plant_status,
     .          g%dm_green,
     .          g%dm_senesced,
     .          g%dm_dead)
      else
         ! don't use message
         call Message_unused ()
      endif


      call pop_routine (my_name)
      return
      end subroutine


! ====================================================================
! This routine is the event handler for all events
! ====================================================================
      subroutine respondToEvent(fromID, eventID, variant)
      Use infrastructure
      use CropModData
      Use CropModModule

      implicit none
      ml_external respondToEvent

      integer, intent(in) :: fromID
      integer, intent(in) :: eventID
      integer, intent(in) :: variant

      if (eventID .eq. id%sow) then 
         !request and receive variables from owner-modules
         call Get_Other_Variables ()

         !start crop, read the sow information and do  more initialisations
         call Start_Crop (variant)

      else if (eventID .eq. id%harvest) then 
               ! harvest crop - report harvest information
               call Crop_Harvest (
     .          g%dm_green,
     .          g%dm_dead,
     .          c%grn_water_cont,
     .          g%grain_no,
     .          g%plants,
     .          g%dm_senesced,
     .          g%leaf_no,
     .          g%N_green,
     .          g%N_dead,
     .          g%N_senesced,
     .          g%flowering_date,
     .          g%maturity_date,
     .          g%flowering_das,
     .          g%maturity_das,
     .          g%lai_max,
     .          g%cswd_photo,
     .          g%days_tot,
     .          g%cswd_expansion,
     .          g%cnd_photo,
     .          g%cnd_grain_conc,
     .          c%stage_names)
      endif
      
      return
      end subroutine respondToEvent

