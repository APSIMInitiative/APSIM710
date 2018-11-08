      Module crp_commModule
      use ConstantsModule
      use stringModule
      use errorModule
      use convertModule
      use ComponentInterfaceModule
      Use infrastructure

      contains

!     Last change:  P     1 Nov 2000   12:11 pm
!     ===========================================================
      subroutine crop_root_incorp (          &
                             dlt_dm_root          &
                            ,dlt_N_root          &
                            ,g_dlayer          &
                            ,g_root_length          &
                            ,g_root_depth          &
                            ,c_crop_type          &
                            ,max_layer          &
                            ,incorpFOMID)
!     ===========================================================

!      dll_export crop_root_incorp
      use crp_rootModule
      implicit none

!+  Sub-Program Arguments
      real       dlt_dm_root           ! (INPUT) new root residue dm (g/m^2)
      real       dlt_N_root            ! (INPUT) new root residue N (g/m^2)
      real       g_dlayer(*)           ! (INPUT) layer thicknesses (mm)
      real       g_root_length(*)      ! (INPUT) layered root length (mm)
      real       g_root_depth          ! (INPUT) root depth (mm)
      character  c_crop_type*(*)       ! (INPUT) crop type
      integer    max_layer             ! (INPUT) maximum no of soil layers
      integer    incorpFOMID           ! (INPUT) ID of incorp fom event.
!+  Purpose
!       Calculate and provide root matter incorporation information
!       to the APSIM messaging system.

!+  Mission Statement
!   Pass root material to the soil modules (based on root length distribution)

!+  Changes
!     <insert here>
!     280800 jngh changed literal incorp_fom to ACTION_incorp_fom
!     011100 dph  added event_interface as a parameter.

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'crop_root_incorp')

!+  Local Variables
      integer    deepest_layer         ! deepest layer in which the roots are
                                       ! growing
      real       dlt_dm_incorp(crop_max_layer) ! root residue (kg/ha)
      real       dlt_N_incorp(crop_max_layer)  ! root residue N (kg/ha)
      type(FOMLayerType) :: IncorpFOM
      integer layer

!- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (max_layer .gt. crop_max_layer) then
         call fatal_error (Err_Internal          &
                    ,'Too many layers for crop routines')

      else

         if (dlt_dm_root.gt.0.0) then

               ! send out root residue

            call crop_root_dist          &
               (          &
                G_dlayer          &
              , G_root_length          &
              , G_root_depth          &
              , dlt_dm_incorp          &
              , dlt_dm_root * gm2kg /sm2ha          &
              , max_layer          &
               )

            call bound_check_real_array          &
                (          &
                 dlt_dm_incorp          &
                ,0.0          &
                ,dlt_dm_root * gm2kg/sm2ha          &
                ,'dlt_dm_incorp'          &
                ,max_layer          &
                )

            call crop_root_dist          &
               (          &
                G_dlayer          &
              , G_root_length          &
              , G_root_depth          &
              , dlt_N_incorp          &
              , dlt_N_root * gm2kg /sm2ha          &
              , max_layer          &
               )
            call bound_check_real_array          &
               (          &
                dlt_n_incorp          &
               ,0.0          &
               ,dlt_n_root * gm2kg/sm2ha          &
               ,'dlt_n_incorp'          &
               ,max_layer          &
               )

            deepest_layer = find_layer_no          &
                         (g_root_depth          &
                         ,g_dlayer          &
                         ,max_layer)

            IncorpFOM%Type = c_crop_type
            IncorpFOM%num_layer = deepest_layer
            do layer = 1, deepest_layer
               IncorpFOM%layer(layer)%FOM%Amount = dlt_dm_incorp(layer)
               IncorpFOM%layer(layer)%FOM%N = dlt_n_incorp(layer)
               IncorpFOM%layer(layer)%FOM%P = 0.0
               IncorpFOM%layer(layer)%FOM%C = 0.0
               IncorpFOM%layer(layer)%FOM%AshAlk = 0.0
               IncorpFOM%layer(layer)%CNR = 0.0
               IncorpFOM%layer(layer)%LabileP = 0.0
            enddo
            call publish_FOMLayer(incorpFOMID, IncorpFOM)

         else
            ! no roots to incorporate
         endif
      endif

      call pop_routine (my_name)
      return
      end subroutine



!     ===========================================================
      subroutine crop_top_residue (          &
                                   c_crop_type          &
                                  ,dlt_residue_weight          &
                                  ,dlt_residue_N          &
                                  )
!     ===========================================================

!      dll_export crop_top_residue
      implicit none

!+  Sub-Program Arguments
      character c_crop_type*(*)
      real       dlt_residue_weight    ! (INPUT) new surface residue (g/m^2)
      real       dlt_residue_N         ! (INPUT) new surface residue N (g/m^2)

!+  Purpose
!       Add residue to residue pool

!+  Mission Statement
!   Pass surface residues to the residue module

!+  Changes
!     <insert here>
!     280800 jngh changed ok = Loader_SendActionToFirstComp
!                    to call   Loader_SendActionToAllComps (D372)
!     280800 jngh changed literal add_residue to ACTION_add_residue
!     011100 dph  added event_interface as a parameter.

!+  Constant Values
      character  my_name*(*)           ! name of procedure
      parameter (my_name  = 'crop_top_residue')

!+  Local Variables
      logical ok
!- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (dlt_residue_weight.gt.0.0) then
            ! send out surface residue

         call New_postbox ()

         call post_char_var('dlt_residue_type','()',c_crop_type)

         call post_real_var ('dlt_residue_wt'          &
                        ,'(kg/ha)'          &
                        ,dlt_residue_weight * gm2kg /sm2ha)

         call post_real_var ('dlt_residue_n'          &
                        ,'(kg/ha)'          &
                        ,dlt_residue_N * gm2kg /sm2ha)

         call event_send(unknown_module,ACTION_add_residue)

         call Delete_postbox ()

      else
         ! no surface residue
      endif

      call pop_routine (my_name)
      return
      end subroutine

! ====================================================================
      subroutine Send_BiomassRemoved_Event (event_ID                 &
                                            , crop_type              &
                                            , dm_type                &
                                            , dlt_crop_dm            &
                                            , dlt_dm_n               &
                                            , fraction_to_Residue    &
                                            , max_part)
! ====================================================================
      implicit none

!+  Sub-Program Arguments
      integer event_ID                      ! (INPUT) event id
      character  crop_type*(*)              ! (INPUT) crop type
      character  dm_type(*)*(*)             ! (INPUT) residue type
      real  dlt_crop_dm(*)                  ! (INPUT) residue weight (kg/ha)
      real  dlt_dm_n(*)                     ! (INPUT) residue N weight (kg/ha)
      real  fraction_to_Residue(*)          ! (INPUT) residue fraction to residue (0-1)
      integer max_part                      ! (INPUT) number of residue types
	  
!+  Purpose
!     Notify other modules of crop chopped.

!+  Mission Statement
!     Notify other modules of crop chopped.

!+  Changes
!   281103 nih - Copied from plant module

!+  Local variables
      type(BiomassRemovedType) :: chopped

!- Implementation Section ----------------------------------

      chopped%crop_type = crop_type
      chopped%dm_type(1:max_part) = dm_type(1:max_part)
      chopped%num_dm_type = max_part
      chopped%dlt_crop_dm(1:max_part) = dlt_crop_dm(1:max_part)
      chopped%num_dlt_crop_dm = max_part
      chopped%dlt_dm_n(1:max_part) = dlt_dm_n(1:max_part)
      chopped%num_dlt_dm_n = max_part
      chopped%dlt_dm_p(1:max_array_size) = 0.0
      chopped%num_dlt_dm_p = max_part
      chopped%fraction_to_residue(1:max_part) = fraction_to_Residue(1:max_part)
      chopped%num_fraction_to_residue = max_part
      call publish_BiomassRemoved(event_ID, chopped)

      return
      end subroutine
	  
! ====================================================================
      subroutine Send_BiomassRemoved_Event_N_P (event_ID             &
                                            , crop_type              &
                                            , dm_type                &
                                            , dlt_crop_dm            &
                                            , dlt_dm_n               &
                                            , dlt_dm_p               &
                                            , fraction_to_Residue    &
                                            , max_part)      
! ====================================================================
      implicit none

!+  Sub-Program Arguments
      integer event_ID                      ! (INPUT) event id
      character  crop_type*(*)              ! (INPUT) crop type
      character  dm_type(*)*(*)             ! (INPUT) residue type
      real  dlt_crop_dm(*)                  ! (INPUT) residue weight (kg/ha)
      real  dlt_dm_n(*)                     ! (INPUT) residue N weight (kg/ha)
      real  dlt_dm_p(*)                     ! (INPUT) residue P weight (kg/ha)
      real  fraction_to_Residue(*)          ! (INPUT) residue fraction to residue (0-1)
      integer max_part                      ! (INPUT) number of residue types
	  
!+  Purpose
!     Notify other modules of crop chopped.

!+  Mission Statement
!     Notify other modules of crop chopped.

!+  Changes
!   281103 nih - Copied from plant module

!+  Local variables
      type(BiomassRemovedType) :: chopped

!- Implementation Section ----------------------------------

      chopped%crop_type = crop_type
      chopped%dm_type(1:max_part) = dm_type(1:max_part)
      chopped%num_dm_type = max_part
      chopped%dlt_crop_dm(1:max_part) = dlt_crop_dm(1:max_part)
      chopped%num_dlt_crop_dm = max_part
      chopped%dlt_dm_n(1:max_part) = dlt_dm_n(1:max_part)
      chopped%num_dlt_dm_n = max_part
      chopped%dlt_dm_p(1:max_array_size) = dlt_dm_p(1:max_part)
      chopped%num_dlt_dm_p = max_part
      chopped%fraction_to_residue(1:max_part) = fraction_to_Residue(1:max_part)
      chopped%num_fraction_to_residue = max_part
      call publish_BiomassRemoved(event_ID, chopped)

      return
      end subroutine
	  
! ====================================================================
      subroutine Send_Crop_Chopped_Event (crop_type                    &
                                                , dm_type              &
                                                , dlt_crop_dm          &
                                                , dlt_dm_n             &
                                                , fraction_to_Residue  &
                                                , max_part)
! ====================================================================
      implicit none

!+  Sub-Program Arguments
      character  crop_type*(*)              ! (INPUT) crop type
      character  dm_type(*)*(*)             ! (INPUT) residue type
      real  dlt_crop_dm(*)                  ! (INPUT) residue weight (kg/ha)
      real  dlt_dm_n(*)                     ! (INPUT) residue N weight (kg/ha)
      real  fraction_to_Residue(*)          ! (INPUT) residue fraction to residue (0-1)
      integer max_part                      ! (INPUT) number of residue types
!+  Purpose
!     Notify other modules of crop chopped.

!+  Mission Statement
!     Notify other modules of crop chopped.

!+  Changes
!   070999 jngh - Programmed and Specified
!   190901 jngh - corrected dm_type to array

!+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Send_Crop_Chopped_Event')

!- Implementation Section ----------------------------------
      call push_routine (myname)

      call new_postbox ()

         ! send message regardless of fatal error - will stop anyway


!cjh      write(*,*) 'plant: '//EVENT_Crop_Chopped
!cjh      write(*,*) 'plant: '//DATA_crop_type
!cjh     :               , ' '//crop_type
!cjh      write(*,*) 'plant: '//DATA_dm_type
!cjh     :               , ' '//dm_type
!cjh      write(*,*) 'plant: '//DATA_dlt_crop_dm
!cjh     :               , dlt_crop_dm
!cjh      write(*,*) 'plant: '//DATA_dlt_dm_n
!cjh     :               , dlt_dm_n
!cjh      write(*,*) 'plant: '//DATA_fraction_to_Residue
!cjh     :               , fraction_to_Residue

      call post_char_var   (DATA_crop_type  &
                             ,'()'         &
                             , crop_type)
      call post_char_array (DATA_dm_type    &
                             ,'()'         &
                             , dm_type     &
                             , max_part)
      call post_real_array (DATA_dlt_crop_dm  &
                             ,'(kg/ha)'      &
                             , dlt_crop_dm   &
                             , max_part)
      call post_real_array (DATA_dlt_dm_n     &
                             ,'(kg/ha)'      &
                             , dlt_dm_n      &
                             , max_part)
      call post_real_array (DATA_fraction_to_Residue   &
                             ,'()'                    &
                             , fraction_to_Residue    &
                             , max_part)

      call event_send (unknown_module,EVENT_Crop_Chopped)

      call delete_postbox ()


      call pop_routine (myname)
      return
      end subroutine

! ====================================================================
      subroutine Send_Crop_Chopped_Event_N_P (crop_type                &
                                                , dm_type              &
                                                , dlt_crop_dm          &
                                                , dlt_dm_n             &
                                                , dlt_dm_p             &
                                                , fraction_to_Residue  &
                                                , max_part)
! ====================================================================
      implicit none

!+  Sub-Program Arguments
      character  crop_type*(*)              ! (INPUT) crop type
      character  dm_type(*)*(*)             ! (INPUT) residue type
      real  dlt_crop_dm(*)                  ! (INPUT) residue weight (kg/ha)
      real  dlt_dm_n(*)                     ! (INPUT) residue N weight (kg/ha)
      real  dlt_dm_p(*)                     ! (INPUT) residue P weight (kg/ha)
      real  fraction_to_Residue(*)          ! (INPUT) residue fraction to residue (0-1)
      integer max_part                      ! (INPUT) number of residue types
!+  Purpose
!     Notify other modules of crop chopped.

!+  Mission Statement
!     Notify other modules of crop chopped.

!+  Changes
!   070999 jngh - Programmed and Specified
!   190901 jngh - corrected dm_type to array

!+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'Send_Crop_Chopped_Event_N_P')

!- Implementation Section ----------------------------------
      call push_routine (myname)

      call new_postbox ()

         ! send message regardless of fatal error - will stop anyway


!cjh      write(*,*) 'plant: '//EVENT_Crop_Chopped
!cjh      write(*,*) 'plant: '//DATA_crop_type
!cjh     :               , ' '//crop_type
!cjh      write(*,*) 'plant: '//DATA_dm_type
!cjh     :               , ' '//dm_type
!cjh      write(*,*) 'plant: '//DATA_dlt_crop_dm
!cjh     :               , dlt_crop_dm
!cjh      write(*,*) 'plant: '//DATA_dlt_dm_n
!cjh     :               , dlt_dm_n
!cjh      write(*,*) 'plant: '//DATA_fraction_to_Residue
!cjh     :               , fraction_to_Residue

      call post_char_var   (DATA_crop_type  &
                             ,'()'         &
                             , crop_type)
      call post_char_array (DATA_dm_type    &
                             ,'()'         &
                             , dm_type     &
                             , max_part)
      call post_real_array (DATA_dlt_crop_dm  &
                             ,'(kg/ha)'      &
                             , dlt_crop_dm   &
                             , max_part)
      call post_real_array (DATA_dlt_dm_n     &
                             ,'(kg/ha)'      &
                             , dlt_dm_n      &
                             , max_part)
      call post_real_array (DATA_dlt_dm_p     &
                             ,'(kg/ha)'      &
                             , dlt_dm_p      &
                             , max_part)
      call post_real_array (DATA_fraction_to_Residue   &
                             ,'()'                    &
                             , fraction_to_Residue    &
                             , max_part)

      call event_send (unknown_module,EVENT_Crop_Chopped)

      call delete_postbox ()


      call pop_routine (myname)
      return
      end subroutine




! ====================================================================
      subroutine crop_get_ext_uptakes (uptake_source          &
                                ,crop_type          &
                                ,uptake_type          &
                                ,unit_conversion_factor          &
                                ,uptake_lbound          &
                                ,uptake_ubound          &
                                ,uptake_array          &
                                ,max_layer          &
                                )
! ====================================================================

!      dll_export crop_get_ext_uptakes
      implicit none

!+  Sub-Program Arguments
      character uptake_source*(*)   !(INPUT) uptake flag
      character crop_type*(*)       !(INPUT) crop type name
      character uptake_type*(*)     !(INPUT) uptake name
      real      unit_conversion_factor!(INPUT) unit conversion factor
      real      uptake_lbound       !(INPUT) uptake lower limit
      real      uptake_ubound       !(INPUT) uptake upper limit
      real      uptake_array(*)     !(OUTPUT) crop uptake array
      integer   max_layer           !(INPUT) max layer number

!+  Purpose
!      Ask swim for uptakes of water or solute

!+  Mission Statement
!   Get the soil uptake for %3 from another module

!+  Notes
!      Bounds should probably be passed in when crops decide what
!      these should be (ie when ini files have limits for uptake
!      in them)

!+  Changes
!     08-05-1997 - huth - Programmed and Specified

!+  Calls


!+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'crop_get_ext_uptakes')

!+  Local Variables
      integer   layer                        ! layer counter
      integer   num_uptakes                  ! num uptake vals
      character uptake_name*(MES_DATA_SIZE)  ! Uptake variable name

!- Implementation Section ----------------------------------
      call push_routine (myname)

      if ((uptake_source.eq.'apsim' .or. uptake_source.eq.'swim3')         &
          .and.          &
           (crop_type.ne.' '))          &
                               then
         ! NB - if crop type is blank then swim will know nothing
         ! about this crop (eg if not initialised yet)

         uptake_name = string_concat('uptake_',uptake_type)
         uptake_name = string_concat(uptake_name,'_')
         uptake_name = string_concat(uptake_name,crop_type)

         call get_real_array (unknown_module          &
                       ,uptake_name          &
                       ,max_layer          &
                       ,'()'          &
                       ,uptake_array          &
                       ,num_uptakes          &
                       ,uptake_lbound          &
                       ,uptake_ubound)

         do 100 layer = 1, num_uptakes
            uptake_array(layer) = uptake_array(layer)          &
                          * unit_conversion_factor
  100    continue
      else
      endif

      call pop_routine (myname)
      return
      end subroutine



! ====================================================================
      subroutine crop_get_ext_supply (uptake_source          &
                                ,crop_type          &
                                ,uptake_type          &
                                ,unit_conversion_factor          &
                                ,uptake_lbound          &
                                ,uptake_ubound          &
                                ,uptake_array          &
                                ,max_layer          &
                                )
! ====================================================================

!      dll_export crop_get_ext_uptakes
      implicit none

!+  Sub-Program Arguments
      character uptake_source*(*)   !(INPUT) uptake flag
      character crop_type*(*)       !(INPUT) crop type name
      character uptake_type*(*)     !(INPUT) uptake name
      real      unit_conversion_factor!(INPUT) unit conversion factor
      real      uptake_lbound       !(INPUT) uptake lower limit
      real      uptake_ubound       !(INPUT) uptake upper limit
      real      uptake_array(*)     !(OUTPUT) crop uptake array
      integer   max_layer           !(INPUT) max layer number

!+  Purpose
!      Ask swim for potential supply of water or solute

!+  Mission Statement
!   Get the soil potential supply for %3 from another module

!+  Notes
!      Bounds should probably be passed in when crops decide what
!      these should be (ie when ini files have limits for uptake
!      in them)


!+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'crop_get_ext_supply')

!+  Local Variables
      integer   layer                        ! layer counter
      integer   num_uptakes                  ! num uptake vals
      character uptake_name*(MES_DATA_SIZE)  ! Uptake variable name

!- Implementation Section ----------------------------------
      call push_routine (myname)

      if ((uptake_source.eq.'apsim')          &
          .and.          &
    (crop_type.ne.' '))          &
then
         ! NB - if crop type is blank then swim will know nothing
         ! about this crop (eg if not initialised yet)

         uptake_name = string_concat('supply_',uptake_type)
         uptake_name = string_concat(uptake_name,'_')
         uptake_name = string_concat(uptake_name,crop_type)

         call get_real_array (unknown_module          &
                       ,uptake_name          &
                       ,max_layer          &
                       ,'()'          &
                       ,uptake_array          &
                       ,num_uptakes          &
                       ,uptake_lbound          &
                       ,uptake_ubound)

         do 100 layer = 1, num_uptakes
            uptake_array(layer) = uptake_array(layer)          &
                          * unit_conversion_factor
  100    continue
      else
      endif

      call pop_routine (myname)
      return
      end subroutine


      end module crp_commModule
