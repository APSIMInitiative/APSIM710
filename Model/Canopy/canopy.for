      module CanopyModule
      use Registrations
      use infrastructure
! ====================================================================
!     canopy constants
! ====================================================================

!   Short description:
!      canopy module constants

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
      integer    max_crops                    ! maximum number of crops in at once
      parameter (max_crops = 20)

      integer    module_name_size             ! maximum length of module name
      parameter (module_name_size = 32)


      type CanopyGlobals
         sequence
         integer    canopy_index(max_crops) ! index to sorted canopy height ()
         real       K_lai_green(max_crops)  ! k*green_lai of each crop ()
         real       K_lai_total(max_crops)  ! k*total_lai of each crop ()
         real       height(max_crops)       ! canopy height of crops (mm)
         real       intc_light(max_crops)   ! fraction of light intercepted by each
                                            ! crop canopy (0-1)
         integer    num_canopies            ! number of canopies present ()
         integer    num_crops               ! number of crops ()
         real       top_layer_light(max_crops) ! fraction of light at top of
                                                 ! each canopy layer (0-1)
         character  intercrop_list(max_crops)*(module_name_size)  ! list of modules to
                                                                    ! swap for
                                                                    ! intercropping
         integer  crop_module(max_crops)        ! list of modules replying
         logical   before_commence          ! Whether initialisation has completed properly
      end type CanopyGlobals
! ====================================================================
      ! instance variables.
      common /InstancePointers/ ID,g,p,c
      save InstancePointers
      type (CanopyGlobals),pointer :: g
      type (IDsType),pointer :: id

      contains


*     ===========================================================
      subroutine canopy_init ()
*     ===========================================================

      implicit none

*+  Purpose
*      Initialise canopy module. Output mesage and get list from control file.

*+  Changes
*     201093 jngh specified and programmed
*     210395 jngh changed from unknown_section to a defined section
*     280999 sdb removed version reference


*+  Constant Values
      character  my_name*(*)           ! procedure name
      parameter (my_name  = 'canopy_init')
*
      character  section_name*(*)      ! name of parameter section
      parameter (section_name = 'parameters')

*+  Local Variables
      integer    num_modules           ! number of module names in list
      character  line*200              ! message
      integer    i                     ! loop counter

*- Implementation Section ----------------------------------

      call push_routine (my_name)

            ! initialisation message

      call Write_string (' Initialising')

            ! now get intercropping swap list from control file

      call read_char_array_optional (section_name
     :                   , 'intercrop', max_crops, '()'
     :                   , g%intercrop_list, num_modules)

      call bound_check_integer_var (num_modules, 0, max_crops
     :                            , 'num_modules')

         ! now report initial conditions

      if (num_modules.gt.1) then
         write (line, '(a)')  ' Module rotation for intercropping :'
         call write_string (line)

         write (line, '(100a)')  (g%intercrop_list(i), i=1, num_modules)
         call write_string (line)

      else
         ! no swapping required
         write (line,'(a)')
     :             ' No module rotation for intercropping'
         call write_string (line)
      endif

      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================
      subroutine canopy_find_crops ()
* ====================================================================

      implicit none

*+  Purpose
*      Find what crops are in system

*+  Changes
*     090896 jngh - Programmed and Specified
*     261196 jngh lengthened crop_type to 100 from 20 and set it blank before us

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'canopy_find_crops')

*+  Local Variables
      integer    crop                  ! index for crops
      character  crop_type*100         ! type of crop
      integer    numvals               ! number of values in string
      integer owner_module             ! owner module of variable

*- Implementation Section ----------------------------------
      call push_routine (myname)


      crop = 0
      crop_type = blank
1000  continue

         call get_char_vars(
     :             crop + 1
     :           , 'crop_type'
     :           , '()'
     :           , crop_type
     :           , numvals)

         if (numvals.ne.0) then
            if (crop+1.le.max_crops) then
               crop = crop + 1
               Owner_module = get_posting_Module ()

               g%crop_module(crop) = owner_module
               goto 1000
            else
               call fatal_error (err_user
     :            , 'Too many modules with crop type.')
            endif
         else
         endif

      g%num_crops = crop

      call pop_routine (myname)
      return
      end subroutine



*     ===========================================================
      subroutine canopy_zero_all_variables ()
*     ===========================================================

      implicit none

*+  Purpose
*     Set all variables in this module to zero.

*+  Changes
*      201093 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! procedure name
      parameter (my_name  = 'canopy_zero_all_variables')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      g%canopy_index = 0
      g%k_lai_total  = 0.0
      g%k_lai_green  = 0.0
      g%height       = 0.0
      g%intc_light   = 0.0
      g%num_canopies = 0
      g%num_crops    = 0
      g%top_layer_light = 0.0

      g%intercrop_list = blank
      g%crop_module    = 0
      g%before_commence = .true.


      call pop_routine (my_name)
      return
      end subroutine

*     ===========================================================
      subroutine canopy_zero_variables ()
*     ===========================================================

      implicit none

*+  Purpose
*     Set all variables in this module to zero.

*+  Changes
*      201093 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! procedure name
      parameter (my_name  = 'canopy_zero_variables')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call fill_integer_array (g%canopy_index, 0, max_crops)
      call fill_real_array (g%k_lai_total, 0.0, max_crops)
      call fill_real_array (g%k_lai_green, 0.0, max_crops)
      call fill_real_array (g%height, 0.0, max_crops)
      call fill_real_array (g%intc_light, 0.0 ,max_crops)
      call fill_real_array (g%top_layer_light, 0.0, max_crops)

      g%num_canopies = 0
      g%num_crops = 0

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine canopy_get_other_variables ()
*     ===========================================================

      implicit none

*+  Purpose
*      Get the values of variables from other modules

*+  Changes
*      201093 jngh specified and programmed
*      261196 jngh tested incoming cover for 1. Set log to 100.0 if it is.

*+  Constant Values
      real       max_height          ! maximum crop canopy height (mm)
      parameter (max_height  = 40000.0)
*
      real       k_lai_full_cover    ! a value for k*lai when cover is 100%
      parameter (k_lai_full_cover = 100.0)
*
      character  my_name*(*)           ! procedure name
      parameter (my_name='canopy_get_other_variables')

*+  Local Variables
      integer    crop                  ! index for crops
      real       temp                  !
      integer    numvals               ! number of values in string
      integer    owner_module          ! owner module of variable

*- Implementation Section ----------------------------------

      call push_routine (my_name)

             ! Get green cover of each crop

      crop = 0
1000  continue

         call get_real_vars (crop+1, 'cover_green', '()'
     :                              , temp, numvals
     :                              , 0.0, 1.0)

         if (numvals.ne.0) then
            if (crop+1.le.max_crops) then
               crop = crop + 1
               Owner_module = get_posting_Module ()
               g%crop_module(crop) = owner_module
               if (temp.lt.1) then
                  g%K_lai_green(crop) = - log (1.0 - temp)
               else
                  g%K_lai_green(crop) = k_lai_full_cover
               endif
               goto 1000
            else
               call fatal_error (err_user
     :            , 'Too many modules with green cover.')
            endif
         else
         endif

         if (crop.ne.g%num_crops) then
            call fatal_error (err_user
     :              , 'Number of modules with green cover different to '
     :              // 'number of modules with crop type.')
         else
         endif

            ! Get total cover of each crop

      crop = 0
2000  continue
         call get_real_vars (crop+1, 'cover_tot', '(mm)'
     :                              , temp, numvals
     :                              , 0.0, 1.0)
         if (numvals.ne.0) then
            if (crop+1.le.max_crops) then
               crop = crop + 1
               Owner_module = get_posting_Module ()
               if (owner_module.eq.g%crop_module(crop)) then
                  if (temp.lt.1) then
                     g%K_lai_total(crop) = - log (1.0 - temp)
                  else
                     g%K_lai_total(crop) = k_lai_full_cover
                  endif
                  goto 2000
               else
                  call fatal_error (err_user
     :              , 'Modules with total cover do not match '
     :             // 'modules with green cover')
               endif
            else
               call fatal_error (err_user
     :            , 'Too many modules with total cover.')
            endif
         else
         endif

         if (crop.ne.g%num_crops) then
            call fatal_error (err_user
     :              , 'Number of modules with total cover different to '
     :              // 'number of modules with green cover.')
         else
         endif

            ! Get canopy heights

      crop = 0
3000  continue
         call get_real_vars (crop+1, 'height', '(mm)'
     :                             , temp, numvals
     :                             , 0.0, max_height)
         if (numvals.ne.0) then
            if (crop+1.le.max_crops) then
               crop = crop + 1
               Owner_module = get_posting_Module ()
               if (owner_module.eq.g%crop_module(crop)) then
                  g%height(crop) = temp
                  goto 3000
               else
                  call fatal_error (err_user
     :              , 'Modules with height do not match '
     :             // 'modules with green cover')
               endif
            else
               call fatal_error (err_user
     :                  , 'Too many modules with height.')
            endif
         else
         endif

         if (crop.ne.g%num_crops) then
            call fatal_error (err_user
     :              , 'Number of modules with height different to '
     :              // 'number of modules with green cover.')
         else
         endif
      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine canopy_send_my_variable (Variable_name)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      character  Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Purpose
*      Return the value of one of our variables to caller

*+  Changes
*      201093 jngh specified and programmed
*      011195 jngh  added call to message_unused
*      010896 jngh changed method of getting module name for gets
*      120996 jngh removed print statement
*      021199 jngh added export of cover_tot_all and cover_height_all arrays

*+  Calls
c      integer    canopy_crop_number    ! function

*+  Constant Values
      character  my_name*(*)           ! procedure name
      parameter (my_name='canopy_send_my_variable')
*
      character  fr_intc_radn_name*(*) ! name of fr_intc_radn variable
      parameter (fr_intc_radn_name = 'fr_intc_radn_')
*
      integer    fr_intc_radn_name_length ! length of name
      parameter (fr_intc_radn_name_length = 13)
*
*   Internal variables - second round
      character  temp_variable_name*(fr_intc_radn_name_length)
                                       ! temporary storage of first part of
                                       !  variable name

*+  Local Variables
      real       cover                 ! temporary cover variable
      integer    module                ! module counter
      character  module_string*(max_module_name_size) ! module name
      real       cover_tot_all(max_crops)   ! total cover of each crop (0-1)
      real       cover_green_all(max_crops) ! green cover of each crop (0-1)
      integer    moduleID
      integer    numvals
      logical    found

*- Implementation Section ----------------------------------

      call push_routine (my_name)


      temp_variable_name = variable_name

      if (temp_variable_name .eq. fr_intc_radn_name) then
         module_string = Variable_name(fr_intc_radn_name_length+1:)
         found = component_name_to_ID(module_string,moduleID)

         if (.not.found) then
            call fatal_error(err_internal
     :                       , 'Bad module id sent with fr_intc_radn.'
     :                       // 'Module id = '
     :                       // module_string)
         else

            module = canopy_crop_number (moduleID)

            if (g%before_commence) then
               call respond2get_real_var (variable_name, '()'
     :                                   , 0.0)
            else if (module.gt.0) then
               call respond2get_real_var (variable_name, '()'
     :                                   , g%intc_light(module))
            else
               call fatal_error (err_user
     :              , 'Module: ' // module_string
     :              // ' requested fr_intc_radn and does not '
     :              // 'have a canopy')

            endif
         endif

      else if (variable_name.eq.'cover_tot_sum') then
         cover = 1.0
     :         - exp (-sum_real_array (g%K_lai_total, g%num_crops))
         call respond2get_real_var (variable_name, '()', cover)

      else if (variable_name.eq.'cover_tot_all') then
         cover_tot_all(:) = 0.0
         do 1000 module = 1, g%num_crops
            cover_tot_all(module) = 1.0
     :                            - exp (-g%K_lai_total(module))
1000     continue
         call respond2get_real_array (variable_name, '()'
     :                                 , cover_tot_all, g%num_crops)

      else if (variable_name.eq.'cover_height_all') then
         call respond2get_real_array (variable_name, '()', g%height
     :                                 , g%num_crops)

      else if (variable_name.eq.'cover_green_sum') then
         cover = 1.0
     :         - exp (-sum_real_array (g%K_lai_green, g%num_crops))
         call respond2get_real_var (variable_name, '()', cover)

      else if (variable_name.eq.'cover_green_all') then
         cover_tot_all(:) = 0.0
         do 2000 module = 1, g%num_crops
            cover_green_all(module) = 1.0
     :                              - exp (-g%K_lai_green(module))
2000     continue
         call respond2get_real_array (variable_name, '()'
     :                                 , cover_green_all, g%num_crops)

      else if (variable_name.eq.'cover_crops_all') then
         call respond2get_integer_array (variable_name, '()'
     :                                 , g%crop_module
     :                                 , g%num_crops)

      else
            ! don't own the variable
         call Message_unused ()
      endif

      call pop_routine (my_name)
      return
      end subroutine



* ====================================================================
       integer function canopy_crop_number (module_id)
* ====================================================================

      implicit none

*+  Sub-Program Arguments
      integer  module_id         ! (INPUT) id of crop to locate

*+  Purpose
*     Return the position of the module_name in module_names array

*+  Changes
*        090896 jngh - Programmed and Specified

*+  Constant Values
      character*(*) myname               ! name of current procedure
      parameter (myname = 'canopy_crop_number')

*+  Local Variables
      integer    crop                  ! crop counter
      integer    crop_num              ! position of crop in array

*- Implementation Section ----------------------------------
      call push_routine (myname)


      do 1000 crop = 1, g%num_crops

         if (module_id.eq.g%crop_module(crop)) then

            crop_num = crop
            goto 1100
         else
         endif

1000  continue
      crop_num = 0

1100  continue

      canopy_crop_number = crop_num

      call pop_routine (myname)
      return
      end function



*     ===========================================================
      subroutine canopy_prepare ()
*     ===========================================================

      implicit none

*+  Purpose
*     Perform calculations before the current timestep. This is the main
*     processing for the arbitrator

*+  Changes
*      201093 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! procedure name
      parameter (my_name='canopy_prepare')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

            ! determine crops with canopies now

      call canopy_canopies_present (g%canopy_index, g%num_canopies)

      if (g%num_canopies.gt.0) then

               ! get light transmitted through each layer

         call canopy_top_layer_light (g%top_layer_light)

               ! get light intercepted by each crop canopy

         call canopy_intc_light (g%intc_light)

      else
            ! no canopies present
      endif

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine canopy_canopies_present (canopy_index, num_canopies)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      integer    canopy_index(*)       ! (OUTPUT) presence of canopy and order
      integer    num_canopies          ! (OUTPUT) number of canopies present

*+  Purpose
*     Determine which canopies are present and their order from top down.

*+  Changes
*      201093 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! procedure name
      parameter (my_name='canopy_canopies_present')

*+  Local Variables
      real       temp(max_crops)       ! temporary height array for sorting
      real       temp1(max_crops)      ! temporary height array for counting

*- Implementation Section ----------------------------------

      call push_routine (my_name)

            ! determine crops with canopies now

            ! We put the heights into a temporary array as negative numbers,
            ! sort that into ascending order, with a key to their original
            ! position before sortine.  This gives us an index to the
            ! height array in descending order of height.

      call fill_real_array (temp, 0.0, max_crops)
      call subtract_real_array (g%height, temp, max_crops)
      call fill_integer_array (canopy_index, 0, max_crops)

            ! determine order of canopies from top down

      call shell_sort_real (temp, -max_crops, canopy_index)

      call fill_real_array (temp1, 0.0, max_crops)
      call subtract_real_array (temp, temp1, max_crops)
      num_canopies = count_of_real_vals (temp1, max_crops)

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine canopy_top_layer_light (layer_light)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      real       layer_light(*)        ! (OUTPUT) light at top of canopy
                                       ! (0-1)

*+  Purpose
*     Determine light at top of each canopy.

*+  Changes
*      201093 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! procedure name
      parameter (my_name='canopy_top_layer_light')

*+  Local Variables
      integer    layer_no              ! layer number in combined canopy ()
      real       K_lai_in_layer(max_crops) ! K*lai product for each canopy in
                                       ! layer
      real       K_lai_in_layer_sum    ! total K*lai of canopies in layer ()
                                       ! (area leaf/area soil)
      integer    layer                 ! layer counter in total canopy ()
      real       light_in              ! fraction of light entering layer (0-1)
      real       light_out             ! fraction of light leaving layer (0-1)
      integer    num_layers            ! number of layers in total canopy ()

*- Implementation Section ----------------------------------

      call push_routine (my_name)

            ! initialise canopy light array and top of combined canopy

      light_out = 1.0
      call fill_real_array (layer_light, 0.0, max_crops)

            ! We define the layer boundaries by the top of each canopy.
            ! Thus thwre are as many layers as canopies.
            ! We now take each layer in turn from the top, in the combined
            ! canopy, and thenlook at each canopy in that layer to get the
            ! combined K*lai value of the canopies present in that layer.
            ! The fractiion of light transmitted out of the bottom of
            ! that layer can be calculated, which is in turn the fraction
            ! entering the next layer below.
            ! The lai here is the lai of green and dead leaves.

      num_layers = g%num_canopies

            ! take each layer in turn from top.

      do 1000 layer = 1, num_layers
         light_in = light_out
         layer_no = g%canopy_index(layer)
         layer_light(layer_no) = light_in

               ! get the combined K*lai of the canopies.

         call canopy_k_lai (K_lai_in_layer, g%K_lai_total, layer)
         K_lai_in_layer_sum = sum_real_array (K_lai_in_layer, max_crops)

               ! now we can get the fraction of transmitted light

               ! this equation implies that leaf interception of radiation
               ! obeys beer's law.

         light_out = exp (-K_lai_in_layer_sum)*light_in
1000  continue

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine canopy_intc_light (intc_light)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      real       intc_light(*)         ! (OUTPUT) fraction of light at top
                                       ! of canopy (0-1)

*+  Purpose
*     Determine light fraction captured by green leaf of each canopy. (0-1)

*+  Changes
*      201093 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! procedure name
      parameter (my_name='canopy_intc_light')

*+  Local Variables
      integer    crop                  ! pointer to current crop array ()
      integer    canopy_in_layer       ! canopy counter in layer ()
      integer    layer_no              ! layer number in combined canopy ()
      real       K_lai_in_layer(max_crops) ! K*lai product for each canopy in
                                       ! layer ()
      real       fr_light_intc         ! fraction of light intercepted (0-1)
      real       K_lai_in_layer_sum    ! total K*lai of canopies in layer ()
                                       ! (area leaf/area soil)
      integer    layer                 ! layer counter in total canopy ()
      real       light_in              ! fraction of light entering layer (0-1)
      real       light_used_in_layer   ! fraction of light used in layer (0-1)
      integer    num_layers            ! number of layers in total canopy ()

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call fill_real_array (intc_light, 0.0, max_crops)

            ! Here we take each layer in turn from the top down, get the
            ! light used by the combined canopy and then apportion that
            ! to each canopy occupying the layer

      num_layers = g%num_canopies
      do 2000 layer = 1, num_layers

               ! get the combined K*lai of the canopies.

         call canopy_k_lai (K_lai_in_layer, g%K_lai_green, layer)
         K_lai_in_layer_sum = sum_real_array (K_lai_in_layer, max_crops)

               ! get the fraction of light used in the layer

         layer_no = g%canopy_index(layer)
         light_in = g%top_layer_light(layer_no)

               ! this equation implies that leaf interception of radiation
               ! obeys beer's law.

         light_used_in_layer = (1.0 - exp (-K_lai_in_layer_sum))
     :                       * light_in

               ! now we divide the total light used amongst the canopies
               ! occupying the layer.  This is done on the basis of the
               ! K*lai product_of of each canopy as its structure (K) must
               ! be taken into account.

         do 1000 canopy_in_layer = 1, g%num_canopies
            crop = g%canopy_index(canopy_in_layer)

cjh            note that the fraction is of the total green - perhaps it
cjh            should be of total tot. This method also ignores the shape
cjh            of the canopies within the layer.

            fr_light_intc = divide (K_lai_in_layer(crop)
     :                            , K_lai_in_layer_sum, 0.0)
            intc_light(crop) = intc_light(crop)
     :                       + fr_light_intc*light_used_in_layer
1000     continue
2000  continue

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      subroutine canopy_k_lai (K_lai_in_layer, K_lai, layer)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      real       K_lai_in_layer(*)     ! (OUTPUT) K*lai product for each
                                       ! crop in layer
      real       K_lai(*)              ! (INPUT) K_lai's of crop canopies
      integer    layer                 ! (INPUT) layer number in total canopy

*+  Purpose
*     Determine product of K and lai for each canopy in a specified layer.

*+  Changes
*      201093 jngh specified and programmed

*+  Calls
c      real       canopy_fract_canopy   ! function

*+  Constant Values
      character  my_name*(*)           ! procedure name
      parameter (my_name='canopy_k_lai')

*+  Local Variables
      integer    canopies_in_layer     ! number of canopies in layer ()
      integer    crop                  ! pointer to current crop array ()
      integer    canopy                ! canopy counter in layer ()

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      call fill_real_array (K_lai_in_layer, 0.0, max_crops)

            ! now take each canopy in turn that possibly lies in the layer
            ! and get its K*lai product_of

      canopies_in_layer = layer
      do 1000 canopy = 1, canopies_in_layer
         crop = g%canopy_index(canopy)
         K_lai_in_layer(crop) = canopy_fract_canopy (crop, layer)
     :                        * K_lai(crop)

1000  continue

      call pop_routine (my_name)
      return
      end subroutine



*     ===========================================================
      real function canopy_fract_canopy (crop, layer)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      integer    crop                  ! (INPUT) crop canopy number
      integer    layer                 ! (INPUT) layer number

*+  Purpose
*     Returns fraction of specified canopy in specified layer. (0-1)

*+  Changes
*      201093 jngh specified and programmed

*+  Calls
            ! describe the canopy shape as a function of height.
c      external   canopy_width
c      real       canopy_width          ! function

*+  Constant Values
      character  my_name*(*)           ! procedure name
      parameter (my_name='canopy_fract_canopy')

*+  Local Variables
      integer    layer_no              ! layer number in combined canopy ()
      real       height_at_top         ! height to top of layer (mm)
      real       height_at_bottom      ! height to bottom of layer (mm)
      integer    next_layer            ! layer number in combined canopy ()
      real       part_in_layer         ! area in layer ()
      real       total_canopy          ! area of total canopy ()

*- Implementation Section ----------------------------------

      call push_routine (my_name)

            ! we get the heights of the top and bottom of the layer and
            ! then find the k_lai contained in each of the heights,
            ! the difference being the k_lai in the layer.

      layer_no = g%canopy_index(layer)
      call bound_check_integer_var (layer+1, 0, max_crops, 'layer+1')
      next_layer = g%canopy_index(layer+1)

      height_at_top = divide (g%height(layer_no)
     :                      , g%height(crop), 0.0)
      height_at_top = bound (height_at_top, 0.0, 1.0)

      height_at_bottom = divide (g%height(next_layer)
     :                         , g%height(crop), 0.0)
      height_at_bottom = bound (height_at_bottom, 0.0, 1.0)

      part_in_layer = integrate_real_lg (height_at_bottom, height_at_top
     :                                 , canopy_width)
      total_canopy = integrate_real_lg (0.0, 1.0, canopy_width)

      canopy_fract_canopy = divide (part_in_layer, total_canopy, 0.0)
      call pop_routine (my_name)
      return
      end function



*     ===========================================================
      real function canopy_width (height_in_canopy)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      real       height_in_canopy      ! (INPUT) normalised height (0-1)

*+  Purpose
*       describe canopy shape as a function of normalised height

*+  Changes
*       201193 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)            ! procedure name
      parameter (my_name = 'canopy_width')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      canopy_width = height_in_canopy**5.0

      call pop_routine (my_name)

      return
      end function



*     ===========================================================
      subroutine canopy_post ()
*     ===========================================================

      implicit none

*+  Purpose
*     Perform calculations after the current timestep.

*+  Changes
*      201093 jngh specified and programmed

*+  Constant Values
      character  my_name*(*)           ! procedure name
      parameter (my_name='canopy_post')

*+  Local Variables
      integer    num_in_list           ! number of names in crop list

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      num_in_list = count_of_char_vals (g%intercrop_list, max_crops)
      if (num_in_list.gt.1) then
         call Change_component_order(g%intercrop_list, num_in_list)
      else
         ! no swapping required
      endif

      call pop_routine (my_name)
      return
      end subroutine



      end module CanopyModule

!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use CanopyModule
      implicit none
      ml_external alloc_dealloc_instance
!STDCALL(alloc_dealloc_instance)

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




*     ===========================================================
      subroutine Main (Action, Data_string)
*     ===========================================================

      use CanopyModule
      implicit none
      ml_external Main

*+  Sub-Program Arguments
      character  Action*(*)            ! (INPUT) Message action to perform
      character  Data_string*(*)       ! (INPUT) Message data

*+  Purpose
*      This routine is the interface between the main system and the
*      canopy module.

*+  Changes
*      201093 jngh specified and programmed
*      011195 jngh  added call to message_unused
*      090299 jngh removed find crops and get other variables from init
*      100299 jngh added find crops back in
*      280999 sdb removed version reference

*+  Constant Values
      character  my_name*(*)
      parameter (my_name = 'canopy_main')

*- Implementation Section ----------------------------------

      call push_routine (my_name)

      if (Action.eq.ACTION_Get_variable) then
            ! respond to requests from other modules
         call canopy_send_my_variable (Data_string)

      elseif (Action .eq. ACTION_Prepare) then
         g%before_commence = .false.
         call canopy_zero_variables ()
         call canopy_find_crops ()
         call canopy_get_other_variables ()
         call canopy_prepare ()

      else if (Action .eq. ACTION_Post) then
         call canopy_post ()

      else if (Action .eq. 'start') then
         g%before_commence = .false.

      else if (Action.eq.ACTION_Init) then
         call canopy_zero_all_variables ()
         call canopy_init ()
!         call canopy_find_crops ()
!         call canopy_get_other_variables ()

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
      use CanopyModule
      
      ml_external doInit1
!STDCALL(doInit1)
      
      call doRegistrations(id)
      end subroutine
 

 ! ====================================================================
! This routine is the event handler for all events
! ====================================================================
      subroutine respondToEvent(fromID, eventID, variant)
      implicit none
      ml_external respondToEvent
!STDCALL(respondToEvent)

      integer, intent(in) :: fromID
      integer, intent(in) :: eventID
      integer, intent(in) :: variant

      return
      end subroutine respondToEvent
