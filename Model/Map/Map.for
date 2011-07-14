*     ========================================
      module MapModule
!     ========================================
      use Registrations
      
      integer    max_size              !the greatest size_of the arrays can be
      parameter (max_size = 50)

      integer    max_arrays              !the greatest size_of the arrays can be
      parameter (max_arrays = 10)

      character  module_name*(*)       ! name of this module
      parameter (module_name='map')


!     ========================================
      Type MapGlobals
         sequence
         real sw(max_size)
         real bd(max_size)
         real dlayer(max_size)
         integer nz
         real coeffs(max_size,max_size)
         real row_sum(max_size)
         logical update
         real map_dlayer(max_size)
         real sim_start(max_size)
         real sim_end(max_size)
         real satpaste_wc(max_size)
      end type MapGlobals
!     ========================================
      Type MapParameters
         sequence
         character*30 arrays2sum_names(max_arrays)
         character*30 arrays2ave_names(max_arrays)
         character*30 arrays2conc_names(max_arrays)
         character*30 arrays2concsw_names(max_arrays)
         character*30 arrays2satpaste_names(max_arrays)
         integer num2sum
         integer num2ave
         integer num2conc
         integer num2concsw
         integer num2satpaste

         real core_start(max_size)
         real core_end(max_size)
         integer num_output_layers

      end type MapParameters
!     ========================================
!      Type MapConstants
!
!      end type MapConstants
!     ========================================

      ! instance variables.
      common /InstancePointers/ ID,g,p,c
      save InstancePointers
      type (MapGlobals),pointer :: g
      type (MapParameters),pointer :: p
      type (IDsType), pointer :: id
      contains







* ====================================================================
       subroutine map_Init ()
* ====================================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Initialise map module

*+  Changes
*     <insert here>

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'map_init')

*+  Local Variables
       character Event_string*40       ! String to output

*- Implementation Section ----------------------------------
      call push_routine (myname)


      ! Notify system that we have initialised

      Event_string = 'Initialising'
      call Write_string (Event_string)

      call map_zero_variables ()

      call map_get_sim_dlayer ()

      call map_read_param ()

      Event_string = 'Soil Mapping initialised'
      call Write_string (Event_string)

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine map_zero_variables ()
* ====================================================================
      Use infrastructure
      implicit none

*+  Purpose
*     Set all variables in this module to zero.

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'map_zero_variables')

*+  Local Variables
      integer in
      integer out

*- Implementation Section ----------------------------------
      call push_routine (myname)

!arrays
      call fill_real_array(g%row_sum,0.0,max_size)
      call fill_real_array(g%dlayer,0.0,max_size)
      call fill_real_array(g%map_dlayer,0.0,max_size)
      call fill_real_array(p%core_start,0.0,max_size)
      call fill_real_array(p%core_end,0.0,max_size)
      call fill_real_array(g%sim_start,0.0,max_size)
      call fill_real_array(g%sim_end,0.0,max_size)
      call fill_real_array(g%sw,0.0,max_size)
      call fill_real_array(g%bd,0.0,max_size)
      call fill_real_array(g%satpaste_wc,0.0,max_size)

      call fill_char_array(p%arrays2sum_names,' ',max_arrays)
      call fill_char_array(p%arrays2ave_names,' ',max_arrays)
      call fill_char_array(p%arrays2conc_names,' ',max_arrays)
      call fill_char_array(p%arrays2concsw_names,' ',max_arrays)
      call fill_char_array(p%arrays2satpaste_names,' ',max_arrays)
      do out=1,max_size
         do in=1,max_size
            g%coeffs(out,in) = 0.0
         enddo
      enddo

!integers
      p%num_output_layers = 0
      g%nz = 0
      p%num2sum = 0
      p%num2ave = 0
      p%num2conc = 0
      p%num2concsw = 0
      p%num2satpaste = 0


!logical
      g%update = .true.

      call pop_routine (myname)

      return
      end subroutine



* ====================================================================
       recursive subroutine map_Send_my_variable (Variable_name)
* ====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
       character Variable_name*(*)     ! (INPUT) Variable name to search for

*+  Purpose
*      Return the value of one of our variables to caller

*+  Changes

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'map_send_my_variable')

*+  Local Variables
      integer arr_num

*- Implementation Section ----------------------------------
      call push_routine (myname)

      do arr_num=1,max(p%num2sum,p%num2ave,p%num2conc
     :                ,p%num2concsw,p%num2satpaste)
         if (Variable_name .eq. 'map_dlayer') then
            call map_send_new_dlayer ()
            goto 300 !because array has been sent - don't try further

         elseif
     :      (Variable_name .eq. 'sum_mineral_n') then
            call map_send_summed_var (arr_num)
            goto 300 !because array has been sent - don't try further

         elseif
     :      (Variable_name .eq. 'sum_'//p%arrays2sum_names(arr_num))
     :      then
            call map_send_summed_var (arr_num)
            goto 300 !because array has been sent - don't try further

         elseif(Variable_name.eq.'ave_'//p%arrays2ave_names(arr_num))
     :      then
            call map_send_averaged_var (arr_num)
            goto 300 !because array has been sent - don't try further

         elseif
     :      (Variable_name.eq.'conc_'//p%arrays2conc_names(arr_num))
     :      then
            call map_send_concentration (arr_num)
            goto 300 !because array has been sent - don't try further

         elseif
     :      (Variable_name.eq.
     :       'concsw_'//p%arrays2concsw_names(arr_num)) then
            call map_send_concentration_sw (arr_num)
            goto 300 !because array has been sent - don't try further

         elseif
     :    (Variable_name.eq.
     :      'satpaste_'//p%arrays2satpaste_names(arr_num)) then
            call map_send_satpaste (arr_num)
            goto 300 !because array has been sent - don't try further

         else
            !do nothing
         endif
      enddo

! We have checked all arrays and we did not respond to anything
            call Message_Unused ()
300   continue  !any arrays have been sent - do nothing

      call pop_routine (myname)
      return
      end subroutine



*     ===========================================================
      subroutine map_read_param ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*       Read all module parameters.

*+  Changes
*     <insert here>

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'map_read_param')
*
      character section_name*(*)
      parameter (section_name = 'parameters')

*+  Local Variables
      integer    numvals               ! number of values read
      real temp(max_size)
      integer in
      integer out
      character*30 name
      character*1 little_number
      character*2 big_number
      integer i
      character*1000 line
      character*3 out_txt

*- Implementation Section ----------------------------------

      call push_routine (myname)

      write(line,'(20a,3a,20a)')'Simulation array','-->', 'Output array'
      call write_string (line)

!arrays2sum_names = nh4n sw_dep
      call read_char_array_optional (
     :           section_name,        ! Section header
     :           'arrays2sum_names',      ! Keyword
     :           max_arrays,         ! array size
     :           '()',                ! Units
     :           p%arrays2sum_names,        ! Array
     :           p%num2sum)         ! Number of values returned
      if (p%num2sum .gt. 0) then
         do i=1,p%num2sum
            write (line,'(20a,3a,20a)')  p%arrays2sum_names(i),
     :                           '-->', 'sum_'//p%arrays2sum_names(i)
            call write_string (line)
         enddo
      endif

!arrays2ave_names = sw soil_temp
         call read_char_array_optional (
     :           section_name,        ! Section header
     :           'arrays2ave_names',      ! Keyword
     :           max_arrays,         ! array size
     :           '()',                ! Units
     :           p%arrays2ave_names,        ! Array
     :           p%num2ave)         ! Number of values returned
      if (p%num2ave .gt. 0) then
         do i=1,p%num2ave
            write (line,'(20a,3a,20a)')  p%arrays2ave_names(i),
     :                           '-->', 'ave_'//p%arrays2ave_names(i)
            call write_string (line)
         enddo
      endif

!arrays2conc_names = no3n  br
         call read_char_array_optional (
     :           section_name,        ! Section header
     :           'arrays2conc_names',      ! Keyword
     :           max_arrays,         ! array size
     :           '()',                ! Units
     :           p%arrays2conc_names,        ! Array
     :           p%num2conc)         ! Number of values returned
      if (p%num2conc .gt. 0) then
         do i=1,p%num2conc
            write (line,'(20a,3a,20a)')  p%arrays2conc_names(i),
     :                           '-->', 'conc_'//p%arrays2conc_names(i)
            call write_string (line)
         enddo
      endif

!arrays2concsw_names = no3n  br
         call read_char_array_optional (
     :           section_name,        ! Section header
     :           'arrays2concsw_names',      ! Keyword
     :           max_arrays,         ! array size
     :           '()',                ! Units
     :           p%arrays2concsw_names,        ! Array
     :           p%num2concsw)         ! Number of values returned
      if (p%num2concsw .gt. 0) then
         do i=1,p%num2concsw
            write (line,'(20a,3a,20a)')  p%arrays2concsw_names(i),
     :                '-->', 'concsw_'//p%arrays2concsw_names(i)
            call write_string (line)
         enddo
      endif

!arrays2satpaste_names = salt
         call read_char_array_optional (
     :           section_name,        ! Section header
     :           'arrays2satpaste_names',      ! Keyword
     :           max_arrays,         ! array size
     :           '()',                ! Units
     :           p%arrays2satpaste_names,        ! Array
     :           p%num2satpaste)         ! Number of values returned
      if (p%num2satpaste .gt. 0) then
         do i=1,p%num2satpaste
            write (line,'(20a,3a,50a)')  p%arrays2satpaste_names(i),
     :              '-->', 'satpaste_'//p%arrays2satpaste_names(i)
     :
            call write_string (line)
         enddo
      endif

!must be at least one array to play with
      if (p%num2sum+p%num2ave+p%num2conc + p%num2concsw + p%num2satpaste
     :  .eq. 0) call fatal_error (err_user, 'Must supply at least '//
     :'one array name')

!core_start = 0.0 100.0 ....
         call read_real_array_optional (
     :           section_name         ! section header
     :          ,'core_start'               ! keyword
     :          ,max_size         ! array size
     :          ,'(mm)'             ! units
     :          ,p%core_start                 ! array
     :          ,numvals             ! number of values returned
     :          ,0.0                  !lower
     :          ,100000.0)                 !upper
      if (numvals .eq. 0) then
         call fatal_error (err_user,'Number of values in core_start '//
     :'must be greater than zero')
      else
         p%num_output_layers = numvals
      endif

!core_end = 100.0 200.0 ....
         call read_real_array_optional (
     :           section_name         ! section header
     :          ,'core_end'               ! keyword
     :          ,max_size         ! array size
     :          ,'(mm)'             ! units
     :          ,p%core_end                 ! array
     :          ,numvals             ! number of values returned
     :          ,0.0                  !lower
     :          ,100000.0)                 !upper
      if (numvals .ne. p%num_output_layers) then
         call fatal_error (err_user,'Number of values in core_end '//
     :'must equal the number values in core_start')
      else
         do i=1,p%num_output_layers
            if (p%core_end(i) .lt. p%core_start(i)) then
               call fatal_error (err_user, 'core_end value '//
     :'shallower than core start value')
            endif
         enddo
!if gets to here then looks OK, calculate test coefficients
!calculate test coefficients
         call map_calc_coefficients ()
!calculate test map_dlayer
         call map_calc_new_dlayer ()
!calculate test row sums for averaging
         call map_calc_row_sum ()
!everything OK, write out to summary file
         call write_string (' ') !blank line
         call write_string ('Calculated coefficients')
         write (line,'(10a,2x,10a,30a)')  'Core_start',
     :                 '    Core_end','    Simulation Layer'
         call write_string (line)
         do out=1,p%num_output_layers
            write (line,'(f10.3,2x,f10.0,100(2x,f5.3))')
     :     p%core_start(out), p%core_end(out)
     :     , (g%coeffs(out,in),in=1,g%nz)
            call write_string (line)
         enddo
      endif

      if (p%num_output_layers .eq. 0) then
         g%update = .false.  !don't look for new dlayer etc
!layer_??
         do out=1,max_size
            if (out .le. 9) then
               write(little_number,'(i1)') out
               name = 'output_0'//little_number
            elseif (out .ge. 10) then
               write(big_number,'(i2)') out
               name = 'output_'//big_number
            endif
            call read_real_array_optional (
     :           section_name         ! section header
     :          ,name               ! keyword
     :          ,max_size         ! array size
     :          ,'(-)'             ! units
     :          ,temp                 ! array
     :          ,numvals             ! number of values returned
     :          ,0.0                  !lower
     :          ,1.0)                 !upper
            if (numvals .eq. 0) then
               p%num_output_layers = out - 1
               goto 100  !because there are no more outputs to be got
            elseif (numvals .ne. g%nz) then
               call fatal_error (err_user, 'Number of '//
     :'coefficients in output_?? must equal number of soil layers')
            endif
            do in=1,g%nz
               g%coeffs(out,in) = temp(in)
            enddo
         enddo
100      continue
         if (out .eq. 0) call fatal_error (err_user,'Map cannot '//
     :'find any output layer coefficients')
!check that each simulation layer has not been over allocated
         do in=1,g%nz
            temp(in) = 0.0
            do out=1,p%num_output_layers
               temp(in) = temp(in) + g%coeffs(out,in)
            enddo
            if (temp(in) .gt. 1.0) call fatal_error (err_user,
     :'Column sums of output_?? must be <= 1.0')
         enddo
!calculate test row sums for averaging
         call map_calc_row_sum ()
!echo coefficients to summary file
         do i=out,p%num_output_layers
            call integer_var_to_string(out, out_txt)
            write (line,'(10a,30(2x,f10.3))')
     :     'output_'//out_txt//'= ', (g%coeffs(out,in),in=1,g%nz)
            call write_string (line)
         enddo
      endif

      call pop_routine  (myname)
      return
      end subroutine



* ====================================================================
       subroutine map_set_my_variable (Variable_name)
* ====================================================================
      Use infrastructure
      implicit none

*+  Sub-Program Arguments
      character Variable_name*(*)      ! (INPUT) Variable name to search for

*+  Purpose
*     Set one of our variables altered by some other module

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'map_set_my_variable')

*- Implementation Section ----------------------------------
      call push_routine (myname)

c      if (Variable_name .eq. 'anything') then
c! do nothing
c      else
         call Message_Unused ()
c      endif

      call pop_routine (myname)

      return
      end subroutine



*     ===========================================================
      subroutine map_calc_row_sum ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*       Calculates the sum of the mapping coefficients for each
*       output layer

*+  Changes
*     <insert here>

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'map_calc_row_sum')

*+  Local Variables
      integer in
      integer out

*- Implementation Section ----------------------------------

      call push_routine (myname)

!calculate row sums for averaging
      do out=1,p%num_output_layers
         g%row_sum(out) = 0.0
         do in=1,g%nz
            g%row_sum(out) = g%row_sum(out) + g%coeffs(out,in)
         enddo
         if (g%row_sum(out) .le. 1e-6) call fatal_error (err_user,
     :'Sum of coefficients for each output layer must be greater than'//
     :' zero')
      enddo

      call pop_routine  (myname)
      return
      end subroutine



*     ===========================================================
      subroutine map_calc_new_dlayer ()
*     ===========================================================
      Use infrastructure
      implicit none

*+  Purpose
*       Maps the simulation layers into the output layers

*+  Changes
*     <insert here>

*+  Constant Values
      character  myname*(*)            ! name of this procedure
      parameter (myname = 'map_calc_new_dlayer')
*
      character section_name*(*)
      parameter (section_name = 'parameters')

*+  Local Variables
      integer in
      integer out

*- Implementation Section ----------------------------------

      call push_routine (myname)

!calculate map_dlayer
      do out=1,p%num_output_layers
         g%map_dlayer(out) = 0.0
         do in=1,g%nz
            g%map_dlayer(out) = g%map_dlayer(out) +
     :                         g%dlayer(in)*g%coeffs(out,in)
         enddo
      enddo

      call pop_routine  (myname)
      return
      end subroutine



* ====================================================================
       subroutine map_get_sim_dlayer ()
* ====================================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Finds out what the simulation layering is

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'map_get_sim_dlayer')

*+  Local Variables
       integer numvals              ! number of values returned
       integer i

*- Implementation Section ----------------------------------
      call push_routine (myname)

!dlayer
      call get_real_array (
     :      unknown_module ! module that responds (not used)
     :          ,'dlayer'               ! keyword
     :          ,max_size      ! Array Size
     :          ,'(mm)'             ! units
     :          ,g%dlayer                 ! array
     :          ,numvals             ! number of values returned
     :          ,0.0                  !lower
     :          ,1000.0)                 !upper
      if (numvals .eq. 0) then
         call fatal_error (err_user, 'Number of soil layers must be '//
     :'greater than 0')
      else
         g%nz = numvals
      endif

!calculate sim_start() and sim_end()
      g%sim_start(1) = 0.0
      do i=1,g%nz-1
         g%sim_end(i) = g%sim_start(i) + g%dlayer(i)
         g%sim_start(i+1) = g%sim_end(i)
      enddo
      g%sim_end(g%nz) = g%sim_start(g%nz) + g%dlayer(g%nz)

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine map_calc_coefficients ()
* ====================================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Calculates the mapping coefficients, 1=sim layer entirely within
*      output layer, <1 = part of sim layer in output layer

*+  Changes
*     <insert here>

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'map_calc_coefficients')

*+  Local Variables
       real sim_up
       real sim_low
       real core_up
       real core_low
       integer in
       integer out
       character  line*200              ! message

*- Implementation Section ----------------------------------
      call push_routine (myname)

      do out=1,p%num_output_layers
         do in = 1,g%nz
            sim_up = -1*g%sim_start(in)
            sim_low = -1*g%sim_end(in)
            core_up = -1*p%core_start(out)
            core_low = -1*p%core_end(out)

!sim layer below data layer
      if (sim_up .lt. core_low) then
         g%coeffs(out,in) = 0.0
!sim layer above data layer
      elseif (sim_low .gt. core_up) then
         g%coeffs(out,in) = 0.0
!sim layer engulfs data layer
      elseif ((sim_up .ge. core_up) .and. (sim_low .le. core_low)) then
!if point sample then special case
         if (core_up .eq. core_low) then
            g%coeffs(out,in) = 1.0
         else
            g%coeffs(out,in) = abs(core_up-core_low)/abs(sim_up-sim_low)
         endif
!sim layer engulfed by data layer
      elseif ((sim_up .le. core_up) .and. (sim_low .ge. core_low)) then
         g%coeffs(out,in) = 1.0
!sim layer spans upper bound of data layer
      elseif ((sim_up .lt. core_up) .and. (sim_low .lt. core_low)) then
         g%coeffs(out,in) = abs(core_low-sim_up)/abs(sim_low-sim_up)
!sim layer spans lower bound of data layer
      elseif ((sim_up .gt. core_up) .and. (sim_low .gt. core_low)) then
         g%coeffs(out,in) = abs(core_up-sim_low)/abs(sim_low-sim_up)
!point sample within or at node
      elseif (core_up .eq. core_low) then
         call write_string
     :       ('Point Sample case - should not be here!!!!!!!!!!')
         call fatal_error (err_internal,
     :                'Coefficients and point samples')
      else
!if didn't fall into one of these catagories then crash
         write (line,'(a,4(2x,f5.0))') 'Failure of coeff. gen. ',
     :            sim_up, sim_low, core_up, core_low
!         write(*,*) line
         call fatal_error (err_internal,line)
      endif

         enddo !in = 1,g%nz
      enddo !out=1,p%num_output_layers

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine map_Send_new_dlayer ()
* ====================================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Returns the output layering

*+  Changes
*    ????
*    070896 jngh added message_unused call at end subroutine
*                changed all literals of first argument in Respond2Get
*                to variable_name

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'map_send_my_variable')

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (g%update) then
         call map_get_sim_dlayer ()
         call map_calc_coefficients ()
         call map_calc_new_dlayer ()
         call map_calc_row_sum ()
      endif

      call respond2get_real_array (
     :               'map_dlayer',
     :               '(mm)',
     :               g%map_dlayer,
     :               p%num_output_layers)

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine map_Send_summed_var (arr_num)
* ====================================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Recalculates a variable to be summed into an output layer
*      - usually an amount thing, like sw_dep (mm) or salt (kg/ha)

*+  Changes
*    ????
*    070896 jngh added message_unused call at end subroutine
*                changed all literals of first argument in Respond2Get
*                to variable_name

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'map_Send_summed_var')

*+  Local Variables
      real mapped(max_size)
      real raw_array(max_size)
      integer in
      integer out
      integer arr_num
      integer numvals
      real temp_array(max_size)

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (g%update) then
         call map_get_sim_dlayer ()
         call map_calc_coefficients ()
         call map_calc_new_dlayer ()
         call map_calc_row_sum ()
      endif

      if (p%arrays2sum_names(arr_num) .eq. 'mineral_n') then
        call Get_real_array (
     :         unknown_module, ! Module that responds (Not Used)
     :         'no3',       ! Variable Name
     :         max_size,      ! Array Size
     :         '(kgN/ha)',         ! Units                (Not Used)
     :         temp_array,       ! Variable
     :         numvals,        ! Number of values returned
     :         0.,            ! Lower Limit for bound checking
     :         100000.)          ! Upper Limit for bound checking
         if (numvals .ne. g%nz) call fatal_error (err_user,
     :   'no3 must have same number of elements as dlayer')
         raw_array(1:g%nz) = temp_array(1:g%nz)
         call Get_real_array (
     :         unknown_module, ! Module that responds (Not Used)
     :         'nh4',       ! Variable Name
     :         max_size,      ! Array Size
     :         '(kgN/ha)',         ! Units                (Not Used)
     :         temp_array,       ! Variable
     :         numvals,        ! Number of values returned
     :         0.,            ! Lower Limit for bound checking
     :         100000.)          ! Upper Limit for bound checking
         if (numvals .ne. g%nz) call fatal_error (err_user,
     :   'nh4 must have same number of elements as dlayer')
         raw_array(1:g%nz) = raw_array(1:g%nz) + temp_array(1:g%nz)
         call Get_real_array (
     :         unknown_module, ! Module that responds (Not Used)
     :         'urea',       ! Variable Name
     :         max_size,      ! Array Size
     :         '(kgN/ha)',         ! Units                (Not Used)
     :         temp_array,       ! Variable
     :         numvals,        ! Number of values returned
     :         0.,            ! Lower Limit for bound checking
     :         100000.)          ! Upper Limit for bound checking
         if (numvals .ne. g%nz) call fatal_error (err_user,
     :   'urea must have same number of elements as dlayer')
         raw_array(1:g%nz) = raw_array(1:g%nz) + temp_array(1:g%nz)
      elseif (p%arrays2sum_names(arr_num) .eq. 'organic_n') then
        call Get_real_array (
     :         unknown_module, ! Module that responds (Not Used)
     :         'fom_n',       ! Variable Name
     :         max_size,      ! Array Size
     :         '(kgN/ha)',         ! Units                (Not Used)
     :         temp_array,       ! Variable
     :         numvals,        ! Number of values returned
     :         0.,            ! Lower Limit for bound checking
     :         100000.)          ! Upper Limit for bound checking
         if (numvals .ne. g%nz) call fatal_error (err_user,
     :   'fom_n must have same number of elements as dlayer')
         raw_array(1:g%nz) = temp_array(1:g%nz)
         call Get_real_array (
     :         unknown_module, ! Module that responds (Not Used)
     :         'biom_n',       ! Variable Name
     :         max_size,      ! Array Size
     :         '(kgN/ha)',         ! Units                (Not Used)
     :         temp_array,       ! Variable
     :         numvals,        ! Number of values returned
     :         0.,            ! Lower Limit for bound checking
     :         100000.)          ! Upper Limit for bound checking
         if (numvals .ne. g%nz) call fatal_error (err_user,
     :   'biom_n must have same number of elements as dlayer')
         raw_array(1:g%nz) = raw_array(1:g%nz) + temp_array(1:g%nz)
         call Get_real_array (
     :         unknown_module, ! Module that responds (Not Used)
     :         'hum_n',       ! Variable Name
     :         max_size,      ! Array Size
     :         '(kgN/ha)',         ! Units                (Not Used)
     :         temp_array,       ! Variable
     :         numvals,        ! Number of values returned
     :         0.,            ! Lower Limit for bound checking
     :         100000.)          ! Upper Limit for bound checking
         if (numvals .ne. g%nz) call fatal_error (err_user,
     :   'hum_n must have same number of elements as dlayer')
         raw_array(1:g%nz) = raw_array(1:g%nz) + temp_array(1:g%nz)
      else
         call Get_real_array (
     :         unknown_module, ! Module that responds (Not Used)
     :         p%arrays2sum_names(arr_num),       ! Variable Name
     :         max_size,      ! Array Size
     :         '(-)',         ! Units                (Not Used)
     :         raw_array,       ! Variable
     :         numvals,        ! Number of values returned
     :         -1.0e10,            ! Lower Limit for bound checking
     :         1.0e10)          ! Upper Limit for bound checking
         if (numvals .ne. g%nz) call fatal_error (err_user,
     :'Array to sum '//p%arrays2sum_names(arr_num)//' must have same '//
     :'number of elements as dlayer')
      endif


      do out=1,p%num_output_layers
         mapped(out) = 0.0
         do in=1,g%nz
            mapped(out) = mapped(out)+raw_array(in)*g%coeffs(out,in)
         enddo
      enddo

      call respond2get_real_array (
     :               'sum_'//p%arrays2sum_names(arr_num),
     :               '(????)',
     :               mapped,
     :               p%num_output_layers)

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine map_Send_averaged_var (arr_num)
* ====================================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Recalculates a variable to be averaged into an output layer
*      - usually an concentration thing, like no3_ppm (ppm)
*      or temperature (C)

*+  Changes
*    ????
*    070896 jngh added message_unused call at end subroutine
*                changed all literals of first argument in Respond2Get
*                to variable_name

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'map_Send_averaged_var')

*+  Local Variables
      real mapped(max_size)
      real raw_array(max_size)
      integer in
      integer out
      integer arr_num
      integer numvals
      real temp_array(max_size)

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (g%update) then
         call map_get_sim_dlayer ()
         call map_calc_coefficients ()
         call map_calc_new_dlayer ()
         call map_calc_row_sum ()
      endif

      if (p%arrays2ave_names(arr_num) .eq. 'mineral_n') then
        call Get_real_array (
     :         unknown_module, ! Module that responds (Not Used)
     :         'no3',       ! Variable Name
     :         max_size,      ! Array Size
     :         '(kgN/ha)',         ! Units                (Not Used)
     :         temp_array,       ! Variable
     :         numvals,        ! Number of values returned
     :         0.,            ! Lower Limit for bound checking
     :         100000.)          ! Upper Limit for bound checking
         if (numvals .ne. g%nz) call fatal_error (err_user,
     :   'no3 must have same number of elements as dlayer')
         raw_array(1:g%nz) = temp_array(1:g%nz)
         call Get_real_array (
     :         unknown_module, ! Module that responds (Not Used)
     :         'nh4',       ! Variable Name
     :         max_size,      ! Array Size
     :         '(kgN/ha)',         ! Units                (Not Used)
     :         temp_array,       ! Variable
     :         numvals,        ! Number of values returned
     :         0.,            ! Lower Limit for bound checking
     :         100000.)          ! Upper Limit for bound checking
         if (numvals .ne. g%nz) call fatal_error (err_user,
     :   'nh4 must have same number of elements as dlayer')
         raw_array(1:g%nz) = raw_array(1:g%nz) + temp_array(1:g%nz)
         call Get_real_array (
     :         unknown_module, ! Module that responds (Not Used)
     :         'urea',       ! Variable Name
     :         max_size,      ! Array Size
     :         '(kgN/ha)',         ! Units                (Not Used)
     :         temp_array,       ! Variable
     :         numvals,        ! Number of values returned
     :         0.,            ! Lower Limit for bound checking
     :         100000.)          ! Upper Limit for bound checking
         if (numvals .ne. g%nz) call fatal_error (err_user,
     :   'urea must have same number of elements as dlayer')
         raw_array(1:g%nz) = raw_array(1:g%nz) + temp_array(1:g%nz)
      elseif (p%arrays2ave_names(arr_num) .eq. 'organic_n') then
        call Get_real_array (
     :         unknown_module, ! Module that responds (Not Used)
     :         'fom_n',       ! Variable Name
     :         max_size,      ! Array Size
     :         '(kgN/ha)',         ! Units                (Not Used)
     :         temp_array,       ! Variable
     :         numvals,        ! Number of values returned
     :         0.,            ! Lower Limit for bound checking
     :         100000.)          ! Upper Limit for bound checking
         if (numvals .ne. g%nz) call fatal_error (err_user,
     :   'fom_n must have same number of elements as dlayer')
         raw_array(1:g%nz) = temp_array(1:g%nz)
         call Get_real_array (
     :         unknown_module, ! Module that responds (Not Used)
     :         'biom_n',       ! Variable Name
     :         max_size,      ! Array Size
     :         '(kgN/ha)',         ! Units                (Not Used)
     :         temp_array,       ! Variable
     :         numvals,        ! Number of values returned
     :         0.,            ! Lower Limit for bound checking
     :         100000.)          ! Upper Limit for bound checking
         if (numvals .ne. g%nz) call fatal_error (err_user,
     :   'biom_n must have same number of elements as dlayer')
         raw_array(1:g%nz) = raw_array(1:g%nz) + temp_array(1:g%nz)
         call Get_real_array (
     :         unknown_module, ! Module that responds (Not Used)
     :         'hum_n',       ! Variable Name
     :         max_size,      ! Array Size
     :         '(kgN/ha)',         ! Units                (Not Used)
     :         temp_array,       ! Variable
     :         numvals,        ! Number of values returned
     :         0.,            ! Lower Limit for bound checking
     :         100000.)          ! Upper Limit for bound checking
         if (numvals .ne. g%nz) call fatal_error (err_user,
     :   'hum_n must have same number of elements as dlayer')
         raw_array(1:g%nz) = raw_array(1:g%nz) + temp_array(1:g%nz)
      else
         call Get_real_array (
     :         unknown_module, ! Module that responds (Not Used)
     :         p%arrays2ave_names(arr_num),       ! Variable Name
     :         max_size,      ! Array Size
     :         '(-)',         ! Units                (Not Used)
     :         raw_array,       ! Variable
     :         numvals,        ! Number of values returned
     :         -1.0e10,            ! Lower Limit for bound checking
     :         1.0e10)          ! Upper Limit for bound checking
         if (numvals .ne. g%nz) call fatal_error (err_user,
     :'Array to sum '//p%arrays2ave_names(arr_num)//' must have same '//
     :'number of elements as dlayer')
      endif


      do out=1,p%num_output_layers
         mapped(out) = 0.0
         do in=1,g%nz
            mapped(out) = mapped(out) +
     :                    raw_array(in)*g%coeffs(out,in)/g%row_sum(out)
!Divide by zero trapped in calc_row_sum
         enddo
      enddo

      call respond2get_real_array (
     :               'ave_'//p%arrays2ave_names(arr_num),
     :               '(????)',
     :               mapped,
     :               p%num_output_layers)


      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine map_Send_concentration (arr_num)
* ====================================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Recalculates an amount variable (eg salt in kg/ha) into a
*      concentration in soil volume (g/m3)

*+  Changes
*    ????
*    070896 jngh added message_unused call at end subroutine
*                changed all literals of first argument in Respond2Get
*                to variable_name

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'map_Send_concentration')

*+  Local Variables
      real mapped(max_size)
      real raw_array(max_size)
      integer in
      integer out
      integer arr_num
      integer numvals
      real temp_array(max_size)

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (g%update) then
         call map_get_sim_dlayer ()
         call map_calc_coefficients ()
         call map_calc_new_dlayer ()
         call map_calc_row_sum ()
      endif

      if (p%arrays2conc_names(arr_num) .eq. 'mineral_n') then
        call Get_real_array (
     :         unknown_module, ! Module that responds (Not Used)
     :         'no3',       ! Variable Name
     :         max_size,      ! Array Size
     :         '(kgN/ha)',         ! Units                (Not Used)
     :         temp_array,       ! Variable
     :         numvals,        ! Number of values returned
     :         0.,            ! Lower Limit for bound checking
     :         100000.)          ! Upper Limit for bound checking
         if (numvals .ne. g%nz) call fatal_error (err_user,
     :   'no3 must have same number of elements as dlayer')
         raw_array(1:g%nz) = temp_array(1:g%nz)
         call Get_real_array (
     :         unknown_module, ! Module that responds (Not Used)
     :         'nh4',       ! Variable Name
     :         max_size,      ! Array Size
     :         '(kgN/ha)',         ! Units                (Not Used)
     :         temp_array,       ! Variable
     :         numvals,        ! Number of values returned
     :         0.,            ! Lower Limit for bound checking
     :         100000.)          ! Upper Limit for bound checking
         if (numvals .ne. g%nz) call fatal_error (err_user,
     :   'nh4 must have same number of elements as dlayer')
         raw_array(1:g%nz) = raw_array(1:g%nz) + temp_array(1:g%nz)
         call Get_real_array (
     :         unknown_module, ! Module that responds (Not Used)
     :         'urea',       ! Variable Name
     :         max_size,      ! Array Size
     :         '(kgN/ha)',         ! Units                (Not Used)
     :         temp_array,       ! Variable
     :         numvals,        ! Number of values returned
     :         0.,            ! Lower Limit for bound checking
     :         100000.)          ! Upper Limit for bound checking
         if (numvals .ne. g%nz) call fatal_error (err_user,
     :   'urea must have same number of elements as dlayer')
         raw_array(1:g%nz) = raw_array(1:g%nz) + temp_array(1:g%nz)
      elseif (p%arrays2conc_names(arr_num) .eq. 'organic_n') then
        call Get_real_array (
     :         unknown_module, ! Module that responds (Not Used)
     :         'fom_n',       ! Variable Name
     :         max_size,      ! Array Size
     :         '(kgN/ha)',         ! Units                (Not Used)
     :         temp_array,       ! Variable
     :         numvals,        ! Number of values returned
     :         0.,            ! Lower Limit for bound checking
     :         100000.)          ! Upper Limit for bound checking
         if (numvals .ne. g%nz) call fatal_error (err_user,
     :   'fom_n must have same number of elements as dlayer')
         raw_array(1:g%nz) = temp_array(1:g%nz)
         call Get_real_array (
     :         unknown_module, ! Module that responds (Not Used)
     :         'biom_n',       ! Variable Name
     :         max_size,      ! Array Size
     :         '(kgN/ha)',         ! Units                (Not Used)
     :         temp_array,       ! Variable
     :         numvals,        ! Number of values returned
     :         0.,            ! Lower Limit for bound checking
     :         100000.)          ! Upper Limit for bound checking
         if (numvals .ne. g%nz) call fatal_error (err_user,
     :   'biom_n must have same number of elements as dlayer')
         raw_array(1:g%nz) = raw_array(1:g%nz) + temp_array(1:g%nz)
         call Get_real_array (
     :         unknown_module, ! Module that responds (Not Used)
     :         'hum_n',       ! Variable Name
     :         max_size,      ! Array Size
     :         '(kgN/ha)',         ! Units                (Not Used)
     :         temp_array,       ! Variable
     :         numvals,        ! Number of values returned
     :         0.,            ! Lower Limit for bound checking
     :         100000.)          ! Upper Limit for bound checking
         if (numvals .ne. g%nz) call fatal_error (err_user,
     :   'hum_n must have same number of elements as dlayer')
         raw_array(1:g%nz) = raw_array(1:g%nz) + temp_array(1:g%nz)
      else
         call Get_real_array (
     :         unknown_module, ! Module that responds (Not Used)
     :         p%arrays2conc_names(arr_num),       ! Variable Name
     :         max_size,      ! Array Size
     :         '(-)',         ! Units                (Not Used)
     :         raw_array,       ! Variable
     :         numvals,        ! Number of values returned
     :         -1.0e10,            ! Lower Limit for bound checking
     :         1.0e10)          ! Upper Limit for bound checking
         if (numvals .ne. g%nz) call fatal_error (err_user,
     :'Array to sum '//p%arrays2conc_names(arr_num)
     : //' must have same '//
     :'number of elements as dlayer')
      endif

!sum into output layers
      do out=1,p%num_output_layers
         mapped(out) = 0.0
         do in=1,g%nz
            mapped(out) = mapped(out)+raw_array(in)*g%coeffs(out,in)
         enddo
      enddo

!convert into concentrations in soil volume
      do out=1,p%num_output_layers
         mapped(out) = mapped(out) / g%map_dlayer(out) * 100.0
      enddo

      call respond2get_real_array (
     :               'conc_'//p%arrays2conc_names(arr_num),
     :               '(g/m3_soil)',
     :               mapped,
     :               p%num_output_layers)

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine map_Send_concentration_sw (arr_num)
* ====================================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Recalculates an amount variable (eg salt in kg/ha) into a
*      concentration in soil water (g/m3)

*+  Changes
*    ????
*    070896 jngh added message_unused call at end subroutine
*                changed all literals of first argument in Respond2Get
*                to variable_name

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'map_Send_concentration_sw')

*+  Local Variables
      real mapped(max_size)
      real mapped_sw(max_size)
      real raw_array(max_size)
      integer in
      integer out
      integer arr_num
      integer numvals
      real temp_array(max_size)

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (g%update) then
         call map_get_sim_dlayer ()
         call map_calc_coefficients ()
         call map_calc_new_dlayer ()
         call map_calc_row_sum ()
      endif

      if (p%arrays2concsw_names(arr_num) .eq. 'mineral_n') then
        call Get_real_array (
     :         unknown_module, ! Module that responds (Not Used)
     :         'no3',       ! Variable Name
     :         max_size,      ! Array Size
     :         '(kgN/ha)',         ! Units                (Not Used)
     :         temp_array,       ! Variable
     :         numvals,        ! Number of values returned
     :         0.,            ! Lower Limit for bound checking
     :         100000.)          ! Upper Limit for bound checking
         if (numvals .ne. g%nz) call fatal_error (err_user,
     :   'no3 must have same number of elements as dlayer')
         raw_array(1:g%nz) = temp_array(1:g%nz)
         call Get_real_array (
     :         unknown_module, ! Module that responds (Not Used)
     :         'nh4',       ! Variable Name
     :         max_size,      ! Array Size
     :         '(kgN/ha)',         ! Units                (Not Used)
     :         temp_array,       ! Variable
     :         numvals,        ! Number of values returned
     :         0.,            ! Lower Limit for bound checking
     :         100000.)          ! Upper Limit for bound checking
         if (numvals .ne. g%nz) call fatal_error (err_user,
     :   'nh4 must have same number of elements as dlayer')
         raw_array(1:g%nz) = raw_array(1:g%nz) + temp_array(1:g%nz)
         call Get_real_array (
     :         unknown_module, ! Module that responds (Not Used)
     :         'urea',       ! Variable Name
     :         max_size,      ! Array Size
     :         '(kgN/ha)',         ! Units                (Not Used)
     :         temp_array,       ! Variable
     :         numvals,        ! Number of values returned
     :         0.,            ! Lower Limit for bound checking
     :         100000.)          ! Upper Limit for bound checking
         if (numvals .ne. g%nz) call fatal_error (err_user,
     :   'urea must have same number of elements as dlayer')
         raw_array(1:g%nz) = raw_array(1:g%nz) + temp_array(1:g%nz)
      elseif (p%arrays2concsw_names(arr_num) .eq. 'organic_n') then
        call Get_real_array (
     :         unknown_module, ! Module that responds (Not Used)
     :         'fom_n',       ! Variable Name
     :         max_size,      ! Array Size
     :         '(kgN/ha)',         ! Units                (Not Used)
     :         temp_array,       ! Variable
     :         numvals,        ! Number of values returned
     :         0.,            ! Lower Limit for bound checking
     :         100000.)          ! Upper Limit for bound checking
         if (numvals .ne. g%nz) call fatal_error (err_user,
     :   'fom_n must have same number of elements as dlayer')
         raw_array(1:g%nz) = temp_array(1:g%nz)
         call Get_real_array (
     :         unknown_module, ! Module that responds (Not Used)
     :         'biom_n',       ! Variable Name
     :         max_size,      ! Array Size
     :         '(kgN/ha)',         ! Units                (Not Used)
     :         temp_array,       ! Variable
     :         numvals,        ! Number of values returned
     :         0.,            ! Lower Limit for bound checking
     :         100000.)          ! Upper Limit for bound checking
         if (numvals .ne. g%nz) call fatal_error (err_user,
     :   'biom_n must have same number of elements as dlayer')
         raw_array(1:g%nz) = raw_array(1:g%nz) + temp_array(1:g%nz)
         call Get_real_array (
     :         unknown_module, ! Module that responds (Not Used)
     :         'hum_n',       ! Variable Name
     :         max_size,      ! Array Size
     :         '(kgN/ha)',         ! Units                (Not Used)
     :         temp_array,       ! Variable
     :         numvals,        ! Number of values returned
     :         0.,            ! Lower Limit for bound checking
     :         100000.)          ! Upper Limit for bound checking
         if (numvals .ne. g%nz) call fatal_error (err_user,
     :   'hum_n must have same number of elements as dlayer')

         raw_array(1:g%nz) = raw_array(1:g%nz) + temp_array(1:g%nz)
      else
         call Get_real_array (
     :         unknown_module, ! Module that responds (Not Used)
     :         p%arrays2concsw_names(arr_num),       ! Variable Name
     :         max_size,      ! Array Size
     :         '(-)',         ! Units                (Not Used)
     :         raw_array,       ! Variable
     :         numvals,        ! Number of values returned
     :         -1.0e10,            ! Lower Limit for bound checking
     :         1.0e10)          ! Upper Limit for bound checking
         if (numvals .ne. g%nz) call fatal_error (err_user,
     :'Array to sum '//p%arrays2concsw_names(arr_num)//' must have same'//
     :' number of elements as dlayer')
      endif

!sum into output layers
      do out=1,p%num_output_layers
         mapped(out) = 0.0
         do in=1,g%nz
            mapped(out) = mapped(out)+raw_array(in)*g%coeffs(out,in)
         enddo
      enddo

!get array of soil water concents
      call Get_real_array (
     :         unknown_module, ! Module that responds (Not Used)
     :         'sw',       ! Variable Name
     :         max_size,      ! Array Size
     :         '(m/m3)',         ! Units                (Not Used)
     :         g%sw,       ! Variable
     :         numvals,        ! Number of values returned
     :         1e-6,            ! Lower Limit for bound checking
     :         1.0)          ! Upper Limit for bound checking

      if (numvals .ne. g%nz) call fatal_error (err_user,
     :'sw must have same number of elements as dlayer')

!average sw into output layers
      do out=1,p%num_output_layers
         mapped_sw(out) = 0.0
         do in=1,g%nz
            mapped_sw(out) = mapped_sw(out) +
     :                         g%sw(in)*g%coeffs(out,in)/g%row_sum(out)
!Divide by zero trapped in calc_row_sum
         enddo
      enddo

!convert into concentrations in soil water
      do out=1,p%num_output_layers
!         mapped(out) = mapped(out) * mapped_sw(out)
!     :                         /
!     :            (g%map_dlayer(out)/1000.0 * 10.0)
         mapped(out) = mapped(out)                   !kgN/ha
     :                 / (g%map_dlayer(out) / 1000.0)  !/m     dlayer in m
     :                 / 10000.0                     !ha/m2  m2 per ha
     :                 * 1000.0                      !gN/kgN
     :                 / mapped_sw(out)              !m3soil/m3water
      enddo

      call respond2get_real_array (
     :               'concsw_'//p%arrays2concsw_names(arr_num),
     :               '(g/m3_water)',
     :               mapped,
     :               p%num_output_layers)

      call pop_routine (myname)
      return
      end subroutine



* ====================================================================
       subroutine map_Send_satpaste (arr_num)
* ====================================================================
      Use infrastructure
      implicit none

*+  Purpose
*      Recalculates an amount variable (eg salt in kg/ha) into a
*      saturation paste conductivity (dS/m)

*+  Changes
*    ????
*    070896 jngh added message_unused call at end subroutine
*                changed all literals of first argument in Respond2Get
*                to variable_name

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'map_Send_satpaste')

*+  Local Variables
      real mapped_ECe(max_size)
      real mapped_conc(max_size)
      real mapped_conc_water(max_size)
      real mapped_conc_adsorb(max_size)
      real mapped_satpaste_wc(max_size)
      real mapped_bd(max_size)
      real mapped_sw(max_size)
      real mapped_exco(max_size)
      real raw_array(max_size)
      character*30 var2get
      real denom(max_size)
      integer in
      integer out
      integer arr_num
      integer numvals

*- Implementation Section ----------------------------------
      call push_routine (myname)

      if (g%update) then
         call map_get_sim_dlayer ()
         call map_calc_coefficients ()
         call map_calc_new_dlayer ()
         call map_calc_row_sum ()
      endif

!get array of concentration adsorbed on soil
      var2get = "conc_adsorb_"//p%arrays2satpaste_names(arr_num)
      call Get_real_array (
     :         unknown_module, ! Module that responds (Not Used)
     :         var2get,       ! Variable Name
     :         max_size,      ! Array Size
     :         '(-)',         ! Units                (Not Used)
     :         raw_array,       ! Variable
     :         numvals,        ! Number of values returned
     :         0.0,            ! Lower Limit for bound checking
     :         1.0e10)          ! Upper Limit for bound checking
      if (numvals .ne. g%nz) call fatal_error (err_user,
     :'Array to conc '//var2get//' must have '//
     :'same number of elements as dlayer')
!average into output layers
      do out=1,p%num_output_layers
         mapped_conc_adsorb(out) = 0.0
         do in=1,g%nz
            mapped_conc_adsorb(out) = mapped_conc_adsorb(out)+
     :                  raw_array(in)*g%coeffs(out,in)/g%row_sum(out)
         enddo
      enddo

!get array of concentration in soil solution
      var2get = "conc_water_"//p%arrays2satpaste_names(arr_num)
      call Get_real_array (
     :         unknown_module, ! Module that responds (Not Used)
     :         var2get,       ! Variable Name
     :         max_size,      ! Array Size
     :         '(-)',         ! Units                (Not Used)
     :         raw_array,       ! Variable
     :         numvals,        ! Number of values returned
     :         0.0,            ! Lower Limit for bound checking
     :         1.0e10)          ! Upper Limit for bound checking
      if (numvals .ne. g%nz) call fatal_error (err_user,
     :'Array to conc '//var2get//' must have '//
     :'same number of elements as dlayer')
!average into output layers
      do out=1,p%num_output_layers
         mapped_conc_water(out) = 0.0
         do in=1,g%nz
            mapped_conc_water(out) = mapped_conc_water(out)+
     :                 raw_array(in)*g%coeffs(out,in)/g%row_sum(out)
         enddo
      enddo

!get array of bulk density
      call Get_real_array (
     :         unknown_module, ! Module that responds (Not Used)
     :         'bd',       ! Variable Name
     :         max_size,      ! Array Size
     :         '(Mg/m3)',         ! Units                (Not Used)
     :         raw_array,       ! Variable
     :         numvals,        ! Number of values returned
     :         0.0,            ! Lower Limit for bound checking
     :         2.6)          ! Upper Limit for bound checking
      if (numvals .ne. g%nz) call fatal_error (err_user,
     :'bulk density must have same number of elements as dlayer')
!average bulk density into output layers
      do out=1,p%num_output_layers
         mapped_bd(out) = 0.0
         do in=1,g%nz
            mapped_bd(out) = mapped_bd(out) +
     :            raw_array(in)*g%coeffs(out,in)/g%row_sum(out)
!Divide by zero trapped in calc_row_sum
         enddo
      enddo

!get array of volumetric soil water content
      call Get_real_array (
     :         unknown_module, ! Module that responds (Not Used)
     :         'sw',       ! Variable Name
     :         max_size,      ! Array Size
     :         '(m3/m3)',         ! Units                (Not Used)
     :         raw_array,       ! Variable
     :         numvals,        ! Number of values returned
     :         0.0,            ! Lower Limit for bound checking
     :         2.6)          ! Upper Limit for bound checking
      if (numvals .ne. g%nz) call fatal_error (err_user,
     :'bulk density must have same number of elements as dlayer')
!average water content into output layers
      do out=1,p%num_output_layers
         mapped_sw(out) = 0.0
         do in=1,g%nz
            mapped_sw(out) = mapped_sw(out) +
     :            raw_array(in)*g%coeffs(out,in)/g%row_sum(out)
!Divide by zero trapped in calc_row_sum
         enddo
      enddo

!get array of saturation paste water contents (grav) and convert to volumetric
      call Get_real_array (
     :         unknown_module, ! Module that responds (Not Used)
     :         'satpaste_wc',       ! Variable Name
     :         max_size,      ! Array Size
     :         '(g/g)',         ! Units                (Not Used)
     :         raw_array,       ! Variable
     :         numvals,        ! Number of values returned
     :         0.0,            ! Lower Limit for bound checking
     :         10.0)          ! Upper Limit for bound checking
      if (numvals .ne. g%nz) call fatal_error (err_user,
     :'satpaste_wc must have same number of elements as dlayer')
!average satpaste_wc into output layers
      do out=1,p%num_output_layers
         mapped_satpaste_wc(out) = 0.0
         do in=1,g%nz
            mapped_satpaste_wc(out) = mapped_satpaste_wc(out) +
     :              raw_array(in)*g%coeffs(out,in)/g%row_sum(out)
!Divide by zero trapped in calc_row_sum
         enddo
      enddo

!calculate concentration of solute in soil volume
      do out = 1, p%num_output_layers
         mapped_conc(out) = mapped_conc_water(out) * mapped_sw(out) +
     :                      mapped_conc_adsorb(out) * mapped_bd(out)
      end do

!calculate exco - if zero concentration, -99 and ECe = 0
      do out = 1, p%num_output_layers
         mapped_exco(out) = divide(mapped_conc_adsorb(out),
     :      mapped_conc_water(out), -99.0)
      end do

!calculate denominator - note cannot be zero because mapped_satpaste_wc > 0
      do out = 1, p%num_output_layers !implict divide satpaste_wc by density water -> vol wc
         denom(out) = (mapped_exco(out) + mapped_satpaste_wc(out)) *
     :                 mapped_bd(out)
      end do

!convert into ECe
      do out=1,p%num_output_layers
         mapped_ECe(out) = mapped_conc(out)              !g/m3 soil volume
     :                     / denom(out)                  !m3/Mg
     :                     / 640.0                       !/(m*mg/L/dS)
      enddo

      call respond2get_real_array (
     :               'satpaste_'//p%arrays2satpaste_names(arr_num),
     :               '(dS/m)',
     :               mapped_ECe,
     :               p%num_output_layers)

      call pop_routine (myname)
      return
      end subroutine



      end module MapModule

!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use MapModule
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
         allocate(p)
         allocate(id)
      else
         deallocate(g)
         deallocate(p)
         deallocate(id)
      end if
      return
      end subroutine



* ====================================================================
       subroutine Main (Action, Data_string)
* ====================================================================
      Use infrastructure
      Use MapModule
      implicit none
      ml_external Main

*+  Sub-Program Arguments
       character Action*(*)            ! Message action to perform
       character Data_string*(*)       ! Message data

*+  Purpose
*      This routine is the interface between the main system and the
*      map module.

*+  Changes

*+  Calls

*+  Constant Values
      character*(*) myname                 ! Name of this procedure
      parameter (myname = 'apsim_map')

*- Implementation Section ----------------------------------
      call push_routine (myname)

cnh      call set_warning_off ()

      if (Action.eq.ACTION_Init) then
         call map_Init ()

      else if (Action.eq.ACTION_Get_variable) then
         call map_Send_my_variable (Data_string)

      else if (Action.eq.ACTION_Set_variable) then
         call map_Set_my_variable (data_string)

      else
         ! Don't use message
         call Message_unused ()

      endif

      call pop_routine (myname)
      return
      end subroutine

      ! ====================================================================
      ! do first stage initialisation stuff.
      ! ====================================================================
      subroutine doInit1 ()
      use infrastructure
      use MapModule
      
      ml_external doInit1
!STDCALL(doInit1)
      
      call doRegistrations(id)
      call map_zero_variables()
      end subroutine


! ====================================================================
! This routine is the event handler for all events
! ====================================================================
      subroutine respondToEvent(fromID, eventID, variant)
      Use infrastructure
      implicit none
      ml_external respondToEvent
!STDCALL(respondToEvent)
      
      integer, intent(in) :: fromID
      integer, intent(in) :: eventID
      integer, intent(in) :: variant
      
      return
      end subroutine respondToEvent
