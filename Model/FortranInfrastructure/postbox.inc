
      integer POSTBOX_SIZE
      parameter (POSTBOX_SIZE = 500)   ! Total number of numbers that can
                                       ! be put in postbox.
      integer MAX_VARIABLES
      parameter (MAX_VARIABLES = 200)  ! Total number of variables that can
                                       ! be put in postbox for all messages.
 
      integer CHAR_SIZE
      parameter (CHAR_SIZE = 500)      ! Size of a character string in postbox.
 
      integer MAX_POSTBOXES
      parameter (MAX_POSTBOXES = 20)    ! maximum number of messages that can be
                                       ! put into postbox system.
 
      integer MAX_VARIABLE_UNIT_SIZE
      parameter (MAX_VARIABLE_UNIT_SIZE = 32)
                                       ! maximum size of a unit.
 
      integer DOUBLE_TYPE
      parameter (DOUBLE_TYPE = 1)      ! double type id
 
      integer CHAR_TYPE
      parameter (CHAR_TYPE = 2)        ! char type id
 
      integer REAL_TYPE                ! ID of real
      parameter (REAL_TYPE=3)
 
      integer INTEGER_TYPE             ! ID of integer
      parameter (INTEGER_TYPE=4)      
 
      integer LOGICAL_TYPE             ! ID of logical
      parameter (LOGICAL_TYPE=5)
 
      integer MAX_ARRAY_SIZE
      parameter (MAX_ARRAY_SIZE=150)   ! maximum size of array

      integer MAX_SAVED_EI
      parameter (MAX_SAVED_EI=20)      ! maximum number of levels of recursion

! ====================================================================
       common /postbox_double/
     .   g_Postbox_double
! ====================================================================
 
!   Short description:
!     Double precision variable for postbox.
 
!   Assumptions:
!      None
 
!   Notes:
!     None
 
!   Changes:
!     DPH 18/10/95
 
! ----------------------- Declaration section ------------------------
 
      double precision g_Postbox_double(POSTBOX_SIZE)   
                              ! Storage for double precision postbox items.


! ====================================================================
       common /postbox_char/
     .   g_Postbox_char,
     .   g_Variable_name,
     .   g_Variable_unit,
     .   g_Last_respond_units,
     .   g_Last_respond_module,
     .   g_variable_owners
! ====================================================================
 
!   Short description:
!     Character variables for postbox.
 
!   Assumptions:
!      None
 
!   Notes:
!     None
 
!   Changes:
!     DPH 18/10/95
 
! ----------------------- Declaration section ------------------------
 
      character g_Postbox_char(POSTBOX_SIZE)*(CHAR_SIZE)
                                       ! Storage for double precision postbox items.
      character g_Variable_name(MAX_VARIABLES)*(MAX_VARIABLE_NAME_SIZE)
                                       ! Variable name
      character g_Variable_unit(MAX_VARIABLES)*(MAX_VARIABLE_UNIT_SIZE)
                                       ! Variable unit          
      character g_last_respond_module*(MAX_MODULE_NAME_SIZE)
                                       ! name of the module that previous posted a variable
      character g_Last_respond_units*(MAX_VARIABLE_UNIT_SIZE)
                                       ! Units of last variable responded to.
      character g_variable_owners(MAX_VARIABLES)*(MAX_MODULE_NAME_SIZE)
                                       ! module owner for each variable posted.


! ====================================================================
       common /postbox_integer/
     .   g_Variable_start,
     .   g_Postbox_start,
     .   g_Postbox_end,
     .   g_Postbox_type,
     .   g_Current_message,
     .   g_Empty_double_slot,
     .   g_Empty_char_slot,
     .   g_Empty_variable_slot,
     .   g_Last_respond_type,
     .   g_SavedEIStack,
     .   g_SavedEIStackPtr

! ====================================================================
 
!   Short description:
!     Integer variables for postbox.
 
!   Assumptions:
!      None
 
!   Notes:
!     None
 
!   Changes:
!     DPH 18/10/95
 
! ----------------------- Declaration section ------------------------
 
      integer g_Variable_start(MAX_POSTBOXES)
                                       ! Pointer to variable array for each message
      integer g_Postbox_start(MAX_VARIABLES)
                                       ! Start pointer to postbox for each varaible
      integer g_Postbox_end(MAX_VARIABLES)
                                       ! End pointer to postbox for each varaible
      integer g_Postbox_type(MAX_VARIABLES)
                                       ! Specifies whether Postbox_start and end point
                                       ! to the postbox_double or Postbox_char arrays.

      integer g_Current_message        ! Pointer to Variable_start array for current
                                       ! message
      integer g_Empty_double_slot      ! Pointer to first empty slot in Postbox_double array
      integer g_Empty_char_slot        ! Pointer to first empty slot in Postbox_char array
      integer g_Empty_variable_slot    ! Pointer to first empty slot in variable arrays
      integer g_Last_respond_type      ! Data type of last variable respond.
      integer g_SavedEIStack(MAX_SAVED_EI)  ! saved event interfaces
      integer g_SavedEIStackPtr        ! index into g_savedeistack