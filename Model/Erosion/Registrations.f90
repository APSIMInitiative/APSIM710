module Registrations
   use DataTypes
   type IDsType
      sequence
      integer :: sysinit
      integer :: end_run
      integer :: process

   end type IDsType

   contains

      subroutine doRegistrations(id)
         use Infrastructure
         type(IDsType) :: id

         id%sysinit = add_registration(respondToEventReg, 'sysinit', nullTypeDDML, '')
         id%end_run = add_registration(respondToEventReg, 'end_run', nullTypeDDML, '')
         id%process = add_registration(respondToEventReg, 'process', nullTypeDDML, '')
      end subroutine
end module Registrations

