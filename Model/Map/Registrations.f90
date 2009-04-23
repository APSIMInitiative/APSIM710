module Registrations
   use DataTypes
   type IDsType
      sequence
      integer :: sysinit
      integer :: create

   end type IDsType

   contains

      subroutine doRegistrations(id)
         use Infrastructure
         type(IDsType) :: id

         id%sysinit = add_registration(respondToEventReg, 'sysinit', nullTypeDDML, '')
         id%create = add_registration(respondToEventReg, 'create', nullTypeDDML, '')
      end subroutine
end module Registrations

