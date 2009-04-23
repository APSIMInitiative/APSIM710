module Registrations
   use DataTypes
   type IDsType
      sequence
      integer :: sysinit

   end type IDsType

   contains

      subroutine doRegistrations(id)
         use Infrastructure
         type(IDsType) :: id

         id%sysinit = add_registration(respondToEventReg, 'sysinit', nullTypeDDML, '')
      end subroutine
end module Registrations

