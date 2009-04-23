module Registrations
   use DataTypes
   type IDsType
      sequence
      integer :: sysinit
      integer :: prepare
      integer :: post

   end type IDsType

   contains

      subroutine doRegistrations(id)
         use Infrastructure
         type(IDsType) :: id

         id%sysinit = add_registration(respondToEventReg, 'sysinit', nullTypeDDML, '')
         id%prepare = add_registration(respondToEventReg, 'prepare', nullTypeDDML, '')
         id%post = add_registration(respondToEventReg, 'post', nullTypeDDML, '')
      end subroutine
end module Registrations

