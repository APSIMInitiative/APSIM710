module Registrations
   use DataTypes
   type IDsType
      sequence
      integer :: post

   end type IDsType

   contains

      subroutine doRegistrations(id)
         use Infrastructure
         type(IDsType) :: id

         id%post = add_registration(respondToEventReg, 'post', nullTypeDDML, '')
      end subroutine
end module Registrations

