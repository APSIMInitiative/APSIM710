module Registrations
   use DataTypes
   type IDsType
      sequence
      integer :: new_solute
      integer :: create
      integer :: sys_init
      integer :: process
      integer :: new_profile

   end type IDsType

   contains

      subroutine doRegistrations(id)
         use Infrastructure
         type(IDsType) :: id

         id%new_solute = add_registration(eventReg, 'new_solute', NewSoluteTypeDDML, '')
         id%create = add_registration(respondToEventReg, 'create', nullTypeDDML, '')
         id%sys_init = add_registration(respondToEventReg, 'sys_init', nullTypeDDML, '')
         id%process = add_registration(respondToEventReg, 'process', nullTypeDDML, '')
         id%new_profile = add_registration(respondToEventReg, 'new_profile', newprofileTypeDDML, '')
      end subroutine
end module Registrations

