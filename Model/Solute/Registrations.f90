module Registrations
   use DataTypes
   type IDsType
      sequence
      integer :: new_solute
      integer :: create
      integer :: sys_init
      integer :: process

   end type IDsType

   contains

      subroutine doRegistrations(id)
         use Infrastructure
         type(IDsType) :: id

         id%new_solute = add_registration(eventReg, 'new_solute', ApsimVariantTypeDDML, '')
         id%create = add_registration(respondToEventReg, 'create', nullTypeDDML, '')
         id%sys_init = add_registration(respondToEventReg, 'sys_init', nullTypeDDML, '')
         id%process = add_registration(respondToEventReg, 'process', nullTypeDDML, '')
      end subroutine
end module Registrations

