module Registrations
   use DataTypes
   type IDsType
      sequence
      integer :: irrigated
      integer :: sysinit
      integer :: irrigate
      integer :: apply
      integer :: apply2
      integer :: water_supplied
      integer :: tick
      integer :: new_solute
      integer :: process

   end type IDsType

   contains

      subroutine doRegistrations(id)
         use Infrastructure
         type(IDsType) :: id

         id%irrigated = add_registration(eventReg, 'irrigated', ApsimVariantTypeDDML, '')
         id%sysinit = add_registration(respondToEventReg, 'sysinit', nullTypeDDML, '')
         id%irrigate = add_registration(respondToEventReg, 'irrigate', ApsimVariantTypeDDML, '')
         id%apply = add_registration(respondToEventReg, 'apply', IrrigationApplicationTypeDDML, '')
         id%water_supplied = add_registration(respondToEventReg, 'water_supplied', nullTypeDDML, '')
         id%tick = add_registration(respondToEventReg, 'tick', nullTypeDDML, '')
         id%new_solute = add_registration(respondToEventReg, 'new_solute', NewSoluteTypeDDML, '')
         id%process = add_registration(respondToEventReg, 'process', nullTypeDDML, '')
      end subroutine
end module Registrations

