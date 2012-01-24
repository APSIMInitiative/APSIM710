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
      integer :: dummy

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

         dummy = add_reg(respondToGetReg, 'irrigation', floatTypeDDML, 'mm', 'Amount of irrigation applied on a given day')
         dummy = add_reg(respondToGetReg, 'irrig_tot', floatTypeDDML, 'mm', 'Total amount of irrigation applied')
         dummy = add_reg(respondToGetReg, 'irrig_loss', floatTypeDDML, 'mm', 'Amount of irrigation water lost')
         dummy = add_reg(respondToGetReg, 'automatic_irrigation', stringTypeDDML, 'mm', 'Turn automatic irrigation on? yes or no')
         dummy = add_reg(respondToGetReg, 'crit_fr_asw', floatTypeDDML, '', '')
         dummy = add_reg(respondToGetReg, 'asw_depth', floatTypeDDML, 'mm', '')
         dummy = add_reg(respondToGetReg, 'irr_fasw', floatTypeDDML, '0-1', '')
         dummy = add_reg(respondToGetReg, 'irr_deficit', floatTypeDDML, 'mm', '')
         dummy = add_reg(respondToGetReg, 'allocation', floatTypeDDML, 'mm', '')
         dummy = add_reg(respondToGetReg, 'carry_over', floatTypeDDML, 'mm', '')
         dummy = add_reg(respondToGetReg, 'allocation_ml', floatTypeDDML, 'ML', '')
         dummy = add_reg(respondToGetReg, 'carry_over_ml', floatTypeDDML, 'ML', '')
         end subroutine
end module Registrations

