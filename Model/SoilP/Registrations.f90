module Registrations
   use DataTypes
   type IDsType
      sequence
      integer :: externalmassflow
      integer :: sysinit
      integer :: reset
      integer :: sum_report
      integer :: tillage
      integer :: incorp_fom
      integer :: IncorpFOMPool
      integer :: decomposed
      integer :: actualresiduedecompositioncalculated
      integer :: tick
      integer :: process
      integer :: add_urine

   end type IDsType

   contains

      subroutine doRegistrations(id)
         use Infrastructure
         type(IDsType) :: id
         integer dummy

         id%externalmassflow = add_registration(eventReg, 'externalmassflow', ExternalMassFlowTypeDDML, '')
         id%sysinit = add_registration(respondToEventReg, 'sysinit', nullTypeDDML, '')
         id%reset = add_registration(respondToEventReg, 'reset', nullTypeDDML, '')
         id%sum_report = add_registration(respondToEventReg, 'sum_report', nullTypeDDML, '')
         id%tillage = add_registration(respondToEventReg, 'tillage', nullTypeDDML, '')
         id%incorp_fom = add_registration(respondToEventReg, 'IncorpFOM', FOMLayerTypeDDML, '')
         id%IncorpFOMPool = add_registration(respondToEventReg, 'IncorpFOMPool', FOMPoolTypeDDML, '')
         id%decomposed = add_registration(respondToEventReg, 'decomposed', nullTypeDDML, '')
         id%actualresiduedecompositioncalculated = add_registration(respondToEventReg, 'actualresiduedecompositioncalculated', SurfaceOrganicMatterDecompTypeDDML, '')
         id%tick = add_registration(respondToEventReg, 'tick', nullTypeDDML, '')
         id%process = add_registration(respondToEventReg, 'process', nullTypeDDML, '')
         id%add_urine = add_registration(respondToEventReg, 'AddUrine', AddUrineTypeDDML, '')

         dummy = add_reg(respondToGetSetReg, 'labile_p', floatarrayTypeDDML, 'kg/ha', 'Labile P')
         dummy = add_reg(respondToGetReg, 'unavail_p', floatarrayTypeDDML, 'kg/ha', 'Unavailable P')
         dummy = add_reg(respondToGetSetReg, 'banded_p', floatarrayTypeDDML, 'kg/ha', 'Banded P')
         dummy = add_reg(respondToGetSetReg, 'rock_p', floatarrayTypeDDML, 'kg/ha', 'Rock P')
         dummy = add_reg(respondToGetReg, 'soil_test_p', floatarrayTypeDDML, 'kg/ha', 'Labile + Banded P')
         dummy = add_reg(respondToGetReg, 'biom_p', floatarrayTypeDDML, 'kg/ha', 'P in biomass pool')
         dummy = add_reg(respondToGetReg, 'hum_p', floatarrayTypeDDML, 'kg/ha', 'P in humic pool')
         dummy = add_reg(respondToGetReg, 'fom_p', floatarrayTypeDDML, 'kg/ha', 'P in FOM pool')
         dummy = add_reg(respondToGetReg, 'fom_p_pool1', floatarrayTypeDDML, 'kg/ha', 'P in FOM pool 1')
         dummy = add_reg(respondToGetReg, 'fom_p_pool2', floatarrayTypeDDML, 'kg/ha', 'P in FOM pool 2')
         dummy = add_reg(respondToGetReg, 'fom_p_pool3', floatarrayTypeDDML, 'kg/ha', 'P in FOM pool 3')
         dummy = add_reg(respondToGetReg, 'fom_cp', floatarrayTypeDDML, 'kg/ha', 'C:P ratio in FOM pool')
         dummy = add_reg(respondToGetReg, 'uptake_p_{crop}', floatarrayTypeDDML, 'kg/ha', 'P uptake from {crop}')
         dummy = add_reg(respondToGetReg, 'demand_p_{crop}', floatTypeDDML, 'kg/ha', 'P demand from {crop}')

      end subroutine
end module Registrations

