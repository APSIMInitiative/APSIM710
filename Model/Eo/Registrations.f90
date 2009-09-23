module Registrations
   use DataTypes
   type IDsType
      sequence
      integer :: sysinit
      integer :: prepare
      integer :: process
      integer :: dummy
   end type IDsType

   contains

      subroutine doRegistrations(id)
         use Infrastructure
         type(IDsType) :: id

         id%sysinit = add_registration(respondToEventReg, 'sysinit', nullTypeDDML, '')
         id%prepare = add_registration(respondToEventReg, 'prepare', nullTypeDDML, '')
         id%process = add_registration(respondToEventReg, 'process', nullTypeDDML, '')
         id%dummy = add_reg(respondToGetReg, 'eo_pm', floatTypeDDML, 'mm', '')
         id%dummy = add_reg(respondToGetReg, 'eo_penman', floatTypeDDML, 'mm', '')
         id%dummy = add_reg(respondToGetReg, 'eo_penman_d', floatTypeDDML, 'mm', '')
         id%dummy = add_reg(respondToGetReg, 'eo_transp', floatTypeDDML, 'mm', '')
         id%dummy = add_reg(respondToGetReg, 'eo_pm_x_cover', floatTypeDDML, 'mm', '')
         id%dummy = add_reg(respondToGetReg, 'eo_penman_x_cover', floatTypeDDML, 'mm', '')
         id%dummy = add_reg(respondToGetReg, 'eo_penman_d_x_cover', floatTypeDDML, 'mm', '')
         id%dummy = add_reg(respondToGetReg, 'eo_pm_x_kfunction', floatTypeDDML, 'mm', '')
         id%dummy = add_reg(respondToGetReg, 'eo_radn_x_kfunction', floatTypeDDML, 'mm', '')
         id%dummy = add_reg(respondToGetSetReg, 'eo_plant', floatTypeDDML, 'mm', '')
         id%dummy = add_reg(respondToGetReg, 'eo_soil', floatTypeDDML, 'mm', '')
         id%dummy = add_reg(respondToGetReg, 'eo_priestly_taylor', floatTypeDDML, 'mm', '')
         id%dummy = add_reg(respondToGetReg, 'eo_ritchie', floatTypeDDML, 'mm', '')
         id%dummy = add_reg(respondToGetReg, 'eo_vpd', floatTypeDDML, 'kpa', '')
         id%dummy = add_reg(respondToGetReg, 'canopy_height', floatTypeDDML, 'mm', '')
         id%dummy = add_reg(respondToGetReg, 'wind_ms_multiplier_height', floatTypeDDML, 'm/s', '')
         id%dummy = add_reg(respondToGetReg, 'wind_ms_reference', floatTypeDDML, 'm/s', '')
         id%dummy = add_reg(respondToGetReg, 'wind_adj', floatTypeDDML, 'km/day', '')
         id%dummy = add_reg(respondToGetReg, 'n_hrs', floatTypeDDML, 'hour', '')
         id%dummy = add_reg(respondToGetReg, 'radn_net', floatTypeDDML, 'W/m2', '')
         id%dummy = add_reg(respondToGetReg, 'da', floatTypeDDML, 'kg/kg', '')
         id%dummy = add_reg(respondToGetReg, 'ra', floatTypeDDML, 's/m', '')
         id%dummy = add_reg(respondToGetReg, 'rc', floatTypeDDML, 's/m', '')
         id%dummy = add_reg(respondToGetReg, 'epsilon', floatTypeDDML, 'kg/kg/K', '')
         id%dummy = add_reg(respondToGetReg, 'eo_daylength', floatTypeDDML, 'hrs', '')
      end subroutine
end module Registrations

