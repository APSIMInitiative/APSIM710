module Registrations
   use DataTypes
   type IDsType
      sequence
      integer :: create
      integer :: sysinit
      integer :: prepare
      integer :: process
      integer :: dummy

   end type IDsType

   contains

      subroutine doRegistrations(id)
         use Infrastructure
         type(IDsType) :: id

         id%create = add_registration(respondToEventReg, 'create', nullTypeDDML, '')
         id%sysinit = add_registration(respondToEventReg, 'sysinit', nullTypeDDML, '')
         id%prepare = add_registration(respondToEventReg, 'prepare', nullTypeDDML, '')
         id%process = add_registration(respondToEventReg, 'process', nullTypeDDML, '')

    ! variables we own and make available to other modules (gettable)
         dummy = add_reg(respondToGetReg, 'final_soil_temp', doubleArrayTypeDDML, 'oC', 'soil temperature, all layers')   
         dummy = add_reg(respondToGetReg, 'final_soil_temp_surface', doubleTypeDDML, 'oC', 'soil surface temperature')   
         dummy = add_reg(respondToGetReg, 'ave_soil_temp', doubleArrayTypeDDML, 'oC', 'average soil temperature, all layers')   
         dummy = add_reg(respondToGetReg, 'ave_soil_temp_surface', doubleTypeDDML, 'oC', 'average soil surface temperature')   
         dummy = add_reg(respondToGetReg, 'mint_soil', doubleArrayTypeDDML, 'oC', 'minimum soil temperature, all layers')   
         dummy = add_reg(respondToGetReg, 'mint_soil_surface', doubleTypeDDML, 'oC', 'minimum soil surface temperature')   
         dummy = add_reg(respondToGetReg, 'maxt_soil', doubleArrayTypeDDML, 'oC', 'maximum soil temperature, all layers')   
         dummy = add_reg(respondToGetReg, 'maxt_soil_surface', doubleTypeDDML, 'oC', 'maximum soil surface temperature')   
         dummy = add_reg(respondToGetReg, 'therm_cond', doubleArrayTypeDDML, 'J/sec/m/K', 'thermal conductivity, all nodes')   
         dummy = add_reg(respondToGetReg, 'heat_store', doubleArrayTypeDDML, 'J/m^3/K/s', 'heat storage, all nodes')   
      end subroutine
end module Registrations
