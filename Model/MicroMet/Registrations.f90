module Registrations
   use DataTypes
   type IDsType
      sequence
      integer :: canopywaterbalancecalculated
      integer :: canopy_energy_balance
      integer :: canopy_water_balance
      integer :: lai_table
      integer :: f_table
      integer :: rs_table
      integer :: rl_table
      integer :: gc_table
      integer :: ga_table
      integer :: pet_table
      integer :: petr_table
      integer :: peta_table
      integer :: omega_table
      integer :: tick
      integer :: newmet
      integer :: domicromet
      integer :: process
      integer :: prepare
      integer :: newcrop
      integer :: new_canopy
      integer :: newpotentialgrowth
      integer :: sys_init
      integer :: create

   end type IDsType

   contains

      subroutine doRegistrations(id)
         use Infrastructure
         type(IDsType) :: id
		 integer :: dummy

         id%canopywaterbalancecalculated = add_registration(eventReg, 'canopywaterbalancecalculated', CanopyWaterBalanceTypeDDML, '')
         id%canopy_energy_balance = add_registration(eventReg, 'canopy_energy_balance', CanopyEnergyBalanceTypeDDML, '')
         id%canopy_water_balance = add_registration(eventReg, 'canopy_water_balance', CanopyWaterBalanceTypeDDML, '')
         id%lai_table = add_registration(respondToEventReg, 'lai_table', nullTypeDDML, '')
         id%f_table = add_registration(respondToEventReg, 'f_table', nullTypeDDML, '')
         id%rs_table = add_registration(respondToEventReg, 'rs_table', nullTypeDDML, '')
         id%rl_table = add_registration(respondToEventReg, 'rl_table', nullTypeDDML, '')
         id%gc_table = add_registration(respondToEventReg, 'gc_table', nullTypeDDML, '')
         id%ga_table = add_registration(respondToEventReg, 'ga_table', nullTypeDDML, '')
         id%pet_table = add_registration(respondToEventReg, 'pet_table', nullTypeDDML, '')
         id%petr_table = add_registration(respondToEventReg, 'petr_table', nullTypeDDML, '')
         id%peta_table = add_registration(respondToEventReg, 'peta_table', nullTypeDDML, '')
         id%omega_table = add_registration(respondToEventReg, 'omega_table', nullTypeDDML, '')
         id%tick = add_registration(respondToEventReg, 'tick', TimeTypeDDML, '')
         id%newmet = add_registration(respondToEventReg, 'newmet', NewMetTypeDDML, '')
         id%domicromet = add_registration(respondToEventReg, 'domicromet', nullTypeDDML, '')
         id%process = add_registration(respondToEventReg, 'process', nullTypeDDML, '')
         id%prepare = add_registration(respondToEventReg, 'prepare', nullTypeDDML, '')
         id%newcrop = add_registration(respondToEventReg, 'newcrop', NewCropTypeDDML, '')
         id%new_canopy = add_registration(respondToEventReg, 'new_canopy', NewCanopyTypeDDML, '')
         id%newpotentialgrowth = add_registration(respondToEventReg, 'newpotentialgrowth', NewPotentialGrowthTypeDDML, '')
         id%sys_init = add_registration(respondToEventReg, 'sys_init', nullTypeDDML, '')
         id%create = add_registration(respondToEventReg, 'create', nullTypeDDML, '')
         dummy = add_registration_with_units(respondToGetReg, 'interception', floatTypeDDML, 'mm')
         dummy = add_registration_with_units(respondToGetReg, 'gc', floatTypeDDML, 'mm/s')
         dummy = add_registration_with_units(respondToGetReg, 'ga', floatTypeDDML, 'mm/s')
         dummy = add_registration_with_units(respondToGetReg, 'petr', floatTypeDDML, 'mm/day')
         dummy = add_registration_with_units(respondToGetReg, 'peta', floatTypeDDML, 'mm/day')
         dummy = add_registration_with_units(respondToGetReg, 'pet_total', floatTypeDDML, 'mm/day')
         dummy = add_registration_with_units(respondToGetReg, 'net_radn', floatTypeDDML, 'MJ/day')
         dummy = add_registration_with_units(respondToGetReg, 'net_rs', floatTypeDDML, 'MJ/day')
         dummy = add_registration_with_units(respondToGetReg, 'net_rl', floatTypeDDML, 'MJ/day')
         dummy = add_registration_with_units(respondToGetReg, 'soil_heat', floatTypeDDML, 'MJ/day')
         dummy = add_registration_with_units(respondToGetReg, 'dryleaffraction', floatTypeDDML, '0-1')
         dummy = add_registration_with_units(getVariableReg, 'latitude', floatTypeDDML, 'deg')
     end subroutine
end module Registrations

