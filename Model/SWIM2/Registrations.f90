module Registrations
   use DataTypes
   type IDsType
      sequence
      integer :: swim_timestep_preparation
      integer :: pre_swim_timestep
      integer :: post_swim_timestep
      integer :: new_profile
      integer :: create
      integer :: sysinit
      integer :: reset
      integer :: sum_report
      integer :: tillage
      integer :: end_run
      integer :: tick
      integer :: prenewmet
      integer :: new_solute
      integer :: irrigated
      integer :: subsurfaceflow
      integer :: prepare
      integer :: process
      integer :: post
      integer :: WaterUptakesCalculated
      integer :: NitrogenChanged
      integer :: RunoffEvent
      integer :: RefreshSoilType

   end type IDsType

   contains

      subroutine doRegistrations(id)
         use Infrastructure
         type(IDsType) :: id
         integer :: dummy
         
         id%swim_timestep_preparation = add_registration(eventReg, 'swim_timestep_preparation', nullTypeDDML, '')
         id%pre_swim_timestep = add_registration(eventReg, 'pre_swim_timestep', nullTypeDDML, '')
         id%post_swim_timestep = add_registration(eventReg, 'post_swim_timestep', nullTypeDDML, '')
         id%new_profile = add_registration(eventReg, 'new_profile', NewProfileTypeDDML, '')
         id%create = add_registration(respondToEventReg, 'create', nullTypeDDML, '')
         id%sysinit = add_registration(respondToEventReg, 'sysinit', nullTypeDDML, '')
         id%reset = add_registration(respondToEventReg, 'reset', nullTypeDDML, '')
         id%sum_report = add_registration(respondToEventReg, 'sum_report', nullTypeDDML, '')
         id%tillage = add_registration(respondToEventReg, 'tillage', nullTypeDDML, '')
         id%end_run = add_registration(respondToEventReg, 'end_run', nullTypeDDML, '')
         id%tick = add_registration(respondToEventReg, 'tick', TimeTypeDDML, '')
         id%prenewmet = add_registration(respondToEventReg, 'prenewmet', nullTypeDDML, '')
         id%new_solute = add_registration(respondToEventReg, 'new_solute', newSoluteTypeDDML, '')
         id%irrigated = add_registration(respondToEventReg, 'irrigated', nullTypeDDML, '')
         id%subsurfaceflow = add_registration(respondToEventReg, 'subsurfaceflow', nullTypeDDML, '')
         id%prepare = add_registration(respondToEventReg, 'prepare', nullTypeDDML, '')
         id%process = add_registration(respondToEventReg, 'process', nullTypeDDML, '')
         id%post = add_registration(respondToEventReg, 'post', nullTypeDDML, '')
         id%WaterUptakesCalculated = add_registration(eventReg, 'WaterUptakesCalculated', nullTypeDDML, '')
         id%NitrogenChanged = add_registration(eventReg, 'NitrogenChanged', NitrogenChangedTypeDDML, '')
         id%RunoffEvent = add_registration(eventReg, 'RunoffEvent', RunoffEventTypeDDML, '')
         id%RefreshSoilType = add_registration(respondToEventReg, 'RefreshSoilType', nullTypeDDML, '')
         
      ! variables that are gettable and settable
      dummy = add_reg(respondToGetSetReg, 'sw', floatarrayTypeDDML, 'mm/mm', 'Soil water content')
      dummy = add_reg(respondToGetReg, 'sw_dep', floatarrayTypeDDML, 'mm', 'Soil water content')
      dummy = add_reg(respondToGetReg, 'dul_dep', floatarrayTypeDDML, 'mm', 'Drained Upper Limit')
      dummy = add_reg(respondToGetReg, 'dul', floatarrayTypeDDML, 'mm/mm', 'Drained Upper Limit')
      dummy = add_reg(respondToGetReg, 'll15_dep', floatarrayTypeDDML, 'mm', '15 bar Lower Limit')
      dummy = add_reg(respondToGetReg, 'll15', floatarrayTypeDDML, 'mm/mm', '15 bar Lower Limit')
      dummy = add_reg(respondToGetReg, 'sat_dep', floatarrayTypeDDML, 'mm', 'Saturated water content')
      dummy = add_reg(respondToGetReg, 'sat', floatarrayTypeDDML, 'mm/mm', 'Saturated water content')
      dummy = add_reg(respondToGetReg, 'dlayer', floatarrayTypeDDML, 'mm', 'Thickness of profile layer')
      dummy = add_reg(respondToGetReg, 'bd', floatarrayTypeDDML, 'g/cm^3', 'Bulk density')
      dummy = add_reg(respondToGetReg, 'swf', floatarrayTypeDDML, '', 'Space weighting factor, water')
      dummy = add_reg(respondToGetReg, 'wp', doubleTypeDDML, 'mm', 'Water amount')
      dummy = add_reg(respondToGetReg, 'p', doublearrayTypeDDML, '', 'Modified psi function')
      dummy = add_reg(respondToGetReg, 'psi', doublearrayTypeDDML, 'cm', 'Soil water potential')
      dummy = add_reg(respondToGetReg, 'hyd_cond', doublearrayTypeDDML, 'mm/h', 'Soil hydraulic conductivity')
      dummy = add_reg(respondToGetReg, 'runoff', doubleTypeDDML, 'mm', 'Water runoff amount')
      dummy = add_reg(respondToGetReg, 'infiltration', doubleTypeDDML, 'mm', 'Water infiltration amount')
      dummy = add_reg(respondToGetReg, 'es', doubleTypeDDML, 'mm', 'Water evaporation amount')
      dummy = add_reg(respondToGetReg, 'eos', doubleTypeDDML, 'mm', 'Potential daily evaporation')
      dummy = add_reg(respondToGetReg, 'bypass', doubleTypeDDML, 'mm', 'Bypass flow amount')
      dummy = add_reg(respondToGetReg, 'drain', doubleTypeDDML, 'mm', 'Drainage amount at bottom of soil profile')
      dummy = add_reg(respondToGetReg, 'eo', doubleTypeDDML, 'mm', 'Potential daily evapotranspiration')
      dummy = add_reg(respondToGetReg, 'psix', doublearrayTypeDDML, 'cm', 'Xylem water potential')
      dummy = add_reg(respondToGetSetReg, 'flow', doublearrayTypeDDML, 'kg/ha', 'Flow of water (???) from each layer')
      dummy = add_reg(respondToGetReg, 'flow_x', doublearrayTypeDDML, 'mm or kg/ha', 'Flow of x (water, no3, urea, etc) from each layer')
      dummy = add_reg(respondToGetReg, 'salb', doubleTypeDDML, '', 'Soil surface albedo')
      dummy = add_reg(respondToGetReg, 'hmin', doubleTypeDDML, 'mm', 'Minimum water storage at surface')
      dummy = add_reg(respondToGetReg, 'h', doubleTypeDDML, 'mm', 'Water storage at soil surface')
      dummy = add_reg(respondToGetReg, 'pond', doubleTypeDDML, 'mm', 'Water amount ponding')
      dummy = add_reg(respondToGetReg, 'scon', doubleTypeDDML, '/h', 'Surface water conductance')
      dummy = add_reg(respondToGetReg, 'scon_min', doubleTypeDDML, '/h', 'Minimum surface conductance')
      dummy = add_reg(respondToGetReg, 'scon_max', doubleTypeDDML, '/h', 'Maximum surface conductance')
      dummy = add_reg(respondToGetReg, 'dr', doubleTypeDDML, 'mm', 'Rain amount for each timestep')
      dummy = add_reg(respondToGetReg, 'dt', doubleTypeDDML, 'min', 'SWIM timestep')
      dummy = add_reg(respondToGetReg, 'subsurface_drain', doubleTypeDDML, 'mm', 'Sub-surface water drainage')
      dummy = add_reg(respondToGetReg, 'water_table', doubleTypeDDML, 'mm', 'Water table depth')
      dummy = add_reg(respondToGetReg, 'exco_x', doublearrayTypeDDML, 'g/cm^3', 'Freundlich exchange factor for solute x (no3, urea, etc)')
      dummy = add_reg(respondToGetReg, 'fip_x', doublearrayTypeDDML, '', 'Freundlich exponent factor for solute x (no3, urea, etc) ')
      dummy = add_reg(respondToGetReg, 'dis_x', doublearrayTypeDDML, 'cm', 'Dispersion coefficient for solute x (no3, urea, etc) ')
      dummy = add_reg(respondToGetReg, 'conc_water_x', doublearrayTypeDDML, 'ug/g', 'Concentration of x (no3, urea, etc) in the soil solution')
      dummy = add_reg(respondToGetReg, 'conc_adsorb_x', doublearrayTypeDDML, 'ug/g', 'Concentration of x (no3, urea, etc) adsorbed')
      dummy = add_reg(respondToGetReg, 'leach_x', doubleTypeDDML, 'kg/ha', 'Amount of x (no3, urea, etc) leached at bottom of profile')

      end subroutine
end module Registrations

