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
         
      ! variables that are gettable and settable
      dummy = add_reg(respondToGetSetReg, 'sw', floatarrayTypeDDML, 'mm/mm', 'Soil water content')
      dummy = add_reg(respondToGetSetReg, 'sw_dep', floatarrayTypeDDML, 'mm', 'Soil water content')
      dummy = add_reg(respondToGetSetReg, 'dul_dep', floatarrayTypeDDML, 'mm', 'Drained Upper Limit')
      dummy = add_reg(respondToGetSetReg, 'dul', floatarrayTypeDDML, 'mm/mm', 'Drained Upper Limit')
      dummy = add_reg(respondToGetSetReg, 'll15_dep', floatarrayTypeDDML, 'mm', '15 bar Lower Limit')
      dummy = add_reg(respondToGetSetReg, 'll15', floatarrayTypeDDML, 'mm/mm', '15 bar Lower Limit')
      dummy = add_reg(respondToGetSetReg, 'sat_dep', floatarrayTypeDDML, 'mm', 'Saturated water content')
      dummy = add_reg(respondToGetSetReg, 'sat', floatarrayTypeDDML, 'mm/mm', 'Saturated water content')
      dummy = add_reg(respondToGetSetReg, 'air_dry_dep', floatarrayTypeDDML, 'mm', 'Air Dry water content')
      dummy = add_reg(respondToGetSetReg, 'air_dry', floatarrayTypeDDML, 'mm/mm', 'Air Dry water content')
      dummy = add_reg(respondToGetSetReg, 'dlayer', floatarrayTypeDDML, 'mm', 'Thickness of profile layer')
      dummy = add_reg(respondToGetReg, 'bd', floatarrayTypeDDML, 'g/cm^3', 'Bulk density')
      dummy = add_reg(respondToGetReg, 'swf', floatarrayTypeDDML, '', '')
      dummy = add_reg(respondToGetReg, 'wp', doubleTypeDDML, '', '')
      dummy = add_reg(respondToGetReg, 'p', doublearrayTypeDDML, '', '')
      dummy = add_reg(respondToGetReg, 'psi', doublearrayTypeDDML, '', '')
      dummy = add_reg(respondToGetReg, 'runoff', doubleTypeDDML, 'mm', '')
      dummy = add_reg(respondToGetReg, 'infiltration', doubleTypeDDML, 'mm', '')
      dummy = add_reg(respondToGetReg, 'es', doubleTypeDDML, 'mm', '')
      dummy = add_reg(respondToGetReg, 'eos', doubleTypeDDML, 'mm', '')
      dummy = add_reg(respondToGetReg, 'drain', doubleTypeDDML, 'mm', '')
      dummy = add_reg(respondToGetReg, 'eo', doubleTypeDDML, 'mm', '')
      dummy = add_reg(respondToGetReg, 'psix', doublearrayTypeDDML, 'cm', '')
      dummy = add_reg(respondToGetReg, 'flow', doublearrayTypeDDML, 'kg/ha', '')
      dummy = add_reg(respondToGetReg, 'salb', doubleTypeDDML, '', '')
      dummy = add_reg(respondToGetReg, 'hmin', doubleTypeDDML, 'mm', '')
      dummy = add_reg(respondToGetReg, 'h', doubleTypeDDML, 'mm', '')
      dummy = add_reg(respondToGetReg, 'pond', doubleTypeDDML, 'mm', '')
      dummy = add_reg(respondToGetReg, 'scon', doubleTypeDDML, '/h', '')
      dummy = add_reg(respondToGetReg, 'scon_min', doubleTypeDDML, '/h', '')
      dummy = add_reg(respondToGetReg, 'scon_max', doubleTypeDDML, '/h', '')
      dummy = add_reg(respondToGetReg, 'dr', doubleTypeDDML, 'mm', '')
      dummy = add_reg(respondToGetReg, 'dt', doubleTypeDDML, 'min', '')
      dummy = add_reg(respondToGetReg, 'subsurface_drain', doubleTypeDDML, 'mm', '')
      dummy = add_reg(respondToGetReg, 'water_table', doubleTypeDDML, 'mm', '')

      end subroutine
end module Registrations

