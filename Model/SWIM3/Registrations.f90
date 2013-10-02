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
      integer :: newcrop
      integer :: cropending
      integer :: irrigated
      integer :: subsurfaceflow
      integer :: prepare
      integer :: process
      integer :: post
      integer :: WaterUptakesCalculated
      integer :: NitrogenChanged
      integer :: RunoffEvent
      integer :: CohortWaterDemand
   end type IDsType

   contains

      subroutine doRegistrations(id)
         use Infrastructure
         type(IDsType) :: id
         integer dummy

         ! Events which we publish
         id%swim_timestep_preparation = add_registration(eventReg, 'swim_timestep_preparation', nullTypeDDML, '')
         id%pre_swim_timestep = add_registration(eventReg, 'pre_swim_timestep', nullTypeDDML, '')
         id%post_swim_timestep = add_registration(eventReg, 'post_swim_timestep', nullTypeDDML, '')
         id%new_profile = add_registration(eventReg, 'new_profile', NewProfileTypeDDML, '')
         id%WaterUptakesCalculated = add_registration(eventReg, 'WaterUptakesCalculated', WaterUptakesCalculatedTypeDDML, '')
         id%NitrogenChanged = add_registration(eventReg, 'NitrogenChanged', NitrogenChangedTypeDDML, '')
         id%RunoffEvent = add_registration(eventReg, 'RunoffEvent', RunoffEventTypeDDML, '')

         ! Events to which we subscribe
         id%create = add_registration(respondToEventReg, 'create', nullTypeDDML, '')
         id%sysinit = add_registration(respondToEventReg, 'sysinit', nullTypeDDML, '')
         id%reset = add_registration(respondToEventReg, 'reset', nullTypeDDML, '')
         id%sum_report = add_registration(respondToEventReg, 'sum_report', nullTypeDDML, '')
         id%tillage = add_registration(respondToEventReg, 'tillage', ApsimVariantTypeDDML, '')
         id%end_run = add_registration(respondToEventReg, 'end_run', nullTypeDDML, '')
         id%tick = add_registration(respondToEventReg, 'tick', TimeTypeDDML, '')
         id%prenewmet = add_registration(respondToEventReg, 'prenewmet', nullTypeDDML, '')
         id%new_solute = add_registration(respondToEventReg, 'new_solute', newSoluteTypeDDML, '')
         id%newcrop = add_registration(respondToEventReg, 'newcrop', NewCropTypeDDML, '')         
         id%cropending = add_registration(respondToEventReg, 'cropending', NewCropTypeDDML, '')                  
         id%irrigated = add_registration(respondToEventReg, 'irrigated', ApsimVariantTypeDDML, '')
         id%subsurfaceflow = add_registration(respondToEventReg, 'subsurfaceflow', ApsimVariantTypeDDML, '')
         id%prepare = add_registration(respondToEventReg, 'prepare', nullTypeDDML, '')
         id%process = add_registration(respondToEventReg, 'process', nullTypeDDML, '')
         id%post = add_registration(respondToEventReg, 'post', nullTypeDDML, '')
         id%CohortWaterDemand = add_registration(respondToEventReg, 'CohortWaterDemand', CohortWaterDemandTypeDDML, '')
   
    ! variables that are both gettable and settable
         dummy = add_reg(respondToGetSetReg, 'sw', doubleArrayTypeDDML, 'cm^3/cm^3', 'Soil water content')   
         dummy = add_reg(respondToGetSetReg, 'psi', doublearrayTypeDDML, 'cm', '')
         dummy = add_reg(respondToGetSetReg, 'scon', doubleTypeDDML, '/h', '')
		 
    ! variables we own and make available to other modules (gettable)
         dummy = add_reg(respondToGetReg, 'dlayer', doubleArrayTypeDDML, 'mm', 'Thickness of profile layer')   
         dummy = add_reg(respondToGetReg, 'bd', doubleArrayTypeDDML, 'g/cm^3', 'Bulk density')   
         dummy = add_reg(respondToGetReg, 'swf', floatArrayTypeDDML, '', 'Unused!!')   
         dummy = add_reg(respondToGetReg, 'sw_dep', doubleArrayTypeDDML, 'mm', 'Soil water content')   
         dummy = add_reg(respondToGetReg, 'll15', doubleArrayTypeDDML, 'cm^3/cm^3', '15 bar Lower Limit')   
         dummy = add_reg(respondToGetReg, 'll15_dep', doubleArrayTypeDDML, 'mm', '15 bar Lower Limit')   
         dummy = add_reg(respondToGetReg, 'dul', doubleArrayTypeDDML, 'cm^3/cm^3', 'Drained Upper Limit')   
         dummy = add_reg(respondToGetReg, 'dul_dep', doubleArrayTypeDDML, 'mm', 'Drained Upper Limit')   
         dummy = add_reg(respondToGetReg, 'sat', doubleArrayTypeDDML, 'cm^3/cm^3', 'Saturated Water Content')   
         dummy = add_reg(respondToGetReg, 'sat_dep', doubleArrayTypeDDML, 'mm', 'Saturated Water Content')   
         dummy = add_reg(respondToGetReg, 'esw', doubleTypeDDML, 'mm', '')
         dummy = add_reg(respondToGetReg, 'wp', doubleTypeDDML, 'mm', '')
         dummy = add_reg(respondToGetReg, 'p', doublearrayTypeDDML, '', '')
         dummy = add_reg(respondToGetReg, 'psio', doublearrayTypeDDML, 'cm', '')
         dummy = add_reg(respondToGetReg, 'runoff', doubleTypeDDML, 'mm', '')
         dummy = add_reg(respondToGetReg, 'cn_runoff', doubleTypeDDML, 'mm', '')
         dummy = add_reg(respondToGetReg, 'cover_surface_runoff', doubleTypeDDML, '0-1', '')
         dummy = add_reg(respondToGetReg, 'infiltration', doubleTypeDDML, 'mm', '')
         dummy = add_reg(respondToGetReg, 'es', doubleTypeDDML, 'mm', '')
         dummy = add_reg(respondToGetReg, 'eos', doubleTypeDDML, 'mm', '')
         dummy = add_reg(respondToGetReg, 'drain', doubleTypeDDML, 'mm', '')
         ! dummy = add_reg(respondToGetReg, 'eo', doubleTypeDDML, 'mm', '')
         dummy = add_reg(respondToGetReg, 'psix', doublearrayTypeDDML, 'cm', '')
         dummy = add_reg(respondToGetReg, 'flow', doublearrayTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'salb', doubleTypeDDML, '0-1', '')
         dummy = add_reg(respondToGetReg, 'hmin', doubleTypeDDML, 'mm', '')
         dummy = add_reg(respondToGetReg, 'h', doubleTypeDDML, 'mm', '')
         dummy = add_reg(respondToGetReg, 'pond', doubleTypeDDML, 'mm', '')
         dummy = add_reg(respondToGetReg, 'scon_min', doubleTypeDDML, '/h', '')
         dummy = add_reg(respondToGetReg, 'scon_max', doubleTypeDDML, '/h', '')
         dummy = add_reg(respondToGetReg, 'dr', doubleTypeDDML, 'mm', '')
         dummy = add_reg(respondToGetReg, 'dt', doubleTypeDDML, 'min', '')
         dummy = add_reg(respondToGetReg, 'subsurface_drain', doubleTypeDDML, 'mm', '')
         dummy = add_reg(respondToGetReg, 'water_table', doubleTypeDDML, 'mm', '')
         dummy = add_reg(respondToGetReg, 'work', doubleTypeDDML, '', '')
         dummy = add_reg(respondToGetReg, 'swim3', doubleTypeDDML, '-', '')

    ! variables that are only settable
         dummy = add_reg(respondToSetReg, 'bbc_potential', doubleTypeDDML, 'cm', 'Constant potential for bottom boundary condition')
         dummy = add_reg(respondToSetReg, 'bbc_gradient', doubleTypeDDML, 'cm', 'Gradient for bottom boundary condition')
		 
      end subroutine
end module Registrations

