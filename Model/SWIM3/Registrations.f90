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
      integer :: RunoffEvent
   end type IDsType

   contains

      subroutine doRegistrations(id)
         use Infrastructure
         type(IDsType) :: id

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
         id%newcrop = add_registration(respondToEventReg, 'newcrop', NewCropTypeDDML, '')         
         id%cropending = add_registration(respondToEventReg, 'cropending', NewCropTypeDDML, '')                  
         id%irrigated = add_registration(respondToEventReg, 'irrigated', nullTypeDDML, '')
         id%subsurfaceflow = add_registration(respondToEventReg, 'subsurfaceflow', nullTypeDDML, '')
         id%prepare = add_registration(respondToEventReg, 'prepare', nullTypeDDML, '')
         id%process = add_registration(respondToEventReg, 'process', nullTypeDDML, '')
         id%post = add_registration(respondToEventReg, 'post', nullTypeDDML, '')
         id%WaterUptakesCalculated = add_registration(eventReg, 'WaterUptakesCalculated', WaterUptakesCalculatedTypeDDML, '')
         id%RunoffEvent = add_registration(eventReg, 'RunoffEvent', RunoffEventTypeDDML, '')
      end subroutine
end module Registrations

