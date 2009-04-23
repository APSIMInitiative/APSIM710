module Registrations
   use DataTypes
   type IDsType
      sequence
      integer :: add_surfaceom
      integer :: kill_parasite
      integer :: create
      integer :: sysinit
      integer :: sow
      integer :: harvest
      integer :: end_run
      integer :: start
      integer :: end
      integer :: finish
      integer :: herbicide
      integer :: weeding
      integer :: kill
      integer :: prepare
      integer :: process
      integer :: post
      integer :: tick
      integer :: newmet
      integer :: new_profile

   end type IDsType

   contains

      subroutine doRegistrations(id)
         use Infrastructure
         type(IDsType) :: id

         id%add_surfaceom = add_registration(eventReg, 'add_surfaceom', nullTypeDDML, '')
         id%kill_parasite = add_registration(respondToEventReg, 'kill_parasite', nullTypeDDML, '')
         id%create = add_registration(respondToEventReg, 'create', nullTypeDDML, '')
         id%sysinit = add_registration(respondToEventReg, 'sysinit', nullTypeDDML, '')
         id%sow = add_registration(respondToEventReg, 'sow', nullTypeDDML, '')
         id%harvest = add_registration(respondToEventReg, 'harvest', nullTypeDDML, '')
         id%end_run = add_registration(respondToEventReg, 'end_run', nullTypeDDML, '')
         id%start = add_registration(respondToEventReg, 'start', nullTypeDDML, '')
         id%end = add_registration(respondToEventReg, 'end', nullTypeDDML, '')
         id%finish = add_registration(respondToEventReg, 'finish', nullTypeDDML, '')
         id%herbicide = add_registration(respondToEventReg, 'herbicide', nullTypeDDML, '')
         id%weeding = add_registration(respondToEventReg, 'weeding', nullTypeDDML, '')
         id%kill = add_registration(respondToEventReg, 'kill', nullTypeDDML, '')
         id%prepare = add_registration(respondToEventReg, 'prepare', nullTypeDDML, '')
         id%process = add_registration(respondToEventReg, 'process', nullTypeDDML, '')
         id%post = add_registration(respondToEventReg, 'post', nullTypeDDML, '')
         id%tick = add_registration(respondToEventReg, 'tick', TimeTypeDDML, '')
         id%newmet = add_registration(respondToEventReg, 'newmet', NewMetTypeDDML, '')
         id%new_profile = add_registration(respondToEventReg, 'new_profile', NewProfileTypeDDML, '')
      end subroutine
end module Registrations

