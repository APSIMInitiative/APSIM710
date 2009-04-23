module Registrations
   use DataTypes
   type IDsType
      sequence
      integer :: crop_chopped
      integer :: incorp_fom
      integer :: sowing
      integer :: harvesting
      integer :: create
      integer :: sysinit
      integer :: sow
      integer :: end_crop
      integer :: kill_crop
      integer :: end_run
      integer :: tick
      integer :: newmet
      integer :: new_profile
      integer :: prepare
      integer :: process

   end type IDsType

   contains

      subroutine doRegistrations(id)
         use Infrastructure
         type(IDsType) :: id

         id%crop_chopped = add_registration(eventReg, 'crop_chopped', ApsimVariantTypeDDML, '')
         id%incorp_fom = add_registration(eventReg, 'incorpfom', FomLayerTypeDDML, '')
         id%sowing = add_registration(eventReg, 'sowing', nullTypeDDML, '')
         id%harvesting = add_registration(eventReg, 'harvesting', nullTypeDDML, '')
         id%create = add_registration(respondToEventReg, 'create', nullTypeDDML, '')
         id%sysinit = add_registration(respondToEventReg, 'sysinit', nullTypeDDML, '')
         id%sow = add_registration(respondToEventReg, 'sow', nullTypeDDML, '')
         id%end_crop = add_registration(respondToEventReg, 'end_crop', nullTypeDDML, '')
         id%kill_crop = add_registration(respondToEventReg, 'kill_crop', nullTypeDDML, '')
         id%end_run = add_registration(respondToEventReg, 'end_run', nullTypeDDML, '')
         id%tick = add_registration(respondToEventReg, 'tick', nullTypeDDML, '')
         id%newmet = add_registration(respondToEventReg, 'newmet', NewMetTypeDDML, '')
         id%new_profile = add_registration(respondToEventReg, 'new_profile', NewProfileTypeDDML, '')
         id%prepare = add_registration(respondToEventReg, 'prepare', nullTypeDDML, '')
         id%process = add_registration(respondToEventReg, 'process', nullTypeDDML, '')
      end subroutine
end module Registrations

