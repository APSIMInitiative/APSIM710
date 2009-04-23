module Registrations
   use DataTypes
   type IDsType
      sequence
      integer :: incorp_fom
      integer :: crop_chopped
      integer :: sowing
      integer :: harvesting
      integer :: externalmassflow
      integer :: create
      integer :: sysinit
      integer :: sow
      integer :: harvest
      integer :: end_crop
      integer :: initiate_crop
      integer :: kill_crop
      integer :: stop_growth
      integer :: tick
      integer :: newmet
      integer :: prepare
      integer :: process

   end type IDsType

   contains

      subroutine doRegistrations(id)
         use Infrastructure
         type(IDsType) :: id

         id%incorp_fom = add_registration(eventReg, 'incorpfom', FomLayerTypeDDML, '')
         id%crop_chopped = add_registration(eventReg, 'crop_chopped', ApsimVariantTypeDDML, '')
         id%sowing = add_registration(eventReg, 'sowing', nullTypeDDML, '')
         id%harvesting = add_registration(eventReg, 'harvesting', nullTypeDDML, '')
         id%externalmassflow = add_registration(eventReg, 'externalmassflow', ExternalMassFlowTypeDDML, '')
         id%create = add_registration(respondToEventReg, 'create', nullTypeDDML, '')
         id%sysinit = add_registration(respondToEventReg, 'sysinit', nullTypeDDML, '')
         id%sow = add_registration(respondToEventReg, 'sow', nullTypeDDML, '')
         id%harvest = add_registration(respondToEventReg, 'harvest', nullTypeDDML, '')
         id%end_crop = add_registration(respondToEventReg, 'end_crop', nullTypeDDML, '')
         id%initiate_crop = add_registration(respondToEventReg, 'initiate_crop', nullTypeDDML, '')
         id%kill_crop = add_registration(respondToEventReg, 'kill_crop', nullTypeDDML, '')
         id%stop_growth = add_registration(respondToEventReg, 'stop_growth', nullTypeDDML, '')
         id%tick = add_registration(respondToEventReg, 'tick', nullTypeDDML, '')
         id%newmet = add_registration(respondToEventReg, 'newmet', NewMetTypeDDML, '')
         id%prepare = add_registration(respondToEventReg, 'prepare', nullTypeDDML, '')
         id%process = add_registration(respondToEventReg, 'process', nullTypeDDML, '')
      end subroutine
end module Registrations

