module Registrations
   use DataTypes
   type IDsType
      sequence
      integer :: crop_chopped
      integer :: incorp_fom
      integer :: killing
      integer :: sowing
      integer :: harvesting
      integer :: sysinit
      integer :: create
      integer :: sow
      integer :: harvest
      integer :: end_crop
      integer :: kill_crop
      integer :: graze
      integer :: hill_up
      integer :: lodge
      integer :: tick
      integer :: prepare
      integer :: process

   end type IDsType

   contains

      subroutine doRegistrations(id)
         use Infrastructure
         type(IDsType) :: id

         id%crop_chopped = add_registration(eventReg, 'crop_chopped', ApsimVariantTypeDDML, '')
         id%incorp_fom = add_registration(eventReg, 'IncorpFOM', IncorpFomTypeDDML, '')
         id%killing = add_registration(eventReg, 'killing', nullTypeDDML, '')
         id%sowing = add_registration(eventReg, 'sowing', nullTypeDDML, '')
         id%harvesting = add_registration(eventReg, 'harvesting', nullTypeDDML, '')
         id%sysinit = add_registration(respondToEventReg, 'sysinit', nullTypeDDML, '')
         id%create = add_registration(respondToEventReg, 'create', nullTypeDDML, '')
         id%sow = add_registration(respondToEventReg, 'sow', nullTypeDDML, '')
         id%harvest = add_registration(respondToEventReg, 'harvest', nullTypeDDML, '')
         id%end_crop = add_registration(respondToEventReg, 'end_crop', nullTypeDDML, '')
         id%kill_crop = add_registration(respondToEventReg, 'kill_crop', nullTypeDDML, '')
         id%graze = add_registration(respondToEventReg, 'graze', nullTypeDDML, '')
         id%hill_up = add_registration(respondToEventReg, 'hill_up', nullTypeDDML, '')
         id%lodge = add_registration(respondToEventReg, 'lodge', nullTypeDDML, '')
         id%tick = add_registration(respondToEventReg, 'tick', nullTypeDDML, '')
         id%prepare = add_registration(respondToEventReg, 'prepare', nullTypeDDML, '')
         id%process = add_registration(respondToEventReg, 'process', nullTypeDDML, '')
      end subroutine
end module Registrations

