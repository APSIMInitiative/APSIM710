module Registrations
   use DataTypes
   type IDsType
      sequence
      integer :: crop_chopped
      integer :: killing
      integer :: establishing
      integer :: sysinit
      integer :: establish
      integer :: kill
      integer :: prepare
      integer :: process
      integer :: remove_crop_biomass
      integer :: detach_crop_biomass

   end type IDsType

   contains

      subroutine doRegistrations(id)
         use Infrastructure
         type(IDsType) :: id

         id%crop_chopped = add_registration(eventReg, 'crop_chopped', ApsimVariantTypeDDML, '')
         id%killing = add_registration(eventReg, 'killing', nullTypeDDML, '')
         id%establishing = add_registration(eventReg, 'establishing', nullTypeDDML, '')
         id%sysinit = add_registration(respondToEventReg, 'sysinit', nullTypeDDML, '')
         id%establish = add_registration(respondToEventReg, 'establish', nullTypeDDML, '')
         id%kill = add_registration(respondToEventReg, 'kill', nullTypeDDML, '')
         id%prepare = add_registration(respondToEventReg, 'prepare', nullTypeDDML, '')
         id%process = add_registration(respondToEventReg, 'process', nullTypeDDML, '')
         id%remove_crop_biomass = add_registration(respondToEventReg, 'remove_crop_biomass', nullTypeDDML, '')
         id%detach_crop_biomass = add_registration(respondToEventReg, 'detach_crop_biomass_rate', nullTypeDDML, '')
      end subroutine
end module Registrations

