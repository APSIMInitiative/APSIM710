module Registrations
   use DataTypes
   type IDsType
      sequence
      integer :: newpotentialgrowth
      integer :: new_canopy
      integer :: newcrop
      integer :: incorp_fom
      integer :: crop_chopped
      integer :: killing
      integer :: thinning
      integer :: cutting
      integer :: establishing
      integer :: create
      integer :: sysinit
      integer :: cut
      integer :: thin
      integer :: establish
      integer :: kill
      integer :: change_class
      integer :: tick
      integer :: newmet
      integer :: prepare
      integer :: process
      integer :: canopy_energy_balance
      integer :: canopy_water_balance

   end type IDsType

   contains

      subroutine doRegistrations(id)
         use Infrastructure
         type(IDsType) :: id

         id%newpotentialgrowth = add_registration(eventReg, 'newpotentialgrowth', NewPotentialGrowthTypeDDML, '')
         id%new_canopy = add_registration(eventReg, 'new_canopy', nullTypeDDML, '')
         id%newcrop = add_registration(eventReg, 'newcrop', NewCropTypeDDML, '')
         id%incorp_fom = add_registration(eventReg, 'IncorpFOM', FOMLayerTypeDDML, '')
         id%crop_chopped = add_registration(eventReg, 'crop_chopped', ApsimVariantTypeDDML, '')
         id%killing = add_registration(eventReg, 'killing', nullTypeDDML, '')
         id%thinning = add_registration(eventReg, 'thinning', nullTypeDDML, '')
         id%cutting = add_registration(eventReg, 'cutting', nullTypeDDML, '')
         id%establishing = add_registration(eventReg, 'establishing', nullTypeDDML, '')
         id%create = add_registration(respondToEventReg, 'create', nullTypeDDML, '')
         id%sysinit = add_registration(respondToEventReg, 'sysinit', nullTypeDDML, '')
         id%cut = add_registration(respondToEventReg, 'cut', nullTypeDDML, '')
         id%thin = add_registration(respondToEventReg, 'thin', nullTypeDDML, '')
         id%establish = add_registration(respondToEventReg, 'establish', nullTypeDDML, '')
         id%kill = add_registration(respondToEventReg, 'kill', nullTypeDDML, '')
         id%change_class = add_registration(respondToEventReg, 'change_class', nullTypeDDML, '')
         id%tick = add_registration(respondToEventReg, 'tick', nullTypeDDML, '')
         id%newmet = add_registration(respondToEventReg, 'newmet', NewMetTypeDDML, '')
         id%prepare = add_registration(respondToEventReg, 'prepare', nullTypeDDML, '')
         id%process = add_registration(respondToEventReg, 'process', nullTypeDDML, '')
         id%canopy_energy_balance = add_registration(respondToEventReg, 'canopy_energy_balance', nullTypeDDML, '')
         id%canopy_water_balance = add_registration(respondToEventReg, 'canopy_water_balance', nullTypeDDML, '')
      end subroutine
end module Registrations

