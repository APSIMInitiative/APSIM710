module Registrations
   use DataTypes
   type IDsType
      sequence
      integer :: gimme_water
      integer :: top_up
      integer :: water_supplied
      integer :: tick
      integer :: newmet
      integer :: process
      integer :: new_solute
      integer :: runoffEvent

   end type IDsType

   contains

      subroutine doRegistrations(id)
         use Infrastructure
         type(IDsType) :: id

         id%gimme_water = add_registration(respondToEventReg, 'gimme_water', nullTypeDDML, '')
         id%top_up = add_registration(respondToEventReg, 'top_up', nullTypeDDML, '')
         id%water_supplied = add_registration(respondToEventReg, 'water_supplied', nullTypeDDML, '')
         id%tick = add_registration(respondToEventReg, 'tick', TimeTypeDDML, '')
         id%newmet = add_registration(respondToEventReg, 'newmet', NewMetTypeDDML, '')
         id%process = add_registration(respondToEventReg, 'process', nullTypeDDML, '')
         id%new_solute = add_registration(respondToEventReg, 'new_solute', ApsimVariantTypeDDML, '')
         id%runoffEvent = add_registration(respondToEventReg, 'RunoffEvent', RunoffEventTypeDDML, '')

      end subroutine
end module Registrations

