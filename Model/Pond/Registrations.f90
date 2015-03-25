module Registrations
   use DataTypes
   type IDsType
      sequence
      integer :: actualresiduedecompositioncalculated
      integer :: create
      integer :: sysinit
      integer :: prepare
      integer :: process
      integer :: tick
      integer :: newmet
      integer :: potentialresiduedecompositioncalculated
      integer :: biomassremoved

   end type IDsType

   contains

      subroutine doRegistrations(id)
         use Infrastructure
         type(IDsType) :: id

         id%create = add_registration(respondToEventReg, 'create', nullTypeDDML, '')
         id%sysinit = add_registration(respondToEventReg, 'sysinit', nullTypeDDML, '')
         id%prepare = add_registration(respondToEventReg, 'prepare', nullTypeDDML, '')
         id%process = add_registration(respondToEventReg, 'process', nullTypeDDML, '')
         id%actualresiduedecompositioncalculated = add_registration(eventReg, 'actualresiduedecompositioncalculated', SurfaceOrganicMatterDecompTypeDDML, '')
         id%tick = add_registration(respondToEventReg, 'tick', TimeTypeDDML, '')
         id%newmet = add_registration(respondToEventReg, 'newmet', NewMetTypeDDML, '')
         id%potentialresiduedecompositioncalculated = add_registration(respondToEventReg, 'potentialresiduedecompositioncalculated', SurfaceOrganicMatterDecompTypeDDML, '')
         id%biomassremoved = add_registration(eventReg, 'BiomassRemoved', BiomassRemovedTypeDDML, '')
      end subroutine
end module Registrations

