module Registrations
   use DataTypes
   type IDsType
      sequence
      integer :: externalmassflow
      integer :: sysinit
      integer :: reset
      integer :: sum_report
      integer :: tillage
      integer :: incorp_fom
      integer :: IncorpFOMPool
      integer :: decomposed
      integer :: actualresiduedecompositioncalculated
      integer :: tick
      integer :: process

   end type IDsType

   contains

      subroutine doRegistrations(id)
         use Infrastructure
         type(IDsType) :: id

         id%externalmassflow = add_registration(eventReg, 'externalmassflow', ExternalMassFlowTypeDDML, '')
         id%sysinit = add_registration(respondToEventReg, 'sysinit', nullTypeDDML, '')
         id%reset = add_registration(respondToEventReg, 'reset', nullTypeDDML, '')
         id%sum_report = add_registration(respondToEventReg, 'sum_report', nullTypeDDML, '')
         id%tillage = add_registration(respondToEventReg, 'tillage', nullTypeDDML, '')
         id%incorp_fom = add_registration(respondToEventReg, 'IncorpFOM', FOMLayerTypeDDML, '')
         id%IncorpFOMPool = add_registration(respondToEventReg, 'IncorpFOMPool', FOMPoolTypeDDML, '')
         id%decomposed = add_registration(respondToEventReg, 'decomposed', nullTypeDDML, '')
         id%actualresiduedecompositioncalculated = add_registration(respondToEventReg, 'actualresiduedecompositioncalculated', SurfaceOrganicMatterDecompTypeDDML, '')
         id%tick = add_registration(respondToEventReg, 'tick', nullTypeDDML, '')
         id%process = add_registration(respondToEventReg, 'process', nullTypeDDML, '')
      end subroutine
end module Registrations

