module Registrations
   use DataTypes
   type IDsType
      sequence
      integer :: potentialresiduedecompositioncalculated
      integer :: surfaceorganicmatterstate
      integer :: IncorpFOMPool
      integer :: residue_added
      integer :: residue_removed
      integer :: surfaceom_removed
      integer :: decomposed
      integer :: externalmassflow
      integer :: tillage
      integer :: tillage_single
      integer :: add_surfaceom
      integer :: sysinit
      integer :: reset
      integer :: create
      integer :: sum_report
      integer :: remove_surfaceom
      integer :: tick
      integer :: newmet
      integer :: irrigated
      integer :: crop_chopped
      integer :: process
      integer :: post
      integer :: actualresiduedecompositioncalculated
      integer :: BiomassRemoved
      integer :: dummy

   end type IDsType

   contains

      subroutine doRegistrations(id)
         use Infrastructure
         type(IDsType) :: id

         id%dummy = add_reg(respondToGetReg, 'surfaceom_wt', floatTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'surfaceom_c', floatTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'surfaceom_n', floatTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'surfaceom_p', floatTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'surfaceom_no3', floatTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'surfaceom_nh4', floatTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'surfaceom_labile_p', floatTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'surfaceom_cover', floatTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'tf', floatTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'cf', floatTypeDDML, '', '')
         id%potentialresiduedecompositioncalculated = add_registration(eventReg, 'potentialresiduedecompositioncalculated', SurfaceOrganicMatterDecompTypeDDML, '')
         id%surfaceorganicmatterstate = add_registration(eventReg, 'surfaceorganicmatterstate', SurfaceOrganicMatterTypeDDML, '')
         id%IncorpFOMPool = add_registration(eventReg, 'IncorpFOMPool', FOMPoolTypeDDML, '')
         id%residue_added = add_registration(eventReg, 'residue_added', ApsimVariantTypeDDML, '')
         id%residue_removed = add_registration(eventReg, 'residue_removed', ApsimVariantTypeDDML, '')
         id%surfaceom_removed = add_registration(eventReg, 'surfaceom_removed', ApsimVariantTypeDDML, '')
         id%decomposed = add_registration(eventReg, 'decomposed', nullTypeDDML, '')
         id%externalmassflow = add_registration(eventReg, 'externalmassflow', ExternalMassFlowTypeDDML, '')
         id%tillage = add_registration(respondToEventReg, 'tillage', nullTypeDDML, '')
         id%tillage_single = add_registration(respondToEventReg, 'tillage_single', nullTypeDDML, '')
         id%add_surfaceom = add_registration(respondToEventReg, 'add_surfaceom', nullTypeDDML, '')
         id%sysinit = add_registration(respondToEventReg, 'sysinit', nullTypeDDML, '')
         id%reset = add_registration(respondToEventReg, 'reset', nullTypeDDML, '')
         id%create = add_registration(respondToEventReg, 'create', nullTypeDDML, '')
         id%sum_report = add_registration(respondToEventReg, 'sum_report', nullTypeDDML, '')
         id%remove_surfaceom = add_registration(respondToEventReg, 'remove_surfaceom', SurfaceOrganicMatterTypeDDML, '')
         id%tick = add_registration(respondToEventReg, 'tick', nullTypeDDML, '')
         id%newmet = add_registration(respondToEventReg, 'newmet', NewMetTypeDDML, '')
         id%irrigated = add_registration(respondToEventReg, 'irrigated', ApsimVariantTypeDDML, '')
         id%crop_chopped = add_registration(respondToEventReg, 'crop_chopped', ApsimVariantTypeDDML, '')
         id%BiomassRemoved = add_registration(respondToEventReg, 'BiomassRemoved', BiomassRemovedTypeDDML, '')
         id%process = add_registration(respondToEventReg, 'process', nullTypeDDML, '')
         id%post = add_registration(respondToEventReg, 'post', nullTypeDDML, '')
         id%actualresiduedecompositioncalculated = add_registration(respondToEventReg, 'actualresiduedecompositioncalculated', SurfaceOrganicMatterDecompTypeDDML, '')

	 end subroutine
end module Registrations

