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
      integer :: add_faeces
      integer :: dummy

   end type IDsType

   contains

      subroutine doRegistrations(id)
         use Infrastructure
         type(IDsType) :: id

         id%dummy = add_reg(respondToGetReg, 'surfaceom_wt', floatTypeDDML, 'kg/ha', '')
         id%dummy = add_reg(respondToGetReg, 'surfaceom_c', floatTypeDDML, 'kg/ha', '')
         id%dummy = add_reg(respondToGetReg, 'surfaceom_n', floatTypeDDML, 'kg/ha', '')
         id%dummy = add_reg(respondToGetReg, 'surfaceom_p', floatTypeDDML, 'kg/ha', '')
         id%dummy = add_reg(respondToGetReg, 'surfaceom_ashalk', floatTypeDDML, 'kg/ha', '')
         id%dummy = add_reg(respondToGetReg, 'surfaceom_no3', floatTypeDDML, 'kg/ha', '')
         id%dummy = add_reg(respondToGetReg, 'surfaceom_nh4', floatTypeDDML, 'kg/ha', '')
         id%dummy = add_reg(respondToGetReg, 'surfaceom_labile_p', floatTypeDDML, 'kg/ha', '')
         id%dummy = add_reg(respondToGetReg, 'surfaceom_cover', floatTypeDDML, 'm^2/m^2', '')
         id%dummy = add_reg(respondToGetReg, 'tf', floatTypeDDML, '0-1', '')
         id%dummy = add_reg(respondToGetReg, 'wf', floatTypeDDML, '0-1', '')
         id%dummy = add_reg(respondToGetReg, 'cf', floatTypeDDML, '0-1', '')
         id%dummy = add_reg(respondToGetReg, 'leaching_fr', floatTypeDDML, '0-1', '')
         id%dummy = add_reg(respondToGetReg, 'surface_organic_matter', intTypeDDML, '', '')
		 id%dummy = add_reg(respondToGetReg, 'carbonbalance', floatTypeDDML, '', 'Carbon Balance')
		 id%dummy = add_reg(respondToGetReg, 'nitrogenbalance', floatTypeDDML, '', 'Nitrogen Balance')
         id%potentialresiduedecompositioncalculated = add_registration(eventReg, 'potentialresiduedecompositioncalculated', SurfaceOrganicMatterDecompTypeDDML, '')
         id%surfaceorganicmatterstate = add_registration(eventReg, 'surfaceorganicmatterstate', SurfaceOrganicMatterTypeDDML, '')
         id%IncorpFOMPool = add_registration(eventReg, 'IncorpFOMPool', FOMPoolTypeDDML, '')
         id%residue_added = add_registration(eventReg, 'residue_added', ApsimVariantTypeDDML, '')
         id%residue_removed = add_registration(eventReg, 'residue_removed', ApsimVariantTypeDDML, '')
         id%surfaceom_removed = add_registration(eventReg, 'surfaceom_removed', ApsimVariantTypeDDML, '')
         id%decomposed = add_registration(eventReg, 'decomposed', nullTypeDDML, '')
         id%externalmassflow = add_registration(eventReg, 'externalmassflow', ExternalMassFlowTypeDDML, '')
         id%tillage = add_registration(respondToEventReg, 'tillage', TillageTypeDDML, '')
         id%add_surfaceom = add_registration(respondToEventReg, 'add_surfaceom', ApsimVariantTypeDDML, '')
         id%sysinit = add_registration(respondToEventReg, 'sysinit', nullTypeDDML, '')
         id%reset = add_registration(respondToEventReg, 'reset', nullTypeDDML, '')
         id%create = add_registration(respondToEventReg, 'create', nullTypeDDML, '')
         id%sum_report = add_registration(respondToEventReg, 'sum_report', nullTypeDDML, '')
         id%remove_surfaceom = add_registration(respondToEventReg, 'remove_surfaceom', SurfaceOrganicMatterTypeDDML, '')
         id%tick = add_registration(respondToEventReg, 'tick', timeTypeDDML, '')
         id%newmet = add_registration(respondToEventReg, 'newmet', NewMetTypeDDML, '')
         id%irrigated = add_registration(respondToEventReg, 'irrigated', ApsimVariantTypeDDML, '')
         id%crop_chopped = add_registration(respondToEventReg, 'crop_chopped', ApsimVariantTypeDDML, '')
         id%BiomassRemoved = add_registration(respondToEventReg, 'BiomassRemoved', BiomassRemovedTypeDDML, '')
         id%process = add_registration(respondToEventReg, 'process', nullTypeDDML, '')
         id%post = add_registration(respondToEventReg, 'post', nullTypeDDML, '')
         id%actualresiduedecompositioncalculated = add_registration(respondToEventReg, 'actualresiduedecompositioncalculated', SurfaceOrganicMatterDecompTypeDDML, '')
         id%add_faeces = add_registration(respondToEventReg, 'add_faeces', AddFaecesTypeDDML, '')

	 end subroutine
end module Registrations

