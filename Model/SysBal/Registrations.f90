module Registrations
   use DataTypes
   type IDsType
      sequence
      integer :: residue_added
      integer :: residue_removed
      integer :: surfaceom_removed
      integer :: prepare
      integer :: post
      integer :: irrigated
      integer :: crop_chopped
      integer :: process
      integer :: remove_surfaceom
      integer :: externalmassflow
      integer :: BiomassRemoved

   end type IDsType

   contains

      subroutine doRegistrations(id)
         use Infrastructure
         type(IDsType) :: id

         id%residue_added = add_registration(eventReg, 'residue_added', nullTypeDDML, '')
         id%residue_removed = add_registration(eventReg, 'residue_removed', nullTypeDDML, '')
         id%surfaceom_removed = add_registration(eventReg, 'surfaceom_removed', nullTypeDDML, '')
         id%prepare = add_registration(respondToEventReg, 'prepare', nullTypeDDML, '')
         id%post = add_registration(respondToEventReg, 'post', nullTypeDDML, '')
         id%irrigated = add_registration(respondToEventReg, 'irrigated', ApsimVariantTypeDDML, '')
         id%crop_chopped = add_registration(respondToEventReg, 'crop_chopped', ApsimVariantTypeDDML, '')
         id%BiomassRemoved = add_registration(respondToEventReg, 'BiomassRemoved', BiomassRemovedTypeDDML, '')
         id%process = add_registration(respondToEventReg, 'process', nullTypeDDML, '')
         id%remove_surfaceom = add_registration(respondToEventReg, 'remove_surfaceom', SurfaceOrganicMatterTypeDDML, '')
         id%externalmassflow = add_registration(respondToEventReg, 'externalmassflow', ExternalMassFlowTypeDDML, '')
      end subroutine
end module Registrations

