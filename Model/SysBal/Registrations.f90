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
		 integer :: dummy

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

         ! variables we own and make gettable
         dummy = add_reg(respondToGetReg, 'n_loss_system', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'n_gain_system', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'n_dlt_system', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'n_dlt_surface', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'n_dlt_crop', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'n_dlt_soil', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'n_error_system', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'n_cum_error_system', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'n_state_system', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'n_state_surface', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'n_state_crop', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'n_state_soil', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'p_loss_system', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'p_gain_system', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'p_dlt_system', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'p_dlt_surface', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'p_dlt_crop', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'p_dlt_soil', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'p_error_system', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'p_cum_error_system', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'p_state_system', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'p_state_surface', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'p_state_crop', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'p_state_soil', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'c_loss_system', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'c_gain_system', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'c_dlt_system', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'c_dlt_surface', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'c_dlt_crop', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'c_dlt_soil', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'c_error_system', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'c_cum_error_system', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'c_state_system', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'c_state_surface', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'c_state_crop', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'c_state_soil', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'dm_loss_system', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'dm_gain_system', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'dm_dlt_system', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'dm_dlt_surface', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'dm_dlt_crop', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'dm_dlt_soil', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'dm_error_system', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'dm_cum_error_system', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'dm_state_system', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'dm_state_surface', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'dm_state_crop', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'dm_state_soil', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetReg, 'sw_loss_system', floatTypeDDML, 'mm', '')
         dummy = add_reg(respondToGetReg, 'sw_gain_system', floatTypeDDML, 'mm', '')
         dummy = add_reg(respondToGetReg, 'sw_dlt_system', floatTypeDDML, 'mm', '')
         dummy = add_reg(respondToGetReg, 'sw_dlt_surface', floatTypeDDML, 'mm', '')
         dummy = add_reg(respondToGetReg, 'sw_dlt_crop', floatTypeDDML, 'mm', '')
         dummy = add_reg(respondToGetReg, 'sw_dlt_soil', floatTypeDDML, 'mm', '')
         dummy = add_reg(respondToGetReg, 'sw_error_system', floatTypeDDML, 'mm', '')
         dummy = add_reg(respondToGetReg, 'sw_cum_error_system', floatTypeDDML, 'mm', '')
         dummy = add_reg(respondToGetReg, 'sw_state_system', floatTypeDDML, 'mm', '')
         dummy = add_reg(respondToGetReg, 'sw_state_surface', floatTypeDDML, 'mm', '')
         dummy = add_reg(respondToGetReg, 'sw_state_crop', floatTypeDDML, 'mm', '')
         dummy = add_reg(respondToGetReg, 'sw_state_soil', floatTypeDDML, 'mm', '')
      end subroutine
end module Registrations
