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
         integer :: dummy

         id%crop_chopped = add_registration(eventReg, 'crop_chopped', ApsimVariantTypeDDML, '')
         id%killing = add_registration(eventReg, 'killing', nullTypeDDML, '')
         id%establishing = add_registration(eventReg, 'establishing', nullTypeDDML, '')
         id%sysinit = add_registration(respondToEventReg, 'sysinit', nullTypeDDML, '')
         id%establish = add_registration(respondToEventReg, 'establish', nullTypeDDML, '')
         id%kill = add_registration(respondToEventReg, 'kill', nullTypeDDML, '')
         id%prepare = add_registration(respondToEventReg, 'prepare', nullTypeDDML, '')
         id%process = add_registration(respondToEventReg, 'process', nullTypeDDML, '')
         id%remove_crop_biomass = add_registration(respondToEventReg, 'remove_crop_biomass', RemoveCropBiomassTypeDDML, '')
         id%detach_crop_biomass = add_registration(respondToEventReg, 'detach_crop_biomass_rate', RemoveCropBiomassTypeDDML, '')

         ! variables we get from other modules.
         dummy = add_registration_with_units(getVariableReg, 'day', IntTypeDDML, '')
         dummy = add_registration_with_units(getVariableReg, 'year', IntTypeDDML, '')
         dummy = add_registration_with_units(getVariableReg, 'maxt', floatTypeDDML, 'oC')
         dummy = add_registration_with_units(getVariableReg, 'mint', floatTypeDDML, 'oC')
         dummy = add_registration_with_units(getVariableReg, 'radn', floatTypeDDML, 'MJ/m^2')
         dummy = add_registration_with_units(getVariableReg, 'pan', floatTypeDDML, 'mm')
         dummy = add_registration_with_units(getVariableReg, 'eo', floatTypeDDML, 'mm')
         dummy = add_registration_with_units(getVariableReg, 'vpd', floatTypeDDML, 'hPa')
         dummy = add_registration_with_units(getVariableReg, 'es', floatTypeDDML, 'mm')
         dummy = add_registration_with_units(getVariableReg, 'dlayer', floatArrayTypeDDML, 'mm')
         dummy = add_registration_with_units(getVariableReg, 'bd', floatArrayTypeDDML, 'mm')
         dummy = add_registration_with_units(getVariableReg, 'dul_dep', floatArrayTypeDDML, 'mm')
         dummy = add_registration_with_units(getVariableReg, 'sw_dep', floatArrayTypeDDML, 'mm')
         dummy = add_registration_with_units(getVariableReg, 'no3', floatArrayTypeDDML, 'kg/ha')
         dummy = add_registration_with_units(getVariableReg, 'soil_loss', floatTypeDDML, 't/ha')
		 
         ! variables we own and make available to other modules.
         dummy = add_reg(respondToGetReg, 'crop_type', stringTypeDDML, '', 'Crop type')   
         dummy = add_reg(respondToGetReg, 'crop_status', intTypeDDML, '', 'Crop status')  
         dummy = add_reg(respondToGetSetReg, 'PlantStatus', stringTypeDDML, ' ', 'Plant status')
         dummy = add_reg(respondToGetSetReg, 'stage', floatTypeDDML, '', 'Phenological stage')
         dummy = add_reg(respondToGetSetReg, 'stage_code', floatTypeDDML, '', 'Phenological stage code')
         dummy = add_reg(respondToGetSetReg, 'stage_name', stringTypeDDML, '', 'Phenological stage name')
         dummy = add_reg(respondToGetSetReg, 'height', floatTypeDDML, 'mm', 'Canopy height')
         dummy = add_reg(respondToGetSetReg, 'CoverTotal', floatTypeDDML, ' ', 'Total cover')
         dummy = add_reg(respondToGetSetReg, 'coverlive', floatTypeDDML, ' ', 'Green cover')
         dummy = add_reg(respondToGetSetReg, 'green_cover', floatTypeDDML, ' ', 'Green cover')
         dummy = add_reg(respondToGetSetReg, 'radn_cover', floatTypeDDML, ' ', '' )
         dummy = add_reg(respondToGetSetReg, 'transp_cover', floatTypeDDML, ' ', '')
         dummy = add_reg(respondToGetSetReg, 'clothesline', floatTypeDDML, ' ', '')
         dummy = add_reg(respondToGetSetReg, 'tfact', floatTypeDDML, ' ', '')
         dummy = add_reg(respondToGetSetReg, 'nfact', floatTypeDDML, ' ', '')
         dummy = add_reg(respondToGetSetReg, 'vdp_estimation', floatTypeDDML, ' ', '')
         dummy = add_reg(respondToGetSetReg, 'tsdm', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetSetReg, 'root_depth', floatTypeDDML, 'mm', '')
         dummy = add_reg(respondToGetSetReg, 'GrowthRate', floatTypeDDML, 'g/m^2', '')
         dummy = add_reg(respondToGetSetReg, 'n_green', floatArrayTypeDDML, 'g/m^2', '')
         dummy = add_reg(respondToGetSetReg, 'n_dead', floatArrayTypeDDML, 'g/m^2', '')
         dummy = add_reg(respondToGetSetReg, 'dm_green', floatArrayTypeDDML, 'g/m^2', '')
         dummy = add_reg(respondToGetSetReg, 'dlt_dm_green', floatArrayTypeDDML, 'g/m^2', '')
         dummy = add_reg(respondToGetSetReg, 'dm_senesced', floatArrayTypeDDML, 'g/m^2', '')
         dummy = add_reg(respondToGetSetReg, 'green_root', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetSetReg, 'green_leaf', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetSetReg, 'leafgreenwt', floatTypeDDML, 'g/m^2', '')
         dummy = add_reg(respondToGetSetReg, 'green_stem', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetSetReg, 'stemgreenwt', floatTypeDDML, 'g/m^2', '')
         dummy = add_reg(respondToGetSetReg, 'green_pool', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetSetReg, 'dead_pool', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetSetReg, 'dead_root', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetSetReg, 'dead_leaf', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetSetReg, 'leafsenescedwt', floatTypeDDML, 'g/m^2', '')
         dummy = add_reg(respondToGetSetReg, 'dead_stem', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetSetReg, 'stemsenescedwt', floatTypeDDML, 'g/m^2', '')
         dummy = add_reg(respondToGetSetReg, 'detachment', floatTypeDDML, 'kg/ha', 'litter')
         dummy = add_reg(respondToGetSetReg, 'basal_area', floatTypeDDML, 'm^2/ha', '')
         dummy = add_reg(respondToGetSetReg, 'acc_growth_for_n', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetSetReg, 'acc_trans_for_n', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetSetReg, 'ep', floatTypeDDML, 'mm', '')
         dummy = add_reg(respondToGetSetReg, 'sw_uptake', floatArrayTypeDDML, 'mm', '')
         dummy = add_reg(respondToGetSetReg, 'sw_pot', floatTypeDDML, 'mm', '')
         dummy = add_reg(respondToGetSetReg, 'growth', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetSetReg, 'GrowthRate', floatTypeDDML, 'g/m^2', '')
         dummy = add_reg(respondToGetSetReg, 'growth_transp', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetSetReg, 'growth_photo', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetSetReg, 'growth_regrowth', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetSetReg, 'death', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetSetReg, 'death_frost', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetSetReg, 'death_frost_leaf', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetSetReg, 'death_frost_stem', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetSetReg, 'death_water', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetSetReg, 'death_water_leaf', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetSetReg, 'death_water_stem', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetSetReg, 'death_pheno', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetSetReg, 'death_pheno_leaf', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetSetReg, 'death_pheno_stem', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetSetReg, 'sw_demand', floatTypeDDML, 'mm', '')
         dummy = add_reg(respondToGetSetReg, 'AboveGroundN', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetSetReg, 'dlt_n_uptake', floatTypeDDML, 'kg/ha', '')
         dummy = add_reg(respondToGetSetReg, 'n_index', floatTypeDDML, '', '')
         dummy = add_reg(respondToGetSetReg, 'rad_index', floatTypeDDML, '', '')
         dummy = add_reg(respondToGetSetReg, 'sw_index', floatTypeDDML, '', '')
         dummy = add_reg(respondToGetSetReg, 'swi', floatArrayTypeDDML, 'mm', '')
         dummy = add_reg(respondToGetSetReg, 'temp_index', floatTypeDDML, '', '')
         dummy = add_reg(respondToGetSetReg, 'growth_index', floatTypeDDML, '', '')
         dummy = add_reg(respondToGetSetReg, 'transp_eff_adj', floatTypeDDML, '', '')
         dummy = add_reg(respondToGetSetReg, 'vpd_hgt_ndx', floatTypeDDML, '', '')
         dummy = add_reg(respondToGetSetReg, 'rlv', floatArrayTypeDDML, '', '')
         dummy = add_reg(respondToGetSetReg, 'max_n_avail', floatArrayTypeDDML, '', '')
		 
         ! settable variables
         dummy = add_registration_with_units(respondToSetReg, 'green_leaf', floatTypeDDML, 'kg/ha')
         dummy = add_registration_with_units(respondToSetReg, 'dlt_green_leaf', floatTypeDDML, 'kg/ha')
         dummy = add_registration_with_units(respondToSetReg, 'green_stem', floatTypeDDML, 'kg/ha')
         dummy = add_registration_with_units(respondToSetReg, 'dlt_green_stem', floatTypeDDML, 'kg/ha')
         dummy = add_registration_with_units(respondToSetReg, 'green_root', floatTypeDDML, 'kg/ha')
         dummy = add_registration_with_units(respondToSetReg, 'dlt_green_root', floatTypeDDML, 'kg/ha')
         dummy = add_registration_with_units(respondToSetReg, 'dead_leaf', floatTypeDDML, 'kg/ha')
         dummy = add_registration_with_units(respondToSetReg, 'dlt_dead_leaf', floatTypeDDML, 'kg/ha')
         dummy = add_registration_with_units(respondToSetReg, 'dead_stem', floatTypeDDML, 'kg/ha')
         dummy = add_registration_with_units(respondToSetReg, 'dlt_dead_stem', floatTypeDDML, 'kg/ha')
         dummy = add_registration_with_units(respondToSetReg, 'dead_root', floatTypeDDML, 'kg/ha')
         dummy = add_registration_with_units(respondToSetReg, 'dlt_dead_root', floatTypeDDML, 'kg/ha')
         dummy = add_registration_with_units(respondToSetReg, 'green_pool', floatTypeDDML, 'kg/ha')
         dummy = add_registration_with_units(respondToSetReg, 'dlt_green_pool', floatTypeDDML, 'kg/ha')
         dummy = add_registration_with_units(respondToSetReg, 'dead_pool', floatTypeDDML, 'kg/ha')
         dummy = add_registration_with_units(respondToSetReg, 'dlt_dead_pool', floatTypeDDML, 'kg/ha')
         dummy = add_registration_with_units(respondToSetReg, 'basal_area', floatTypeDDML, '%')
         dummy = add_registration_with_units(respondToSetReg, 'root_depth', floatTypeDDML, 'mm')
         dummy = add_registration_with_units(respondToSetReg, 'height_1000kg', floatTypeDDML, 'mm')
         dummy = add_registration_with_units(respondToSetReg, 'kl2rlv', floatTypeDDML, '')
      end subroutine
end module Registrations

