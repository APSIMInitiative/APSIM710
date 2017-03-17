!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!! This file is only used in CropMod.dll which is only used by sunflower
!!!!!!!!! MAIZE has it's own CropModMain.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!     ===========================================================
      subroutine alloc_dealloc_instance(doAllocate)
!     ===========================================================
      use CropModModule
      implicit none
      ml_external alloc_dealloc_instance
!STDCALL(alloc_dealloc_instance)

!+  Sub-Program Arguments
      logical, intent(in) :: doAllocate

!+  Purpose
!      Module instantiation routine.

!- Implementation Section ----------------------------------

      if (doAllocate) then
         allocate(g)
         allocate(p)
         allocate(c)
         allocate(id)
      else
         deallocate(g)
         deallocate(p)
         deallocate(c)
         deallocate(id)
      end if
      return
      end subroutine

* ====================================================================
      subroutine doInit1()
* ====================================================================
      use CropModData
      use CropModModule
      implicit none
      ml_external doInit1
!STDCALL(doInit1)
      integer dummy

      id%externalmassflow = add_registration(eventReg
     :                     , 'externalmassflow'
     :                     , ExternalMassFlowTypeDDML, '')
      id%incorp_fom = add_registration(eventReg, 'incorpfom',
     :                                 nullTypeDDML, '')
      id%add_residue_p = add_registration(eventReg, 'add_residue_p',
     :                                    nullTypeDDML, '')
      id%crop_chopped = add_registration(eventReg, 'crop_chopped',
     :                                   ApsimVariantTypeDDML, '')
      id%biomass_removed = add_registration(eventReg, 'BiomassRemoved',
     :                                   BiomassRemovedTypeDDML, '')
      id%sowing = add_registration(eventReg, 'sowing',
     :                             nullTypeDDML, '')
      id%harvesting = add_registration(eventReg, 'harvesting',
     :                                 nullTypeDDML, '')
      id%create = add_registration(respondToEventReg, 'create',
     :                             nullTypeDDML, '')
      id%sysinit = add_registration(respondToEventReg, 'sysinit',
     :                              nullTypeDDML, '')
      id%sow = add_registration(respondToEventReg, 'sow',
     :                          SowTypeDDML, '')
      id%harvest = add_registration(respondToEventReg, 'harvest',
     :                              HarvestTypeDDML, '')
      id%kill_crop = add_registration(respondToEventReg, 'kill_crop',
     :                                nullTypeDDML, '')
      id%end_crop = add_registration(respondToEventReg, 'end_crop',
     :                               nullTypeDDML, '')
      id%prepare = add_registration(respondToEventReg, 'prepare',
     :                              nullTypeDDML, '')
      id%process = add_registration(respondToEventReg, 'process',
     :                              nullTypeDDML, '')
      id%newcrop = add_registration(eventReg, 'newcrop', 
     :              nullTypeDDML, '')	 
      id%BiocharDecomposed = add_registration(respondToEventReg, 
     :                          'BiocharDecomposed', 
     :                          BiocharDecomposedTypeDDML, '')
	 
      dummy = add_registration_with_units(respondToGetReg, 'crop_type',
     :                                    stringTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :                          'extinct_coeff', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 'radn_int',
     :                                    floatTypeDDML, 'MJ/m^2/d')
      dummy = add_registration_with_units(respondToGetReg, 'rue_day',
     :                                    floatTypeDDML, 'g/MJ')
      dummy = add_registration_with_units(respondToGetReg,
     :                                   'DaysAfterSowing',
     :                                    intTypeDDML, 'days')
      dummy = add_registration_with_units(respondToGetReg
     :                         , 'plant_status', stringTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 'StageName',
     :                                    StringTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 'stage_code',
     :                                    floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 'stage',
     :                                    floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :                            'zadok_stage', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 'DeltaStage',
     :                                    floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 'TT',
     :                                    floatTypeDDML, 'oCd')
      dummy = add_registration_with_units(respondToGetReg, 'tt_tot',
     :                                    floatarrayTypeDDML, 'oC')
      dummy = add_registration_with_units(respondToGetReg, 'tt_sum',
     :                                    floatTypeDDML, 'ddays')
      dummy = add_registration_with_units(respondToGetReg, 'days_tot',
     :                                    floatarrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg, 'phase_tt',
     :                                    floatarrayTypeDDML, 'oC')
      dummy = add_registration_with_units(respondToGetReg, 'dlt_tt_fm',
     :                                    floatTypeDDML, 'oC')
      dummy = add_registration_with_units(respondToGetReg, 'tt_tot_fm',
     :                                    floatarrayTypeDDML, 'oC')
      dummy = add_registration_with_units(respondToGetReg,
     :                  'flowering_date',  intTypeDDML, 'doy')
      dummy = add_registration_with_units(respondToGetReg,
     :                  'maturity_date', intTypeDDML, 'doy')
      dummy = add_registration_with_units(respondToGetReg,
     :                  'flowering_das', intTypeDDML, 'days')
      dummy = add_registration_with_units(respondToGetReg,
     :                    'maturity_das', intTypeDDML, 'days')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'leaf_primodia', floatTypeDDML, 'lvs')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'leaf_no_final', floatTypeDDML, 'lvs')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'leaf_no', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'dlt_leaf_no', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'leaf_no_dead', floatarrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'leaf_area', floatarrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'cover_green', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'cover_tot', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'lai', floatTypeDDML, 'm^2/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'lai_max', floatTypeDDML, 'm^2/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'lai_sum', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'tlai', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'slai', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'tlai_dead', floatTypeDDML, 'm^2/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'sla', floatTypeDDML, 'mm^2/g')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'dlt_lai', floatTypeDDML, 'm^2/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'dlt_lai_pot', floatTypeDDML, 'm^2/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :                    'dlt_lai_stressed', floatTypeDDML, 'm^2/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'tiller_tt_tot', floatTypeDDML, 'Cd')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'dlt_slai', floatTypeDDML, 'm^2/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'dlt_slai_age', floatTypeDDML, 'm^2/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :                     'dlt_slai_light', floatTypeDDML, 'm^2/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :                     'dlt_slai_water', floatTypeDDML, 'm^2/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :                 'dlt_slai_nitrogen', floatTypeDDML, 'm^2/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'plants', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'height', floatTypeDDML, 'mm')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'tiller_no', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'tiller_no_fertile', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'grain_no', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'grain_size', floatTypeDDML, 'g')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'root_depth', floatTypeDDML, 'mm')
      dummy = add_registration_with_units(respondToGetReg,
     :                 'root_length', floatarrayTypeDDML, 'mm/mm^2')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'rlv', floatarrayTypeDDML, 'mm/mm^3')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'leaf_part', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'stem_part', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'grain_part', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'root_part', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'LeafGreenWt', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'StemGreenWt', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'flower_wt', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :                     'stemandflower_wt', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'grain_wt', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'RootGreenWt', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'droot_wt', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'troot_wt', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'biomass_wt', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :                  'green_biomass_wt', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'stover_wt', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'GreenWt', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :                     'SenescedWt', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'dm_dead', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'yield', floatTypeDDML, 'kg/ha')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'biomass', floatTypeDDML, 'kg/ha')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'stover', floatTypeDDML, 'kg/ha')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'green_biomass', floatTypeDDML, 'kg/ha')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'hi', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'dlt_dm_water', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'dlt_dm_light', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'dlt_dm', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :                'dlt_dm_green', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :          'dlt_dm_green_retrans', floatarrayTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :            'dlt_dm_senesced', floatarrayTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :             'dlt_dm_detached', floatarrayTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :         'dlt_dm_dead_detached', floatarrayTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'swdef_pheno', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'swdef_photo', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'swdef_expan', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'swdef_tiller', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'sw_stress_photo', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'sw_stress_pheno', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'sw_stress_expan', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'sw_stress_tiller', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'ep', floatTypeDDML, 'mm')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'sw_uptake', floatarrayTypeDDML, 'mm')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'transpiration', floatTypeDDML, 'mm')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'transpiration_tot', floatTypeDDML, 'mm')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'cep', floatTypeDDML, 'mm')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'esw_layer', floatarrayTypeDDML, 'mm')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'esw_profile', floatarrayTypeDDML, 'mm')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'sw_deficit', floatarrayTypeDDML, 'mm')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'transp_eff', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'sw_demand', floatTypeDDML, 'mm')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'sw_demand_te', floatTypeDDML, 'mm')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'sw_supply', floatTypeDDML, 'mm')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'sw_supply_sum', floatTypeDDML, 'mm')
      dummy = add_registration_with_units(respondToGetReg,
     :              'sw_supply_demand_ratio', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetSetReg,
     :                        'll', floatarrayTypeDDML, '%')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'll_dep', floatarrayTypeDDML, 'mm')
      dummy = add_registration_with_units(respondToGetSetReg,
     :                        'kl', floatarrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetSetReg,
     :                        'xf', floatarrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :              'leaf_nd', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'stem_nd', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'flower_nd', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'grain_nd', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'root_nd', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'n_demand', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'no3_demand', floatTypeDDML, 'kg/ha')
      dummy = add_registration_with_units(respondToGetReg,
     :              'n_supply_soil', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'n_massflow_uptake', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'no3_massflow_uptake', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'nh4_massflow_uptake', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'no3_diffusion_uptake', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'nh4_diffusion_uptake', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'n_total_uptake', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'no3_total_uptake', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'nh4_total_uptake', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'n_cum_uptake', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'no3_uptake', floatarrayTypeDDML, 'kg/ha')
      dummy = add_registration_with_units(respondToGetReg,
     :              'nh4_uptake', floatarrayTypeDDML, 'kg/ha')
      dummy = add_registration_with_units(respondToGetReg,
     :              'no3_tot', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'nh4_tot', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'hi_n', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :              'biomass_n', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'green_biomass_n', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'stover_n', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'grain_n', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'gleaf_n', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'dleaf_n', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'tleaf_n', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'stem_n', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'flower_n', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'groot_n', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'droot_n', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'troot_n', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'n_green', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'n_senesced', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'n_dead', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'dlt_n_green', floatarrayTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'dlt_n_retrans', floatarrayTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'dlt_n_detached', floatarrayTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :          'dlt_n_dead_detached', floatarrayTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'sln', floatTypeDDML, 'gN/m^2leaf')
      dummy = add_registration_with_units(respondToGetReg,
     :              'n_conc_stover', floatTypeDDML, '%')
      dummy = add_registration_with_units(respondToGetReg,
     :              'LeafGreenNConc', floatTypeDDML, '%')
      dummy = add_registration_with_units(respondToGetReg,
     :              'StemGreenNConc', floatTypeDDML, '%')
      dummy = add_registration_with_units(respondToGetReg,
     :              'RootGreenNConc', floatTypeDDML, '%')
      dummy = add_registration_with_units(respondToGetReg,
     :              'n_conc_grain', floatTypeDDML, '%')
      dummy = add_registration_with_units(respondToGetReg,
     :              'n_conc_leaf_crit', floatTypeDDML, '%')
      dummy = add_registration_with_units(respondToGetReg,
     :              'n_conc_stem_crit', floatTypeDDML, '%')
      dummy = add_registration_with_units(respondToGetReg,
     :              'n_conc_flower_crit', floatTypeDDML, '%')
      dummy = add_registration_with_units(respondToGetReg,
     :              'n_conc_root_crit', floatTypeDDML, '%')
      dummy = add_registration_with_units(respondToGetReg,
     :              'n_conc_stover_crit', floatTypeDDML, '%')
      dummy = add_registration_with_units(respondToGetReg,
     :              'n_conc_leaf_max', floatTypeDDML, '%')
      dummy = add_registration_with_units(respondToGetReg,
     :              'n_conc_stem_max', floatTypeDDML, '%')
      dummy = add_registration_with_units(respondToGetReg,
     :              'n_conc_flower_max', floatTypeDDML, '%')
      dummy = add_registration_with_units(respondToGetReg,
     :              'n_conc_root_max', floatTypeDDML, '%')
      dummy = add_registration_with_units(respondToGetReg,
     :              'n_conc_stover_max', floatTypeDDML, '%')
      dummy = add_registration_with_units(respondToGetReg,
     :              'n_conc_leaf_min', floatTypeDDML, '%')
      dummy = add_registration_with_units(respondToGetReg,
     :              'n_conc_stem_min', floatTypeDDML, '%')
      dummy = add_registration_with_units(respondToGetReg,
     :              'n_conc_flower_min', floatTypeDDML, '%')
      dummy = add_registration_with_units(respondToGetReg,
     :              'n_conc_root_min', floatTypeDDML, '%')
      dummy = add_registration_with_units(respondToGetReg,
     :              'n_conc_stover_min', floatTypeDDML, '%')
      dummy = add_registration_with_units(respondToGetReg,
     :              'n_grain_pcnt', floatTypeDDML, '%')
      dummy = add_registration_with_units(respondToGetReg,
     :              'grain_protein', floatTypeDDML, '%')
      dummy = add_registration_with_units(respondToGetReg,
     :              'nfact_photo', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :              'nfact_pheno', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :              'nfact_expan', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :              'nfact_tiller', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :              'nfact_grain', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :              'nfact_grain_tot', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :              'n_stress_photo', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :              'n_stress_pheno', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :              'n_stress_expan', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :              'n_stress_tiller', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :              'n_stress_grain', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :              'GreenP', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'p_sen', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'p_demand', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'pfact_photo', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :              'pfact_expansion', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :              'pfact_expan', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :              'pfact_expansion', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :              'pfact_grain', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :              'p_stress_photo', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :              'p_stress_fact_pheno', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :              'p_stress_fact_expan', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :              'p_stress_fact_grain', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :              'p_stress_fact_grain', floatTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
     :              'biomass_p', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'p_uptake', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'green_biomass_p', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'grain_p', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'leafgreenp', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'stemgreenp', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'rootgreenp', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'deadleaf_p', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'flower_p', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'head_p', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'SenescedP', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'p_dead', floatarrayTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'GrowthP', floatarrayTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'dlt_p_retrans', floatarrayTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'DetachingP', floatarrayTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'dlt_p_dead', floatarrayTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'dlt_p_sen', floatarrayTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'p_conc_stover', floatTypeDDML, '%')
      dummy = add_registration_with_units(respondToGetReg,
     :              'LeafGreenPConc', floatTypeDDML, '%')
      dummy = add_registration_with_units(respondToGetReg,
     :              'StemGreenPConc', floatTypeDDML, '%')
      dummy = add_registration_with_units(respondToGetReg,
     :              'GrainGreenPConc', floatTypeDDML, '%')
      dummy = add_registration_with_units(respondToGetReg,
     :              'p_grain_pcnt', floatTypeDDML, '%')
      dummy = add_registration_with_units(respondToGetReg,
     :              'p_uptake_stover', floatTypeDDML, 'g/m^2')
      dummy = add_registration_with_units(respondToGetReg,
     :              'grain_p_demand', floatTypeDDML, 'g/m^2')
	  dummy = add_registration_with_units(respondToGetReg,
     :              'shoot_lag', floatTypeDDML, 'oCd')
	  dummy = add_registration_with_units(respondToGetReg,
     :              'root_depth_rate', floatarrayTypeDDML, 'g/m^2')
	  dummy = add_reg(respondToSetReg,
     :              'shoot_lag', floatTypeDDML, '', 'shoot lag')
	  dummy = add_reg(respondToSetReg,
     :              'root_depth_rate', floatarrayTypeDDML, '', 'dlt_rd')

      end subroutine doInit1

*=====================================================================
      subroutine Main (action, data_string)
*=====================================================================
      use CropLibrary
      Use CropModModule
      implicit none
      ml_external Main


*+  Sub-Program Arguments
      character  action*(*)            ! (INPUT) Message action to perform
      character  data_string*(*)       ! (INPUT) Message data

*+  Purpose
*     This module performs crop growth simulation simulates crop phenological
*     development, growth of root, leaf, head, stem and grain,
*     Water and  nitrogen uptake, leaf and root senescense.

*+  Changes
*      271198 ew
*      250894 sc    specified and programmed
*      011195 jngh  added call to message_unused

*+  Calls

*+  Constant Values
      character  my_name*(*)         ! name of this procedure
      parameter (my_name='CropMod')


      LOGICAL TestTrue
      REAL    daylength
      REAL    radn_ext
      REAL    rue_max
      REAL    diff_radn_frac
      REAL    rue_diff_radn_modifier
      character  module_name*(max_module_name_size)

*- Implementation Section ----------------------------------
      call push_routine (my_name)
      call print_routine (my_name)

      if (action.eq.ACTION_init) then

         !Zero pools inlcuding contants and parameters
         call Zero_Variables (.true.)

         !Read the crop specific contants from ini files
         call CropMod_Initialisation ()

         !Request and receive variables from owner-modules
         call Get_Other_Variables ()

         call get_name (module_name)

         if (module_name .eq. 'sorghum' .OR.
     :       module_name .eq. 'maize') then
             call Fatal_error (ERR_user,
     :            'Please use standalone crop module')
         endif

         !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c         if (c%crop_type .eq. 'wheat') then
c             TestTrue = .TRUE.
c         else
c             TestTrue = .FALSE.
c         endif
c
c             TestTrue = .FALSE.
c
c         if (TestTrue) open (1, FILE='test.dat')
         !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


      elseif (action.eq.ACTION_set_variable) then

         ! Respond to request to reset variable values of variables from other modules
         call Set_My_Variable (data_string)

      elseif (action.eq.ACTION_get_variable) then

         !Respond to request for variable values - from other modules
          call Send_My_Variable (Data_string)

      elseif (action.eq.ACTION_prepare) then !this happens each time step (daily)

         if (g%plant_status.ne.status_out) then

            !Zero all daily rate variables
            call Zero_Daily_Variables ()

            !Request and receive variables from owner-modules
            call Get_Other_Variables ()

            !Do crop processes prepare
            call Simulation_Prepare ()

         else
            ! Crop not in the field, do nothing
            call Zero_Variables (.false.)
         endif

      elseif (action.eq.ACTION_process) then


         if (g%plant_status.ne.status_out) then

            !request and receive variables from owner-modules
            call Get_Other_Variables ()

            !Crop processes - Dynamic prcess sub-modules
            call Crop_Process ()

            !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c           if (TestTrue) then

c               call Photoperiod_Wang (
c     :                 g%day_of_year
c     :               , g%latitude
c     :               , c%twilight
c     :               , daylength )

c               call ExtraTerrestrialRadiationDailyTotal (
c     :                 g%day_of_year
c     :               , g%latitude
c     :               , radn_ext )
c
c               call  Diffuse_Radiation_fraction
c     :                 (
c     :                 g%radn,
c     :                 radn_ext,
c     :                 diff_radn_frac
c     :                 )

c              call RUE_Diffuse_Radiation_Modifier (
c     :                 diff_radn_frac,
c     :                 rue_diff_radn_modifier
c     :                 )

c     .          g%tiller_area_max,
c     .          p%tiller_curve,
c     .          p%tiller_tt_infl,
c     .          g%tiller_area_pot,

c               WRITE(1,FMT="(i6,2x,10f7.2,
c     :                       10f7.2,
c     :                       10f7.2,
c     :                       10f7.2
c     :                       )")
c     :                       g%day_of_year
c     :                      ,g%tiller_area_max(1:10)
c     :                      ,p%tiller_curve(1:10)
c     :                      ,p%tiller_tt_infl(1:10)
c     :                      ,g%tiller_area_pot(1:10)
c
c               WRITE(1,FMT="(i6,  3x,
c     :                       f6.2,3x,
c     :                       f6.2,3x,
c     :                       f6.2,3x,
c     :                       f6.2,3x,
c     :                       f6.2,3x,
c     :                       )")
c     :                  g%day_of_year
c     :                 ,daylength
c     :                 ,radn_ext
c     :                 ,g%radn
c     :                 ,c%RUE_Max*rue_diff_radn_modifier
c     :                 ,1.34

c               PRINT *, '-------------------------------------'
c               PRINT *, 'DayOfYear =', g%day_of_year
c               PRINT *, 'day_len   =', daylength
c               PRINT *, 'radn_ext  =', radn_ext
c               PRINT *, 'radn_act  =', g%radn
c               PRINT *, 'RUE_max   =', RUE_Max*0.48
c               PRINT *, 'RUE_act   =', 1.34

c            endif

            !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


            !Send changes of other variables to owner-modules
            call Set_Other_Variables ()

         else
            !crop not in, do nothing
         endif

      elseif (action.eq.ACTION_end_crop) then

         !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c        if (TestTrue)   close (1)
         !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

         !end crop - turn the stover into residue
         call End_Crop ()

!            !Zero all the globals, but not the contants and parameters
!            call Zero_Variables (.false.)

         !Set plant status to status_out and stage to plant_end subroutine
         if (g%plant_status.ne.status_out) then
             g%plant_status  = status_out
             g%current_stage = real (plant_end)
         end if

         elseif (action.eq.ACTION_kill_crop) then
            ! kill crop - died, but biomass remain in field
            call Kill_Crop (
     .          g%plant_status,
     .          g%dm_green,
     .          g%dm_senesced,
     .          g%dm_dead)
      else
         ! don't use message
         call Message_unused ()
      endif


      call pop_routine (my_name)
      return
      end subroutine


! ====================================================================
! This routine is the event handler for all events
! ====================================================================
      subroutine respondToEvent(fromID, eventID, variant)
      use CropModData
      Use CropModModule

      implicit none
      ml_external respondToEvent
!STDCALL(respondToEvent)

      integer, intent(in) :: fromID
      integer, intent(in) :: eventID
      integer, intent(in) :: variant
      type(HarvestType) harvest

      if (eventID .eq. id%sow) then
         !request and receive variables from owner-modules
         call Get_Other_Variables ()

         !start crop, read the sow information and do  more initialisations
         call Start_Crop (variant)

      else if (eventID .eq. id%harvest) then
         call unpack_Harvest(variant, harvest)
         ! harvest crop - report harvest information
         call Crop_Harvest (
     .          g%dm_green,
     .          g%dm_dead,
     .          c%grn_water_cont,
     .          g%grain_no,
     .          g%plants,
     .          g%dm_senesced,
     .          g%leaf_no,
     .          g%N_green,
     .          g%N_dead,
     .          g%N_senesced,
     .          g%flowering_date,
     .          g%maturity_date,
     .          g%flowering_das,
     .          g%maturity_das,
     .          g%lai_max,
     .          g%cswd_photo,
     .          g%days_tot,
     .          g%cswd_expansion,
     .          g%cnd_photo,
     .          g%cnd_grain_conc,
     .          c%stage_names,
     .          real(harvest%remove))
     
      else if (eventID .EQ. id%BiocharDecomposed) then
         call OnBiocharDecomposed(variant)
      endif

      return
      end subroutine respondToEvent

