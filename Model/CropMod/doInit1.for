* ====================================================================
      subroutine doInit1()
* ====================================================================
      use CropModData
      Use infrastructure
      implicit none
	  ! Comment out the following line if building under gfortran
      ml_external doInit1        
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
      id%sowing = add_registration(eventReg, 'sowing',
     :                             nullTypeDDML, '')
      id%harvesting = add_registration(eventReg, 'harvesting',
     :                                 nullTypeDDML, '')
      id%create = add_registration(respondToEventReg, 'create',
     :                             nullTypeDDML, '')
      id%sysinit = add_registration(respondToEventReg, 'sysinit',
     :                              nullTypeDDML, '')
      id%sow = add_registration(respondToEventReg, 'sow',
     :                          nullTypeDDML, '')
      id%harvest = add_registration(respondToEventReg, 'harvest',
     :                              nullTypeDDML, '')
      id%kill_crop = add_registration(respondToEventReg, 'kill_crop',
     :                                nullTypeDDML, '')
      id%end_crop = add_registration(respondToEventReg, 'end_crop',
     :                               nullTypeDDML, '')
      id%prepare = add_registration(respondToEventReg, 'prepare',
     :                              nullTypeDDML, '')
      id%process = add_registration(respondToEventReg, 'process',
     :                              nullTypeDDML, '')

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
      dummy = add_registration_with_units(respondToGetReg,
     :                        'll', floatarrayTypeDDML, '%')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'll_dep', floatarrayTypeDDML, 'mm')
      dummy = add_registration_with_units(respondToGetReg,
     :                        'kl', floatarrayTypeDDML, '')
      dummy = add_registration_with_units(respondToGetReg,
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


      end subroutine doInit1
