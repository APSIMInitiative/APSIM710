module Registrations
   use DataTypes
   type IDsType
      sequence
      integer :: biomass_removed
      integer :: incorp_fom
      integer :: killing
      integer :: sowing
      integer :: harvesting
      integer :: sysinit
      integer :: create
      integer :: sow
      integer :: harvest
      integer :: end_crop
      integer :: kill_crop
      integer :: graze
      integer :: hill_up
      integer :: lodge
      integer :: tick
      integer :: prepare
      integer :: process
      integer :: dummy

   end type IDsType

   contains

      subroutine doRegistrations(id)
         use Infrastructure
         implicit none
         type(IDsType) :: id

         id%biomass_removed = add_registration(eventReg, 'BiomassRemoved', BiomassRemovedTypeDDML, '')
         id%incorp_fom = add_registration(eventReg, 'IncorpFOM', IncorpFomTypeDDML, '')
         id%killing = add_registration(eventReg, 'killing', nullTypeDDML, '')
         id%sowing = add_registration(eventReg, 'sowing', nullTypeDDML, '')
         id%harvesting = add_registration(eventReg, 'harvesting', nullTypeDDML, '')
         id%sysinit = add_registration(respondToEventReg, 'sysinit', nullTypeDDML, '')
         id%create = add_registration(respondToEventReg, 'create', nullTypeDDML, '')
         id%sow = add_registration(respondToEventReg, 'sow', sowTypeDDML, '')
         id%harvest = add_registration(respondToEventReg, 'harvest', nullTypeDDML, '')
         id%end_crop = add_registration(respondToEventReg, 'end_crop', nullTypeDDML, '')
         id%kill_crop = add_registration(respondToEventReg, 'kill_crop', nullTypeDDML, '')
         id%graze = add_registration(respondToEventReg, 'graze', nullTypeDDML, '')
         id%hill_up = add_registration(respondToEventReg, 'hill_up', nullTypeDDML, '')
         id%lodge = add_registration(respondToEventReg, 'lodge', nullTypeDDML, '')
         id%tick = add_registration(respondToEventReg, 'tick', nullTypeDDML, '')
         id%prepare = add_registration(respondToEventReg, 'prepare', nullTypeDDML, '')
         id%process = add_registration(respondToEventReg, 'process', nullTypeDDML, '')
         id%dummy = add_reg(respondToGetReg, 'crop_status', stringTypeDDML, ' ', 'Sugar Crop status (dead,alive,out)')
         id%dummy = add_reg(respondToGetReg, 'stage', floatTypeDDML, '', 'Sugar current phenological stage')
         id%dummy = add_reg(respondToGetReg, 'stage_code', floatTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'stagename', stringTypeDDML, '', 'Sugar Stage name')
         id%dummy = add_reg(respondToGetReg, 'croptype', stringTypeDDML, '', '')
         id%dummy = add_reg(respondToGetSetReg, 'plants', floatTypeDDML, '/m2', '')
         id%dummy = add_reg(respondToGetReg, 'ratoon_no', intTypeDDML, '', 'Sugar Ratoon number')
         id%dummy = add_reg(respondToGetReg, 'phase_tt', floatTypeDDML, 'oC', '')
         id%dummy = add_reg(respondToGetReg, 'tt_tot', floatTypeDDML, 'oC', '')
         id%dummy = add_reg(respondToGetReg, 'leaf_no', floatTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'node_no_dead', floatTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'leaf_area', floatTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'leaf_dm', floatTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'height', floatTypeDDML, 'mm', '')
         id%dummy = add_reg(respondToGetReg, 'root_depth', floatTypeDDML, 'mm', 'Sugar depth of roots')
         id%dummy = add_reg(respondToGetReg, 'cover_green', floatTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'radn_int', floatTypeDDML, 'mj/m2', '')
         id%dummy = add_reg(respondToGetReg, 'cover_tot', floatTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'lai_sum', floatTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'tlai', floatTypeDDML, '', 'Sugar total leaf area index (green and senesced)')
         id%dummy = add_reg(respondToGetReg, 'tla', floatTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'slai', floatTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'lai', floatTypeDDML, 'm^2/m^2', 'Sugar green leaf area index')
         id%dummy = add_reg(respondToGetReg, 'rootgreenwt', floatTypeDDML, 'g/m^2', '')
         id%dummy = add_reg(respondToGetReg, 'leafgreenwt', floatTypeDDML, 'g/m^2', '')
         id%dummy = add_reg(respondToGetReg, 'sstem_wt', floatTypeDDML, 'g/m^2', 'Sugar structural stem weight')
         id%dummy = add_reg(respondToGetReg, 'canefw', floatTypeDDML, 't/ha', 'Sugar cane fresh weight')
         id%dummy = add_reg(respondToGetReg, 'ccs', floatTypeDDML, '%', '')
         id%dummy = add_reg(respondToGetReg, 'scmstf', floatTypeDDML, 'g/g', '')
         id%dummy = add_reg(respondToGetReg, 'scmst', floatTypeDDML, 'g/g', '')
         id%dummy = add_reg(respondToGetReg, 'sucrose_wt', floatTypeDDML, 'g/m^2', 'Sugar sucrose weight')
         id%dummy = add_reg(respondToGetReg, 'cabbage_wt', floatTypeDDML, 'g/m^2', 'Sugar cabbage weight')
         id%dummy = add_reg(respondToGetReg, 'cane_wt', floatTypeDDML, 'g/m^2', 'Sugar cane dry weight')
         id%dummy = add_reg(respondToGetReg, 'biomass', floatTypeDDML, 'g/m^2', 'Sugar total above-ground biomass (green + trash)')
         id%dummy = add_reg(respondToGetReg, 'green_biomass', floatTypeDDML, 'g/m^2', 'Sugar green biomass')
         id%dummy = add_reg(respondToGetReg, 'greenwt', floatTypeDDML, 'g/m^2', '')
         id%dummy = add_reg(respondToGetReg, 'senescedwt', floatTypeDDML, 'g/m^2', '')
         id%dummy = add_reg(respondToGetReg, 'dm_dead', floatTypeDDML, 'g/m^2', '')
         id%dummy = add_reg(respondToGetReg, 'dlt_dm', floatTypeDDML, 'g/m^2', 'Sugar daily biomass production')
         id%dummy = add_reg(respondToGetReg, 'partition_xs', floatTypeDDML, 'g/m^2', '')
         id%dummy = add_reg(respondToGetReg, 'dlt_dm_green', floatTypeDDML, 'g/m^2', '')
         id%dummy = add_reg(respondToGetReg, 'dlt_dm_detached', floatArrayTypeDDML, 'g/m^2', '')
         id%dummy = add_reg(respondToGetReg, 'n_critical', floatArrayTypeDDML, 'g/g', '')
         id%dummy = add_reg(respondToGetReg, 'n_minimum', floatArrayTypeDDML, 'g/g', '')
         id%dummy = add_reg(respondToGetReg, 'n_conc_leaf', floatTypeDDML, 'g/m^2', 'Sugar N concentration in green leaf')
         id%dummy = add_reg(respondToGetReg, 'n_conc_cab', floatTypeDDML, 'g/m^2', 'Sugar N concentration in cabbage')
         id%dummy = add_reg(respondToGetReg, 'n_conc_cane', floatTypeDDML, 'g/m^2', 'Sugar N concentration in cane')
         id%dummy = add_reg(respondToGetReg, 'n_leaf_crit', floatTypeDDML, 'g/m^2', '')
         id%dummy = add_reg(respondToGetReg, 'n_leaf_min', floatTypeDDML, 'g/m^2', '')
         id%dummy = add_reg(respondToGetReg, 'biomass_n', floatTypeDDML, 'g/m^2', 'Sugar total N in biomass (green + trash)')
         id%dummy = add_reg(respondToGetReg, 'plant_n_tot', floatTypeDDML, 'g/m^2', '')
         id%dummy = add_reg(respondToGetReg, 'green_biomass_n', floatTypeDDML, 'g/m^2', 'Sugar green biomass N')
         id%dummy = add_reg(respondToGetReg, 'n_green', floatArrayTypeDDML, 'g/m^2', 'Sugar green N for all parts')
         id%dummy = add_reg(respondToGetReg, 'greenn', floatTypeDDML, 'g/m^2', '')
         id%dummy = add_reg(respondToGetReg, 'senescedn', floatTypeDDML, 'g/m^2', '')
         id%dummy = add_reg(respondToGetReg, 'dlt_n_green', floatArrayTypeDDML, 'g/m^2', '')
         id%dummy = add_reg(respondToGetReg, 'swdef_pheno', floatTypeDDML, '', 'Sugar water stress factor on phenology')
         id%dummy = add_reg(respondToGetReg, 'swdef_photo', floatTypeDDML, '', 'Sugar water stress factor on photosynthesis')
         id%dummy = add_reg(respondToGetReg, 'swdef_expan', floatTypeDDML, '', 'Sugar water stress factor on cell expansion')
         id%dummy = add_reg(respondToGetReg, 'swdef_stalk', floatTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'nfact_photo', floatTypeDDML, '', 'Sugar Nitrogen stress factor on photosynthesis')
         id%dummy = add_reg(respondToGetSetReg, 'lodge_redn_photo', floatTypeDDML, '', '')
         id%dummy = add_reg(respondToGetSetReg, 'lodge_redn_sucrose', floatTypeDDML, '', '')
         id%dummy = add_reg(respondToGetSetReg, 'lodge_redn_green_leaf', floatTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'nfact_expan', floatTypeDDML, '', 'Sugar Nitrogen stress factor on cell expansion')
         id%dummy = add_reg(respondToGetReg, 'ep', floatTypeDDML, 'mm', 'Sugar daily crop water uptake')
         id%dummy = add_reg(respondToGetReg, 'sw_uptake', floatTypeDDML, 'mm', '')
         id%dummy = add_reg(respondToGetReg, 'cep', floatTypeDDML, 'mm', 'Sugar cumulative crop water uptake')
         id%dummy = add_reg(respondToGetReg, 'sw_demand', floatTypeDDML, 'mm', 'Sugar crop demand for water (potential uptake)')
         id%dummy = add_reg(respondToGetReg, 'sw_demand_te', floatTypeDDML, 'mm', '')
         id%dummy = add_reg(respondToGetReg, 'fasw', floatTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'esw_layr', floatArrayTypeDDML, 'mm', '')
         id%dummy = add_reg(respondToGetReg, 'cane_dmf', floatTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'oxdef_photo', floatTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'daysaftersowing', intTypeDDML, 'days', 'Sugar days after sowing')
         id%dummy = add_reg(respondToGetReg, 'n_uptake', floatTypeDDML, 'kg/ha', '')
         id%dummy = add_reg(respondToGetReg, 'no3_tot', floatTypeDDML, 'g/m^2', '')
         id%dummy = add_reg(respondToGetReg, 'n_demand', floatTypeDDML, 'g/m^2', 'Sugar nitrogen demand')
         id%dummy = add_reg(respondToGetReg, 'no3_demand', floatTypeDDML, 'g/m^2', 'Sugar nitrate demand')
         id%dummy = add_reg(respondToGetReg, 'n_supply', floatTypeDDML, 'g/m^2', '')
         id%dummy = add_reg(respondToGetReg, 'no3_uptake', floatArrayTypeDDML, 'g/m^2', '')
         id%dummy = add_reg(respondToGetReg, 'nh4_uptake', floatArrayTypeDDML, 'g/m^2', '')
         id%dummy = add_reg(respondToGetReg, 'no3_uptake_pot', floatArrayTypeDDML, 'g/m^2', '')
         id%dummy = add_reg(respondToGetReg, 'nh4_uptake_pot', floatArrayTypeDDML, 'g/m^2', '')
         id%dummy = add_reg(respondToGetReg, 'rlv', floatArrayTypeDDML, 'mm/mm3', 'Sugar root length density for each layer')
         id%dummy = add_reg(respondToGetReg, 'rlv_tot', floatArrayTypeDDML, 'mm/mm3', '')
         id%dummy = add_reg(respondToGetReg, 'll_dep', floatArrayTypeDDML, 'mm', 'Sugar lower limit')
         id%dummy = add_reg(respondToGetReg, 'dm_graze', floatTypeDDML, 'g/m^2', '')
         id%dummy = add_reg(respondToGetReg, 'n_graze', floatTypeDDML, 'g/m^2', '')
         id%dummy = add_reg(respondToGetReg, 'lai2', floatTypeDDML, 'g/m^2', '')
         id%dummy = add_reg(respondToGetReg, 'leaf_wt2', floatTypeDDML, 'g/m^2', '')
      end subroutine
end module Registrations

