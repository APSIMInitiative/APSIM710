module Registrations
   use DataTypes
   type IDsType
      sequence
      integer :: newpotentialgrowth
      integer :: new_canopy
      integer :: newcrop
      integer :: incorp_fom
      integer :: biomass_removed
      integer :: killing
      integer :: thinning
      integer :: cutting
      integer :: establishing
      integer :: create
      integer :: sysinit
      integer :: cut
      integer :: thin
      integer :: establish
      integer :: kill
      integer :: change_class
      integer :: tick
      integer :: newmet
      integer :: prepare
      integer :: process
      integer :: canopy_energy_balance
      integer :: canopy_water_balance

   end type IDsType

   contains

      subroutine doRegistrations(id)
         use Infrastructure
         type(IDsType) :: id
         integer dummy

         id%newpotentialgrowth = add_registration(eventReg, 'newpotentialgrowth', NewPotentialGrowthTypeDDML, '')
         id%new_canopy = add_registration(eventReg, 'new_canopy', NewCanopyTypeDDML, '')
         id%newcrop = add_registration(eventReg, 'newcrop', NewCropTypeDDML, '')
         id%incorp_fom = add_registration(eventReg, 'IncorpFOM', FOMLayerTypeDDML, '')
         id%biomass_removed = add_registration(eventReg, 'BiomassRemoved', BiomassRemovedTypeDDML, '')
         id%killing = add_registration(eventReg, 'killing', nullTypeDDML, '')
         id%thinning = add_registration(eventReg, 'thinning', nullTypeDDML, '')
         id%cutting = add_registration(eventReg, 'cutting', nullTypeDDML, '')
         id%establishing = add_registration(eventReg, 'establishing', nullTypeDDML, '')
         id%create = add_registration(respondToEventReg, 'create', nullTypeDDML, '')
         id%sysinit = add_registration(respondToEventReg, 'sysinit', nullTypeDDML, '')
         id%cut = add_registration(respondToEventReg, 'cut', ApsimVariantTypeDDML, '')
         id%thin = add_registration(respondToEventReg, 'thin', ApsimVariantTypeDDML, '')
         id%establish = add_registration(respondToEventReg, 'establish', ApsimVariantTypeDDML, '')
         id%kill = add_registration(respondToEventReg, 'kill', nullTypeDDML, '')
         id%change_class = add_registration(respondToEventReg, 'change_class', ApsimVariantTypeDDML, '')
         id%tick = add_registration(respondToEventReg, 'tick', TimeTypeDDML, '')
         id%newmet = add_registration(respondToEventReg, 'newmet', NewMetTypeDDML, '')
         id%prepare = add_registration(respondToEventReg, 'prepare', nullTypeDDML, '')
         id%process = add_registration(respondToEventReg, 'process', nullTypeDDML, '')
         id%canopy_energy_balance = add_registration(respondToEventReg, 'canopy_energy_balance', CanopyEnergyBalanceTypeDDML, '')
         id%canopy_water_balance = add_registration(respondToEventReg, 'canopy_water_balance',  CanopyWaterBalanceTypeDDML, '')
		 
    ! variables that are both gettable and settable
         dummy = add_reg(respondToGetSetReg, 'foliage_n_conc', floatTypeDDML, 'g/g', '')   
         dummy = add_reg(respondToGetSetReg, 'fd', floatTypeDDML, '0-1', '')
         dummy = add_reg(respondToGetSetReg, 'retranslocation_fract', floatTypeDDML, '0-1', '')
         dummy = add_reg(respondToGetSetReg, 'n_uptake_switch', stringTypeDDML, '', '')
		 
    ! variables we own and make available to other modules (gettable)
         dummy = add_reg(respondToGetReg, 'crop_type', stringTypeDDML, '', 'Crop type for looking up properties')   
         dummy = add_reg(respondToGetReg, 'plant_status', stringTypeDDML, '', '''in'', ''out'', ''dead'' etc')   
         dummy = add_reg(respondToGetReg, 'dlt_an_green', floatArrayTypeDDML, 'kg/ha', 'Change in an_green today due to N uptake from soil')   
         dummy = add_reg(respondToGetReg, 'dlr_bn_green', floatArrayTypeDDML, 'kg/ha', 'Change in bn_green today due to N uptake from soil')   
         dummy = add_reg(respondToGetReg, 'dlt_an_green_fix', floatArrayTypeDDML, 'kg/ha', '')   
         dummy = add_reg(respondToGetReg, 'dlt_bn_green_fix', floatArrayTypeDDML, 'kg/ha', '')   
         dummy = add_reg(respondToGetReg, 'dlt_root_n', floatTypeDDML, 'kg/ha', 'Change in root_n today due to uptake of N from soil')   
         dummy = add_reg(respondToGetReg, 'dlt_foliage_n', floatTypeDDML, 'kg/ha', 'Change in foliage_n today due to N uptake from soil')   
         dummy = add_reg(respondToGetReg, 'dlt_root_n_fix', floatTypeDDML, 'kg/ha', '')   
         dummy = add_reg(respondToGetReg, 'dlt_foliage_n_fix', floatTypeDDML, 'kg/ha', '')   
         dummy = add_reg(respondToGetReg, 'dlt_foliage_mess_sen', floatTypeDDML, 'kg/ha', 'Change in foliage_mass_sen today due to senescence of live foliage')   
         dummy = add_reg(respondToGetReg, 'foliage_mass_sen', floatTypeDDML, 'kg/ha', 'Mass of senesced foliage')   
         dummy = add_reg(respondToGetReg, 'dlt_foliage_mass_detached', floatTypeDDML, 'kg/ha', '')   
         dummy = add_reg(respondToGetReg, 'rue_actual', floatTypeDDML, 'g/MJ', 'Radiation use efficiency = RUE * Frgr')   
         dummy = add_reg(respondToGetReg, 'sla_senescing', floatTypeDDML, 'mm/g', 'Specific leaf area of the senescing leaves')   
         dummy = add_reg(respondToGetReg, 'dlt_lai_sen', floatTypeDDML, 'm^2/m^2', 'Change in lai_sen today due to senescence of live foliage')   
         dummy = add_reg(respondToGetReg, 'dlt_lai_sen_age', floatTypeDDML, 'm^2/m^2', 'Change in lai_sen today due to age driven senescence of live foliage')   
         dummy = add_reg(respondToGetReg, 'dlt_lai_sen_frost', floatTypeDDML, 'm^2/m^2', 'Change in lai_sen today due to frosting of live foliage')   
         dummy = add_reg(respondToGetReg, 'dlt_lai_sen_light', floatTypeDDML, 'm^2/m^2', 'Change in lai_sen today due to shading of live foliage')   
         dummy = add_reg(respondToGetReg, 'dlt_root_mass_sen', floatTypeDDML, 'kg/ha', 'Change in root_mass_sen today due to senescence of live fine roots')   
         dummy = add_reg(respondToGetReg, 'dlt_root_n_sen', floatTypeDDML, 'kg/ha', 'Change in root_n_sen today due to senescence of live fine roots')   
         dummy = add_reg(respondToGetReg, 'plants', floatTypeDDML, '/ha', 'Number of plants per ha')   
         dummy = add_reg(respondToGetReg, 'age', floatTypeDDML, 'y', 'Age of the plants')   
         dummy = add_reg(respondToGetReg, 'sw_demand', floatTypeDDML, 'mm', 'Soil water demand')   
         dummy = add_reg(respondToGetReg, 'sw_supply', floatTypeDDML, 'mm', 'Soil water supply')   
         dummy = add_reg(respondToGetReg, 'cover', floatTypeDDML, '0-1', 'Fractional ground cover from green material')   
         dummy = add_reg(respondToGetReg, 'lai', floatTypeDDML, 'm^2/m^2', 'Leaf area index')   
         dummy = add_reg(respondToGetReg, 'slai', floatTypeDDML, 'm^2/m^2', 'Senesced leaf area index')   
         dummy = add_reg(respondToGetReg, 'foliage_mass', floatTypeDDML, 'kg/ha', 'Mass of foliage')   
         dummy = add_reg(respondToGetReg, 'dlt_foliage_mass', floatTypeDDML, 'kg/ha', 'Change in foliage_mass today due to photosynthesis')   
         dummy = add_reg(respondToGetReg, 'foliage_n', floatTypeDDML, 'kg/ha', 'N in the foliage')   
         dummy = add_reg(respondToGetReg, 'foliage_n_sen', floatTypeDDML, 'kg/ha', 'N in senesced foliage')   
         dummy = add_reg(respondToGetReg, 'ep', floatTypeDDML, 'mm', 'Actual water uptake summed across all soil layers')   
         dummy = add_reg(respondToGetReg, 'sw_uptake', floatArrayTypeDDML, 'mm', 'Water uptake for each soil layer')   
         dummy = add_reg(respondToGetReg, 'cum_ep', floatTypeDDML, 'mm', '')   
         dummy = add_reg(respondToGetReg, 'cover_green', floatTypeDDML, '0-1', 'Fractional ground cover from green material')   
         dummy = add_reg(respondToGetReg, 'cover_tot', floatTypeDDML, '0-1', 'Fractional ground cover from all material')   
         dummy = add_reg(respondToGetReg, 'rlv_growth', floatArrayTypeDDML, 'cm/cm^3', 'Same as rlv but with different units')   
         dummy = add_reg(respondToGetReg, 'rlv', floatArrayTypeDDML, 'mm/mm^3', 'Root length density corrected for aeration stress')   
         dummy = add_reg(respondToGetReg, 'rld', floatArrayTypeDDML, 'mm/mm^3', 'Root length density')   
         dummy = add_reg(respondToGetReg, 'root_length', floatArrayTypeDDML, 'mm/mm^2', 'Root length for each layer (area basis)')   
         dummy = add_reg(respondToGetReg, 'root_depth', floatTypeDDML, 'mm', 'Depth of the root system')   
         dummy = add_reg(respondToGetReg, 'root_mass', floatTypeDDML, 'kg/ha', 'Mass of live fine roots')   
         dummy = add_reg(respondToGetReg, 'dlt_root_mass', floatTypeDDML, 'kg/ha', 'Change in root_mass today due to photosynthesis')   
         dummy = add_reg(respondToGetReg, 'root_n', floatTypeDDML, 'kg/ha', 'N in the live fine root system')   
         dummy = add_reg(respondToGetReg, 'dlt_dm', floatTypeDDML, 'kg/ha', 'Change in total dry matter today')   
         dummy = add_reg(respondToGetReg, 'ft', floatTypeDDML, '0-1', 'Stress factor for temperature')   
         dummy = add_reg(respondToGetReg, 'ftcanopy', floatTypeDDML, '0-1', '')   
         dummy = add_reg(respondToGetReg, 'fwcanopy', floatTypeDDML, '0-1', '')   
         dummy = add_reg(respondToGetReg, 'ffasw', floatTypeDDML, '0-1', 'Stress factor for fasw - used to capture increased partitioning to roots in dry conditions')   
         dummy = add_reg(respondToGetReg, 'fdl', floatTypeDDML, '0-1', 'Stress factor for daylength - used to capture increased partitioning to roots prior to winter')   
         dummy = add_reg(respondToGetReg, 'fw', floatTypeDDML, '0-1', 'Stress factor for water supply (= supply/demand)')   
         dummy = add_reg(respondToGetReg, 'cum_stress', floatTypeDDML, 'd', '')   
         dummy = add_reg(respondToGetReg, 'fvpd', floatTypeDDML, '0-1', 'Stress factor for vapour pressure deficit')   
         dummy = add_reg(respondToGetReg, 'fn', floatTypeDDML, '0-1', 'Stress factor for nitrogen')   
         dummy = add_reg(respondToGetReg, 'ff', floatTypeDDML, '0-1', 'Stress factor for frost')   
         dummy = add_reg(respondToGetReg, 'fage', floatTypeDDML, '0-1', 'Stress factor for age - generic factor used to capture loss of productivity as plant stands mature')   
         dummy = add_reg(respondToGetReg, 'frgr', floatTypeDDML, '0-1', 'Relative growth rate factor for photosynthesis = min(Ft, Fn, Fvpd, Fage)')   
         dummy = add_reg(respondToGetReg, 'fasw', floatTypeDDML, '0-1', 'Fraction of plant available soil water')   
         dummy = add_reg(respondToGetReg, 'biomass', floatTypeDDML, 'kg/ha', 'Total above ground dry matter')   
         dummy = add_reg(respondToGetReg, 'dlt_foliage_mass_detached', floatTypeDDML, 'kg/ha', 'Change in foliage_mass_detached today from senesced foliage')   
         dummy = add_reg(respondToGetReg, 'dlt_foliage_n_detached', floatTypeDDML, 'kg/ha', 'Change in foliage_n today from senesced foliage')   
         dummy = add_reg(respondToGetReg, 'adm_green', floatTypeDDML, 'kg/ha', 'Green dry matter in each of the above ground part of live plants')   
         dummy = add_reg(respondToGetReg, 'dlt_adm_total', floatTypeDDML, 'kg/ha', '')   
         dummy = add_reg(respondToGetReg, 'dlt_bdm_total', floatTypeDDML, 'kg/ha', '')   
         dummy = add_reg(respondToGetReg, 'dlt_adm_green', floatArrayTypeDDML, 'kg/ha', 'Change in adm_green today due to photosynthesis')   
         dummy = add_reg(respondToGetReg, 'an_green', floatArrayTypeDDML, 'kg/ha', 'N in each of the green above ground parts of live plants')   
         dummy = add_reg(respondToGetReg, 'an_sen', floatArrayTypeDDML, 'kg/ha', 'N in each of the senesced above ground parts of live plants')   
         dummy = add_reg(respondToGetReg, 'an_dead', floatArrayTypeDDML, 'kg/ha', 'N in each of the above ground parts of dead plants')   
         dummy = add_reg(respondToGetReg, 'bn_green', floatArrayTypeDDML, 'kg/ha', 'N in each of the green below ground parts of live plants')   
         dummy = add_reg(respondToGetReg, 'bn_sen', floatArrayTypeDDML, 'kg/ha', 'N in each of the senesced below ground parts of live plants')   
         dummy = add_reg(respondToGetReg, 'bn_dead', floatArrayTypeDDML, 'kg/ha', 'N in each of the below ground parts of dead plants')   
         dummy = add_reg(respondToGetReg, 'total_n', floatTypeDDML, 'kg/ha', 'Total N in the plants, above and below ground')   
         dummy = add_reg(respondToGetReg, 'biomass_n', floatTypeDDML, 'g/m^2', '')   
         dummy = add_reg(respondToGetReg, 'an_total', floatTypeDDML, 'kg/ha', 'Total N in the plants above ground parts')   
         dummy = add_reg(respondToGetReg, 'bn_total', floatTypeDDML, 'kg/ha', 'Total N in the plants above ground parts')   
         dummy = add_reg(respondToGetReg, 'dlt_n_uptake_total', floatTypeDDML, 'kg/ha', '')   
         dummy = add_reg(respondToGetReg, 'dlt_n_fix_total', floatTypeDDML, 'kg/ha', '')   
         dummy = add_reg(respondToGetReg, 'dlt_an_uptake_total', floatTypeDDML, 'kg/ha', '')   
         dummy = add_reg(respondToGetReg, 'dlt_bn_uptake_total', floatTypeDDML, 'kg/ha', '')   
         dummy = add_reg(respondToGetReg, 'adm_sen', floatArrayTypeDDML, 'kg/ha', 'Senesced dry matter in each of the above ground part of live plants')   
         dummy = add_reg(respondToGetReg, 'adm_dead', floatArrayTypeDDML, 'kg/ha', 'Dry matter in each of the above ground part of dead plants')   
         dummy = add_reg(respondToGetReg, 'bdm_green', floatArrayTypeDDML, 'kg/ha', 'Green matter in each of the below ground parts of live plants')   
         dummy = add_reg(respondToGetReg, 'dlt_bdm_green', floatArrayTypeDDML, 'kg/ha', '')   
         dummy = add_reg(respondToGetReg, 'bn_green', floatArrayTypeDDML, 'kg/ha', '')   
         dummy = add_reg(respondToGetReg, 'bdm_sen', floatArrayTypeDDML, 'kg/ha', 'Senesced matter in each of the below ground parts of live plants')   
         dummy = add_reg(respondToGetReg, 'bdm_dead', floatArrayTypeDDML, 'kg/ha', 'Dry matter in each of the below ground parts of dead plants')   
         dummy = add_reg(respondToGetReg, 'height', floatTypeDDML, 'mm', 'Height of the plants ')   
         dummy = add_reg(respondToGetReg, 'no3_demand', floatTypeDDML, 'kg/ha', 'Nitrate Nitrogen demand')   
         dummy = add_reg(respondToGetReg, 'n_demand', floatTypeDDML, 'kg/ha', 'Nitrogen demand')   
         dummy = add_reg(respondToGetReg, 'dlt_no3', floatArrayTypeDDML, 'kg/ha', 'Change in no3 N today (i.e. uptake of NO3 N from each layer by the plant)')   
         dummy = add_reg(respondToGetReg, 'dm_green', floatTypeDDML, 'g/m^2', '')   
         dummy = add_reg(respondToGetReg, 'dm_senesced', floatTypeDDML, 'g/m^2', '')   
         dummy = add_reg(respondToGetReg, 'dm_dead', floatTypeDDML, 'g/m^2', '')   
         dummy = add_reg(respondToGetReg, 'dlt_dm_green', floatTypeDDML, 'g/m^2', '')   
         dummy = add_reg(respondToGetReg, 'n_green', floatTypeDDML, 'g/m^2', '')   
         dummy = add_reg(respondToGetReg, 'n_senesced', floatTypeDDML, 'g/m^2', '')   
         dummy = add_reg(respondToGetReg, 'n_dead', floatTypeDDML, 'g/m^2', '')   
      end subroutine
end module Registrations

