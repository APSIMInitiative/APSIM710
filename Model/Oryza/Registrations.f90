module Registrations
   use DataTypes
   type IDsType
      sequence
      integer :: biomass_removed
      integer :: incorp_fom
      integer :: sowing
      integer :: harvesting
      integer :: create
      integer :: sysinit
      integer :: sow
      integer :: end_crop
      integer :: kill_crop
      integer :: end_run
      integer :: tick
      integer :: newmet
      integer :: new_profile
      integer :: prepare
      integer :: process

   end type IDsType

   contains

      subroutine doRegistrations(id)
         use Infrastructure
         type(IDsType) :: id
         integer dummy

         id%biomass_removed = add_registration(eventReg, 'BiomassRemoved', BiomassRemovedTypeDDML, '')
         id%incorp_fom = add_registration(eventReg, 'incorpfom', FomLayerTypeDDML, '')
         id%sowing = add_registration(eventReg, 'sowing', nullTypeDDML, '')
         id%harvesting = add_registration(eventReg, 'harvesting', nullTypeDDML, '')
         id%create = add_registration(respondToEventReg, 'create', nullTypeDDML, '')
         id%sysinit = add_registration(respondToEventReg, 'sysinit', nullTypeDDML, '')
         id%sow = add_registration(respondToEventReg, 'sow', sowTypeDDML, '')
         id%end_crop = add_registration(respondToEventReg, 'end_crop', nullTypeDDML, '')
         id%kill_crop = add_registration(respondToEventReg, 'kill_crop', nullTypeDDML, '')
         id%end_run = add_registration(respondToEventReg, 'end_run', nullTypeDDML, '')
         id%tick = add_registration(respondToEventReg, 'tick', nullTypeDDML, '')
         id%newmet = add_registration(respondToEventReg, 'newmet', NewMetTypeDDML, '')
         id%new_profile = add_registration(respondToEventReg, 'new_profile', NewProfileTypeDDML, '')
         id%prepare = add_registration(respondToEventReg, 'prepare', nullTypeDDML, '')
         id%process = add_registration(respondToEventReg, 'process', nullTypeDDML, '')

    ! variables we own and make available to other modules.
         dummy = add_reg(respondToGetReg, 'plant_status', stringTypeDDML, '', 'status of crop')   
         dummy = add_reg(respondToGetReg, 'crop_type', stringTypeDDML, '', 'low-land rice or high-land rice')   
         dummy = add_reg(respondToGetReg, 'lrstrs', floatTypeDDML, '', 'drought stress factor causing leaf rolling')   
         dummy = add_reg(respondToGetReg, 'ldstrs', floatTypeDDML, '', 'drought stress factor accelerating leaf death')   
         dummy = add_reg(respondToGetReg, 'lestrs', floatTypeDDML, '', 'drought stress reducing leaf expansion')   
         dummy = add_reg(respondToGetReg, 'pcew', floatTypeDDML, '', 'drought stress reduction in potential transpiration rate')   
         dummy = add_reg(respondToGetReg, 'nflv', floatTypeDDML, '', 'nitrogen fraction in leaves on leaf area basis')   
         dummy = add_reg(respondToGetReg, 'fnlv', floatTypeDDML, '', 'nitrogen fraction in leaves on weight basis')   
         dummy = add_reg(respondToGetReg, 'nacr', floatTypeDDML, 'kg/ha/d', 'actual nitrogen uptake rate by crop')   
         dummy = add_reg(respondToGetReg, 'tnsoil', floatTypeDDML, 'kg/ha', 'amount of N available for uptake from soil')   
         dummy = add_reg(respondToGetReg, 'wso', floatTypeDDML, 'kg/ha', 'dry weight of storage organs')   
         dummy = add_reg(respondToGetReg, 'wst', floatTypeDDML, 'kg/ha', 'dry weight of stems')   
         dummy = add_reg(respondToGetReg, 'wrt', floatTypeDDML, 'kg/ha', 'dry weight of roots')   
         dummy = add_reg(respondToGetReg, 'wrr', floatTypeDDML, 'kg/ha', 'dry weight of rough rice')   
         dummy = add_reg(respondToGetReg, 'wlv', floatTypeDDML, 'kg/ha', 'dry weight of leaves')   
         dummy = add_reg(respondToGetReg, 'ancr', floatTypeDDML, 'kg/ha', 'amount of N in crop')   
         dummy = add_reg(respondToGetReg, 'anso', floatTypeDDML, 'kg/ha', 'amount of N in storage organs')   
         dummy = add_reg(respondToGetReg, 'wagt', floatTypeDDML, 'kg/ha', 'total aboveground dry matter')   
         dummy = add_reg(respondToGetReg, 'rnstrs', floatTypeDDML, '', 'reduction factor on relative leaf growth rate caused by N stress')   
         dummy = add_reg(respondToGetReg, 'anlv', floatTypeDDML, 'kg/ha', 'amount of N in leaves')   
         dummy = add_reg(respondToGetReg, 'wlvg', floatTypeDDML, 'kg/ha', 'dry weight of green leaves')   
         dummy = add_reg(respondToGetReg, 'wlvd', floatTypeDDML, 'kg/ha', 'dry weight of dead leaves')   
         dummy = add_reg(respondToGetReg, 'dtga', floatTypeDDML, 'kg/ha', 'daily gross CO2 assimilation of crop')   
         dummy = add_reg(respondToGetReg, 'dvs', floatTypeDDML, '', 'development stage of crop')   
         dummy = add_reg(respondToGetReg, 'eff', floatTypeDDML, '', 'initial light-use efficiency')   
         dummy = add_reg(respondToGetReg, 'ssga', floatTypeDDML, '', 'specific green stem area')   
         dummy = add_reg(respondToGetReg, 'dvr', floatTypeDDML, '', 'development rate of crop')   
         dummy = add_reg(respondToGetReg, 'hu', floatTypeDDML, '', 'heat units effective for phenological development')   
         dummy = add_reg(respondToGetReg, 'zrt', floatTypeDDML, 'm', 'root length or rooting depth')   
         dummy = add_reg(respondToGetReg, 'zll', floatTypeDDML, 'm', 'summed root depths of preceding soil layers')   
         dummy = add_reg(respondToGetReg, 'etrd', floatTypeDDML, 'mm/d', 'radiation-driven part of reference evapotranspiration rate')   
         dummy = add_reg(respondToGetReg, 'etae', floatTypeDDML, 'mm/d', 'dryness-driven part of reference evapotranspiration rate')   
         dummy = add_reg(respondToGetReg, 'trc', floatTypeDDML, 'mm/d', 'potential transpiration rate of crop with given LAI')   
         dummy = add_reg(respondToGetReg, 'trw', floatTypeDDML, 'mm/d', 'actual transpiration rate of crop with today''s LAI')   
         dummy = add_reg(respondToGetReg, 'gcr', floatTypeDDML, 'kg/ha/d', 'Gross growth rate of crop')   
         dummy = add_reg(respondToGetReg, 'rlai', floatTypeDDML, '', 'rice leaf area index')   
         dummy = add_reg(respondToGetReg, 'etd', floatTypeDDML, 'mm/d', 'reference evapotranspiration')   
         dummy = add_reg(respondToGetReg, 'cropsta', intTypeDDML, '', 'crop stage')   
         dummy = add_reg(respondToGetReg, 'dae', intTypeDDML, 'd', 'days after emergence')   
         dummy = add_reg(respondToGetReg, 'cover_green', floatTypeDDML, '0-1', 'live plant cover')   
         dummy = add_reg(respondToGetReg, 'cover_tot', floatTypeDDML, '0-1', 'total plant cover')   
         dummy = add_reg(respondToGetReg, 'height', floatTypeDDML, 'mm', 'plant height (a    dummy variable)')   
         dummy = add_reg(respondToGetReg, 'wcl', floatArrayTypeDDML, 'mm^3/mm^3', 'actual soil water content, per soil layer')   
         dummy = add_reg(respondToGetReg, 'gnsp', floatTypeDDML, '/ha/d', 'rate of increase in spikelet number')   
         dummy = add_reg(respondToGetReg, 'spgf', floatTypeDDML, '/kg', 'spikelet growth factor')   
         dummy = add_reg(respondToGetReg, 'coldtt', floatTypeDDML, '', 'accumulated cold degree days')   
         dummy = add_reg(respondToGetReg, 'sf1', floatTypeDDML, '', 'spikelet sterility factor because of low temperatures')   
         dummy = add_reg(respondToGetReg, 'sf2', floatTypeDDML, '', 'spikelet sterility factor because of high temperatures')   
         dummy = add_reg(respondToGetReg, 'spfert', floatTypeDDML, '', 'spikelet fertility factor')   
         dummy = add_reg(respondToGetReg, 'fso', floatTypeDDML, '0-1', 'fraction of shoot dry matter allocated to storage organs')   
         dummy = add_reg(respondToGetReg, 'gso', floatTypeDDML, 'kg/ha/d', 'growth rate of storage organs')   
         dummy = add_reg(respondToGetReg, 'nsp', floatTypeDDML, '/ha', 'number of spikelets')   
         dummy = add_reg(respondToGetReg, 'gngr', floatTypeDDML, '/ha/d', 'rate of increase in grain number')   
         dummy = add_reg(respondToGetReg, 'ggr', floatTypeDDML, 'kg/ha/d', 'rate of increase in grain weight')   
         dummy = add_reg(respondToGetReg, 'ngr', floatTypeDDML, '/ha', 'number of grains')   
         dummy = add_reg(respondToGetReg, 'wgrmx', floatTypeDDML, 'kg', 'maximum individual grain weight')   
         dummy = add_reg(respondToGetReg, 'sw_demand', floatTypeDDML, 'mm', 'potential transpiration rate')   
         dummy = add_reg(respondToGetReg, 'no3_demand', floatTypeDDML, 'mm', 'demand for nitrate (a    dummy variable)')   
         dummy = add_reg(respondToGetReg, 'rlv', floatArrayTypeDDML, 'mm/mm^3', 'root length per unit volume for each soil layer')   
         dummy = add_reg(respondToGetReg, 'root_weight_layer', floatArrayTypeDDML, 'kg/ha', 'root mass in each soil layer')   
         dummy = add_reg(respondToGetReg, 'root_length_layer', floatArrayTypeDDML, 'mm/mm^2', 'root length in each soil layer')   
         dummy = add_reg(respondToGetReg, 'rat_grazing_perc', floatTypeDDML, '%', 'rat-grazing proportion of crop')   
         dummy = add_reg(respondToGetReg, 'leaves_rat_grazed', floatTypeDDML, 'kg/ha/d', 'loss rate of leaf weight due to rat grazing')   
         dummy = add_reg(respondToGetReg, 'grains_rat_grazed', floatTypeDDML, 'kg/ha/d', 'grain weight grazed by rats on daily basis')   

      end subroutine
end module Registrations
