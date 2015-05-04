module Registrations
   use DataTypes
   type IDsType
      sequence
      integer :: actualresiduedecompositioncalculated
      integer :: create
      integer :: sysinit
      integer :: prepare
      integer :: process
      integer :: tick
      integer :: newmet
      integer :: potentialresiduedecompositioncalculated
      integer :: biomassremoved

   end type IDsType

   contains

      subroutine doRegistrations(id)
         use Infrastructure
         type(IDsType) :: id
         integer dummy

         id%create = add_registration(respondToEventReg, 'create', nullTypeDDML, '')
         id%sysinit = add_registration(respondToEventReg, 'sysinit', nullTypeDDML, '')
         id%prepare = add_registration(respondToEventReg, 'prepare', nullTypeDDML, '')
         id%process = add_registration(respondToEventReg, 'process', nullTypeDDML, '')
         id%actualresiduedecompositioncalculated = add_registration(eventReg, 'actualresiduedecompositioncalculated', SurfaceOrganicMatterDecompTypeDDML, '')
         id%tick = add_registration(respondToEventReg, 'tick', TimeTypeDDML, '')
         id%newmet = add_registration(respondToEventReg, 'newmet', NewMetTypeDDML, '')
         id%potentialresiduedecompositioncalculated = add_registration(respondToEventReg, 'potentialresiduedecompositioncalculated', SurfaceOrganicMatterDecompTypeDDML, '')
         id%biomassremoved = add_registration(eventReg, 'BiomassRemoved', BiomassRemovedTypeDDML, '')
         
         ! variables we own and make available to other modules.
         dummy = add_reg(respondToGetReg, 'pond_active', stringTypeDDML, '', 'yes or no depending on whether a pond is present in simulation')   
         dummy = add_reg(respondToGetReg, 'consecutive_ponded_days', intTypeDDML, 'days', 'consecutive ponded days (ie pond_depth > 0)')   
         dummy = add_reg(respondToGetReg, 'pond_no3', floatTypeDDML, 'kg/ha', 'mineral N as nitrate in pond')   
         dummy = add_reg(respondToGetReg, 'pond_nh4', floatTypeDDML, 'kg/ha', 'mineral N as ammonium in pond')   
         dummy = add_reg(respondToGetReg, 'pond_urea', floatTypeDDML, 'kg/ha', 'urea in pond')   
         dummy = add_reg(respondToGetReg, 'pond_no3_conc', floatTypeDDML, 'ppm', 'concentration of mineral N as nitrate in pond')   
         dummy = add_reg(respondToGetReg, 'pond_nh4_conc', floatTypeDDML, 'ppm', 'concentration of mineral N as ammonium in pond')   
         dummy = add_reg(respondToGetReg, 'pab_mass', floatTypeDDML, 'kg/ha', 'mass of algae in pond (capped at 500kg/ha)')   
         dummy = add_reg(respondToGetReg, 'algal_turnover', floatTypeDDML, 'kg/ha', 'mass of algae turned over each day and added into surfaceom pool')   
         dummy = add_reg(respondToGetReg, 'pond_hum_c', floatTypeDDML, 'kg/ha', 'humic C in pond')   
         dummy = add_reg(respondToGetReg, 'pond_biom_c', floatTypeDDML, 'kg/ha', 'biom C in pond')   
         dummy = add_reg(respondToGetReg, 'ftmax', floatTypeDDML, 'C', 'maximum pond temperature today')   
         dummy = add_reg(respondToGetReg, 'ftmin', floatTypeDDML, 'C', 'minimum pond temperature today')   
         dummy = add_reg(respondToGetReg, 'ftmean', floatTypeDDML, 'C', 'mean pond temperature today')   
         dummy = add_reg(respondToGetReg, 'ftmax_yest', floatTypeDDML, 'C', 'maximum pond temperature yesterday')   
         dummy = add_reg(respondToGetReg, 'ftmin_yest', floatTypeDDML, 'C', 'minimum pond temperature yesterday')   
         dummy = add_reg(respondToGetReg, 'ftmean_yest', floatTypeDDML, 'C', 'mean pond temperature yesterday')   
         dummy = add_reg(respondToGetReg, 'pond_depth', floatTypeDDML, 'mm', 'depth of ponded layer today')   
         dummy = add_reg(respondToGetReg, 'pond_evap', floatTypeDDML, 'mm', 'evaporation from pond today')   
         dummy = add_reg(respondToGetReg, 'rlai_from_pondmodule', floatTypeDDML, 'm2/m2', 'rice leaf area index')   
         dummy = add_reg(respondToGetReg, 'rep', floatTypeDDML, 'mm', 'rice evapotranspiration')   
         dummy = add_reg(respondToGetReg, 'oc1', floatTypeDDML, '%', 'organic carbon in soil layer 1')   
         dummy = add_reg(respondToGetReg, 'dlayer1', floatTypeDDML, 'mm', 'depth of soil layer 1')   
         dummy = add_reg(respondToGetReg, 'bd1', floatTypeDDML, 'g/cm3', 'bulk density of soil layer 1')   
         dummy = add_reg(respondToGetReg, 'algact', floatTypeDDML, '0-1', 'pond algal activity')   
         dummy = add_reg(respondToGetReg, 'fni', floatTypeDDML, '0-1', 'nitrogen factor affecting algal growth')   
         dummy = add_reg(respondToGetReg, 'fpi', floatTypeDDML, '0-1', 'phosphorus factor affecting algal growth')   
         dummy = add_reg(respondToGetReg, 'fti', floatTypeDDML, '0-1', 'temperature factor affecting algal growth')   
         dummy = add_reg(respondToGetReg, 'ali', floatTypeDDML, '0-1', 'light factor affefcting algal growth')   
         dummy = add_reg(respondToGetReg, 'maxrate_pab', floatTypeDDML, 'kg/ha/day', 'maximum rate at which Phostosynthetic Algal Biomass (pab)can accumulate in the floodwater')   
         dummy = add_reg(respondToGetReg, 'yalga', floatTypeDDML, '0-1', '??')   
         dummy = add_reg(respondToGetReg, 'algfix', floatTypeDDML, 'kg/ha', '??')   
         dummy = add_reg(respondToGetReg, 'tmpfon', floatTypeDDML, 'kg/ha', 'pond organic nitrogen')   
         dummy = add_reg(respondToGetReg, 'totuh', floatTypeDDML, 'kg/ha', 'cumulative urea hydrolised over the 12 daily timesteps')   
         dummy = add_reg(respondToGetReg, 'fph', floatTypeDDML, '', 'floodwater ph for timestep')   
         dummy = add_reg(respondToGetReg, 'pot_hydrolysis', floatTypeDDML, 'kg/ha/d', 'maximum potential rate of pond urea hydrolysis, soil OC limited ')   
         dummy = add_reg(respondToGetReg, 'totaml', floatTypeDDML, 'kgha', 'total ammonia loss from pond')   
         dummy = add_reg(respondToGetReg, 'amloss', floatTypeDDML, 'kg/ha', 'daily ammonia loss from pond')   
         dummy = add_reg(respondToGetReg, 'dlt_res_nh4_min', floatTypeDDML, 'kg/ha', 'Net Residue NH4 mineralisation')   
         dummy = add_reg(respondToGetReg, 'dlt_res_no3_min', floatTypeDDML, 'kg/ha', 'Net Residue NO3 mineralisation')   
         dummy = add_reg(respondToGetReg, 'kill', intArrayTypeDDML, '', 'algal kill (0 or 1)')   
         dummy = add_reg(respondToGetReg, 'dlt_res_C_decomp', floatArrayTypeDDML, 'kg/ha', 'residue C decomposition')   
         dummy = add_reg(respondToGetReg, 'dlt_res_N_decomp', floatArrayTypeDDML, 'kg/ha', 'residue N decomposition ')   
         dummy = add_reg(respondToGetReg, 'dlt_res_C_atm', floatArrayTypeDDML, 'kg/ha', 'carbon from residues lost to atmosphere')   
         dummy = add_reg(respondToGetReg, 'dlt_res_C_biom', floatArrayTypeDDML, 'kg/ha', 'carbon from residues to biomass')   
         dummy = add_reg(respondToGetReg, 'dlt_res_C_hum', floatArrayTypeDDML, 'kg/ha', 'carbon from residues to humic')   
         dummy = add_reg(respondToGetReg, 'infiltration_pond_calc', floatTypeDDML, 'mm', 'infiltration from pond into soil')   
         dummy = add_reg(respondToGetReg, 'no3_infiltrated', floatArrayTypeDDML, 'kg/ha', '??')   
         dummy = add_reg(respondToGetReg, 'nh4_infiltrated', floatArrayTypeDDML, 'kg/ha', '??')   
         dummy = add_reg(respondToGetReg, 'urea_infiltrated', floatArrayTypeDDML, 'kg/ha', '??')   

      end subroutine
end module Registrations

