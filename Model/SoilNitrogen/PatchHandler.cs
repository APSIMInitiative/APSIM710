using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ModelFramework;
using CSGeneral;
using System.Xml;

/// <summary>
/// This partial class contains the various methods to handle patches
/// </summary>

    public partial class SoilNitrogen
    {

        private void InitialisePatch(int k)
        {
            // + Purpose
            //    set all variables within a single patch to defaul/initial values

            #region Parameters added by RCichota

            // whether to use new functions to compute temp and moist factors
            Patch[k].useNewSTFFunction = useNewSTFFunction;
            Patch[k].useNewSTFFunction = useNewSWFFunction;
            //Patch[k].useNewProcesses = useNewProcesses;
            Patch[k].useNewProcesses = false;
            Patch[k].useSingleMinerFactors = useSingleMinerFactors;

            // whether calculate one set of mineralisation factors (stf and swf) or one for each pool
            Patch[k].useFactorsBySOMpool = useFactorsBySOMpool;
            Patch[k].useFactorsByFOMpool = useFactorsByFOMpool;

            #endregion

            #region Parameters for alternative N2O emission processes FLi
            //passing params from SoilNitrogen to Patch

            //Console.Out.WriteLine("#### 1Passing n2o_approch = " + n2o_approach);
            Patch[k].SoilTextureID = new double[dlayer.Length];
            double value = 2.0;  // default texture is medium
            for (int layer = 0; layer < dlayer.Length; layer++)
            {
                if (texture != null)
                    value = texture[layer];
                Patch[k].SoilTextureID[layer] = value;
            }

            Patch[k].n2o_approach = n2o_approach;                          // Approches used for nitri/denitri process for n2o emission 

            Patch[k].wnmm_n_alpha = wnmm_n_alpha;
            Patch[k].wnmm_dn_alpha = wnmm_dn_alpha;

            Patch[k].nemis_dn_km = nemis_dn_km;
            Patch[k].nemis_dn_pot = nemis_dn_pot;

            Patch[k].cent_n_soilt_ave = cent_n_soilt_ave;
            Patch[k].cent_n_maxt_ave = cent_n_maxt_ave;
            Patch[k].cent_n_wfps_ave = cent_n_wfps_ave;
            Patch[k].cent_n_max_rate = cent_n_max_rate;
            #endregion

            #region Values needed for initalisation only

            #region General setting parameters

            // soil parameterisation to use, used to determine which node of xml file will be used to read [Param]
            Patch[k].SoilParamSet = SoilParamSet;

            // switch indicating whether soil profile reduction is allowed (from erosion)
            Patch[k].AllowProfileReduction = AllowProfileReduction;

            // marker for whether organic solute are to be simulated (always false as it is not implemented)
            Patch[k].useOrganicSolutes = useOrganicSolutes;

            for (int layer = 0; layer < dlayer.Length; layer++)
            {
                // minimum allowable Urea content (ppm)
                Patch[k].urea_min[layer] = urea_min[layer];

                // minimum allowable NH4 content (ppm)
                Patch[k].nh4_min[layer] = nh4_min[layer];

                // minimum allowable NO3 content (ppm)
                Patch[k].no3_min[layer] = no3_min[layer];
            }

            // minimum allowable FOM content (kg/ha)
            Patch[k].fom_min = fom_min;

            // conversion from OC to OM
            Patch[k].oc2om_factor = oc2om_factor;

            // weight fraction of C in carbohydrates
            Patch[k].c_in_fom = c_in_fom;

            // value to evaluate precision
            Patch[k].epsilon = epsilon;

            Patch[k].WarningThreshold = -WarningThreshold;
            Patch[k].FatalThreshold = -FatalThreshold;

            #endregion

            #region Parameters for handling soil loss process

            // enrichment equation coefficient a
            Patch[k].enr_a_coeff = enr_a_coeff;

            // enrichment equation coefficient b
            Patch[k].enr_b_coeff = enr_b_coeff;

            #endregion

            #region Parameters for setting soil organic matter

            // the C:N ratio of soil humus
            Patch[k].hum_cn = hum_cn;

            // C:N ratio of microbial biomass
            Patch[k].biom_cn = biom_cn;

            for (int layer = 0; layer < dlayer.Length; layer++)
            {
                // the initial soil OC
                Patch[k].OC_reset[layer] = OC_reset[layer];

                // initial ratio of biomass-C to mineralizable humic-C (0-1)
                Patch[k].fbiom[layer] = fbiom[layer];

                // initial proportion of total soil C that is not subject to mineralization (0-1)
                Patch[k].finert[layer] = finert[layer];
            }

            #endregion

            #region Parameters for setting fresh organic matter (FOM)

            // initial weight of fom in the soil (kgDM/ha)
            Patch[k].fom_ini_wt = fom_ini_wt;

            // initial depth over which fom is distributed within the soil profile (mm)
            Patch[k].fom_ini_depth = fom_ini_depth;


            // initial C:N ratio FOM pools
            Patch[k].fomPools_cn = new double[3];
            for (int fract = 0; fract < 3; fract++)
                Patch[k].fomPools_cn[fract] = fomPools_cn[fract];

            // the various fom types
            int p = fom_types.Length;
            Patch[k].fom_types = new string[p];
            Patch[k].fract_carb = new double[p];
            Patch[k].fract_cell = new double[p];
            Patch[k].fract_lign = new double[p];
            for (int type = 0; type < fom_types.Length; type++)
            {
                // the fom type name
                Patch[k].fom_types[type] = fom_types[type];

                // fraction of carbohydrate in FOM (0-1)
                Patch[k].fract_carb[type] = fract_carb[type];

                // fraction of cellulose in FOM (0-1)
                Patch[k].fract_cell[type] = fract_cell[type];

                // fraction of lignine in FOM (0-1)
                Patch[k].fract_lign[type] = fract_lign[type];
            }

            #endregion

            #region Parameters for FOM and SurfOM mineralisation process

            #region Surface OM

            // fraction of residue C mineralised retained in system (0-1)
            Patch[k].ef_res = ef_res;

            // fraction of retained residue C transferred to biomass (0-1)
            Patch[k].fr_res_biom = fr_res_biom;

            // depth from which mineral N can be immobilised by decomposing residues (mm)
            Patch[k].min_depth = min_depth;

            #endregion

            #region Fresh OM

            #region Old parameters

            Patch[k].rd_carb = new double[2];
            Patch[k].rd_cell = new double[2];
            Patch[k].rd_lign = new double[2];
            for (int index = 0; index < 2; index++)
            {
                // maximum rate constants for decomposition of FOM pools [carbohydrate component] (0-1)
                Patch[k].rd_carb[index] = rd_carb[index];

                // maximum rate constants for decomposition of FOM pools [cellulose component] (0-1)
                Patch[k].rd_cell[index] = rd_cell[index];            // maximum rate constants for decomposition of FOM pools [cellulose component] (0-1)

                // maximum rate constants for decomposition of FOM pools [lignine component] (0-1)
                Patch[k].rd_lign[index] = rd_lign[index];
            }

            // fraction of FOM C mineralised retained in system (0-1)   
            Patch[k].ef_fom = ef_fom;

            // fraction of retained FOM C transferred to biomass (0-1)
            Patch[k].fr_fom_biom = fr_fom_biom;

            // coeff. to determine the magnitude of C:N effects on decomposition of FOM ()
            Patch[k].cnrf_coeff = cnrf_coeff;

            // C:N above which decomposition rate of FOM declines ()
            Patch[k].cnrf_optcn = cnrf_optcn;

            #endregion

            #region New parameters

            // parameters for temperature factor for FOM mineralisation
            Patch[k].stfData_MinerFOM.TempOptimum = TempFactor_minerFOM.TempOptimum;
            Patch[k].stfData_MinerFOM.FactorAtZero = TempFactor_minerFOM.FactorAtZero;
            Patch[k].stfData_MinerFOM.CurveExponent = TempFactor_minerFOM.CurveExponent;

            // parameters for soil moisture factor for FOM mineralisation
            Patch[k].swfData_MinerFOM.xVals = MoistFactor_minerFOM.xVals;
            Patch[k].swfData_MinerFOM.yVals = MoistFactor_minerFOM.yVals;

            // parameters for C:N factor for FOM mineralisation
            Patch[k].CNFactorMinerFOM_OptCN = CNFactorMinerFOM_OptCN;
            Patch[k].CNFactorMinerFOM_RateCN = CNFactorMinerFOM_RateCN;

            #endregion

            #endregion

            #endregion

            #region Parameters for SOM mineralisation/immobilisation process

            Patch[k].rd_biom = new double[2];
            Patch[k].rd_hum = new double[2];
            Patch[k].opt_temp = new double[2];
            for (int index = 0; index < 2; index++)
            {
                // potential rate of soil biomass mineralisation (per day)
                Patch[k].rd_biom[index] = rd_biom[index];

                // potential rate of humus mineralisation (per day)
                Patch[k].rd_hum[index] = rd_hum[index];

                // Soil temperature above which there is no further effect on mineralisation and nitrification (oC)
                Patch[k].opt_temp[index] = opt_temp[index];
            }

            // fraction of biomass C mineralised retained in system (0-1)
            Patch[k].ef_biom = ef_biom;

            // fraction of retained biomass C returned to biomass (0-1)
            Patch[k].fr_biom_biom = fr_biom_biom;

            // fraction of humic C mineralised retained in system (0-1)
            Patch[k].ef_hum = ef_hum;

            #region Old parameters

            Patch[k].wfmin_index = new double[wfmin_index.Length];
            Patch[k].wfmin_values = new double[wfmin_index.Length];
            for (int i = 0; i < wfmin_index.Length; i++)
            {
                // index specifying water content for water factor for mineralisation
                Patch[k].wfmin_index[i] = wfmin_index[i];
                // value of water factor(mineralisation) function at given index values
                Patch[k].wfmin_values[i] = wfmin_values[i];
            }

            #endregion

            #region New parameters

            // parameters for temperature factor for mineralisation
            Patch[k].stfData_MinerSOM.TempOptimum = TempFactor_Miner.TempOptimum;
            Patch[k].stfData_MinerSOM.FactorAtZero = TempFactor_Miner.FactorAtZero;
            Patch[k].stfData_MinerSOM.CurveExponent = TempFactor_Miner.CurveExponent;

            // parameters for soil moisture factor for OM mineralisation
            Patch[k].swfData_MinerSOM.xVals = MoistFactor_Miner.xVals;
            Patch[k].swfData_MinerSOM.yVals = MoistFactor_Miner.yVals;

            #region parameter for each OM type

            // parameters for temperature factor for humus mineralisation
            Patch[k].stfData_MinerHum.TempOptimum = TempFactor_minerHum.TempOptimum;
            Patch[k].stfData_MinerHum.FactorAtZero = TempFactor_minerHum.FactorAtZero;
            Patch[k].stfData_MinerHum.CurveExponent = TempFactor_minerHum.CurveExponent;

            // parameters for soil moisture factor for humus mineralisation
            Patch[k].swfData_MinerHum.xVals = MoistFactor_minerHum.xVals;
            Patch[k].swfData_MinerHum.yVals = MoistFactor_minerHum.yVals;

            // parameters for temperature factor for OM biomass mineralisation
            Patch[k].stfData_MinerBiom.TempOptimum = TempFactor_minerBiom.TempOptimum;
            Patch[k].stfData_MinerBiom.FactorAtZero = TempFactor_minerBiom.FactorAtZero;
            Patch[k].stfData_MinerBiom.CurveExponent = TempFactor_minerBiom.CurveExponent;

            // parameters for soil moisture factor for OM biomass mineralisation
            Patch[k].swfData_MinerBiom.xVals = MoistFactor_minerBiom.xVals;
            Patch[k].swfData_MinerBiom.yVals = MoistFactor_minerBiom.yVals;

            #endregion

            #endregion

            #endregion

            #region Parameters for urea hydrolisys process

            // parameters for temperature factor for urea hydrolisys
            Patch[k].TempFactor_Hydrol.TempOptimum = TempFactor_Hydrol.TempOptimum;
            Patch[k].TempFactor_Hydrol.FactorAtZero = TempFactor_Hydrol.FactorAtZero;
            Patch[k].TempFactor_Hydrol.CurveExponent = TempFactor_Hydrol.CurveExponent;

            // parameters for soil moisture factor for hydrolisys
            Patch[k].MoistFactor_Hydrol.xVals = MoistFactor_Hydrol.xVals;
            Patch[k].MoistFactor_Hydrol.yVals = MoistFactor_Hydrol.yVals;

            // parameters for potential urea hydrolisys
            Patch[k].potHydrol_min = potHydrol_min;
            Patch[k].potHydrol_parmA = potHydrol_parmA;
            Patch[k].potHydrol_parmB = potHydrol_parmB;
            Patch[k].potHydrol_parmC = potHydrol_parmC;
            Patch[k].potHydrol_parmD = potHydrol_parmD;

            #endregion

            #region Parameters for nitrification process

            // Potential nitrification by soil (ppm)
            Patch[k].nitrification_pot = nitrification_pot;

            // nh4 conc at half potential (ppm)   
            Patch[k].nh4_at_half_pot = nh4_at_half_pot;

            #region Old parameters

            Patch[k].wfnit_index = new double[wfnit_index.Length];
            Patch[k].wfnit_values = new double[wfnit_index.Length];
            for (int i = 0; i < wfnit_index.Length; i++)
            {
                // index specifying water content for water factor for nitrification
                Patch[k].wfnit_index[i] = wfnit_index[i];

                // value of water factor(nitrification) function at given index values
                Patch[k].wfnit_values[i] = wfnit_values[i];
            }

            Patch[k].pHf_nit_pH = new double[pHf_nit_pH.Length];
            Patch[k].pHf_nit_values = new double[pHf_nit_pH.Length];
            for (int i = 0; i < pHf_nit_pH.Length; i++)
            {
                // pH values for specifying pH factor for nitrification
                Patch[k].pHf_nit_pH[i] = pHf_nit_pH[i];

                // value of pH factor(nitrification) function for given pH values
                Patch[k].pHf_nit_values[i] = pHf_nit_values[i];
            }

            #endregion

            #region New parameters

            // parameters for temperature factor for nitrification
            Patch[k].TempFactor_Nitrif.TempOptimum = TempFactor_Nitrif.TempOptimum;
            Patch[k].TempFactor_Nitrif.FactorAtZero = TempFactor_Nitrif.FactorAtZero;
            Patch[k].TempFactor_Nitrif.CurveExponent = TempFactor_Nitrif.CurveExponent;

            // parameters for soil moisture factor for nitrification
            Patch[k].MoistFactor_Nitrif.xVals = MoistFactor_Nitrif.xVals;
            Patch[k].MoistFactor_Nitrif.yVals = MoistFactor_Nitrif.yVals;

            // parameters for pH factor for nitrification
            Patch[k].pHFactor_Nitrif.xVals = pHFactor_Nitrif.xVals;
            Patch[k].pHFactor_Nitrif.yVals = pHFactor_Nitrif.yVals;

            #endregion

            #endregion

            #region Parameters for denitrification and N2O emission processes

            Patch[k].dnit_rate_coeff = dnit_rate_coeff;      // denitrification rate coefficient (kg/mg)
            Patch[k].dnit_nitrf_loss = dnit_nitrf_loss;      // Fraction of nitrification lost as denitrification
            Patch[k].dnit_k1 = dnit_k1;              // K1 parameter from Thorburn et al (2010) for N2O model

            #region Old parameters

            Patch[k].dnit_wf_power = dnit_wf_power;        // denitrification water factor power term

            Patch[k].dnit_wfps = new double[dnit_wfps.Length];
            Patch[k].dnit_n2o_factor = new double[dnit_wfps.Length];
            for (int i = 0; i < dnit_wfps.Length; i++)
            {
                Patch[k].dnit_wfps[i] = dnit_wfps[i];            // WFPS for calculating the n2o fraction of denitrification
                Patch[k].dnit_n2o_factor[i] = dnit_n2o_factor[i];       // WFPS factor for n2o fraction of denitrification
            }

            #endregion

            #region New parameters

            // parameter 2 to compute active carbon (for denitrification)
            Patch[k].actC_parmA = actC_parmA;

            // parameter 1 to compute active carbon (for denitrification)
            Patch[k].actC_parmB = actC_parmB;

            // parameters for temperature factor for denitrification
            Patch[k].TempFactor_Denit.TempOptimum = TempFactor_Denit.TempOptimum;
            Patch[k].TempFactor_Denit.FactorAtZero = TempFactor_Denit.FactorAtZero;
            Patch[k].TempFactor_Denit.CurveExponent = TempFactor_Denit.CurveExponent;

            // parameters for soil moisture factor for denitrification
            Patch[k].MoistFactor_Denit.xVals = MoistFactor_Denit.xVals;
            Patch[k].MoistFactor_Denit.yVals = MoistFactor_Denit.yVals;

            // parameter for TermA in N2N2O function
            Patch[k].N2N2O_parmA = N2N2O_parmA;

            // parameter for TermB in N2N2O function
            Patch[k].N2N2O_parmB = N2N2O_parmB;

            // parameters to compute WFPS factors on N2:N2O ration calc
            Patch[k].wfpsfData_n2n2o.xVals = WFPSFactor_N2N2O.xVals;
            Patch[k].wfpsfData_n2n2o.yVals = WFPSFactor_N2N2O.yVals;

            #endregion

            #endregion

            #endregion

            #region Parameters needed for initalisation, but may change during simulation

            #region Soil Data

            Patch[k].dlayer = new double[dlayer.Length];
            Patch[k].bd = new double[dlayer.Length];
            Patch[k].sat_dep = new double[dlayer.Length];
            Patch[k].dul_dep = new double[dlayer.Length];
            Patch[k].ll15_dep = new double[dlayer.Length];
            Patch[k].sw_dep = new double[dlayer.Length];
            Patch[k].ph = new double[dlayer.Length];
            Patch[k].st = new double[dlayer.Length];
            for (int layer = 0; layer < dnit_wfps.Length; layer++)
            {
                // soil layers' thichness (mm)
                Patch[k].dlayer[layer] = dlayer[layer];

                // soil bulk density for each layer (kg/dm3)
                Patch[k].bd[layer] = bd[layer];

                // soil water content at saturation
                Patch[k].sat_dep[layer] = sat_dep[layer];

                // soil water content at drainage upper limit
                Patch[k].dul_dep[layer] = dul_dep[layer];

                // soil water content at drainage lower limit
                Patch[k].ll15_dep[layer] = ll15_dep[layer];

                // today's soil water content
                Patch[k].sw_dep[layer] = sw_dep[layer];

                // soil pH
                Patch[k].ph[layer] = ph[layer];

                // soil temperature (as computed by another module - SoilTemp)
                Patch[k].st[layer] = st[layer];
            }

            // soil albedo (0-1)
            Patch[k].salb = salb;

            #endregion

            #region Soil loss data

            // soil loss, due to erosion (?)
            Patch[k].soil_loss = soil_loss;

            #endregion

            #endregion
        }

        private void UpdatePatches()
        {
            // +  Purpose:
            //      Updates the variables in each existing patch
            //         all values that are the same for all patches

            for (int k = 0; k < Patch.Count; k++)
            {
                // today's date
                Patch[k].Today = Clock.Today;

                for (int layer = 0; layer < dlayer.Length; layer++)
                {
                    // soil layers' thichness (mm)
                    Patch[k].dlayer[layer] = dlayer[layer];

                    // soil bulk density for each layer (kg/dm3)
                    Patch[k].bd[layer] = bd[layer];

                    // soil water content at saturation
                    Patch[k].sat_dep[layer] = sat_dep[layer];

                    // soil water content at drainage upper limit
                    Patch[k].dul_dep[layer] = dul_dep[layer];

                    // soil water content at drainage lower limit
                    Patch[k].ll15_dep[layer] = ll15_dep[layer];

                    // today's soil water content
                    Patch[k].sw_dep[layer] = sw_dep[layer];

                    Patch[k].ph[layer] = ph[layer];       // soil pH

                    // soil temperature (as computed by another module - SoilTemp)
                    Patch[k].st[layer] = st[layer];
                }

                // soil albedo (0-1)
                Patch[k].salb = salb;

                // soil loss, due to erosion (?)
                Patch[k].soil_loss = soil_loss;
            }
        }

        private void SplitPatch(int j)
        {
            // +  Purpose:
            //      Split an existing patch in two. That is, creates a new patch (k) based on an existing one (j)

            // creat new patch
            soilCNPatch newPatch = new soilCNPatch();
            Patch.Add(newPatch);
            int k = Patch.Count - 1;

            // set the size of arrays
            Patch[k].ResizeLayerArrays(dlayer.Length);

            // initialise the patch
            InitialisePatch(k);

            // set C and N variables to the same state as the 'mother' patch
            for (int layer = 0; layer < dlayer.Length; layer++)
            {
                Patch[k].urea[layer] = Patch[j].urea[layer];
                Patch[k].nh4[layer] = Patch[j].nh4[layer];
                Patch[k].no3[layer] = Patch[j].no3[layer];
                Patch[k].inert_c[layer] = Patch[j].inert_c[layer];
                Patch[k].biom_c[layer] = Patch[j].biom_c[layer];
                Patch[k].biom_n[layer] = Patch[j].biom_n[layer];
                Patch[k].hum_c[layer] = Patch[j].hum_c[layer];
                Patch[k].hum_n[layer] = Patch[j].hum_n[layer];
                Patch[k].fom_c_pool1[layer] = Patch[j].fom_c_pool1[layer];
                Patch[k].fom_c_pool2[layer] = Patch[j].fom_c_pool2[layer];
                Patch[k].fom_c_pool3[layer] = Patch[j].fom_c_pool3[layer];
                Patch[k].fom_n_pool1[layer] = Patch[j].fom_n_pool1[layer];
                Patch[k].fom_n_pool2[layer] = Patch[j].fom_n_pool2[layer];
                Patch[k].fom_n_pool3[layer] = Patch[j].fom_n_pool3[layer];
            }

            // store today's values
            Patch[k].InitCalc();
        }

        private void MergePatches(int recipient, int disappearing)
        {
            // +  Purpose:
            //      Merges two patches, one is disappearing and its pools will be added to the recipient

            // get the weighted average for each variable and assign to the recipient patch
            double[] newValue = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
            {
                Patch[recipient].urea[layer] = (Patch[recipient].urea[layer] * Patch[recipient].PatchArea + Patch[disappearing].urea[layer] * Patch[disappearing].PatchArea) / Patch[recipient].PatchArea;
                Patch[recipient].nh4[layer] = (Patch[recipient].nh4[layer] * Patch[recipient].PatchArea + Patch[disappearing].nh4[layer] * Patch[disappearing].PatchArea) / Patch[recipient].PatchArea;
                Patch[recipient].no3[layer] = (Patch[recipient].no3[layer] * Patch[recipient].PatchArea + Patch[disappearing].no3[layer] * Patch[disappearing].PatchArea) / Patch[recipient].PatchArea;
                Patch[recipient].inert_c[layer] = (Patch[recipient].inert_c[layer] * Patch[recipient].PatchArea + Patch[disappearing].inert_c[layer] * Patch[disappearing].PatchArea) / Patch[recipient].PatchArea;
                Patch[recipient].biom_c[layer] = (Patch[recipient].biom_c[layer] * Patch[recipient].PatchArea + Patch[disappearing].biom_c[layer] * Patch[disappearing].PatchArea) / Patch[recipient].PatchArea;
                Patch[recipient].biom_n[layer] = (Patch[recipient].biom_n[layer] * Patch[recipient].PatchArea + Patch[disappearing].biom_n[layer] * Patch[disappearing].PatchArea) / Patch[recipient].PatchArea;
                Patch[recipient].hum_c[layer] = (Patch[recipient].hum_c[layer] * Patch[recipient].PatchArea + Patch[disappearing].hum_c[layer] * Patch[disappearing].PatchArea) / Patch[recipient].PatchArea;
                Patch[recipient].hum_n[layer] = (Patch[recipient].hum_n[layer] * Patch[recipient].PatchArea + Patch[disappearing].hum_n[layer] * Patch[disappearing].PatchArea) / Patch[recipient].PatchArea;
                Patch[recipient].fom_c_pool1[layer] = (Patch[recipient].fom_c_pool1[layer] * Patch[recipient].PatchArea + Patch[disappearing].fom_c_pool1[layer] * Patch[disappearing].PatchArea) / Patch[recipient].PatchArea;
                Patch[recipient].fom_c_pool2[layer] = (Patch[recipient].fom_c_pool2[layer] * Patch[recipient].PatchArea + Patch[disappearing].fom_c_pool2[layer] * Patch[disappearing].PatchArea) / Patch[recipient].PatchArea;
                Patch[recipient].fom_c_pool3[layer] = (Patch[recipient].fom_c_pool3[layer] * Patch[recipient].PatchArea + Patch[disappearing].fom_c_pool3[layer] * Patch[disappearing].PatchArea) / Patch[recipient].PatchArea;
                Patch[recipient].fom_n_pool1[layer] = (Patch[recipient].fom_n_pool1[layer] * Patch[recipient].PatchArea + Patch[disappearing].fom_n_pool1[layer] * Patch[disappearing].PatchArea) / Patch[recipient].PatchArea;
                Patch[recipient].fom_n_pool2[layer] = (Patch[recipient].fom_n_pool2[layer] * Patch[recipient].PatchArea + Patch[disappearing].fom_n_pool2[layer] * Patch[disappearing].PatchArea) / Patch[recipient].PatchArea;
                Patch[recipient].fom_n_pool3[layer] = (Patch[recipient].fom_n_pool3[layer] * Patch[recipient].PatchArea + Patch[disappearing].fom_n_pool3[layer] * Patch[disappearing].PatchArea) / Patch[recipient].PatchArea;
            }

            // delete disappearing patch
            Patch.RemoveAt(disappearing);
        }

        private int[] CheckPatchIDs(int[] IDs, string[] Names)
        {
            // +  Purpose:
            //      Check the list of patch names and ids passed by 'AddSoilCNPatch', output only ids
            //        verify whether the names correspond to existing patches, verify whether there are replicates

            // List of patch ids for output
            List<int> SelectedIDs = new List<int>();

            if (Math.Max(IDs.Length, Names.Length) > 0)
            {  // at least one patch has been selected
                if (Names.Length > 0)
                {  // at least one name was selected, check value and get id
                    for (int pName = 0; pName < Names.Length; pName++)
                    {
                        for (int k = 0; k < Patch.Count; k++)
                        {
                            if (Names[pName] == Patch[k].PatchName)
                            {  // found the patch, check for replicates
                                for (int i = 0; i < SelectedIDs.Count; i++)
                                {
                                    if (SelectedIDs[i] == k)
                                    {  // id already selected
                                        i = SelectedIDs.Count;
                                        k = Patch.Count;
                                    }
                                    else
                                    {  // store the id
                                        SelectedIDs.Add(k);
                                        i = SelectedIDs.Count;
                                        k = Patch.Count;
                                    }
                                }
                            }
                            else
                            {  // name passed did not correspond to any patch
                                Console.WriteLine(Clock.Today.ToString("dd MMMM yyyy") + "(Day of year=" + Clock.Today.DayOfYear.ToString() + "), SoilNitrogen.AddPatch:");
                                Console.WriteLine("  the patch name '" + Names[pName] + "' did not correspond to any existing patch. Patch will be ignored.");
                            }
                        }
                    }
                }
                if (IDs.Length > 0)
                {  // at least one ID was selected, check value
                    for (int pId = 0; pId < IDs.Length; pId++)
                    {
                        for (int k = 0; k < Patch.Count; k++)
                        {
                            if (IDs[pId] == k)
                            {  // found the patch, check for replicates
                                for (int i = 0; i < SelectedIDs.Count; i++)
                                {
                                    if (SelectedIDs[i] == k)
                                    {  // id already selected
                                        i = SelectedIDs.Count;
                                        k = Patch.Count;
                                    }
                                    else
                                    {  // store the id
                                        SelectedIDs.Add(k);
                                        i = SelectedIDs.Count;
                                        k = Patch.Count;
                                    }
                                }
                            }
                            else
                            {  // id passed did not correspond to any patch
                                Console.WriteLine(Clock.Today.ToString("dd MMMM yyyy") + "(Day of year=" + Clock.Today.DayOfYear.ToString() + "), SoilNitrogen.AddPatch:");
                                Console.WriteLine("  the patch id '" + IDs[pId] + "' did not correspond to any existing patch. Patch will be ignored.");
                            }
                        }
                    }
                }
            }
            else
            {  // no patch was indicated, use 'base'
                SelectedIDs.Add(0);
            }
            // pass data into an array to return as result
            int[] result = new int[SelectedIDs.Count];
            for (int i = 0; i < SelectedIDs.Count; i++)
                result[i] = SelectedIDs[i];
            return result;
        }

        private PatchIDs ComparePatches()
        {
            // +  Purpose:
            //      Compare all existing patches, return the pairs that can be merged

            PatchIDs Patches = new PatchIDs();
            List<int> recipient = new List<int>();
            List<int> disappearing = new List<int>();

            for (int k1 = 0; k1 < Patch.Count; k1++)
            {
                for (int k2 = k1 + 1; k2 < Patch.Count; k2++)
                {
                    // go through a series of criteria t evaluate whether the two patches can be merged
                    if (Math.Abs(Patch[k1].carbon_tot[0] - Patch[k2].carbon_tot[0]) < epsilon)
                    {
                        if (Math.Abs(Patch[k1].no3[0] - Patch[k2].no3[0]) < epsilon)
                        {
                            recipient.Add(k1);
                            disappearing.Add(k2);
                        }
                    }
                }
            }
            Patches.recipient = recipient;
            Patches.disappearing = disappearing;
            return Patches;
        }

        private double[][] partitionDelta(double[] incoming, string SoluteName)
        {
            // + Purpose
            //     calculate how the dlt's are partitioned amongst patches
            //      return a dlt value for each patch and each layer to which an incoming dlt was provided

            // 1- initialise the result to zero
            double[][] result = new double[Patch.Count][];
            for (int k = 0; k < Patch.Count; k++)
                result[k] = new double[dlayer.Length];

            try
            {
                // 2- gather how much solute is already in the soil
                double[][] alreadyThere = new double[Patch.Count][];
                for (int k = 0; k < Patch.Count; k++)
                {
                    alreadyThere[k] = new double[dlayer.Length];
                    if (SoluteName == "urea")
                        for (int layer = 0; layer < dlayer.Length; layer++)
                            alreadyThere[k][layer] = Patch[k].urea[layer];
                    else if (SoluteName == "nh4")
                        for (int layer = 0; layer < dlayer.Length; layer++)
                            alreadyThere[k][layer] = Patch[k].nh4[layer];
                    else if (SoluteName == "no3")
                        for (int layer = 0; layer < dlayer.Length; layer++)
                            alreadyThere[k][layer] = Patch[k].no3[layer];
                }

                // 3- calculations are done for each layer 
                for (int layer = 0; layer < (dlayer.Length); layer++)
                {
                    // 3.1- compute the total solute amount, over all patches
                    double totalSol = 0.0;
                    for (int k = 0; k < Patch.Count; k++)
                        totalSol += alreadyThere[k][layer] * Patch[k].PatchArea;

                    // 3.2- calculations for each patch
                    for (int k = 0; k < Patch.Count; k++)
                    {
                        // 3.2.1- compute the weights (based on existing solute amount)
                        double weight = 1.0;
                        if (totalSol > 0)
                            weight = alreadyThere[k][layer] / totalSol;

                        // 3.2.2- partition the dlt's for each patch
                        result[k][layer] = incoming[layer] * weight;

                    }
                }
            }
            catch (Exception e)
            {
                throw new System.InvalidOperationException(" problem with " + SoluteName + "- " + e.ToString());
            }
            return result;
        }
    }
