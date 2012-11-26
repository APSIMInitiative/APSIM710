using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ModelFramework;
using CSGeneral;
using System.Xml;

/// <summary>
/// This partial class contains most of the variables and properties of SoilNitrogen
/// </summary>

    public partial class SoilNitrogen
    {

        #region Parameters and inputs provided by the user or APSIM

        #region Parameters added by RCichota

        // whether to use new functions to compute temp and moist factors
        private bool useNewSTFFunction = false;  // for stf
        private bool useNewSWFFunction = false;  // for swf
        private bool useNewProcesses = false;    // for processes
        [Param]
        private string useAllNewFunctions
        {
            set
            {
                useNewSTFFunction = value.ToLower().Contains("yes");
                useNewSWFFunction = useNewSTFFunction;
                useNewProcesses = useNewSTFFunction;
            }
        }
        [Param]
        private string useNewFunction4TF
        { set { useNewSTFFunction = value.ToLower().Contains("yes"); } } // for stf
        [Param]
        private string useNewFunction4WF
        { set { useNewSWFFunction = value.ToLower().Contains("yes"); } } // for swf

        // whether to use single temp and moist factors for SOM and FOM mineralisation ir separated
        private bool useSingleMinerFactors = true;
        [Param]
        private string useSingleFactors4Miner
        { set { useSingleMinerFactors = value.ToLower().Contains("yes"); } }

        // whether calculate one set of mineralisation factors (stf and swf) or one for each pool
        private bool useFactorsBySOMpool = false;
        [Param]
        private string useMultiFactors4MinerSOM
        { set { useFactorsBySOMpool = value.ToLower().Contains("yes"); } }
        private bool useFactorsByFOMpool = false;
        [Param]
        private string useMultiFactors4MinerFOM
        { set { useFactorsByFOMpool = value.ToLower().Contains("yes"); } }


        #endregion

        //*Following parameters might be better merged into other regions but it is clear to have it separtately, FLi 
        #region ALTERNATIVE Params for alternarive nitrification/denirification processes

        // soil texture by layer: COARSE = 1.0;/MEDIUM = 2.0; FINE = 3.0; VERYFINE = 4.0;
        [Param(IsOptional = true, MinVal = 1.0, MaxVal = 4.0)]
        [Input(IsOptional = true)]
        private double[] texture;

        //Alternative N2O emission
        [Param(MinVal = 0, MaxVal = 10)]
        private int n2o_approach = 0;           // Approches used for nitri/denitri process for n2o emission 

        //WNMM
        [Param(MinVal = 0.0, MaxVal = 1.0)]
        private double wnmm_n_alpha = 0.002;             // maximum fraction of nitrification rate as N2O

        [Param(MinVal = 0.0, MaxVal = 1.0)]
        private double wnmm_dn_alpha = 0.5;            // maximum fraction of denitrification rate at wfps = 0.8

        //NWMIS
        [Param(MinVal = 0.0, MaxVal = 50.0)]
        private double nemis_dn_km = 22;              // half-saturation consntant for NO3 reduction (unit ppm = mgN/kg)

        [Param(MinVal = 0.0, MaxVal = 50.0)]
        private double nemis_dn_pot = 7.194; 	        // default = 7.194; potential denitrification rate at 20C, on undisturbed soil 
        // saturated with water in the lab and placed at a nitrate content near to 200 mgN/kg
        //CENTURY
        [Param(MinVal = 0.0, MaxVal = 60.0)]
        private double cent_n_soilt_ave = 15;             // average soil surface temperature

        [Param(MinVal = 0.0, MaxVal = 60.0)]
        private double cent_n_maxt_ave = 25; 	            // long term average maximum monthly temperature of the hottest month	

        [Param(MinVal = 0.0, MaxVal = 1.0)]
        private double cent_n_wfps_ave = 0.7;              // default = 0.7; average wfps in top nitrifyDepth of soil

        [Param(MinVal = 0.0, MaxVal = 1.0)]
        private double cent_n_max_rate = 0.1;              // default = 0.1, maximum fraction of ammonium to NO3 during nitrification (gN/m2)
        #endregion

        #region Parameters used on initialisation only

        #region General setting parameters

        // soil parameterisation to use, used to determine which node of xml file will be used to read [Param]
        [Param(IsOptional = true)]
        private string SoilParamSet = "standard";

        // whether simpleSoilTemp is allowed, if 'no' SoiTemp must be present, if 'yes' SoilTemp will be used if present
        private bool AllowsimpleSoilTemp = false;
        [Param]
        private string allow_simpleSoilTemp
        { set { AllowsimpleSoilTemp = value.ToLower().Contains("yes"); } }

        // switch indicating whether soil profile reduction is allowed (from erosion)
        private bool AllowProfileReduction = false;
        [Param]
        private string profile_reduction
        { set { AllowProfileReduction = value.ToLower().StartsWith("on"); } }

        // marker for whether organic solute are to be simulated (always false as it is not implemented)
        private bool useOrganicSolutes = false;
        [Param(IsOptional = true)]
        private string use_organic_solutes
        { set { useOrganicSolutes = value.ToLower().StartsWith("on"); } }

        // minimum allowable Urea content (ppm), per layer
        [Param(MinVal = 0.0, MaxVal = 1000.0)]
        private double ureappm_min = 0.0;

        // minimum allowable NH4 content (ppm), per layer
        [Param(MinVal = 0.0, MaxVal = 1000.0)]
        private double nh4ppm_min = 0.0;

        // minimum allowable NO3 content (ppm), per layer
        [Param(MinVal = 0.0, MaxVal = 1000.0)]
        private double no3ppm_min = 0.0;

        // minimum allowable FOM content (kg/ha), per layer
        [Param(MinVal = 0.0, MaxVal = 1.0)]
        private double fom_min;

        // conversion from OC to OM
        [Param(MinVal = 0.0, MaxVal = 3.0)]
        private double oc2om_factor = 1.7;

        // weight fraction of C in carbohydrates
        [Param(MinVal = 0.0, MaxVal = 1.0)]
        private double c_in_fom = 0.4;

        // minimum relative area (fraction of paddock) for any patch
        [Param(MinVal = 0.0, MaxVal = 1.0)]
        private double minPatchArea = 0.001;

        #endregion

        #region Parameters for handling soil loss process

        // enrichment equation coefficient a
        [Param]
        private double enr_a_coeff = 0.0;

        // enrichment equation coefficient b
        [Param]
        private double enr_b_coeff = 0.0;

        #endregion

        #region Parameters for setting soil organic matter

        // the soil C:N ratio (actually of humus)
        private double hum_cn = 0.0;
        [Param(MinVal = 1.0, MaxVal = 25.0)]
        private double soil_cn
        {
            get { return hum_cn; }
            set { hum_cn = value; }
        }

        // the C:N ratio of microbial biomass
        private double biom_cn = 8.0;
        [Param(IsOptional = true, MinVal = 1.0, MaxVal = 50.0)]
        private double mcn
        {
            get { return biom_cn; }
            set { biom_cn = value; }
        }

        // initial ratio of biomass-C to mineralizable humic-C (0-1)
        [Param(MinVal = 0.0, MaxVal = 1.0)]
        private double[] fbiom = null;

        // initial proportion of total soil C that is not subject to mineralisation (0-1)
        [Param(MinVal = 0.0, MaxVal = 1.0)]
        private double[] finert = null;

        #endregion

        #region Parameters for setting fresh organic matter (FOM)

        // initial weight of fom in the soil (kgDM/ha)
        private double fom_ini_wt = 0.0;
        [Param(MinVal = 0.0, MaxVal = 10000.0)]
        private double root_wt
        {
            get { return fom_ini_wt; }
            set { fom_ini_wt = value; }
        }

        // initial depth over which fom is distributed within the soil profile (mm)
        private double fom_ini_depth = 0.0;
        [Param(IsOptional = true, MinVal = 0.0, MaxVal = 5000.0)]
        private double root_depth
        {
            get { return fom_ini_depth; }
            set { fom_ini_depth = value; }
        }

        // initial C:N ratio of roots (actually FOM)
        private double fom_ini_cn = 0.0;
        [Param(MinVal = 0.1, MaxVal = 750.0)]
        private double root_cn
        {
            get { return fom_ini_cn; }
            set { fom_ini_cn = value; }
        }

        // initial C:N ratio of each of the three fom composition pools (carbohydrate, cellulose, and lignin) - case not given, fom_cn is used
        private double[] fomPools_cn = null;
        [Param(IsOptional = true, MinVal = 0.0, MaxVal = 1000.0)]
        private double[] root_cn_pool
        {
            get { return fomPools_cn; }
            set { fomPools_cn = value; }
        }

        // list of fom types
        [Param(Name = "fom_type")]
        private String[] fom_types;

        // fraction of carbohydrate in FOM (0-1), for each FOM type
        [Param(MinVal = 0.0, MaxVal = 1.0)]
        private double[] fract_carb;

        // fraction of cellulose in FOM (0-1), for each FOM type
        [Param(MinVal = 0.0, MaxVal = 1.0)]
        private double[] fract_cell;

        // fraction of lignin in FOM (0-1), for each FOM type
        [Param(MinVal = 0.0, MaxVal = 1.0)]
        private double[] fract_lign;

        #endregion

        #region Parameters for FOM and SurfOM mineralisation process

        #region Surface OM

        // fraction of residue C mineralised retained in system (0-1)
        [Param(MinVal = 0.0, MaxVal = 1.0)]
        private double ef_res;

        // fraction of retained residue C transferred to biomass (0-1)
        [Param(MinVal = 0.0, MaxVal = 1.0)]
        private double fr_res_biom;

        // depth from which mineral N can be immobilised by decomposing residues (mm)
        [Param(MinVal = 0.0, MaxVal = 1000.0)]
        private double min_depth;

        #endregion

        #region Fresh OM

        #region Old parameters

        // maximum rate constants for decomposition of FOM pools [carbohydrate component] (0-1)
        [Param(MinVal = 0.0, MaxVal = 1.0)]
        private double[] rd_carb;

        // maximum rate constants for decomposition of FOM pools [cellulose component] (0-1)
        [Param(MinVal = 0.0, MaxVal = 1.0)]
        private double[] rd_cell;

        // maximum rate constants for decomposition of FOM pools [lignin component] (0-1)
        [Param(MinVal = 0.0, MaxVal = 1.0)]
        private double[] rd_lign;

        // fraction of FOM C mineralised retained in system (0-1)   
        [Param(MinVal = 0.0, MaxVal = 1.0)]
        private double ef_fom;

        // fraction of retained FOM C transferred to biomass (0-1)
        [Param(MinVal = 0.0, MaxVal = 1.0)]
        private double fr_fom_biom;

        // coeff. to determine the magnitude of C:N effects on decomposition of FOM ()
        [Param(MinVal = 0.0, MaxVal = 10.0)]
        private double cnrf_coeff;

        // C:N above which decomposition rate of FOM declines ()
        [Param(MinVal = 5.0, MaxVal = 100.0)]
        private double cnrf_optcn;

        #endregion

        #region New parameters

        // optimum temperature for mineralisation of FOM
        private TempFactorData TempFactor_minerFOM = new TempFactorData();
        [Param]
        private double[] stfMinerFOM_Topt
        { set { TempFactor_minerFOM.TempOptimum = value; } }

        // temperature factor for mineralisation of FOM at zero degrees
        [Param]
        private double[] stfMinerFOM_FctrZero
        { set { TempFactor_minerFOM.FactorAtZero = value; } }

        // curve exponent for temperature factor for mineralisation of FOM
        [Param]
        private double[] stfMinerFOM_CvExp
        { set { TempFactor_minerFOM.CurveExponent = value; } }

        // parameters for soil moisture factor for mineralisation of FOM
        private XYData MoistFactor_minerFOM = new XYData();
        [Param]
        private double[] swfMinerFOM_x
        { set { MoistFactor_minerFOM.xVals = value; } }
        [Param]
        private double[] swfMinerFOM_y
        { set { MoistFactor_minerFOM.yVals = value; } }

        // parameters for C:N factor mineralisation of FOM
        private double CNFactorMinerFOM_OptCN;
        [Param]
        private double cnfMinerFOM_OptCN
        { set { CNFactorMinerFOM_OptCN = value; } }
        private double CNFactorMinerFOM_RateCN;
        [Param]
        private double cnfMinerFOM_RateCN
        { set { CNFactorMinerFOM_RateCN = value; } }

        #endregion

        #endregion

        #endregion

        #region Parameters for SOM mineralisation/immobilisation process

        // potential rate of soil biomass mineralisation (per day)
        [Param(MinVal = 0.0, MaxVal = 1.0)]
        private double[] rd_biom = null;

        // fraction of biomass C mineralised retained in system (0-1)
        [Param(MinVal = 0.0, MaxVal = 1.0)]
        private double ef_biom;

        // fraction of retained biomass C returned to biomass (0-1)
        [Param(MinVal = 0.0, MaxVal = 1.0)]
        private double fr_biom_biom;

        // potential rate of humus mineralisation (per day)
        [Param(MinVal = 0.0, MaxVal = 1.0)]
        private double[] rd_hum = null;

        // fraction of humic C mineralised retained in system (0-1)
        [Param(MinVal = 0.0, MaxVal = 1.0)]
        private double ef_hum;

        #region Old parameters

        // Soil temperature above which there is no further effect on mineralisation and nitrification (oC)
        [Param(MinVal = 5.0, MaxVal = 100.0)]
        private double[] opt_temp;

        // index specifying water content for water factor for mineralisation
        [Param(MinVal = 0.0, MaxVal = 2.0)]
        private double[] wfmin_index;

        // value of water factor(mineralisation) function at given index values
        [Param(MinVal = 0.0, MaxVal = 1.0)]
        private double[] wfmin_values;

        #endregion

        #region New parameters

        // optimum temperature for OM mineralisation
        private TempFactorData TempFactor_Miner = new TempFactorData();
        [Param]
        private double[] stfMiner_Topt
        { set { TempFactor_Miner.TempOptimum = value; } }

        // temperature factor for OM mineralisation at zero degrees
        [Param]
        private double[] stfMiner_FctrZero
        { set { TempFactor_Miner.FactorAtZero = value; } }

        // curve exponent for temperature factor for OM mineralisation
        [Param]
        private double[] stfMiner_CvExp
        { set { TempFactor_Miner.CurveExponent = value; } }

        // parameters for soil moisture factor for OM mineralisation
        private XYData MoistFactor_Miner = new XYData();
        [Param]
        private double[] swfMiner_x
        { set { MoistFactor_Miner.xVals = value; } }
        [Param]
        private double[] swfMiner_y
        { set { MoistFactor_Miner.yVals = value; } }

        #region Parameters for each OM type

        // optimum temperature for mineralisation of humus
        private TempFactorData TempFactor_minerHum = new TempFactorData();
        [Param]
        private double[] stfMinerHum_Topt
        { set { TempFactor_minerHum.TempOptimum = value; } }

        // temperature factor for mineralisation of humus at zero degrees
        [Param]
        private double[] stfMinerHum_FctrZero
        { set { TempFactor_minerHum.FactorAtZero = value; } }

        // curve exponent for temperature factor for mineralisation of humus
        [Param]
        private double[] stfMinerHum_CvExp
        { set { TempFactor_minerHum.CurveExponent = value; } }

        // parameters for soil moisture factor for mineralisation of humus
        private XYData MoistFactor_minerHum = new XYData();
        [Param]
        private double[] swfMinerHum_x
        { set { MoistFactor_minerHum.xVals = value; } }
        [Param]
        private double[] swfMinerHum_y
        { set { MoistFactor_minerHum.yVals = value; } }

        // optimum temperature for mineralisation of OM biomass
        private TempFactorData TempFactor_minerBiom = new TempFactorData();
        [Param]
        private double[] stfMinerBiom_Topt
        { set { TempFactor_minerBiom.TempOptimum = value; } }

        // temperature factor for mineralisation of OM biomass at zero degrees
        [Param]
        private double[] stfMinerBiom_FctrZero
        { set { TempFactor_minerBiom.FactorAtZero = value; } }

        // curve exponent for temperature factor for mineralisation of OM biomass
        [Param]
        private double[] stfMinerBiom_CvExp
        { set { TempFactor_minerBiom.CurveExponent = value; } }

        // parameters for soil moisture factor for mineralisation of OM biomass
        private XYData MoistFactor_minerBiom = new XYData();
        [Param]
        private double[] swfMinerBiom_x
        { set { MoistFactor_minerBiom.xVals = value; } }
        [Param]
        private double[] swfMinerBiom_y
        { set { MoistFactor_minerBiom.yVals = value; } }

        #endregion

        #endregion

        #endregion

        #region Parameters for urea hydrolisys process

        // optimum temperature for urea hydrolisys
        private TempFactorData TempFactor_Hydrol = new TempFactorData();
        [Param]
        private double[] stfHydrol_Topt
        { set { TempFactor_Hydrol.TempOptimum = value; } }

        // temperature factor for urea hydrolisys at zero degrees
        [Param]
        private double[] stfHydrol_FctrZero
        { set { TempFactor_Hydrol.FactorAtZero = value; } }

        // curve exponent for temperature factor for urea hydrolisys
        [Param]
        private double[] stfHydrol_CvExp
        { set { TempFactor_Hydrol.CurveExponent = value; } }

        // parameters for soil moisture factor for hydrolisys
        private XYData MoistFactor_Hydrol = new XYData();
        [Param]
        private double[] swfHydrol_x
        { set { MoistFactor_Hydrol.xVals = value; } }
        [Param]
        private double[] swfHydrol_y
        { set { MoistFactor_Hydrol.yVals = value; } }

        // parameters for potential urea hydrolisys
        [Param]
        private double potHydrol_min;  // minimum value

        [Param]
        private double potHydrol_parmA;

        [Param]
        private double potHydrol_parmB;

        [Param]
        private double potHydrol_parmC;

        [Param]
        private double potHydrol_parmD;

        #endregion

        #region Parameters for nitrification process

        [Param(MinVal = 0.0, MaxVal = 100.0)]
        private double nitrification_pot;    // Potential nitrification by soil (ppm)

        [Param(MinVal = 0.0, MaxVal = 200.0)]
        private double nh4_at_half_pot;      // nh4 conc at half potential (ppm)   

        #region Old parameters

        [Param(MinVal = 0.0, MaxVal = 2.0)]
        private double[] wfnit_index;        // index specifying water content for water factor for nitrification

        [Param(MinVal = 0.0, MaxVal = 1.0)]
        private double[] wfnit_values;       // value of water factor(nitrification) function at given index values

        [Param(MinVal = 0.0, MaxVal = 14.0)]
        private double[] pHf_nit_pH;         // pH values for specifying pH factor for nitrification

        [Param(MinVal = 0.0, MaxVal = 1.0)]
        private double[] pHf_nit_values;     // value of pH factor(nitrification) function for given pH values

        #endregion

        #region New parameters

        // optimum temperature for nitrification
        private TempFactorData TempFactor_Nitrif = new TempFactorData();
        [Param]
        private double[] stfNitrif_Topt
        { set { TempFactor_Nitrif.TempOptimum = value; } }

        // temperature factor for nitrification at zero degrees
        [Param]
        private double[] stfNitrif_FctrZero
        { set { TempFactor_Nitrif.FactorAtZero = value; } }

        // curve exponent for temperature factor for nitrification
        [Param]
        private double[] stfNitrif_CvExp
        { set { TempFactor_Nitrif.CurveExponent = value; } }

        // parameters for soil moisture factor for nitrification
        private XYData MoistFactor_Nitrif = new XYData();
        [Param]
        private double[] swfNitrif_x
        { set { MoistFactor_Nitrif.xVals = value; } }
        [Param]
        private double[] swfNitrif_y
        { set { MoistFactor_Nitrif.yVals = value; } }

        // parameters for soil pH factor for nitrification
        private XYData pHFactor_Nitrif = new XYData();
        [Param]
        private double[] sphfNitrif_x
        { set { pHFactor_Nitrif.xVals = value; } }
        [Param]
        private double[] sphfNitrif_y
        { set { pHFactor_Nitrif.yVals = value; } }

        #endregion

        #endregion

        #region Parameters for denitrification and N2O emission processes

        [Param(MinVal = 0.0, MaxVal = 1.0)]
        private double dnit_rate_coeff;      // denitrification rate coefficient (kg/mg)

        [Param(MinVal = 0.0, MaxVal = 1.0)]
        private double dnit_nitrf_loss;      // Fraction of nitrification lost as denitrification

        [Param(MinVal = 0.0, MaxVal = 100.0)]
        private double dnit_k1;              // K1 parameter from Thorburn et al (2010) for N2O model

        #region Old parameters

        [Param(MinVal = 0.0, MaxVal = 5.0)]
        private double dnit_wf_power;        // denitrification water factor power term

        [Param(MinVal = 0.0, MaxVal = 100.0)]
        private double[] dnit_wfps;            // WFPS for calculating the n2o fraction of denitrification

        [Param(MinVal = 0.0, MaxVal = 100.0)]
        private double[] dnit_n2o_factor;      // WFPS factor for n2o fraction of denitrification

        #endregion

        #region New parameters

        // parameter A to compute active carbon (for denitrification)
        [Param]
        private double actC_parmB;

        // parameter B to compute active carbon (for denitrification)
        [Param]
        private double actC_parmA;

        // optimum temperature for denitrification
        private TempFactorData TempFactor_Denit = new TempFactorData();
        [Param]
        private double[] stfDenit_Topt
        { set { TempFactor_Denit.TempOptimum = value; } }

        // temperature factor for denitrification at zero degrees
        [Param]
        private double[] stfDenit_FctrZero
        { set { TempFactor_Denit.FactorAtZero = value; } }

        // curve exponent for temperature factor for denitrification
        [Param]
        private double[] stfDenit_CvExp
        { set { TempFactor_Denit.CurveExponent = value; } }

        // parameters for soil moisture factor for denitrification
        private XYData MoistFactor_Denit = new XYData();
        [Param]
        private double[] swfDenit_x
        { set { MoistFactor_Denit.xVals = value; } }
        [Param]
        private double[] swfDenit_y
        { set { MoistFactor_Denit.yVals = value; } }

        // parameter A in the N2N2O function
        [Param]
        private double N2N2O_parmA;

        // parameter B in the N2N2O function
        [Param]
        private double N2N2O_parmB;

        // parameters for soil moisture factor for denitrification
        private XYData WFPSFactor_N2N2O = new XYData();
        [Param]
        private double[] wfpsN2N2O_x
        { set { WFPSFactor_N2N2O.xVals = value; } }
        [Param]
        private double[] wfpsN2N2O_y
        { set { WFPSFactor_N2N2O.yVals = value; } }

        #endregion

        #endregion

        #endregion

        #region Parameters that do or may change during simulation

        // today's date
        [Input]
        DateTime today;

        #region Parameters to set soil pH

        // pH of soil (assumed equivalent to a 1:1 soil-water slurry)
        [Param(IsOptional = true, MinVal = 3.5, MaxVal = 11.0)]
        [Input(IsOptional = true)]
        private double[] ph = null;

        #endregion

        #region Values for soil organic matter (som)

        // total soil organic carbon content (%)
        private double[] OC_reset;      // stores initial values until dlayer is available, can be used for a Reset operation
        [Param]
        [Output]
        [Units("%")]
        [Description("Soil organic carbon (exclude FOM)")]
        double[] oc
        {
            get
            {
                double[] result;
                if (initDone)
                {
                    result = new double[dlayer.Length];
                    for (int layer = 0; layer < dlayer.Length; ++layer)
                        for (int k = 0; k < Patch.Count; k++)
                            result[layer] += Patch[k].oc[layer] * Patch[k].PatchArea;
                }
                else
                    result = OC_reset;
                return result;
            }
            set
            {
                if (initDone)
                {
                    Console.WriteLine(" Attempt to assign values for OC during simulation, this operation is not valid and will be ignored");
                }
                else
                    OC_reset = value;
            }
        }

        #endregion

        #region Values for soil mineral N

        // soil urea nitrogen content (ppm) 
        private double[] ureappm_reset;     // stores initial value until dlayer is available, can be used for a Reset operation
        [Param(IsOptional = true, MinVal = 0.0, MaxVal = 10000.0)]
        [Output]
        [Units("mg/kg")]
        [Description("Soil urea nitrogen content")]
        private double[] ureappm
        {
            get
            {
                double[] result;
                if (initDone)
                {
                    result = new double[dlayer.Length];
                    for (int layer = 0; layer < dlayer.Length; ++layer)
                        for (int k = 0; k < Patch.Count; k++)
                            result[layer] += Patch[k].urea[layer] * convFactor_kgha2ppm(layer) * Patch[k].PatchArea;
                }
                else
                    result = ureappm_reset;
                return result;
            }
            set
            {
                if (initDone)
                {
                    double sumOld = SumDoubleArray(urea);      // original amount

                    for (int layer = 0; layer < value.Length; ++layer)
                        value[layer] = MathUtility.Divide(value[layer], convFactor_kgha2ppm(layer), 0.0);       //Convert from ppm to convFactor_kgha2ppm/ha
                    for (int k = 0; k < Patch.Count; k++)
                        Patch[k].urea = value;

                    if (!inReset)
                        SendExternalMassFlowN(SumDoubleArray(urea) - sumOld);

                }
                else
                    ureappm_reset = value;
            }
        }

        // soil urea nitrogen amount (kgN/ha)
        [Output]
        [Units("kgN/ha")]
        [Description("Soil urea nitrogen amount")]
        double[] urea
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].urea[layer] * Patch[k].PatchArea;
                return result;
            }
            set
            {
                double sumOld = SumDoubleArray(urea);

                for (int k = 0; k < Patch.Count; k++)
                    Patch[k].urea = value;

                SendExternalMassFlowN(SumDoubleArray(urea) - sumOld);
            }
        }

        // soil ammonium nitrogen content (ppm)
        private double[] nh4ppm_reset;      // stores initial value until dlayer is available, can be used for a Reset operation
        [Param(IsOptional = true, MinVal = 0.0, MaxVal = 10000.0)]
        [Output]
        [Units("mg/kg")]
        [Description("Soil ammonium nitrogen content")]
        private double[] nh4ppm
        {
            get
            {
                double[] result;
                if (initDone)
                {
                    result = new double[dlayer.Length];
                    for (int layer = 0; layer < dlayer.Length; ++layer)
                        for (int k = 0; k < Patch.Count; k++)
                            result[layer] += Patch[k].nh4[layer] * convFactor_kgha2ppm(layer) * Patch[k].PatchArea;
                }
                else
                    result = nh4ppm_reset;
                return result;
            }
            set
            {
                if (initDone)
                {
                    double sumOld = SumDoubleArray(nh4);   // original values

                    for (int layer = 0; layer < value.Length; ++layer)
                        value[layer] = MathUtility.Divide(value[layer], convFactor_kgha2ppm(layer), 0.0);       //Convert from ppm to convFactor_kgha2ppm/ha
                    for (int k = 0; k < Patch.Count; k++)
                        Patch[k].nh4 = value;

                    if (!inReset)
                        SendExternalMassFlowN(SumDoubleArray(nh4) - sumOld);
                }
                else
                    nh4ppm_reset = value;
            }
        }

        // Soil ammonium nitrogen amount (kg/ha)
        [Output]
        [Units("kgN/ha")]
        [Description("Soil ammonium nitrogen amount")]
        double[] nh4
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].nh4[layer] * Patch[k].PatchArea;
                return result;
            }
            set
            {
                double sumOld = SumDoubleArray(nh4);

                for (int k = 0; k < Patch.Count; k++)
                    Patch[k].nh4 = value;

                SendExternalMassFlowN(SumDoubleArray(nh4) - sumOld);
            }
        }

        // soil nitrate nitrogen content (ppm)
        private double[] no3ppm_reset;      // stores initial value until dlayer is available, can be used for a Reset operation
        [Param(IsOptional = true, MinVal = 0.0, MaxVal = 10000.0)]
        [Output]
        [Units("mg/kg")]
        [Description("Soil nitrate nitrogen content")]
        private double[] no3ppm
        {
            get
            {
                double[] result;
                if (initDone)
                {
                    result = new double[dlayer.Length];
                    for (int layer = 0; layer < dlayer.Length; ++layer)
                        for (int k = 0; k < Patch.Count; k++)
                            result[layer] += Patch[k].no3[layer] * convFactor_kgha2ppm(layer) * Patch[k].PatchArea;
                }
                else
                    result = no3ppm_reset;
                return result;
            }
            set
            {
                if (initDone)
                {
                    double sumOld = SumDoubleArray(no3);   // original values

                    for (int layer = 0; layer < value.Length; ++layer)
                        value[layer] = MathUtility.Divide(value[layer], convFactor_kgha2ppm(layer), 0.0);       //Convert from ppm to convFactor_kgha2ppm/ha
                    for (int k = 0; k < Patch.Count; k++)
                        Patch[k].no3 = value;

                    if (!inReset)
                        SendExternalMassFlowN(SumDoubleArray(no3) - sumOld);
                }
                else
                    no3ppm_reset = value;
            }
        }

        // soil nitrate nitrogen amount (kgN/ha)
        [Output]
        [Units("kgN/ha")]
        [Description("Soil nitrate nitrogen amount")]
        double[] no3
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].no3[layer] * Patch[k].PatchArea;
                return result;
            }
            set
            {
                double sumOld = SumDoubleArray(no3);

                for (int k = 0; k < Patch.Count; k++)
                    Patch[k].no3 = value;

                SendExternalMassFlowN(SumDoubleArray(no3) - sumOld);
            }
        }

        #endregion

        #region Values for simpleSoilTemp

        // local latitude
        [Input(IsOptional = true)]
        [Units("deg")]
        private double latitude = -999.0;

        // annual average ambient air temperature (oC)
        [Input(IsOptional = true)]
        [Units("oC")]
        private double tav = -999.0;

        // annual amplitude of the mean monthly air temperature (oC)
        [Input(IsOptional = true)]
        [Units("oC")]
        private double amp = -999.0;

        // today's net solar radiation
        [Input]
        [Units("oC")]
        private double radn = 0.0;

        // today's maximun air temperature
        [Input]
        [Units("oC")]
        private double maxt = 0.0;

        // today's minimun air temperature
        [Input]
        [Units("oC")]
        private double mint = 0.0;

        #endregion

        #region Soil physics data

        // soil layers' thichness (mm)
        [Input]
        [Units("mm")]
        private float[] dlayer = null;

        // soil bulk density for each layer (kg/dm3)
        [Input]
        [Units("g/cm3")]
        private float[] bd = null;

        // soil water content at saturation
        [Input]
        [Units("mm")]
        private float[] sat_dep = null;

        // soil water content at drainage upper limit
        [Input]
        [Units("mm")]
        private float[] dul_dep = null;

        // soil water content at drainage lower limit
        [Input]
        [Units("mm")]
        private float[] ll15_dep = null;

        // today's soil water content
        [Input]
        [Units("mm")]
        private float[] sw_dep = null;

        // soil albedo (0-1)
        [Input]
        private double salb;

        // soil temperature (as computed by another module - SoilTemp)
        [Input(IsOptional = true)]
        [Units("oC")]
        private double[] ave_soil_temp;

        #endregion

        #region Soil loss data

        // soil loss, due to erosion (?)
        [Input(IsOptional = true)]
        [Units("t/ha")]
        private double soil_loss = 0.0;

        #endregion

        #region Pond data

        // switch indicating whether pond is active or not
        //private Boolean is_pond_active = false;
        [Input(IsOptional = true)]
        private string pond_active
        {
            set
            {
                is_pond_active = (value == "yes");
                for (int k = 0; k < Patch.Count; k++)
                    Patch[k].is_pond_active = is_pond_active;
            }
        }

        // C decomposed in pond that is added to soil biomass
        [Input(IsOptional = true)]
        [Units("kg/ha")]
        private double pond_biom_C
        {
            set
            {
                for (int k = 0; k < Patch.Count; k++)
                    Patch[k].pond_biom_C = value;
            }
        }

        // C decomposed in pond that is added to soil humus
        [Input(IsOptional = true)]
        [Units("kg/ha")]
        private double pond_hum_C
        {
            set
            {
                for (int k = 0; k < Patch.Count; k++)
                    Patch[k].pond_hum_C = value;
            }
        }

        #endregion

        #region Inhibitors data

        // factor reducing nitrification due to the presence of a inhibitor
        double[] _nitrification_inhibition;
        [Input(IsOptional = true)]
        [Units("0-1")]
        double[] nitrification_inhibition
        {
            get { return _nitrification_inhibition; }
            set
            {
                _nitrification_inhibition = value;
                for (int layer = 0; layer <= dlayer.Length; layer++)
                {
                    if (_nitrification_inhibition[layer] < 0.0)
                    {
                        _nitrification_inhibition[layer] = 0.0;
                        Console.WriteLine("Value for nitrification inhibition is below lower limit, value will be adjusted to 0.0");
                    }
                    if (_nitrification_inhibition[layer] > 1.0)
                    {
                        _nitrification_inhibition[layer] = 1.0;
                        Console.WriteLine("Value for nitrification inhibition is above upper limit, value will be adjusted to 1.0");
                    }
                }
                for (int k = 0; k < Patch.Count; k++)
                    Patch[k].nitrification_inhibition = _nitrification_inhibition;
            }
        }

        // factor reducing urea hydrolysis due to the presence of an inhibitor - not implemented yet
        private double[] _hydrolysis_inhibition = null;
        [Input(IsOptional = true)]
        [Units("0-1")]
        private double[] hydrolysis_inhibition
        {
            get { return _hydrolysis_inhibition; }
            set
            {
                _hydrolysis_inhibition = value;
                for (int layer = 0; layer <= dlayer.Length; layer++)
                {
                    if (_hydrolysis_inhibition[layer] < 0.0)
                    {
                        _hydrolysis_inhibition[layer] = 0.0;
                        Console.WriteLine("Value for hydrolysis inhibition is below lower limit, value will be adjusted to 0.0");
                    }
                    if (_hydrolysis_inhibition[layer] > 1.0)
                    {
                        _hydrolysis_inhibition[layer] = 1.0;
                        Console.WriteLine("Value for hydrolysis inhibition is above upper limit, value will be adjusted to 1.0");
                    }
                }
                for (int k = 0; k < Patch.Count; k++)
                    Patch[k].hydrolysis_inhibition = _hydrolysis_inhibition;
            }
        }

        // factor reducing mineralisation processes due to the presence of an inhibitor - not implemented yet
        private double[] _mineralisation_inhibition = null;
        [Input(IsOptional = true)]
        [Units("0-1")]
        private double[] mineralisation_inhibition
        {
            get { return _mineralisation_inhibition; }
            set
            {
                _mineralisation_inhibition = value;
                for (int layer = 0; layer <= dlayer.Length; layer++)
                {
                    if (_mineralisation_inhibition[layer] < 0.0)
                    {
                        _mineralisation_inhibition[layer] = 0.0;
                        Console.WriteLine("Value for mineralisation inhibition is below lower limit, value will be adjusted to 0.0");
                    }
                    if (_mineralisation_inhibition[layer] > 1.0)
                    {
                        _mineralisation_inhibition[layer] = 1.0;
                        Console.WriteLine("Value for mineralisation inhibition is above upper limit, value will be adjusted to 1.0");
                    }
                }
                for (int k = 0; k < Patch.Count; k++)
                    Patch[k].mineralisation_inhibition = _mineralisation_inhibition;
            }
        }

        #endregion

        #endregion

        #region Settable variables

        // Even though these properties are settable, and not meant to be readable,
        // they still bear the "Output" attribute. 
        // Perhaps that bit of the infrastructure needs a re-think.

        // switch indicating whether soil profile reduction is allowed (from erosion)
        [Output]
        string n_reduction
        { set { AllowProfileReduction = value.StartsWith("on"); } }

        #region Mineral nitrogen

        // variation in ureappm as given by another component
        [Output]
        [Units("mg/kg")]
        double[] dlt_ureappm
        {
            set
            {
                // for now any incoming dlt is passed to all patches, this will have to be handled differently in the future
                for (int layer = 0; layer < value.Length; ++layer)
                    value[layer] = MathUtility.Divide(value[layer], convFactor_kgha2ppm(layer), 0.0);  // convert from ppm to kg/ha
                for (int k = 0; k < Patch.Count; k++)
                    Patch[k].dlt_urea = value;
            }
        }

        // variation in urea as given by another component
        [Output]
        [Units("kgN/ha")]
        double[] dlt_urea
        {
            set
            {
                // for now any incoming dlt is passed to all patches, this will have to be handled differently in the future
                for (int k = 0; k < Patch.Count; k++)
                    Patch[k].dlt_urea = value;
            }
        }

        // variation in nh4ppm as given by another component
        [Output]
        [Units("mg/kg")]
        double[] dlt_nh4ppm
        {
            set
            {
                // for now any incoming dlt is passed to all patches, this will have to be handled differently in the future
                for (int layer = 0; layer < value.Length; ++layer)
                    value[layer] = MathUtility.Divide(value[layer], convFactor_kgha2ppm(layer), 0.0);  // convert from ppm to kg/ha
                for (int k = 0; k < Patch.Count; k++)
                    Patch[k].dlt_nh4 = value;
            }
        }

        // variation in nh4 as given by another component
        [Output]
        [Units("kgN/ha")]
        double[] dlt_nh4
        {
            set
            {
                // for now any incoming dlt is passed to all patches, this will have to be handled differently in the future
                for (int k = 0; k < Patch.Count; k++)
                    Patch[k].dlt_nh4 = value;
            }
        }

        // variation in no3ppm as given by another component
        [Output]
        [Units("mg/kg")]
        double[] dlt_no3ppm
        {
            set
            {
                // for now any incoming dlt is passed to all patches, this will have to be handled differently in the future
                for (int layer = 0; layer < value.Length; ++layer)
                    value[layer] = MathUtility.Divide(value[layer], convFactor_kgha2ppm(layer), 0.0);  // convert from ppm to kg/ha
                for (int k = 0; k < Patch.Count; k++)
                    Patch[k].dlt_no3 = value;
            }
        }

        // variation in no3 as given by another component
        [Output]
        [Units("kgN/ha")]
        double[] dlt_no3
        {
            set
            {
                // for now any incoming dlt is passed to all patches, this will have to be handled differently in the future
                for (int k = 0; k < Patch.Count; k++)
                    Patch[k].dlt_no3 = value;
            }
        }

        #endregion

        #region Organic N and C

        [Output]
        [Units("kgN/ha")]
        double[] dlt_org_n
        {
            set
            {
                // for now any incoming dlt is passed to all patches, this might have to be handled differently in the future
                for (int k = 0; k < Patch.Count; k++)
                    Patch[k].dlt_org_n = value;
            }
        }

        [Output]
        [Units("kg/ha")]
        double[] dlt_org_c_pool1
        {
            set
            {
                // for now any incoming dlt is passed to all patches, this might have to be handled differently in the future
                for (int k = 0; k < Patch.Count; k++)
                    Patch[k].dlt_org_c_pool1 = value;
            }
        }

        [Output]
        [Units("kg/ha")]
        double[] dlt_org_c_pool2
        {
            set
            {
                // for now any incoming dlt is passed to all patches, this might have to be handled differently in the future
                for (int k = 0; k < Patch.Count; k++)
                    Patch[k].dlt_org_c_pool2 = value;
            }
        }

        [Output]
        [Units("kg/ha")]
        double[] dlt_org_c_pool3
        {
            set
            {
                // for now any incoming dlt is passed to all patches, this might have to be handled differently in the future
                for (int k = 0; k < Patch.Count; k++)
                    Patch[k].dlt_org_c_pool3 = value;
            }
        }

        #endregion

        #endregion

        #endregion

        #region Outputs we make available to other components

        #region Values that other components can get or set

        [Output]
        [Units("kg/ha")]
        [Description("Not fully implemented")]
        double[] org_c_pool1 // Doesn't seem to be fully implemented
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].fom_c_pool1[layer] * Patch[k].PatchArea;
                return result;
            }
            set
            {
                if (value.Length == dlayer.Length)
                {
                    for (int layer = 0; layer < value.Length; ++layer)
                    {
                        if (value[layer] < 0)
                            throw new Exception("Value given for fom_c_pool1 is negative");
                        else
                            for (int k = 0; k < Patch.Count; k++)
                                Patch[k].fom_c_pool1[layer] = value[layer];
                    }
                }
            }
        }

        [Output]
        [Units("kg/ha")]
        [Description("Not fully implemented")]
        double[] org_c_pool2 // Doesn't seem to be fully implemented
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].fom_c_pool2[layer] * Patch[k].PatchArea;
                return result;
            }
            set
            {
                if (value.Length == dlayer.Length)
                {
                    for (int layer = 0; layer < value.Length; ++layer)
                    {
                        if (value[layer] < 0)
                            throw new Exception("Value given for fom_c_pool2 is negative");
                        else
                            for (int k = 0; k < Patch.Count; k++)
                                Patch[k].fom_c_pool2[layer] = value[layer];
                    }
                }
            }
        }

        [Output]
        [Units("kg/ha")]
        [Description("Not fully implemented")]
        double[] org_c_pool3 // Doesn't seem to be fully implemented
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].fom_c_pool3[layer] * Patch[k].PatchArea;
                return result;
            }
            set
            {
                if (value.Length == dlayer.Length)
                {
                    for (int layer = 0; layer < value.Length; ++layer)
                    {
                        if (value[layer] < 0)
                            throw new Exception("Value given for fom_c_pool3 is negative");
                        else
                            for (int k = 0; k < Patch.Count; k++)
                                Patch[k].fom_c_pool3[layer] = value[layer];
                    }
                }
            }
        }

        [Output]
        [Units("kg/ha")]
        [Description("Not fully implemented")]
        double[] org_n // Doesn't seem to be fully implemented
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].fom_n[layer] * Patch[k].PatchArea;
                return result;
            }
            set
            {
                if (value.Length == dlayer.Length)
                {
                    for (int layer = 0; layer < value.Length; ++layer)
                    {
                        if (value[layer] < 0)
                            throw new Exception("Value given for fom_n is negative");
                        else
                            for (int k = 0; k < Patch.Count; k++)
                                Patch[k].fom_n[layer] = value[layer];

                    }
                }
            }
        }

        #endregion

        #region Values that other components can only get

        #region Outputs for Nitrogen

        #region General values

        // minimum allowable Urea
        [Output]
        [Units("kgN/ha")]
        [Description("Minimum allowable urea")]
        double[] urea_min;

        // minimum allowable NH4
        [Output]
        [Units("kgN/ha")]
        [Description("Minimum allowable NH4")]
        double[] nh4_min;

        // minimum allowable NO3
        [Output]
        [Units("kgN/ha")]
        [Description("Minimum allowable NO3")]
        double[] no3_min;

        #endregion

        #region Changes for today - deltas

        // N carried out via sediment
        [Output]
        [Units("kg")]
        [Description("N loss carried in sediment")]
        double dlt_n_loss_in_sed
        {
            get
            {
                double result = 0.0;
                for (int k = 0; k < Patch.Count; k++)
                    result += Patch[k].dlt_n_loss_in_sed * Patch[k].PatchArea;
                return result;
            }
        }

        // net nh4 change today
        [Output]
        [Units("kgN/ha")]
        [Description("Net NH4 change today")]
        double[] dlt_nh4_net
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].dlt_nh4_net[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        // net NH4 transformation today
        [Output]
        [Units("kgN/ha")]
        [Description("Net NH4 transformation")]
        double[] nh4_transform_net
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].nh4_transform_net[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        // net no3 change today
        [Output]
        [Units("kgN/ha")]
        [Description("Net NO3 change today")]
        double[] dlt_no3_net
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].dlt_no3_net[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        // net NO3 transformation today
        [Output]
        [Units("kgN/ha")]
        [Description("Net NO3 transformation")]
        double[] no3_transform_net
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].no3_transform_net[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        [Output]
        [Units("kgN/ha")]
        [Description("Net N mineralised in soil")]
        double[] dlt_n_min         // net mineralisation
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; layer++)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].dlt_n_min[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        [Output]
        [Units("kgN/ha")]
        [Description("Net N mineralisation from residue decomposition")]
        double[] dlt_n_min_res
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; layer++)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].dlt_n_min_res[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        // Net NH4 mineralisation from residue decomposition
        [Output]
        [Units("kgN/ha")]
        [Description("Net convertion of NH4 for residue mineralisation/immobilisation")]
        double[] dlt_res_nh4_min
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].dlt_nh4_decomp[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        // Net NO3 mineralisation from residue decomposition
        [Output]
        [Units("kgN/ha")]
        [Description("Net convertion of NO3 for residue mineralisation/immobilisation")]
        double[] dlt_res_no3_min
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].dlt_no3_decomp[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        // net fom N mineralised (negative for immobilisation)
        [Output]
        [Units("kgN/ha")]
        [Description("Net FOM N mineralised, negative for immobilisation")]
        double[] dlt_fom_n_min
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].dlt_n_fom_2_min[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        // net humic N mineralised
        [Output]
        [Units("kgN/ha")]
        [Description("Net humic N mineralised, negative for immobilisation")]
        double[] dlt_hum_n_min
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].dlt_n_hum_2_min[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        // net biomass N mineralised
        [Output]
        [Units("kgN/ha")]
        [Description("Net biomass N mineralised")]
        double[] dlt_biom_n_min
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].dlt_n_biom_2_min[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        [Output]
        [Units("kgN/ha")]
        [Description("Total net N mineralised (soil plus residues)")]
        double[] dlt_n_min_tot
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; layer++)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].dlt_n_min_tot[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        // nitrogen coverted by hydrolysis (from urea to NH4)
        [Output]
        [Units("kgN/ha")]
        [Description("Nitrogen coverted by hydrolysis")]
        double[] dlt_urea_hydrol
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; layer++)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].dlt_urea_hydrolised[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        // nitrogen coverted by nitrification (from NH4 to either NO3 or N2O)
        [Output]
        [Units("kgN/ha")]
        [Description("Nitrogen coverted by nitrification")]
        double[] dlt_rntrf
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].dlt_nitrification[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        [Output]
        [Units("kgN/ha")]
        [Description("Nitrogen coverted by nitrification")]
        double[] nitrification
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].dlt_nitrification[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        // effective nitrogen coverted by nitrification (from NH4 to NO3)
        [Output]
        [Units("kgN/ha")]
        [Description("Effective nitrogen coverted by nitrification")]
        double[] effective_nitrification
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].effective_nitrification[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        // NH4 N denitrified
        [Output]
        [Units("kgN/ha")]
        [Description("NH4 N denitrified")]
        double[] dlt_nh4_dnit
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].dlt_nh4_dnit[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        // NO3 N denitrified
        [Output]
        [Units("kgN/ha")]
        [Description("NO3 N denitrified")]
        double[] dlt_no3_dnit
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].dlt_no3_dnit[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        // amount of N2O produced
        [Output]
        [Units("kgN/ha")]
        [Description("Amount of N2O produced")]
        double[] n2o_atm
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].n2o_atm[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        // amount of N2 produced
        [Output]
        [Units("kgN/ha")]
        [Description("Amount of N2 produced")]
        double[] n2_atm
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].n2_atm[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        [Output]
        [Units("kgN/ha")]
        [Description("N converted by denitrification")]
        double[] dnit
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; layer++)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].dnit[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        // excess N required above NH4 supply
        [Output]
        [Units("kgN/ha")]
        [Description("NH4 deficit for immobilisation")]
        double[] nh4_deficit_immob
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].nh4_deficit_immob[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        #endregion

        #region Amounts in various pools

        // nitrogen in FOM
        [Output]
        [Units("kgN/ha")]
        [Description("Nitrogen in FOM")]
        double[] fom_n
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].fom_n[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        [Output]
        [Units("kgN/ha")]
        [Description("Nitrogen in FOM pool 1")]
        double[] fom_n_pool1
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].fom_n_pool1[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        [Output]
        [Units("kgN/ha")]
        [Description("Nitrogen in FOM pool 2")]
        double[] fom_n_pool2
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].fom_n_pool2[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        [Output]
        [Units("kgN/ha")]
        [Description("Nitrogen in FOM pool 3")]
        double[] fom_n_pool3
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].fom_n_pool3[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        // Soil humic N
        [Output]
        [Units("kgN/ha")]
        [Description("Soil humic nitrogen")]
        double[] hum_n
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].hum_n[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        // Soil biomass nitrogen
        [Output]
        [Units("kgN/ha")]
        [Description("Soil biomass nitrogen")]
        double[] biom_n
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].biom_n[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        // total N in soil   
        [Output]
        [Units("kgN/ha")]
        [Description("Total N in soil")]
        double[] nit_tot
        {
            get
            {
                double[] result = null;
                if (dlayer != null)
                {
                    result = new double[dlayer.Length];
                    for (int layer = 0; layer < dlayer.Length; ++layer)
                        for (int k = 0; k < Patch.Count; k++)
                            result[layer] += Patch[k].nit_tot[layer] * Patch[k].PatchArea;
                }
                return result;
            }
        }

        #endregion

        #region Nitrogen balance

        [Output]
        [Description("Nitrogen balance")]
        double nitrogenbalance
        {
            get
            {
                double deltaN = SumDoubleArray(nit_tot) - dailyInitialN;  // variation in N today
                double losses = SumDoubleArray(dlt_nh4_dnit) + SumDoubleArray(dlt_no3_dnit);
                return -(losses + deltaN);
                // why leaching losses are not accounted and what about the inputs?
            }
        }

        #endregion

        #endregion

        #region Outputs for Carbon

        #region General values

        // number of fom types read
        [Output]
        [Description("Number of FOM types")]
        private int num_fom_types
        { get { return fom_types.Length; } }

        // carbohydrate fraction of FOM (0-1)
        [Output]
        [Description("Fraction of carbohydrate in FOM")]
        private double fr_carb
        { get { return fract_carb[fom_type]; } }

        // cellulose fraction of FOM (0-1)
        [Output]
        [Description("Fraction of cellulose in FOM")]
        private double fr_cell
        { get { return fract_cell[fom_type]; } }

        // lignin fraction of FOM (0-1)
        [Output]
        [Description("Fraction of lignin in FOM")]
        private double fr_lign
        { get { return fract_lign[fom_type]; } }

        #endregion

        #region Changes for today - deltas

        [Output]
        [Units("kg")]
        [Description("Carbon loss in sediment")]
        double dlt_c_loss_in_sed
        {
            get
            {
                double result = 0.0;
                for (int k = 0; k < Patch.Count; k++)
                    result += Patch[k].dlt_c_loss_in_sed * Patch[k].PatchArea;
                return result;
            }
        }

        [Output]
        [Units("kg/ha")]
        [Description("FOM C converted to humic")]
        double[] dlt_fom_c_hum  // fom C converted to humic (kg/ha)
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].dlt_fom_c_hum[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        [Output]
        [Units("kg/ha")]
        [Description("FOM C converted to biomass")]
        double[] dlt_fom_c_biom // fom C converted to biomass (kg/ha)
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].dlt_fom_c_biom[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        [Output]
        [Units("kg/ha")]
        [Description("FOM C lost to atmosphere")]
        double[] dlt_fom_c_atm  // fom C lost to atmosphere (kg/ha)
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].dlt_fom_c_atm[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        [Output]
        [Units("kg/ha")]
        [Description("Humic C converted to biomass")]
        double[] dlt_hum_c_biom
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].dlt_c_hum_2_biom[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        [Output]
        [Units("kg/ha")]
        [Description("Humic C lost to atmosphere")]
        double[] dlt_hum_c_atm
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].dlt_c_hum_2_atm[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        [Output]
        [Units("kg/ha")]
        [Description("Biomass C converted to humic")]
        double[] dlt_biom_c_hum
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].dlt_c_biom_2_hum[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        [Output]
        [Units("kg/ha")]
        [Description("Biomass C lost to atmosphere")]
        double[] dlt_biom_c_atm
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].dlt_c_biom_2_atm[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        [Output]
        [Units("kg/ha")]
        [Description("Carbon from residues converted to biomass")]
        double[] dlt_res_c_biom
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].dlt_res_c_biom[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        [Output]
        [Units("kg/ha")]
        [Description("Carbon from residues converted to humic")]
        double[] dlt_res_c_hum
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].dlt_res_c_hum[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        [Output]
        [Units("kg/ha")]
        [Description("Carbon from residues lost to atmosphere")]
        double[] dlt_res_c_atm
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].dlt_res_c_atm[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        [Output]
        [Units("kg/ha")]
        [Description("Delta FOM C pool in fraction 1")]
        double[] dlt_fom_c_pool1
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].dlt_fom_c_pool1[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        [Output]
        [Units("kg/ha")]
        [Description("Delta FOM C pool in fraction 2")]
        double[] dlt_fom_c_pool2
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].dlt_fom_c_pool2[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        [Output]
        [Units("kg/ha")]
        [Description("Delta FOM C pool in fraction 3")]
        double[] dlt_fom_c_pool3
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].dlt_fom_c_pool3[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        [Output]
        [Units("kg/ha")]
        [Description("Carbon from all residues to atmosphere")]
        double[] soilp_dlt_res_c_atm
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].soilp_dlt_res_c_atm[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        [Output]
        [Units("kg/ha")]
        [Description("Carbon from all residues to humic")]
        double[] soilp_dlt_res_c_hum
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].soilp_dlt_res_c_hum[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        [Output]
        [Units("kg/ha")]
        [Description("Carbon from all residues to biomass")]
        double[] soilp_dlt_res_c_biom
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].soilp_dlt_res_c_biom[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        #endregion

        #region Amounts in various pools

        // fresh organic C
        [Output]
        [Units("kg/ha")]
        [Description("Soil FOM C")]
        double[] fom_c
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].fom_c[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        [Output]
        [Units("kg/ha")]
        [Description("FOM C in pool 1")]
        double[] fom_c_pool1
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].fom_c_pool1[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        [Output]
        [Units("kg/ha")]
        [Description("FOM C in pool 2")]
        double[] fom_c_pool2
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].fom_c_pool2[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        [Output]
        [Units("kg/ha")]
        [Description("FOM C in pool 3")]
        double[] fom_c_pool3
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].fom_c_pool3[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        // Humic C
        [Output]
        [Units("kg/ha")]
        [Description("Soil humic C")]
        double[] hum_c
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].hum_c[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        // biomass carbon 
        [Output]
        [Units("kg/ha")]
        [Description("Soil biomass C")]
        double[] biom_c
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].biom_c[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        // total carbon in soil
        [Output]
        [Units("kg/ha")]
        [Description("Total soil carbon")]
        double[] carbon_tot
        {
            get
            {
                double[] result = null;
                if (dlayer != null)
                {
                    result = new double[dlayer.Length];
                    for (int layer = 0; layer < dlayer.Length; ++layer)
                        for (int k = 0; k < Patch.Count; k++)
                            result[layer] += Patch[k].carbon_tot[layer] * Patch[k].PatchArea;
                }
                return result;
            }
        }

        #endregion

        #region Carbon Balance

        [Output]
        [Description("Carbon balance")]
        double carbonbalance
        {
            get
            {
                double deltaC = SumDoubleArray(carbon_tot) - dailyInitialC;     // variation in C today
                double losses = SumDoubleArray(dlt_res_c_atm) + SumDoubleArray(dlt_fom_c_atm) + SumDoubleArray(dlt_hum_c_atm) + SumDoubleArray(dlt_biom_c_atm);
                return -(losses + deltaC);
            }
        }

        #endregion

        #endregion

        #region Factors and other outputs

        [Output]
        [Units("kg/ha")]
        [Description("P coverted by residue mineralisation (needed by SoilP)")]
        double[] soilp_dlt_org_p
        {
            get
            {
                double[] result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].soilp_dlt_org_p[layer] * Patch[k].PatchArea;
                return result;
            }
        }

        [Output]
        [Units("oC")]
        [Description("Soil temperature")]
        double[] st;

        [Output]
        [Description("Temperature factor for nitrification and mineralisation")]
        double[] tf
        {
            get
            {
                double[] result = new double[dlayer.Length];
                // RCichota: deactivated
                //int index = (!is_pond_active) ? 1 : 2;
                //for (int layer = 0; layer < dlayer.Length; layer++)
                //    result[layer] = (soiltype == "rothc") ? RothcTF(layer, index) : TF(layer, index);
                return result;
            }
        }

        [Output]
        [Description("Number of internal patches")]
        int numPatches
        { get { return Patch.Count; } }

        [Output]
        [Description("Relative area of each internal patch")]
        double[] PatchArea
        {
            get
            {
                double[] result = new double[Patch.Count];
                for (int k = 0; k < Patch.Count; k++)
                    result[k] = Patch[k].PatchArea;
                return result;
            }
        }


        #endregion

        #endregion

        #endregion

        #region Useful constants

        // An "epsilon" value for single-precision floating point
        // We use this since other components are likely to use single-precision math
        private double epsilon = Math.Pow(2, -24);

        #endregion

        #region Internal variables

        private bool initDone = false;                  // marker for whether initialisation has been finished
        private bool inReset = false;                   // marker for whether a reset is going on
        private bool use_external_st = false;           // marker for whether external soil temperature is supplied
        private bool use_external_ph = false;           // marker for whether external ph is supplied, otherwise default is used
        private bool is_pond_active = false;            // marker for whether there is a pond, decomposition od surface OM will be done by that model
        private double dailyInitialC;                   // Total C content at the beginning of the day
        private double dailyInitialN;                   // Total N content at the beginning of the day
        private int fom_type;                           // Type of fom
        private int num_residues = 0;                   // number of residues decomposing

        private simpleSoilTemp simpleST;                // the internal soil temp module - to be avoided

        private struct PatchIDs
        {
            // list with patch ids to merge
            public List<int> disappearing;
            public List<int> recipient;
        }

        public struct TempFactorData
        {
            // the parameters to compute the temperature factor
            public double[] TempOptimum;
            public double[] FactorAtZero;
            public double[] CurveExponent;
        }

        private struct XYData
        {
            // lists with value of x and y used to describe certain functions (water factor, for ex.)
            public double[] xVals;
            public double[] yVals;
        }

        #endregion

    }
