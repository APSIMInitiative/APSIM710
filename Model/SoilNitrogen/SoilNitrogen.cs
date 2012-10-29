using System;
using System.Reflection;
using System.Collections.Generic;
using System.Text;
using ModelFramework;
using CSGeneral;
using System.Xml;

/// <summary>
/// Initially ported from Fortran SoilN model by Eric Zurcher Sept/Oct-2010.
/// Code tidied up by RCichota on Aug/Sep-2012: mostly modifying how some variables are handled (substitute 'get's by [input]), added regions to ease access,
///   updated error messages, moved all soilTemp code to a separate class (the idea is to eliminate it in the future), also added some of the constants to xml.
/// Changes on Sep/Oct-2012 by RCichota, add patch capability: move all code for soil C and N to a separate class (SoilCNPatch), allow several instances to be
///   initialised, modified inputs to handle the partitioning of incoming N, also modified outputs to sum up the pools from the several instances (patches)
///   
/// 
/// </summary>

public class SoilNitrogen
{

    [Link]
    Component My = null;

    List<soilCNPatch> Patch;

    public SoilNitrogen()
    {
        Patch = new List<soilCNPatch>();
        soilCNPatch newPatch = new soilCNPatch();
        Patch.Add(newPatch);
        Patch[0].PatchArea = 1.0;
        Patch[0].PatchName = "base";
    }

    #region Parameters used to initialise the model

    #region Parameters expected to be provided by the user or by APSIM

    // soil model type, spec used to determine some mineralisation model variations
    private string SoilN_MinerModel = "standard";
    [Param(IsOptional = true)]
    private string soiltype
    {
        get { return SoilN_MinerModel; }
        set { SoilN_MinerModel = value; }
    }


    #region parameters to initialise soilph

    // pH of soil (assumed equivalent to a 1:1 soil-water slurry)
    [Param(IsOptional = true, MinVal = 3.5, MaxVal = 11.0)]
    [Input(IsOptional = true)]
    private double[] ph = null;

    #endregion

    #region parameters to initialise fresh organic matter (fom)

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

    #endregion

    #region parameters to initialise soil organic matter (som)

    // initial ratio of biomass-C to mineralizable humic-C (0-1)
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    private double[] fbiom = null;

    // initial proportion of total soil C that is not subject to mineralisation (0-1)
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    private double[] finert = null;

    // the soil C:N ratio (actually of humus)
    private double hum_cn = 0.0;
    [Param(MinVal = 1.0, MaxVal = 25.0)]
    private double soil_cn
    {
        get { return hum_cn; }
        set { hum_cn = value; }
    }

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

    #region values to initialise soil mineral N

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
    //private double[] _urea;     // Internal variable holding the urea amounts
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
    //private double[] _nh4;     // Internal variable holding the nh4 amounts
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
    //private double[] _no3 = null;
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

    #endregion

    #region Parameters not usually provided by the user

    // minimum allowable Urea (ppm)
    [Param(MinVal = 0.0, MaxVal = 1000.0)]
    private double ureappm_min = 0.0;

    // minimum allowable NH4 (ppm)
    [Param(MinVal = 0.0, MaxVal = 1000.0)]
    private double nh4ppm_min = 0.0;

    // minimum allowable NO3 (ppm)
    [Param(MinVal = 0.0, MaxVal = 1000.0)]
    private double no3ppm_min = 0.0;

    // enrichment equation coefficient a
    [Param]
    private double enr_a_coeff = 0.0;

    // enrichment equation coefficient b
    [Param]
    private double enr_b_coeff = 0.0;

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

    // C:N ratio of microbes ()
    private double biom_cn = 8.0;
    [Param(IsOptional = true, MinVal = 1.0, MaxVal = 50.0)]
    private double mcn
    {
        get { return biom_cn; }
        set { biom_cn = value; }
    }

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    private double ef_fom;               // fraction of FOM C mineralised retained in system (0-1)   

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    private double fr_fom_biom;          // fraction of retained FOM C transferred to biomass (0-1)

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    private double ef_biom;              // fraction of biomass C mineralised retained in system (0-1)

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    private double fr_biom_biom;         // fraction of retained biomass C returned to biomass (0-1)

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    private double ef_hum;               // fraction of humic C mineralised retained in system (0-1)

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    private double[] rd_biom = null;     // potential rate of soil biomass mineralisation (per day)

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    private double[] rd_hum = null;      // potential rate of humus mineralisation (per day)

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    private double ef_res;               // fraction of residue C mineralised retained in system (0-1)

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    private double fr_res_biom;          // fraction of retained residue C transferred to biomass (0-1)

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    private double[] rd_carb;            // maximum rate constants for decomposition of FOM pools [carbohydrate component] (0-1)

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    private double[] rd_cell;            // maximum rate constants for decomposition of FOM pools [cellulose component] (0-1)

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    private double[] rd_lign;            // maximum rate constants for decomposition of FOM pools [lignin component] (0-1)

    [Param(Name = "fom_type")]
    private String[] fom_types;           // list of fom types

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Description("Fraction of carbohydrate in FOM, for each FOM type")]
    private double[] fract_carb;            // carbohydrate fraction of FOM (0-1)          

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Description("Fraction of cellulose in FOM, for each FOM type")]
    private double[] fract_cell;            // cellulose fraction of FOM (0-1)          

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Description("Fraction of lignin in FOM, for each FOM type")]
    private double[] fract_lign;            // lignin fraction of FOM (0-1)          

    [Param(MinVal = 0.0, MaxVal = 3.0)]
    private double oc2om_factor;         // conversion from OC to OM

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    private double fom_min;              // minimum allowable FOM (kg/ha)

    [Param(MinVal = 0.0, MaxVal = 1000.0)]
    private double min_depth;            // depth from which mineral N can be immobilised by decomposing residues (mm)

    [Param(MinVal = 0.0, MaxVal = 10.0)]
    private double cnrf_coeff;           // coeff. to determine the magnitude of C:N effects on decomposition of FOM ()

    [Param(MinVal = 5.0, MaxVal = 100.0)]
    private double cnrf_optcn;           // C:N above which decomposition rate of FOM declines ()

    [Param(MinVal = 5.0, MaxVal = 100.0)]
    private double[] opt_temp;           // Soil temperature above which there is no further effect on mineralisation and nitrification (oC)

    [Param(MinVal = 0.0, MaxVal = 2.0)]
    private double[] wfmin_index;        // index specifying water content for water factor for mineralisation

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    private double[] wfmin_values;       // value of water factor(mineralisation) function at given index values

    [Param(MinVal = 0.0, MaxVal = 2.0)]
    private double[] wfnit_index;        // index specifying water content for water factor for nitrification

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    private double[] wfnit_values;       // value of water factor(nitrification) function at given index values

    [Param(MinVal = 0.0, MaxVal = 100.0)]
    private double nitrification_pot;    // Potential nitrification by soil (ppm)

    [Param(MinVal = 0.0, MaxVal = 200.0)]
    private double nh4_at_half_pot;      // nh4 conc at half potential (ppm)   

    [Param(MinVal = 0.0, MaxVal = 14.0)]
    private double[] pHf_nit_pH;         // pH values for specifying pH factor for nitrification

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    private double[] pHf_nit_values;     // value of pH factor(nitrification) function for given pH values

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    private double dnit_rate_coeff;      // denitrification rate coefficient (kg/mg)

    [Param(MinVal = 0.0, MaxVal = 5.0)]
    private double dnit_wf_power;        // denitrification water factor power term

    [Param(MinVal = 0.0, MaxVal = 100.0)]
    private double dnit_k1;              // K1 parameter from Thorburn et al (2010) for N2O model

    [Param(MinVal = 0.0, MaxVal = 100.0)]
    private double[] dnit_wfps;            // WFPS for calculating the n2o fraction of denitrification

    [Param(MinVal = 0.0, MaxVal = 100.0)]
    private double[] dnit_n2o_factor;      // WFPS factor for n2o fraction of denitrification

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    private double dnit_nitrf_loss;      // Fraction of nitrification lost as denitrification

    #endregion

    #region Parameters added by RCichota

    // marker for what set of functions will be used (original or new)
    private bool useNewFunctions = false;
    [Param]
    private string functions2use
    { set { useNewFunctions = value.ToLower().Contains("new"); } }

    // minimum relative area (fraction of paddock) for any patch
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    private double minPatchArea = 0.001;

    #region Parameters for hydrolisys process

    // optimum temperature for urea hydrolisys
    private TempFactorData TempFactor_Hydrol = new TempFactorData();
    [Param]
    private double[] stf_hydrol_p1
    { set { TempFactor_Hydrol.TempOptimum = value; } }

    // temperature factor for urea hydrolisys at zero degrees
    [Param]
    private double[] stf_hydrol_p2
    { set { TempFactor_Hydrol.FactorAtZero = value; } }

    // curve exponent for temperature factor for urea hydrolisys
    [Param]
    private double[] stf_hydrol_p3
    { set { TempFactor_Hydrol.CurveExponent = value; } }

    // parameters for soil moisture factor for hydrolisys
    private XYData MoistFactor_Hydrol = new XYData();
    [Param]
    private double[] swf_hydrol_x
    { set { MoistFactor_Hydrol.xVals = value; } }
    [Param]
    private double[] swf_hydrol_y
    { set { MoistFactor_Hydrol.yVals = value; } }

    // parameters for potential urea hydrolisys
    [Param]
    private double pot_hydrol_min;  // minimum value

    [Param]
    private double pot_hydrol_termA;

    [Param]
    private double pot_hydrol_termB;

    [Param]
    private double pot_hydrol_termC;

    [Param]
    private double pot_hydrol_termD;

    #endregion

    #region Parameters for nitrification process

    // optimum temperature for nitrification
    private TempFactorData TempFactor_Nitrif = new TempFactorData();
    [Param]
    private double[] stf_nitrif_p1
    { set { TempFactor_Nitrif.TempOptimum = value; } }

    // temperature factor for nitrification at zero degrees
    [Param]
    private double[] stf_nitrif_p2
    { set { TempFactor_Nitrif.FactorAtZero = value; } }

    // curve exponent for temperature factor for nitrification
    [Param]
    private double[] stf_nitrif_p3
    { set { TempFactor_Nitrif.CurveExponent = value; } }

    // parameters for soil moisture factor for nitrification
    private XYData MoistFactor_Nitrif = new XYData();
    [Param]
    private double[] swf_nitrif_x
    { set { MoistFactor_Nitrif.xVals = value; } }
    [Param]
    private double[] swf_nitrif_y
    { set { MoistFactor_Nitrif.yVals = value; } }

    // parameters for soil pH factor for nitrification
    private XYData pHFactor_Nitrif = new XYData();
    [Param]
    private double[] sphf_nitrif_x
    { set { pHFactor_Nitrif.xVals = value; } }
    [Param]
    private double[] sphf_nitrif_y
    { set { pHFactor_Nitrif.yVals = value; } }

    #endregion

    #region Parameters for denitrification and N2O emission processes

    // optimum temperature for denitrification
    private TempFactorData TempFactor_Denit = new TempFactorData();
    [Param]
    private double[] stf_denit_p1
    { set { TempFactor_Denit.TempOptimum = value; } }

    // temperature factor for denitrification at zero degrees
    [Param]
    private double[] stf_denit_p2
    { set { TempFactor_Denit.FactorAtZero = value; } }

    // curve exponent for temperature factor for denitrification
    [Param]
    private double[] stf_denit_p3
    { set { TempFactor_Denit.CurveExponent = value; } }

    // parameters for soil moisture factor for denitrification
    private XYData MoistFactor_Denit = new XYData();
    [Param]
    private double[] swf_denit_x
    { set { MoistFactor_Denit.xVals = value; } }
    [Param]
    private double[] swf_denit_y
    { set { MoistFactor_Denit.yVals = value; } }

    // parameter for TermA in N2N2O function
    [Param]
    private double dnit_A;

    // parameter for TermB in N2N2O function
    [Param]
    private double dnit_B;

    // parameter for TermC in N2N2O function
    [Param]
    private double dnit_C;

    // parameter 1 to compute active carbon (for denitrification)
    [Param]
    private double actC_p1;

    // parameter 2 to compute active carbon (for denitrification)
    [Param]
    private double actC_p2;

    #endregion

    #region Parameters for mineralisation/immobilisation process

    // whether mineralisation factors use single calculation of for each type
    private bool useSingleMinerFactors = false;
    [Param]
    private string useSingleFactors
    { set { useSingleMinerFactors = value.ToLower().Contains("yes"); } }

    // optimum temperature for OM mineralisation
    private TempFactorData TempFactor_Miner = new TempFactorData();
    [Param]
    private double[] stf_miner_p1
    { set { TempFactor_Miner.TempOptimum = value; } }

    // temperature factor for OM mineralisation at zero degrees
    [Param]
    private double[] stf_miner_p2
    { set { TempFactor_Miner.FactorAtZero = value; } }

    // curve exponent for temperature factor for OM mineralisation
    [Param]
    private double[] stf_miner_p3
    { set { TempFactor_Miner.CurveExponent = value; } }

    // parameters for soil moisture factor for OM mineralisation
    private XYData MoistFactor_Miner = new XYData();
    [Param]
    private double[] swf_miner_x
    { set { MoistFactor_Miner.xVals = value; } }
    [Param]
    private double[] swf_miner_y
    { set { MoistFactor_Miner.yVals = value; } }

    // parameters for C:N factor for OM mineralisation
    double CNFactorMiner_Opt;
    [Param]
    private double cnf_miner_p1
    { set { CNFactorMiner_Opt = value; } }
    double CNFactorMiner_rate;
    [Param]
    private double cnf_miner_p2
    { set { CNFactorMiner_rate = value; } }

    #region Parameters for especifity OM types

    // optimum temperature for mineralisation of humus
    private TempFactorData TempFactor_minerHum = new TempFactorData();
    [Param]
    private double[] stf_minerHum_p1
    { set { TempFactor_minerHum.TempOptimum = value; } }

    // temperature factor for mineralisation of humus at zero degrees
    [Param]
    private double[] stf_minerHum_p2
    { set { TempFactor_minerHum.FactorAtZero = value; } }

    // curve exponent for temperature factor for mineralisation of humus
    [Param]
    private double[] stf_minerHum_p3
    { set { TempFactor_minerHum.CurveExponent = value; } }

    // parameters for soil moisture factor for mineralisation of humus
    private XYData MoistFactor_minerHum = new XYData();
    [Param]
    private double[] swf_minerHum_x
    { set { MoistFactor_minerHum.xVals = value; } }
    [Param]
    private double[] swf_minerHum_y
    { set { MoistFactor_minerHum.yVals = value; } }

    // optimum temperature for mineralisation of OM biomass
    private TempFactorData TempFactor_minerBiom = new TempFactorData();
    [Param]
    private double[] stf_minerBiom_p1
    { set { TempFactor_minerBiom.TempOptimum = value; } }

    // temperature factor for mineralisation of OM biomass at zero degrees
    [Param]
    private double[] stf_minerBiom_p2
    { set { TempFactor_minerBiom.FactorAtZero = value; } }

    // curve exponent for temperature factor for mineralisation of OM biomass
    [Param]
    private double[] stf_minerBiom_p3
    { set { TempFactor_minerBiom.CurveExponent = value; } }

    // parameters for soil moisture factor for mineralisation of OM biomass
    private XYData MoistFactor_minerBiom = new XYData();
    [Param]
    private double[] swf_minerBiom_x
    { set { MoistFactor_minerBiom.xVals = value; } }
    [Param]
    private double[] swf_minerBiom_y
    { set { MoistFactor_minerBiom.yVals = value; } }

    // optimum temperature for mineralisation of FOM
    private TempFactorData TempFactor_minerFOM = new TempFactorData();
    [Param]
    private double[] stf_minerFOM_p1
    { set { TempFactor_minerFOM.TempOptimum = value; } }

    // temperature factor for mineralisation of FOM at zero degrees
    [Param]
    private double[] stf_minerFOM_p2
    { set { TempFactor_minerFOM.FactorAtZero = value; } }

    // curve exponent for temperature factor for mineralisation of FOM
    [Param]
    private double[] stf_minerFOM_p3
    { set { TempFactor_minerFOM.CurveExponent = value; } }

    // parameters for soil moisture factor for mineralisation of FOM
    private XYData MoistFactor_minerFOM = new XYData();
    [Param]
    private double[] swf_minerFOM_x
    { set { MoistFactor_minerFOM.xVals = value; } }
    [Param]
    private double[] swf_minerFOM_y
    { set { MoistFactor_minerFOM.yVals = value; } }

    // parameters for C:N factor mineralisation of FOM
    double CNFactorMinerFOM_Opt;
    [Param]
    private double cnf_minerFOM_p1
    { set { CNFactorMinerFOM_Opt = value; } }
    double CNFactorMinerFOM_rate;
    [Param]
    private double cnf_minerFOM_p2
    { set { CNFactorMinerFOM_rate = value; } }

    #endregion

    #endregion

    #endregion

    #endregion

    #region Values we obtain from other components

    #region values needed by simpleSoilTemp

    // local latitude
    [Input(IsOptional = true)]
    [Units("deg")]
    private double latitude = -999.0;

    // today's date
    [Input]
    DateTime today;

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

    #region Soil data

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
    private double[] ave_soil_temp
    {
        get { return null; }
        set { st = value; }
    }

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
    //    [Output]
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

    #region organic N and C

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

    #region Useful constants

    // weight fraction of C in carbohydrates
    private const float c_in_fom = 0.4F;

    // An "epsilon" value for single-precision floating point
    // We use this since other components are likely to use single-precision math
    private double epsilon = Math.Pow(2, -24);

    #endregion

    #region  Various internal variables

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

    #region Events which we publish

    [Event]
    public event ExternalMassFlowDelegate ExternalMassFlow;

    [Event]
    public event NewSoluteDelegate new_solute;

    [Event]
    public event SurfaceOrganicMatterDecompDelegate actualresiduedecompositioncalculated;

    #endregion

    #region Events to which we subscribe, and their handlers

    [EventHandler]
    public void OnInitialised()
    {
        // +  Purpose:
        //      Performs the initial checks and setup

        // set the size of arrays
        ResizeLayerArrays(dlayer.Length);
        foreach (soilCNPatch aPatch in Patch)
            aPatch.ResizeLayerArrays(dlayer.Length);

        // check few initialisation parameters
        CheckParams();

        // create first patch
        InitialisePatch(0);

        // perform initial calculations and setup
        InitCalc();

        // initialise soil temperature
        if (use_external_st)
            st = ave_soil_temp;
        else
        {
            simpleST = new simpleSoilTemp(latitude, tav, amp, mint, maxt);
            st = simpleST.SoilTemperature(today, mint, maxt, radn, salb, dlayer, bd, ll15_dep, sw_dep);
        }

        // notifify apsim about solutes
        AdvertiseMySolutes();

        // print SoilN report
        WriteSummaryReport();
    }

    [EventHandler(EventName = "reset")]
    public void OnReset()
    {
        // +  Purpose:
        //      Reset those values that the user may have changed since initialisation

        inReset = true;

        // Save present state
        SaveState();

        // Zero internal state variables - not used yet
        ZeroVariables();

        // reset the size of arrays - so it zeroes them
        ResizeLayerArrays(dlayer.Length);

        // reset patches
        Patch.Clear();
        soilCNPatch newPatch = new soilCNPatch();
        Patch.Add(newPatch);

        foreach (soilCNPatch aPatch in Patch)
            aPatch.ResizeLayerArrays(dlayer.Length);

        InitialisePatch(0);

        // reset C and N variables to their initial state
        oc = OC_reset;
        no3ppm = no3ppm_reset;
        nh4ppm = nh4ppm_reset;
        ureappm = ureappm_reset;

        // perform initial calculations and setup
        InitCalc();

        // reset soil temperature
        if (use_external_st)
            st = ave_soil_temp;
        else
        {
            simpleST = new simpleSoilTemp(latitude, tav, amp, mint, maxt);
            st = simpleST.SoilTemperature(today, mint, maxt, radn, salb, dlayer, bd, ll15_dep, sw_dep);
        }

        // get the changes of state and publish (let other component to know)
        DeltaState();

        // print SoilN report
        WriteSummaryReport();

        inReset = false;
    }

    [EventHandler(EventName = "process")]
    public void OnProcess()
    {
        // +  Purpose:
        //      Performs every-day calculations - main process phase

        // update soil temperature
        if (use_external_st)
            st = ave_soil_temp;
        else
            st = simpleST.SoilTemperature(today, mint, maxt, radn, salb, dlayer, bd, ll15_dep, sw_dep);

        // update patch data
        UpdatePatches();

        // calculate C and N processes
        Process();

        // send actual decomposition back to surface OM
        if (!is_pond_active)
            SendActualResidueDecompositionCalculated();
    }

    [EventHandler(EventName = "post")]
    public void OnPost()
    {
        // +  Purpose:
        //      Performs every-day calculations - end of day processes

        if (Patch.Count > 10) // must set this to zero later
        {
            // we have more than one patch, check whether they are similar enough to be merged
            PatchIDs Patches = new PatchIDs();
            Patches = comparePatches();

            if (Patches.disappearing.Count > 0)
            {  // there are patches that will be merged
                for (int k = 0; k < Patches.disappearing.Count; k++)
                {
                    MergePatches(Patches.recipient[k], Patches.disappearing[k]);
                    Console.WriteLine(today.ToString("dd MMMM yyyy") + "(Day of year=" + today.DayOfYear.ToString() + "), SoilNitrogen.MergePatch:");
                    Console.WriteLine("   merging Patch(" + Patches.disappearing[k].ToString() + ") into Patch(" + Patches.recipient[k].ToString() +
                        "). New patch area = " + Patch[Patches.recipient[k]].PatchArea.ToString("#0.00#"));
                }
            }
        }
    }

    [EventHandler(EventName = "sum_report")]
    public void OnSum_report()
    {
        WriteSummaryReport();
    }

    [EventHandler(EventName = "tick")]
    public void OnTick(TimeType time)
    {
        // +  Purpose:
        //      Reset potential decomposition variables and get initial C and N status

        foreach (soilCNPatch aPatch in Patch)
            aPatch.OnTick();

        // Calculations for NEW sysbal component
        dailyInitialC = SumDoubleArray(carbon_tot);
        dailyInitialN = SumDoubleArray(nit_tot);
    }

    [EventHandler(EventName = "IncorpFOM")]
    public void OnIncorpFOM(FOMLayerType FOMdata)
    {
        // +  Purpose:
        //      Partition the given FOM C and N into fractions in each layer.
        //      In this event all FOM is given as one, so it will be assumed that the CN ratios of all fractions are equal

        foreach (soilCNPatch aPatch in Patch)
            aPatch.OnIncorpFOM(FOMdata);

        fom_type = Patch[0].fom_type;
    }

    [EventHandler(EventName = "IncorpFOMPool")]
    public void OnIncorpFOMPool(FOMPoolType FOMPoolData)
    {
        // +  Purpose:
        //      Partition the given FOM C and N into fractions in each layer.
        //      In this event each of the three pools is given

        foreach (soilCNPatch aPatch in Patch)
            aPatch.OnIncorpFOMPool(FOMPoolData);
    }

    [EventHandler(EventName = "PotentialResidueDecompositionCalculated")]
    public void OnPotentialResidueDecompositionCalculated(SurfaceOrganicMatterDecompType SurfaceOrganicMatterDecomp)
    {
        //+  Purpose
        //     Get information of potential residue decomposition

        foreach (soilCNPatch aPatch in Patch)
            aPatch.OnPotentialResidueDecompositionCalculated(SurfaceOrganicMatterDecomp);

        num_residues = SurfaceOrganicMatterDecomp.Pool.Length;
    }

    [EventHandler(EventName = "new_profile")]
    public void OnNew_profile(NewProfileType NewProfile)
    {
        //+  Purpose
        //     Consider soil profile changes - primarily due to by erosion (??)

        foreach (soilCNPatch aPatch in Patch)
            aPatch.OnNew_profile(NewProfile);
    }

    [EventHandler(EventName = "NitrogenChanged")]
    public void OnNitrogenChanged(NitrogenChangedType NitrogenChanges)
    {
        //+  Purpose
        //     Get the delta mineral N from other module
        //     Send deltas to each patch, if delta comes from soil or plant then the values are modified (partioned)
        //      based on N content. If sender is any other module then values are passed to patches as they come

        string module = NitrogenChanges.Sender.ToLower();
        if (module == "soilwat" || module == "agpasture" || module == "plant")
        {
            // values supplied by a module from which a different treatment for each patch is required,
            //  they will be partitioned according to the N content in each patch

            // 1- consider urea:
            if (hasValues(NitrogenChanges.DeltaUrea, epsilon))
            {
                // send incoming dlt to be partitioned amongst patches
                double[][] newDelta = partitionDelta(NitrogenChanges.DeltaUrea, "urea");
                // 1.1- send dlt's to each patch
                for (int k = 0; k < Patch.Count; k++)
                    Patch[k].dlt_urea = newDelta[k];
            }

            // 2- consider nh4:
            if (hasValues(NitrogenChanges.DeltaNH4, epsilon))
            {
                // send incoming dlt to be partitioned amongst patches
                double[][] newDelta = partitionDelta(NitrogenChanges.DeltaNH4, "nh4");
                // 2.1- send dlt's to each patch
                for (int k = 0; k < Patch.Count; k++)
                    Patch[k].dlt_nh4 = newDelta[k];
            }

             // 3- consider no3:
            if (hasValues(NitrogenChanges.DeltaNO3, epsilon))
            {
                // send incoming dlt to be partitioned amongst patches
                double[][] newDelta = partitionDelta(NitrogenChanges.DeltaNO3, "no3");
                // 3.1- send dlt's to each patch
                for (int k = 0; k < Patch.Count; k++)
                    Patch[k].dlt_no3 = newDelta[k];
            }
        }
        else
        {
            // values will passed to patches as they come
            for (int k = 0; k < Patch.Count; k++)
            {
                Patch[k].dlt_urea = NitrogenChanges.DeltaUrea;
                Patch[k].dlt_nh4 = NitrogenChanges.DeltaNH4;
                Patch[k].dlt_no3 = NitrogenChanges.DeltaNO3;
            }
        }
    }

    [EventHandler(EventName = "AddUrine")]
    public void OnAddUrine(AddUrineType UrineAdded)
    {
        //+  Purpose
        //     Add urine

        // Starting with the minimalist version. To be updated by Val's group to
        // include a urine patch algorithm

        // test for adding urine patches
        // if VolumePerUrination = 0.0 then no patch will be added, otherwise a patch will be added (based on 'base' patch)
        // assuming new PatchArea is passed as a fraction and this will be subtracted from original
        // urea will be added to the top layer for now

        double[] newUrea = new double[dlayer.Length];
        newUrea[0] = UrineAdded.Urea;
        
        if (UrineAdded.VolumePerUrination > 0.0)
        {
            SplitPatch(0);
            double oldArea = Patch[0].PatchArea;
            double newArea = oldArea - UrineAdded.AreaPerUrination;
            Patch[0].PatchArea = newArea;
            int k = Patch.Count - 1;  // make it explicit for now to ease reading
            Patch[k].PatchArea = UrineAdded.AreaPerUrination;
            Patch[k].PatchName = "Patch" + k.ToString();
            if (UrineAdded.Urea > epsilon)
                Patch[k].dlt_urea = newUrea;
        }
        else
            for (int k = 0; k < Patch.Count; k++)
                Patch[k].dlt_urea = newUrea;
    }

    [EventHandler(EventName = "AddUrinePatch")]
    public void OnAddUrinePatch(AddUrinePatchType PatchtoAdd)
    {
        //+  Purpose
        //     Handling the addition of urine patches

        // data passed with this event:
        //.Sender: the name of the module that raised this event
        //.DepositionType: the type of deposition:
        //  - Homogeneous: No patch is created, add urine as given to all patches. It is the default
        //  - ToSpecificPatch: No patch is created, add urine to selected patches (given by id or name if supplied, or default to Homogenous)
        //  - NewSimplePatch: create new patch(es) based on an existing patch (given by id or name if supplied, or by default the base/Patch[0])
        //  - NewOverlappingPatch, create new patch(es), overlaps with all existing patches, provided the new area is larger than a minimum (minPatchArea)
        //.AddToPatches_id: the index of the existing patches to which urine will be added
        //.AddToPatches_nm: the name of the existing patches to which urine will be added
        //.PatchArea: the relative area of the patch (0-1)
        //.UrineWater: amount of water to add, not handled here
        //.Urea: amount of urea to add, per layer

        double[] deltaUrea = new double[PatchtoAdd.Urea.Length];
        double totalUrea = SumDoubleArray(deltaUrea);

        //int[] fakeIDs = new int[PatchtoAdd.AddToPatches_id.Length];
        List<int> PatchesToDelete = new List<int>();

        if ((PatchtoAdd.DepositionType.ToLower() == "NewSimplePatch".ToLower()) || (PatchtoAdd.DepositionType.ToLower() == "NewOverlappingPatch".ToLower()))
        {
            // get the list of patch id's to which urine will be added, and the area affected
            int[] PatchIDs;
            double AreaAffected = 0;
            if (PatchtoAdd.DepositionType.ToLower() == "NewOverlappingPatch".ToLower())
            {
                PatchIDs = new int[Patch.Count - 1];
                for (int k = 0; k < Patch.Count; k++)
                    PatchIDs[k] = k;
                AreaAffected = 1.0;
            }
            else
            {
                PatchIDs = CheckPatchIDs(PatchtoAdd.AddToPatches_id, PatchtoAdd.AddToPatches_nm);
                for (int i = 0; i < PatchIDs.Length; i++)
                    AreaAffected += Patch[PatchIDs[i]].PatchArea;
            }

            for (int i = 0; i < PatchIDs.Length; i++)
            {
                // check the areas:
                double OldPatch_oldArea = Patch[PatchIDs[i]].PatchArea;
                double NewPatch_area = (OldPatch_oldArea / AreaAffected) * PatchtoAdd.PatchArea;
                double OldPatch_newArea = NewPatch_area - NewPatch_area;
                if (NewPatch_area < minPatchArea)
                {  // area to create is too small, patch will not be created
                    Console.WriteLine(today.ToString("dd MMMM yyyy") + "(Day of year=" + today.DayOfYear.ToString() + "), SoilNitrogen.AddPatch:");
                    Console.WriteLine("   attempt to create a new patch with area too small or negative (" + NewPatch_area.ToString("#0.00#") + "). The patch will not be created");
                }
                else if (OldPatch_newArea < minPatchArea)
                {  // negative or too small area, patch will be created but old one will be deleted
                    Console.WriteLine(today.ToString("dd MMMM yyyy") + "(Day of year=" + today.DayOfYear.ToString() + "), SoilNitrogen.AddPatch:");
                    Console.WriteLine(" attempt to set the area of existing patch(" + PatchIDs[i].ToString() + ") to a value too small or negative (" + OldPatch_newArea.ToString("#0.00#") + "). The patch will be eliminated");
                    
                    // mark old patch for deletion
                    PatchesToDelete.Add(PatchIDs[i]);

                    // create new patch by taking the old one
                    soilCNPatch OldPatch = Patch[PatchIDs[i]];
                    Patch.Add(OldPatch);
                    int k = Patch.Count - 1;
                    Patch[k].PatchName = "Patch" + k.ToString();
                    Patch[k].dlt_urea = deltaUrea;
                    Console.WriteLine(" create new patch, with area = " + OldPatch_oldArea.ToString("#0.00#") + ", based on extint patch(" + PatchIDs[i].ToString() +
                        "), area = " + OldPatch_oldArea.ToString("#0.00#"));
                }
                else
                {  // create new patch by spliting an existing one
                    SplitPatch(PatchIDs[i]);
                    Patch[PatchIDs[i]].PatchArea = OldPatch_newArea;
                    int k = Patch.Count - 1;
                    Patch[k].PatchArea = NewPatch_area;
                    Patch[k].PatchName = "Patch" + k.ToString();
                    Patch[k].dlt_urea = deltaUrea;
                    Console.WriteLine(today.ToString("dd MMMM yyyy") + "(Day of year=" + today.DayOfYear.ToString() + "), SoilNitrogen.AddPatch:");
                    Console.WriteLine(" create new patch, with area = " + NewPatch_area.ToString("#0.00#") + ", based on existing patch(" + PatchIDs[i].ToString() + 
                        ") - Old area = " + NewPatch_area.ToString("#0.00#") + ", new area = "+ NewPatch_area.ToString("#0.00#"));
                }
            }
        }
        else if (PatchtoAdd.DepositionType.ToLower() == "ToSpecificPatch".ToLower())
        {
        // add urine to selected patches, no new patch will be created
            // 1. check whether N is being added, if not skip this
            if (totalUrea > epsilon)
            {
                // 2. get the list of patch id's to which urine will be added
                int[] PatchIDs = CheckPatchIDs(PatchtoAdd.AddToPatches_id, PatchtoAdd.AddToPatches_nm);
                // 3. Add deltaUrea to each patch
                for (int i = 0; i < PatchIDs.Length; i++)
                    Patch[i].dlt_urea = deltaUrea;
            }
        }
        else
        // add urine to all existing patches, no new patch will be created
            // 1. check whether N is being added, if not skip this
            if (totalUrea > epsilon)
            {
                // 2. Add deltaUrea to each patch
                for (int k = 0; k < Patch.Count; k++)
                    Patch[k].dlt_urea = deltaUrea;
            }
    }

    #endregion

    #region Setup calculations

    private void CheckParams()
    {
        // +  Purpose:
        //      Check initialisation parameters and let user know

        Console.WriteLine();
        Console.WriteLine("        - Reading/checking parameters");

        Console.WriteLine("           - Using " + SoilN_MinerModel + " soil mineralisation specification");

        // check whether soil temperature is present. If not, check whether the basic params for simpleSoilTemp have been supplied
        if (AllowsimpleSoilTemp)
            use_external_st = (ave_soil_temp != null);
        if (!use_external_st)
        {
            if (latitude == -999.0)
                throw new Exception("Value for latitude was not supplied");
            if (tav == -999.0)
                throw new Exception("Value for TAV was not supplied");
            if (amp == -999.0)
                throw new Exception("Value for AMP was not supplied");
        }

        // check whether ph is supplied, use a default if not  - might be better to throw an exception?
        use_external_ph = (ph != null);
        if (!use_external_ph)
        {
            for (int layer = 0; layer < dlayer.Length; ++layer)
                ph[layer] = 6.0; // ph_ini
        }

        // convert minimum values for nh4 and no3 from ppm to kg/ha
        for (int layer = 0; layer < dlayer.Length; ++layer)
        {
            double convFact = convFactor_kgha2ppm(layer);
            urea_min[layer] = MathUtility.Divide(ureappm_min, convFact, 0.0);
            nh4_min[layer] = MathUtility.Divide(nh4ppm_min, convFact, 0.0);
            no3_min[layer] = MathUtility.Divide(no3ppm_min, convFact, 0.0);
        }

        // Check if all fom values have been supplied
        if (num_fom_types != fract_carb.Length)
            throw new Exception("Number of \"fract_carb\" different to \"fom_type\"");
        if (num_fom_types != fract_cell.Length)
            throw new Exception("Number of \"fract_cell\" different to \"fom_type\"");
        if (num_fom_types != fract_lign.Length)
            throw new Exception("Number of \"fract_lign\" different to \"fom_type\"");

        // Check if all C:N values have been supplied. If not use average C:N ratio in all pools
        if (fomPools_cn == null || fomPools_cn.Length < 3)
        {
            fomPools_cn = new double[3];
            for (int i = 0; i < 3; i++)
                fomPools_cn[i] = fom_ini_cn;
        }

        // Check if inital fom depth has been supplied, if not assume that initial fom is distributed over the whole profile
        if (fom_ini_depth == 0.0)
        {
            for (int i = 0; i < dlayer.Length; ++i)
                fom_ini_depth += dlayer[i];
        }
    }

    private void InitCalc()
    {
        // +  Purpose:
        //      Do the initial setup and calculations - also used onReset

        int nLayers = dlayer.Length;

        // Factor to distribute fom over the soil profile. Uses a exponential function and goes till the especified depth
        double[] fom_FracLayer = new double[nLayers];
        double cum_depth = 0.0;
        int deepest_layer = getCumulativeIndex(fom_ini_depth, dlayer);
        for (int layer = 0; layer <= deepest_layer; layer++)
        {
            fom_FracLayer[layer] = Math.Exp(-3.0 * Math.Min(1.0, MathUtility.Divide(cum_depth + dlayer[layer], fom_ini_depth, 0.0))) *
                Math.Min(1.0, MathUtility.Divide(fom_ini_depth - cum_depth, dlayer[layer], 0.0));
            cum_depth += dlayer[layer];
        }
        double fom_FracLayer_tot = SumDoubleArray(fom_FracLayer);

        // ensure initial OC has a value for each layer
        Array.Resize(ref OC_reset, nLayers);

        // Distribute an convert C an N values over the profile
        for (int layer = 0; layer < nLayers; layer++)
        {
            double convFact = convFactor_kgha2ppm(layer);
            double newValue = 0.0;
            // check and distribute the mineral nitrogen
            if (ureappm_reset != null)
            {
                newValue = MathUtility.Divide(ureappm_reset[layer], convFact, 0.0);       //Convert from ppm to convFactor_kgha2ppm/ha
                for (int k = 0; k < Patch.Count; k++)
                    Patch[k].urea[layer] = newValue;
            }
            newValue = MathUtility.Divide(nh4ppm_reset[layer], convFact, 0.0);       //Convert from ppm to convFactor_kgha2ppm/ha
            for (int k = 0; k < Patch.Count; k++)
                Patch[k].nh4[layer] = newValue;
            newValue = MathUtility.Divide(no3ppm_reset[layer], convFact, 0.0);       //Convert from ppm to convFactor_kgha2ppm/ha
            for (int k = 0; k < Patch.Count; k++)
                Patch[k].no3[layer] = newValue;

            // calculate total soil C
            double Soil_OC = OC_reset[layer] * 10000;     // = (oc/100)*1000000 - convert from % to ppm
            Soil_OC = MathUtility.Divide(Soil_OC, convFact, 0.0);  // kg/ha

            // calculate inert soil C
            double InertC = finert[layer] * Soil_OC;

            // calculate microbial biomass C and N
            double BiomassC = MathUtility.Divide((Soil_OC - InertC) * fbiom[layer], 1.0 + fbiom[layer], 0.0);
            double BiomassN = MathUtility.Divide(BiomassC, biom_cn, 0.0);

            // calculate C and N values for active humus
            double HumusC = Soil_OC - BiomassC;
            double HumusN = MathUtility.Divide(HumusC, hum_cn, 0.0);

            // distribute and calculate the fom N and C
            double fom = MathUtility.Divide(fom_ini_wt * fom_FracLayer[layer], fom_FracLayer_tot, 0.0);

            for (int k = 0; k < Patch.Count; k++)
            {
                Patch[k].inert_c[layer] = InertC;
                Patch[k].biom_c[layer] = BiomassC;
                Patch[k].biom_n[layer] = BiomassN;
                Patch[k].hum_c[layer] = HumusC;
                Patch[k].hum_n[layer] = HumusN;
                Patch[k].fom_c_pool1[layer] = fom * fract_carb[0] * c_in_fom;
                Patch[k].fom_c_pool2[layer] = fom * fract_cell[0] * c_in_fom;
                Patch[k].fom_c_pool3[layer] = fom * fract_lign[0] * c_in_fom;
                Patch[k].fom_n_pool1[layer] = MathUtility.Divide(Patch[k].fom_c_pool1[layer], root_cn_pool[0], 0.0);
                Patch[k].fom_n_pool2[layer] = MathUtility.Divide(Patch[k].fom_c_pool2[layer], root_cn_pool[1], 0.0);
                Patch[k].fom_n_pool3[layer] = MathUtility.Divide(Patch[k].fom_c_pool3[layer], root_cn_pool[2], 0.0);
            }

            // store today's values
            for (int k = 0; k < Patch.Count; k++)
                Patch[k].InitCalc();
        }

        // Calculations for NEW sysbal component
        dailyInitialC = SumDoubleArray(carbon_tot);
        dailyInitialN = SumDoubleArray(nit_tot);

        initDone = true;
    }

    private void ZeroVariables()
    {
        // +  Purpose:
        //      Wait and see whether this is needed
    }

    private void ResizeLayerArrays(int nLayers)
    {
        // +  Purpose:
        //      Set the size of all public arrays (with nLayers), this doesn't clear the existing values

        Array.Resize(ref st, nLayers);
        Array.Resize(ref urea_min, nLayers);
        Array.Resize(ref nh4_min, nLayers);
        Array.Resize(ref no3_min, nLayers);
    }

    private void AdvertiseMySolutes()
    {
        // + Purpose
        //    Notify any interested modules about this module's ownership of solute information.

        if (new_solute != null)
        {
            string[] solute_names;
            if (useOrganicSolutes)
            {
                solute_names = new string[7] { "urea", "nh4", "no3", "org_c_pool1", "org_c_pool2", "org_c_pool3", "org_n" };
            }
            else
            { // don't publish the organic solutes
                solute_names = new string[3] { "urea", "nh4", "no3" };
            }

            NewSoluteType SoluteData = new NewSoluteType();
            //data.sender_id = (int)ParentComponent().GetId();      I think it should have the sender's name as well
            SoluteData.solutes = solute_names;

            new_solute.Invoke(SoluteData);
        }
    }

    private void SaveState()
    {
        // +  Purpose:
        //      Calculates variations in C an N, needed for both NEW and OLD sysbal component

        dailyInitialN = SumDoubleArray(nit_tot);
        dailyInitialC = SumDoubleArray(carbon_tot);
    }

    private void DeltaState()
    {
        // +  Purpose:
        //      Calculates variations in C an N, and publishes Mass flows to apsim

        double dltN = SumDoubleArray(nit_tot) - dailyInitialN;
        double dltC = SumDoubleArray(carbon_tot) - dailyInitialC;

        SendExternalMassFlowN(dltN);
        SendExternalMassFlowC(dltC);
    }

    private void WriteSummaryReport()
    {
        // +  Purpose:
        //      Write report about setup and status of SoilNitrogen

        string myMessage = "";
        if (use_external_st)
            myMessage = "   - Soil temperature supplied by apsim";
        else
            myMessage = "   - Soil temperature calculated internally";
        Console.WriteLine("        " + myMessage);
        if (use_external_ph)
            myMessage = "   - Soil pH supplied by apsim";
        else
            myMessage = "   - Soil pH was not supplied, default value will be used";
        Console.WriteLine("        " + myMessage);
        Console.WriteLine();

        Console.Write(@"
                      Soil Profile Properties
          ------------------------------------------------
           Layer    pH    OC     NO3     NH4    Urea
                         (%) (kg/ha) (kg/ha) (kg/ha)
          ------------------------------------------------
");
        for (int layer = 0; layer < dlayer.Length; ++layer)
        {
            Console.WriteLine("          {0,4:d1}     {1,4:F2}  {2,4:F2}  {3,6:F2}  {4,6:F2}  {5,6:F2}",
            layer + 1, ph[layer], OC_reset[layer], no3[layer], nh4[layer], urea[layer]);
        }
        Console.WriteLine("          ------------------------------------------------");
        Console.WriteLine("           Totals              {0,6:F2}  {1,6:F2}  {2,6:F2}",
                  SumDoubleArray(no3), SumDoubleArray(nh4), SumDoubleArray(urea));
        Console.WriteLine("          ------------------------------------------------");
        Console.WriteLine();
        Console.Write(@"
                  Initial Soil Organic Matter Status
          ---------------------------------------------------------
           Layer      Hum-C   Hum-N  Biom-C  Biom-N   FOM-C   FOM-N
                    (kg/ha) (kg/ha) (kg/ha) (kg/ha) (kg/ha) (kg/ha)
          ---------------------------------------------------------
");

        double TotalFomC = 0.0;
        for (int layer = 0; layer < dlayer.Length; ++layer)
        {
            //double FomC = fom_c_pool1[layer] + fom_c_pool2[layer] + fom_c_pool3[layer];
            TotalFomC += fom_c[layer];
            Console.WriteLine("          {0,4:d1}   {1,10:F1}{2,8:F1}{3,8:F1}{4,8:F1}{5,8:F1}{6,8:F1}",
            layer + 1, hum_c[layer], hum_n[layer], biom_c[layer], biom_n[layer], fom_c[layer], fom_n[layer]);
        }
        Console.WriteLine("          ---------------------------------------------------------");
        Console.WriteLine("           Totals{0,10:F1}{1,8:F1}{2,8:F1}{3,8:F1}{4,8:F1}{5,8:F1}",
            SumDoubleArray(hum_c), SumDoubleArray(hum_n), SumDoubleArray(biom_c),
            SumDoubleArray(biom_n), TotalFomC, SumDoubleArray(fom_n));
        Console.WriteLine("          ---------------------------------------------------------");
        Console.WriteLine();
    }

    #endregion

    #region Methods to deal with patches

    private void InitialisePatch(int k)
    {
        // + Purpose
        //    set all variables within a single patch to defaul/initial values

        #region Values needed for initalisation only

        Patch[k].SoilN_MinerModel = SoilN_MinerModel;

        Patch[k].epsilon = epsilon;

        Patch[k].c_in_fom = c_in_fom;

        for (int layer = 0; layer < dlayer.Length; layer++)
        {
            Patch[k].urea_min[layer] = urea_min[layer];       // minimum allowable urea

            Patch[k].nh4_min[layer] = nh4_min[layer];       // minimum allowable NH4

            Patch[k].no3_min[layer] = no3_min[layer];       // minimum allowable NO3

            // initial ratio of biomass-C to mineralizable humic-C (0-1)
            Patch[k].fbiom[layer] = fbiom[layer];

            // initial proportion of total soil C that is not subject to mineralization (0-1)
            Patch[k].finert[layer] = finert[layer];
        }

        Patch[k].fom_ini_wt = fom_ini_wt;

        // initial depth over which fom is distributed within the soil profile (mm)
        Patch[k].fom_ini_depth = fom_ini_depth;

        // initial C:N ratio FOM pools
        Patch[k].fomPools_cn = fomPools_cn;

        Patch[k].enr_a_coeff = enr_a_coeff;    // enrichment equation coefficient a

        Patch[k].enr_b_coeff = enr_b_coeff; // enrichment equation coefficient b

        Patch[k].AllowProfileReduction = AllowProfileReduction;

        // marker for whether organic solute are to be simulated (always false as it is not implemented)
        Patch[k].useOrganicSolutes = useOrganicSolutes;

        // C:N ratio of microbes ()
        Patch[k].biom_cn = biom_cn;

        // the soil C:N ratio (actually of humus)
        Patch[k].hum_cn = hum_cn;

        Patch[k].ef_fom = ef_fom;               // fraction of FOM C mineralized retained in system (0-1)   

        Patch[k].fr_fom_biom = fr_fom_biom;          // fraction of retained FOM C transferred to biomass (0-1)

        Patch[k].ef_biom = ef_biom;              // fraction of biomass C mineralized retained in system (0-1)

        Patch[k].fr_biom_biom = fr_biom_biom;         // fraction of retained biomass C returned to biomass (0-1)

        Patch[k].ef_hum = ef_hum;               // fraction of humic C mineralized retained in system (0-1)

        Patch[k].ef_res = ef_res;               // fraction of residue C mineralized retained in system (0-1)

        Patch[k].fr_res_biom = fr_res_biom;          // fraction of retained residue C transferred to biomass (0-1)

        Patch[k].rd_biom = new double[2];
        Patch[k].rd_hum = new double[2];
        Patch[k].rd_carb = new double[2];
        Patch[k].rd_cell = new double[2];
        Patch[k].rd_lign = new double[2];
        Patch[k].opt_temp = new double[2];
        for (int index = 0; index < 2; index++)
        {
            Patch[k].rd_biom[index] = rd_biom[index];     // potential rate of soil biomass mineralization (per day)

            Patch[k].rd_hum[index] = rd_hum[index];      // potential rate of humus mineralization (per day)

            Patch[k].rd_carb[index] = rd_carb[index];            // maximum rate constants for decomposition of FOM pools [carbohydrate component] (0-1)

            Patch[k].rd_cell[index] = rd_cell[index];            // maximum rate constants for decomposition of FOM pools [cellulose component] (0-1)

            Patch[k].rd_lign[index] = rd_lign[index];            // maximum rate constants for decomposition of FOM pools [lignin component] (0-1)

            Patch[k].opt_temp[index] = opt_temp[index];           // Soil temperature above which there is no further effect on mineralisation and nitrification (oC)
        }

        int p = fom_types.Length;
        Patch[k].fom_types = new string[p];
        Patch[k].fract_carb = new double[p];
        Patch[k].fract_cell = new double[p];
        Patch[k].fract_lign = new double[p];
        for (int pool = 0; pool < fom_types.Length; pool++)
        {
            Patch[k].fom_types[pool] = fom_types[pool];           // list of fom types

            Patch[k].fract_carb[pool] = fract_carb[pool];            // carbohydrate fraction of FOM (0-1)          

            Patch[k].fract_cell[pool] = fract_cell[pool];            // cellulose fraction of FOM (0-1)          

            Patch[k].fract_lign[pool] = fract_lign[pool];            // lignin fraction of FOM (0-1)          
        }

        Patch[k].oc2om_factor = oc2om_factor;         // conversion from OC to OM

        Patch[k].fom_min = fom_min;              // minimum allowable FOM (kg/ha)

        Patch[k].min_depth = min_depth;            // depth from which mineral N can be immobilized by decomposing residues (mm)

        Patch[k].cnrf_coeff = cnrf_coeff;           // coeff. to determine the magnitude of C:N effects on decomposition of FOM ()

        Patch[k].cnrf_optcn = cnrf_optcn;           // C:N above which decomposition rate of FOM declines ()

        Patch[k].wfmin_index = new double[wfmin_index.Length];
        Patch[k].wfmin_values = new double[wfmin_index.Length];
        for (int i = 0; i < wfmin_index.Length; i++)
        {
            Patch[k].wfmin_index[i] = wfmin_index[i];        // index specifying water content for water factor for mineralization
            Patch[k].wfmin_values[i] = wfmin_values[i];       // value of water factor(mineralization) function at given index values
        }

        Patch[k].wfnit_index = new double[wfnit_index.Length];
        Patch[k].wfnit_values = new double[wfnit_index.Length];
        for (int i = 0; i < wfnit_index.Length; i++)
        {
            Patch[k].wfnit_index[i] = wfnit_index[i];        // index specifying water content for water factor for nitrification
            Patch[k].wfnit_values[i] = wfnit_values[i];       // value of water factor(nitrification) function at given index values
        }

        Patch[k].nitrification_pot = nitrification_pot;    // Potential nitrification by soil (ppm)

        Patch[k].nh4_at_half_pot = nh4_at_half_pot;      // nh4 conc at half potential (ppm)   

        Patch[k].pHf_nit_pH = new double[pHf_nit_pH.Length];
        Patch[k].pHf_nit_values = new double[pHf_nit_pH.Length];
        for (int i = 0; i < pHf_nit_pH.Length; i++)
        {
            Patch[k].pHf_nit_pH[i] = pHf_nit_pH[i];         // pH values for specifying pH factor for nitrification
            Patch[k].pHf_nit_values[i] = pHf_nit_values[i];     // value of pH factor(nitrification) function for given pH values
        }

        Patch[k].dnit_rate_coeff = dnit_rate_coeff;      // denitrification rate coefficient (kg/mg)

        Patch[k].dnit_wf_power = dnit_wf_power;        // denitrification water factor power term

        Patch[k].dnit_k1 = dnit_k1;              // K1 parameter from Thorburn et al (2010) for N2O model

        Patch[k].dnit_wfps = new double[dnit_wfps.Length];
        Patch[k].dnit_n2o_factor = new double[dnit_wfps.Length];
        for (int i = 0; i < dnit_wfps.Length; i++)
        {
            Patch[k].dnit_wfps[i] = dnit_wfps[i];            // WFPS for calculating the n2o fraction of denitrification
            Patch[k].dnit_n2o_factor[i] = dnit_n2o_factor[i];       // WFPS factor for n2o fraction of denitrification
        }
        Patch[k].dnit_nitrf_loss = dnit_nitrf_loss;      // Fraction of nitrification lost as denitrification

        #endregion

        #region Parameters added by RCichota

        Patch[k].useNewFunctions = useNewFunctions;
        Patch[k].minPatchArea = minPatchArea;

        // parameters for temperature factor for urea hydrolisys
        Patch[k].TempFactor_Hydrol.TempOptimum = TempFactor_Hydrol.TempOptimum;
        Patch[k].TempFactor_Hydrol.FactorAtZero = TempFactor_Hydrol.FactorAtZero;
        Patch[k].TempFactor_Hydrol.CurveExponent = TempFactor_Hydrol.CurveExponent;

        Patch[k].pot_hydrol_min = pot_hydrol_min;
        Patch[k].pot_hydrol_termA = pot_hydrol_termA;
        Patch[k].pot_hydrol_termB = pot_hydrol_termB;
        Patch[k].pot_hydrol_termC = pot_hydrol_termC;
        Patch[k].pot_hydrol_termD = pot_hydrol_termD;

        // parameters for soil moisture factor for hydrolisys
        Patch[k].MoistFactor_Hydrol.xVals = MoistFactor_Hydrol.xVals;
        Patch[k].MoistFactor_Hydrol.yVals = MoistFactor_Hydrol.yVals;

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

        // parameters for temperature factor for denitrification
        Patch[k].TempFactor_Denit.TempOptimum = TempFactor_Denit.TempOptimum;
        Patch[k].TempFactor_Denit.FactorAtZero = TempFactor_Denit.FactorAtZero;
        Patch[k].TempFactor_Denit.CurveExponent = TempFactor_Denit.CurveExponent;

        // parameters for soil moisture factor for denitrification
        Patch[k].MoistFactor_Denit.xVals = MoistFactor_Denit.xVals;
        Patch[k].MoistFactor_Denit.yVals = MoistFactor_Denit.yVals;

        // parameter for TermA in N2N2O function
        Patch[k].dnit_A = dnit_A;

        // parameter for TermB in N2N2O function
        Patch[k].dnit_B = dnit_B;

        // parameter for TermC in N2N2O function
        Patch[k].dnit_C = dnit_C;

        // parameter 1 to compute active carbon (for denitrification)
        Patch[k].actC_p1 = actC_p1;

        // parameter 2 to compute active carbon (for denitrification)
        Patch[k].actC_p2 = actC_p2;

        // whether mineralisation factors are computed single or for each type
        Patch[k].useSingleMinerFactors = useSingleMinerFactors;

        // parameters for temperature factor for mineralisation
        Patch[k].TempFactor_Miner.TempOptimum = TempFactor_Miner.TempOptimum;
        Patch[k].TempFactor_Miner.FactorAtZero = TempFactor_Miner.FactorAtZero;
        Patch[k].TempFactor_Miner.CurveExponent = TempFactor_Miner.CurveExponent;

        // parameters for soil moisture factor for OM mineralisation
        Patch[k].MoistFactor_Miner.xVals = MoistFactor_Miner.xVals;
        Patch[k].MoistFactor_Miner.yVals = MoistFactor_Miner.yVals;

        // parameters for C:N factor for OM mineralisation
        Patch[k].CNFactorMiner_Opt = CNFactorMiner_Opt;
        Patch[k].CNFactorMiner_rate = CNFactorMiner_rate;

        #region parameter for each OM type

        // parameters for temperature factor for humus mineralisation
        Patch[k].TempFactor_minerHum.TempOptimum = TempFactor_minerHum.TempOptimum;
        Patch[k].TempFactor_minerHum.FactorAtZero = TempFactor_minerHum.FactorAtZero;
        Patch[k].TempFactor_minerHum.CurveExponent = TempFactor_minerHum.CurveExponent;

        // parameters for soil moisture factor for humus mineralisation
        Patch[k].MoistFactor_minerHum.xVals = MoistFactor_minerHum.xVals;
        Patch[k].MoistFactor_minerHum.yVals = MoistFactor_minerHum.yVals;

        // parameters for temperature factor for OM biomass mineralisation
        Patch[k].TempFactor_minerBiom.TempOptimum = TempFactor_minerBiom.TempOptimum;
        Patch[k].TempFactor_minerBiom.FactorAtZero = TempFactor_minerBiom.FactorAtZero;
        Patch[k].TempFactor_minerBiom.CurveExponent = TempFactor_minerBiom.CurveExponent;

        // parameters for soil moisture factor for OM biomass mineralisation
        Patch[k].MoistFactor_minerBiom.xVals = MoistFactor_minerBiom.xVals;
        Patch[k].MoistFactor_minerBiom.yVals = MoistFactor_minerBiom.yVals;

        // parameters for temperature factor for FOM mineralisation
        Patch[k].TempFactor_minerFOM.TempOptimum = TempFactor_minerFOM.TempOptimum;
        Patch[k].TempFactor_minerFOM.FactorAtZero = TempFactor_minerFOM.FactorAtZero;
        Patch[k].TempFactor_minerFOM.CurveExponent = TempFactor_minerFOM.CurveExponent;

        // parameters for soil moisture factor for FOM mineralisation
        Patch[k].MoistFactor_minerFOM.xVals = MoistFactor_minerFOM.xVals;
        Patch[k].MoistFactor_minerFOM.yVals = MoistFactor_minerFOM.yVals;

        // parameters for C:N factor for FOM mineralisation
        Patch[k].CNFactorMinerFOM_Opt = CNFactorMinerFOM_Opt;
        Patch[k].CNFactorMinerFOM_rate = CNFactorMinerFOM_rate;

        #endregion

        #endregion

        #region Values needed for initalisation an during the simulation

        Patch[k].dlayer = new float[dlayer.Length];
        Patch[k].bd = new float[dlayer.Length];
        Patch[k].sat_dep = new float[dlayer.Length];
        Patch[k].dul_dep = new float[dlayer.Length];
        Patch[k].ll15_dep = new float[dlayer.Length];
        Patch[k].sw_dep = new float[dlayer.Length];
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

        // soil loss, due to erosion (?)
        Patch[k].soil_loss = soil_loss;

        #endregion
    }

    private void UpdatePatches()
    {
        // +  Purpose:
        //      Updates the variable in each existing patch

        // Values that are the same for all patches
        for (int k =0; k <Patch.Count; k++)
        {
            // today's date
            Patch[k].Today = today;
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

        soilCNPatch newPatch = new soilCNPatch();
        Patch.Add(newPatch);

        int k = Patch.Count - 1;

        Patch[k].ResizeLayerArrays(dlayer.Length);

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
        //      Check the list of patch names and ids passed by 'AddUrinePatch', output only ids
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
                            Console.WriteLine(today.ToString("dd MMMM yyyy") + "(Day of year=" + today.DayOfYear.ToString() + "), SoilNitrogen.AddPatch:");
                            Console.WriteLine("  the patch name '" + Names[pName] + "' did not correspond to any existing patch. Patch will be ignored.");
                        }
                    }
                }
                // check the ids
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
                            Console.WriteLine(today.ToString("dd MMMM yyyy") + "(Day of year=" + today.DayOfYear.ToString() + "), SoilNitrogen.AddPatch:");
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

    private PatchIDs comparePatches()
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

    #endregion

    #region Process calculations

    private void Process()
    {
        // + Purpose
        //     This routine performs the soil C and N balance, daily.
        //      - Assesses potential decomposition of surface residues (adjust decompostion if needed, accounts for mineralisation/immobilisation of N)
        //      - Calculates hydrolysis of urea, denitrification, transformations on soil organic matter (including N mineralisation/immobilition) and nitrification.

        for (int k = 0; k < Patch.Count; k++)
            Patch[k].Process();
    }

    private void SendExternalMassFlowN(double dltN)
    {
        // + Purpose
        //     Let other components know that N amount in the soil has changed

        ExternalMassFlowType massBalanceChange = new ExternalMassFlowType();
        if (Math.Abs(dltN) <= epsilon)
            dltN = 0.0;
        massBalanceChange.FlowType = dltN >= 0 ? "gain" : "loss";
        massBalanceChange.PoolClass = "soil";
        massBalanceChange.N = (float)Math.Abs(dltN);
        ExternalMassFlow.Invoke(massBalanceChange);
    }

    private void SendExternalMassFlowC(double dltC)
    {
        // + Purpose
        //     Let other components know that soil C has changed

        ExternalMassFlowType massBalanceChange = new ExternalMassFlowType();
        if (Math.Abs(dltC) <= epsilon)
            dltC = 0.0;
        massBalanceChange.FlowType = dltC >= 0 ? "gain" : "loss";
        massBalanceChange.PoolClass = "soil";
        massBalanceChange.N = (float)Math.Abs(dltC);
        ExternalMassFlow.Invoke(massBalanceChange);
    }

    private void SendActualResidueDecompositionCalculated()
    {
        // + Purpose
        //     Send back the information about actual decomposition
        //      Potential decomposition was given to this module by a residue/surfaceOM module.  Now we explicitly tell the module the actual decomposition
        //      rate for each of its residues.  If there wasn't enough mineral N to decompose, the rate will be reduced from the potential value.

        if (actualresiduedecompositioncalculated != null)
        {

            // will have to pack the SOMdecomp data from each patch and then invoke the event
            //int num_residues = Patch[0].SOMDecomp.Pool.Length;
            int nLayers = dlayer.Length;

            SurfaceOrganicMatterDecompType SOMDecomp = new SurfaceOrganicMatterDecompType();
            Array.Resize(ref SOMDecomp.Pool, num_residues);

            for (int residue = 0; residue < num_residues; residue++)
            {
                float c_summed = 0.0F;
                float n_summed = 0.0F;
                for (int k = 0; k < Patch.Count; k++)
                {
                    c_summed += Patch[k].SOMDecomp.Pool[residue].FOM.C * (float)Patch[k].PatchArea;
                    n_summed += Patch[k].SOMDecomp.Pool[residue].FOM.N * (float)Patch[k].PatchArea;
                }

                SOMDecomp.Pool[residue] = new SurfaceOrganicMatterDecompPoolType();
                SOMDecomp.Pool[residue].FOM = new FOMType();
                SOMDecomp.Pool[residue].Name = Patch[0].SOMDecomp.Pool[residue].Name;
                SOMDecomp.Pool[residue].OrganicMatterType = Patch[0].SOMDecomp.Pool[residue].OrganicMatterType;
                SOMDecomp.Pool[residue].FOM.amount = 0.0F;
                SOMDecomp.Pool[residue].FOM.C = c_summed;
                SOMDecomp.Pool[residue].FOM.N = n_summed;
                SOMDecomp.Pool[residue].FOM.P = 0.0F;
                SOMDecomp.Pool[residue].FOM.AshAlk = 0.0F;
            }

            // send the decomposition information
            actualresiduedecompositioncalculated.Invoke(SOMDecomp);
        }
    }

    #endregion

    #region Auxiliar functions

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

    private double convFactor_kgha2ppm(int layer)
    {
        // + Purpose
        //     Calculate conversion factor from kg/ha to ppm (mg/kg)

        if (bd == null || dlayer == null || bd.Length == 0 || dlayer.Length == 0)
        {
            return 0.0;
            throw new Exception(" Error on computing convertion factor, kg/ha to ppm. Value for dlayer or bulk density not valid");
        }
        return MathUtility.Divide(100.0, bd[layer] * dlayer[layer], 0.0);
    }

    private bool hasValues(double[] anArray, double MinValue)
    {
        // + Purpose
        //     check whether there is any considerable values in the array

        bool result = false;
        if (anArray != null)
        {
            foreach (double Value in anArray)
            {
                if (Math.Abs(Value) > MinValue)
                {
                    result = true;
                    break;
                }
            }
        }
        return result;
    }

    private double SumDoubleArray(double[] anArray)
    {
        // + Purpose
        //     calculate the sum of all values of an array of doubles

        double result = 0.0;
        if (anArray != null)
        {
            foreach (double Value in anArray)
                result += Value;
        }
        return result;
    }

    private int getCumulativeIndex(double sum, float[] realArray)
    {
        // + Purpose
        //     get the index at which the cumulative amount is equal or greater than 'sum'

        float cum = 0.0f;
        for (int i = 0; i < realArray.Length; i++)
        {
            cum += realArray[i];
            if (cum >= sum)
                return i;
        }
        return realArray.Length - 1;
    }

    #endregion
}


public class SoilTypeDefinition
{
    [Param]
    protected XmlNode SoilTypeDefinitionXML;
}