using System;
using System.Reflection;
using System.Collections.Generic;
using System.Text;
using ModelFramework;
using CSGeneral;
using System.Xml;

/// <summary>
/// A more-or-less direct port of the Fortran SoilN model
/// Ported by Eric Zurcher Sept/Oct 2010
/// Changes by RCichota on July/2012: a bit of tidying up - mostly modifying how some variables are handled (substitute get by input, add limits)
/// Further changes on September/2012, more tidying up, moving pieces of code around, updating error messages and finish cleaning the variable inputs
/// Changes to add patch capability (September/2012)
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

    // switch indicating whether soil profile reduction is allowed (from erosion)
    private bool AllowProfileReduction = false;
    [Param]
    private string profile_reduction
    { set { AllowProfileReduction = value.StartsWith("on"); } }

    // marker for whether organic solute are to be simulated (always false as it is not implemented)
    private bool useOrganicSolutes = false;
    [Param(IsOptional = true)]
    private string use_organic_solutes
    { set { useOrganicSolutes = value.StartsWith("on"); } }

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

    double[] dlt_nitrification;     // nitrogen coverted by nitrification (from NH4 to either NO3 or N2O)
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
    //private double oldN;
    //private double oldC;
    private double dailyInitialC;               // Total C content at the beginning of the day
    private double dailyInitialN;               // Total N content at the beginning of the day
    private int fom_type;
    //private double[] inert_c;                       // humic C that is not subject to mineralisation (kg/ha)
    //private double[] nh4_yesterday;                 // yesterday's ammonium nitrogen(kg/ha)
    //private double[] no3_yesterday;                 // yesterday's nitrate nitrogen (kg/ha)
    private int num_residues = 0;                   // number of residues decomposing
    //private string[] residue_name;                  // name of residues decomposing
    //private string[] residue_type;                  // type of decomposing residue
    //private double[] pot_c_decomp;                  // Potential residue C decomposition (kg/ha)
    //private double[] pot_n_decomp;                  // Potential residue N decomposition (kg/ha)
    //private double[] pot_p_decomp;                  // Potential residue P decomposition (kg/ha)
    //private double[][] dlt_c_decomp;            // residue C decomposition (kg/ha)
    //private double[][] dlt_n_decomp;            // residue N decomposition (kg/ha)

    //ref double[][] _dlt_res_c_biom, ref double[][] _dlt_res_c_hum, ref double[][] _dlt_res_c_atm, ref double[] dlt_res_nh4_min, ref double[] dlt_res_no3_min

    private simpleSoilTemp simpleST;     // the internal soil temp module - to be avoided

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

        // check few initialisation parameters
        CheckParams();

        // create first patch
        InitialisePatch(0);

        // perform initial calculations and setup
        InitCalc();

        // initialise soil temperature
        //SoilTemp();
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
        InitialisePatch(0);

        // reset C and N variables to their initial state
        oc = OC_reset;
        no3ppm = no3ppm_reset;
        nh4ppm = nh4ppm_reset;
        ureappm = ureappm_reset;

        // perform initial calculations and setup
        InitCalc();

        // reset soil temperature
        //SoilTemp();
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
        //      Performs every-day calcualtions

        // update soil temperature
        //SoilTemp();
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

    [EventHandler(EventName = "sum_report")]
    public void OnSum_report()
    {
        WriteSummaryReport();
    }

    [EventHandler(EventName = "tick")]
    public void OnTick(TimeType time)
    {
        // Reset Potential Decomposition Register

        foreach (soilCNPatch aPatch in Patch)
            aPatch.OnTick();


        //num_residues = 0;
        //Array.Resize(ref pot_c_decomp, 0);
        //Array.Resize(ref pot_n_decomp, 0);
        //Array.Resize(ref pot_p_decomp, 0);

        // Calculations for NEW sysbal component
        dailyInitialC = SumDoubleArray(carbon_tot);
        dailyInitialN = SumDoubleArray(nit_tot);

    }

    private double TotalC()
    {
        double result = 0.0;
        if (dlayer != null)
        {
            for (int layer = 0; layer < dlayer.Length; layer++)
            {
                for (int k = 0; k < Patch.Count; k++)
                    result += (Patch[k].fom_c_pool1[layer] +
                                   Patch[k].fom_c_pool2[layer] +
                                   Patch[k].fom_c_pool3[layer] +
                                   Patch[k].hum_c[layer] +
                                   Patch[k].biom_c[layer]) *
                                   Patch[k].PatchArea;
            }
        }
        return result;
    }


    [EventHandler(EventName = "IncorpFOM")]
    public void OnIncorpFOM(FOMLayerType FOMdata)
    {
        //    We partition the C and N into fractions in each layer.
        //    We will do this by assuming that the CN ratios
        //    of all fractions are equal

        foreach (soilCNPatch aPatch in Patch)
            aPatch.OnIncorpFOM(FOMdata);

        fom_type = Patch[0].fom_type;


        //bool nSpecified = false;
        //for (int i = 0; i < IncorpFOM.Layer.Length; i++)
        //{
        //    // If the caller specified CNR values then use them to calculate N from Amount.
        //    if (IncorpFOM.Layer[i].CNR > 0.0)
        //        IncorpFOM.Layer[i].FOM.N = (IncorpFOM.Layer[i].FOM.amount * c_in_fom) /
        //                                   IncorpFOM.Layer[i].CNR;
        //    // Was any N specified?
        //    nSpecified |= IncorpFOM.Layer[i].FOM.N != 0.0;
        //}

        //if (nSpecified)
        //{
        //    fom_type = 0; // use as default if fom type not found
        //    for (int i = 0; i < fom_types.Length; i++)
        //    {
        //        if (fom_types[i] == IncorpFOM.Type)
        //        {
        //            fom_type = i;
        //            break;
        //        }
        //    }
        //    // Now convert the IncorpFOM.DeltaWt and IncorpFOM.DeltaN arrays to
        //    // include fraction information and add to pools.
        //    int nLayers = IncorpFOM.Layer.Length;
        //    if (nLayers > dlayer.Length)
        //    {
        //        Array.Resize(ref dlayer, nLayers);
        //        ResizeLayerArrays(nLayers);
        //    }
        //    for (int i = 0; i < nLayers; i++)
        //    {
        //        fom_c_pool1[i] += IncorpFOM.Layer[i].FOM.amount * fract_carb[fom_type] * c_in_fom;
        //        fom_c_pool2[i] += IncorpFOM.Layer[i].FOM.amount * fract_cell[fom_type] * c_in_fom;
        //        fom_c_pool3[i] += IncorpFOM.Layer[i].FOM.amount * fract_lign[fom_type] * c_in_fom;

        //        fom_n_pool1[i] += IncorpFOM.Layer[i].FOM.N * fract_carb[fom_type];
        //        fom_n_pool2[i] += IncorpFOM.Layer[i].FOM.N * fract_cell[fom_type];
        //        fom_n_pool3[i] += IncorpFOM.Layer[i].FOM.N * fract_lign[fom_type];

        //        // add up fom_n in each layer by adding up each of the pools
        //        fom_n[i] = fom_n_pool1[i] + fom_n_pool2[i] + fom_n_pool3[i];
        //    }
        //}
    }

    [EventHandler(EventName = "IncorpFOMPool")]
    public void OnIncorpFOMPool(FOMPoolType FOMPoolData)
    {
        // INCREMENT THE POOLS wtih the unpacked deltas

        foreach (soilCNPatch aPatch in Patch)
            aPatch.OnIncorpFOMPool(FOMPoolData);


        //for (int i = 0; i < IncorpFOMPool.Layer.Length; i++)
        //{
        //    fom_c_pool1[i] += IncorpFOMPool.Layer[i].Pool[0].C;
        //    fom_c_pool2[i] += IncorpFOMPool.Layer[i].Pool[1].C;
        //    fom_c_pool3[i] += IncorpFOMPool.Layer[i].Pool[2].C;

        //    fom_n_pool1[i] += IncorpFOMPool.Layer[i].Pool[0].N;
        //    fom_n_pool2[i] += IncorpFOMPool.Layer[i].Pool[1].N;
        //    fom_n_pool3[i] += IncorpFOMPool.Layer[i].Pool[2].N;

        //    // add up fom_n in each layer by adding up each of the pools
        //    fom_n[i] = fom_n_pool1[i] + fom_n_pool2[i] + fom_n_pool3[i];

        //    _no3[i] += IncorpFOMPool.Layer[i].no3;
        //    _nh4[i] += IncorpFOMPool.Layer[i].nh4;
        //}
    }

    [EventHandler(EventName = "PotentialResidueDecompositionCalculated")]
    public void OnPotentialResidueDecompositionCalculated(SurfaceOrganicMatterDecompType SurfaceOrganicMatterDecomp)
    {
        //+  Purpose
        //     Get information of potential residue decomposition

        foreach (soilCNPatch aPatch in Patch)
            aPatch.OnPotentialResidueDecompositionCalculated(SurfaceOrganicMatterDecomp);

        num_residues = SurfaceOrganicMatterDecomp.Pool.Length;

        //Array.Resize(ref residue_name, num_residues);
        //Array.Resize(ref residue_type, num_residues);
        //Array.Resize(ref pot_c_decomp, num_residues);
        //Array.Resize(ref pot_n_decomp, num_residues);
        //Array.Resize(ref pot_p_decomp, num_residues);

        //for (int layer = 0; layer < dlt_c_res_2_biom.Length; layer++)
        //{
        //    Array.Resize(ref dlt_c_res_2_biom[layer], num_residues);
        //    Array.Resize(ref dlt_c_res_2_hum[layer], num_residues);
        //    Array.Resize(ref dlt_c_res_2_atm[layer], num_residues);
        //    Array.Resize(ref dlt_c_decomp[layer], num_residues);
        //    Array.Resize(ref dlt_n_decomp[layer], num_residues);
        //}

        //for (int residue = 0; residue < num_residues; residue++)
        //{
        //    residue_name[residue] = SurfaceOrganicMatterDecomp.Pool[residue].Name;
        //    residue_type[residue] = SurfaceOrganicMatterDecomp.Pool[residue].OrganicMatterType;
        //    pot_c_decomp[residue] = SurfaceOrganicMatterDecomp.Pool[residue].FOM.C;
        //    pot_n_decomp[residue] = SurfaceOrganicMatterDecomp.Pool[residue].FOM.N;
        //    // this P decomposition is needed to formulate data required by SOILP - struth, this is very ugly
        //    pot_p_decomp[residue] = SurfaceOrganicMatterDecomp.Pool[residue].FOM.P;
        //}
    }

    [EventHandler(EventName = "new_profile")]
    public void OnNew_profile(NewProfileType NewProfile)
    {
        // Soil profile is changed - primarily by erosion (??)

        foreach (soilCNPatch aPatch in Patch)
            aPatch.OnNew_profile(NewProfile);

        //bd = NewProfile.bd;
        //sat_dep = NewProfile.dul_dep;
        //dul_dep = NewProfile.dul_dep;
        //ll15_dep = NewProfile.ll15_dep;
        //sw_dep = NewProfile.sw_dep;

        //CheckProfile(NewProfile.dlayer);
    }

    [EventHandler(EventName = "NitrogenChanged")]
    public void OnNitrogenChanged(NitrogenChangedType NitrogenChanged)
    {

        // send deltas to each patch, will need to handle this differently in the future
        for (int k = 0; k < Patch.Count; k++)
        {
            Patch[k].dlt_no3 = NitrogenChanged.DeltaNO3;
            Patch[k].dlt_nh4 = NitrogenChanged.DeltaNH4;
        }
    }

    [EventHandler(EventName = "AddUrine")]
    public void OnAddUrine(AddUrineType UrineAdded)
    {
        // Starting with the minimalist version. To be updated by Val's group to
        // include a urine patch algorithm
        urea[0] += UrineAdded.Urea;
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
                //_urea[layer] = MathUtility.Divide(ureappm_reset[layer], convFact, 0.0);
                //if (_nh4[layer] < nh4_min[layer] - epsilon)
                //{
                //    Console.WriteLine(" Attempt to initialise Urea(" + (layer + 1).ToString() + ") to a value below the lower limit, value will be set to minimum (" + urea_min[layer].ToString() + ")");
                //    _urea[layer] = urea_min[layer];
                //}
            }
            newValue = MathUtility.Divide(nh4ppm_reset[layer], convFact, 0.0);       //Convert from ppm to convFactor_kgha2ppm/ha
            for (int k = 0; k < Patch.Count; k++)
                Patch[k].nh4[layer] = newValue;
            //_nh4[layer] = MathUtility.Divide(nh4ppm_reset[layer], convFact, 0.0);
            //if (_nh4[layer] < nh4_min[layer] - epsilon)
            //{
            //    Console.WriteLine(" Attempt to initialise NH4(" + (layer + 1).ToString() + ") to a value below the lower limit, value will be set to minimum (" + nh4_min[layer].ToString() + ")");
            //    _nh4[layer] = nh4_min[layer];
            //}
            newValue = MathUtility.Divide(no3ppm_reset[layer], convFact, 0.0);       //Convert from ppm to convFactor_kgha2ppm/ha
            for (int k = 0; k < Patch.Count; k++)
                Patch[k].no3[layer] = newValue;
            //_no3[layer] = MathUtility.Divide(no3ppm_reset[layer], convFact, 0.0);
            //if (_no3[layer] < no3_min[layer] - epsilon)
            //{
            //    Console.WriteLine(" Attempt to initialise NO3(" + (layer + 1).ToString() + ") to a value below the lower limit, value will be set to minimum (" + no3_min[layer].ToString() + ")");
            //    _no3[layer] = no3_min[layer];
            //}

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
                Patch[k].fom_n_pool2[layer] = MathUtility.Divide(Patch[k].fom_c_pool2[layer], root_cn_pool[0], 0.0);
                Patch[k].fom_n_pool3[layer] = MathUtility.Divide(Patch[k].fom_c_pool3[layer], root_cn_pool[0], 0.0);
            }
            //// C amount for each pool
            //fom_c_pool1[layer] = fom * fract_carb[0] * c_in_fom;
            //fom_c_pool2[layer] = fom * fract_cell[0] * c_in_fom;
            //fom_c_pool3[layer] = fom * fract_lign[0] * c_in_fom;

            //// N amount for each pool
            //fom_n_pool1[layer] = MathUtility.Divide(fom_c_pool1[layer], root_cn_pool[0], 0.0);
            //fom_n_pool2[layer] = MathUtility.Divide(fom_c_pool2[layer], root_cn_pool[1], 0.0);
            //fom_n_pool3[layer] = MathUtility.Divide(fom_c_pool3[layer], root_cn_pool[2], 0.0);

            //// total fom N in each layer
            //fom_n[layer] = fom_n_pool1[layer] + fom_n_pool2[layer] + fom_n_pool3[layer];

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
        //Array.Resize(ref _nh4, nLayers);
        //Array.Resize(ref _no3, nLayers);
        //Array.Resize(ref _urea, nLayers);
        //Array.Resize(ref no3_yesterday, nLayers);
        //Array.Resize(ref nh4_yesterday, nLayers);
        Array.Resize(ref urea_min, nLayers);
        Array.Resize(ref nh4_min, nLayers);
        Array.Resize(ref no3_min, nLayers);
        //Array.Resize(ref inert_c, nLayers);
        //Array.Resize(ref biom_c, nLayers);
        //Array.Resize(ref biom_n, nLayers);
        //Array.Resize(ref hum_c, nLayers);
        //Array.Resize(ref hum_n, nLayers);
        //Array.Resize(ref fom_c_pool1, nLayers);
        //Array.Resize(ref fom_c_pool2, nLayers);
        //Array.Resize(ref fom_c_pool3, nLayers);
        //Array.Resize(ref fom_n_pool1, nLayers);
        //Array.Resize(ref fom_n_pool2, nLayers);
        //Array.Resize(ref fom_n_pool3, nLayers);
        //Array.Resize(ref fom_n, nLayers);
        //Array.Resize(ref _nitrification_inhibition, nLayers);
        //Array.Resize(ref nh4_transform_net, nLayers);
        //Array.Resize(ref no3_transform_net, nLayers);
        //Array.Resize(ref dlt_nh4_net, nLayers);
        //Array.Resize(ref dlt_no3_net, nLayers);
        //Array.Resize(ref dlt_c_hum_2_atm, nLayers);
        //Array.Resize(ref dlt_c_biom_2_atm, nLayers);
        //for (int i = 0; i < 3; i++)
        //{
        //    Array.Resize(ref dlt_c_fom_2_biom[i], nLayers);
        //    Array.Resize(ref dlt_c_fom_2_hum[i], nLayers);
        //    Array.Resize(ref dlt_c_fom_2_atm[i], nLayers);
        //}
        //Array.Resize(ref dlt_c_res_2_biom, nLayers);
        //Array.Resize(ref dlt_c_res_2_hum, nLayers);
        //Array.Resize(ref dlt_c_res_2_atm, nLayers);
        //Array.Resize(ref dlt_c_decomp, nLayers);
        //Array.Resize(ref dlt_n_decomp, nLayers);
        //Array.Resize(ref dlt_nitrification, nLayers);
        //Array.Resize(ref effective_nitrification, nLayers);
        //Array.Resize(ref dlt_urea_hydrolised, nLayers);
        //Array.Resize(ref nh4_deficit_immob, nLayers);
        //Array.Resize(ref dlt_n_fom_2_min, nLayers);
        //Array.Resize(ref dlt_n_biom_2_min, nLayers);
        //Array.Resize(ref dlt_n_hum_2_min, nLayers);
        //Array.Resize(ref dlt_fom_c_pool1, nLayers);
        //Array.Resize(ref dlt_fom_c_pool2, nLayers);
        //Array.Resize(ref dlt_fom_c_pool3, nLayers);
        //Array.Resize(ref dlt_no3_decomp, nLayers);
        //Array.Resize(ref dlt_nh4_decomp, nLayers);
        //Array.Resize(ref dlt_no3_dnit, nLayers);
        //Array.Resize(ref dlt_nh4_dnit, nLayers);
        //Array.Resize(ref n2o_atm, nLayers);
        //Array.Resize(ref dlt_c_hum_2_biom, nLayers);
        //Array.Resize(ref dlt_c_biom_2_hum, nLayers);
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


    private void InitialisePatch(int k)
    {
        // + Purpose
        //    set all variables within a single patch to defaul/initial values (there must be only one patch)

        #region Values needed for initalisation only

        Patch[k].SoilN_MinerModel = SoilN_MinerModel;

        Patch[k].epsilon = epsilon;

        Patch[k].c_in_fom = c_in_fom;

        // parameter for TermA in N2N2O function ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Patch[k].dnit_A = dnit_A;

        // parameter for TermB in N2N2O function
        Patch[k].dnit_B = dnit_B;

        // parameter for TermC in N2N2O function
        Patch[k].dnit_C = dnit_C;

        // parameter 1 to compute active carbon (for denitrification)
        Patch[k].actC_p1 = actC_p1;

        // parameter 2 to compute active carbon (for denitrification) ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        Patch[k].actC_p2 = actC_p2;

        Patch[k].urea_min = urea_min;       // minimum allowable urea

        Patch[k].nh4_min = nh4_min;       // minimum allowable NH4

        Patch[k].no3_min = no3_min;       // minimum allowable NO3

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

        // initial ratio of biomass-C to mineralizable humic-C (0-1)
        Patch[k].fbiom = fbiom;

        // initial proportion of total soil C that is not subject to mineralization (0-1)
        Patch[k].finert = finert;

        // C:N ratio of microbes ()
        Patch[k].biom_cn = biom_cn;

        // the soil C:N ratio (actually of humus)
        Patch[k].hum_cn = hum_cn;

        Patch[k].ef_fom = ef_fom;               // fraction of FOM C mineralized retained in system (0-1)   

        Patch[k].fr_fom_biom = fr_fom_biom;          // fraction of retained FOM C transferred to biomass (0-1)

        Patch[k].ef_biom = ef_biom;              // fraction of biomass C mineralized retained in system (0-1)

        Patch[k].fr_biom_biom = fr_biom_biom;         // fraction of retained biomass C returned to biomass (0-1)

        Patch[k].ef_hum = ef_hum;               // fraction of humic C mineralized retained in system (0-1)

        Patch[k].rd_biom = rd_biom;     // potential rate of soil biomass mineralization (per day)

        Patch[k].rd_hum = rd_hum;      // potential rate of humus mineralization (per day)

        Patch[k].ef_res = ef_res;               // fraction of residue C mineralized retained in system (0-1)

        Patch[k].fr_res_biom = fr_res_biom;          // fraction of retained residue C transferred to biomass (0-1)

        Patch[k].rd_carb = rd_carb;            // maximum rate constants for decomposition of FOM pools [carbohydrate component] (0-1)

        Patch[k].rd_cell = rd_cell;            // maximum rate constants for decomposition of FOM pools [cellulose component] (0-1)

        Patch[k].rd_lign = rd_lign;            // maximum rate constants for decomposition of FOM pools [lignin component] (0-1)

        Patch[k].fom_types = fom_types;           // list of fom types

        Patch[k].fract_carb = fract_carb;            // carbohydrate fraction of FOM (0-1)          

        Patch[k].fract_cell = fract_cell;            // cellulose fraction of FOM (0-1)          

        Patch[k].fract_lign = fract_lign;            // lignin fraction of FOM (0-1)          

        Patch[k].oc2om_factor = oc2om_factor;         // conversion from OC to OM

        Patch[k].fom_min = fom_min;              // minimum allowable FOM (kg/ha)

        Patch[k].min_depth = min_depth;            // depth from which mineral N can be immobilized by decomposing residues (mm)

        Patch[k].cnrf_coeff = cnrf_coeff;           // coeff. to determine the magnitude of C:N effects on decomposition of FOM ()

        Patch[k].cnrf_optcn = cnrf_optcn;           // C:N above which decomposition rate of FOM declines ()

        Patch[k].opt_temp = opt_temp;           // Soil temperature above which there is no further effect on mineralisation and nitrification (oC)

        Patch[k].wfmin_index = wfmin_index;        // index specifying water content for water factor for mineralization

        Patch[k].wfmin_values = wfmin_values;       // value of water factor(mineralization) function at given index values

        Patch[k].wfnit_index = wfnit_index;        // index specifying water content for water factor for nitrification

        Patch[k].wfnit_values = wfnit_values;       // value of water factor(nitrification) function at given index values

        Patch[k].nitrification_pot = nitrification_pot;    // Potential nitrification by soil (ppm)

        Patch[k].nh4_at_half_pot = nh4_at_half_pot;      // nh4 conc at half potential (ppm)   

        Patch[k].pHf_nit_pH = pHf_nit_pH;         // pH values for specifying pH factor for nitrification

        Patch[k].pHf_nit_values = pHf_nit_values;     // value of pH factor(nitrification) function for given pH values

        Patch[k].dnit_rate_coeff = dnit_rate_coeff;      // denitrification rate coefficient (kg/mg)

        Patch[k].dnit_wf_power = dnit_wf_power;        // denitrification water factor power term

        Patch[k].dnit_k1 = dnit_k1;              // K1 parameter from Thorburn et al (2010) for N2O model

        Patch[k].dnit_wfps = dnit_wfps;            // WFPS for calculating the n2o fraction of denitrification

        Patch[k].dnit_n2o_factor = dnit_n2o_factor;       // WFPS factor for n2o fraction of denitrification

        Patch[k].dnit_nitrf_loss = dnit_nitrf_loss;      // Fraction of nitrification lost as denitrification

        #endregion

        #region Values needed for initalisation an during the simulation

        // soil layers' thichness (mm)
        Patch[k].dlayer = dlayer;

        // soil bulk density for each layer (kg/dm3)
        Patch[k].bd = bd;

        // soil water content at saturation
        Patch[k].sat_dep = sat_dep;

        // soil water content at drainage upper limit
        Patch[k].dul_dep = dul_dep;

        // soil water content at drainage lower limit
        Patch[k].ll15_dep = ll15_dep;

        // today's soil water content
        Patch[k].sw_dep = sw_dep;

        Patch[k].ph = ph;       // soil pH

        // soil albedo (0-1)
        Patch[k].salb = salb;

        // soil temperature (as computed by another module - SoilTemp)
        Patch[k].st = st;

        // soil loss, due to erosion (?)
        Patch[k].soil_loss = soil_loss;


        // total soil organic carbon content (%)
        //Patch[k].oc = _oc;

        //// soil urea nitrogen amount (kgN/ha)
        //Patch[k].urea = urea;

        //// soil ammonium nitrogen amount (kgN/ha)
        //Patch[k].nh4 = nh4;

        //// soil nitrate nitrogen amount (kgN/ha)
        //Patch[k].no3 = no3;

        //// switch indicating whether pond is active or not
        //Patch[k].is_pond_active = is_pond_active;

        //// C decomposed in pond that is added to soil biomass
        //Patch[k].pond_biom_C = pond_biom_C;

        //// C decomposed in pond that is added to soil humus
        //Patch[k].pond_hum_C = pond_hum_C;

        // factor reducing nitrification due to the presence of a inhibitor
        //Patch[k].nitrification_inhibition = nitrification_inhibition;

        //// factor reducing urea hydrolysis due to the presence of an inhibitor - not implemented yet
        //Patch[k].hydrolysis_inhibition = hydrolysis_inhibition;

        //// factor reducing mineralisation processes due to the presence of an inhibitor - not implemented yet
        //Patch[k].mineralisation_inhibition = mineralisation_inhibition;

        #endregion

    }

    private void UpdatePatches()
    {
        // +  Purpose:
        //      Updates the variable in each existing patch

        // Values that are the same for all patches    
        foreach (soilCNPatch aPatch in Patch)
        {

            // soil layers' thichness (mm)
            aPatch.dlayer = dlayer;

            // soil bulk density for each layer (kg/dm3)
            aPatch.bd = bd;

            // soil water content at saturation
            aPatch.sat_dep = sat_dep;

            // soil water content at drainage upper limit
            aPatch.dul_dep = dul_dep;

            // soil water content at drainage lower limit
            aPatch.ll15_dep = ll15_dep;

            // today's soil water content
            aPatch.sw_dep = sw_dep;

            aPatch.ph = ph;

            // soil albedo (0-1)
            aPatch.salb = salb;

            // soil temperature (as computed by another module - SoilTemp)
            aPatch.st = st;

            // soil loss, due to erosion (?)
            aPatch.soil_loss = soil_loss;
        }
    }

    #region process calculations

    #region Main processes

    private void Process()
    {
        // + Purpose
        //     This routine performs the soil C and N balance, daily.
        //      - Assesses potential decomposition of surface residues (adjust decompostion if needed, accounts for mineralisation/immobilisation of N)
        //      - Calculates hydrolysis of urea, denitrification, transformations on soil organic matter (including N mineralisation/immobilition) and nitrification.

        for (int k = 0; k < Patch.Count; k++)
            Patch[k].Process();


        //int nLayers = dlayer.Length;                    // number of layers in the soil
        //double[,] dlt_fom_n = new double[3, nLayers];   // fom N mineralised in each fraction (kg/ha)

        //if (is_pond_active)
        //{
        //    // dsg 190508,  If there is a pond, the POND module will decompose residues - not SoilNitrogen
        //    // dsg 110708   Get the biom & hum C decomposed in the pond and add to soil - on advice of MEP

        //    // increment the hum and biom C pools in top soil layer
        //    hum_c[0] += pond_hum_C;         // humic material from breakdown of residues in pond
        //    biom_c[0] += pond_biom_C;       // biom material from breakdown of residues in pond

        //    // reset the N amounts of N in hum and biom pools
        //    hum_n[0] = MathUtility.Divide(hum_c[0], hum_cn, 0.0);
        //    biom_n[0] = MathUtility.Divide(biom_c[0], biom_cn, 0.0);
        //}
        //else
        //{
        //    // Decompose residues
        //    //  assess the potential decomposition of surface residues and calculate actual mineralisation/immobilisation
        //    DecomposeResidues();

        //    // update C content in hum and biom pools
        //    for (int layer = 0; layer < nLayers; layer++)
        //    {
        //        hum_c[layer] += SumDoubleArray(dlt_c_res_2_hum[layer]);
        //        biom_c[layer] += SumDoubleArray(dlt_c_res_2_biom[layer]);
        //    }

        //    // update N content in hum and biom pools as well as the mineral N
        //    for (int layer = 0; layer < nLayers; layer++)
        //    {
        //        hum_n[layer] = MathUtility.Divide(hum_c[layer], hum_cn, 0.0);
        //        biom_n[layer] = MathUtility.Divide(biom_c[layer], biom_cn, 0.0);

        //        // update soil mineral N
        //        _nh4[layer] += dlt_nh4_decomp[layer];
        //        _no3[layer] += dlt_no3_decomp[layer];
        //    }
        //}

        //// now take each layer in turn and compute N processes
        //for (int layer = 0; layer < nLayers; layer++)
        //{
        //    // urea hydrolysis
        //    dlt_urea_hydrolised[layer] = UreaHydrolysis(layer);
        //    _nh4[layer] += dlt_urea_hydrolised[layer];
        //    _urea[layer] -= dlt_urea_hydrolised[layer];

        //    // nitrate-N denitrification
        //    dlt_no3_dnit[layer] = Denitrification(layer);
        //    _no3[layer] -= dlt_no3_dnit[layer];

        //    // N2O loss to atmosphere - due to denitrification
        //    n2o_atm[layer] = 0.0;
        //    double N2N2O = Denitrification_Nratio(layer);
        //    n2o_atm[layer] = dlt_no3_dnit[layer] / (N2N2O + 1.0);

        //    // Calculate transformations of soil organic matter (C and N)

        //    // humic pool mineralisation
        //    MineraliseHumus(layer);

        //    // microbial biomass pool mineralisation
        //    MineraliseBiomass(layer);

        //    // mineralisation of fresh organic matter pools
        //    // need to be revisited - create FOM pools as array
        //    //for (int fract = 0; fract < 3; fract++)
        //    //{
        //    //    MinFom(layer, fract);
        //    //    dlt_c_fom_2_biom[fract][layer] = dlt_fc_biom[fract];
        //    //    dlt_c_fom_2_hum[fract][layer] = dlt_fc_hum[fract];
        //    //    dlt_c_fom_2_atm[fract][layer] = dlt_fc_atm[fract];
        //    //    dlt_fom_n[fract, layer] = dlt_f_n[fract];
        //    //}

        //    double[] dlt_f_n;
        //    double[] dlt_fc_biom;
        //    double[] dlt_fc_hum;
        //    double[] dlt_fc_atm;
        //    MineraliseFOM(layer, out dlt_fc_biom, out dlt_fc_hum, out dlt_fc_atm, out dlt_f_n, out dlt_n_fom_2_min[layer]);

        //    for (int fract = 0; fract < 3; fract++)
        //    {
        //        dlt_c_fom_2_biom[fract][layer] = dlt_fc_biom[fract];
        //        dlt_c_fom_2_hum[fract][layer] = dlt_fc_hum[fract];
        //        dlt_c_fom_2_atm[fract][layer] = dlt_fc_atm[fract];
        //        dlt_fom_n[fract, layer] = dlt_f_n[fract];
        //    }

        //    // update pools C an N contents

        //    hum_c[layer] += dlt_c_biom_2_hum[layer] - dlt_c_hum_2_biom[layer] - dlt_c_hum_2_atm[layer] +
        //                   dlt_c_fom_2_hum[0][layer] + dlt_c_fom_2_hum[1][layer] + dlt_c_fom_2_hum[2][layer];

        //    hum_n[layer] = MathUtility.Divide(hum_c[layer], hum_cn, 0.0);

        //    biom_c[layer] += dlt_c_hum_2_biom[layer] - dlt_c_biom_2_hum[layer] - dlt_c_biom_2_atm[layer] +
        //                   dlt_c_fom_2_biom[0][layer] + dlt_c_fom_2_biom[1][layer] + dlt_c_fom_2_biom[2][layer];

        //    biom_n[layer] = MathUtility.Divide(biom_c[layer], biom_cn, 0.0);

        //    fom_c_pool1[layer] -= (dlt_c_fom_2_hum[0][layer] + dlt_c_fom_2_biom[0][layer] + dlt_c_fom_2_atm[0][layer]);
        //    fom_c_pool2[layer] -= (dlt_c_fom_2_hum[1][layer] + dlt_c_fom_2_biom[1][layer] + dlt_c_fom_2_atm[1][layer]);
        //    fom_c_pool3[layer] -= (dlt_c_fom_2_hum[2][layer] + dlt_c_fom_2_biom[2][layer] + dlt_c_fom_2_atm[2][layer]);

        //    fom_n_pool1[layer] -= dlt_fom_n[0, layer];
        //    fom_n_pool2[layer] -= dlt_fom_n[1, layer];
        //    fom_n_pool3[layer] -= dlt_fom_n[2, layer];

        //    // dsg  these 3 dlts are calculated for the benefit of soilp which needs to 'get' them
        //    dlt_fom_c_pool1[layer] = dlt_c_fom_2_hum[0][layer] + dlt_c_fom_2_biom[0][layer] + dlt_c_fom_2_atm[0][layer];
        //    dlt_fom_c_pool2[layer] = dlt_c_fom_2_hum[1][layer] + dlt_c_fom_2_biom[1][layer] + dlt_c_fom_2_atm[1][layer];
        //    dlt_fom_c_pool3[layer] = dlt_c_fom_2_hum[2][layer] + dlt_c_fom_2_biom[2][layer] + dlt_c_fom_2_atm[2][layer];

        //    // add up fom in each layer in each of the pools
        //    //double fom_c = fom_c_pool1[layer] + fom_c_pool2[layer] + fom_c_pool3[layer];
        //    //fom_n[layer] = fom_n_pool1[layer] + fom_n_pool2[layer] + fom_n_pool3[layer];

        //    // update soil mineral N after mineralisation/immobilisation

        //    // starts with nh4
        //    _nh4[layer] += dlt_n_hum_2_min[layer] + dlt_n_biom_2_min[layer] + dlt_n_fom_2_min[layer];

        //    // check whether there is enough NH4 to be immobilised
        //    if (_nh4[layer] < nh4_min[layer])
        //    {
        //        nh4_deficit_immob[layer] = nh4_min[layer] - _nh4[layer];
        //        _nh4[layer] = nh4_min[layer];
        //    }

        //    // now change no3
        //    _no3[layer] -= nh4_deficit_immob[layer];
        //    if (_no3[layer] < no3_min[layer] - epsilon)
        //    {
        //        // note: tests for adequate mineral N for immobilisation have been made so this no3 should not go below no3_min
        //        throw new Exception("N immobilisation resulted in mineral N in layer(" + (layer + 1).ToString() + ") to go below minimum");
        //    }

        //    // nitrification of ammonium-N (total)
        //    dlt_nitrification[layer] = Nitrification(layer);

        //    // denitrification loss during nitrification
        //    dlt_nh4_dnit[layer] = DenitrificationInNitrification(layer);

        //    // effective or net nitrification
        //    effective_nitrification[layer] = dlt_nitrification[layer] - dlt_nh4_dnit[layer];

        //    // N2O loss to atmosphere from nitrification
        //    n2o_atm[layer] += dlt_nh4_dnit[layer];

        //    // update soil mineral N
        //    _no3[layer] += effective_nitrification[layer];
        //    _nh4[layer] -= dlt_nitrification[layer];

        //    // check some of the values
        //    if (Math.Abs(_urea[layer]) < epsilon)
        //        _urea[layer] = 0.0;
        //    if (Math.Abs(_nh4[layer]) < epsilon)
        //        _nh4[layer] = 0.0;
        //    if (Math.Abs(_no3[layer]) < epsilon)
        //        _no3[layer] = 0.0;
        //    if (_urea[layer] < urea_min[layer] || _urea[layer] > 9000.0)
        //        throw new Exception("Value for urea(layer) is out of range");
        //    if (_nh4[layer] < nh4_min[layer] || _nh4[layer] > 9000.0)
        //        throw new Exception("Value for NH4(layer) is out of range");
        //    if (_no3[layer] < no3_min[layer] || _no3[layer] > 9000.0)
        //        throw new Exception("Value for NO3(layer) is out of range");

        //    // net N tansformations
        //    nh4_transform_net[layer] = dlt_nh4_decomp[layer] + dlt_n_fom_2_min[layer] + dlt_n_biom_2_min[layer] + dlt_n_hum_2_min[layer] - dlt_nitrification[layer] + dlt_urea_hydrolised[layer] + nh4_deficit_immob[layer];
        //    no3_transform_net[layer] = dlt_no3_decomp[layer] - dlt_no3_dnit[layer] + effective_nitrification[layer] - nh4_deficit_immob[layer];

        //for (int layer = 0; layer < dlayer.Length; layer++)
        //{
        //    // net deltas
        //    dlt_nh4_net[layer] = nh4[layer] - nh4_yesterday[layer];
        //    dlt_no3_net[layer] = no3[layer] - no3_yesterday[layer];

        //    // store these values so they may be used tomorrow
        //    nh4_yesterday[layer] = nh4[layer];
        //    no3_yesterday[layer] = no3[layer];
        //}
    }

    //private void DecomposeResidues()
    //{
    //    // + Purpose
    //    //     Calculate the actual C and N mineralised/immobilised from residue decomposition
    //    //      Check whether adequate mineral nitrogen is available to sustain potential rate of decomposition of surface
    //    //       residues and calculate net rate of nitrogen mineralisation/immobilisation

    //    // Initialise to zero by assigning new
    //    int nLayers = dlayer.Length;
    //    double[] no3_available = new double[nLayers]; // no3 available for mineralisation
    //    double[] nh4_available = new double[nLayers]; // nh4 available for mineralisation
    //    dlt_c_decomp = new double[nLayers][];
    //    dlt_n_decomp = new double[nLayers][];
    //    dlt_c_res_2_biom = new double[nLayers][];
    //    dlt_c_res_2_hum = new double[nLayers][];
    //    dlt_c_res_2_atm = new double[nLayers][];
    //    for (int layer = 0; layer < nLayers; layer++)
    //    {
    //        dlt_c_decomp[layer] = new double[num_residues];
    //        dlt_n_decomp[layer] = new double[num_residues];
    //        dlt_c_res_2_biom[layer] = new double[num_residues];
    //        dlt_c_res_2_hum[layer] = new double[num_residues];
    //        dlt_c_res_2_atm[layer] = new double[num_residues];
    //    }
    //    dlt_nh4_decomp = new double[nLayers];
    //    dlt_no3_decomp = new double[nLayers];

    //    // get total available mineral N in soil layer which can supply N to decomposition (min_depth)
    //    double[] fracLayer = new double[dlayer.Length];
    //    double cumFracLayer = 0.0;
    //    double cumDepth = 0.0;
    //    int DecompLayer = 0;
    //    for (int layer = 0; layer < nLayers; layer++)
    //    {
    //        fracLayer[layer] = Math.Min(1, Math.Max(0, min_depth - cumDepth) / dlayer[layer]);
    //        if (fracLayer[layer] <= epsilon)
    //            break;  // no need to continue calculating
    //        cumFracLayer += fracLayer[layer];
    //        cumDepth += dlayer[layer];
    //        DecompLayer = layer;
    //        no3_available[layer] = Math.Max(0.0, _no3[layer] - no3_min[layer]) * fracLayer[layer];
    //        nh4_available[layer] = Math.Max(0.0, _nh4[layer] - nh4_min[layer]) * fracLayer[layer];
    //    }

    //    double n_available = SumDoubleArray(no3_available) + SumDoubleArray(nh4_available) + SumDoubleArray(pot_n_decomp);

    //    // get N demand from potential decomposition
    //    double n_demand = MathUtility.Divide(SumDoubleArray(pot_c_decomp) * ef_res * fr_res_biom, biom_cn, 0.0) +
    //                      MathUtility.Divide(SumDoubleArray(pot_c_decomp) * ef_res * (1.0 - fr_res_biom), hum_cn, 0.0);

    //    // test whether there is adequate N available to meet potential immobilisation demand
    //    //      if not, calculate a factor to reduce the mineralisation rates
    //    double ReductionFactor = 1.0;
    //    if (n_demand > n_available)
    //        ReductionFactor = Math.Max(0.0, Math.Min(1.0, MathUtility.Divide(SumDoubleArray(no3_available) + SumDoubleArray(nh4_available), n_demand - SumDoubleArray(pot_n_decomp), 0.0)));

    //    // Partition the additions of C and N to layers
    //    double dlt_n_decomp_tot = 0.0;
    //    for (int layer = 0; layer <= DecompLayer; layer++)
    //    {
    //        double DecompFraction = fracLayer[layer] / cumFracLayer;  // the fraction of decomposition for each soil layer
    //        for (int residue = 0; residue < num_residues; residue++)
    //        {
    //            // adjust carbon transformations and distribute over the layers
    //            dlt_c_decomp[layer][residue] = pot_c_decomp[residue] * ReductionFactor * DecompFraction;
    //            dlt_n_decomp[layer][residue] = pot_n_decomp[residue] * ReductionFactor * DecompFraction;
    //            dlt_n_decomp_tot += dlt_n_decomp[layer][residue];

    //            // partition the decomposed C between pools and losses
    //            dlt_c_res_2_biom[layer][residue] = dlt_c_decomp[layer][residue] * ef_res * fr_res_biom;
    //            dlt_c_res_2_hum[layer][residue] = dlt_c_decomp[layer][residue] * ef_res * (1.0 - fr_res_biom);
    //            dlt_c_res_2_atm[layer][residue] = dlt_c_decomp[layer][residue] - dlt_c_res_2_biom[layer][residue] - dlt_c_res_2_hum[layer][residue];
    //        }
    //    }

    //    // net N mineralised (hg/ha)
    //    double dlt_n_min = dlt_n_decomp_tot - n_demand * ReductionFactor;

    //    if (dlt_n_min > 0.0)
    //    {
    //        // Mineralisation occurred - distribute NH4 over the layers
    //        for (int layer = 0; layer <= DecompLayer; layer++)
    //        {
    //            double DecompFraction = fracLayer[layer] / cumFracLayer;  // the fraction of decomposition for each soil layer
    //            dlt_nh4_decomp[layer] = dlt_n_min * DecompFraction;
    //        }
    //    }
    //    else if (dlt_n_min < 0.0)
    //    {
    //        // Immobilisation occurred - soak up any N required, from NH4 first then NO3 if needed
    //        for (int layer = 0; layer <= DecompLayer; layer++)
    //        {
    //            dlt_nh4_decomp[layer] = -Math.Min(nh4_available[layer], Math.Abs(dlt_n_min));
    //            dlt_n_min -= dlt_nh4_decomp[layer];
    //        }
    //        for (int layer = 0; layer <= DecompLayer; layer++)
    //        {
    //            dlt_no3_decomp[layer] = -Math.Min(no3_available[layer], Math.Abs(dlt_n_min));
    //            dlt_n_min -= dlt_no3_decomp[layer];
    //        }

    //        // There should now be no remaining immobilisation demand
    //        if (dlt_n_min < -0.001 || dlt_n_min > 0.001)
    //            throw new Exception("Value for remaining immobilisation is out of range");
    //    }
    //}

    //private void MineraliseHumus(int layer)
    //{
    //    // + Purpose
    //    //     Calculate the daily transformation of the the soil humic pool, mineralisation (+ve) or immobilisation (-ve)

    //    // + Assumptions
    //    //     There is an inert_C component of the humic pool that is not subject to mineralisation

    //    // dsg 200508  use different values for some constants when there's a pond and anaerobic conditions dominate
    //    int index = (!is_pond_active) ? 1 : 2;

    //    // get the soil temperature factor
    //    double tf = (SoilN_MinerModel == "rothc") ? RothcTF(layer, index) : TF(layer, index);

    //    // get the soil water factor
    //    double wf = WF(layer, index);

    //    // get the rate of mineralisation of N from the humic pool
    //    double dlt_c_min_tot = (hum_c[layer] - inert_c[layer]) * rd_hum[index - 1] * tf * wf;
    //    double dlt_n_min_tot = MathUtility.Divide(dlt_c_min_tot, hum_cn, 0.0);

    //    // distribute the mineralised N and C
    //    dlt_c_hum_2_biom[layer] = dlt_c_min_tot * ef_hum;
    //    dlt_c_hum_2_atm[layer] = dlt_c_min_tot * (1.0 - ef_hum);
    //    dlt_n_hum_2_min[layer] = dlt_n_min_tot - MathUtility.Divide(dlt_c_hum_2_biom[layer], biom_cn, 0.0);
    //}

    //private void MineraliseBiomass(int layer)
    //{
    //    // + Purpose
    //    //     Calculate the daily transformation of the soil biomass pool, mineralisation (+ve) or immobilisation (-ve)

    //    // dsg 200508  use different values for some constants when anaerobic conditions dominate
    //    int index = (!is_pond_active) ? 1 : 2;

    //    // get the soil temperature factor
    //    double tf = (SoilN_MinerModel == "rothc") ? RothcTF(layer, index) : TF(layer, index);

    //    // get the soil water factor
    //    double wf = WF(layer, index);

    //    // get the rate of mineralisation of C & N from the biomass pool
    //    double dlt_n_min_tot = biom_n[layer] * rd_biom[index - 1] * tf * wf;
    //    double dlt_c_min_tot = dlt_n_min_tot * biom_cn;

    //    // distribute the carbon
    //    dlt_c_biom_2_hum[layer] = dlt_c_min_tot * ef_biom * (1.0 - fr_biom_biom);
    //    dlt_c_biom_2_atm[layer] = dlt_c_min_tot * (1.0 - ef_biom);

    //    // calculate net N mineralisation
    //    dlt_n_biom_2_min[layer] = dlt_n_min_tot - MathUtility.Divide(dlt_c_biom_2_hum[layer], hum_cn, 0.0) - MathUtility.Divide((dlt_c_min_tot - dlt_c_biom_2_atm[layer] - dlt_c_biom_2_hum[layer]), biom_cn, 0.0);
    //}

    //private void MineraliseFOM(int layer, out double[] dlt_c_biom, out double[] dlt_c_hum, out double[] dlt_c_atm, out double[] dlt_fom_n, out double dlt_n_min)
    //{
    //    // + Purpose
    //    //     Calculate the daily transformation of the soil fresh organic matter pools, mineralisation (+ve) or immobilisation (-ve)

    //    dlt_c_hum = new double[3];
    //    dlt_c_biom = new double[3];
    //    dlt_c_atm = new double[3];
    //    dlt_fom_n = new double[3];
    //    dlt_n_min = 0.0;

    //    // dsg 200508  use different values for some constants when anaerobic conditions dominate
    //    // index = 1 for aerobic conditions, 2 for anaerobic conditions
    //    int index = (!is_pond_active) ? 1 : 2;

    //    // get total available mineral N (kg/ha)
    //    double nitTot = Math.Max(0.0, (_no3[layer] - no3_min[layer]) + (_nh4[layer] - nh4_min[layer]));

    //    // fresh organic carbon (kg/ha)
    //    double fomC = fom_c_pool1[layer] + fom_c_pool2[layer] + fom_c_pool3[layer];

    //    // fresh organic nitrogen (kg/ha)
    //    double fomN = fom_n_pool1[layer] + fom_n_pool2[layer] + fom_n_pool3[layer];

    //    // ratio of C in fresh OM to N available for decay
    //    double cnr = MathUtility.Divide(fomC, fomN + nitTot, 0.0);

    //    // calculate the C:N ratio factor - Bound to [0, 1]
    //    double cnrf = Math.Max(0.0, Math.Min(1.0, Math.Exp(-cnrf_coeff * (cnr - cnrf_optcn) / cnrf_optcn)));

    //    // get the soil temperature factor
    //    double tf = (SoilN_MinerModel == "rothc") ? RothcTF(layer, index) : TF(layer, index);

    //    // get the soil water factor
    //    double wf = WF(layer, index);

    //    // calculate gross amount of C & N released due to mineralisation of the fresh organic matter.
    //    if (fomC >= fom_min)
    //    {
    //        double dlt_fom_n_min_tot = 0.0; // amount of fresh organic N mineralised across fpools (kg/ha)
    //        double dlt_fom_c_min_tot = 0.0; // total C mineralised (kg/ha) summed across fpools
    //        double[] dlt_n_min_tot = new double[3]; // amount of fresh organic N mineralised in each pool (kg/ha)
    //        double[] dlt_c_min_tot = new double[3]; // amount of C mineralised (kg/ha) from each pool

    //        // C:N ratio of fom
    //        double fom_cn = MathUtility.Divide(fomC, fomN, 0.0);

    //        // get the decomposition of carbohydrate-like, cellulose-like and lignin-like fractions (fpools) in turn.
    //        for (int fractn = 0; fractn < 3; fractn++)
    //        {
    //            // get the max decomposition rate for each fpool
    //            double decomp_rate = FractRDFom(fractn)[index - 1] * cnrf * tf * wf;

    //            // calculate the gross amount of fresh organic carbon mineralised (kg/ha)
    //            double gross_c_decomp = decomp_rate * FractFomC(fractn)[layer];

    //            // calculate the gross amount of N released from fresh organic matter (kg/ha)
    //            double gross_n_decomp = decomp_rate * FractFomN(fractn)[layer];

    //            dlt_fom_n_min_tot += gross_n_decomp;
    //            dlt_c_min_tot[fractn] = gross_c_decomp;
    //            dlt_n_min_tot[fractn] = gross_n_decomp;
    //            dlt_fom_c_min_tot += gross_c_decomp;
    //        }

    //        // calculate potential transfers of C mineralised to biomass
    //        double dlt_c_biom_tot = dlt_fom_c_min_tot * ef_fom * fr_fom_biom;

    //        // calculate potential transfers of C mineralised to humus
    //        double dlt_c_hum_tot = dlt_fom_c_min_tot * ef_fom * (1.0 - fr_fom_biom);

    //        // test whether there is adequate N available to meet immobilisation demand
    //        double n_demand = MathUtility.Divide(dlt_c_biom_tot, biom_cn, 0.0) + MathUtility.Divide(dlt_c_hum_tot, hum_cn, 0.0);
    //        double n_avail = nitTot + dlt_fom_n_min_tot;

    //        // factor to reduce mineralisation rates if insufficient N to meet immobilisation demand
    //        double Navail_factor = 1.0;
    //        if (n_demand > n_avail)
    //            Navail_factor = Math.Max(0.0, Math.Min(1.0, MathUtility.Divide(nitTot, n_demand - dlt_fom_n_min_tot, 0.0)));

    //        // now adjust carbon transformations etc. and similarly for npools
    //        for (int fractn = 0; fractn < 3; fractn++)
    //        {
    //            dlt_c_hum[fractn] = dlt_c_min_tot[fractn] * ef_fom * (1.0 - fr_fom_biom) * Navail_factor;
    //            dlt_c_biom[fractn] = dlt_c_min_tot[fractn] * ef_fom * fr_fom_biom * Navail_factor;
    //            dlt_c_atm[fractn] = dlt_c_min_tot[fractn] * (1.0 - ef_fom) * Navail_factor;
    //            dlt_fom_n[fractn] = dlt_n_min_tot[fractn] * Navail_factor;

    //            dlt_c_hum[fractn] = MathUtility.RoundToZero(dlt_c_hum[fractn]);
    //            dlt_c_biom[fractn] = MathUtility.RoundToZero(dlt_c_biom[fractn]);
    //            dlt_c_atm[fractn] = MathUtility.RoundToZero(dlt_c_atm[fractn]);
    //            dlt_fom_n[fractn] = MathUtility.RoundToZero(dlt_fom_n[fractn]);
    //        }

    //        dlt_n_min = (dlt_fom_n_min_tot - n_demand) * Navail_factor;
    //    }
    //}

    //private double UreaHydrolysis(int layer)
    //{
    //    // + Purpose
    //    //     Calculate the amount of urea converted to NH4 via hydrolysis

    //    // dsg 200508  use different values for some constants when anaerobic conditions dominate
    //    double result;
    //    int index = (!is_pond_active) ? 1 : 2;

    //    if (_urea[layer] > 0.0)
    //    {
    //        // we have urea, so can do some hydrolysis
    //        if (_urea[layer] < 0.1)
    //            // urea amount is too small, all is hydrolised
    //            result = _urea[layer];
    //        else
    //        {
    //            // get the soil water factor
    //            double swf = Math.Max(0.0, Math.Min(1.0, WF(layer, index) + 0.20));

    //            // get the soil temperature factor
    //            double tf = Math.Max(0.0, Math.Min(1.0, (st[layer] / 40.0) + 0.20));

    //            // note (jngh) oc & ph are not updated during simulation
    //            //      mep    following equation would be better written in terms of hum_C and biom_C
    //            //      mep    oc(layer) = (hum_C(layer) + biom_C(layer))*soiln2_fac (layer)*10000.

    //            // get potential fraction of urea for hydrolysis
    //            double ak = Math.Max(0.25, Math.Min(1.0, -1.12 + 1.31 * (hum_c[layer] + biom_c[layer]) + 0.203 * ph[layer] - 0.155 * (hum_c[layer] + biom_c[layer]) * ph[layer]));
    //            //double ak = Math.Max(0.25, Math.Min(1.0, -1.12 + 1.31 * OC_reset[layer] + 0.203 * ph[layer] - 0.155 * OC_reset[layer] * ph[layer]));

    //            //get amount hydrolysed;
    //            result = Math.Max(0.0, Math.Min(_urea[layer], ak * _urea[layer] * Math.Min(swf, tf)));
    //        }
    //    }
    //    else
    //        result = 0.0;
    //    return result;
    //}

    //private double Nitrification(int layer)
    //{
    //    // + Purpose
    //    //     Calculate the amount of NH4 converted to NO3 via nitrification

    //    // + Notes
    //    //        This routine is much simplified from original CERES code
    //    //        pH effect on nitrification is not invoked

    //    // dsg 200508  use different values for some constants when anaerobic conditions dominate
    //    int index;                 // index = 1 for aerobic and 2 for anaerobic conditions
    //    index = (!is_pond_active) ? 1 : 2;

    //    // get the soil ph factor
    //    double phf = pHFNitrf(layer);

    //    // get the soil  water factor
    //    double wfd = WFNitrf(layer, index);

    //    // get the soil temperature factor
    //    double tf = TF(layer, index);

    //    // calculate the optimum nitrification rate (ppm)
    //    double nh4_ppm = _nh4[layer] * convFactor_kgha2ppm(layer);
    //    double opt_nitrif_rate_ppm = MathUtility.Divide(nitrification_pot * nh4_ppm, nh4_ppm + nh4_at_half_pot, 0.0);

    //    // calculate the optimum nitrification rate (kgN/ha)
    //    double opt_nitrif_rate = MathUtility.Divide(opt_nitrif_rate_ppm, convFactor_kgha2ppm(layer), 0.0);

    //    // calculate the theoretical nitrification rate (after limiting factor and inhibition)
    //    double theor_nitrif_rate = opt_nitrif_rate * Math.Min(wfd, Math.Min(tf, phf)) * Math.Max(0.0, 1.0 - _nitrification_inhibition[layer]);
    //    // NOTE: factors to adjust rate of nitrification are used combined index, with phn removed to match CERES v1

    //    // calculate the actual nitrification rate (make sure NH4 will not go below minimum value)
    //    double actual_nitrif_rate = Math.Max(0.0, Math.Min(theor_nitrif_rate, _nh4[layer] - nh4_min[layer]));

    //    //dlt_nh4_dnit[layer] = actual_nitrif_rate * dnit_nitrf_loss;
    //    //effective_nitrification[layer] = actual_nitrif_rate - dlt_nh4_dnit[layer];
    //    //n2o_atm[layer] += dlt_nh4_dnit[layer];

    //    return actual_nitrif_rate;
    //}

    //private double DenitrificationInNitrification(int layer)
    //{
    //    // + Purpose
    //    //     Calculate the amount of N2O produced during nitrification

    //    double result = dlt_nitrification[layer] * dnit_nitrf_loss;

    //    return result;
    //}

    //private double Denitrification(int layer)
    //{
    //    // + Purpose
    //    //     Calculate the amount of N2O produced during denitrification

    //    //+  Purpose
    //    //     Calculate amount of NO3 transformed via denitrification.
    //    //       Will happend whenever: 
    //    //         - the soil water in the layer > the drained upper limit (Godwin et al., 1984),
    //    //         - the NO3 nitrogen concentration > 1 mg N/kg soil,
    //    //         - the soil temperature >= a minimum temperature.

    //    // + Assumptions
    //    //     That there is a root system present.  Rolston et al. say that the denitrification rate coeffficient (dnit_rate_coeff) of non-cropped
    //    //       plots was 0.000168 and for cropped plots 3.6 times more (dnit_rate_coeff = 0.0006). The larger rate coefficient was required
    //    //       to account for the effects of the root system in consuming oxygen and in adding soluble organic C to the soil.

    //    //+  Notes
    //    //       Reference: Rolston DE, Rao PSC, Davidson JM, Jessup RE (1984). "Simulation of denitrification losses of Nitrate fertiliser applied
    //    //        to uncropped, cropped, and manure-amended field plots". Soil Science Vol 137, No 4, pp 270-278.
    //    //
    //    //       Reference for Carbon availability factor: Reddy KR, Khaleel R, Overcash MR (). "Carbon transformations in land areas receiving 
    //    //        organic wastes in relation to nonpoint source pollution: A conceptual model".  J.Environ. Qual. 9:434-442.

    //    // make sure no3 will not go below minimum
    //    if (_no3[layer] < no3_min[layer])
    //        return 0.0;


    //    // get available carbon from soil organic pools
    //    double active_c = actC_p1 * (hum_c[layer] + fom_c_pool1[layer] + fom_c_pool2[layer] + fom_c_pool3[layer]) * convFactor_kgha2ppm(layer) + actC_p2;
    //    // Note CM V2 had active_c = fom_C_conc + 0.0031*hum_C_conc + 24.5

    //    // get the soil water factor
    //    double wf = WFDenit(layer);

    //    // get the soil temperature factor
    //    double tf = Math.Max(0.0, Math.Min(1.0, 0.1 * Math.Exp(0.046 * st[layer])));
    //    // This is an empirical dimensionless function to account for the effect of temperature.
    //    // The upper limit of 1.0 means that optimum denitrification temperature is 50 oC and above.  At 0 oC it is 0.1 of optimum, and at -20 oC is about 0.04.

    //    // calculate denitrification rate  - kg/ha
    //    double result = dnit_rate_coeff * active_c * wf * tf * _no3[layer];

    //    // prevent no3 from falling below NO3_min
    //    result = Math.Max(0.0, Math.Min(result, _no3[layer] - no3_min[layer]));

    //    return result;
    //}

    //private double Denitrification_Nratio(int layer)
    //{
    //    // + Purpose
    //    //     Calculate the N2 to N2O ration during denitrification

    //    // the water filled pore space (%)
    //    double WFPS = sw_dep[layer] / sat_dep[layer] * 100.0;

    //    // CO2 production today (kgC/ha)
    //    double CO2_prod = (dlt_c_fom_2_atm[0][layer] + dlt_c_fom_2_atm[1][layer] + dlt_c_fom_2_atm[2][layer] + dlt_c_biom_2_atm[layer] + dlt_c_hum_2_atm[layer]);

    //    // calculate the terms for the formula from Thornburn et al (2010)
    //    double RtermA = dnit_A * dnit_k1;
    //    double RtermB = 0.0;
    //    if (CO2_prod > 0.0)
    //        RtermB = dnit_k1 * Math.Exp(-dnit_B * (_no3[layer] / CO2_prod));
    //    double RtermC = dnit_C;
    //    bool didInterpolate;
    //    double RtermD = MathUtility.LinearInterpReal(WFPS, dnit_wfps, dnit_n2o_factor, out didInterpolate);
    //    // RTermD = (0.015 * WFPS) - 0.32;

    //    double result = Math.Max(RtermA, RtermB) * Math.Max(RtermC, RtermD);

    //    return result;
    //}

    //private void CheckProfile(float[] newProfile)
    //{
    //    // + Purpose
    //    //     Check whether profile has changed and move values between layers

    //    // How to decide:
    //    // if bedrock is lower than lowest  profile depth, we won't see
    //    // any change in profile, even if there is erosion. Ideally we
    //    // should test both soil_loss and dlayer for changes to cater for
    //    // manager control. But, the latter means we have to fudge enr for the
    //    // loss from top layer.

    //    dlt_n_loss_in_sed = 0.0;
    //    dlt_c_loss_in_sed = 0.0;
    //    if (soil_loss > 0.0 && AllowProfileReduction)
    //    {
    //        // move pools
    //        // EJZ:: Why aren't no3 and urea moved????
    //        dlt_n_loss_in_sed += MoveLayers(ref _nh4, newProfile);
    //        dlt_c_loss_in_sed += MoveLayers(ref inert_c, newProfile);
    //        dlt_c_loss_in_sed += MoveLayers(ref biom_c, newProfile);
    //        dlt_n_loss_in_sed += MoveLayers(ref biom_n, newProfile);
    //        dlt_c_loss_in_sed += MoveLayers(ref hum_c, newProfile);
    //        dlt_n_loss_in_sed += MoveLayers(ref hum_n, newProfile);
    //        dlt_n_loss_in_sed += MoveLayers(ref fom_n_pool1, newProfile);
    //        dlt_n_loss_in_sed += MoveLayers(ref fom_n_pool2, newProfile);
    //        dlt_n_loss_in_sed += MoveLayers(ref fom_n_pool3, newProfile);
    //        dlt_c_loss_in_sed += MoveLayers(ref fom_c_pool1, newProfile);
    //        dlt_c_loss_in_sed += MoveLayers(ref fom_c_pool2, newProfile);
    //        dlt_c_loss_in_sed += MoveLayers(ref fom_c_pool3, newProfile);

    //    }
    //    if (dlayer == null || newProfile.Length != dlayer.Length)
    //        ResizeLayerArrays(newProfile.Length);
    //    dlayer = newProfile;
    //}

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

                //int nLayers = dlayer.Length;
                //soilp_dlt_res_c_atm = new double[nLayers];
                //soilp_dlt_res_c_hum = new double[nLayers];
                //soilp_dlt_res_c_biom = new double[nLayers];
                //soilp_dlt_org_p = new double[nLayers];
                //double soilp_cpr = MathUtility.Divide(SumDoubleArray(pot_p_decomp), SumDoubleArray(pot_c_decomp), 0.0);  // P:C ratio for potential decomposition

                //SurfaceOrganicMatterDecompType SOMDecomp = new SurfaceOrganicMatterDecompType();
                //Array.Resize(ref SOMDecomp.Pool, num_residues);


                //for (int residue = 0; residue < num_residues; residue++)
                //{
                //    double c_summed = 0.0;
                //    double n_summed = 0.0;
                //    double[] dlt_res_c_decomp = new double[nLayers];
                //    double[] dlt_res_n_decomp = new double[nLayers];
                //    for (int layer = 0; layer < nLayers; layer++)
                //    {
                //        dlt_res_c_decomp[layer] = dlt_c_res_2_hum[layer][residue] +
                //                                  dlt_c_res_2_biom[layer][residue] +
                //                                  dlt_c_res_2_atm[layer][residue];
                //        c_summed += dlt_res_c_decomp[layer];

                //        dlt_res_n_decomp[layer] = this.dlt_n_decomp[layer][residue];
                //        n_summed += dlt_res_n_decomp[layer];
                //    }

                //    // dsg 131103  Now, pack up the structure to return decompositions to SurfaceOrganicMatter
                //    SOMDecomp.Pool[residue] = new SurfaceOrganicMatterDecompPoolType();
                //    SOMDecomp.Pool[residue].FOM = new FOMType();
                //    SOMDecomp.Pool[residue].Name = residue_name[residue];
                //    SOMDecomp.Pool[residue].OrganicMatterType = residue_type[residue];

                //    // dsg 131103   The 'amount' value will not be used by SurfaceOrganicMatter, so send zero as default
                //    SOMDecomp.Pool[residue].FOM.amount = 0.0F;
                //    if (Math.Abs(c_summed) < epsilon)
                //        c_summed = 0.0;
                //    if (Math.Abs(n_summed) < epsilon)
                //        n_summed = 0.0;
                //    SOMDecomp.Pool[residue].FOM.C = (float)c_summed;
                //    SOMDecomp.Pool[residue].FOM.N = (float)n_summed;

                //    // dsg 131103   The 'P' value will not be collected by SurfaceOrganicMatter, so send zero as default.
                //    SOMDecomp.Pool[residue].FOM.P = 0.0F;
                //    SOMDecomp.Pool[residue].FOM.AshAlk = 0.0F;

                //    // dsg 131004 soilp needs some stuff - very ugly process - needs to be streamlined
                //    //  create some variables which soilp can "get" - layer based arrays independant of residues
                //    for (int layer = 0; layer < nLayers; layer++)
                //    {
                //        soilp_dlt_res_c_atm[layer] += dlt_c_res_2_atm[layer][residue];
                //        soilp_dlt_res_c_hum[layer] += dlt_c_res_2_hum[layer][residue];
                //        soilp_dlt_res_c_biom[layer] += dlt_c_res_2_biom[layer][residue];
                //        soilp_dlt_org_p[layer] += dlt_res_c_decomp[layer] * soilp_cpr;
                //    }
            }

            // send the decomposition information
            actualresiduedecompositioncalculated.Invoke(SOMDecomp);
        }
    }

    //private double LayerFract(int layer)
    //{
    //    double layerFract = soil_loss * convFactor_kgha2ppm(layer) / 1000.0;
    //    if (layerFract > 1.0)
    //    {
    //        int layerNo = layer + 1; // Convert to 1-based index for display
    //        double layerPercent = layerFract * 100.0; // Convert fraction to percentage
    //        throw new Exception("Soil loss is greater than depth of layer(" + layerNo.ToString() + ") by " +
    //            layerPercent.ToString() + "%.\nConstrained to this layer. Re-mapping of SoilN pools will be incorrect.");
    //    }
    //    return Math.Min(0.0, layerFract);
    //}

    #endregion

    #region Auxiliar processes

    //private double[] FractFomC(int fract)
    //{
    //    switch (fract)
    //    {
    //        case 0: return fom_c_pool1;
    //        case 1: return fom_c_pool2;
    //        case 2: return fom_c_pool3;
    //        default: throw new Exception("Coding error: bad fraction in FractFomC");
    //    }
    //}

    //private double[] FractFomN(int fract)
    //{
    //    switch (fract)
    //    {
    //        case 0: return fom_n_pool1;
    //        case 1: return fom_n_pool2;
    //        case 2: return fom_n_pool3;
    //        default: throw new Exception("Coding error: bad fraction in FractFomN");
    //    }
    //}

    //private double[] FractRDFom(int fract)
    //{
    //    switch (fract)
    //    {
    //        case 0: return rd_carb;
    //        case 1: return rd_cell;
    //        case 2: return rd_lign;
    //        default: throw new Exception("Coding error: bad fraction in FractRDFom");
    //    }
    //}

    //private double MoveLayers(ref double[] variable, float[] newProfile)
    //{
    //    // + Purpose
    //    //     Move the values of a given varible between layers, from bottom to top
    //    //      Changed from subroutine to function returning amount of profile loss

    //    double profile_loss = 0.0;
    //    double layer_loss = 0.0;
    //    double layer_gain = 0.0;
    //    int lowest_layer = dlayer.Length;
    //    int new_lowest_layer = newProfile.Length;

    //    double yesterdays_n = SumDoubleArray(variable);

    //    // initialise layer loss from below profile same as bottom layer

    //    double profile_depth = SumFloatArray(dlayer);
    //    double new_profile_depth = SumFloatArray(newProfile);

    //    if (MathUtility.FloatsAreEqual(profile_depth, new_profile_depth))
    //    {
    //        // move from below bottom layer - assume it has same properties
    //        // as bottom layer
    //        layer_loss = variable[lowest_layer - 1] * LayerFract(lowest_layer - 1);
    //    }
    //    else
    //    {
    //        // we're going into bedrock
    //        layer_loss = 0.0;
    //        // now see if bottom layers have been merged.
    //        if (lowest_layer > new_lowest_layer && lowest_layer > 1)
    //        {
    //            // merge the layers
    //            for (int layer = lowest_layer - 1; layer >= new_lowest_layer; layer--)
    //            {
    //                variable[layer - 1] += variable[layer];
    //                variable[layer] = 0.0;
    //            }
    //            Array.Resize(ref variable, new_lowest_layer);
    //        }
    //    }
    //    double profile_gain = layer_loss;

    //    // now move from bottom layer to top
    //    for (int layer = new_lowest_layer - 1; layer >= 0; layer--)
    //    {
    //        // this layer gains what the lower layer lost
    //        layer_gain = layer_loss;
    //        layer_loss = variable[layer] * LayerFract(layer);
    //        variable[layer] += layer_gain - layer_loss;
    //    }

    //    // now adjust top layer for enrichment
    //    double enr = enr_a_coeff * Math.Pow(soil_loss * 1000, -1.0 * enr_b_coeff);
    //    enr = Math.Max(1.0, Math.Min(enr, enr_a_coeff));

    //    profile_loss = layer_loss * enr;
    //    variable[0] = Math.Max(0.0, variable[0] + layer_loss - profile_loss);

    //    // check mass balance
    //    double todays_n = SumDoubleArray(variable);
    //    yesterdays_n += profile_gain - profile_loss;
    //    if (!MathUtility.FloatsAreEqual(todays_n, yesterdays_n))
    //    {
    //        throw new Exception("N mass balance out");
    //    }
    //    return profile_loss;
    //}

    //private double pHFNitrf(int layer)
    //{
    //    // +  Purpose
    //    //      Calculates a 0-1 pH factor for nitrification.

    //    bool DidInterpolate;
    //    return MathUtility.LinearInterpReal(ph[layer], pHf_nit_pH, pHf_nit_values, out DidInterpolate);
    //}

    //private double WFNitrf(int layer, int index)
    //{
    //    // +  Purpose
    //    //      Calculates a 0-1 water factor for nitrification.

    //    // +  Assumptions
    //    //     index = 1 for aerobic conditions, 2 for anaerobic

    //    // temporary water factor (0-1)
    //    double wfd = 1.0;
    //    if (sw_dep[layer] > dul_dep[layer] && sat_dep[layer] > dul_dep[layer])
    //    {   // saturated
    //        wfd = 1.0 + (sw_dep[layer] - dul_dep[layer]) / (sat_dep[layer] - dul_dep[layer]);
    //        wfd = Math.Max(1.0, Math.Min(2.0, wfd));
    //    }
    //    else
    //    {
    //        // unsaturated
    //        // assumes rate of mineralisation is at optimum rate until soil moisture midway between dul and ll15
    //        wfd = MathUtility.Divide(sw_dep[layer] - ll15_dep[layer], dul_dep[layer] - ll15_dep[layer], 0.0);
    //        wfd = Math.Max(0.0, Math.Min(1.0, wfd));
    //    }

    //    bool didInterpolate;
    //    if (index == 1)
    //        return MathUtility.LinearInterpReal(wfd, wfnit_index, wfnit_values, out didInterpolate);
    //    else
    //        // if pond is active, and aerobic conditions dominate, assume wf_nitrf = 0
    //        return 0;
    //}

    //private double WFDenit(int layer)
    //{
    //    // + Purpose
    //    //     Calculates a 0-1 water factor for denitrification

    //    // temporary water factor (0-1); 0 is used if unsaturated
    //    double wfd = 0.0;
    //    if (sw_dep[layer] > dul_dep[layer] && sat_dep[layer] > dul_dep[layer])  // saturated
    //        wfd = Math.Pow((sw_dep[layer] - dul_dep[layer]) / (sat_dep[layer] - dul_dep[layer]), dnit_wf_power);
    //    return Math.Max(0.0, Math.Min(1.0, wfd));
    //}

    //private double WF(int layer, int index)
    //{
    //    // + Purpose
    //    //     Calculates a 0-1 water factor for mineralisation.

    //    // + Assumptions
    //    //     index = 1 for aerobic conditions, 2 for anaerobic

    //    // temporary water factor (0-1)
    //    double wfd;
    //    if (sw_dep[layer] > dul_dep[layer])
    //    { // saturated
    //        if (sat_dep[layer] == dul_dep[layer])
    //            wfd = 1.0;
    //        else
    //            wfd = Math.Max(1.0, Math.Min(2.0,
    //                1.0 + (sw_dep[layer] - dul_dep[layer]) / (sat_dep[layer] - dul_dep[layer])));
    //    }
    //    else
    //    { // unsaturated
    //        // assumes rate of mineralisation is at optimum rate until soil moisture midway between dul and ll15
    //        if (dul_dep[layer] == ll15_dep[layer])
    //            wfd = 0.0;
    //        else
    //            wfd = Math.Max(0.0, Math.Min(1.0, (sw_dep[layer] - ll15_dep[layer]) / (dul_dep[layer] - ll15_dep[layer])));
    //    }

    //    if (index == 1)
    //    {
    //        bool didInterpolate;
    //        return MathUtility.LinearInterpReal(wfd, wfmin_index, wfmin_values, out didInterpolate);
    //    }
    //    else if (index == 2) // if pond is active, and liquid conditions dominate, assume wf = 1
    //        return 1.0;
    //    else
    //        throw new Exception("SoilN2 WF function - invalid value for \"index\" parameter");
    //}

    //private double TF(int layer, int index)
    //{
    //    // + Purpose
    //    //     Calculate a temperature factor, based on the soil temperature of the layer, for nitrification and mineralisation

    //    // + Assumptions
    //    //     index = 1 for aerobic conditions, 2 for anaerobic

    //    // Alternate version from CM:
    //    //      tf = (soil_temp[layer] - 5.0) /30.0
    //    // because tf is bound between 0 and 1, the effective temperature (soil_temp) lies between 5 to 35.
    //    // alternative quadratic temperature function is preferred with optimum temperature (CM - used 32 deg)

    //    if (st[layer] > 0.0)
    //    {
    //        if (opt_temp[index - 1] == 0.0)
    //            return 0.0;
    //        else
    //            return Math.Max(0.0, Math.Min(1.0, (st[layer] * st[layer]) / Math.Pow(opt_temp[index - 1], 2.0)));
    //    }
    //    else // soil is too cold for mineralisation
    //        return 0.0;
    //}

    //private double RothcTF(int layer, int index)
    //{
    //    // + Purpose
    //    //     Calculate a temperature factor, based on the soil temperature of the layer, for nitrification and mineralisation

    //    double t = Math.Min(st[layer], opt_temp[layer]);
    //    return 47.9 / (1.0 + Math.Exp(106.0 / (t + 18.3)));
    //}

    private double convFactor_kgha2ppm(int layer)
    {
        // Calculate conversion factor from kg/ha to ppm (mg/kg)

        if (bd == null || dlayer == null || bd.Length == 0 || dlayer.Length == 0)
        {
            return 0.0;
            throw new Exception(" Error on computing convertion factor, kg/ha to ppm. Value for dlayer or bulk density not valid");
        }
        return MathUtility.Divide(100.0, bd[layer] * dlayer[layer], 0.0);
    }

    #endregion

    #endregion

    #region general auxiliar procedures

    private double SumDoubleArray(double[] anArray)
    {
        double result = 0.0;
        if (anArray != null)
        {
            foreach (double Value in anArray)
                result += Value;
        }
        return result;
    }

    //private float SumFloatArray(float[] anArray)
    //{
    //    float result = 0.0F;
    //    foreach (float Value in anArray)
    //        result += Value;
    //    return result;
    //}

    private int getCumulativeIndex(double sum, float[] realArray)
    {
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