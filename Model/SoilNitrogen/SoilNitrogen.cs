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
/// Further changes on September/2012, more tidying up, moving pieces of code aroun, updating error messages and finish cleaning the variable imputs
/// </summary>

public class SoilNitrogen
{

    [Link]
    Component My = null;

    #region Parameters used to initialise the model

    #region Parameters expected to be provided by the user or by APSIM

    // soil model type, spec used to determine some mineralisation model variations
    [Param(IsOptional = true)]
    private string soiltype = "standard";

    #region values to initialise simpleSoilTemp

    // local latidute
    [Input(IsOptional = true)]
    [Units("deg")]
    private double latitude = -999.0;

    // annual average ambient air temperature (oC)
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 55.0)]
    [Input(IsOptional = true)]
    [Units("oC")]
    private double tav = -999.0;

    // annual amplitude of the mean monthly air temperature (oC)
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 50.0)]
    [Input(IsOptional = true)]
    [Units("oC")]
    private double amp = -999.0;

    #endregion

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

    // initial proportion of total soil C that is not subject to mineralization (0-1)
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
    private double[] OC_reset;      // stores initial values, can be used for a Reset operation
    private double[] _oc;           // this can be set, but doesn't get updated internally, the fractions are updated
    [Param]
    [Output]
    [Units("%")]
    [Description("Soil organic carbon")]
    double[] oc
    {
        get
        {
            if (dlayer == null || hum_c == null || biom_c == null)
                return null;

            double[] oc_percent = new double[dlayer.Length];
            for (int i = 0; i < dlayer.Length; i++)
            {
                oc_percent[i] = (hum_c[i] + biom_c[i]) * convFactor_kgha2ppm(i) / 10000;  // (100/1000000) = convert to ppm and then to %
            }
            return oc_percent;
        }
        set
        {
            if (!initDone)
                OC_reset = value;
            _oc = value;
        }
    }

    #endregion

    #region values to initialise soil mineral N

    // soil urea nitrogen content (ppm) 
    private double[] ureappm_reset;     // stores initial value, can be used for a Reset operation
    private double[] _ureappm;          // local variable to hold parameter value until we can get dlayer!
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 100000.0)]
    [Output]
    [Units("mg/kg")]
    [Description("Soil urea nitrogen content")]
    private double[] ureappm
    {
        get
        {
            if (initDone)
            {
                Array.Resize(ref _ureappm, _urea.Length);
                for (int layer = 0; layer < _urea.Length; ++layer)
                    _ureappm[layer] = _urea[layer] * convFactor_kgha2ppm(layer);
            }
            return _ureappm;
        }
        set
        {
            if (initDone)
            {
                double sumOld = SumDoubleArray(_urea);
                Array.Resize(ref _urea, value.Length);
                for (int layer = 0; layer < value.Length; ++layer)
                {
                    if (value[layer] < nh4ppm_min - epsilon)
                    {
                        Console.WriteLine(" ureappm(" + layer.ToString() + ") = " + value[layer].ToString() + "ppm is less than lower limit of " + ureappm_min.ToString() + ", value will be adjusted to minimum value.");
                        value[layer] = ureappm_min;
                    }
                    _urea[layer] = MathUtility.Divide(value[layer], convFactor_kgha2ppm(layer), 0.0);
                }
                if (!inReset)
                    SendExternalMassFlowN(SumDoubleArray(_urea) - sumOld);
            }
            else
            {
                _ureappm = value;
                ureappm_reset = value;
            }
        }
    }

    // soil urea nitrogen amount (kgN/ha)
    private double[] _urea;     // Internal variable holding the urea amounts
    [Output]
    [Units("kgN/ha")]
    [Description("Soil urea nitrogen amount")]
    double[] urea
    {
        get { return _urea; }
        set
        {
            double sumOld = SumDoubleArray(_urea);
            for (int layer = 0; layer < Math.Min(value.Length, _urea.Length); ++layer)
                if (layer >= _urea.Length)
                {
                    Console.WriteLine(" Attempt to assign urea value to a non-existent soil layer - extra values will be ignored");
                    break;
                }
                else
                {
                    if (value[layer] < nh4_min[layer] - epsilon)
                    {
                        Console.WriteLine(" urea(" + layer.ToString() + ") = " + value[layer].ToString() + "kg/ha is less than lower limit of " + urea_min[layer].ToString() + ", value will be adjusted to minimum value.");
                        value[layer] = urea_min[layer];
                    }
                    _urea[layer] = value[layer];
                }
            SendExternalMassFlowN(SumDoubleArray(_urea) - sumOld);
        }
    }

    // soil ammonium nitrogen content (ppm)
    private double[] nh4ppm_reset;      // stores initial value, can be used for a Reset operation
    private double[] _nh4ppm;           // local variable to hold parameter value until we can get dlayer!
    [Param]
    [Output]
    [Units("mg/kg")]
    [Description("Soil ammonium nitrogen content")]
    private double[] nh4ppm
    {
        get
        {
            if (initDone)
            {
                Array.Resize(ref _nh4ppm, _nh4.Length);
                for (int layer = 0; layer < _nh4.Length; ++layer)
                    _nh4ppm[layer] = _nh4[layer] * convFactor_kgha2ppm(layer);
            }
            return _nh4ppm;
        }
        set
        {
            if (initDone)
            {
                double sumOld = SumDoubleArray(_nh4);
                Array.Resize(ref _nh4, value.Length);
                for (int layer = 0; layer < value.Length; ++layer)
                {
                    if (value[layer] < nh4ppm_min - epsilon)
                    {
                        Console.WriteLine(" nh4ppm(" + layer.ToString() + ") = " + value[layer].ToString() + "ppm is less than lower limit of " + nh4ppm_min.ToString() + ", value will be adjusted to minimum value.");
                        value[layer] = nh4ppm_min;
                    }
                    _nh4[layer] = MathUtility.Divide(value[layer], convFactor_kgha2ppm(layer), 0.0);
                }
                if (!inReset)
                    SendExternalMassFlowN(SumDoubleArray(_nh4) - sumOld);
            }
            else
            {
                _nh4ppm = value;
                nh4ppm_reset = value;
            }
        }
    }

    private double[] _nh4;     // Internal variable holding the nh4 amounts
    [Output]
    [Units("kgN/ha")]
    [Description("Soil ammonium nitrogen amount")]
    double[] nh4
    {
        get { return _nh4; }
        set
        {
            double sumOld = SumDoubleArray(_nh4);
            for (int layer = 0; layer < value.Length; ++layer)
            {
                if (layer >= _nh4.Length)
                {
                    Console.WriteLine(" Attempt to assign ammonium value to a non-existent soil layer - extra values will be ignored");
                    break;
                }
                else
                {
                    if (value[layer] < nh4_min[layer] - epsilon)
                    {
                        Console.WriteLine(" nh4(" + layer.ToString() + ") = " + value[layer].ToString() + "kg/ha is less than lower limit of " + nh4_min[layer].ToString() + ", value will be adjusted to minimum value.");
                        value[layer] = nh4_min[layer];
                    }
                    _nh4[layer] = value[layer];
                }
            }
            SendExternalMassFlowN(SumDoubleArray(_nh4) - sumOld);
        }
    }

    // soil nitrate nitrogen content (ppm) - Watch out! no3 and no3ppm aren't quite the same thing... different units
    private double[] no3ppm_reset;      // stores initial value, can be used for a Reset operation
    private double[] _no3ppm;           // local variable to hold parameter value until we can get dlayer!
    [Param]
    [Output]
    [Units("mg/kg")]
    [Description("Soil nitrate nitrogen content")]
    private double[] no3ppm
    {
        get
        {
            if (initDone)
            {
                Array.Resize(ref _no3ppm, _no3.Length);
                for (int layer = 0; layer < _no3.Length; ++layer)
                    _no3ppm[layer] = _no3[layer] * convFactor_kgha2ppm(layer);
            }
            return _no3ppm;
        }
        set
        {
            if (initDone)
            {
                double sumOld = SumDoubleArray(_no3);
                Array.Resize(ref _no3, value.Length);
                for (int layer = 0; layer < value.Length; ++layer)
                {
                    if (value[layer] < no3ppm_min - epsilon)
                    {
                        Console.WriteLine(" no3ppm(" + layer.ToString() + ") = " + value[layer].ToString() + "ppm is less than lower limit of " + no3ppm_min.ToString() + ", value will be adjusted to minimum value.");
                        value[layer] = no3ppm_min;
                    }
                    _no3[layer] = MathUtility.Divide(value[layer], convFactor_kgha2ppm(layer), 0.0);
                }
                if (!inReset)
                    SendExternalMassFlowN(SumDoubleArray(_no3) - sumOld);
            }
            else
            {
                _no3ppm = value;
                if (!initDone)
                    no3ppm_reset = value;
            }
        }
    }

    // soil nitrate nitrogen amount (kgN/ha)
    private double[] _no3 = null;
    [Output]
    [Units("kgN/ha")]
    [Description("Soil nitrate nitrogen amount")]
    double[] no3
    {
        get { return _no3; }
        set
        {
            double sumOld = SumDoubleArray(_no3);
            for (int layer = 0; layer < value.Length; ++layer)
            {
                if (layer >= _no3.Length)
                {
                    Console.WriteLine(" Attempt to assign nitrate value to a non-existent soil layer - extra values will be ignored");
                    break;
                }
                else
                {
                    if (value[layer] < no3_min[layer] - epsilon)
                    {
                        Console.WriteLine(" no3(" + layer.ToString() + ") = " + value[layer].ToString() + "kg/ha is less than lower limit of " + no3_min[layer].ToString() + ", value will be adjusted to minimum value.");
                        value[layer] = no3_min[layer];
                    }
                    _no3[layer] = value[layer];
                }
            }
            SendExternalMassFlowN(SumDoubleArray(_no3) - sumOld);
        }
    }

    #endregion

    #endregion

    #region Parameters not usually provided by the user

    // minimum allowable Urea (ppm)
    private double ureappm_min = 0.0;

    // minimum allowable NH4 (ppm)
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    private double nh4ppm_min = 0.0;

    // minimum allowable NO3 (ppm)
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    private double no3ppm_min = 0.0;

    // enrichment equation coefficient a
    [Param]
    private double enr_a_coeff = 0.0;

    // enrichment equation coefficient b
    [Param]
    private double enr_b_coeff = 0.0;

    [Param]
    private string profile_reduction = "off";

    [Param(IsOptional = true)]
    private string use_organic_solutes = "off";

    // C:N ratio of microbes ()
    private double biom_cn = 8.0;
    [Param(IsOptional = true, MinVal = 1.0, MaxVal = 50.0)]
    private double mcn
    {
        get { return biom_cn; }
        set { biom_cn = value; }
    }

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    private double ef_fom;               // fraction of FOM C mineralized retained in system (0-1)   

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    private double fr_fom_biom;          // fraction of retained FOM C transferred to biomass (0-1)

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    private double ef_biom;              // fraction of biomass C mineralized retained in system (0-1)

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    private double fr_biom_biom;         // fraction of retained biomass C returned to biomass (0-1)

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    private double ef_hum;               // fraction of humic C mineralized retained in system (0-1)

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    private double[] rd_biom = null;     // potential rate of soil biomass mineralization (per day)

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    private double[] rd_hum = null;      // potential rate of humus mineralization (per day)

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    private double ef_res;               // fraction of residue C mineralized retained in system (0-1)

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
    private double min_depth;            // depth from which mineral N can be immobilized by decomposing residues (mm)

    [Param(MinVal = 0.0, MaxVal = 10.0)]
    private double cnrf_coeff;           // coeff. to determine the magnitude of C:N effects on decomposition of FOM ()

    [Param(MinVal = 5.0, MaxVal = 100.0)]
    private double cnrf_optcn;           // C:N above which decomposition rate of FOM declines ()

    [Param(MinVal = 5.0, MaxVal = 100.0)]
    private double[] opt_temp;           // Soil temperature above which there is no further effect on mineralisation and nitrification (oC)

    [Param(MinVal = 0.0, MaxVal = 2.0)]
    private double[] wfmin_index;        // index specifying water content for water factor for mineralization

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    private double[] wfmin_values;       // value of water factor(mineralization) function at given index values

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

    [Input]
    DateTime today;

    #region Weather data

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
    private Boolean is_pond_active = false;
    [Input(IsOptional = true)]
    private string pond_active
    { set { is_pond_active = (value == "yes"); } }

    // C decomposed in pond that is added to soil biomass
    [Input(IsOptional = true)]
    [Units("kg/ha")]
    private double pond_biom_C;

    // C decomposed in pond that is added to soil humus
    [Input(IsOptional = true)]
    [Units("kg/ha")]
    private double pond_hum_C;

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
        }
    }

    // factor reducing urea hydrolysis due to the presence of an inhibitor - not implemented yet
    [Input(IsOptional = true)]
    [Units("0-1")]
    private double[] hydrolysis_inhibition = null;

    // factor reducing mineralisation processes due to the presence of an inhibitor - not implemented yet
    [Input(IsOptional = true)]
    [Units("0-1")]
    private double[] mineralisation_inhibition = null;

    #endregion

    #endregion

    #region Outputs we make available to other components

    #region Values that other components can get or set

    [Output]
    [Units("kg/ha")]
    [Description("Not fully implemented")]
    double[] org_c_pool1 // Doesn't seem to be fully implemented
    {
        get { return new double[fom_c_pool1.Length]; }
        set
        {
            for (int layer = 0; layer < value.Length; ++layer)
                fom_c_pool1[layer] += value[layer];
        }
    }

    [Output]
    [Units("kg/ha")]
    [Description("Not fully implemented")]
    double[] org_c_pool2 // Doesn't seem to be fully implemented
    {
        get { return new double[fom_c_pool2.Length]; }
        set
        {
            for (int layer = 0; layer < value.Length; ++layer)
                fom_c_pool2[layer] += value[layer];
        }
    }

    [Output]
    [Units("kg/ha")]
    [Description("Not fully implemented")]
    double[] org_c_pool3 // Doesn't seem to be fully implemented
    {
        get { return new double[fom_c_pool3.Length]; }
        set
        {
            for (int layer = 0; layer < value.Length; ++layer)
                fom_c_pool3[layer] += value[layer];
        }
    }

    [Output]
    [Units("kg/ha")]
    [Description("Not fully implemented")]
    double[] org_n // Doesn't seem to be fully implemented
    {
        get { return new double[fom_n.Length]; }
        set
        {
            for (int layer = 0; layer < value.Length; ++layer)
                fom_n[layer] += value[layer];
        }
    }

    #endregion

    #region Values that other components can only get

    #region Outputs for Nitrogen

    #region General values

    [Output]
    [Units("kgN/ha")]
    [Description("Minimum allowable urea")]
    double[] urea_min;       // minimum allowable Urea

    [Output]
    [Units("kgN/ha")]
    [Description("Minimum allowable NH4")]
    double[] nh4_min;       // minimum allowable NH4

    [Output]
    [Units("kgN/ha")]
    [Description("Minimum allowable NO3")]
    double[] no3_min;       // minimum allowable NO3

    [Output]
    [Units("kgN/ha")]
    [Description("Excess N required above NH4 supply")]
    double[] excess_nh4;    // excess N required above NH4 supply

    #endregion

    #region Changes for today - deltas

    [Output]
    [Units("kgN/ha")]
    [Description("Net NH4 change today")]
    double[] dlt_nh4_net;   // net nh4 change today

    [Output]
    [Units("kgN/ha")]
    [Description("Net NH4 transformation")]
    double[] nh4_transform_net; // net NH4 transformation today

    [Output]
    [Units("kgN/ha")]
    [Description("Net NO3 change today")]
    double[] dlt_no3_net;   // net no3 change today

    [Output]
    [Units("kgN/ha")]
    [Description("Net NO3 transformation")]
    double[] no3_transform_net; // net NO3 transformation today

    [Output]
    [Units("kgN/ha")]
    [Description("Net N mineralized")]
    double[] dlt_n_min         // net mineralisation
    {
        get
        {
            double[] _dlt_n_min = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                _dlt_n_min[layer] = dlt_hum_n_min[layer] + dlt_biom_n_min[layer] + dlt_fom_n_min[layer];
            return _dlt_n_min;
        }
    }

    [Output]
    [Units("kgN/ha")]
    [Description("Net Residue N mineralisation")]
    double[] dlt_n_min_res
    {
        get
        {
            double[] _dlt_n_min_res = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                _dlt_n_min_res[layer] = dlt_res_no3_min[layer] + dlt_res_nh4_min[layer];
            return _dlt_n_min_res;
        }
    }

    [Output]
    [Units("kgN/ha")]
    [Description("Net use of NH4 for residue mineralization/immobilization")]
    double[] dlt_res_nh4_min;   // Net Residue NH4 mineralisation

    [Output]
    [Units("kgN/ha")]
    [Description("Net use of NO3 for residue mineralization/immobilization")]
    double[] dlt_res_no3_min;   // Net Residue NO3 mineralisation

    [Output]
    [Units("kgN/ha")]
    [Description("Net FOM N mineralized, negative for immobilization")]
    double[] dlt_fom_n_min;     // net fom N mineralized (negative for immobilization) 

    [Output]
    [Units("kgN/ha")]
    [Description("Net humic N mineralized")]
    double[] dlt_hum_n_min;     // net humic N mineralized

    [Output]
    [Units("kgN/ha")]
    [Description("Humic N mineralized")]
    double[] dlt_n_min_tot
    {
        get
        {
            double[] _dlt_n_min_tot = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                _dlt_n_min_tot[layer] = dlt_hum_n_min[layer] + dlt_biom_n_min[layer] + dlt_fom_n_min[layer] + dlt_res_no3_min[layer] + dlt_res_nh4_min[layer];
            return _dlt_n_min_tot;
        }
    }

    [Output]
    [Units("kgN/ha")]
    [Description("Net biomass N mineralized")]
    double[] dlt_biom_n_min;    // net biomass N mineralized

    [Output]
    [Units("kgN/ha")]
    [Description("Nitrogen coverted by hydrolysis")]
    double[] dlt_urea_hydrol;   // nitrogen coverted by hydrolysis (from urea to NH4)

    double[] dlt_nitrification;     // nitrogen coverted by nitrification (from NH4 to either NO3 or N2O)
    [Output]
    [Units("kgN/ha")]
    [Description("Nitrogen coverted by nitrification")]
    double[] dlt_rntrf
    { get { return dlt_nitrification; } }

    [Output]
    [Units("kgN/ha")]
    [Description("Nitrogen coverted by nitrification")]
    double[] nitrification
    { get { return dlt_nitrification; } }

    [Output]
    [Units("kgN/ha")]
    [Description("Effective nitrogen coverted by nitrification")]
    double[] effective_nitrification; // effective nitrogen coverted by nitrification (from NH4 to NO3)
    // (Alias dlt_rntrf_eff)

    [Output]
    [Units("kgN/ha")]
    [Description("NH4 N denitrified")]
    double[] dlt_nh4_dnit;      // NH4 N denitrified

    [Output]
    [Units("kgN/ha")]
    [Description("NO3 N denitrified")]
    double[] dlt_no3_dnit;      // NO3 N denitrified

    [Output]
    [Units("kgN/ha")]
    [Description("Amount of N2O produced")]
    double[] n2o_atm;           // amount of N2O produced

    [Output]
    [Units("kgN/ha")]
    [Description("Denitrification")]
    double[] dnit
    {
        get
        {
            double[] dnit_tot = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                dnit_tot[layer] = dlt_no3_dnit[layer] + dlt_nh4_dnit[layer];
            return dnit_tot;
        }
    }

    [Output]
    [Units("kg")]
    [Description("N loss in sediment")]
    double dlt_n_loss_in_sed;

    #endregion

    #region Amounts in various pools

    [Output]
    [Units("kgN/ha")]
    [Description("Nitrogen in FOM")]
    double[] fom_n;         // nitrogen in FOM

    [Output]
    [Units("kgN/ha")]
    [Description("Nitrogen in FOM pool 1")]
    double[] fom_n_pool1;

    [Output]
    [Units("kgN/ha")]
    [Description("Nitrogen in FOM pool 2")]
    double[] fom_n_pool2;

    [Output]
    [Units("kgN/ha")]
    [Description("Nitrogen in FOM pool 3")]
    double[] fom_n_pool3;

    [Output]
    [Units("kgN/ha")]
    [Description("Humic nitrogen")]
    double[] hum_n;         // Humic N

    [Output]
    [Units("kgN/ha")]
    [Description("Biomass nitrogen")]
    double[] biom_n;        // biomass nitrogen

    [Output]
    [Units("kgN/ha")]
    [Description("Total N in soil")]
    double[] nit_tot           // total N in soil   
    {
        get
        {
            double[] nitrogen_tot = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                nitrogen_tot[layer] += fom_n[layer] + hum_n[layer] + biom_n[layer] + _no3[layer] + _nh4[layer] + _urea[layer];
            return nitrogen_tot;
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
            double deltaN = TotalN() - dailyInitialN; // Delta storage
            double losses = 0.0;
            for (int layer = 0; layer < dlayer.Length; layer++)
                losses += dlt_no3_dnit[layer] + dlt_nh4_dnit[layer];
            return -(losses + deltaN);
        }
    }

    #endregion

    #endregion

    #region Outputs for Carbon

    #region General values

    [Output]
    [Description("Number of FOM types")]
    private int num_fom_types      // number of fom types read
    { get { return fom_types.Length; } }

    [Output]
    [Description("Fraction of carbohydrate in FOM")]
    private double fr_carb            // carbohydrate fraction of FOM (0-1)          
    { get { return fract_carb[fom_type]; } }

    [Output]
    [Description("Fraction of cellulose in FOM")]
    private double fr_cell            // cellulose fraction of FOM (0-1)          
    { get { return fract_cell[fom_type]; } }

    [Output]
    [Description("Fraction of lignin in FOM")]
    private double fr_lign            // lignin fraction of FOM (0-1)          
    { get { return fract_lign[fom_type]; } }

    #endregion

    #region Changes for today - deltas

    [Output]
    [Units("kg")]
    [Description("Carbon loss in sediment")]
    double dlt_c_loss_in_sed;

    [Output]
    [Units("oC")]
    [Description("Soil temperature")]
    double[] st;

    double[][] _dlt_fom_c_hum = new double[3][];
    [Output]
    [Units("kg/ha")]
    [Description("FOM C converted to humic")]
    double[] dlt_fom_c_hum  // fom C converted to humic (kg/ha)
    {
        get
        {
            int nLayers = _dlt_fom_c_hum[0].Length;
            double[] result = new double[nLayers];
            for (int layer = 0; layer < nLayers; layer++)
                result[layer] = _dlt_fom_c_hum[0][layer] + _dlt_fom_c_hum[1][layer] + _dlt_fom_c_hum[2][layer];
            return result;
        }
    }

    double[][] _dlt_fom_c_biom = new double[3][];
    [Output]
    [Units("kg/ha")]
    [Description("FOM C converted to biomass")]
    double[] dlt_fom_c_biom // fom C converted to biomass (kg/ha)
    {
        get
        {
            int nLayers = _dlt_fom_c_biom[0].Length;
            double[] result = new double[nLayers];
            for (int layer = 0; layer < nLayers; layer++)
                result[layer] = _dlt_fom_c_biom[0][layer] + _dlt_fom_c_biom[1][layer] + _dlt_fom_c_biom[2][layer];
            return result;
        }
    }

    double[][] _dlt_fom_c_atm = new double[3][];
    [Output]
    [Units("kg/ha")]
    [Description("FOM C lost to atmosphere")]
    double[] dlt_fom_c_atm  // fom C lost to atmosphere (kg/ha)
    {
        get
        {
            int nLayers = _dlt_fom_c_atm[0].Length;
            double[] result = new double[nLayers];
            for (int layer = 0; layer < nLayers; layer++)
                result[layer] = _dlt_fom_c_atm[0][layer] + _dlt_fom_c_atm[1][layer] + _dlt_fom_c_atm[2][layer];
            return result;
        }
    }

    [Output]
    [Units("kg/ha")]
    [Description("Humic C converted to biomass")]
    double[] dlt_hum_c_biom;

    [Output]
    [Units("kg/ha")]
    [Description("Humic C lost to atmosphere")]
    double[] dlt_hum_c_atm;

    [Output]
    [Units("kg/ha")]
    [Description("Biomass C converted to humic")]
    double[] dlt_biom_c_hum;

    [Output]
    [Units("kg/ha")]
    [Description("Biomass C lost to atmosphere")]
    double[] dlt_biom_c_atm;

    double[][] _dlt_res_c_biom;
    [Output]
    [Units("kg/ha")]
    [Description("Carbon from residues converted to biomass")]
    double[] dlt_res_c_biom
    {
        get
        {
            int nLayers = _dlt_res_c_biom.Length;
            double[] result = new double[nLayers];
            for (int layer = 0; layer < nLayers; layer++)
                result[layer] = SumDoubleArray(_dlt_res_c_biom[layer]);
            return result;
        }
    }

    double[][] _dlt_res_c_hum;
    [Output]
    [Units("kg/ha")]
    [Description("Carbon from residues converted to humic")]
    double[] dlt_res_c_hum
    {
        get
        {
            int nLayers = _dlt_res_c_hum.Length;
            double[] result = new double[nLayers];
            for (int layer = 0; layer < nLayers; layer++)
                result[layer] = SumDoubleArray(_dlt_res_c_hum[layer]);
            return result;
        }
    }

    double[][] _dlt_res_c_atm;
    [Output]
    [Units("kg/ha")]
    [Description("Carbon from residues lost to atmosphere")]
    double[] dlt_res_c_atm
    {
        get
        {
            int nLayers = _dlt_res_c_atm.Length;
            double[] result = new double[nLayers];
            for (int layer = 0; layer < nLayers; layer++)
                result[layer] = SumDoubleArray(_dlt_res_c_atm[layer]);
            return result;
        }
    }

    [Output]
    [Units("kg/ha")]
    [Description("Delta FOM C pool in fraction 1")]
    double[] dlt_fom_c_pool1;

    [Output]
    [Units("kg/ha")]
    [Description("Delta FOM C pool in fraction 2")]
    double[] dlt_fom_c_pool2;

    [Output]
    [Units("kg/ha")]
    [Description("Delta FOM C pool in fraction 3")]
    double[] dlt_fom_c_pool3;

    [Output]
    [Units("kg/ha")]
    [Description("Carbon from all residues to atmosphere")]
    double[] soilp_dlt_res_c_atm;

    [Output]
    [Units("kg/ha")]
    [Description("Carbon from all residues to humic")]
    double[] soilp_dlt_res_c_hum;

    [Output]
    [Units("kg/ha")]
    [Description("Carbon from all residues to biomass")]
    double[] soilp_dlt_res_c_biom;

    #endregion

    #region Amounts in various pools

    [Output]
    [Units("kg/ha")]
    [Description("FOM C")]
    double[] fom_c         // fresh organic C        
    {
        get
        {
            double[] _fom_c = new double[fom_c_pool1.Length];
            for (int layer = 0; layer < fom_c_pool1.Length; ++layer)
            {
                _fom_c[layer] = fom_c_pool1[layer] + fom_c_pool2[layer] + fom_c_pool3[layer];
            }
            return _fom_c;
        }
    }

    [Output]
    [Units("kg/ha")]
    [Description("FOM C in pool 1")]
    double[] fom_c_pool1;

    [Output]
    [Units("kg/ha")]
    [Description("FOM C in pool 2")]
    double[] fom_c_pool2;

    [Output]
    [Units("kg/ha")]
    [Description("FOM C in pool 3")]
    double[] fom_c_pool3;

    [Output]
    [Units("kg/ha")]
    [Description("Humic C")]
    double[] hum_c;         // Humic C

    [Output]
    [Units("kg/ha")]
    [Description("Biomass C")]
    double[] biom_c;        // biomass carbon 

    [Output]
    [Units("kg/ha")]
    [Description("Total carbon")]
    double[] carbon_tot    // total carbon in soil
    {
        get
        {
            int numLayers = dlayer.Length;
            double[] _carbon_tot = new double[numLayers];
            for (int layer = 0; layer < numLayers; layer++)
            {
                _carbon_tot[layer] += fom_c_pool1[layer] + fom_c_pool2[layer] + fom_c_pool3[layer] + hum_c[layer] + biom_c[layer];
            }
            return _carbon_tot;
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
            double deltaC = TotalC() - dailyInitialC; // Delta storage
            double losses = 0.0;
            for (int layer = 0; layer < dlayer.Length; layer++)
                losses += _dlt_fom_c_atm[0][layer] + _dlt_fom_c_atm[1][layer] + _dlt_fom_c_atm[2][layer] + dlt_hum_c_atm[layer] + dlt_biom_c_atm[layer] + SumDoubleArray(_dlt_res_c_atm[layer]);
            return -(losses + deltaC);
        }
    }

    #endregion

    #endregion

    #region Factors and other outputs

    [Output]
    [Units("kg/ha")]
    [Description("Variable needed by soilp in its calculations")]
    double[] soilp_dlt_org_p;

    [Output]
    [Description("Temperature factor for nitrification and mineralisation")]
    double[] tf
    {
        get
        {
            int nLayers = dlayer.Length;
            double[] result = new double[nLayers];
            int index = (!is_pond_active) ? 1 : 2;
            for (int layer = 0; layer < nLayers; layer++)
                result[layer] = (soiltype == "rothc") ? RothcTF(layer, index) : TF(layer, index);
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

    #region Mineral nitrogen

    // variation in ureappm as given by another component
    [Output]
    [Units("mg/kg")]
    double[] dlt_ureappm
    {
        set
        {
            for (int layer = 0; layer < value.Length; ++layer)
            {
                double oldValue = _urea[layer];
                _urea[layer] += MathUtility.Divide(value[layer], convFactor_kgha2ppm(layer), 0.0);
                if (_urea[layer] < urea_min[layer] - epsilon)
                {
                    Console.WriteLine(" urea(" + layer.ToString() + ") is being changed from " + oldValue.ToString() + " to " + _urea[layer].ToString() +
                        " kg/ha. This is less than lower limit of " + urea_min[layer].ToString() + ", value will be adjusted to minimum value.");
                    _urea[layer] = urea_min[layer];
                }
            }
        }
    }

    // variation in no3 as given by another component
    [Output]
    [Units("kgN/ha")]
    double[] dlt_urea
    {
        set
        {
            for (int layer = 0; layer < value.Length; ++layer)
            {
                _urea[layer] += value[layer];
                if (_urea[layer] < urea_min[layer] - epsilon)
                {
                    Console.WriteLine("Attempt to change urea(" + layer.ToString() + ") to a value below lower limit, value will be set to " + urea_min[layer].ToString() + " kgN/ha");
                    _urea[layer] = urea_min[layer];
                }
            }
        }
    }

    // variation in nh4ppm as given by another component
    [Output]
    [Units("mg/kg")]
    double[] dlt_nh4ppm
    {
        set
        {
            for (int layer = 0; layer < value.Length; ++layer)
            {
                _nh4[layer] += MathUtility.Divide(value[layer], convFactor_kgha2ppm(layer), 0.0);
                if (_nh4[layer] < nh4_min[layer] - epsilon)
                {
                    Console.WriteLine("Attempt to change nh4(" + layer.ToString() + ") to a value below lower limit, value will be set to " + nh4_min[layer].ToString() + " kgN/ha");
                    _nh4[layer] = nh4_min[layer];
                }
            }
        }
    }

    // variation in nh4 as given by another component
    [Output]
    [Units("kgN/ha")]
    double[] dlt_nh4
    {
        set
        {
            for (int layer = 0; layer < value.Length; ++layer)
            {
                _nh4[layer] += value[layer];
                if (_nh4[layer] < nh4_min[layer] - epsilon)
                {
                    Console.WriteLine("Attempt to change nh4(" + layer.ToString() + ") to a value below lower limit, value will be set to " + nh4_min[layer].ToString() + " kgN/ha");
                    _nh4[layer] = nh4_min[layer];
                }
            }
        }
    }

    // variation in no3ppm as given by another component
    [Output]
    [Units("mg/kg")]
    double[] dlt_no3ppm
    {
        set
        {
            for (int layer = 0; layer < value.Length; ++layer)
            {
                _no3[layer] += MathUtility.Divide(value[layer], convFactor_kgha2ppm(layer), 0.0);
                if (_no3[layer] < no3_min[layer] - epsilon)
                {
                    Console.WriteLine("Attempt to change no3(" + layer.ToString() + ") to a value below lower limit, value will be set to " + no3_min[layer].ToString() + " kgN/ha");
                    _no3[layer] = no3_min[layer];
                }
            }
        }
    }

    // variation in no3 as given by another component
    [Output]
    [Units("kgN/ha")]
    double[] dlt_no3
    {
        set
        {
            for (int layer = 0; layer < value.Length; ++layer)
            {
                _no3[layer] += value[layer];
                if (_no3[layer] < no3_min[layer] - epsilon)
                {
                    Console.WriteLine("Attempt to change no3(" + layer.ToString() + ") to a value below lower limit, value will be set to " + no3_min[layer].ToString() + " kgN/ha");
                    _no3[layer] = no3_min[layer];
                }
            }
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
            for (int layer = 0; layer < value.Length; ++layer)
                fom_n[layer] += value[layer];
        }
    }

    [Output]
    [Units("kg/ha")]
    double[] dlt_org_c_pool1
    {
        set
        {
            for (int layer = 0; layer < value.Length; ++layer)
                fom_c_pool1[layer] += value[layer];
        }
    }

    [Output]
    [Units("kg/ha")]
    double[] dlt_org_c_pool2
    {
        set
        {
            for (int layer = 0; layer < value.Length; ++layer)
                fom_c_pool2[layer] += value[layer];
        }
    }

    [Output]
    [Units("kg/ha")]
    double[] dlt_org_c_pool3
    {
        set
        {
            for (int layer = 0; layer < value.Length; ++layer)
                fom_c_pool3[layer] += value[layer];
        }
    }

    #endregion

    [Output]
    string n_reduction
    { set { p_n_reduction = value.StartsWith("on"); } }

    #endregion

    #region Useful constants

    // weight fraction of C in carbohydrates
    private const float c_in_fom = 0.4F;

    // An "epsilon" value for single-precision floating point
    // We use this since other components are likely to use single-precision math
    private double epsilon = Math.Pow(2, -24);

    #endregion

    #region  Various internal variables

    private double oldN;
    private double oldC;
    private double dailyInitialC;
    private double dailyInitialN;
    private bool use_external_st = false;           // marker for whether external soil temperature is supplied
    private bool use_external_tav = false;          //
    private bool use_external_amp = false;          //
    private bool use_external_ph = false;           // marker for whether external ph is supplied, otherwise default is used
    private bool p_n_reduction = false;
    private bool useOrganicSolutes = false;         // marker for whether organic solute are to be simulated (always false as it is not implemented)
    private int fom_type;
    private double[] inert_c;                       // humic C that is not subject to mineralization (kg/ha)
    private double[] nh4_yesterday;                 // yesterday's ammonium nitrogen(kg/ha)
    private double[] no3_yesterday;                 // yesterday's nitrate nitrogen (kg/ha)
    private bool initDone = false;                  // marker for whether initialisation has been finished
    private bool inReset = false;                   // marker for whether a reset is going on
    private int num_residues = 0;                   // number of residues decomposing
    private string[] residue_name;                  // name of residues decomposing
    private string[] residue_type;                  // type of decomposing residue
    private double[] pot_c_decomp;                  // Potential residue C decomposition (kg/ha)
    private double[] pot_n_decomp;                  // Potential residue N decomposition (kg/ha)
    private double[] pot_p_decomp;                  // Potential residue P decomposition (kg/ha)
    private double[][] dlt_res_c_decomp;            // residue C decomposition (kg/ha)
    private double[][] dlt_res_n_decomp;            // residue N decomposition (kg/ha)

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

        // perform initial calculations and setup
        InitCalc();

        // initialise soil temperature
        //SoilTemp();
        if (use_external_st)
            st = ave_soil_temp;
        else
        {
            simpleST = new simpleSoilTemp(latitude, tav, amp, maxt, mint);
            st = simpleST.SoilTemp(today, maxt, mint, radn, salb, dlayer, bd, ll15_dep, sw_dep);
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
            simpleST = new simpleSoilTemp(latitude, tav, amp, maxt, mint);
            st = simpleST.SoilTemp(today, maxt, mint, radn, salb, dlayer, bd, ll15_dep, sw_dep);
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
            st = simpleST.SoilTemp(today, maxt, mint, radn, salb, dlayer, bd, ll15_dep, sw_dep);

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

    [EventHandler(EventName = "IncorpFOM")]
    public void OnIncorpFOM(FOMLayerType IncorpFOM)
    {
        //    We partition the C and N into fractions in each layer.
        //    We will do this by assuming that the CN ratios
        //    of all fractions are equal

        bool nSpecified = false;
        for (int i = 0; i < IncorpFOM.Layer.Length; i++)
        {
            // If the caller specified CNR values then use them to calculate N from Amount.
            if (IncorpFOM.Layer[i].CNR > 0.0)
                IncorpFOM.Layer[i].FOM.N = (IncorpFOM.Layer[i].FOM.amount * c_in_fom) /
                                           IncorpFOM.Layer[i].CNR;
            // Was any N specified?
            nSpecified |= IncorpFOM.Layer[i].FOM.N != 0.0;
        }

        if (nSpecified)
        {
            fom_type = 0; // use as default if fom type not found
            for (int i = 0; i < fom_types.Length; i++)
            {
                if (fom_types[i] == IncorpFOM.Type)
                {
                    fom_type = i;
                    break;
                }
            }
            // Now convert the IncorpFOM.DeltaWt and IncorpFOM.DeltaN arrays to
            // include fraction information and add to pools.
            int nLayers = IncorpFOM.Layer.Length;
            if (nLayers > dlayer.Length)
            {
                Array.Resize(ref dlayer, nLayers);
                ResizeLayerArrays(nLayers);
            }
            for (int i = 0; i < nLayers; i++)
            {
                fom_c_pool1[i] += IncorpFOM.Layer[i].FOM.amount * fract_carb[fom_type] * c_in_fom;
                fom_c_pool2[i] += IncorpFOM.Layer[i].FOM.amount * fract_cell[fom_type] * c_in_fom;
                fom_c_pool3[i] += IncorpFOM.Layer[i].FOM.amount * fract_lign[fom_type] * c_in_fom;

                fom_n_pool1[i] += IncorpFOM.Layer[i].FOM.N * fract_carb[fom_type];
                fom_n_pool2[i] += IncorpFOM.Layer[i].FOM.N * fract_cell[fom_type];
                fom_n_pool3[i] += IncorpFOM.Layer[i].FOM.N * fract_lign[fom_type];

                // add up fom_n in each layer by adding up each of the pools
                fom_n[i] = fom_n_pool1[i] + fom_n_pool2[i] + fom_n_pool3[i];
            }
        }
    }

    [EventHandler(EventName = "tick")]
    public void OnTick(TimeType time)
    {
        //DateUtility.JulianDayNumberToDayOfYear(time.startday, out day_of_year, out year);

        // Reset Potential Decomposition Register
        num_residues = 0;
        Array.Resize(ref pot_c_decomp, 0);
        Array.Resize(ref pot_n_decomp, 0);
        Array.Resize(ref pot_p_decomp, 0);

        // Calculations for NEW sysbal component
        dailyInitialC = TotalC();
        dailyInitialN = TotalN();

    }

    [EventHandler(EventName = "NewMet")]
    public void OnNewmet(NewMetType NewMet)
    {
        //radn = NewMet.radn;
        //maxt = NewMet.maxt;
        //mint = NewMet.mint;
    }

    [EventHandler(EventName = "PotentialResidueDecompositionCalculated")]
    public void OnPotentialResidueDecompositionCalculated(SurfaceOrganicMatterDecompType SurfaceOrganicMatterDecomp)
    {
        //+  Purpose
        //     Get information of potential residue decomposition

        num_residues = SurfaceOrganicMatterDecomp.Pool.Length;

        Array.Resize(ref residue_name, num_residues);
        Array.Resize(ref residue_type, num_residues);
        Array.Resize(ref pot_c_decomp, num_residues);
        Array.Resize(ref pot_n_decomp, num_residues);
        Array.Resize(ref pot_p_decomp, num_residues);

        for (int layer = 0; layer < _dlt_res_c_biom.Length; layer++)
        {
            Array.Resize(ref _dlt_res_c_biom[layer], num_residues);
            Array.Resize(ref _dlt_res_c_hum[layer], num_residues);
            Array.Resize(ref _dlt_res_c_atm[layer], num_residues);
            Array.Resize(ref dlt_res_c_decomp[layer], num_residues);
            Array.Resize(ref dlt_res_n_decomp[layer], num_residues);
        }

        for (int residue = 0; residue < num_residues; residue++)
        {
            residue_name[residue] = SurfaceOrganicMatterDecomp.Pool[residue].Name;
            residue_type[residue] = SurfaceOrganicMatterDecomp.Pool[residue].OrganicMatterType;
            pot_c_decomp[residue] = SurfaceOrganicMatterDecomp.Pool[residue].FOM.C;
            pot_n_decomp[residue] = SurfaceOrganicMatterDecomp.Pool[residue].FOM.N;
            // this P decomposition is needed to formulate data required by SOILP - struth, this is very ugly
            pot_p_decomp[residue] = SurfaceOrganicMatterDecomp.Pool[residue].FOM.P;
        }
    }

    [EventHandler(EventName = "IncorpFOMPool")]
    public void OnIncorpFOMPool(FOMPoolType IncorpFOMPool)
    {
        // INCREMENT THE POOLS wtih the unpacked deltas
        for (int i = 0; i < IncorpFOMPool.Layer.Length; i++)
        {
            fom_c_pool1[i] += IncorpFOMPool.Layer[i].Pool[0].C;
            fom_c_pool2[i] += IncorpFOMPool.Layer[i].Pool[1].C;
            fom_c_pool3[i] += IncorpFOMPool.Layer[i].Pool[2].C;

            fom_n_pool1[i] += IncorpFOMPool.Layer[i].Pool[0].N;
            fom_n_pool2[i] += IncorpFOMPool.Layer[i].Pool[1].N;
            fom_n_pool3[i] += IncorpFOMPool.Layer[i].Pool[2].N;

            // add up fom_n in each layer by adding up each of the pools
            fom_n[i] = fom_n_pool1[i] + fom_n_pool2[i] + fom_n_pool3[i];

            _no3[i] += IncorpFOMPool.Layer[i].no3;
            _nh4[i] += IncorpFOMPool.Layer[i].nh4;
        }
    }

    [EventHandler(EventName = "new_profile")]
    public void OnNew_profile(NewProfileType NewProfile)
    {
        // not sure this is needed
        bd = NewProfile.bd;
        ll15_dep = NewProfile.ll15_dep;
        dul_dep = NewProfile.dul_dep;
        sat_dep = NewProfile.sat_dep;
        sw_dep = NewProfile.sw_dep;

        CheckProfile(NewProfile.dlayer);
    }

    [EventHandler(EventName = "NitrogenChanged")]
    public void OnNitrogenChanged(NitrogenChangedType NitrogenChanged)
    {
        for (int layer = 0; layer < NitrogenChanged.DeltaNO3.Length; ++layer)
        {
            _no3[layer] += NitrogenChanged.DeltaNO3[layer];
            if (Math.Abs(_no3[layer]) < epsilon)
                _no3[layer] = 0.0;
            if (_no3[layer] < no3_min[layer])
            {
                Console.WriteLine("Attempt to change no3(" + layer.ToString() + ") to a value below lower limit, value will be set to " + no3_min[layer].ToString() + " kgN/ha");
                _no3[layer] = no3_min[layer];
            }
        }
        for (int layer = 0; layer < NitrogenChanged.DeltaNH4.Length; ++layer)
        {
            _nh4[layer] += NitrogenChanged.DeltaNH4[layer];
            if (Math.Abs(_nh4[layer]) < epsilon)
                _nh4[layer] = 0.0;
            if (_nh4[layer] < nh4_min[layer])
            {
                Console.WriteLine("Attempt to change nh4(" + layer.ToString() + ") to a value below lower limit, value will be set to " + nh4_min[layer].ToString() + " kgN/ha");
                _nh4[layer] = nh4_min[layer];
            }
        }
    }

    [EventHandler(EventName = "AddUrine")]
    public void OnAddUrine(AddUrineType UrineAdded)
    {
        // Starting with the minimalist version. To be updated by Val's group to
        // include a urine patch algorithm
        _urea[0] += UrineAdded.Urea;
    }

    #endregion

    #region Setup calculations

    private void CheckParams()
    {
        // +  Purpose:
        //      Check initialisation parameters and let user know

        Console.WriteLine();
        Console.WriteLine("        - Reading/checking parameters");

        Console.WriteLine("           - Using " + soiltype + " soil mineralisation specification");

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

        // Check if all fom values are supplied
        if (num_fom_types != fract_carb.Length)
            throw new Exception("Number of \"fract_carb\" different to \"fom_type\"");
        if (num_fom_types != fract_cell.Length)
            throw new Exception("Number of \"fract_cell\" different to \"fom_type\"");
        if (num_fom_types != fract_lign.Length)
            throw new Exception("Number of \"fract_lign\" different to \"fom_type\"");

        // Check if all C:N values are supplied. If not use average C:N ratio in all pools
        if (fomPools_cn == null || fomPools_cn.Length < 3)
        {
            fomPools_cn = new double[3];
            for (int i = 0; i < 3; i++)
                fomPools_cn[i] = fom_ini_cn;
        }

        // if 'fom_ini_depth' not provided, assume that 'fom_ini_wt' is distributed over whole profile
        if (fom_ini_depth == 0.0)
        {
            for (int i = 0; i < dlayer.Length; ++i)
                fom_ini_depth += dlayer[i];
        }

        // Do we need these? - are these still being used?
        p_n_reduction = profile_reduction == "on";
        useOrganicSolutes = use_organic_solutes == "on";
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

        // Distribute an convert C an N values over the profile
        for (int layer = 0; layer < nLayers; layer++)
        {
            double convFact = convFactor_kgha2ppm(layer);
            _no3[layer] = MathUtility.Divide(_no3ppm[layer], convFact, 0.0);
            if (_no3[layer] < no3_min[layer] - epsilon)
            {
                Console.WriteLine(" Attempt to initialise NO3 below lower limit, value will be adjusted to minimum");
                _no3[layer] = no3_min[layer];
            }
            _nh4[layer] = MathUtility.Divide(_nh4ppm[layer], convFact, 0.0);
            if (_nh4[layer] < nh4_min[layer] - epsilon)
            {
                Console.WriteLine("Attempt to initialise NH4 below lower limit, value will be adjusted to minimum");
                _nh4[layer] = nh4_min[layer];
            }
            if (_ureappm != null)
                _urea[layer] = MathUtility.Divide(_ureappm[layer], convFact, 0.0);

            // calculate total soil C
            double oc_ppm = _oc[layer] * 10000;     // = (oc/100)*1000000 - convert from % to ppm
            double carbon_tot = MathUtility.Divide(oc_ppm, convFact, 0.0);

            // calculate inert soil C
            inert_c[layer] = finert[layer] * carbon_tot;

            // calculate microbial biomass C and N
            biom_c[layer] = MathUtility.Divide((carbon_tot - inert_c[layer]) * fbiom[layer], 1.0 + fbiom[layer], 0.0);
            biom_n[layer] = MathUtility.Divide(biom_c[layer], biom_cn, 0.0);

            // calculate C and N values for active humus
            hum_c[layer] = carbon_tot - biom_c[layer];
            hum_n[layer] = MathUtility.Divide(hum_c[layer], hum_cn, 0.0);

            // distribute and calculate the fom N and C
            double fom = MathUtility.Divide(fom_ini_wt * fom_FracLayer[layer], fom_FracLayer_tot, 0.0);

            // C amount for each pool
            fom_c_pool1[layer] = fom * fract_carb[0] * c_in_fom;
            fom_c_pool2[layer] = fom * fract_cell[0] * c_in_fom;
            fom_c_pool3[layer] = fom * fract_lign[0] * c_in_fom;

            // N amount for each pool
            fom_n_pool1[layer] = MathUtility.Divide(fom_c_pool1[layer], root_cn_pool[0], 0.0);
            fom_n_pool2[layer] = MathUtility.Divide(fom_c_pool2[layer], root_cn_pool[1], 0.0);
            fom_n_pool3[layer] = MathUtility.Divide(fom_c_pool3[layer], root_cn_pool[2], 0.0);

            // total fom N in each layer
            fom_n[layer] = fom_n_pool1[layer] + fom_n_pool2[layer] + fom_n_pool3[layer];

            // store today's values
            no3_yesterday[layer] = _no3[layer];
            nh4_yesterday[layer] = _nh4[layer];
        }

        // Calculations for NEW sysbal component
        dailyInitialC = TotalC();
        dailyInitialN = TotalN();

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
        //      Initialise all public arrays with nLayers , might include pools in the future

        Array.Resize(ref st, nLayers);
        Array.Resize(ref _nh4, nLayers);
        Array.Resize(ref _no3, nLayers);
        Array.Resize(ref _urea, nLayers);
        Array.Resize(ref no3_yesterday, nLayers);
        Array.Resize(ref nh4_yesterday, nLayers);
        Array.Resize(ref urea_min, nLayers);
        Array.Resize(ref nh4_min, nLayers);
        Array.Resize(ref no3_min, nLayers);
        Array.Resize(ref inert_c, nLayers);
        Array.Resize(ref biom_c, nLayers);
        Array.Resize(ref biom_n, nLayers);
        Array.Resize(ref hum_c, nLayers);
        Array.Resize(ref hum_n, nLayers);
        Array.Resize(ref fom_c_pool1, nLayers);
        Array.Resize(ref fom_c_pool2, nLayers);
        Array.Resize(ref fom_c_pool3, nLayers);
        Array.Resize(ref fom_n_pool1, nLayers);
        Array.Resize(ref fom_n_pool2, nLayers);
        Array.Resize(ref fom_n_pool3, nLayers);
        Array.Resize(ref fom_n, nLayers);
        Array.Resize(ref _nitrification_inhibition, nLayers);
        Array.Resize(ref nh4_transform_net, nLayers);
        Array.Resize(ref no3_transform_net, nLayers);
        Array.Resize(ref dlt_nh4_net, nLayers);
        Array.Resize(ref dlt_no3_net, nLayers);
        Array.Resize(ref dlt_hum_c_atm, nLayers);
        Array.Resize(ref dlt_biom_c_atm, nLayers);
        for (int i = 0; i < 3; i++)
        {
            Array.Resize(ref _dlt_fom_c_biom[i], nLayers);
            Array.Resize(ref _dlt_fom_c_hum[i], nLayers);
            Array.Resize(ref _dlt_fom_c_atm[i], nLayers);
        }
        Array.Resize(ref _dlt_res_c_biom, nLayers);
        Array.Resize(ref _dlt_res_c_hum, nLayers);
        Array.Resize(ref _dlt_res_c_atm, nLayers);
        Array.Resize(ref dlt_res_c_decomp, nLayers);
        Array.Resize(ref dlt_res_n_decomp, nLayers);
        Array.Resize(ref dlt_nitrification, nLayers);
        Array.Resize(ref effective_nitrification, nLayers);
        Array.Resize(ref dlt_urea_hydrol, nLayers);
        Array.Resize(ref excess_nh4, nLayers);
        Array.Resize(ref dlt_fom_n_min, nLayers);
        Array.Resize(ref dlt_biom_n_min, nLayers);
        Array.Resize(ref dlt_hum_n_min, nLayers);
        Array.Resize(ref dlt_fom_c_pool1, nLayers);
        Array.Resize(ref dlt_fom_c_pool2, nLayers);
        Array.Resize(ref dlt_fom_c_pool3, nLayers);
        Array.Resize(ref dlt_res_no3_min, nLayers);
        Array.Resize(ref dlt_res_nh4_min, nLayers);
        Array.Resize(ref dlt_no3_dnit, nLayers);
        Array.Resize(ref dlt_nh4_dnit, nLayers);
        Array.Resize(ref n2o_atm, nLayers);
        Array.Resize(ref dlt_hum_c_biom, nLayers);
        Array.Resize(ref dlt_biom_c_hum, nLayers);
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

        dailyInitialN = oldN = TotalN();
        dailyInitialC = oldC = TotalC();
    }

    private void DeltaState()
    {
        // +  Purpose:
        //      Calculates variations in C an N, and publishes Mass flows to apsim

        double dltN = TotalN() - oldN;
        double dltC = TotalC() - oldC;

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
            layer + 1, ph[layer], _oc[layer], _no3[layer], _nh4[layer], _urea[layer]);
        }
        Console.WriteLine("          ------------------------------------------------");
        Console.WriteLine("           Totals              {0,6:F2}  {1,6:F2}  {2,6:F2}",
                  SumDoubleArray(_no3), SumDoubleArray(_nh4), SumDoubleArray(_urea));
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
            double FomC = fom_c_pool1[layer] + fom_c_pool2[layer] + fom_c_pool3[layer];
            TotalFomC += FomC;
            Console.WriteLine("          {0,4:d1}   {1,10:F1}{2,8:F1}{3,8:F1}{4,8:F1}{5,8:F1}{6,8:F1}",
            layer + 1, hum_c[layer], hum_n[layer], biom_c[layer], biom_n[layer], FomC, fom_n[layer]);
        }
        Console.WriteLine("          ---------------------------------------------------------");
        Console.WriteLine("           Totals{0,10:F1}{1,8:F1}{2,8:F1}{3,8:F1}{4,8:F1}{5,8:F1}",
            SumDoubleArray(hum_c), SumDoubleArray(hum_n), SumDoubleArray(biom_c),
            SumDoubleArray(biom_n), TotalFomC, SumDoubleArray(fom_n));
        Console.WriteLine("          ---------------------------------------------------------");
        Console.WriteLine();
    }

    #endregion

    #region process calculations

    #region Main processes

    private void Process()
    {
        // + Purpose
        //     This routine performs the soil C and N balance, daily.
        //      - Assesses potential decomposition of surface residues (adjust decompostion if needed, accounts for mineralisation/immobilisation of N)
        //      - Calculates hydrolysis of urea, denitrification, transformations on soil organic matter (including N mineralisation/immobilition) and nitrification.

        int nLayers = dlayer.Length;                    // number of layers in the soil
        double[,] dlt_fom_n = new double[3, nLayers];   // fom N mineralised in each fraction (kg/ha)

        if (is_pond_active)
        {
            // dsg 190508,  If there is a pond, the POND module will decompose residues - not SoilNitrogen
            // dsg 110708   Get the biom & hum C decomposed in the pond and add to soil - on advice of MEP

            // increment the hum and biom C pools in top soil layer
            hum_c[0] += pond_hum_C;         // humic material from breakdown of residues in pond
            biom_c[0] += pond_biom_C;       // biom material from breakdown of residues in pond

            // reset the N amounts of N in hum and biom pools
            hum_n[0] = MathUtility.Divide(hum_c[0], hum_cn, 0.0);
            biom_n[0] = MathUtility.Divide(biom_c[0], biom_cn, 0.0);
        }
        else
        {   // Decompose residues

            // assess the potential decomposition of surface residues
            MinResidues(dlt_res_c_decomp, dlt_res_n_decomp,
                  ref _dlt_res_c_biom, ref _dlt_res_c_hum, ref _dlt_res_c_atm, ref dlt_res_nh4_min, ref dlt_res_no3_min);

            // update C content in hum and biom pools
            for (int layer = 0; layer < nLayers; layer++)
            {
                hum_c[layer] += SumDoubleArray(_dlt_res_c_hum[layer]);
                biom_c[layer] += SumDoubleArray(_dlt_res_c_biom[layer]);
            }

            // update N content in hum and biom pools as well as the mineral N
            for (int layer = 0; layer < nLayers; layer++)
            {
                hum_n[layer] = MathUtility.Divide(hum_c[layer], hum_cn, 0.0);
                biom_n[layer] = MathUtility.Divide(biom_c[layer], biom_cn, 0.0);

                // update soil mineral N

                _nh4[layer] += dlt_res_nh4_min[layer];
                _no3[layer] += dlt_res_no3_min[layer];
            }
        }

        // now take each layer in turn and compute N processes
        for (int layer = 0; layer < nLayers; layer++)
        {
            // urea hydrolysis
            dlt_urea_hydrol[layer] = UreaHydrolysis(layer);
            _nh4[layer] += dlt_urea_hydrol[layer];
            _urea[layer] -= dlt_urea_hydrol[layer];

            // nitrate-N denitrification
            dlt_no3_dnit[layer] = Denitrification(layer);
            _no3[layer] -= dlt_no3_dnit[layer];

            // N2O loss to atmosphere - due to denitrification
            n2o_atm[layer] = 0.0;
            double N2N2O = Denitrification_Nratio(layer);
            n2o_atm[layer] = dlt_no3_dnit[layer] / (N2N2O + 1.0);

            // Calculate transformations of soil organic matter (C and N)

            // humic pool mineralisation
            MinHumic(layer, ref dlt_hum_c_biom[layer], ref dlt_hum_c_atm[layer], ref dlt_hum_n_min[layer]);

            // microbial biomass pool mineralisation 
            MinBiomass(layer, ref dlt_biom_c_hum[layer], ref dlt_biom_c_atm[layer], ref dlt_biom_n_min[layer]);

            // mineralisation of fresh organic matter pools
            double[] dlt_f_n;
            double[] dlt_fc_biom;
            double[] dlt_fc_hum;
            double[] dlt_fc_atm;
            MinFom(layer, out dlt_fc_biom, out dlt_fc_hum, out dlt_fc_atm, out dlt_f_n, out dlt_fom_n_min[layer]);

            for (int fract = 0; fract < 3; fract++)
            {
                _dlt_fom_c_biom[fract][layer] = dlt_fc_biom[fract];
                _dlt_fom_c_hum[fract][layer] = dlt_fc_hum[fract];
                _dlt_fom_c_atm[fract][layer] = dlt_fc_atm[fract];
                dlt_fom_n[fract, layer] = dlt_f_n[fract];
            }

            // update pools C an N contents

            hum_c[layer] += dlt_biom_c_hum[layer] - dlt_hum_c_biom[layer] - dlt_hum_c_atm[layer] +
                           _dlt_fom_c_hum[0][layer] + _dlt_fom_c_hum[1][layer] + _dlt_fom_c_hum[2][layer];

            hum_n[layer] = MathUtility.Divide(hum_c[layer], hum_cn, 0.0);

            biom_c[layer] += dlt_hum_c_biom[layer] - dlt_biom_c_hum[layer] - dlt_biom_c_atm[layer] +
                           _dlt_fom_c_biom[0][layer] + _dlt_fom_c_biom[1][layer] + _dlt_fom_c_biom[2][layer];

            biom_n[layer] = MathUtility.Divide(biom_c[layer], biom_cn, 0.0);

            fom_c_pool1[layer] -= (_dlt_fom_c_hum[0][layer] + _dlt_fom_c_biom[0][layer] + _dlt_fom_c_atm[0][layer]);
            fom_c_pool2[layer] -= (_dlt_fom_c_hum[1][layer] + _dlt_fom_c_biom[1][layer] + _dlt_fom_c_atm[1][layer]);
            fom_c_pool3[layer] -= (_dlt_fom_c_hum[2][layer] + _dlt_fom_c_biom[2][layer] + _dlt_fom_c_atm[2][layer]);

            fom_n_pool1[layer] -= dlt_fom_n[0, layer];
            fom_n_pool2[layer] -= dlt_fom_n[1, layer];
            fom_n_pool3[layer] -= dlt_fom_n[2, layer];

            // dsg  these 3 dlts are calculated for the benefit of soilp which needs to 'get' them
            dlt_fom_c_pool1[layer] = _dlt_fom_c_hum[0][layer] + _dlt_fom_c_biom[0][layer] + _dlt_fom_c_atm[0][layer];
            dlt_fom_c_pool2[layer] = _dlt_fom_c_hum[1][layer] + _dlt_fom_c_biom[1][layer] + _dlt_fom_c_atm[1][layer];
            dlt_fom_c_pool3[layer] = _dlt_fom_c_hum[2][layer] + _dlt_fom_c_biom[2][layer] + _dlt_fom_c_atm[2][layer];

            // add up fom in each layer in each of the pools
            double fom_c = fom_c_pool1[layer] + fom_c_pool2[layer] + fom_c_pool3[layer];
            fom_n[layer] = fom_n_pool1[layer] + fom_n_pool2[layer] + fom_n_pool3[layer];

            // update soil mineral N after mineralisation/immobilisation

            // starts with nh4
            _nh4[layer] += dlt_hum_n_min[layer] + dlt_biom_n_min[layer] + dlt_fom_n_min[layer];

            // check whether there is enough NH4 to be immobilized
            double no3_needed4immob = 0.0;
            if (_nh4[layer] < nh4_min[layer])
            {
                no3_needed4immob = nh4_min[layer] - _nh4[layer];
                _nh4[layer] = nh4_min[layer];
            }

            // now change no3
            _no3[layer] -= no3_needed4immob;
            if (_no3[layer] < no3_min[layer] - epsilon)
            {
                // note: tests for adequate mineral N for immobilization have been made so this no3 should not go below no3_min
                throw new Exception("N immobilisation resulted in mineral N in layer(" + layer.ToString() + ") to go below minimum");
            }

            // nitrification of ammonium-N (total)
            dlt_nitrification[layer] = Nitrification(layer);

            // denitrification loss during nitrification
            dlt_nh4_dnit[layer] = DenitrificationInNitrification(layer);

            // effective or net nitrification
            effective_nitrification[layer] = dlt_nitrification[layer] - dlt_nh4_dnit[layer];

            // N2O loss to atmosphere from nitrification
            n2o_atm[layer] += dlt_nh4_dnit[layer];

            // update soil mineral N
            _no3[layer] += effective_nitrification[layer];
            _nh4[layer] -= dlt_nitrification[layer];

            // check some of the values
            if (Math.Abs(_urea[layer]) < epsilon)
                _urea[layer] = 0.0;
            if (Math.Abs(_nh4[layer]) < epsilon)
                _nh4[layer] = 0.0;
            if (Math.Abs(_no3[layer]) < epsilon)
                _no3[layer] = 0.0;
            if (_urea[layer] < urea_min[layer] || _urea[layer] > 9000.0)
                throw new Exception("Value for urea(layer) is out of range");
            if (_nh4[layer] < nh4_min[layer] || _nh4[layer] > 9000.0)
                throw new Exception("Value for NH4(layer) is out of range");
            if (_no3[layer] < no3_min[layer] || _no3[layer] > 9000.0)
                throw new Exception("Value for NO3(layer) is out of range");

            // net N tansformations
            nh4_transform_net[layer] = dlt_res_nh4_min[layer] + dlt_fom_n_min[layer] + dlt_biom_n_min[layer] + dlt_hum_n_min[layer] - dlt_nitrification[layer] + dlt_urea_hydrol[layer] + no3_needed4immob;
            no3_transform_net[layer] = dlt_res_no3_min[layer] - dlt_no3_dnit[layer] + effective_nitrification[layer] - no3_needed4immob;

            // net deltas
            dlt_nh4_net[layer] = _nh4[layer] - nh4_yesterday[layer];
            dlt_no3_net[layer] = _no3[layer] - no3_yesterday[layer];

            excess_nh4[layer] = no3_needed4immob; // is this used?

            // store these values so they may be used tomorrow
            nh4_yesterday[layer] = _nh4[layer];
            no3_yesterday[layer] = _no3[layer];
        }
    }

    private void MinResidues(double[][] dlt_c_decomp, double[][] dlt_n_decomp,
            ref double[][] dlt_c_biom, ref double[][] dlt_c_hum, ref double[][] dlt_c_atm,
            ref double[] dlt_nh4_min, ref double[] dlt_no3_min)
    {
        //+  Sub-Program Arguments
        // dlt_C_decomp(max_layer, max_residues)   // C decomposed for each residue (Kg/ha)
        // dlt_N_decomp(max_layer, max_residues)   // N decomposed for each residue (Kg/ha)
        // dlt_c_atm(max_layer, max_residues)      // (OUTPUT) carbon to atmosphere (kg/ha)
        // dlt_c_biom(max_layer, max_residues)     // (OUTPUT) carbon to biomass (kg/ha)
        // dlt_c_hum(max_layer, max_residues)      // (OUTPUT) carbon to humic (kg/ha)
        // dlt_nh4_min(max_layer)      // (OUTPUT) N to NH4 (kg/ha)
        // dlt_no3_min(max_layer)      // (OUTPUT) N to NO3 (kg/ha)

        //+  Purpose
        //       Test to see whether adequate mineral nitrogen is available
        //       to sustain potential rate of decomposition of surface residues
        //       and calculate net rate of nitrogen mineralization/immobilization

        //+  Mission Statement
        //     Calculate rate of nitrogen mineralization/immobilization

        int nLayers = dlayer.Length;

        double[] avail_no3 = new double[nLayers]; // no3 available for mineralisation
        double[] avail_nh4 = new double[nLayers]; // nh4 available for mineralisation
        // Initialise to zero by assigning new
        dlt_c_hum = new double[nLayers][];
        dlt_c_biom = new double[nLayers][];
        for (int layer = 0; layer < nLayers; layer++)
        {
            dlt_c_hum[layer] = new double[num_residues];
            dlt_c_biom[layer] = new double[num_residues];
        }
        dlt_nh4_min = new double[nLayers];
        dlt_no3_min = new double[nLayers];

        // get total available mineral N in surface soil
        int min_layer = getCumulativeIndex(min_depth, dlayer); // soil layer to which N is available for mineralisation.

        double min_layer_top = 0.0; // depth of the top of min_layer
        for (int layer = 0; layer < min_layer; layer++)
            min_layer_top += dlayer[layer];  // This needs testing...

        double nit_tot = 0.0; // total N avaliable for immobilization (kg/ha)

        double[] fraction = new double[min_layer + 1];

        for (int layer = 0; layer <= min_layer; layer++)
        {
            avail_no3[layer] = Math.Max(0.0, _no3[layer] - no3_min[layer]);
            avail_nh4[layer] = Math.Max(0.0, _nh4[layer] - nh4_min[layer]);

            fraction[layer] = 1.0;
            if (layer == min_layer)
            {
                fraction[layer] = Math.Max(0.0, Math.Min(1.0,
                    (dlayer[layer] == 0.0) ? 0.0 : (min_depth - min_layer_top) / dlayer[layer]));

            }

            avail_no3[layer] *= fraction[layer];
            avail_nh4[layer] *= fraction[layer];

            nit_tot += avail_no3[layer] + avail_nh4[layer];
        }

        // get potential decomposition rates of residue C and N
        //      from residue module

        // jpd
        // determine potential decomposition rates of residue C and N
        // in relation to Avail N, for residue and manure surface material
        // i.e. Do Scaling for avail N, combining residue&manure decomposition

        // calculate potential transfers to biom and humic pools

        double[] dlt_c_biom_tot = new double[num_residues]; // C mineralized converted to biomass
        double[] dlt_c_hum_tot = new double[num_residues];  // C mineralized converted to humic

        for (int residue = 0; residue < num_residues; residue++)
        {
            dlt_c_biom_tot[residue] = pot_c_decomp[residue] * ef_res * fr_res_biom;
            dlt_c_hum_tot[residue] = pot_c_decomp[residue] * ef_res * (1.0 - fr_res_biom);
        }

        // test whether adequate N available to meet immobilization demand

        // potential N immobilization
        double n_demand = MathUtility.Divide(SumDoubleArray(dlt_c_biom_tot), biom_cn, 0.0) +
                          MathUtility.Divide(SumDoubleArray(dlt_c_hum_tot), hum_cn, 0.0);
        double n_avail = nit_tot + SumDoubleArray(pot_n_decomp);

        double scale_of = 1.0; // factor to reduce mineralization rates if insufficient N available
        if (n_demand > n_avail)
        {
            scale_of = Math.Max(0.0, Math.Min(1.0,
                MathUtility.Divide(nit_tot, n_demand - SumDoubleArray(pot_n_decomp), 0.0)));
        }

        // Partition Additions of C and N to layers

        double dlt_n_decomp_tot = 0.0;

        for (int layer = 0; layer <= min_layer; layer++)
        {
            double part_fraction = MathUtility.Divide(dlayer[layer] * fraction[layer], min_depth, 0.0);
            for (int residue = 0; residue < num_residues; residue++)
            {
                // now adjust carbon transformations etc.
                dlt_c_decomp[layer][residue] = pot_c_decomp[residue] * scale_of * part_fraction;
                dlt_n_decomp[layer][residue] = pot_n_decomp[residue] * scale_of * part_fraction;
                dlt_n_decomp_tot += dlt_n_decomp[layer][residue];

                dlt_c_hum[layer][residue] = dlt_c_hum_tot[residue] * scale_of * part_fraction;
                dlt_c_biom[layer][residue] = dlt_c_biom_tot[residue] * scale_of * part_fraction;
                dlt_c_atm[layer][residue] = dlt_c_decomp[layer][residue] - dlt_c_hum[layer][residue] - dlt_c_biom[layer][residue];
            }
        }

        // net N mineralized (hg/ha)
        double dlt_n_min = dlt_n_decomp_tot - n_demand * scale_of;

        if (dlt_n_min > 0.0)
        {
            // we have mineralisation into NH4
            // distribute it over the layers
            for (int layer = 0; layer <= min_layer; layer++)
            {
                double part_fraction = MathUtility.Divide(dlayer[layer] * fraction[layer], min_depth, 0.0);
                dlt_nh4_min[layer] = dlt_n_min * part_fraction;
            }
        }
        else if (dlt_n_min < 0.0)
        {
            // Now soak up any N required for immobilisation from NH4 then NO3
            for (int layer = 0; layer <= min_layer; layer++)
            {
                dlt_nh4_min[layer] = -Math.Min(avail_nh4[layer], Math.Abs(dlt_n_min));
                dlt_n_min -= dlt_nh4_min[layer];
            }

            for (int layer = 0; layer <= min_layer; layer++)
            {
                dlt_no3_min[layer] = -Math.Min(avail_no3[layer], Math.Abs(dlt_n_min));
                dlt_n_min -= dlt_no3_min[layer];
            }

            // There should now be no remaining immobilization demand
            if (dlt_n_min < -0.001 || dlt_n_min > 0.001)
                throw new Exception("Value for remaining immobilization is out of range");
        }
        else // no N transformation
        {
        }
    }

    private void MinHumic(int layer, ref double dlt_c_biom, ref double dlt_c_atm, ref double dlt_n_min)
    {
        // + Purpose
        //     Calculate the daily transformation of the the soil humic pool, mineralisation (+ve) or immobilisation (-ve)

        // + Assumptions
        //     There is an inert_C component of the humic pool that is not subject to mineralization

        // dsg 200508  use different values for some constants when there's a pond and anaerobic conditions dominate
        int index = (!is_pond_active) ? 1 : 2;

        // get the soil temperature factor
        double tf = (soiltype == "rothc") ? RothcTF(layer, index) : TF(layer, index);

        // get the soil water factor
        double wf = WF(layer, index);

        // get the rate of mineralization of N from the humic pool
        double dlt_c_min_tot = (hum_c[layer] - inert_c[layer]) * rd_hum[index - 1] * tf * wf;
        double dlt_n_min_tot = MathUtility.Divide(dlt_c_min_tot, hum_cn, 0.0);

        // distribute the mineralised N and C
        dlt_c_biom = dlt_c_min_tot * ef_hum;
        dlt_c_atm = dlt_c_min_tot * (1.0 - ef_hum);
        dlt_n_min = dlt_n_min_tot - MathUtility.Divide(dlt_c_biom, biom_cn, 0.0);
    }

    private void MinBiomass(int layer, ref double dlt_c_hum, ref double dlt_c_atm, ref double dlt_n_min)
    {
        // + Purpose
        //     Calculate the daily transformation of the soil biomass pool, mineralisation (+ve) or immobilisation (-ve)

        // dsg 200508  use different values for some constants when anaerobic conditions dominate
        int index = (!is_pond_active) ? 1 : 2;

        // get the soil temperature factor
        double tf = (soiltype == "rothc") ? RothcTF(layer, index) : TF(layer, index);

        // get the soil water factor
        double wf = WF(layer, index);

        // get the rate of mineralization of C & N from the biomass pool
        double dlt_n_min_tot = biom_n[layer] * rd_biom[index - 1] * tf * wf;
        double dlt_c_min_tot = dlt_n_min_tot * biom_cn;

        // distribute the carbon
        dlt_c_hum = dlt_c_min_tot * ef_biom * (1.0 - fr_biom_biom);
        dlt_c_atm = dlt_c_min_tot * (1.0 - ef_biom);

        // calculate net N mineralization
        dlt_n_min = dlt_n_min_tot - MathUtility.Divide(dlt_c_hum, hum_cn, 0.0) - MathUtility.Divide((dlt_c_min_tot - dlt_c_atm - dlt_c_hum), biom_cn, 0.0);
    }

    private void MinFom(int layer, out double[] dlt_c_biom, out double[] dlt_c_hum,
                        out double[] dlt_c_atm, out double[] dlt_fom_n, out double dlt_n_min)
    {
        // + Purpose
        //     Calculate the daily transformation of the soil fresh organic matter pools, mineralisation (+ve) or immobilisation (-ve)

        dlt_c_hum = new double[3];
        dlt_c_biom = new double[3];
        dlt_c_atm = new double[3];
        dlt_fom_n = new double[3];
        dlt_n_min = 0.0;

        // dsg 200508  use different values for some constants when anaerobic conditions dominate
        // index = 1 for aerobic conditions, 2 for anaerobic conditions
        int index = (!is_pond_active) ? 1 : 2;

        // get total available mineral N (kg/ha)
        double nitTot = Math.Max(0.0, (_no3[layer] - no3_min[layer]) + (_nh4[layer] - nh4_min[layer]));

        // fresh organic carbon (kg/ha)
        double fomC = fom_c_pool1[layer] + fom_c_pool2[layer] + fom_c_pool3[layer];

        // fresh organic nitrogen (kg/ha)
        double fomN = fom_n_pool1[layer] + fom_n_pool2[layer] + fom_n_pool3[layer];

        // ratio of C in fresh OM to N available for decay
        double cnr = MathUtility.Divide(fomC, fomN + nitTot, 0.0);

        // calculate the C:N ratio factor - Bound to [0, 1]
        double cnrf = Math.Max(0.0, Math.Min(1.0, Math.Exp(-cnrf_coeff * (cnr - cnrf_optcn) / cnrf_optcn)));

        // get the soil temperature factor
        double tf = (soiltype == "rothc") ? RothcTF(layer, index) : TF(layer, index);

        // get the soil water factor
        double wf = WF(layer, index);

        // calculate gross amount of C & N released due to mineralization of the fresh organic matter.
        if (fomC >= fom_min)
        {
            double dlt_fom_n_min_tot = 0.0; // amount of fresh organic N mineralized across fpools (kg/ha)
            double dlt_fom_c_min_tot = 0.0; // total C mineralized (kg/ha) summed across fpools
            double[] dlt_n_min_tot = new double[3]; // amount of fresh organic N mineralized in each pool (kg/ha)
            double[] dlt_c_min_tot = new double[3]; // amount of C mineralized (kg/ha) from each pool

            // C:N ratio of fom
            double fom_cn = MathUtility.Divide(fomC, fomN, 0.0);

            // get the decomposition of carbohydrate-like, cellulose-like and lignin-like fractions (fpools) in turn.
            for (int fractn = 0; fractn < 3; fractn++)
            {
                // get the max decomposition rate for each fpool
                double decomp_rate = FractRDFom(fractn)[index - 1] * cnrf * tf * wf;

                // calculate the gross amount of fresh organic carbon mineralized (kg/ha)
                double gross_c_decomp = decomp_rate * FractFomC(fractn)[layer];

                // calculate the gross amount of N released from fresh organic matter (kg/ha)
                double gross_n_decomp = decomp_rate * FractFomN(fractn)[layer];

                dlt_fom_n_min_tot += gross_n_decomp;
                dlt_c_min_tot[fractn] = gross_c_decomp;
                dlt_n_min_tot[fractn] = gross_n_decomp;
                dlt_fom_c_min_tot += gross_c_decomp;
            }

            // calculate potential transfers of C mineralized to biomass
            double dlt_c_biom_tot = dlt_fom_c_min_tot * ef_fom * fr_fom_biom;

            // calculate potential transfers of C mineralized to humus
            double dlt_c_hum_tot = dlt_fom_c_min_tot * ef_fom * (1.0 - fr_fom_biom);

            // test whether there is adequate N available to meet immobilization demand
            double n_demand = MathUtility.Divide(dlt_c_biom_tot, biom_cn, 0.0) + MathUtility.Divide(dlt_c_hum_tot, hum_cn, 0.0);
            double n_avail = nitTot + dlt_fom_n_min_tot;

            // factor to reduce mineralization rates if insufficient N to meet immobilization demand
            double Navail_factor = 1.0;
            if (n_demand > n_avail)
                Navail_factor = Math.Max(0.0, Math.Min(1.0, MathUtility.Divide(nitTot, n_demand - dlt_fom_n_min_tot, 0.0)));

            // now adjust carbon transformations etc. and similarly for npools
            for (int fractn = 0; fractn < 3; fractn++)
            {
                dlt_c_hum[fractn] = dlt_c_min_tot[fractn] * ef_fom * (1.0 - fr_fom_biom) * Navail_factor;
                dlt_c_biom[fractn] = dlt_c_min_tot[fractn] * ef_fom * fr_fom_biom * Navail_factor;
                dlt_c_atm[fractn] = dlt_c_min_tot[fractn] * (1.0 - ef_fom) * Navail_factor;
                dlt_fom_n[fractn] = dlt_n_min_tot[fractn] * Navail_factor;

                dlt_c_hum[fractn] = MathUtility.RoundToZero(dlt_c_hum[fractn]);
                dlt_c_biom[fractn] = MathUtility.RoundToZero(dlt_c_biom[fractn]);
                dlt_c_atm[fractn] = MathUtility.RoundToZero(dlt_c_atm[fractn]);
                dlt_fom_n[fractn] = MathUtility.RoundToZero(dlt_fom_n[fractn]);
            }

            dlt_n_min = (dlt_fom_n_min_tot - n_demand) * Navail_factor;
        }
    }

    private double UreaHydrolysis(int layer)
    {
        // + Purpose
        //     Calculate the amount of urea converted to NH4 via hydrolysis

        // dsg 200508  use different values for some constants when anaerobic conditions dominate
        double result;
        int index = (!is_pond_active) ? 1 : 2;

        if (_urea[layer] > 0.0)
        {
            // we have urea, so can do some hydrolysis
            if (_urea[layer] < 0.1)
                // urea amount is too small, all is hydrolised
                result = _urea[layer];
            else
            {
                // get the soil water factor
                double swf = Math.Max(0.0, Math.Min(1.0, WF(layer, index) + 0.20));

                // get the soil temperature factor
                double tf = Math.Max(0.0, Math.Min(1.0, (st[layer] / 40.0) + 0.20));

                // note (jngh) oc & ph are not updated during simulation
                //      mep    following equation would be better written in terms of hum_C and biom_C
                //      mep    oc(layer) = (hum_C(layer) + biom_C(layer))*soiln2_fac (layer)*10000.

                // get potential fraction of urea for hydrolysis
                double ak = Math.Max(0.25, Math.Min(1.0, -1.12 + 1.31 * _oc[layer] + 0.203 * ph[layer] - 0.155 * _oc[layer] * ph[layer]));

                //get amount hydrolysed;
                result = Math.Max(0.0, Math.Min(_urea[layer], ak * _urea[layer] * Math.Min(swf, tf)));
            }
        }
        else
            result = 0.0;
        return result;
    }

    private double Nitrification(int layer)
    {
        // + Purpose
        //     Calculate the amount of NH4 converted to NO3 via nitrification

        // + Notes
        //        This routine is much simplified from original CERES code
        //        pH effect on nitrification is not invoked

        // dsg 200508  use different values for some constants when anaerobic conditions dominate
        int index;                 // index = 1 for aerobic and 2 for anaerobic conditions
        index = (!is_pond_active) ? 1 : 2;

        // get the soil ph factor
        double phf = pHFNitrf(layer);

        // get the soil  water factor
        double wfd = WFNitrf(layer, index);

        // get the soil temperature factor
        double tf = TF(layer, index);

        // calculate the optimum nitrification rate (ppm)
        double nh4_ppm = _nh4[layer] * convFactor_kgha2ppm(layer);
        double opt_nitrif_rate_ppm = MathUtility.Divide(nitrification_pot * nh4_ppm, nh4_ppm + nh4_at_half_pot, 0.0);

        // calculate the optimum nitrification rate (kgN/ha)
        double opt_nitrif_rate = MathUtility.Divide(opt_nitrif_rate_ppm, convFactor_kgha2ppm(layer), 0.0);

        // calculate the theoretical nitrification rate (after limiting factor and inhibition)
        double theor_nitrif_rate = opt_nitrif_rate * Math.Min(wfd, Math.Min(tf, phf)) * Math.Max(0.0, 1.0 - _nitrification_inhibition[layer]);
        // NOTE: factors to adjust rate of nitrification are used combined index, with phn removed to match CERES v1

        // calculate the actual nitrification rate (make sure NH4 will not go below minimum value)
        double actual_nitrif_rate = Math.Max(0.0, Math.Min(theor_nitrif_rate, _nh4[layer] - nh4_min[layer]));

        //dlt_nh4_dnit[layer] = actual_nitrif_rate * dnit_nitrf_loss;
        //effective_nitrification[layer] = actual_nitrif_rate - dlt_nh4_dnit[layer];
        //n2o_atm[layer] += dlt_nh4_dnit[layer];

        return actual_nitrif_rate;
    }

    private double DenitrificationInNitrification(int layer)
    {
        // + Purpose
        //     Calculate the amount of N2O produced during nitrification

        double result = dlt_nitrification[layer] * dnit_nitrf_loss;

        return result;
    }

    private double Denitrification(int layer)
    {
        // + Purpose
        //     Calculate the amount of N2O produced during denitrification

        //+  Purpose
        //     Calculate amount of NO3 transformed via denitrification.
        //       Will happend whenever: 
        //         - the soil water in the layer > the drained upper limit (Godwin et al., 1984),
        //         - the NO3 nitrogen concentration > 1 mg N/kg soil,
        //         - the soil temperature >= a minimum temperature.

        // + Assumptions
        //     That there is a root system present.  Rolston et al. say that the denitrification rate coeffficient (dnit_rate_coeff) of non-cropped
        //       plots was 0.000168 and for cropped plots 3.6 times more (dnit_rate_coeff = 0.0006). The larger rate coefficient was required
        //       to account for the effects of the root system in consuming oxygen and in adding soluble organic C to the soil.

        //+  Notes
        //       Reference: Rolston DE, Rao PSC, Davidson JM, Jessup RE (1984). "Simulation of denitrification losses of Nitrate fertiliser applied
        //        to uncropped, cropped, and manure-amended field plots". Soil Science Vol 137, No 4, pp 270-278.
        //
        //       Reference for Carbon availability factor: Reddy KR, Khaleel R, Overcash MR (). "Carbon transformations in land areas receiving 
        //        organic wastes in relation to nonpoint source pollution: A conceptual model".  J.Environ. Qual. 9:434-442.


        //double active_c;              // water extractable organic carbon as
        //// "available" C conc. (mg C/kg soil)
        //double tf;                    // temperature factor affecting
        ////    denitrification rate (0-1)
        //double wf;                    // soil moisture factor affecting
        ////    denitrification rate (0-1)
        //double no3_avail;             // soil nitrate available (kg/ha)
        //double hum_c_conc;            // carbon conc. of humic pool
        ////    (mg C/kg soil)
        //double fom_c_conc;            // carbon conc. of fresh organic pool
        //    (mg C/kg soil)

        // make sure no3 will not go below minimum
        if (_no3[layer] < no3_min[layer])
            return 0.0;


        // get available carbon from soil organic pools
        double active_c = 0.0031 * (hum_c[layer] + fom_c_pool1[layer] + fom_c_pool2[layer] + fom_c_pool3[layer]) * convFactor_kgha2ppm(layer) + 24.5;
        // Note CM V2 had active_c = fom_C_conc + 0.0031*hum_C_conc + 24.5

        // get the soil water factor
        double wf = WFDenit(layer);

        // get the soil temperature factor
        double tf = Math.Max(0.0, Math.Min(1.0, 0.1 * Math.Exp(0.046 * st[layer])));
        // This is an empirical dimensionless function to account for the effect of temperature.
        // The upper limit of 1.0 means that optimum denitrification temperature is 50 oC and above.  At 0 oC it is 0.1 of optimum, and at -20 oC is about 0.04.

        // calculate denitrification rate  - kg/ha
        double result = dnit_rate_coeff * active_c * wf * tf * _no3[layer];

        // prevent no3 from falling below NO3_min
        result = Math.Max(0.0, Math.Min(result, _no3[layer] - no3_min[layer]));

        return result;
    }

    private double Denitrification_Nratio(int layer)
    {
        // + Purpose
        //     Calculate the N2 to N2O ration during denitrification

        // the water filled pore space (%)
        double WFPS = sw_dep[layer] / sat_dep[layer] * 100.0;

        // CO2 production today (kgC/ha)
        double CO2 = (_dlt_fom_c_atm[0][layer] + _dlt_fom_c_atm[1][layer] + _dlt_fom_c_atm[2][layer] + dlt_biom_c_atm[layer] + dlt_hum_c_atm[layer]);
        //                      /(bd[layer] * dlayer[layer]) * 100.0;

        // calculate the terms for the formula from Thornburn et al (2010)
        double RtermA = 0.16 * dnit_k1;
        //double RtermB = (CO2 > 0.0) ?
        //     dnit_k1 * (Math.Exp(-0.8 * (_no3[layer] * convFactor_kgha2ppm(layer) / CO2)))
        //     : 0.0;
        double RtermB = (CO2 > 0.0) ? dnit_k1 * (Math.Exp(-0.8 * (_no3[layer] / CO2))) : 0.0;
        double RtermC = 0.1;
        bool didInterpolate;
        double RtermD = MathUtility.LinearInterpReal(WFPS, dnit_wfps, dnit_n2o_factor, out didInterpolate);
        // RTermD = (0.015 * WFPS) - 0.32;

        double result = Math.Max(RtermA, RtermB) * Math.Max(RtermC, RtermD);

        return result;
    }

    private void CheckProfile(float[] newProfile)
    {
        dlt_n_loss_in_sed = 0.0;
        dlt_c_loss_in_sed = 0.0;

        // How to decide:
        // if bedrock is lower than lowest  profile depth, we won't see
        // any change in profile, even if there is erosion. Ideally we
        // should test both soil_loss and dlayer for changes to cater for
        // manager control. But, the latter means we have to fudge enr for the
        // loss from top layer.

        if (soil_loss > 0.0 && p_n_reduction)
        {
            // move pools
            // EJZ:: Why aren't no3 and urea moved????
            dlt_n_loss_in_sed += MoveLayers(ref _nh4, newProfile);
            dlt_c_loss_in_sed += MoveLayers(ref inert_c, newProfile);
            dlt_c_loss_in_sed += MoveLayers(ref biom_c, newProfile);
            dlt_n_loss_in_sed += MoveLayers(ref biom_n, newProfile);
            dlt_c_loss_in_sed += MoveLayers(ref hum_c, newProfile);
            dlt_n_loss_in_sed += MoveLayers(ref hum_n, newProfile);
            dlt_n_loss_in_sed += MoveLayers(ref fom_n_pool1, newProfile);
            dlt_n_loss_in_sed += MoveLayers(ref fom_n_pool2, newProfile);
            dlt_n_loss_in_sed += MoveLayers(ref fom_n_pool3, newProfile);
            dlt_c_loss_in_sed += MoveLayers(ref fom_c_pool1, newProfile);
            dlt_c_loss_in_sed += MoveLayers(ref fom_c_pool2, newProfile);
            dlt_c_loss_in_sed += MoveLayers(ref fom_c_pool3, newProfile);

        }
        if (dlayer == null || newProfile.Length != dlayer.Length)
            ResizeLayerArrays(newProfile.Length);
        dlayer = newProfile;
    }

    private void SendExternalMassFlowN(double dltN)
    {
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
        ExternalMassFlowType massBalanceChange = new ExternalMassFlowType();
        if (Math.Abs(dltC) <= epsilon)
            dltC = 0.0;
        massBalanceChange.FlowType = dltC >= 0 ? "gain" : "loss";
        massBalanceChange.PoolClass = "soil";
        massBalanceChange.N = (float)Math.Abs(dltC);
        ExternalMassFlow.Invoke(massBalanceChange);
    }

    private void SendActualResidueDecompositionCalculated()
    { // Send the equivalent of the old 'Do_Decompose' action to residue/manure and soilP
        if (actualresiduedecompositioncalculated != null)
        {
            int nLayers = dlayer.Length;

            soilp_dlt_res_c_atm = new double[nLayers];
            soilp_dlt_res_c_hum = new double[nLayers];
            soilp_dlt_res_c_biom = new double[nLayers];
            soilp_dlt_org_p = new double[nLayers];
            double[] c_summed_layer = new double[nLayers];
            SurfaceOrganicMatterDecompType SOMDecomp = new SurfaceOrganicMatterDecompType();
            Array.Resize(ref SOMDecomp.Pool, num_residues);

            // Potential decomposition was given to this module by a residue
            // module.  We now explicitly tell the residue module to decompose
            // some of its residue now.  If we have been unable to decompose the
            // potential it gave us, the value returned belows will have been
            // reduced to the actual value.

            for (int residue = 0; residue < num_residues; residue++)
            {
                double c_summed = 0.0;
                double n_summed = 0.0;
                double[] dlt_res_c_decomp = new double[nLayers];
                double[] dlt_res_n_decomp = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                {
                    dlt_res_c_decomp[layer] = _dlt_res_c_hum[layer][residue] +
                                              _dlt_res_c_biom[layer][residue] +
                                              _dlt_res_c_atm[layer][residue];
                    c_summed += dlt_res_c_decomp[layer];

                    dlt_res_n_decomp[layer] = this.dlt_res_n_decomp[layer][residue];
                    n_summed += dlt_res_n_decomp[layer];
                }

                // dsg 131103  Now, pack up the structure to return decompositions to SurfaceOrganicMatter
                SOMDecomp.Pool[residue] = new SurfaceOrganicMatterDecompPoolType();
                SOMDecomp.Pool[residue].FOM = new FOMType();
                SOMDecomp.Pool[residue].Name = residue_name[residue];
                SOMDecomp.Pool[residue].OrganicMatterType = residue_type[residue];

                // dsg 131103   The 'amount' value will not be used by SurfaceOrganicMatter, so send zero as default
                SOMDecomp.Pool[residue].FOM.amount = 0.0F;
                if (Math.Abs(c_summed) < epsilon)
                    c_summed = 0.0;
                if (Math.Abs(n_summed) < epsilon)
                    n_summed = 0.0;
                SOMDecomp.Pool[residue].FOM.C = (float)c_summed;
                SOMDecomp.Pool[residue].FOM.N = (float)n_summed;

                // dsg 131103   The 'P' value will not be collected by SurfaceOrganicMatter, so send zero as default.
                SOMDecomp.Pool[residue].FOM.P = 0.0F;
                SOMDecomp.Pool[residue].FOM.AshAlk = 0.0F;

                // dsg 131004 soilp needs some stuff - very ugly process - needs to be streamlined
                //  create some variables which soilp can "get" - layer based arrays independant of residues
                for (int layer = 0; layer < nLayers; layer++)
                {
                    soilp_dlt_res_c_atm[layer] += _dlt_res_c_atm[layer][residue];
                    soilp_dlt_res_c_hum[layer] += _dlt_res_c_hum[layer][residue];
                    soilp_dlt_res_c_biom[layer] += _dlt_res_c_biom[layer][residue];
                    c_summed_layer[layer] += dlt_res_c_decomp[layer];
                }
            }

            // dsg 131004  calculate the old dlt_org_p (from the old Decomposed event sent by residue2) for getting by soilp
            for (int layer = 0; layer < nLayers; layer++)
                soilp_dlt_org_p[layer] = c_summed_layer[layer] *
                    MathUtility.Divide(SumDoubleArray(pot_p_decomp), SumDoubleArray(pot_c_decomp), 0.0);

            actualresiduedecompositioncalculated.Invoke(SOMDecomp);
        }
    }

    private double LayerFract(int layer)
    {
        double layerFract = soil_loss * convFactor_kgha2ppm(layer) / 1000.0;
        if (layerFract > 1.0)
        {
            int layerNo = layer + 1; // Convert to 1-based index for display
            double layerPercent = layerFract * 100.0; // Convert fraction to percentage
            throw new Exception("Soil loss is greater than depth of layer(" + layerNo.ToString() + ") by " +
                layerPercent.ToString() + "%.\nConstrained to this layer. Re-mapping of SoilN pools will be incorrect.");
        }
        return Math.Min(0.0, layerFract);
    }

    #endregion

    #region Auxiliar processes

    private double[] FractFomC(int fract)
    {
        switch (fract)
        {
            case 0: return fom_c_pool1;
            case 1: return fom_c_pool2;
            case 2: return fom_c_pool3;
            default: throw new Exception("Coding error: bad fraction in FractFomC");
        }
    }

    private double[] FractFomN(int fract)
    {
        switch (fract)
        {
            case 0: return fom_n_pool1;
            case 1: return fom_n_pool2;
            case 2: return fom_n_pool3;
            default: throw new Exception("Coding error: bad fraction in FractFomN");
        }
    }

    private double[] FractRDFom(int fract)
    {
        switch (fract)
        {
            case 0: return rd_carb;
            case 1: return rd_cell;
            case 2: return rd_lign;
            default: throw new Exception("Coding error: bad fraction in FractRDFom");
        }
    }

    // Changed from subroutine to function returning amount of profile loss
    private double MoveLayers(ref double[] variable, float[] newProfile)
    {
        double profile_loss = 0.0;
        double layer_loss = 0.0;
        double layer_gain = 0.0;
        int lowest_layer = dlayer.Length;
        int new_lowest_layer = newProfile.Length;

        double yesterdays_n = SumDoubleArray(variable);

        // initialise layer loss from beloe profile same as bottom layer

        double profile_depth = SumFloatArray(dlayer);
        double new_profile_depth = SumFloatArray(newProfile);

        if (MathUtility.FloatsAreEqual(profile_depth, new_profile_depth))
        {
            // move from below bottom layer - assume it has same properties
            // as bottom layer
            layer_loss = variable[lowest_layer - 1] * LayerFract(lowest_layer - 1);
        }
        else
        {
            // we're going into bedrock
            layer_loss = 0.0;
            // now see if bottom layers have been merged.
            if (lowest_layer > new_lowest_layer && lowest_layer > 1)
            {
                // merge the layers
                for (int layer = lowest_layer - 1; layer >= new_lowest_layer; layer--)
                {
                    variable[layer - 1] += variable[layer];
                    variable[layer] = 0.0;
                }
                Array.Resize(ref variable, new_lowest_layer);
            }
        }
        double profile_gain = layer_loss;

        // now move from bottom layer to top
        for (int layer = new_lowest_layer - 1; layer >= 0; layer--)
        {
            // this layer gains what the lower layer lost
            layer_gain = layer_loss;
            layer_loss = variable[layer] * LayerFract(layer);
            variable[layer] += layer_gain - layer_loss;
        }

        // now adjust top layer for enrichment
        double enr = enr_a_coeff * Math.Pow(soil_loss * 1000, -1.0 * enr_b_coeff);
        enr = Math.Max(1.0, Math.Min(enr, enr_a_coeff));

        profile_loss = layer_loss * enr;
        variable[0] = Math.Max(0.0, variable[0] + layer_loss - profile_loss);

        // check mass balance
        double todays_n = SumDoubleArray(variable);
        yesterdays_n += profile_gain - profile_loss;
        if (!MathUtility.FloatsAreEqual(todays_n, yesterdays_n))
        {
            throw new Exception("N mass balance out");
        }
        return profile_loss;
    }

    private double TotalC()
    {
        double carbon_tot = 0.0;
        if (dlayer != null)
        {
            int numLayers = dlayer.Length;
            for (int layer = 0; layer < numLayers; layer++)
            {
                carbon_tot += fom_c_pool1[layer] + fom_c_pool2[layer] + fom_c_pool3[layer] + hum_c[layer] + biom_c[layer];
            }
        }
        return carbon_tot;
    }

    private double TotalN()
    {
        double nitrogen_tot = 0.0;
        if (dlayer != null)
        {
            int numLayers = dlayer.Length;
            for (int layer = 0; layer < numLayers; layer++)
            {
                nitrogen_tot += fom_n[layer] + hum_n[layer] + biom_n[layer] + _no3[layer] + _nh4[layer] + _urea[layer];
            }
        }
        return nitrogen_tot;
    }

    private double pHFNitrf(int layer)
    {
        // +  Purpose
        //      Calculates a 0-1 pH factor for nitrification.

        bool DidInterpolate;
        return MathUtility.LinearInterpReal(ph[layer], pHf_nit_pH, pHf_nit_values, out DidInterpolate);
    }

    private double WFNitrf(int layer, int index)
    {
        // +  Purpose
        //      Calculates a 0-1 water factor for nitrification.

        // +  Assumptions
        //     index = 1 for aerobic conditions, 2 for anaerobic

        // temporary water factor (0-1)
        double wfd = 1.0;
        if (sw_dep[layer] > dul_dep[layer] && sat_dep[layer] > dul_dep[layer])
        {   // saturated
            wfd = 1.0 + (sw_dep[layer] - dul_dep[layer]) / (sat_dep[layer] - dul_dep[layer]);
            wfd = Math.Max(1.0, Math.Min(2.0, wfd));
        }
        else
        {
            // unsaturated
            // assumes rate of mineralization is at optimum rate until soil moisture midway between dul and ll15
            wfd = MathUtility.Divide(sw_dep[layer] - ll15_dep[layer], dul_dep[layer] - ll15_dep[layer], 0.0);
            wfd = Math.Max(0.0, Math.Min(1.0, wfd));
        }

        bool didInterpolate;
        if (index == 1)
            return MathUtility.LinearInterpReal(wfd, wfnit_index, wfnit_values, out didInterpolate);
        else
            // if pond is active, and aerobic conditions dominate, assume wf_nitrf = 0
            return 0;
    }

    private double WFDenit(int layer)
    {
        // + Purpose
        //     Calculates a 0-1 water factor for denitrification

        // temporary water factor (0-1); 0 is used if unsaturated
        double wfd = 0.0;
        if (sw_dep[layer] > dul_dep[layer] && sat_dep[layer] > dul_dep[layer])  // saturated
            wfd = Math.Pow((sw_dep[layer] - dul_dep[layer]) / (sat_dep[layer] - dul_dep[layer]), dnit_wf_power);
        return Math.Max(0.0, Math.Min(1.0, wfd));
    }

    private double WF(int layer, int index)
    {
        // + Purpose
        //     Calculates a 0-1 water factor for mineralisation.

        // + Assumptions
        //     index = 1 for aerobic conditions, 2 for anaerobic

        // temporary water factor (0-1)
        double wfd;
        if (sw_dep[layer] > dul_dep[layer])
        { // saturated
            if (sat_dep[layer] == dul_dep[layer])
                wfd = 1.0;
            else
                wfd = Math.Max(1.0, Math.Min(2.0,
                    1.0 + (sw_dep[layer] - dul_dep[layer]) / (sat_dep[layer] - dul_dep[layer])));
        }
        else
        { // unsaturated
            // assumes rate of mineralization is at optimum rate until soil moisture midway between dul and ll15
            if (dul_dep[layer] == ll15_dep[layer])
                wfd = 0.0;
            else
                wfd = Math.Max(0.0, Math.Min(1.0, (sw_dep[layer] - ll15_dep[layer]) / (dul_dep[layer] - ll15_dep[layer])));
        }

        if (index == 1)
        {
            bool didInterpolate;
            return MathUtility.LinearInterpReal(wfd, wfmin_index, wfmin_values, out didInterpolate);
        }
        else if (index == 2) // if pond is active, and liquid conditions dominate, assume wf = 1
            return 1.0;
        else
            throw new Exception("SoilN2 WF function - invalid value for \"index\" parameter");
    }

    private double TF(int layer, int index)
    {
        // + Purpose
        //     Calculate a temperature factor, based on the soil temperature of the layer, for nitrification and mineralisation

        // + Assumptions
        //     index = 1 for aerobic conditions, 2 for anaerobic

        // Alternate version from CM:
        //      tf = (soil_temp[layer] - 5.0) /30.0
        // because tf is bound between 0 and 1, the effective temperature (soil_temp) lies between 5 to 35.
        // alternative quadratic temperature function is preferred with optimum temperature (CM - used 32 deg)

        if (st[layer] > 0.0)
        {
            if (opt_temp[index - 1] == 0.0)
                return 0.0;
            else
                return Math.Max(0.0, Math.Min(1.0, (st[layer] * st[layer]) / Math.Pow(opt_temp[index - 1], 2.0)));
        }
        else // soil is too cold for mineralisation
            return 0.0;
    }

    private double RothcTF(int layer, int index)
    {
        // + Purpose
        //     Calculate a temperature factor, based on the soil temperature of the layer, for nitrification and mineralisation

        double t = Math.Min(st[layer], opt_temp[layer]);
        return 47.9 / (1.0 + Math.Exp(106.0 / (t + 18.3)));
    }

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
        foreach (double Value in anArray)
            result += Value;
        return result;
    }

    private float SumFloatArray(float[] anArray)
    {
        float result = 0.0F;
        foreach (float Value in anArray)
            result += Value;
        return result;
    }

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