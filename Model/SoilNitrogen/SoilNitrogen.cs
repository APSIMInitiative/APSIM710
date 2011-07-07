using System;
using System.Reflection;
using System.Collections.Generic;
using System.Text;
using ModelFramework;
using CSGeneral;

/// <summary>
/// A more-or-less direct port of the Fortran SoilN model
/// Ported by Eric Zurcher Sept/Oct 2010
/// </summary>
public class SoilN : Instance
{
#region Parameters used to initialise the model

#region Parameters we expect to see provided by the user
    [Param(Optional=true)]
    private string soiltype = "standard";   // soil type spec used to determine mineralisation properties

    [Param(Optional=true, MinVal=0.0, MaxVal=50.0)]
    [Input(Optional = true)]
    [Units("oC")]
    private double amp = 0.0;       // annual amplitude in mean monthly temperature (oC) [ave_temp]

    [Param(Optional=true, MinVal=0.0, MaxVal=50.0)]
    [Input(Optional = true)]
    [Units("oC")]
    private double tav = 0.0;       // annual average ambient temperature (oC)

    [Param(MinVal=0.0, MaxVal=500.0)]
    private double root_cn = 0.0;   // initial C:N ratio of roots 

    [Param(Optional=true, MinVal=0.0, MaxVal=1000.0)]
    private double[] root_cn_pool = null;   // initial C:N ratio of each of the three root composition pools (carbohydrate, cellulose, and lignin)

    [Param]
    private double root_wt = 0.0;   // initial root weight

    [Param(Optional=true)]
    private double root_depth = 0.0;// initial depth over which roots are distributed (mm)

    [Param]
    private double soil_cn = 0.0;   // soil C:N ration

    private double[] _oc;  // Internal variable for oc. This is set, but doesn't get updated
    [Param]
    [Output]
    [Units("%")]
    [Description("Organic carbon")]
    double[] oc             // organic carbon concentration (%)
    {
        get 
        {
            int nLayers = dlayer.Length;
            double[] oc_percent = new double[nLayers];
            for (int i = 0; i < nLayers; i++)
            {
                oc_percent[i] = (hum_c[i] + biom_c[i]) * SoilN2Fac(i) * ppm2fract * fract2pcnt;
            }
            return oc_percent;
        }
        set
        {
            _oc = value;
        }
    }

    [Param(Optional = true, MinVal=3.5, MaxVal=11.0)]
    [Input(Optional = true)]
    private double[] ph = null;     // pH of soil in a 1:1 soil-water slurry

    [Param]
    private double[] fbiom = null;  // initial ration of biomass-C to mineralizable humic-C (0-1) [fr_biom_C]

    [Param]
    private double[] finert = null; // initial proportion of total soil C that is not subject to mineralization (0-1) [fr_inert_C]

    private double[] _no3ppm;   // local variable to hold parameter value until we can get dlayer!
    [Param]
    [Output]
    [Units("mg/kg")]
    [Description("Nitrate nitrogen")]
    private double[] no3ppm     // nitrate nitrogen (ppm)  Watch out! no3 and no3ppm aren't quite the same thing... different units
    {
        get 
        {
            if (initDone)
            {
                Array.Resize(ref _no3ppm, _no3.Length);
                for (int layer = 0; layer < _no3.Length; ++layer)
                    _no3ppm[layer] = _no3[layer] * SoilN2Fac(layer);
            }
            return _no3ppm; 
        }
        set
        {
            _no3ppm = value;
            
            if (initDone && _no3 != null)
            {
                double sumOld = SumDoubleArray(_no3);
                Array.Resize(ref _no3, value.Length);
                for (int layer = 0; layer < value.Length; ++layer)
                {
                    double convFact = SoilN2Fac(layer);
                    if (value[layer] < no3ppm_min)
                        Console.WriteLine("no3ppm[layer] = " + value[layer].ToString() +
                                " less than lower limit of " + no3ppm_min);
                    _no3[layer] = MathUtility.Divide(value[layer], convFact, 0.0);
                }
                SendExternalMassFlow(SumDoubleArray(_no3) - sumOld);
            }
        }
    }

    private double[] _nh4ppm; // local variable to hold parameter value until we can get dlayer!
    [Param]
    [Output]
    [Units("mg/kg")]
    [Description("Ammonium nitrogen")]
    private double[] nh4ppm    // ammonium nitrogen (ppm) 
    {
        get
        {
            if (initDone)
            {
                Array.Resize(ref _nh4ppm, _nh4.Length);
                for (int layer = 0; layer < _nh4.Length; ++layer)
                    _nh4ppm[layer] = _nh4[layer] * SoilN2Fac(layer);
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
                    double convFact = SoilN2Fac(layer);
                    if (value[layer] < nh4ppm_min)
                        Console.WriteLine("nh4ppm[layer] = " + value[layer].ToString() +
                                " less than lower limit of " + nh4ppm_min);
                    _nh4[layer] = MathUtility.Divide(value[layer], convFact, 0.0);
                }
                SendExternalMassFlow(SumDoubleArray(_nh4) - sumOld);
            }
            else
                _nh4ppm = value;
        }
    }

    private double[] _ureappm; // local variable to hold parameter value until we can get dlayer!
    [Param(Optional = true)]
    [Output]
    [Units("mg/kg")]
    [Description("Urea nitrogen")]
    private double[] ureappm    // urea nitrogen (ppm)
    {
        get
        {
            if (initDone)
            {
                Array.Resize(ref _ureappm, _urea.Length);
                for (int layer = 0; layer < _urea.Length; ++layer)
                    _ureappm[layer] = _urea[layer] * SoilN2Fac(layer);
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
                    double convFact = SoilN2Fac(layer);
                    _urea[layer] = MathUtility.Divide(value[layer], convFact, 0.0);
                }
                SendExternalMassFlow(SumDoubleArray(_urea) - sumOld);
            }
            else
                _ureappm = value;
        }
    }
    #endregion
#region Parameters not usually provided by the user
    [Param]
    private double enr_a_coeff = 0.0;   // enrichment equation coefficient a

    [Param]
    private double enr_b_coeff = 0.0;   // enrichment equation coefficient b

    [Param]
    private string profile_reduction = "off";

    [Param(Optional=true)]
    private string use_organic_solutes = "off";

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    public double no3ppm_min;           // minimum allowable NO3 (ppm)

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    public double nh4ppm_min;           // minimum allowable NH4 (ppm)

    [Param(Optional = true, MinVal = 1.0, MaxVal = 50.0)]
    public double mcn = 8.0;            // C:N ratio of microbes ()

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    public double ef_fom;               // fraction of FOM C mineralized retained in system (0-1)   

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    public double fr_fom_biom;          // fraction of retained FOM C transferred to biomass (0-1)

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    public double ef_biom;              // fraction of biomass C mineralized retained in system (0-1)

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    public double fr_biom_biom;         // fraction of retained biomass C returned to biomass (0-1)

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    public double ef_hum;               // fraction of humic C mineralized retained in system (0-1)

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    public double[] rd_biom = null;     // potential rate of soil biomass mineralization (per day)

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    public double[] rd_hum = null;      // potential rate of humus mineralization (per day)

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    public double ef_res;               // fraction of residue C mineralized retained in system (0-1)

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    public double fr_res_biom;          // fraction of retained residue C transferred to biomass (0-1)

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    public double[] rd_carb;            // maximum rate constants for decomposition of FOM pools [carbohydrate component] (0-1)

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    public double[] rd_cell;            // maximum rate constants for decomposition of FOM pools [cellulose component] (0-1)

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    public double[] rd_lign;            // maximum rate constants for decomposition of FOM pools [lignin component] (0-1)

    [Param(Name = "fom_type")]
    public String[] fom_types;           // list of fom types

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Description("Fraction of carbohydrate in FOM, for each FOM type")]
    public double[] fract_carb;            // carbohydrate fraction of FOM (0-1)          

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Description("Fraction of cellulose in FOM, for each FOM type")]
    public double[] fract_cell;            // cellulose fraction of FOM (0-1)          

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Description("Fraction of lignin in FOM, for each FOM type")]
    public double[] fract_lign;            // lignin fraction of FOM (0-1)          

    [Param(MinVal = 0.0, MaxVal = 3.0)]
    public double oc2om_factor;         // conversion from OC to OM

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    public double fom_min;              // minimum allowable FOM (kg/ha)

    [Param(MinVal = 0.0, MaxVal = 1000.0)]
    public double min_depth;            // depth from which mineral N can be immobilized by decomposing residues (mm)

    [Param(MinVal = 0.0, MaxVal = 10.0)]
    public double cnrf_coeff;           // coeff. to determine the magnitude of C:N effects on decomposition of FOM ()

    [Param(MinVal = 5.0, MaxVal = 100.0)]
    public double cnrf_optcn;           // C:N above which decomposition rate of FOM declines ()

    [Param(MinVal = 5.0, MaxVal = 100.0)]
    public double[] opt_temp;           // Soil temperature above which there is no further effect on mineralisation and nitrification (oC)

    [Param(MinVal = 0.0, MaxVal = 2.0)]
    public double[] wfmin_index;        // index specifying water content for water factor for mineralization

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    public double[] wfmin_values;       // value of water factor(mineralization) function at given index values

    [Param(MinVal = 0.0, MaxVal = 2.0)]
    public double[] wfnit_index;        // index specifying water content for water factor for nitrification

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    public double[] wfnit_values;       // value of water factor(nitrification) function at given index values

    [Param(MinVal = 0.0, MaxVal = 100.0)]
    public double nitrification_pot;    // Potential nitrification by soil (ppm)

    [Param(MinVal = 0.0, MaxVal = 200.0)]
    public double nh4_at_half_pot;      // nh4 conc at half potential (ppm)   

    [Param(MinVal = 0.0, MaxVal = 14.0)]
    public double[] pHf_nit_pH;         // pH values for specifying pH factor for nitrification

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    public double[] pHf_nit_values;     // value of pH factor(nitrification) function for given pH values

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    public double dnit_rate_coeff;      // denitrification rate coefficient (kg/mg)

    [Param(MinVal = 0.0, MaxVal = 5.0)]
    public double dnit_wf_power;        // denitrification water factor power term

    [Param(MinVal = 0.0, MaxVal = 100.0)]
    public double dnit_k1;              // K1 parameter from Thorburn et al (2010) for N2O model

    [Param(MinVal = 0.0, MaxVal = 100.0)]
    public double[] dnit_wfps;            // WFPS for calculating the n2o fraction of denitrification

    [Param(MinVal = 0.0, MaxVal = 100.0)]
    public double[] dnit_n2o_factor;      // WFPS factor for n2o fraction of denitrification

    [Param(MinVal = 0.0, MaxVal = 1.0)]
    public double dnit_nitrf_loss;      // Fraction of nitrification lost as denitrification
#endregion
#endregion

#region Outputs we make available
    private double[] _no3 = null;  // Internal variable associated with the no3 property
    // Output variables made available to other components
    [Output]
    [Units("kg/ha")]
    [Description("Nitrate nitrogen")]
    double[] no3           // nitrate nitrogen
    {
        get { return _no3; }
        set
        {
            double sumOld = SumDoubleArray(_no3);
            for (int layer = 0; layer < value.Length; ++layer)
            {
                if (layer >= _no3.Length)
                {
                    Console.WriteLine("Attempt to assign nitrate value to non-existent soil layer");
                    break;
                }
                else
                {

                    _no3[layer] = value[layer];
                    if (_no3[layer] < no3_min[layer])
                        Console.WriteLine("no3[layer] = " + _no3[layer].ToString() +
                                " less than lower limit of " + no3_min[layer]);
                }
            }
            SendExternalMassFlow(SumDoubleArray(_no3) - sumOld);
        }
    }

    [Output]
    [Units("kg/ha")]
    [Description("Net NO3 change today")]
    double[] dlt_no3_net;   // net no3 change today

    [Output]
    [Units("kg/ha")]
    [Description("Minimum allowable NO3")]
    double[] no3_min;       // minimum allowable NO3

    private double[] _nh4;  // Internal variable associated with the no3 property
    [Output]
    [Units("kg/ha")]
    [Description("Ammonium nitrogen")]
    double[] nh4            // ammonium nitrogen
    {
        get { return _nh4; }
        set
        {
            double sumOld = SumDoubleArray(_nh4);
            for (int layer = 0; layer < value.Length; ++layer)
            {
                if (layer >= _nh4.Length)
                {
                    Console.WriteLine("Attempt to assign ammonium value to non-existent soil layer");
                    break;
                }
                else
                {
                    _nh4[layer] = value[layer];
                    if (_nh4[layer] < nh4_min[layer])
                        Console.WriteLine("nh4[layer] = " + _nh4[layer].ToString() +
                                " less than lower limit of " + nh4_min[layer]);
                }
            }
            SendExternalMassFlow(SumDoubleArray(_nh4) - sumOld);
        }
    }

    [Output]
    [Units("kg/ha")]
    [Description("Net NH4 change today")]
    double[] dlt_nh4_net;   // net nh4 change today

    [Output]
    [Units("kg/ha")]
    [Description("Minimum allowable NH4")]
    double[] nh4_min;       // minimum allowable NH4

    private double[] _urea;  // Internal variable associated with the urea property
    [Output]
    [Units("kg/ha")]
    [Description("Urea nitrogen")]
    double[] urea            // urea nitrogen
    {
        get { return _urea; }
        set
        {
            double sumOld = SumDoubleArray(_urea);
            for (int layer = 0; layer < Math.Min(value.Length, _urea.Length); ++layer)
                if (layer >= _urea.Length)
                {
                    Console.WriteLine("Attempt to assign urea value to non-existent soil layer");
                }
                else
                    _urea[layer] = value[layer];
            SendExternalMassFlow(SumDoubleArray(_urea) - sumOld);
        }
    }

    [Output]
    [Units("kg/ha")]
    [Description("Nitrogen moved by nitrification")]
    double[] dlt_rntrf;     // nitrogen moved by nitrification (kg/ha)

    [Output]
    [Units("kg/ha")]
    [Description("Nitrogen moved by nitrification")]
    double[] nitrification
    { get { return dlt_rntrf; } }

    [Output]
    [Units("kg/ha")]
    [Description("Effective nitrogen moved by nitrification")]
    double[] effective_nitrification; // effective nitrogen moved by nitrification
                                      // (Alias dlt_rntrf_eff)

    [Output]
    [Units("kg/ha")]
    [Description("Nitrogen moved by hydrolysis")]
    double[] dlt_urea_hydrol;   // nitrogen moved by hydrolysis

    [Output]
    [Units("kg/ha")]
    [Description("Excess N required above NH4 supply")]
    double[] excess_nh4;    // excess N required above NH4 supply

    [Output]
    [Units("kg/ha")]
    [Description("Nitrogen in FOM")]
    double[] fom_n;         // nitrogen in FOM

    [Output]
    [Units("kg/ha")]
    [Description("Nitrogen in FOM pool 1")]
    double[] fom_n_pool1;

    [Output]
    [Units("kg/ha")]
    [Description("Nitrogen in FOM pool 2")]
    double[] fom_n_pool2;

    [Output]
    [Units("kg/ha")]
    [Description("Nitrogen in FOM pool 3")]
    double[] fom_n_pool3;

    [Output]
    [Units("kg/ha")]
    [Description("Humic nitrogen")]
    double[] hum_n;         // Humic N

    [Output]
    [Units("kg/ha")]
    [Description("Biomass nitrogen")]
    double[] biom_n;        // biomass nitrogen

    [Output]
    [Units("kg/ha")]
    [Description("FOM C")]
    double[] fom_c         // fresh organic C        
    {
        get
        {
            double[] _fom_c = new double[fom_c_pool1.Length];
            for (int i = 0; i < fom_c_pool1.Length; ++i)
            {
                _fom_c[i] = LayerFomC(i);
            }
            return _fom_c;
        }
    }

    [Output]
    [Description("Number of FOM types")]
    int num_fom_types      // number of fom types read
    { get { return fom_types.Length; } }

    [Output]
    [Description("Fraction of carbohydrate in FOM")]
    public double fr_carb            // carbohydrate fraction of FOM (0-1)          
    { get { return fract_carb[fom_type]; } }

    [Output]
    [Description("Fraction of cellulose in FOM")]
    public double fr_cell            // cellulose fraction of FOM (0-1)          
    { get { return fract_cell[fom_type]; } }

    [Output]
    [Description("Fraction of lignin in FOM")]
    public double fr_lign            // lignin fraction of FOM (0-1)          
    { get { return fract_lign[fom_type]; } }

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
                _carbon_tot[layer] += LayerFomC(layer) +
                                  hum_c[layer] +
                                  biom_c[layer];
            }
            return _carbon_tot;
        }
    }

    [Output]
    [Units("kg/ha")]
    [Description("Net NH4 transformation")]
    double[] nh4_transform_net; // net NH4 transformation today

    [Output]
    [Units("kg/ha")]
    [Description("Net NO3 transformation")]
    double[] no3_transform_net; // net NO3 transformation today

    [Output]
    [Units("kg/ha")]
    [Description("Net NH4 transformation")]
    double[] dlt_res_nh4_min;   // Net Residue NH4 mineralisation

    [Output]
    [Units("kg/ha")]
    [Description("Net FOM N mineralized, negative for immobilization")]
    double[] dlt_fom_n_min;     // net fom N mineralized (negative for immobilization) 

    [Output]
    [Units("kg/ha")]
    [Description("Net biomass N mineralized")]
    double[] dlt_biom_n_min;    // net biomass N mineralized

    [Output]
    [Units("kg/ha")]
    [Description("Net humic N mineralized")]
    double[] dlt_hum_n_min;     // net humic N mineralized

    [Output]
    [Units("kg/ha")]
    [Description("Net Residue NO3 mineralisation")]
    double[] dlt_res_no3_min;   // Net Residue NO3 mineralisation

    [Output]
    [Units("kg/ha")]
    [Description("NO3 N denitrified")]
    double[] dlt_no3_dnit;      // NO3 N denitrified

    [Output]
    [Units("kg/ha")]
    [Description("NH4 N denitrified")]
    double[] dlt_nh4_dnit;      // NH4 N denitrified

    [Output]
    [Units("kg/ha")]
    [Description("Amount of N2O produced")]
    double[] n2o_atm;           // amount of N2O produced

    [Output]
    [Units("kg/ha")]
    [Description("Total N in soil")]
    double[] nit_tot           // total N in soil   
    {
        get
        {
            int numLayers = dlayer.Length;
            double[] nitrogen_tot = new double[numLayers];
            for (int layer = 0; layer < numLayers; layer++)
            {
                nitrogen_tot[layer] += fom_n[layer] +
                                       hum_n[layer] +
                                       biom_n[layer] +
                                       _no3[layer] +
                                       _nh4[layer] +
                                       _urea[layer];
            }
            return nitrogen_tot;
        }
    }

    [Output]
    [Units("kg/ha")]
    [Description("Net N mineralized")]
    double[] dlt_n_min         // net mineralisation
    {
        get
        {
            int nLayers = dlayer.Length;
            double[] _dlt_n_min = new double[nLayers];
            for (int layer = 0; layer < nLayers; layer++)
                _dlt_n_min[layer] = dlt_hum_n_min[layer] +
                                    dlt_biom_n_min[layer] +
                                    dlt_fom_n_min[layer];
            return _dlt_n_min;
        }
    }

    [Output]
    [Units("kg/ha")]
    [Description("Net Residue N mineralisation")]
    double[] dlt_n_min_res
    {
        get
        {
            int nLayers = dlayer.Length;
            double[] _dlt_n_min_res = new double[nLayers];
            for (int layer = 0; layer < nLayers; layer++)
                _dlt_n_min_res[layer] = dlt_res_no3_min[layer] +
                                        dlt_res_nh4_min[layer];
            return _dlt_n_min_res;
        }
    }

    [Output]
    [Units("kg/ha")]
    [Description("Humic N mineralized")]
    double[] dlt_n_min_tot
    {
        get
        {
            int nLayers = dlayer.Length;
            double[] _dlt_n_min_tot = new double[nLayers];
            for (int layer = 0; layer < nLayers; layer++)
                _dlt_n_min_tot[layer] = dlt_hum_n_min[layer] +
                                        dlt_biom_n_min[layer] +
                                        dlt_fom_n_min[layer] +
                                        dlt_res_no3_min[layer] +
                                        dlt_res_nh4_min[layer];
            return _dlt_n_min_tot;
        }
    }

    [Output]
    [Units("kg/ha")]
    [Description("Denitrification")]
    double[] dnit
    {
        get
        {
            int nLayers = dlayer.Length;
            double[] dnit_tot = new double[nLayers];
            for (int layer = 0; layer < nLayers; layer++)
                dnit_tot[layer] = dlt_no3_dnit[layer] + dlt_nh4_dnit[layer];
            return dnit_tot;
        }
    }

    [Output]
    [Units("kg")]
    [Description("Carbon loss in sediment")]
    double dlt_c_loss_in_sed;

    [Output]
    [Units("kg")]
    [Description("N loss in sediment")]
    double dlt_n_loss_in_sed;

    [Output]
    [Units("oC")]
    [Description("Soil temperature")]
    double[] st;

    [Output]
    [Units("kg/ha")]
    [Description("Not fully implemented")]
    double[] org_c_pool1 // Doesn't seem to be fully implemented
    {
        get { return new double [fom_c_pool1.Length]; }
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
                result[layer] = _dlt_fom_c_hum[0][layer] +
                                _dlt_fom_c_hum[1][layer] +
                                _dlt_fom_c_hum[2][layer];
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
                result[layer] = _dlt_fom_c_biom[0][layer] +
                                _dlt_fom_c_biom[1][layer] +
                                _dlt_fom_c_biom[2][layer];
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
                result[layer] = _dlt_fom_c_atm[0][layer] +
                                _dlt_fom_c_atm[1][layer] +
                                _dlt_fom_c_atm[2][layer];
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
            int index = (pond_active == "no") ? 1 : 2;
            for (int layer = 0; layer < nLayers; layer++)
              result[layer] = (soiltype == "rothc") ? RothcTF (layer, index) : TF(layer, index);
            return result;
        }
    }

    // Settable variables
    // Even though these properties are settable, and not meant to be readable,
    // it appears we still need to provide a "get" method for them, since they
    // bear the "Output" attribute. 
    // Perhaps that bit of the infrastructure needs a re-think.
    [Output]
    [Units("kg/ha")]
    double[] dlt_no3
    {
        get { return null; }
        set
        {
            for (int layer = 0; layer < value.Length; ++layer)
            {
                _no3[layer] += value[layer];
                if (_no3[layer] < no3_min[layer])
                    Console.WriteLine("no3[layer] = " + _no3[layer].ToString() +
                            " less than lower limit of " + no3_min[layer]);
            }
        }
    }

    [Output]
    [Units("kg/ha")]
    double[] dlt_nh4 
    {
        get { return null; }
        set
        {
            for (int layer = 0; layer < value.Length; ++layer)
            {
                _nh4[layer] += value[layer];
                if (_nh4[layer] < nh4_min[layer])
                    Console.WriteLine("nh4[layer] = " + _nh4[layer].ToString() +
                            " less than lower limit of " + nh4_min[layer]);
            }
        }
    }

    [Output]
    [Units("kg/ha")]
    double[] dlt_no3ppm
    {
        get { return null; }
        set
        {
            for (int layer = 0; layer < value.Length; ++layer)
            {
                double convFact = SoilN2Fac(layer);
                _no3[layer] += MathUtility.Divide(value[layer], convFact, 0.0);
                if (_no3[layer] < no3_min[layer])
                    Console.WriteLine("no3[layer] = " + _no3[layer].ToString() +
                            " less than lower limit of " + no3_min[layer]);
            }
        }
    }

    [Output]
    [Units("kg/ha")]
    double[] dlt_nh4ppm
    {
        get { return null; }
        set
        {
            for (int layer = 0; layer < value.Length; ++layer)
            {
                double convFact = SoilN2Fac(layer);
                _nh4[layer] += MathUtility.Divide(value[layer], convFact, 0.0);
                if (_nh4[layer] < nh4_min[layer])
                    Console.WriteLine("nh4[layer] = " + _nh4[layer].ToString() +
                            " less than lower limit of " + nh4_min[layer]);
            }
        }
    }

    [Output]
    [Units("kg/ha")]
    double[] dlt_urea
    {
        get { return null; }
        set
        {
            const double urea_min = 0.0;
            for (int layer = 0; layer < value.Length; ++layer)
            {
                _urea[layer] += value[layer];
                if (_urea[layer] < urea_min)
                    Console.WriteLine("urea[layer] = " + _urea[layer].ToString() +
                            " less than lower limit of " + urea_min);
            }
        }
    }

    [Output]
    string n_reduction
    {
        get { return null; }
        set
        {
            p_n_reduction = value.StartsWith("on");
        }
    }

    [Output]
    [Units("kg/ha")]
    double[] dlt_org_n
    {
        get { return null; }
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
        get { return null; }
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
        get { return null; }
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
        get { return null; }
        set
        {
            for (int layer = 0; layer < value.Length; ++layer)
                fom_c_pool3[layer] += value[layer];
        }
    }

    double[] _nitrification_inhibition;
    [Output]
    double[] nitrification_inhibition
    {
        get { return _nitrification_inhibition; }
        set { _nitrification_inhibition = value; }
    }

    [Output]
    [Description("Carbon balance")]
    double carbonbalance
    {
        get
        {
            double deltaC = TotalC() - dailyInitialC; // Delta storage
            double losses = 0.0;
            int nLayers = dlayer.Length;
            for (int layer = 0; layer < nLayers; layer++)
               losses += _dlt_fom_c_atm[0][layer] +
                         _dlt_fom_c_atm[1][layer] +
                         _dlt_fom_c_atm[2][layer] +
                         dlt_hum_c_atm[layer] +
                         dlt_biom_c_atm[layer] +
                         SumDoubleArray(_dlt_res_c_atm[layer]);
            return - (losses + deltaC);
        }
    }

    [Output]
    [Description("Nitrogen balance")]
    double nitrogenbalance
    {
        get
        {
            double deltaN = TotalN() - dailyInitialN; // Delta storage
            double losses = 0.0;
            int nLayers = dlayer.Length;
            for (int layer = 0; layer < nLayers; layer++)
               losses += dlt_no3_dnit[layer] +
                         dlt_nh4_dnit[layer];
            return - (losses + deltaN);
        }
    }
#endregion

#region Drivers we obtain from other components
    [Input]
    [Units("mm")]
    private float[] sw_dep = null;

    [Input]
    [Units("mm")]
    private float[] dlayer = null;  // Thickness of soil layer (mm)

    [Input]
    private double salb;

    [Input]
    [Units("deg")]
    private double latitude;

    [Input(Optional = true)]
    [Units("t/ha")]
    private double soil_loss = 0.0;

    [Input(Optional = true)]
    private string pond_active = "no";

    [Input(Optional = true)]
    [Units("oC")]
    private double[] ave_soil_temp
    {
        get { return null; }
        set
        {
            st = value;
        }
    }

    [Input(Optional = true)]
    [Units("kg/ha")]
    private double pond_biom_C;

    [Input(Optional = true)]
    [Units("kg/ha")]
    private double pond_hum_C;
#endregion 

#region Events which we publish
    [Event]
    public event ExternalMassFlowDelegate ExternalMassFlow;

    [Event]
    public event NewSoluteDelegate new_solute;

//    [Event] // Not currently used
//    public event ApsimVariantDelegate n_balance;

//    [Event] // Not currently used
//    public event ApsimVariantDelegate c_balance;

    [Event]
    public event SurfaceOrganicMatterDecompDelegate actualresiduedecompositioncalculated;
#endregion

#region Events to which we subscribe, and their handlers
    [EventHandler]
    public void OnInit2()
    {
        GetSiteVariables();
        GetOtherVariables();  // Get information which may vary through time
        ReadParam();
        ReadConstants();
        InitCalc();
        Notification();
        SumReport();
    }


    [EventHandler]
    public void OnProcess() 
    {
        //GetOtherVariables(); // Should occur automatically once we're up and running...
        Process();
//        SendNBalanceEvent();  // Not currently used
//        SendCBalanceEvent();  // Not currently used
        if (pond_active == "no")
            SendActualResidueDecompositionCalculated();
    }

    [EventHandler]
    public void OnReset() 
    {
        SaveState();          // Save state
        ZeroVariables();      // Zero internal state variables
        GetSiteVariables();   // Get information specific to the site
        GetOtherVariables();  // Get information which may vary through time
        ReadParam();          // Get all parameters from parameter file
        ReadConstants();      // Get all coefficients from parameter file
        InitCalc();           // Perform initial calculations from inputs
        DeltaState();         // Change of state
    }

    [EventHandler]
    public void OnSum_report() 
    { 
        SumReport(); 
    }

    [EventHandler]
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
            nSpecified |= IncorpFOM.Layer[i].FOM.N > 0.0;
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

    [EventHandler]
    public void OnTick(TimeType time) 
    {
        DateUtility.JulianDateToDayOfYear(time.startday, out day_of_year, out year);
        
        // Reset Potential Decomposition Register
        num_residues = 0;
        Array.Resize(ref pot_c_decomp, 0);
        Array.Resize(ref pot_n_decomp, 0);
        Array.Resize(ref pot_p_decomp, 0);

        // Calculations for NEW sysbal component
        dailyInitialC = TotalC();
        dailyInitialN = TotalN();
   
    }

    [EventHandler]
    public void OnNewmet(NewMetType NewMet) 
    {
        radn = NewMet.radn;
        maxt = NewMet.maxt;
        mint = NewMet.mint;
    }

    [EventHandler]
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

    [EventHandler]
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

    [EventHandler]
    public void OnNew_profile(NewProfileType NewProfile) 
    {
        ll15_dep = NewProfile.ll15_dep;
        dul_dep  = NewProfile.dul_dep;
        sat_dep  = NewProfile.sat_dep;
        sw_dep   = NewProfile.sw_dep;
        bd       = NewProfile.bd;

        CheckProfile(NewProfile.dlayer);
    }

    [EventHandler]
    public void OnNitrogenChanged(NitrogenChangedType NitrogenChanged) 
    {
        for (int layer = 0; layer < NitrogenChanged.DeltaNO3.Length; ++layer)
        {
            _no3[layer] += NitrogenChanged.DeltaNO3[layer];
            if (_no3[layer] < no3_min[layer])
                Console.WriteLine("no3[layer] = " + _no3[layer].ToString() +
                        " less than lower limit of " + no3_min[layer]);
        }
        for (int layer = 0; layer < NitrogenChanged.DeltaNH4.Length; ++layer)
        {
            _nh4[layer] += NitrogenChanged.DeltaNH4[layer];
            if (_nh4[layer] < nh4_min[layer])
                Console.WriteLine("nh4[layer] = " + _nh4[layer].ToString() +
                        " less than lower limit of " + nh4_min[layer]);
        }
    }

#endregion

#region  Various internal variables
    private double oldN;
    private double oldC;
    private double dailyInitialC;
    private double dailyInitialN;
    private float[] ll15_dep; // lower limit (@15 bar) of soil water content (mm)
    private float[] dul_dep;  // drained upper limit soil water content (mm)
    private float[] sat_dep;  // saturated water content (mm)
    private float[] bd;       // moist bulk density of soil (g/cm^3)
    private bool use_external_st = false;
    private bool use_external_tav_amp = false;
    private bool use_external_ph = false;
    private bool p_n_reduction = false;
    private bool useOrganicSolutes = false;
    private bool soiltypeOverridden;
    private double radn;    // solar radiation (MJ/m^2/day)
    private double maxt;    // maximum air temperature (oC)
    private double mint;    // minimum air temperature (oC)
    private int fom_type;
    private double[] surf_temp = new double[366]; // actual soil surface temperatures (oC)
    private double[] inert_c;   // humic C that is not subject to mineralization (kg/ha)
    private double[] nh4_yesterday; // yesterday's ammonium nitrogen(kg/ha)
    private double[] no3_yesterday; // yesterday's nitrate nitrogen (kg/ha)
    private bool initDone = false;
    private int year;       // year
    private int day_of_year;  // day of year
    private int num_residues = 0;  // number of residues decomposing
    private string[] residue_name; 
    private string[] residue_type;
    private double[] pot_c_decomp; // Potential residue C decomposition (kg/ha)
    private double[] pot_n_decomp; // Potential residue N decomposition (kg/ha)
    private double[] pot_p_decomp; // Potential residue P decomposition (kg/ha)
    private double[][] dlt_res_c_decomp; // residue C decomposition (kg/ha)
    private double[][] dlt_res_n_decomp; // residue N decomposition (kg/ha)
#endregion

#region Useful constants
    private const float c_in_fom = 0.4F; // fraction weight of C in carbohydrate
    private const double ppm = 1000000.0; // factor to convert parts to parts per million
    private const double ppm2fract = 1.0 / 1000000.0;
    private const double pcnt2fract = 1.0 / 100.0;    // convert percent to fraction
    private const double fract2pcnt = 100.0;  // convert fraction to percent
    private const double t2kg = 1000.0; // tonnes to kilograms
#endregion

    public override void Initialised()
    {
        base.Initialised();
        soiltypeOverridden = Override(typeof(SoilType), soiltype);
    }

    private void ReadParam()  // Could do checking of the parameters here....
    {
        Console.WriteLine();
        Console.WriteLine("        - Reading Parameters");
        
        DoubleType val = new DoubleType();
        DoubleArrayType arrayVal = new DoubleArrayType();

        use_external_st = ParentComponent().Get("ave_soil_temp", arrayVal, true) && (arrayVal.Value != null);
        if (use_external_st)
            st = arrayVal.Value;

        else // only need to read these if soil temp is not external
        {
            val.Value = Double.NaN;
            bool use_external_amp = ParentComponent().Get("amp", val, true);
            use_external_amp = use_external_amp && (!Double.IsNaN(val.Value));
            if (use_external_amp)
            {
                amp = val.Value;
                if (amp < 0.0 || amp > 50.0)
                    throw new Exception("External value for amp out of range");
            }
            val.Value = Double.NaN;
            use_external_tav_amp = ParentComponent().Get("tav", val, true);
            use_external_tav_amp = use_external_tav_amp && (!Double.IsNaN(val.Value));
            if (use_external_tav_amp)
            {
                tav = val.Value;
                if (tav < 0.0 || tav > 50.0)
                    throw new Exception("External value for tav out of range");
            }
            if (use_external_tav_amp && !use_external_amp)
                throw new Exception("Default AMP with external TAV not permitted");
            if (!use_external_tav_amp && use_external_amp)
                throw new Exception("External AMP with default TAV not permitted");
        }

        use_external_ph = ParentComponent().Get("ph", arrayVal, true) && (arrayVal.Value != null);

        // Check if all values supplied. If not use average C:N ratio in all pools
        if (root_cn_pool == null || root_cn_pool.Length < 3)
        {
            root_cn_pool = new double[3];
            for (int i = 0; i < 3; i++)
                root_cn_pool[i] = root_cn;
        }

        if (root_depth == 0.0) // if 'root_depth' not provided, assume that 'root_wt' is distributed over whole profile
        {
            for (int i = 0; i < dlayer.Length; ++i)
                root_depth += dlayer[i];
        }

        p_n_reduction = profile_reduction == "on";
        useOrganicSolutes = use_organic_solutes == "on";
    }

    private void ReadConstants()  // Actually, they've already been read, but we'll do some checking
    {
        Console.WriteLine();
        Console.WriteLine("        - Reading Constants");
        if (soiltype != "standard")
        {
            if (soiltypeOverridden)
                Console.WriteLine("     Using soil mineralisation specification for " + soiltype);
            else
                Console.WriteLine("     Using standard soil mineralisation for soil type " + soiltype);
        }
        if (num_fom_types != fract_carb.Length)
            throw new Exception("Number of \"fract_carb\" different to \"fom_type\"");
        if (num_fom_types != fract_cell.Length)
            throw new Exception("Number of \"fract_cell\" different to \"fom_type\"");
        if (num_fom_types != fract_lign.Length)
            throw new Exception("Number of \"fract_lign\" different to \"fom_type\"");
    }

    private void ZeroAllGlobals()
    { // Not sure we really need to do much...
        use_external_st = true;
    }

    private void ZeroVariables()
    { // Wait and see whether this is needed
        use_external_st = true;
    }

    private void SaveState()
    {
        // Calculations for both NEW and OLD sysbal component
        dailyInitialN = oldN = TotalN();
        dailyInitialC = oldC = TotalC();
    }

    private void DeltaState()
    {
        double dltN = TotalN() - oldN;
        double dltC = TotalC() - oldC;

        SendExternalMassFlow(dltN);
        SendExternalMassFlowC(dltC);
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

    private void ResizeLayerArrays(int nLayers)
    {
        Array.Resize(ref st, nLayers);
        Array.Resize(ref _nh4, nLayers);
        Array.Resize(ref _no3, nLayers);
        Array.Resize(ref _urea, nLayers);
        Array.Resize(ref no3_yesterday, nLayers);
        Array.Resize(ref nh4_yesterday, nLayers);
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
        Array.Resize(ref dlt_rntrf, nLayers);
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

    private void InitCalc()
    {
        double ave_temp = (maxt + mint) * 0.5;
        for (int i = 0; i < surf_temp.Length; i++)
            surf_temp[i] = ave_temp;

        int nLayers = dlayer.Length;
        ResizeLayerArrays(nLayers);
        SoilTemp();

        double[] root_distrib = new double[nLayers];
        double cum_depth = 0.0;
        double previous_cum_depth = 0.0;
        double factor;
        int deepest_layer = getCumulativeIndex(root_depth, dlayer);

        for (int i = 0; i <= deepest_layer; i++)
        {
            cum_depth += dlayer[i];
            factor = Math.Min(1.0, MathUtility.Divide(root_depth - previous_cum_depth, dlayer[i], 0.0));
            root_distrib[i] = Math.Exp(-3.0 * Math.Min(1.0, MathUtility.Divide(cum_depth, root_depth, 0.0))) * factor;
            previous_cum_depth = cum_depth;
        }

        double root_distrib_tot = SumDoubleArray(root_distrib);

        for (int layer = 0; layer < nLayers; layer++)
        {
            double convFact = SoilN2Fac(layer);
            nh4_min[layer] = MathUtility.Divide(nh4ppm_min, convFact, 0.0);
            no3_min[layer] = MathUtility.Divide(no3ppm_min, convFact, 0.0);
            _no3[layer] = MathUtility.Divide(_no3ppm[layer], convFact, 0.0);
            if (_no3[layer] < no3_min[layer])
                Console.WriteLine("Attempt to initialise NO3 below lower limit");
            _nh4[layer] = MathUtility.Divide(_nh4ppm[layer], convFact, 0.0);
            if (_nh4[layer] < nh4_min[layer])
                Console.WriteLine("Attempt to initialise NH4 below lower limit");
            if (_ureappm != null)
               _urea[layer] = MathUtility.Divide(_ureappm[layer], convFact, 0.0);

            // calculate total soil C
            double oc_ppm = _oc[layer] * pcnt2fract * ppm;
            double carbon_tot = MathUtility.Divide(oc_ppm, convFact, 0.0);

            // calculate inert soil C
            inert_c[layer] = finert[layer] * carbon_tot;

            // fbiom is ratio of biomass-c to humic-c that is subject
            // to mineralization

            biom_c[layer] = MathUtility.Divide((carbon_tot - inert_c[layer]) * fbiom[layer], 1.0 + fbiom[layer], 0.0);
            biom_n[layer] = MathUtility.Divide(biom_c[layer], mcn, 0.0);

            hum_c[layer] = carbon_tot - biom_c[layer];
            hum_n[layer] = MathUtility.Divide(hum_c[layer], soil_cn, 0.0);

            double fom = MathUtility.Divide(root_wt * root_distrib[layer], root_distrib_tot, 0.0);

            fom_c_pool1[layer] = fom * fract_carb[0] * c_in_fom;
            fom_c_pool2[layer] = fom * fract_cell[0] * c_in_fom;
            fom_c_pool3[layer] = fom * fract_lign[0] * c_in_fom;

            // Calculate the N in each pool in each layer, fom_n_pool(n)[layer]
            fom_n_pool1[layer] = MathUtility.Divide(fom_c_pool1[layer], root_cn_pool[0], 0.0);
            fom_n_pool2[layer] = MathUtility.Divide(fom_c_pool2[layer], root_cn_pool[1], 0.0);
            fom_n_pool3[layer] = MathUtility.Divide(fom_c_pool3[layer], root_cn_pool[2], 0.0);

            fom_n[layer] = fom_n_pool1[layer] + fom_n_pool2[layer] + fom_n_pool3[layer];

            no3_yesterday[layer] = _no3[layer];
            nh4_yesterday[layer] = _nh4[layer];
        }

        // Calculations for NEW sysbal component
        dailyInitialC = TotalC();
        dailyInitialN = TotalN();
   
        initDone = true;
    }

    private void SumReport()
    {
        if (use_external_st)
            Console.WriteLine("           Soil temperature supplied externally");
        if (use_external_tav_amp)
            Console.WriteLine("           TAV and AMP supplied externally");
        if (use_external_ph)
            Console.WriteLine("           Soil pH supplied externally");
        Console.Write(@"

                      Soil Profile Properties
          ------------------------------------------------
           Layer    pH    OC     NO3     NH4    Urea
                         (%) (kg/ha) (kg/ha) (kg/ha)
          ------------------------------------------------
");  
        for (int i = 0; i < dlayer.Length; ++i)
        {
            Console.WriteLine(
"          {0,4:d1}     {1,4:F2}  {2,4:F2}  {3,6:F2}  {4,6:F2}  {5,6:F2}", 
            i + 1,       ph[i],    _oc[i],   _no3[i],  _nh4[i], _urea[i]);
        }
        Console.WriteLine("          ------------------------------------------------");
        Console.WriteLine(
"           Totals              {0,6:F2}  {1,6:F2}  {2,6:F2}",
                  SumDoubleArray(_no3), SumDoubleArray(_nh4), SumDoubleArray(_urea));
        Console.WriteLine("          ------------------------------------------------");
        Console.WriteLine();
        Console.WriteLine();
        Console.WriteLine();
        Console.WriteLine();
        Console.Write(@"
                  Initial Soil Organic Matter Status
          ---------------------------------------------------------
           Layer      Hum-C   Hum-N  Biom-C  Biom-N   FOM-C   FOM-N
                    (kg/ha) (kg/ha) (kg/ha) (kg/ha) (kg/ha) (kg/ha)
          ---------------------------------------------------------
");

        double TotalFomC = 0.0;
        for (int i = 0; i < dlayer.Length; ++i)
        {
            double layerFomC = LayerFomC(i);
            TotalFomC += layerFomC;
            Console.WriteLine(
"          {0,4:d1}   {1,10:F1}{2,8:F1}{3,8:F1}{4,8:F1}{5,8:F1}{6,8:F1}", 
            i+1, hum_c[i], hum_n[i], biom_c[i], biom_n[i], layerFomC, fom_n[i]);
        }
        Console.WriteLine(
"          ---------------------------------------------------------");
        Console.WriteLine(
"           Totals{0,10:F1}{1,8:F1}{2,8:F1}{3,8:F1}{4,8:F1}{5,8:F1}",
            SumDoubleArray(hum_c), SumDoubleArray(hum_n), SumDoubleArray(biom_c),
            SumDoubleArray(biom_n), TotalFomC, SumDoubleArray(fom_n));
        Console.WriteLine(
"          ---------------------------------------------------------");
    }

    private void Process()
    {
    //+  Purpose
    //        This routine performs the soil N balance.
    //
    //        It calculates hydrolysis of g_urea, mineralisation of organic
    //        matter and immobilization of mineral nitrogen due to crop
    //        residue and soil organic matter decomposition, denitrification
    //        and nitrification.

    //+  Mission Statement
    //     Perform all APSIM Timestep calculations

        // update soil temperature
        SoilTemp();

        int nLayers = dlayer.Length;

        if (pond_active == "no")
        {
            // decompose surface residues

            // dsg 010508 If there is no pond, then mineralise residues into top soil layer
            //     as done previously.  If there is a pond, then we need to mineralise directly into the pond water, calculating the
            //     immobilisation demand using mineral N in the pond also.  If 'pond_active' = 'yes' then this will be done in the 'pond' module.
            //     SoilN2 would get some of the N back from the Pond module via a combination of mass flow and adsorption.

            MinResidues(dlt_res_c_decomp, dlt_res_n_decomp,
                  ref _dlt_res_c_biom, ref _dlt_res_c_hum, ref _dlt_res_c_atm, ref dlt_res_nh4_min, ref dlt_res_no3_min);

            for (int layer = 0; layer < nLayers; layer++)
            {
                hum_c[layer] += SumDoubleArray(_dlt_res_c_hum[layer]);
                biom_c[layer] += SumDoubleArray(_dlt_res_c_biom[layer]);
            }

            for (int layer = 0; layer < nLayers; layer++)
            {
                hum_n[layer] = MathUtility.Divide(hum_c[layer], soil_cn, 0.0);
                biom_n[layer] = MathUtility.Divide(biom_c[layer], mcn, 0.0);

                // update soil mineral N

                _nh4[layer] += dlt_res_nh4_min[layer];
                _no3[layer] += dlt_res_no3_min[layer];
            }
        }
        else
        {
            // dsg 190508,  there is a pond, so POND module will decompose residues - not SoilN2
            // dsg 110708   Get the biom & hum C decomposed in the pond and add to soil - on advice of MEP
            DoubleType value = new DoubleType();
            ParentComponent().Get("pond_biom_C", value, false);
            pond_biom_C = value.Value; // biom material from breakdown of residues in pond (if present)
            ParentComponent().Get("pond_hum_C", value, false);
            pond_hum_C = value.Value;  // humic material from breakdown of residues in pond (if present)

            // increment the soiln2 hum and biom C pools in top soil layer
            hum_c[0] += pond_hum_C;
            biom_c[0] += pond_biom_C;

            hum_n[0] = MathUtility.Divide(hum_c[0], soil_cn, 0.0);
            biom_n[0] = MathUtility.Divide(biom_c[0], mcn, 0.0);
        }

        double[,] dlt_fom_n = new double[3, nLayers]; // fom N mineralised in each fraction (kg/ha)
        // now take each layer in turn
        for (int layer = 0; layer < nLayers; layer++)
        {
            // hydrolyse some urea

            double dltUreaHydrol = UreaHydrolysis(layer);

            _nh4[layer] += dltUreaHydrol;
            _urea[layer] -= dltUreaHydrol;

            // denitrification of nitrate-N

            dlt_no3_dnit[layer] = Denitrification(layer);
            _no3[layer] -= dlt_no3_dnit[layer];

            // Calculate transformations of soil organic matter and
            // organic nitrogen.

            // get action from humic pool

            MinHumic(layer, ref dlt_hum_c_biom[layer], ref dlt_hum_c_atm[layer], ref dlt_hum_n_min[layer]);

            // get action from biom pool
 
            MinBiomass(layer, ref dlt_biom_c_hum[layer], ref dlt_biom_c_atm[layer], ref dlt_biom_n_min[layer]);

            // get action from fom pool
            double[] dlt_f_n;
            double[] dlt_fc_biom;
            double[] dlt_fc_hum;
            double[] dlt_fc_atm;
            MinFom (layer, out dlt_fc_biom, out dlt_fc_hum, out dlt_fc_atm, out dlt_f_n, out dlt_fom_n_min[layer]);

            for (int fract = 0; fract < 3; fract++)
            {
                _dlt_fom_c_biom[fract][layer] = dlt_fc_biom[fract];
                _dlt_fom_c_hum[fract][layer] = dlt_fc_hum[fract];
                _dlt_fom_c_atm[fract][layer] = dlt_fc_atm[fract];
//              _dlt_fom_c_biom[fract][layer] = dlt_fc_biom[fract];
                dlt_fom_n[fract, layer] = dlt_f_n[fract];
            }

            // update pools

            hum_c[layer] += dlt_biom_c_hum[layer] - dlt_hum_c_biom[layer] - dlt_hum_c_atm[layer] + 
                           _dlt_fom_c_hum[0][layer] + _dlt_fom_c_hum[1][layer] + _dlt_fom_c_hum[2][layer];

            hum_n[layer] = MathUtility.Divide(hum_c[layer], soil_cn, 0.0);

            biom_c[layer] += dlt_hum_c_biom[layer] - dlt_biom_c_hum[layer] - dlt_biom_c_atm[layer] +
                           _dlt_fom_c_biom[0][layer] + _dlt_fom_c_biom[1][layer] + _dlt_fom_c_biom[2][layer];

            biom_n[layer] = MathUtility.Divide(biom_c[layer], mcn, 0.0);

            fom_c_pool1[layer] -= (_dlt_fom_c_hum[0][layer] + _dlt_fom_c_biom[0][layer] + _dlt_fom_c_atm[0][layer]);
            fom_c_pool2[layer] -= (_dlt_fom_c_hum[1][layer] + _dlt_fom_c_biom[1][layer] + _dlt_fom_c_atm[1][layer]);
            fom_c_pool3[layer] -= (_dlt_fom_c_hum[2][layer] + _dlt_fom_c_biom[2][layer] + _dlt_fom_c_atm[2][layer]);

            // dsg  also perform calculation for n
            fom_n_pool1[layer] -= dlt_fom_n[0, layer];
            fom_n_pool2[layer] -= dlt_fom_n[1, layer];
            fom_n_pool3[layer] -= dlt_fom_n[2, layer];
            fom_n[layer] = fom_n_pool1[layer] + fom_n_pool2[layer] + fom_n_pool3[layer];

            // dsg  these 3 dlts are calculated for the benefit of soilp which needs to 'get' them
            dlt_fom_c_pool1[layer] = _dlt_fom_c_hum[0][layer] + _dlt_fom_c_biom[0][layer] + _dlt_fom_c_atm[0][layer];
            dlt_fom_c_pool2[layer] = _dlt_fom_c_hum[1][layer] + _dlt_fom_c_biom[1][layer] + _dlt_fom_c_atm[1][layer];
            dlt_fom_c_pool3[layer] = _dlt_fom_c_hum[2][layer] + _dlt_fom_c_biom[2][layer] + _dlt_fom_c_atm[2][layer];

            double fom_c = fom_c_pool1[layer] + fom_c_pool2[layer] + fom_c_pool3[layer];

            // dsg    add up fom_n in each layer in each of the pools
            fom_n[layer] = fom_n_pool1[layer] +  fom_n_pool2[layer] + fom_n_pool3[layer];


            // update soil mineral N
            _nh4[layer] += dlt_hum_n_min[layer] + dlt_biom_n_min[layer] + dlt_fom_n_min[layer];

            // now check if too much NH4 immobilized
            double nh4_excess;
            if (_nh4[layer] < nh4_min[layer])
            {
                nh4_excess = nh4_min[layer] - _nh4[layer];
                _nh4[layer] = nh4_min[layer];
            }
            else
            {
                nh4_excess = 0.0;
            }

            _no3[layer] -= nh4_excess;

            // note soiln2_min_fom tests for adequate mineral N for
            // immobilization so that g_NO3 should not go below g_NO3_min

            _no3[layer] = Math.Max(_no3[layer], no3_min[layer]);

	        // nitrification of some ammonium-N

            double dltRntrf = Nitrification(layer);

            _no3[layer] += effective_nitrification[layer];
            _nh4[layer] -= dltRntrf;

            if (_no3[layer] < no3_min[layer] || no3[layer] > 9000.0)
                throw new Exception("Value for NO3(layer) is out of range");
            if (_nh4[layer] < nh4_min[layer] || nh4[layer] > 9000.0)
                throw new Exception("Value for NH4(layer) is out of range");
            if (_urea[layer] < 0.0 || urea[layer] > 9000.0)
                throw new Exception("Value for urea(layer) is out of range");

            nh4_transform_net[layer] = dlt_res_nh4_min[layer] + dlt_fom_n_min[layer] + dlt_biom_n_min[layer] + dlt_hum_n_min[layer] - dltRntrf + dltUreaHydrol + nh4_excess;

            no3_transform_net[layer] = dlt_res_no3_min[layer] - dlt_no3_dnit[layer] + effective_nitrification[layer] - nh4_excess;

            dlt_rntrf[layer]      = dltRntrf;
            dlt_urea_hydrol[layer] = dltUreaHydrol;
            excess_nh4[layer]      = nh4_excess;

            dlt_nh4_net[layer] = _nh4[layer] - nh4_yesterday[layer];
            dlt_no3_net[layer] = _no3[layer] - no3_yesterday[layer];

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
        double n_demand = MathUtility.Divide(SumDoubleArray(dlt_c_biom_tot), mcn, 0.0) +
                          MathUtility.Divide(SumDoubleArray(dlt_c_hum_tot), soil_cn, 0.0);
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
                dlt_no3_min[layer] = - Math.Min(avail_no3[layer], Math.Abs(dlt_n_min));
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

    private double LayerFract(int layer)
    {
        double layerFract = soil_loss * SoilN2Fac(layer) / 1000.0;
        if (layerFract > 1.0)
        {
            int layerNo = layer + 1; // Convert to 1-based index for display
            double layerPercent = layerFract*100.0; // Convert fraction to percentage
            throw new Exception("Soil loss is greater than depth of layer(" + layerNo.ToString() +
                ") by " + layerPercent.ToString() + 
                "%.\nConstrained to this layer. Re-mapping of SoilN pools will be incorrect.");
        }
        return Math.Min(0.0, layerFract);
     }

    private double LayerFomC(int layer)
    {
        return fom_c_pool1[layer] + fom_c_pool2[layer] + fom_c_pool3[layer];
    }

    private double[] FractFomC(int fract)
    {
        switch (fract)
        {
            case 0 : return fom_c_pool1;
            case 1 : return fom_c_pool2;
            case 2 : return fom_c_pool3;
            default : throw new Exception("Coding error: bad fraction in FractFomC");
        }
    }

    private double[] FractFomN(int fract)
    {
        switch (fract)
        {
            case 0 : return fom_n_pool1;
            case 1 : return fom_n_pool2;
            case 2 : return fom_n_pool3;
            default : throw new Exception("Coding error: bad fraction in FractFomN");
        }
    }

    private double[] FractRDFom(int fract)
    {
        switch (fract)
        {
            case 0 : return rd_carb;
            case 1 : return rd_cell;
            case 2 : return rd_lign;
            default : throw new Exception("Coding error: bad fraction in FractRDFom");
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
        double enr = enr_a_coeff * Math.Pow(t2kg * soil_loss, -1.0 * enr_b_coeff);
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
                carbon_tot += LayerFomC(layer) +
                              hum_c[layer] +
                              biom_c[layer];
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
                nitrogen_tot += fom_n[layer] +
                                hum_n[layer] +
                                biom_n[layer] +
                                _no3[layer] +
                                _nh4[layer] +
                                _urea[layer];
            }
        }
        return nitrogen_tot;
    }

    private double SoilN2Fac(int layer)
    {
        if (dlayer == null)
        {
            SingleArrayType dlayer_array = new SingleArrayType();
            ParentComponent().Get("dlayer", dlayer_array, false);
            dlayer = dlayer_array.Value;
        }
        if (bd == null)
        {
            SingleArrayType bd_array = new SingleArrayType();
            ParentComponent().Get("bd", bd_array, false);
            bd = bd_array.Value;
        }
        if (bd == null || dlayer == null || bd.Length == 0 || dlayer.Length == 0) return 0.0;
        // Calculate conversion factor from kg/ha to ppm (mg/kg)
        double ratio = bd[layer] * dlayer[layer];
        return MathUtility.Divide(100.0, ratio, 0.0);
    }

    private void SoilTemp()
    {
        // Calculates average soil temperature at the centre of each layer
        // based on the soil temperature model of EPIC (Williams et al 1984)
        const double days_in_year = 365.25; // no. of days in one year
        const double nth_solst = 173.0;     // day of year of nthrn summer solstice
        const double temp_delay = 27.0;     // delay from solstice to warmest day (days)
        const double nth_hot = nth_solst + temp_delay; // warmest day of year in nth hemisphere
        const double sth_solst = nth_solst + days_in_year / 2.0; // day of year of sthrn summer solstice
        const double sth_hot = sth_solst + temp_delay; // warmest day of year of sth hemisphere
        const double ang = (2.0 * Math.PI) / days_in_year; // length of one day in radians
                            // factor to convert day of year to radian fraction of year

        if (use_external_st)
        {
            // another module is supplying soil temperature
            DoubleArrayType st_array = new DoubleArrayType();
            ParentComponent().Get("ave_soil_temp", st_array, false);
            st = st_array.Value;
        }
        else
        {
            // Get a factor to calculate "normal" soil temperature from the
            // day of year assumed to have the warmest average soil temperature
            // of the year.  The normal soil temperature varies as a cosine
            // function of alx.  This is the number of radians (time) of a
            // year today is from the warmest soil temp.

            // Check for nth/sth hemisphere
            double alx; // time in radians of year from hottest instance to current
                        // day of year as a radian fraction of one year for soil
                        // temperature calculations
            if (latitude >= 0)
                alx = ang * DateUtility.OffsetDayOfYear(year, day_of_year, (int)-nth_hot);
            else
                alx = ang * DateUtility.OffsetDayOfYear(year, day_of_year, (int)-sth_hot);
            if (alx < 0.0 || alx > 6.31)
                throw new Exception("Value for alx is out of range");

            // get change in soil temperature since hottest day, dec c.
            double dlt_temp = SoilTempDt(alx);

            // get temperature dumping depth (mm per radian of a year)
            double damp = SoilTempDampDepth();

            double cum_depth = 0.0;
           // Now get the average soil temperature for each layer.
           // The difference in temperature between surface and subsurface
           // layers ( exp(zd)) is an exponential function of the ratio of
           // the depth to the bottom of the layer and the temperature
           // damping depth of the soil.

            int nLayers = dlayer.Length;
            st = new double[nLayers];
            for (int layer = 0; layer < nLayers; layer++)
            {
                // get the cumulative depth to bottom of current layer
                cum_depth += dlayer[layer];

                // get the lag factor for depth. This reduces changes in
                // soil temperature with depth. (radians of a year)
                double depth_lag = MathUtility.Divide(cum_depth, damp, 0.0);

                // allow subsurface temperature changes to lag behind
                // surface temperature changes
                st[layer] = LayerTemp(depth_lag, alx, dlt_temp);
                if (st[layer] < -20.0 || st[layer] > 80.0)
                    throw new Exception("Value for soil_temp is out of range");
            }
        }
    }

    private double LayerTemp(double depth_lag, double alx, double dlt_temp)
    {
      // Now get the average soil temperature for the layer.
      // The difference in temperature between surface and subsurface
      // layers ( exp(-depth_lag)) is an exponential function of the ratio of
      // the depth to the bottom of the layer and the temperature
      // damping depth of the soil.

        return tav + (amp / 2.0 *  Math.Cos(alx - depth_lag) + dlt_temp) * Math.Exp(-depth_lag);
    }

    private double SoilTempDt(double alx)
    {
    //+  Purpose
    //           Calculates  the rate of change in soil surface temperature
    //           with time.
    //           jngh 24-12-91.  I think this is actually a correction to adjust
    //           today's normal sinusoidal soil surface temperature to the
    //           current temperature conditions.

    //+  Mission Statement
    //     Rate of change in soil surface temperature with time

      // Get today's top layer temp from yesterdays temp and today's
      // weather conditions.
      // The actual soil surface temperature is affected by current
      // weather conditions.
        int yesterday = DateUtility.OffsetDayOfYear(year, day_of_year, -1);
        double ave_temp = (maxt + mint) * 0.5;

        surf_temp[day_of_year - 1] = (1.0 - salb) * (ave_temp + (maxt - ave_temp) *
            Math.Sqrt(radn * 23.8846 / 800.0)) + salb * surf_temp[yesterday - 1];

        // get last few days soil surface temperature for moving average
        const int ndays = 5;
        double[] temp0 = new double[ndays];

        for (int day = 0; day < ndays; day++)
        {
            int doy = DateUtility.OffsetDayOfYear(year, day_of_year, -day);
            temp0[day] = surf_temp[doy - 1];
        }

        double ave_temp0 = SumDoubleArray(temp0) / ndays;

        // Get today's normal surface soil temperature
        // There is no depth lag, being the surface, and there
        // is no adjustment for the current temperature conditions
        // as we want the "normal" sinusoidal temperature for this
        // time of year.

        double temp_a = LayerTemp(0.0, alx, 0.0);

        // Get the rate of change in soil surface temperature with time.
        // This is the difference between a five-day moving average and
        // today's normal surface soil temperature.

        double result = ave_temp0 - temp_a;
        
        // check output

        if (result < -100.0 || result > 100.0)
            throw new Exception("Value for soiln2_SoilTemp_dt is out of range");

        return result;
    }

    private double SoilTempDampDepth()
    {
    //+  Purpose
    //           Now get the temperature damping depth. This is a function of the
    //             average bulk density of the soil and the amount of water above
    //             the lower limit. I think the damping depth units are
    //             mm depth/radian of a g_year

    //+  Notes
    //       241091 consulted Brian Wall.  For soil temperature an estimate of
    //       the water content of the total profile is required, not the plant
    //       extractable soil water.  Hence the method used here - difference
    //       total lower limit and total soil water instead of sum of differences
    //       constrained to and above.  Here the use of lower limit is of no
    //       significance - it is merely a reference point, just as 0.0 could
    //       have been used.  jngh
        const double sw_avail_tot_min = 0.01;

        int nLayers = dlayer.Length;

        // get average bulk density
        double bd_tot = 0.0;
        for (int layer = 0; layer < nLayers; layer++)
            bd_tot += bd[layer] * dlayer[layer];
        double cum_depth = SumFloatArray(dlayer);
        double ave_bd = MathUtility.Divide(bd_tot, cum_depth, 0.0);

        // favbd ranges from almost 0 to almost 1
        // damp_depth_max ranges from 1000 to almost 3500
        // It seems damp_depth_max is the damping depth potential.

        double favbd = ave_bd / (ave_bd + 686.0 * Math.Exp(-5.63 * ave_bd));
        double damp_depth_max = Math.Max(0.0, 1000.0 + 2500.0 * favbd);

        // Potential sw above lower limit - mm water/mm soil depth
        // note that this function says that average bulk density
        // can't go above 2.47222, otherwise potential becomes negative.
        // This function allows potential (ww) to go from 0 to .356

        double ww = Math.Max(0.0, 0.356 - 0.144 * ave_bd);

        // calculate amount of soil water, using lower limit as the
        // reference point.
        double ll_tot = SumFloatArray(ll15_dep);
        double sw_dep_tot = SumFloatArray(sw_dep);
        double sw_avail_tot = Math.Max(sw_dep_tot - ll_tot, sw_avail_tot_min);

        // get fractional water content -

        // wc can range from 0 to 1 while
        // wcf ranges from 1 to 0

        double wc = MathUtility.Divide(sw_avail_tot, ww * cum_depth, 0.0);
        wc = Math.Max(0.0, Math.Min(1.0, wc));
        double wcf = (1.0 - wc) / (1.0 + wc);

        // Here b can range from -.69314 to -1.94575
        // and f ranges from 1 to  0.142878
        // When wc is 0, wcf=1 and f=500/damp_depth_max
        // and soiln2_SoilTemp_DampDepth=500
        // When wc is 1, wcf=0 and f=1
        // and soiln2_SoilTemp_DampDepth=damp_depth_max
        // and that damp_depth_max is the maximum.

        double b = Math.Log(MathUtility.Divide(500.0, damp_depth_max, 1.0e10));
        double f = Math.Exp(b * wcf * wcf);
        // Get the temperature damping depth. (mm soil/radian of a g_year)
        // discount the potential damping depth by the soil water deficit.
        // Here soiln2_SoilTemp_DampDepth ranges from 500 to almost
        // 3500 mm/58 days.
        return f * damp_depth_max;
    }

    private double UreaHydrolysis(int layer)
    {
    //mep  urea is hydrolysed to ammonium_N

    //+  Purpose
    //       Hydrolyse g_urea.
    //       NOTE - not tested as not in old code - from CM V2.

    //+  Mission Statement
    //     Hydrolyse urea in %1
        
    // dsg 200508  use different values for some constants when anaerobic conditions dominate
        double result;
        int index = (pond_active == "no") ? 1 : 2;

        if (_urea[layer] > 0.0)
        {
        // do some g%urea hydrolysis.
            if (_urea[layer] < 0.1)
                result = _urea[layer];
            else
            {
                // get soil water factor
                double swf = Math.Max(0.0, Math.Min(1.0, 
                    WF(layer, index) + 0.20));

                // get soil temperature factor
                double tf = Math.Max(0.0, Math.Min(1.0, 
                    (st[layer] / 40.0) + 0.20));

                // get potential fraction of urea for hydrolysis
                // note (jngh) oc & ph are not updated during simulation

                //mep    following equation would be better written in terms of hum_C and biom_C
                //mep    oc(layer) = (hum_C(layer) + biom_C(layer))*soiln2_fac (layer)*10000.

                double ak = Math.Max(0.25, Math.Min(1.0,
                    -1.12 + 1.31 * _oc[layer] + 0.203 * ph[layer] - 0.155 * _oc[layer] * ph[layer]));
                
                //get amount hydrolysed;
                result = Math.Max(0.0, Math.Min(_urea[layer],
                    ak * _urea[layer] * Math.Min(swf, tf)));
            }
        }
        else
            result = 0.0;
        return result;
    }

    private void MinHumic(int layer, ref double dlt_c_biom, ref double dlt_c_atm, ref double dlt_n_min)
    {
    //+  Sub-Program Arguments
    // index                 // index - 1 for aerobic and 2 for anaerobic conditions
    // layer                 // (INPUT) layer count
    // dlt_n_min             // (OUTPUT) net humic N mineralized
                             // (kg/ha)
    // dlt_c_biom            // (OUTPUT) carbon to biomass pool (kg/ha)
    // dlt_c_atm             // (OUTPUT) carbon to atmosphere (kg/ha)

    //+  Purpose
    //       Mineralise some humic material.  Calculates the
    //       daily rate of decomposition and net nitrogen mineralisation from
    //       the humic pool.

    //+  Assumptions
    //       There is an g_inert_C component of the humic pool that is not
    //       subject to mineralization

    //+  Notes
    //       Net mineralisation can be negative if
    //         soil_CN > mCN/ef_hum

    //+  Mission Statement
    //     Calculate the humic rate of decomposition and nitrogen mineralisation

        // dsg 200508  use different values for some constants when there's a pond and anaerobic conditions dominate
        int index = (pond_active == "no") ? 1 : 2;
        
        double tf = (soiltype == "rothc") ? RothcTF (layer, index) : TF(layer, index);
        double mf = WF(layer, index);

        // get the rate of mineralization of N from the humic pool

        double dlt_c_min_tot = (hum_c[layer] - inert_c[layer])* rd_hum[index - 1] * tf * mf;
        double dlt_n_min_tot = MathUtility.Divide(dlt_c_min_tot, soil_cn, 0.0);

        dlt_c_biom = dlt_c_min_tot * ef_hum;
        dlt_c_atm = dlt_c_min_tot *(1.0 - ef_hum);
        dlt_n_min = dlt_n_min_tot - MathUtility.Divide (dlt_c_biom, mcn, 0.0);
    }

    private void MinBiomass(int layer, ref double dlt_c_hum, ref double dlt_c_atm, ref double dlt_n_min)
    {
    //+  Sub-Program Arguments
    // layer           // (INPUT) layer count
    // dlt_n_min       // (OUTPUT) net biomass N mineralized (kg/ha)
    // dlt_c_hum       // (OUTPUT) carbon to humic pool (kg/ha)
    // dlt_c_atm       // (OUTPUT) carbon to atmosphere (kg/ha)

    //+  Purpose
    //       Mineralise some soil biomass material.  Calculates the
    //       daily rate of decomposition and net nitrogen mineralisation
    //       from the biomass pool

    //+  Mission Statement
    //     Calculate the biomass rate of decomposition and nitrogen mineralisation

        // dsg 200508  use different values for some constants when anaerobic conditions dominate
        int index = (pond_active == "no") ? 1 : 2;
        
        double tf = (soiltype == "rothc") ? RothcTF (layer, index) : TF(layer, index);
        double mf = WF(layer, index);

        // get the rate of mineralization of C & N from the biomass pool

        double dlt_n_min_tot = biom_n[layer] * rd_biom[index - 1] * tf * mf;
        double dlt_c_min_tot = dlt_n_min_tot * mcn;

        dlt_c_hum = dlt_c_min_tot * ef_biom * (1.0 - fr_biom_biom);
        dlt_c_atm = dlt_c_min_tot * (1.0 - ef_biom);

        // calculate net mineralization

        dlt_n_min = dlt_n_min_tot - MathUtility.Divide(dlt_c_hum, soil_cn, 0.0) - 
                                    MathUtility.Divide((dlt_c_min_tot - dlt_c_atm - dlt_c_hum), mcn, 0.0);
    }

    private void MinFom(int layer, out double[] dlt_c_biom, out double[] dlt_c_hum,
                        out double[] dlt_c_atm, out double[] dlt_fom_n, out double dlt_n_min)
    {
        //+  Sub-Program Arguments
        // layer           // (INPUT) soil layer number
        // dlt_c_atm (*)   // (OUTPUT) carbon to atmosphere (kg/ha)
        // dlt_c_biom (*)  // (OUTPUT) carbon to biomass (kg/ha)
        // dlt_c_hum (*)   // (OUTPUT) carbon to humic (kg/ha)
        // dlt_fom_n(*)    // (OUTPUT) amount of N mineralized (kg/ha)
        //  from each pool
        // dlt_n_min       // (OUTPUT) net N mineralized (kg/ha)

        //+  Purpose
        //       Mineralise some fresh organic matter.
        //       Calculates the daily rate of decomposition, partitions the carbon
        //       in to soil pools and loss to atmosphere,
        //       and the net nitrogen mineralisation (negative if N immobilized) from
        //       the fresh organic matter pool.

        //+  Mission Statement
        //     Calculate the fresh organic matter rate of decomposition and nitrogen mineralisation

        dlt_c_hum = new double[3];
        dlt_c_biom = new double[3];
        dlt_c_atm = new double[3];
        dlt_fom_n = new double[3];
        dlt_n_min = 0.0;

        // dsg 200508  use different values for some constants when anaerobic conditions dominate
        // index = 1 for aerobic conditions, 2 for anaerobic conditions
        int index = (pond_active == "no") ? 1 : 2;

        // get total available minernal N (kg/ha)
        double nitTot = Math.Max(0.0, (_no3[layer] - no3_min[layer]) + (_nh4[layer] - nh4_min[layer]));

        // fresh organic carbon (kg/ha)
        double fomC = LayerFomC(layer);

        // fresh organic nitrogen (kg/ha)
        // dsg   the fom nitrogen must now also be totalled
        double fomN = fom_n_pool1[layer] + fom_n_pool2[layer] + fom_n_pool3[layer];

        // ratio C in fresh OM to N available for decay
        // calculate a C:N ratio that includes mineral-N in the lay
        double cnr = MathUtility.Divide(fomC, fomN + nitTot, 0.0);

        // calculate the C:N ratio factor
        double cnrf = Math.Max(0.0, Math.Min(1.0, // Bound to [0, 1]
             Math.Exp(-cnrf_coeff * (cnr - cnrf_optcn) / cnrf_optcn)));

        // get temperature & moisture factors for the layer
        double tf = (soiltype == "rothc") ? RothcTF (layer, index) : TF(layer, index);
        double mf = WF(layer, index);

        // calculate gross amount of C & N released due to mineralization
        // of the fresh organic matter.

        if (fomC >= fom_min)
        {
            // take the decomposition of carbohydrate-like,
            // cellulose-like and lignin-like fractions (fpools)
            // of the residue in turn.

            double dlt_fom_n_min_tot = 0.0; // amount of fresh organic N mineralized across fpools (kg/ha)
            double dlt_fom_c_min_tot = 0.0; // total C mineralized (kg/ha) summed across fpools
            double[] dlt_n_min_tot = new double[3]; // amount of fresh organic N mineralized in each pool (kg/ha)
            double[] dlt_c_min_tot = new double[3]; // amount of C mineralized (kg/ha) from each pool

            double fom_cn = MathUtility.Divide(fomC, fomN, 0.0); // CN ratio of fom pool

            for (int fractn = 0; fractn < 3; fractn++)
            {
                double drate = FractRDFom(fractn)[index - 1] * cnrf * tf * mf;

                // calculate the amounts of carbon and nitrogen mineralized
                double grcm = drate * FractFomC(fractn)[layer]; // gross amount of fresh organic carbon mineralized (kg/ha)
                // dsg calculate grnm using same method as grcm
                double grnm = drate * FractFomN(fractn)[layer]; // gross amount of N released from fresh organic matter (kg/ha)

                dlt_fom_n_min_tot += grnm;
                dlt_c_min_tot[fractn] = grcm;
                dlt_n_min_tot[fractn] = grnm;
                dlt_fom_c_min_tot += grcm;
            }

            // calculate potential transfers to biom and humic pools

            double dlt_c_biom_tot = dlt_fom_c_min_tot * ef_fom * fr_fom_biom; // C mineralized converted to biomass
            double dlt_c_hum_tot = dlt_fom_c_min_tot * ef_fom* (1.0 - fr_fom_biom); // C mineralized converted to humic

            // test whether adequate N available to meet immobilization demand
            double n_demand = MathUtility.Divide(dlt_c_biom_tot, mcn, 0.0) + 
                              MathUtility.Divide(dlt_c_hum_tot, soil_cn, 0.0);
            double n_avail = nitTot + dlt_fom_n_min_tot;

            double scale_of; // factor to reduce mineralization rates if insufficient N to meet immobilization demand
            if (n_demand > n_avail)
                // rate of mineralization must be scaled back so that immobilization can be satisfied
                scale_of = Math.Max(0.0, Math.Min(1.0, // Bound to [0, 1]
                    MathUtility.Divide(nitTot, n_demand - dlt_fom_n_min_tot, 0.0)));
            else
                scale_of = 1.0;   // supply exceeds demand

            // now adjust carbon transformations etc.
            // dsg   and similarly for npools

            for (int fractn = 0; fractn < 3; fractn++)
            {
                dlt_c_hum[fractn] = dlt_c_min_tot[fractn] * ef_fom * (1.0 - fr_fom_biom) * scale_of;
                dlt_c_biom[fractn] = dlt_c_min_tot[fractn] * ef_fom * fr_fom_biom * scale_of;
                dlt_c_atm[fractn] = dlt_c_min_tot[fractn] * (1.0 - ef_fom)* scale_of;
                dlt_fom_n[fractn] = dlt_n_min_tot[fractn] * scale_of;

                dlt_c_hum[fractn] = MathUtility.RoundToZero(dlt_c_hum[fractn]);
                dlt_c_biom[fractn] = MathUtility.RoundToZero(dlt_c_biom[fractn]);
                dlt_c_atm[fractn] = MathUtility.RoundToZero(dlt_c_atm[fractn]);
                dlt_fom_n[fractn] = MathUtility.RoundToZero(dlt_fom_n[fractn]);
            }

            dlt_n_min = (dlt_fom_n_min_tot - n_demand) * scale_of;
        }
    }

    private double pHFNitrf(int layer)
    {
        //+  Purpose
        //       Calculates a 0-1 pH factor for nitrification.

        //+  Assumptions
        //       1 < layer < num_layers

        //+  Mission Statement
        //     Calculate pH factor for nitrification
        bool DidInterpolate;
        return MathUtility.LinearInterpReal(ph[layer], pHf_nit_pH, pHf_nit_values, out DidInterpolate);
    }

    private double Nitrification(int layer)
    {
    //+  Sub-Program Arguments
    // layer                 // (INPUT) soil layer count
    // dlt_rntrf             // (OUTPUT) actual rate of nitrification
                             //    (kg/ha)

    //+  Purpose
    //           Calculates nitrification of NH4 in a given soil layer.

    //+  Notes
    //        This routine is much simplified from original CERES code
    //        g_ph effect on nitrification is not invoked

    //+  Mission Statement
    //     Calculate nitrification of NH4 in %1

    //+  Local Variables
        int    index;                 // index - 1 for aerobic and 2 for anaerobic conditions
        double opt_rate_ppm;          // rate of nitrification
                                      // under optimum conditions (ppm)
        double opt_rate;              // rate of nitrification
                                      // under optimum conditions (kg/ha)
        double phf;                   // g_ph factor
        double pni;                   // potential nitrification index (0-1)
        double nh4_avail;             // available ammonium (kg/ha)
        double nh4ppm;                // ammonium in soil (ppm)
        double tf;                    // temperature factor (0-1)
        double wfd;                   // water factor (0-1)

   // dsg 200508  use different values for some constants when anaerobic conditions dominate
        index = (pond_active == "no") ? 1 : 2;

        phf = pHFNitrf(layer);
       // get a 0-1 water factor for nitrification
        wfd = WFNitrf(layer, index);

       // get a 0-1 temperature factor from soil temperature
        tf = TF(layer, index);

       // use a combined index to adjust rate of nitrification
       // NOTE phn removed to match CERES v1
        pni = Math.Min(wfd, Math.Min(tf, phf));

       // get actual rate of nitrification for layer
        double convFact = SoilN2Fac(layer);
        nh4ppm = _nh4[layer] * convFact;
        opt_rate_ppm = MathUtility.Divide(nitrification_pot * nh4ppm, nh4ppm + nh4_at_half_pot, 0.0);
        opt_rate = MathUtility.Divide(opt_rate_ppm, convFact, 0.0);

       //-----Changes by VOS 13 Dec 09, Reviewed by RCichota (9/02/2010)----------------
        opt_rate = MathUtility.Divide(opt_rate_ppm, convFact, 0.0) * Math.Max(0.0, 1.0 - _nitrification_inhibition[layer]);
       // old code-> opt_rate = divide(opt_rate_ppm,soiln2_fac(layer),0.0)
       //--------------------------------------------------------------------------------

        double result = pni * opt_rate;
        nh4_avail = Math.Max(_nh4[layer] - nh4_min[layer], 0.0);
        result = Math.Max(0.0, Math.Min(nh4_avail, result));

        dlt_nh4_dnit[layer] = result * dnit_nitrf_loss;
        effective_nitrification[layer] = result - dlt_nh4_dnit[layer];
        n2o_atm[layer] += dlt_nh4_dnit[layer];

        return result;
    }    

    private double Denitrification(int layer)
    {
    //+  Sub-Program Arguments
    //      dlt_n_atm             // (OUTPUT) denitrification rate
                                  //    - kg/ha/day
    //      layer                 // (INPUT) soil layer counter

    //+  Purpose
    //           Calculates denitrification whenever the soil water in the
    //           layer > the drained upper limit (Godwin et al., 1984),
    //           the NO3 nitrogen concentration > 1 mg N/kg soil,
    //           and the soil temperature >= a minimum temperature.
    //           NOTE denitrification routine not validated

    //+  Assumptions
    //       That there is a root system present.  Rolston et al. say that the
    //       denitrification rate coeffficient (dnit_rate_coeff) of non-cropped
    //       plots was 0.000168 and for cropped plots 3.6 times more
    //       (dnit_rate_coeff = 0.0006). The larger rate coefficient was required
    //       to account for the effects of the root system in consuming oxygen
    //       and in adding soluble organic C to the soil.

    //+  Notes
    //       Reference: Rolston DE, Rao PSC, Davidson JM, Jessup RE.
    //       "Simulation of denitrification losses of Nitrate fertiliser applied
    //        to uncropped, cropped, and manure-amended field plots".
    //        Soil Science April 1984 Vol 137, No 4, pp 270-278.
    //
    //       Reference for Carbon availability factor -
    //       Reddy KR, Khaleel R, Overcash MR. "Carbon transformations in land
    //       areas receiving organic wastes in relation to nonpoint source
    //       pollution: A conceptual model".  J.Environ. Qual. 9:434-442.

    //+  Mission Statement
    //     Calculate denitrification in %1

        double active_c;              // water extractable organic carbon as
                                      // "available" C conc. (mg C/kg soil)
        double tf;                    // temperature factor affecting
                                      //    denitrification rate (0-1)
        double wf;                    // soil moisture factor affecting
                                      //    denitrification rate (0-1)
        double no3_avail;             // soil nitrate available (kg/ha)
        double hum_c_conc;            // carbon conc. of humic pool
                                      //    (mg C/kg soil)
        double fom_c_conc;            // carbon conc. of fresh organic pool
                                      //    (mg C/kg soil)

        if (_no3[layer] < no3_min[layer])
        {
            n2o_atm[layer] = 0.0;
            return 0.0;
        }

        hum_c_conc = hum_c[layer] * SoilN2Fac(layer);
        fom_c_conc = LayerFomC(layer) * SoilN2Fac(layer);

      // get available carbon concentration from soil organic
      // carbon concentration (24.5+)

      // Note CM V2 had active_c = fom_C_conc + 0.0031*hum_C_conc + 24.5
        active_c = 0.0031 * (fom_c_conc + hum_c_conc) + 24.5;

      // Get water factor (0-1)
        wf = WFDenit(layer);

      // get temperature factor from soil temperature (0-1)
      // This is an empirical dimensionless function to account for
      // the effect of temperature.
      // The upper limit of 1.0 means that optimum denitrification
      // temperature is 50 oC and above.  At 0 oC it is 0.1 of optimum,
      // and at -20 oC is about 0.04.
        tf = 0.1* Math.Exp(0.046 * st[layer]);
        tf = Math.Max(0.0, Math.Min(1.0, tf));

      // calculate denitrification rate  - kg/ha
        double result = dnit_rate_coeff * active_c * wf * tf * _no3[layer];

      // prevent NO3 - N concentration from falling below NO3_min
        no3_avail = _no3[layer] - no3_min[layer];
        result = Math.Max(0.0, Math.Min(no3_avail, result));

        double WFPS = sw_dep[layer] / sat_dep[layer] * 100.0; // Water filled pore space (%)
        double CO2 = (_dlt_fom_c_atm[0][layer] + _dlt_fom_c_atm[1][layer] +_dlt_fom_c_atm[2][layer] +
                      dlt_biom_c_atm[layer] + dlt_hum_c_atm[layer]) /
                      (bd[layer] * dlayer[layer]) * 100.0;
        double RtermA = 0.16 * dnit_k1;
        double RtermB = (CO2 > 0.0) ?
             dnit_k1 * (Math.Exp(-0.8 * (_no3[layer] * SoilN2Fac(layer) / CO2))) 
             : 0.0;
        double RtermC = 0.1;
        bool didInterpolate;
        double RtermD = MathUtility.LinearInterpReal(WFPS, dnit_wfps, dnit_n2o_factor, out didInterpolate);
        // RTermD = (0.015 * WFPS) - 0.32;

        double N2N2O = Math.Max(RtermA, RtermB) * Math.Max(RtermC, RtermD);
        n2o_atm[layer] = result / (N2N2O + 1.0);

        return result;
    }

    private double WFNitrf(int layer, int index)
    {
        // index = 1 for aerobic conditions, 2 for anaerobic
        //+  Purpose
        //       Calculates a 0-1 water factor for nitrification.

        //+  Assumptions
        //       0 < layer < num_layers - 1

        //+  Mission Statement
        //     Water factor for nitrification in %1

        double wfd = 1.0; // temporary water factor (0-1)
        if (sw_dep[layer] > dul_dep[layer] && sat_dep[layer] > dul_dep[layer])
        {   // saturated
            wfd = 1.0 + (sw_dep[layer] - dul_dep[layer]) / (sat_dep[layer] - dul_dep[layer]);
            wfd = Math.Max(1.0, Math.Min(2.0, wfd));
        }
        else
        {
            // unsaturated
            // assumes rate of mineralization is at optimum rate
            // until soil moisture midway between dul and ll15
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
    //+  Purpose
    //       Calculates a 0-1 water factor for denitrification

    //+  Assumptions
    //       0 < layer < num_layers - 1
    
    //+  Mission Statement
    //     Water factor for denitrification in %1

        double wfd = 0.0; // temporary water factor (0-1); 0 is used if unsaturated
        if (sw_dep[layer] > dul_dep[layer] && sat_dep[layer] > dul_dep[layer])  // saturated
            wfd = Math.Pow((sw_dep[layer] - dul_dep[layer]) / (sat_dep[layer] - dul_dep[layer]), 
                            dnit_wf_power);
        return Math.Max(0.0, Math.Min(1.0, wfd));
    }

    private double WF(int layer, int index)
    {
    // index = 1 for aerobic conditions, 2 for anaerobic
    //+  Purpose
    //       Calculates a 0-1 water factor for mineralisation.

    //+  Assumptions
    //       0 < layer < num_layers - 1

    //+  Mission Statement
    //     Water factor for mineralisation in %1

        double wfd;  // temporary water factor (0-1)

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
        // assumes rate of mineralization is at optimum rate
        // until soil moisture midway between dul and ll15
            if (dul_dep[layer] == ll15_dep[layer])
                wfd = 0.0;
            else
                wfd = Math.Max(0.0, Math.Min(1.0,
                    (sw_dep[layer] - ll15_dep[layer]) / (dul_dep[layer] - ll15_dep[layer])));
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
    // index = 1 for aerobic conditions, 2 for anaerobic
    //+  Purpose
    //       Calculate a temperature factor, based on the soil temperature
    //       of the layer, for nitrification and mineralisation

    //+  Notes
    //           - the layer l < 1 or > num_layers
    //           - the soil temperature falls outside of lower to upper

    //+  Mission Statement
    //     Nitrification and mineralisation soil temperature factor in %1

        // Alternate version from CM
        //      tf = (soil_temp[layer] - 5.0) /30.0
        // because tf is bound between 0 and 1, the effective
        // temperature (soil_temp) lies between 5 to 35.

        // alternative quadratic temperature function is preferred
        //  with optimum temperature (CM - used 32 deg)

        if (st[layer] > 0.0)
        {
            if (opt_temp[index - 1] == 0.0)
                return 0.0;
            else
                return Math.Max(0.0, Math.Min(1.0,
                    (st[layer] * st[layer]) / Math.Pow(opt_temp[index - 1], 2.0)));
        }
        else // soil is too cold for mineralisation
            return 0.0;
    }

    private double RothcTF(int layer, int index)
    {
        // index = 1 for aerobic conditions, 2 for anaerobic
        //+  Purpose
        //       Calculate a temperature factor, based on the soil temperature
        //       of the layer, for nitrification and mineralisation

        //+  Notes
        //           - the layer l < 1 or > num_layers
        //           - the soil temperature falls outside of lower to upper

        //+  Mission Statement
        //     Nitrification and mineralisation soil temperature factor in %1
        double t = Math.Min(st[layer], opt_temp[layer]);
        return 47.9 / (1.0 + Math.Exp(106.0 / (t + 18.3)));
    }

    private void GetSiteVariables()
    { // Not really needed. This will happen automagically once we're up and running,
      // but this helps ensure we'll have access to the values during init2
        SingleType value = new SingleType();
        ParentComponent().Get("latitude", value, false);
        latitude = value.Value;
        ParentComponent().Get("salb", value, false);
        salb = value.Value;
    }

    private void GetOtherVariables()
    {
        SingleArrayType sw_dep_array = new SingleArrayType();
        ParentComponent().Get("sw_dep", sw_dep_array, false);
        sw_dep = sw_dep_array.Value;
        SingleArrayType dlayer_array = new SingleArrayType();
        ParentComponent().Get("dlayer", dlayer_array, false);
        dlayer = dlayer_array.Value;

        if (p_n_reduction) // ONLY need soil loss if profile reduction is on
        {
            DoubleType value = new DoubleType();
            value.Value = Double.NaN;
            Boolean found = ParentComponent().Get("soil_loss", value, true);
            if (found && (!Double.IsNaN(value.Value)))
              soil_loss = value.Value;
        }
        
        if (use_external_ph)
        {
            DoubleArrayType value = new DoubleArrayType();
            Boolean found = ParentComponent().Get("ph", value, true);
            if (found && (value.Value != null))
              ph = value.Value;
        }
        CheckPond();
    }

    private void CheckPond()
    {
        StringType stVal = new StringType();
        Boolean found = ParentComponent().Get("pond_active", stVal, true);
        if (!found || (stVal.Value == null))
            pond_active = "no";
    }

    private void SendExternalMassFlow(double dltN)
    {
        ExternalMassFlowType massBalanceChange = new ExternalMassFlowType();
        massBalanceChange.FlowType = dltN >= 0 ? "gain" : "loss";
        massBalanceChange.PoolClass = "soil";
        massBalanceChange.N = (float)Math.Abs(dltN);
        ExternalMassFlow.Invoke(massBalanceChange);
    }

    private void SendExternalMassFlowC(double dltC)
    {
        ExternalMassFlowType massBalanceChange = new ExternalMassFlowType();
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

/*
    private void SendNBalanceEvent()
    {
        // This event was sent from the Fortran version, but nothing anywhere ever subscribed
        // to it. If it's ever used, it should be revised to not use Apsim variants.
        // It transmitted four arrays (elements correspond to layers) of values:
        //    nh4_transform_net
        //    no3_transform_net
        //    dlt_nh4_net
        //    dlt_no3_net

    }

    private void SendCBalanceEvent()
    {
        // This event was sent from the Fortran version, but nothing anywhere ever subscribed
        // to it. If it's ever used, it should be revised to not use Apsim variants.
        // It transmitted two arrays  (elements correspond to layers) of values:
        //    dlt_oc
        //    dlt_om
  
    }
*/ 

    private void Notification()
    {
    //+  Purpose
    //      Notify all interested modules about this module's ownership
    //      of solute information.

    //+  Mission Statement
    //     Notify other modules of ownership of solute information
        if (new_solute != null)
        {
            string[] solute_names = new string[7] {
                "no3",
                "nh4",
                "urea",
                "org_c_pool1",
                "org_c_pool2",
                "org_c_pool3",
                "org_n"
            };

            if (!useOrganicSolutes)  // don't publish the organic solutes
                Array.Resize(ref solute_names, 3);

            NewSoluteType data = new NewSoluteType();
            data.sender_id = (int)ParentComponent().GetId();
            data.solutes = solute_names;

          
            new_solute.Invoke(data);
        }
    }

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
}

public class SoilType : DerivedInstance
{
}