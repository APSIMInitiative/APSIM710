using System;
using System.Collections.Generic;
using System.Reflection;
using System.Text;
using System.Linq;
using ModelFramework;
using CSGeneral;
using System.Xml;

class soilCNPatch
{

    public double PatchArea = 1.0;
    public string PatchName = "base";

    public soilCNPatch()
    { }

    #region Parameters and inputs provided by the user or APSIM

    #region Parameters added by RCichota

    // marker for what set of functions will be used (original or new)
    public bool useNewFunctions = false;

    #endregion

    #region Parameters used on initialisation only

    #region General setting parameters

    // soil model type, spec used to determine some mineralisation model variations
    public string SoilParamSet = "standard";

    // switch indicating whether soil profile reduction is allowed (from erosion)
    public bool AllowProfileReduction = false;

    // marker for whether organic solute are to be simulated (always false as it is not implemented)
    public bool useOrganicSolutes = false;

    // minimum allowable Urea content (ppm), per layer
    public double[] urea_min;

    // minimum allowable NH4 content (ppm), per layer
    public double[] nh4_min;

    // minimum allowable NO3 content (ppm), per layer
    public double[] no3_min;

    // minimum allowable FOM content (kg/ha), per layer
    public double fom_min;

    // conversion from OC to OM
    public double oc2om_factor;

    // weight fraction of C in carbohydrates
    public double c_in_fom = 0.40;

    // value to evaluate precision
    public double epsilon = 0.0;

    #endregion

    #region Parameters for handling soil loss process

    // enrichment equation coefficient a
    public double enr_a_coeff = 0.0;

    // enrichment equation coefficient b
    public double enr_b_coeff = 0.0;

    #endregion

    #region Parameters for setting soil organic matter

    // the C:N ratio of soil humus
    public double hum_cn = 0.0;

    // the C:N ratio of microbial biomass
    public double biom_cn = 8.0;

    // initial ratio of biomass-C to mineralizable humic-C (0-1)
    public double[] fbiom = null;

    // initial proportion of total soil C that is not subject to mineralization (0-1)
    public double[] finert = null;

    #endregion

    #region Parameters for setting fresh organic matter (FOM)

    // initial weight of fom in the soil (kgDM/ha)
    public double fom_ini_wt = 0.0;

    // initial depth over which fom is distributed within the soil profile (mm)
    public double fom_ini_depth = 0.0;

    // list of fom types
    public String[] fom_types;

    // initial C:N ratio FOM pools
    public double[] fomPools_cn = null;

    // fraction of carbohydrate in FOM (0-1), for each FOM type
    public double[] fract_carb;

    // fraction of cellulose in FOM (0-1), for each FOM type
    public double[] fract_cell;

    // fraction of lignin in FOM (0-1), for each FOM type
    public double[] fract_lign;

    #endregion

    #region Parameters for FOM and SurfOM mineralisation process

    #region Surface OM

    // fraction of residue C mineralised retained in system (0-1)
    public double ef_res;

    // fraction of retained residue C transferred to biomass (0-1)
    public double fr_res_biom;

    // depth from which mineral N can be immobilised by decomposing residues (mm)
    public double min_depth;

    #endregion

    #region Fresh OM

    #region Old parameters

    public double[] rd_carb;            // maximum rate constants for decomposition of FOM pools [carbohydrate component] (0-1)

    public double[] rd_cell;            // maximum rate constants for decomposition of FOM pools [cellulose component] (0-1)

    public double[] rd_lign;            // maximum rate constants for decomposition of FOM pools [lignin component] (0-1)

    public double ef_fom;               // fraction of FOM C mineralized retained in system (0-1)   

    public double fr_fom_biom;          // fraction of retained FOM C transferred to biomass (0-1)

    public double cnrf_coeff;           // coeff. to determine the magnitude of C:N effects on decomposition of FOM ()

    public double cnrf_optcn;           // C:N above which decomposition rate of FOM declines ()

    #endregion

    #region New parameters
    
    // parameters for temperature factor for FOM mineralisation
    public TempFactorData TempFactor_minerFOM = new TempFactorData();

    // parameters for soil moisture factor for FOM mineralisation
    public XYData MoistFactor_minerFOM = new XYData();

    // parameters for C:N factor for OM mineralisation
    public double CNFactorMinerFOM_OptCN;
    public double CNFactorMinerFOM_RateCN;

    #endregion

    #endregion

    #endregion

    #region Parameters for SOM mineralisation/immobilisation process

    #region Old parameters

    public double[] rd_biom = null;     // potential rate of soil biomass mineralization (per day)

    public double ef_biom;              // fraction of biomass C mineralized retained in system (0-1)

    public double fr_biom_biom;         // fraction of retained biomass C returned to biomass (0-1)

    public double[] rd_hum = null;      // potential rate of humus mineralization (per day)

    public double ef_hum;               // fraction of humic C mineralized retained in system (0-1)

    public double[] opt_temp;           // Soil temperature above which there is no further effect on mineralisation and nitrification (oC)

    public double[] wfmin_index;        // index specifying water content for water factor for mineralization

    public double[] wfmin_values;       // value of water factor(mineralization) function at given index values

    #endregion

    #region New parameters

        // whether mineralisation factors are computed single or for each type
    public bool useSingleMinerFactors = true;

    // parameters for temperature factor for OM mineralisation
    public TempFactorData TempFactor_Miner = new TempFactorData();

    // parameters for soil moisture factor for OM mineralisation
    public XYData MoistFactor_Miner = new XYData();

    #region parameters for each OM type

    // parameters for temperature factor for humus mineralisation
    public TempFactorData TempFactor_minerHum = new TempFactorData();

    // parameters for soil moisture factor for humus mineralisation
    public XYData MoistFactor_minerHum = new XYData();

    // parameters for temperature factor for OM biomass mineralisation
    public TempFactorData TempFactor_minerBiom = new TempFactorData();

    // parameters for soil moisture factor for OM biomass mineralisation
    public XYData MoistFactor_minerBiom = new XYData();

    #endregion

    #endregion

    #endregion

    #region Parameters for urea hydrolisys process

        // parameters for temperature factor for urea hydrolisys
    public TempFactorData TempFactor_Hydrol = new TempFactorData();

    // parameters for soil moisture factor for hydrolisys
    public XYData MoistFactor_Hydrol = new XYData();

    // parameters for potential urea hydrolisys
    public double potHydrol_min;  // minimum value
    public double potHydrol_parmA;
    public double potHydrol_parmB;
    public double potHydrol_parmC;
    public double potHydrol_parmD;

    #endregion
    
    #region Parameters for nitrification process

    #region Old parameters

    public double nitrification_pot;    // Potential nitrification by soil (ppm)

    public double nh4_at_half_pot;      // nh4 conc at half potential (ppm)   

    public double[] wfnit_index;        // index specifying water content for water factor for nitrification

    public double[] wfnit_values;       // value of water factor(nitrification) function at given index values

    public double[] pHf_nit_pH;         // pH values for specifying pH factor for nitrification

    public double[] pHf_nit_values;     // value of pH factor(nitrification) function for given pH values

    #endregion

    #region New parameters

        // parameters for temperature factor for nitrification
    public TempFactorData TempFactor_Nitrif = new TempFactorData();

    // parameters for soil moisture factor for nitrification
    public XYData MoistFactor_Nitrif = new XYData();

    // parameters for soil pH factor for nitrification
    public XYData pHFactor_Nitrif = new XYData();

    #endregion

    #endregion

    #region Parameters for denitrification and N2O emission processes

    #region Old parameters

    public double dnit_rate_coeff;      // denitrification rate coefficient (kg/mg)

    public double dnit_wf_power;        // denitrification water factor power term

    public double dnit_k1;              // K1 parameter from Thorburn et al (2010) for N2O model

    public double[] dnit_wfps;            // WFPS for calculating the n2o fraction of denitrification

    public double[] dnit_n2o_factor;      // WFPS factor for n2o fraction of denitrification

    public double dnit_nitrf_loss;      // Fraction of nitrification lost as denitrification

    #endregion

    #region New parameters
    
    // parameters for temperature factor for denitrification
    public TempFactorData TempFactor_Denit = new TempFactorData();

    // parameters for soil moisture factor for denitrification
    public XYData MoistFactor_Denit = new XYData();

    // parameter for TermA in N2N2O function
    public double N2N2O_parmA;

    // parameter for TermB in N2N2O function
    public double N2N2O_parmB;

    // parameter for TermC in N2N2O function
    public double N2N2O_parmC;

    // parameter 1 to compute active carbon (for denitrification)
    public double actC_parmA;

    // parameter 2 to compute active carbon (for denitrification)
    public double actC_parmB;

    #endregion

    #endregion

    #endregion

    #region Parameters that do or may change during simulation

    // today's date
    public DateTime Today;

    #region Parameters to set soil pH
    
    public double[] ph;

    #endregion

    #region Values for soil organic matter (som)

    // total soil organic carbon content (%)
    public double[] oc
    {
        get
        {
            double[] result;
            result = new double[dlayer.Length];
            for (int i = 0; i < dlayer.Length; i++)
                result[i] = (hum_c[i] + biom_c[i]) * convFactor_kgha2ppm(i) / 10000;  // (100/1000000) = convert to ppm and then to %
            return result;
        }
    }

    #endregion

    #region Values for soil mineral N

        // soil urea nitrogen amount (kgN/ha)
    private double[] _urea;     // Internal variable holding the urea amounts
    public double[] urea
    {
        get { return _urea; }
        set
        {
            for (int layer = 0; layer < Math.Max(value.Length, dlayer.Length); ++layer)
            {
                if (layer >= dlayer.Length)
                {
                    Console.WriteLine(" Attempt to assign urea value to a non-existent soil layer - extra values will be ignored");
                    break;
                }
                else if (layer >= value.Length)
                {
                    // not all values were supplied, assume minimum
                    value[layer] = urea_min[layer];
                }
                else
                {
                    if (value[layer] < nh4_min[layer] - epsilon)
                    {
                        Console.WriteLine(" Attempt to set urea(" + (layer + 1).ToString() + ") to a value below the lower limit, value will be set to minimum (" + urea_min[layer].ToString() + ")");
                        value[layer] = urea_min[layer];
                    }
                }
                _urea[layer] = value[layer];
            }
        }
    }

    //private double[] nh4_reset;      // stores initial values, can be used for a Reset operation
    private double[] _nh4;     // Internal variable holding the nh4 amounts
    public double[] nh4
    {
        get { return _nh4; }
        set
        {
            for (int layer = 0; layer < Math.Max(value.Length, dlayer.Length); ++layer)
            {
                if (layer >= _nh4.Length)
                {
                    Console.WriteLine(" Attempt to assign ammonium value to a non-existent soil layer - extra values will be ignored");
                    break;
                }
                else if (layer >= value.Length)
                {
                    // not all values were supplied, assume minimum
                    value[layer] = nh4_min[layer];
                }
                else
                {
                    if (value[layer] < nh4_min[layer] - epsilon)
                    {
                        Console.WriteLine(" Attempt to set nh4(" + (layer + 1).ToString() + ") to a value below the lower limit, value will be set to minimum (" + nh4_min[layer].ToString() + ")");
                        value[layer] = nh4_min[layer];
                    }
                }
                _nh4[layer] = value[layer];
            }
        }
    }

    // soil nitrate nitrogen amount (kgN/ha)
    //private double[] no3_reset;      // stores initial values, can be used for a Reset operation
    private double[] _no3 = null;
    public double[] no3
    {
        get { return _no3; }
        set
        {
            for (int layer = 0; layer < Math.Max(value.Length, dlayer.Length); ++layer)
            {
                if (layer >= _no3.Length)
                {
                    Console.WriteLine(" Attempt to assign no3 value to a non-existent soil layer - extra values will be ignored");
                    break;
                }
                else if (layer >= value.Length)
                {
                    // not all values were supplied, assume minimum
                    value[layer] = no3_min[layer];
                }
                else
                {
                    if (value[layer] < no3_min[layer] - epsilon)
                    {
                        Console.WriteLine(" Attempt to set no3(" + (layer + 1).ToString() + ") to a value below the lower limit, value will be set to minimum (" + no3_min[layer].ToString() + ")");
                        value[layer] = no3_min[layer];
                    }
                }
                _no3[layer] = value[layer];
            }
        }
    }

    #endregion

    #region Soil physics data

    // soil layers' thichness (mm)
    public float[] dlayer;

    // soil bulk density for each layer (kg/dm3)
    public float[] bd;

    // soil water content at saturation
    public float[] sat_dep;

    // soil water content at drainage upper limit
    public float[] dul_dep;

    // soil water content at drainage lower limit
    public float[] ll15_dep;

    // today's soil water content
    public float[] sw_dep;

    // soil albedo (0-1)
    public double salb;

    // soil temperature (as computed by another module - SoilTemp)
    public double[] st;

    #endregion

    #region Soil loss data

    // soil loss, due to erosion (?)
    public double soil_loss = 0.0;

    #endregion

    #region Pond data

    // switch indicating whether pond is active or not
    public Boolean is_pond_active = false;

    // C decomposed in pond that is added to soil biomass
    public double pond_biom_C;

    // C decomposed in pond that is added to soil humus
    public double pond_hum_C;

    #endregion

    #region Inhibitors data

    // factor reducing nitrification due to the presence of a inhibitor
    public double[] nitrification_inhibition;

    // factor reducing urea hydrolysis due to the presence of an inhibitor - not implemented yet
    public double[] hydrolysis_inhibition;

    // factor reducing mineralisation processes due to the presence of an inhibitor - not implemented yet
    public double[] mineralisation_inhibition;

    #endregion

    #endregion

    #region Settable variables

    #region Mineral nitrogen

    // variation in urea as given by another component
    public double[] dlt_urea
    {
        set
        {
            for (int layer = 0; layer < value.Length; ++layer)
            {
                if (layer < dlayer.Length)
                {
                    _urea[layer] += value[layer];
                    if (_urea[layer] < urea_min[layer] - epsilon)
                    {
                        //Console.WriteLine("Attempt to change urea(" + (layer + 1).ToString() + ") to a value below the lower limit, value will be set to " + urea_min[layer].ToString() + " kgN/ha");
                        Console.WriteLine(Today.ToShortDateString() + " - Attempt to change urea(" + (layer + 1).ToString() + " in Patch(" + PatchName + ") to a value below the lower limit");
                        Console.WriteLine("  The value [" + _urea[layer].ToString("#0.0###") + "] will be reset to " + urea_min[layer].ToString("#0.000#") + " kgN/ha");
                        _urea[layer] = urea_min[layer];
                    }
                }
                else
                {
                    Console.WriteLine("Attempt to change urea to a non existing layer, extra values will be ignored");
                    break;
                }
            }
        }
    }

    // variation in nh4 as given by another component
    public double[] dlt_nh4
    {
        set
        {
            for (int layer = 0; layer < value.Length; ++layer)
            {
                if (layer < dlayer.Length)
                {
                    _nh4[layer] += value[layer];
                    if (_nh4[layer] < nh4_min[layer] - epsilon)
                    {
                        //Console.WriteLine("Attempt to change nh4(" + (layer + 1).ToString() + ") to a value below the lower limit, value will be set to " + nh4_min[layer].ToString() + " kgN/ha");
                        Console.WriteLine(Today.ToShortDateString() + " - Attempt to change nh4(" + (layer + 1).ToString() + " in Patch(" + PatchName + ") to a value below the lower limit");
                        Console.WriteLine("  The value [" + _nh4[layer].ToString("#0.0###") + "] will be reset to " + nh4_min[layer].ToString("#0.000#") + " kgN/ha");
                        _nh4[layer] = nh4_min[layer];
                    }
                }
                else
                {
                    Console.WriteLine("Attempt to change nh4 to a non existing layer, extra values will be ignored");
                    break;
                }
            }
        }
    }

    // variation in no3 as given by another component
    public double[] dlt_no3
    {
        set
        {
            for (int layer = 0; layer < value.Length; ++layer)
            {
                if (layer < dlayer.Length)
                {
                    _no3[layer] += value[layer];
                    if (_no3[layer] < no3_min[layer] - epsilon)
                    {
                        //Console.WriteLine("Attempt to change no3(" + (layer + 1).ToString() + ") to a value below the lower limit, value will be set to " + no3_min[layer].ToString() + " kgN/ha");
                        Console.WriteLine(Today.ToShortDateString() + " - Attempt to change no3(" + (layer + 1).ToString() + " in Patch(" + PatchName + ") to a value below the lower limit");
                        Console.WriteLine("  The value [" + _no3[layer].ToString("#0.0###") + "] will be reset to " + no3_min[layer].ToString("#0.000#") + " kgN/ha");
                        _no3[layer] = no3_min[layer];
                    }
                }
                else
                {
                    Console.WriteLine("Attempt to change no3 to a non existing layer, extra values will be ignored");
                    break;
                }
            }
        }
    }

    #endregion

    #region organic N and C

    public double[] dlt_org_n
    {
        set
        {
            for (int layer = 0; layer < value.Length; ++layer)
            {
                fom_n[layer] += value[layer];
                if (fom_n[layer] < 0.0)
                {
                    //Console.WriteLine("Attempt to change org_n(" + (layer + 1).ToString() + ") to a negative value, value will be set to 0.0 kg/ha");
                    Console.WriteLine(Today.ToShortDateString() + " - Attempt to change fom_n(" + (layer + 1).ToString() + " in Patch(" + PatchName + ") to a negative value");
                    Console.WriteLine("  The value [" + fom_n[layer].ToString("#0.0###") + "] will be reset to 0.000 kgN/ha");
                    fom_n[layer] = 0.0;
                }
            }
        }
    }

    public double[] dlt_org_c_pool1
    {
        set
        {
            for (int layer = 0; layer < value.Length; ++layer)
            {
                fom_c_pool1[layer] += value[layer];
                if (fom_c_pool1[layer] < 0.0)
                {
                    //Console.WriteLine("Attempt to change fom_c_pool1(" + (layer + 1).ToString() + ") to a negative value, value will be set to 0.0 kg/ha");
                    Console.WriteLine(Today.ToShortDateString() + " - Attempt to change fom_c_pool1(" + (layer + 1).ToString() + " in Patch(" + PatchName + ") to a negative value");
                    Console.WriteLine("  The value [" + fom_c_pool1[layer].ToString("#0.0###") + "] will be reset to 0.000 kg/ha");
                    fom_c_pool1[layer] = 0.0;
                }
            }
        }
    }

    public double[] dlt_org_c_pool2
    {
        set
        {
            for (int layer = 0; layer < value.Length; ++layer)
            {
                fom_c_pool2[layer] += value[layer];
                if (fom_c_pool2[layer] < 0.0)
                {
                    //Console.WriteLine("Attempt to change fom_c_pool2(" + (layer + 1).ToString() + ") to a negative value, value will be set to 0.0 kg/ha");
                    Console.WriteLine(Today.ToShortDateString() + " - Attempt to change fom_c_pool2(" + (layer + 1).ToString() + " in Patch(" + PatchName + ") to a negative value");
                    Console.WriteLine("  The value [" + fom_c_pool2[layer].ToString("#0.0###") + "] will be reset to 0.000 kg/ha");
                    fom_c_pool2[layer] = 0.0;
                }
            }
        }
    }

    public double[] dlt_org_c_pool3
    {
        set
        {
            for (int layer = 0; layer < value.Length; ++layer)
            {
                fom_c_pool3[layer] += value[layer];
                if (fom_c_pool3[layer] < 0.0)
                {
                    //Console.WriteLine("Attempt to change fom_c_pool3(" + (layer + 1).ToString() + ") to a negative value, value will be set to 0.0 kg/ha");
                    Console.WriteLine(Today.ToShortDateString() + " - Attempt to change fom_c_pool3(" + (layer + 1).ToString() + " in Patch(" + PatchName + ") to a negative value");
                    Console.WriteLine("  The value [" + fom_c_pool3[layer].ToString("#0.0###") + "] will be reset to 0.000 kg/ha");
                    fom_c_pool3[layer] = 0.0;
                }
            }
        }
    }

    #endregion

    #endregion

    #endregion

    #region Outputs we make available to other components

    #region Outputs for Nitrogen

    #region Changes for today - deltas

    public double[] dlt_nh4_net;   // net nh4 change today

    public double[] nh4_transform_net; // net NH4 transformation today

    public double[] dlt_no3_net;   // net no3 change today

    public double[] no3_transform_net; // net NO3 transformation today

    public double[] dlt_n_min         // net mineralisation
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                result[layer] = dlt_n_hum_2_min[layer] + dlt_n_biom_2_min[layer] + dlt_n_fom_2_min[layer];
            return result;
        }
    }

    public double[] dlt_n_min_res
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                result[layer] = dlt_no3_decomp[layer] + dlt_nh4_decomp[layer];
            return result;
        }
    }

    public double[] dlt_nh4_decomp;   // Net Residue NH4 mineralisation

    public double[] dlt_no3_decomp;   // Net Residue NO3 mineralisation

    public double[] dlt_n_fom_2_min;     // net fom N mineralized (negative for immobilization) 

    public double[] dlt_n_hum_2_min;     // net humic N mineralized

    public double[] dlt_n_biom_2_min;    // net biomass N mineralized

    public double[] dlt_n_min_tot
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                result[layer] = dlt_n_hum_2_min[layer] + dlt_n_biom_2_min[layer] + dlt_n_fom_2_min[layer] + dlt_no3_decomp[layer] + dlt_nh4_decomp[layer];
            return result;
        }
    }

    public double[] dlt_urea_hydrolised;   // nitrogen coverted by hydrolysis (from urea to NH4)

    public double[] dlt_nitrification;     // nitrogen coverted by nitrification (from NH4 to either NO3 or N2O)

    public double[] effective_nitrification; // effective nitrogen coverted by nitrification (from NH4 to NO3)
    // (Alias dlt_rntrf_eff)

    public double[] dlt_nh4_dnit;      // NH4 N denitrified

    public double[] dlt_no3_dnit;      // NO3 N denitrified

    public double[] n2o_atm;           // amount of N2O produced

    public double[] n2_atm;            // amount of N2 produced

    public double[] dnit
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                result[layer] = dlt_no3_dnit[layer] + dlt_nh4_dnit[layer];
            return result;
        }
    }

    public double dlt_n_loss_in_sed;

    public double[] nh4_deficit_immob;    // excess N required above NH4 supply    #endregion

    #endregion

    #region Amounts in various pools

    public double[] fom_n         // nitrogen in FOM
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; ++layer)
            {
                result[layer] = fom_n_pool1[layer] + fom_n_pool2[layer] + fom_n_pool3[layer];
            }
            return result;
        }
    }

    public double[] fom_n_pool1;

    public double[] fom_n_pool2;

    public double[] fom_n_pool3;

    public double[] hum_n;         // Humic N

    public double[] biom_n;        // biomass nitrogen

    public double[] nit_tot           // total N in soil
    {
        get
        {
            double[] result = null;
            if (dlayer != null)
            {
                result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[layer] += fom_n[layer] + hum_n[layer] + biom_n[layer] + _no3[layer] + _nh4[layer] + _urea[layer];
            }
            return result;
        }
    }

    #endregion

    #endregion

    #region Outputs for Carbon

    #region Changes for today - deltas

    public double dlt_c_loss_in_sed;

    double[][] dlt_c_fom_2_hum = new double[3][];
    public double[] dlt_fom_c_hum  // fom C converted to humic (kg/ha)
    {
        get
        {
            int nLayers = dlt_c_fom_2_hum[0].Length;
            double[] result = new double[nLayers];
            for (int layer = 0; layer < nLayers; layer++)
                result[layer] = dlt_c_fom_2_hum[0][layer] + dlt_c_fom_2_hum[1][layer] + dlt_c_fom_2_hum[2][layer];
            return result;
        }
    }

    double[][] dlt_c_fom_2_biom = new double[3][];
    public double[] dlt_fom_c_biom // fom C converted to biomass (kg/ha)
    {
        get
        {
            int nLayers = dlt_c_fom_2_biom[0].Length;
            double[] result = new double[nLayers];
            for (int layer = 0; layer < nLayers; layer++)
                result[layer] = dlt_c_fom_2_biom[0][layer] + dlt_c_fom_2_biom[1][layer] + dlt_c_fom_2_biom[2][layer];
            return result;
        }
    }

    double[][] dlt_c_fom_2_atm = new double[3][];
    public double[] dlt_fom_c_atm  // fom C lost to atmosphere (kg/ha)
    {
        get
        {
            int nLayers = dlt_c_fom_2_atm[0].Length;
            double[] result = new double[nLayers];
            for (int layer = 0; layer < nLayers; layer++)
                result[layer] = dlt_c_fom_2_atm[0][layer] + dlt_c_fom_2_atm[1][layer] + dlt_c_fom_2_atm[2][layer];
            return result;
        }
    }

    public double[] dlt_c_hum_2_biom;

    public double[] dlt_c_hum_2_atm;

    public double[] dlt_c_biom_2_hum;

    public double[] dlt_c_biom_2_atm;

    public double[][] dlt_c_res_2_biom;
    public double[] dlt_res_c_biom
    {
        get
        {
            int nLayers = dlt_c_res_2_biom.Length;
            double[] result = new double[nLayers];
            for (int layer = 0; layer < nLayers; layer++)
                result[layer] = SumDoubleArray(dlt_c_res_2_biom[layer]);
            return result;
        }
    }

    public double[][] dlt_c_res_2_hum;
    public double[] dlt_res_c_hum
    {
        get
        {
            int nLayers = dlt_c_res_2_hum.Length;
            double[] result = new double[nLayers];
            for (int layer = 0; layer < nLayers; layer++)
                result[layer] = SumDoubleArray(dlt_c_res_2_hum[layer]);
            return result;
        }
    }

    public double[][] dlt_c_res_2_atm;
    public double[] dlt_res_c_atm
    {
        get
        {
            int nLayers = dlt_c_res_2_atm.Length;
            double[] result = new double[nLayers];
            for (int layer = 0; layer < nLayers; layer++)
                result[layer] = SumDoubleArray(dlt_c_res_2_atm[layer]);
            return result;
        }
    }

    public double[] dlt_fom_c_pool1;

    public double[] dlt_fom_c_pool2;

    public double[] dlt_fom_c_pool3;

    public double[] soilp_dlt_res_c_atm;

    public double[] soilp_dlt_res_c_hum;

    public double[] soilp_dlt_res_c_biom;

    #endregion

    #region Amounts in various pools

    public double[] fom_c         // fresh organic C
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; ++layer)
            {
                result[layer] = fom_c_pool1[layer] + fom_c_pool2[layer] + fom_c_pool3[layer];
            }
            return result;
        }
    }

    public double[] fom_c_pool1;

    public double[] fom_c_pool2;

    public double[] fom_c_pool3;

    public double[] hum_c;         // Humic C

    public double[] biom_c;        // biomass carbon

    public double[] inert_c;       // humic C that is not subject to mineralization (kg/ha)

    public double[] carbon_tot    // total carbon in soil
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
            {
                result[layer] += fom_c_pool1[layer] + fom_c_pool2[layer] + fom_c_pool3[layer] + hum_c[layer] + biom_c[layer];
            }
            return result;
        }
    }

    #endregion

    #endregion

    #region Factors and other outputs

    public double[] soilp_dlt_org_p;

    #endregion

    #endregion

    #region Internal variables

    private double[] nh4_yesterday;                 // yesterday's ammonium nitrogen(kg/ha)
    private double[] no3_yesterday;                 // yesterday's nitrate nitrogen (kg/ha)
    public int fom_type;
    private int num_residues = 0;                   // number of residues decomposing   
    private string[] residue_name;                  // name of residues decomposing
    private string[] residue_type;                  // type of decomposing residue
    private double[] pot_c_decomp;                  // Potential residue C decomposition (kg/ha)
    private double[] pot_n_decomp;                  // Potential residue N decomposition (kg/ha)
    private double[] pot_p_decomp;                  // Potential residue P decomposition (kg/ha)
    private double[][] dlt_c_decomp;            // residue C decomposition (kg/ha)
    private double[][] dlt_n_decomp;            // residue N decomposition (kg/ha)

    public SurfaceOrganicMatterDecompType SOMDecomp;

    public struct TempFactorData
    {
        // the parameters to compute the temperature factor
        public double[] TempOptimum;
        public double[] FactorAtZero;
        public double[] CurveExponent;
    }

    public struct XYData
    {
        // lists with value of x and y used to describe certain functions (water factor, for ex.)
        public double[] xVals;
        public double[] yVals;
    }

    private struct FOMdecompData
    {
        // lists with values from FOM decompostion
        public double[] dlt_c_hum;
        public double[] dlt_c_biom;
        public double[] dlt_c_atm;
        public double[] dlt_fom_n;
        public double dlt_n_min;
    }

    #endregion

    #region Setup calculations

    public void InitCalc()
    {
        for (int layer = 0; layer < dlayer.Length; layer++)
        {
            // store these values so they may be used tomorrow
            nh4_yesterday[layer] = _nh4[layer];
            no3_yesterday[layer] = _no3[layer];
        }
    }

    #endregion

    #region Process calculations

    #region Daily processses

    public void Process()
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
        {
            // Decompose residues
            //  assess the potential decomposition of surface residues and calculate actual mineralisation/immobilisation
            DecomposeResidues();

            // update C content in hum and biom pools
            for (int layer = 0; layer < nLayers; layer++)
            {
                hum_c[layer] += SumDoubleArray(dlt_c_res_2_hum[layer]);
                biom_c[layer] += SumDoubleArray(dlt_c_res_2_biom[layer]);
            }

            // update N content in hum and biom pools as well as the mineral N
            for (int layer = 0; layer < nLayers; layer++)
            {
                hum_n[layer] = MathUtility.Divide(hum_c[layer], hum_cn, 0.0);
                biom_n[layer] = MathUtility.Divide(biom_c[layer], biom_cn, 0.0);

                // update soil mineral N
                _nh4[layer] += dlt_nh4_decomp[layer];
                _no3[layer] += dlt_no3_decomp[layer];
            }
        }

        // now take each layer in turn and compute N processes
        for (int layer = 0; layer < nLayers; layer++)
        {
            // urea hydrolysis
            dlt_urea_hydrolised[layer] = UreaHydrolysis(layer);
            _nh4[layer] += dlt_urea_hydrolised[layer];
            _urea[layer] -= dlt_urea_hydrolised[layer];

            // nitrate-N denitrification
            dlt_no3_dnit[layer] = Denitrification(layer);
            _no3[layer] -= dlt_no3_dnit[layer];

            // N2O loss to atmosphere - due to denitrification
            n2o_atm[layer] = 0.0;
            double N2N2O = Denitrification_Nratio(layer);
            n2o_atm[layer] = dlt_no3_dnit[layer] / (N2N2O + 1.0);

            // Calculate transformations of soil organic matter (C and N)

            // humic pool mineralisation
            MineraliseHumus(layer);

            // microbial biomass pool mineralisation
            MineraliseBiomass(layer);

            // mineralisation of fresh organic matter pools
            // need to be revisited - create FOM pools as array
            //for (int fract = 0; fract < 3; fract++)
            //{
            //    MinFom(layer, fract);
            //    dlt_c_fom_2_biom[fract][layer] = dlt_fc_biom[fract];
            //    dlt_c_fom_2_hum[fract][layer] = dlt_fc_hum[fract];
            //    dlt_c_fom_2_atm[fract][layer] = dlt_fc_atm[fract];
            //    dlt_fom_n[fract, layer] = dlt_f_n[fract];
            //}

            double[] dlt_f_n;
            double[] dlt_fc_biom;
            double[] dlt_fc_hum;
            double[] dlt_fc_atm;
            FOMdecompData MineralisedFOM = new FOMdecompData();
            if (useNewFunctions)
            {
                MineralisedFOM = MineraliseFOM1(layer);
                for (int fract = 0; fract < 3; fract++)
                {
                    dlt_c_fom_2_hum[fract][layer] = MineralisedFOM.dlt_c_hum[fract];
                    dlt_c_fom_2_biom[fract][layer] = MineralisedFOM.dlt_c_biom[fract];
                    dlt_c_fom_2_atm[fract][layer] = MineralisedFOM.dlt_c_atm[fract];
                    dlt_fom_n[fract, layer] = MineralisedFOM.dlt_fom_n[fract];
                }
                dlt_n_fom_2_min[layer] = MineralisedFOM.dlt_n_min;
            }
            else
            {
                MineraliseFOM(layer, out dlt_fc_biom, out dlt_fc_hum, out dlt_fc_atm, out dlt_f_n, out dlt_n_fom_2_min[layer]);

                for (int fract = 0; fract < 3; fract++)
                {
                    dlt_c_fom_2_biom[fract][layer] = dlt_fc_biom[fract];
                    dlt_c_fom_2_hum[fract][layer] = dlt_fc_hum[fract];
                    dlt_c_fom_2_atm[fract][layer] = dlt_fc_atm[fract];
                    dlt_fom_n[fract, layer] = dlt_f_n[fract];
                }
            }
            // update pools C an N contents

            hum_c[layer] += dlt_c_biom_2_hum[layer] - dlt_c_hum_2_biom[layer] - dlt_c_hum_2_atm[layer] +
                           dlt_c_fom_2_hum[0][layer] + dlt_c_fom_2_hum[1][layer] + dlt_c_fom_2_hum[2][layer];

            hum_n[layer] = MathUtility.Divide(hum_c[layer], hum_cn, 0.0);

            biom_c[layer] += dlt_c_hum_2_biom[layer] - dlt_c_biom_2_hum[layer] - dlt_c_biom_2_atm[layer] +
                           dlt_c_fom_2_biom[0][layer] + dlt_c_fom_2_biom[1][layer] + dlt_c_fom_2_biom[2][layer];

            biom_n[layer] = MathUtility.Divide(biom_c[layer], biom_cn, 0.0);

            fom_c_pool1[layer] -= (dlt_c_fom_2_hum[0][layer] + dlt_c_fom_2_biom[0][layer] + dlt_c_fom_2_atm[0][layer]);
            fom_c_pool2[layer] -= (dlt_c_fom_2_hum[1][layer] + dlt_c_fom_2_biom[1][layer] + dlt_c_fom_2_atm[1][layer]);
            fom_c_pool3[layer] -= (dlt_c_fom_2_hum[2][layer] + dlt_c_fom_2_biom[2][layer] + dlt_c_fom_2_atm[2][layer]);

            fom_n_pool1[layer] -= dlt_fom_n[0, layer];
            fom_n_pool2[layer] -= dlt_fom_n[1, layer];
            fom_n_pool3[layer] -= dlt_fom_n[2, layer];

            // dsg  these 3 dlts are calculated for the benefit of soilp which needs to 'get' them
            dlt_fom_c_pool1[layer] = dlt_c_fom_2_hum[0][layer] + dlt_c_fom_2_biom[0][layer] + dlt_c_fom_2_atm[0][layer];
            dlt_fom_c_pool2[layer] = dlt_c_fom_2_hum[1][layer] + dlt_c_fom_2_biom[1][layer] + dlt_c_fom_2_atm[1][layer];
            dlt_fom_c_pool3[layer] = dlt_c_fom_2_hum[2][layer] + dlt_c_fom_2_biom[2][layer] + dlt_c_fom_2_atm[2][layer];

            // add up fom in each layer in each of the pools
            //double fom_c = fom_c_pool1[layer] + fom_c_pool2[layer] + fom_c_pool3[layer];
            //fom_n[layer] = fom_n_pool1[layer] + fom_n_pool2[layer] + fom_n_pool3[layer];

            // update soil mineral N after mineralisation/immobilisation

            // starts with nh4
            _nh4[layer] += dlt_n_hum_2_min[layer] + dlt_n_biom_2_min[layer] + dlt_n_fom_2_min[layer];

            // check whether there is enough NH4 to be immobilised
            nh4_deficit_immob = new double[dlayer.Length];
            if (_nh4[layer] < nh4_min[layer])
            {
                nh4_deficit_immob[layer] = nh4_min[layer] - _nh4[layer];
                _nh4[layer] = nh4_min[layer];
            }

            // now change no3
            _no3[layer] -= nh4_deficit_immob[layer];
            if (_no3[layer] < no3_min[layer] - epsilon)
            {
                // note: tests for adequate mineral N for immobilisation have been made so this no3 should not go below no3_min
                throw new Exception("N immobilisation resulted in mineral N in layer(" + (layer + 1).ToString() + ") to go below minimum");
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
            nh4_transform_net[layer] = dlt_nh4_decomp[layer] + dlt_n_fom_2_min[layer] + dlt_n_biom_2_min[layer] + dlt_n_hum_2_min[layer] - dlt_nitrification[layer] + dlt_urea_hydrolised[layer] + nh4_deficit_immob[layer];
            no3_transform_net[layer] = dlt_no3_decomp[layer] - dlt_no3_dnit[layer] + effective_nitrification[layer] - nh4_deficit_immob[layer];

            // net deltas
            dlt_nh4_net[layer] = _nh4[layer] - nh4_yesterday[layer];
            dlt_no3_net[layer] = _no3[layer] - no3_yesterday[layer];

            // store these values so they may be used tomorrow
            nh4_yesterday[layer] = _nh4[layer];
            no3_yesterday[layer] = _no3[layer];
        }
    }

    public void OnTick()
    {
        // +  Purpose:
        //      Reset potential decomposition variables

        num_residues = 0;
        Array.Resize(ref pot_c_decomp, 0);
        Array.Resize(ref pot_n_decomp, 0);
        Array.Resize(ref pot_p_decomp, 0);
    }

    private void DecomposeResidues()
    {
        // + Purpose
        //     Calculate the actual C and N mineralised/immobilised from residue decomposition
        //      Check whether adequate mineral nitrogen is available to sustain potential rate of decomposition of surface
        //       residues and calculate net rate of nitrogen mineralisation/immobilisation

        // Initialise to zero by assigning new
        int nLayers = dlayer.Length;
        double[] no3_available = new double[nLayers]; // no3 available for mineralisation
        double[] nh4_available = new double[nLayers]; // nh4 available for mineralisation
        dlt_c_decomp = new double[nLayers][];
        dlt_n_decomp = new double[nLayers][];
        dlt_c_res_2_biom = new double[nLayers][];
        dlt_c_res_2_hum = new double[nLayers][];
        dlt_c_res_2_atm = new double[nLayers][];
        for (int layer = 0; layer < nLayers; layer++)
        {
            dlt_c_decomp[layer] = new double[num_residues];
            dlt_n_decomp[layer] = new double[num_residues];
            dlt_c_res_2_biom[layer] = new double[num_residues];
            dlt_c_res_2_hum[layer] = new double[num_residues];
            dlt_c_res_2_atm[layer] = new double[num_residues];
        }
        dlt_nh4_decomp = new double[nLayers];
        dlt_no3_decomp = new double[nLayers];

        // get total available mineral N in soil layer which can supply N to decomposition (min_depth)
        double[] fracLayer = new double[dlayer.Length];
        double cumFracLayer = 0.0;
        double cumDepth = 0.0;
        int DecompLayer = 0;
        for (int layer = 0; layer < nLayers; layer++)
        {
            fracLayer[layer] = Math.Min(1, Math.Max(0, min_depth - cumDepth) / dlayer[layer]);
            if (fracLayer[layer] <= epsilon)
                break;  // no need to continue calculating
            cumFracLayer += fracLayer[layer];
            cumDepth += dlayer[layer];
            DecompLayer = layer;
            no3_available[layer] = Math.Max(0.0, _no3[layer] - no3_min[layer]) * fracLayer[layer];
            nh4_available[layer] = Math.Max(0.0, _nh4[layer] - nh4_min[layer]) * fracLayer[layer];
        }

        double n_available = SumDoubleArray(no3_available) + SumDoubleArray(nh4_available) + SumDoubleArray(pot_n_decomp);

        // get N demand from potential decomposition
        double n_demand = MathUtility.Divide(SumDoubleArray(pot_c_decomp) * ef_res * fr_res_biom, biom_cn, 0.0) +
                          MathUtility.Divide(SumDoubleArray(pot_c_decomp) * ef_res * (1.0 - fr_res_biom), hum_cn, 0.0);

        // test whether there is adequate N available to meet potential immobilisation demand
        //      if not, calculate a factor to reduce the mineralisation rates
        double ReductionFactor = 1.0;
        if (n_demand > n_available)
            ReductionFactor = Math.Max(0.0, Math.Min(1.0, MathUtility.Divide(SumDoubleArray(no3_available) + SumDoubleArray(nh4_available), n_demand - SumDoubleArray(pot_n_decomp), 0.0)));

        // Partition the additions of C and N to layers
        double dlt_n_decomp_tot = 0.0;
        for (int layer = 0; layer <= DecompLayer; layer++)
        {
            double DecompFraction = fracLayer[layer] / cumFracLayer;  // the fraction of decomposition for each soil layer
            for (int residue = 0; residue < num_residues; residue++)
            {
                // adjust carbon transformations and distribute over the layers
                dlt_c_decomp[layer][residue] = pot_c_decomp[residue] * ReductionFactor * DecompFraction;
                dlt_n_decomp[layer][residue] = pot_n_decomp[residue] * ReductionFactor * DecompFraction;
                dlt_n_decomp_tot += dlt_n_decomp[layer][residue];

                // partition the decomposed C between pools and losses
                dlt_c_res_2_biom[layer][residue] = dlt_c_decomp[layer][residue] * ef_res * fr_res_biom;
                dlt_c_res_2_hum[layer][residue] = dlt_c_decomp[layer][residue] * ef_res * (1.0 - fr_res_biom);
                dlt_c_res_2_atm[layer][residue] = dlt_c_decomp[layer][residue] - dlt_c_res_2_biom[layer][residue] - dlt_c_res_2_hum[layer][residue];
            }
        }

        // net N mineralised (hg/ha)
        double dlt_n_min = dlt_n_decomp_tot - n_demand * ReductionFactor;

        if (dlt_n_min > 0.0)
        {
            // Mineralisation occurred - distribute NH4 over the layers
            for (int layer = 0; layer <= DecompLayer; layer++)
            {
                double DecompFraction = fracLayer[layer] / cumFracLayer;  // the fraction of decomposition for each soil layer
                dlt_nh4_decomp[layer] = dlt_n_min * DecompFraction;
            }
        }
        else if (dlt_n_min < 0.0)
        {
            // Immobilisation occurred - soak up any N required, from NH4 first then NO3 if needed
            for (int layer = 0; layer <= DecompLayer; layer++)
            {
                dlt_nh4_decomp[layer] = -Math.Min(nh4_available[layer], Math.Abs(dlt_n_min));
                dlt_n_min -= dlt_nh4_decomp[layer];
            }
            for (int layer = 0; layer <= DecompLayer; layer++)
            {
                dlt_no3_decomp[layer] = -Math.Min(no3_available[layer], Math.Abs(dlt_n_min));
                dlt_n_min -= dlt_no3_decomp[layer];
            }

            // There should now be no remaining immobilisation demand
            if (dlt_n_min < -0.001 || dlt_n_min > 0.001)
                throw new Exception("Value for remaining immobilisation is out of range");
        }

        // gather the info for 'SendActualResidueDecompositionCalculated'
        PackActualResidueDecomposition();
    }

    private void MineraliseHumus(int layer)
    {
        // + Purpose
        //     Calculate the daily transformation of the the soil humic pool, mineralisation (+ve) or immobilisation (-ve)

        // + Assumptions
        //     There is an inert_C component of the humic pool that is not subject to mineralisation

        // dsg 200508  use different values for some constants when there's a pond and anaerobic conditions dominate
        int index = (!is_pond_active) ? 1 : 2;

        // get the soil temperature factor
        double tf = (SoilParamSet == "rothc") ? RothcTF(layer, index) : TF(layer, index);
        if (useNewFunctions)
            if (useSingleMinerFactors)
            {
                tf = SoilTempFactor(layer, index, TempFactor_Miner);
            }
            else
            {
                tf = SoilTempFactor(layer, index, TempFactor_minerHum);
            }

        // get the soil water factor
        double wf = WF(layer, index);
        if (useNewFunctions)
            if (useSingleMinerFactors)
            {
                wf = SoilMoistFactor(layer, index, MoistFactor_Miner);
            }
            else
            {
                wf = SoilMoistFactor(layer, index, MoistFactor_minerHum);
            }

        // get the rate of mineralisation of N from the humic pool
        double dlt_c_min_hum = (hum_c[layer] - inert_c[layer]) * rd_hum[index - 1] * tf * wf;
        double dlt_n_min_hum = MathUtility.Divide(dlt_c_min_hum, hum_cn, 0.0);

        // distribute the mineralised N and C
        dlt_c_hum_2_biom[layer] = dlt_c_min_hum * ef_hum;
        dlt_c_hum_2_atm[layer] = dlt_c_min_hum * (1.0 - ef_hum);
        dlt_n_hum_2_min[layer] = dlt_n_min_hum - MathUtility.Divide(dlt_c_hum_2_biom[layer], biom_cn, 0.0);
    }

    private void MineraliseBiomass(int layer)
    {
        // + Purpose
        //     Calculate the daily transformation of the soil biomass pool, mineralisation (+ve) or immobilisation (-ve)

        // dsg 200508  use different values for some constants when anaerobic conditions dominate
        int index = (!is_pond_active) ? 1 : 2;

        // get the soil temperature factor
        double tf = (SoilParamSet == "rothc") ? RothcTF(layer, index) : TF(layer, index);
        if (useNewFunctions)
            if (useSingleMinerFactors)
            {
                tf = SoilTempFactor(layer, index, TempFactor_Miner);
            }
            else
            {
                tf = SoilTempFactor(layer, index, TempFactor_minerBiom);
            }

        // get the soil water factor
        double wf = WF(layer, index);
        if (useSingleMinerFactors)
            if (useNewFunctions)
            {
                wf = SoilMoistFactor(layer, index, MoistFactor_Miner);
            }
            else
            {
                wf = SoilMoistFactor(layer, index, MoistFactor_minerBiom);
            }

        // get the rate of mineralisation of C & N from the biomass pool
        double dlt_n_min_biom = biom_n[layer] * rd_biom[index - 1] * tf * wf;       // why the calculation is on n while for hum is on C?
        double dlt_c_min_biom = dlt_n_min_biom * biom_cn;

        // distribute the carbon
        dlt_c_biom_2_hum[layer] = dlt_c_min_biom * ef_biom * (1.0 - fr_biom_biom);
        dlt_c_biom_2_atm[layer] = dlt_c_min_biom * (1.0 - ef_biom);

        // calculate net N mineralisation
        dlt_n_biom_2_min[layer] = dlt_n_min_biom - MathUtility.Divide(dlt_c_biom_2_hum[layer], hum_cn, 0.0) - MathUtility.Divide((dlt_c_min_biom - dlt_c_biom_2_atm[layer] - dlt_c_biom_2_hum[layer]), biom_cn, 0.0);
    }

    private void MineraliseFOM(int layer, out double[] dlt_c_biom, out double[] dlt_c_hum, out double[] dlt_c_atm, out double[] dlt_fom_n, out double dlt_n_min)
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
        if (useNewFunctions)
            cnrf = CNorgFactor(layer, index, CNFactorMinerFOM_OptCN, CNFactorMinerFOM_RateCN);

        // get the soil temperature factor
        double tf = (SoilParamSet == "rothc") ? RothcTF(layer, index) : TF(layer, index);
        if (useNewFunctions)
            tf = SoilTempFactor(layer, index, TempFactor_Miner);

        // get the soil water factor
        double wf = WF(layer, index);
        if (useNewFunctions)
            wf = SoilMoistFactor(layer, index, MoistFactor_Miner);

        // calculate gross amount of C & N released due to mineralisation of the fresh organic matter.
        if (fomC >= fom_min)
        {
            double dlt_n_min_fom = 0.0; // amount of fresh organic N mineralised across fpools (kg/ha)
            double dlt_c_min_fom = 0.0; // total C mineralised (kg/ha) summed across fpools
            double[] dlt_n_min_tot = new double[3]; // amount of fresh organic N mineralised in each pool (kg/ha)
            double[] dlt_c_min_tot = new double[3]; // amount of C mineralised (kg/ha) from each pool

            // C:N ratio of fom
            double fom_cn = MathUtility.Divide(fomC, fomN, 0.0);

            // get the decomposition of carbohydrate-like, cellulose-like and lignin-like fractions (fpools) in turn.
            for (int fractn = 0; fractn < 3; fractn++)
            {
                // get the max decomposition rate for each fpool
                double decomp_rate = FractRDFom(fractn)[index - 1] * cnrf * tf * wf;

                // calculate the gross amount of fresh organic carbon mineralised (kg/ha)
                double gross_c_decomp = decomp_rate * FractFomC(fractn)[layer];

                // calculate the gross amount of N released from fresh organic matter (kg/ha)
                double gross_n_decomp = decomp_rate * FractFomN(fractn)[layer];

                dlt_n_min_fom += gross_n_decomp;
                dlt_c_min_tot[fractn] = gross_c_decomp;
                dlt_n_min_tot[fractn] = gross_n_decomp;
                dlt_c_min_fom += gross_c_decomp;
            }

            // calculate potential transfers of C mineralised to biomass
            double dlt_c_biom_tot = dlt_c_min_fom * ef_fom * fr_fom_biom;

            // calculate potential transfers of C mineralised to humus
            double dlt_c_hum_tot = dlt_c_min_fom * ef_fom * (1.0 - fr_fom_biom);

            // test whether there is adequate N available to meet immobilisation demand
            double n_demand = MathUtility.Divide(dlt_c_biom_tot, biom_cn, 0.0) + MathUtility.Divide(dlt_c_hum_tot, hum_cn, 0.0);
            double n_avail = nitTot + dlt_n_min_fom;

            // factor to reduce mineralisation rates if insufficient N to meet immobilisation demand
            double Navail_factor = 1.0;
            if (n_demand > n_avail)
                Navail_factor = Math.Max(0.0, Math.Min(1.0, MathUtility.Divide(nitTot, n_demand - dlt_n_min_fom, 0.0)));

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

            dlt_n_min = (dlt_n_min_fom - n_demand) * Navail_factor;
        }
    }

    private FOMdecompData MineraliseFOM1(int layer)
    {
        // + Purpose
        //     Calculate the daily transformation of the soil fresh organic matter pools, mineralisation (+ve) or immobilisation (-ve)

        double[] dlt_c_hum = new double[3];     // dlt_c from fom to humus
        double[] dlt_c_biom = new double[3];    // dlt_c from fom to biomass
        double[] dlt_c_atm = new double[3];     // dlt_c from fom to atmosphere
        double[] dlt_fom_n = new double[3];     // dlt_n from fom pools to OM
        double dlt_n_min = 0.0;                 // dlt_n from fom to mineral

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
        if (useNewFunctions)
            cnrf = CNorgFactor(layer, index, CNFactorMinerFOM_OptCN, CNFactorMinerFOM_RateCN);

        // get the soil temperature factor
        double tf = (SoilParamSet == "rothc") ? RothcTF(layer, index) : TF(layer, index);
        if (useNewFunctions)
            if (useNewFunctions)
            {
                tf = SoilTempFactor(layer, index, TempFactor_Miner);
            }
            else
            {
                tf = SoilTempFactor(layer, index, TempFactor_minerFOM);
            }

        // get the soil water factor
        double wf = WF(layer, index);
        if (useNewFunctions)
            if (useNewFunctions)
            {
                wf = SoilMoistFactor(layer, index, MoistFactor_Miner);
            }
            else
            {
                wf = SoilMoistFactor(layer, index, MoistFactor_minerFOM);
            }

        // calculate gross amount of C & N released due to mineralisation of the fresh organic matter.
        if (fomC >= fom_min)
        {
            double dlt_n_min_fom = 0.0; // amount of fresh organic N mineralised across fpools (kg/ha)
            double dlt_c_min_fom = 0.0; // total C mineralised (kg/ha) summed across fpools
            double[] dlt_n_min_tot = new double[3]; // amount of fresh organic N mineralised in each pool (kg/ha)
            double[] dlt_c_min_tot = new double[3]; // amount of C mineralised (kg/ha) from each pool

            // C:N ratio of fom
            double fom_cn = MathUtility.Divide(fomC, fomN, 0.0);

            // get the decomposition of carbohydrate-like, cellulose-like and lignin-like fractions (fpools) in turn.
            for (int fractn = 0; fractn < 3; fractn++)
            {
                // get the max decomposition rate for each fpool
                double decomp_rate = FractRDFom(fractn)[index - 1] * cnrf * tf * wf;

                // calculate the gross amount of fresh organic carbon mineralised (kg/ha)
                double gross_c_decomp = decomp_rate * FractFomC(fractn)[layer];

                // calculate the gross amount of N released from fresh organic matter (kg/ha)
                double gross_n_decomp = decomp_rate * FractFomN(fractn)[layer];

                dlt_n_min_fom += gross_n_decomp;
                dlt_c_min_tot[fractn] = gross_c_decomp;
                dlt_n_min_tot[fractn] = gross_n_decomp;
                dlt_c_min_fom += gross_c_decomp;
            }

            // calculate potential transfers of C mineralised to biomass
            double dlt_c_biom_tot = dlt_c_min_fom * ef_fom * fr_fom_biom;

            // calculate potential transfers of C mineralised to humus
            double dlt_c_hum_tot = dlt_c_min_fom * ef_fom * (1.0 - fr_fom_biom);

            // test whether there is adequate N available to meet immobilisation demand
            double n_demand = MathUtility.Divide(dlt_c_biom_tot, biom_cn, 0.0) + MathUtility.Divide(dlt_c_hum_tot, hum_cn, 0.0);
            double n_avail = nitTot + dlt_n_min_fom;

            // factor to reduce mineralisation rates if insufficient N to meet immobilisation demand
            double Navail_factor = 1.0;
            if (n_demand > n_avail)
                Navail_factor = Math.Max(0.0, Math.Min(1.0, MathUtility.Divide(nitTot, n_demand - dlt_n_min_fom, 0.0)));

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

            dlt_n_min = (dlt_n_min_fom - n_demand) * Navail_factor;
        }

        FOMdecompData Result = new FOMdecompData();
        Result.dlt_c_hum = dlt_c_hum;
        Result.dlt_c_biom = dlt_c_biom;
        Result.dlt_c_atm = dlt_c_atm;
        Result.dlt_fom_n = dlt_fom_n;
        Result.dlt_n_min = dlt_n_min;

        return Result;
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
            double LowUrea = 0.1 * dlayer[layer] / 200;  //original value was 0.1, now it is corrected by dlayer, assuming'typical' was 20cm
            if (_urea[layer] < 0.1)
                // urea amount is too small, all will be hydrolised
                result = _urea[layer];
            else
            {
                // get the soil water factor
                double swf = Math.Max(0.0, Math.Min(1.0, WF(layer, index) + 0.20));
                if (useNewFunctions)
                    swf = SoilMoistFactor(layer, index, MoistFactor_Hydrol);

                // get the soil temperature factor
                double stf = Math.Max(0.0, Math.Min(1.0, (st[layer] / 40.0) + 0.20));
                if (useNewFunctions)
                    stf = SoilTempFactor(layer, index, TempFactor_Hydrol);

                // note (jngh) oc & ph are not updated during simulation
                //      mep    following equation would be better written in terms of hum_C and biom_C
                //      mep    oc(layer) = (hum_C(layer) + biom_C(layer))*soiln2_fac (layer)*10000.

                // get potential fraction of urea for hydrolysis
                double ak = potHydrol_parmA +
                            potHydrol_parmB * (hum_c[layer] + biom_c[layer]) +
                            potHydrol_parmC * ph[layer] +
                            potHydrol_parmD * (hum_c[layer] + biom_c[layer]) * ph[layer];
                ak = Math.Max(potHydrol_min, Math.Min(1.0, ak));
                //change oc on eq.: double ak = Math.Max(0.25, Math.Min(1.0, -1.12 + 1.31 * (hum_c[layer] + biom_c[layer]) + 0.203 * ph[layer] - 0.155 * (hum_c[layer] + biom_c[layer]) * ph[layer]));
                //original eq.: double ak = Math.Max(0.25, Math.Min(1.0, -1.12 + 1.31 * OC_reset[layer] + 0.203 * ph[layer] - 0.155 * OC_reset[layer] * ph[layer]));

                //get amount hydrolysed;
                result = Math.Max(0.0, Math.Min(_urea[layer], ak * _urea[layer] * Math.Min(swf, stf)));
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
        if (useNewFunctions)
            phf = SoilpHFactor(layer, index, pHFactor_Nitrif);

        // get the soil  water factor
        double wfd = WFNitrf(layer, index);
        if (useNewFunctions)
            wfd = SoilMoistFactor(layer, index, MoistFactor_Nitrif);

        // get the soil temperature factor
        double tf = TF(layer, index);
        if (useNewFunctions)
            tf = SoilTempFactor(layer, index, TempFactor_Nitrif);

        // calculate the optimum nitrification rate (ppm)
        double nh4_ppm = _nh4[layer] * convFactor_kgha2ppm(layer);
        double opt_nitrif_rate_ppm = MathUtility.Divide(nitrification_pot * nh4_ppm, nh4_ppm + nh4_at_half_pot, 0.0);

        // calculate the optimum nitrification rate (kgN/ha)
        double opt_nitrif_rate = MathUtility.Divide(opt_nitrif_rate_ppm, convFactor_kgha2ppm(layer), 0.0);

        // calculate the theoretical nitrification rate (after limiting factor and inhibition)
        double theor_nitrif_rate = opt_nitrif_rate * Math.Min(wfd, Math.Min(tf, phf)) * Math.Max(0.0, 1.0 - nitrification_inhibition[layer]);
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

        // make sure no3 will not go below minimum
        if (_no3[layer] < no3_min[layer])
            return 0.0;


        // get available carbon from soil organic pools
        double active_c = actC_parmA * (hum_c[layer] + fom_c_pool1[layer] + fom_c_pool2[layer] + fom_c_pool3[layer]) * convFactor_kgha2ppm(layer) + actC_parmB;
        // Note CM V2 had active_c = fom_C_conc + 0.0031*hum_C_conc + 24.5

        int index = 0; // denitrification calcs are not different whether there is pond or not. use 1 as default
        // get the soil water factor
        double wf = WFDenit(layer);
        if (useNewFunctions)
            wf = SoilMoistFactor(layer, index, MoistFactor_Denit);

        // get the soil temperature factor
        double tf = Math.Max(0.0, Math.Min(1.0, 0.1 * Math.Exp(0.046 * st[layer])));
        // This is an empirical dimensionless function to account for the effect of temperature.
        // The upper limit of 1.0 means that optimum denitrification temperature is 50 oC and above.  At 0 oC it is 0.1 of optimum, and at -20 oC is about 0.04.
        if (useNewFunctions)
            tf = SoilTempFactor(layer, index, TempFactor_Denit);

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
        double CO2_prod = (dlt_c_fom_2_atm[0][layer] + dlt_c_fom_2_atm[1][layer] + dlt_c_fom_2_atm[2][layer] + dlt_c_biom_2_atm[layer] + dlt_c_hum_2_atm[layer]);

        // calculate the terms for the formula from Thornburn et al (2010)
        double RtermA = N2N2O_parmA * dnit_k1;
        double RtermB = 0.0;
        if (CO2_prod > 0.0)
            RtermB = dnit_k1 * Math.Exp(-N2N2O_parmB * (_no3[layer] / CO2_prod));
        double RtermC = N2N2O_parmC;
        bool didInterpolate;
        double RtermD = MathUtility.LinearInterpReal(WFPS, dnit_wfps, dnit_n2o_factor, out didInterpolate);
        // RTermD = (0.015 * WFPS) - 0.32;

        double result = Math.Max(RtermA, RtermB) * Math.Max(RtermC, RtermD);

        return result;
    }

    private void PackActualResidueDecomposition()
    {
        // + Purpose
        //     Send back the information about actual decomposition
        //      Potential decomposition was given to this module by a residue/surfaceOM module.  Now we explicitly tell the module the actual decomposition
        //      rate for each of its residues.  If there wasn't enough mineral N to decompose, the rate will be reduced from the potential value.

        int nLayers = dlayer.Length;

        soilp_dlt_res_c_atm = new double[nLayers];
        soilp_dlt_res_c_hum = new double[nLayers];
        soilp_dlt_res_c_biom = new double[nLayers];
        soilp_dlt_org_p = new double[nLayers];
        double soilp_cpr = MathUtility.Divide(SumDoubleArray(pot_p_decomp), SumDoubleArray(pot_c_decomp), 0.0);  // P:C ratio for potential decomposition

        //SurfaceOrganicMatterDecompType SOMDecomp = new SurfaceOrganicMatterDecompType();
        SOMDecomp = new SurfaceOrganicMatterDecompType();
        Array.Resize(ref SOMDecomp.Pool, num_residues);


        for (int residue = 0; residue < num_residues; residue++)
        {
            double c_summed = 0.0;
            double n_summed = 0.0;
            double[] dlt_res_c_decomp = new double[nLayers];
            double[] dlt_res_n_decomp = new double[nLayers];
            for (int layer = 0; layer < nLayers; layer++)
            {
                dlt_res_c_decomp[layer] = dlt_c_res_2_hum[layer][residue] +
                                          dlt_c_res_2_biom[layer][residue] +
                                          dlt_c_res_2_atm[layer][residue];
                c_summed += dlt_res_c_decomp[layer];

                //dlt_res_n_decomp[layer] = this.dlt_n_decomp[layer][residue];
                dlt_res_n_decomp[layer] = dlt_n_decomp[layer][residue];
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
            //  create some variables which soilp can "get" - layer based arrays independent of residues
            for (int layer = 0; layer < nLayers; layer++)
            {
                soilp_dlt_res_c_atm[layer] += dlt_c_res_2_atm[layer][residue];
                soilp_dlt_res_c_hum[layer] += dlt_c_res_2_hum[layer][residue];
                soilp_dlt_res_c_biom[layer] += dlt_c_res_2_biom[layer][residue];
                soilp_dlt_org_p[layer] += dlt_res_c_decomp[layer] * soilp_cpr;
            }
        }
    }
    
    #endregion

    #region Frequent and sporadic processes

    public void OnIncorpFOM(FOMLayerType FOMdata)
    {
        // +  Purpose:
        //      Partition the given FOM C and N into fractions in each layer.
        //      It will be assumed that the CN ratios of all fractions are equal

        bool nSpecified = false;
        for (int layer = 0; layer < FOMdata.Layer.Length; layer++)
        {
            // If the caller specified CNR values then use them to calculate N from Amount.
            if (FOMdata.Layer[layer].CNR > 0.0)
                FOMdata.Layer[layer].FOM.N = (FOMdata.Layer[layer].FOM.amount * (float)c_in_fom) / FOMdata.Layer[layer].CNR;
            // Was any N specified?
            nSpecified |= FOMdata.Layer[layer].FOM.N != 0.0;
        }

        if (nSpecified)
        {
            fom_type = 0; // use as default if fom type not found
            for (int i = 0; i < fom_types.Length; i++)
            {
                if (fom_types[i] == FOMdata.Type)
                {
                    fom_type = i;
                    break;
                }
            }
            // Now convert the IncorpFOM.DeltaWt and IncorpFOM.DeltaN arrays to include fraction information and add to pools.
            // int nLayers = FOMdata.Layer.Length;
            //if (nLayers > dlayer.Length)
            //{
            //    Array.Resize(ref dlayer, nLayers);        -  this is wrong, dlayer should not be reset by this module
            //    ResizeLayerArrays(nLayers);
            //}
            //for (int layer = 0; layer < nLayers; layer++)

            for (int layer = 0; layer < FOMdata.Layer.Length; layer++)
            {
                if (layer < dlayer.Length)
                {
                    fom_c_pool1[layer] += FOMdata.Layer[layer].FOM.amount * fract_carb[fom_type] * c_in_fom;
                    fom_c_pool2[layer] += FOMdata.Layer[layer].FOM.amount * fract_cell[fom_type] * c_in_fom;
                    fom_c_pool3[layer] += FOMdata.Layer[layer].FOM.amount * fract_lign[fom_type] * c_in_fom;

                    fom_n_pool1[layer] += FOMdata.Layer[layer].FOM.N * fract_carb[fom_type];
                    fom_n_pool2[layer] += FOMdata.Layer[layer].FOM.N * fract_cell[fom_type];
                    fom_n_pool3[layer] += FOMdata.Layer[layer].FOM.N * fract_lign[fom_type];
                }
                else
                    Console.WriteLine(" Number of FOM values given is larger than the number of layers, estra values will be ignored");
            }
        }
    }

    public void OnIncorpFOMPool(FOMPoolType FOMPoolData)
    {
        // +  Purpose:
        //      Partition the given FOM C and N into fractions in each layer.

        for (int layer = 0; layer < FOMPoolData.Layer.Length; layer++)
        {
            if (layer < dlayer.Length)
            {
                fom_c_pool1[layer] += FOMPoolData.Layer[layer].Pool[0].C;
                fom_c_pool2[layer] += FOMPoolData.Layer[layer].Pool[1].C;
                fom_c_pool3[layer] += FOMPoolData.Layer[layer].Pool[2].C;

                fom_n_pool1[layer] += FOMPoolData.Layer[layer].Pool[0].N;
                fom_n_pool2[layer] += FOMPoolData.Layer[layer].Pool[1].N;
                fom_n_pool3[layer] += FOMPoolData.Layer[layer].Pool[2].N;

                _no3[layer] += FOMPoolData.Layer[layer].no3;
                _nh4[layer] += FOMPoolData.Layer[layer].nh4;
            }
            else
                Console.WriteLine(" Number of FOM values given is larger than the number of layers, estra values will be ignored");
        }
    }

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
        for (int layer = 0; layer < dlt_c_res_2_biom.Length; layer++)
        {
            Array.Resize(ref dlt_c_res_2_biom[layer], num_residues);
            Array.Resize(ref dlt_c_res_2_hum[layer], num_residues);
            Array.Resize(ref dlt_c_res_2_atm[layer], num_residues);
            Array.Resize(ref dlt_c_decomp[layer], num_residues);
            Array.Resize(ref dlt_n_decomp[layer], num_residues);
        }

        for (int residue = 0; residue < num_residues; residue++)
        {
            residue_name[residue] = SurfaceOrganicMatterDecomp.Pool[residue].Name;
            residue_type[residue] = SurfaceOrganicMatterDecomp.Pool[residue].OrganicMatterType;
            pot_c_decomp[residue] = SurfaceOrganicMatterDecomp.Pool[residue].FOM.C;
            pot_n_decomp[residue] = SurfaceOrganicMatterDecomp.Pool[residue].FOM.N;
            pot_p_decomp[residue] = SurfaceOrganicMatterDecomp.Pool[residue].FOM.P;
        }
    }

    public void OnNew_profile(NewProfileType NewProfile)
    {
        //+  Purpose
        //     Consider soil profile changes - primarily due to by erosion (??)

        bd = NewProfile.bd;
        sat_dep = NewProfile.dul_dep;
        dul_dep = NewProfile.dul_dep;
        ll15_dep = NewProfile.ll15_dep;
        sw_dep = NewProfile.sw_dep;

        CheckProfile(NewProfile.dlayer);
    }

    public void ResizeLayerArrays(int nLayers)
    {
        // +  Purpose:
        //      Set the size of all public arrays (with nLayers), this doesn't clear the existing values

        Array.Resize(ref st, nLayers);
        Array.Resize(ref _nh4, nLayers);
        Array.Resize(ref _no3, nLayers);
        Array.Resize(ref _urea, nLayers);
        Array.Resize(ref urea_min, nLayers);
        Array.Resize(ref nh4_min, nLayers);
        Array.Resize(ref no3_min, nLayers);
        Array.Resize(ref no3_yesterday, nLayers);
        Array.Resize(ref nh4_yesterday, nLayers);
        Array.Resize(ref fbiom, nLayers);
        Array.Resize(ref finert, nLayers);
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
        Array.Resize(ref nitrification_inhibition, nLayers);
        Array.Resize(ref nh4_transform_net, nLayers);
        Array.Resize(ref no3_transform_net, nLayers);
        Array.Resize(ref dlt_nh4_net, nLayers);
        Array.Resize(ref dlt_no3_net, nLayers);
        Array.Resize(ref dlt_c_hum_2_atm, nLayers);
        Array.Resize(ref dlt_c_biom_2_atm, nLayers);
        for (int i = 0; i < 3; i++)
        {
            Array.Resize(ref dlt_c_fom_2_biom[i], nLayers);
            Array.Resize(ref dlt_c_fom_2_hum[i], nLayers);
            Array.Resize(ref dlt_c_fom_2_atm[i], nLayers);
        }
        Array.Resize(ref dlt_c_res_2_biom, nLayers);
        Array.Resize(ref dlt_c_res_2_hum, nLayers);
        Array.Resize(ref dlt_c_res_2_atm, nLayers);
        Array.Resize(ref dlt_c_decomp, nLayers);
        Array.Resize(ref dlt_n_decomp, nLayers);
        Array.Resize(ref dlt_nitrification, nLayers);
        Array.Resize(ref effective_nitrification, nLayers);
        Array.Resize(ref dlt_urea_hydrolised, nLayers);
        Array.Resize(ref nh4_deficit_immob, nLayers);
        Array.Resize(ref dlt_n_fom_2_min, nLayers);
        Array.Resize(ref dlt_n_biom_2_min, nLayers);
        Array.Resize(ref dlt_n_hum_2_min, nLayers);
        Array.Resize(ref dlt_fom_c_pool1, nLayers);
        Array.Resize(ref dlt_fom_c_pool2, nLayers);
        Array.Resize(ref dlt_fom_c_pool3, nLayers);
        Array.Resize(ref dlt_no3_decomp, nLayers);
        Array.Resize(ref dlt_nh4_decomp, nLayers);
        Array.Resize(ref dlt_no3_dnit, nLayers);
        Array.Resize(ref dlt_nh4_dnit, nLayers);
        Array.Resize(ref n2o_atm, nLayers);
        Array.Resize(ref dlt_c_hum_2_biom, nLayers);
        Array.Resize(ref dlt_c_biom_2_hum, nLayers);
    }

    private void CheckProfile(float[] newProfile)
    {
        // + Purpose
        //     Check whether profile has changed and move values between layers

        // How to decide:
        // if bedrock is lower than lowest  profile depth, we won't see
        // any change in profile, even if there is erosion. Ideally we
        // should test both soil_loss and dlayer for changes to cater for
        // manager control. But, the latter means we have to fudge enr for the
        // loss from top layer.

        dlt_n_loss_in_sed = 0.0;
        dlt_c_loss_in_sed = 0.0;
        if (soil_loss > 0.0 && AllowProfileReduction)
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

    private double MoveLayers(ref double[] variable, float[] newProfile)
    {
        // + Purpose
        //     Move the values of a given varible between layers, from bottom to top
        //      Changed from subroutine to function returning amount of profile loss

        double profile_loss = 0.0;
        double layer_loss = 0.0;
        double layer_gain = 0.0;
        int lowest_layer = dlayer.Length;
        int new_lowest_layer = newProfile.Length;

        double yesterdays_n = SumDoubleArray(variable);

        // initialise layer loss from below profile same as bottom layer

        double profile_depth = SumFloatArray(dlayer);
        double new_profile_depth = SumFloatArray(newProfile);

        if (MathUtility.FloatsAreEqual(profile_depth, new_profile_depth))
        {
            // move from below bottom layer - assume it has same properties as bottom layer
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
    
    #endregion

    #region Factor's calculation

    private double LayerFract(int layer)
    {
        // + Purpose
        //     Calculate 
        
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

    #region Original factors

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
            // assumes rate of mineralisation is at optimum rate until soil moisture midway between dul and ll15
            wfd = MathUtility.Divide(sw_dep[layer] - ll15_dep[layer], dul_dep[layer] - ll15_dep[layer], 0.0);
            wfd = Math.Max(0.0, Math.Min(1.0, wfd));
        }

        bool didInterpolate;
        if (index == 1)
            return MathUtility.LinearInterpReal(wfd, wfnit_index, wfnit_values, out didInterpolate);
        else
            // if pond is active, and aerobic conditions dominate, assume wf_nitrf = 0
            return 0.0;
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
            // assumes rate of mineralisation is at optimum rate until soil moisture midway between dul and ll15
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

    #endregion

    #region New Factors

    private double SoilTempFactor(int layer, int index, TempFactorData Parameters)
    {
        // + Purpose
        //     Calculate a temperature factor for C and N processes

        // + Assumptions
        //     index = 0 for aerobic conditions, 1 for anaerobic

        index -= 1;  // use this untill can change the whole code. (index used to be [1-2]
        if (index > Parameters.TempOptimum.Length - 1)
            throw new Exception("SoilNitrogen.SoilTempFactor - invalid value for \"index\" parameter");

        double Toptimum = Parameters.TempOptimum[index];
        double Fzero = Parameters.FactorAtZero[index];
        double CurveN = Parameters.CurveExponent[index];
        double AuxV = Math.Pow(Fzero, 1 / CurveN);
        double Tzero = Toptimum * AuxV / (AuxV - 1);
        double beta = 1 / (Toptimum - Tzero);

        return Math.Min(1.0, Math.Pow(beta * Math.Max(0.0, st[layer] - Tzero), CurveN));
    }

    private double SoilMoistFactor(int layer, int index, XYData Parameters)
    {
        // + Purpose
        //     Calculate a soil moist factor for C and N processes

        // + Assumptions
        //     index = 0 for aerobic conditions, 1 for anaerobic

        index -= 1;  // use this untill can change the whole code. (index used to be [1-2]
        if (index == 0)
        {
            bool didInterpolate;

            // get the modified soil water variable
            double[] yVals = { 0.0, 1.0, 2.0, 3.0 };
            double[] xVals = { 0.0, ll15_dep[layer], dul_dep[layer], sat_dep[layer] };
            double myX = MathUtility.LinearInterpReal(sw_dep[layer], xVals, yVals, out didInterpolate);

            // get the soil moist factor
            return MathUtility.LinearInterpReal(myX, Parameters.xVals, Parameters.yVals, out didInterpolate);
        }
        else if (index == 1) // if pond is active
            return 1.0;
        else
            throw new Exception("SoilNitrogen.SoilMoistFactor - invalid value for \"index\" parameter");
    }

    private double SoilpHFactor(int layer, int index, XYData Parameters)
    {
        // + Purpose
        //     Calculate a pH factor for C and N processes

        bool DidInterpolate;
        return MathUtility.LinearInterpReal(ph[layer], Parameters.xVals, Parameters.yVals, out DidInterpolate);
    }
    
    private double CNorgFactor(int layer, int index, double OptCN, double rateCN)
    {
        // + Purpose
        //     Calculate a C:N ratio factor for C and N processes

        // get total available mineral N (kg/ha)
        double nitTot = Math.Max(0.0, (_no3[layer] - no3_min[layer]) + (_nh4[layer] - nh4_min[layer]));

        // fresh organic carbon (kg/ha)
        double fomC = fom_c_pool1[layer] + fom_c_pool2[layer] + fom_c_pool3[layer];

        // fresh organic nitrogen (kg/ha)
        double fomN = fom_n_pool1[layer] + fom_n_pool2[layer] + fom_n_pool3[layer];

        // ratio of C in fresh OM to N available for decay
        double cnr = MathUtility.Divide(fomC, fomN + nitTot, 0.0);

        return Math.Max(0.0, Math.Min(1.0, Math.Exp(-rateCN * (cnr - OptCN) / OptCN)));
    }

    #endregion

    #endregion

    #endregion

    #region Auxiliary functions

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

    private float SumFloatArray(float[] anArray)
    {
        float result = 0.0F;
        foreach (float Value in anArray)
            result += Value;
        return result;
    }

    #endregion

}

