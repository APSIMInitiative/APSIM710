using System;
using System.Reflection;
using System.Collections.Generic;
using System.Xml.Serialization;
using System.Xml;
using System.Linq;
using System.Text;
using ModelFramework;
using CSGeneral;

/// <remarks>
/// This partial class contains most of the variables and input properties of SoilNitrogen
/// </remarks>
public partial class SoilNitrogen
{
    #region Links to other modules

    /// <summary>
    /// Link to APSIM's clock (time information)
    /// </summary>
    [Link]
    public Clock Clock = null;

    /// <summary>
    /// Link to APSIM's metFile (weather data)
    /// </summary>
    [Link]
    public MetFile MetFile = null;

    /// <summary>
    /// Link to container paddock
    /// </summary>
    [Link]
    public Paddock Paddock;     // not sure why it is here

    #endregion

    #region Parameters and inputs provided by the user or APSIM

    #region Parameters used on initialisation only

    #region General setting parameters

    /// <summary>
    /// Soil parameterisation set to use
    /// </summary>
    /// <remarks>
    /// Used to determine which node of xml file will be used to overwrite some [Param]'s
    /// </remarks>
    [Param(IsOptional = true)]
    [Units("")]
    [Description("Soil parameterisation set to use")]
    public string SoilNParameterSet= "standard";

    /// <summary>
    /// Indicates whether simpleSoilTemp is allowed
    /// </summary>
    /// <remarks>
    /// When 'yes', soil temperature may be computed internally, if an external value is not supplied.
    /// If 'no', a value for soil temperature must be supplied or an fatal error will occur.
    /// </remarks>
    [Param]
    [Units("yes/no")]
    [Description("Indicates whether simpleSoilTemp is allowed")]
    public string allowSimpleSoilTemp
    {
        get { return (SimpleSoilTempAllowed) ? "yes" : "no"; }
        set { SimpleSoilTempAllowed = value.ToLower().Contains("yes"); }
    }

    /// <summary>
    /// Indicates whether soil profile reduction is allowed ('on')
    /// </summary>
    [Param]
    [Units("yes/no")]
    [Description("Indicates whether soil profile reduction is allowed")]
    public string allowProfileReduction
    {
        get { return (ProfileReductionAllowed) ? "yes" : "no"; }
        set { ProfileReductionAllowed = value.ToLower().Contains("yes"); }
    }

    /// <summary>
    /// Indicates whether organic solutes are to be simulated ('on')
    /// </summary>
    /// <remarks>
    /// It should always be false, as organic solutes are not implemented yet
    /// </remarks>
    [Param(IsOptional = true)]
    [Units("yes/no")]
    [Description("Indicates whether organic solutes are to be simulated")]
    public string allowOrganicSolutes
    {
        get { return (OrganicSolutesAllowed) ? "yes" : "no"; }
        set { OrganicSolutesAllowed = value.ToLower().Contains("yes"); }
    }

    /// <summary>
    /// Factor to convert from OC to OM
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 3.0)]
    [Units("")]
    [Description("Factor to convert from OC to OM")]
    public double defaultCarbonInSoilOM;

    /// <summary>
    /// Default weight fraction of C in carbohydrates
    /// </summary>
    /// <remarks>
    /// Used to convert FOM amount into fom_c
    /// </remarks>
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Weight fraction of C in FOM")]
    public double defaultCarbonInFOM = 0.4;

    /// <summary>
    /// Default value for initialising soil pH
    /// </summary>
    [Param()]
    [Units("")]
    [Description("Default value for pH at initialisation")]
    public double defaultInitialpH;

    /// <summary>
    /// Threshold value to trigger a warning message when negative values are detected
    /// </summary>
    [Param()]
    [Units("")]
    [Description("Threshold value to trigger a warning message when negative values are detected")]
    public double WarningNegativeThreshold;

    /// <summary>
    /// Threshold value to trigger a fatal error when negative values are detected
    /// </summary>
    [Param()]
    [Units("")]
    [Description("Threshold value to trigger a fatal error when negative values are detected")]
    public double FatalNegativeThreshold;

    #endregion general settings

    #region Parameters for setting up soil organic matter

    /// <summary>
    /// The C:N ratio of the soil humus (active + inert)
    /// </summary>
    /// <remarks>
    /// Remains fixed throughout the simulation
    /// </remarks>
    [Param(MinVal = 1.0, MaxVal = 25.0)]
    [Units("")]
    [Description("The C:N ratio of the soil OM (humus)")]
    public double HumusCNr = 0.0;

    /// <summary>
    /// The C:N ratio of microbial biomass
    /// </summary>
    /// <remarks>
    /// Remains fixed throughout the simulation
    /// </remarks>
    [Param(IsOptional = true, MinVal = 1.0, MaxVal = 50.0)]
    [Units("")]
    [Description("The C:N ratio of microbial biomass")]
    public double MBiomassCNr = 8.0;

    /// <summary>
    /// Proportion of biomass-C in the initial mineralizable humic-C (0-1)
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Fraction of biomass in the active humus")]
    public double[] fbiom;

    /// <summary>
    /// Proportion of the initial total soil C that is inert, not subject to mineralisation (0-1)
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Fraction humus that is inert")]
    public double[] finert;

    #endregion params for OM setup

    #region Parameters for setting up fresh organic matter (FOM)

    /// <summary>
    /// Initial amount of FOM in the soil (kgDM/ha)
    /// </summary>
    [Input(IsOptional = true)]
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 100000.0)]
    [Units("kg/ha")]
    [Description("Initial amount of FOM in the soil")]
    public double InitialFOMAmount = 0.0;

    /// <summary>
    /// Initial depth over which FOM is distributed within the soil profile (mm)
    /// </summary>
    /// <remarks>
    /// If not given fom will be distributed over the whole soil profile
    /// Distribution follows an exponential function
    /// </remarks>
    [Input(IsOptional = true)]
    [Param(IsOptional = true)]
    [Units("mm")]
    [Description("Initial depth over which FOM is distributed in the soil")]
    public double InitialFOMDepth = -99.0;

    /// <summary>
    /// Exponent of function used to compute initial distribution of FOM in the soil
    /// </summary>
    /// <remarks>
    /// If not given, a default value  might be considered (3.0)
    /// </remarks>
    [Param(IsOptional = true, MinVal = 0.01, MaxVal = 10.0)]
    [Units("")]
    [Description("Exponent for the FOM distribution in soil")]
    public double FOMDistributionCoefficient = 3.0;

    /// <summary>
    /// Initial C:N ratio of roots (actually FOM)
    /// </summary>
    /// <remarks>
    /// This may not be used if values for each pool are given, thus it is optional
    /// however, a default value is always needed as it may happen that neither is given
    /// </remarks>
    [Param(IsOptional = true, MinVal = 0.1, MaxVal = 750.0)]
    [Units("")]
    [Description("Initial C:N ratio of FOM")]
    public double InitialCNrFOM = 40.0;

    /// <summary>
    /// FOM type to be used on initialisation
    /// </summary>
    /// <remarks>
    /// This sets the partition of FOM C between the different pools (carbohydrate, cellulose, lignine)
    /// A default value (0) is always assumed
    /// </remarks>
    private int FOMtypeID_reset = 0;
    [Param(IsOptional = true)]
    [Units("")]
    [Description("FOM type to be used on initialisation")]
    public string InitialFOMType
    {
        get { return fom_types[FOMtypeID_reset]; }
        set
        {
            FOMtypeID_reset = 0;
            for (int i = 0; i < fom_types.Length; i++)
            {
                if (fom_types[i] == value)
                {
                    FOMtypeID_reset = i;
                    break;
                }
            }
            if (InitialFOMType != fom_types[FOMtypeID_reset])
            {   // no valid FOM type was given, use default
                FOMtypeID_reset = 0;
                // let the user know that the default type will be used
                Console.WriteLine("  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
                Console.WriteLine("                 APSIM Warning Error");
                Console.WriteLine("   The initial FOM type was not found, the default type will be used");
                Console.WriteLine("  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
            }
        }
    }

    /// <summary>
    /// List of available FOM types names
    /// </summary>
    [Param(Name = "fom_type")]
    [XmlArray("fom_type")]
    [Description("List of available FOM types names")]
    public String[] fom_types;

    /// <summary>
    /// Fraction of carbohydrate in FOM (0-1), for each FOM type
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Fraction of carbohydrate in FOM, for each FOM type")]
    public double[] fract_carb;

    /// <summary>
    /// Fraction of cellulose in FOM (0-1), for each FOM type
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Fraction of cellulose in FOM, for each FOM type")]
    public double[] fract_cell;

    /// <summary>
    /// Fraction of lignin in FOM (0-1), for each FOM type
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Fraction of lignin in FOM, for each FOM type")]
    public double[] fract_lign;

    #endregion  params for FOM setup

    #region Parameters for the decomposition process of FOM and SurfaceOM

    #region Surface OM

    /// <summary>
    /// Fraction of residue C mineralised retained in the soil OM (0-1)
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Fraction of residue C mineralised retained in the soil OM")]
    public double ResiduesRespirationFactor;

    /// <summary>
    /// Fraction of retained residue C transferred to biomass (0-1)
    /// </summary>
    /// <remarks>
    /// Remaining will got into humus
    /// </remarks>
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Fraction of retained residue C transferred to biomass")]
    public double ResiduesFractionIntoBiomass;

    /// <summary>
    /// Depth from which mineral N can be immobilised when decomposing surface residues (mm)
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 1000.0)]
    [Units("mm")]
    [Description("Depth from which mineral N can be immobilised when decomposing surface residues")]
    public double ImmobilisationDepth;

    #endregion

    #region Fresh OM

    /// <summary>
    /// Optimum rate for decomposition of FOM pools [carbohydrate component] (0-1)
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Optimum decomposition rate of FOM carbohydrate")]
    public double[] FOMCarbTurnOverRate;

    /// <summary>
    /// Optimum rate for decomposition of FOM pools [cellulose component] (0-1)
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Optimum decomposition rate of FOM cellulose")]
    public double[] FOMCellTurnOverRate;

    /// <summary>
    /// Optimum rate for decomposition of FOM pools [lignin component] (0-1)
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Optimum decomposition rate of FOM lignine")]
    public double[] FOMLignTurnOverRate;

    /// <summary>
    /// Fraction of the FOM C decomposed retained in the soil OM (0-1)
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Fraction of the FOM C decomposed retained in the soil OM")]
    public double FOMRespirationFactor;

    /// <summary>
    /// Fraction of the retained FOM C transferred to biomass (0-1)
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Fraction of the retained FOM C transferred to biomass")]
    public double FOMFractionIntoBiomass;

    #region Limiting factors

    /// <summary>
    /// Coeff. to determine the magnitude of C:N effects on decomposition of FOM
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 10.0)]
    public double cnrf_ReductionCoeff;

    /// <summary>
    /// C:N above which decomposition rate of FOM declines
    /// </summary>
    [Param(MinVal = 5.0, MaxVal = 100.0)]
    public double cnrf_CNthreshold;

    /// <summary>
    /// Data for calculating the temperature effect on FOM decomposition
    /// </summary>
    private BentStickData TempFactorData_DecompFOM = new BentStickData();

    /// <summary>
    /// Optimum temperature for decomposition of FOM
    /// </summary>
    [Param]
    [Units("oC")]
    [Description("Optimum temperature for decomposition of FOM")]
    public double[] stf_DecompFOM_Topt
    {
        get { return TempFactorData_DecompFOM.xValueForOptimum; }
        set { TempFactorData_DecompFOM.xValueForOptimum = value; }
    }

    /// <summary>
    /// Temperature factor for decomposition of FOM at zero degrees
    /// </summary>
    [Param]
    [Units("0-1")]
    [Description("Temperature factor for decomposition of FOM at zero degrees")]
    public double[] stf_DecompFOM_FctrZero
    {
        get { return TempFactorData_DecompFOM.yValueAtZero; }
        set { TempFactorData_DecompFOM.yValueAtZero = value; }
    }

    /// <summary>
    /// Curve exponent for temperature factor for decomposition of FOM
    /// </summary>
    [Param]
    [Units("")]
    [Description("Curve exponent for temperature factor")]
    public double[] stf_DecompFOM_CvExp
    {
        get { return TempFactorData_DecompFOM.CurveExponent; }
        set { TempFactorData_DecompFOM.CurveExponent = value; }
    }

    /// <summary>
    /// Parameters for calculating the soil moisture factor for FOM decomposition
    /// </summary>
    private BrokenStickData MoistFactorData_DecompFOM = new BrokenStickData();

    /// <summary>
    /// Values of modified soil water content at which the moisture factor is given
    /// </summary>
    [Param]
    [Units("0-3")]
    [Description("X values for the moisture factor function")]
    public double[] swf_DecompFOM_swx
    { set { MoistFactorData_DecompFOM.xVals = value; } }

    /// <summary>
    /// Moiture factor values for the given modified soil water content
    /// </summary>
    [Param]
    [Units("0-1")]
    [Description("Y values for the moisture factor function")]
    public double[] swf_DecompFOM_y
    { set { MoistFactorData_DecompFOM.yVals = value; } }

    #endregion

    #endregion FOM

    #endregion params for SurfOM + FOM decompostion

    #region Parameters for SOM mineralisation/immobilisation process

    /// <summary>
    /// Potential rate of soil biomass mineralisation (fraction per day)
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Potential rate of soil biomass mineralisation")]
    public double[] MBiomassTurnOverRate;

    /// <summary>
    /// Fraction of biomass C mineralised retained in soil OM (0-1)
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Fraction of biomass C mineralised retained in soil OM")]
    public double MBiomassRespirationFactor;

    /// <summary>
    /// Fraction of retained biomass C returned to biomass (0-1)
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Fraction of retained biomass C returned to biomass")]
    public double MBiomassFractionIntoBiomass;

    /// <summary>
    /// Potential rate of humus mineralisation (per day, 0-1)
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Potential rate of humus mineralisation")]
    public double[] AHumusTurnOverRate;

    /// <summary>
    /// Fraction of humic C mineralised retained in soil OM (0-1)
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Fraction of humic C mineralised retained in soil OM")]
    public double AHumusRespirationFactor;

    #region Limiting factors

    /// <summary>
    /// Data to calculate the temperature effect on soil OM mineralisation
    /// </summary>
    private BentStickData TempFactorData_MinerSOM = new BentStickData();

    /// <summary>
    /// Optimum temperature for soil OM mineralisation
    /// </summary>
    [Param]
    [Units("oC")]
    [Description("Optimum temperature for mineralisation of soil OM")]
    public double[] stf_MinerSOM_Topt
    {
        get { return TempFactorData_MinerSOM.xValueForOptimum; }
        set { TempFactorData_MinerSOM.xValueForOptimum = value; }
    }

    /// <summary>
    /// Temperature factor for soil OM mineralisation at zero degree
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Temperature factor for mineralisation of soil OM at zero degrees")]
    public double[] stf_MinerSOM_FctrZero
    {
        get { return TempFactorData_MinerSOM.yValueAtZero; }
        set { TempFactorData_MinerSOM.yValueAtZero = value; }
    }

    /// <summary>
    /// Curve exponent to calculate temperature factor for soil OM mineralisation
    /// </summary>
    [Param]
    [Units("")]
    [Description("Curve exponent for temperature factor")]
    public double[] stf_MinerSOM_CvExp
    {
        get { return TempFactorData_MinerSOM.CurveExponent; }
        set { TempFactorData_MinerSOM.CurveExponent = value; }
    }

    /// <summary>
    /// Parameters to calculate soil moisture factor for soil OM mineralisation
    /// </summary>
    /// <remarks>
    /// These are pairs of points representing a broken stick function
    /// </remarks>
    private BrokenStickData MoistFactorData_MinerSOM = new BrokenStickData();

    /// <summary>
    /// Values of the modified soil water content at which misture factor is know
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 3.0)]
    [Units("0-3")]
    [Description("X values for the moisture factor function")]
    public double[] swf_MinerSOM_swx
    {
        get { return MoistFactorData_MinerSOM.xVals; }
        set { MoistFactorData_MinerSOM.xVals = value; }
    }

    /// <summary>
    /// Values of the moisture factor at the given modified water content
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Y values for the moisture factor function")]
    public double[] swf_MinerSOM_y
    {
        get { return MoistFactorData_MinerSOM.yVals; }
        set { MoistFactorData_MinerSOM.yVals = value; }
    }

    #endregion

    #endregion params for OM decomposition

    #region Parameters for urea hydrolisys process

    /// <summary>
    /// Parameters to calculate the temperature effect on urea hydrolysis
    /// </summary>
    private BentStickData TempFactorData_UHydrol = new BentStickData();

    /// <summary>
    /// Optimum temperature for urea hydrolisys
    /// </summary>
    [Param(MinVal = 5.0, MaxVal = 100.0)]
    [Units("oC")]
    [Description("Optimum temperature for urea hydrolysis")]
    public double[] stf_Hydrol_Topt
    {
        get { return TempFactorData_UHydrol.xValueForOptimum; }
        set { TempFactorData_UHydrol.xValueForOptimum = value; }
    }

    /// <summary>
    /// Temperature factor for urea hydrolisys at zero degrees
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Temperature factor for urea hydrolisys at zero degrees")]
    public double[] stf_Hydrol_FctrZero
    {
        get { return TempFactorData_UHydrol.yValueAtZero; }
        set { TempFactorData_UHydrol.yValueAtZero = value; }
    }

    /// <summary>
    /// Curve exponent to calculate the temperature factor for urea hydrolisys
    /// </summary>
    [Param]
    [Units("")]
    [Description("Curve exponent for temperature factor")]
    public double[] stf_Hydrol_CvExp
    {
        get { return TempFactorData_UHydrol.CurveExponent; }
        set { TempFactorData_UHydrol.CurveExponent = value; }
    }

    /// <summary>
    /// Parameters to calculate the moisture effect on urea hydrolysis
    /// </summary>
    private BrokenStickData MoistFactorData_UHydrol = new BrokenStickData();

    /// <summary>
    /// Values of the modified soil water content at which factor is known
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 3.0)]
    [Units("0-3")]
    [Description("X values for the moisture factor function")]
    public double[] swf_Hydrol_swx
    {
        get { return MoistFactorData_UHydrol.xVals; }
        set { MoistFactorData_UHydrol.xVals = value; }
    }

    /// <summary>
    /// Values of the modified moisture factor at given water content
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Y values for the moisture factor function")]
    public double[] swf_Hydrol_y
    {
        get { return MoistFactorData_UHydrol.yVals; }
        set { MoistFactorData_UHydrol.yVals = value; }
    }

    /// Parameters for calculating the potential urea hydrolisys
    /// <summary>
    /// Minimum value for hydrolysis rate
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Minimum value for hydrolysis rate")]
    public double potHydrol_min;

    /// <summary>
    /// Parameter A of the function determining potential urea hydrolysis
    /// </summary>
    [Param]
    [Units("")]
    [Description("Parameter A of the potential urea hydrolysis function")]
    public double potHydrol_parmA;

    /// <summary>
    /// Parameter B of the function determining potential urea hydrolysis
    /// </summary>
    [Param]
    [Units("")]
    [Description("Parameter B of the potential urea hydrolysis function")]
    public double potHydrol_parmB;

    /// <summary>
    /// Parameter C of the function determining potential urea hydrolysis
    /// </summary>
    [Param]
    [Units("")]
    [Description("Parameter C of the potential urea hydrolysis function")]
    public double potHydrol_parmC;

    /// <summary>
    /// Parameter D of the function determining potential urea hydrolysis
    /// </summary>
    [Param]
    [Units("")]
    [Description("Parameter D of the potential urea hydrolysis function")]
    public double potHydrol_parmD;

    #endregion params for hydrolysis

    #region Parameters for nitrification process

    /// <summary>
    /// Maximum potential nitrification (ppm/day)
    /// </summary>
    /// <remarks>
    /// This is the parameter M on Michaelis-Menten equation
    /// r = MC/(k+C)
    /// </remarks>
    [Param(MinVal = 0.0, MaxVal = 100.0)]
    [Units("ppm/day")]
    [Description("Maximum potential nitrification")]
    public double nitrification_pot;

    /// <summary>
    /// NH4 concentration when nitrification rate is half of potential (ppm)
    /// </summary>
    /// <remarks>
    /// This is the parameter k on Michaelis-Menten equation
    /// r = MC/(k+C)
    /// </remarks>
    [Param(MinVal = 0.0, MaxVal = 200.0)]
    [Units("ppm")]
    [Description("NH4 concentration when nitrification rate is half of potential")]
    public double nh4_at_half_pot;

    #region Limiting factors

    /// <summary>
    /// Parameters to calculate the temperature effect on nitrification
    /// </summary>
    private BentStickData TempFactorData_Nitrif = new BentStickData();

    /// <summary>
    /// Optimum temperature for nitrification
    /// </summary>
    [Param(MinVal = 5.0, MaxVal = 100.0)]
    [Units("oC")]
    [Description("Optimum temperature for nitrification")]
    public double[] stf_Nitrif_Topt
    {
        get { return TempFactorData_Nitrif.xValueForOptimum; }
        set { TempFactorData_Nitrif.xValueForOptimum = value; }
    }

    /// <summary>
    /// Temperature factor for nitrification at zero degrees
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Temperature factor for nitrification at zero degrees")]
    public double[] stf_Nitrif_FctrZero
    {
        get { return TempFactorData_Nitrif.yValueAtZero; }
        set { TempFactorData_Nitrif.yValueAtZero = value; }
    }

    /// <summary>
    /// Curve exponent for calculating the temperature factor for nitrification
    /// </summary>
    [Param]
    [Units("")]
    [Description("Curve exponent for temperature factor")]
    public double[] stf_Nitrif_CvExp
    {
        get { return TempFactorData_Nitrif.CurveExponent; }
        set { TempFactorData_Nitrif.CurveExponent = value; }
    }

    /// <summary>
    /// Parameters to calculate the soil moisture factor for nitrification
    /// </summary>
    private BrokenStickData MoistFactorData_Nitrif = new BrokenStickData();

    /// <summary>
    /// Values of the modified soil water content at which the moisture factor is known
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 3.0)]
    [Units("0-3")]
    [Description("X values for the moisture factor function")]
    public double[] swf_Nitrif_swx
    {
        get { return MoistFactorData_Nitrif.xVals; }
        set { MoistFactorData_Nitrif.xVals = value; }
    }

    /// <summary>
    /// Values of the moisture factor at given water content
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Y values for the moisture factor function")]
    public double[] swf_Nitrif_y
    {
        get { return MoistFactorData_Nitrif.yVals; }
        set { MoistFactorData_Nitrif.yVals = value; }
    }

    /// <summary>
    /// Parameters to calculate the soil pH factor for nitrification
    /// </summary>
    private BrokenStickData pHFactorData_Nitrif = new BrokenStickData();

    /// <summary>
    /// Values of pH at which factors is known
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 14.0)]
    [Units("")]
    [Description("X values of pH factor function")]
    public double[] phf_Nitrif_phx
    {
        get { return pHFactorData_Nitrif.xVals; }
        set { pHFactorData_Nitrif.xVals = value; }
    }

    /// <summary>
    /// Values of pH factor at given pH values
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Y values of pH factor function")]
    public double[] phf_Nitrif_y
    {
        get { return pHFactorData_Nitrif.yVals; }
        set { pHFactorData_Nitrif.yVals = value; }
    }

    #endregion factors

    #endregion params for nitrification

    #region Parameters for denitrification and N2O emission processes

    /// <summary>
    /// Denitrification rate coefficient (kg/mg)
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("")]
    [Description("")]
    public double DenitRateCoefficient;

    /// <summary>
    /// Fraction of nitrification lost as denitrification
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("")]
    [Description("")]
    public double n2oLossFactor;

    /// <summary>
    /// Parameter k1 from Thorburn et al (2010) for N2O model
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 100.0)]
    [Units("")]
    [Description("")]
    public double dnit_k1;

    /// <summary>
    /// Parameter A in the N2N2O function
    /// </summary>
    [Param]
    [Units("")]
    [Description("Parameter A in the function computing the N2:N2O ratio")]
    public double N2N2O_parmA;

    /// <summary>
    /// Parameter B in the function computing the N2:N2O ratio
    /// </summary>
    [Param]
    [Units("")]
    [Description("Parameter B in the function computing the N2:N2O ratio")]
    public double N2N2O_parmB;

    #region Limiting factors

    /// <summary>
    /// Flag whether water soluble carbon is computed using newly defined pools
    /// </summary>
    /// <remarks>
    /// Classic definition uses all humus and all FOM
    /// New definition uses active humus, biomass and pool1 of FOM (carbohydrate)
    /// </remarks>
    [Param]
    [Units("")]
    [Description("")]
    public string allowNewPools
    {
        get { return (usingNewPools) ? "yes" : "no"; }
        set { usingNewPools = value.ToLower().Contains("yes"); }
    }

    /// <summary>
    /// Flag whether an exponential function is used to compute water soluble C
    /// </summary>
    /// <remarks>
    /// Classic approach is a linear function (soluble carbon is not zero when total C is zero)
    /// New exponential function is quite similar, but ensures zero soluble C
    /// </remarks>
    [Param]
    [Units("")]
    [Description("")]
    public string allowExpFunction
    {
        get { return (usingExpFunction) ? "yes" : "no"; }
        set { usingExpFunction = value.ToLower().Contains("yes"); }
    }

    /// <summary>
    /// Parameter A of linear function to compute soluble carbon (for denitrification)
    /// </summary>
    [Param]
    [Units("")]
    [Description("")]
    public double actC_parmA;

    /// <summary>
    /// Parameter B of linear function to compute soluble carbon (for denitrification)
    /// </summary>
    [Param]
    [Units("")]
    [Description("")]
    public double actC_parmB;

    /// <summary>
    /// Parameter A of exponential function to compute soluble carbon (for denitrification)
    /// </summary>
    [Param]
    [Units("")]
    [Description("")]
    public double actCExp_parmA;

    /// <summary>
    /// Parameter B of exponential function to compute soluble carbon (for denitrification)
    /// </summary>
    [Param]
    [Units("")]
    [Description("")]
    public double actCExp_parmB;

    /// <summary>
    /// Parameters to calculate the temperature effect on denitrification
    /// </summary>
    private BentStickData TempFactorData_Denit = new BentStickData();

    /// <summary>
    /// Optimum temperature for denitrification
    /// </summary>
    [Param(MinVal = 5.0, MaxVal = 100.0)]
    [Units("oC")]
    [Description("Optimum temperature for denitrification")]
    public double[] stf_dnit_Topt
    {
        get { return TempFactorData_Denit.xValueForOptimum; }
        set { TempFactorData_Denit.xValueForOptimum = value; }
    }

    /// <summary>
    /// Temperature factor for denitrification at zero degrees
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Temperature factor for denitrification at zero degrees")]
    public double[] stf_dnit_FctrZero
    {
        get { return TempFactorData_Denit.yValueAtZero; }
        set { TempFactorData_Denit.yValueAtZero = value; }
    }

    /// <summary>
    /// Curve exponent for calculating the temperature factor for denitrification
    /// </summary>
    [Param]
    [Units("")]
    [Description("Curve exponent for temperature factor")]
    public double[] stf_dnit_CvExp
    {
        get { return TempFactorData_Denit.CurveExponent; }
        set { TempFactorData_Denit.CurveExponent = value; }
    }

    /// <summary>
    /// Parameters to calculate the soil moisture factor for denitrification
    /// </summary>
    private BrokenStickData MoistFactorData_Denit = new BrokenStickData();

    /// <summary>
    /// Values of modified soil water content at which the moisture factor is known
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 3.0)]
    [Units("0-3")]
    [Description("X values for the moisture factor function")]
    public double[] swf_dnit_swx
    {
        get { return MoistFactorData_Denit.xVals; }
        set { MoistFactorData_Denit.xVals = value; }
    }

    /// <summary>
    /// Values of the moisture factor at given water content values
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Y values for the moisture factor function")]
    public double[] swf_dnit_y
    {
        get { return MoistFactorData_Denit.yVals; }
        set { MoistFactorData_Denit.yVals = value; }
    }

    /// <summary>
    /// Parameters to calculate the N2:N2O ratio during denitrification
    /// </summary>
    private BrokenStickData WFPSFactorData_Denit = new BrokenStickData();

    /// <summary>
    /// Values of soil water filled pore sapce at which the WFPS factor is known
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 100.0)]
    [Units("%")]
    [Description("X values for the WFPS factor function")]
    public double[] swpsf_dnit_swpx
    {
        get { return WFPSFactorData_Denit.xVals; }
        set { WFPSFactorData_Denit.xVals = value; }
    }

    /// <summary>
    /// Values of the WFPS factor at given water fille pore space values
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 1.0)]
    [Units("0-1")]
    [Description("Y values for the WFPS factor function")]
    public double[] swpsf_dnit_y
    {
        get { return WFPSFactorData_Denit.yVals; }
        set { WFPSFactorData_Denit.yVals = value; }
    }

    #endregion factors

    #endregion params for denitrification

    #region Parameters for handling soil loss process

    /// <summary>
    /// Coefficient a of the enrichment equation
    /// </summary>
    [Param()]
    [Units("")]
    [Description("Erosion enrichment coefficient A")]
    public double enr_a_coeff;

    /// <summary>
    /// Coefficient b of the enrichment equation
    /// </summary>
    [Param()]
    [Units("")]
    [Description("Erosion enrichment coefficient B")]
    public double enr_b_coeff;

    #endregion params for soil loss

    #endregion params for initialisation

    #region Parameters that do or may change during simulation

    #region Parameter for handling patches

    /// <summary>
    /// The approach used for partitioning the N between patches
    /// </summary>
    [Param]
    [Output]
    [Units("")]
    [Description("Approach used for partitioning N between patches")]
    public string NPartitionApproach
    {
        get { return PatchNPartitionApproach; }
        set { PatchNPartitionApproach = value; }
    }

    /// <summary>
    /// Layer thickness to consider when N partiton is BasedOnSoilConcentration (mm)
    /// </summary>
    [Param(IsOptional = true)]
    [Output]
    [Units("mm")]
    [Description("Layer thickness to use when N partiton is BasedOnSoilConcentration")]
    public double LayerNPartition = -99;

    /// <summary>
    /// Minimum relative area (fraction of paddock) for any patch
    /// </summary>
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 1.0)]
    [Output]
    [Units("0-1")]
    [Description("Minimum allowable relative area for a CNpatch")]
    public double MininumRelativeAreaCNPatch
    {
        get { return MinimumPatchArea; }
        set { MinimumPatchArea = value; }
    }

    /// <summary>
    /// Maximum NH4 uptake rate for plants (ppm/day)
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 10000.0)]
    [Output]
    [Units("ppm/day")]
    [Description("Maximum NH4 uptake rate for plants")]
    public double MaximumUptakeRateNH4
    {
        get { return reset_MaximumNH4Uptake; }
        set
        {
            reset_MaximumNH4Uptake = value;
            if (initDone)
            { // after initialisation done, the setting has to be here
                for (int layer = 0; layer < dlayer.Length; layer++)
                {
                    // set maximum uptake rates for N forms (only really used for AgPasture when patches exist)
                    MaximumNH4UptakeRate[layer] = reset_MaximumNH4Uptake / convFactor[layer];
                }
            }
        }
    }

    /// <summary>
    /// Maximum NO3 uptake rate for plants (ppm/day)
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 10000.0)]
    [Output]
    [Units("ppm/day")]
    [Description("Maximum NO3 uptake rate for plants")]
    public double MaximumUptakeRateNO3
    {
        get { return reset_MaximumNO3Uptake; }
        set { 
            reset_MaximumNO3Uptake = value; 
            if (initDone)
            { // after initialisation done, the setting has to be here
                for (int layer = 0; layer < dlayer.Length; layer++)
                {
                    // set maximum uptake rates for N forms (only really used for AgPasture when patches exist)
                    MaximumNO3UptakeRate[layer] = reset_MaximumNO3Uptake / convFactor[layer];
                }
            }
        }
    }

    /// <summary>
    /// The maximum amount of N that is made available to plants in one day (kg/ha/day)
    /// </summary>
    [Param(MinVal = 0.0, MaxVal = 10000.0)]
    [Output]
    [Units("kg/ha/day")]
    [Description("Maximum amount of N that is made available to plants in one day")]
    public double MaximumNitrogenAvailableToPlants
    {
        get { return maxTotalNAvailableToPlants; }
        set
        {
            maxTotalNAvailableToPlants = value;
        }
    }

    #region Parameter for amalgamating patches

    /// <summary>
    /// whether auto amalgamation of CN patches is allowed (yes/no)
    /// </summary>
    [Param]
    [Output]
    [Units("yes/no")]
    [Description("whether auto amalgamation of CN patches is allowed")]
    public string allowPatchAmalgamation
    {
        get { return (PatchAmalgamationAllowed) ? "yes" : "no"; }
        set { PatchAmalgamationAllowed = value.ToLower().Contains("yes"); }
    }

    /// <summary>
    /// Approach to use when comparing patches for AutoAmalagamation
    /// </summary>
    /// <remarks>
    /// Options:
    ///  - CompareAll: All patches are compared before they are merged
    ///  - CompareBase: All patches are compare to base first, then merged, then compared again
    ///  - CompareMerge: Patches are compare and merged at once if deemed equal, then compare to next
    /// </remarks>
    [Param]
    [Output]
    [Units("")]
    public string AutoAmalgamationApproach
    {
        get { return PatchAmalgamationApproach; }
        set { PatchAmalgamationApproach = value.Trim(); }
    }

    /// <summary>
    /// Approach to use when defining the base patch
    /// </summary>
    /// <remarks>
    /// This is used to define the patch considered the 'base'. It is only used when comparing patches during
    /// potential auto-amalgamation (comparison against base are more lax)
    /// Options:
    ///  - IDBased: the patch with lowest ID (=0) is used as the base
    ///  - AreaBased: The [first] patch with the biggest area is used as base
    /// </remarks>
    [Param]
    [Output]
    [Units("")]
    public string basePatchApproach
    {
        get { return PatchbasePatchApproach; }
        set { PatchbasePatchApproach = value.Trim(); }
    }

    /// <summary>
    /// Patch age for forced merging (years)
    /// </summary>
    [Param]
    [Units("years")]
    [Description("Relative difference in total organic carbon")]
    public double ageForForcedMerge;

    /// <summary>
    /// Relative difference in total organic carbon (0-1)
    /// </summary>
    [Param]
    [Units("0-1")]
    [Description("Relative difference in total organic carbon")]
    public double relativeDiff_TotalOrgC;

    /// <summary>
    /// Relative difference in total organic nitrogen (0-1)
    /// </summary>
    [Param]
    [Units("0-1")]
    [Description("Relative difference in total organic nitrogen")]
    public double relativeDiff_TotalOrgN;

    /// <summary>
    /// Relative difference in total organic nitrogen (0-1)
    /// </summary>
    [Param]
    [Units("0-1")]
    [Description("Relative difference in total organic nitrogen")]
    public double relativeDiff_TotalBiomC;

    /// <summary>
    /// Relative difference in total urea N amount (0-1)
    /// </summary>
    [Param]
    [Units("0-1")]
    [Description("Relative difference in total urea N amount")]
    public double relativeDiff_TotalUrea;

    /// <summary>
    /// Relative difference in total NH4 N amount (0-1)
    /// </summary>
    [Param]
    [Units("0-1")]
    [Description("Relative difference in total NH4 N amount")]
    public double relativeDiff_TotalNH4;

    /// <summary>
    /// Relative difference in total NO3 N amount (0-1)
    /// </summary>
    [Param]
    [Units("0-1")]
    [Description("Relative difference in total NO3 N amount")]
    public double relativeDiff_TotalNO3;

    /// <summary>
    /// Relative difference in urea N amount at any layer (0-1)
    /// </summary>
    [Param]
    [Units("0-1")]
    [Description("Relative difference in urea N amount at any layer")]
    public double relativeDiff_LayerBiomC;

    /// <summary>
    /// Relative difference in urea N amount at any layer (0-1)
    /// </summary>
    [Param]
    [Units("0-1")]
    [Description("Relative difference in urea N amount at any layer")]
    public double relativeDiff_LayerUrea;

    /// <summary>
    /// Relative difference in NH4 N amount at any layer (0-1)
    /// </summary>
    [Param]
    [Units("0-1")]
    [Description("Relative difference in NH4 N amount at any layer")]
    public double relativeDiff_LayerNH4;

    /// <summary>
    /// Relative difference in NO3 N amount at any layer (0-1)
    /// </summary>
    [Param]
    [Units("0-1")]
    [Description("Relative difference in NO3 N amount at any layer")]
    public double relativeDiff_LayerNO3;

    /// <summary>
    /// Absolute difference in total organic carbon (kg/ha)
    /// </summary>
    [Param]
    [Units("kg/ha")]
    [Description("Absolute difference in total organic carbon")]
    public double absoluteDiff_TotalOrgC;

    /// <summary>
    /// Absolute difference in total organic nitrogen (kg/ha)
    /// </summary>
    [Param]
    [Units("kg/ha")]
    [Description("Absolute difference in total organic nitrogen")]
    public double absoluteDiff_TotalOrgN;

    /// <summary>
    /// Absolute difference in total organic nitrogen (kg/ha)
    /// </summary>
    [Param]
    [Units("kg/ha")]
    [Description("Absolute difference in total organic nitrogen")]
    public double absoluteDiff_TotalBiomC;

    /// <summary>
    /// Absolute difference in total urea N amount (kg/ha)
    /// </summary>
    [Param]
    [Units("kg/ha")]
    [Description("Absolute difference in total urea N amount")]
    public double absoluteDiff_TotalUrea;
    /// <summary>
    /// Absolute difference in total NH4 N amount (kg/ha)
    /// </summary>
    [Param]
    [Units("kg/ha")]
    [Description("Absolute difference in total NH4 N amount")]
    public double absoluteDiff_TotalNH4;

    /// <summary>
    /// Absolute difference in total NO3 N amount (kg/ha)
    /// </summary>
    [Param]
    [Units("kg/ha")]
    [Description("Absolute difference in total NO3 N amount")]
    public double absoluteDiff_TotalNO3;

    /// <summary>
    /// Absolute difference in urea N amount at any layer (kg/ha)
    /// </summary>
    [Param]
    [Units("kg/ha")]
    [Description("Absolute difference in urea N amount at any layer")]
    public double absoluteDiff_LayerBiomC;

    /// <summary>
    /// Absolute difference in urea N amount at any layer (kg/ha)
    /// </summary>
    [Param]
    [Units("kg/ha")]
    [Description("Absolute difference in urea N amount at any layer")]
    public double absoluteDiff_LayerUrea;

    /// <summary>
    /// Absolute difference in NH4 N amount at any layer (kg/ha)
    /// </summary>
    [Param]
    [Units("kg/ha")]
    [Description("Absolute difference in NH4 N amount at any layer")]
    public double absoluteDiff_LayerNH4;

    /// <summary>
    /// Absolute difference in NO3 N amount at any layer (kg/ha)
    /// </summary>
    [Param]
    [Units("kg/ha")]
    [Description("Absolute difference in NO3 N amount at any layer")]
    public double absoluteDiff_LayerNO3;

    /// <summary>
    /// Depth to consider when testing diffs by layer, if -ve soil depth is used (mm)
    /// </summary>
    [Param]
    [Units("mm")]
    [Description("Depth to consider when testing diffs by layer, if -ve soil depth is used")]
    public double DepthToTestByLayer;

    /// <summary>
    /// Factor to adjust the tests between patches other than base (0-1)
    /// </summary>
    [Param]
    [Units("0-1")]
    [Description("factor to adjust the tests between patches other than base")]
    public double DiffAdjustFactor;

    #endregion amalgamating patches

    #endregion

    #region Soil physics data

    /// <summary>
    /// Today's soil water amount (mm)
    /// </summary>
    [Input]
    [Units("mm")]
    [Description("Soil water amount")]
    private double[] sw_dep;

    /// <summary>
    /// Soil albedo (0-1)
    /// </summary>
    [Input]
    [Units("0-1")]
    [Description("Soil albedo")]
    private double salb;

    /// <summary>
    /// Soil temperature (oC), as computed by an external module (SoilTemp)
    /// </summary>
    [Input(IsOptional = true)]
    [Units("oC")]
    [Description("Soil temperature")]
    private double[] ave_soil_temp;

    #endregion physiscs data

    #region Soil pH data

    /// <summary>
    /// pH of soil (assumed equivalent to a 1:1 soil-water slurry)
    /// </summary>
    [Param(IsOptional = true, MinVal = 3.5, MaxVal = 11.0)]
    [Input(IsOptional = true)]
    [Description("Soil pH")]
    public double[] ph;

    #endregion ph data

    #region Values for soil organic matter (som)

    /// <summary>
    /// Total soil organic carbon content (%)
    /// </summary>
    [Param]
    [Output]
    [Units("%")]
    [Description("Soil organic carbon (exclude FOM)")]
    public double[] oc
    {
        get
        {
            double[] result;
            if (initDone)
            {
                result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    result[layer] = (hum_c[layer] + biom_c[layer]) * convFactor[layer] / 10000;  // (100/1000000) = convert to ppm and then to %
            }
            else
            {
                // no value has been asigned yet, return null
                result = reset_oc;
            }
            return result;
        }
        set
        {
            if (initDone)
            {
                Console.WriteLine(" Attempt to assign values for OC during simulation, "
                                 + "this operation is not valid and will be ignored");
            }
            else
            {
                // Store initial values, check and initialisation of C pools is done on InitCalc().
                reset_oc = value;
                // these values are also used OnReset
            }
        }
    }

    #endregion soil organic matter data

    #region Values for soil mineral N

    /// <summary>
    /// Soil urea nitrogen content (ppm)
    /// </summary>
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 10000.0)]
    [Output]
    [Units("mg/kg")]
    [Description("Soil urea nitrogen content")]
    public double[] ureappm
    {
        get
        {
            double[] result;
            if (initDone)
            {
                result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    result[layer] = urea[layer] * convFactor[layer];
            }
            else
                result = reset_ureappm;
            return result;
        }
        set
        {
            if (!initDone)
                reset_ureappm = value;    // check is done on InitCalc
            else
                writeMessage("An external module attempted to change the value of urea during simulation, the command will be ignored");
        }
    }

    /// <summary>
    /// Soil ammonium nitrogen content (ppm)
    /// </summary>
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 10000.0)]
    [Output]
    [Units("mg/kg")]
    [Description("Soil ammonium nitrogen content")]
    public double[] nh4ppm
    {
        get
        {
            double[] result;
            if (initDone)
            {
                result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    result[layer] = nh4[layer] * convFactor[layer];
            }
            else
                result = reset_nh4ppm;
            return result;
        }
        set
        {
            if (!initDone)
                reset_nh4ppm = value;
            else
                writeMessage("An external module attempted to change the value of NH4 during simulation, the command will be ignored");
        }
    }

    /// <summary>
    /// Soil nitrate nitrogen content (ppm)
    /// </summary>
    [Param(IsOptional = true, MinVal = 0.0, MaxVal = 10000.0)]
    [Output]
    [Units("mg/kg")]
    [Description("Soil nitrate nitrogen content")]
    public double[] no3ppm
    {
        get
        {
            double[] result;
            if (initDone)
            {
                result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    result[layer] = no3[layer] * convFactor[layer];
            }
            else
                result = reset_no3ppm;
            return result;
        }
        set
        {
            if (!initDone)
                reset_no3ppm = value;
            else
                writeMessage("An external module attempted to change the value of NO3 during simulation, the command will be ignored");
        }
    }

    #endregion  mineral N data

    #region Soil loss data

    // it is assumed any changes in soil profile are due to erosion
    // this should be done via an event (RCichota)

    /// <summary>
    /// 
    /// </summary>
    [Output]
    [Units("on/off")]
    [Description("Define whether soil profile reduction is on")]
    private string n_reduction
    { set { ProfileReductionAllowed = value.StartsWith("on"); } }

    /// <summary>
    /// Soil loss due to erosion (t/ha)
    /// </summary>
    [Input(IsOptional = true)]
    [Units("t/ha")]
    [Description("Soil loss due to erosion")]
    private double soil_loss;

    #endregion

    #region Pond data

    /// <summary>
    /// Indicates whether pond is active or not
    /// </summary>
    /// <remarks>
    /// If there is a pond, the decomposition of surface OM will be done by that model
    /// </remarks>
    private bool isPondActive = false;
    [Input(IsOptional = true)]
    [Units("yes/no")]
    [Description("Indicates whether pond is active or not")]
    private string pond_active
    { set { isPondActive = (value == "yes"); } }

    /// <summary>
    /// Amount of C decomposed in pond that is added to soil m. biomass
    /// </summary>
    [Input(IsOptional = true)]
    [Units("kg/ha")]
    [Description("Amount of C decomposed in pond being acced to Biom")]
    private double pond_biom_C;

    /// <summary>
    /// Amount of C decomposed in pond that is added to soil humus
    /// </summary>  
    [Input(IsOptional = true)]
    [Units("kg/ha")]
    [Description("Amount of C decomposed in pond being acced to Humus")]
    private double pond_hum_C;

    #endregion

    #region Inhibitors data

    /// <summary>
    /// Factor reducing nitrification due to the presence of a inhibitor
    /// </summary>
    [Input(IsOptional = true)]
    [Units("0-1")]
    [Description("Factor reducing nitrification rate")]
    private double[] nitrification_inhibition
    {
        set
        {
            if (initDone)
            {
                for (int layer = 0; layer < dlayer.Length; layer++)
                {
                    if (layer < value.Length)
                    {
                        InhibitionFactor_Nitrification[layer] = value[layer];
                        if (InhibitionFactor_Nitrification[layer] < -epsilon)
                        {
                            InhibitionFactor_Nitrification[layer] = 0.0;
                            Console.WriteLine("Value for nitrification inhibition is below lower limit, value will be adjusted to 0.0");
                        }
                        else if (InhibitionFactor_Nitrification[layer] > 1.0)
                        {
                            InhibitionFactor_Nitrification[layer] = 1.0;
                            Console.WriteLine("Value for nitrification inhibition is above upper limit, value will be adjusted to 1.0");
                        }
                    }
                    else
                        InhibitionFactor_Nitrification[layer] = 0.0;
                }
            }
        }
    }

    #endregion

    #region Plant data

    private double rootDepth = 0.0;
    /// <summary>
    /// Depth of root zone  (mm)
    /// </summary>
    [Input(IsOptional = true)]
    [Param(IsOptional = true)]
    [Units("mm")]
    [Description("Depth of root zone")]
    public double RootingDepth
    {
        get { return rootDepth; }
        set { rootDepth = value; }
    }

    #endregion

    #endregion params that may change

    #endregion params and inputs

    #region Outputs we make available to other components

    #region Outputs for Nitrogen

    #region Changes for today - deltas

    /// <summary>
    /// N carried out in sediment via runoff/erosion
    /// </summary>
    [Output]
    [Units("kg")]
    [Description("N loss carried in sediment")]
    private double dlt_n_loss_in_sed
    {
        get
        {
            double result = 0.0;
            for (int k = 0; k < Patch.Count; k++)
                result += Patch[k].dlt_n_loss_in_sed * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Net nh4 change today
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Net NH4 change today")]
    private double[] dlt_nh4_net
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += (Patch[k].nh4[layer] - Patch[k].TodaysInitialNH4[layer]) * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Net NH4 transformation today
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Net NH4 transformation")]
    private double[] nh4_transform_net
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += (Patch[k].dlt_res_nh4_min[layer] +
                                     Patch[k].dlt_n_fom_to_min[layer] +
                                     Patch[k].dlt_n_biom_to_min[layer] +
                                     Patch[k].dlt_n_hum_to_min[layer] -
                                     Patch[k].dlt_nitrification[layer] +
                                     Patch[k].dlt_urea_hydrolysis[layer] +
                                     Patch[k].nh4_deficit_immob[layer]) *
                                     Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Net no3 change today
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Net NO3 change today")]
    private double[] dlt_no3_net
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += (Patch[k].no3[layer] - Patch[k].TodaysInitialNO3[layer]) * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Net NO3 transformation today
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Net NO3 transformation")]
    private double[] no3_transform_net
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += (Patch[k].dlt_res_no3_min[layer] -
                                     Patch[k].dlt_no3_dnit[layer] +
                                     Patch[k].dlt_nitrification[layer] -
                                     Patch[k].dlt_n2o_nitrif[layer] -
                                     Patch[k].nh4_deficit_immob[layer]) * 
                                     Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Net mineralisation today
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Net N mineralised in soil")]
    private double[] dlt_n_min
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += (Patch[k].dlt_n_hum_to_min[layer] + 
                                      Patch[k].dlt_n_biom_to_min[layer] +
                                      Patch[k].dlt_n_fom_to_min[layer]) * 
                                      Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Net N mineralisation from residue decomposition
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Net N mineralisation from residue decomposition")]
    private double[] dlt_n_min_res
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += (Patch[k].dlt_res_no3_min[layer] + Patch[k].dlt_res_nh4_min[layer]) * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Net NH4 mineralisation from residue decomposition
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Net convertion of NH4 for residue mineralisation/immobilisation")]
    private double[] dlt_res_nh4_min
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += Patch[k].dlt_res_nh4_min[layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Net NO3 mineralisation from residue decomposition
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Net convertion of NO3 for residue mineralisation/immobilisation")]
    private double[] dlt_res_no3_min
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += Patch[k].dlt_res_no3_min[layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Amount of N converted from each FOM pool
    /// </summary>
    //private double[][] dlt_n_fom = new double[3][];

    /// <summary>
    /// Net FOM N mineralised (negative for immobilisation)
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Net FOM N mineralised, negative for immobilisation")]
    private double[] dlt_fom_n_min
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += Patch[k].dlt_n_fom_to_min[layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Net N mineralised for humic pool
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Net humic N mineralised, negative for immobilisation")]
    private double[] dlt_hum_n_min
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += Patch[k].dlt_n_hum_to_min[layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Net N mineralised from m. biomass pool
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Net biomass N mineralised")]
    private double[] dlt_biom_n_min
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += Patch[k].dlt_n_biom_to_min[layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Total net N mineralised (residues plus soil OM)
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total net N mineralised (soil plus residues)")]
    private double[] dlt_n_min_tot
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += (Patch[k].dlt_n_hum_to_min[layer] +
                                      Patch[k].dlt_n_biom_to_min[layer] +
                                      Patch[k].dlt_n_fom_to_min[layer] +
                                      Patch[k].dlt_res_no3_min[layer] +
                                      Patch[k].dlt_res_nh4_min[layer]) *
                                      Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Nitrogen coverted by hydrolysis (from urea to NH4)
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Nitrogen coverted by hydrolysis")]
    private double[] dlt_urea_hydrol
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += Patch[k].dlt_urea_hydrolysis[layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Nitrogen coverted by nitrification (from NH4 to either NO3 or N2O)
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Nitrogen coverted by nitrification")]
    private double[] dlt_rntrf
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += Patch[k].dlt_nitrification[layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Nitrogen coverted by nitrification (from NH4 to either NO3 or N2O)
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Nitrogen coverted by nitrification")]
    private double[] nitrification
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += Patch[k].dlt_nitrification[layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Effective, or net, nitrogen coverted by nitrification (from NH4 to NO3)
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Effective nitrogen coverted by nitrification")]
    private double[] effective_nitrification
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; ++layer)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += (Patch[k].dlt_nitrification[layer] - Patch[k].dlt_n2o_nitrif[layer]) * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// N2O N produced during nitrification
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("N2O N produced during nitrification")]
    private double[] dlt_n2o_nitrif
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; ++layer)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += Patch[k].dlt_n2o_nitrif[layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// N2O N produced during nitrification
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("NH4 N denitrified")]
    private double[] dlt_nh4_dnit
    { get { return dlt_n2o_nitrif; } }

    /// <summary>
    /// N2O N produced during nitrification
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("NH4 N denitrified")]
    private double[] n2o_atm_nitrification
    { get { return dlt_n2o_nitrif; } }

    /// <summary>
    /// NO3 N denitrified
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("NO3 N denitrified")]
    private double[] dlt_no3_dnit
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; ++layer)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += Patch[k].dlt_no3_dnit[layer] * Patch[k].RelativeArea;
            return result;
        }
    }
    
    /// <summary>
    /// N2O N produced during denitrification
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("N2O N produced during denitrification")]
    private double[] dlt_n2o_dnit
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; ++layer)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += Patch[k].dlt_n2o_dnit[layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// N2O N produced during denitrification
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("N2O N produced during denitrification")]
    private double[] n2o_atm_denitrification
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; ++layer)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += Patch[k].dlt_n2o_dnit[layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Total N2O amount produced today
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Amount of N2O produced")]
    private double[] n2o_atm
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; ++layer)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += (Patch[k].dlt_n2o_dnit[layer] + Patch[k].dlt_n2o_nitrif[layer]) * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Amount of N2 produced
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Amount of N2 produced")]
    private double[] n2_atm
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; ++layer)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += (Patch[k].dlt_no3_dnit[layer] - Patch[k].dlt_n2o_dnit[layer]) * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// N converted by denitrification
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("N converted by denitrification")]
    private double[] dnit
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += (Patch[k].dlt_no3_dnit[layer] + Patch[k].dlt_n2o_nitrif[layer]) * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Excess N required above NH4 supply (for immobilisation)
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("NH4 deficit for immobilisation")]
    private double[] nh4_deficit_immob
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += Patch[k].nh4_deficit_immob[layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    #endregion deltas

    #region Amounts in solute forms

    /// <summary>
    /// Soil urea nitrogen amount (kgN/ha)
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Soil urea nitrogen amount")]
    private double[] urea
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += Patch[k].urea[layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Soil ammonium nitrogen amount (kgN/ha)
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Soil ammonium nitrogen amount")]
    private double[] nh4
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += Patch[k].nh4[layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Soil nitrate nitrogen amount (kgN/ha)
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Soil nitrate nitrogen amount")]
    private double[] no3
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += Patch[k].no3[layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Soil ammonium nitrogen amount available to plants, limited per patch (kgN/ha)
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Soil ammonium nitrogen amount available to plants, limited per patch")]
    private double[] nh4_PlantAvailable
    {
        get
        {
            double[] result = new double[dlayer.Length];
            //for (int layer = 0; layer < dlayer.Length; layer++)
            //    for (int k = 0; k < Patch.Count; k++)
            //        result[layer] += Math.Min(Patch[k].nh4[layer], MaximumNH4UptakeRate[layer]) * Patch[k].RelativeArea;

            if (initDone)
            {
                for (int k = 0; k < Patch.Count; k++)
                {
                    Patch[k].CalcTotalMineralNInRootZone();
                    for (int layer = 0; layer < dlayer.Length; layer++)
                        result[layer] += Patch[k].nh4AvailableToPlants[layer] * Patch[k].RelativeArea;
                }
            }
            return result;
        }
    }

    /// <summary>
    /// Soil nitrate nitrogen amount available to plants, limited per patch (kgN/ha)
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Soil nitrate nitrogen amount available to plants, limited per patch")]
    private double[] no3_PlantAvailable
    {
        get
        {
            double[] result = new double[dlayer.Length];
            if (initDone)
            {
                for (int k = 0; k < Patch.Count; k++)
                {
                    Patch[k].CalcTotalMineralNInRootZone();
                    for (int layer = 0; layer < dlayer.Length; layer++)
                        result[layer] += Patch[k].no3AvailableToPlants[layer] * Patch[k].RelativeArea;
                }
            }
            return result;
        }
    }

    #endregion

    #region Amounts in various pools

    /// <summary>
    /// Total nitrogen in FOM
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Nitrogen in FOM")]
    private double[] fom_n
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    for (int pool = 0; pool < Patch[k].fom_n.Length; pool++)
                        result[layer] += Patch[k].fom_n[pool][layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Nitrogen in FOM pool 1
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Nitrogen in FOM pool 1")]
    private double[] fom_n_pool1
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += Patch[k].fom_n[0][layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Nitrogen in FOM pool 2
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Nitrogen in FOM pool 2")]
    private double[] fom_n_pool2
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += Patch[k].fom_n[1][layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Nitrogen in FOM pool 3
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Nitrogen in FOM pool 3")]
    private double[] fom_n_pool3
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += Patch[k].fom_n[2][layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Soil humic N
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Soil humic nitrogen")]
    private double[] hum_n
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += Patch[k].hum_n[layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Inactive soil humic N
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Inert soil humic nitrogen")]
    private double[] inert_n
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += Patch[k].inert_n[layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Soil biomass nitrogen
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Soil biomass nitrogen")]
    private double[] biom_n
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += Patch[k].biom_n[layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Soil mineral nitrogen
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Soil mineral nitrogen")]
    private double[] mineral_n
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += (Patch[k].urea[layer] + Patch[k].nh4[layer] + Patch[k].no3[layer]) * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Soil organic nitrogen, old style
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Soil organic nitrogen")]
    private double[] org_n
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += (Patch[k].fom_n[0][layer]
                                   + Patch[k].fom_n[1][layer]
                                   + Patch[k].fom_n[2][layer]
                                   + Patch[k].hum_n[layer]
                                   + Patch[k].biom_n[layer]) * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Soil organic nitrogen
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Soil organic nitrogen")]
    private double[] organic_n
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += (Patch[k].fom_n[0][layer]
                                   + Patch[k].fom_n[1][layer]
                                   + Patch[k].fom_n[2][layer]
                                   + Patch[k].hum_n[layer]
                                   + Patch[k].biom_n[layer]) * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Total N in soil
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total N in soil")]
    private double[] nit_tot
    {
        get
        {
            double[] result = null;
            if (dlayer != null)
            {
                result = new double[dlayer.Length];
                for (int layer = 0; layer < dlayer.Length; ++layer)
                    for (int k = 0; k < Patch.Count; k++)
                        result[layer] = Patch[k].nit_tot[layer] * Patch[k].RelativeArea;
            }
            return result;
        }
    }

    #endregion

    #region Nitrogen balance

    /// <summary>
    /// SoilN balance for nitrogen: deltaN - losses
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Nitrogen balance in SoilN")]
    private double nitrogenbalance
    {
        get
        {
            double Nlosses = 0.0;
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    Nlosses += (Patch[k].dlt_n2o_nitrif[layer] + Patch[k].dlt_no3_dnit[layer]) * Patch[k].RelativeArea;  // exchange with 'outside' world computed by soilN

            double deltaN = SumDoubleArray(nit_tot) - TodaysInitialN;  // Variation in N today  -  Not sure how/when inputs and leaching are taken in account
            return -(Nlosses + deltaN);
        }
    }

    #endregion

    #endregion

    #region Outputs for Carbon

    #region General values

    /// <summary>
    /// Carbohydrate fraction of FOM (0-1)
    /// </summary>
    [Output]
    [Units("0-1")]
    [Description("Fraction of carbohydrate in FOM")]
    private double fr_carb
    { get { return fract_carb[fom_type]; } }

    /// <summary>
    /// Cellulose fraction of FOM (0-1)
    /// </summary>
    [Output]
    [Units("0-1")]
    [Description("Fraction of cellulose in FOM")]
    private double fr_cell
    { get { return fract_cell[fom_type]; } }

    /// <summary>
    /// Lignin fraction of FOM (0-1)
    /// </summary>
    [Output]
    [Units("0-1")]
    [Description("Fraction of lignin in FOM")]
    private double fr_lign
    { get { return fract_lign[fom_type]; } }

    #endregion

    #region Changes for today - deltas

    /// <summary>
    /// Carbon loss in sediment, via runoff/erosion
    /// </summary>
    [Output]
    [Units("kg")]
    [Description("Carbon loss in sediment")]
    private double dlt_c_loss_in_sed
    {
        get
        {
            double result = 0.0;
            for (int k = 0; k < Patch.Count; k++)
                result += Patch[k].dlt_c_loss_in_sed * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Amount of C converted from FOM to humic (kg/ha)
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("FOM C converted to humic")]
    private double[] dlt_fom_c_hum
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; ++layer)
                for (int k = 0; k < Patch.Count; k++)
                    for (int pool = 0; pool < 3; pool++)
                        result[layer] += Patch[k].dlt_c_fom_to_hum[pool][layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Amount of C converted from FOM to m. biomass (kg/ha)
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("FOM C converted to biomass")]
    private double[] dlt_fom_c_biom
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; ++layer)
                for (int k = 0; k < Patch.Count; k++)
                    for (int pool = 0; pool < 3; pool++)
                        result[layer] += Patch[k].dlt_c_fom_to_biom[pool][layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Amount of C lost to atmosphere from FOM
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("FOM C lost to atmosphere")]
    private double[] dlt_fom_c_atm
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; ++layer)
                for (int k = 0; k < Patch.Count; k++)
                    for (int pool = 0; pool < 3; pool++)
                        result[layer] += Patch[k].dlt_c_fom_to_atm[pool][layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Humic C converted to biomass
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Humic C converted to biomass")]
    private double[] dlt_hum_c_biom
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; ++layer)
                for (int k = 0; k < Patch.Count; k++)
                        result[layer] += Patch[k].dlt_c_hum_to_biom[layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Humic C lost to atmosphere
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Humic C lost to atmosphere")]
    private double[] dlt_hum_c_atm
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; ++layer)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += Patch[k].dlt_c_hum_to_atm[layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Biomass C converted to humic
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Biomass C converted to humic")]
    private double[] dlt_biom_c_hum
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; ++layer)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += Patch[k].dlt_c_biom_to_hum[layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Biomass C lost to atmosphere
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Biomass C lost to atmosphere")]
    private double[] dlt_biom_c_atm
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; ++layer)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += Patch[k].dlt_c_biom_to_atm[layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Carbon from residues converted to biomass C
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Carbon from residues converted to biomass")]
    private double[] dlt_res_c_biom
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; ++layer)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += Patch[k].dlt_c_res_to_biom[layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Carbon from residues converted to humic C
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Carbon from residues converted to humus")]
    private double[] dlt_res_c_hum
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; ++layer)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += Patch[k].dlt_c_res_to_hum[layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Carbon from residues lost to atmosphere
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Carbon from residues lost to atmosphere during decomposition")]
    private double dlt_res_c_atm
    {
        get
        {
            //double[] result = new double[dlayer.Length];
            double result = 0.0;
            for (int layer = 0; layer < dlayer.Length; ++layer)
                for (int k = 0; k < Patch.Count; k++)
                    result += Patch[k].dlt_c_res_to_atm[layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Delta C in pool 1 of FOM - needed by SoilP
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Delta FOM C due to decomposition in fraction 1")]
    private double[] dlt_fom_c_pool1
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; ++layer)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += (Patch[k].dlt_c_fom_to_hum[0][layer] +
                                      Patch[k].dlt_c_fom_to_biom[0][layer] +
                                      Patch[k].dlt_c_fom_to_atm[0][layer]) *
                                      Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Delta C in pool 2 of FOM - needed by SoilP
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Delta FOM C due to decomposition in fraction 2")]
    private double[] dlt_fom_c_pool2
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; ++layer)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += (Patch[k].dlt_c_fom_to_hum[1][layer] +
                                      Patch[k].dlt_c_fom_to_biom[1][layer] +
                                      Patch[k].dlt_c_fom_to_atm[1][layer]) *
                                      Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Delta C in pool 3 of FOM - needed by SoilP
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Delta FOM C due to decomposition in fraction 3")]
    private double[] dlt_fom_c_pool3
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; ++layer)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += (Patch[k].dlt_c_fom_to_hum[2][layer] +
                                      Patch[k].dlt_c_fom_to_biom[2][layer] +
                                      Patch[k].dlt_c_fom_to_atm[2][layer]) *
                                      Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Carbon from all residues to m. biomass
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Carbon from all residues to biomass")]
    private double[] soilp_dlt_res_c_biom
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; ++layer)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += Patch[k].dlt_c_res_to_biom[layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Carbon from all residues to humic pool
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Carbon from all residues to humic")]
    private double[] soilp_dlt_res_c_hum
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; ++layer)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += Patch[k].dlt_c_res_to_hum[layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Carbon lost from all residues to atmosphere
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Carbon from all residues to atmosphere")]
    private double[] soilp_dlt_res_c_atm
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; ++layer)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += Patch[k].dlt_c_res_to_atm[layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Total CO2 amount produced today
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Amount of co2 produced in the soil")]
    private double[] co2_atm
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += Patch[k].co2_atm[layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    #endregion

    #region Amounts in various pools

    /// <summary>
    /// Fresh organic C - FOM
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Soil FOM C")]
    private double[] fom_c
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    for (int pool = 0; pool < 3; pool++)
                        result[layer] += Patch[k].fom_c[pool][layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Amount of C in pool 1 of FOM
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("FOM C in pool 1")]
    private double[] fom_c_pool1
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += Patch[k].fom_c[0][layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Amount of C in pool 2 of FOM
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("FOM C in pool 2")]
    private double[] fom_c_pool2
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += Patch[k].fom_c[1][layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Amount of C in pool 3 of FOM
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("FOM C in pool 3")]
    private double[] fom_c_pool3
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += Patch[k].fom_c[2][layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Amount of C in humic pool
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Soil humic C")]
    private double[] hum_c
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += Patch[k].hum_c[layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Amount of C in inert humic pool
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Soil humic inert C")]
    private double[] inert_c
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += Patch[k].inert_c[layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Amount of C in m. biomass pool
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Soil biomass C")]
    private double[] biom_c
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += Patch[k].biom_c[layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Amount of water soluble C
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Soil water soluble C")]
    private double[] waterSoluble_c
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += Patch[k].waterSoluble_c[layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Total carbon amount in the soil
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total soil carbon")]
    private double[] carbon_tot
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += Patch[k].carbon_tot[layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    #endregion

    #region Carbon Balance

    /// <summary>
    /// Balance of C in soil: deltaC - losses
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Carbon balance")]
    private double carbonbalance
    {
        get
        {
            double Closses = 0.0;
            for (int layer = 0; layer < dlayer.Length; layer++)
                for (int k = 0; k < Patch.Count; k++)
                    Closses += (Patch[k].dlt_c_res_to_atm[layer] +
                                Patch[k].dlt_c_fom_to_atm[0][layer] +
                                Patch[k].dlt_c_fom_to_atm[1][layer] +
                                Patch[k].dlt_c_fom_to_atm[2][layer] +
                                Patch[k].dlt_c_hum_to_atm[layer] +
                                Patch[k].dlt_c_biom_to_atm[layer]) *
                                Patch[k].RelativeArea;
            double deltaC = SumDoubleArray(carbon_tot) - TodaysInitialC;
            return -(Closses + deltaC);
        }
    }

    #endregion

    #endregion

    #region Factors and other outputs

    /// <summary>
    /// amount of P coverted by residue mineralisation
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("P coverted by residue mineralisation (needed by SoilP)")]
    private double[] soilp_dlt_org_p
    {
        get
        {
            double[] result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; ++layer)
                for (int k = 0; k < Patch.Count; k++)
                    result[layer] += Patch[k].soilp_dlt_org_p[layer] * Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Soil temperature (oC), values actually used in the model
    /// </summary>
    private double[] Tsoil;

    /// <summary>
    /// SoilN's simple soil temperature
    /// </summary>
    [Output]
    [Units("oC")]
    [Description("Soil temperature")]
    private double[] st
    {
        get
        {
            double[] result = new double[0];
            // if (usingSimpleSoilTemp)   // this should limit the output to only the variable calculated here. However the plant modules still look for 'st' instead of 'ave_soil_temp'
            //  result = Tsoil;
            return Tsoil;
        }
    }

    #endregion

    #region Outputs related to internal patches    * * * * * * * * * * * * * * 

    #region General variables

    /// <summary>
    /// Number of internal patches
    /// </summary>
    [Output]
    [Units("")]
    [Description("Number of internal patches")]
    private int PatchCount
    { get { return Patch.Count; } }

    /// <summary>
    /// Relative area of each internal patch
    /// </summary>
    [Output]
    [Units("0-1")]
    [Description("Relative area of each internal patch")]
    private double[] PatchArea
    {
        get
        {
            double[] result = new double[Patch.Count];
            for (int k = 0; k < Patch.Count; k++)
                result[k] = Patch[k].RelativeArea;
            return result;
        }
    }

    /// <summary>
    /// Name of each internal patch
    /// </summary>
    [Output]
    [Units("")]
    [Description("Name of each internal patch")]
    private string[] PatchName
    {
        get
        {
            string[] result = new string[Patch.Count];
            for (int k = 0; k < Patch.Count; k++)
                result[k] = Patch[k].PatchName;
            return result;
        }
    }

    /// <summary>
    /// Age of each existing internal patch
    /// </summary>
    [Output]
    [Units("days")]
    [Description("Age of each existing internal patch")]
    private double[] PatchAge
    {
        get
        {
            double[] result = new double[Patch.Count];
            for (int k = 0; k < Patch.Count; k++)
                result[k] = (Clock.Today - Patch[k].CreationDate).TotalDays + 1;
            return result;
        }
    }

    #endregion

    #region Outputs for Nitrogen

    #region Changes for today - deltas

    #region Totals (whole profile)

    /// <summary>
    /// N carried out for each patch in sediment via runoff/erosion
    /// </summary>
    [Output]
    [Units("kg")]
    [Description("N loss carried in sediment for each patch")]
    private double[] PatchNLostInSediment
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                result[k] = Patch[k].dlt_n_loss_in_sed;
            return result;
        }
    }

    // - These are summed up over the whole profile  ----------------------------------------------

    /// <summary>
    /// Total net N mineralisation from residue decomposition, for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total net N mineralisation from residue decomposition, for each patch")]
    private double[] PatchTotalNMineralisedFromResidues
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].dlt_res_no3_min[layer] + Patch[k].dlt_res_nh4_min[layer];
            return result;
        }
    }

    /// <summary>
    /// Total net FOM N mineralised, for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total net FOM N mineralised, for each patch")]
    private double[] PatchTotalNMineralisedFromFOM
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].dlt_n_fom_to_min[layer];
            return result;
        }
    }

    /// <summary>
    /// Total net humic N mineralised, for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total net humic N mineralised, for each patch")]
    private double[] PatchTotalNMineralisedFromHumus
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].dlt_n_hum_to_min[layer];
            return result;
        }
    }

    /// <summary>
    /// Total net biomass N mineralised, for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total net biomass N mineralised, for each patch")]
    private double[] PatchTotalNMineralisedFromMBiomass
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].dlt_n_biom_to_min[layer];
            return result;
        }
    }

    /// <summary>
    /// Total net N mineralised, for each patch (residues plus soil OM)
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total net N mineralised, for each patch")]
    private double[] PatchTotalNMineralised
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].dlt_n_biom_to_min[layer];
            return result;
        }
    }

    /// <summary>
    /// Total nitrogen coverted by hydrolysis, for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total nitrogen coverted by hydrolysis, for each patch")]
    private double[] PatchTotalUreaHydrolysis
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].dlt_urea_hydrolysis[layer];
            return result;
        }
    }

    /// <summary>
    /// Total nitrogen coverted by nitrification, for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total nitrogen coverted by nitrification, for each patch")]
    private double[] PatchTotalNitrification
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].dlt_nitrification[layer];
            return result;
        }
    }

    /// <summary>
    /// Total effective amount of NH4-N coverted into NO3 by nitrification, for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total effective amount of NH4-N coverted into NO3 by nitrification, for each patch")]
    private double[] PatchTotalEffectiveNitrification
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].dlt_nitrification[layer] - Patch[k].dlt_n2o_nitrif[layer];
            return result;
        }
    }

    /// <summary>
    /// Total N2O N produced during nitrification, for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total N2O N produced during nitrification, for each patch")]
    private double[] PatchTotalN2O_Nitrification
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].dlt_n2o_nitrif[layer];
            return result;
        }
    }

    /// <summary>
    /// Total NO3 N denitrified, for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total NO3 N denitrified, for each patch")]
    private double[] PatchTotalNO3_Denitrification
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].dlt_no3_dnit[layer];
            return result;
        }
    }

    /// <summary>
    /// Total N2O N produced during denitrification, for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total N2O N produced during denitrification, for each patch")]
    private double[] PatchTotalN2O_Denitrification
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].dlt_n2o_dnit[layer];
            return result;
        }
    }

    /// <summary>
    /// Total amount of N2O N produced, for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total amount of N2O N produced, for each patch")]
    private double[] PatchTotalN2OLostToAtmosphere
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].dlt_n2o_dnit[layer] + Patch[k].dlt_n2o_nitrif[layer];
            return result;
        }
    }

    /// <summary>
    /// Total amount of N2 produced, for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total amount of N2 produced, for each patch")]
    private double[] PatchTotalN2LostToAtmosphere
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].dlt_no3_dnit[layer] - Patch[k].dlt_n2o_dnit[layer];
            return result;
        }
    }

    /// <summary>
    /// Total N converted by denitrification (all forms), for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total N converted by denitrification (all forms), for each patch")]
    private double[] PatchTotalDenitrification
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].dlt_no3_dnit[layer] + Patch[k].dlt_n2o_nitrif[layer];
            return result;
        }
    }

    /// <summary>
    /// Total amount of urea changed by the soil water module, for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total amount of urea changed by the soil water module, for each patch")]
    private double[] PatchTotalUreaLeached
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                result[k] += Patch[k].urea_flow[dlayer.Length - 1];
            return result;
        }
    }

    /// <summary>
    /// Total amount of NH4 changed by the soil water module, for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total amount of NH4 changed by the soil water module, for each patch")]
    private double[] PatchTotalNH4Leached
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                result[k] += Patch[k].nh4_flow[dlayer.Length - 1];
            return result;
        }
    }

    /// <summary>
    /// Total amount of NO3 changed by the soil water module, for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total amount of NO3 changed by the soil water module, for each patch")]
    private double[] PatchTotalNO3Leached
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                result[k] += Patch[k].no3_flow[dlayer.Length - 1];
            return result;
        }
    }

    /// <summary>
    /// Total amount of urea taken up by any plant module, for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total amount of urea taken up by any plant module, for each patch")]
    private double[] PatchTotalUreaUptake
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].urea_uptake[layer];
            return result;
        }
    }

    /// <summary>
    /// Total amount of NH4 taken up by any plant module, for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total amount of NH4 taken up by any plant module, for each patch")]
    private double[] PatchTotalNH4Uptake
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].nh4_uptake[layer];
            return result;
        }
    }

    /// <summary>
    /// Total amount of NO3 taken up by any plant module, for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total amount of NO3 taken up by any plant module, for each patch")]
    private double[] PatchTotalNO3Uptake
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].no3_uptake[layer];
            return result;
        }
    }

    /// <summary>
    /// Total amount of urea added by the fertiliser module, for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total amount of urea added by the fertiliser module, for each patch")]
    private double[] PatchTotalUreaFertiliser
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].urea_fertiliser[layer];
            return result;
        }
    }

    /// <summary>
    /// Total amount of NH4 added by the fertiliser module, for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total amount of NH4 added by the fertiliser module, for each patch")]
    private double[] PatchTotalNH4Fertiliser
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].nh4_fertiliser[layer];
            return result;
        }
    }

    /// <summary>
    /// Total amount of NO3 added by the fertiliser module, for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total amount of NO3 added by the fertiliser module, for each patch")]
    private double[] PatchTotalNO3Fertiliser
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].no3_fertiliser[layer];
            return result;
        }
    }

    /// <summary>
    /// Total amount of urea changed by the any other module, for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total amount of urea changed by the any other module, for each patch")]
    private double[] PatchTotalUreaChangedOther
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].urea_ChangedOther[layer];
            return result;
        }
    }

    /// <summary>
    /// Total amount of NH4 changed by the any other module, for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total amount of NH4 changed by the any other module, for each patch")]
    private double[] PatchTotalNH4ChangedOther
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].nh4_ChangedOther[layer];
            return result;
        }
    }

    /// <summary>
    /// Total amount of NO3 changed by the any other module, for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total amount of NO3 changed by the any other module, for each patch")]
    private double[] PatchTotalNO3ChangedOther
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].no3_ChangedOther[layer];
            return result;
        }
    }

    // --------------------------------------------------------------------------------------------
    #endregion

    #region Values for each soil layer

    /// <summary>
    /// Net N mineralisation from residue decomposition for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Net N mineralisation from residue decomposition in each patch")]
    private CNPatchVariableType PatchNMineralisedFromResidues
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].dlt_res_no3_min[layer] + Patch[k].dlt_res_nh4_min[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Net FOM N mineralised for each patch (negative for immobilisation)
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Net FOM N mineralised for each patch, negative for immobilisation")]
    private CNPatchVariableType PatchNMineralisedFromFOM
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] =  Patch[k].dlt_n_fom_to_min[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Net N mineralised for humic pool for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Net humic N mineralised for each patch, negative for immobilisation")]
    private CNPatchVariableType PatchNMineralisedFromHumus
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].dlt_n_hum_to_min[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Net N mineralised from m. biomass pool for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Net biomass N mineralised for each patch")]
    private CNPatchVariableType PatchNMineralisedFromMBiomass
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].dlt_n_biom_to_min[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Net total N mineralised for each patch (residues plus soil OM)
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Net total N mineralised for each patch (soil OM plus residues)")]
    private CNPatchVariableType PatchNMineralisedTotal
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].dlt_n_hum_to_min[layer] +
                                                   Patch[k].dlt_n_biom_to_min[layer] +
                                                   Patch[k].dlt_n_fom_to_min[layer] +
                                                   Patch[k].dlt_res_no3_min[layer] +
                                                   Patch[k].dlt_res_nh4_min[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Nitrogen coverted by hydrolysis (from urea to NH4) for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Nitrogen coverted by hydrolysis for each patch")]
    private CNPatchVariableType PatchDltUreaHydrolysis
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].dlt_urea_hydrolysis[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Nitrogen coverted by nitrification (from NH4 to either NO3 or N2O) for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Nitrogen coverted by nitrification (into either NO3 or N2O) for each patch")]
    private CNPatchVariableType PatchDltNitrification
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].dlt_nitrification[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Effective, or net, nitrogen coverted by nitrification for each patch (from NH4 to NO3)
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Effective amount of NH4-N coverted into NO3 by nitrification for each patch")]
    private CNPatchVariableType PatchEffectiveDltNitrification
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].dlt_nitrification[layer] - Patch[k].dlt_n2o_nitrif[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// N2O N produced during nitrification for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("N2O N produced during nitrification for each patch")]
    private CNPatchVariableType PatchDltN2O_Nitrification
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].dlt_n2o_nitrif[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// NO3 N denitrified for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("NO3 N denitrified for each patch")]
    private CNPatchVariableType PatchDltNO3_Denitrification
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].dlt_no3_dnit[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// N2O N produced during denitrification for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("N2O N produced during denitrification for each patch")]
    private CNPatchVariableType PatchDltN2O_Denitrification
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].dlt_n2o_dnit[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Total N2O amount produced for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Amount of N2O produced for each patch")]
    private CNPatchVariableType PatchN2OLostToAtmosphere
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].dlt_n2o_dnit[layer] + Patch[k].dlt_n2o_nitrif[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Amount of N2 produced for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Amount of N2 produced for each patch")]
    private CNPatchVariableType PatchN2LostToAtmosphere
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].dlt_no3_dnit[layer] - Patch[k].dlt_n2o_dnit[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// N converted by all forms of denitrification for each patch (to be deleted?)
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("N converted by denitrification (all forms) for each patch")]
    private CNPatchVariableType Patch_dnit
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].dlt_no3_dnit[layer] + Patch[k].dlt_n2o_nitrif[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// N converted by all forms of denitrification for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("N converted by denitrification (all forms) for each patch")]
    private CNPatchVariableType PatchDltDenitrification
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].dlt_no3_dnit[layer] + Patch[k].dlt_n2o_nitrif[layer];
            }
            return result;
        }
    }

    // ---------------------------------------------------------------------------------------------------------------------
    // Variables that are owned by other modules but which are related to SoilNitrogen.
    // here the estimated values partitioned amongst internal patches are published

    /// <summary>
    /// Amount of urea changed by the soil water module, for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Amount of urea changed by the soil water module, for each patch")]
    private CNPatchVariableType Patch_UreaFlow
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].urea_flow[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Amount of NH4 changed by the soil water module, for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Amount of NH4 changed by the soil water module, for each patch")]
    private CNPatchVariableType Patch_NH4Flow
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].nh4_flow[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Amount of NO3 changed by the soil water module, for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Amount of NO3 changed by the soil water module, for each patch")]
    private CNPatchVariableType Patch_NO3Flow
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].no3_flow[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Amount of urea taken up by any plant module, for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Amount of urea taken up by any plant module, for each patch")]
    private CNPatchVariableType Patch_UreaUptake
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].urea_uptake[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Amount of NH4 taken up by any plant module, for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Amount of NH4 taken up by any plant module, for each patch")]
    private CNPatchVariableType Patch_NH4Uptake
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].nh4_uptake[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Amount of NO3 taken up by any plant module, for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Amount of NO3 taken up by any plant module, for each patch")]
    private CNPatchVariableType Patch_NO3Uptake
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].no3_uptake[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Amount of urea added by the fertiliser module, for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Amount of urea added by the fertiliser module, for each patch")]
    private CNPatchVariableType Patch_UreaFertiliser
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].urea_fertiliser[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Amount of NH4 added by the fertiliser module, for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Amount of NH4 added by the fertiliser module, for each patch")]
    private CNPatchVariableType Patch_NH4Fertiliser
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].nh4_fertiliser[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Amount of NO3 added by the fertiliser module, for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Amount of NO3 added by the fertiliser module, for each patch")]
    private CNPatchVariableType Patch_NO3Fertiliser
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].no3_fertiliser[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Amount of urea changed by the any other module, for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Amount of urea changed by the any other module, for each patch")]
    private CNPatchVariableType Patch_UreaChangedOther
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].urea_ChangedOther[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Amount of NH4 changed by the any other module, for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Amount of NH4 changed by the any other module, for each patch")]
    private CNPatchVariableType Patch_NH4ChangedOther
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].nh4_ChangedOther[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Amount of NO3 changed by the any other module, for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Amount of NO3 changed by the any other module, for each patch")]
    private CNPatchVariableType Patch_NO3ChangedOther
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].no3_ChangedOther[layer];
            }
            return result;
        }
    }

    // ----------------------------------------------------------------------------------------------------------------

    #endregion

    #endregion deltas

    #region Amounts in solute forms

    /// <summary>
    /// Amount of urea N in each internal patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Amount of N as urea in each patch")]
    private CNPatchVariableType PatchUrea
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].urea[layer];
            }

            if (Patch.Count > 1)
                nLayers += 0;

            return result;
        }
    }

    /// <summary>
    /// Amount of NH4 N in each internal patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Amount of N as NH4 in each patch")]
    private CNPatchVariableType PatchNH4
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].nh4[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Amount of NO3 N in each internal patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Amount of N as NO3 in each patch")]
    private CNPatchVariableType PatchNO3
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].no3[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Amount of N as NH4 available to plants, in each internal patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Amount of N as NH4 available to plants, in each patch")]
    private CNPatchVariableType PatchPlantAvailableNH4
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                if (initDone)
                {
                    Patch[k].CalcTotalMineralNInRootZone();
                    for (int layer = 0; layer < nLayers; layer++)
                        result.Patch[k].Value[layer] = Patch[k].nh4AvailableToPlants[layer];
                }
            }
            return result;
        }
    }

    /// <summary>
    /// Amount of N as NO3 available to plants, in each internal patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Amount of N as NO3 available to plants, in each patch")]
    private CNPatchVariableType PatchPlantAvailableNO3
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                if (initDone)
                {
                    Patch[k].CalcTotalMineralNInRootZone();
                    for (int layer = 0; layer < nLayers; layer++)
                        result.Patch[k].Value[layer] = Patch[k].no3AvailableToPlants[layer];
                }
            }
            return result;
        }
    }

    /// <summary>
    /// Total urea N in each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total urea N in each patch")]
    private double[] PatchTotalUrea
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].urea[layer];
            return result;
        }
    }

    /// <summary>
    /// Total NH4 N in each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total NH4 N in each patch")]
    private double[] PatchTotalNH4
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].nh4[layer];
            return result;
        }
    }

    /// <summary>
    /// Total NO3 N in each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total NO3 N in each patch")]
    private double[] PatchTotalNO3
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].no3[layer];
            return result;
        }
    }

    /// <summary>
    /// Total NH4 N available to plants in each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total NH4 N available to plants in each patch")]
    private double[] PatchTotalNH4_PlantAvailable
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            if (initDone)
            {
                for (int k = 0; k < nPatches; k++)
                {
                    Patch[k].CalcTotalMineralNInRootZone();
                    for (int layer = 0; layer < dlayer.Length; layer++)
                        result[k] += Patch[k].nh4AvailableToPlants[layer];
                }
            }
            return result;
        }
    }

    /// <summary>
    /// Total NO3 N available to plants in each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total NO3 N available to plants in each patch")]
    private double[] PatchTotalNO3_PlantAvailable
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            if (initDone)
            {
                for (int k = 0; k < nPatches; k++)
                {
                    Patch[k].CalcTotalMineralNInRootZone();
                    for (int layer = 0; layer < dlayer.Length; layer++)
                        result[k] += Patch[k].no3AvailableToPlants[layer];
                }
            }
            return result;
        }
    }

    #endregion

    #region Amounts in various pools

    /// <summary>
    /// Total nitrogen in FOM for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Nitrogen in FOM for each Patch")]
    private CNPatchPoolVariableType PatchFOM_N
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nPools = 3;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchPoolVariableType result = new CNPatchPoolVariableType();
            result.Patch = new CNPatchPoolVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchPoolVariablePatchType();
                result.Patch[k].Pool = new CNPatchPoolVariablePatchPoolType[nPools];
                for (int pool = 0; pool < nPools; pool++)
                {
                    result.Patch[k].Pool[pool] = new CNPatchPoolVariablePatchPoolType();
                    result.Patch[k].Pool[pool].Value = new double[nLayers];
                    for (int layer = 0; layer < nLayers; layer++)
                        result.Patch[k].Pool[pool].Value[layer] = Patch[k].fom_n[pool][layer];
                }
            }
            return result;
        }
    }

    /// <summary>
    /// Soil humic N for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Soil humic nitrogen for each patch")]
    private CNPatchVariableType PatchHum_N
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].hum_n[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Inactive soil humic N for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Inert soil humic nitrogen for each patch")]
    private CNPatchVariableType PatchInert_N
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].inert_n[layer];
            }
            return result;
        }
    }
    
    /// <summary>
    /// Soil biomass nitrogen for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Soil biomass nitrogen for each patch")]
    private CNPatchVariableType PatchBiom_N
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].biom_n[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Total mineral N in soil for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total mineral N in soil for each patch")]
    private CNPatchVariableType PatchSoilMineralN
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].urea[layer] + Patch[k].nh4[layer] + Patch[k].no3[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Total organic N in soil for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total organic N in soil for each patch")]
    private CNPatchVariableType PatchSoilOrganicN
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].fom_n[0][layer]
                                                 + Patch[k].fom_n[1][layer] 
                                                 + Patch[k].fom_n[2][layer]
                                                 + Patch[k].hum_n[layer] 
                                                 + Patch[k].biom_n[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Total N in soil for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total N in soil for each patch")]
    private CNPatchVariableType PatchTotalN
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].nit_tot[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Total FOM N in the whole profile for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total FOM N in the whole profile for each patch")]
    private double[] PatchTotalFOM_N
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].fom_n[0][layer] + Patch[k].fom_n[1][layer] + Patch[k].fom_n[2][layer];
            return result;
        }
    }

    /// <summary>
    /// Total Humic N in the whole profile for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total humic N in the whole profile for each patch")]
    private double[] PatchTotalHum_N
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].hum_n[layer];
            return result;
        }
    }

    /// <summary>
    /// Total inert N in the whole profile for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total inert N in the whole profile for each patch")]
    private double[] PatchTotalInert_N
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].inert_n[layer];
            return result;
        }
    }

    /// <summary>
    /// Total biomass N in the whole profile for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total biomass N in the whole profile for each patch")]
    private double[] PatchTotalBiom_N
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].biom_n[layer];
            return result;
        }
    }

    /// <summary>
    /// Total mineral N in the whole profile for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total mineral N in the whole profile for each patch")]
    private double[] PatchTotalSoilMineralN
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].urea[layer] + Patch[k].nh4[layer] + Patch[k].no3[layer];

            return result;
        }
    }

    /// <summary>
    /// Total organic N in the whole profile for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total organic N in the whole profile for each patch")]
    private double[] PatchTotalSoilOrganicN
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].fom_n[0][layer] + Patch[k].fom_n[1][layer] + Patch[k].fom_n[2][layer] +
                                 Patch[k].hum_n[layer] + Patch[k].biom_n[layer];
            return result;
        }
    }

    /// <summary>
    /// Total N in the whole profile for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total N in the whole profile for each patch")]
    private double[] PatchTotalSoilN
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].nit_tot[layer];
            return result;
        }
    }

    #endregion

    #region Nitrogen balance

    /// <summary>
    /// SoilN balance for nitrogen, for each patch: deltaN - losses
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Nitrogen balance in SoilN for each patch")]
    private double[] PatchNitrogenBalance
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            double[] result = new double[nLayers];
            for (int k = 0; k < Patch.Count; k++)
            {
                double Nlosses = 0.0;
                for (int layer = 0; layer < dlayer.Length; layer++)
                    Nlosses += (Patch[k].dlt_n2o_nitrif[layer] + Patch[k].dlt_no3_dnit[layer]);
                double deltaN = SumDoubleArray(nit_tot) - TodaysInitialN;
                result[k] = -(Nlosses + deltaN);
            }
            return result;
        }
    }

    #endregion

    #endregion

    #region Outputs for Carbon

    #region Changes for today - deltas

    /// <summary>
    /// Carbon loss in sediment for each patch, via runoff/erosion
    /// </summary>
    [Output]
    [Units("kg")]
    [Description("Carbon loss in sediment for each patch")]
    private double[] PatchCLostInSediment
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                result[k] = Patch[k].dlt_c_loss_in_sed;
            return result;
        }
    }

    /// <summary>
    /// Amount of C converted from FOM to humic for each patch (kg/ha)
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("FOM C converted to humic for each patch")]
    private CNPatchVariableType PatchDltCFromFOMToHumus
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    for (int pool = 0; pool < 3; pool++)
                        result.Patch[k].Value[layer] = Patch[k].dlt_c_fom_to_hum[pool][layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Amount of C converted from FOM to m. biomass for each patch (kg/ha)
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("FOM C converted to biomass for each patch")]
    private CNPatchVariableType PatchDltCFromFOMToMBiomass
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    for (int pool = 0; pool < 3; pool++)
                        result.Patch[k].Value[layer] = Patch[k].dlt_c_fom_to_biom[pool][layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Amount of C lost to atmosphere from FOM for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("FOM C lost to atmosphere for each patch")]
    private CNPatchVariableType PatchDltCFromFOMToAtmosphere
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    for (int pool = 0; pool < 3; pool++)
                        result.Patch[k].Value[layer] = Patch[k].dlt_c_fom_to_atm[pool][layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Humic C converted to biomass for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Humic C converted to biomass for each patch")]
    private CNPatchVariableType PatchDltCFromHumusToMBiomass
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].dlt_c_hum_to_biom[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Humic C lost to atmosphere for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Humic C lost to atmosphere for each patch")]
    private CNPatchVariableType PatchDltCFromHumusToAtmosphere
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].dlt_c_hum_to_atm[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Biomass C converted to humic for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Biomass C converted to humic for each patch")]
    private CNPatchVariableType PatchDltCFromMBiomassToHumus
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].dlt_c_biom_to_hum[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Biomass C lost to atmosphere for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Biomass C lost to atmosphere for each patch")]
    private CNPatchVariableType PatchDltCFromMBiomassToAtmosphere
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].dlt_c_biom_to_atm[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Carbon from residues converted to biomass C for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Carbon from residues converted to biomass for each patch")]
    private CNPatchVariableType PatchDltCFromResiduesToMBiomass
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].dlt_c_res_to_biom[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Carbon from residues converted to humic C for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Carbon from residues converted to humus for each patch")]
    private CNPatchVariableType PatchDltCFromResiduesToHumus
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].dlt_c_res_to_hum[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Carbon from residues lost to atmosphere for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Carbon from residues lost to atmosphere during decomposition for each patch")]
    private CNPatchVariableType PatchDltCFromResiduesToAtmosphere
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].dlt_c_res_to_atm[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Total CO2 amount produced in the soil for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Amount of co2 produced in the soil for each patch")]
    private CNPatchVariableType PatchCO2Produced
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].co2_atm[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Total amount of C converted from FOM to humic for each patch (kg/ha)
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total amount of C converted from FOM to humic for each patch")]
    private double[] PatchTotalDltCFromFOMToHumus
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    for (int pool = 0; pool < 3; pool++)
                        result[k] += Patch[k].dlt_c_fom_to_hum[pool][layer];
            return result;
        }
    }

    /// <summary>
    /// Total amount of C converted from FOM to m. biomass for each patch (kg/ha)
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total amount of C converted from FOM to m. biomass for each patch")]
    private double[] PatchTotalDltCFromFOMToMBiomass
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    for (int pool = 0; pool < 3; pool++)
                        result[k] += Patch[k].dlt_c_fom_to_biom[pool][layer];
            return result;
        }
    }

    /// <summary>
    /// Total amount of FOM C lost to atmosphere for each patch (kg/ha)
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total amount of FOM C lost to atmosphere for each patch")]
    private double[] PatchTotalDltCFromFOMToAtmosphere
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    for (int pool = 0; pool < 3; pool++)
                        result[k] += Patch[k].dlt_c_fom_to_atm[pool][layer];
            return result;
        }
    }

    /// <summary>
    /// Total amount of humic C converted to m. biomass for each patch (kg/ha)
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total amount of humic C converted to m. biomass for each patch")]
    private double[] PatchTotalDltCFromHumusToMBiomass
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].dlt_c_hum_to_biom[layer];
            return result;
        }
    }

    /// <summary>
    /// Total amount of humic C lost to atmosphere for each patch (kg/ha)
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total amount of humic C lost to atmosphere for each patch")]
    private double[] PatchTotalDltCFromHumusToAtmosphere
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].dlt_c_hum_to_atm[layer];
            return result;
        }
    }

    /// <summary>
    /// Total amount of biomass C converted to humus for each patch (kg/ha)
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total amount of biomass C converted to humus for each patch")]
    private double[] PatchTotalDltCFromMBiomassToHumus
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].dlt_c_biom_to_hum[layer];
            return result;
        }
    }

    /// <summary>
    /// Total amount of biomass C lost to atmosphere for each patch (kg/ha)
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total amount of biomass C lost to atmosphere for each patch")]
    private double[] PatchTotalDltCFromMBiomassToAtmosphere
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].dlt_c_biom_to_atm[layer];
            return result;
        }
    }

    /// <summary>
    /// Total amount of C from residues converted to m. biomass for each patch (kg/ha)
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total amount of C from residues converted to m. biomass for each patch")]
    private double[] PatchTotalDltCFromResiduesToMBiomass
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].dlt_c_res_to_biom[layer];
            return result;
        }
    }

    /// <summary>
    /// Total amount of C from residues converted to humus for each patch (kg/ha)
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total amount of C from residues converted to humus for each patch")]
    private double[] PatchTotalDltCFromResiduesToHumus
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].dlt_c_res_to_hum[layer];
            return result;
        }
    }

    /// <summary>
    /// Total amount of C from residues lost to atmosphere for each patch (kg/ha)
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total amount of C from residues lost to atmosphere for each patch")]
    private double[] PatchTotalDltCFromResiduesToAtmosphere
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].dlt_c_res_to_atm[layer];
            return result;
        }
    }

    /// <summary>
    /// Total CO2 amount produced in the soil for each patch (kg/ha)
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total CO2 amount produced in the soil for each patch")]
    private double[] PatchTotalCO2Produced
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].co2_atm[layer];
            return result;
        }
    }

    #endregion

    #region Amounts in various pools

    /// <summary>
    /// Fresh organic C - FOM for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Soil FOM C for each patch")]
    private CNPatchPoolVariableType PatchFOM_C
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nPools = 3;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchPoolVariableType result = new CNPatchPoolVariableType();
            result.Patch = new CNPatchPoolVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchPoolVariablePatchType();
                result.Patch[k].Pool = new CNPatchPoolVariablePatchPoolType[nPools];
                for (int pool = 0; pool < nPools; pool++)
                {
                    result.Patch[k].Pool[pool] = new CNPatchPoolVariablePatchPoolType();
                    result.Patch[k].Pool[pool].Value = new double[nLayers];
                    for (int layer = 0; layer < nLayers; layer++)
                        result.Patch[k].Pool[pool].Value[layer] = Patch[k].fom_c[pool][layer];
                }
            }
            return result;
        }
    }

    /// <summary>
    /// Amount of C in humic pool for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Soil humic C for each patch")]
    private CNPatchVariableType PatchHum_C
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].hum_c[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Amount of C in inert humic pool for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Soil humic inert C for each patch")]
    private CNPatchVariableType PatchInert_C
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].inert_c[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Amount of C in m. biomass pool for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Soil biomass C for each patch")]
    private CNPatchVariableType PatchBiom_C
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].biom_c[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Amount of water soluble C for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Amount of water soluble C for each patch")]
    private CNPatchVariableType PatchWaterSolubleC
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].waterSoluble_c[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Total carbon amount in the soil for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total soil carbon for each patch")]
    private CNPatchVariableType PatchTotalC
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            CNPatchVariableType result = new CNPatchVariableType();
            result.Patch = new CNPatchVariablePatchType[nPatches];
            for (int k = 0; k < nPatches; k++)
            {
                result.Patch[k] = new CNPatchVariablePatchType();
                result.Patch[k].Value = new double[nLayers];
                for (int layer = 0; layer < nLayers; layer++)
                    result.Patch[k].Value[layer] = Patch[k].carbon_tot[layer];
            }
            return result;
        }
    }

    /// <summary>
    /// Total FOM C in the whole profile for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total FOM C in the whole profile for each patch")]
    private double[] PatchTotalFOM_C
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].fom_c[0][layer] + Patch[k].fom_c[1][layer] + Patch[k].fom_c[2][layer];
            return result;
        }
    }

    /// <summary>
    /// Total Humic C in the whole profile for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total humic C in the whole profile for each patch")]
    private double[] PatchTotalHum_C
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].hum_c[layer];
            return result;
        }
    }

    /// <summary>
    /// Total inert C in the whole profile for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total inert C in the whole profile for each patch")]
    private double[] PatchTotalInert_C
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].inert_c[layer];
            return result;
        }
    }

    /// <summary>
    /// Total biomass C in the whole profile for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total biomass C in the whole profile for each patch")]
    private double[] PatchTotalBiom_C
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].biom_c[layer];
            return result;
        }
    }

    /// <summary>
    /// Total water soluble C in the whole profile for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total water soluble C in the whole profile for each patch")]
    private double[] PatchTotalWaterSolubleC
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].waterSoluble_c[layer];
            return result;
        }
    }

    /// <summary>
    /// Total C in the whole profile for each patch
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Total C in the whole profile for each patch")]
    private double[] PatchTotalSoilC
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            double[] result = new double[nPatches];
            for (int k = 0; k < nPatches; k++)
                for (int layer = 0; layer < dlayer.Length; layer++)
                    result[k] += Patch[k].carbon_tot[layer];
            return result;
        }
    }

    #endregion

    #region Carbon Balance

    /// <summary>
    /// Balance of C in soil,  for each patch: deltaC - losses
    /// </summary>
    [Output]
    [Units("kg/ha")]
    [Description("Carbon balance for each patch")]
    private double[] PatchCarbonBalance
    {
        get
        {
            int nPatches = (Patch != null) ? Patch.Count : 1;
            int nLayers = (dlayer != null) ? dlayer.Length : 0;
            double[] result = new double[nLayers];
            for (int k = 0; k < nPatches; k++)
            {
                double CLosses = 0.0;
                for (int layer = 0; layer < nLayers; layer++)
                    CLosses += (Patch[k].dlt_c_res_to_atm[layer] +
                                        Patch[k].dlt_c_fom_to_atm[0][layer] +
                                        Patch[k].dlt_c_fom_to_atm[1][layer] +
                                        Patch[k].dlt_c_fom_to_atm[2][layer] +
                                        Patch[k].dlt_c_hum_to_atm[layer] +
                                        Patch[k].dlt_c_biom_to_atm[layer]);
                double deltaC = SumDoubleArray(Patch[k].carbon_tot) - Patch[k].TodaysInitialC;
                result[k]= -(CLosses + deltaC);
            }
            return result;
        }
    }

    #endregion

    #endregion

    #endregion outputs from patches      * * * * * * * * * * * * * * * * * * 

    #endregion outputs

    #region Useful constants

    /// <summary>
    /// Value to evaluate precision against floating point variables
    /// </summary>
    private readonly double epsilon = 0.000000000001;
    //private double epsilon = Math.Pow(2, -24);

    #endregion constants

    #region Internal variables

    #region Components

    /// <summary>
    /// List of all existing patches (internal instances of C and N processes)
    /// </summary>
    public List<soilCNPatch> Patch;

    /// <summary>
    /// The SoilN internal soil temperature module - to be avoided (deprecated)
    /// </summary>
    private simpleSoilTemp simpleST;

    #endregion components

    #region Soil physics data

    /// <summary>
    /// The soil layer thickness at the start of the simulation
    /// </summary>
    private double[] reset_dlayer;

    /// <summary>
    /// Soil layers' thichness (mm)
    /// </summary>
    private double[] dlayer;

    /// <summary>
    /// Soil bulk density for each layer (g/cm3)
    /// </summary>
    private double[] SoilDensity;

    /// <summary>
    /// Soil water amount at saturation (mm)
    /// </summary>
    private double[] sat_dep;

    /// <summary>
    /// Soil water amount at drainage upper limit (mm)
    /// </summary>
    private double[] dul_dep;

    /// <summary>
    /// Soil water amount at drainage lower limit (mm)
    /// </summary>
    private double[] ll15_dep;

    #endregion

    #region Initial C and N amounts

    /// <summary>
    /// The initial OC content for each layer of the soil (%). Also used onReset
    /// </summary>
    private double[] reset_oc;

    /// <summary>
    /// Initial content of urea in each soil layer (ppm). Also used onReset
    /// </summary>
    private double[] reset_ureappm;

    /// <summary>
    /// Initial content of NH4 in each soil layer (ppm). Also used onReset
    /// </summary>
    private double[] reset_nh4ppm;

    /// <summary>
    /// Initial content of NO3 in each soil layer (ppm). Also used onReset
    /// </summary>
    private double[] reset_no3ppm;

    #endregion

    #region Mineral N amounts

    ///// <summary>
    ///// Internal variable holding the urea amounts
    ///// </summary>
    //private double[] _urea;

    ///// <summary>
    ///// Internal variable holding the nh4 amounts
    ///// </summary>
    //private double[] _nh4;

    ///// <summary>
    ///// Internal variable holding the no3 amounts
    ///// </summary>
    //private double[] _no3 = null;

    #endregion

    #region Organic C and N amounts

    ///// <summary>
    ///// Carbon amount in FOM pools
    ///// </summary>
    //private double[][] fom_c_pool = new double[3][];

    ///// <summary>
    ///// Nitrogen amount in FOM pools
    ///// </summary>
    //private double[][] fom_n_pool = new double[3][];

    #endregion

    #region Deltas in mineral nitrogen

    /// <summary>
    /// Variations in urea as given by another component
    /// </summary>
    /// <remarks>
    /// This property checks changes in the amount of urea at each soil layer
    ///  - If values are not supplied for all layers, these will be assumed zero
    ///  - If values are supplied in excess, these will ignored
    ///  - Each value is tested whether it is within bounds, then it is added to the actual amount, this amount is then tested for its bounds
    /// </remarks>
    private double[] dlt_urea
    {
        set
        {
            // pass the value to patches
            for (int k = 0; k < Patch.Count; k++)
                Patch[k].dlt_urea = value;
        }
    }

    /// <summary>
    /// Variations in nh4 as given by another component
    /// </summary>
    private double[] dlt_nh4
    {
        set
        {
            // pass the value to patches
            for (int k = 0; k < Patch.Count; k++)
                Patch[k].dlt_nh4 = value;
        }
    }

    /// <summary>
    /// Variations in no3 as given by another component
    /// </summary>
    private double[] dlt_no3
    {
        set
        {
            // pass the value to patches
            for (int k = 0; k < Patch.Count; k++)
                Patch[k].dlt_no3 = value;
        }
    }

    #endregion

    #region Decision auxiliary variables

    /// <summary>
    /// Marker for whether initialisation has been finished or not
    /// </summary>
    private bool initDone = false;

    /// <summary>
    /// Marker for whether a reset is going on
    /// </summary>
    private bool isResetting = false;

    /// <summary>
    /// Indicates whether soil profile reduction is allowed (from erosion)
    /// </summary> 
    private bool ProfileReductionAllowed = false;

    /// <summary>
    /// Indicates whether organic solutes are to be simulated
    /// </summary>
    /// <remarks>
    /// It should always be false, as organic solutes are not implemented yet
    /// </remarks>
    private bool OrganicSolutesAllowed = false;

    /// <summary>
    /// Indicates whether simpleSoilTemp is allowed
    /// </summary>
    private bool SimpleSoilTempAllowed = false;

    /// <summary>
    /// Marker for whether external soil temperature is supplied, otherwise use internal
    /// </summary>
    private bool usingSimpleSoilTemp = false;

    /// <summary>
    /// Marker for whether external ph is supplied, otherwise default is used
    /// </summary>
    private bool usingSimpleSoilpH = false;

    /// <summary>
    /// whether water soluble carbon calcualtion uses new C pools
    /// </summary>
    private bool usingNewPools = false;

    /// <summary>
    /// whether exponential function is used to compute water soluble carbon
    /// </summary>
    private bool usingExpFunction = false;

    #endregion

    #region Residue decomposition information

    /// <summary>
    /// Name of residues decomposing
    /// </summary>
    private string[] residueName;

    /// <summary>
    /// Type of decomposing residue
    /// </summary>
    private string[] residueType;

    /// <summary>
    /// Potential residue C decomposition (kg/ha)
    /// </summary>
    private double[] pot_c_decomp;

    /// <summary>
    /// Potential residue N decomposition (kg/ha)
    /// </summary>
    private double[] pot_n_decomp;

    /// <summary>
    /// Potential residue P decomposition (kg/ha)
    /// </summary>
    private double[] pot_p_decomp;

    #endregion

    #region Miscelaneous

    /// <summary>
    /// Total C content at the beginning of the day
    /// </summary>
    public double TodaysInitialC;

    /// <summary>
    /// Total N content at the beginning of the day
    /// </summary>
    public double TodaysInitialN;

    /// <summary>
    /// The initial FOM distribution, a 0-1 fraction for each layer
    /// </summary>
    private double[] FOMiniFraction;

    /// <summary>
    /// Type of FOM being used
    /// </summary>
    private int fom_type;

    /// <summary>
    /// The C:N ratio of each fom pool
    /// </summary>
    private double[] fomPoolsCNratio;

    /// <summary>
    /// Factor for converting kg/ha into ppm
    /// </summary>
    private double[] convFactor;

    /// <summary>
    /// Factor reducing nitrification due to the presence of a inhibitor
    /// </summary>
    private double[] InhibitionFactor_Nitrification = null;

    #endregion

    #region CNpatch related variables

    /// <summary>
    /// Minimum relative area (fraction of paddock) for any patch
    /// </summary>
    private double MinimumPatchArea = 1.0;

    /// <summary>
    /// The approach used for partitioning the N between patches
    /// </summary>
    private string PatchNPartitionApproach;

    /// <summary>
    /// Whether auto amalgamation of patches is allowed
    /// </summary>
    private bool PatchAmalgamationAllowed = false;

    /// <summary>
    /// Approach used in auto amalgamation of patches
    /// </summary>
    private string PatchAmalgamationApproach = "CompareAll";

    /// <summary>
    /// Approach used to define 'base' patch
    /// </summary>
    private string PatchbasePatchApproach = "AreaBased";

    /// <summary>
    /// Layer down to which test for diffs are made (upon auto amalgamation)
    /// </summary>
    private int LayerDepthToTestDiffs;

    /// <summary>
    /// A description of the module sending a change in soil nitrogen (used for partitioning)
    /// </summary>
    private string senderModule;

    /// <summary>
    /// Maximum NH4 uptake rate for plants (ppm/day) (only used when dealing with patches)
    /// </summary>
    private double reset_MaximumNH4Uptake;

    /// <summary>
    /// Maximum NO3 uptake rate for plants (ppm/day) (only used when dealing with patches)
    /// </summary>
    private double reset_MaximumNO3Uptake;

    /// <summary>
    /// Maximum NH4 uptake rate for plants (only used when dealing with patches)
    /// </summary>
    private double[] MaximumNH4UptakeRate;

    /// <summary>
    /// Maximum NO3 uptake rate for plants (only used when dealing with patches)
    /// </summary>
    private double[] MaximumNO3UptakeRate;
        
    /// <summary>
    /// The maximum amount of N that is made available to plants in one day
    /// </summary>
    private double maxTotalNAvailableToPlants;

    #endregion

    #endregion internal variables

    #region Types and structures

    /// <summary>
    /// The parameters to compute a exponential type function (used for example for temperature factor)
    /// </summary>
    private struct BentStickData
    {
        // this is a bending stick type data

        /// <summary>
        /// Optimum temperature, when factor is equal to one
        /// </summary>
        public double[] xValueForOptimum;
        /// <summary>
        /// Value of factor when temperature is equal to zero celsius
        /// </summary>
        public double[] yValueAtZero;
        /// <summary>
        /// Exponent defining the curvature of the factors
        /// </summary>
        public double[] CurveExponent;
    }

    /// <summary>
    /// Lists of x and y values used to describe certain a 'broken stick' function (e.g. moisture factor)
    /// </summary>
    private struct BrokenStickData
    {
        /// <summary>
        /// The values in the x-axis
        /// </summary>
        public double[] xVals;
        /// <summary>
        /// The values in the y-axis
        /// </summary>
        public double[] yVals;
    }

    #endregion
}
