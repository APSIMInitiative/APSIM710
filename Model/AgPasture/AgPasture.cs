using System;
using System.Collections.Generic;
using System.Text;
using System.Linq;
using System.Linq.Expressions;
using ModelFramework;
using System.Xml;
using System.Xml.Schema;
using CSGeneral;

/// <summary>
/// A multi-species pasture model
/// </summary>
public class AgPasture
{
	private Species[] SP;

	#region Parameters for initialisation

	//component constant
	private const float SVPfrac = 0.66F;
	private NewMetType MetData = new NewMetType();  // Daily Met Data
	
    private int NumSpecies = 0;
    
    //parameters whose initial values are taken from the .xml

    [Param]
    [Description("Name of the sward mix")]
    [Units("")]
    private string thisCropName = "";

    [Param]
    [Description("Name of species to simulate")]
    [Units("")]
    private string[] speciesToSimulate = null;

    [Param]
    [Description("Actual name of each species")]
    [Units("")]
    private string[] speciesName = null;
    [Param]
	[Description("Plant type for micromet")]
	[Units("")]
	private string[] micrometType = null;
	[Param]
	[Description("Photosynthesis pathway (C3 or C4)")]
	[Units("3/4")]
	private double[] photoPath = null;
    [Param]
    [Description("Whether the species is legume (1=yes, 0=no)")]
    [Units("0/1")]
    private double[] isLegume = null;

    [Param]
    [Description("Reference leaf C assimilation during photosynthesis")]
    [Units("mgCO2/m^2/s")]
    private double[] Pm;
    [Param]
    [Description("Pgrowth/Pgross")]
    [Units("")]
    private double[] growthEfficiency;
    [Param]
    [Description("Maintenance respiration")]
    [Units("%")]
    private double[] maintRespiration;
    [Param]
    [Description("Leaf gross photosynthesis rate")]
    [Units("mg CO2/J")]
    private double[] alphaPhoto;
    [Param]
    [Description("photosynthesis curvature parameter")]
    [Units("J/kg/s")]
    private double[] thetaPhoto;

    [Param]
    [Description("Light extinction coefficient")]
    [Units("")]
    private double[] lightExtCoeff;

	[Param]
	[Description("Minimum temperature for growth")]
	[Units("")]
	private double[] growthTmin;
    [Param]
    [Description("Optimum temperature for growth")]
    [Units("")]
    private double[] growthTopt;
    [Param]
    [Description("reference temperature for growth")]
    [Units("")]
    private double[] growthTref;
    [Param]
	[Description("Coefficient q on temperature function for plant growth")]
	[Units("")]
	private double[] growthTq;

    [Param]
    [Description("whether heat stress will be use")]
    [Units("")]
    private string[] useHeatStress;
    [Param]
	[Description("onset tempeature for heat effects")]
	[Units("")]
	private double[] heatOnsetT;
	[Param]
	[Description("full temperature for heat effects")]
	[Units("")]
	private double[] heatFullT;
	[Param]
	[Description("temperature sum for recovery - sum of (25-mean)")]
	[Units("")]
	private double[] heatSumT;
    [Param]
    [Description("")]
    [Units("oC")]
    private double[] heatRecoverT;
    [Param]
    [Description("")]
    [Units("")]
    private double[] heatTq;

    [Param]
    [Description("whether heat stress will be use")]
    [Units("")]
    private string[] useColdStress;
    [Param]
	[Description("onset tempeature for cold effects")]
	[Units("")]
	private double[] coldOnsetT;
	[Param]
	[Description("full tempeature for cold effects")]
	[Units("")]
	private double[] coldFullT;
	[Param]
	[Description("temperature sum for recovery - sum of means")]
	[Units("")]
	private double[] coldSumT;
    [Param]
    [Description("")]
    [Units("oC")]
    private double[] coldRecoverT;
    [Param]
    [Description("")]
    [Units("")]
    private double[] coldTq;

    [Param]
    [Description("Base CO2 content in atmosphere")]
    [Units("")]
    private double[] referenceCO2;
    [Param]
    [Description("")]
    [Units("")]
    private double[] CO2PmaxScale;
    [Param]
    [Description("")]
    [Units("")]
    private double[] CO2NScale;
    [Param]
    [Description("")]
    [Units("")]
    private double[] CO2NMin;
    [Param]
    [Description("")]
    [Units("")]
    private double[] CO2NCurvature;

	[Param]
	[Description("Maximum fraction of biomass allocated to roots")]
	[Units("0-1")]
	private double[] maxRootFraction;

	[Param]
    [Description("Factor for increasing DM allocation to shoot during reproductive growth")]
	[Units("0-1")]
	private double[] allocationSeasonF;

    [Param]
    [Description("")]
	[Units("0-1")]
	private double[] StartHighAllocation;
    [Param]
    [Description("")]
	[Units("0-1")]
	private double[] DurationHighAllocation;
    [Param]
    [Description("")]
	[Units("0-1")]
	private double[] ShoulderHighAllocation;


    [Param]
    [Description("Whether DM allocation (shoot/root) will be adjusted using latitude")]
    [Units("yes/no")]
    private string[] useLatitudeFunction;

    [Param]
    [Description("Reference latitude, beyond which DM allocation is hardly affected")]
    [Units("degrees")]
    private double[] ReferenceLatitude;
    [Param]
    [Description("Exponent of function for defining the start of period with high allocation to shoot")]
    [Units("-")]
    private double[] paramALatFunction;

    [Param]
    [Description("Define the duration of onset phase with high allocation, fraction of main phase")]
    [Units("0-1")]
    private double[] onsetFacLatFunction;
    [Param]
    [Description("Define the duration of outset phase with high allocation, fraction of main phase")]
    [Units("0-1")]
    private double[] outsetFacLatFunction;
    [Param]
    [Description("Maximum duration of the shoulder phases with high DM allocation")]
    [Units("days")]
    private double[] maxShoulderLatFunction;
    [Param]
    [Description("Minimum duration of main phase (plateau) with high DM allocation")]
    [Units("days")]
    private double[] minPlateauLatFunction;
    [Param]
    [Description("Exponent of function defining the duration of main phase with high allocation")]
    [Units("days")]
    private double[] paramBLatFunction;

    [Param]
    [Description("Maximum increase in DM allocation to shoot during reproductive growth")]
    [Units("0-1")]
    private double[] allocationMax;
    [Param]
    [Description("Exponent of function defining the duration of main phase with high allocation")]
    [Units("days")]
    private double[] paramCLatFunction;

	// this has been removed from params as it is not actually used
	//private double[] leafRate;
	[Param]
	[Description("Fraction of new growth allocated to leaf (0-1)")]
	[Units("")]
	private double[] fLeaf;
	[Param]
	[Description("Fraction of new growth allocated to stolon (0-1)")]
	[Units("")]
	private double[] fStolon;

    private double[] specificLeafArea;
    [Param]
    [Description("Specific leaf area, per dry matter weight")]
    [Units("m^2/kgDM")]
    public double[] SpecificLeafArea
    {
        get { return specificLeafArea; }
        set
        {
            specificLeafArea = new double[value.Length];
            for (int s = 0; s < value.Length; s++)
                specificLeafArea[s] = value[s];
        }
    }

    private double[] specificRootLength;
    [Param]
    [Description("Specific root length, per dry matter weight")]
    [Units("m/gDM")]
    public double[] SpecificRootLength
    {
        get { return specificRootLength; }
        set
        {
            specificRootLength = new double[value.Length];
            for (int s = 0; s < value.Length; s++)
                specificRootLength[s] = value[s];
        }
    }

    [Param]
    [Description("Factor for adjusting DM turnover of growing tissue")]
    [Units("<0.0")]
    private double[] facGrowingTissue;
    [Param]
	[Description("Senescence rate for shoot (live to dead material)")]
	[Units("")]
	private double[] rateLive2Dead;
    [Param]
    [Description("Senescence rate for stolon (live to dead material)")]
    [Units("")]
    private double[] rateLive2DeadStolon;
    [Param]
	[Description("Littering rate (dead to litter)")]
	[Units("")]
	private double[] rateDead2Litter;
	[Param]
	[Description("Senescence rate for roots (live to soil FOM)")]
	[Units("")]
	private double[] rateRootSen;

    [Param]
    [Description("Mass flux minimum temperature")]
    [Units("")]
    private double[] massFluxTmin;
    [Param]
    [Description("Mass flux optimum temperature")]
    [Units("")]
    private double[] massFluxTopt;
    [Param]
    [Description("")]
    [Units("")]
    private double[] massFluxTq;
    [Param(MinVal = 1.0)]
    [Description("Mass flux scale factor at GLFwater=0.0")]
    [Units("")]
    private double[] massFluxW0;
    [Param]
    [Description("Mass flux optimum GLFwater=0.5")]
    [Units("")]
    private double[] massFluxWopt;
    [Param]
    [Description("Mass flux exponent")]
    [Units("")]
    private double[] massFluxDeadWq;

    
    /// <summary>
    /// Effective stocking rate trampling on pasture (increase senescence)
    /// </summary>
    public double StockRate
    {
        get { return Species.stockingRate; }
        set { Species.stockingRate = value; }
    }
    [Param]
	[Description("Parameter for stock influence on senescence")]
	[Units("")]
	private double[] stockParameter;
    [Param]
    [Description("Coefficient for using luxury N from tissue 2 to Nremob")]
    [Units("0-1")]
    private double[] Kappa2_Remob;
    [Param]
    [Description("Coefficient for using luxury N from tissue 3 to Nremob")]
    [Units("0-1")]
    private double[] Kappa3_Remob;
    [Param]
    [Description("Coefficient for partitioning non-used Nremob into tissue 4")]
    [Units("0-1")]
    private double[] Kappa4_Remob;

	[Param]
	[Description("Digestibility of live plant material (0-1)")]
	[Units("")]
	private double[] digestLive;
	[Param]
	[Description("Digestibility of dead plant material (0-1)")]
	[Units("")]
	private double[] digestDead;

	[Param(IsOptional = true)]
	private double[] dmtotal = null;	//This would be deleted (it has been replaced by dmshoot - but keep as optional for back-compatibility)

    // -- Initial values (to be input from interface, overwrites the values of dmshoot, dmroot, and rootdepth
    [Param(IsOptional = true)]
    private double[] iniShootDM = null;
    [Param(IsOptional = true)]
    private double[] iniRootDM = null;
    [Param(IsOptional = true)]
    private double[] iniRootDepth = null;

	[Param]
	[Description("Shoot dry weight")]
	[Units("kgDM/ha")]
	private double[] dmshoot;  //default initial shoot mass (RCichota May 2014, change from dmtotal to dmshoot)

	//following varibles will be calculated, not [Param] any more
	private double[] dmleaf1;            //leaf 1 (kg/ha)
	private double[] dmleaf2;            //leaf 2 (kg/ha)
	private double[] dmleaf3;            //leaf 3 (kg/ha)
	private double[] dmleaf4;            //leaf dead (kg/ha)
	private double[] dmstem1;            //sheath and stem 1 (kg/ha)
	private double[] dmstem2;            //sheath and stem 2 (kg/ha)
	private double[] dmstem3;            //sheath and stem 3 (kg/ha)
	private double[] dmstem4;            //sheath and stem dead (kg/ha)
	private double[] dmstol1;            //stolon 1 (kg/ha)
	private double[] dmstol2;            //stolon 2 (kg/ha)
	private double[] dmstol3;            //stolon 3 (kg/ha)

	[Param]
	[Description("Root dry weight")]
	[Units("kgDM/ha")]
	private double[] dmroot;

	[Param]
	[Description("Minimum green DM")]
	[Units("kgDM/ha")]
	private double[] dmgreenmin;
	[Param]
	[Description("Minimum dead DM")]
	[Units("kgDM/ha")]
	private double[] dmdeadmin;

	[Param]
	[Description("Fractions of initial dmshoot for each biomass pool, for non-legumes")]
	[Units("0-1")]
	private double[] initialDMFractions_grass;

	[Param]
	[Description("Fractions of initial dmshoot for each biomass pool, for legume species")]
	[Units("0-1")]
	private double[] initialDMFractions_legume;

	[Param]
	[Description("Optimum N concentration of leaves (no stress)")]
	[Units("%")]
	private double[] NconcOptimum_leaves;
	[Param]
	[Description("Maximum N concentration of leaves (luxury uptake)")]
	[Units("%")]
	private double[] NconcMaximum_leaves;
	[Param]
	[Description("Minimum N concentration of leaves (dead leaves)")]
	[Units("%")]
	private double[] NconcMinimum_leaves;
	[Param]
	[Description("N concentration for stems, relative to leaves")]
	[Units("0-1")]
	private double[] RelativeNconc_Stems;
	[Param]
	[Description("N concentration for stolons, relative to leaves")]
	[Units("0-1")]
	private double[] RelativeNconc_Stolons;
	[Param]
	[Description("N concentration for roots, relative to leaves")]
	[Units("0-1")]
	private double[] RelativeNconc_Roots;
	[Param]
	[Description("N concentration for plants at stage 2 (developing), relative to optimum")]
	[Units("0-1")]
	private double[] RelativeNconc_stage2;
	[Param]
	[Description("N concentration for plants at stage 3 (mature), relative to optimum")]
	[Units("0-1")]
	private double[] RelativeNconc_stage3;

	//following varibles will be calculated, not [Param]
	private double[] Ncleaf1;    //leaf 1 (critical N %)
	private double[] Ncleaf2;    //leaf 2
	private double[] Ncleaf3;    //leaf 3
	private double[] Ncleaf4;    //leaf dead
	private double[] Ncstem1;    //sheath and stem 1
	private double[] Ncstem2;    //sheath and stem 2
	private double[] Ncstem3;    //sheath and stem 3
	private double[] Ncstem4;    //sheath and stem dead
	private double[] Ncstol1;    //stolon 1
	private double[] Ncstol2;    //stolon 2
	private double[] Ncstol3;    //stolon 3
	private double[] Ncroot;    //root

	[Param]
	[Description("Minimum fraction of N demand fixed by legumes")]
	[Units("")]
	private double[] NMinFix;
	[Param]
	[Description("Maximum fraction of N demand fixed by legumes")]
	[Units("")]
	private double[] NMaxFix;

	[Param]
	[Description("Coefficient for modifying the effect of N stress on plant growth")]
	[Units("")]
	private double[] NdilutCoeff;

    [Param]
    [Description("Effects of water uptake/demand on plant growth")]
    [Units("0-1")]
    private double[] waterStressFactor;
    [Param]
    [Description("Soil moisture saturation effects on growth")]
    [Units("0-1")]
    private double[] soilSatFactor;

	[Param]
	[Description("Generic growth limiting factor")]
	[Units("0-1")]
	private double[] Frgr;

    /// <summary>
    /// Current root depth (mm)
    /// </summary>
    private double[] myRootDepth;
    [Param]
    [Description("Initial root depth")]
    [Units("mm")]
    private double[] rootDepth
    {
        get
        {
            return myRootDepth;
        }
        set
        {
            myRootDepth = value;
            p_rootFrontier = 0;
            foreach (double x in myRootDepth)
            {
                p_rootFrontier = (x > p_rootFrontier) ? x : p_rootFrontier;
            }
        }
    }

    private int p_RootDistributionMethod = 0;
    [Param]
    [Output]
    [Description("Root distribution method")]
    [Units("")]
    public string RootDistributionMethod
    {
        get
        {
            switch (p_RootDistributionMethod)
            {
                case 1:
                    return "UserDefined";
                case 2:
                    return "ExpoLinear";
                default:
                    // case = 0
                    return "Homogenous";
            }
        }
        set
        {
            if (value.ToLower() == "userdefined")
                p_RootDistributionMethod = 1;
            else if (value.ToLower() == "expolinear")
                p_RootDistributionMethod = 2;
            else      // default = homogeneous
                p_RootDistributionMethod = 0;
        }
    }

    private double[] p_ExpoLinearDepthParam;
    [Param]
    [Output]
    [Description("Fraction of root depth where its proportion starts to decrease")]
    [Units("0-1")]
    public double[] ExpoLinearDepthParam
    {
        get { return p_ExpoLinearDepthParam; }
        set
        {
            p_ExpoLinearDepthParam = new double[value.Length];
            for (int i = 0; i < value.Length; i++)
                p_ExpoLinearDepthParam[i] = value[i];
            if (p_ExpoLinearDepthParam[0] == 1.0)
                p_RootDistributionMethod = 0;	// effectivelly it defines a homogeneous distribution
        }
    }

    private double[] p_ExpoLinearCurveParam;
    [Param]
    [Output]
    [Description("Exponent to determine mass distribution in the soil profile")]
    [Units("")]
    public double[] ExpoLinearCurveParam
    {
        get { return p_ExpoLinearCurveParam; }
        set
        {
            p_ExpoLinearCurveParam = new double[value.Length];
            for (int i = 0; i < value.Length; i++)
                p_ExpoLinearCurveParam[i] = value[i];
            if (p_ExpoLinearCurveParam[0] == 0.0)
                p_RootDistributionMethod = 0;	// It is impossible to solve, but its limit is a homogeneous distribution
        }
    }

	//Soil & roots
	[Param]
	[Output]
	[Description("Relative root length density")]
	[Units("0-1")]
	public double[] rlvp = null;
	[Param]
	public double[] kl = null;      //SW uptake parameter (/day)
	[Param]
	public double[] ll = null;      //Crop Lower Limit (mm/mm)
	[Param]
	public double[] xf = null;      //effects of X-factors on root growth(fraction)

	[Param]
	[Description("Whether water uptake is calculated by agpasture or apsim")]
	[Units("calc/apsim")]
	public string WaterUptakeSource = "calc";
	[Param]
	[Description("Whether N uptake is calculated by agpasture or apsim")]
	[Units("calc/apsim")]
	public string NUptakeSource = "calc";


	[Param]
	[Description("Weight factor defining the preference level for green DM")]
	private double[] PreferenceForGreenDM;
	[Param]
	[Description("Weight factor defining the preference level for dead DM")]
	private double[] PreferenceForDeadDM;


    [Link]
    private LinearInterpolation FVPDFunction = null;    //Senescence rate is affected by min(gf-N, gf_water)
    [Link]
    private LinearInterpolation HeightMassFN = null;

    //[Param]
    [Description("Whether the alternative N uptake routine is to be used")]
    [Units("yes/no")]
    private string alt_N_uptake = "no";

    // --  Paramters for annuals were de-actived (hiden) as they are not really used (RCichota, Oct/2014) ----
    //[Param]
    [Description("Species type (1=annual,0=perennial)")]
    [Units("0/1")]
    private double[] isAnnual = null;
    //[Param]
    [Description("Earliest day of emergence (for annuals only)")]
    [Units("")]
    private double[] dayEmerg;
    //[Param]
    [Description("Earliest month of emergence (for annuals only)")]
    [Units("")]
    private double[] monEmerg;
    //[Param]
    [Description("Earliest day of anthesis (for annuals only)")]
    [Units("")]
    private double[] dayAnth;
    //[Param]
    [Description("Earliest month of anthesis (for annuals only)")]
    [Units("")]
    private double[] monAnth;
    //[Param]
    [Description("Days from anthesis to maturity (for annuals only)")]
    [Units("")]
    private double[] daysToMature;

    //[Param]
    [Description("Daily root growth")]
    [Units("(mm)")]
    private double[] dRootDepth;
    //[Param]
    [Description("Maximum root depth")]
    [Units("(mm)")]
    private double[] maxRootDepth;
    // -----------------------------------------------------------------------------------------------------

#endregion

    #region Inputs from othe modules

    [Input]
	public DateTime Today;

	[Input]
	public float[] dlayer;   //Soil Layer Thickness (mm)
	[Input]
	private float[] sw_dep;  //soil water by layer
	[Input]
	private float[] SAT;     //saturation point
	[Input]
	private float[] DUL;     //drainage upper limit (field capacity);
	[Input]
	private float[] no3;     //SNO3dep = new float[dlayer.Length];
	[Input]
	private float[] nh4;     //SNH4dep = new float[dlayer.Length];

	[Input]
	private double day_length = 12;
	[Input]
	private double latitude;
	[Input]
	private int day_of_month;
	[Input]
	private int month;
	[Input]
	private int year;

    [Input(IsOptional = true)]
    [Description("Actual CO2, updated from met and ClimateControl")]
    [Units("")]
    private double co2 = 380;

	#endregion

    #region general intenal variables

    //** Aggregated pasture parameters of all species (wiht a prefix 'p_')
	//p_d... variables are daily changes (delta)
	private double p_dGrowthPot;      //daily growth potential
	private double p_dGrowthW;        //daily growth with water-deficit incoprporated
	private double p_dGrowth;         //daily growth
	private double p_dHerbage;        //daily herbage (total standing DM) increae
	private double p_dLitter;         //daily litter formation
	private double p_dRootSen;        //daily root senescence
	private double p_dNLitter;        //daily litter formation
	private double p_dNRootSen;       //daily root senescence

	//p_... variables are pasture states at a given time (day)
	private double p_fShoot;          //actual fraction of dGrowth to shoot
	private double p_height;          // Canopy height (mm)
	private double p_greenLAI;
	private double p_deadLAI;
	private double p_totalLAI;
	private double p_lightExtCoeff;
	private double p_greenDM;         //green is the live aboveground herbage mass, kgDM/ha
	private double p_deadDM;
	private double p_totalDM;

	private double p_rootMass;        //total root mass
	private double p_rootFrontier;    //depth of root frontier

	//soil
	private double p_bottomRootLayer;   //the soil layer just below root zone
	private double p_soilNdemand;       //plant N demand (shoot + root) for daily growth from soil (excludingfixation and remob)
	// private double p_soilNdemandMax;    //plant N demand with luxury uptake
	private double p_soilNavailable;    //Plant available N in soil kgN/ha, at the present day
	private double p_soilNuptake;       //Plant N uptake, daily
	private float[] SNSupply;
	private float[] SNUptake;
	private double p_Nfix = 0;
	private double p_gfn;               // = effect of p_Nstress on growth

	private double p_waterDemand;   // Daily Soil Water Demand (mm)
	private double p_waterUptake;   // Daily Soil Water uptake (mm)
	private double p_waterSupply;   // plant extractable soil moisture (mm)
	private float[] SWSupply;
	private float[] SWUptake;
	private double p_gfwater;       // = effects of water stress on growth
	private double p_gftemp;

	private double p_harvestDM;              //daily harvested dm
	private double p_harvestN;               //daily harvested n
	private double p_harvestDigest;
	private double p_herbageDigest;
	private bool p_Live = true;              //flag signialling crop is live (not killed)

    //temporary testing, will be removed later when IL1 can be get from micromet
    private int canopiesNum = 1;            //number of canpy including this one
    private double[] canopiesRadn = null;   //Radn intercepted by canopies

    #endregion

    #region Events to be invoked

    [Event]
	public event NewCropDelegate NewCrop;
	[Event]
	public event NewCanopyDelegate New_Canopy;
	[Event]
	public event NewPotentialGrowthDelegate NewPotentialGrowth;
	[Event]
	public event FOMLayerDelegate IncorpFOM;
	[Event]
	public event BiomassRemovedDelegate BiomassRemoved;
	[Event]
	public event WaterChangedDelegate WaterChanged;
	[Event]
	public event NitrogenChangedDelegate NitrogenChanged;

    #endregion

	/// <summary>
	/// Initialise parameters
	/// </summary>
	private void InitParameters()
	{
        // get the actual number os species to simulate
        NumSpecies = speciesToSimulate.Length;

		//Rainss 20110711 - Introduced broken stick root distribution
		// This hack uses rlvp as interface to pass distribution parameters and then recalc rlvp
		// This is not what I would call good pratice due to the parameters technically changing meaning during
		// initilisation, but it will do in the interim.
		if (rlvp.Length == NumSpecies)
		{
			p_RootDistributionMethod = 2;
            for (int s = 0; s < NumSpecies; s++)
            {
                p_ExpoLinearDepthParam[s] = rlvp[s];
                p_ExpoLinearCurveParam[s] = 1.0;
            }
			// This has been maintained for backwards compatibility, use should be avoided
		}

		// rlvp is used as input only, in the calculations it has been usper-seeded by RootFraction (the proportion of roots mass in each layer)
		// The RootFraction should add up to 1.0 over the soil profile
		RootFraction = RootProfileDistribution();

		// check that initialisation fractions have been supplied accordingly
		Array.Resize(ref initialDMFractions_grass, 11);
		Array.Resize(ref initialDMFractions_legume, 11);

		//Create and initialise each species  -----------------------------------------------------------------------------
        // added by RCichota, Oct/2003 - check which species to run
        
        // check number of species
        if (NumSpecies > speciesName.Length)
            throw new Exception("Number of species to simulate is greater than the number of species for which parameters were given");
        // check for duplicates and non-existing species
        for (int s1 = 0; s1 < NumSpecies; s1++)
        {
            for (int s2 = s1+1; s2 < NumSpecies; s2++)
                if (speciesToSimulate[s2].ToLower() == speciesToSimulate[s1].ToLower())
                    throw new Exception("The name \"" + speciesToSimulate[s1] + "\" was given more than once. Only one is allowed");

            int myCount = 0;
            for (int s2 = 0; s2 < speciesName.Length; s2++)
                if (speciesName[s2].ToLower() == speciesToSimulate[s1].ToLower())
                    myCount += 1;
            if(myCount < 1)
                throw new Exception("The name \"" + speciesToSimulate[s1] +"\" does not correspond to any parameterised species, check spelling");
        }

        SP = new Species[NumSpecies];
        for (int s1 = 0; s1 < NumSpecies; s1++)
            for (int s2 = 0; s2 < speciesName.Length; s2++)
            {
                if (speciesName[s2].ToLower() == speciesToSimulate[s1].ToLower())
                {
                    SP[s1] = new Species();
                    InitSpeciesValues(s1, s2);
                    break;
                }
            }

		FractionToHarvest = new double[NumSpecies];

		//Initialising the aggregated pasture parameters from initial valuses of each species
		p_rootFrontier = 0.0;
		p_rootMass = 0.0;
		double sum_fShoot = 0.0;
		double sum_lightExtCoeff = 0.0;

		for (int s = 0; s < NumSpecies; s++)
		{
			//accumulate LAI of all species
			p_greenLAI += SP[s].greenLAI;
			p_deadLAI += SP[s].deadLAI;

			p_greenDM += SP[s].dmgreen;
			p_deadDM += SP[s].dmdead;

			//accumulate the sum for weighted average
			sum_fShoot += SP[s].fShoot * SP[s].dmshoot;
			sum_lightExtCoeff += SP[s].lightExtCoeff * SP[s].totalLAI;

			//Set the deepest root frontier
			if (SP[s].rootDepth > p_rootFrontier)
				p_rootFrontier = SP[s].rootDepth;

			p_rootMass += SP[s].dmroot;

		}
		p_totalLAI = p_greenLAI + p_deadLAI;
		p_totalDM = p_greenDM + p_deadDM;

		if (p_totalDM == 0) { p_fShoot = 0; }
		else { p_fShoot = sum_fShoot / p_totalDM; }

		if (p_totalLAI == 0) { p_lightExtCoeff = 0.5; }
		else { p_lightExtCoeff = sum_lightExtCoeff / p_totalLAI; }

		//init
		p_dGrowthPot = 0.0;
		p_dGrowthW = 0.0;
		p_dGrowth = 0.0;

		p_dLitter = 0.0;         //daily litter formation
		p_dRootSen = 0.0;        //daily root senescence
		p_dNLitter = 0.0;        //daily litter formation N
		p_dNRootSen = 0.0;       //daily root senescence N

		//Parameters for environmental factors
		p_soilNdemand = 0;
		p_soilNavailable = 0;
		p_soilNuptake = 0;
		p_gfn = 0;

		p_waterDemand = 0;
		p_waterUptake = 0;
		p_gfwater = 0;

		//if (rlvp.Length != dlayer.Length)
		//{
		//    String msg = "Warning: Number of layers specified for root length density (rlvp) is different ";
		//    msg += "\nfrom the number of soil layers.The simulation will run using the minimum of the two.";
		//    Console.WriteLine(msg);
		//}
	}

    /// <summary>
    /// Set parameter values for each species in the sward
	/// </summary>
	/// <param name="s1">The index for the species being setup</param>
	/// <param name="s2">The index for the species in the parameter set</param>
    private void InitSpeciesValues(int s1, int s2)
	{
		SP[s1].speciesName = speciesName[s2];
		SP[s1].micrometType = micrometType[s2];

        SP[s1].isLegume = ((int)isLegume[s2] == 1);
        SP[s1].photoPath = (int)photoPath[s2];


        // -- deactivating all the annual stuff (never really used)
        //if (isAnnual[s] == 1) SP[s].isAnnual = true;
        //else SP[s].isAnnual = false;
        SP[s1].isAnnual = false;

        SP[s1].dayEmerg = 1; // (int)dayEmerg[s];
        SP[s1].monEmerg = 1; //(int)monEmerg[s];
        SP[s1].dayAnth = 1; //(int)dayAnth[s];
        SP[s1].monAnth = 1; //(int)monAnth[s];
        SP[s1].daysToMature = 1; //(int)daysToMature[s];
		if (SP[s1].isAnnual) //calulate days from Emg to Antheis
			SP[s1].CalcDaysEmgToAnth();
        SP[s1].dRootDepth = 1; //(int)dRootDepth[s];
        SP[s1].maxRootDepth = 10; //(int)maxRootDepth[s];


        SP[s1].allocationSeasonF = allocationSeasonF[s2];
        SP[s1].startHighAllocation = StartHighAllocation[s2];
        SP[s1].durationHighAllocation = DurationHighAllocation[s2];
        SP[s1].shoulderHighAllocation = ShoulderHighAllocation[s2];
        SP[s1].usingLatFunctionFShoot = (useLatitudeFunction[s2].ToLower() == "yes");
        SP[s1].referenceLatitude = ReferenceLatitude[s2];
        SP[s1].paramALatFunction = paramALatFunction[s2];
        SP[s1].onsetFacLatFunction = onsetFacLatFunction[s2];
        SP[s1].outsetFacLatFunction = outsetFacLatFunction[s2];
        SP[s1].maxShoulderLatFunction = maxShoulderLatFunction[s2];
        SP[s1].minPlateauLatFunction = minPlateauLatFunction[s2];
        SP[s1].paramBLatFunction = paramBLatFunction[s2];
        SP[s1].allocationMax = allocationMax[s2];
        SP[s1].paramCLatFunction = paramCLatFunction[s2];


        SP[s1].Pm = Pm[s2];                            //reference leaf co2 mg/m^2/s maximum
        SP[s1].maintRespiration = maintRespiration[s2];    //in %
        SP[s1].growthEfficiency = growthEfficiency[s2];
        SP[s1].alphaPhoto = alphaPhoto[s2];
        SP[s1].thetaPhoto = thetaPhoto[s2];
        SP[s1].lightExtCoeff = lightExtCoeff[s2];
        
        SP[s1].growthTmin = growthTmin[s2];
        SP[s1].growthTopt = growthTopt[s2];
        SP[s1].growthTref = growthTref[s2];
        SP[s1].growthTq = growthTq[s2];
        SP[s1].usingHeatStress = (useHeatStress[s2].ToLower() == "yes");
		SP[s1].heatOnsetT = heatOnsetT[s2];            //onset tempeature for heat effects
		SP[s1].heatFullT = heatFullT[s2];            //full temperature for heat effects
        SP[s1].heatTq = heatTq[s2];
		SP[s1].heatSumT = heatSumT[s2];                //temperature sum for recovery - sum of (25-mean)
        SP[s1].heatRecoverT = heatRecoverT[s2];
        SP[s1].usingColdStress = (useColdStress[s2].ToLower() == "yes");
        SP[s1].coldOnsetT = coldOnsetT[s2];           //onset tempeature for cold effects
		SP[s1].coldFullT = coldFullT[s2];            //full tempeature for cold effects
        SP[s1].coldTq = coldTq[s2];
        SP[s1].coldSumT = coldSumT[s2];                //temperature sum for recovery - sum of means
        SP[s1].coldRecoverT = coldRecoverT[s2];

        //CO2
        SP[s1].referenceCO2 = referenceCO2[s2];
        SP[s1].CO2PmaxScale = CO2PmaxScale[s2];
        SP[s1].CO2NScale = CO2NScale[s2];
        SP[s1].CO2NMin = CO2NMin[s2];
        SP[s1].CO2NCurvature = CO2NCurvature[s2];

		SP[s1].rateLive2Dead = rateLive2Dead[s2];
        SP[s1].facGrowingTissue = facGrowingTissue[s2];
        SP[s1].rateLive2DeadStolon = rateLive2DeadStolon[s2];
		SP[s1].rateDead2Litter = rateDead2Litter[s2];
		SP[s1].rateRootSen = rateRootSen[s2];
        SP[s1].massFluxTmin = massFluxTmin[s2];
        SP[s1].massFluxTopt = massFluxTopt[s2];
        SP[s1].massFluxTq = massFluxTq[s2];
        SP[s1].massFluxW0 = massFluxW0[s2];
        SP[s1].massFluxWopt = massFluxWopt[s2];
        SP[s1].exponentGLFW2dead = massFluxDeadWq[s2];
        SP[s1].stockParameter = stockParameter[s2];
		
        SP[s1].maxSRratio = (1 - maxRootFraction[s2]) / maxRootFraction[s2]; // The input is actually the max % allocated to roots
		SP[s1].leafRate = 0.0;
		SP[s1].fLeaf = fLeaf[s2];
		SP[s1].fStolon = fStolon[s2];
        SP[s1].specificLeafArea = specificLeafArea[s2];
        SP[s1].specificRootLength = specificRootLength[s2];
        SP[s1].digestLive = digestLive[s2];
		SP[s1].digestDead = digestDead[s2];

        if (iniRootDepth[s1] > 0.0)
            myRootDepth[s2] = iniRootDepth[s1];
        SP[s1].rootDepth = (int)myRootDepth[s2];

        if (dmtotal[s2] >= 0.0)
        { // a value for dmtotal was supplied, assume that it should overwrite dmshoot (needed for backward-compatibility-RCichota, May2014)
            dmshoot[s2] = dmtotal[s2];
        }
        if (iniShootDM[s1] > 0.0)
            dmshoot[s2] = iniShootDM[s1];
        SP[s1].dmshoot = dmshoot[s2];

        if (SP[s1].dmshoot == 0.0) SP[s1].phenoStage = 0;
		else SP[s1].phenoStage = 1;

        if (!SP[s1].isLegume)
        {
            SP[s1].dmleaf1 = SP[s1].dmshoot * initialDMFractions_grass[0];
            SP[s1].dmleaf2 = SP[s1].dmshoot * initialDMFractions_grass[1];
            SP[s1].dmleaf3 = SP[s1].dmshoot * initialDMFractions_grass[2];
            SP[s1].dmleaf4 = SP[s1].dmshoot * initialDMFractions_grass[3];
            SP[s1].dmstem1 = SP[s1].dmshoot * initialDMFractions_grass[4];
            SP[s1].dmstem2 = SP[s1].dmshoot * initialDMFractions_grass[5];
            SP[s1].dmstem3 = SP[s1].dmshoot * initialDMFractions_grass[6];
            SP[s1].dmstem4 = SP[s1].dmshoot * initialDMFractions_grass[7];
            SP[s1].dmstol1 = SP[s1].dmshoot * initialDMFractions_grass[8];
            SP[s1].dmstol2 = SP[s1].dmshoot * initialDMFractions_grass[9];
            SP[s1].dmstol3 = SP[s1].dmshoot * initialDMFractions_grass[10];
        }
        else //legume
        {
            SP[s1].dmleaf1 = SP[s1].dmshoot * initialDMFractions_legume[0];
            SP[s1].dmleaf2 = SP[s1].dmshoot * initialDMFractions_legume[1];
            SP[s1].dmleaf3 = SP[s1].dmshoot * initialDMFractions_legume[2];
            SP[s1].dmleaf4 = SP[s1].dmshoot * initialDMFractions_legume[3];
            SP[s1].dmstem1 = SP[s1].dmshoot * initialDMFractions_legume[4];
            SP[s1].dmstem2 = SP[s1].dmshoot * initialDMFractions_legume[5];
            SP[s1].dmstem3 = SP[s1].dmshoot * initialDMFractions_legume[6];
            SP[s1].dmstem4 = SP[s1].dmshoot * initialDMFractions_legume[7];
            SP[s1].dmstol1 = SP[s1].dmshoot * initialDMFractions_legume[8];
            SP[s1].dmstol2 = SP[s1].dmshoot * initialDMFractions_legume[9];
            SP[s1].dmstol3 = SP[s1].dmshoot * initialDMFractions_legume[10];
        }

        if (iniRootDM[s1] > 0.0)
            dmroot[s2] = iniRootDM[s1];
		if (dmroot[s2] >= 0.0)
			SP[s1].dmroot = dmroot[s2];
		else
			SP[s1].dmroot = dmshoot[s2] / SP[s1].maxSRratio;

		SP[s1].dmgreenmin = dmgreenmin[s2];
		SP[s1].dmdeadmin = dmdeadmin[s2];

		SP[s1].Frgr = (float)Frgr[s2];

		SP[s1].waterStressFactor = waterStressFactor[s2];
		SP[s1].soilSatFactor = soilSatFactor[s2];

		//init N
		// double Fn = =SP[s].NCO2Effects() //Delay teh [co2] effect to calculating N demand.
		SP[s1].NcstemFr = RelativeNconc_Stems[s2];      //stem Nc as % of leaf Nc
		SP[s1].NcstolFr = RelativeNconc_Stolons[s2];      //stol Nc as % of leaf Nc
		SP[s1].NcrootFr = RelativeNconc_Roots[s2];      //root Nc as % of leaf Nc

		SP[s1].NcRel2 = RelativeNconc_stage2[s2];
		SP[s1].NcRel3 = RelativeNconc_stage3[s2];

		//0.01 is for conversion of % to fraction [i.e., 4% ->0.04]
		SP[s1].NcleafOpt = 0.01 * NconcOptimum_leaves[s2];                  //leaf critical N %)
		SP[s1].NcstemOpt = SP[s1].NcleafOpt * SP[s1].NcstemFr;     //stem
		SP[s1].NcstolOpt = SP[s1].NcleafOpt * SP[s1].NcstolFr;     //stolon
		SP[s1].NcrootOpt = SP[s1].NcleafOpt * SP[s1].NcrootFr;     //root

		SP[s1].NcleafMax = 0.01 * NconcMaximum_leaves[s2];          // NcLeafMax[s] TO INPUT
		SP[s1].NcstemMax = SP[s1].NcleafMax * SP[s1].NcstemFr; //sheath and stem
		SP[s1].NcstolMax = SP[s1].NcleafMax * SP[s1].NcstolFr;    //stolon
		SP[s1].NcrootMax = SP[s1].NcleafMax * SP[s1].NcrootFr;    //root

		SP[s1].NcleafMin = 0.01 * NconcMinimum_leaves[s2];
		SP[s1].NcstemMin = SP[s1].NcleafMin * SP[s1].NcstemFr;
		SP[s1].NcstolMin = SP[s1].NcleafMin * SP[s1].NcstolFr;
		SP[s1].NcrootMin = SP[s1].NcleafMin * SP[s1].NcrootFr;

		//init as optimum
		SP[s1].Ncleaf1 = SP[s1].NcleafOpt;
		SP[s1].Ncleaf2 = SP[s1].NcleafOpt * SP[s1].NcRel2;
		SP[s1].Ncleaf3 = SP[s1].NcleafOpt * SP[s1].NcRel3;
		SP[s1].Ncleaf4 = SP[s1].NcleafMin; //this could become much small depending on [N] in green tisssue

		SP[s1].Ncstem1 = SP[s1].NcstemOpt; //stem [N] is 50% of the leaf [N]
		SP[s1].Ncstem2 = SP[s1].NcstemOpt * SP[s1].NcRel2;
		SP[s1].Ncstem3 = SP[s1].NcstemOpt * SP[s1].NcRel3;
		SP[s1].Ncstem4 = SP[s1].NcstemMin;

		SP[s1].Ncstol1 = SP[s1].NcstolOpt;
		SP[s1].Ncstol2 = SP[s1].NcstolOpt * SP[s1].NcRel2;
		SP[s1].Ncstol3 = SP[s1].NcstolOpt * SP[s1].NcRel3;

		SP[s1].Ncroot = SP[s1].NcrootOpt;

		SP[s1].MaxFix = NMaxFix[s2];   //N-fix fraction when no soil N available, read in later
		SP[s1].MinFix = NMinFix[s2];   //N-fix fraction when soil N sufficient
       
        SP[s1].NdilutCoeff = NdilutCoeff[s2];

		SP[s1].Kappa2 = Kappa2_Remob[s2];
		SP[s1].Kappa3 = Kappa3_Remob[s2];
		SP[s1].Kappa4 = Kappa4_Remob[s2];

		//Init total N in each pool
		SP[s1].Nleaf1 = SP[s1].dmleaf1 * SP[s1].Ncleaf1; //convert % to fraction [i.e., 4% ->0.02]
		SP[s1].Nleaf2 = SP[s1].dmleaf2 * SP[s1].Ncleaf2;
		SP[s1].Nleaf3 = SP[s1].dmleaf3 * SP[s1].Ncleaf3;
		SP[s1].Nleaf4 = SP[s1].dmleaf4 * SP[s1].Ncleaf4;
		SP[s1].Nstem1 = SP[s1].dmstem1 * SP[s1].Ncstem1;
		SP[s1].Nstem2 = SP[s1].dmstem2 * SP[s1].Ncstem2;
		SP[s1].Nstem3 = SP[s1].dmstem3 * SP[s1].Ncstem3;
		SP[s1].Nstem4 = SP[s1].dmstem4 * SP[s1].Ncstem4;
		SP[s1].Nstol1 = SP[s1].dmstol1 * SP[s1].Ncstol1;
		SP[s1].Nstol2 = SP[s1].dmstol2 * SP[s1].Ncstol2;
		SP[s1].Nstol3 = SP[s1].dmstol3 * SP[s1].Ncstol3;
		SP[s1].Nroot = SP[s1].dmroot * SP[s1].Ncroot;

		//calculated, DM and LAI,  species-specific
		SP[s1].updateAggregated();   // agregated properties, such as p_totalLAI

		SP[s1].dGrowthPot = 0;       // daily growth potential
		SP[s1].dGrowthW = 0;          // daily growth actual
		SP[s1].dGrowth = 0;          // daily growth actual
		SP[s1].dGrowthRoot = 0;      // daily root growth
		SP[s1].fShoot = 1;            // actual fraction of dGrowth allocated to shoot

		SWSupply = new float[dlayer.Length];
		SWUptake = new float[dlayer.Length];
		SNSupply = new float[dlayer.Length];
		SNUptake = new float[dlayer.Length];
	}

	/// <summary>
	/// Let species know weather conditions
	/// </summary>
	/// <returns></returns>
	private bool SetSpeciesMetData()
	{
		//pass metData & day_length to species (same to all species)
		Species.dayLength = day_length;
		Species.latitude = latitude;
		Species.MetData = MetData;
		Species.day_of_month = day_of_month;
		Species.month = month;
		Species.year = year;
		Species.CO2 = co2;
		Species.PIntRadn = IntRadn;
		Species.PCoverGreen = Cover_green;
		Species.PLightExtCoeff = p_lightExtCoeff;
		Species.Pdmshoot = AboveGroundWt;   //dm_shoot;

		//partition the MetData to species
		double sumRadnIntercept = 0.0;   //Intercepted Fraction of the solar Radn available to a species
		for (int s = 0; s < NumSpecies; s++)
		{
			sumRadnIntercept += SP[s].coverGreen;
		}
		//update available Radn for each species at current day
		//IntRadn - the total intecepted radn by whole canopy of mixed species
		for (int s = 0; s < NumSpecies; s++)
		{
			if (sumRadnIntercept == 0)
			{
				SP[s].intRadnFrac = 0;
				SP[s].intRadn = 0;
			}
			else
			{
				SP[s].intRadnFrac = SP[s].coverGreen / sumRadnIntercept;
				SP[s].intRadn = IntRadn * SP[s].intRadnFrac;
			}
		}

		return true;
	}

	/// <summary>
	/// Write initialisation info to summary file
	/// </summary>
	private void writeSummary()
	{
		Console.WriteLine();
		Console.Write(@"
           AgPature Properties
         -----------------------------------------------------------------------------
          Species        TotalWt  ShootWt  RootWt   LAI  TotalC   TotalN   RootDepth
                         (kg/ha)  (kg/ha)  (kg/ha)   () (kg/ha)   (kg/ha)       (mm)
         -----------------------------------------------------------------------------
");
		for (int specie = 0; specie < SP.Length; ++specie)
		{
			Console.WriteLine("          {0,-12}    {1,6:F1}   {2,6:F1}  {3,6:F1}  {4,4:F2}  {5,6:F1}    {6,5:F1}      {7,6:F1}",
			SP[specie].speciesName, 
			SP[specie].dmshoot + SP[specie].dmroot, 
			SP[specie].dmshoot, 
			SP[specie].dmroot, 
			SP[specie].totalLAI, 
			(SP[specie].dmshoot + SP[specie].dmroot) * 0.4, 
			SP[specie].Nshoot + SP[specie].Nroot,
			SP[specie].rootDepth);
		}
		Console.WriteLine("         -----------------------------------------------------------------------------");
		Console.WriteLine("          Totals          {0,6:F1}   {1,6:F1}  {2,6:F1}  {3,4:F2}  {4,6:F1}    {5,5:F1}      {6,6:F1}",
		TotalPlantWt, AboveGroundWt, BelowGroundWt, LAI_total, TotalPlantC, TotalPlantN,p_rootFrontier);
		Console.WriteLine("         -----------------------------------------------------------------------------");

		Console.WriteLine();
		Console.WriteLine("          - N uptake controlled by AgPasture");
		Console.WriteLine("          - Water uptake controlled by " + ((WaterUptakeSource == "calc") ? "AgPasture" : "an external module"));
		Console.WriteLine();

		Console.Write(@"
          Root distribution
         -----------------------------
          Layer     Depth  FractionWt
         -----------------------------
");
		double LayerTop = 0;
		for (int layer = 0; layer < dlayer.Length; layer++)
		{
			Console.WriteLine("          {0,3}  {1,10}     {2,6:F3}", layer, LayerTop.ToString() + "-" + (LayerTop + dlayer[layer]).ToString(), RootFraction[layer]);
			LayerTop += dlayer[layer];
		}
		Console.WriteLine("         -----------------------------");
	}

	/// <summary>
	/// Set drought stress factor to each species
	/// Worth more efforts in this area
	/// </summary>
	private void SetSpeciesLimitingFactors()
	{

		if (p_waterDemand == 0)
		{
			p_gfwater = 1.0;
			for (int s = 0; s < NumSpecies; s++)
				SP[s].gfwater = p_gfwater;
			return;                                 //case (1) return
		}
		if (p_waterDemand > 0 && p_waterUptake == 0)
		{
			p_gfwater = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				SP[s].gfwater = p_gfwater;
			return;                                 //case (2) return
		}

		p_gfwater = p_waterUptake / p_waterDemand;
		float spDepth = 0;              // soil profile depth
		if (p_gfwater > 0.999)  //possible saturation
		{
			// calculate soil moisture content in root zone
			double SW = 0;      //soil water content
			double Sat = 0;     //water content at saturation
			double FC = 0;      //water contenct at field capacity

			for (int layer = 0; layer < dlayer.Length; layer++)
			{
				spDepth += dlayer[layer];
				if (spDepth <= p_rootFrontier)
				{
					SW += sw_dep[layer];
					Sat += SAT[layer] * dlayer[layer];
					FC += DUL[layer] * dlayer[layer];
				}
			}
			if (SW > FC) //if saturated
			{
				//    double soilSatFactor = 0.2;         //gfwater = 1-0.2 (when SW == Sat)
				//    p_gfwater = 1 - soilSatFactor * (SW - FC) / (Sat - FC);
				//    if (p_gfwater > 1.0) p_gfwater = 1.0;
				//}
				//    for (int s=0; s<Nsp; s++)
				//        SP[s].gfwater = p_gfwater;

				double accum_gfwater = 0;
				p_greenLAI = 0;     //update p_greenLAI before using it.
				for (int s = 0; s < NumSpecies; s++)
				{
					SP[s].gfwater = 1 - SP[s].soilSatFactor * (SW - FC) / (Sat - FC);
					accum_gfwater += SP[s].gfwater * SP[s].greenLAI;   //weighted by greenLAI
					p_greenLAI += SP[s].greenLAI;                      //FLi 19 Sept 2011 for avoiding error of an unupdated
				}                                                      //p_greenLAI when using SWIM for waterUptake
				if (p_greenLAI > 0)
					p_gfwater = accum_gfwater / p_greenLAI;
				else
					p_gfwater = 1.0;
				return;                         //case (3) return
			}
			//Reaching here is possible (SW < FC) even with a p_gfwater ==1     //FLi 20 Oct 2012
			//not return, but go though to the case (4) below
		}

		//Original block Set specieS.gfwater = p_gfwater, to distinguish them later
		for (int s = 0; s < NumSpecies; s++)
		{
			SP[s].gfwater = p_gfwater;
		}
		//Console.Out.WriteLine("gfwater4: " + p_gfwater);
		return;                                     //case (4) return


		/*/////
		int  dep = SWUptake.Length;
		double[] transpLAI = new double[dep];       //partitioning soil water in each layer according to species demand (LAI)
		//This process sounds more reasonable, but not giving better results, possible
		//due to that compensation of moisture among layers are important between species
		spDepth = 0;                         //Soil profile depth
		for (int layer = 0; layer < dep; layer++)
		{
		spDepth += dlayer[layer];
		for (int s = 0; s < Nsp; s++)
		{
		if (spDepth <= SP[s].rootDepth)
		transpLAI[layer] += SP[s].greenLAI;     //totalLAI which has root ToString this layer
		}
		}

		//species soil water demand
		for (int s = 0; s < Nsp; s++)
		{
		SP[s].swuptake = 0; //init
		SP[s].swdemandFrac = 0;
		if (p_greenLAI > 0)
		SP[s].swdemandFrac =  SP[s].greenLAI / p_greenLAI;
		}

		//soil water uptake (assumed)
		spDepth = 0;
		for (int layer = 0; layer < dep; layer++)
		{
		spDepth += dlayer[layer];
		for (int s = 0; s < Nsp; s++)
		{
		if (SP[s].rootDepth >= spDepth && SP[s].greenLAI > 0 && transpLAI[layer] > 0)
		SP[s].swuptake += SWUptake[layer] * SP[s].greenLAI /transpLAI[layer];
		}
		}

		for (int s = 0; s < Nsp; s++)
		{
		if (SP[s].swdemandFrac > 0 && p_waterUptake > 0)
		SP[s].gfwater = SP[s].swuptake / (SP[s].swdemandFrac * p_waterUptake);

		if (SP[s].gfwater > 1.0)
		SP[s].gfwater = 1.0;
		}

		return;                                         //case (4) return
		*/

	}

	/// <summary>
	/// plant growth and partitioning and tissue turnover
	/// </summary>
	private void GrowthAndPartition()
	{
		p_greenLAI = 0;
		p_deadLAI = 0;

		p_greenDM = 0.0;
		p_deadDM = 0.0;

		p_dHerbage = 0.0;
		p_rootMass = 0.0;

		p_dLitter = 0;
		p_dNLitter = 0;

		p_dRootSen = 0;
		p_dNRootSen = 0;

		for (int s = 0; s < NumSpecies; s++)
		{
			SP[s].PartitionTurnover();

			p_greenLAI += SP[s].greenLAI;
			p_deadLAI += SP[s].deadLAI;

			p_greenDM += SP[s].dmgreen;
			p_deadDM += SP[s].dmdead;
			p_rootMass += SP[s].dmroot;

			// RCichota may2014: change dmtotal by dmshoot (more clear)
			p_dHerbage += (SP[s].dmshoot - SP[s].pS.dmshoot);
			//p_dHerbage += (SP[s].dmtotal - SP[s].pS.dmtotal);
			//p_dHerbage += SP[s].dGrowth - SP[s].dLitter;

			p_dLitter += SP[s].dLitter;
			p_dNLitter += SP[s].dNLitter;

			p_dRootSen += SP[s].dRootSen;
			p_dNRootSen += SP[s].dNrootSen;
		}

		p_totalLAI = p_greenLAI + p_deadLAI;
		p_totalDM = p_greenDM + p_deadDM;



		//litter return to surface OM completely (frac = 1.0)
		DoSurfaceOMReturn(p_dLitter, p_dNLitter, 1.0);

		//Root FOM return
		DoIncorpFomEvent(p_dRootSen, p_dNRootSen);

		// RCichota May2014: zero out the stored pS.dmdefoliated (it has been used today)
		for (int s = 0; s < NumSpecies; s++)
			SP[s].pS.dmdefoliated = 0.0;

	}


	# region "EventSenders"

	//--------------------------------------------------------------------------------------------
	/// <summary>
	/// Event publication - new crop
	/// </summary>
	private void DoNewCropEvent()
	{
		// Send out New Crop Event to tell other modules who I am and what I am
		NewCropType EventData = new NewCropType();
		EventData.crop_type = micrometType[0];  // need to separate crop type for micromet & canopy name !!
		EventData.sender = thisCropName;        //
		NewCrop.Invoke(EventData);

	}

	//----------------------------------------------------------------
	/// <summary>
	/// Event publication - new canopy
	/// </summary>
	private void DoNewCanopyEvent()
	{
		NewCanopyType canopy = new NewCanopyType();
		canopy.sender = thisCropName;
		canopy.lai = (float)p_greenLAI;
		canopy.lai_tot = (float)p_totalLAI;
		p_height = HeightfromDM;
		canopy.height = (int)p_height;             // height effect, mm
		canopy.depth = (int)p_height;              // canopy depth
		canopy.cover = (float)Cover_green;
		canopy.cover_tot = (float)Cover_tot;

		New_Canopy.Invoke(canopy);
	}

	//----------------------------------------------------------------
	/// <summary>
	/// Send out plant growth limiting factor for other module calculating potential transp.
	/// </summary>
	private void DoNewPotentialGrowthEvent()
	{
		NewPotentialGrowthType EventData = new NewPotentialGrowthType();
		EventData.sender = thisCropName;
		p_gftemp = 0;     //weighted average


		double Tday = 0.75 * MetData.maxt + 0.25 * MetData.mint; //Tday
		for (int s = 0; s < NumSpecies; s++)
		{
			double prop = 1.0 / NumSpecies;
			if (p_greenDM != 0.0)
			{
				prop = SP[s].dmgreen / AboveGroundLiveWt;   // dm_green;
			}
			p_gftemp += SP[s].GFTemperature(Tday) * prop;
		}

		double gft = 1;
		if (Tday < 20) gft = Math.Sqrt(p_gftemp);
		else gft = p_gftemp;
		// Note: p_gftemp is for gross photosysthsis.
		// This is different from that for net production as used in other APSIM crop models, and is
		// assumesd in calculation of temperature effect on transpiration (in micromet).
		// Here we passed it as sqrt - (Doing so by a comparison of p_gftemp and that
		// used in wheat). Temperature effects on NET produciton of forage species in other models
		// (e.g., grassgro) are not so significant for T = 10-20 degrees(C)

		//Also, have tested the consequences of passing p_Ncfactor in (different concept for gfwater),
		//coulnd't see any differnece for results
		EventData.frgr = (float)Math.Min(FVPD, gft);
		// RCichota, Jan/2014: removed AgPasture's Frgr from here, it is considered at the same level as nitrogen etc...
		NewPotentialGrowth.Invoke(EventData);
	}

	#endregion //EventSender

	#region "EventHandlers"
	/// <summary>
	/// Eventhandler - initialisation
	/// </summary>
	[EventHandler]
    public void OnInit2()
	{
		InitParameters();            // Init parameters after reading the data

		SetSpeciesMetData();         // This is needed for the first day after knowing the number of species

		DoNewCropEvent();            // Tell other modules that I exist
		DoNewCanopyEvent();          // Tell other modules about my canopy
		DoNewPotentialGrowthEvent(); // Tell other modules about my current growth status

		// write some basic initialisation info
		writeSummary();

	}

	/// <summary>
	/// Get new meteo- data
	/// </summary>
	/// <param name="NewMetData"></param>
	[EventHandler]
	public void OnNewMet(NewMetType NewMetData)
	{
		MetData = NewMetData;
	}

	/// <summary>
	/// EventHandeler - preparation befor the main process
	/// </summary>
	[EventHandler]
	public void OnPrepare()
	{
		//  p_harvestDM = 0.0;      // impartant to have this reset because
		//  p_harvestN = 0.0;       // they are used to DM & N returns
		//  p_harvestDigest = 0.0;

		// RCichota May2014, moved here from onProcess (really owe to be onNewMet but have issues at initialisation)
		//**Zero out some variables
		for (int s = 0; s < NumSpecies; s++)
			SP[s].DailyRefresh();

		// clear FractionHarvest by assigning new
		FractionToHarvest = new double[NumSpecies];


		DoNewCanopyEvent();
		DoNewPotentialGrowthEvent();

	}

	/// <summary>
	/// Get plant potential transpiration
	/// </summary>
	/// <param name="CWB"></param>
	[EventHandler]
	public void OnCanopy_Water_Balance(CanopyWaterBalanceType CWB)
	{
		for (int i = 0; i < CWB.Canopy.Length; i++)
		{
			if (CWB.Canopy[i].name.ToUpper() == thisCropName.ToUpper())
			{
				p_waterDemand = (double)CWB.Canopy[i].PotentialEp;
			}
		}
	}

	[EventHandler]
	public void OnCanopy_Energy_Balance(CanopyEnergyBalanceType LP)
	{
		canopiesNum = LP.Interception.Length;
		canopiesRadn = new double[canopiesNum];

		for (int i = 0; i < canopiesNum; i++)
		{
			if (LP.Interception[i].name.ToUpper() == thisCropName.ToUpper())  //TO: species by species, and get the total?
			{
				IntRadn = 0;
				for (int j = 0; j < LP.Interception[i].layer.Length; j++)
				{
					IntRadn += LP.Interception[i].layer[j].amount;
				}
				canopiesRadn[i] = IntRadn;
			}
			else //Radn intercepted possibly by other canopies used for a rough IL estimation,
			{    //potenital use when species were specified separately in pasture. (not used of now.11Mar10 )
				double otherRadn = 0;
				for (int j = 0; j < LP.Interception[i].layer.Length; j++)
				{
					otherRadn += LP.Interception[i].layer[j].amount;
				}
				canopiesRadn[i] = otherRadn;
			}
		}
	}

	[EventHandler]
	public void OnProcess()
	{
		if (!p_Live)
			return;

		//**Remember last status, and update root depth frontier (root depth mainly for annuals)
		for (int s = 0; s < NumSpecies; s++)
		{
			SP[s].SetPrevPools(); //pool values yesterday is also retained in current state
			//SP[s].DailyRefresh();

			double spRootDepth = SP[s].rootGrowth();    //update root depth
			if (p_rootFrontier < spRootDepth)
				p_rootFrontier = spRootDepth;			// the deepest root_depth is used

			//RCichota May2014: The code commented out above was moved to onPrepare 
		}

		//Console.WriteLine("Warning message");
		//throw new Exception("throw ...");

		//**To partition Radn to different species
		SetSpeciesMetData();

		//** advance phenology
		int anyEmerged = 0;
		for (int s = 0; s < NumSpecies; s++)
		{
			anyEmerged += SP[s].Phenology();
		}

		//**Potential growth
		p_dGrowthPot = 0;
		for (int s = 0; s < NumSpecies; s++)
		{
			//p_dGrowthPot += SP[s].DailyGrowthPot();   // alternative way for calclating potential growth
			p_dGrowthPot += SP[s].DailyGrowthPot();   //pot here incorporated [N] effects
		}


		//**Calculate soil N available in root zone
		p_soilNavailable = calcPlantAvailableN();
		//p_soilNavailable = calcPlantExtractableN();   //need to do more validation/calibration for activating this
		//**Water supply & uptake
		if (WaterUptakeSource == "calc")
		{
			p_waterUptake = SWUptakeProcess();      //actual uptake by all species
		}
		else
		{
			//Water uptake be calculated by other modules (e.g., SWIM) and got by [EventHandler]
		}
		SetSpeciesLimitingFactors();  // * root competition for water when SM is deficit: species-specific ?

		//**add drought effects (before considering other nutrient limitation)
		p_dGrowthW = 0;
		for (int s = 0; s < NumSpecies; s++)
		{
			p_dGrowthW += SP[s].DailyGrowthW();
		}
		double nuptake = NBudgetAndUptake();

		//**actual daily growth
		p_dGrowth = 0;
		for (int s = 0; s < NumSpecies; s++)
		{
			p_dGrowth += SP[s].DailyGrowthAct();
		}

		/*trick species for specified clover%
		DateTime d97J1 = new DateTime(1997, 7, 1, 0, 0, 0);
		DateTime d98J1 = new DateTime(1998, 7, 1, 0, 0, 0);
		DateTime d99J1 = new DateTime(1999, 7, 1, 0, 0, 0);
		DateTime d00J1 = new DateTime(2000, 7, 1, 0, 0, 0);
		DateTime d01J1 = new DateTime(2001, 7, 1, 0, 0, 0);
		DateTime d02J1 = new DateTime(2002, 7, 1, 0, 0, 0);
		DateTime d03J1 = new DateTime(2003, 7, 1, 0, 0, 0);
		DateTime d04J1 = new DateTime(2004, 7, 1, 0, 0, 0);
		DateTime d05J1 = new DateTime(2005, 7, 1, 0, 0, 0);
		DateTime d06J1 = new DateTime(2006, 7, 1, 0, 0, 0);
		DateTime d07J1 = new DateTime(2007, 7, 1, 0, 0, 0);
		DateTime d08J1 = new DateTime(2008, 7, 1, 0, 0, 0);
		double legumeF = 0.10;                                                                      //ElevObs  //AmbObs
		if (DateTime.Compare(Today, d97J1) >= 0 && DateTime.Compare(Today, d98J1) < 0) legumeF = 0.03;//0.05;//0.03;
		else if (DateTime.Compare(Today, d98J1) >= 0 && DateTime.Compare(Today, d99J1) < 0) legumeF = 0.06;//0.19;//0.06;
		else if (DateTime.Compare(Today, d99J1) >= 0 && DateTime.Compare(Today, d00J1) < 0) legumeF = 0.17;//0.31;//0.17;
		else if (DateTime.Compare(Today, d00J1) >= 0 && DateTime.Compare(Today, d01J1) < 0) legumeF = 0.21;//0.34;//0.21;
		else if (DateTime.Compare(Today, d01J1) >= 0 && DateTime.Compare(Today, d02J1) < 0) legumeF = 0.03;//0.04;//0.03;
		else if (DateTime.Compare(Today, d02J1) >= 0 && DateTime.Compare(Today, d03J1) < 0) legumeF = 0.03;//0.07;//0.03;
		else if (DateTime.Compare(Today, d03J1) >= 0 && DateTime.Compare(Today, d04J1) < 0) legumeF = 0.09;//0.06;//0.09;
		else if (DateTime.Compare(Today, d04J1) >= 0 && DateTime.Compare(Today, d05J1) < 0) legumeF = 0.10;//0.22;//0.10;
		else if (DateTime.Compare(Today, d05J1) >= 0 && DateTime.Compare(Today, d06J1) < 0) legumeF = 0.11;//0.07;//0.11;
		else if (DateTime.Compare(Today, d06J1) >= 0 && DateTime.Compare(Today, d07J1) < 0) legumeF = 0.02;//0.05;//0.02;
		else if (DateTime.Compare(Today, d07J1) >= 0 && DateTime.Compare(Today, d08J1) < 0) legumeF = 0.05;//0.06;//0.05;

		SP[0].dGrowth = p_dGrowth * (1 - legumeF);
		SP[1].dGrowth = p_dGrowth * legumeF;
		Console.WriteLine(" legumeF = " + legumeF);
		//end of trick#
		*/


		//**partitioning & turnover
		GrowthAndPartition();       // litter returns to surfaceOM; Root returns to soil FOM dead in this turnover routines

		/* if (!p_HarvestDay)
		{
		p_harvestDM = 0.0;      // impartant to have this reset because
		p_harvestN = 0.0;       // they are used to DM & N returns
		p_harvestDigest = 0.0;
		}
		p_HarvestDay = false;    //reset the
		*/
	}

	[EventHandler]
	public void Onremove_crop_biomass(RemoveCropBiomassType rm)
	{
		//Note: It is resposibility of the calling module to check the
		// amount of herbage in each pools of AbovegroundBiomassWt and set the
		// the correct amount in 'rm'.
		// No checking if the removing amount passed in are too much here

		const double gm2ha = 10;   // constant for conversion of g/m^2 to kg/ha,
		// rm.dm.dlt should be in g/m^2

		double dm_leaf_green = LeafLiveWt;
		double dm_stem_green = StemLiveWt;
		double dm_leaf_dead = LeafDeadWt;
		double dm_stem_dead = StemDeadWt;

		for (int s = 0; s < NumSpecies; s++)     // for accumulating the total DM & N removal of species from verious pools
		{
			SP[s].dmdefoliated = 0.0;
			SP[s].Ndefoliated = 0.0;
		}

		for (int i = 0; i < rm.dm.Length; i++)              //for each pool
		{
			for (int j = 0; j < rm.dm[i].dlt.Length; j++)   //for each part
			{
				if (rm.dm[i].pool == "green" && rm.dm[i].part[j] == "leaf")
				{
					for (int s = 0; s < NumSpecies; s++)           //for each species
					{
						if (dm_leaf_green != 0)             //resposibility of other modules to check the amount
						{
							double rm_leaf = gm2ha * rm.dm[i].dlt[j] * SP[s].dmleaf_green / dm_leaf_green;
							double rm_leaf1 = rm_leaf * SP[s].dmleaf1 / SP[s].dmleaf_green;
							double rm_leaf2 = rm_leaf * SP[s].dmleaf2 / SP[s].dmleaf_green;
							double rm_leaf3 = rm_leaf * SP[s].dmleaf3 / SP[s].dmleaf_green;
							SP[s].dmleaf1 -= rm_leaf1;
							SP[s].dmleaf2 -= rm_leaf2;
							SP[s].dmleaf3 -= rm_leaf3;
							SP[s].dmdefoliated += rm_leaf1 + rm_leaf2 + rm_leaf3;

							SP[s].Nleaf1 -= SP[s].Ncleaf1 * rm_leaf1;
							SP[s].Nleaf2 -= SP[s].Ncleaf2 * rm_leaf2;
							SP[s].Nleaf3 -= SP[s].Ncleaf3 * rm_leaf3;
							SP[s].Ndefoliated += SP[s].Ncleaf1 * rm_leaf1 + SP[s].Ncleaf2 * rm_leaf2 + SP[s].Ncleaf3 * rm_leaf3;
						}
					}
				}
				else if (rm.dm[i].pool == "green" && rm.dm[i].part[j] == "stem")
				{
					for (int s = 0; s < NumSpecies; s++)
					{
						if (dm_stem_green != 0)  //resposibility of other modules to check the amount
						{
							double rm_stem = gm2ha * rm.dm[i].dlt[j] * SP[s].dmstem_green / dm_stem_green;
							double rm_stem1 = rm_stem * SP[s].dmstem1 / SP[s].dmstem_green;
							double rm_stem2 = rm_stem * SP[s].dmstem2 / SP[s].dmstem_green;
							double rm_stem3 = rm_stem * SP[s].dmstem3 / SP[s].dmstem_green;
							SP[s].dmstem1 -= rm_stem1;
							SP[s].dmstem2 -= rm_stem2;
							SP[s].dmstem3 -= rm_stem3;
							SP[s].dmdefoliated += rm_stem1 + rm_stem2 + rm_stem3;

							SP[s].Nstem1 -= SP[s].Ncstem1 * rm_stem1;
							SP[s].Nstem2 -= SP[s].Ncstem2 * rm_stem2;
							SP[s].Nstem3 -= SP[s].Ncstem3 * rm_stem3;
							SP[s].Ndefoliated += SP[s].Ncstem1 * rm_stem1 + SP[s].Ncstem2 * rm_stem2 + SP[s].Ncstem3 * rm_stem3;
						}
					}
				}
				else if (rm.dm[i].pool == "dead" && rm.dm[i].part[j] == "leaf")
				{
					for (int s = 0; s < NumSpecies; s++)
					{
						if (dm_leaf_dead != 0)  //resposibility of other modules to check the amount
						{
							double rm_leaf4 = gm2ha * rm.dm[i].dlt[j] * SP[s].dmleaf4 / dm_leaf_dead;
							SP[s].dmleaf4 -= rm_leaf4;
							SP[s].dmdefoliated += rm_leaf4;

							SP[s].Ndefoliated += SP[s].Ncleaf4 * rm_leaf4;
							SP[s].Nleaf4 -= SP[s].Ncleaf4 * rm_leaf4;
						}
					}
				}
				else if (rm.dm[i].pool == "dead" && rm.dm[i].part[j] == "stem")
				{
					for (int s = 0; s < NumSpecies; s++)
					{
						if (dm_stem_dead != 0)  //resposibility of other modules to check the amount
						{
							double rm_stem4 = gm2ha * rm.dm[i].dlt[j] * SP[s].dmstem4 / dm_stem_dead;
							SP[s].dmstem4 -= rm_stem4;
							SP[s].dmdefoliated += rm_stem4;

							SP[s].Nstem4 -= SP[s].Ncstem4 * rm_stem4;
							SP[s].Ndefoliated += SP[s].Ncstem4 * rm_stem4;
						}
					}
				}
			}
		}

		p_harvestDM = 0;
		p_harvestN = 0;
		for (int s = 0; s < NumSpecies; s++)
		{
			p_harvestDM += SP[s].dmdefoliated;
			p_harvestN += SP[s].Ndefoliated;
			SP[s].updateAggregated();

			// RCichota May 2014: store the defoliated amount (to use for senescence)
			SP[s].pS.dmdefoliated = SP[s].dmdefoliated;
		}

		//In this routine of no selection among species, the removed tissue from different species
		//will be in proportion with exisisting mass of each species.
		//The digetibility below is an approximation (= that of pasture swards).
		//It is more reasonable to calculate it organ-by-organ for each species, then put them together.
		p_harvestDigest = HerbageDigestibility;

	}

	public void Harvest(string type, double amount)
	{
		GrazeType GZ = new GrazeType();
		GZ.amount = (float)amount;
		GZ.type = type;
		OnGraze(GZ);
	}

	[EventHandler]
	public void OnGraze(GrazeType GZ)
	{
		if ((!p_Live) || p_totalDM == 0)
			return;

		// get the amount that can potentially be removed
		double AmountRemovable = 0.0;
		for (int s = 0; s < NumSpecies; s++)
			AmountRemovable += Math.Max(0.0, SP[s].dmleaf_green + SP[s].dmstem_green - SP[s].dmgreenmin) + Math.Max(0.0, SP[s].dmdead - SP[s].dmdeadmin);
		AmountRemovable = Math.Max(0.0, AmountRemovable);

		// get the amount required to remove
		double AmountRequired = 0.0;
		if (GZ.type.ToLower() == "SetResidueAmount".ToLower())
		{
			// Remove all DM above given residual amount
			AmountRequired = Math.Max(0.0, StandingPlantWt - GZ.amount);
		}
		else if (GZ.type.ToLower() == "SetRemoveAmount".ToLower())
		{
			// Attempt to remove a given amount
			AmountRequired = Math.Max(0.0, GZ.amount);
		}
		else
		{
			Console.WriteLine("  AgPasture - Method to set amount to remove was not recognized, command will be ignored");
		}
		// get the actual amount to remove
		double AmountToRemove = Math.Min(AmountRequired, AmountRemovable);

		p_harvestDM = AmountToRemove;
		p_harvestN = 0.0;
		p_harvestDigest = 0.0;

		// get the amounts to remove by species:
		double FractionNotRemoved = 0.0;
		if (AmountRemovable > 0.0)
			FractionNotRemoved = Math.Max(0.0, (AmountRemovable - AmountToRemove) / AmountRemovable);
		double[] TempWeights = new double[NumSpecies];
		double[] TempAmounts = new double[NumSpecies];
		double TempTotal = 0.0;
		if (AmountRequired > 0.0)
		{
			// get the weights for each species, consider preference and available DM
			double TotalPreference =0.0;
			for (int s = 0; s < NumSpecies; s++)
				TotalPreference += PreferenceForGreenDM[s] + PreferenceForDeadDM[s];
			for (int s = 0; s < NumSpecies; s++)
			{
				TempWeights[s] = PreferenceForGreenDM[s] + PreferenceForDeadDM[s];
				TempWeights[s] += (TotalPreference - TempWeights[s]) * (1 - FractionNotRemoved);
				TempAmounts[s] = Math.Max(0.0, SP[s].dmleaf_green + SP[s].dmstem_green - SP[s].dmgreenmin) + Math.Max(0.0, SP[s].dmdead - SP[s].dmdeadmin);
				TempTotal += TempAmounts[s] * TempWeights[s];
			}

			// get the actual amounts to remove for each species
			for (int s = 0; s < NumSpecies; s++)
			{
				if (TempTotal > 0.0)
					FractionToHarvest[s] = Math.Max(0.0, Math.Min(1.0, TempWeights[s] * TempAmounts[s] / TempTotal));
				else
					FractionToHarvest[s] = 0.0;
				p_harvestN += SP[s].RemoveDM(AmountToRemove * FractionToHarvest[s], PreferenceForGreenDM[s], PreferenceForDeadDM[s]);

				// get digestibility of harvested material
				p_harvestDigest += SP[s].digestDefoliated * SP[s].dmdefoliated / AmountToRemove;
			}
		}
	}

	[EventHandler]
	public void OnWaterUptakesCalculated(WaterUptakesCalculatedType SoilWater)
	{
		// Gets the water uptake for each layer as calculated by an external module (SWIM)
		p_waterUptake = 0;
		for (int i_Crop = 0; i_Crop != SoilWater.Uptakes.Length; i_Crop++)
		{
			string MyName = SoilWater.Uptakes[i_Crop].Name;
			if (MyName == thisCropName)
			{
				int length = SoilWater.Uptakes[i_Crop].Amount.Length;
				for (int layer = 0; layer < length; layer++)
				{
					SWUptake[layer] = (float)SoilWater.Uptakes[i_Crop].Amount[layer];
					p_waterUptake += SoilWater.Uptakes[i_Crop].Amount[layer];
				}
			}
		}
	}

	[EventHandler]
	public void OnSow(SowType PSow)
	{
		/*SowType is our type and is defined like this:
		<type name="Sow">
		<field name="Cultivar" kind="string" />
		<field name="Population" kind="double" />
		<field name="Depth" kind="double" />
		<field name="MaxCover" kind="double" />
		<field name="BudNumber" kind="double" />
		</type>
		*/

		p_Live = true;
		ResetZero();
		for (int s = 0; s < NumSpecies; s++)
			SP[s].SetInGermination();

	}

	[EventHandler]
	public void OnKillCrop(KillCropType PKill)
	{
		double frac = PKill.KillFraction;
		//always complete kill for pasture, ignore fraction

		//Above_ground part returns to surface OM comletey (frac = 1.0)
		DoSurfaceOMReturn(p_totalDM, AboveGroundN, 1.0);    //n_shoot

		//Incorporate root mass in soil fresh organic matter
		DoIncorpFomEvent(p_rootMass, BelowGroundN);         //n_root);

		ResetZero();

		p_Live = false;
	}

	private void ResetZero()
	{
		//shoot
		p_greenLAI = 0;
		p_deadLAI = 0;
		p_totalLAI = 0;
		p_greenDM = 0;
		p_deadDM = 0;
		p_totalDM = 0;
		p_height = 0;

		//root
		p_rootMass = 0;
		p_rootFrontier = 0;

		//daily changes
		p_dGrowthPot = p_dGrowthW = p_dGrowth = p_dHerbage = 0;   //daily DM increase
		p_dLitter = p_dNLitter = 0;
		p_dRootSen = p_dNRootSen = 0;

		p_waterDemand = p_waterUptake = 0;
		p_soilNdemand = p_soilNuptake = 0;

		//species (ignore fraction)
		for (int s = 0; s < NumSpecies; s++)
			SP[s].ResetZero();

	}

    private double calcPlantAvailableN()
	{
		p_soilNavailable = 0;
		double spDepth = 0;         // depth before next soil layer
		int sLayer = 0;
		for (sLayer = 0; sLayer < dlayer.Length; sLayer++)
		{
			if (spDepth <= p_rootFrontier)
			{
				/* an approach for controlling N uptake
				const float KNO3 = 0.1F;
				const float KNH4 = 0.1F;
				double swaf = 1.0;
				swaf = (sw_dep[sLayer] - ll[sLayer]) / (DUL[sLayer] - ll[sLayer]);
				swaf = Math.Max(0.0, Math.Min(swaf, 1.0));
				p_soilNavailable += (no3[sLayer] * KNO3 + nh4[sLayer] * KNH4 ) * swaf;
				SNSupply[sLayer] = (no3[sLayer] * KNO3 + nh4[sLayer] * KNH4 ) * (float)swaf;
				*/
				//original below
				p_soilNavailable += (no3[sLayer] + nh4[sLayer]);
				SNSupply[sLayer] = (no3[sLayer] + nh4[sLayer]);
			}
			else
			{
				p_bottomRootLayer = sLayer;
				break;
			}

			spDepth += (double)dlayer[sLayer];

		}

		if (p_bottomRootLayer == 0 && sLayer > 0)
			p_bottomRootLayer = sLayer - 1;

		return p_soilNavailable;
	}

	private double calcPlantExtractableN()    // not all minN is extractable
	{
		p_soilNavailable = 0;
		double spDepth = 0;         // depth before next soil layer
		int sLayer = 0;
		for (sLayer = 0; sLayer < dlayer.Length; sLayer++)
		{
			if (spDepth <= p_rootFrontier)
			{
				//an approach for controlling N uptake
				const float KNO3 = 0.1F;
				const float KNH4 = 0.1F;
				double swaf = 1.0;
				swaf = (sw_dep[sLayer] - ll[sLayer]) / (DUL[sLayer] - ll[sLayer]);
				swaf = Math.Max(0.0, Math.Min(swaf, 1.0));
				p_soilNavailable += (no3[sLayer] * KNO3 + nh4[sLayer] * KNH4) * Math.Pow(swaf, 0.25);
				SNSupply[sLayer] = (no3[sLayer] * KNO3 + nh4[sLayer] * KNH4) * (float)Math.Pow(swaf, 0.25);

				//original below
				//p_soilNavailable += (no3[sLayer] + nh4[sLayer]);
				//SNSupply[sLayer] = (no3[sLayer] + nh4[sLayer]);
			}
			else
			{
				p_bottomRootLayer = sLayer;
				break;
			}

			spDepth += (double)dlayer[sLayer];

		}

		if (p_bottomRootLayer == 0 && sLayer > 0)
			p_bottomRootLayer = sLayer - 1;

		return p_soilNavailable;
	}

    // RCichota, Jun 2014: cleaned up and add consideration for remobilisation of luxury N
	private double NBudgetAndUptake()
	{
		//1) Get the total N demand (species by species)
		p_Nfix = 0.0;
		double p_Ndemand = 0.0;
		double p_NdemandOpt = 0.0;
		for (int s = 0; s < NumSpecies; s++)
		{
			p_Nfix += SP[s].CalcNdemand();       //minimum N fixation
			p_NdemandOpt += SP[s].NdemandOpt;    //demand for optimum [N]
			p_Ndemand += SP[s].NdemandLux;       //demand for luxury [N]
		}

		//2) Update Nfix of legume species under N stress
		double Nstress = 1.0;
		if (p_Ndemand > 0.0 && (p_Ndemand > p_soilNavailable + p_Nfix))
			Nstress = p_soilNavailable / (p_Ndemand - p_Nfix);

		for (int s = 0; s < NumSpecies; s++)
		{
			if ((SP[s].isLegume) && (Nstress < 0.99))  //more fixation under Nstress
			{
				double newNfix = (SP[s].MaxFix - (SP[s].MaxFix - SP[s].MinFix) * Nstress) * SP[s].NdemandLux;
				double moreNfix = Math.Max(0.0, (newNfix - SP[s].Nfix));
				SP[s].Nfix = newNfix;
				p_Nfix += moreNfix;
			}
		}

		//3) Get N remobilised and calculate N demand from soil
		p_soilNdemand = 0.0;
		for (int s = 0; s < NumSpecies; s++)
		{
			if (SP[s].NdemandLux <= SP[s].Nremob + SP[s].Nfix)
			{
				// Nremob and Nfix are able to supply all N - note: Nfix = 0 for non-legumes
				SP[s].remob2NewGrowth = Math.Max(0.0, SP[s].NdemandLux - SP[s].Nfix);
				SP[s].Nremob -= SP[s].remob2NewGrowth;
				SP[s].soilNdemand = 0.0;
			}
			else
			{
				// not enough N within the plant, uptake is needed
				SP[s].remob2NewGrowth = SP[s].Nremob;
				SP[s].Nremob = 0.0;
				SP[s].soilNdemand = SP[s].NdemandLux - (SP[s].Nfix + SP[s].remob2NewGrowth);
			}
			SP[s].newGrowthN = SP[s].remob2NewGrowth + SP[s].Nfix;
			p_soilNdemand += SP[s].soilNdemand;
		}

		//4) Compute soil N uptake, newGrowthN and N limitation factor
		p_soilNuptake = 0.0;
		p_gfn = 0.0;
		for (int s = 0; s < NumSpecies; s++)
		{
			if (SP[s].soilNdemand == 0.0)
			{
				SP[s].soilNuptake = 0.0;
				SP[s].NFastRemob3 = 0.0;
				SP[s].NFastRemob2 = 0.0;
				SP[s].gfn = 1.0;
			}
			else
			{
				if (p_soilNavailable >= p_soilNdemand)
				{
					// soil can supply all remaining N needed
					SP[s].soilNuptake = SP[s].soilNdemand;
					SP[s].NFastRemob3 = 0.0;
					SP[s].NFastRemob2 = 0.0;
					SP[s].newGrowthN += SP[s].soilNuptake;
					SP[s].gfn = 1.0;
				}
				else
				{
					// soil cannot supply all N needed. Get the available N and partition between species
					SP[s].soilNuptake = p_soilNavailable * SP[s].soilNdemand / p_soilNdemand;
					SP[s].newGrowthN += SP[s].soilNuptake;

					// check whether demand for optimum growth is satisfied
					if (SP[s].NdemandOpt > SP[s].newGrowthN)
					{
						// plant still needs more N for optimum growth (luxury uptake is ignored), check whether luxury N in plants can be used
						double Nmissing = SP[s].NdemandOpt - SP[s].newGrowthN;
						if (Nmissing <= SP[s].NLuxury2 + SP[s].NLuxury3)
						{
							// There is luxury N that can be used for optimum growth, first from tissue 3
							if (Nmissing <= SP[s].NLuxury3)
							{
								SP[s].NFastRemob3 = Nmissing;
								SP[s].NFastRemob2 = 0.0;
								Nmissing = 0.0;
							}
							else
							{
								SP[s].NFastRemob3 = SP[s].NLuxury3;
								Nmissing -= SP[s].NLuxury3;

								// remaining from tissue 2
								SP[s].NFastRemob2 = Nmissing;
								Nmissing = 0.0;
							}
						}
						else
						{
							// N luxury is not enough for optimum growth, use up all there is
							if (SP[s].NLuxury2 + SP[s].NLuxury3 > 0)
							{
								SP[s].NFastRemob3 = SP[s].NLuxury3;
								SP[s].NFastRemob2 = SP[s].NLuxury2;
								Nmissing -= (SP[s].NLuxury3 + SP[s].NLuxury2);
							}
						}
						SP[s].newGrowthN += SP[s].NFastRemob3 + SP[s].NFastRemob2;
					}
					else
					{
						// N supply is enough for optimum growth, although luxury uptake is not fully accomplished
						SP[s].NFastRemob3 = 0.0;
						SP[s].NFastRemob2 = 0.0;
					}
					SP[s].gfn = Math.Min(1.0, Math.Max(0.0, SP[s].newGrowthN / SP[s].NdemandOpt));
				}
			}
			p_soilNuptake += SP[s].soilNuptake;

			//weighted average of species gfn
			if (p_dGrowthW == 0)
			{ 
				p_gfn = 1;
			}
			else
			{
				p_gfn += SP[s].gfn * SP[s].dGrowthW / p_dGrowthW;
			}
		}

		//5) Actual uptake, remove N from soil
		double soilNremoved = 0;
		if (NUptakeSource == "calc")
		{
			soilNremoved = SNUptakeProcess();               //N remove from soil
		}
		else
		{
			// N uptake calculated by other modules (e.g., SWIM)
			string msg = "Only one option for N uptake is implemented in AgPasture. Please specify N uptake source as default \"calc\".";
			throw new Exception(msg);
		}

		return soilNremoved;

	}

	#endregion Eventhandlers

	#region Output properties

	[Output]
	[Description("Intercepted solar radiation")]
	[Units("MJ")]
	private float IntRadn;

	[Output]
	[Description("Generic type of crop")]         //  useful for SWIM
	[Units("")]
	public string Crop_type
	{
        get { return thisCropName; }  // micrometType[0]
	}

    [Output]
    [Description("Name of this crop")]
    [Units("")]
    public string Crop_name
    {
        get { return thisCropName; }
    }

    [Output]
    [Description("Generic crop type of each species")]
    [Units("")]
    public string[] SpeciesCrop_type
    {
        get { return micrometType; }
    }

    [Output]
    [Description("Name of each species")]
    [Units("")]
    public string[] Species_name
    {
        get
        {
            string[] result = new string[NumSpecies];
            for (int s = 0; s < NumSpecies; s++)
                result[s] = SP[s].speciesName;
            return result;
        }
    }

	[Output]
	[Description("Plant status (dead, alive, etc)")]
	[Units("")]
	public string plant_status
	{
		get
		{
			if (p_Live) return "alive";
			else return "out";
		}
	}
	[Output]
	[Description("Plant development stage number")]
	[Units("")]
	public int Stage
	{
		//An approximate of teh stages corresponding to that of other arable crops for management application settings.
		//Phenostage of the first species (ryegrass) is used for this approximation
		get
		{
			int cropStage = 0; //default as "phase out"
			if (p_Live)
			{
				if (SP[0].phenoStage == 0)
					cropStage = 1;    //"sowing & germination";
				if (SP[0].phenoStage == 1)
					cropStage = 3;    //"emergence";
			}
			return cropStage;
		}
	}
	[Output]
	[Description("Plant development stage name")]
	[Units("")]
	public string StageName
	{
		get
		{
			string name = "out";
			if (p_Live)
			{
				if (SP[0].phenoStage == 0)
					name = "sowing";    //cropStage = 1 & 2
				if (SP[0].phenoStage == 1)
					name = "emergence"; // cropStage = 3
			}
			return name;
		}
	}

	[Output]
	[Description("Total amount of C in plants")]
	[Units("kgDM/ha")]
	public double TotalPlantC
	{
		get { return 0.4 * (p_totalDM + p_rootMass); }
	}

	[Output]
	[Description("Total dry matter weight of plants")]
	[Units("kgDM/ha")]
	public double TotalPlantWt
	{
		get { return (AboveGroundWt + BelowGroundWt); }
	}

	[Output]
	[Description("Total dry matter weight of plants above ground")]
	[Units("kgDM/ha")]
	public double AboveGroundWt
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += SP[s].dmshoot;
			return result;
		}
	}
	[Output]
	[Description("Total dry matter weight of plants below ground")]
	[Units("kgDM/ha")]
	public double BelowGroundWt
	{
		get { return p_rootMass; }
	}
	[Output]
	[Description("Total dry matter weight of standing plants parts")]
	[Units("kgDM/ha")]
	public double StandingPlantWt
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += SP[s].dmleaf + SP[s].dmstem;
			return result;
		}
	}

	[Output]
	[Description("Total dry matter weight of plants alive above ground")]
	[Units("kgDM/ha")]
	public double AboveGroundLiveWt
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += SP[s].dmgreen;
			return result;
		}
	}

	[Output]
	[Description("Total dry matter weight of dead plants above ground")]
	[Units("kgDM/ha")]
	public double AboveGroundDeadWt
	{
		get { return p_deadDM; }
	}
	[Output]
	[Description("Total dry matter weight of plant's leaves")]
	[Units("kgDM/ha")]
	public double LeafWt
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += SP[s].dmleaf1 + SP[s].dmleaf2 + SP[s].dmleaf3 + SP[s].dmleaf4;
			return result;
		}
	}
	[Output]
	[Description("Total dry matter weight of plant's leaves alive")]
	[Units("kgDM/ha")]
	public double LeafLiveWt
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += SP[s].dmleaf1 + SP[s].dmleaf2 + SP[s].dmleaf3;
			return result;
		}
	}
	[Output]
	[Description("Total dry matter weight of plant's leaves dead")]
	[Units("kgDM/ha")]
	public double LeafDeadWt
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += SP[s].dmleaf4;
			return result;
		}
	}

	[Output]
	[Description("Total dry matter weight of plant's stems")]
	[Units("kgDM/ha")]
	public double StemWt
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += SP[s].dmstem1 + SP[s].dmstem2 + SP[s].dmstem3 + SP[s].dmstem4;
			return result;
		}
	}

	[Output]
	[Description("Total dry matter weight of plant's stems alive")]
	[Units("kgDM/ha")]
	public double StemLiveWt
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += SP[s].dmstem1 + SP[s].dmstem2 + SP[s].dmstem3;
			return result;
		}
	}
	[Output]
	[Description("Total dry matter weight of plant's stems dead")]
	[Units("kgDM/ha")]
	public double StemDeadWt
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += SP[s].dmstem4;
			return result;
		}
	}

	[Output]
	[Description("Total dry matter weight of plant's stolons")]
	[Units("kgDM/ha")]
	public double StolonWt
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += SP[s].dmstol1 + SP[s].dmstol2 + SP[s].dmstol3;
			return result;
		}
	}
	[Output]
	[Description("Total dry matter weight of plant's roots")]
	[Units("kgDM/ha")]
	public double RootWt
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += SP[s].dmroot;
			return result;
		}
	}

	//for consistency, passing variables in Onremove_crop_biomass() similar with other plant modules
	[Output]
	[Description("Total dry matter weight of plants above ground")]
	[Units("kg/ha")]
	public double biomass { get { return AboveGroundWt; } }
	[Output]
	[Description("Total dry matter weight of plant's leaves alive")]
	[Units("g/m^2")]
	public double leafgreenwt { get { return LeafLiveWt / 10; } }
	[Output]
	[Description("Total dry matter weight of plant's leaves dead")]
	[Units("g/m^2")]
	public double stemgreenwt { get { return StemLiveWt / 10; } }
	[Output]
	[Description("Total dry matter weight of plant's stems dead")]
	[Units("g/m^2")]
	public double leafsenescedwt { get { return LeafDeadWt / 10; } }
	[Output]
	[Description("Total dry matter weight of plant's stems alive")]
	[Units("g/m^2")]
	public double stemsenescedwt { get { return StemDeadWt / 10; } }

    [Output]
    [Description("Plant potential carbon assimilation")]
    [Units("kgC/ha")]
    public double PlantPotentialCarbonAssimilation
    {
        get
        {
            double result = 0.0;
            for (int s = 0; s < NumSpecies; s++)
                result += SP[s].Pgross;
            return result;
        }
    }

    [Output]
    [Description("Plant carbon loss by respiration")]
    [Units("kgC/ha")]
    public double PlantCarbonLossRespiration
    {
        get
        {
            double result = 0.0;
            for (int s = 0; s < NumSpecies; s++)
                result += SP[s].Resp_m + SP[s].Pgross * (1 - SP[s].growthEfficiency);
            return result;
        }
    }

    [Output]
    [Description("Plant gross potential growth")]
    [Units("kgDM/ha")]
    public double PlantPotentialGrossGrowth
    {
        get
        {
            double result = 0.0;
            for (int s = 0; s < NumSpecies; s++)
                result += SP[s].Pgross * 2.5;
            return result;
        }
    }
	[Output]
	[Description("Potential plant growth, correct for extreme temperatures")]
	[Units("kgDM/ha")]
	public double PlantPotentialGrowthWt
	{
		get { return p_dGrowthPot; }
	}
	[Output]
	[Description("Potential plant growth, correct for temperature and water")]
	[Units("kgDM/ha")]
	public double PlantGrowthNoNLimit
	{
		get { return p_dGrowthW; }
	}
	[Output]
	[Description("Actual plant growth (before littering)")]
	[Units("kgDM/ha")]
	public double PlantGrowthWt
	{
		//dm_daily_growth, including roots & before littering
		get { return p_dGrowth; }
	}

	[Output]
	[Description("Actual herbage (shoot) growth")]
	[Units("kgDM/ha")]
	public double HerbageGrowthWt
	{
		get { return p_dHerbage; }
	}
    [Output]
    [Description("Plant effective growth (actual minus tissue turnover)")]
    [Units("kgDM/ha")]
    public double PlantEffectiveGrowthWt
    {
        get { return p_dGrowth - p_dLitter - p_dRootSen; }
    }

	[Output]
	[Description("Dry matter amount of litter deposited onto soil surface")]
	[Units("kgDM/ha")]
	public double LitterDepositionWt
	{
		get { return p_dLitter; }
	}

	[Output]
	[Description("Dry matter amount of senescent roots added to soil FOM")]
	[Units("kgDM/ha")]
	public double RootSenescenceWt
	{
		get { return p_dRootSen; }
	}

	[Output]
	[Description("Plant C remobilisation")]
	[Units("kgC/ha")]
	public double PlantRemobilisedC
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += SP[s].Cremob;
			return result;
		}
	}

	[Output]
	[Description("Total dry matter amount available for removal (leaf+stem)")]
	[Units("kgDM/ha")]
	public double HarvestableWt
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += Math.Max(0.0, SP[s].dmleaf_green + SP[s].dmstem_green - SP[s].dmgreenmin)
					    + Math.Max(0.0, SP[s].dmdead - SP[s].dmdeadmin);
			return result;
		}
	}

	[Output]
	[Description("Amount of plant dry matter removed by harvest")]
	[Units("kgDM/ha")]
	public double HarvestWt
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += SP[s].dmdefoliated;
			return result;
		}
	}

	//**LAI & Cover
	[Output]
	[Description("Leaf area index of green leaves")]
	[Units("m^2/m^2")]
	public double LAI_green
	{
		get { return p_greenLAI; }
	}

	[Output]
	[Description("Leaf area index of dead leaves")]
	[Units("m^2/m^2")]
	public double LAI_dead
	{
		get { return p_deadLAI; }
	}

	[Output]
	[Description("Total leaf area index")]
	[Units("m^2/m^2")]
	public double LAI_total
	{
		get { return p_totalLAI; }
	}

	[Output]
	[Description("Fraction of soil covered by green leaves")]
	[Units("%")]
	public double Cover_green
	{
		get
		{
			if (p_greenLAI == 0) return 0;
			return (1.0 - Math.Exp(-p_lightExtCoeff * p_greenLAI));
		}

	}

	[Output]
	[Description("Fraction of soil covered by dead leaves")]
	[Units("%")]
	public double Cover_dead
	{
		get
		{
			if (p_deadLAI == 0) return 0;
			return (1.0 - Math.Exp(-p_lightExtCoeff * p_deadLAI));
		}
	}

	[Output]
	[Description("Fraction of soil covered by plants")]
	[Units("%")]
	public double Cover_tot
	{
		get
		{
			if (p_totalLAI == 0) return 0;
			return (1.0 - (Math.Exp(-p_lightExtCoeff * p_totalLAI)));
		}
	}

	//** Nitrogen
	[Output]
	[Description("Total amount of N in plants")]
	[Units("kg/ha")]
	public double TotalPlantN
	{
		get { return (AboveGroundN + BelowGroundN); }
	}

	[Output]
	[Description("Total amount of N in plants above ground")]
	[Units("kgN/ha")]
	public double AboveGroundN
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += SP[s].Nshoot;       //remoblised N is reported in stem
			return result;
		}
	}

	[Output]
	[Description("Total amount of N in plants below ground")]
	[Units("kgN/ha")]
	public double BelowGroundN
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += SP[s].Nroot;
			return result;
		}
	}

	[Output]
	[Description("Proportion of N above ground in relation to below ground")]
	[Units("%")]
	public double AboveGroundNPct
	{
		get
		{
			double result = 0;
			if (AboveGroundWt != 0)
				result = 100 * AboveGroundN / AboveGroundWt;
			return result;
		}
	}
	
	[Output]
	[Description("Total amount of N in standing plants")]
	[Units("kgN/ha")]
	public double StandingPlantN
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += SP[s].Nleaf + SP[s].Nstem;
			return result;
		}
	}
	[Output]
	[Description("Average N concentration of standing plants")]
	[Units("kgN/kgDM")]
	public double StandingPlantNConc
	{
		get
		{
			double Namount = 0.0;
			double DMamount = 0.0;
			for (int s = 0; s < NumSpecies; s++)
			{
				Namount += SP[s].Nleaf + SP[s].Nstem;
				DMamount += SP[s].dmleaf + SP[s].dmstem;
			}
			double result = Namount / DMamount;
			return result;
		}
	}

	[Output]
	[Description("Total amount of N in plants alive above ground")]
	[Units("kgN/ha")]
	public double AboveGroundLiveN
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += SP[s].Ngreen;
			return result;
		}
	}
	[Output]
	[Description("Total amount of N in dead plants above ground")]
	[Units("kgN/ha")]
	public double AboveGroundDeadN
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += SP[s].Ndead;
			return result;
		}
	}

	[Output]
	[Description("Total amount of N in the plant's leaves")]
	[Units("kgN/ha")]
	public double LeafN
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += SP[s].Nleaf;
			return result;
		}
	}

	[Output]
	[Description("Total amount of N in the plant's stems")]
	[Units("kgN/ha")]
	public double StemN
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += SP[s].Nstem;
			return result;
		}
	}

	[Output]
	[Description("Total amount of N in the plant's stolons")]
	[Units("kgN/ha")]
	public double StolonN
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += SP[s].Nstolon;
			return result;
		}
	}

	[Output]
	[Description("Total amount of N in the plant's roots")]
	[Units("kgN/ha")]
	public double RootN
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += SP[s].Nroot;
			return result;
		}
	}

	[Output]
	[Description("Average N concentration of leaves")]
	[Units("kgN/kgDM")]
	public double LeafNConc
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += SP[s].Ncleaf1 * SP[s].dmleaf1
			  		    + SP[s].Ncleaf2 * SP[s].dmleaf2
						+ SP[s].Ncleaf3 * SP[s].dmleaf3
						+ SP[s].Ncleaf4 * SP[s].dmleaf4;
			result = result / LeafWt;
			return result;
		}
	}

	[Output]
	[Description("Average N concentration in stems")]
	[Units("kgN/kgDM")]
	public double StemNConc
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += SP[s].Ncstem1 * SP[s].dmstem1
					    + SP[s].Ncstem2 * SP[s].dmstem2
					    + SP[s].Ncstem3 * SP[s].dmstem3
					    + SP[s].Ncstem4 * SP[s].dmstem4;
			result = result / StemWt;
			return result;
		}
	}

	[Output]
	[Description("Average N concentration in stolons")]
	[Units("kgN/kgDM")]
	public double StolonNConc
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += SP[s].Ncstol1 * SP[s].dmstol1
					    + SP[s].Ncstol2 * SP[s].dmstol2
					    + SP[s].Ncstol3 * SP[s].dmstol3;
			result = result / StolonWt;
			return result;
		}
	}

	[Output]
	[Description("Average N concentration in roots")]
	[Units("kgN/kgDM")]
	public double RootNConc
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += SP[s].Ncroot * SP[s].dmroot;
			result = result / RootWt;
			return result;
		}
	}

	[Output]
	[Description("Amount of N removed by harvest")]
	[Units("kgN/ha")]
	public double HarvestN
	{
		get
		{
			double result = 0.0;
			if (HarvestWt > 0.0)
			{
				for (int s = 0; s < NumSpecies; s++)
					result += SP[s].Ndefoliated;
			}
			return result;
		}
	}
	[Output]
	[Description("Average herbage digestibility")]
	[Units("0-1")]
	public double HerbageDigestibility
	{
		get
		{
			if (!p_Live || (StemWt + LeafWt) <= 0)
				return 0;

			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += SP[s].digestHerbage * (SP[s].dmstem + SP[s].dmleaf) / (StemWt + LeafWt);  //(dm_stem + dm_leaf);
			return result;
		}
	}
	[Output]
	[Description("Average digestibility of harvested material")]
	[Units("0-1")]
	public double DefoliatedDigestibility
	{
		get { return p_harvestDigest; }
	}
	[Output]
	[Description("Average ME of herbage")]
	[Units("(MJ/ha)")]
	public double HerbageME
	{
		get
		{
			double result = 16 * HerbageDigestibility * (StemWt + LeafWt);
			return result;
		}
	}
    [Output]
    [Description("Average ME concentration of herbage")]
    [Units("(MJ/kgDM)")]
    public double HerbageMEconc
    {
        get { return 16 * HerbageDigestibility; }
    }
	[Output]
	[Description("Amount of atmospheric N fixed")]
	[Units("kgN/ha")]
	public double PlantFixedN
	{
		get { return p_Nfix; }
	}

    [Output]
    [Description("Amount of N from senescing tissue potentially remobilisable")]
    [Units("kgN/ha")]
    public double PlantRemobilisableSenescedN
    {
        get
        {
            double result = 0.0;
            for (int s = 0; s < NumSpecies; s++)
                result += SP[s].Nremob;
            return result;
        }
    }
	[Output]
	[Description("Amount of N remobilised from senescing tissue")]
	[Units("kgN/ha")]
	public double PlantRemobilisedN
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += SP[s].remob2NewGrowth;
			return result;
		}
	}

	[Output]
	[Description("Amount of luxury N remobilised")]
	[Units("kgN/ha")]
	public double PlantLuxuryNRemobilised
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += SP[s].NFastRemob2 + SP[s].NFastRemob3;
			return result;
		}
	}

	[Output]
	[Description("Amount of luxury N potentially remobilisable")]
	[Units("kgN/ha")]
	public double PlantRemobilisableLuxuryN
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += SP[s].NLuxury2 + SP[s].NLuxury3;
			return result;
		}
	}
	[Output]
	[Description("Amount of N deposited as litter onto soil surface")]
	[Units("kgN/ha")]
	public double LitterDepositionN
	{
		get { return p_dNLitter; }
	}

	[Output]
	[Description("Amount of N added to soil FOM by senescent roots")]
	[Units("kgN/ha")]
	public double RootSenescenceN
	{
		get { return p_dNRootSen; }
	}

	[Output]
	[Description("Plant nitrogen requirement with luxury uptake")]
	[Units("kgN/ha")]
	public double NitrogenRequiredLuxury
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
			{
				result += SP[s].NdemandLux;
			}
			return result;
		}
	}

	[Output]
	[Description("Plant nitrogen requirement for optimum growth")]
	[Units("kgN/ha")]
	public double NitrogenRequiredOptimum
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
			{
				result += SP[s].NdemandOpt;
			}
			return result;
		}
	}

	[Output]
	[Description("Nitrogen amount in new growth")]
	[Units("kgN/ha")]
	public double PlantGrowthN
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
			{
				result += SP[s].newGrowthN;
			}
			return result;
		}
	}

	[Output]
	[Description("Nitrogen concentration in new growth")]
	[Units("-")]
	public double PlantGrowthNconc
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
			{
				result += SP[s].newGrowthN;
			}
			if (PlantGrowthWt > 0)
				result = result / PlantGrowthWt;
			else
				result = 0.0;
			return result;
		}
	}

	[Output]
	[Description("Plant nitrogen demand from soil")]
	[Units("kgN/ha")]
	public double NitrogenDemand
	{
		get { return p_soilNdemand; }
	}

	[Output]
	[Description("Plant available nitrogen in soil")]
	[Units("kgN/ha")]
	public double NitrogenSupply
	{
		get { return p_soilNavailable; }
	}

	[Output]
	[Description("Plant available nitrogen in soil layers")]
	[Units("kgN/ha")]
	public float[] NitrogenSupplyLayers
	{
		get { return SNSupply; }
	}

	[Output]
	[Description("Plant nitrogen uptake")]
	[Units("kgN/ha")]
	public double NitrogenUptake
	{
		get { return p_soilNuptake; }
	}

	[Output]
	[Description("Plant nitrogen uptake from soil layers")]
	[Units("kgN/ha")]
	public float[] NitrogenUptakeLayers
	{
		get { return SNUptake; }
	}

	[Output]
	[Description("Plant growth limiting factor due to nitrogen stress")]
	[Units("0-1")]
	public double GLFn
	{
		get { return p_gfn; }
	}
	[Output]
	[Description("Plant growth limiting factor due to plant N concentration")]
	[Units("0-1")]
	public double GLFnConcentration
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += SP[s].Ncfactor * SP[s].dmshoot;
			return (result / AboveGroundWt);
		}
	}
	[Output]
	[Description("Dry matter allocated to roots")]
	[Units("kgDM/ha")]
	public double DMToRoots
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
			{
				result += (1 - SP[s].fShoot) * SP[s].dGrowth;
			}
			return result;
		}
	}
	[Output]
	[Description("Dry matter allocated to shoot")]
	[Units("kgDM/ha")]
	public double DMToShoot
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
			{
				result += SP[s].fShoot * SP[s].dGrowth;
			}
			return result;
		}
	}
	[Output]
	[Description("Fraction of growth allocated to roots")]
	[Units("0-1")]
	public double FractionGrowthToRoot
	{
		get
		{
			double result = 0.0;
			if (p_dGrowth > 0)
				result = DMToRoots / p_dGrowth;
			return result;
		}
	}

	//** water related
	[Output]
	[Description("Root length density")]
	[Units("mm/mm^3")]
	public double[] rlv
	{
        get
        {
            double[] result = new double[dlayer.Length];
            //double p_srl = 75;           // specific root length (m root/g DMroot)
            //Compute the root length, total over the whole profile
            double Total_Rlength = (p_rootMass * 0.1) * specificRootLength[0];   // m root/m2 soil
            Total_Rlength *= 0.001;          // convert into mm root/mm2 soil)
            for (int layer = 0; layer < result.Length; layer++)
                result[layer] = RootFraction[layer] * Total_Rlength / dlayer[layer];    // mm root/mm3 soil
            return result;
        }
	}

	private double[] RootFraction;
	[Output]
	[Description("Fraction of root dry matter for each soil layer")]
	[Units("0-1")]
	public double[] RootWtFraction
	{
		get { return RootFraction; }
	}

	[Output]
	[Description("Plant water demand")]
	[Units("mm")]
	public double WaterDemand
	{
		get { return p_waterDemand; }
	}

	[Output]
	[Description("Plant available water in soil")]
	[Units("mm")]
	public double WaterSupply
	{
		get { return p_waterSupply; }
	}

	[Output]
	[Description("Plant available water in soil layers")]
	[Units("mm")]
	public float[] WaterSupplyLayers
	{
		get { return SWSupply; }
	}

	[Output]
	[Description("Plant water uptake")]
	[Units("mm")]
	public double WaterUptake
	{
		get { return p_waterUptake; }
	}

	[Output]
	[Description("Plant water uptake from soil layers")]
	[Units("mm")]
	public float[] WaterUptakeLayers
	{
		get { return SWUptake; }
	}

	[Output]
	[Description("Plant growth limiting factor due to water deficit")]
	[Units("0-1")]
	public double GLFwater
	{
		get { return p_gfwater; }
	}

	//**Stress factors
	[Output]
	[Description("Plant growth limiting factor due to temperature")]
	[Units("0-1")]
	public double GLFtemp
	{
		get { return p_gftemp; }
	}

	[Output]
	[Description("Generic plant growth limiting factor, used for other factors")]
	[Units("0-1")]
	public double GLFrgr
	{
		get
		{
			double p_Frgr = 0; //weighted value
			for (int s = 0; s < NumSpecies; s++)
			{
				double prop = 1.0 / NumSpecies;
				if (p_greenDM != 0.0)
				{
					prop = SP[s].dmgreen / AboveGroundLiveWt;
				}
				p_Frgr += SP[s].Frgr * prop;
			}
			return p_Frgr;
		}
	}

	[Output]
	[Description("Sward average height")]                 //needed by micromet
	[Units("mm")]
	public double Height
	{
		get { return p_height; }
	}

	//testing purpose
	[Output]
	[Description("Dry matter of plant pools at stage 1 (young)")]
	[Units("kgN/ha")]
	public double PlantStage1Wt
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += SP[s].dmleaf1 + SP[s].dmstem1 + SP[s].dmstol1;
			return result;
		}
	}

	[Output]
	[Description("Dry matter of plant pools at stage 2 (developing)")]
	[Units("kgN/ha")]
	public double PlantStage2Wt
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += SP[s].dmleaf2 + SP[s].dmstem2 + SP[s].dmstol2;
			return result;
		}
	}
	[Output]
	[Description("Dry matter of plant pools at stage 3 (mature)")]
	[Units("kgN/ha")]
	public double PlantStage3Wt
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += SP[s].dmleaf3 + SP[s].dmstem3 + SP[s].dmstol3;
			return result;
		}
	}
	[Output]
	[Description("Dry matter of plant pools at stage 4 (senescent)")]
	[Units("kgN/ha")]
	public double PlantStage4Wt
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += SP[s].dmleaf4 + SP[s].dmstem4;
			return result;
		}
	}

	[Output]
	[Description("N content of plant pools at stage 1 (young)")]
	[Units("kgN/ha")]
	public double PlantStage1N
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += SP[s].Nleaf1 + SP[s].Nstem1 + SP[s].Nstol1;
			return result;
		}
	}
	[Output]
	[Description("N content of plant pools at stage 2 (developing)")]
	[Units("kgN/ha")]
	public double PlantStage2N
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += SP[s].Nleaf2 + SP[s].Nstem2 + SP[s].Nstol2;
			return result;
		}
	}
	[Output]
	[Description("N content of plant pools at stage 3 (mature)")]
	[Units("kgN/ha")]
	public double PlantStage3N
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += SP[s].Nleaf3 + SP[s].Nstem3 + SP[s].Nstol3;
			return result;
		}
	}
	[Output]
	[Description("N content of plant pools at stage 4 (senescent)")]
	[Units("kgN/ha")]
	public double PlantStage4N
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += SP[s].Nleaf4 + SP[s].Nstem4;
			return result;
		}
	}

	private float HeightfromDM        // height calculation from DM, not output
	{
		get
		{
			float ht = (float)HeightMassFN.Value(p_greenDM + p_deadDM);
			if (ht < 20.0) ht = 20.0F;      // minimum = 20mm
			return ht;
		}

	}
	[Output]
	[Description("Vapour pressure deficit")]
	[Units("kPa")]
	public double VPD_out              // VPD effect on Growth Interpolation Set
	{
		get { return VPD(); }
	}

	[Output]
	[Description("Effect of vapour pressure on growth (used by micromet)")]
	[Units("0-1")]
	public double FVPD              // VPD effect on Growth Interpolation Set
	{                               // mostly = 1 for crop/grass/forage
		get { return FVPDFunction.Value(VPD()); }
	}

	//Following are species values (arrays)
	[Output]
	[Description("Leaf area index of green leaves, for each species")]
	[Units("m^2/m^2")]
	public double[] SpeciesGreenLAI
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].greenLAI;
			return result;
		}
	}
	[Output]
	[Description("Leaf area index of dead leaves, for each species")]
	[Units("m^2/m^2")]
	public double[] SpeciesDeadLAI
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].deadLAI;
			return result;
		}
	}
	[Output]
	[Description("Total leaf area index, for each species")]
	[Units("m^2/m^2")]
	public double[] SpeciesTotalLAI
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].totalLAI;
			return result;
		}
	}

	[Output]
	[Description("Total dry matter weight of plants for each plant species")]
	[Units("kgDM/ha")]
	public double[] SpeciesTotalWt
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].dmshoot + SP[s].dmroot;
			return result;
		}
	}
	[Output]
	[Description("Dry matter weight of plants above ground, for each species")]
	[Units("kgDM/ha")]
	public double[] SpeciesAboveGroundWt
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].dmshoot;
			return result;
		}
	}
	[Output]
	[Description("Dry matter weight of plants below ground, for each species")]
	[Units("kgDM/ha")]
	public double[] SpeciesBelowGroundWt
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].dmroot;
			return result;
		}
	}
	[Output]
	[Description("Dry matter weight of standing herbage, for each species")]
	[Units("kgDM/ha")]
	public double[] SpeciesStandingWt
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].dmleaf + SP[s].dmstem;
			return result;
		}
	}
	[Output]
	[Description("Dry matter weight of live standing plants parts for each species")]
	[Units("kgDM/ha")]
	public double[] SpeciesStandingLiveWt
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].dmleaf_green + SP[s].dmstem_green;
			return result;
		}
	}
	[Output]
	[Description("Dry matter weight of dead standing plants parts for each species")]
	[Units("kgDM/ha")]
	public double[] SpeciesStandingDeadWt
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].dmleaf4 + SP[s].dmstem4;
			return result;
		}
	}

	[Output]
	[Description("Dry matter weight of leaves for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesLeafWt
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].dmleaf1 + SP[s].dmleaf2 + SP[s].dmleaf3 + SP[s].dmleaf4;
			return result;
		}
	}
	[Output]
	[Description("Dry matter weight of stems for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesStemWt
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].dmstem1 + SP[s].dmstem2 + SP[s].dmstem3 + SP[s].dmstem4;
			return result;
		}
	}
	[Output]
	[Description("Dry matter weight of stolons for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesStolonWt
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].dmstol1 + SP[s].dmstol2 + SP[s].dmstol3;
			return result;
		}
	}
	[Output]
	[Description("Dry matter weight of roots for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesRootWt
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].dmroot;
			return result;
		}
	}

	[Output]
	[Description("Total N amount for each plant species")]
	[Units("kgN/ha")]
	public double[] SpeciesTotalN
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].Nshoot + SP[s].Nroot;
			return result;
		}
	}
	[Output]
	[Description("N amount of standing herbage, for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesStandingN
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].Nleaf + SP[s].Nstem;
			return result;
		}
	}

	[Output]
	[Description("N amount in the plant's leaves, for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesLeafN
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].Nleaf1 + SP[s].Nleaf2 + SP[s].Nleaf3 + SP[s].Nleaf4;
			return result;
		}
	}
	[Output]
	[Description("N amount in the plant's stems, for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesStemN
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].Nstem1 + SP[s].Nstem2 + SP[s].Nstem3 + SP[s].Nstem4;
			return result;
		}
	}
	[Output]
	[Description("N amount in the plant's stolons, for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesStolonN
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].Nstol1 + SP[s].Nstol2 + SP[s].Nstol3;
			return result;
		}
	}
	[Output]
	[Description("N amount in the plant's roots, for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesRootsN
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].Nroot;
			return result;
		}
	}

	[Output]
	[Description("Average N concentration in leaves, for each species")]
	[Units("kgN/kgDM")]
	public double[] SpeciesLeafNConc
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
			{
				result[s] = SP[s].Ncleaf1 * SP[s].dmleaf1
						+ SP[s].Ncleaf2 * SP[s].dmleaf2
						+ SP[s].Ncleaf3 * SP[s].dmleaf3
						+ SP[s].Ncleaf4 * SP[s].dmleaf4;
				result[s] = result[s] / SP[s].dmleaf;
			}
			return result;
		}
	}
	[Output]
	[Description("Average N concentration in stems, for each species")]
	[Units("kgN/kgDM")]
	public double[] SpeciesStemNConc
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
			{
				result[s] = SP[s].Ncstem1 * SP[s].dmstem1
						+ SP[s].Ncstem2 * SP[s].dmstem2
						+ SP[s].Ncstem3 * SP[s].dmstem3
						+ SP[s].Ncstem4 * SP[s].dmstem4;
				result[s] = result[s] / SP[s].dmstem;
			}
			return result;
		}
	}
	[Output]
	[Description("Average N concentration in stolons, for each species")]
	[Units("kgN/kgDM")]
	public double[] SpeciesStolonNConc
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
			{
                if (SP[s].dmstol > 0)
                {
                    result[s] = SP[s].Ncstol1 * SP[s].dmstol1
                          + SP[s].Ncstol2 * SP[s].dmstol2
                          + SP[s].Ncstol3 * SP[s].dmstol3;
                    result[s] = result[s] / SP[s].dmstol;
                }
                else
                    result[s] = 0.0;
			}
			return result;
		}
	}
	[Output]
	[Description("Average N concentration in roots, for each species")]
	[Units("kgN/kgDM")]
	public double[] SpeciesRootNConc
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
			{
				result[s] = SP[s].Ncroot * SP[s].dmroot;
				result[s] = result[s] / SP[s].dmroot;
			}
			return result;
		}
	}


	[Output]
	[Description("Dry matter weight of leaves at stage 1 (young) for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesLeafStage1Wt
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].dmleaf1;
			return result;
		}
	}
	[Output]
	[Description("Dry matter weight of leaves at stage 2 (developing) for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesLeafStage2Wt
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].dmleaf2;
			return result;
		}
	}
	[Output]
	[Description("Dry matter weight of leaves at stage 3 (mature) for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesLeafStage3Wt
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].dmleaf3;
			return result;
		}
	}
	[Output]
	[Description("Dry matter weight of leaves at stage 4 (dead) for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesLeafStage4Wt
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].dmleaf4;
			return result;
		}
	}
	[Output]
	[Description("Dry matter weight of stems at stage 1 (young) for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesStemStage1Wt
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].dmstem1;
			return result;
		}
	}
	[Output]
	[Description("Dry matter weight of stems at stage 2 (developing) for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesStemStage2Wt
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].dmstem2;
			return result;
		}
	}
	[Output]
	[Description("Dry matter weight of stems at stage 3 (mature) for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesStemStage3Wt
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].dmstem3;
			return result;
		}
	}
	[Output]
	[Description("Dry matter weight of stems at stage 4 (dead) for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesStemStage4Wt
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].dmstem4;
			return result;
		}
	}
	[Output]
	[Description("Dry matter weight of stolons at stage 1 (young) for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesStolonStage1Wt
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].dmstol1;
			return result;
		}
	}
	[Output]
	[Description("Dry matter weight of stolons at stage 2 (developing) for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesStolonStage2Wt
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].dmstol2;
			return result;
		}
	}
	[Output]
	[Description("Dry matter weight of stolons at stage 3 (mature) for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesStolonStage3Wt
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].dmstol3;
			return result;
		}
	}

	[Output]
	[Description("N amount in leaves at stage 1 (young) for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesLeafStage1N
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].Nleaf1;
			return result;
		}
	}
	[Output]
	[Description("N amount in leaves at stage 2 (developing) for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesLeafStage2N
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].Nleaf2;
			return result;
		}
	}
	[Output]
	[Description("N amount in leaves at stage 3 (mature) for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesLeafStage3N
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].Nleaf3;
			return result;
		}
	}
	[Output]
	[Description("N amount in leaves at stage 4 (dead) for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesLeafStage4N
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].Nleaf4;
			return result;
		}
	}
	[Output]
	[Description("N amount in stems at stage 1 (young) for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesStemStage1N
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].Nstem1;
			return result;
		}
	}
	[Output]
	[Description("N amount in stems at stage 2 (developing) for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesStemStage2N
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].Nstem2;
			return result;
		}
	}
	[Output]
	[Description("N amount in stems at stage 3 (mature) for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesStemStage3N
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].Nstem3;
			return result;
		}
	}
	[Output]
	[Description("N amount in stems at stage 4 (dead) for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesStemStage4N
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].Nstem4;
			return result;
		}
	}
	[Output]
	[Description("N amount in stolons at stage 1 (young) for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesStolonStage1N
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].Nstol1;
			return result;
		}
	}
	[Output]
	[Description("N amount in stolons at stage 2 (developing) for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesStolonStage2N
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].Nstol2;
			return result;
		}
	}
	[Output]
	[Description("N amount in stolons at stage 3 (mature) for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesStolonStage3N
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].Nstol3;
			return result;
		}
	}

	[Output]
	[Description("N concentration of leaves at stage 1 (young) for each species")]
	[Units("kgN/kgDM")]
	public double[] SpeciesLeafStage1NConc
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].Ncleaf1;
			return result;
		}
	}
	[Output]
	[Description("N concentration of leaves at stage 2 (developing) for each species")]
	[Units("kgN/kgDM")]
	public double[] SpeciesLeafStage2NConc
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].Ncleaf2;
			return result;
		}
	}
	[Output]
	[Description("N concentration of leaves at stage 3 (mature) for each species")]
	[Units("kgN/kgDM")]
	public double[] SpeciesLeafStage3NConc
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].Ncleaf3;
			return result;
		}
	}
	[Output]
	[Description("N concentration of leaves at stage 4 (dead) for each species")]
	[Units("kgN/kgDM")]
	public double[] SpeciesLeafStage4NConc
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].Ncleaf4;
			return result;
		}
	}
	[Output]
	[Description("N concentration of stems at stage 1 (young) for each species")]
	[Units("kgN/kgDM")]
	public double[] SpeciesStemStage1NConc
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].Ncstem1;
			return result;
		}
	}
	[Output]
	[Description("N concentration of stems at stage 2 (developing) for each species")]
	[Units("kgN/kgDM")]
	public double[] SpeciesStemStage2NConc
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].Ncstem2;
			return result;
		}
	}
	[Output]
	[Description("N concentration of stems at stage 3 (mature) for each species")]
	[Units("kgN/kgDM")]
	public double[] SpeciesStemStage3NConc
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].Ncstem3;
			return result;
		}
	}
	[Output]
	[Description("N concentration of stems at stage 4 (dead) for each species")]
	[Units("kgN/kgDM")]
	public double[] SpeciesStemStage4NConc
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].Ncstem4;
			return result;
		}
	}
	[Output]
	[Description("N concentration of stolons at stage 1 (young) for each species")]
	[Units("kgN/kgDM")]
	public double[] SpeciesStolonStage1NConc
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].Ncstol1;
			return result;
		}
	}
	[Output]
	[Description("N concentration of stolons at stage 2 (developing) for each species")]
	[Units("kgN/kgDM")]
	public double[] SpeciesStolonStage2NConc
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].Ncstol2;
			return result;
		}
	}
	[Output]
	[Description("N concentration of stolons at stage 3 (mature) for each species")]
	[Units("kgN/kgDM")]
	public double[] SpeciesStolonStage3NConc
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].Ncstol3;
			return result;
		}
	}

    [Output]
    [Description("Potential growth, after water stress, for each species")]
    [Units("kgDM/ha")]
    public double[] SpeciesPotGrowthW
    {
        get
        {
            double[] result = new double[NumSpecies];
            for (int s = 0; s < NumSpecies; s++)
                result[s] = SP[s].dGrowthW;
            return result;
        }
    }
    [Output]
    [Description("Gross potential growth for each species")]
    [Units("kgDM/ha")]
    public double[] SpeciesPotGrowthGross
    {
        get
        {
            double[] result = new double[NumSpecies];
            for (int s = 0; s < NumSpecies; s++)
                result[s] = SP[s].Pgross * 2.5;
            return result;
        }
    }
    [Output]
    [Description("Net potential growth for each species (after respiration)")]
    [Units("kgDM/ha")]
    public double[] SpeciesPotGrowthNet
    {
        get
        {
            double[] result = new double[NumSpecies];
            for (int s = 0; s < NumSpecies; s++)
                result[s] = SP[s].dGrowthPot;
            return result;
        }
    }

	[Output]
	[Description("Actual growth for each species")]
	[Units("kgDM/ha")]
	public double[] SpeciesGrowthWt
	{
		get
		{
			double[] result = new double[NumSpecies];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].dGrowth;
			return result;
		}
	}

	[Output]
	[Description("Litter amount deposited onto soil surface, for each species")]
	[Units("kgDM/ha")]
	public double[] SpeciesLitterWt
	{
		get
		{
			double[] result = new double[NumSpecies];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].dLitter;
			return result;
		}
	}
	[Output]
	[Description("Amount of senesced roots added to soil FOM, for each species")]
	[Units("kgDM/ha")]
	public double[] SpeciesRootSenescedWt
	{
		get
		{
			double[] result = new double[NumSpecies];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].dRootSen;
			return result;
		}
	}

	[Output]
	[Description("Amount of dry matter harvestable for each species (leaf+stem)")]
	[Units("kgDM/ha")]
	public double[] SpeciesHarvestableWt
	{
		get
		{
			double[] result = new double[NumSpecies];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = Math.Max(0.0, SP[s].dmleaf_green + SP[s].dmstem_green - SP[s].dmgreenmin)
						  + Math.Max(0.0, SP[s].dmdead - SP[s].dmdeadmin);
			return result;
		}
	}
	[Output]
	[Description("Amount of plant dry matter removed by harvest, for each species")]
	[Units("kgDM/ha")]
	public double[] SpeciesHarvestWt
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].dmdefoliated;
			return result;
		}
	}

	[Output]
	[Description("Proportion in the dry matter harvested of each species")]
	[Units("%")]
	public double[] SpeciesHarvestPct
	{
		get
		{
			double[] result = new double[SP.Length];
			double myTotal = StandingPlantWt;
			for (int s = 0; s < NumSpecies; s++)
			{
				if (myTotal > 0.0)
					result[s] = (SP[s].dmstem + SP[s].dmleaf) * 100 / myTotal;
			}
			return result;
		}
	}

	private double[] FractionToHarvest;
	[Output]
	[Description("Fraction to harvest for each species")]
	[Units("0-1")]
	public double[] SpeciesHarvestFraction
	{
		get { return FractionToHarvest; }
	}

	[Output]
	[Description("Rate of turnover for live DM, for each species")]
	[Units("0-1")]
	public double[] SpeciesLiveDMTurnoverRate
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
			{
				result[s] = SP[s].gama;
			}
			return result;
		}
	}
	[Output]
	[Description("Rate of turnover for dead DM, for each species")]
	[Units("0-1")]
	public double[] SpeciesDeadDMTurnoverRate
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
			{
				result[s] = SP[s].gamad;
			}
			return result;
		}
	}
	[Output]
	[Description("Rate of DM turnover for stolons, for each species")]
	[Units("0-1")]
	public double[] SpeciesStolonDMTurnoverRate
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
			{
				result[s] = SP[s].gamas;
			}
			return result;
		}
	}
	[Output]
	[Description("Rate of DM turnover for roots, for each species")]
	[Units("0-1")]
	public double[] SpeciesRootDMTurnoverRate
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
			{
				result[s] = SP[s].gamar;
			}
			return result;
		}
	}


    [Output]
    [Description("Amount of N from senescing tissue potentially remobilisable, for each species")]
    [Units("kgN/ha")]
    public double[] SpeciesSenescedNRemobilisable
    {
        get
        {
            double[] result = new double[SP.Length];
            for (int s = 0; s < NumSpecies; s++)
            {
                result[s] = SP[s].Nremob;
            }
            return result;
        }
    }
	[Output]
	[Description("Amount of N remobilised from senesced material, for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesSenescedNRemobilised
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
			{
				result[s] = SP[s].remob2NewGrowth;
			}
			return result;
		}
	}


	[Output]
	[Description("Amount of luxury N potentially remobilisable, for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesRemobilisableNLuxury
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
			{
				result[s] = SP[s].NLuxury2 + SP[s].NLuxury3;
			}
			return result;
		}
	}

    [Output]
    [Description("Amount of luxury N remobilised, for each species")]
    [Units("kgN/ha")]
    public double[] SpeciesLuxuryNRemobilised
    {
        get
        {
            double[] result = new double[SP.Length];
            for (int s = 0; s < NumSpecies; s++)
            {
                result[s] = SP[s].NFastRemob2 + SP[s].NFastRemob3;
            }
            return result;
        }
    }
	[Output]
	[Description("Amount of atmospheric N fixed, for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesFixedN
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
			{
				result[s] = SP[s].Nfix;
			}
			return result;
		}
	}

	[Output]
	[Description("Amount of N required with luxury uptake, for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesRequiredNLuxury
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
			{
				result[s] = SP[s].NdemandLux;
			}
			return result;
		}
	}

	[Output]
	[Description("Amount of N required for optimum growth, for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesRequiredNOptimum
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
			{
				result[s] = SP[s].NdemandOpt;
			}
			return result;
		}
	}

	[Output]
	[Description("Amount of N demaned from soil, for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesDemandN
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
			{
				result[s] = SP[s].soilNdemand;
			}
			return result;
		}
	}

	[Output]
	[Description("Amount of N in new growth, for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesGrowthN
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
			{
				result[s] = SP[s].newGrowthN;
			}
			return result;
		}
	}

	[Output]
	[Description("Nitrogen concentration in new growth, for each species")]
	[Units("kgN/kgDM")]
    public double[] SpeciesGrowthNconc
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
			{
				if (SP[s].dGrowth > 0)
					result[s] = SP[s].newGrowthN / SP[s].dGrowth;
				else
					result[s] = 0.0;
			}
			return result;
		}
	}
	[Output]
	[Description("Amount of N uptake, for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesUptakeN
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
			{
				result[s] = SP[s].soilNuptake;
			}
			return result;
		}
	}

	[Output]
	[Description("Amount of N deposited as litter onto soil surface, for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesLitterN
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
			{
				result[s] = SP[s].dNLitter;
			}
			return result;
		}
	}
	[Output]
	[Description("Amount of N from senesced roots added to soil FOM, for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesSenescedN
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
			{
				result[s] = SP[s].dNrootSen;
			}
			return result;
		}
	}

	[Output]
	[Description("Amount of plant nitrogen removed by harvest, for each species")]
	[Units("kgN/ha")]
	public double[] SpeciesHarvestN
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].Ndefoliated;
			return result;
		}
	}

    [Output]
    [Description("Stress factor on photosynthesis due to temperature, for each species")]
    [Units("0-1")]
    public double[] SpeciesTstress
    {
        get
        {
            double[] result = new double[SP.Length];
            for (int s = 0; s < NumSpecies; s++)
                result[s] = SP[s].lowTempStress * SP[s].highTempStress;
            return result;
        }
    }

	[Output]
	[Description("Growth limiting factor due to nitrogen, for each species")]
	[Units("0-1")]
	public double[] SpeciesGLFN
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].gfn;
			return result;
		}
	}
	[Output]
	[Description("Growth limiting factor due to temperature, for each species")]
	[Units("0-1")]
	public double[] SpeciesGLFT
	{
		get
		{
			double[] result = new double[SP.Length];
            double Tmnw = 0.75 * MetData.maxt + 0.25 * MetData.mint;  // weighted Tmean
            for (int s = 0; s < NumSpecies; s++)
                result[s] = SP[s].GFTemperature(Tmnw);
			return result;
		}
	}
	[Output]
	[Description("Growth limiting factor due to water deficit, for each species")]
	[Units("0-1")]
	public double[] SpeciesGLFW
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].gfwater;
			return result;
		}
	}

	[Output]
	[Description("Irridance on the top of canopy")]
	[Units("W.m^2/m^2")]
	public double[] SpeciesIrradianceTopCanopy
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].IL1;
			return result;
		}
	}
	[Output]
	[Description("Potential C assimilation, corrected for extreme temperatures")]
	[Units("kgC/ha")]
	public double[] SpeciesPotCarbonAssimilation
	{
		get
		{
			double[] result = new double[SP.Length];
			for (int s = 0; s < NumSpecies; s++)
				result[s] = SP[s].Pgross;
			return result;
		}
	}
	[Output]
	[Description("Loss of C via respiration")]
	[Units("kgC/ha")]
	public double[] SpeciesCarbonLossRespiration
	{
		get
		{
			double[] result = new double[SP.Length];
            for (int s = 0; s < NumSpecies; s++)
                result[s] = SP[s].Resp_m + SP[s].Pgross * (1.0 - SP[s].growthEfficiency);
			return result;
		}
	}

	[Output]
	[Description("Gross primary productivity")]
	[Units("kgC/ha")]
	public double GPP
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += SP[s].Pgross;
			return result;
		}
	}
	[Output]
	[Description("Net primary productivity")]
	[Units("kgC/ha")]
	public double NPP
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += SP[s].Pgross * 0.75 - SP[s].Resp_m;
			return result;
		}
	}
	[Output]
	[Description("Net above-ground primary productivity")]
	[Units("kgC/ha")]
	public double NAPP
	{
		get
		{
			double result = 0.0;
			for (int s = 0; s < NumSpecies; s++)
				result += (SP[s].Pgross * SP[s].growthEfficiency - SP[s].Resp_m) * SP[s].fShoot;
			return result;
		}
	}

    [Output]
    [Description("Net below-ground primary productivity")]
    [Units("kgC/ha")]
    public double NBPP
    {
        get
        {
            double result = 0.0;
            for (int s = 0; s < NumSpecies; s++)
                result += (SP[s].Pgross * SP[s].growthEfficiency - SP[s].Resp_m) * (1.0 - SP[s].fShoot);
            return result;
        }
    }

    [Output]
    [Description("Fraction of DM allocated to shoot")]
    [Units("0-1")]
    public double[] speciesFShoot
    {
        get
        {
            double[] result = new double[NumSpecies];
            for (int s = 0; s < NumSpecies; s++)
                result[s] = SP[s].fShoot;
            return result;
        }
    }
	#endregion

    #region "Functions"

	/// <summary>
	/// water uptake processes ...
	/// Rainss Notes 20010707
	///  - Should this be done per species? Is using the root frontier an acceptable solution?
	///  - Plant2 breaks this into two parts: WaterSupply and DoWaterUptake
	/// </summary>
	/// <returns></returns>
	private float SWUptakeProcess()
	{
		//find out soil available water
		p_waterSupply = 0;
		for (int layer = 0; layer < dlayer.Length; layer++)
		{
			SWSupply[layer] = (float)(Math.Max(0.0,
			kl[layer] * (sw_dep[layer] - ll[layer] * (dlayer[layer]))) * LayerFractionForRoots(layer, p_rootFrontier));

			if (layer < p_bottomRootLayer)
				p_waterSupply += SWSupply[layer];
		}

		//uptake in proportion
		WaterChangedType WaterUptake = new WaterChangedType();
		WaterUptake.DeltaWater = new double[dlayer.Length];
		//float[] SWUptake = new float[dlayer.Length];
		float Fraction = (float)Math.Min(1.0, p_waterDemand / p_waterSupply);
		float actualUptake = 0.0F;
		for (int layer = 0; layer < p_bottomRootLayer; layer++)
		{   //water are taken up only in top layers that root can reach.
			SWUptake[layer] = SWSupply[layer] * Fraction;
			actualUptake += SWUptake[layer];
			WaterUptake.DeltaWater[layer] = -SWUptake[layer];
		}

		if (WaterChanged != null)
			WaterChanged.Invoke(WaterUptake);

		return actualUptake;
	}

	/// <summary>
	/// Compute the distribution of roots in the soil profile (sum is equal to one)
	/// </summary>
	/// <returns>The proportion of root mass in each soil layer</returns>
	private double[] RootProfileDistribution()
	{
		int nLayers = dlayer.Length;
		double[] result = new double[nLayers];
		double sumProportion = 0;

		switch (p_RootDistributionMethod)
		{
			case 0:
				{
					// homogenous distribution over soil profile (same root density throughout the profile)
					double DepthTop = 0;
					for (int layer = 0; layer < nLayers; layer++)
					{
						if (DepthTop >= p_rootFrontier)
							result[layer] = 0.0;
						else if (DepthTop + dlayer[layer] <= p_rootFrontier)
                            result[layer] = dlayer[layer];
						else
							result[layer] = p_rootFrontier - DepthTop;
						sumProportion += result[layer];
						DepthTop += dlayer[layer];
					}
					break;
				}
			case 1:
				{
					// distribution given by the user
					Array.Resize(ref rlvp, nLayers);	// This will remove values in excess (non-existing layers) or add zeroes if layers are missing
					for (int layer = 0; layer < nLayers; layer++)
					{
						result[layer] = rlvp[layer];
						sumProportion += result[layer];
					}
					break;
				}
			case 2:
				{
					// distribution calculated using ExpoLinear method
					//  Considers homogeneous distribution from surface down to a fraction of root depth (p_ExpoLinearDepthParam)
					//   below this depth, the proportion of root decrease following a power function (exponent = p_ExpoLinearCurveParam)
					//   if exponent is one than the proportion decreases linearly.
					double DepthTop = 0;
					double DepthFirstStage = p_rootFrontier * p_ExpoLinearDepthParam[0];
					double DepthSecondStage = p_rootFrontier - DepthFirstStage;
					for (int layer = 0; layer < nLayers; layer++)
					{
						if (DepthTop >= p_rootFrontier)
							result[layer] = 0.0;
						else if (DepthTop + dlayer[layer] <= DepthFirstStage)
							result[layer] = dlayer[layer];  // 1.0
						else
						{
							if (DepthTop < DepthFirstStage)
                                result[layer] = (DepthFirstStage - DepthTop);   // / dlayer[layer]
                            if ((p_ExpoLinearDepthParam[0] < 1.0) && (p_ExpoLinearCurveParam[0] > 0.0))
							{
								double thisDepth = Math.Max(0.0, DepthTop - DepthFirstStage);
                                double Ftop = (thisDepth - DepthSecondStage) * Math.Pow(1 - thisDepth / DepthSecondStage, p_ExpoLinearCurveParam[0]) / (p_ExpoLinearCurveParam[0] + 1);
								thisDepth = Math.Min(DepthTop + dlayer[layer] - DepthFirstStage, DepthSecondStage);
                                double Fbottom = (thisDepth - DepthSecondStage) * Math.Pow(1 - thisDepth / DepthSecondStage, p_ExpoLinearCurveParam[0]) / (p_ExpoLinearCurveParam[0] + 1);
                                result[layer] += Math.Max(0.0, Fbottom - Ftop);  // / dlayer[layer]
							}
							else if (DepthTop + dlayer[layer] <= p_rootFrontier)
                                result[layer] += Math.Min(DepthTop + dlayer[layer], p_rootFrontier) - Math.Max(DepthTop, DepthFirstStage);  //  / dlayer[layer]
						}
                        sumProportion += result[layer];
						DepthTop += dlayer[layer];
					}
					break;
				}
			default:
				{
					throw new Exception("No valid method for computing root distribution was selected");
				}
		}
		if (sumProportion > 0)
			for (int layer = 0; layer < nLayers; layer++)
				result[layer] = result[layer] / sumProportion;
		else
			throw new Exception("Could not calculate root distribution");
		return result;
	}

	/// <summary>
	/// Compute how much of the layer is actually explored by roots
	/// </summary>
	/// <param name="layer"></param>
	/// <param name="root_depth"></param>
	/// <returns>Fraction of layer explored by roots</returns>
	private double LayerFractionForRoots(int layer, double root_depth)
	{
		double depth_to_layer_top = 0;      // depth to top of layer (mm)
		double depth_to_layer_bottom = 0;   // depth to bottom of layer (mm)
		double fraction_in_layer = 0;
		for (int i = 0; i <= layer; i++)
			depth_to_layer_bottom += dlayer[i];
		depth_to_layer_top = depth_to_layer_bottom - dlayer[layer];
		fraction_in_layer = (root_depth - depth_to_layer_top) / (depth_to_layer_bottom - depth_to_layer_top);

		return Math.Min(1.0, Math.Max(0.0, fraction_in_layer));
	}

    /// <summary>
	/// Nitrogen uptake process
	/// </summary>
	/// <returns></returns>
	private double SNUptakeProcess()
	{
		//Uptake from the root_zone
		NitrogenChangedType NUptake = new NitrogenChangedType();
		NUptake.Sender = "AgPasture";
		NUptake.SenderType = "Plant";
		NUptake.DeltaNO3 = new double[dlayer.Length];
		NUptake.DeltaNH4 = new double[dlayer.Length];

		float Fraction = 0;
		if (p_soilNavailable > 0)
		{
			Fraction = (float)Math.Min(1.0, p_soilNuptake / p_soilNavailable);
		}

		double n_uptake = 0;

		if (alt_N_uptake == "yes")
		{
			double
			uptake_multiplier = double.MaxValue,
			totSWUptake = SWUptake.Sum();

			double[]
			availableNH4_bylayer = new double[dlayer.Length],
			availableNO3_bylayer = new double[dlayer.Length],
			diffNH4_bylayer = new double[dlayer.Length],
			diffNO3_bylayer = new double[dlayer.Length];

			for (int sLayer = 0; sLayer < dlayer.Length; sLayer++)
			{
				double
				totN = nh4[sLayer] + no3[sLayer],
				fracH2O = SWUptake[sLayer] / totSWUptake;

				if (totN > 0)
				{
					availableNH4_bylayer[sLayer] = fracH2O * nh4[sLayer] / totN;
					availableNO3_bylayer[sLayer] = fracH2O * no3[sLayer] / totN;

					//if we have no3 and nh4 in this layer then calculate our uptake multiplier, otherwise set it to 0
					//the idea behind the multiplier is that it allows us to calculate the max amount of N we can extract
					//without forcing any of the layers below 0 AND STILL MAINTAINING THE RATIO as calculated with fracH2O
					//NOTE: it doesn't matter whether we use nh4 or no3 for this calculation, we will get the same answer regardless
					uptake_multiplier = nh4[sLayer] * no3[sLayer] > 0 ? Math.Min(uptake_multiplier, nh4[sLayer] / availableNH4_bylayer[sLayer]) : 0;
				}
				else
				{
					availableNH4_bylayer[sLayer] = 0;
					availableNO3_bylayer[sLayer] = 0;
				}
			}

			//adjust availability values with the multiplier we just calculated
			availableNH4_bylayer = availableNH4_bylayer.Select(x => x * uptake_multiplier).ToArray();
			availableNO3_bylayer = availableNO3_bylayer.Select(x => x * uptake_multiplier).ToArray();

			//calculate how much no3/nh4 will be left in the soil layers (diff_nxx[layer] = nxx[layer] - availableNH4_bylayer[layer])
			diffNH4_bylayer = nh4.Select((x, sLayer) => Math.Max(0, x - availableNH4_bylayer[sLayer])).ToArray();
			diffNO3_bylayer = no3.Select((x, sLayer) => Math.Max(0, x - availableNO3_bylayer[sLayer])).ToArray();

			//adjust this by the sum of all leftover so we get a ratio we can use later
			double sum_diff = diffNH4_bylayer.Sum() + diffNO3_bylayer.Sum();
			diffNH4_bylayer = diffNH4_bylayer.Select(x => x / sum_diff).ToArray();
			diffNO3_bylayer = diffNO3_bylayer.Select(x => x / sum_diff).ToArray();

			double
				//available N from our 'withwater' calcs (still some left in the 'diff' arrays if this isn't enough)
			avail_withwater = availableNH4_bylayer.Sum() + availableNO3_bylayer.Sum(),
				//if not enough N was available via the 'withwater' calcs this will be positive and will require more from the 'diffs' we calculated
			shortfall_withwater = p_soilNuptake - avail_withwater;

			if (shortfall_withwater > 0)
			{
				//this cap should not be needed because shortfall is already capped via the math.min in the scaled_demand calcs (leave it here though)
				double scaled_diff = Math.Min(shortfall_withwater / avail_withwater, 1);

				availableNH4_bylayer = availableNH4_bylayer.Select((x, sLayer) => x + shortfall_withwater * diffNH4_bylayer[sLayer]).ToArray();
				availableNO3_bylayer = availableNO3_bylayer.Select((x, sLayer) => x + shortfall_withwater * diffNO3_bylayer[sLayer]).ToArray();
			}

			NUptake.DeltaNH4 = availableNH4_bylayer.Select(x => x * -1).ToArray();
			NUptake.DeltaNO3 = availableNO3_bylayer.Select(x => x * -1).ToArray();

			for (int layer = 0; layer < p_bottomRootLayer; layer++)
				n_uptake += SNUptake[layer] = (float)(NUptake.DeltaNH4[layer] + NUptake.DeltaNO3[layer]) * -1;

			double[] diffs = NUptake.DeltaNO3.Select((x, i) => Math.Max(no3[i] + x + 0.00000001, 0)).ToArray();
			if (diffs.Any(x => x == 0))
				throw new Exception();

		}

		/*if (ValsMode == "withwater")
		{
		NUptake.DeltaNO3 = SP[0].availableNO3_bylayer.Select(x => x * -1).ToArray();
		NUptake.DeltaNH4 = SP[0].availableNH4_bylayer.Select(x => x * -1).ToArray();

		for (int layer = 0; layer < p_bottomRootLayer; layer++)
		SNUptake[layer] = (float)(SP[0].availableNO3_bylayer[layer] + SP[0].availableNH4_bylayer[layer]);
		n_uptake = SNUptake.Sum();
		}*/
		else
		{
			for (int layer = 0; layer < p_bottomRootLayer; layer++)
			{   //N are taken up only in top layers that root can reach (including buffer Zone).
				n_uptake += (no3[layer] + nh4[layer]) * Fraction;
				SNUptake[layer] = (no3[layer] + nh4[layer]) * Fraction;

				NUptake.DeltaNO3[layer] = -no3[layer] * Fraction;
				NUptake.DeltaNH4[layer] = -nh4[layer] * Fraction;
			}
		}

		if (NitrogenChanged != null)
			NitrogenChanged.Invoke(NUptake);
		return n_uptake;
	}

	/// <summary>
	/// return plant litter to surface organic matter poor
	/// </summary>
	/// <param name="amtDM"></param>
	/// <param name="amtN"></param>
	/// <param name="frac"></param>
	private void DoSurfaceOMReturn(double amtDM, double amtN, double frac)
	{
		float dDM = (float)amtDM;

		BiomassRemovedType BR = new BiomassRemovedType();
		string[] type = new string[1];
		float[] dltdm = new float[1];
		float[] dltn = new float[1];
		float[] dltp = new float[1];
		float[] fraction = new float[1];

		type[0] = "grass";
		dltdm[0] = dDM;                 // kg/ha
		dltn[0] = (float)amtN;         // dDM * (float)dead_nconc;
		dltp[0] = dltn[0] * 0.3F;       //just a stub here, no P budgeting process in this module
		fraction[0] = (float)frac;

		BR.crop_type = "grass";
		BR.dm_type = type;
		BR.dlt_crop_dm = dltdm;
		BR.dlt_dm_n = dltn;
		BR.dlt_dm_p = dltp;
		BR.fraction_to_residue = fraction;
		BiomassRemoved.Invoke(BR);

	}

	/// <summary>
	/// return scenescent roots into fresh organic matter pool in soil
	/// </summary>
	/// <param name="rootSen"></param>
	/// <param name="NinRootSen"></param>
	private void DoIncorpFomEvent(double rootSen, double NinRootSen)
	{
		FOMLayerLayerType[] fomLL = new FOMLayerLayerType[dlayer.Length];


		// ****  RCichota, Jun, 2014 change how RootFraction (rlvp) is used in here ****************************************
		// root senesced are returned to soil (as FOM) considering return is proportional to root mass

		double dAmtLayer = 0.0; //amount of root litter in a layer
		double dNLayer = 0.0;
		for (int i = 0; i < dlayer.Length; i++)
		{
			dAmtLayer = rootSen * RootFraction[i];
			dNLayer = NinRootSen * RootFraction[i];
			
			float amt = (float)dAmtLayer;

			FOMType fom = new FOMType();
			fom.amount = amt;
			fom.N = (float)dNLayer; // 0.03F * amt;    // N in dead root
			fom.C = 0.40F * amt;    //40% of OM is C. Actually, 'C' is not used, as shown in DataTypes.xml
			fom.P = 0;              //to consider later
			fom.AshAlk = 0;         //to consider later

			FOMLayerLayerType Layer = new FOMLayerLayerType();
			Layer.FOM = fom;
			Layer.CNR = 0;       //not used
			Layer.LabileP = 0;   //not used

			fomLL[i] = Layer;
		}

		FOMLayerType FomLayer = new FOMLayerType();
		FomLayer.Type = "agpasture";
		FomLayer.Layer = fomLL;
		IncorpFOM.Invoke(FomLayer);
	}

    #endregion Funcitons

	#region "Utilities"

    /// <summary>
	/// The following helper functions [VDP and svp] are for calculating Fvdp
	/// </summary>
	/// <returns></returns>
	private double VPD()
	{
		double VPDmint = svp(MetData.mint) - MetData.vp;
		VPDmint = Math.Max(VPDmint, 0.0);

		double VPDmaxt = svp(MetData.maxt) - MetData.vp;
		VPDmaxt = Math.Max(VPDmaxt, 0.0);

		double vdp = SVPfrac * VPDmaxt + (1 - SVPfrac) * VPDmint;
		return vdp;
	}
	private double svp(double temp)  // from Growth.for documented in MicroMet
	{
		return 6.1078 * Math.Exp(17.269 * temp / (237.3 + temp));
	}

	#endregion

}

//------------------------------------------------------------------------------
public class LinearInterpolation
{
	[Param]
	public string[] XYs;

	public double[] X;
	public double[] Y;

	[EventHandler]
	public void OnInitialised()
	{

		X = new double[XYs.Length];
		Y = new double[XYs.Length];
		for (int i = 0; i < XYs.Length; i++)
		{
			string[] XYBits = XYs[i].Split(",".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
			if (XYBits.Length != 2)
				throw new Exception("Invalid XY coordinate for function. Value: " + XYs[i]);
			X[i] = Convert.ToDouble(XYBits[0]);
			Y[i] = Convert.ToDouble(XYBits[1]);
		}
	}
	public double Value(double dX)
	{
		bool DidInterpolate = false;
		return MathUtility.LinearInterpReal(dX, X, Y, out DidInterpolate);
	}
}
