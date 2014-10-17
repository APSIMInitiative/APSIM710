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
	private int Debug_Level = 0;
	private Species[] SP;
	private Species[] pSP;

	#region Parameters for initialisation

	//component constant
	private const float SVPfrac = 0.66F;
	private NewMetType MetData = new NewMetType();  // Daily Met Data

	//parameters whose initial values are taken from the .xml
	[Param]
	[Description("Number of species to simulate")]
	[Units("")]
	private int Nspecies = 0;
	[Param]
	[Description("Name of the sward mix")]
	[Units("")]
	private string thisCropName = "";
	[Param]
	[Description("Plant type for micromet")]
	[Units("")]
	private string[] micrometType = null;
	[Param]
	[Description("Actual name of each species")]
	[Units("")]
	private string[] speciesName = null;
	[Param]
	[Description("Species type (1=annual,0=perennial)")]
	[Units("0/1")]
	private double[] isAnnual = null;
	[Param]
	[Description("Whether the species is legume (1=yes, 0=no)")]
	[Units("0/1")]
	private double[] isLegume = null;
	[Param]
	[Description("Photosynthesis pathway (C3 or C4)")]
	[Units("3/4")]
	private double[] photoPath = null;
	[Param]
	[Description("Earliest day of emergence (for annuals only)")]
	[Units("")]
	private double[] dayEmerg;
	[Param]
	[Description("Earliest month of emergence (for annuals only)")]
	[Units("")]
	private double[] monEmerg;
	[Param]
	[Description("Earliest day of anthesis (for annuals only)")]
	[Units("")]
	private double[] dayAnth;
	[Param]
	[Description("Earliest month of anthesis (for annuals only)")]
	[Units("")]
	private double[] monAnth;
	[Param]
	[Description("Days from anthesis to maturity (for annuals only)")]
	[Units("")]
	private double[] daysToMature;

	[Param]
	[Description("Daily root growth")]
	[Units("(mm)")]
	private double[] dRootDepth;
	[Param]
	[Description("Maximum root depth")]
	[Units("(mm)")]
	private double[] maxRootDepth;


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

	[Param]
	[Description("Root function type, 0=default_1=Ritchie_2=power_law_3=proportional_depth")]
	[Units("")]
	private double[] rootFnType;

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

	private double p_ExpoLinearDepthParam;
	[Param]
	[Output]
	[Description("Fraction of root depth where its proportion starts to decrease")]
	[Units("0-1")]
	public double ExpoLinearDepthParam
	{
		get { return p_ExpoLinearDepthParam; }
		set
		{
			p_ExpoLinearDepthParam = value;
			if (p_ExpoLinearDepthParam == 1.0)
				p_RootDistributionMethod = 0;	// effectivelly it defines a homogeneous distribution
		}
	}

	private double p_ExpoLinearCurveParam;
	[Param]
	[Output]
	[Description("Exponent to determine mass distribution in the soil profile")]
	[Units("")]
	public double ExpoLinearCurveParam
	{
		get { return p_ExpoLinearCurveParam; }
		set
		{
			p_ExpoLinearCurveParam = value;
			if (p_ExpoLinearCurveParam == 0.0)
				p_RootDistributionMethod = 0;	// It is impossible to solve, but its limit is a homogeneous distribution
		}
	}

	[Param]
	[Description("Minimum temperature for growth")]
	[Units("")]
	private double[] growthTmin;
	[Param]
	[Description("Maximum temperature for growth")]
	[Units("")]
	private double[] growthTmax;
	[Param]
	[Description("Optimum temperature for growth")]
	[Units("")]
	private double[] growthTopt;
	[Param]
	[Description("Coefficient q on temperature function for plant growth")]
	[Units("")]
	private double[] growthTq;

	[Param]
	[Description("Mass flux minimum temperature")]
	[Units("")]
	private double[] massFluxTmin;
	[Param]
	[Description("Mass flux optimum temperature")]
	[Units("")]
	private double[] massFluxTopt;
	[Param(MinVal=1.0)]
	[Description("Mass flux scale factor at GLFwater=0.0")]
	[Units("")]
	private double[] massFluxW0;
	[Param]
	[Description("Mass flux optimum GLFwater=0.5")]
	[Units("")]
	private double[] massFluxWopt;

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
	[Description("Reference leaf C assimilation during photosynthesis")]
	[Units("gCO2/m^2/s")]
	private double[] Pm;
	[Param]
	[Description("Maintenance respiration")]
	[Units("%")]
	private double[] maintRespiration;
	[Param]
	[Description("Pgrowth/Pgross")]
	[Units("")]
	private double[] growthEfficiency;
	[Param]
	[Description("Saturated canopy radiation Pnet")]
	[Units("MJ/m^2/day")]
	private double[] satRadn;
	[Param]
	[Description("Light extinction coefficient")]
	[Units("")]
	private double[] lightExtCoeff;
	[Param]
	[Description("Radiation use efficiency")]
	[Units("")]
	private double[] rue;
	[Param]
	[Description("max assimilation rate, at ref. Temp.=20C and daylength=12hrs")]
	[Units("kg/ha/day")]
	private double[] maxAssimiRate;

	private double[] SLA;
	[Param]
	[Description("Leaf area per dry matter weight")]
	[Units("m^2/kgDM")]
	public double[] SpecificLeafArea
	{
		get { return SLA; }
		set
		{
			SLA = new double[value.Length];
			for (int s = 0; s < value.Length; s++)
				SLA[s] = value[s];
		}
	}

	private double[] SRL;   // mm/g = m/kg
	[Param]
	[Description("Root length per dry matter weight")]
	[Units("m/gDM")]
	public double[] SpecificRootLength
	{
		get { return SRL; }
		set
		{
			SRL = new double[value.Length];
			for (int s = 0; s < value.Length; s++)
				SRL[s] = value[s] * 1000;
		}
	}


	[Param]
	[Description("Maximum fraction of biomass allocated to roots")]
	[Units("")]
	private double[] maxRootFraction;
	[Param]
	[Description("Factor for increasing DM allocation to shoot over 'spring' (seasonality in growth allocation)")]
	[Units("0-1")]
	private double[] allocationSeasonF;

    [Param]
    [Description("Flag whether DM allocation (shoot/root) will be adjusted using latitude")]
    [Units("")]
    private string useLatitudeFunction = "no";

    [Param]
    [Description("Parameter of LatitudeFunction, for defining the start of period with high allocation to shoot")]
    [Units("")]
    private double paramALatfunction;

    [Param]
    [Description("Parameter of LatitudeFunction, for defining the maximum duration of high allocation period")]
    [Units("")]
    private double paramBLatfunction;
    [Param]
    [Description("Parameter of LatitudeFunction, for defining the variation of the period of high allocation")]
    [Units("")]
    private double paramCLatfunction;
    [Param]
    [Description("Parameter of LatitudeFunction, for defining of duration of onset phase")]
    [Units("")]
    private double paramDLatfunction;
    [Param]
    [Description("Parameter of LatitudeFunction, for defining the duration of offset phase")]
    [Units("")]
    private double paramELatfunction;



	// this has been removed from params as it is not actually used
	private double[] leafRate;
	[Param]
	[Description("Fraction of new growth allocated to leaf (0-1)")]
	[Units("")]
	private double[] fLeaf;
	[Param]
	[Description("Fraction of new growth allocated to stolon (0-1)")]
	[Units("")]
	private double[] fStolon;

	[Param]
	[Description("Senescence rate for shoot (live to dead material)")]
	[Units("")]
	private double[] rateLive2Dead;
	[Param]
	[Description("Littering rate (dead to litter)")]
	[Units("")]
	private double[] rateDead2Litter;
	[Param]
	[Description("Senescence rate for roots (live to soil FOM)")]
	[Units("")]
	private double[] rateRootSen;
	[Param]
	[Description("Parameter for stock influence on senescence")]
	[Units("")]
	private double[] stockParameter;

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

	[Param]
	[Description("Shoot dry weight")]
	[Units("kgDM/ha")]
	private double[] dmshoot;  //initial shoot mass (RCichota May 2014, change from dmtotal to dmshoot)

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
	[Description("Generic growth limiting factor")]
	[Units("0-1")]
	private double[] Frgr;

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
	[Input(IsOptional = true)]
	[Description("Base CO2 content in atmosphere")]
	[Units("")]
	private double CO2ambient = 380; //expected to be updated from MET
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

	[Input(IsOptional = true)]
	[Description("Actual CO2, updated from met and ClimateControl")]
	[Units("")]
	private double co2 = 380;

	[Link]
	private LinearInterpolation FVPDFunction = null;    //Senescence rate is affected by min(gf-N, gf_water)
	[Link]
	private LinearInterpolation HeightMassFN = null;

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
	[Description("Effects of water uptake/demand on plant growth")]
	[Units("0-1")]
	private double[] waterStressFactor;
	[Param]
	[Description("Soil moisture saturation effects on growth")]
	[Units("0-1")]
	private double[] soilSatFactor;
	[Param]
	[Description("Whether water uptake is calculated by agpasture or apsim")]
	[Units("calc/apsim")]
	public string WaterUptakeSource = "calc";
	[Param]
	[Description("Whether N uptake is calculated by agpasture or apsim")]
	[Units("calc/apsim")]
	public string NUptakeSource = "calc";

	[Param(IsOptional = true)]
	[Description("Whether the alternative N uptake routine is to be used")]
	[Units("yes/no")]
	private string alt_N_uptake = "no";

	[Param]
	[Description("Method used to partition biomass removal between plant parts")]
	public string BiomassRemovalMethod;
	[Param]
	[Description("Weight factor defining the preference level for green DM")]
	private double[] PreferenceForGreenDM;
	[Param]
	[Description("Weight factor defining the preference level for dead DM")]
	private double[] PreferenceForDeadDM;

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

	#endregion

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

	//Events
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

	//temporary testing, will be removed later when IL1 can be get from micromet
	private int canopiesNum = 1;            //number of canpy including this one
	private double[] canopiesRadn = null;   //Radn intercepted by canopies

	//----------------------------------------------------------------
	/// <summary>
	/// Initialise parameters
	/// </summary>
	private void InitParameters()
	{
		//Rainss 20110711 - Introduced broken stick root distribution
		// This hack uses rlvp as interface to pass distribution parameters and then recalc rlvp
		// This is not what I would call good pratice due to the parameters technically changing meaning during
		// initilisation, but it will do in the interim.
		if (rlvp.Length == Nspecies)
		{
			p_RootDistributionMethod = 2;
			p_ExpoLinearDepthParam = rlvp[0];
			// This has been maintained for backwards compatibility, use should be avoided

		}

		// rlvp is used as input only, in the calculations it has been usper-seeded by RootFraction (the proportion of roots mass in each layer)
		// The RootFraction should add up to 1.0 over the soil profile
		RootFraction = RootProfileDistribution();

		// check that initialisation fractions have been supplied accordingly
		Array.Resize(ref initialDMFractions_grass, 11);
		Array.Resize(ref initialDMFractions_legume, 11);

		//Create and initialise each species
		SP = new Species[Nspecies];         //species of the pasture
		pSP = new Species[Nspecies];        //For storing species status at previous day
		for (int s = 0; s < Nspecies; s++)
		{
			SP[s] = new Species();
			pSP[s] = new Species();
			InitSpeciesValues(s);
		}

		FractionToHarvest = new double[Nspecies];

		//Initialising the aggregated pasture parameters from initial valuses of each species
		p_rootFrontier = 0.0;
		p_rootMass = 0.0;
		double sum_fShoot = 0.0;
		double sum_lightExtCoeff = 0.0;

		for (int s = 0; s < Nspecies; s++)
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
	//----------------------------------------------------------
	/// <summary>
	/// Set parameter valuse that each species need to know
	/// - from pasture to species
	/// </summary>
	/// <param name="s"></param>
	private void InitSpeciesValues(int s)
	{
		Species.CO2ambient = CO2ambient;
		Species.thisCropName = thisCropName;

		SP[s].speciesName = speciesName[s];
		SP[s].micrometType = micrometType[s];

		if (isAnnual[s] == 1) SP[s].isAnnual = true;
		else SP[s].isAnnual = false;

		if (isLegume[s] == 1) SP[s].isLegume = true;
		else SP[s].isLegume = false;
		SP[s].photoPath = (int)photoPath[s];
		SP[s].dayEmerg = (int)dayEmerg[s];
		SP[s].monEmerg = (int)monEmerg[s];
		SP[s].dayAnth = (int)dayAnth[s];
		SP[s].monAnth = (int)monAnth[s];
		SP[s].daysToMature = (int)daysToMature[s];

		if (SP[s].isAnnual) //calulate days from Emg to Antheis
			SP[s].CalcDaysEmgToAnth();

		//** SP[s].cropFactor = cropFactor[s];
		//** SP[s].maxResidCover = maxResidCover[s];
		SP[s].dRootDepth = (int)dRootDepth[s];
		SP[s].maxRootDepth = (int)maxRootDepth[s];
		SP[s].allocationSeasonF = allocationSeasonF[s];
        SP[s].usingLatFunctionFShoot = (useLatitudeFunction == "no" ? false : true);
        SP[s].ParamALatFunction = paramALatfunction;
        SP[s].ParamBLatFunction = paramBLatfunction;
        SP[s].ParamCLatFunction = paramCLatfunction;
        SP[s].ParamDLatFunction = paramDLatfunction;
        SP[s].ParamELatFunction = paramELatfunction;


		SP[s].NdilutCoeff = NdilutCoeff[s];
		SP[s].rootDepth = (int)rootDepth[s];
		//**SP[s].rootFnType = (int)rootFnType[s];
		SP[s].growthTmin = growthTmin[s];
		SP[s].growthTmax = growthTmax[s];
		SP[s].growthTopt = growthTopt[s];
		SP[s].growthTq = growthTq[s];
		SP[s].massFluxTmin = massFluxTmin[s];
		SP[s].massFluxTopt = massFluxTopt[s];
		SP[s].massFluxW0 = massFluxW0[s];
		SP[s].massFluxWopt = massFluxWopt[s];
		SP[s].heatOnsetT = heatOnsetT[s];            //onset tempeature for heat effects
		SP[s].heatFullT = heatFullT[s];            //full temperature for heat effects
		SP[s].heatSumT = heatSumT[s];                //temperature sum for recovery - sum of (25-mean)
		SP[s].coldOnsetT = coldOnsetT[s];           //onset tempeature for cold effects
		SP[s].coldFullT = coldFullT[s];            //full tempeature for cold effects
		SP[s].coldSumT = coldSumT[s];                //temperature sum for recovery - sum of means
		SP[s].Pm = Pm[s];                            //reference leaf co2 g/m^2/s maximum
		SP[s].maintRespiration = maintRespiration[s];    //in %
		SP[s].growthEfficiency = growthEfficiency[s];
		SP[s].SLA = SLA[s];
		SP[s].lightExtCoeff = lightExtCoeff[s];
		SP[s].lightExtCoeff_ref = lightExtCoeff[s];
		SP[s].rue = rue[s];
		SP[s].maxAssimiRate = maxAssimiRate[s];
		SP[s].rateLive2Dead = rateLive2Dead[s];
		SP[s].rateDead2Litter = rateDead2Litter[s];
		SP[s].rateRootSen = rateRootSen[s];
		SP[s].stockParameter = stockParameter[s];
		SP[s].maxSRratio = (1 - maxRootFraction[s]) / maxRootFraction[s]; // The input is actually the max % allocated to roots
		SP[s].leafRate = 0.0;
		//SP[s].leafRate = leafRate[s];  set to zero as it is not actually used
		SP[s].fLeaf = fLeaf[s];
		SP[s].fStolon = fStolon[s];
		SP[s].digestLive = digestLive[s];
		SP[s].digestDead = digestDead[s];

		SP[s].leafPref = 1;
		if (SP[s].isLegume) SP[s].leafPref = 1.5;        //Init DM (is partitioned to different pools)

		if (dmtotal[s] >= 0.0)
		{ // a value for dmtotal was supplied, assume that it should overwrite dmshoot (needed for back-compatibility-RCichota, May2014)
			dmshoot[s] = dmtotal[s];
		}

		SP[s].dmtotal = dmshoot[s];
		SP[s].dmshoot = dmshoot[s];
		if (dmshoot[s] == 0.0) SP[s].phenoStage = 0;
		else SP[s].phenoStage = 1;

		if (!SP[s].isLegume)
		{
			SP[s].dmleaf1 = dmshoot[s] * initialDMFractions_grass[0];
			SP[s].dmleaf2 = dmshoot[s] * initialDMFractions_grass[1];
			SP[s].dmleaf3 = dmshoot[s] * initialDMFractions_grass[2];
			SP[s].dmleaf4 = dmshoot[s] * initialDMFractions_grass[3];
			SP[s].dmstem1 = dmshoot[s] * initialDMFractions_grass[4];
			SP[s].dmstem2 = dmshoot[s] * initialDMFractions_grass[5];
			SP[s].dmstem3 = dmshoot[s] * initialDMFractions_grass[6];
			SP[s].dmstem4 = dmshoot[s] * initialDMFractions_grass[7];
			SP[s].dmstol1 = SP[s].dmstol2 = SP[s].dmstol3 = 0;
		}
		else //legume
		{
			SP[s].dmleaf1 = dmshoot[s] * initialDMFractions_legume[0];
			SP[s].dmleaf2 = dmshoot[s] * initialDMFractions_legume[1];
			SP[s].dmleaf3 = dmshoot[s] * initialDMFractions_legume[2];
			SP[s].dmleaf4 = dmshoot[s] * initialDMFractions_legume[3];
			SP[s].dmstem1 = dmshoot[s] * initialDMFractions_legume[4];
			SP[s].dmstem2 = dmshoot[s] * initialDMFractions_legume[5];
			SP[s].dmstem3 = dmshoot[s] * initialDMFractions_legume[6];
			SP[s].dmstem4 = dmshoot[s] * initialDMFractions_legume[7];
			SP[s].dmstol1 = dmshoot[s] * initialDMFractions_legume[8];
			SP[s].dmstol2 = dmshoot[s] * initialDMFractions_legume[9];
			SP[s].dmstol3 = dmshoot[s] * initialDMFractions_legume[10];
		}

		if (dmroot[s] >= 0)
			SP[s].dmroot = dmroot[s];
		else
			SP[s].dmroot = dmshoot[s] / SP[s].maxSRratio; 
		SP[s].dmgreenmin = dmgreenmin[s];
		SP[s].dmdeadmin = dmdeadmin[s];

		SP[s].Frgr = (float)Frgr[s];

		//CO2
		SP[s].CO2PmaxScale = CO2PmaxScale[s];
		SP[s].CO2NScale = CO2NScale[s];
		SP[s].CO2NMin = CO2NMin[s];
		SP[s].CO2NCurvature = CO2NCurvature[s];

		SP[s].waterStressFactor = waterStressFactor[s];
		SP[s].soilSatFactor = soilSatFactor[s];

		//init N
		// double Fn = =SP[s].NCO2Effects() //Delay teh [co2] effect to calculating N demand.
		SP[s].NcstemFr = RelativeNconc_Stems[s];      //stem Nc as % of leaf Nc
		SP[s].NcstolFr = RelativeNconc_Stolons[s];      //stol Nc as % of leaf Nc
		SP[s].NcrootFr = RelativeNconc_Roots[s];      //root Nc as % of leaf Nc

		SP[s].NcRel2 = RelativeNconc_stage2[s];
		SP[s].NcRel3 = RelativeNconc_stage3[s];

		//0.01 is for conversion of % to fraction [i.e., 4% ->0.04]
		SP[s].NcleafOpt = 0.01 * NconcOptimum_leaves[s];                  //leaf critical N %)
		SP[s].NcstemOpt = SP[s].NcleafOpt * SP[s].NcstemFr;     //stem
		SP[s].NcstolOpt = SP[s].NcleafOpt * SP[s].NcstolFr;     //stolon
		SP[s].NcrootOpt = SP[s].NcleafOpt * SP[s].NcrootFr;     //root

		SP[s].NcleafMax = 0.01 * NconcMaximum_leaves[s];          // NcLeafMax[s] TO INPUT
		SP[s].NcstemMax = SP[s].NcleafMax * SP[s].NcstemFr; //sheath and stem
		SP[s].NcstolMax = SP[s].NcleafMax * SP[s].NcstolFr;    //stolon
		SP[s].NcrootMax = SP[s].NcleafMax * SP[s].NcrootFr;    //root

		SP[s].NcleafMin = 0.01 * NconcMinimum_leaves[s];
		SP[s].NcstemMin = SP[s].NcleafMin * SP[s].NcstemFr;
		SP[s].NcstolMin = SP[s].NcleafMin * SP[s].NcstolFr;
		SP[s].NcrootMin = SP[s].NcleafMin * SP[s].NcrootFr;

		//init as optimum
		SP[s].Ncleaf1 = SP[s].NcleafOpt;
		SP[s].Ncleaf2 = SP[s].NcleafOpt * SP[s].NcRel2;
		SP[s].Ncleaf3 = SP[s].NcleafOpt * SP[s].NcRel3;
		SP[s].Ncleaf4 = SP[s].NcleafMin; //this could become much small depending on [N] in green tisssue

		SP[s].Ncstem1 = SP[s].NcstemOpt; //stem [N] is 50% of the leaf [N]
		SP[s].Ncstem2 = SP[s].NcstemOpt * SP[s].NcRel2;
		SP[s].Ncstem3 = SP[s].NcstemOpt * SP[s].NcRel3;
		SP[s].Ncstem4 = SP[s].NcstemMin;

		SP[s].Ncstol1 = SP[s].NcstolOpt;
		SP[s].Ncstol2 = SP[s].NcstolOpt * SP[s].NcRel2;
		SP[s].Ncstol3 = SP[s].NcstolOpt * SP[s].NcRel3;

		SP[s].Ncroot = SP[s].NcrootOpt;

		SP[s].MaxFix = NMaxFix[s];   //N-fix fraction when no soil N available, read in later
		SP[s].MinFix = NMinFix[s];   //N-fix fraction when soil N sufficient

		SP[s].Kappa2 = Kappa2_Remob[s];
		SP[s].Kappa3 = Kappa3_Remob[s];
		SP[s].Kappa4 = Kappa4_Remob[s];

		//Init total N in each pool
		SP[s].Nleaf1 = SP[s].dmleaf1 * SP[s].Ncleaf1; //convert % to fraction [i.e., 4% ->0.02]
		SP[s].Nleaf2 = SP[s].dmleaf2 * SP[s].Ncleaf2;
		SP[s].Nleaf3 = SP[s].dmleaf3 * SP[s].Ncleaf3;
		SP[s].Nleaf4 = SP[s].dmleaf4 * SP[s].Ncleaf4;
		SP[s].Nstem1 = SP[s].dmstem1 * SP[s].Ncstem1;
		SP[s].Nstem2 = SP[s].dmstem2 * SP[s].Ncstem2;
		SP[s].Nstem3 = SP[s].dmstem3 * SP[s].Ncstem3;
		SP[s].Nstem4 = SP[s].dmstem4 * SP[s].Ncstem4;
		SP[s].Nstol1 = SP[s].dmstol1 * SP[s].Ncstol1;
		SP[s].Nstol2 = SP[s].dmstol2 * SP[s].Ncstol2;
		SP[s].Nstol3 = SP[s].dmstol3 * SP[s].Ncstol3;
		SP[s].Nroot = SP[s].dmroot * SP[s].Ncroot;

		//calculated, DM and LAI,  species-specific
		SP[s].updateAggregated();   // agregated properties, such as p_totalLAI

		SP[s].dGrowthPot = 0;       // daily growth potential
		SP[s].dGrowthW = 0;          // daily growth actual
		SP[s].dGrowth = 0;          // daily growth actual
		SP[s].dGrowthRoot = 0;      // daily root growth
		SP[s].fShoot = 1;            // actual fraction of dGrowth allocated to shoot

		SWSupply = new float[dlayer.Length];
		SWUptake = new float[dlayer.Length];
		SNSupply = new float[dlayer.Length];
		SNUptake = new float[dlayer.Length];
	}

	//---------------------------------------------------------------------------
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
		Species.coverRF = coverRF();

		//partition the MetData to species
		double sumRadnIntercept = 0.0;   //Intercepted Fraction of the solar Radn available to a species
		for (int s = 0; s < Nspecies; s++)
		{
			sumRadnIntercept += SP[s].coverGreen;
		}
		//update available Radn for each species at current day
		//IntRadn - the total intecepted radn by whole canopy of mixed species
		for (int s = 0; s < Nspecies; s++)
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

		//testing SNF decline by factor df
		double dFrac = 1.0;
		if (co2 == 475)
		{
			for (int s = 0; s < Nspecies; s++)
			{
				SP[s].MaxFix = 0.5;// dFrac;
				SP[s].MinFix = 0.2;// dFrac;
			}
		}


		return true;
	}

	//---------------------------------------------------------------------------
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

	//--------------------------------------------------------------------------
	/// <summary>
	/// Set drought stress factor to each species
	/// Worth more efforts in this area
	/// </summary>
	private void SetSpeciesLimitingFactors()
	{

		if (p_waterDemand == 0)
		{
			p_gfwater = 1.0;
			for (int s = 0; s < Nspecies; s++)
				SP[s].gfwater = p_gfwater;
			return;                                 //case (1) return
		}
		if (p_waterDemand > 0 && p_waterUptake == 0)
		{
			p_gfwater = 0.0;
			for (int s = 0; s < Nspecies; s++)
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
				for (int s = 0; s < Nspecies; s++)
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
		for (int s = 0; s < Nspecies; s++)
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


	//--------------------------------------------------------------------------
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

		for (int s = 0; s < Nspecies; s++)
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
		for (int s = 0; s < Nspecies; s++)
			SP[s].pS.dmdefoliated = 0.0;

	}


	//==============================

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
		for (int s = 0; s < Nspecies; s++)
		{
			double prop = 1.0 / Nspecies;
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
	//======================================================================

	#region "EventHandlers"
	/// <summary>
	/// Eventhandeler - initialisation
	/// </summary>
	[EventHandler]
	public void OnInit2() //overrides Sub init2()
	{
		InitParameters();            // Init parameters after reading the data

		SetSpeciesMetData();         // This is needed for the first day after knowing the number of species

		DoNewCropEvent();            // Tell other modules that I exist
		DoNewCanopyEvent();          // Tell other modules about my canopy
		DoNewPotentialGrowthEvent(); // Tell other modules about my current growth status

		alt_N_uptake = alt_N_uptake.ToLower();
		if (alt_N_uptake == "yes")
			if (Nspecies > 1)
				throw new Exception("When working with multiple species, 'ValsMode' must ALWAYS be 'none'");

		// write some basic initialisation info
		writeSummary();

	}


	//---------------------------------------------------------------------
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
		for (int s = 0; s < Nspecies; s++)
			SP[s].DailyRefresh();

		// clear FractionHarvest by assigning new
		FractionToHarvest = new double[Nspecies];


		DoNewCanopyEvent();
		DoNewPotentialGrowthEvent();

	}

	//---------------------------------------------------------------------
	/// <summary>
	/// Get new meteo- data
	/// </summary>
	/// <param name="NewMetData"></param>
	[EventHandler]
	public void OnNewMet(NewMetType NewMetData)
	{
		MetData = NewMetData;
	}

	//---------------------------------------------------------------------
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

	//---------------------------------------------------------------------
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


	//---------------------------------------------------------------------
	[EventHandler]
	public void OnProcess()
	{
		if (!p_Live)
			return;

		//**Remember last status, and update root depth frontier (root depth mainly for annuals)
		for (int s = 0; s < Nspecies; s++)
		{
			pSP[s] = SP[s];       //Species state yesterday is rememberd
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
		for (int s = 0; s < Nspecies; s++)
		{
			anyEmerged += SP[s].Phenology();
		}

		//**Potential growth
		p_dGrowthPot = 0;
		for (int s = 0; s < Nspecies; s++)
		{
			//p_dGrowthPot += SP[s].DailyGrowthPot();   // alternative way for calclating potential growth
			p_dGrowthPot += SP[s].DailyEMGrowthPot();   //pot here incorporated [N] effects
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
		for (int s = 0; s < Nspecies; s++)
		{
			p_dGrowthW += SP[s].DailyGrowthW();
		}
		double nuptake = NBudgetAndUptake();

		//**actual daily growth
		p_dGrowth = 0;
		for (int s = 0; s < Nspecies; s++)
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


	//----------------------------------------------------------------------
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

		for (int s = 0; s < Nspecies; s++)     // for accumulating the total DM & N removal of species from verious pools
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
					for (int s = 0; s < Nspecies; s++)           //for each species
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
					for (int s = 0; s < Nspecies; s++)
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
					for (int s = 0; s < Nspecies; s++)
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
					for (int s = 0; s < Nspecies; s++)
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
		for (int s = 0; s < Nspecies; s++)
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

	//----------------------------------------------------------------------
	public void Harvest(string type, double amount)  //Being called not by Event
	{
		GrazeType GZ = new GrazeType();
		GZ.amount = (float)amount;
		GZ.type = type;
		OnGraze(GZ);
	}

	//----------------------------------------------------------------------
	[EventHandler]
	public void OnGraze(GrazeType GZ)
	{
		if ((!p_Live) || p_totalDM == 0)
			return;

		// get the amount that can potentially be removed
		double AmountRemovable = 0.0;
		for (int s = 0; s < Nspecies; s++)
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
		double[] TempWeights = new double[Nspecies];
		double[] TempAmounts = new double[Nspecies];
		double TempTotal = 0.0;
		if (AmountRequired > 0.0)
		{
			// get the weights for each species, consider preference and available DM
			double TotalPreference =0.0;
			for (int s = 0; s < Nspecies; s++)
				TotalPreference += PreferenceForGreenDM[s] + PreferenceForDeadDM[s];
			for (int s = 0; s < Nspecies; s++)
			{
				TempWeights[s] = PreferenceForGreenDM[s] + PreferenceForDeadDM[s];
				TempWeights[s] += (TotalPreference - TempWeights[s]) * (1 - FractionNotRemoved);
				TempAmounts[s] = Math.Max(0.0, SP[s].dmleaf_green + SP[s].dmstem_green - SP[s].dmgreenmin) + Math.Max(0.0, SP[s].dmdead - SP[s].dmdeadmin);
				TempTotal += TempAmounts[s] * TempWeights[s];
			}

			// get the actual amounts to remove for each species
			for (int s = 0; s < Nspecies; s++)
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

	//----------------------------------------------------------
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

	//----------------------------------------------------------------------
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
		for (int s = 0; s < Nspecies; s++)
			SP[s].SetInGermination();

	}

	//----------------------------------------------------------------------
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

	//-----------------------------------------------------------------------
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
		for (int s = 0; s < Nspecies; s++)
			SP[s].ResetZero();

	}
	//-----------------------------------------------------------------------
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

	//-----------------------------------------------------------------------
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
	//-------------------------------------------------------
	// RCichota, Jun 2014: cleaned up and add consideration for remobilisation of luxury N
	private double NBudgetAndUptake()
	{
		//1) Get the total N demand (species by species)
		p_Nfix = 0.0;
		double p_Ndemand = 0.0;
		double p_NdemandOpt = 0.0;
		for (int s = 0; s < Nspecies; s++)
		{
			p_Nfix += SP[s].CalcNdemand();       //minimum N fixation
			p_NdemandOpt += SP[s].NdemandOpt;    //demand for optimum [N]
			p_Ndemand += SP[s].NdemandLux;       //demand for luxury [N]
		}

		//2) Update Nfix of legume species under N stress
		double Nstress = 1.0;
		if (p_Ndemand > 0.0 && (p_Ndemand > p_soilNavailable + p_Nfix))
			Nstress = p_soilNavailable / (p_Ndemand - p_Nfix);

		for (int s = 0; s < Nspecies; s++)
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
		for (int s = 0; s < Nspecies; s++)
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
		for (int s = 0; s < Nspecies; s++)
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

	private double NBudgetAndUptake_original()
	{
		//1)Calculate soil N demand (species by species)
		p_Nfix = 0;
		p_soilNdemand = 0;

		double p_Ndemand = 0;
		double p_NdemandOpt = 0;

		for (int s = 0; s < Nspecies; s++)
		{
			p_Nfix += SP[s].CalcNdemand();      //Also, default SP[s].Nfix is set assuming soil N supply is sufficient
			p_NdemandOpt += SP[s].NdemandOpt;   //demand to optimum [N]
			p_Ndemand += SP[s].NdemandLux;         //for luxury uptake
		}

		//2)Update Nfix of legume species under N stress
		double Nstress = 1.0;
		if (p_Ndemand > 0.0 && (p_Ndemand > p_soilNavailable + p_Nfix))
			Nstress = p_soilNavailable / (p_Ndemand - p_Nfix);

		for (int s = 0; s < Nspecies; s++)          //Pasture N demand
		{
			if (!SP[s].isLegume)
			{
				if (SP[s].NdemandLux <= SP[s].Nremob)
				{
					SP[s].soilNdemand = 0;
					SP[s].remob2NewGrowth = SP[s].NdemandLux;
					SP[s].Nremob -= SP[s].NdemandLux;
				}
				else
				{
					SP[s].soilNdemand = SP[s].NdemandLux - SP[s].Nremob;
					SP[s].remob2NewGrowth = SP[s].Nremob;
					SP[s].Nremob = 0;
				}
			}
			else
			{
				if (Nstress < 0.99)  //more fixation under Nstress
				{
					double newNfix = (SP[s].MaxFix - (SP[s].MaxFix - SP[s].MinFix) * Nstress) * SP[s].NdemandLux;
					double moreNfix = Math.Max(0.0, (newNfix - SP[s].Nfix));
					SP[s].Nfix = newNfix;
					p_Nfix += moreNfix;
				}

				if (SP[s].NdemandLux <= SP[s].Nremob + SP[s].Nfix)
				{
					SP[s].remob2NewGrowth = SP[s].NdemandLux - SP[s].Nfix;
					SP[s].Nremob -= SP[s].remob2NewGrowth;
					SP[s].soilNdemand = 0;
				}
				else
				{
					SP[s].remob2NewGrowth = SP[s].Nremob;
					SP[s].soilNdemand = SP[s].NdemandLux - SP[s].Nfix - SP[s].Nremob;
					SP[s].Nremob = 0;
				}
			}
			p_soilNdemand += SP[s].soilNdemand;
		}

		//3) soil N uptake & N limiation factor
		p_soilNuptake = 0;
		p_gfn = 0;
		for (int s = 0; s < Nspecies; s++)          //Pasture N demand and uptake
		{
			if (Nstress < 0.99)
			{
				//partitioning available N among species according to their N demand%
				if (p_soilNdemand == 0)
				{
					SP[s].soilNuptake = 0;
					SP[s].gfn = 1.0;
					SP[s].newGrowthN = SP[s].remob2NewGrowth + SP[s].Nfix; //Nfix = 0 for non-legume
				}
				else
				{
					SP[s].soilNuptake = p_soilNavailable * SP[s].soilNdemand / p_soilNdemand;
					if (SP[s].NdemandLux == 0)
					{
						SP[s].gfn = 1.0;
						SP[s].newGrowthN = 0;
					}
					else
					{
						if (SP[s].isLegume)
						{
							SP[s].newGrowthN = SP[s].soilNuptake + SP[s].remob2NewGrowth + SP[s].Nfix;
							SP[s].gfn = SP[s].newGrowthN / SP[s].NdemandOpt;
						}
						else
						{
							SP[s].newGrowthN = SP[s].soilNuptake + SP[s].remob2NewGrowth;
							SP[s].gfn = SP[s].newGrowthN / SP[s].NdemandOpt;
						}

						if (SP[s].gfn > 1.0) SP[s].gfn = 1.0;
						if (SP[s].gfn < 0.0) SP[s].gfn = 0.0;
					}
				}
			}
			else
			{
				SP[s].soilNuptake = SP[s].soilNdemand;
				SP[s].gfn = 1.0;
				SP[s].newGrowthN = SP[s].soilNuptake + SP[s].remob2NewGrowth + SP[s].Nfix; //Nfix = 0 for non-legume
			}
			p_soilNuptake += SP[s].soilNuptake;

			if (p_dGrowthW == 0)
			{ p_gfn = 1; }
			else
			{
				p_gfn += SP[s].gfn * SP[s].dGrowthW / p_dGrowthW;   //weighted average of species gfn
			}
		}
		//4) Go and uptake
		double soilNremoved = 0;
		if (NUptakeSource == "calc")
		{
			soilNremoved = SNUptakeProcess();               //N remove from soil
		}
		else
		{
			// N uptake calculated by other modules (e.g., SWIM)
			string msg = "\nInforamtion: AgPasture calculates N uptake. No other approach is available now.";
			msg += "\n             Please specify N uptake source as default \"calc\".";
			Console.WriteLine(msg);

		}

		return soilNremoved;


	}


	#endregion //Eventhandlers
	//--------------------------------------------------------------------------------------

	//==================================================================

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
            string[] result = new string[Nspecies];
            for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
            for (int s = 0; s < Nspecies; s++)
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
            for (int s = 0; s < Nspecies; s++)
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
            for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
				for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
            for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			double[] rlv = new double[dlayer.Length];
			//double p_srl = 75000;           // specific root length (mm root/g root)
			//Compute the root length, total over the whole profile
			double Total_Rlength = p_rootMass * SRL[0];   // m root/ha
			Total_Rlength *= 0.0000001;  // convert into mm root/mm2 soil)
			for (int layer = 0; layer < rlv.Length; layer++)
			{
				rlv[layer] = RootFraction[layer] * Total_Rlength /dlayer[layer];    // mm root/mm3 soil
			}
			return rlv;
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
			for (int s = 0; s < Nspecies; s++)
			{
				double prop = 1.0 / Nspecies;
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
            double[] result = new double[Nspecies];
            for (int s = 0; s < Nspecies; s++)
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
            double[] result = new double[Nspecies];
            for (int s = 0; s < Nspecies; s++)
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
            double[] result = new double[Nspecies];
            for (int s = 0; s < Nspecies; s++)
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
			double[] result = new double[Nspecies];
			for (int s = 0; s < Nspecies; s++)
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
			double[] result = new double[Nspecies];
			for (int s = 0; s < Nspecies; s++)
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
			double[] result = new double[Nspecies];
			for (int s = 0; s < Nspecies; s++)
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
			double[] result = new double[Nspecies];
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
            for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
            for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
				result[s] = SP[s].Ndefoliated;
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
			for (int s = 0; s < Nspecies; s++)
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
            for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
            for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
			for (int s = 0; s < Nspecies; s++)
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
            for (int s = 0; s < Nspecies; s++)
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
            double[] result = new double[Nspecies];
            for (int s = 0; s < Nspecies; s++)
                result[s] = SP[s].fShoot;
            return result;
        }
    }
	#endregion
	//=================================================================


	//=================================================================
	#region "Functions"

	//===============================================
	/// <summary>
	/// water uptake processes ...
	/// Rainss Notes 20010707
	///  - Should this be done per species? Is using the root frontier an acceptable solution?
	///  - Plant2 breaks this into two parts: WaterSupply and DoWaterUptake
	///
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
					double DepthFirstStage = p_rootFrontier * p_ExpoLinearDepthParam;
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
							if ((p_ExpoLinearDepthParam < 1.0) && (p_ExpoLinearCurveParam > 0.0))
							{
								double thisDepth = Math.Max(0.0, DepthTop - DepthFirstStage);
								double Ftop = (thisDepth - DepthSecondStage) * Math.Pow(1 - thisDepth / DepthSecondStage, p_ExpoLinearCurveParam) / (p_ExpoLinearCurveParam + 1);
								thisDepth = Math.Min(DepthTop + dlayer[layer] - DepthFirstStage, DepthSecondStage);
								double Fbottom = (thisDepth - DepthSecondStage) * Math.Pow(1 - thisDepth / DepthSecondStage, p_ExpoLinearCurveParam) / (p_ExpoLinearCurveParam + 1);
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



	//================================================
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


	//-------------------------------------------------------
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



	//--------------------------------------------------------------------------------
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


	/// <summary>
	///  Temporary for estimating IL reduction factor when considering other possible canopies.
	///  Only useful for species reporting to micromet as separate pasture canopies and having
	///  similar growth forms (height).
	///  Need to be cautious when using it, and this be removed we may get IL of a species
	///  from multiple canopies (of separate species) in other module
	/// </summary>
	/// <returns></returns>
	public double coverRF()
	{
		if (canopiesNum == 1)
			return 1;

		double totR = 0;
		for (int i = 0; i < canopiesNum; i++)
			totR += canopiesRadn[i];

		double sumCov = 0;
		double sumLAI = 0;
		for (int i = 0; i < canopiesNum; i++)
		{
			double cover = Cover_green * canopiesRadn[i] / IntRadn;
			sumCov += cover;
			sumLAI += -Math.Log(1 - cover) / p_lightExtCoeff;

		}
		double totCov = 1 - Math.Exp(-p_lightExtCoeff * sumLAI);
		return totCov / sumCov;
	}
	#endregion //Funcitons


	#region "Utilities"
	//-----------------------------------------------------------------
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

	#endregion //Utility

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

//================================================================================
// One species
//
//================================================================================
public class Species
{

	public DMPools pS = new DMPools();              //for remember the status of previous day
	//constants
	const double CD2C = 12.0 / 44.0;    //convert CO2 into C
	const double C2DM = 2.5;            //C to DM convertion
	const double DM2C = 0.4;            //DM to C converion
	const double N2Protein = 6.25;      //this is for plants... (higher amino acids)
	const double C2N_protein = 3.5;     //C:N in remobilised material
	//const double growthTref = 20.0;      //reference temperature

	//static variables for common parameters among species
	public static NewMetType MetData = new NewMetType();    //climate data applied to all species
	public static double latitude;
	public static double dayLength;                         //day length
	public static double CO2 = 380;                         //CO2 concentration
	public static double CO2ambient = 380;                  //ambient CO2 concentration
	public static int day_of_month;
	public static int month;
	public static int year;
	public static double PIntRadn;                          //total Radn intecepted by pasture
	public static double PCoverGreen;
	public static double PLightExtCoeff;                    //k of mixed pasture
	public static string thisCropName;
	public static double Pdmshoot;


	public double intRadnFrac;     //fraction of Radn intercepted by this species = intRadn/Radn
	public double intRadn;         //Intercepted Radn by this species

	public string speciesName;
	public string micrometType;

	public bool isAnnual;        //Species type (1=annual,0=perennial)
	public bool isLegume;        //Legume (0=no,1=yes)
	public int photoPath;       //Phtosynthesis pathways: 3=C3, 4=C4; //no consideration for CAM(=3)

	//annual species parameters
	public int dayEmerg;         //Earlist day of emergence (for annuals only)
	public int monEmerg;        //Earlist month of emergence (for annuals only)
	public int dayAnth;            //Earlist day of anthesis (for annuals only)
	public int monAnth;            //Earlist month of anthesis (for annuals only)
	public int daysToMature;    //Days from anthesis to maturity (for annuals only)
	public int daysEmgToAnth;   //Days from emergence to Anthesis (calculated, annual only)
	public int phenoStage = 1;  //pheno stages: 0 - pre_emergence, 1 - vegetative, 2 - reproductive
	public double phenoFactor = 1;
	public int daysfromEmergence = 0;   //days
	public int daysfromAnthesis = 0;    //days

	private bool bSown = false;
	private double DDSfromSowing = 0;
	private double DDSfromEmergence = 0;
	private double DDSfromAnthesis = 0;

	//**public double cropFactor;    //Crop Factor
	//**public double maxResidCover;//Maximum Residue Cover (0-1) (added to ccov to define cover)
	public int dRootDepth;        //Daily root growth (mm)
	public int maxRootDepth;    //Maximum root depth (mm)
	public double NdilutCoeff;
	public int rootDepth;       //current root depth (mm)
	//**public int rootFnType;        //Root function 0=default 1=Ritchie 2=power_law 3=proportional_depth

    public double allocationSeasonF; //factor for different biomass allocation among seasons
    internal bool usingLatFunctionFShoot = false;
    internal double ParamALatFunction = 6.4;
    internal double ParamBLatFunction = 245;
    internal double ParamCLatFunction = 148;
    internal double ParamDLatFunction = 0.5;
    internal double ParamELatFunction = 0.5;


	public double growthTmin;   //Minimum temperature (grtmin) - originally 0
	public double growthTmax;   //Maximum temperature (grtmax) - originally 30
	public double growthTopt;   //Optimum temperature (grtopt) - originally 20
	public double growthTq;        //Temperature n (grtemn) --fyl: q curvature coefficient, 1.5 for c3 & 2 for c4 in IJ

	public double heatOnsetT;            //onset tempeature for heat effects
	public double heatFullT;            //full temperature for heat effects
	public double heatSumT;            //temperature sum for recovery - sum of (25-mean)
	public double coldOnsetT;          //onset tempeature for cold effects
	public double coldFullT;            //full tempeature for cold effects
	public double coldSumT;            //temperature sum for recovery - sum of means
	public double Pm;                    //reference leaf co2 g/m^2/s maximum
	public double maintRespiration;    //in %
	public double growthEfficiency;


	private double highTempEffect = 1;  //fraction of growth rate due to high temp. effect
	private double lowTempEffect = 1;   //fraction of growth rate due to low temp. effect
	private double accumT = 0;          //accumulated temperature from previous heat strike = sum of '25-MeanT'(>0)
	private double accumTLow = 0;       //accumulated temperature from previous cold strike = sum of MeanT (>0)

	public double massFluxTmin;            //grfxt1    Mass flux minimum temperature
	public double massFluxTopt;            //grfxt2    Mass flux optimum temperature
	public double massFluxW0;            //grfw1        Mass flux scale factor at GLFwater=0 (must be > 1)
	public double massFluxWopt;         //grfw2        Mass flux optimum temperature

	//**public double satRadn;         //Saturated canopy radiation Pnet (MJ/m^2/day)
	public double SLA;                //Specific leaf area (m2/kg dwt)
	public double lightExtCoeff;    //Light extinction coefficient
	public double lightExtCoeff_ref;
	public double rue;              //radiaiton use efficiency
	public double maxAssimiRate;    //Maximum Assimulation rate at reference temp & daylength (20C & 12Hrs)
	public double rateLive2Dead;    //Decay coefficient between live and dead
	public double rateDead2Litter;    //Decay coefficient between dead and litter
	public double rateRootSen;      //Decay reference root senescence rate (%/day)
	public double stockParameter;   //Stock influence parameter
	public double maxSRratio;       //Shoot-Root ratio maximum
	public double leafRate;         //reference leaf appearance rate without stress
	public double fLeaf;            //Fixed growth partition to leaf (0-1)
	public double fStolon;            //Fixed growth partition to stolon (0-1)

	public double digestLive;   //Digestibility of live plant material (0-1)
	public double digestDead;   //Digestibility of dead plant material (0-1)

	public double dmleaf1;    //leaf 1 (kg/ha)
	public double dmleaf2;    //leaf 2 (kg/ha)
	public double dmleaf3;    //leaf 3 (kg/ha)
	public double dmleaf4;    //leaf dead (kg/ha)
	public double dmstem1;    //sheath and stem 1 (kg/ha)
	public double dmstem2;    //sheath and stem 2 (kg/ha)
	public double dmstem3;    //sheath and stem 3 (kg/ha)
	public double dmstem4;    //sheath and stem dead (kg/ha)
	public double dmstol1;    //stolon 1 (kg/ha)
	public double dmstol2;    //stolon 2 (kg/ha)
	public double dmstol3;    //stolon 3 (kg/ha)
	public double dmroot;    //root (kg/ha)
	public double dmgreenmin; // minimum grenn dm
	public double dmdeadmin; // minimum dead dm
	public float Frgr;

	//CO2
	public double CO2PmaxScale;
	public double CO2NScale;
	public double CO2NMin;
	public double CO2NCurvature;

	//water
	public double swuptake;
	public double swdemandFrac;
	public double waterStressFactor;
	public double soilSatFactor;

	//Nc - N concentration
	public double NcstemFr;   //stem Nc as % of leaf Nc
	public double NcstolFr;   //stolon Nc as % of leaf Nc
	public double NcrootFr;   //root Nc as % of leaf Nc

	public double NcRel2;     //N concentration in tissue 2 relative to tissue 1
	public double NcRel3;     //N concentration in tissue 3 relative to tissue 1

	//current
	public double Ncleaf1;    //leaf 1  (critical N %)
	public double Ncleaf2;    //leaf 2
	public double Ncleaf3;    //leaf 3
	public double Ncleaf4;    //leaf dead
	public double Ncstem1;    //sheath and stem 1
	public double Ncstem2;    //sheath and stem 2
	public double Ncstem3;    //sheath and stem 3
	public double Ncstem4;    //sheath and stem dead
	public double Ncstol1;    //stolon 1
	public double Ncstol2;    //stolon 2
	public double Ncstol3;    //stolon 3
	public double Ncroot;        //root

	//Max, Min & Opt = critical N
	public double NcleafOpt;    //leaf   (critical N %)
	public double NcstemOpt;    //sheath and stem
	public double NcstolOpt;    //stolon
	public double NcrootOpt;    //root
	public double NcleafMax;    //leaf  (critical N %)
	public double NcstemMax;    //sheath and stem
	public double NcstolMax;    //stolon
	public double NcrootMax;    //root
	public double NcleafMin;
	public double NcstemMin;
	public double NcstolMin;
	public double NcrootMin;
	public double MaxFix;   //N-fix fraction when no soil N available, read in later
	public double MinFix;   //N-fix fraction when soil N sufficient

	//N in each pool (calculated as dm * Nc)
	public double Nleaf1 = 0;    //leaf 1 (kg/ha)
	public double Nleaf2 = 0;    //leaf 2 (kg/ha)
	public double Nleaf3 = 0;    //leaf 3 (kg/ha)
	public double Nleaf4 = 0;    //leaf dead (kg/ha)
	public double Nstem1 = 0;    //sheath and stem 1 (kg/ha)
	public double Nstem2 = 0;    //sheath and stem 2 (kg/ha)
	public double Nstem3 = 0;    //sheath and stem 3 (kg/ha)
	public double Nstem4 = 0;    //sheath and stem dead (kg/ha)
	public double Nstol1 = 0;    //stolon 1 (kg/ha)
	public double Nstol2 = 0;    //stolon 2 (kg/ha)
	public double Nstol3 = 0;    //stolon 3 (kg/ha)
	public double Nroot = 0;    //root (kg/ha)

	//calculated
	//DM
	public double dmtotal;      //=dmgreen + dmdead
	public double dmgreen;
	public double dmdead;
	public double dmleaf;
	public double dmstem;
	public double dmleaf_green;
	public double dmstem_green;
	public double dmstol_green;
	public double dmstol;
	public double dmshoot;

	public double dmdefoliated;
	public double Ndefoliated;
	public double digestHerbage;
	public double digestDefoliated;
	//LAI
	public double greenLAI; //sum of 3 pools
	public double deadLAI;  //pool dmleaf4
	public double totalLAI;
	//N plant
	public double Nshoot;    //above-ground total N (kg/ha)
	public double Nleaf;    //leaf N
	public double Nstem;    //stem N
	public double Ngreen;    //live N
	public double Ndead;    //in standing dead (kg/ha)
	public double Nstolon;    //stolon

	public double NremobMax;  //maximum N remob of the day
	public double Nremob = 0;       //N remobiliesd N during senescing
	public double Cremob = 0;
	public double Nleaf3Remob = 0;
	public double Nstem3Remob = 0;
	public double Nstol3Remob = 0;
	public double NrootRemob = 0;
	public double remob2NewGrowth = 0;
	public double newGrowthN = 0;    //N plant-soil
	public double NdemandLux;      //N demand for new growth, with luxury uptake
	public double NdemandOpt;
	public double Nfix;         //N fixed by legumes

	public double Kappa2 = 0.0;
	public double Kappa3 = 0.0;
	public double Kappa4 = 0.0;
	public double NLuxury2;		       // luxury N (above Nopt) potentially remobilisable
	public double NLuxury3;		       // luxury N (above Nopt)potentially remobilisable
	public double NFastRemob2 = 0.0;   // amount of luxury N remobilised from tissue 2
	public double NFastRemob3 = 0.0;   // amount of luxury N remobilised from tissue 3

	public double soilNAvail;   //N available to this species
	public double soilNdemand;  //N demand from soil (=Ndemand-Nremob-Nfixed)
	public double soilNdemandMax;   //N demand for luxury uptake
	public double soilNuptake;  //N uptake of the day

	//growth limiting factors
	public double gfwater;  //from water stress
	public double gftemp;   //from temperature
	public double gfn;      //from N deficit
	public double Ncfactor;
	public double fNavail2Max; //demand/Luxruy uptake

	//calculated, species delta
	public double dGrowthPot;    //daily growth potential
	public double dGrowthW;      //daily growth with water-deficit incorporated
	public double dGrowth;       //daily growth
	public double dGrowthRoot;   //daily root growth
	public double dGrowthHerbage; //daily growth shoot

	public double dLitter;       //daily litter production
	public double dNLitter;      //N in dLitter
	public double dRootSen;      //daily root sennesce
	public double dNrootSen;     //N in dRootSen

	public double fShoot;         //actual fraction of dGrowth to shoot
	public int dayCounter;
	public double sumGFW;

	// transfer coefficients 
	public double gama = 0.0;	// from tissue 1 to 2, then 3 then 4
	public double gamas = 0.0;	// for stolons
	public double gamad = 0.0;	// from dead to litter
	public double gamar = 0.0;	// for roots (to dead/FOM)

	public double leafPref = 1;    //leaf preference
	// public double accumtotalnewG = 0;
	// public double accumtotalnewN = 0;
	public double IL1;
	public double Pgross;
	public double Resp_m;
	public double Resp_root;
	public static double coverRF = 1;    //temp. for reduce IL considering other canopies

	//Species ------------------------------------------------------------
	public Species()
	{
		//Initialise parameters (for those not set from parameter readings)
		Nremob = 0.0;
		Cremob = 0;
		Nfix = 0.0;
		NdemandLux = 0.0;
		soilNdemand = 0.0;
		soilNuptake = 0.0;
		dmdefoliated = 0.0;
		Ndefoliated = 0;
		digestHerbage = 0;
		digestDefoliated = 0;

	}
	//Species -----------------------
	public void DailyRefresh()
	{
		dmdefoliated = 0.0;
		Ndefoliated = 0.0;
		digestDefoliated = 0.0;
	}

	//Species -----------------------------
	public double RemoveDM(double AmountToRemove, double PrefGreen, double PrefDead)
	{

		// check existing amount and what is harvestable
		double PreRemovalDM = dmshoot;
		double PreRemovalN = Nshoot;
		double AmountRemovable = Math.Max(0.0, dmleaf_green + dmstem_green - dmgreenmin) + Math.Max(0.0, dmleaf4 + dmstem4 - dmdeadmin);

		// get the weights for each pool, consider preference and available DM
		double FractionNotRemoved = 0.0;
		if (AmountRemovable>0)
			FractionNotRemoved = Math.Max(0.0, (AmountRemovable - AmountToRemove) / AmountRemovable);

		double TempPrefGreen = PrefGreen + (PrefDead * (1 - FractionNotRemoved));
		double TempPrefDead = PrefDead + (PrefGreen * (1 - FractionNotRemoved));
		double TempRemovableGreen = Math.Max(0.0,dmleaf_green + dmstem_green - dmgreenmin);
		double TempRemovableDead = Math.Max(0.0,dmleaf4 + dmstem4 - dmdeadmin);

		// get partiton between dead and live materials
		double TempTotal = TempRemovableGreen * TempPrefGreen + TempRemovableDead * TempPrefDead;
		double FractionToHarvestGreen = 0.0;
		double FractionToHarvestDead = 0.0;
		if (TempTotal > 0.0)
		{
			FractionToHarvestGreen = TempRemovableGreen * TempPrefGreen / TempTotal;
			FractionToHarvestDead = TempRemovableDead * TempPrefDead / TempTotal;
		}

		// get amounts removed
		double RemovingGreenDM = AmountToRemove * FractionToHarvestGreen;
		double RemovingDeadDM = AmountToRemove * FractionToHarvestDead;
		// Fraction of DM remaining in the field
		double FractionRemainingGreen = 1.0;
		if (dmleaf_green + dmstem_green > 0.0)
			FractionRemainingGreen -= RemovingGreenDM / (dmleaf_green + dmstem_green);
		double FractionRemainingDead = 1.0;
		if (dmleaf4 + dmstem4 > 0.0)
			FractionRemainingDead -= RemovingDeadDM / (dmleaf4 + dmstem4);
		FractionRemainingGreen = Math.Max(0.0, Math.Min(1.0, FractionRemainingGreen));
		FractionRemainingDead = Math.Max(0.0, Math.Min(1.0, FractionRemainingDead));

		// get digestibility of DM being harvested
		digestDefoliated = calcDigestability();

		// update the various pools
		dmleaf1 = FractionRemainingGreen * dmleaf1;
		dmleaf2 = FractionRemainingGreen * dmleaf2;
		dmleaf3 = FractionRemainingGreen * dmleaf3;
		dmleaf4 = FractionRemainingDead * dmleaf4;
		dmstem1 = FractionRemainingGreen * dmstem1;
		dmstem2 = FractionRemainingGreen * dmstem2;
		dmstem3 = FractionRemainingGreen * dmstem3;
		dmstem4 = FractionRemainingDead * dmstem4;
		//No stolon remove

		// N remove
		Nleaf1 = FractionRemainingGreen * Nleaf1;
		Nleaf2 = FractionRemainingGreen * Nleaf2;
		Nleaf3 = FractionRemainingGreen * Nleaf3;
		Nleaf4 = FractionRemainingDead * Nleaf4;
		Nstem1 = FractionRemainingGreen * Nstem1;
		Nstem2 = FractionRemainingGreen * Nstem2;
		Nstem3 = FractionRemainingGreen * Nstem3;
		Nstem4 = FractionRemainingDead * Nstem4;

		//Nremob is also removed proportionally (not sensitive?)
		double PreRemovalNRemob = Nremob;
		Nremob = FractionRemainingGreen * Nremob;

		// update Luxury N pools
		NLuxury2 *= FractionRemainingGreen;
		NLuxury3 *= FractionRemainingGreen;

		// update variables
		updateAggregated();

		// check balance and set outputs
		double NremobRemove = PreRemovalNRemob - Nremob;
		dmdefoliated = PreRemovalDM - dmshoot;
		pS.dmdefoliated = dmdefoliated;
		Ndefoliated = PreRemovalN - Nshoot;
		if (Math.Abs(dmdefoliated - AmountToRemove) > 0.00001)
			throw new Exception("  AgPasture - removal of DM resulted in loss of mass balance");

		return Ndefoliated;
	}

	//Species ------------------------------------------------------------
	public double calcDigestability()
	{
		if ((dmleaf + dmstem) <= 0)
		{
			digestHerbage = 0;
			return digestHerbage;
		}

		double fSugar = 0.5 * dGrowth / dmgreen;    //dmgreen: live shoots including leaves/stems/stolons
		double CNp = 3.5;                           //CN ratio of protein
		double CNw = 100;                           //CN ratio of cell wall

		//Live
		double digestabilityLive = 0;
		if (dmgreen > 0 & Ngreen > 0)
		{
			double CNlive = 0.4 * dmgreen / Ngreen;                                //CN ratio of live shoots
			double fProteinLive = (CNw / CNlive - (1 - fSugar)) / (CNw / CNp - 1); //Fraction of protein in liveing shoots
			double fWallLive = 1 - fSugar - fProteinLive;                          //Fraction of cell wall in living shoots
			digestabilityLive = fSugar + fProteinLive + digestLive * fWallLive;
		}

		//Dead
		double digestabilityDead = 0;
		double standingDead = dmleaf4 + dmstem4;        //Not including stolons here for stolons are not grazed
		if (standingDead > 0 && Ndead > 0)
		{
			double CNdead = 0.4 * dmdead / Ndead;                       //CN ratio of standing dead;
			double fProteinDead = (CNw / CNdead - 1) / (CNw / CNp - 1); //Fraction of protein in standing dead
			double fWallDead = 1 - fProteinDead;                        //Fraction of cell wall in standing dead
			digestabilityDead = fProteinDead + digestDead * fWallDead;
		}

		double deadFrac = standingDead / (dmleaf + dmstem);
		digestHerbage = (1 - deadFrac) * digestabilityLive + deadFrac * digestabilityDead;

		return digestHerbage;
	}
	//Species ------------------------------------------------------------
	public double updateAggregated()   //update DM, N & LAI
	{
		//DM
		dmleaf = dmleaf1 + dmleaf2 + dmleaf3 + dmleaf4;
		dmstem = dmstem1 + dmstem2 + dmstem3 + dmstem4;
		dmstol = dmstol1 + dmstol2 + dmstol3;
		dmshoot = dmleaf + dmstem + dmstol;

		dmleaf_green = dmleaf1 + dmleaf2 + dmleaf3;
		dmstem_green = dmstem1 + dmstem2 + dmstem3;
		dmstol_green = dmstol1 + dmstol2 + dmstol3;

		dmgreen = dmleaf1 + dmleaf2 + dmleaf3
		+ dmstem1 + dmstem2 + dmstem3
		+ dmstol1 + dmstol2 + dmstol3;

		dmdead = dmleaf4 + dmstem4;
		dmtotal = dmgreen + dmdead;

		//N
		Nleaf = Nleaf1 + Nleaf2 + Nleaf3 + Nleaf4;
		Nstem = Nstem1 + Nstem2 + Nstem3 + Nstem4;// +Nremob;  //separately handled, not reported in stem
		Nstolon = Nstol1 + Nstol2 + Nstol3;

		Nshoot = Nleaf + Nstem + Nstolon;   //shoot

		Ngreen = Nleaf1 + Nleaf2 + Nleaf3
		+ Nstem1 + Nstem2 + Nstem3
		+ Nstol1 + Nstol2 + Nstol3;
		Ndead = Nleaf4 + Nstem4;


		//LAI                                   //0.0001: kg/ha->kg/m2; SLA: m2/kg
		greenLAI = 0.0001 * dmleaf_green * SLA + 0.0001 * dmstol * 0.3 * SLA;   //insensitive? assuming Mass2GLA = 0.3*SLA

		// Resilence after unfovoured conditions
		// Consider cover will be bigger for the same amount of DM when DM is low due to
		// - light extinction coefficient will be bigger - plant leaves will be more plate than in dense high swards
		// - more parts will turn green for photosysntheses?
		// - quick response of plant shoots to fovoured conditions after release of stress
		if (!isLegume && dmgreen < 1000)
		{
			greenLAI += 0.0001 * dmstem_green * SLA * Math.Sqrt((1000 - dmgreen) / 1000);
		}

		deadLAI = 0.0001 * dmleaf4 * SLA;
		totalLAI = greenLAI + deadLAI;

		return totalLAI;

	}

	//Species --------------------------------------------
	public double rootGrowth()
	{
		if (isAnnual)
		{
			rootDepth = 50 + (maxRootDepth - 50) * daysfromEmergence / daysEmgToAnth;
			//considering root distribution change, here?
		}
		return rootDepth;  // no root depth change for pereniel pasture
	}

	//Species -------------------------------------------------
	public int CalcDaysEmgToAnth()
	{
		daysEmgToAnth = 0;
		int numbMonths = monAnth - monEmerg;  //emergence & anthesis in the same calendar year: monEmerg < monAnth
		if (monEmerg >= monAnth)              //...across the calendar year
			numbMonths += 12;

		daysEmgToAnth = (int)(30.5 * numbMonths + (dayAnth - dayEmerg));

		return daysEmgToAnth;
	}

	//Species -------------------------------------------------------------
	public int Phenology()
	{
		const double DDSEmergence = 150;   // to be an input parameter
		double meanT = 0.5 * (MetData.maxt + MetData.mint);

		if (bSown && phenoStage == 0)            //  before emergence
		{
			DDSfromSowing += meanT;
			if (DDSfromSowing > DDSEmergence)
			{
				phenoStage = 1;
				DDSfromSowing = 0;
				SetEmergentState();      //Initial states at 50% emergence

			}
		}

		/*TO DO later
		*      else if (phenoStage == 1)       //  Vege
		{
		DDSfromEmergence += meanT;
		if (DDSfromEmergence > 1000)
		phenoStage = 2;
		}
		else if (phenoStage == 2)       //  Reprod
		{
		DDSfromAnthesis += meanT;
		if (DDSfromEmergence > 1000)
		phenoStage = 3;
		}
		else if (phenoStage == 4)       //  Post_reprod
		{
		DDSfromAnthesis += meanT;
		if (DDSfromEmergence > 1000)
		phenoStage = 1;         // return to vege
		}
		*/
		return phenoStage;
	}

	//Species -------------------------------------------------------------
	private double SetEmergentState()
	{
		dmleaf1 = 10;   //(kg/ha)
		dmleaf2 = 20;
		dmleaf3 = 20;
		dmleaf4 = 0;
		if (!isLegume)
		{
			dmstem1 = 5;
			dmstem2 = 10;
			dmstem3 = 0;
			dmstem4 = 0;
			dmroot = 50;
		}
		else
		{
			dmstol1 = 5;
			dmstol2 = 10;
			dmstol3 = 0;
			dmroot = 25;
		}

		//Init total N in each pool
		Nleaf1 = dmleaf1 * Ncleaf1;
		Nleaf2 = dmleaf2 * Ncleaf2;
		Nleaf3 = dmleaf3 * Ncleaf3;
		Nleaf4 = dmleaf4 * Ncleaf4;
		Nstem1 = dmstem1 * Ncstem1;
		Nstem2 = dmstem2 * Ncstem2;
		Nstem3 = dmstem3 * Ncstem3;
		Nstem4 = dmstem4 * Ncstem4;
		Nstol1 = dmstol1 * Ncstol1;
		Nstol2 = dmstol2 * Ncstol2;
		Nstol3 = dmstol3 * Ncstol3;
		Nroot = dmroot * Ncroot;

		//calculated, DM and LAI,  species-specific
		updateAggregated();   // agregated properties, such as p_totalLAI

		dGrowthPot = 0;       // daily growth potential
		dGrowthW = 0;         // daily growth considering only water deficit
		dGrowth = 0;          // daily growth actual
		dGrowthRoot = 0;      // daily root growth
		fShoot = 1;              // actual fraction of dGrowth allocated to shoot

		return dmtotal;       // total shoot mass

	}

	//Species -------------------------------------------------------------
	public double DailyGrowthPot()   //GrassGro routine. Not used since Aug 09. FYLi
	{
		//*** This process is finally not used, so not updated. Need reexmie it if it is used! FLi Dec 2010)

		//phebology
		if (isAnnual)
		{
			if (month == monEmerg && day_of_month == dayEmerg)
				phenoStage = 1;     //vegetative stage
			else if (month == monAnth && day_of_month == dayAnth)
				phenoStage = 2;     //reproductive

			if (phenoStage == 0)    //before emergence
			{
				dGrowthPot = 0;
				return dGrowthPot;  //no growth
			}

			if (phenoStage == 1)        //vege
			{
				daysfromEmergence++;
			}
			else if (phenoStage == 2)   //repro
			{
				daysfromAnthesis++;
				if (daysfromAnthesis >= daysToMature)
				{
					phenoStage = 0;
					daysfromEmergence = 0;
					daysfromAnthesis = 0;
					dGrowthPot = 0;
					return dGrowthPot;      // no growth after mature
				}
			}
		}

		//RUE //GrassGro routine
		//     radiation use efficiency can be either entered as:
		//     a) defined at a reference solar radiation intensity of 1.67 MJ m-2 h-1
		//     b) defined in terms of maximum assimilation
		//
		//     Typically
		//     a) grxki3 units are 8.0 phalaris, 8.5 subterranean clover  (?refRUE)
		//     b) 280 kg/ha/day maximum assimilation at 20 MJ m-2 radiation and 12 hr daylength
		//
		//     Therefore redefine radiation use efficiency when case (b) entered
		//     as defined by equation 27 to convert kg/ha/day to g/MJ.

		//If the reference maximum assimilation is entered, convert it into rue (g/MJ)
		//  has the input, using GrassGro approach for potential growth
		//  that is, maxAssimiRate == 'maximum assimilation: kg/ha/day' is entered

		double refRadn = 20;
		double refDayL = 12;
		double refRI = refRadn / refDayL;   // 1.67 = 20MJ/12hours: reference solar radition flux intensity (IFstd)
		double riEffect = 0.6;              // Effects of refRI on RUE,    phxki4 = 0.6;

		//maxAssimiRate;                    // kg/ha/day  phxki3 = 330.0; (SGS: 330 prereneial) or 240 annual)
		double refRUE;
		if (maxAssimiRate > 25.0)
		{
			refRUE = 0.1 * maxAssimiRate / refRadn;   //0.1 -- for converting to g/m^2 from kg/ha
		}
		else
		{
			refRUE = maxAssimiRate;
		}

		//To consider: should use MetData.radn or SP.intRadn - depending on the methods in seting up simulation
		double RI = ((refRI + riEffect) / ((MetData.radn / dayLength) + riEffect));
		rue = refRUE * RI; // ((refRI + riEffect) / ((MetData.radn / dayLength) + riEffect));

		//consider a growth efficiency coefficient (as grgeff or sgseff in GRAZPLAN)
		// rue *= growthEfficiency; // Note: 'growthEfficiecy' was 1 in this routine
		//        It is now used as '(Pgross - Prespi)/Pgross' in DailyEMGrowthPot()
		//        FYL - Sep 09

		//This assume no difference in terms of layer distribution
		dGrowthPot = rue * intRadn;                     //intRadn is the Radn intecepted by this species

		double tempStress = HeatEffect() * ColdEffect();            //only one of teh effect is < 1
		dGrowthPot *= Math.Min(tempStress * GFTemperature(), Frgr);    //Temperature effects is considered in potential growth
		//Frgr too, because it is considered for calculating PET
		dGrowthPot *= 10;                                           //=> kg/ha


		dGrowthPot *= PCO2Effects();                    //multiply the CO2 effects


		/* //if RUE is not the net, but gross phs, then maizntenance (respiration) is calculated
		bool bGrossPHS = false;
		if (bGrossPHS)
		{
		//Calculate maintenance respiration (kg C m2/day)
		double Mcoeff = 0.03;   //grcowm = maintenenance coefficient at 20C (default=0.03/day
		double Nlive = 0.1;     //fnlive = nitrogen content of the live shoot and root plant material
		double Nref  = 0.1;     //fnreff = reference palnt nitrogen content (0.1 Kg N/Kg C)
		double rootF = 1.0;     //xnetam = ????
		//fmtm20 = temperature response for for maintenence respiration
		//rm     = grcowm*fmtm20*(fnlive/fnreff)*(sumshoot+(xnetam*dmsrot(ipastr)))

		double Tresp = 0.5*(MetData.maxt+MetData.mint)/20;  //temperature response for maintenance with 20 C as reference
		double Nresp = Nlive/Nref;                          //N response for maintenance with Nref as reference
		double maint = Mcoeff * (dmgreen + rootF*dmroot)* Tresp * Nresp;

		//Calculate scenescent carbonhydrates
		double remob = 0.0;
		//This should be remobilised reserve from root? If so,

		double GLFcrit = 0.2;  // threthold growth-limiting factor for remobilisation for underground reserves
		double rebRate = 0.02; // relative rateDead2Litter of remobilisation (per day)
		if (GLFcrit < Math.Min(gfwater, gftemp))
		{
		remob = rebRate * dmroot;
		}
		//Calculate rate of synethesis of new structural material
		dGrowthPot = dGrowthPot + remob - maint;
		}
		*/

		// phenologically related reduction of annual species (from IJ)
		if (isAnnual)
		{
			double rFactor = 1;  // reduction factor of annual species
			if (phenoStage == 1 && daysfromEmergence < 60)  //decline at the begining due to seed bank effects ???
			{
				rFactor = 0.5 + 0.5 * daysfromEmergence / 60;
			}
			else if (phenoStage == 2)                       //decline of photosynthesis when approaching maturity
			{
				rFactor = 1.0 - (double)daysfromAnthesis / daysToMature;
			}
			dGrowthPot *= rFactor;
		}

		return dGrowthPot;
	}

	//Species ----------------------------------------------------------
	public double DailyEMGrowthPot()
	{
		//annual phebology
		if (isAnnual)
		{
			bool moreGrowth = annualPhenology();
			if (!moreGrowth)
				return dGrowthPot = 0;
		}

		//
		if (phenoStage == 0 || greenLAI == 0) //Before gemination
			return dGrowthPot = 0;

		const double alfa = 0.01;                 //P_al, leaf gross photosynthesis rate: mg co2/J
		const double theta = 0.8;                 //P_th, curvature parameter: J /kg/s

		//following parometers are from input (.xml)
		double maint_coeff = 0.01 * maintRespiration;  //reference maintnance respiration as % of live weight
		double Yg = growthEfficiency;                  //default =0.75; //Efficiency of plant photosynthesis growth)
		//Pm is an input

		//Add temp effects to Pm
		double Tmean = (MetData.maxt + MetData.mint) / 2;
		double Tday = Tmean + 0.5 * (MetData.maxt - Tmean);

		double Pm_mean = Pm * GFTemperature(Tmean) * PCO2Effects() * PmxNeffect();  //Dec10: added CO2 & [N]effects
		double Pm_day = Pm * GFTemperature(Tday) * PCO2Effects() * PmxNeffect();    //Dec10: added CO2 & [N]effects

		double tau = 3600 * dayLength;                //conversion of hour to seconds //  tau := 0.0036 * hours ;
		//IL_1 := k_light * 1.33333 * 0.5 * light/tau;  // flat bit - goes with Pm_day
		//FYL: k_light*light/tau = Irridance intercepted by 1 LAI on 1 m^2 ground: J/(m^2 ground)/s

		//IL:  irridance on the top of canopy, with unit: J/(m^2 LAI)/(m^2 ground)/second.  PAR = 0.5*Radn; 1 MJ = 10^6 J

		//IL1 = 1.33333 * 0.5 * PIntRadn / (PCoverGreen*coverRF) * PLightExtCoeff * 1000000 / tau;
		IL1 = 1.33333 * 0.5 * PIntRadn * PLightExtCoeff * 1000000 / tau;                    //ignore putting 2 species seperately for now
		double IL2 = IL1 / 2;                      //IL for early & late period of a day

		//Photosynthesis per LAI under full irridance at the top of the canopy
		double Pl1 = (0.5 / theta) * (alfa * IL1 + Pm_day
		- Math.Sqrt((alfa * IL1 + Pm_day) * (alfa * IL1 + Pm_day) - 4 * theta * alfa * IL1 * Pm_day));
		double Pl2 = (0.5 / theta) * (alfa * IL2 + Pm_mean
		- Math.Sqrt((alfa * IL2 + Pm_mean) * (alfa * IL2 + Pm_mean) - 4 * theta * alfa * IL2 * Pm_mean));

		//Upscaling from 'per LAI' to 'per ground area'
		double carbon_m2 = 0.000001 * CD2C * 0.5 * tau * (Pl1 + Pl2) * PCoverGreen * intRadnFrac / lightExtCoeff;
		//tau: per second => per day; 0.000001: mg/m^2=> kg/m^2_ground/day;
		//only 'intRadnFrac' portion for this species;
		//using lightExeCoeff (species, result in a lower yield with ample W & N)

		carbon_m2 *= 1;// coverRF;                       //coverRF == 1 when puting species together

		Pgross = 10000 * carbon_m2;                 //10000: 'kg/m^2' =>'kg/ha'

		//Add extreme temperature effects;
		Pgross *= HeatEffect() * ColdEffect();      // in practice only one temp stress factor is < 1

		//Maintenance respiration
		double Teffect = 0;                         //Add temperature effects on respi
		if (Tmean > growthTmin)
		{
			if (Tmean < growthTopt)
			{
				Teffect = GFTemperature(Tmean);
				//Teffect = Math.Pow(Teffect, 1.5);
			}
			else
			{
				//Teffect = 1;
				Teffect = Tmean / growthTopt;        // Using growthTopt (e.g., 20 C) as reference, and set maximum
				if (Teffect > 1.25) Teffect = 1.25;  // Resp_m
			}   //The extreme high temperatue (heat) effect is added separately
		}


		double YgFactor = 1.0;
		//Ignore [N] effects in potential growth here
		Resp_m = maint_coeff * Teffect * PmxNeffect() * (dmgreen + dmroot) * DM2C;       //converting DM to C    (kg/ha)
		//Dec10: added [N] effects here

		// ** C budget is not explicitly done here as in EM
		Cremob = 0;                     // Nremob* C2N_protein;    // No carbon budget here
		// Nu_remob[elC] := C2N_protein * Nu_remob[elN];
		// need to substract CRemob from dm rutnover?
		dGrowthPot = Yg * YgFactor * (Pgross + Cremob - Resp_m);     //Net potential growth (C) of the day (excluding growth respiration)
		dGrowthPot = Math.Max(0.0, dGrowthPot);
		//double Resp_g = Pgross * (1 - Yg) / Yg;
		//dGrowthPot *= PCO2Effects();                      //multiply the CO2 effects. Dec10: This ihas been now incoporated in Pm/leaf area above

		//convert C to DM
		dGrowthPot *= C2DM;

		// phenologically related reduction of annual species (from IJ)
		if (isAnnual)
			dGrowthPot = annualSpeciesReduction();

		return dGrowthPot;

	}

	//Species --------------------------------------------------------------
	// phenology of anuual species
	public bool annualPhenology()
	{
		if (month == monEmerg && day_of_month == dayEmerg)
			phenoStage = 1;         //vegetative stage
		else if (month == monAnth && day_of_month == dayAnth)
			phenoStage = 2;         //reproductive

		if (phenoStage == 0)        //before emergence
		{
			dGrowthPot = 0;
			return false;           //no growth
		}

		if (phenoStage == 1)        //vege
		{
			daysfromEmergence++;
			return true;
		}

		if (phenoStage == 2)
		{
			daysfromAnthesis++;
			if (daysfromAnthesis >= daysToMature)
			{
				phenoStage = 0;
				daysfromEmergence = 0;
				daysfromAnthesis = 0;
				dGrowthPot = 0;
				return false;       // Flag no growth after mature
			}
			return true;
		}
		return true;
	}


	//Species --------------------------------------------------------------
	// phenologically related reduction of annual species
	public double annualSpeciesReduction()
	{
		double rFactor = 1;  // reduction factor of annual species
		if (phenoStage == 1 && daysfromEmergence < 60)  //decline at the begining due to seed bank effects ???
		{
			rFactor = 0.5 + 0.5 * daysfromEmergence / 60;
		}
		else if (phenoStage == 2)                       //decline of photosynthesis when approaching maturity
		{
			rFactor = 1.0 - (double)daysfromAnthesis / daysToMature;
		}
		dGrowthPot *= rFactor;
		return dGrowthPot;
	}



	//Species --------------------------------------------------------------
	//Plant photosynthesis increase to eleveated [CO2]
	public double PCO2Effects()
	{
		if (Math.Abs(CO2 - CO2ambient) < 0.5)
			return 1.0;

		double Kp = CO2PmaxScale; //700; for C3 plants & 150 for C4
		if (photoPath == 4)     //C4 plants
			Kp = 150;

		double Fp = (CO2 / (Kp + CO2)) * ((CO2ambient + Kp) / CO2ambient);
		return Fp;
	}

	//Species --------------------------------------------------------------
	// Plant nitrogen [N] decline to elevated [CO2]
	public double NCO2Effects()
	{
		if (Math.Abs(CO2 - CO2ambient) < 0.5)
			return 1.0;

		double L = CO2NMin;         // 0.7 - lamda: same for C3 & C4 plants
		double Kn = CO2NScale;      // 600 - ppm,   when CO2 = 600ppm, Fn = 0.5*(1+lamda);
		double Qn = CO2NCurvature;  //2 - curveture factor

		double interm = Math.Pow((Kn - CO2ambient), Qn);
		double Fn = (L + (1 - L) * interm / (interm + Math.Pow((CO2 - CO2ambient), Qn)));
		return Fn;
	}

	//Species --------------------------------------------------------------
	//Canopy conductiance decline to elevated [CO2]
	public double ConductanceCO2Effects()
	{
		if (Math.Abs(CO2 - CO2ambient) < 0.5)
			return 1.0;
		//Hard coded here, not used, should go to Micromet!
		double Gmin = 0.2;      //Fc = Gmin when CO2->unlimited
		double Gmax = 1.25;     //Fc = Gmax when CO2 = 0;
		double beta = 2.5;      //curvature factor,

		double Fc = Gmin + (Gmax - Gmin) * (1 - Gmin) * Math.Pow(CO2ambient, beta) /
		((Gmax - 1) * Math.Pow(CO2, beta) + (1 - Gmin) * Math.Pow(CO2ambient, beta));
		return Fc;
	}

	//Species ---------------------------------------------------------------
	//Calculate species N demand for potential growth (soilNdemand);
	public double CalcNdemand()
	{
        fShoot = NewGrowthToShoot();
		double fL = UpdatefLeaf(); //to consider more dm to leaf when DM is lower?

		double toRoot = dGrowthW * (1.0 - fShoot);
		double toStol = dGrowthW * fShoot * fStolon;
		double toLeaf = dGrowthW * fShoot * fLeaf;
		double toStem = dGrowthW * fShoot * (1.0 - fStolon - fLeaf);

		//N demand for new growth, optimum N (kg/ha)   -  RCichota, Jun/2014: changed actual N concentration for optimum
		NdemandOpt = toRoot * NcrootOpt + toStol * NcstolOpt + toLeaf * NcleafOpt + toStem * NcstemOpt;
		//NdemandOpt = toRoot * Ncroot + toStol * Ncstol1 + toLeaf * Ncleaf1 + toStem * Ncstem1;
		
		NdemandOpt *= NCO2Effects();    //reduce the demand under elevated [co2],
		//this will reduce the N stress under N limitation for the same soilN

		//N demand for new growth assuming luxury uptake (maximum [N])
		NdemandLux = toRoot * NcrootMax + toStol * NcstolMax + toLeaf * NcleafMax + toStem * NcstemMax;
		//Ndemand *= NCO2Effects();       //luxary uptake not reduce

		//even with sufficient soil N available
		if (isLegume)
			Nfix = MinFix * NdemandLux;
		else
			Nfix = 0.0;

		return Nfix;
	}


	//------------------------------------------
	public double UpdatefLeaf()
	{
		//temporary, need to do as interpolatiopon set
		double fL = 1.0;   //fraction of shoot goes to leaf
		if (isLegume)
		{
			if (dmgreen > 0 && (dmstol / dmgreen) > fStolon)
				fL = 1.0;
			else if (Pdmshoot < 2000)
				fL = fLeaf + (1 - fLeaf) * Pdmshoot / 2000;
			else
				fL = fLeaf;
		}
		else //grasses
		{
			if (Pdmshoot < 2000)
				fL = fLeaf + (1 - fLeaf) * Pdmshoot / 2000;
			else
				fL = fLeaf;
		}
		return fL;
	}

	//Species -------------------------------------------------------------
	public double DailyGrowthW()
	{
		Ncfactor = PmxNeffect();

		// NcFactor were addeded in Pm and Resp_m, Dec 10
		//  dGrowthW = dGrowthPot * Math.Min(gfwater, Ncfactor);
		dGrowthW = dGrowthPot * Math.Pow(gfwater, waterStressFactor);

		/*if (dGrowthPot > 0)
		{
		Console.Out.WriteLine(" growthPot: " + dGrowthPot);
		Console.Out.WriteLine(" gfwater: " + gfwater);
		Console.Out.WriteLine(" WstressW: " + waterStressFactor);
		Console.Out.WriteLine(" growthW: " + dGrowthW);

		}*/
		return dGrowthW;
	}

	//Species -------------------------------------------------------------
	public double DailyGrowthAct()
	{
		double gfnit = 0.0;
		if (isLegume)
			gfnit = gfn;                           //legume no dilution, but reducing more DM (therefore LAI)
		else
			gfnit = Math.Pow(gfn, NdilutCoeff);    // more DM growth than N limited, due to dilution (typically NdilutCoeff = 0.5)

		dGrowth = dGrowthW * Math.Min(gfnit, Frgr);
		return dGrowth;

		//RCichota, Jan/2014: updated the function, added account for Frgr
	}

	//Species -------------------------------------------------------------
	public double PmxNeffect()
	{
		double Fn = NCO2Effects();

		double Nleaf_green = 0;
		double effect = 1.0;
		if (!isAnnual)  //  &&and FVegPhase and ( VegDay < 10 ) ) then  // need this or it never gets going
		{
			Nleaf_green = Nleaf1 + Nleaf2 + Nleaf3;
			if (dmleaf_green > 0)
			{
				double Ncleaf_green = Nleaf_green / dmleaf_green;
				if (Ncleaf_green < NcleafOpt * Fn)     //Fn
				{
					if (Ncleaf_green > NcleafMin)
					{
						//effect = Math.Min(1.0, Ncleaf_green / NcleafOpt*Fn);
						effect = Math.Min(1.0, (Ncleaf_green - NcleafMin) / (NcleafOpt * Fn - NcleafMin));
					}
					else
					{
						effect = 0;
					}
				}
			}
		}
		return effect;
	}

	//Species -------------------------------------------------------------
	public double NFixCost()
	{
		double costF = 1.0;    //  redcuiton fraction of net prodcution as cost of N-fixining
		if (!isLegume || Nfix == 0 || NdemandLux == 0)      //  happens when plant has no growth
		{ return costF; }

		double actFix = Nfix / NdemandLux;
		costF = 1 - 0.24 * (actFix - MinFix) / (MaxFix - MinFix);
		if (costF < 0.76)
			costF = 0.76;
		return costF;
	}


	//Species -------------------------------------------------------------
	public double PartitionTurnover()
	{
		double GFT = GFTemperature();       // Temperature response

		//Leaf appearance rate is modified by temp & water stress
		double rateLeaf = leafRate * GFT * (Math.Pow(gfwater, 0.33333));  //why input is 3
		if (rateLeaf < 0.0) rateLeaf = 0.0;
		if (rateLeaf > 1.0) rateLeaf = 1.0;

		if (dGrowth > 0.0)                  // if no net growth, then skip "partition" part
		{
			//Not re-calculate fShoot for avoiding N-inbalance

			//New growth is allocated to the first tissue pools
			//fLeaf & fStolon: fixed partition to leaf & stolon.
			//Fractions [eq.4.13]
			double toRoot = 1.0 - fShoot;
			double toStol = fShoot * fStolon;
			double toLeaf = fShoot * fLeaf;
			double toStem = fShoot * (1.0 - fStolon - fLeaf);

			//checking
			double ToAll = toLeaf + toStem + toStol + toRoot;
			if (Math.Abs(ToAll - 1.0) > 0.0001)
				throw new Exception("  AgPasture - Mass balance lost on partition of new growth");
		 /* {Console.WriteLine("checking partitioning fractions") };*/ 

			//Assign the partitioned growth to the 1st tissue pools
			dmleaf1 += toLeaf * dGrowth;
			dmstem1 += toStem * dGrowth;
			dmstol1 += toStol * dGrowth;
			dmroot += toRoot * dGrowth;
			dGrowthHerbage = (toLeaf + toStem + toStol) * dGrowth;

			//partitioing N based on not only the DM, but also [N] in plant parts
			double Nsum = toLeaf * NcleafMax + toStem * NcstemMax + toStol * NcstolMax + toRoot * NcrootMax;
			double toLeafN = toLeaf * NcleafMax / Nsum;
			double toStemN = toStem * NcstemMax / Nsum;
			double toStolN = toStol * NcstolMax / Nsum;
			double toRootN = toRoot * NcrootMax / Nsum;

			Nleaf1 += toLeafN * newGrowthN;
			Nstem1 += toStemN * newGrowthN;
			Nstol1 += toStolN * newGrowthN;
			Nroot += toRootN * newGrowthN;

			double leftoverNremob = Nremob * Kappa4;  // fraction of Nremob not used, added to dead tissue
			if (leftoverNremob > 0)
			{
				double DMsum = dmleaf4 + dmstem;
				Nleaf4 += leftoverNremob * dmleaf4 / DMsum;
				Nstem4 += leftoverNremob * dmstem4 / DMsum;
			}

			// check whether luxury N was remobilised during N balance
			if (NFastRemob2 + NFastRemob3 > 0.0)
			{
				// partition any used N into plant parts (by N content)
				if (NFastRemob2 > 0.0)
				{
					Nsum = Nleaf2 + Nstem2 + Nstol2;
					Nleaf2 -= NFastRemob2 * Nleaf2 / Nsum;
					Nstem2 -= NFastRemob2 * Nstem2 / Nsum;
					Nstol2 -= NFastRemob2 * Nstol2 / Nsum;
				}
				if (NFastRemob3 > 0.0)
				{
					Nsum = Nleaf3 + Nstem3 + Nstol3;
					Nleaf3 -= NFastRemob3 * Nleaf3 / Nsum;
					Nstem3 -= NFastRemob3 * Nstem3 / Nsum;
					Nstol3 -= NFastRemob3 * Nstol3 / Nsum;
				}
			}

		}  //end of "partition" block

		//**Tussue turnover among the 12 standing biomass pools
		//The rates are affected by water and temperature factor
		double gftt = GFTempTissue();
		double gfwt = GFWaterTissue();

		gama = gftt * gfwt * rateLive2Dead;
		gamas = gama;                                    //for stolon of legumes
		//double gamad = gftt * gfwt * rateDead2Litter;
		double SR = 0;  //stocking rate affacting transfer of dead to little (default as 0 for now)
		gamad = rateDead2Litter * Math.Pow(gfwater, 3) * digestDead / 0.4 + stockParameter * SR;

		gamar = gftt * (2 - gfwater) * rateRootSen;  //gfwt * rateRootSen;


		if (gama == 0.0) //if gama ==0 due to gftt or gfwt, then skip "turnover" part
		{
			//no new little or root senensing
			dLitter = 0;
			dNLitter = 0;
			dRootSen = 0;
			dNrootSen = 0;
			//Nremob = Nremob; //no change
			//Nroot = Nroot;
		}
		else
		{
			if (isAnnual)
			{
				if (phenoStage == 1)        //vege
				{
					double Kv = (double)daysfromEmergence / daysEmgToAnth;
					gama *= Kv;
					gamar *= Kv;
				}
				else if (phenoStage == 2)    //repro
				{
					double Kr = (double)daysfromAnthesis / daysToMature;
					gama = 1 - (1 - gama) * (1 - Kr * Kr);
				}
			}

			// get daily defoliation: Fd = fraction of defoliation
			double Fd = 0;                                  //TODO with animal module later
			if (pS.dmdefoliated != 0 && pS.dmshoot != 0)
				Fd = pS.dmdefoliated / (pS.dmdefoliated + pS.dmshoot);

			//gamar = gamar + Fd * Fd * (1 - gamar);
			//**Nov 09: Decided not to reduce root mass mmediately in a high proportion according to defoliation,
			//**Gradual process is more reasonable, and this results in a very smmall difference in predicting prodution

			if (isLegume) gamas = gama + Fd * (1 - gama);   //increase stolon senescence

			//if today's turnover will result in a dmgreen < dmgreen_minimum, then adjust the rate,
			//double dmgreenToBe = dmgreen + dGrowth - gamad * (pS.dmleaf4 + pS.dmstem4 + pS.dmstol3);
			//Possibly to skip this for annuals to allow them to die - phenololgy-related?
			double dmgreenToBe = dmgreen + dGrowth - gama * (pS.dmleaf3 + pS.dmstem3 + pS.dmstol3);
			if (dmgreenToBe < dmgreenmin)
			{
				double preDMgreen = pS.dmgreen;
				if (gama > 0.0)
				{
					if (dmgreen + dGrowth < dmgreenmin)
					{
						gama = 0;
						gamas = 0;
						//  gamad = 0;
						gamar = 0;
					}
					else
					{
						double gama_adj = (dmgreen + dGrowth - dmgreenmin) / (pS.dmleaf3 + pS.dmstem3 + pS.dmstol3);
						gamar = gamar * gama_adj / gama;
						gamad = gamad * gama_adj / gama;
						gama = gama_adj;
					}
				}
			}
			if (dmroot < 0.5 * dmgreenmin)          //set a minimum root too
				gamar = 0;

			//Do actual DM turnover
			dmleaf1 = dmleaf1 - 2 * gama * pS.dmleaf1;                //except dmleaf1, other pool dm* = pS.dm*
			dmleaf2 = dmleaf2 - gama * pS.dmleaf2 + 2 * gama * pS.dmleaf1;
			dmleaf3 = dmleaf3 - gama * pS.dmleaf3 + gama * pS.dmleaf2;
			dmleaf4 = dmleaf4 - gamad * pS.dmleaf4 + gama * pS.dmleaf3;
			dGrowthHerbage -= gamad * pS.dmleaf4;

			dmstem1 = dmstem1 - 2 * gama * pS.dmstem1;
			dmstem2 = dmstem2 - gama * pS.dmstem2 + 2 * gama * pS.dmstem1;
			dmstem3 = dmstem3 - gama * pS.dmstem3 + gama * pS.dmstem2;
			dmstem4 = dmstem4 - gamad * pS.dmstem4 + gama * pS.dmstem3;
			dGrowthHerbage -= gamad * pS.dmstem4;

			dmstol1 = dmstol1 - 2 * gamas * pS.dmstol1;
			dmstol2 = dmstol2 - gamas * pS.dmstol2 + 2 * gamas * pS.dmstol1;
			dmstol3 = dmstol3 - gamas * pS.dmstol3 + gamas * pS.dmstol2;
			dGrowthHerbage -= gamas * pS.dmstol3;

			dRootSen = gamar * pS.dmroot;
			dmroot = dmroot - dRootSen;// -Resp_root;

			//Previous: N (assuming that Ncdead = Ncleaf4, Ncstem4 or Nclitter):  Nc --[N]
			double Nleaf1to2 = Ncleaf1 * 2 * gama * pS.dmleaf1;
			double Nleaf2to3 = Ncleaf2 * gama * pS.dmleaf2;
			double Nleaf3to4 = NcleafMin * gama * pS.dmleaf3;         //Ncleaf4 = NcleafMin: [N] in naturally scenescend tissue
			double Nleaf3Remob = (Ncleaf3 - NcleafMin) * gama * pS.dmleaf3;
			double Nleaf4toL = Ncleaf4 * gamad * pS.dmleaf4;        //to litter
			Nleaf1 = Nleaf1 - Nleaf1to2;
			Nleaf2 = Nleaf2 + Nleaf1to2 - Nleaf2to3;
			Nleaf3 = Nleaf3 + Nleaf2to3 - Nleaf3to4 - Nleaf3Remob;
			Nleaf4 = Nleaf4 + Nleaf3to4 - Nleaf4toL;

			if (dmleaf1 != 0) { Ncleaf1 = Nleaf1 / dmleaf1; }
			if (dmleaf2 != 0) { Ncleaf2 = Nleaf2 / dmleaf2; }
			if (dmleaf3 != 0) { Ncleaf3 = Nleaf3 / dmleaf3; }
			if (dmleaf4 != 0) { Ncleaf4 = Nleaf4 / dmleaf4; }

			double Nstem1to2 = Ncstem1 * 2 * gama * pS.dmstem1;
			double Nstem2to3 = Ncstem2 * gama * pS.dmstem2;
			double Nstem3to4 = NcstemMin * gama * pS.dmstem3;
			double Nstem3Remob = (Ncstem3 - NcstemMin) * gama * pS.dmstem3;
			double Nstem4toL = Ncstem4 * gamad * pS.dmstem4;   //to litter

			Nstem1 = Nstem1 - Nstem1to2;
			Nstem2 = Nstem2 + Nstem1to2 - Nstem2to3;
			Nstem3 = Nstem3 + Nstem2to3 - Nstem3to4 - Nstem3Remob;
			Nstem4 = Nstem4 + Nstem3to4 - Nstem4toL;

			if (dmstem1 != 0) { Ncstem1 = Nstem1 / dmstem1; }
			if (dmstem2 != 0) { Ncstem2 = Nstem2 / dmstem2; }
			if (dmstem3 != 0) { Ncstem3 = Nstem3 / dmstem3; }
			if (dmstem4 != 0) { Ncstem4 = Nstem4 / dmstem4; }

			double Nstol1to2 = Ncstol1 * 2 * gamas * pS.dmstol1;
			double Nstol2to3 = Ncstol2 * gamas * pS.dmstol2;
			double Nstol3Remob = 0.5 * (Ncstol3 - NcstolMin) * gamas * pS.dmstol3;       //gamas is acelerated by defoliation
			double Nstol3toL = Ncstol3 * gamas * pS.dmstol3 - Nstol3Remob;

			Nstol1 = Nstol1 - Nstol1to2;
			Nstol2 = Nstol2 + Nstol1to2 - Nstol2to3;
			Nstol3 = Nstol3 + Nstol2to3 - Nstol3toL - Nstol3Remob;

			if (dmstol1 != 0) { Ncstol1 = Nstol1 / dmstol1; } //grass has no stolon
			if (dmstol2 != 0) { Ncstol2 = Nstol2 / dmstol2; }
			if (dmstol3 != 0) { Ncstol3 = Nstol3 / dmstol3; }

			//rootN
			NrootRemob = 0.5 * (Ncroot - NcrootMin) * dRootSen;    //acelerated by defoliation, the N remob smaller
			dNrootSen = Ncroot * dRootSen - NrootRemob;
			Nroot = Nroot - Ncroot * dRootSen;              // (Ncroot goes to both Remob & FOM in soil)
			if (dmroot != 0) Ncroot = Nroot / dmroot;       // dmroot==0 this should not happen

			dLitter = gamad * (pS.dmleaf4 + pS.dmstem4) + gamas * pS.dmstol3;

			double leftoverNremob = Nremob * (1 - Kappa4);  // fraction of Nremob not used, added to litter
			dNLitter = Nleaf4toL + Nstem4toL + Nstol3toL + leftoverNremob;    //Nremob of previous day after newgrowth, go to litter
			//The leftover 'Nremob' of previous day (if>0) indicates more N should go to litter in previous day, so do it now
			//this is especially importatn in automn

			// remobilised and remobilisable N (these will be used tomorrow)
			Nremob = Nleaf3Remob + Nstem3Remob + Nstol3Remob + NrootRemob;
			NLuxury2 = Math.Max(0.0, Nleaf2 - dmleaf2 * NcleafOpt * NcRel2)
					 + Math.Max(0.0, Nstem2 - dmstem2 * NcstemOpt * NcRel2)
					 + Math.Max(0.0, Nstol2 - dmstol2 * NcstolOpt * NcRel2);
			NLuxury3 = Math.Max(0.0, Nleaf3 - dmleaf3 * NcleafOpt * NcRel3)
					 + Math.Max(0.0, Nstem3 - dmstem3 * NcstemOpt * NcRel3)
					 + Math.Max(0.0, Nstol3 - dmstol3 * NcstolOpt * NcRel3);
			// only a fraction of luxury N is available for remobilisation:
			NLuxury2 *= Kappa2;
			NLuxury3 *= Kappa3;

			//Sugar remobilisation and C balance:
			Cremob = 0;// not explicitely considered

			/*Cremob = (Nremob - leftoverNremob) * C2N_protein;    //Cremob is calculated one day later so as to know if it is really
			//remobilised with N
			if (Cremob > 0)
			{
			if (dLitter > Cremob * C2DM)
			{
			dLitter -= Cremob * C2DM;  //remove from litter (most likely in this case)
			}
			else
			{
			Cremob = dLitter / C2DM;
			dLitter = 0;
			}
			}
			else
			{
			dLitter += Cremob * C2DM;
			Cremob = 0;
			}*/


		}  //end of "turnover" block

		updateAggregated();

		calcDigestability();

		return dGrowth;
	}

    private double NewGrowthToShoot()
    {
        //The input maxSRratio (maximum percentage allocated to roots = 20%) was converted into
        //the real ratio (=4) at the beginning when setting specific values
        double GFmin = Math.Min(gfwater, gfn);      //To consider other nutrients later

        //Variable maxSR - maximum shoot/root ratio accoding to phenoloty
        double targetSR = maxSRratio;
        double newSR = targetSR;
        // fac: Assuming the new growth partition is towards a shoot:root ratio of 'maxSR' during reproductive stage,
        //      then the partition will be towards a lower shoot:root ratio of (frac*maxSRratio) during vegetative stage
       
        if (pS.dmroot > 0.00001)                    //pS is the previous state (yesterday)
        {
            double fac = 1.0;                   //day-to-day fraction of reduction
            //double minF = allocationSeasonF;    //default = 0.8;
            double doy = day_of_month + (int)((month - 1) * 30.5);
            // NOTE: the type for doy has to be double or the divisions below will be rounded (to int) and thus be [slightly] wrong

            int doyC = 232;             // Default as in South-hemisphere
            int[] ReproSeasonIntval = new int[] { 35, 60, 30 };

            if (usingLatFunctionFShoot)
            {
                doyC = (int)(Math.Abs(latitude) * ParamALatFunction);
                ReproSeasonIntval[1] = (int)(ParamBLatFunction * Math.Exp(-doyC / ParamCLatFunction));
                ReproSeasonIntval[0] = Math.Min(60, (int)(ReproSeasonIntval[1] * ParamDLatFunction));
                ReproSeasonIntval[2] = Math.Min(60, (int)(ReproSeasonIntval[1] * ParamELatFunction));
            }
            if (latitude > 0)           // If it is in North-hemisphere.
                doyC = doyC - 183;

            //int doyF = doyC + 35;   //75
            //int doyD = doyC + 95;   // 110;
            //int doyE = doyC + 125;  // 140;
            //if (doyE > 365) doyE = doyE - 365;

            int doyF = doyC + ReproSeasonIntval[0];
            int doyD = doyF + ReproSeasonIntval[1];
            int doyE = doyD + ReproSeasonIntval[2];

            int doyEoY = 365 + (DateTime.IsLeapYear(year) ? 1 : 0);

            if (doy > doyC)
            {
                if (doy <= doyF)
                    fac = 1.0 + allocationSeasonF * (doy - doyC) / (doyF - doyC);
                else if (doy <= doyD)
                    fac = 1.0 + allocationSeasonF;
                else if (doy <= doyE)
                    fac = 1 + allocationSeasonF * (1 - (doy - doyD) / (doyE - doyD));
            }
            else
            {
                // check whether the high allocation period goes across the year (should only needed for southern hemisphere)
                if ((doyD > doyEoY) && (doy <= doyD - doyEoY))
                    fac = 1.0 + allocationSeasonF;
                else if ((doyE > doyEoY) && (doy <= doyE - doyEoY))
                    fac = 1.0 + allocationSeasonF * (1 - (doyEoY + doy - doyD) / (doyE - doyD));
            }
            targetSR = fac * maxSRratio;
            //targetSR = 1.25 * fac * maxSRratio;    //maxR is bigger in reproductive stage (i.e., less PHT going to root)
            //fac = 0.8 ~ 1; i.e., maxSR = 1.0 ~ 1.25 of maxSRratio (i.e., SRratio may be 1.25 times of specified maxSRratio during reproductive stage)

            //calculate shoot:root partitioning: fShoot = fraction to shoot [eq.4.12c]
            //if (pS.dmroot > 0.00001)                    //pS is the previous state (yesterday)
            //{
            double presentSR = dmgreen / pS.dmroot;
            //if (presentSR > targetSR) presentSR = targetSR;
            if (presentSR > targetSR)
                newSR = GFmin * targetSR;
            else
                newSR = GFmin * targetSR * targetSR / presentSR;

            fShoot = newSR / (1.0 + newSR);
        }
        else
        {
            fShoot = 1.0;  // shouldn't this be zero??
        }
        if (fShoot / (1 - fShoot) < targetSR)
            fShoot = targetSR / (1 + targetSR);   // as the specified that the system maxSR towards to (useful under stress)

        if (dmgreen < pS.dmroot)  //this may happen under stress. There may be CHTs move up too
            fShoot = 1.0;

        return fShoot;
    }

	//Species -------------------------------------------------------------------
	public float coverGreen
	{
		get { return (float)(1.0 - Math.Exp(-lightExtCoeff * greenLAI)); }
	}
	//Species -------------------------------------------------------------------
	public float coverDead
	{
		get { return (float)(1.0 - Math.Exp(-lightExtCoeff * deadLAI)); }
	}
	//Species -------------------------------------------------------------------
	public float coverTot
	{
		get { return (float)(1.0 - (Math.Exp(-lightExtCoeff * totalLAI))); }
	}

	//Species ---------------------------------------------------------------------
	public double GFTemperature()
	{
		if (photoPath == 4) gftemp = GFTempC4();
		else gftemp = GFTempC3();               //CAM path ?
		return gftemp;
	}
	public double GFTemperature(double T)       //passing T
	{
		if (photoPath == 4) gftemp = GFTempC4(T);
		else gftemp = GFTempC3(T);
		return gftemp;
	}
	//Species -------------------------------------------------
	// Photosynthesis temperature response curve for C3 plants
	public double GFTempC3()
	{
		double gft3 = 0.0;
		double T = (MetData.maxt + MetData.mint) / 2;
		if (T > growthTmin && T < growthTmax)
		{
			double Tmax = growthTopt + (growthTopt - growthTmin) / growthTq;
			double val1 = Math.Pow((T - growthTmin), growthTq) * (Tmax - T);
			double val2 = Math.Pow((growthTopt - growthTmin), growthTq) * (Tmax - growthTopt);
			gft3 = val1 / val2;

			if (gft3 < 0.0) gft3 = 0.0;
			if (gft3 > 1.0) gft3 = 1.0;
		}
		return gft3;
	}
	//Species -------------------------------------------------
	// Photosynthesis temperature response curve for C3 plants, passing T
	public double GFTempC3(double T)
	{
		double gft3 = 0.0;
		if (T > growthTmin && T < growthTmax)
		{
			double Tmax = growthTopt + (growthTopt - growthTmin) / growthTq;
			double val1 = Math.Pow((T - growthTmin), growthTq) * (Tmax - T);
			double val2 = Math.Pow((growthTopt - growthTmin), growthTq) * (Tmax - growthTopt);
			gft3 = val1 / val2;

			if (gft3 < 0.0) gft3 = 0.0;
			if (gft3 > 1.0) gft3 = 1.0;
		}
		return gft3;
	}

	//Species ---------------------------------------------
	// Photosynthesis temperature response curve for C4 plants
	public double GFTempC4()
	{
		double gft4 = 0.0;          // Assign value 0 for the case of T < Tmin
		double T = (MetData.maxt + MetData.mint) / 2;

		if (T > growthTmin)         // same as GFTempC3 for [Tmin,Topt], but T as Topt if T > Topt
		{
			if (T > growthTopt)
				T = growthTopt;

			double Tmax = growthTopt + (growthTopt - growthTmin) / growthTq;
			double val1 = Math.Pow((T - growthTmin), growthTq) * (Tmax - T);
			double val2 = Math.Pow((growthTopt - growthTmin), growthTq) * (Tmax - growthTopt);
			gft4 = val1 / val2;

			if (gft4 < 0.0) gft4 = 0.0;
			if (gft4 > 1.0) gft4 = 1.0;
		}
		return gft4;
	}

	//Species ---------------------------------------------
	// Photosynthesis temperature response curve for C4 plants, passing T
	public double GFTempC4(double T)
	{
		double gft4 = 0.0;          // Assign value 0 for the case of T < Tmin

		if (T > growthTmin)         // same as GFTempC3 for [Tmin,Topt], but T as Topt if T > Topt
		{
			if (T > growthTopt)
				T = growthTopt;

			double Tmax = growthTopt + (growthTopt - growthTmin) / growthTq;
			double val1 = Math.Pow((T - growthTmin), growthTq) * (Tmax - T);
			double val2 = Math.Pow((growthTopt - growthTmin), growthTq) * (Tmax - growthTopt);
			gft4 = val1 / val2;

			if (gft4 < 0.0) gft4 = 0.0;
			if (gft4 > 1.0) gft4 = 1.0;
		}
		return gft4;
	}

	//Species ---------------------------------------------
	// Heat effect: reduction = (MaxT-28)/35, recovery after accumulating 50C of (meanT-25)
	private double HeatEffect()
	{
		//constants are now set from interface
		//recover from the previous high temp. effect
		double recoverF = 1.0;

		if (highTempEffect < 1.0)
		{
			double meanT = 0.5 * (MetData.maxt + MetData.mint);
			if (25 - meanT > 0)
			{
				accumT += (25 - meanT);
			}

			if (accumT < heatSumT)
			{
				recoverF = highTempEffect + (1 - highTempEffect) * accumT / heatSumT;
			}
		}

		//possible new high temp. effect
		double newHeatF = 1.0;
		if (MetData.maxt > heatFullT)
		{
			newHeatF = 0;
		}
		else if (MetData.maxt > heatOnsetT)
		{
			newHeatF = (MetData.maxt - heatOnsetT) / (heatFullT - heatOnsetT);
		}

		// If this new high temp. effect is compounded with the old one &
		// re-start of the recovery from the new effect
		if (newHeatF < 1.0)
		{
			highTempEffect = recoverF * newHeatF;
			accumT = 0;
			recoverF = highTempEffect;
		}

		return recoverF;
	}

	//Species ---------------------------------------------
	// Cold effect: reduction, recovery after accumulating 20C of meanT
	private double ColdEffect()
	{
		//recover from the previous high temp. effect
		double recoverF = 1.0;
		if (lowTempEffect < 1.0)
		{
			double meanT = 0.5 * (MetData.maxt + MetData.mint);
			if (meanT > 0)
			{
				accumTLow += meanT;
			}

			if (accumTLow < coldSumT)
			{
				recoverF = lowTempEffect + (1 - lowTempEffect) * accumTLow / coldSumT;
			}
		}

		//possible new low temp. effect
		double newColdF = 1.0;
		if (MetData.mint < coldFullT)
		{
			newColdF = 0;
		}
		else if (MetData.mint < coldOnsetT)
		{
			newColdF = (MetData.mint - coldFullT) / (coldOnsetT - coldFullT);
		}

		// If this new cold temp. effect happens when serious cold effect is still on,
		// compound & then re-start of the recovery from the new effect
		if (newColdF < 1.0)
		{
			lowTempEffect = newColdF * recoverF;
			accumTLow = 0;
			recoverF = lowTempEffect;
		}

		return recoverF;
	}

	//Species ----------------------------------------------------------
	// Tissue turnover rate's response to water stress (eq. 4.15h)
	public double GFWaterTissue()
	{
		double gfwt = 1.0;

		if (gfwater < massFluxWopt)
			gfwt = 1 + (massFluxW0 - 1.0) * ((massFluxWopt - gfwater) / massFluxWopt);

		if (gfwt < 1.0) gfwt = 1.0;
		if (gfwt > massFluxW0) gfwt = massFluxW0;
		return gfwt;
	}

	//Species ------------------------------------------------------
	// Tissue turnover rate's response to temperature (eq 4.15f)
	// Tissue turnover: Tmin=5, Topt=20 - same for C3 & C4 plants ?
	public double GFTempTissue()
	{
		double T = (MetData.maxt + MetData.mint) / 2;

		double gftt = 0.0;        //default as T < massFluxTmin
		if (T > massFluxTmin && T <= massFluxTopt)
		{
			gftt = (T - massFluxTmin) / (massFluxTopt - massFluxTmin);
		}
		else if (T > massFluxTopt)
		{
			gftt = 1.0;
		}
		return gftt;
	}
	// Species ----------------------------------------------------------------------
	public void ResetZero()  //kill this crop
	{
		//Reset dm pools
		dmleaf1 = dmleaf2 = dmleaf3 = dmleaf4 = 0;    //(kg/ha)
		dmstem1 = dmstem2 = dmstem3 = dmstem4 = 0;    //sheath and stem
		dmstol1 = dmstol2 = dmstol3 = 0;
		dmroot = 0;
		
		dmdefoliated = 0;

		//Reset N pools
		Nleaf1 = Nleaf2 = Nleaf3 = Nleaf4 = 0;
		Nstem1 = Nstem2 = Nstem3 = Nstem4 = 0;
		Nstol1 = Nstol2 = Nstol3 = Nroot = 0;

		phenoStage = 0;

		if (updateAggregated() > 0.0)  //return totalLAI = 0
		{
			Console.WriteLine("Plant is not completely killed.");
		}
	}


	//Species ---------------------------------------------------------
	public void SetInGermination()
	{
		bSown = true;
		phenoStage = 0; //before germination
	}

	//Species ---------------------------------------------------------
	public bool SetPrevPools()
	{
		pS.dmleaf1 = dmleaf1;
		pS.dmleaf2 = dmleaf2;
		pS.dmleaf3 = dmleaf3;
		pS.dmleaf4 = dmleaf4;
		pS.dmstem1 = dmstem1;
		pS.dmstem2 = dmstem2;
		pS.dmstem3 = dmstem3;
		pS.dmstem4 = dmstem4;
		pS.dmstol1 = dmstol1;
		pS.dmstol2 = dmstol2;
		pS.dmstol3 = dmstol3;
		pS.dmroot = dmroot;
		pS.dmleaf_green = dmleaf_green;
		pS.dmstem_green = dmstem_green;
		pS.dmstol_green = dmstol_green;
		pS.dmleaf = dmleaf;
		pS.dmstem = dmstem;
		pS.dmstol = dmstol;
		pS.dmshoot = dmshoot;
		pS.dmgreen = dmgreen;
		pS.dmdead = dmdead;
		pS.dmtotal = dmtotal;

		// RCichota May 2014: moved pS.dmdefoliated to be stored at the time of a removal (it is zeroed at the end of process)
		//pS.dmdefoliated = dmdefoliated;   // not really used
		//pS.Nremob = Nremob;

		return true;
	}

} //class Species


//DMPools =================================================
//for remember the pool status of previous day
public class DMPools
{
	public double dmleaf1;
	public double dmleaf2;
	public double dmleaf3;
	public double dmleaf4;
	public double dmstem1;
	public double dmstem2;
	public double dmstem3;
	public double dmstem4;
	public double dmstol1;
	public double dmstol2;
	public double dmstol3;
	public double dmroot;

	public double dmleaf;
	public double dmstem;
	public double dmleaf_green;
	public double dmstem_green;
	public double dmstol_green;
	public double dmstol;
	public double dmshoot;
	public double dmgreen;
	public double dmdead;
	public double dmtotal;
	public double dmdefoliated;
	public double Nremob;

	public DMPools() { }


} //class DMPools
