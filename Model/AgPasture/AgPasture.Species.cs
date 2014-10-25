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
/// A pasture species model
/// </summary>
public class Species
{

	internal SpeciesState pS = new SpeciesState();              //for remember the status of previous day
	//constants
	const double CD2C = 12.0 / 44.0;    //convert CO2 into C
	const double C2DM = 2.5;            //C to DM convertion
	const double DM2C = 0.4;            //DM to C converion
	const double N2Protein = 6.25;      //this is for plants... (higher amino acids)
	const double C2N_protein = 3.5;     //C:N in remobilised material
	//const double growthTref = 20.0;      //reference temperature

	//static variables for common parameters among species
	internal static NewMetType MetData = new NewMetType();    //climate data applied to all species
	internal static double latitude;
	internal static double dayLength;                         //day length
	internal static double CO2 = 380;                         //CO2 concentration
	internal static int day_of_month;
	internal static int month;
	internal static int year;
	internal static double PIntRadn;                          //total Radn intecepted by pasture
	internal static double PCoverGreen;
	internal static double PLightExtCoeff;                    //k of mixed pasture
	internal static double Pdmshoot;

	internal double intRadnFrac;     //fraction of Radn intercepted by this species = intRadn/Radn
	internal double intRadn;         //Intercepted Radn by this species

	internal string speciesName;
	internal string micrometType;

	internal bool isAnnual;        //Species type (1=annual,0=perennial)
	internal bool isLegume;        //Legume (0=no,1=yes)
	internal int photoPath;       //Phtosynthesis pathways: 3=C3, 4=C4; //no consideration for CAM(=3)

	//annual species parameters
	internal int dayEmerg;         //Earlist day of emergence (for annuals only)
	internal int monEmerg;        //Earlist month of emergence (for annuals only)
	internal int dayAnth;            //Earlist day of anthesis (for annuals only)
	internal int monAnth;            //Earlist month of anthesis (for annuals only)
	internal int daysToMature;    //Days from anthesis to maturity (for annuals only)
	internal int daysEmgToAnth;   //Days from emergence to Anthesis (calculated, annual only)
	internal int phenoStage = 1;  //pheno stages: 0 - pre_emergence, 1 - vegetative, 2 - reproductive
	internal double phenoFactor = 1;
	internal int daysfromEmergence = 0;   //days
	internal int daysfromAnthesis = 0;    //days

	private bool bSown = false;
	private double DDSfromSowing = 0;
	private double DDSfromEmergence = 0;
	private double DDSfromAnthesis = 0;

	//**internal double cropFactor;    //Crop Factor
	//**internal double maxResidCover;//Maximum Residue Cover (0-1) (added to ccov to define cover)
	internal int dRootDepth;        //Daily root growth (mm)
	internal int maxRootDepth;    //Maximum root depth (mm)
	internal double NdilutCoeff;
	internal int rootDepth;       //current root depth (mm)
	//**internal int rootFnType;        //Root function 0=default 1=Ritchie 2=power_law 3=proportional_depth

    internal double allocationSeasonF; //factor for different biomass allocation among seasons
    internal double startHighAllocation;
    internal double durationHighAllocation;
    internal double shoulderHighAllocation;
    internal bool usingLatFunctionFShoot = false;
    internal double referenceLatitude = 60;
    internal double paramALatFunction = 5.0;
    internal double onsetFacLatFunction = 0.5;
    internal double outsetFacLatFunction = 0.5;
    internal double maxShoulderLatFunction = 60;
    internal double minPlateauLatFunction = 15;
    internal double paramBLatFunction = 2.75;
    internal double allocationMax = 0.4;
    internal double paramCLatFunction = 4.0;

    internal double Pm;                    //reference leaf co2 mg/m^2/s maximum
    internal double maintRespiration;    //in %
    internal double growthEfficiency;
    internal double growthTmin;   //Minimum temperature (grtmin) - originally 0
	internal double growthTopt;   //Optimum temperature (grtopt) - originally 20
    internal double growthTref;
    internal double growthTq;        //Temperature n (grtemn) --fyl: q curvature coefficient, 1.5 for c3 & 2 for c4 in IJ
    internal bool usingHeatStress = false;
	internal double heatOnsetT;            //onset tempeature for heat effects
	internal double heatFullT;            //full temperature for heat effects
	internal double heatSumT;            //temperature sum for recovery - sum of (25-mean)
    internal double heatRecoverT;
    internal bool usingColdStress = false;
    internal double coldOnsetT;          //onset tempeature for cold effects
	internal double coldFullT;            //full tempeature for cold effects
	internal double coldSumT;            //temperature sum for recovery - sum of means
    internal double coldRecoverT;

	private double highTempEffect = 1;  //fraction of growth rate due to high temp. effect
	private double lowTempEffect = 1;   //fraction of growth rate due to low temp. effect
	private double accumT = 0;          //accumulated temperature from previous heat strike = sum of '25-MeanT'(>0)
	private double accumTLow = 0;       //accumulated temperature from previous cold strike = sum of MeanT (>0)

	internal double massFluxTmin;            //grfxt1    Mass flux minimum temperature
	internal double massFluxTopt;            //grfxt2    Mass flux optimum temperature
    internal double massFluxTq;
	internal double massFluxW0;            //grfw1        Mass flux scale factor at GLFwater=0 (must be > 1)
	internal double massFluxWopt;         //grfw2        Mass flux optimum temperature

	//**internal double satRadn;         //Saturated canopy radiation Pnet (MJ/m^2/day)
	internal double specificLeafArea;                //Specific leaf area (m2/kg dwt)
    internal double specificRootLength;
	internal double lightExtCoeff;    //Light extinction coefficient
	internal double rue;              //radiaiton use efficiency
	internal double maxAssimiRate;    //Maximum Assimulation rate at reference temp & daylength (20C & 12Hrs)
	internal double rateLive2Dead;    //Decay coefficient between live and dead
	internal double rateDead2Litter;    //Decay coefficient between dead and litter
	internal double rateRootSen;      //Decay reference root senescence rate (%/day)
	internal double stockParameter;   //Stock influence parameter
    internal static double stockingRate = 0;  //stocking rate affacting transfer of dead to little (default as 0 for now)
    internal double maxSRratio;       //Shoot-Root ratio maximum
	internal double leafRate;         //reference leaf appearance rate without stress
	internal double fLeaf;            //Fixed growth partition to leaf (0-1)
	internal double fStolon;            //Fixed growth partition to stolon (0-1)

	internal double digestLive;   //Digestibility of live plant material (0-1)
	internal double digestDead;   //Digestibility of dead plant material (0-1)

	internal double dmleaf1;    //leaf 1 (kg/ha)
	internal double dmleaf2;    //leaf 2 (kg/ha)
	internal double dmleaf3;    //leaf 3 (kg/ha)
	internal double dmleaf4;    //leaf dead (kg/ha)
	internal double dmstem1;    //sheath and stem 1 (kg/ha)
	internal double dmstem2;    //sheath and stem 2 (kg/ha)
	internal double dmstem3;    //sheath and stem 3 (kg/ha)
	internal double dmstem4;    //sheath and stem dead (kg/ha)
	internal double dmstol1;    //stolon 1 (kg/ha)
	internal double dmstol2;    //stolon 2 (kg/ha)
	internal double dmstol3;    //stolon 3 (kg/ha)
	internal double dmroot;    //root (kg/ha)
	internal double dmgreenmin; // minimum grenn dm
	internal double dmdeadmin; // minimum dead dm
	internal float Frgr;

	//CO2
    internal double referenceCO2 = 380;                  //ambient CO2 concentration
    internal double CO2PmaxScale;
	internal double CO2NScale;
	internal double CO2NMin;
	internal double CO2NCurvature;

	//water
	internal double swuptake;
	internal double swdemandFrac;
	internal double waterStressFactor;
	internal double soilSatFactor;

	//Nc - N concentration
	internal double NcstemFr;   //stem Nc as % of leaf Nc
	internal double NcstolFr;   //stolon Nc as % of leaf Nc
	internal double NcrootFr;   //root Nc as % of leaf Nc

	internal double NcRel2;     //N concentration in tissue 2 relative to tissue 1
	internal double NcRel3;     //N concentration in tissue 3 relative to tissue 1

	//current
	internal double Ncleaf1;    //leaf 1  (critical N %)
	internal double Ncleaf2;    //leaf 2
	internal double Ncleaf3;    //leaf 3
	internal double Ncleaf4;    //leaf dead
	internal double Ncstem1;    //sheath and stem 1
	internal double Ncstem2;    //sheath and stem 2
	internal double Ncstem3;    //sheath and stem 3
	internal double Ncstem4;    //sheath and stem dead
	internal double Ncstol1;    //stolon 1
	internal double Ncstol2;    //stolon 2
	internal double Ncstol3;    //stolon 3
	internal double Ncroot;        //root

	//Max, Min & Opt = critical N
	internal double NcleafOpt;    //leaf   (critical N %)
	internal double NcstemOpt;    //sheath and stem
	internal double NcstolOpt;    //stolon
	internal double NcrootOpt;    //root
	internal double NcleafMax;    //leaf  (critical N %)
	internal double NcstemMax;    //sheath and stem
	internal double NcstolMax;    //stolon
	internal double NcrootMax;    //root
	internal double NcleafMin;
	internal double NcstemMin;
	internal double NcstolMin;
	internal double NcrootMin;
	internal double MaxFix;   //N-fix fraction when no soil N available, read in later
	internal double MinFix;   //N-fix fraction when soil N sufficient

	//N in each pool (calculated as dm * Nc)
	internal double Nleaf1 = 0;    //leaf 1 (kg/ha)
	internal double Nleaf2 = 0;    //leaf 2 (kg/ha)
	internal double Nleaf3 = 0;    //leaf 3 (kg/ha)
	internal double Nleaf4 = 0;    //leaf dead (kg/ha)
	internal double Nstem1 = 0;    //sheath and stem 1 (kg/ha)
	internal double Nstem2 = 0;    //sheath and stem 2 (kg/ha)
	internal double Nstem3 = 0;    //sheath and stem 3 (kg/ha)
	internal double Nstem4 = 0;    //sheath and stem dead (kg/ha)
	internal double Nstol1 = 0;    //stolon 1 (kg/ha)
	internal double Nstol2 = 0;    //stolon 2 (kg/ha)
	internal double Nstol3 = 0;    //stolon 3 (kg/ha)
	internal double Nroot = 0;    //root (kg/ha)

	//calculated
	//DM
	internal double dmtotal;      //=dmgreen + dmdead
	internal double dmgreen;
	internal double dmdead;
	internal double dmleaf;
	internal double dmstem;
	internal double dmleaf_green;
	internal double dmstem_green;
	internal double dmstol_green;
	internal double dmstol;
	internal double dmshoot;

	internal double dmdefoliated;
	internal double Ndefoliated;
	internal double digestHerbage;
	internal double digestDefoliated;
	//LAI
	internal double greenLAI; //sum of 3 pools
	internal double deadLAI;  //pool dmleaf4
	internal double totalLAI;
	//N plant
	internal double Nshoot;    //above-ground total N (kg/ha)
	internal double Nleaf;    //leaf N
	internal double Nstem;    //stem N
	internal double Ngreen;    //live N
	internal double Ndead;    //in standing dead (kg/ha)
	internal double Nstolon;    //stolon

	internal double NremobMax;  //maximum N remob of the day
	internal double Nremob = 0;       //N remobiliesd N during senescing
	internal double Cremob = 0;
	internal double Nleaf3Remob = 0;
	internal double Nstem3Remob = 0;
	internal double Nstol3Remob = 0;
	internal double NrootRemob = 0;
	internal double remob2NewGrowth = 0;
	internal double newGrowthN = 0;    //N plant-soil
	internal double NdemandLux;      //N demand for new growth, with luxury uptake
	internal double NdemandOpt;
	internal double Nfix;         //N fixed by legumes

	internal double Kappa2 = 0.0;
	internal double Kappa3 = 0.0;
	internal double Kappa4 = 0.0;
	internal double NLuxury2;		       // luxury N (above Nopt) potentially remobilisable
	internal double NLuxury3;		       // luxury N (above Nopt)potentially remobilisable
	internal double NFastRemob2 = 0.0;   // amount of luxury N remobilised from tissue 2
	internal double NFastRemob3 = 0.0;   // amount of luxury N remobilised from tissue 3

    internal int rootDistributionMethod = 2;
    internal double expoLinearDepthParam = 0.12;
    internal double expoLinearCurveParam = 3.2;

	internal double soilNAvail;   //N available to this species
	internal double soilNdemand;  //N demand from soil (=Ndemand-Nremob-Nfixed)
	internal double soilNdemandMax;   //N demand for luxury uptake
	internal double soilNuptake;  //N uptake of the day

	//growth limiting factors
	internal double gfwater;  //from water stress
	internal double gftemp;   //from temperature
	internal double gfn;      //from N deficit
	internal double Ncfactor;
	internal double fNavail2Max; //demand/Luxruy uptake

	//calculated, species delta
	internal double dGrowthPot;    //daily growth potential
	internal double dGrowthW;      //daily growth with water-deficit incorporated
	internal double dGrowth;       //daily growth
	internal double dGrowthRoot;   //daily root growth
	internal double dGrowthHerbage; //daily growth shoot

	internal double dLitter;       //daily litter production
	internal double dNLitter;      //N in dLitter
	internal double dRootSen;      //daily root sennesce
	internal double dNrootSen;     //N in dRootSen

	internal double fShoot;         //actual fraction of dGrowth to shoot
	internal int dayCounter;
	internal double sumGFW;

	// transfer coefficients 
	internal double gama = 0.0;	// from tissue 1 to 2, then 3 then 4
	internal double gamas = 0.0;	// for stolons
	internal double gamad = 0.0;	// from dead to litter
	internal double gamar = 0.0;	// for roots (to dead/FOM)

	//internal double leafPref = 1;    //leaf preference
	internal double IL1;
	internal double Pgross;
	internal double Resp_m;
	internal double Resp_root;

	public void DailyRefresh()
	{
		dmdefoliated = 0.0;
		Ndefoliated = 0.0;
		digestDefoliated = 0.0;
	}

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
		greenLAI = 0.0001 * dmleaf_green * specificLeafArea + 0.0001 * dmstol * 0.3 * specificLeafArea;   //insensitive? assuming Mass2GLA = 0.3*SLA

		// Resilence after unfovoured conditions
		// Consider cover will be bigger for the same amount of DM when DM is low due to
		// - light extinction coefficient will be bigger - plant leaves will be more plate than in dense high swards
		// - more parts will turn green for photosysntheses?
		// - quick response of plant shoots to fovoured conditions after release of stress
		if (!isLegume && dmgreen < 1000)
		{
			greenLAI += 0.0001 * dmstem_green * specificLeafArea * Math.Sqrt((1000 - dmgreen) / 1000);
		}

		deadLAI = 0.0001 * dmleaf4 * specificLeafArea;
		totalLAI = greenLAI + deadLAI;

		return totalLAI;

	}

	public double rootGrowth()
	{
		if (isAnnual)
		{
			rootDepth = 50 + (maxRootDepth - 50) * daysfromEmergence / daysEmgToAnth;
			//considering root distribution change, here?
		}
		return rootDepth;  // no root depth change for pereniel pasture
	}

	public int CalcDaysEmgToAnth()
	{
		daysEmgToAnth = 0;
		int numbMonths = monAnth - monEmerg;  //emergence & anthesis in the same calendar year: monEmerg < monAnth
		if (monEmerg >= monAnth)              //...across the calendar year
			numbMonths += 12;

		daysEmgToAnth = (int)(30.5 * numbMonths + (dayAnth - dayEmerg));

		return daysEmgToAnth;
	}

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

	private void SetEmergentState()
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
	}

	public double DailyGrowthPot()
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

	//Plant photosynthesis increase to eleveated [CO2]
	public double PCO2Effects()
	{
		if (Math.Abs(CO2 - referenceCO2) < 0.5)
			return 1.0;

		double Kp = CO2PmaxScale; //700; for C3 plants & 150 for C4
		if (photoPath == 4)     //C4 plants
			Kp = 150;

		double Fp = (CO2 / (Kp + CO2)) * ((referenceCO2 + Kp) / referenceCO2);
		return Fp;
	}

    // Plant nitrogen [N] decline to elevated [CO2]
	public double NCO2Effects()
	{
		if (Math.Abs(CO2 - referenceCO2) < 0.5)
			return 1.0;

		double L = CO2NMin;         // 0.7 - lamda: same for C3 & C4 plants
		double Kn = CO2NScale;      // 600 - ppm,   when CO2 = 600ppm, Fn = 0.5*(1+lamda);
		double Qn = CO2NCurvature;  //2 - curveture factor

		double interm = Math.Pow((Kn - referenceCO2), Qn);
		double Fn = (L + (1 - L) * interm / (interm + Math.Pow((CO2 - referenceCO2), Qn)));
		return Fn;
	}

	//Canopy conductiance decline to elevated [CO2]
	public double ConductanceCO2Effects()
	{
		if (Math.Abs(CO2 - referenceCO2) < 0.5)
			return 1.0;
		//Hard coded here, not used, should go to Micromet!
		double Gmin = 0.2;      //Fc = Gmin when CO2->unlimited
		double Gmax = 1.25;     //Fc = Gmax when CO2 = 0;
		double beta = 2.5;      //curvature factor,

		double Fc = Gmin + (Gmax - Gmin) * (1 - Gmin) * Math.Pow(referenceCO2, beta) /
		((Gmax - 1) * Math.Pow(CO2, beta) + (1 - Gmin) * Math.Pow(referenceCO2, beta));
		return Fc;
	}

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

	public double PartitionTurnover()
	{
		//Leaf appearance rate is modified by temp & water stress
        //double rateLeaf = leafRate * GFT * (Math.Pow(gfwater, 0.33333));  //why input is 3
        //if (rateLeaf < 0.0) rateLeaf = 0.0;
        //if (rateLeaf > 1.0) rateLeaf = 1.0;

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
		gamad = rateDead2Litter * Math.Pow(gfwater, 3) * digestDead / 0.4 + stockParameter * stockingRate;

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

            double doyC = startHighAllocation;             // Default as in South-hemisphere: 232
            int doyEoY = 365 + (DateTime.IsLeapYear(year) ? 1 : 0);
            int[] ReproSeasonIntval = new int[3]; // { 35, 60, 30 };
            double allocationIncrease = allocationSeasonF;
            ReproSeasonIntval[0] = (int)(durationHighAllocation * shoulderHighAllocation * 1.17);
            ReproSeasonIntval[1] = (int)durationHighAllocation;
            ReproSeasonIntval[2] = (int)(durationHighAllocation * shoulderHighAllocation);

            if (usingLatFunctionFShoot)
            {
                int doyWinterSolstice = (latitude < 0) ? 171 : 354;
                // compute the day to start the period with higher DM allocation to shoot
                double doyIniPlateau = doyWinterSolstice;
                if (Math.Abs(latitude) > referenceLatitude)
                    doyIniPlateau += 183;
                else
                {
                    double myB = Math.Abs(latitude) / referenceLatitude;
                    doyIniPlateau += 183 * (paramALatFunction - paramALatFunction * myB + myB) * Math.Pow(myB, paramALatFunction - 1.0);
                }

                // compute the duration of the three phases (onset, plateau, and outset)
                double maxPlateauPeriod = doyEoY - 2 * maxShoulderLatFunction;
                ReproSeasonIntval[1] = (int)(minPlateauLatFunction + (maxPlateauPeriod - minPlateauLatFunction) * Math.Pow(1 - Math.Abs(latitude) / 90, paramBLatFunction));
                ReproSeasonIntval[0] = (int)Math.Min(maxShoulderLatFunction, ReproSeasonIntval[1] * onsetFacLatFunction);
                ReproSeasonIntval[2] = (int)Math.Min(maxShoulderLatFunction, ReproSeasonIntval[1] * outsetFacLatFunction);
                if (ReproSeasonIntval.Sum() > doyEoY)
                    throw new Exception("Error when calculating period with high DM allocation, greater then one year");

                doyC = doyIniPlateau - ReproSeasonIntval[0];
                // compute the factor to augment allocation
                allocationIncrease = allocationMax;
                if (Math.Abs(latitude) < referenceLatitude)
                {
                    double myB = Math.Abs(latitude) / referenceLatitude;
                    allocationIncrease *= (paramCLatFunction - paramCLatFunction * myB + myB) * Math.Pow(myB, paramCLatFunction - 1.0);
                }
            }

            //int doyF = doyC + 35;   //75
            //int doyD = doyC + 95;   // 110;
            //int doyE = doyC + 125;  // 140;
            //if (doyE > 365) doyE = doyE - 365;

            int doyF = (int)doyC + ReproSeasonIntval[0];
            int doyD = doyF + ReproSeasonIntval[1];
            int doyE = doyD + ReproSeasonIntval[2];

            if (doy > doyC)
            {
                if (doy <= doyF)
                    fac = 1.0 + allocationIncrease * (doy - doyC) / (doyF - doyC);
                else if (doy <= doyD)
                    fac = 1.0 + allocationIncrease;
                else if (doy <= doyE)
                    fac = 1 + allocationIncrease * (1 - (doy - doyD) / (doyE - doyD));
            }
            else
            {
                // check whether the high allocation period goes across the year (should only needed for southern hemisphere)
                if ((doyD > doyEoY) && (doy <= doyD - doyEoY))
                    fac = 1.0 + allocationIncrease;
                else if ((doyE > doyEoY) && (doy <= doyE - doyEoY))
                    fac = 1.0 + allocationIncrease * (1 - (doyEoY + doy - doyD) / (doyE - doyD));
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

	public float coverGreen
	{
		get { return (float)(1.0 - Math.Exp(-lightExtCoeff * greenLAI)); }
	}

    public float coverDead
	{
		get { return (float)(1.0 - Math.Exp(-lightExtCoeff * deadLAI)); }
	}

    public float coverTot
	{
		get { return (float)(1.0 - (Math.Exp(-lightExtCoeff * totalLAI))); }
	}

	public double GFTemperature(double T)       //passing T
	{
		if (photoPath == 4) gftemp = GFTempC4(T);
		else gftemp = GFTempC3(T);
		return gftemp;
	}

	// Photosynthesis temperature response curve for C3 plants, passing T
	public double GFTempC3(double T)
	{
		double gft3 = 0.0;
        double growthTmax = growthTopt + (growthTopt - growthTmin) / growthTq;
		if (T > growthTmin && T < growthTmax)
		{
			double val1 = Math.Pow((T - growthTmin), growthTq) * (growthTmax - T);
			double val2 = Math.Pow((growthTopt - growthTmin), growthTq) * (growthTmax - growthTopt);
			gft3 = val1 / val2;

			if (gft3 < 0.0) gft3 = 0.0;
			if (gft3 > 1.0) gft3 = 1.0;
		}
		return gft3;
	}

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

	// Heat effect: reduction = (MaxT-28)/35, recovery after accumulating 50C of (meanT-25)
	private double HeatEffect()
	{
		//constants are now set from interface
		//recover from the previous high temp. effect
		double recoverF = 1.0;

		if (highTempEffect < 1.0)
		{
			double meanT = 0.5 * (MetData.maxt + MetData.mint);
            if (heatRecoverT > meanT)
			{
				accumT += (heatRecoverT - meanT);
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

	// Cold effect: reduction, recovery after accumulating 20C of meanT
	private double ColdEffect()
	{
		//recover from the previous high temp. effect
		double recoverF = 1.0;
		if (lowTempEffect < 1.0)
		{
			double meanT = 0.5 * (MetData.maxt + MetData.mint);
			if (meanT > coldRecoverT)
			{
                accumTLow += (meanT - coldRecoverT);
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

	// Tissue turnover rate's response to temperature (eq 4.15f)
	// Tissue turnover: Tmin=5, Topt=20 - same for C3 & C4 plants ?
	public double GFTempTissue()
	{
		double T = (MetData.maxt + MetData.mint) / 2;

		double gftt = 0.0;        //default as T < massFluxTmin
		if (T > massFluxTmin && T <= massFluxTopt)
		{
            gftt = Math.Pow((T - massFluxTmin) / (massFluxTopt - massFluxTmin), massFluxTq);
		}
		else if (T > massFluxTopt)
		{
			gftt = 1.0;
		}
		return gftt;
	}

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

	public void SetInGermination()
	{
		bSown = true;
		phenoStage = 0; //before germination
	}

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

		// RCichota May 2014: moved pS.dmdefoliated to be stored at the time of a removal (it is zeroed at the end of process)

		return true;
	}

}

//------------------------------------------------------------------------------
//for remember the pool status of previous day
public class SpeciesState
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

    public SpeciesState() { }

}
