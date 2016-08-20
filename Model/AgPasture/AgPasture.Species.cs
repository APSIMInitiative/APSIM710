using System;
using System.Collections.Generic;
using System.Text;
using System.Linq;
using System.Linq.Expressions;
using System.Xml;
using System.Xml.Schema;
using ModelFramework;
using CSGeneral;


/// <summary>
/// A pasture species model
/// </summary>
public class Species
{
    /// <summary>Constructor, initialise the basic structure of a plant</summary>
    public Species()
    {
        leaves = new Organ(4);
        stems = new Organ(4);
        stolons = new Organ(4);
        roots = new Organ(2);
        InitialState = new SpeciesBasicState();
    }

    #region Static variables, to assess parameters common for all species  ------------------------

    /// <summary>Some Description</summary>
    internal static Clock Clock;

    /// <summary>Some Description</summary>
    internal static MetFile MetFile;

    /// <summary>Some Description</summary>
    internal static double CO2 = 380;

    /// <summary>Some Description</summary>
    internal static double swardInterceptedRadn; //total Radn intercepted by pasture

    /// <summary>Some Description</summary>
    internal static double swardCoverGreen;

    /// <summary>Some Description</summary>
    internal static double swardLightExtCoeff; //k of mixed pasture

    /// <summary>Soil layering</summary>
    internal double[] dlayer;

    #endregion

    #region Model parameters  ---------------------------------------------------------------------

    //// - General parameters  --------------------------------------------------------------------

    /// <summary>Some Description</summary>
    internal string speciesName;

    /// <summary>Some Description</summary>
    internal string micrometType;

    /// <summary>Photosynthesis pathways: 3=C3, 4=C4; no consideration for CAM(=3)</summary>
    internal string photoPath;

    //// - Specific parameters  -------------------------------------------------------------------

    //// > Photosysnthesis and growth  >>>

    /// <summary>reference leaf co2 mg/m^2/s maximum</summary>
    internal double Pm;

    /// <summary>Some Description</summary>
    internal double maintRespiration;

    /// <summary>Some Description</summary>
    internal double growthEfficiency;

    /// <summary>Some Description</summary>
    internal double alphaPhoto;

    /// <summary>Some Description</summary>
    internal double thetaPhoto;

    /// <summary>Some Description</summary>
    internal double fractionPAR;

    /// <summary>Light extinction coefficient</summary>
    internal double lightExtCoeff;

    /// <summary>Minimum temperature for growth</summary>
    internal double growthTmin;

    /// <summary>Optimum temperature for growth</summary>
    internal double growthTopt;

    /// <summary>Temperature curvature coefficient</summary>
    internal double growthTq;

    /// <summary>Some Description</summary>
    internal bool usingHeatStress = false;

    /// <summary>onset tempeature for heat effects</summary>
    internal double heatOnsetT;

    /// <summary>full temperature for heat effects</summary>
    internal double heatFullT;

    /// <summary>Some Description</summary>
    internal double heatTq;

    /// <summary>temperature sum for recovery</summary>
    internal double heatSumT;

    /// <summary>Some Description</summary>
    internal double heatRecoverT;

    /// <summary>Some Description</summary>
    internal bool usingColdStress = false;

    /// <summary>Some Description</summary>
    internal double coldOnsetT; //onset tempeature for cold effects

    /// <summary>Some Description</summary>
    internal double coldFullT; //full tempeature for cold effects

    /// <summary>Some Description</summary>
    internal double coldTq;

    /// <summary>Some Description</summary>
    internal double coldSumT; //temperature sum for recovery - sum of means

    /// <summary>Some Description</summary>
    internal double coldRecoverT;

    /// <summary>Some Description</summary>
    internal double referenceCO2 = 380; //ambient CO2 concentration

    /// <summary>Some Description</summary>
    internal double CO2PmaxScale;

    /// <summary>Some Description</summary>
    internal double CO2NScale;

    /// <summary>Some Description</summary>
    internal double CO2NMin;

    /// <summary>Some Description</summary>
    internal double CO2NCurvature;

    /// <summary>Some Description</summary>
    internal double maxTempEffectResp = 1.25;

    /// <summary>Some Description</summary>
    internal double respTref;

    //// > Germination and emergence >>>

    /// <summary>Some Description</summary>
    internal double degreesdayForGermination;

    /// <summary>Some Description</summary>
    internal double[] emergenceDM;

    //// > DM partition >>>

    /// <summary>Some Description</summary>
    internal double targetSRratio; //Shoot-Root ratio maximum

    /// <summary>Some Description</summary>
    internal double maxRootFraction; //Root DM allocation maximum (to be deprecated)

    /// <summary>Some Description</summary>
    internal double allocationSeasonF; //factor for different biomass allocation among seasons

    /// <summary>Some Description</summary>
    internal double startHighAllocation;

    /// <summary>Some Description</summary>
    internal double durationHighAllocation;

    /// <summary>Some Description</summary>
    internal double shoulderHighAllocation;

    /// <summary>Some Description</summary>
    internal bool usingLatFunctionFShoot = false;

    /// <summary>Some Description</summary>
    internal double referenceLatitude = 60;

    /// <summary>Some Description</summary>
    internal double paramALatFunction = 5.0;

    /// <summary>Some Description</summary>
    internal double onsetFacLatFunction = 0.5;

    /// <summary>Some Description</summary>
    internal double outsetFacLatFunction = 0.5;

    /// <summary>Some Description</summary>
    internal double maxShoulderLatFunction = 60;

    /// <summary>Some Description</summary>
    internal double minPlateauLatFunction = 15;

    /// <summary>Some Description</summary>
    internal double paramBLatFunction = 2.75;

    /// <summary>Some Description</summary>
    internal double allocationMax = 0.4;

    /// <summary>Some Description</summary>
    internal double paramCLatFunction = 4.0;

    /// <summary>Some Description</summary>
    internal double maxFLeaf;

    /// <summary>Some Description</summary>
    internal double minFLeaf;

    /// <summary>Some Description</summary>
    internal double dmMaxFLeaf;

    /// <summary>Some Description</summary>
    internal double dmReferenceFLeaf;

    /// <summary>Some Description</summary>
    internal double exponentFLeaf;

    /// <summary>Some Description</summary>
    internal double fStolon; //Fixed growth partition to stolon (0-1)

    /// <summary>Some Description</summary>
    internal double specificLeafArea; //Specific leaf area (m2/kg dwt)

    /// <summary>Some Description</summary>
    internal double specificRootLength;

    //// > DM turnover and senescence  >>>

    /// <summary>Some Description</summary>
    internal double liveLeavesPerTiller;

    /// <summary>Some Description</summary>
    internal double refTissueTurnoverRate; //Decay coefficient between live and dead

    /// <summary>Some Description</summary>
    internal double facGrowingTissue;

    /// <summary>Some Description</summary>
    internal double refTurnoverRateStolon;

    /// <summary>Some Description</summary>
    internal double refLitteringRate; //Decay coefficient between dead and litter

    /// <summary>Some Description</summary>
    internal double rateRootSen; //Decay reference root senescence rate (%/day)

    /// <summary>Some Description</summary>
    internal double massFluxTmin; //grfxt1    Mass flux minimum temperature

    /// <summary>Some Description</summary>
    internal double massFluxTopt; //grfxt2    Mass flux optimum temperature

    /// <summary>Some Description</summary>
    internal double massFluxTq;

    /// <summary>Some Description</summary>
    internal double massFluxW0; //grfw1        Mass flux scale factor at GLFwater=0 (must be > 1)

    /// <summary>Some Description</summary>
    internal double massFluxWopt; //grfw2        Mass flux optimum temperature

    /// <summary>Some Description</summary>
    internal double exponentGLFW2dead;

    /// <summary>Some Description</summary>
    internal double stockParameter; //Stock influence parameter

    /// <summary>Some Description</summary>
    internal static double stockingRate = 0.0;
        //stocking rate affacting transfer of dead to little (default as 0 for now)

    /// <summary>Some Description</summary>
    internal double Kappa2 = 0.0;

    /// <summary>Some Description</summary>
    internal double Kappa3 = 0.0;

    /// <summary>Some Description</summary>
    internal double Kappa4 = 0.0;

    //// > Nc - N concentration  >>>

    /// <summary>Some Description</summary>
    internal double NcstemFr; //stem Nc as % of leaf Nc

    /// <summary>Some Description</summary>
    internal double NcstolFr; //stolon Nc as % of leaf Nc

    /// <summary>Some Description</summary>
    internal double NcrootFr; //root Nc as % of leaf Nc

    /// <summary>Some Description</summary>
    internal double NcRel2; //N concentration in tissue 2 relative to tissue 1

    /// <summary>Some Description</summary>
    internal double NcRel3; //N concentration in tissue 3 relative to tissue 1

    //// > N fixation  >>>

    /// <summary>N fixation fraction when no soil N available</summary>
    internal double MaxFix;

    /// <summary>N fixation fraction when soil N sufficient</summary>
    internal double MinFix;

    /// <summary>Maximum reduction in growth as cost for N fixation</summary>
    internal double NFixCostMax;

    /// <summary>Respiration cost due to the presence of symbiont bacteria (gC/gC roots)</summary>
    internal double symbiontCostFactor;

    /// <summary>Activity cost of N fixation (gC/gN fixed)</summary>
    internal double NFixingCostFactor;

    /// <summary>Some Description</summary>
    internal int NFixationCostMethod;

    //// > growth limiting factors  >>>

    /// <summary>Some Description</summary>
    internal double waterStressFactor;

    /// <summary>Some Description</summary>
    internal double soilSatFactor;

    /// <summary>Some Description</summary>
    internal double minMacroPorosity;

    /// <summary>Some Description</summary>
    internal double NdilutCoeff;

    /// <summary>Some Description</summary>
    internal double tempFactorRespiration;

    /// <summary>Some Description</summary>
    internal double tempFacTTurnover;

    /// <summary>Some Description</summary>
    internal double swFacTTurnover;

    /// <summary>Some Description</summary>
    internal double GLFGeneric;

    /// <summary>Some Description</summary>
    internal double GLFSFertility;

    //// > Digestibility and feed quality  >>>

    /// <summary>Some Description</summary>
    internal double digestLive; //Digestibility of live plant material (0-1)

    /// <summary>Some Description</summary>
    internal double digestDead; //Digestibility of dead plant material (0-1)

    //// > Minimum DM and preferences when harvesting  >>>

    /// <summary>minimum green DM</summary>
    internal double dmgreenmin;

    ////  >> Plant height  >>>

    /// <summary>Some Description</summary>
    internal bool usingSpeciesHeight;

    /// <summary>Some Description</summary>
    internal double MaxPlantHeight;

    /// <summary>Some Description</summary>
    internal double MassForMaxHeight;

    /// <summary>Some Description</summary>
    internal double ExponentHeightFromMass;

    /// <summary>Some Description</summary>
    internal double MinimumHeight;

    //// > Root depth and distribution  >>>

    /// <summary>Some Description</summary>
    internal bool usingSpeciesRoot;

    /// <summary>minimum root depth (mm)</summary>
    internal double minRootDepth;

    /// <summary>maximum root depth</summary>
    internal double maxRootDepth; //Maximum root depth (mm)

    /// <summary>Base root elongation rate (mm/day)</summary>
    internal double rootElongationRate;

    /// <summary>Some Description</summary>
    internal int rootDistributionMethod = 2;

    /// <summary>Some Description</summary>
    internal double expoLinearDepthParam = 90.0;

    /// <summary>Some Description</summary>
    internal double expoLinearCurveParam = 3.2;

    /// <summary>Some Description</summary>
    internal double MaximumUptakeRateNH4 = 1.0;

    /// <summary>Some Description</summary>
    internal double MaximumUptakeRateNO3 = 1.0;

    /// <summary>Some Description</summary>
    internal double referenceRLD = 0.2;

    /// <summary>Some Description</summary>
    internal double NextraSWF = 0.25;

    /// <summary>Some Description</summary>
    internal double[] xf;

    //// > annual species parameters  >>>

    /// <summary>Some Description</summary>
    internal int dayEmerg; //Earlist day of emergence (for annuals only)

    /// <summary>Some Description</summary>
    internal int monEmerg; //Earlist month of emergence (for annuals only)

    /// <summary>Some Description</summary>
    internal int dayAnth; //Earlist day of anthesis (for annuals only)

    /// <summary>Some Description</summary>
    internal int monAnth; //Earlist month of anthesis (for annuals only)

    /// <summary>Some Description</summary>
    internal int daysToMature; //Days from anthesis to maturity (for annuals only)

    /// <summary>Some Description</summary>
    internal int daysEmgToAnth; //Days from emergence to Anthesis (calculated, annual only)

    /// <summary>Some Description</summary>
    internal double DDSEmergence;

    #endregion

    #region Private variables  --------------------------------------------------------------------

    //// > General plant variables >>>

    /// <summary>Flag whether the species is alive</summary>
    internal bool isAlive = true;

    /// <summary>state of leaves (DM and N)</summary>
    internal Organ leaves;

    /// <summary>state of sheath/stems (DM and N)</summary>
    internal Organ stems;

    /// <summary>state of stolons (DM and N)</summary>
    internal Organ stolons;

    /// <summary>state of roots (DM and N)</summary>
    internal Organ roots;

    /// <summary>basic state variables for each species (to be used for reset)</summary>
    internal SpeciesBasicState InitialState;

    /// <summary>Previous state</summary>
    internal SpeciesState prevState = new SpeciesState(); //for remembering the state of previous day

    // defining the plant type  -------------------------------------------------------------------

    /// <summary>Flag whether the species is a legume</summary>
    internal bool isLegume;

    /// <summary>Some Description</summary>
    internal bool isAnnual; //Species type (1=annual,0=perennial)

    // Annuals and phenology  ---------------------------------------------------------------------

    /// <summary>Some Description</summary>
    internal int phenoStage = 0; //pheno stages: 0 - pre_emergence, 1 - vegetative, 2 - reproductive

    /// <summary>Some Description</summary>
    internal int daysfromEmergence = 0; //days

    /// <summary>Some Description</summary>
    internal int daysfromAnthesis = 0; //days

    internal bool bSown = false;
    private double DDSfromSowing = 0.0;
    private double DDSfromEmergence = 0.0;
    private double DDSfromAnthesis = 0.0;

    // Photosynthesis, growth, and turnover  ----------------------------------------------------------------------

    /// <summary>Some Description</summary>
    internal double intRadnFrac; //fraction of Radn intercepted by this species = intRadn/Radn

    /// <summary>Some Description</summary>
    internal double interceptedRadn; //Intercepted Radn by this species (MJ/day)

    /// <summary>Some Description</summary>
    internal double IrradianceTopOfCanopy;

    /// <summary>Some Description</summary>
    internal double PotPhoto;

    /// <summary>Some Description</summary>
    internal double Pgross;

    /// <summary>Some Description</summary>
    internal double Resp_m;

    /// <summary>Some Description</summary>
    internal double Resp_g;

    /// <summary>Some Description</summary>
    internal double Cremob = 0.0;

    /// <summary>Some Description</summary>
    internal double costNFixation;

    /// <summary>Some Description</summary>
    internal double dGrowthPot = 0.0; //daily growth potential

    /// <summary>Some Description</summary>
    internal double dGrowthW = 0.0; //daily growth with water-deficit incorporated

    /// <summary>Some Description</summary>
    internal double dGrowth = 0.0; //daily growth

    /// <summary>Some Description</summary>
    internal double dGrowthRoot = 0.0; //daily root growth

    /// <summary>Some Description</summary>
    internal double dGrowthHerbage = 0.0; //daily growth shoot

    /// <summary>Some Description</summary>
    internal double dDMLitter = 0.0; //daily litter production

    /// <summary>Some Description</summary>
    internal double dNLitter = 0.0; //N in dDMLitter

    /// <summary>Some Description</summary>
    internal double dDMRootSen = 0.0; //daily root sennesce

    /// <summary>Some Description</summary>
    internal double dNRootSen = 0.0; //N in dDMRootSen

    /// <summary>actual fraction of new growth added to shoot</summary>
    internal double fShoot = 1.0;

    /// <summary>Actual fraction of shoot growth added to leaves</summary>
    internal double fLeaf;

    /// <summary>Some Description</summary>
    internal double gama = 0.0; // from tissue 1 to 2, then 3 then 4

    /// <summary>Some Description</summary>
    internal double gamaS = 0.0; // for stolons

    /// <summary>Some Description</summary>
    internal double gamaD = 0.0; // from dead to litter

    /// <summary>Some Description</summary>
    internal double gamaR = 0.0; // for roots (to dead/FOM)

    /// <summary>Some Description</summary>
    internal double germinationGDD = 0.0;

    // Plant height, LAI and cover  ---------------------------------------------------------------

    /// <summary>Plant height</summary>
    internal double height;

    /// <summary>Some Description</summary>
    internal double greenLAI; //sum of 3 pools

    /// <summary>Some Description</summary>
    internal double deadLAI; //pool dmleaf4

    /// <summary>Some Description</summary>
    internal double totalLAI;

    //// > Root depth and distribution  >>>

    /// <summary>current root depth (mm)</summary>
    internal double rootDepth;

    /// <summary>Some Description</summary>
    internal int layerBottomRootZone;

    /// <summary>Daily root growth (mm)</summary>
    internal double dRootDepth;

    /// <summary>Some Description</summary>
    internal double[] rootFraction;
    internal double[] rootFraction1;

    /// <summary>Some Description</summary>
    internal double[] targetRootAllocation;

    // Amounts and fluxes of N in the plant  ------------------------------------------------------

    /// <summary>Some Description</summary>
    internal double NdemandLux = 0.0; //N demand for new growth, with luxury uptake

    /// <summary>Some Description</summary>
    internal double NdemandOpt = 0.0;

    /// <summary>Some Description</summary>
    internal double NFixed = 0.0; //N fixed by legumes

    /// <summary>Some Description</summary>
    internal double Nremob = 0.0; //N remobiliesd N during senescing

    /// <summary>Some Description</summary>
    internal double Nleaf3Remob = 0.0;

    /// <summary>Some Description</summary>
    internal double Nstem3Remob = 0.0;

    /// <summary>Some Description</summary>
    internal double Nstol3Remob = 0.0;

    /// <summary>Some Description</summary>
    internal double NrootRemob = 0.0;

    /// <summary>Some Description</summary>
    internal double remob2NewGrowth = 0.0;

    /// <summary>Some Description</summary>
    internal double newGrowthN = 0.0; //N plant-soil

    /// <summary>uxury N (above Nopt) potentially remobilisable from tissue 2</summary>
    internal double NLuxury2 = 0.0;

    /// <summary>luxury N (above Nopt) potentially remobilisable from tissue 3</summary>
    internal double NLuxury3 = 0.0;

    /// <summary>amount of luxury N remobilised from tissue 2</summary>
    internal double NLuxuryRemob2 = 0.0;

    /// <summary>amount of luxury N remobilised from tissue 3</summary>
    internal double NLuxuryRemob3 = 0.0;

    // N uptake process  --------------------------------------------------------------------------

    /// <summary>N demand from soil</summary>
    internal double soilNdemand = 0.0;

    /// <summary>Amount of NH4 available for uptake</summary>
    internal double[] soilAvailableNH4;

    /// <summary>Amount of NO3 available for uptake</summary>
    internal double[] soilAvailableNO3;

    /// <summary>Amount of N_NH4 taken up</summary>
    internal double soilNH4Uptake;

    /// <summary>Amount of N_NO3 taken up</summary>
    internal double soilNO3Uptake;

    // water uptake process  ----------------------------------------------------------------------

    /// <summary>Plant water demand</summary>
    internal double WaterDemand = 0.0;

    /// <summary>Plant soil available water</summary>
    internal double[] soilAvailableWater;

    /// <summary>Amount of soil water taken up</summary>
    internal double WaterUptake = 0.0;

    /// <summary>Amount of soil water taken up</summary>
    internal double[] soilWaterUptake;

    // growth limiting factors --------------------------------------------------------------------

    /// <summary>Some Description</summary>
    internal double canopyCompetitionFactor;

    /// <summary>Some Description</summary>
    internal double RadnFactor = 1.0;

    /// <summary>Some Description</summary>
    internal double CO2Factor;

    /// <summary>Some Description</summary>
    internal double NcFactor;

    /// <summary>Some Description</summary>
    internal double glfTemp = 1.0; //from temperature

    /// <summary>Some Description</summary>
    internal double TempFactor;

    /// <summary>Some Description</summary>
    internal double ExtremeTempStress;

    /// <summary>Some Description</summary>
    internal double glfWater = 1.0; //from water stress

    /// <summary>Some Description</summary>
    internal double glfAeration = 1.0;

    /// <summary>Some Description</summary>
    internal double glfN = 1.0; //from N deficit

    // auxiliary variables for temperature stress  ------------------------------------------------

    /// <summary>fraction of growth rate due to high temp. effect</summary>
    internal double highTempStress = 1.0;

    /// <summary>accumulated temperature from previous heat strike</summary>
    private double accumTHeat = 0.0;

    /// <summary>fraction of growth rate due to low temp. effect</summary>
    internal double lowTempStress = 1.0;

    /// <summary>accumulated temperature from previous cold strike</summary>
    private double accumTCold = 0.0;

    // Harvest and digestibility  -----------------------------------------------------------------

    /// <summary>Some Description</summary>
    internal double dmdefoliated;

    /// <summary>Some Description</summary>
    internal double Ndefoliated;

    /// <summary>Some Description</summary>
    internal double digestHerbage;

    /// <summary>Some Description</summary>
    internal double digestDefoliated;

    #endregion

    #region Internal constants  -------------------------------------------------------------------

    //// - Constants  -----------------------------------------------------------------------------

    /// <summary>DM to C converion</summary>
    const double CarbonFractionDM = 0.4;

    /// <summary>N mols per mol of protein</summary>
    const double N2Protein = 6.25; //this is for plants... (higher amino acids)

    /// <summary>CN ratio of proteins</summary>
    const double ProteinCNr = 3.5; //C:N in remobilised material

    /// <summary>CN ratio of cell wall</summary>
    const double CNw = 100;

    #endregion

    #region Model outputs  ------------------------------------------------------------------------

    /// <summary>Dry matter amount aboveground (kg/ha)</summary>
    internal double AboveGroundWt
    {
        get { return leaves.DMTotal + stems.DMTotal + stolons.DMTotal; }
    }

    /// <summary>Dry matter amount of green material aboveground (kg/ha)</summary>
    internal double AboveGroundLiveWt
    {
        get { return leaves.DMGreen + stems.DMGreen + stolons.DMGreen; }
    }

    /// <summary>Dry matter amount aboveground (kg/ha)</summary>
    internal double AboveGroundDeadWt
    {
        get { return leaves.DMDead + stems.DMDead + stolons.DMDead; }
    }

    /// <summary>N amount aboveground (kg/ha)</summary>
    internal double AboveGroundN
    {
        get { return leaves.NTotal + stems.NTotal + stolons.NTotal; }
    }

    /// <summary>N amount of green material aboveground (kg/ha)</summary>
    internal double AboveGroundLiveN
    {
        get { return leaves.NGreen + stems.NGreen + stolons.NGreen; }
    }

    /// <summary>N amount aboveground (kg/ha)</summary>
    internal double AboveGroundDeadN
    {
        get { return leaves.NDead + stems.NDead + stolons.NDead; }
    }

    /// <summary>Root length density (mm/mm3)</summary>
    internal double[] RLD
    {
        get
        {
            double[] Result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
            {
                Result[layer] = (roots.DMGreen * 0.1) * rootFraction[layer] * specificRootLength; // m/m2
                Result[layer] /= dlayer[layer] * 1000; // mm/mm3
            }

            return Result;
        }
    }

    #endregion

    #region Initialisation methods  ---------------------------------------------------------------

    /// <summary>
    /// Set DM and N values for each species in the sward
    /// </summary>
    /// <param name="MyState">The collection of basic state defining values</param>
    internal void SetSpeciesState(SpeciesBasicState MyState)
    {
        //// Shoot DM ....................................................
        leaves.tissue[0].DM = MyState.DMWeight[0];
        leaves.tissue[1].DM = MyState.DMWeight[1];
        leaves.tissue[2].DM = MyState.DMWeight[2];
        leaves.tissue[3].DM = MyState.DMWeight[3];
        stems.tissue[0].DM = MyState.DMWeight[4];
        stems.tissue[1].DM = MyState.DMWeight[5];
        stems.tissue[2].DM = MyState.DMWeight[6];
        stems.tissue[3].DM = MyState.DMWeight[7];
        stolons.tissue[0].DM = MyState.DMWeight[8];
        stolons.tissue[1].DM = MyState.DMWeight[9];
        stolons.tissue[2].DM = MyState.DMWeight[10];
        roots.tissue[0].DM = MyState.DMWeight[11];

        //// Initial N amount in each pool ...............................
        leaves.tissue[0].Namount = MyState.NAmount[0];
        leaves.tissue[1].Namount = MyState.NAmount[1];
        leaves.tissue[2].Namount = MyState.NAmount[2];
        leaves.tissue[3].Namount = MyState.NAmount[3];
        stems.tissue[0].Namount = MyState.NAmount[4];
        stems.tissue[1].Namount = MyState.NAmount[5];
        stems.tissue[2].Namount = MyState.NAmount[6];
        stems.tissue[3].Namount = MyState.NAmount[7];
        stolons.tissue[0].Namount = MyState.NAmount[8];
        stolons.tissue[1].Namount = MyState.NAmount[9];
        stolons.tissue[2].Namount = MyState.NAmount[10];
        roots.tissue[0].Namount = MyState.NAmount[11];

        //// Aggregated and plant parts variables ........................
        UpdateAggregatedVariables();

        //// Plant height ................................................
        if (usingSpeciesHeight)
        {
            // each species has it own height
            height = HeightfromDM();
        }

        //// Root depth and distribution  ................................
        rootDepth = MyState.RootDepth;
        //layerBottomRootZone = GetRootZoneBottomLayer();
        targetRootAllocation = RootDistributionTarget();
        rootFraction1 = CurrentRootDistributionTarget();

        //// General status ..............................................
        //mySpecies[s].isAlive = isAlive;
        //if (MyState.ShootDM <= 1e-12)
        //    phenoStage = 0;
        //else
        //    phenoStage = 1;
    }

    /// <summary>
    /// Emergence to anthesys
    /// </summary>
    /// <returns>Number of days</returns>
    internal int CalcDaysEmgToAnth()
    {
        daysEmgToAnth = 0;
        int numbMonths = monAnth - monEmerg; //emergence & anthesis in the same calendar year: monEmerg < monAnth
        if (monEmerg >= monAnth) //...across the calendar year
            numbMonths += 12;

        daysEmgToAnth = (int) (30.5 * numbMonths + (dayAnth - dayEmerg));

        return daysEmgToAnth;
    }

    #endregion

    #region Daily processes  ----------------------------------------------------------------------

    /// <summary>
    /// Refresh variables
    /// </summary>
    internal void DailyRefresh()
    {
        dmdefoliated = 0.0;
        Ndefoliated = 0.0;
        digestDefoliated = 0.0;
    }

    #region Plant growth and DM partition  --------------------------------------------------------

    /// <summary>
    /// Computation of daily progress through germination
    /// </summary>
    /// <returns>Fraction of germination phase completed</returns>
    internal double DailyGerminationProgress()
    {
        double result = 0.0;
        if (isAnnual)
        {
            DDSfromSowing += Tmean;
            result = DDSfromSowing / DDSEmergence;
        }
        else
        {
            germinationGDD += Math.Max(0.0, Tmean - growthTmin);
            result = germinationGDD / degreesdayForGermination;
        }
        return result;
    }

    /// <summary>
    /// Daily potential growth
    /// </summary>
    /// <returns>Pot growth</returns>
    internal double DailyGrowthPot()
    {
        CO2Factor = 1.0;
        NcFactor = 1.0;
        RadnFactor = 1.0;

        // annuals phenology
        if (isAnnual)
        {
            bool moreGrowth = annualPhenology();
            if (!moreGrowth)
                return dGrowthPot = 0.0;
        }

        // skip growth if plant hasn't germinated yet
        if (phenoStage == 0 || greenLAI == 0)
            return dGrowthPot = 0.0;

        //Add temp effects to Pm
        double Tday = Tmean + 0.5 * (MetFile.MaxT - Tmean);

        //Temperature growth factor (for reporting purposes only)
        TempFactor = (0.25 * GFTemperature(Tmean)) + (0.75 * GFTemperature(Tday));

        double glfT = GFTemperature(Tmean);
        CO2Factor = PCO2Effects();
        NcFactor = PmxNeffect();

        double Pm_mean = Pm * glfT * CO2Factor * NcFactor;

        glfT = GFTemperature(Tday);
        double Pm_day = Pm * glfT * CO2Factor * NcFactor;

        double tau = 3600 * MetFile.day_length; //conversion of hour to seconds
        double IL = swardLightExtCoeff * 1.33333 * 0.5 * swardInterceptedRadn * 1000000 / tau;
        double IL2 = IL / 2; //IL for early & late period of a day
        IrradianceTopOfCanopy = IL;

        // Photosynthesis per LAI under full irradiance at the top of the canopy
        double photoAux1 = alphaPhoto * IL + Pm_day;
        double photoAux2 = 4 * thetaPhoto * alphaPhoto * IL * Pm_day;
        double Pl1 = (0.5 / thetaPhoto) * (photoAux1 - Math.Sqrt(Math.Pow(photoAux1, 2) - photoAux2));

        photoAux1 = alphaPhoto * IL2 + Pm_mean;
        photoAux2 = 4 * thetaPhoto * alphaPhoto * IL2 * Pm_mean;
        double Pl2 = (0.5 / thetaPhoto) * (photoAux1 - Math.Sqrt(Math.Pow(photoAux1, 2) - photoAux2));

        // Upscaling from 'per LAI' to 'per ground area'
        double carbon_m2 = 0.5 * (Pl1 + Pl2) * swardCoverGreen * intRadnFrac / lightExtCoeff;
        carbon_m2 *= 0.000001 * tau * (12.0 / 44.0);
        // tau: from second => day;
        // 0.000001: gtom mg/m^2 => kg/m^2_ground/day;
        // (12.0 / 44.0): from CO2 to carbohydrate (DM)

        // Fraction of total radiation available to this plant
        canopyCompetitionFactor = intRadnFrac;

        PotPhoto = 10000 * carbon_m2; //10000: 'kg/m^2' =>'kg/ha'

        //Add extreme temperature effects;
        ExtremeTempStress = HeatEffect() * ColdEffect(); // in practice only one temp stress factor is < 1
        Pgross = PotPhoto * ExtremeTempStress;

        // Consider generic growth limiting factor
        Pgross *= GLFGeneric;

        // Radiation effects (for reporting purposes only)
        RadnFactor = MathUtility.Divide((0.25 * Pl2) + (0.75 * Pl1), (0.25 * Pm_mean) + (0.75 * Pm_day), 1.0);

        // Respiration, maintenance and growth
        tempFactorRespiration = 0.0;
        if (Tmean > growthTmin)
        {
            if (Tmean < growthTopt)
            {
                tempFactorRespiration = GFTemperature(Tmean);
                //Teffect = Math.Pow(Teffect, 1.5);
            }
            else
            {
                //Teffect = 1;
                tempFactorRespiration = Tmean / growthTopt;
                    // Using growthTopt as reference, and set a maximum rise to 1.25
                if (tempFactorRespiration > maxTempEffectResp) tempFactorRespiration = maxTempEffectResp;
                tempFactorRespiration *= GFTemperature(growthTopt);
                    // Added by RCichota,oct/2014 - after changes in temp function needed this to make the function continuous
            } //The extreme high temperature (heat) effect is added separately
        }

        double LiveDM = (AboveGroundLiveWt + roots.DMGreen) * CarbonFractionDM; //converting DM to C    (kgC/ha)
        Resp_m = maintRespiration * tempFactorRespiration * NcFactor * LiveDM;
        Resp_g = Pgross * (1 - growthEfficiency);

        // N fixation costs
        costNFixation = 0.0;
        if (isLegume)
        {
            if (NFixationCostMethod == 1)
                costNFixation = NFixationCost_M1(Pgross);
            else if (NFixationCostMethod == 2)
                costNFixation = NFixationCost_M2();
        }

        // ** C budget is not explicitly done here as in EM
        Cremob = 0.0; // Nremob* C2N_protein;    // No carbon budget here
        // Nu_remob[elC] := C2N_protein * Nu_remob[elN];
        // need to substract CRemob from dm turnover?

        // Net potential growth (C) of the day (excluding growth respiration)
        dGrowthPot = Pgross + Cremob - Resp_g - Resp_m - costNFixation;
        dGrowthPot = Math.Max(0.0, dGrowthPot);

        //convert C to DM
        dGrowthPot /= CarbonFractionDM;

        // phenologically related reduction of annual species (from IJ)
        if (isAnnual)
            dGrowthPot = annualSpeciesReduction();

        return dGrowthPot;
    }

    /// <summary>
    /// Daily potential carbon assimilation
    /// </summary>
    internal void DailyPotentialPhotosynthesis()
    {
        bool isGrowing = true;

        // annuals phenology
        if (isAnnual)
            isGrowing = annualPhenology();

        // set basic values for growth factors
        CO2Factor = 1.0;
        NcFactor = 1.0;
        RadnFactor = 1.0;

        if (isGrowing)
        {
            //CO2 effects
            CO2Factor = PCO2Effects();

            //N concentration effects
            NcFactor = PmxNeffect();

            //Temperature effects at dawn/dusk
            double glfTmean = GFTemperature(Tmean);

            //Temperature growth factor (for reporting purposes only)
            TempFactor = (0.25 * glfTmean) + (0.75 * glfTemp);

            //Potential photosynthetic rate at dawn/dusk (first and last quarter of the day)
            double Pm1 = Pm * glfTmean * CO2Factor * NcFactor;

            //Potential photosynthetic rate at bright light (half of sunlight length, middle of day)
            double Pm2 = Pm * glfTemp * CO2Factor * NcFactor;

            //Sunlight length, converted to seconds
            double tau = 3600 * MetFile.day_length;

            //Photosynthetic active irradiance - converted from MJ/m2.day to J/m2.s
            double incidentPAR = MetFile.Radn * fractionPAR * 1000000 / tau;

            //Irradiance at top of canopy in the middle of the day (J/m2 leaf/s)
            IrradianceTopOfCanopy = lightExtCoeff * incidentPAR * (4.0 / 3.0);

            //Photosynthesis per leaf area under full irradiance at the top of the canopy (mg CO2/m^2 leaf/s)
            double Pl1 = SingleLeafPhotosynthesis(0.5 * IrradianceTopOfCanopy, Pm1); // early and late parts of the day
            double Pl2 = SingleLeafPhotosynthesis(IrradianceTopOfCanopy, Pm2); // middle part of the day

            // Radiation effects (for reporting purposes only)
            RadnFactor = MathUtility.Divide((0.25 * Pl1) + (0.75 * Pl2), (0.25 * Pm1) + (0.75 * Pm2), 1.0);

            // Fraction of total radiation available to this plant
            canopyCompetitionFactor = MathUtility.Divide(interceptedRadn, MetFile.Radn * coverGreen, 1.0);

            //Canopy photosynthesis - Upscaling from 'per leaf' to 'per ground' area
            double carbon_m2 = 0.5 * (Pl1 + Pl2); // mgCO2/m2 leaf/s
            carbon_m2 *= coverGreen * canopyCompetitionFactor; // mgCO2/m2 leaf/s - canopy
            carbon_m2 /= lightExtCoeff; // mgCO2/m2.s - ground area
            carbon_m2 *= 0.000001; // kgCO2/m2.s
            carbon_m2 *= tau; // kgCO2/m2.day
            carbon_m2 *= 12.0 / 44.0; // kgC/m2.day
            PotPhoto = 10000 * carbon_m2; // kgC/ha.day

            //Add extreme temperature effects;
            ExtremeTempStress = HeatEffect() * ColdEffect(); // in practice only one temp stress factor is < 1
            Pgross = PotPhoto * ExtremeTempStress;

            // Consider a generic growth limiting factor
            Pgross *= GLFGeneric;
        }
        else
        {
            Pgross = 0.0;
        }
    }

    /// <summary>
    /// Compute the photosynthetic rate for a single leaf
    /// </summary>
    /// <param name="IL">Instantaneous intercepted radiation (depend on time of day)</param>
    /// <param name="Pmax">Max photosyntehsis rate, given T, CO2 and N concentration</param>
    /// <returns></returns>
    private double SingleLeafPhotosynthesis(double IL, double Pmax)
    {
        double photoAux1 = alphaPhoto * IL + Pmax;
        double photoAux2 = 4 * thetaPhoto * alphaPhoto * IL * Pmax;
        double Pl = (0.5 / thetaPhoto) * (photoAux1 - Math.Sqrt(Math.Pow(photoAux1, 2) - photoAux2));
        return Pl;
    }

    /// <summary>
    /// Compute plant respiration, growth and maintenance
    /// </summary>
    /// <returns>Amount of C lost by respiration</returns>
    internal void DailyPlantRespiration()
    {
        double LiveBiomassC = (AboveGroundLiveWt + roots.DMGreen) * CarbonFractionDM; //converting DM to C    (kgC/ha)

        // maintenance respiration
        //tempFactorRespiration = TemperatureEffectOnRespirationOld();
        tempFactorRespiration = TemperatureEffectOnRespiration();
        if (LiveBiomassC > 0.0)
            Resp_m = maintRespiration * tempFactorRespiration * NcFactor * LiveBiomassC;
        else
            Resp_m = 0.0;

        // growth respiration
        Resp_g = Pgross * (1 - growthEfficiency);
    }

    /// <summary>
    /// Plant net potential growth
    /// </summary>
    /// <returns>net potential growth</returns>
    internal double DailyPotentialGrowth()
    {
        // ** C budget is not explicitly done here as in EM
        Cremob = 0.0; // Nremob* C2N_protein;    // No carbon budget here
        // Nu_remob[elC] := C2N_protein * Nu_remob[elN];
        // need to substract CRemob from dm rutnover?

        // N fixation costs
        costNFixation = 0.0;
        if (isLegume)
        {
            if (NFixationCostMethod == 1)
                costNFixation = NFixationCost_M1(Pgross);
            else if (NFixationCostMethod == 2)
                costNFixation = NFixationCost_M2();
        }

        // Net potential growth (C) of the day (excluding growth respiration)
        dGrowthPot = Pgross + Cremob - Resp_g - Resp_m - costNFixation;
        dGrowthPot = Math.Max(0.0, dGrowthPot);

        if (dGrowthPot > 0.0)
        {
            //convert C to DM
            dGrowthPot /= CarbonFractionDM;

            // phenologically related reduction of annual species (from IJ)
            if (isAnnual)
                dGrowthPot = annualSpeciesReduction();
        }

        return dGrowthPot;
    }

    /// <summary>
    /// Daily growth after moisture factors
    /// </summary>
    /// <returns>Pot growth</returns>
    internal double DailyGrowthW()
    {
        dGrowthW = dGrowthPot * Math.Pow(Math.Min(glfWater, glfAeration), waterStressFactor);

        return dGrowthW;
    }

    /// <summary>
    /// Daily growth after nutrient factors
    /// </summary>
    /// <returns>Actual growth</returns>
    internal double DailyGrowthAct()
    {
        double gfnit = 0.0;
        if (isLegume)
            gfnit = glfN; //legume no dilution, but reducing more DM (therefore LAI)
        else
            gfnit = Math.Pow(glfN, NdilutCoeff);
                // more DM growth than N limited, due to dilution (typically NdilutCoeff = 0.5)

        dGrowth = dGrowthW * Math.Min(gfnit, GLFSFertility);
        return dGrowth;

        //RCichota, Jan/2014: updated the function, added account for Frgr (now GLFSFertility)
    }

    /// <summary>
    /// Partition DM from new growth
    /// </summary>
    internal void PartitionDMGrown()
    {
        //Leaf appearance rate is modified by temp & water stress
        //double rateLeaf = leafRate * GFT * (Math.Pow(gfwater, 0.33333));  //why input is 3
        //if (rateLeaf < 0.0) rateLeaf = 0.0;
        //if (rateLeaf > 1.0) rateLeaf = 1.0;

        if (dGrowth > 0.0) // if no net growth, then skip "partition" part
        {
            // fShoot was calculated on CalcNdemand()
            // Calc new fLeaf

            // Fractions of new growth to be allocated to the 1st tissue pools
            double toRoot = 1.0 - fShoot;
            double toStol = fShoot * fStolon;
            double toLeaf = fShoot * fLeaf;
            double toStem = fShoot * (1.0 - fStolon - fLeaf);

            // checking
            double ToAll = toLeaf + toStem + toStol + toRoot;
            if (Math.Abs(ToAll - 1.0) > 0.0001)
                throw new Exception("Mass balance lost during partition of new growth DM");

            // Allocate the partitioned growth to the 1st tissue pools
            leaves.tissue[0].DM += toLeaf * dGrowth;
            stems.tissue[0].DM += toStem * dGrowth;
            stolons.tissue[0].DM += toStol * dGrowth;
            roots.tissue[0].DM += toRoot * dGrowth;
            dGrowthHerbage = (toLeaf + toStem + toStol) * dGrowth;

            // Partitioing N, based on DM and [N] in each plant part
            double myNsum = (toLeaf * leaves.NConcMaximum)
                            + (toStem * stems.NConcMaximum)
                            + (toStol * stolons.NConcMaximum)
                            + (toRoot * roots.NConcMaximum);
            if (myNsum > 0.0)
            {
                double toLeafN = toLeaf * leaves.NConcMaximum / myNsum;
                double toStemN = toStem * stems.NConcMaximum / myNsum;
                double toStolN = toStol * stolons.NConcMaximum / myNsum;
                double toRootN = toRoot * roots.NConcMaximum / myNsum;

                // checking
                ToAll = toLeafN + toStemN + toStolN + toRootN;
                if (Math.Abs(ToAll - 1.0) > 0.0001)
                    throw new Exception("Mass balance lost during partition of new growth N");

                // Allocate new N to the 1st tissue pools
                leaves.tissue[0].Namount += toLeafN * newGrowthN;
                stems.tissue[0].Namount += toStemN * newGrowthN;
                stolons.tissue[0].Namount += toStolN * newGrowthN;
                roots.tissue[0].Namount += toRootN * newGrowthN;
            }

            // fraction of Nremob not used on growth that is added to dead tissue (remaining goes into litter)
            double leftoverNremob = Nremob * Kappa4;
            if (leftoverNremob > 0.0)
            {
                double myDMsum = leaves.tissue[3].DM + stems.tissue[3].DM;
                leaves.tissue[3].Namount += leftoverNremob * MathUtility.Divide(leaves.tissue[3].DM, myDMsum, 0.0);
                stems.tissue[3].Namount += leftoverNremob * MathUtility.Divide(stems.tissue[3].DM, myDMsum, 0.0);
            }

            // check whether luxury N was remobilised during N balance
            if (NLuxuryRemob2 + NLuxuryRemob3 > 0.0)
            {
                // partition any used N into plant parts (by N content)
                myNsum = leaves.tissue[1].Namount + stems.tissue[1].Namount + stolons.tissue[1].Namount;
                if (NLuxuryRemob2 > 0.0 && myNsum > 0.0)
                {
                    leaves.tissue[1].Namount -= NLuxuryRemob2 * leaves.tissue[1].Namount / myNsum;
                    stems.tissue[1].Namount -= NLuxuryRemob2 * stems.tissue[1].Namount / myNsum;
                    stolons.tissue[1].Namount -= NLuxuryRemob2 * stolons.tissue[1].Namount / myNsum;
                }

                myNsum = leaves.tissue[2].Namount + stems.tissue[2].Namount + stolons.tissue[2].Namount;
                if (NLuxuryRemob3 > 0.0 && myNsum > 0.0)
                {
                    leaves.tissue[2].Namount -= NLuxuryRemob3 * leaves.tissue[2].Namount / myNsum;
                    stems.tissue[2].Namount -= NLuxuryRemob3 * stems.tissue[2].Namount / myNsum;
                    stolons.tissue[2].Namount -= NLuxuryRemob3 * stolons.tissue[2].Namount / myNsum;
                }
            }
        }
    }

    /// <summary>
    /// Tissue turnover among the 12 biomass pools
    /// </summary>
    internal void TissueTurnover()
    {
        // The turnover rates are affected by soil water and air temperature
        //  the number of leaves per tiller also influences the rate (3 stage pools are used to describe any number of leaves)

        // Get the temperature factor for tissue turnover
        tempFacTTurnover = TemperatureEffectOnTissueTurnover();

        // Get the moisture factor for tissue turnover
        swFacTTurnover = MoistureEffectOnTissueTurnover();

        // Get the moisture factor for littering rate
        double swFacTTDead = Math.Pow(Math.Min(glfWater, glfAeration), exponentGLFW2dead);

        // Consider the number of leaves
        double leafFac = 3.0 / liveLeavesPerTiller; // three refers to the number of stages used in the model

        // Leaf and stems turnover rate
        gama = refTissueTurnoverRate * tempFacTTurnover * swFacTTurnover * leafFac;

        // Stolons turnover rate (legumes)
        if (isLegume)
            gamaS = refTurnoverRateStolon * tempFacTTurnover * swFacTTurnover * leafFac;

        // Littering rate
        gamaD = refLitteringRate * swFacTTDead * digestDead / 0.4;

        // Adjust littering rate due to stock trampling
        gamaD += stockParameter * stockingRate;

        // Roots turnover rate
        gamaR = tempFacTTurnover * (2 - Math.Min(glfWater, glfAeration)) * rateRootSen;

        if (gama == 0.0)
        {
            //if gama ==0 due to gftt or gfwt, then skip "turnover" part
            dDMLitter = 0.0;
            dNLitter = 0.0;
            dDMRootSen = 0.0;
            dNRootSen = 0.0;
        }
        else
        {
            if (isAnnual)
            {
                // Adjust turnover rate for annuals
                if (phenoStage == 1)
                {
                    //vegetative
                    double Kv = (double) daysfromEmergence / daysEmgToAnth;
                    gama *= Kv;
                    gamaR *= Kv;
                }
                else if (phenoStage == 2)
                {
                    //reproductive
                    double Kr = (double) daysfromAnthesis / daysToMature;
                    //gama = 1 - (1 - gama) * (1 - Kr * Kr);
                    gama *= 1 - (Kr * Kr);
                    gama += Kr * Kr;
                }
            }

            // Get daily defoliation factor
            double defoliationFactor = MathUtility.Divide(prevState.dmdefoliated,
                prevState.dmdefoliated + prevState.dmshoot, 0.0);

            // Increase stolon senescence if there was defoliation
            if (isLegume)
                gamaS += defoliationFactor * (1 - gamaS);

            // If today's turnover will result in a dmgreenShoot < dmgreen_minimum, then adjust the rates,
            // Possibly will have to skip this for annuals to allow them to die - phenololgy-related?
            double dmgreenToBe = AboveGroundLiveWt + dGrowth - (gama * (prevState.leaves.tissue[2].DM +
                                                                   prevState.stems.tissue[2].DM +
                                                                   prevState.stolons.tissue[2].DM));
            if (dmgreenToBe < dmgreenmin)
            {
                if (gama > 0.0)
                {
                    if (AboveGroundLiveWt + dGrowth < dmgreenmin)
                    {
                        gama = 0.0;
                        gamaS = 0.0;
                        gamaR = 0.0;
                    }
                    else
                    {
                        double gama_adj = MathUtility.Divide(AboveGroundLiveWt + dGrowth - dmgreenmin,
                            prevState.leaves.tissue[2].DM +
                            prevState.stems.tissue[2].DM +
                            prevState.stolons.tissue[2].DM, 0.0);
                        gamaR *= gama_adj / gama;
                        gama = gama_adj;
                    }
                }
            }

            // consider a minimum for roots too
            if (roots.DMGreen < 0.5 * dmgreenmin)
            {
                gamaR = 0.0;
            }

            //// Do the actual turnover, update DM and N

            // Leaves
            double DMfrom1to2 = facGrowingTissue * gama * prevState.leaves.tissue[0].DM;
            double DMfrom2to3 = gama * prevState.leaves.tissue[1].DM;
            double DMfrom3to4 = gama * prevState.leaves.tissue[2].DM;
            double DMfrom4toL = gamaD * prevState.leaves.tissue[3].DM;

            leaves.tissue[0].DM += 0.0 - DMfrom1to2; // DM in was considered in PartitionDMGrown()
            leaves.tissue[1].DM += DMfrom1to2 - DMfrom2to3;
            leaves.tissue[2].DM += DMfrom2to3 - DMfrom3to4;
            leaves.tissue[3].DM += DMfrom3to4 - DMfrom4toL;
            dGrowthHerbage -= DMfrom4toL;
            dDMLitter = DMfrom4toL;

            double Nfrom1to2 = prevState.leaves.tissue[0].Nconc * DMfrom1to2;
            double Nfrom2to3 = prevState.leaves.tissue[1].Nconc * DMfrom2to3;
            double Nfrom3to4 = leaves.NConcMinimum * DMfrom3to4;
            double Nfrom4toL = prevState.leaves.tissue[3].Nconc * DMfrom4toL;
            Nleaf3Remob = (prevState.leaves.tissue[2].Nconc - leaves.NConcMinimum) * DMfrom3to4;

            leaves.tissue[0].Namount += 0.0 - Nfrom1to2; // N in was considered in PartitionDMGrown()
            leaves.tissue[1].Namount += Nfrom1to2 - Nfrom2to3;
            leaves.tissue[2].Namount += Nfrom2to3 - Nfrom3to4 - Nleaf3Remob;
            leaves.tissue[3].Namount += Nfrom3to4 - Nfrom4toL;
            dNLitter = Nfrom4toL;

            // Stems
            DMfrom1to2 = facGrowingTissue * gama * prevState.stems.tissue[0].DM;
            DMfrom2to3 = gama * prevState.stems.tissue[1].DM;
            DMfrom3to4 = gama * prevState.stems.tissue[2].DM;
            DMfrom4toL = gamaD * prevState.stems.tissue[3].DM;

            stems.tissue[0].DM += 0.0 - DMfrom1to2; // DM in was considered in PartitionDMGrown()
            stems.tissue[1].DM += DMfrom1to2 - DMfrom2to3;
            stems.tissue[2].DM += DMfrom2to3 - DMfrom3to4;
            stems.tissue[3].DM += DMfrom3to4 - DMfrom4toL;
            dGrowthHerbage -= DMfrom4toL;
            dDMLitter += DMfrom4toL;

            Nfrom1to2 = prevState.stems.tissue[0].Nconc * DMfrom1to2;
            Nfrom2to3 = prevState.stems.tissue[1].Nconc * DMfrom2to3;
            Nfrom3to4 = stems.NConcMinimum * DMfrom3to4;
            Nfrom4toL = prevState.stems.tissue[3].Nconc * DMfrom4toL;
            Nstem3Remob = (prevState.stems.tissue[2].Nconc - stems.NConcMinimum) * DMfrom3to4;

            stems.tissue[0].Namount += 0.0 - Nfrom1to2; // N in was considered in PartitionDMGrown()
            stems.tissue[1].Namount += Nfrom1to2 - Nfrom2to3;
            stems.tissue[2].Namount += Nfrom2to3 - Nfrom3to4 - Nstem3Remob;
            stems.tissue[3].Namount += Nfrom3to4 - Nfrom4toL;
            dNLitter += Nfrom4toL;

            // Stolons
            if (isLegume)
            {
                DMfrom1to2 = facGrowingTissue * gamaS * prevState.stolons.tissue[0].DM;
                DMfrom2to3 = gamaS * prevState.stolons.tissue[1].DM;
                double DMfrom3toL = gamaS * prevState.stolons.tissue[2].DM;

                stolons.tissue[0].DM += 0.0 - DMfrom1to2; // DM in was considered in PartitionDMGrown()
                stolons.tissue[1].DM += DMfrom1to2 - DMfrom2to3;
                stolons.tissue[2].DM += DMfrom2to3 - DMfrom3toL;
                dGrowthHerbage -= DMfrom3toL;
                dDMLitter += DMfrom3toL;

                Nfrom1to2 = prevState.stolons.tissue[0].Nconc * DMfrom1to2;
                Nfrom2to3 = prevState.stolons.tissue[1].Nconc * DMfrom2to3;
                Nfrom3to4 = 0.5 * (prevState.stolons.tissue[2].Nconc + stolons.NConcMinimum) * DMfrom3toL;
                Nstol3Remob = 0.5 * (prevState.stolons.tissue[2].Nconc - stolons.NConcMinimum) * DMfrom3toL;

                stolons.tissue[0].Namount += 0.0 - Nfrom1to2; // N in was considered in PartitionDMGrown()
                stolons.tissue[1].Namount += Nfrom1to2 - Nfrom2to3;
                stolons.tissue[2].Namount += Nfrom2to3 - Nfrom3to4 - Nstol3Remob;
                dNLitter += Nfrom3to4;

                // Add stuff from dead material (should only have values if KillCrop was used)
                if (stolons.tissue[3].DM > 0.0)
                {
                    dDMLitter += stolons.tissue[3].DM;
                    dNLitter += stolons.tissue[3].Namount;
                    stolons.tissue[3].DM = 0.0;
                    stolons.tissue[3].Namount = 0.0;
                }
            }

            // Roots
            dDMRootSen = gamaR * prevState.roots.tissue[0].DM;
            roots.tissue[0].DM -= dDMRootSen;
            NrootRemob = 0.5 * (prevState.roots.tissue[0].Nconc - roots.NConcMinimum) * dDMRootSen;
            dNRootSen = prevState.roots.tissue[0].Nconc * dDMRootSen - NrootRemob;
            roots.tissue[0].Namount -= prevState.roots.tissue[0].Nconc * dDMRootSen;

            // Add stuff from dead material (should only have values if KillCrop was used)
            if (roots.tissue[1].DM > 0.0)
            {
                dDMRootSen += roots.tissue[1].DM;
                dNRootSen += roots.tissue[1].Namount;
                roots.tissue[1].DM = 0.0;
                roots.tissue[1].Namount = 0.0;
            }

            // fraction of Nremob not used in growth that is added to litter
            double leftoverNremob = Nremob * (1 - Kappa4);
            dNLitter += leftoverNremob;
            //The leftover 'Nremob' of previous day (if>0) indicates more N should go to litter in previous day, so do it now

            // remobilised and remobilisable N (these will be used tomorrow)
            Nremob = Nleaf3Remob + Nstem3Remob + Nstol3Remob + NrootRemob;
            NLuxury2 = Math.Max(0.0, leaves.tissue[1].Nconc - leaves.NConcOptimum * NcRel2) * leaves.tissue[1].DM +
                       Math.Max(0.0, stems.tissue[1].Nconc - stems.NConcOptimum * NcRel2) * stems.tissue[1].DM +
                       Math.Max(0.0, stolons.tissue[1].Nconc - stolons.NConcOptimum * NcRel2) * stolons.tissue[1].DM;
            NLuxury3 = Math.Max(0.0, leaves.tissue[2].Nconc - leaves.NConcOptimum * NcRel3) * leaves.tissue[2].DM +
                       Math.Max(0.0, stems.tissue[2].Nconc - stems.NConcOptimum * NcRel3) * stems.tissue[2].DM +
                       Math.Max(0.0, stolons.tissue[2].Nconc - stolons.NConcOptimum * NcRel3) * stolons.tissue[2].DM;
            // only a fraction of luxury N is available for remobilisation:
            NLuxury2 *= Kappa2;
            NLuxury3 *= Kappa3;

            //Sugar remobilisation and C balance:
            Cremob = 0.0; // not explicitly considered
        }
    }

    /// <summary>
    /// Update aggregated variables
    /// </summary>
    internal void UpdateAggregatedVariables() //update DM, N
    {
        //// - LAI  ------------------------------------------------------
        greenLAI = GreenLAI();
        deadLAI = DeadLAI();
        totalLAI = greenLAI + deadLAI;

        //// - Plant height  ---------------------------------------------
        if (usingSpeciesHeight)
        {
            height = HeightfromDM();
        }

        //// - Root distribution  ----------------------------------------
        if (usingSpeciesRoot)
        {
            // Add new growth
            rootFraction = RootGrowthDistribution(dGrowth);
            // Subtract senescence
            rootFraction = RootGrowthDistribution(-dDMRootSen);
        }
    }

    /// <summary>
    /// Root growth
    /// </summary>
    /// <returns>root depth</returns>
    internal double rootGrowth()
    {
        if (isAnnual)
        {
            rootDepth = 50 + (maxRootDepth - 50) * MathUtility.Divide(daysfromEmergence, daysEmgToAnth, 1.0);
            layerBottomRootZone = GetRootZoneBottomLayer();
            //considering root distribution change, here?
        }
        else
        {
            if ((phenoStage > 0) && (rootDepth < maxRootDepth))
            {
                double tempFactor = GFTemperature(Tmean);
                dRootDepth = rootElongationRate * tempFactor;
                rootDepth = Math.Min(maxRootDepth, Math.Max(minRootDepth, rootDepth + dRootDepth));
            }
            else
            {
                dRootDepth = 0.0;
                rootDepth = 0.0;
            }
        }

        return rootDepth;
    }

    #endregion

    #region - Handling and auxilary processes  -------------------------------------------------------------------------

    /// <summary>
    /// Set germination
    /// </summary>
    internal void SetInGermination()
    {
        bSown = true;
        phenoStage = 0; //before germination
    }

    /// <summary>
    /// Set state
    /// </summary>
    internal void SetEmergenceState()
    {
        phenoStage = 1;

        leaves.tissue[0].DM = emergenceDM[0];
        leaves.tissue[1].DM = emergenceDM[1];
        leaves.tissue[2].DM = emergenceDM[2];
        leaves.tissue[3].DM = emergenceDM[3];
        stems.tissue[0].DM = emergenceDM[4];
        stems.tissue[1].DM = emergenceDM[5];
        stems.tissue[2].DM = emergenceDM[6];
        stems.tissue[3].DM = emergenceDM[7];
        stolons.tissue[0].DM = emergenceDM[8];
        stolons.tissue[1].DM = emergenceDM[9];
        stolons.tissue[2].DM = emergenceDM[10];
        stolons.tissue[3].DM = 0.0;
        roots.tissue[0].DM = emergenceDM[11];
        roots.tissue[1].DM = 0.0;

        //Init total N in each pool
        leaves.tissue[0].Nconc = leaves.NConcOptimum;
        leaves.tissue[1].Nconc = leaves.NConcOptimum * NcRel2;
        leaves.tissue[2].Nconc = leaves.NConcOptimum * NcRel3;
        leaves.tissue[3].Nconc = leaves.NConcMinimum;
        stems.tissue[0].Nconc = stems.NConcOptimum;
        stems.tissue[1].Nconc = stems.NConcOptimum * NcRel2;
        stems.tissue[2].Nconc = stems.NConcOptimum * NcRel3;
        stems.tissue[3].Nconc = stems.NConcMinimum;
        stolons.tissue[0].Nconc = stolons.NConcOptimum;
        stolons.tissue[1].Nconc = stolons.NConcOptimum * NcRel2;
        stolons.tissue[2].Nconc = stolons.NConcOptimum * NcRel3;
        stolons.tissue[3].Nconc = 0.0;
        roots.tissue[0].Nconc = roots.NConcOptimum;
        roots.tissue[1].Nconc = 0.0;

        //calculated, DM and LAI,  species-specific
        UpdateAggregatedVariables();

        dGrowthPot = 0.0; // daily growth potential
        dGrowthW = 0.0; // daily growth considering only water deficit
        dGrowth = 0.0; // daily growth actual
        dGrowthRoot = 0.0; // daily root growth
        fShoot = 1.0; // actual fraction of dGrowth allocated to shoot
    }

    /// <summary>
    /// Get phenology state
    /// </summary>
    /// <returns>Phenology index</returns>
    internal int Phenology()
    {
        if (bSown && phenoStage == 0) //  before emergence
        {
            DDSfromSowing += Tmean;
            if (DDSfromSowing > DDSEmergence)
            {
                DDSfromSowing = 0;
                SetEmergenceState(); //Initial states at 50% emergence
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

    /// <summary>
    /// Annuals phenology
    /// </summary>
    /// <returns>true or false</returns>
    internal bool annualPhenology()
    {
        if (Clock.month == monEmerg && Clock.day_of_month == dayEmerg)
            phenoStage = 1; //vegetative stage
        else if (Clock.month == monAnth && Clock.day_of_month == dayAnth)
            phenoStage = 2; //reproductive

        if (phenoStage == 0) //before emergence
        {
            dGrowthPot = 0.0;
            return false; //no growth
        }

        if (phenoStage == 1) //vegetative
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
                dGrowthPot = 0.0;
                return false; // Flag no growth after mature
            }
            return true;
        }
        return true;
    }

    /// <summary>
    /// Reduction of growth in annual species, related to phenology
    /// </summary>
    /// <returns>Pot growth</returns>
    public double annualSpeciesReduction()
    {
        double rFactor = 1; // reduction factor of annual species
        if (phenoStage == 1 && daysfromEmergence < 60) //decline at the begining due to seed bank effects ???
        {
            rFactor = 0.5 + 0.5 * daysfromEmergence / 60;
        }
        else if (phenoStage == 2) //decline of photosynthesis when approaching maturity
        {
            rFactor = 1.0 - MathUtility.Divide(daysfromAnthesis, daysToMature, 0.0);
        }
        dGrowthPot *= rFactor;
        return dGrowthPot;
    }

    /// <summary>
    /// Compute the LAI value of green material
    /// </summary>
    /// <returns>green LAI</returns>
    internal double GreenLAI()
    {
        double myLAI = (0.0001 * leaves.DMGreen * specificLeafArea)
                       + (0.0001 * stolons.DMTotal * 0.3 * specificLeafArea);
        // 0.0001: kg/ha->kg/m2; SLA: m2/kg - assuming Mass2GLA = 0.3*SLA
        // Resilience after unfavoured conditions
        // Consider cover will be bigger for the same amount of DM when DM is low due to
        // - light extinction coefficient will be bigger - plant leaves will be more horizontal than in dense high swards
        // - more parts will turn green for photosysnthesis (?)
        // - quick response of plant shoots to favoured conditions after release of stress
        // » Specific leaf area should be reduced (RCichota2014)
        if (!isLegume && AboveGroundLiveWt < 1000)
        {
            myLAI += 0.0001 * stems.DMGreen * specificLeafArea * Math.Sqrt((1000 - AboveGroundLiveWt) / 1000);
        }

        return myLAI;
    }

    /// <summary>
    /// Compute the LAI value of dead material
    /// </summary>
    /// <returns>dead LAI</returns>
    internal double DeadLAI()
    {
        double myLAI = 0.0001 * leaves.tissue[3].DM * specificLeafArea;
        return myLAI;
    }

    #endregion

    #endregion

    #region Other processes  -------------------------------------------------------------------------------------------

    /// <summary>
    /// Move a given amount of DM and N from live to dead pools
    /// </summary>
    /// <param name="KillFaction">The fraction of each live pool to be moved into dead</param>
    internal void KillCrop(double KillFaction)
    {
        double fractionRemaining = 1.0 - KillFaction;
        leaves.tissue[3].DM += (leaves.tissue[0].DM + leaves.tissue[1].DM + leaves.tissue[2].DM) * KillFaction;
        leaves.tissue[0].DM *= fractionRemaining;
        leaves.tissue[1].DM *= fractionRemaining;
        leaves.tissue[2].DM *= fractionRemaining;
        stems.tissue[3].DM += (stems.tissue[0].DM + stems.tissue[1].DM + stems.tissue[2].DM) * KillFaction;
        stems.tissue[0].DM *= fractionRemaining;
        stems.tissue[1].DM *= fractionRemaining;
        stems.tissue[2].DM *= fractionRemaining;
        stolons.tissue[3].DM += (stolons.tissue[0].DM + stolons.tissue[1].DM + stolons.tissue[2].DM) * KillFaction;
        stolons.tissue[0].DM *= fractionRemaining;
        stolons.tissue[1].DM *= fractionRemaining;
        stolons.tissue[2].DM *= fractionRemaining;
        roots.tissue[1].DM += roots.tissue[0].DM * KillFaction;
        roots.tissue[0].DM *= fractionRemaining;

        // note: N concentration do not change for green material, so do not need to update N amount;
        leaves.tissue[3].Namount += (leaves.tissue[0].Namount + leaves.tissue[1].Namount + leaves.tissue[2].Namount) *
                                    KillFaction;
        leaves.tissue[0].Namount *= fractionRemaining;
        leaves.tissue[1].Namount *= fractionRemaining;
        leaves.tissue[2].Namount *= fractionRemaining;
        stems.tissue[3].Namount += (stems.tissue[0].Namount + stems.tissue[1].Namount + stems.tissue[2].Namount) *
                                   KillFaction;
        stems.tissue[0].Namount *= fractionRemaining;
        stems.tissue[1].Namount *= fractionRemaining;
        stems.tissue[2].Namount *= fractionRemaining;
        stolons.tissue[3].Namount += (stolons.tissue[0].Namount + stolons.tissue[1].Namount + stolons.tissue[2].Namount) *
                                     KillFaction;
        stolons.tissue[0].Namount *= fractionRemaining;
        stolons.tissue[1].Namount *= fractionRemaining;
        stolons.tissue[2].Namount *= fractionRemaining;
        roots.tissue[1].Namount += roots.tissue[0].Namount * KillFaction;
        roots.tissue[0].Namount *= fractionRemaining;

        UpdateAggregatedVariables();
        SetPrevPools();
    }

    /// <summary>
    /// Store state of various pools
    /// </summary>
    internal void SetPrevPools()
    {
        prevState.leaves.tissue[0].DM = leaves.tissue[0].DM;
        prevState.leaves.tissue[1].DM = leaves.tissue[1].DM;
        prevState.leaves.tissue[2].DM = leaves.tissue[2].DM;
        prevState.leaves.tissue[3].DM = leaves.tissue[3].DM;
        prevState.stems.tissue[0].DM = stems.tissue[0].DM;
        prevState.stems.tissue[1].DM = stems.tissue[1].DM;
        prevState.stems.tissue[2].DM = stems.tissue[2].DM;
        prevState.stems.tissue[3].DM = stems.tissue[3].DM;
        prevState.stolons.tissue[0].DM = stolons.tissue[0].DM;
        prevState.stolons.tissue[1].DM = stolons.tissue[1].DM;
        prevState.stolons.tissue[2].DM = stolons.tissue[2].DM;
        prevState.roots.tissue[0].DM = roots.tissue[0].DM; // only one pool for roots

        prevState.leaves.tissue[0].Namount = leaves.tissue[0].Namount;
        prevState.leaves.tissue[1].Namount = leaves.tissue[1].Namount;
        prevState.leaves.tissue[2].Namount = leaves.tissue[2].Namount;
        prevState.leaves.tissue[3].Namount = leaves.tissue[3].Namount;
        prevState.stems.tissue[0].Namount = stems.tissue[0].Namount;
        prevState.stems.tissue[1].Namount = stems.tissue[1].Namount;
        prevState.stems.tissue[2].Namount = stems.tissue[2].Namount;
        prevState.stems.tissue[3].Namount = stems.tissue[3].Namount;
        prevState.stolons.tissue[0].Namount = stolons.tissue[0].Namount;
        prevState.stolons.tissue[1].Namount = stolons.tissue[1].Namount;
        prevState.stolons.tissue[2].Namount = stolons.tissue[2].Namount;
        prevState.roots.tissue[0].Namount = roots.tissue[0].Namount; // only one pool for roots
    }

    /// <summary>
    /// Reset variables
    /// </summary>
    internal void ResetZero()
    {
        //Reset dm pools (kg/ha)
        leaves.tissue[0].DM = leaves.tissue[1].DM = leaves.tissue[2].DM = leaves.tissue[3].DM = 0.0;
        stems.tissue[0].DM = stems.tissue[1].DM = stems.tissue[2].DM = stems.tissue[3].DM = 0.0;
        stolons.tissue[0].DM = stolons.tissue[1].DM = stolons.tissue[2].DM = stolons.tissue[3].DM = 0.0;
        roots.tissue[0].DM = roots.tissue[1].DM = 0.0;

        dmdefoliated = 0.0;

        //Reset N pools
        leaves.tissue[0].Namount = leaves.tissue[1].Namount = leaves.tissue[2].Namount = leaves.tissue[3].Namount = 0.0;
        stems.tissue[0].Namount = stems.tissue[1].Namount = stems.tissue[2].Namount = stems.tissue[3].Namount = 0.0;
        stolons.tissue[0].Namount = stolons.tissue[1].Namount = stolons.tissue[2].Namount = stolons.tissue[3].Namount = 0.0;
        roots.tissue[0].Namount = roots.tissue[1].Namount = 0.0;

        Ndefoliated = 0.0;

        phenoStage = 0;
    }

    #endregion

    #region Functions  ----------------------------------------------------------------------------

    /// <summary>
    /// The mean temperature for the day
    /// </summary>
    private double Tmean
    {
        get { return 0.5 * (MetFile.MaxT + MetFile.MinT); }
    }

    /// <summary>
    /// Effects of atmospheric [CO2] on plant photosynthesis
    /// </summary>
    /// <returns>CO2 effect on photosynthesis</returns>
    private double PCO2Effects()
    {
        double termActual = (CO2 + CO2PmaxScale) / CO2;
        double termReference = (referenceCO2 + CO2PmaxScale) / referenceCO2;
        return termReference / termActual;
    }

    /// <summary>
    /// Plant nitrogen [N] decline due to elevated [CO2]
    /// </summary>
    /// <returns>CO2 effect on plant N concentration</returns>
    private double NCO2Effects()
    {
        if (CO2 <= referenceCO2)
            return 1.0;

        double factorCO2 = Math.Pow((CO2NScale - referenceCO2) / (CO2 - referenceCO2), CO2NCurvature);
        double Fn = (CO2NMin + factorCO2) / (1 + factorCO2);
        return Fn;
    }

    /// <summary>
    /// Canopy conductance decline to elevated [CO2]
    /// </summary>
    /// <returns>CO2 effects on canopy conductance</returns>
    private double ConductanceCO2Effects()
    {
        if (Math.Abs(CO2 - referenceCO2) < 0.5)
            return 1.0;
        //Hard coded here, not used, should go to Micromet!
        double Gmin = 0.2; //Fc = Gmin when CO2->unlimited
        double Gmax = 1.25; //Fc = Gmax when CO2 = 0;
        double beta = 2.5; //curvature factor,

        double Fc = Gmin + (Gmax - Gmin) * (1 - Gmin) * Math.Pow(referenceCO2, beta) /
                    ((Gmax - 1) * Math.Pow(CO2, beta) + (1 - Gmin) * Math.Pow(referenceCO2, beta));
        return Fc;
    }

    /// <summary>
    /// Computes the N demand for potential growth (at optimum and luxury N levels)
    /// </summary>
    internal void CalcTotalNDemand()
    {
        fShoot = NewGrowthToShoot();
        double fL = UpdatefLeaf(); //to consider more dm to leaf when DM is lower?
        fLeaf = maxFLeaf;

        double toRoot = dGrowthW * (1.0 - fShoot);
        double toStol = dGrowthW * fShoot * fStolon;
        double toLeaf = dGrowthW * fShoot * fLeaf;
        double toStem = dGrowthW * fShoot * (1.0 - fStolon - fLeaf);

        //N demand for new growth, optimum N (kg/ha)
        NdemandOpt = (toLeaf * leaves.NConcOptimum) + (toStem * stems.NConcOptimum)
                   + (toStol * stolons.NConcOptimum) + (toRoot * roots.NConcOptimum);

        NdemandOpt *= NCO2Effects();
        //this will reduce the N stress under under elevated [co2] for the same soilN

        //N demand for new growth assuming luxury uptake (maximum [N])
        NdemandLux = (toLeaf * leaves.NConcMaximum) + (toStem * stems.NConcMaximum)
                   + (toStol * stolons.NConcMaximum) + (toRoot * roots.NConcMaximum);
        //luxury uptake is not affected by [co2]
    }

    /// <summary>
    /// Computes amount of biologically fixed N
    /// </summary>
    internal void CalcNFixation()
    {
        double SoilNavailable = soilAvailableNH4.Sum() + soilAvailableNO3.Sum();
        double Nstress = 1.0;
        if (NdemandOpt > 0.0 && (NdemandOpt > SoilNavailable + MinFix))
            Nstress = SoilNavailable / (NdemandOpt - MinFix);

        if (Nstress <= 0.999)
        {
            // more fixation under N stress
            double moreNfixation = (MaxFix - MinFix) * (1 - Nstress);
            moreNfixation = Math.Max(0.0, Math.Min(1.0, moreNfixation));
            NFixed = (MinFix + moreNfixation) * NdemandOpt;
        }
        else
        {
            // minimum fixation even if not needed
            NFixed = MinFix * NdemandOpt;
        }
    }

    /// <summary>
    /// Computes the amount of N remobilised from senescent material used in new growth
    /// </summary>
    internal void CalcNRemobSenescent()
    {
        if (NdemandLux <= Nremob + NFixed)
        {
            // Nremob and/or Nfix are able to supply all N
            remob2NewGrowth = Math.Max(0.0, NdemandLux - NFixed);
            Nremob -= remob2NewGrowth;
        }
        else
        {
            // not enough N within the plant, uptake is needed
            remob2NewGrowth = Nremob;
            Nremob = 0.0;
        }

        newGrowthN = remob2NewGrowth + NFixed;
    }

    /// <summary>
    /// Computes the amount of N remobilisation from luxury N to be used in new growth
    /// </summary>
    internal void CalcNRemobLuxury()
    {
        double remainingNdemand = NdemandOpt - newGrowthN;
        if (remainingNdemand <= NLuxury2 + NLuxury3)
        {
            // there is luxury N that can be used for optimum growth
            if (remainingNdemand <= NLuxury3)
            {
                // tissue 3 is able to supply all
                NLuxuryRemob3 = remainingNdemand;
                NLuxuryRemob2 = 0.0;
                remainingNdemand = 0.0;
            }
            else
            {
                // get first from tissue 3, then from tissue 2

                NLuxuryRemob3 = NLuxury3;
                remainingNdemand -= NLuxury3;
                NLuxuryRemob2 = remainingNdemand;
                remainingNdemand = 0.0;
            }
        }
        else
        {
            // N luxury is not enough for optimum growth, use up all there is
            if (NLuxury2 + NLuxury3 > 0.0)
            {
                NLuxuryRemob3 = NLuxury3;
                remainingNdemand -= NLuxury3;
                NLuxuryRemob2 = NLuxury2;
                remainingNdemand -= NLuxury2;
            }
        }

        newGrowthN += NLuxuryRemob3 + NLuxuryRemob2;
    }

    /// <summary>
    /// Computes the amount of N taken up from soil
    /// </summary>
    internal void CalcNUptake()
    {
        double soilNavailable = soilAvailableNH4.Sum() + soilAvailableNO3.Sum();
        double soilNuptake = 0.0;
        if (soilNavailable >= soilNdemand)
        {
            // soil can supply all N needed
            soilNuptake = soilNdemand;
        }
        else
        {
            // soil cannot supply all N needed. Uptake the available
            soilNuptake = soilNavailable;
        }

        newGrowthN += soilNuptake;

        if (soilNuptake > 0.0)
        {
            // values for each N form
            double nFormFrac = Math.Min(1.0, MathUtility.Divide(soilAvailableNH4.Sum(), soilNavailable, 0.0));
            soilNH4Uptake = soilNuptake * nFormFrac;
            soilNO3Uptake = soilNuptake * (1.0 - nFormFrac);
        }
    }

    /// <summary>
    /// New fraction to leaves
    /// </summary>
    /// <returns>fLeaf</returns>
    private double UpdatefLeaf()
    {
        double fL = maxFLeaf; //fraction of shoot that goes to leaf

        if ((minFLeaf < maxFLeaf) && (AboveGroundLiveWt > dmMaxFLeaf))
        {
            double dmAux = Math.Pow((AboveGroundLiveWt - dmMaxFLeaf) / (dmReferenceFLeaf - dmMaxFLeaf), exponentFLeaf);
            fL = minFLeaf + (maxFLeaf - minFLeaf) / (1 + dmAux);
        }
        return fL;
    }

    /// <summary>
    /// N concentration effects on photosyntesis
    /// </summary>
    /// <returns>N effect</returns>
    private double PmxNeffect()
    {
        double Fn = NCO2Effects();

        double effect = 1.0;
        if (!isAnnual) //  &&and FVegPhase and ( VegDay < 10 ) ) then  // need this or it never gets going
        {
            if (leaves.DMGreen > 0.0)
            {
                if (leaves.NconcGreen < leaves.NConcOptimum * Fn) //Fn
                {
                    if (leaves.NconcGreen > leaves.NConcMinimum)
                    {
                        //effect = Math.Min(1.0, Ncleaf_green / NcleafOpt*Fn);
                        effect = Math.Min(1.0,
                            MathUtility.Divide(leaves.NconcGreen - leaves.NConcMinimum, leaves.NConcOptimum * Fn - leaves.NConcMinimum, 0.0));
                    }
                    else
                    {
                        effect = 0.0;
                    }
                }
            }
        }
        return effect;
    }

    /// <summary>
    /// Cost of N fixation
    /// </summary>
    /// <remarks>
    /// Original method (F. Li), modified by RCichota to return an equivalent to respiration
    /// </remarks>
    /// <returns>Carbon spent on N fixation</returns>
    private double NFixationCost_M1(double GrowthC)
    {
        //  reduction of net production as cost of N-fixing
        double costFactor = 0.0;
        if (NdemandOpt > 0.0)
        {
            double actFix = NFixed / NdemandOpt;
            costFactor = NFixCostMax * (actFix - MinFix) / (MaxFix - MinFix);
        }

        return GrowthC * Math.Min(1.0, Math.Max(0.0, costFactor));
    }

    /// <summary>
    /// Calculates the costs of N fixation
    /// </summary>
    /// <remarks>
    /// Approach separates maintenance and activity costs, based roughly on results from:
    /// Rainbird RM, Hitz WD, Hardy RWF 1984. Experimental determination of the respiration associated with soybean/rhizobium nitrogenase 
    ///    function, nodule maintenance, and total nodule nitrogen fixation. Plant Physiology 75(1): 49-53.
    /// Voisin AS, Salon C, Jeudy C, Warembourg FR 2003. Symbiotic N2 fixation activity in relation to C economy of Pisum sativum L. as a
    ///    function of plant phenology. Journal of Experimental Botany 54(393): 2733-2744.
    /// Minchin FR, Witty JF 2005. Respiratory/carbon costs of symbiotic nitrogen fixation in legumes. In: Lambers H, Ribas-Carbo M eds. 
    ///    Plant Respiration. Advances in Photosynthesis and Respiration, Springer Netherlands. Pp. 195-205.
    /// </remarks>
    /// <returns>Carbon spent on N fixation</returns>
    private double NFixationCost_M2()
    {
        //  respiration cost of symbiont (presence of rhizobia is assumed to be proportional to root mass)
        double Tfactor = GFTemperature(Tmean);
        double maintenanceCost = roots.DMGreen * CarbonFractionDM * symbiontCostFactor * Tfactor;

        //  respiration cost of N fixation (assumed as a simple function of N fixed)
        double activityCost = NFixed * NFixingCostFactor;

        return maintenanceCost + activityCost;
    }

    /// <summary>
    /// Calculate the fraction of new growth allocated to shoot
    /// </summary>
    /// <returns>fShoot</returns>
    private double NewGrowthToShoot()
    {
        // shoot/root ratio for today's DM partition
        double todaysSR = targetSRratio;

        if (roots.DMGreen > 0.00001)
        {
            double fac = 1.0; //day-to-day fraction of reduction
            //double minF = allocationSeasonF;    //default = 0.8;
            double doy = Clock.day_of_month + (int) ((Clock.month - 1) * 30.5);
            // NOTE: the type for doy has to be double or the divisions below will be rounded (to int) and thus be [slightly] wrong

            double doyC = startHighAllocation; // Default as in South-hemisphere: 232
            int doyEoY = 365 + (DateTime.IsLeapYear(Clock.year) ? 1 : 0);
            int[] ReproSeasonIntval = new int[3]; // { 35, 60, 30 };
            double allocationIncrease = allocationSeasonF;
            ReproSeasonIntval[0] = (int) (durationHighAllocation * shoulderHighAllocation * 1.17);
            ReproSeasonIntval[1] = (int) durationHighAllocation;
            ReproSeasonIntval[2] = (int) (durationHighAllocation * shoulderHighAllocation);

            if (usingLatFunctionFShoot)
            {
                int doyWinterSolstice = (MetFile.Latitude < 0.0) ? 171 : 354;
                // compute the day to start the period with higher DM allocation to shoot
                double doyIniPlateau = doyWinterSolstice;
                if (Math.Abs(MetFile.Latitude) > referenceLatitude)
                    doyIniPlateau += 183;
                else
                {
                    double myB = Math.Abs(MetFile.Latitude) / referenceLatitude;
                    doyIniPlateau += 183 * (paramALatFunction - (paramALatFunction * myB) + myB) *
                                     Math.Pow(myB, paramALatFunction - 1.0);
                }

                // compute the duration of the three phases (onset, plateau, and outset)
                double maxPlateauPeriod = doyEoY - 2 * maxShoulderLatFunction;
                ReproSeasonIntval[1] =
                    (int)
                        (minPlateauLatFunction +
                         (maxPlateauPeriod - minPlateauLatFunction) *
                         Math.Pow(1 - Math.Abs(MetFile.Latitude) / 90, paramBLatFunction));
                ReproSeasonIntval[0] =
                    (int) Math.Min(maxShoulderLatFunction, ReproSeasonIntval[1] * onsetFacLatFunction);
                ReproSeasonIntval[2] =
                    (int) Math.Min(maxShoulderLatFunction, ReproSeasonIntval[1] * outsetFacLatFunction);
                if (ReproSeasonIntval.Sum() > doyEoY)
                    throw new Exception("Error when calculating period with high DM allocation, greater then one year");

                doyC = doyIniPlateau - ReproSeasonIntval[0];
                // compute the factor to augment allocation
                allocationIncrease = allocationMax;
                if (Math.Abs(MetFile.Latitude) < referenceLatitude)
                {
                    double myB = Math.Abs(MetFile.Latitude) / referenceLatitude;
                    allocationIncrease *= (paramCLatFunction - (paramCLatFunction * myB) + myB) *
                                          Math.Pow(myB, paramCLatFunction - 1.0);
                }
            }

            //int doyF = doyC + 35;   //75
            //int doyD = doyC + 95;   // 110;
            //int doyE = doyC + 125;  // 140;
            //if (doyE > 365) doyE = doyE - 365;

            int doyF = (int) doyC + ReproSeasonIntval[0];
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

            // update todays shoot/root partition
            todaysSR = fac * targetSRratio;

            // get the soil related growth limiting factor (the smaller this is the higher the allocation of DM to roots)
            double GFmin = Math.Min(glfWater, glfN);

            // get the current shoot/root ratio (the smaller this is the higher the allocation of DM to shoot)
            double presentSR = MathUtility.Divide(AboveGroundLiveWt, roots.DMGreen, 1000.0);

            // update todays shoot/root partition
            todaysSR *= GFmin * todaysSR / presentSR;

            // compute fraction to shoot
            fShoot = todaysSR / (1.0 + todaysSR);
        }
        else
        {
            fShoot = 1.0; // this should not happen (might happen if plant is dead)
        }

        // check for maximum root allocation (kept here mostly for backward compatibility)
        if ((1 - fShoot) > maxRootFraction)
            fShoot = 1 - maxRootFraction;

        return fShoot;
    }

    /// <summary>
    /// Gets the green cover
    /// </summary>
    internal double coverGreen
    {
        get { return (1.0 - Math.Exp(-lightExtCoeff * greenLAI)); }
    }

    /// <summary>
    /// Gets the dead cover
    /// </summary>
    internal double coverDead
    {
        get { return (1.0 - Math.Exp(-lightExtCoeff * deadLAI)); }
    }

    /// <summary>
    /// Gets the total cover
    /// </summary>
    internal double coverTotal
    {
        get { return (1.0 - (Math.Exp(-lightExtCoeff * totalLAI))); }
    }

    /// <summary>
    /// Growth limiting factor due to temperature
    /// </summary>
    /// <param name="T">Temperature</param>
    /// <returns>GLFtemp</returns>
    internal double GFTemperature(double T)
    {
        if (photoPath == "C4")
            return GFTempC4(T);
        else
            return GFTempC3(T);
    }

    /// <summary>
    ///  Temperature effects on photosynthesis for C3 plants
    /// </summary>
    /// <param name="T">Temperature</param>
    /// <returns>GLFTemp</returns>
    private double GFTempC3(double T)
    {
        double result = 0.0;
        double growthTmax = growthTopt + ((growthTopt - growthTmin) / growthTq);
        if ((T > growthTmin) && (T < growthTmax))
        {
            double val1 = Math.Pow((T - growthTmin), growthTq) * (growthTmax - T);
            double val2 = Math.Pow((growthTopt - growthTmin), growthTq) * (growthTmax - growthTopt);
            result = val1 / val2;
            if (result < 0.0) result = 0.0;
        }

        return result;
    }

    /// <summary>
    /// Temperature effects on photosynthesis for C4 plants
    /// </summary>
    /// <param name="T">Temperature</param>
    /// <returns>GLFTemp</returns>
    public double GFTempC4(double T)
    {
        double result;
        if (T <= growthTmin)
        {
            result = 0.0;
        }
        else if (T >= growthTopt)
        {
            result = 1.0;
        }
        else
        {
            double Tmax = growthTopt + (growthTopt - growthTmin) / growthTq;
            double val1 = Math.Pow((T - growthTmin), growthTq) * (Tmax - T);
            double val2 = Math.Pow((growthTopt - growthTmin), growthTq) * (Tmax - growthTopt);
            result = val1 / val2;
            if (result < 0.0) result = 0.0;
        }

        return result;
    }

    /// <summary>
    /// Heat effects on photosynthesis
    /// </summary>
    /// <returns>Heat effect</returns>
    private double HeatEffect()
    {
        if (usingHeatStress)
        {
            double heatFactor;
            if (MetFile.MaxT > heatFullT)
            {
                // very high temperature, full stress
                heatFactor = 0.0;
                accumTHeat = 0.0;
            }
            else if (MetFile.MaxT > heatOnsetT)
            {
                // high temperature, add some stress
                heatFactor = highTempStress * (heatFullT - MetFile.MaxT) / (heatFullT - heatOnsetT);
                accumTHeat = 0.0;
            }
            else
            {
                // cool temperature, same stress as yesterday
                heatFactor = highTempStress;
            }

            // check recovery factor
            double recoveryFactor = 0.0;
            if (MetFile.MaxT <= heatOnsetT)
                recoveryFactor = (1 - heatFactor) * Math.Pow(accumTHeat / heatSumT, heatTq);

            // accumulate temperature
            accumTHeat += Math.Max(0.0, heatRecoverT - Tmean);

            // heat stress
            highTempStress = Math.Min(1.0, heatFactor + recoveryFactor);

            return highTempStress;
        }
        else
            return 1.0;
    }

    /// <summary>
    /// Cold effect on photosynthesis
    /// </summary>
    /// <returns>Cold effect</returns>
    private double ColdEffect()
    {
        if (usingColdStress)
        {
            double coldFactor;
            if (MetFile.MinT < coldFullT)
            {
                // very low temperature, full stress
                coldFactor = 0.0;
                accumTCold = 0.0;
            }
            else if (MetFile.MinT < coldOnsetT)
            {
                // low temperature, add some stress
                coldFactor = lowTempStress * (MetFile.MinT - coldFullT) / (coldOnsetT - coldFullT);
                accumTCold = 0.0;
            }
            else
            {
                // warm temperature, same stress as yesterday
                coldFactor = lowTempStress;
            }

            // check recovery factor
            double recoveryFactor = 0.0;
            if (MetFile.MinT >= coldOnsetT)
                recoveryFactor = (1 - coldFactor) * Math.Pow(accumTCold / coldSumT, coldTq);

            // accumulate temperature
            accumTCold += Math.Max(0.0, Tmean - coldRecoverT);

            // cold stress
            lowTempStress = Math.Min(1.0, coldFactor + recoveryFactor);

            return lowTempStress;
        }
        else
            return 1.0;
    }

    /// <summary>
    /// Computes the effects of temperature on respiration
    /// </summary>
    /// <returns>Temperature factor</returns>
    private double TemperatureEffectOnRespiration()
    {
        double result;
        double respTmin = 0.5 * growthTmin; // assume average between zero and growthTmin
        if (Tmean <= respTmin)
        {
            // too cold, no respiration
            result = 0.0;
        }
        else if (Tmean < respTref)
        {
            // respiration is a power function of temperature
            double respTmax = respTref * growthTq / (growthTq - 1);
            double val1 = Math.Pow((Tmean - respTmin) / (respTref - respTmin), growthTq);
            double val2 = (respTmax - Tmean) / (respTmax - respTref);
            result = val1 * val2;
        }
        else
        {
            // continue to rise proportionally to temperature, up to a maximum
            result = Math.Min(maxTempEffectResp, Tmean / respTref);
        }

        return result;
    }

    /// <summary>
    /// Computes the effects of temperature on respiration
    /// </summary>
    /// <returns>Temperature factor</returns>
    private double TemperatureEffectOnRespirationNew()
    {
        double result;
        if (Tmean <= 0)
        {
            // too cold, no respiration
            result = 0.0;
        }
        else
        {
            // scale factor
            double scalef = 1 - Math.Exp(-1);
            double baseEffect = 1 - Math.Exp(-Math.Pow(Tmean / respTref, 2.0));
            result = baseEffect / scalef;
        }

        return result;
    }

    /// <summary>
    ///  Effect of temperature on tissue turnover rate
    /// </summary>
    /// <returns>Temperature effect</returns>
    private double TemperatureEffectOnTissueTurnover()
    {
        double result;
        if (Tmean <= massFluxTmin)
            result = 0.0;
        else if (Tmean < massFluxTopt)
            result = Math.Pow((Tmean - massFluxTmin) / (massFluxTopt - massFluxTmin), massFluxTq);
        else
            result = 1.0;

        return result;
    }

    /// <summary>
    /// Effect of water stress on tissue turnover rate
    /// </summary>
    /// <returns>Moisture effect</returns>
    private double MoistureEffectOnTissueTurnover()
    {
        double result = 1.0;
        if (Math.Min(glfWater, glfAeration) < massFluxWopt)
            result = 1.0 + (massFluxW0 - 1.0) * ((massFluxWopt - Math.Min(glfWater, glfAeration)) / massFluxWopt);

        return Math.Max(1.0, Math.Min(massFluxW0, result));
    }

    /// <summary>
    /// Remove plant DM
    /// </summary>
    /// <param name="AmountToRemove">The DM amount to remove</param>
    /// <param name="PrefGreen">Preference level for green tissue</param>
    /// <param name="PrefDead">Preference level for dead tissue</param>
    /// <returns>Amount removed</returns>
    internal double RemoveDM(double AmountToRemove, double PrefGreen, double PrefDead)
    {
        // save current state
        SetPrevPools();

        // check existing amount and what is harvestable
        double amountRemovable = Math.Max(0.0, leaves.DMGreen + stems.DMGreen - dmgreenmin)
                                 + Math.Max(0.0, leaves.tissue[3].DM + stems.tissue[3].DM);

        // get the weights for each pool, consider preference and available DM
        double fractionToRemoved = 0.0;
        if (amountRemovable > 0.0)
            fractionToRemoved = Math.Min(1.0, MathUtility.Divide(AmountToRemove, amountRemovable, 0.0));

        double tempPrefGreen = PrefGreen + (PrefDead * fractionToRemoved);
        double tempPrefDead = PrefDead + (PrefGreen * fractionToRemoved);
        double tempRemovableGreen = Math.Max(0.0, leaves.DMGreen + stems.DMGreen - dmgreenmin);
        double tempRemovableDead = Math.Max(0.0, leaves.tissue[3].DM + stems.tissue[3].DM);

        // get partiton between dead and live materials
        double tempTotal = tempRemovableGreen * tempPrefGreen
                           + tempRemovableDead * tempPrefDead;
        double fractionToHarvestGreen = 0.0;
        double fractionToHarvestDead = 0.0;
        if (tempTotal > 0.0)
        {
            fractionToHarvestGreen = tempRemovableGreen * tempPrefGreen / tempTotal;
            fractionToHarvestDead = tempRemovableDead * tempPrefDead / tempTotal;
        }

        // get amounts removed
        double removingGreenDm = AmountToRemove * fractionToHarvestGreen;
        double removingDeadDm = AmountToRemove * fractionToHarvestDead;
        // Fraction of DM remaining in the field
        double fractionRemainingGreen = 1.0;
        if (leaves.DMGreen + stems.DMGreen > 0.0)
            fractionRemainingGreen -= removingGreenDm / (leaves.DMGreen + stems.DMGreen);
        double fractionRemainingDead = 1.0;
        if (leaves.tissue[3].DM + stems.tissue[3].DM > 0.0)
            fractionRemainingDead -= removingDeadDm / (leaves.tissue[3].DM + stems.tissue[3].DM);
        fractionRemainingGreen = Math.Max(0.0, Math.Min(1.0, fractionRemainingGreen));
        fractionRemainingDead = Math.Max(0.0, Math.Min(1.0, fractionRemainingDead));

        // get digestibility of DM being harvested
        double fCN = (AboveGroundLiveWt * CarbonFractionDM) / AboveGroundLiveN;
        double greenDigestibility = tissueDigestibility(fCN, digestLive, 0.5 * dGrowth / AboveGroundLiveWt);
            // assume half of new growth is sugar
        fCN = (AboveGroundDeadWt * CarbonFractionDM) / AboveGroundDeadN;
        double deadDigestibility = tissueDigestibility(fCN, digestDead, 0.0); // no sugars in dead material
        digestDefoliated = fractionToHarvestGreen * greenDigestibility + fractionToHarvestDead * deadDigestibility;

        // update the various pools
        leaves.tissue[0].DM *= fractionRemainingGreen;
        leaves.tissue[1].DM *= fractionRemainingGreen;
        leaves.tissue[2].DM *= fractionRemainingGreen;
        leaves.tissue[3].DM *= fractionRemainingDead;
        stems.tissue[0].DM *= fractionRemainingGreen;
        stems.tissue[1].DM *= fractionRemainingGreen;
        stems.tissue[2].DM *= fractionRemainingGreen;
        stems.tissue[3].DM *= fractionRemainingDead;
        //No stolon removed

        leaves.tissue[0].Namount *= fractionRemainingGreen;
        leaves.tissue[1].Namount *= fractionRemainingGreen;
        leaves.tissue[2].Namount *= fractionRemainingGreen;
        leaves.tissue[3].Namount *= fractionRemainingDead;
        stems.tissue[0].Namount *= fractionRemainingGreen;
        stems.tissue[1].Namount *= fractionRemainingGreen;
        stems.tissue[2].Namount *= fractionRemainingGreen;
        stems.tissue[3].Namount *= fractionRemainingDead;

        //Nremob is also removed proportionally (not sensitive?)
        double PreRemovalNRemob = Nremob;
        Nremob *= fractionRemainingGreen;

        // update Luxury N pools
        NLuxury2 *= fractionRemainingGreen;
        NLuxury3 *= fractionRemainingGreen;

        // Update the variables with aggregated data and plant parts (dmshoot, LAI, etc)
        UpdateAggregatedVariables();

        // check balance and set outputs
        double NremobRemove = PreRemovalNRemob - Nremob;
        dmdefoliated = prevState.dmshoot - AboveGroundWt;
        prevState.dmdefoliated = dmdefoliated;
        Ndefoliated = prevState.Nshoot - AboveGroundN;
        prevState.Ndefoliated = Ndefoliated;
        if (Math.Abs(dmdefoliated - AmountToRemove) > 0.00001)
            throw new Exception("  AgPasture - removal of DM resulted in loss of mass balance");

        return dmdefoliated;
    }

    /// <summary>
    /// Calculate the average herbage digestibility (above ground)
    /// </summary>
    /// <returns>digestibility</returns>
    internal void calcDigestibility()
    {
        double fSugar;
        double CNtissue;
        if (AboveGroundWt > 0.0)
        {
            //Live
            double digestibilityLive = 0.0;
            double standingDMGreen = leaves.DMGreen + stems.DMGreen;
            double standingNGreen = leaves.NGreen + stems.NGreen;
            if ((standingDMGreen > 0.0) & (standingNGreen > 0.0))
            {
                fSugar = 0.5 * dGrowth / AboveGroundLiveWt; //sugar fraction is assumed as half of growth
                CNtissue = 0.4 * standingDMGreen / standingNGreen; //CN ratio of live shoots (minus stolon)
                digestibilityLive = tissueDigestibility(CNtissue, digestLive, fSugar);
            }

            //Dead
            double digestibilityDead = 0.0;
            if ((AboveGroundDeadWt > 0.0) && (AboveGroundDeadN > 0.0))
            {
                fSugar = 0.0; //no sugar in dead material
                CNtissue = (AboveGroundDeadWt * CarbonFractionDM) / AboveGroundDeadN; //CN ratio of standing dead;
                digestibilityDead = tissueDigestibility(CNtissue, digestDead, 0.0);
            }

            double deadFrac = MathUtility.Divide(AboveGroundDeadWt, standingDMGreen, 0.0);
            digestHerbage = digestibilityLive * (1 - deadFrac) + digestibilityDead * deadFrac;
        }
        else
            digestHerbage = 0.0;
    }

    /// <summary>
    /// Calculate the digestibility of plant material
    /// </summary>
    /// <remarks>
    /// Assumes fixed CN ratios and digestibilites of various materials
    /// Digestibility of sugars (dissolved carbohydrates) and proteins is assumed to be one
    /// </remarks>
    /// <param name="tissueCN">the CN ratio of the tissue</param>
    /// <param name="nonSolubleDigest">the digestibility of non-soluble fraction of the tissue</param>
    /// <param name="fSugar">the fraction of soluble carbohydrate of the tissue</param>
    /// <returns>the tissue digestibility</returns>
    internal double tissueDigestibility(double tissueCN, double nonSolubleDigest, double fSugar)
    {
        double result = 0.0;
        if (tissueCN > 0.0)
        {
            //Fraction of protein in the tissue
            double fProtein = (fSugar - 1 + (CNw / tissueCN)) * (ProteinCNr / (CNw - ProteinCNr));
            //Fraction of non-soluble material in the tissue (cell wall)
            double fCellWall = 1 - fSugar - fProtein;

            result = fSugar + fProtein + fCellWall * nonSolubleDigest;
        }

        return result;
    }

    /// <summary>
    /// Find the layer at the bottom of the root zone
    /// </summary>
    /// <returns>layer at bottom of root zone</returns>
    internal int GetRootZoneBottomLayer()
    {
        double depthFromSurface = 0.0;
        int result = dlayer.Length - 1;
        double maxDepth = Math.Min(rootDepth, dlayer.Sum());
        for (int layer = 0; layer < dlayer.Length; layer++)
        {
            if (depthFromSurface >= maxDepth)
            {
                result = layer - 1;
                layer = dlayer.Length;
            }
            else
            {
                depthFromSurface += dlayer[layer];
            }
        }

        return result;
    }

    /// <summary>
    /// Compute the distribution of roots in the soil profile after growth (sum is equal to one)
    /// </summary>
    /// <remarks>
    /// Root distribution is not changed during the simulation, so this will return the initial RootFraction
    /// </remarks>
    /// <returns>The proportion of root mass in each soil layer</returns>
    private double[] RootGrowthDistribution(double deltaDM)
    {
        return rootFraction;
    }

    /// <summary>Compute the current target distribution of roots in the soil profile</summary>
    /// <remarks>
    /// This distribution is a correction of the target distribution, taking into account the depth of soil
    /// as well as the current rooting depth
    /// </remarks>
    /// <returns>The proportion of root mass in each soil layer</returns>
    private double[] CurrentRootDistributionTarget()
    {
        int nLayers = dlayer.Length;
        double currentDepth = 0.0;
        double cumAllocation = 0.0;
        for (int layer = 0; layer < nLayers; layer++)
        {
            if (currentDepth < rootDepth)
            {
                // layer is within the root zone
                currentDepth += dlayer[layer];
                if (currentDepth <= rootDepth)
                {
                    // layer is fully in the root zone
                    cumAllocation += targetRootAllocation[layer];
                }
                else
                {
                    // layer is partially in the root zone
                    double layerFrac = (rootDepth - (currentDepth - dlayer[layer]))
                                     / (maxRootDepth - (currentDepth - dlayer[layer]));
                    cumAllocation += targetRootAllocation[layer] * Math.Min(1.0, Math.Max(0.0, layerFrac));
                }
            }
            else
                layer = nLayers;
        }

        double[] result = new double[nLayers];
        for (int layer = 0; layer < nLayers; layer++)
            result[layer] = targetRootAllocation[layer] / cumAllocation;

        return result;
    }

    /// <summary>Compute the ideal distribution of roots in the soil profile</summary>
    /// <remarks>
    /// These values are used to allocate initial rootDM as well as any growth over the profile
    /// </remarks>
    /// <returns>A weighting factor for each soil layer (mm of soil)</returns>
    private double[] RootDistributionTarget()
    {
        // 1. Base distribution calculated using and ExpoLinear function
        //  Considers homogeneous distribution from surface down to a fraction of root depth (rootTopDepthParam)
        //   below this depth, the proportion of root decrease following a power function, it reaches zero slightly
        //   below the maxRootDepth (defined by rootDeepDepthParam), function's exponent is given by rootCurveParam
        //  The values are further adjusted using the values of XF (so there will be less roots in those layers)

        int nLayers = dlayer.Length;
        double[] result = new double[nLayers];
        double depthTop = 0.0;
        double depthBottom = 0.0;
        double depthFirstStage = Math.Min(maxRootDepth, expoLinearDepthParam);
        double rootDeepDepthParam = 1.0;
        double xFac;
        for (int layer = 0; layer < nLayers; layer++)
        {
            if (xf.Length > 1)
                xFac = xf[layer];
            else
                xFac = 1.0;
            depthBottom += dlayer[layer];
            if (depthTop >= maxRootDepth)
            {
                // totally out of root zone
                result[layer] = 0.0;
            }
            else if (depthBottom <= depthFirstStage)
            {
                // totally in the first stage
                result[layer] = dlayer[layer] * xFac;
            }
            else
            {
                // at least partially on second stage
                result[layer] = Math.Pow(maxRootDepth * rootDeepDepthParam - Math.Max(depthTop, depthFirstStage), expoLinearCurveParam + 1)
                              - Math.Pow(maxRootDepth * rootDeepDepthParam - Math.Min(depthBottom, maxRootDepth), expoLinearCurveParam + 1);
                result[layer] /= (expoLinearCurveParam + 1) * Math.Pow(maxRootDepth * rootDeepDepthParam - depthFirstStage, expoLinearCurveParam);
                if (depthTop < depthFirstStage)
                {
                    // partially in first stage
                    result[layer] += depthFirstStage - depthTop;
                }

                result[layer] *= xFac;
            }

            depthTop += dlayer[layer];
        }

        return result;
    }

    /// <summary>
    /// Compute how much of the layer is actually explored by roots
    /// </summary>
    /// <param name="layer"></param>
    /// <param name="root_depth"></param>
    /// <returns>Fraction of layer explored by roots</returns>
    private double LayerFractionWithRoots(int layer, double root_depth)
    {
        double depthToTopOfLayer = 0.0;
        double fraction_in_layer = 0.0;
        for (int i = 0; i < layer; i++)
            depthToTopOfLayer += dlayer[i];
        fraction_in_layer = (root_depth - depthToTopOfLayer) / dlayer[layer];

        return Math.Min(1.0, Math.Max(0.0, fraction_in_layer));
    }

    /// <summary>
    /// Calculate the plant height, as function of DM
    /// </summary>
    /// <returns>Plant height</returns>
    internal double HeightfromDM()
    {
        //double TodaysHeight = MaxPlantHeight - MinimumHeight;
        double TodaysHeight = MaxPlantHeight;
        double standingDM = (leaves.DMTotal + stems.DMTotal);

        if (standingDM <= MassForMaxHeight)
        {
            double massRatio = standingDM / MassForMaxHeight;
            double heightF = ExponentHeightFromMass
                             - (ExponentHeightFromMass * massRatio)
                             + massRatio;
            heightF *= Math.Pow(massRatio, ExponentHeightFromMass - 1);
            TodaysHeight *= heightF;
        }

        //return TodaysHeight + MinimumHeight;
        return Math.Max(TodaysHeight, MinimumHeight);
    }

    #endregion

    #region Auxiliary classes  ------------------------------------------------------------------------------------

    /// <summary>
    /// Defines a generic organ of a plant
    /// </summary>
    /// <remarks>
    /// Each organ (leaf, stem, etc) is defined as a collection of four tissues
    /// Each tissue has a record of DM and N amounts
    /// Methods to compute DM and N for total and 'green' tissue is given
    /// </remarks>
    internal class Organ
    {
        /// <summary>Initialise tissues</summary>
        public Organ(int numTissues)
        {
            nTissues = numTissues;
            tissue = new Tissue[nTissues];
            for (int t = 0; t < nTissues; t++)
            {
                tissue[t] = new Tissue();
            }
        }

        /// <summary>the collection of tissues for this organ</summary>
        internal Tissue[] tissue;

        /// <summary>Number of tissue pools to create</summary>
        internal int nTissues;

        /// <summary>N concentration for optimal growth (kg/kg)</summary>
        internal double NConcOptimum = 4.0;

        /// <summary>Maximum N concentration, for luxury uptake (kg/kg)</summary>
        internal double NConcMaximum = 6.0;

        /// <summary>Minimum N concentration, structural N (kg/kg)</summary>
        internal double NConcMinimum = 1.2;

        /// <summary>The total dry matter in this tissue (kg/ha)</summary>
        internal double DMTotal
        {
            get
            {
                double result = 0.0;
                for (int t = 0; t < nTissues; t++)
                {
                    result += tissue[t].DM;
                }

                return result;
            }
        }

        /// <summary>The dry matter in the live (green) tissues (kg/ha)</summary>
        internal double DMGreen
        {
            get
            {
                double result = 0.0;
                for (int t = 0; t < Math.Min(3, nTissues); t++)
                {
                    result += tissue[t].DM;
                }

                return result;
            }
        }

        /// <summary>The dry matter in the dead tissues (kg/ha)</summary>
        internal virtual double DMDead
        {
            get
            {
                double result = 0.0;
                if (nTissues > 3)
                {
                    result += tissue[3].DM;
                }

                return result;
            }
        }

        /// <summary>The total N amount in this tissue (kg/ha)</summary>
        internal double NTotal
        {
            get
            {
                double result = 0.0;
                for (int t = 0; t < nTissues; t++)
                {
                    result += tissue[t].Namount;
                }

                return result;
            }
        }

        /// <summary>The N amount in the live (green) tissues (kg/ha)</summary>
        internal double NGreen
        {
            get
            {
                double result = 0.0;
                for (int t = 0; t < Math.Min(3, nTissues); t++)
                {
                    result += tissue[t].Namount;
                }

                return result;
            }
        }

        /// <summary>The N amount in the dead tissues (kg/ha)</summary>
        internal double NDead
        {
            get
            {
                double result = 0.0;
                if (nTissues > 3)
                {
                    result += tissue[3].Namount;
                }

                return result;
            }
        }

        /// <summary>The average N concentration in this tissue (kg/kg)</summary>
        internal double NconcTotal
        {
            get
            {
                double result = 0.0;
                if (DMTotal > 0.0)
                {
                    result = NTotal / DMTotal;
                }

                return result;
            }
        }

        /// <summary>The dry matter in the live (green) tissues (kg/kg)</summary>
        internal double NconcGreen
        {
            get
            {
                double result = 0.0;
                if (DMGreen > 0.0)
                {
                    result = NGreen / DMGreen;
                }

                return result;
            }
        }

        /// <summary>The dry matter in the dead tissues (kg/kg)</summary>
        internal double NconcDead
        {
            get
            {
                double result = 0.0;
                if (DMDead > 0.0)
                {
                    result = NDead / DMDead;
                }

                return result;
            }
        }

        /// <summary>Defines a generic plant tissue</summary>
        internal class Tissue
        {
            /// <summary>The dry matter amount (kg/ha)</summary>
            internal double DM = 0.0;

            /// <summary>The N content (kg/ha)</summary>
            internal double Namount = 0.0;

            /// <summary>The P content (kg/ha)</summary>
            internal double Pamount = 0.0;

            /// <summary>The nitrogen concentration (kg/kg)</summary>
            internal double Nconc
            {
                get { return MathUtility.Divide(Namount, DM, 0.0); }
                set { Namount = value * DM; }
            }

            /// <summary>The phosphorus concentration (kg/kg)</summary>
            internal double Pconc
            {
                get { return MathUtility.Divide(Pamount, DM, 0.0); }
                set { Pamount = value * DM; }
            }
        }
    }

    /// <summary>
    /// Stores the values of pool status of previous day
    /// </summary>
    internal class SpeciesState
    {
        /// <summary>state of leaves (DM and N)</summary>
        internal Organ leaves;

        /// <summary>state of sheath/stems (DM and N)</summary>
        internal Organ stems;

        /// <summary>state of stolons (DM and N)</summary>
        internal Organ stolons;

        /// <summary>state of roots (DM and N)</summary>
        internal Organ roots;

        /// <summary>The constructor</summary>
        public SpeciesState()
        {
            leaves = new Organ(4);
            stems = new Organ(4);
            stolons = new Organ(3);
            roots = new Organ(1);
        }

        /// <summary>DM above ground</summary>
        internal double dmshoot
        {
            get { return leaves.DMTotal + stems.DMTotal + stolons.DMTotal; }
        }

        /// <summary>Total plant DM</summary>
        internal double dmtotal
        {
            get { return leaves.DMTotal + stems.DMTotal + stolons.DMTotal + roots.DMTotal; }
        }

        /// <summary>N in above ground material</summary>
        internal double Nshoot
        {
            get { return leaves.NTotal + stems.NTotal + stolons.NTotal; }
        }

        /// <summary>DM weight of defoliated material</summary>
        internal double dmdefoliated;

        /// <summary>N in defoliated material</summary>
        internal double Ndefoliated;

        /// <summary>N remobilsed from senesced tissue</summary>
        internal double Nremob;
    }

    #endregion
}

/// <summary>Basic values defining the state of a pasture species</summary>
[Serializable]
public class SpeciesBasicState
{
    /// <summary>DM weight for each biomass pool</summary>
    internal double[] DMWeight;

    /// <summary>N amount for each biomass pool</summary>
    internal double[] NAmount;

    /// <summary>Root depth</summary>
    internal double RootDepth;

    /// <summary>Constructor, initialise the arrays</summary>
    public SpeciesBasicState()
    {
        // there are 12 tissue pools, in order: leaf1, leaf2, leaf3, leaf4, stem1, stem2, stem3, stem4, stolon1, stolon2, stolon3, and root
        DMWeight = new double[12];
        NAmount = new double[12];
    }
}

////-----------------------------------------------------------------------------------------------
