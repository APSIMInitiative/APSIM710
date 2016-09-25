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

    #region Links and static variables  ------------------------------------------------------------------------------------

    /// <summary>Some Description</summary>
    internal static Clock Clock;

    /// <summary>Some Description</summary>
    internal static MetFile MetFile;

    /// <summary>Some Description</summary>
    internal static double CO2 = 380;

    /// <summary>total Radn intercepted by pasture</summary>
    internal static double swardInterceptedRadn;

    /// <summary>Some Description</summary>
    internal static double swardCoverGreen;

    /// <summary>Some Description</summary>
    internal static double swardLightExtCoeff;

    /// <summary>Soil layering</summary>
    internal double[] dlayer;

    #endregion  ------------------------------------------------------------------------------------------------------------

    #region Model parameters  ----------------------------------------------------------------------------------------------

    ////- General parameters (name and type) >>>  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    /// <summary>Some Description</summary>
    internal string speciesName;

    /// <summary>Some Description</summary>
    internal string micrometType;

    /// <summary>Photosynthesis pathways: 3=C3, 4=C4; no consideration for CAM(=3)</summary>
    internal string photoPath;

    ////- Potential growth (photosynthesis) >>> - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    /// <summary>reference leaf co2 mg/m^2/s maximum</summary>
    internal double Pm;

    /// <summary>Partition factor for intercepted light for each species</summary>
    internal double lightPartitioningFactor;

    /// <summary>Some Description</summary>
    internal double alphaPhoto;

    /// <summary>Some Description</summary>
    internal double thetaPhoto;

    /// <summary>Some Description</summary>
    internal double fractionPAR;

    /// <summary>Light extinction coefficient</summary>
    internal double lightExtCoeff;

    /// <summary>Some Description</summary>
    internal double referenceCO2 = 380;

    /// <summary>Some Description</summary>
    internal double CO2PmaxScale;

    /// <summary>Some Description</summary>
    internal double CO2NScale;

    /// <summary>Some Description</summary>
    internal double CO2NMin;

    /// <summary>Some Description</summary>
    internal double CO2NCurvature;

    /// <summary>Minimum temperature for growth</summary>
    internal double growthTmin;

    /// <summary>Optimum temperature for growth</summary>
    internal double growthTopt;

    /// <summary>Temperature curvature coefficient</summary>
    internal double growthTq;

    /// <summary>Some Description</summary>
    internal bool usingHeatStress = false;

    /// <summary>onset temperature for heat effects</summary>
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

    /// <summary>onset temperature for cold effects</summary>
    internal double coldOnsetT;

    /// <summary>full temperature for cold effects</summary>
    internal double coldFullT;

    /// <summary>Some Description</summary>
    internal double coldTq;

    /// <summary>temperature sum for recovery - sum of means</summary>
    internal double coldSumT;

    /// <summary>Some Description</summary>
    internal double coldRecoverT;

    ////- Respiration parameters >>>  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

        /// <summary>Some Description</summary>
    internal double maintRespiration;

    /// <summary>Some Description</summary>
    internal double growthEfficiency;

    /// <summary>Some Description</summary>
    internal double maxTempEffectResp = 1.25;

    /// <summary>Some Description</summary>
    internal double respTref;

    /// <summary>Some Description</summary>
    internal double respExponent;

    ////- Germination and emergence >>> - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    /// <summary>Some Description</summary>
    internal double degreesdayForGermination;

    /// <summary>Some Description</summary>
    internal double[] emergenceDM;

    ////- Allocation of new growth >>>  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    /// <summary>Shoot-Root ratio targeted</summary>
    internal double TargetSRratio;

    /// <summary>Maximum allocation of DM to roots</summary>
    internal double MaxRootAllocation;

    /// <summary>factor for different biomass allocation among seasons</summary>
    internal double allocationSeasonF;

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

    /// <summary>Maximum effect that soil GLFs have on Shoot-Root ratio</summary>
    internal double GlfEffectOnSR = 0.5;

    /// <summary>Flag whether the effect of reproductive stage on shoot DM allocation are considered</summary>
    internal bool UsingReproSeasonFactor = true;

    /// <summary>Reference latitude determining timing for reproductive season</summary>
    internal double ReproSeasonReferenceLatitude = 41;

    /// <summary>Controls how the time to start the reproductive season varies with latitude</summary>
    internal double ReproSeasonTimingCoeff = 0.14;

    /// <summary>Control how the duration of the reproductive season varies with latitude</summary>
    internal double ReproSeasonDurationCoeff = 2.0;
     
    /// <summary>The ratio between the length of shoulders and the period with full reproductive growth effect</summary>
    internal double ReproSeasonShouldersLengthFactor = 1.0;
     
    /// <summary>The proportion of the length of shoulder before the period with full reproductive growth effect</summary>
    internal double ReproSeasonOnsetDurationFactor = 0.6;

    /// <summary>Maximum increase in DM allocation to shoot during reproductive growth</summary>
    internal double ReproSeasonMaxAllocationIncrease = 0.5;

    /// <summary>Controls how the increase in shoot allocation during reproductive growth varies with latitude</summary>
    internal double ReproSeasonAllocationCoeff = 0.10;

    /// <summary>Some Description</summary>
    internal double FractionLeafMaximum;

    /// <summary>Some Description</summary>
    internal double FractionLeafMinimum;

    /// <summary>Some Description</summary>
    internal double FractionLeafDMThreshold;

    /// <summary>Some Description</summary>
    internal double FractionLeafDMFactor;

    /// <summary>Some Description</summary>
    internal double FractionLeafExponent;

    /// <summary>Growth partition to stolon (0-1)</summary>
    internal double StolonAllocationFactor;

    /// <summary>Specific leaf area (m2/kg dwt)</summary>
    internal double SpecificLeafArea;

    /// <summary>Specific root length (m/kg dwt)</summary>
    internal double SpecificRootLength;

    ////- Tissue turnover and senescence >>>  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    /// <summary>Some Description</summary>
    internal double LiveLeavesPerTiller;

    /// <summary>Some Description</summary>
    internal double refTissueTurnoverRate; //Decay coefficient between live and dead

    /// <summary>Some Description</summary>
    internal double facGrowingTissue;

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
    internal double factorGLFW2dead;

    /// <summary>Some Description</summary>
    internal double stockParameter; //Stock influence parameter

    /// <summary>Some Description</summary>
    internal static double stockingRate = 0.0;
    //stocking rate affecting transfer of dead to litter (default as 0 for now)

    /// <summary>Fraction of luxury N in tissue 1 that can be remobilised</summary>
    internal double Kappa1 = 0.0;

    /// <summary>Fraction of luxury N in tissue 2 that can be remobilised</summary>
    internal double Kappa2 = 0.0;

    /// <summary>Fraction of luxury N in tissue 3 that can be remobilised</summary>
    internal double Kappa3 = 0.0;

    /// <summary>Some Description</summary>
    internal double Kappa4 = 0.0;

    ////- N concentrations thresholds >>> - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    /// <summary>Some Description</summary>
    internal double NcstemFr; //stem Nc as % of leaf Nc

    /// <summary>Some Description</summary>
    internal double NcstolFr; //stolon Nc as % of leaf Nc

    /// <summary>Some Description</summary>
    internal double NcrootFr; //root Nc as % of leaf Nc

    /// <summary>N concentration in tissue 2 relative to tissue 1</summary>
    internal double NcRel2;

    /// <summary>N concentration in tissue 3 relative to tissue 1</summary>
    internal double NcRel3;

    ////- N fixation (for legumes) >>>  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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

    ////- Growth limiting factors >>> - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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

    ////- Harvest limits and preferences >>>  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    /// <summary>Relative preference for green tissues over dead, for DM removal</summary>
    internal double PreferenceGreenOverDead;

    /// <summary>Relative preference for leaves over stem-stolon, for DM removal</summary>
    internal double PreferenceLeafOverStem;

    ////- Plant height >>>  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    /// <summary>Some Description</summary>
    internal double MaxPlantHeight;

    /// <summary>Some Description</summary>
    internal double MassForMaxHeight;

    /// <summary>Some Description</summary>
    internal double ExponentHeightFromMass;

    /// <summary>Some Description</summary>
    internal double MinimumHeight;

    ////- Root depth and distribution >>> - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    /// <summary>minimum root depth (mm)</summary>
    internal double minRootDepth;

    /// <summary>maximum root depth</summary>
    internal double maxRootDepth; //Maximum root depth (mm)

    /// <summary>Base root elongation rate (mm/day)</summary>
    internal double rootElongationRate;

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
    internal double exponentSWCuptake = 0.25;

    /// <summary>Some Description</summary>
    internal double[] xf;

    ////- Parameters for annual species >>> - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    /// <summary>Earliest day of year for emergence (for annuals only)</summary>
    internal double dayGermn;

    /// <summary>Days needed to complete germination</summary>
    internal double daysToGermn;

    /// <summary>Days from emergence to Anthesis</summary>
    internal double daysEmgToAnth;

    /// <summary>Days from anthesis to maturity</summary>
    internal double daysAnthToMatur;

    #endregion  ------------------------------------------------------------------------------------------------------------

    #region Private variables  ---------------------------------------------------------------------------------------------

    ////- Plant parts and state >>> - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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

    ////- Defining the plant type >>> - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    /// <summary>Flag whether the species is a legume</summary>
    internal bool isLegume;

    /// <summary>Some Description</summary>
    internal bool isAnnual; //Species type (1=annual,0=perennial)

    ////- Annuals and phenology >>> - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    /// <summary>Some Description</summary>
    internal int phenoStage = 0; //pheno stages: 0 - pre_emergence, 1 - vegetative, 2 - reproductive

    /// <summary>Some Description</summary>
    internal double daysfromEmergence = 0;

    ////- Photosynthesis, growth, and turnover >>>  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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

    /// <summary>daily total growth</summary>
    internal double dGrowth = 0.0;

    /// <summary>daily shoot growth</summary>
    internal double dGrowthShoot = 0.0;

    /// <summary>daily root growth</summary>
    internal double dGrowthRoot = 0.0;

    /// <summary>N amount in new shoot growth</summary>
    internal double dGrowthShootN = 0.0;

    /// <summary>N amount in new root growth</summary>
    internal double dGrowthRootN = 0.0;

    /// <summary>Some Description</summary>
    internal double dDMLitter = 0.0; //daily litter production

    /// <summary>Some Description</summary>
    internal double dNLitter = 0.0; //N in dDMLitter

    /// <summary>Some Description</summary>
    internal double dDMRootSen = 0.0; //daily root sennesce

    /// <summary>Some Description</summary>
    internal double dNRootSen = 0.0; //N in dDMRootSen

    /// <summary>actual fraction of new growth added to shoot</summary>
    internal double ShootAllocationFactor = 1.0;

    /// <summary>The day of the year for the start of the reproductive season</summary>
    private double doyIniReproSeason;

    /// <summary>The intervals defining the reproductive season</summary>
    private double[] reproSeasonInterval;

    /// <summary>The relative increase in the shoot-root ratio during reproductive season</summary>
    private double allocationIncreaseRepro;

    /// <summary>Actual fraction of shoot growth added to leaves</summary>
    internal double LeafAllocationFactor;

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

    ////- Plant height, LAI and cover >>> - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    /// <summary>Plant height</summary>
    internal double height;

    /// <summary>Some Description</summary>
    internal double greenLAI; //sum of 3 pools

    /// <summary>Some Description</summary>
    internal double deadLAI; //pool dmleaf4

    /// <summary>Some Description</summary>
    internal double totalLAI;

    ////- Root depth and distribution >>> - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    /// <summary>current root depth (mm)</summary>
    internal double rootDepth;

    /// <summary>Some Description</summary>
    internal int layerBottomRootZone;

    /// <summary>Daily root growth (mm)</summary>
    internal double dRootDepth;

    /// <summary>Some Description</summary>
    internal double[] rootFraction;

    /// <summary>Some Description</summary>
    internal double[] targetRootAllocation;

    ////- Amounts and fluxes of N in the plant >>>  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    /// <summary>Some Description</summary>
    internal double NdemandLux = 0.0; //N demand for new growth, with luxury uptake

    /// <summary>Some Description</summary>
    internal double NdemandOpt = 0.0;

    /// <summary>Some Description</summary>
    internal double NFixed = 0.0; //N fixed by legumes

    /// <summary>Some Description</summary>
    internal double newGrowthN = 0.0; //N plant-soil

    /// <summary>Some Description</summary>
    internal double NRemobilised2NewGrowth = 0.0;

    /// <summary>amount of luxury N remobilised from tissue 3</summary>
    internal double NLuxury2NewGrowth = 0.0;

    ////- N uptake process >>>  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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

    ////- Water uptake process >>>  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    /// <summary>Plant water demand</summary>
    internal double WaterDemand = 0.0;

    /// <summary>Plant soil available water</summary>
    internal double[] soilAvailableWater;

    /// <summary>Amount of soil water taken up</summary>
    internal double WaterUptake = 0.0;

    /// <summary>Amount of soil water taken up</summary>
    internal double[] soilWaterUptake;

    ////- Growth limiting factors >>> - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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

    /// <summary>fraction of growth rate due to high temp. effect</summary>
    internal double highTempStress = 1.0;

    /// <summary>accumulated temperature from previous heat strike</summary>
    private double accumTHeat = 0.0;

    /// <summary>fraction of growth rate due to low temp. effect</summary>
    internal double lowTempStress = 1.0;

    /// <summary>accumulated temperature from previous cold strike</summary>
    private double accumTCold = 0.0;

    /// <summary>Some Description</summary>
    internal double glfWater = 1.0; //from water stress

    /// <summary>Some Description</summary>
    internal double glfAeration = 1.0;

    /// <summary>Some Description</summary>
    internal double glfN = 1.0; //from N deficit

    ////- Harvest and digestibility >>> - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    /// <summary>Some Description</summary>
    internal double dmdefoliated;

    /// <summary>Some Description</summary>
    internal double Ndefoliated;

    /// <summary>Some Description</summary>
    private double fractionDefoliated;

    /// <summary>Some Description</summary>
    internal double digestHerbage;

    /// <summary>Some Description</summary>
    internal double digestDefoliated;

    #endregion  ------------------------------------------------------------------------------------------------------------

    #region Constants  -----------------------------------------------------------------------------------------------------

    /// <summary>DM to C conversion</summary>
    internal const double CarbonFractionDM = 0.4;

    /// <summary>N mols per mol of protein</summary>
    /// <remarks>This is for plants... (higher in amino acids)</remarks>
    internal const double N2Protein = 6.25;

    /// <summary>CN ratio of proteins</summary>
    /// <remarks>C:N in remobilised material</remarks>
    internal const double ProteinCNr = 3.5;

    /// <summary>CN ratio of cell wall</summary>
    internal const double CNw = 100;

    /// <summary>Minimum significant difference between two values</summary>
    internal const double Epsilon = 0.000000001;

    #endregion  ------------------------------------------------------------------------------------------------------------

    #region Model outputs  -------------------------------------------------------------------------------------------------

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

    /// <summary>Dry matter amount of standing green material (kg/ha)</summary>
    internal double StandingWt
    {
        get { return StandingLiveWt + StandingDeadWt; }
    }

    /// <summary>Dry matter amount of standing green material (kg/ha)</summary>
    internal double StandingLiveWt
    {
        get { return leaves.DMGreen + stems.DMGreen + (stolons.DMGreen * stolons.FractionStanding); }
    }

    /// <summary>Dry matter amount of standing green material (kg/ha)</summary>
    internal double StandingDeadWt
    {
        get { return leaves.DMDead + stems.DMDead; }
    }

    /// <summary>Dry matter amount that can be harvested (kg/ha)</summary>
    internal double HarvestableWt
    {
        get { return leaves.DMTotalHarvestable + stems.DMTotalHarvestable + stolons.DMTotalHarvestable; }
    }

    /// <summary>N amount of standing green material (kg/ha)</summary>
    internal double StandingN
    {
        get { return leaves.NTotal + stems.NTotal + (stolons.NTotal * stolons.FractionStanding); }
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

    /// <summary>N remobiliesed N during senescence</summary>
    internal double NSenescedRemobilisable
    {
        get { return leaves.NSenesced + stems.NSenesced + stolons.NSenesced + roots.NSenesced; }
    }

    /// <summary>N remobiliesed N during senescence</summary>
    internal double NLuxuryRemobilisable
    {
        get { return leaves.NLuxury + stems.NLuxury + stolons.NLuxury + roots.NLuxury; }
    }

    /// <summary>Root length density (mm/mm3)</summary>
    internal double[] RLD
    {
        get
        {
            double[] Result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
            {
                Result[layer] = (roots.DMGreen * 0.1) * rootFraction[layer] * SpecificRootLength; // m/m2
                Result[layer] /= dlayer[layer] * 1000; // mm/mm3
            }

            return Result;
        }
    }

    #endregion  ------------------------------------------------------------------------------------------------------------

    #region Initialisation methods  ----------------------------------------------------------------------------------------

    /// <summary>Set DM and N values for each species in the sward</summary>
    /// <param name="MyState">The collection of basic state defining values</param>
    internal void SetSpeciesState(SpeciesBasicState MyState)
    {
        // Shoot DM
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

        // Root depth and distribution
        rootDepth = MyState.RootDepth;
        targetRootAllocation = RootDistributionTarget();
        rootFraction = CurrentRootDistributionTarget();

        // N amount
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

        // Update the aggregated variables (LAI, height, etc.)
        UpdateAggregatedVariables();
    }

    /// <summary>Set plant state at emergence</summary>
    internal void SetEmergenceState()
    {
        // init DM for each pool
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

        // init roots
        rootDepth = minRootDepth;
        rootFraction = CurrentRootDistributionTarget();

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

        // init pheno stage
        phenoStage = 1;

        // init the aggregated variables (LAI, height, etc)
        UpdateAggregatedVariables();
    }

    /// <summary>Initialises the parameters to compute factor increasing shoot allocation during reproductive growth</summary>
    /// <remarks>
    /// Reproductive phase of perennials is not simulated by the model, the ReproductiveGrowthFactor attempts to mimic the main
    ///  effect, which is a higher allocation of DM to shoot during this period. The beginning and length of the reproductive
    ///  phase is computed as function of latitude (it occurs later in spring and is shorter the further the location is from
    ///  the equator). The extent at which allocation to shoot increases is also a function of latitude, maximum allocation is
    ///  greater for higher latitudes. Shoulder periods occur before and after the main phase, in these the DM allocation varies
    ///  between the default allocation and that of the main reproductive phase (allocationIncreaseRepro).
    /// </remarks>
    internal void InitReproductiveGrowthFactor()
    {
        int yearLength = 365 + (DateTime.IsLeapYear(Clock.year) ? 1 : 0);
        int doyWinterSolstice = (MetFile.Latitude < 0.0) ? 172 : 355;
        reproSeasonInterval = new double[3];

        // compute the day to start the period with maximum DM allocation to shoot
        double doyIniPlateau = doyWinterSolstice;
        doyIniPlateau += 0.5 * yearLength / (1 + Math.Exp(-ReproSeasonTimingCoeff * (Math.Abs(MetFile.Latitude) - ReproSeasonReferenceLatitude)));

        // compute the duration of the main phase (with max allocation to shoot)
        reproSeasonInterval[1] = (yearLength / 24.0);
        reproSeasonInterval[1] += (yearLength * 11.0 / 24.0) * Math.Pow(1 - (Math.Abs(MetFile.Latitude) / 90.0), ReproSeasonDurationCoeff);

        // compute the duration of the onset and outset phases (shoulders)
        reproSeasonInterval[0] = reproSeasonInterval[1] * ReproSeasonShouldersLengthFactor * ReproSeasonOnsetDurationFactor;
        reproSeasonInterval[2] = reproSeasonInterval[1] * ReproSeasonShouldersLengthFactor * (1.0 - ReproSeasonOnsetDurationFactor);

        if (reproSeasonInterval.Sum() > yearLength)
            throw new Exception("Error on calculating period with high DM allocation, it is greater then one year");

        // get the day for the start of reproductive season
        doyIniReproSeason = doyIniPlateau - reproSeasonInterval[0];
        if (doyIniReproSeason < 0.0) doyIniReproSeason += yearLength;

        // compute the factor to augment allocation to shoot at main phase
        allocationIncreaseRepro = ReproSeasonMaxAllocationIncrease;
        allocationIncreaseRepro /= (1 + Math.Exp(-ReproSeasonAllocationCoeff * (Math.Abs(MetFile.Latitude) - ReproSeasonReferenceLatitude)));
    }

    #endregion  ------------------------------------------------------------------------------------------------------------

    #region Daily processes  -----------------------------------------------------------------------------------------------

    /// <summary>Refresh variables</summary>
    internal void RefreshVariables()
    {
        dmdefoliated = 0.0;
        Ndefoliated = 0.0;
        digestDefoliated = 0.0;
        leaves.DoCleanTransferAmounts();
        stems.DoCleanTransferAmounts();
        stolons.DoCleanTransferAmounts();
        roots.DoCleanTransferAmounts();
    }

    #region - Plant growth processes - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - - - - -

    /// <summary>Computes the daily progress through germination</summary>
    /// <returns>Fraction of germination phase completed (0-1)</returns>
    internal double DailyGerminationProgress()
    {
        double result = 0.0;
        germinationGDD += Math.Max(0.0, Tmean - growthTmin);
        result = germinationGDD / degreesdayForGermination;
        return result;
    }

    /// <summary>Calculates the daily potential growth</summary>
    /// <returns>Plant growth (kg/ha)</returns>
    internal double CalcDailyPotentialGrowth()
    {
        // Get today's gross potential photosynthetic rate (kgC/ha/day)
        DailyPotentialPhotosynthesis();

        // Get respiration rates (kgC/ha/day)
        DailyPlantRespiration();

        // Get C remobilisation, not explicitly done here as in EM
        Cremob = 0.0;

        // Get N fixation costs
        costNFixation = 0.0;
        if (isLegume && (NFixationCostMethod == 2))
            costNFixation = NFixationCost_M2();

        // Net potential growth (C) of the day
        dGrowthPot = Pgross + Cremob - Resp_g - Resp_m - costNFixation;
        dGrowthPot = Math.Max(0.0, dGrowthPot);

        // Get growth reduction for annual species, related to phenology (from IJ)
        if (isAnnual)
            dGrowthPot *= AnnualSpeciesGrowthFactor();

        //convert C to DM
        dGrowthPot /= CarbonFractionDM;

        // Net potential growth (kgDM/ha/day)
        return dGrowthPot;
    }

    /// <summary>Calculates the daily growth after moisture limitations</summary>
    /// <returns>Plant growth (kg/ha)</returns>
    internal double CalcGrowthAfterWaterLimitations()
    {
        dGrowthW = dGrowthPot * Math.Pow(Math.Min(glfWater, glfAeration), waterStressFactor);

        return dGrowthW;
    }

    /// <summary>Calculates the daily growth after nutrient limitations</summary>
    /// <returns>Plant growth (kg/ha)</returns>
    internal double CalcGrowthAfterNLimitations()
    {
        double gfnit = 0.0;
        if (isLegume)
            gfnit = glfN; //legume no dilution, but reducing more DM (therefore LAI)
        else
            gfnit = Math.Pow(glfN, NdilutCoeff);
        // more DM growth than N limited, due to dilution (typically NdilutCoeff = 0.5)

        dGrowth = dGrowthW * Math.Min(gfnit, GLFSFertility);
        return dGrowth;
    }

    /// <summary>Daily potential carbon assimilation</summary>
    internal void DailyPotentialPhotosynthesis()
    {
        bool isGrowing = true;

        // annuals phenology
        if (isAnnual)
            isGrowing = EvaluatePhenologyOfAnnuals();

        // set basic values for growth factors
        CO2Factor = 1.0;
        NcFactor = 1.0;
        RadnFactor = 1.0;

        if (isGrowing)
        {
            //CO2 effects
            CO2Factor = CO2EffectsOnPhotosynthesis();

            //N concentration effects
            NcFactor = NEffectOnPhotosynthesis();

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
            canopyCompetitionFactor = MathUtility.Divide(interceptedRadn * lightPartitioningFactor, MetFile.Radn * coverGreen, 1.0);

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

    /// <summary>Compute the photosynthetic rate for a single leaf</summary>
    /// <param name="IL">Instantaneous intercepted radiation (depend on time of day)</param>
    /// <param name="Pmax">Max photosynthesis rate, given T, CO2 and N concentration</param>
    /// <returns></returns>
    private double SingleLeafPhotosynthesis(double IL, double Pmax)
    {
        double photoAux1 = alphaPhoto * IL + Pmax;
        double photoAux2 = 4 * thetaPhoto * alphaPhoto * IL * Pmax;
        double Pl = (0.5 / thetaPhoto) * (photoAux1 - Math.Sqrt(Math.Pow(photoAux1, 2) - photoAux2));
        return Pl;
    }

    /// <summary>Compute plant respiration, growth and maintenance</summary>
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

    /// <summary>Daily potential growth</summary>
    /// <returns>Pot growth</returns>
    internal double DailyGrowthPot()
    {
        CO2Factor = 1.0;
        NcFactor = 1.0;
        RadnFactor = 1.0;

        // annuals phenology
        if (isAnnual)
        {
            bool moreGrowth = EvaluatePhenologyOfAnnuals();
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
        CO2Factor = CO2EffectsOnPhotosynthesis();
        NcFactor = NEffectOnPhotosynthesis();

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
        double carbon_m2 = 0.5 * (Pl1 + Pl2) * swardCoverGreen * intRadnFrac * lightPartitioningFactor / lightExtCoeff;
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
        if (isLegume && (NFixationCostMethod == 2))
            costNFixation = NFixationCost_M2();

        // ** C budget is not explicitly done here as in EM
        Cremob = 0.0; // Nremob * C2N_protein;    // No carbon budget here
        // Nu_remob[elC] := C2N_protein * Nu_remob[elN];
        // need to subtract CRemob from dm turnover?

        // Net potential growth (C) of the day (excluding growth respiration)
        dGrowthPot = Pgross + Cremob - Resp_g - Resp_m - costNFixation;
        dGrowthPot = Math.Max(0.0, dGrowthPot);

        //convert C to DM
        dGrowthPot /= CarbonFractionDM;

        // phenologically related reduction of annual species (from IJ)
        if (isAnnual)
            dGrowthPot *= AnnualSpeciesGrowthFactor();

        return dGrowthPot;
    }

    /// <summary>Tissue turnover among the 12 biomass pools</summary>
    internal void EvaluateTissueTurnover()
    {
        // The turnover rates are affected by soil water and air temperature
        //  the number of leaves per tiller also influences the rate (3 stage pools are used to describe any number of leaves)

        // Get the temperature factor for tissue turnover
        tempFacTTurnover = TemperatureEffectOnTissueTurnover();

        // Get the moisture factor for tissue turnover
        swFacTTurnover = MoistureEffectOnTissueTurnover();

        // Get the moisture factor for littering rate
        double swFacTTDead = Math.Pow(Math.Min(glfWater, glfAeration), exponentGLFW2dead);
        swFacTTDead = factorGLFW2dead + Math.Max(0.0, 1.0 - factorGLFW2dead) * swFacTTDead;

        // Consider the number of leaves
        double leafFac = 3.0 / LiveLeavesPerTiller; // three refers to the number of stages used in the model

        // Leaf and stems turnover rate
        gama = refTissueTurnoverRate * tempFacTTurnover * swFacTTurnover * leafFac;

        // Littering rate
        double digestDead = (leaves.DigestibilityDead * leaves.DMDead) + (stems.DigestibilityDead * stems.DMDead);
        digestDead = MathUtility.Divide(digestDead, leaves.DMDead + stems.DMDead, 0.0);
        gamaD = refLitteringRate * swFacTTDead * digestDead / 0.4;

        // Adjust littering rate due to stock trampling
        gamaD += stockParameter * stockingRate;
        if (gamaD <= 0.03)
            gamaD += 0.0;

        // Roots turnover rate
        gamaR = tempFacTTurnover * (2 - Math.Min(glfWater, glfAeration)) * rateRootSen;

        // check turnover rates
        if ((gama > 1.0)|| (gamaD > 1.0)||(gamaR > 1.0))
            throw new Exception(" AgPasture Computed tissue turnover rate greater than one");

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
                    double Kv = daysfromEmergence / daysEmgToAnth;
                    gama *= Kv;
                    gamaR *= Kv;
                }
                else if (phenoStage == 2)
                {
                    //reproductive
                    double Kr = (daysfromEmergence - daysEmgToAnth) / daysAnthToMatur;
                    //gama = 1 - (1 - gama) * (1 - Kr * Kr);
                    gama *= 1 - (Kr * Kr);
                    gama += Kr * Kr;
                }
            }

            // If today's turnover will result in a dmgreenShoot < dmgreen_minimum, then adjust the rates,
            // Possibly will have to skip this for annuals to allow them to die - phenololgy-related?
            double dmgreenToBe = AboveGroundLiveWt + dGrowth - (gama * (leaves.tissue[2].DM + stems.tissue[2].DM + stolons.tissue[2].DM));
            double minimumDMgreen = leaves.MinimumGreenDM + stems.MinimumGreenDM + stolons.MinimumGreenDM;
            if (dmgreenToBe < minimumDMgreen)
            {
                if (gama > 0.0)
                {
                    if (AboveGroundLiveWt + dGrowth < minimumDMgreen)
                    {
                        gama = 0.0;
                        gamaR = 0.0;
                    }
                    else
                    {
                        double gama_adj = MathUtility.Divide(AboveGroundLiveWt + dGrowth - minimumDMgreen,
                            leaves.tissue[2].DM + stems.tissue[2].DM + stolons.tissue[2].DM, 0.0);
                        gamaR *= gama_adj / gama;
                        gama = gama_adj;
                    }
                }
            }

            // consider a minimum for roots too
            if (roots.DMGreen < roots.MinimumGreenDM)
            {
                gamaR = 0.0;
            }

            // Stolons turnover rate (legumes)
            if (isLegume)
            {
                gamaS = gama;

                // Increase stolon senescence if there was defoliation
                gamaS += fractionDefoliated * (1.0 - gamaS);
            }
            else
                gamaS = 0.0;

            // clear fraction removed, need to do it here to avoid issue with when a removal has happened (onPrepare, onProcess, onPost)
            fractionDefoliated = 0.0;

            // Get the turnover amounts (DM and N) for each organ  - DM into tissue1 was considered in PartitionDMGrown()

            // Leaves
            leaves.tissue[0].DMTransferedOut = facGrowingTissue * gama * leaves.tissue[0].DM;
            leaves.tissue[1].DMTransferedIn = leaves.tissue[0].DMTransferedOut;
            leaves.tissue[1].DMTransferedOut = gama * leaves.tissue[1].DM;
            leaves.tissue[2].DMTransferedIn = leaves.tissue[1].DMTransferedOut;
            leaves.tissue[2].DMTransferedOut = gama * leaves.tissue[2].DM;
            leaves.tissue[3].DMTransferedIn = leaves.tissue[2].DMTransferedOut;
            leaves.tissue[3].DMTransferedOut = gamaD * leaves.tissue[3].DM;
            dDMLitter = leaves.tissue[3].DMTransferedOut;

            leaves.tissue[0].NTransferedOut = leaves.tissue[0].Nconc * leaves.tissue[0].DMTransferedOut;
            leaves.tissue[1].NTransferedIn = leaves.tissue[0].NTransferedOut;
            leaves.tissue[1].NTransferedOut = leaves.tissue[1].Nconc * leaves.tissue[1].DMTransferedOut;
            leaves.tissue[2].NTransferedIn = leaves.tissue[1].NTransferedOut;
            leaves.tissue[2].NTransferedOut = leaves.tissue[2].Nconc * leaves.tissue[2].DMTransferedOut;
            leaves.tissue[3].NTransferedIn = leaves.tissue[2].NTransferedOut;
            leaves.tissue[3].NTransferedOut = leaves.tissue[3].Nconc * leaves.tissue[3].DMTransferedOut;
            dNLitter = leaves.tissue[3].NTransferedOut;
            leaves.tissue[3].NRemobilisable = (leaves.tissue[2].Nconc - leaves.NConcMinimum) * leaves.tissue[2].DMTransferedOut;

            // Stems
            stems.tissue[0].DMTransferedOut = facGrowingTissue * gama * stems.tissue[0].DM;
            stems.tissue[1].DMTransferedIn = stems.tissue[0].DMTransferedOut;
            stems.tissue[1].DMTransferedOut = gama * stems.tissue[1].DM;
            stems.tissue[2].DMTransferedIn = stems.tissue[1].DMTransferedOut;
            stems.tissue[2].DMTransferedOut = gama * stems.tissue[2].DM;
            stems.tissue[3].DMTransferedIn = stems.tissue[2].DMTransferedOut;
            stems.tissue[3].DMTransferedOut = gamaD * stems.tissue[3].DM;
            dDMLitter += stems.tissue[3].DMTransferedOut;

            stems.tissue[0].NTransferedOut = stems.tissue[0].Nconc * stems.tissue[0].DMTransferedOut;
            stems.tissue[1].NTransferedIn = stems.tissue[0].NTransferedOut;
            stems.tissue[1].NTransferedOut = stems.tissue[1].Nconc * stems.tissue[1].DMTransferedOut;
            stems.tissue[2].NTransferedIn = stems.tissue[1].NTransferedOut;
            stems.tissue[2].NTransferedOut = stems.tissue[2].Nconc * stems.tissue[2].DMTransferedOut;
            stems.tissue[3].NTransferedIn = stems.tissue[2].NTransferedOut;
            stems.tissue[3].NTransferedOut = stems.tissue[3].Nconc * stems.tissue[3].DMTransferedOut;
            dNLitter += stems.tissue[3].NTransferedOut;
            stems.tissue[3].NRemobilisable = (stems.tissue[2].Nconc - stems.NConcMinimum) * stems.tissue[2].DMTransferedOut;

            // Stolons
            if (isLegume)
            {

                stolons.tissue[0].DMTransferedOut = facGrowingTissue * gamaS * stolons.tissue[0].DM;
                stolons.tissue[1].DMTransferedIn = stolons.tissue[0].DMTransferedOut;
                stolons.tissue[1].DMTransferedOut = gamaS * stolons.tissue[1].DM;
                stolons.tissue[2].DMTransferedIn = stolons.tissue[1].DMTransferedOut;
                stolons.tissue[2].DMTransferedOut = gamaS * stolons.tissue[2].DM;
                stolons.tissue[3].DMTransferedIn = stolons.tissue[2].DMTransferedOut;
                stolons.tissue[3].DMTransferedOut = stolons.tissue[3].DM;
                dDMLitter += stolons.tissue[3].DMTransferedOut;

                stolons.tissue[0].NTransferedOut = stolons.tissue[0].Nconc * stolons.tissue[0].DMTransferedOut;
                stolons.tissue[1].NTransferedIn = stolons.tissue[0].NTransferedOut;
                stolons.tissue[1].NTransferedOut = stolons.tissue[1].Nconc * stolons.tissue[1].DMTransferedOut;
                stolons.tissue[2].NTransferedIn = stolons.tissue[1].NTransferedOut;
                stolons.tissue[2].NTransferedOut = stolons.tissue[2].Nconc * stolons.tissue[2].DMTransferedOut;
                stolons.tissue[3].NTransferedIn = stolons.tissue[2].NTransferedOut;
                stolons.tissue[3].NTransferedOut = stolons.tissue[3].Namount;
                dNLitter += stolons.tissue[3].NTransferedOut;
                stolons.tissue[3].NRemobilisable = (stolons.tissue[2].Nconc - stolons.NConcMinimum) * stolons.tissue[2].DMTransferedOut;
            }

            // Roots (note: only two tissue pools)
            roots.tissue[0].DMTransferedOut = gamaR * roots.tissue[0].DM;
            roots.tissue[1].DMTransferedIn = roots.tissue[0].DMTransferedOut;
            roots.tissue[1].DMTransferedOut = roots.tissue[1].DM;
            dDMRootSen = roots.tissue[1].DM;

            roots.tissue[0].NTransferedOut = roots.tissue[0].Nconc * roots.tissue[0].DMTransferedOut;
            roots.tissue[1].NTransferedIn = roots.tissue[0].NTransferedOut;
            roots.tissue[1].NTransferedOut = roots.tissue[1].Namount;
            dNRootSen = roots.tissue[1].NTransferedOut;
            roots.tissue[1].NRemobilisable = (roots.tissue[0].Nconc - roots.NConcMinimum) * roots.tissue[1].DMTransferedOut;

            // Evaluate remobilisable luxury N
            leaves.tissue[0].NRemobilisable = Math.Max(0.0, leaves.tissue[0].Nconc - leaves.NConcOptimum) * leaves.tissue[0].DM * Kappa1;
            leaves.tissue[1].NRemobilisable = Math.Max(0.0, leaves.tissue[1].Nconc - leaves.NConcOptimum * NcRel2) * leaves.tissue[1].DM * Kappa2;
            leaves.tissue[2].NRemobilisable = Math.Max(0.0, leaves.tissue[2].Nconc - leaves.NConcOptimum * NcRel3) * leaves.tissue[2].DM * Kappa3;
            stems.tissue[0].NRemobilisable = Math.Max(0.0, stems.tissue[0].Nconc - stems.NConcOptimum) * stems.tissue[0].DM * Kappa1;
            stems.tissue[1].NRemobilisable = Math.Max(0.0, stems.tissue[1].Nconc - stems.NConcOptimum * NcRel2) * stems.tissue[1].DM * Kappa2;
            stems.tissue[2].NRemobilisable = Math.Max(0.0, stems.tissue[2].Nconc - stems.NConcOptimum * NcRel3) * stems.tissue[2].DM * Kappa3;
            stolons.tissue[0].NRemobilisable = Math.Max(0.0, stolons.tissue[0].Nconc - stolons.NConcOptimum) * stolons.tissue[0].DM * Kappa1;
            stolons.tissue[1].NRemobilisable = Math.Max(0.0, stolons.tissue[1].Nconc - stolons.NConcOptimum * NcRel2) * stolons.tissue[1].DM * Kappa2;
            stolons.tissue[2].NRemobilisable = Math.Max(0.0, stolons.tissue[2].Nconc - stolons.NConcOptimum * NcRel3) * stolons.tissue[2].DM * Kappa3;
            roots.tissue[0].NRemobilisable = Math.Max(0.0, roots.tissue[0].Nconc - roots.NConcOptimum) * roots.tissue[0].DM * Kappa1;

            //Sugar remobilisation and C balance:
            Cremob = 0.0; // not explicitly considered
        }
    }

    /// <summary>Partition DM from new growth</summary>
    internal void EvaluateAllocationNewGrowth()
    {
        if (dGrowth > 0.0) // if no net growth, then skip "partition" part
        {
            // fShoot and fLeaf were calculated on CalcNdemand()

            // Fractions of new growth to be allocated to the 1st tissue pools
            double toRoot = 1.0 - ShootAllocationFactor;
            double toStolon = ShootAllocationFactor * StolonAllocationFactor;
            double toLeaf = ShootAllocationFactor * LeafAllocationFactor;
            double toStem = ShootAllocationFactor * (1.0 - StolonAllocationFactor - LeafAllocationFactor);

            // checking
            double ToAll = toLeaf + toStem + toStolon + toRoot;
            if (Math.Abs(ToAll - 1.0) > 0.0001)
                throw new Exception("Mass balance lost during partition of new growth DM");

            // Allocate the partitioned growth to the 1st tissue pools
            dGrowthShoot = ShootAllocationFactor * dGrowth;
            dGrowthRoot = toRoot * dGrowth;
            leaves.tissue[0].DMTransferedIn = toLeaf * dGrowth;
            stems.tissue[0].DMTransferedIn = toStem * dGrowth;
            stolons.tissue[0].DMTransferedIn = toStolon * dGrowth;
            roots.tissue[0].DMTransferedIn = dGrowthRoot;

            // Set the amount of sugar in each organ
            double fToSugar = 0.5;
            leaves.SugarWt = fToSugar * toLeaf * dGrowth;
            stems.SugarWt = fToSugar * toStem * dGrowth;
            stolons.SugarWt = fToSugar * toStolon * dGrowth;

            // Partition N, based on DM and [N] in each plant part
            double myNsum = (toLeaf * leaves.NConcMaximum)
                            + (toStem * stems.NConcMaximum)
                            + (toStolon * stolons.NConcMaximum)
                            + (toRoot * roots.NConcMaximum);
            if (myNsum > 0.0)
            {
                double toLeafN = toLeaf * leaves.NConcMaximum / myNsum;
                double toStemN = toStem * stems.NConcMaximum / myNsum;
                double toStolN = toStolon * stolons.NConcMaximum / myNsum;
                double toRootN = toRoot * roots.NConcMaximum / myNsum;

                // checking
                ToAll = toLeafN + toStemN + toStolN + toRootN;
                if (Math.Abs(ToAll - 1.0) > 0.0001)
                    throw new Exception("Mass balance lost during partition of new growth N");

                // Allocate new N to the 1st tissue pools
                leaves.tissue[0].NTransferedIn = toLeafN * newGrowthN;
                stems.tissue[0].NTransferedIn = toStemN * newGrowthN;
                stolons.tissue[0].NTransferedIn = toStolN * newGrowthN;
                roots.tissue[0].NTransferedIn = toRootN * newGrowthN;

                dGrowthShootN = leaves.tissue[0].NTransferedIn + stems.tissue[0].NTransferedIn + stolons.tissue[0].NTransferedIn;
                dGrowthRootN = roots.tissue[0].NTransferedIn;
            }
        }
    }

    /// <summary>Update the DM and N of each tissue</summary>
    internal void DoUpdateTissues()
    {
        // Save some variables for mass balance check
        double preTotalWt = AboveGroundWt + roots.DMTotal;
        double preTotalN = AboveGroundN + roots.NTotal;

        // Update each organ, returns test for mass balance
        if (leaves.DoOrganUpdate() == false)
            throw new Exception("Growth and tissue turnover resulted in loss of mass balance for leaves");

        if (stems.DoOrganUpdate() == false)
            throw new Exception("Growth and tissue turnover resulted in loss of mass balance for stems");

        if (stolons.DoOrganUpdate() == false)
            throw new Exception("Growth and tissue turnover resulted in loss of mass balance for stolons");

        if (roots.DoOrganUpdate() == false)
            throw new Exception("Growth and tissue turnover resulted in loss of mass balance for roots");

        // Check for loss of mass balance of total plant
        if (Math.Abs(preTotalWt + dGrowth - dDMLitter - dDMRootSen - (AboveGroundWt + roots.DMTotal)) > Epsilon)
            throw new Exception("  " + speciesName + " - Growth and tissue turnover resulted in loss of mass balance");

        if (Math.Abs(preTotalN + newGrowthN - dNLitter - dNRootSen - NRemobilised2NewGrowth - (AboveGroundN + roots.NTotal)) > Epsilon)
            throw new Exception("  " + speciesName + " - Growth and tissue turnover resulted in loss of mass balance");

        // update some aggregated variables (LAI, height, etc.)
        UpdateAggregatedVariables();
    }

    /// <summary>Update aggregated variables (LAI, height, etc.)</summary>
    internal void UpdateAggregatedVariables()
    {
        // LAI
        EvaluateLAI();

        // Plant height
        height = HeightfromDM();

        // Tissue digestibility
        EvaluateDigestibility();
    }

    /// <summary>Computes the phenology of annual species</summary>
    /// <returns>Whether plant is growing</returns>
    internal bool EvaluatePhenologyOfAnnuals()
    {
        // check whether germination started
        if (Clock.Today.DayOfYear == dayGermn)
            phenoStage = 0; // germinating

        if (phenoStage == 0)
        {
            daysfromEmergence += 1;
            if (daysfromEmergence >= daysToGermn)
            {
                phenoStage = 1; // vegetative stage
                daysfromEmergence = 0;
            }
        }
        else if (phenoStage > 0)
        {
            daysfromEmergence += 1;
            if (daysfromEmergence >= daysEmgToAnth)
                phenoStage = 2; // reproductive stage

            if (daysfromEmergence >= (daysEmgToAnth + daysAnthToMatur))
            {
                phenoStage = -1; // maturity / death
                daysfromEmergence = 0;
            }
        }

        return (phenoStage > 0);
    }

    #endregion  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    #region - Nitrogen uptake processes  - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - -

    /// <summary>Computes the N demand for potential growth (at optimum and luxury N levels)</summary>
    internal void CalcTotalNDemand()
    {
        double toRoot = dGrowthW * (1.0 - ShootAllocationFactor);
        double toStol = dGrowthW * ShootAllocationFactor * StolonAllocationFactor;
        double toLeaf = dGrowthW * ShootAllocationFactor * LeafAllocationFactor;
        double toStem = dGrowthW * ShootAllocationFactor * (1.0 - StolonAllocationFactor - LeafAllocationFactor);

        //N demand for new growth, optimum N (kg/ha)
        NdemandOpt = (toLeaf * leaves.NConcOptimum) + (toStem * stems.NConcOptimum)
                   + (toStol * stolons.NConcOptimum) + (toRoot * roots.NConcOptimum);

        NdemandOpt *= NVariationDueToCO2();
        //this will reduce the N stress under under elevated [co2] for the same soilN

        //N demand for new growth assuming luxury uptake (maximum [N])
        NdemandLux = (toLeaf * leaves.NConcMaximum) + (toStem * stems.NConcMaximum)
                   + (toStol * stolons.NConcMaximum) + (toRoot * roots.NConcMaximum);
        //luxury uptake is not affected by [co2]
    }

    /// <summary>Computes the amount of biologically fixed N</summary>
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

    /// <summary>Calculates the costs of N fixation</summary>
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

    /// <summary>Computes the amount of N remobilised from senescent material used in new growth</summary>
    internal void CalcNRemobSenescent()
    {
        if (NdemandLux <= NSenescedRemobilisable + NFixed)
        {
            // Nremob and/or Nfix are able to supply all N
            NRemobilised2NewGrowth = Math.Max(0.0, NdemandLux - NFixed);
        }
        else
        {
            // not enough N within the plant, uptake is needed
            NRemobilised2NewGrowth = NSenescedRemobilisable;
        }

        // pass the remobilised amount to each organ
        double fracRemob = MathUtility.Divide(NRemobilised2NewGrowth, NSenescedRemobilisable, 0.0);
        leaves.tissue[3].DoRemobiliseN(fracRemob);
        stems.tissue[3].DoRemobiliseN(fracRemob);
        stolons.tissue[3].DoRemobiliseN(fracRemob);
        roots.tissue[1].DoRemobiliseN(fracRemob);

        newGrowthN = NRemobilised2NewGrowth + NFixed;
    }

    /// <summary>Computes the amount of N taken up from soil</summary>
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

    /// <summary>Computes the amount of N remobilisation from luxury N to be used in new growth</summary>
    internal void CalcNRemobLuxury()
    {
        double remainingNdemand = NdemandOpt - newGrowthN;
        if ((remainingNdemand > Epsilon) && (NLuxuryRemobilisable > Epsilon))
        {
            // there is still N demand, check N luxury 
            if (remainingNdemand >= NLuxuryRemobilisable)
            {
                // All luxury N is used up
                NLuxury2NewGrowth = NLuxuryRemobilisable;
                double fracRemob = 1.0;
                for (int t = 0; t < 3; t++)
                {
                    leaves.tissue[t].DoRemobiliseN(fracRemob);
                    stems.tissue[t].DoRemobiliseN(fracRemob);
                    stolons.tissue[t].DoRemobiliseN(fracRemob);
                }
                roots.tissue[0].DoRemobiliseN(fracRemob);
            }
            else
            {
                // N luxury is enough, get first from tissue 3 and proceed downwards
                double fracRemob = 1.0;
                double NLuxTissue = 0.0;
                for (int t = 2; t >= 0; t--)
                {
                    NLuxTissue = leaves.tissue[t].NRemobilisable + stems.tissue[t].NRemobilisable + stolons.tissue[t].NRemobilisable;
                    if (t == 0) NLuxTissue += roots.tissue[t].NRemobilisable;
                    if (NLuxTissue > Epsilon)
                    {
                        if (remainingNdemand >= NLuxTissue)
                        {
                            remainingNdemand -= NLuxTissue;
                            fracRemob = 1.0;
                        }
                        else
                        {
                            fracRemob = remainingNdemand / NLuxTissue;
                            NLuxTissue = remainingNdemand;
                        }

                        NLuxury2NewGrowth += NLuxTissue;
                        leaves.tissue[t].DoRemobiliseN(fracRemob);
                        stems.tissue[t].DoRemobiliseN(fracRemob);
                        stolons.tissue[t].DoRemobiliseN(fracRemob);
                    }
                }
            }
        }
        else
        {
            // no use of luxury N
            NLuxury2NewGrowth = 0.0;
        }

        newGrowthN += NLuxury2NewGrowth;
    }

    #endregion  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    #region - DM allocation and related processes  - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - -

    /// <summary>Computes the allocations into shoot and leaves of today's growth</summary>
    internal void EvaluateAllocationFractions()
    {
        EvaluateAllocationToShoot();
        EvaluateAllocationToLeaf();
    }

    /// <summary>Calculates the fraction of new growth allocated to shoot</summary>
    /// <remarks>
    /// Allocation of new growth to shoot is a function of the current and a target (ideal) Shoot-Root ratio; it is further
    ///  modified according to soil's growth limiting factors (plants favour root growth when water or N are limiting).
    /// The target Shoot-Root ratio will be adjusted (increased) during spring for mimicking changes in DM allocation during
    ///  the reproductive season if usingReproSeasonFactor.
    /// The allocation to shoot may be further modified to ensure a minimum allocation (= 1.0 - MaxRootAllocation).
    /// </remarks>
    private void EvaluateAllocationToShoot()
    {
        if (roots.DMGreen > Epsilon)
        {
            // get the soil related growth limiting factor (the smaller this is the higher the allocation of DM to roots)
            double glfMin = Math.Min(glfWater, glfN);

            // get the actual effect of limiting factors on SR (varies between one and GlfEffectOnSR)
            double glfFactor = 1.0 - GlfEffectOnSR * (1.0 - Math.Pow(glfMin, 1.0 / GlfEffectOnSR));

            // get the current shoot/root ratio (partition will try to make this value closer to targetSR)
            double currentSR = MathUtility.Divide(AboveGroundLiveWt, roots.DMGreen, 1000000.0);

            // get the factor for the reproductive season of perennials (increases shoot allocation during spring)
            double reproFac = 1.0;
            if (UsingReproSeasonFactor && !isAnnual)
                reproFac = CalcReproductiveGrowthFactor();

            // get today's target SR
            double targetSR = TargetSRratio * reproFac;

            // update today's shoot/root partition
            double growthSR = targetSR * glfFactor * targetSR / currentSR;

            // compute fraction to shoot
            ShootAllocationFactor = growthSR / (1.0 + growthSR);
        }
        else
        {
            // use default value, this should not happen (might happen if plant is dead)
            ShootAllocationFactor = 1.0;
        }

        // check for maximum root allocation (kept here mostly for backward compatibility)
        if ((1.0 - ShootAllocationFactor) > MaxRootAllocation)
            ShootAllocationFactor = 1 - MaxRootAllocation;
    }

    /// <summary>Computes the fraction of new shoot DM that is allocated to leaves</summary>
    /// <remarks>
    /// This method is used to reduce the proportion of leaves as plants grow, this is used for species that 
    ///  allocate proportionally more DM to stolon/stems when the whole plant's DM is high.
    /// To avoid too little allocation to leaves in case of grazing, the current leaf:stem ratio is evaluated
    ///  and used to modify the targeted value in a similar way as shoot:root ratio.
    /// </remarks>
    private void EvaluateAllocationToLeaf()
    {
        // compute new target FractionLeaf
        double targetFLeaf = FractionLeafMaximum;
        if ((FractionLeafMinimum < FractionLeafMaximum) && (AboveGroundLiveWt > FractionLeafDMThreshold))
        {
            // compute new target fLeaf
            double fLeafAux = (AboveGroundLiveWt - FractionLeafDMThreshold) / (FractionLeafDMFactor - FractionLeafDMThreshold);
            fLeafAux = Math.Pow(fLeafAux, FractionLeafExponent);
            targetFLeaf = FractionLeafMinimum + (FractionLeafMaximum - FractionLeafMinimum) / (1 + fLeafAux);
        }

        // get today's target leaf:stem ratio
        double targetLS = targetFLeaf / (1 - targetFLeaf);

        // get current leaf:stem ratio
        double currentLS = leaves.DMGreen / (stems.DMGreen + stolons.DMGreen);

        // adjust leaf:stem ratio, to avoid excess allocation to stem/stolons
        double newLS = targetLS * targetLS / currentLS;

        LeafAllocationFactor = newLS / (1 + newLS);
    }

    /// <summary>Calculates variations in root growth and distribution</summary>
    internal void EvaluateRootGrowth()
    {
        if (phenoStage > 0)
        {
            // do root elongation
            if ((dGrowthRoot > 0.0) && (rootDepth < maxRootDepth))
            {
                double tempFactor = GFTemperature(Tmean);
                dRootDepth = rootElongationRate * tempFactor;
                rootDepth = Math.Min(maxRootDepth, Math.Max(minRootDepth, rootDepth + dRootDepth));
            }
            else
            {
                // no root growth, depth does not change
                dRootDepth = 0.0;
            }

            // do root distribution
            if (dRootDepth > 0.0)
            {
                // only need to update root distribution if root depth changed
                double[] curTarget = CurrentRootDistributionTarget();
                for (int layer = 0; layer < dlayer.Length; layer++)
                {
                    // Senesced DM is not accounted for here
                    double newAmountLayer = (roots.DMGreen * rootFraction[layer]) + (dGrowthRoot * curTarget[layer]);
                    rootFraction[layer] = newAmountLayer / (roots.DMGreen + dGrowthRoot);
                }
            }
        }
        else
        {
            dRootDepth = 0.0;
            rootDepth = 0.0;
        }
    }

    /// <summary>Calculates the plant height, as function of DM</summary>
    /// <returns>Plant height</returns>
    internal double HeightfromDM()
    {
        double TodaysHeight = MaxPlantHeight - MinimumHeight;
        double standingDM = (leaves.DMTotal + stems.DMTotal);

        if (standingDM <= MassForMaxHeight)
        {
            double massRatio = standingDM / MassForMaxHeight;
            double heightF = ExponentHeightFromMass - (ExponentHeightFromMass * massRatio) + massRatio;
            heightF *= Math.Pow(massRatio, ExponentHeightFromMass - 1);
            TodaysHeight *= heightF;
        }

        return TodaysHeight + MinimumHeight;
    }

    /// <summary>Calculates the LAI values for green and dead material</summary>
    internal void EvaluateLAI()
    {
        greenLAI = (0.0001 * leaves.DMGreen * SpecificLeafArea)
                   + (0.0001 * stolons.DMTotal * 0.3 * SpecificLeafArea);
        // 0.0001: kg/ha->kg/m2; SLA: m2/kg - assuming stolon = 0.3*SLA
        // Resilience after unfavoured conditions
        // Consider cover will be bigger for the same amount of DM when DM is low due to
        // - light extinction coefficient will be bigger - plant leaves will be more horizontal than in dense high swards
        // - more parts will turn green for photosynthesis (?)
        // - quick response of plant shoots to favoured conditions after release of stress
        // » Specific leaf area should be reduced (RCichota2014)
        if (!isLegume && AboveGroundLiveWt < 1000)
        {
            greenLAI += 0.0001 * stems.DMGreen * SpecificLeafArea * Math.Sqrt((1000 - AboveGroundLiveWt) / 1000);
        }

        deadLAI = 0.0001 * leaves.tissue[3].DM * SpecificLeafArea;
        totalLAI = greenLAI + deadLAI;
    }

    #endregion  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    #endregion  ------------------------------------------------------------------------------------------------------------

    #region Intermittent processes  ----------------------------------------------------------------------------------------

    /// <summary>
    /// Move a given amount of DM and N from live to dead pools
    /// </summary>
    /// <param name="killFaction">The fraction of each live pool to be moved into dead</param>
    internal void KillCrop(double killFaction)
    {
        double fractionRemaining = Math.Max(0.0, 1.0 - killFaction);
        leaves.tissue[3].DM += (leaves.tissue[0].DM + leaves.tissue[1].DM + leaves.tissue[2].DM) * killFaction;
        leaves.tissue[0].DM *= fractionRemaining;
        leaves.tissue[1].DM *= fractionRemaining;
        leaves.tissue[2].DM *= fractionRemaining;
        stems.tissue[3].DM += (stems.tissue[0].DM + stems.tissue[1].DM + stems.tissue[2].DM) * killFaction;
        stems.tissue[0].DM *= fractionRemaining;
        stems.tissue[1].DM *= fractionRemaining;
        stems.tissue[2].DM *= fractionRemaining;
        stolons.tissue[3].DM += (stolons.tissue[0].DM + stolons.tissue[1].DM + stolons.tissue[2].DM) * killFaction;
        stolons.tissue[0].DM *= fractionRemaining;
        stolons.tissue[1].DM *= fractionRemaining;
        stolons.tissue[2].DM *= fractionRemaining;
        roots.tissue[1].DM += roots.tissue[0].DM * killFaction;
        roots.tissue[0].DM *= fractionRemaining;

        // note: N concentration do not change for green material, so do not need to update N amount;
        leaves.tissue[3].Namount += (leaves.tissue[0].Namount + leaves.tissue[1].Namount + leaves.tissue[2].Namount) * killFaction;
        leaves.tissue[0].Namount *= fractionRemaining;
        leaves.tissue[1].Namount *= fractionRemaining;
        leaves.tissue[2].Namount *= fractionRemaining;
        stems.tissue[3].Namount += (stems.tissue[0].Namount + stems.tissue[1].Namount + stems.tissue[2].Namount) *killFaction;
        stems.tissue[0].Namount *= fractionRemaining;
        stems.tissue[1].Namount *= fractionRemaining;
        stems.tissue[2].Namount *= fractionRemaining;
        stolons.tissue[3].Namount += (stolons.tissue[0].Namount + stolons.tissue[1].Namount + stolons.tissue[2].Namount) * killFaction;
        stolons.tissue[0].Namount *= fractionRemaining;
        stolons.tissue[1].Namount *= fractionRemaining;
        stolons.tissue[2].Namount *= fractionRemaining;
        roots.tissue[1].Namount += roots.tissue[0].Namount * killFaction;
        roots.tissue[0].Namount *= fractionRemaining;

        UpdateAggregatedVariables();
    }

    /// <summary>Removes some plant DM</summary>
    /// <param name="amountToRemove">The DM amount to remove</param>
    /// <returns>Amount removed</returns>
    internal double RemoveDM(double amountToRemove)
    {
        // save current state
        double preRemovalDMShoot = AboveGroundWt;
        double preRemovalNShoot = AboveGroundN;

        // get the weights for each pool, consider preference and available DM
        double tempPrefGreen = PreferenceGreenOverDead + (amountToRemove / StandingWt);
        double tempPrefDead = 1.0 + (PreferenceGreenOverDead * amountToRemove / StandingWt);
        double tempRemovableGreen = Math.Max(0.0, leaves.DMGreenHarvestable + stems.DMGreenHarvestable + stolons.DMGreenHarvestable);
        double tempRemovableDead = Math.Max(0.0, StandingDeadWt);

        // get partition between dead and live materials
        double tempTotal = tempRemovableGreen * tempPrefGreen + tempRemovableDead * tempPrefDead;
        double fractionToHarvestGreen = tempRemovableGreen * tempPrefGreen / tempTotal;
        double fractionToHarvestDead = tempRemovableDead * tempPrefDead / tempTotal;

        // get amounts removed
        double removingGreenDM = amountToRemove * fractionToHarvestGreen;
        double removingDeadDM = amountToRemove * fractionToHarvestDead;

        // Fraction of DM remaining in the field
        double fractionRemainingGreen = 1.0;
        if (StandingLiveWt > Epsilon)
            fractionRemainingGreen -= Math.Min(1.0, removingGreenDM / StandingLiveWt);
        double fractionRemainingDead = 1.0;
        if (StandingDeadWt > Epsilon)
            fractionRemainingDead -= Math.Min(1.0, removingDeadDM / StandingDeadWt);
        double fractionRemainingStolon = 1.0;
        if (StandingLiveWt > Epsilon)
            fractionRemainingStolon -= Math.Min(1.0, removingGreenDM * stolons.FractionStanding / StandingLiveWt);

        if ((fractionRemainingGreen < -Epsilon) || (fractionRemainingDead < -Epsilon) || (fractionRemainingStolon < -Epsilon))
            throw new Exception(" AgPasture - partition of DM removed resulted in loss of mass balance");

        // get digestibility of DM being harvested
        double greenDigestibility = (leaves.DigestibilityLive * leaves.DMGreen) + (stems.DigestibilityLive * stems.DMGreen);
        greenDigestibility += (stolons.DigestibilityLive * stolons.DMGreen * stolons.FractionStanding);
        double deadDigestibility = (leaves.DigestibilityDead * leaves.DMDead) + (stems.DigestibilityDead * stems.DMDead);
        greenDigestibility = MathUtility.Divide(greenDigestibility, StandingLiveWt, 0.0);
        deadDigestibility = MathUtility.Divide(deadDigestibility, StandingDeadWt, 0.0);
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
        stolons.tissue[0].DM *= fractionRemainingStolon;
        stolons.tissue[1].DM *= fractionRemainingStolon;
        stolons.tissue[2].DM *= fractionRemainingStolon;

        leaves.tissue[0].Namount *= fractionRemainingGreen;
        leaves.tissue[1].Namount *= fractionRemainingGreen;
        leaves.tissue[2].Namount *= fractionRemainingGreen;
        leaves.tissue[3].Namount *= fractionRemainingDead;
        stems.tissue[0].Namount *= fractionRemainingGreen;
        stems.tissue[1].Namount *= fractionRemainingGreen;
        stems.tissue[2].Namount *= fractionRemainingGreen;
        stems.tissue[3].Namount *= fractionRemainingDead;
        stolons.tissue[0].Namount *= fractionRemainingStolon;
        stolons.tissue[1].Namount *= fractionRemainingStolon;
        stolons.tissue[2].Namount *= fractionRemainingStolon;

        // Update Luxury N pools
        for (int t = 0; t < 3; t++)
        {
            leaves.tissue[t].NRemobilisable *= fractionRemainingGreen;
            stems.tissue[t].NRemobilisable *= fractionRemainingGreen;
            stolons.tissue[t].NRemobilisable *= fractionRemainingGreen;
        }

        leaves.tissue[3].NRemobilisable *= fractionRemainingDead;
        stems.tissue[3].NRemobilisable *= fractionRemainingDead;
        stolons.tissue[3].NRemobilisable *= fractionRemainingDead;

        // Update the aggregated variables (LAI, height, etc.)
        UpdateAggregatedVariables();

        // Check balance and set outputs
        dmdefoliated = preRemovalDMShoot - AboveGroundWt;
        Ndefoliated = preRemovalNShoot - AboveGroundN;
        fractionDefoliated = dmdefoliated / preRemovalDMShoot;
        if (Math.Abs(dmdefoliated - amountToRemove) > 0.00001)
            throw new Exception("  AgPasture - removal of DM resulted in loss of mass balance");

        return dmdefoliated;
    }

    #endregion  ------------------------------------------------------------------------------------------------------------

    #region Auxiliary functions and processes  -----------------------------------------------------------------------------

    /// <summary>Growth factor for annual species, related to phenology/population</summary>
    /// <returns>Growth factor</returns>
    public double AnnualSpeciesGrowthFactor()
    {
        double rFactor = 1.0;
        if (phenoStage == 1 && daysfromEmergence < 60)
        {
            //decline at the beginning due to population effects ???
            rFactor = 0.5 + 0.5 * daysfromEmergence / 60;
        }
        else if (phenoStage == 2)
        {
            //decline of photosynthesis when approaching maturity
            rFactor = 1.0 - (daysfromEmergence - daysEmgToAnth) / daysAnthToMatur;
        }

        return rFactor;
    }

    /// <summary>Effects of atmospheric [CO2] on plant photosynthesis</summary>
    /// <returns>Growth factor</returns>
    private double CO2EffectsOnPhotosynthesis()
    {
        double termActual = (CO2 + CO2PmaxScale) / CO2;
        double termReference = (referenceCO2 + CO2PmaxScale) / referenceCO2;
        return termReference / termActual;
    }

    /// <summary>N concentration effects on photosynthesis</summary>
    /// <returns>Growth factor</returns>
    private double NEffectOnPhotosynthesis()
    {
        double Fn = NVariationDueToCO2();

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

    /// <summary>Plant nitrogen [N] decline due to elevated [CO2]</summary>
    /// <returns>Effect on plant N concentration</returns>
    private double NVariationDueToCO2()
    {
        if (CO2 <= referenceCO2)
            return 1.0;

        double factorCO2 = Math.Pow((CO2NScale - referenceCO2) / (CO2 - referenceCO2), CO2NCurvature);
        double Fn = (CO2NMin + factorCO2) / (1 + factorCO2);
        return Fn;
    }

    /// <summary>Canopy conductance decline to elevated [CO2]</summary>
    /// <returns>Effects on canopy conductance</returns>
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
    /// The mean temperature for the day
    /// </summary>
    private double Tmean
    {
        get { return 0.5 * (MetFile.MaxT + MetFile.MinT); }
    }

    /// <summary>Heat effects on photosynthesis</summary>
    /// <returns>Growth factor</returns>
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

    /// <summary>Cold effect on photosynthesis</summary>
    /// <returns>Growth factor</returns>
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

    /// <summary>Growth limiting factor due to temperature</summary>
    /// <param name="T">Temperature</param>
    /// <returns>Growth factor</returns>
    internal double GFTemperature(double T)
    {
        if (photoPath == "C4")
            return GFTempC4(T);
        else
            return GFTempC3(T);
    }

    /// <summary>Temperature effects on photosynthesis for C3 plants</summary>
    /// <param name="T">Temperature</param>
    /// <returns>Growth factor</returns>
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

    /// <summary>Temperature effects on photosynthesis for C4 plants</summary>
    /// <param name="T">Temperature</param>
    /// <returns>Growth factor</returns>
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

    /// <summary>Computes the effects of temperature on respiration</summary>
    /// <returns>Respiration factor</returns>
    private double TemperatureEffectOnRespiration()
    {
        double result;
        if (Tmean <= 0.0)
        {
            // too cold, no respiration
            result = 0.0;
        }
        else
        {
            double scalef = 1.0 - Math.Exp(-1.0);
            double baseEffect = 1.0 - Math.Exp(-Math.Pow(Tmean / respTref, respExponent));
            result = baseEffect / scalef;
        }

        return result;
    }

    /// <summary>Effect of temperature on tissue turnover rate</summary>
    /// <returns>Turnover factor</returns>
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

    /// <summary>Effect of water stress on tissue turnover rate</summary>
    /// <returns>Turnover factor</returns>
    private double MoistureEffectOnTissueTurnover()
    {
        double result = 1.0;
        if (Math.Min(glfWater, glfAeration) < massFluxWopt)
            result = 1.0 + (massFluxW0 - 1.0) * ((massFluxWopt - Math.Min(glfWater, glfAeration)) / massFluxWopt);

        return Math.Max(1.0, Math.Min(massFluxW0, result));
    }

    /// <summary>Calculate the factor increasing DM allocation to shoot during reproductive growth</summary>
    /// <remarks>
    /// This mimics the changes in DM allocation during reproductive season; allocation to shoot increases up to a maximum
    ///  value (defined by allocationIncreaseRepro). This value is used during the main phase, two shoulder periods are
    ///  defined on either side of the main phase (duration is given by reproSeasonInterval, translated into days of year),
    ///  Onset phase goes between doyA and doyB, main phase between doyB and doyC, and outset between doyC and doyD.
    /// NOTE: The days have to be set as doubles or the division operations will be rounded and be slightly wrong
    /// </remarks>
    /// <returns>A factor to correct shoot allocation</returns>
    private double CalcReproductiveGrowthFactor()
    {
        double result = 1.0;
        int yearLength = 365 + (DateTime.IsLeapYear(Clock.year) ? 1 : 0);
        double doy = Clock.Today.DayOfYear;
        double doyC = doyIniReproSeason;
        double doyF = doyC + reproSeasonInterval[0];
        double doyD = doyF + reproSeasonInterval[1];
        double doyE = doyD + reproSeasonInterval[2];

        if (doy > doyC)
        {
            if (doy <= doyF)
                result = 1.0 + allocationIncreaseRepro * (doy - doyC) / (doyF - doyC);
            else if (doy <= doyD)
                result = 1.0 + allocationIncreaseRepro;
            else if (doy <= doyE)
                result = 1 + allocationIncreaseRepro * (1 - (doy - doyD) / (doyE - doyD));
        }
        else
        {
            // check whether the high allocation period goes across the year (should only be needed for southern hemisphere)
            if ((doyD > yearLength) && (doy <= doyD - yearLength))
                result = 1.0 + allocationIncreaseRepro;
            else if ((doyE > yearLength) && (doy <= doyE - yearLength))
                result = 1.0 + allocationIncreaseRepro * (1 - (yearLength + doy - doyD) / (doyE - doyD));
        }

        return result;
    }

    /// <summary>Gets the green tissues cover</summary>
    internal double coverGreen
    {
        get { return (1.0 - Math.Exp(-lightExtCoeff * greenLAI)); }
    }

    /// <summary>Gets the dead tissues cover</summary>
    internal double coverDead
    {
        get { return (1.0 - Math.Exp(-lightExtCoeff * deadLAI)); }
    }

    /// <summary>Gets the total plant cover</summary>
    internal double coverTotal
    {
        get { return (1.0 - (Math.Exp(-lightExtCoeff * totalLAI))); }
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
        double cumProportion = 0.0;
        for (int layer = 0; layer < nLayers; layer++)
        {
            if (currentDepth < rootDepth)
            {
                // layer is within the root zone
                currentDepth += dlayer[layer];
                if (currentDepth <= rootDepth)
                {
                    // layer is fully in the root zone
                    cumProportion += targetRootAllocation[layer];
                }
                else
                {
                    // layer is partially in the root zone
                    double layerFrac = (rootDepth - (currentDepth - dlayer[layer]))
                                     / (maxRootDepth - (currentDepth - dlayer[layer]));
                    cumProportion += targetRootAllocation[layer] * Math.Min(1.0, Math.Max(0.0, layerFrac));
                }
            }
            else
                layer = nLayers;
        }

        double[] result = new double[nLayers];
        for (int layer = 0; layer < nLayers; layer++)
            result[layer] = targetRootAllocation[layer] / cumProportion;

        return result;
    }

    /// <summary>
    /// Compute how much of the layer is actually explored by roots
    /// </summary>
    /// <param name="layer"></param>
    /// <returns>Fraction of layer explored by roots</returns>
    internal double LayerFractionWithRoots(int layer)
    {
        double depthToTopOfLayer = 0.0;
        for (int i = 0; i < layer; i++)
            depthToTopOfLayer += dlayer[i];
        double fractionInLayer = (rootDepth - depthToTopOfLayer) / dlayer[layer];

        return Math.Min(1.0, Math.Max(0.0, fractionInLayer));
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
    /// Calculates the average herbage digestibility (standing herbage)
    /// </summary>
    /// <returns>digestibility</returns>
    internal void EvaluateDigestibility()
    {
        double result = 0.0;
        if (StandingWt > 0.0)
        {
            result = (leaves.DigestibilityLive * leaves.DMGreen) + (leaves.DigestibilityDead * leaves.DMDead)
                   + (stems.DigestibilityLive * stems.DMGreen) + (stems.DigestibilityDead * stems.DMDead)
                   + (stolons.DigestibilityLive * stolons.DMGreen * stolons.FractionStanding);
            result /= StandingWt;
        }

        digestHerbage = result;
    }

    #endregion

    #region Auxiliary classes  ---------------------------------------------------------------------------------------------

    /// <summary>Defines a generic organ of a plant</summary>
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

        #region Organ specific characteristics  -----------------------------------------------------------------------

        /// <summary>N concentration for optimal growth (kg/kg)</summary>
        internal double NConcOptimum = 4.0;

        /// <summary>Maximum N concentration, for luxury uptake (kg/kg)</summary>
        internal double NConcMaximum = 6.0;

        /// <summary>Minimum N concentration, structural N (kg/kg)</summary>
        internal double NConcMinimum = 1.2;

        /// <summary>Minimum DM amount of live tissues [kg/ha]</summary>
        internal double MinimumGreenDM = 0.0;

        /// <summary>Proportion of organ DM that is standing, available to harvest [0-1]</summary>
        internal double FractionStanding = 1.0;

        /// <summary>Digestibility of cell walls in live tissues [0-1]</summary>
        internal double DigestLiveCellWall = 0.6;

        /// <summary>Digestibility of cell walls in dead tissues [0-1]</summary>
        internal double DigestDeadCellWall = 0.2;

        /// <summary>The fraction of DM in sugar form for this organ [0-1]</summary>
        internal double SugarWt = 0.1;

        #endregion  ---------------------------------------------------------------------------------------------------

        #region Organ Properties (summary of tissues)  ----------------------------------------------------------------

        /// <summary>Number of tissue pools to create</summary>
        internal int nTissues;

        /// <summary>The total dry matter in this organ (kg/ha)</summary>
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

        /// <summary>The total dry matter in this organ available to harvest (kg/ha)</summary>
        internal double DMTotalHarvestable
        {
            get { return DMGreenHarvestable + DMDeadHarvestable; }
        }

        /// <summary>The dry matter in the live (green) tissues available to harvest (kg/ha)</summary>
        internal double DMGreenHarvestable
        {
            get { return Math.Max(0.0, Math.Min(DMGreen - MinimumGreenDM, DMGreen * FractionStanding)); }
        }

        /// <summary>The dry matter in the dead tissues available to harvest (kg/ha)</summary>
        internal virtual double DMDeadHarvestable
        {
            get { return DMDead * FractionStanding; }
        }

        /// <summary>Gets the DM amount added to this organ via growth [kg/ha]</summary>
        internal double DMGrowth
        {
            get { return tissue[0].DMTransferedIn; }
        }

        /// <summary>Gets the DM amount detached from this organ [kg/ha]</summary>
        internal double DMDetached
        {
            get { return tissue[nTissues - 1].DMTransferedOut; }
        }

        /// <summary>The total N amount in this organ (kg/ha)</summary>
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

        /// <summary>The average N concentration in this organ (kg/kg)</summary>
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

        /// <summary>The total N amount in the harvestable DM of this organ(kg/ha)</summary>
        internal double NTotalHarvestable
        {
            get { return (DMGreenHarvestable * NconcGreen) + (DMDeadHarvestable * NconcDead); }
        }

        /// <summary>Gets the amount of N added to this organ via growth [kg/ha]</summary>
        internal double NGrowth
        {
            get { return tissue[0].NTransferedIn; }
        }

        /// <summary>Gets the amount of N detached from this organ [kg/ha]</summary>
        internal double NDetached
        {
            get { return tissue[nTissues - 1].NTransferedOut; }
        }


        /// <summary>The total N amount available to remobilise from senesced tissues in this organ (kg/ha)</summary>
        internal double NSenesced
        {
            get { return tissue[nTissues - 1].NRemobilisable; }
        }

        /// <summary>Gets the amount of senesced N remobilised into new growth [kg/ha]</summary>
        internal double NSenescedRemobilised
        {
            get { return tissue[nTissues - 1].NRemobilised; }
        }

        /// <summary>The total N amount available to remobilise from luxury N in this organ (kg/ha)</summary>
        internal double NLuxury
        {
            get
            {
                double result = 0.0;
                for (int t = 0; t < nTissues - 1; t++)
                {
                    result += tissue[t].NRemobilisable;
                }

                return result;
            }
        }
        
        /// <summary>Gets the amount of luxury N remobilised into new growth [kg/ha]</summary>
        internal double NLuxuryRemobilised
        {
            get
            {
                double result = 0.0;
                for (int t = 0; t < nTissues - 1; t++)
                    result += tissue[t].NRemobilised;

                return result;
            }
        }

        #endregion  ---------------------------------------------------------------------------------------------------

        #region Organ Methods  ----------------------------------------------------------------------------------------

        /// <summary>The digestibility of live plant material</summary>
        /// <remarks>
        /// Assumes fixed CN ratios and digestibilities of various materials
        /// Digestibility of sugars (dissolved carbohydrates) and proteins is assumed to be one
        /// </remarks>
        /// <returns>the digestibility for this organ</returns>
        internal double DigestibilityLive
        {
            get
            {
                double result = 0.0;
                double tissueCN = MathUtility.Divide(DMGreen * CarbonFractionDM, NGreen, 0.0);

                if (tissueCN > 0.0)
                {
                    //Fraction of protein in the tissue
                    double fSugar = SugarWt / DMGreen;
                    double fProtein = (fSugar - 1.0 + (CNw / tissueCN)) * (ProteinCNr / (CNw - ProteinCNr));
                    //Fraction of non-soluble material in the tissue (cell wall)
                    double fCellWall = 1.0 - fSugar - fProtein;

                    result = fSugar + fProtein + fCellWall * DigestLiveCellWall;
                }

                return result;
            }
        }

        /// <summary>The digestibility of dead plant material</summary>
        /// <remarks>
        /// Assumes fixed CN ratios and digestibilities of various materials
        /// Digestibility of proteins is assumed to be one, assume no sugars in dead material
        /// </remarks>
        /// <returns>the digestibility for this organ</returns>
        internal double DigestibilityDead
        {
            get
            {
                double result = 0.0;
                double tissueCN = MathUtility.Divide(DMDead * CarbonFractionDM, NDead, 0.0);

                if (tissueCN > 0.0)
                {
                    //Fraction of protein in the tissue
                    double fProtein = ((CNw - tissueCN) / tissueCN) * (ProteinCNr / (CNw - ProteinCNr));
                    //Fraction of non-soluble material in the tissue (cell wall)
                    double fCellWall = 1.0 - fProtein;

                    result = fProtein + fCellWall * DigestDeadCellWall;
                }

                return result;
            }
        }

        /// <summary>Updates each tissue, make changes in DM and N effective</summary>
        /// <returns>A flag whether mass balance was maintained or not</returns>
        internal bool DoOrganUpdate()
        {
            // save current state
            double previousDM = DMTotal;
            double previousN = NTotal;

            // update all tissues
            for (int t = 0; t < nTissues; t++)
                tissue[t].DoUpdateTissue();

            // check mass balance
            bool dmIsOk = Math.Abs(previousDM + DMGrowth - DMDetached - DMTotal) <= Epsilon;
            bool nIsOk = Math.Abs(previousN + NGrowth - NDetached - NSenescedRemobilised - NTotal) <= Epsilon;
            return (dmIsOk || nIsOk);
        }

        /// <summary>Reset the transfer amounts in all tissues of this organ</summary>
        internal void DoCleanTransferAmounts()
        {
            for (int t = 0; t < nTissues; t++)
            {
                tissue[t].DMTransferedIn = 0.0;
                tissue[t].DMTransferedOut = 0.0;
                tissue[t].NTransferedIn = 0.0;
                tissue[t].NTransferedOut = 0.0;
                tissue[t].NRemobilisable = 0.0;
                tissue[t].NRemobilised = 0.0;
            }
        }

        /// <summary>Reset all amounts to zero in all tissues of this organ</summary>
        internal void DoResetOrgan()
        {
            for (int t = 0; t < nTissues; t++)
            {
                tissue[t].DM = 0.0;
                tissue[t].Namount = 0.0;
                tissue[t].Pamount = 0.0;
                DoCleanTransferAmounts();
            }
        }

        #endregion  ---------------------------------------------------------------------------------------------------

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
            
            // >> Amounts in and out ..................................................................

            /// <summary>Gets or sets the DM amount transferred into this tissue [kg/ha]</summary>
            internal double DMTransferedIn = 0.0;

            /// <summary>Gets or sets the DM amount transferred out of this tissue [kg/ha]</summary>
            internal double DMTransferedOut = 0.0;

            /// <summary>Gets or sets the amount of N transferred into this tissue [kg/ha]</summary>
            internal double NTransferedIn = 0.0;

            /// <summary>Gets or sets the amount of N transferred out of this tissue [kg/ha]</summary>
            internal double NTransferedOut = 0.0;

            /// <summary>Gets or sets the amount of N available for remobilisation [kg/ha]</summary>
            internal double NRemobilisable = 0.0;

            /// <summary>Gets or sets the amount of N remobilised into new growth [kg/ha]</summary>
            internal double NRemobilised = 0.0;

            /// <summary>Removes a fraction of remobilisable N for use into new growth</summary>
            /// <param name="fraction">fraction to remove [0-1]</param>
            internal void DoRemobiliseN(double fraction)
            {
                NRemobilised += NRemobilisable * fraction;
            }

            /// <summary>Updates the tissue state, make changes in DM and N effective</summary>
            internal virtual void DoUpdateTissue()
            {
                DM += DMTransferedIn - DMTransferedOut;
                Namount += NTransferedIn - (NTransferedOut + NRemobilised);
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

    #endregion  ------------------------------------------------------------------------------------------------------------
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
