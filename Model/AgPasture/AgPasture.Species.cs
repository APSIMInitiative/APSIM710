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

    /// <summary>Actual name of this species</summary>
    internal string speciesName;

    /// <summary>Plant type for micromet</summary>
    internal string micrometType;

    /// <summary>Metabolic pathway for C fixation during photosynthesis (C3 or C4)</summary>
    internal string myPhotosyntheticPathway;

    /// <summary>Whether the plant is a legume species (1=yes, 0=no)</summary>
    internal bool isLegume;

    /// <summary>Whether the plant is an annual species (1=yes, 0=no)</summary>
    internal bool isAnnual;

    ////- Potential growth (photosynthesis) >>> - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    /// <summary>Reference leaf CO2 assimilation rate for photosynthesis</summary>
    internal double myReferencePhotosyntheticRate;

    /// <summary>Relative factor for light partition between species</summary>
    internal double myLightPartitioningFactor;

    /// <summary>Leaf photosynthetic efficiency</summary>
    internal double myPhotosyntheticEfficiency;

    /// <summary>Photosynthesis curvature parameter</summary>
    internal double myPhotosynthesisCurveFactor;

    /// <summary>Fraction of radiation that is photosynthetic active</summary>
    internal double myFractionPAR;

    /// <summary>Light extinction coefficient</summary>
    internal double myLightExtinctionCoefficient;

    /// <summary>Reference CO2 concentration for photosynthesis</summary>
    internal double myReferenceCO2 = 380;

    /// <summary>Scaling parameter for the CO2 effect on photosynthesis</summary>
    internal double myCO2EffectScaleFactor;

    /// <summary>Scaling parameter for the CO2 effects on N requirements</summary>
    internal double myCO2EffectOffsetFactor;

    /// <summary>Minimum value for the CO2 effect on N requirements</summary>
    internal double myCO2EffectMinimum;

    /// <summary>Exponent controlling the CO2 effect on N requirements</summary>
    internal double myCO2EffectExponent;

    /// <summary>Minimum temperature for growth</summary>
    internal double myGrowthTminimum;

    /// <summary>Optimum temperature for growth</summary>
    internal double myGrowthToptimum;

    /// <summary>Curve parameter for growth response to temperature</summary>
    internal double myGrowthTEffectExponent;

    /// <summary>Whether heat damage stress is enabled</summary>
    internal bool usingHeatStress = false;

    /// <summary>Onset temperature for heat effects on photosynthesis</summary>
    internal double myHeatOnsetTemperature;

    /// <summary>Temperature for full heat effect on photosynthesis</summary>
    internal double myHeatFullTemperature;

    /// <summary>Cumulative degrees-day for recovery from heat stress</summary>
    internal double myHeatRecoverySumDD;

    /// <summary>Reference temperature for recovery from heat stress</summary>
    internal double myHeatRecoveryTReference;

    /// <summary>Whether cold damage stress is enabled</summary>
    internal bool usingColdStress = false;

    /// <summary>Onset temperature for cold effects on photosynthesis</summary>
    internal double myColdOnsetTemperature;

    /// <summary>Temperature for full cold effect on photosynthesis</summary>
    internal double myColdFullTemperature;

    /// <summary>Cumulative degrees for recovery from cold stress</summary>
    internal double myColdRecoverySumDD;

    /// <summary>Reference temperature for recovery from cold stress</summary>
    internal double myColdRecoveryTReference;

    ////- Respiration parameters >>>  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    /// <summary>Maintenance respiration coefficient</summary>
    internal double myMaintenanceRespirationCoefficient;

    /// <summary>Growth respiration coefficient</summary>
    internal double myGrowthRespirationCoefficient;

    /// <summary>Reference temperature for maintenance respiration</summary>
    internal double myRespirationTReference;

    /// <summary>Exponent controlling the effect of temperature on respiration</summary>
    internal double myRespirationExponent;

    ////- N concentrations thresholds >>> - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    /// <summary>N concentration for plants at stage 2 (developing), relative to optimum</summary>
    internal double NcRel2;

    /// <summary>N concentration for plants at stage 3 (mature), relative to optimum</summary>
    internal double NcRel3;

    ////- Germination and emergence >>> - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    /// <summary>Cumulative degrees-day needed for seed germination</summary>
    internal double myDegreesDayForGermination;

    /// <summary>The fractions of DM for each plant part at emergence, for all plants</summary>
    internal double[] emergenceDM;

    ////- Allocation of new growth >>>  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    /// <summary>Target or ideal plant's shoot:root ratio at vegetative stage</summary>
    internal double myTargetShootRootRatio;

    /// <summary>Maximum fraction of DM growth that can be allocated to roots</summary>
    internal double MaxRootAllocation;

    /// <summary>Maximum effect that soil GLFs have on Shoot-Root ratio</summary>
    internal double myShootRootGlfFactor = 0.5;

    /// <summary>Whether Shoot:Root ratio should be adjusted to mimic DM allocation during reproductive season (perennial species)</summary>
    internal bool UsingReproSeasonFactor = true;

    /// <summary>Reference latitude determining timing for reproductive season</summary>
    internal double myReproSeasonReferenceLatitude = 41;

    /// <summary>Coefficient controlling the time to start the reproductive season as function of latitude</summary>
    internal double myReproSeasonTimingCoeff = 0.14;

    /// <summary>Coefficient controlling the duration of the reproductive season as function of latitude</summary>
    internal double myReproSeasonDurationCoeff = 2.0;

    /// <summary>Ratio between the length of shoulders and the period with full reproductive growth effect</summary>
    internal double myReproSeasonShouldersLengthFactor = 1.0;

    /// <summary>The proportion of the length of shoulder before the period with full reproductive growth effect</summary>
    internal double myReproSeasonOnsetDurationFactor = 0.6;

    /// <summary>Maximum increase in DM allocation to shoot during reproductive growth</summary>
    internal double myReproSeasonMaxAllocationIncrease = 0.5;

    /// <summary>Coefficient controlling the increase in shoot allocation during reproductive growth as function of latitude</summary>
    internal double myReproSeasonAllocationCoeff = 0.10;

    /// <summary>Maximum target allocation of new growth to leaves</summary>
    internal double myFractionLeafMaximum;

    /// <summary>Minimum target allocation of new growth to leaves</summary>
    internal double myFractionLeafMinimum;

    /// <summary>Shoot DM at which allocation of new growth to leaves start to decrease</summary>
    internal double myFractionLeafDMThreshold;

    /// <summary>Shoot DM when allocation to leaves is midway maximum and minimum</summary>
    internal double myFractionLeafDMFactor;

    /// <summary>Exponent of function describing DM allocation to leaves</summary>
    internal double myFractionLeafExponent;

    /// <summary>Fraction of new growth to be allocated to stolon</summary>
    internal double myFractionToStolon;

    /// <summary>Specific leaf area, per dry matter weight</summary>
    internal double mySpecificLeafArea;

    /// <summary>Specific root length, per dry matter weight</summary>
    internal double mySpecificRootLength;

    /// <summary>Fraction of stolon tissue used when computing green LAI</summary>
    internal double myStolonEffectOnLAI = 0.0;

    /// <summary>Maximum aboveground biomass for using stems when computing LAI</summary>
    internal double myShootMaxEffectOnLAI = 1000;

    /// <summary>Maximum effect of stems when computing green LAI</summary>
    internal double myMaxStemEffectOnLAI = 1.0;

    ////- Tissue turnover and senescence >>>  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    /// <summary>Number of live leaves per tiller</summary>
    internal double myLiveLeavesPerTiller;

    /// <summary>Reference daily DM turnover rate for shoot tissues</summary>
    internal double myTissueTurnoverRateShoot;

    /// <summary>Reference daily DM turnover rate for root tissues</summary>
    internal double myTissueTurnoverRateRoot;

    /// <summary>Relative turnover rate for emerging tissues</summary>
    internal double myRelativeTurnoverEmerging;

    /// <summary>Reference daily detachment rate for dead tissues</summary>
    internal double myDetachmentRateShoot;

    /// <summary>Minimum temperature for tissue turnover</summary>
    internal double myTurnoverTemperatureMin;

    /// <summary>Reference temperature for tissue turnover</summary>
    internal double myTurnoverTemperatureRef;

    /// <summary>Exponent of function for temperature effect on tissue turnover</summary>
    internal double myTurnoverTemperatureExponent;

    /// <summary>Maximum increase in tissue turnover due to water deficit</summary>
    internal double myTurnoverDroughtEffectMax;

    /// <summary>Minimum GLFwater without effect on tissue turnover</summary>
    internal double myTurnoverDroughtThreshold;

    /// <summary>Coefficient controlling detachment rate as function of moisture</summary>
    internal double myDetachmentDroughtCoefficient;

    /// <summary>Minimum effect of drought on detachment rate</summary>
    internal double myDetachmentDroughtEffectMin;

    /// <summary>Factor increasing tissue turnover rate due to stock trampling</summary>
    internal double myTurnoverStockFactor;

    /// <summary></summary>
    internal static double stockingRate = 0.0;
    //stocking rate affecting transfer of dead to litter (default as 0 for now)

    /// <summary>Coefficient of function increasing the stolons turnover rate due to defoliation</summary>
    internal double myTurnoverDefoliationCoefficient;

    /// <summary>Minimum significant daily effect of defoliation on stolons turnover rate</summary>
    internal double myTurnoverDefoliationEffectMin;

    /// <summary>Increase in root turnover rate due to defoliation relative to stolons</summary>
    internal double myTurnoverDefoliationRootEffect;

    /// <summary>Fraction of luxury N remobilisable from living tissue</summary>
    internal double[] FractionNLuxuryRemobilisable;

    ////- N fixation (for legumes) >>>  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    /// <summary>Maximum fraction of N demand supplied by biologic N fixation</summary>
    internal double myMinimumNFixation;

    /// <summary>Minimum fraction of N demand supplied by biologic N fixation</summary>
    internal double myMaximumNFixation;

    /// <summary>Whether the costs for N fixation are computed explicitly</summary>
    internal bool usingNFixationCost;

    /// <summary>Respiration cost factor due to the presence of symbiont bacteria</summary>
    internal double mySymbiontCostFactor;

    /// <summary>Respiration cost factor due to the activity of symbiont bacteria</summary>
    internal double myNFixingCostFactor;

    ////- Growth limiting factors >>> - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    /// <summary>Maximum reduction in plant growth due to water logging (saturated soil)</summary>
    internal double mySoilSaturationEffectMax;

    /// <summary>Minimum water-free pore space for growth with no limitations</summary>
    internal double myMinimumWaterFreePorosity;

    /// <summary>Daily recovery rate from water logging</summary>
    internal double mySoilSaturationRecoveryFactor;

    /// <summary>Exponent for modifying the effect of N deficiency on plant growth</summary>
    internal double myNDillutionCoefficient;

    /// <summary>Generic factor affecting potential plant growth</summary>
    internal double myGlfGeneric;

    /// <summary>Generic growth limiting factor due to soil fertility</summary>
    internal double myGlfSoilFertility;

    ////- Plant height >>>  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    /// <summary>Minimum plant height, for each species in the sward</summary>
    internal double myPlantHeightMaximum;

    /// <summary>Maximum plant height, for each species in the sward</summary>
    internal double myPlantHeightMinimum;

    /// <summary>DM weight above ground for maximum plant height</summary>
    internal double myPlantHeightMassForMax;

    /// <summary>Exponent controlling shoot height as function of DM weight</summary>
    internal double myPlantHeightExponent;

    ////- Root depth and distribution >>> - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    /// <summary>Minimum rooting depth, at emergence</summary>
    internal double myRootDepthMinimum;

    /// <summary>Maximum rooting depth</summary>
    internal double myRootDepthMaximum;

    /// <summary>Daily root elongation rate at optimum temperature</summary>
    internal double myRootElongationRate;

    /// <summary>Depth from surface where root proportion starts to decrease</summary>
    internal double myRootDistributionDepthParam = 90.0;

    /// <summary>Exponent controlling the root distribution as function of depth</summary>
    internal double myRootDistributionExponent = 3.2;

    ////- Harvest limits and preferences >>>  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    /// <summary>Fraction of soluble carbohydrates in newly grown tissues (0-1)</summary>
    internal double mySugarFractionNewGrowth = 0.5;

    /// <summary>Relative preference for live over dead material during graze</summary>
    internal double myPreferenceForGreenOverDead;

    /// <summary>Relative preference for leaf over stem-stolon material during graze</summary>
    internal double myPreferenceForLeafOverStems;

    ////- Soil related (water and N uptake) >>> - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    /// <summary>Maximum NH4 uptake rate for each species</summary>
    internal double myMaximumUptakeRateNH4 = 1.0;

    /// <summary>Maximum NO3 uptake rate for each species</summary>
    internal double myMaximumUptakeRateNO3 = 1.0;

    /// <summary>Reference root length density for water and N uptake</summary>
    internal double myReferenceRLD = 0.2;

    /// <summary>Exponent of water content factor for water and N uptake</summary>
    internal double myExponentSWCUptake = 0.25;

    /// <summary>Root exploration factor, for each layer</summary>
    internal double[] xf;

    ////- Parameters for annual species >>> - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    /// <summary>Earliest day of emergence (for annuals only)</summary>
    internal double dayGermn;

    /// <summary>Earliest month of emergence (for annuals only)</summary>
    internal double daysToGermn;

    /// <summary>Earliest day of anthesis (for annuals only)</summary>
    internal double daysEmgToAnth;

    /// <summary>Earliest month of anthesis (for annuals only)</summary>
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

    ////- Annuals and phenology >>> - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    /// <summary>Some Description</summary>
    internal int phenoStage = 0; //pheno stages: 0 - pre_emergence, 1 - vegetative, 2 - reproductive

    /// <summary>Some Description</summary>
    internal double daysfromEmergence = 0;

    /// <summary>Factor describing progress through phenological phases (0-1).</summary>
    private double phenoFactor;

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
    internal double gamaD = 0.0; // from dead to litter

    /// <summary>Some Description</summary>
    internal double gamaR = 0.0; // for roots (to dead/FOM)

    /// <summary>Some Description</summary>
    internal double gamaS = 0.0; // for stolons

    /// <summary>Some Description</summary>
    internal double tempFactorRespiration;

    /// <summary>Some Description</summary>
    internal double tempFacTTurnover;

    /// <summary>Some Description</summary>
    internal double swFacTTurnover;

    /// <summary>Effect of defoliation on stolon turnover (0-1).</summary>
    private double cumDefoliationFactor;

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
    internal double NSenesced2NewGrowth = 0.0;

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
    internal double glfWLogging = 1.0;

    /// <summary>Cumulative water logging factor</summary>
    internal double cumWaterLogging;

    /// <summary>Some Description</summary>
    internal double glfN = 1.0; //from N deficit

    ////- Harvest and digestibility >>> - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    /// <summary>Some Description</summary>
    internal double dmdefoliated;

    /// <summary>Some Description</summary>
    internal double Ndefoliated;

    /// <summary>Some Description</summary>
    internal double fractionDefoliated;

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
                Result[layer] = (roots.DMGreen * 0.1) * rootFraction[layer] * mySpecificRootLength; // m/m2
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
        layerBottomRootZone = GetRootZoneBottomLayer();
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
        rootDepth = myRootDepthMinimum;
        layerBottomRootZone = GetRootZoneBottomLayer();
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
        doyIniPlateau += 0.5 * yearLength / (1 + Math.Exp(-myReproSeasonTimingCoeff * (Math.Abs(MetFile.Latitude) - myReproSeasonReferenceLatitude)));

        // compute the duration of the main phase (with max allocation to shoot)
        reproSeasonInterval[1] = (yearLength / 24.0);
        reproSeasonInterval[1] += (yearLength * 11.0 / 24.0) * Math.Pow(1 - (Math.Abs(MetFile.Latitude) / 90.0), myReproSeasonDurationCoeff);

        // compute the duration of the onset and outset phases (shoulders)
        reproSeasonInterval[0] = reproSeasonInterval[1] * myReproSeasonShouldersLengthFactor * myReproSeasonOnsetDurationFactor;
        reproSeasonInterval[2] = reproSeasonInterval[1] * myReproSeasonShouldersLengthFactor * (1.0 - myReproSeasonOnsetDurationFactor);

        if (reproSeasonInterval.Sum() > yearLength)
            throw new Exception("Error on calculating period with high DM allocation, it is greater then one year");

        // get the day for the start of reproductive season
        doyIniReproSeason = doyIniPlateau - reproSeasonInterval[0];
        if (doyIniReproSeason < 0.0) doyIniReproSeason += yearLength;

        // compute the factor to augment allocation to shoot at main phase
        allocationIncreaseRepro = myReproSeasonMaxAllocationIncrease;
        allocationIncreaseRepro /= (1 + Math.Exp(-myReproSeasonAllocationCoeff * (Math.Abs(MetFile.Latitude) - myReproSeasonReferenceLatitude)));
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
        germinationGDD += Math.Max(0.0, Tmean - myGrowthTminimum);
        result = germinationGDD / myDegreesDayForGermination;
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
        if (isLegume && usingNFixationCost)
            costNFixation = DailyNFixationCosts();

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
        dGrowthW = dGrowthPot * Math.Min(glfWater, glfWLogging);

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
            gfnit = Math.Pow(glfN, myNDillutionCoefficient);
        // more DM growth than N limited, due to dilution (typically NdilutCoeff = 0.5)

        dGrowth = dGrowthW * Math.Min(gfnit, myGlfSoilFertility);
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
            double Pm1 = myReferencePhotosyntheticRate * glfTmean * CO2Factor * NcFactor;

            //Potential photosynthetic rate at bright light (half of sunlight length, middle of day)
            double Pm2 = myReferencePhotosyntheticRate * glfTemp * CO2Factor * NcFactor;

            //Sunlight length, converted to seconds
            double tau = 3600 * MetFile.day_length;

            //Photosynthetic active irradiance - converted from MJ/m2.day to J/m2.s
            double incidentPAR = MetFile.Radn * myFractionPAR * 1000000 / tau;

            //Irradiance at top of canopy in the middle of the day (J/m2 leaf/s)
            IrradianceTopOfCanopy = myLightExtinctionCoefficient * incidentPAR * (4.0 / 3.0);

            //Photosynthesis per leaf area under full irradiance at the top of the canopy (mg CO2/m^2 leaf/s)
            double Pl1 = SingleLeafPhotosynthesis(0.5 * IrradianceTopOfCanopy, Pm1); // early and late parts of the day
            double Pl2 = SingleLeafPhotosynthesis(IrradianceTopOfCanopy, Pm2); // middle part of the day

            // Radiation effects (for reporting purposes only)
            RadnFactor = MathUtility.Divide((0.25 * Pl1) + (0.75 * Pl2), (0.25 * Pm1) + (0.75 * Pm2), 1.0);

            // Fraction of total radiation available to this plant
            canopyCompetitionFactor = MathUtility.Divide(interceptedRadn * myLightPartitioningFactor, MetFile.Radn * coverGreen, 1.0);

            //Canopy photosynthesis - Upscaling from 'per leaf' to 'per ground' area
            double carbon_m2 = 0.5 * (Pl1 + Pl2); // mgCO2/m2 leaf/s
            carbon_m2 *= coverGreen * canopyCompetitionFactor; // mgCO2/m2 leaf/s - canopy
            carbon_m2 /= myLightExtinctionCoefficient; // mgCO2/m2.s - ground area
            carbon_m2 *= 0.000001; // kgCO2/m2.s
            carbon_m2 *= tau; // kgCO2/m2.day
            carbon_m2 *= 12.0 / 44.0; // kgC/m2.day
            PotPhoto = 10000 * carbon_m2; // kgC/ha.day

            //Add extreme temperature effects;
            ExtremeTempStress = HeatEffect() * ColdEffect(); // in practice only one temp stress factor is < 1
            Pgross = PotPhoto * ExtremeTempStress;

            // Consider a generic growth limiting factor
            Pgross *= myGlfGeneric;
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
        double photoAux1 = myPhotosyntheticEfficiency * IL + Pmax;
        double photoAux2 = 4 * myPhotosynthesisCurveFactor * myPhotosyntheticEfficiency * IL * Pmax;
        double Pl = (0.5 / myPhotosynthesisCurveFactor) * (photoAux1 - Math.Sqrt(Math.Pow(photoAux1, 2) - photoAux2));
        return Pl;
    }

    /// <summary>Compute plant respiration, growth and maintenance</summary>
    /// <returns>Amount of C lost by respiration</returns>
    internal void DailyPlantRespiration()
    {
        double LiveBiomassC = (AboveGroundLiveWt + roots.DMGreen) * CarbonFractionDM; //converting DM to C    (kgC/ha)

        // maintenance respiration
        tempFactorRespiration = TemperatureEffectOnRespiration();
        if (LiveBiomassC > Epsilon)
            Resp_m = myMaintenanceRespirationCoefficient * tempFactorRespiration * NcFactor * LiveBiomassC;
        else
            Resp_m = 0.0;

        // growth respiration
        Resp_g = Pgross * (1 - myGrowthRespirationCoefficient);
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

        double Pm_mean = myReferencePhotosyntheticRate * glfT * CO2Factor * NcFactor;

        glfT = GFTemperature(Tday);
        double Pm_day = myReferencePhotosyntheticRate * glfT * CO2Factor * NcFactor;

        double tau = 3600 * MetFile.day_length; //conversion of hour to seconds
        double IL = swardLightExtCoeff * 1.33333 * 0.5 * swardInterceptedRadn * 1000000 / tau;
        double IL2 = IL / 2; //IL for early & late period of a day
        IrradianceTopOfCanopy = IL;

        // Photosynthesis per LAI under full irradiance at the top of the canopy
        double photoAux1 = myPhotosyntheticEfficiency * IL + Pm_day;
        double photoAux2 = 4 * myPhotosynthesisCurveFactor * myPhotosyntheticEfficiency * IL * Pm_day;
        double Pl1 = (0.5 / myPhotosynthesisCurveFactor) * (photoAux1 - Math.Sqrt(Math.Pow(photoAux1, 2) - photoAux2));

        photoAux1 = myPhotosyntheticEfficiency * IL2 + Pm_mean;
        photoAux2 = 4 * myPhotosynthesisCurveFactor * myPhotosyntheticEfficiency * IL2 * Pm_mean;
        double Pl2 = (0.5 / myPhotosynthesisCurveFactor) * (photoAux1 - Math.Sqrt(Math.Pow(photoAux1, 2) - photoAux2));

        // Upscaling from 'per LAI' to 'per ground area'
        double carbon_m2 = 0.5 * (Pl1 + Pl2) * swardCoverGreen * intRadnFrac * myLightPartitioningFactor / myLightExtinctionCoefficient;
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
        Pgross *= myGlfGeneric;

        // Radiation effects (for reporting purposes only)
        RadnFactor = MathUtility.Divide((0.25 * Pl2) + (0.75 * Pl1), (0.25 * Pm_mean) + (0.75 * Pm_day), 1.0);

        // Respiration, maintenance and growth
        tempFactorRespiration = TemperatureEffectOnRespiration();
        double LiveDM = (AboveGroundLiveWt + roots.DMGreen) * CarbonFractionDM; //converting DM to C    (kgC/ha)
        Resp_m = myMaintenanceRespirationCoefficient * tempFactorRespiration * NcFactor * LiveDM;
        Resp_g = Pgross * (1 - myGrowthRespirationCoefficient);

        // N fixation costs
        costNFixation = 0.0;
        if (isLegume && usingNFixationCost)
            costNFixation = DailyNFixationCosts();

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
        double swFacTTDead = Math.Pow(Math.Min(glfWater, glfWLogging), myDetachmentDroughtCoefficient);
        swFacTTDead = myDetachmentDroughtEffectMin + Math.Max(0.0, 1.0 - myDetachmentDroughtEffectMin) * swFacTTDead;

        // Consider the number of leaves
        double leafFac = 3.0 / myLiveLeavesPerTiller; // three refers to the number of stages used in the model

        // Leaf and stems turnover rate
        gama = myTissueTurnoverRateShoot * tempFacTTurnover * swFacTTurnover * leafFac;

        // Factor to increase stolon/root senescence if there was a defoliation
        double defoliationFactor = DefoliationEffectOnTissueTurnover();

        // Stolons turnover rate (legumes)
        if (isLegume)
            gamaS = gama + defoliationFactor * (1.0 - gama);
        else
            gamaS = 0.0;

        // Roots turnover rate
        gamaR = tempFacTTurnover * (2.0 - Math.Min(glfWater, glfWLogging)) * myTissueTurnoverRateRoot;

        // Adjust root turnover due to defoliation
        gamaR += myTurnoverDefoliationRootEffect * defoliationFactor * (1.0 - gamaR);

        // Littering rate
        double digestDead = (leaves.DigestibilityDead * leaves.DMDead) + (stems.DigestibilityDead * stems.DMDead);
        digestDead = MathUtility.Divide(digestDead, leaves.DMDead + stems.DMDead, 0.0);
        gamaD = myDetachmentRateShoot * swFacTTDead * digestDead / 0.4;

        // Adjust littering rate due to stock trampling
        gamaD += myTurnoverStockFactor * stockingRate;
        if (gamaD <= 0.03)
            gamaD += 0.0;

        // check turnover rates
        if ((gama > 1.0) || (gamaS > 1.0) || (gamaD > 1.0) || (gamaR > 1.0))
            throw new Exception(" AgPasture Computed tissue turnover rate greater than one");

        // Adjust turnover rate for annuals
        if (isAnnual)
        {
            if (phenoStage == 1)
            {
                //vegetative, turnover is zero at emergence and increases with age
                gama *= Math.Pow(phenoFactor, 0.5);
                gamaS *= Math.Pow(phenoFactor, 0.5);
                gamaR *= Math.Pow(phenoFactor, 2.0);
                gamaD *= Math.Pow(phenoFactor, 2.0);
            }
            else if (phenoStage == 2)
            {
                //reproductive, turnover increases with age and reach one at maturity
                gama += (1.0 - gama) * Math.Pow(phenoFactor, 2.0);
                gamaS += (1.0 - gamaS) * Math.Pow(phenoFactor, 2.0);
                gamaR += (1.0 - gamaR) * Math.Pow(phenoFactor, 3.0);
                gamaD += (1.0 - gamaD) * Math.Pow(phenoFactor, 3.0);
            }
        }

        // If today's turnover will result in a dmgreenShoot < dmgreen_minimum, then adjust the rates
        if (gama > Epsilon)
        {
            double dmMature = leaves.tissue[2].DM + stems.tissue[2].DM + stolons.tissue[2].DM;
            double dmgreenToBe = AboveGroundLiveWt - (gama * (dmMature));
            double minimumDMgreen = leaves.MinimumGreenDM + stems.MinimumGreenDM + stolons.MinimumGreenDM;
            if (dmgreenToBe < minimumDMgreen)
            {
                double gamaIni = gama;
                if (AboveGroundLiveWt <= minimumDMgreen)
                    gama = 0.0;
                else
                    gama = MathUtility.Divide(AboveGroundLiveWt - minimumDMgreen, dmMature, 0.0);

                // reduce stolon and root turnover too (half of the reduction in leaf/stem)
                double dmFactor = 0.5 * (gamaIni + gama) / gamaIni;
                gamaS *= dmFactor;
                gamaR *= dmFactor;
            }
        }

        // consider a minimum for roots too
        double dmRootToBe = roots.DMGreen - roots.tissue[0].DM * gamaR;
        if (dmRootToBe < roots.MinimumGreenDM)
        {
            if (roots.DMGreen < roots.MinimumGreenDM)
                gamaR = 0.0;
            else
                gamaR = MathUtility.Divide(roots.DMGreen - roots.MinimumGreenDM, roots.tissue[0].DM, 0.0);
        }

        // Get the turnover amounts (DM and N) for each organ  - DM into tissue[0] was considered in PartitionDMGrown()
        if (gama > Epsilon)
        {
            // Leaves
            leaves.tissue[0].DMTransferedOut = myRelativeTurnoverEmerging * gama * leaves.tissue[0].DM;
            leaves.tissue[1].DMTransferedIn = leaves.tissue[0].DMTransferedOut;
            leaves.tissue[1].DMTransferedOut = gama * leaves.tissue[1].DM;
            leaves.tissue[2].DMTransferedIn = leaves.tissue[1].DMTransferedOut;
            leaves.tissue[2].DMTransferedOut = gama * leaves.tissue[2].DM;
            leaves.tissue[3].DMTransferedIn = leaves.tissue[2].DMTransferedOut;

            leaves.tissue[0].NTransferedOut = leaves.tissue[0].Nconc * leaves.tissue[0].DMTransferedOut;
            leaves.tissue[1].NTransferedIn = leaves.tissue[0].NTransferedOut;
            leaves.tissue[1].NTransferedOut = leaves.tissue[1].Nconc * leaves.tissue[1].DMTransferedOut;
            leaves.tissue[2].NTransferedIn = leaves.tissue[1].NTransferedOut;
            leaves.tissue[2].NTransferedOut = leaves.tissue[2].Nconc * leaves.tissue[2].DMTransferedOut;
            leaves.tissue[3].NTransferedIn = leaves.tissue[2].NTransferedOut;

            // Stems
            stems.tissue[0].DMTransferedOut = myRelativeTurnoverEmerging * gama * stems.tissue[0].DM;
            stems.tissue[1].DMTransferedIn = stems.tissue[0].DMTransferedOut;
            stems.tissue[1].DMTransferedOut = gama * stems.tissue[1].DM;
            stems.tissue[2].DMTransferedIn = stems.tissue[1].DMTransferedOut;
            stems.tissue[2].DMTransferedOut = gama * stems.tissue[2].DM;
            stems.tissue[3].DMTransferedIn = stems.tissue[2].DMTransferedOut;

            stems.tissue[0].NTransferedOut = stems.tissue[0].Nconc * stems.tissue[0].DMTransferedOut;
            stems.tissue[1].NTransferedIn = stems.tissue[0].NTransferedOut;
            stems.tissue[1].NTransferedOut = stems.tissue[1].Nconc * stems.tissue[1].DMTransferedOut;
            stems.tissue[2].NTransferedIn = stems.tissue[1].NTransferedOut;
            stems.tissue[2].NTransferedOut = stems.tissue[2].Nconc * stems.tissue[2].DMTransferedOut;
            stems.tissue[3].NTransferedIn = stems.tissue[2].NTransferedOut;
        }

        // Get the littering amounts (DM and N) for each organ
        if (gamaD > Epsilon)
        {
            // Leaves
            leaves.tissue[3].DMTransferedOut = gamaD * leaves.tissue[3].DM;
            dDMLitter = leaves.tissue[3].DMTransferedOut;
            leaves.tissue[3].NTransferedOut = leaves.tissue[3].Nconc * leaves.tissue[3].DMTransferedOut;
            dNLitter = leaves.tissue[3].NTransferedOut;
            leaves.tissue[3].NRemobilisable = (leaves.tissue[2].Nconc - leaves.NConcMinimum) * leaves.tissue[2].DMTransferedOut;

            // Stems
            stems.tissue[3].DMTransferedOut = gamaD * stems.tissue[3].DM;
            dDMLitter += stems.tissue[3].DMTransferedOut;
            stems.tissue[3].NTransferedOut = stems.tissue[3].Nconc * stems.tissue[3].DMTransferedOut;
            dNLitter += stems.tissue[3].NTransferedOut;
            stems.tissue[3].NRemobilisable = (stems.tissue[2].Nconc - stems.NConcMinimum) * stems.tissue[2].DMTransferedOut;
        }
        else
        {
            dDMLitter = 0.0;
            dNLitter = 0.0;
        }

        // Stolons
        if (isLegume && gamaS > Epsilon)
        {
            stolons.tissue[0].DMTransferedOut = myRelativeTurnoverEmerging * gamaS * stolons.tissue[0].DM;
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
        if (gamaR > Epsilon)
        {
            roots.tissue[0].DMTransferedOut = gamaR * roots.tissue[0].DM;
            roots.tissue[1].DMTransferedIn = roots.tissue[0].DMTransferedOut;
            roots.tissue[1].DMTransferedOut = roots.tissue[1].DM;
            dDMRootSen = roots.tissue[1].DM;

            roots.tissue[0].NTransferedOut = roots.tissue[0].Nconc * roots.tissue[0].DMTransferedOut;
            roots.tissue[1].NTransferedIn = roots.tissue[0].NTransferedOut;
            roots.tissue[1].NTransferedOut = roots.tissue[1].Namount;
            dNRootSen = roots.tissue[1].NTransferedOut;
            roots.tissue[1].NRemobilisable = (roots.tissue[0].Nconc - roots.NConcMinimum) * roots.tissue[0].DMTransferedOut;
        }
        else
        {
            dDMRootSen = 0.0;
            dNRootSen = 0.0;
        }
        
        // Evaluate remobilisable luxury N
        leaves.tissue[0].NRemobilisable = Math.Max(0.0, leaves.tissue[0].Nconc - leaves.NConcOptimum)
                                          * leaves.tissue[0].DM * FractionNLuxuryRemobilisable[0];
        leaves.tissue[1].NRemobilisable = Math.Max(0.0, leaves.tissue[1].Nconc - leaves.NConcOptimum * NcRel2)
                                          * leaves.tissue[1].DM * FractionNLuxuryRemobilisable[1];
        leaves.tissue[2].NRemobilisable = Math.Max(0.0, leaves.tissue[2].Nconc - leaves.NConcOptimum * NcRel3)
                                          * leaves.tissue[2].DM * FractionNLuxuryRemobilisable[2];
        stems.tissue[0].NRemobilisable = Math.Max(0.0, stems.tissue[0].Nconc - stems.NConcOptimum)
                                         * stems.tissue[0].DM * FractionNLuxuryRemobilisable[0];
        stems.tissue[1].NRemobilisable = Math.Max(0.0, stems.tissue[1].Nconc - stems.NConcOptimum * NcRel2)
                                         * stems.tissue[1].DM * FractionNLuxuryRemobilisable[1];
        stems.tissue[2].NRemobilisable = Math.Max(0.0, stems.tissue[2].Nconc - stems.NConcOptimum * NcRel3)
                                         * stems.tissue[2].DM * FractionNLuxuryRemobilisable[2];
        stolons.tissue[0].NRemobilisable = Math.Max(0.0, stolons.tissue[0].Nconc - stolons.NConcOptimum)
                                           * stolons.tissue[0].DM * FractionNLuxuryRemobilisable[0];
        stolons.tissue[1].NRemobilisable = Math.Max(0.0, stolons.tissue[1].Nconc - stolons.NConcOptimum * NcRel2)
                                           * stolons.tissue[1].DM * FractionNLuxuryRemobilisable[1];
        stolons.tissue[2].NRemobilisable = Math.Max(0.0, stolons.tissue[2].Nconc - stolons.NConcOptimum * NcRel3)
                                           * stolons.tissue[2].DM * FractionNLuxuryRemobilisable[2];
        roots.tissue[0].NRemobilisable = Math.Max(0.0, roots.tissue[0].Nconc - roots.NConcOptimum)
                                         * roots.tissue[0].DM * FractionNLuxuryRemobilisable[0];

        //Sugar remobilisation and C balance:
        Cremob = 0.0; // not explicitly considered
    }

    /// <summary>Partition DM from new growth</summary>
    internal void EvaluateAllocationNewGrowth()
    {
        if (double.IsNaN(dGrowth))
            System.Diagnostics.Debugger.Break();

        if (dGrowth > Epsilon) // if no net growth, then skip "partition" part
        {
            // fShoot and fLeaf were calculated on CalcNdemand()

            // Fractions of new growth to be allocated to the 1st tissue pools
            double toRoot = 1.0 - ShootAllocationFactor;
            double toStolon = ShootAllocationFactor * myFractionToStolon;
            double toLeaf = ShootAllocationFactor * LeafAllocationFactor;
            double toStem = ShootAllocationFactor * (1.0 - myFractionToStolon - LeafAllocationFactor);

            // checking
            double ToAll = toLeaf + toStem + toStolon + toRoot;
            if (Math.Abs(ToAll - 1.0) > Epsilon)
                throw new Exception("Mass balance lost during partition of new growth DM");

            // Allocate the partitioned growth to the 1st tissue pools
            dGrowthShoot = ShootAllocationFactor * dGrowth;
            dGrowthRoot = toRoot * dGrowth;
            leaves.tissue[0].DMTransferedIn = toLeaf * dGrowth;
            stems.tissue[0].DMTransferedIn = toStem * dGrowth;
            stolons.tissue[0].DMTransferedIn = toStolon * dGrowth;
            roots.tissue[0].DMTransferedIn = dGrowthRoot;

            // Set the amount of sugar in each organ
            leaves.SugarWt = mySugarFractionNewGrowth * toLeaf * dGrowth;
            stems.SugarWt = mySugarFractionNewGrowth * toStem * dGrowth;
            stolons.SugarWt = mySugarFractionNewGrowth * toStolon * dGrowth;

            // Partition N, based on DM and [N] in each plant part
            double myNsum = (toLeaf * leaves.NConcMaximum)
                            + (toStem * stems.NConcMaximum)
                            + (toStolon * stolons.NConcMaximum)
                            + (toRoot * roots.NConcMaximum);
            if (myNsum > Epsilon)
            {
                double toLeafN = toLeaf * leaves.NConcMaximum / myNsum;
                double toStemN = toStem * stems.NConcMaximum / myNsum;
                double toStolN = toStolon * stolons.NConcMaximum / myNsum;
                double toRootN = toRoot * roots.NConcMaximum / myNsum;

                // checking
                ToAll = toLeafN + toStemN + toStolN + toRootN;
                if (Math.Abs(ToAll - 1.0) > Epsilon)
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
            throw new Exception("  " + speciesName + " - Growth and tissue turnover resulted in loss of mass balance for DM");

        if (Math.Abs(preTotalN + newGrowthN - dNLitter - dNRootSen - NSenesced2NewGrowth - NLuxury2NewGrowth - (AboveGroundN + roots.NTotal)) > Epsilon)
            throw new Exception("  " + speciesName + " - Growth and tissue turnover resulted in loss of mass balance for N");

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
            if (daysfromEmergence < daysEmgToAnth)
            {
                //vegetative stage
                phenoFactor = MathUtility.Divide(daysfromEmergence, daysEmgToAnth, 1.0);
            }
            else if (daysfromEmergence >= daysEmgToAnth)
            {
                // reproductive stage
                phenoStage = 2;
                phenoFactor = MathUtility.Divide(daysfromEmergence - daysEmgToAnth, daysAnthToMatur, 1.0);
            }
            else if (daysfromEmergence >= (daysEmgToAnth + daysAnthToMatur))
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
        double toStol = dGrowthW * ShootAllocationFactor * myFractionToStolon;
        double toLeaf = dGrowthW * ShootAllocationFactor * LeafAllocationFactor;
        double toStem = dGrowthW * ShootAllocationFactor * (1.0 - myFractionToStolon - LeafAllocationFactor);

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
        if (NdemandOpt > Epsilon && (NdemandOpt > SoilNavailable + myMinimumNFixation))
            Nstress = SoilNavailable / (NdemandOpt - myMinimumNFixation);

        if (Nstress <= 0.999)
        {
            // more fixation under N stress
            double moreNfixation = (myMaximumNFixation - myMinimumNFixation) * (1 - Nstress);
            moreNfixation = Math.Max(0.0, Math.Min(1.0, moreNfixation));
            NFixed = (myMinimumNFixation + moreNfixation) * NdemandOpt;
        }
        else
        {
            // minimum fixation even if not needed
            NFixed = myMinimumNFixation * NdemandOpt;
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
    /// <returns>The amount of carbon spent on N fixation (kg/ha)</returns>
    private double DailyNFixationCosts()
    {
        //  respiration cost of symbiont (presence of rhizobia is assumed to be proportional to root mass)
        double Tfactor = GFTemperature(Tmean);
        double maintenanceCost = roots.DMGreen * CarbonFractionDM * mySymbiontCostFactor * Tfactor;

        //  respiration cost of N fixation (assumed as a simple linear function of N fixed)
        double activityCost = NFixed * myNFixingCostFactor;

        return maintenanceCost + activityCost;
    }

    /// <summary>Computes the amount of N remobilised from senescent material used in new growth</summary>
    internal void CalcNRemobSenescent()
    {
        if (NdemandLux <= NSenescedRemobilisable + NFixed)
        {
            // Nremob and/or Nfix are able to supply all N
            NSenesced2NewGrowth = Math.Max(0.0, NdemandLux - NFixed);
        }
        else
        {
            // not enough N within the plant, uptake is needed
            NSenesced2NewGrowth = NSenescedRemobilisable;
        }

        // pass the remobilised amount to each organ
        double fracRemob = MathUtility.Divide(NSenesced2NewGrowth, NSenescedRemobilisable, 0.0);
        leaves.tissue[3].DoRemobiliseN(fracRemob);
        stems.tissue[3].DoRemobiliseN(fracRemob);
        stolons.tissue[3].DoRemobiliseN(fracRemob);
        roots.tissue[1].DoRemobiliseN(fracRemob);

        newGrowthN = NSenesced2NewGrowth + NFixed;
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

        if (soilNuptake > Epsilon)
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
                // N luxury is enough, get what is needed, first from tissue 3 and proceed downwards
                NLuxury2NewGrowth = 0.0;
                double fracRemob = 1.0;
                double NLuxTissue = 0.0;
                for (int t = 2; t >= 0; t--)
                {
                    NLuxTissue = leaves.tissue[t].NRemobilisable + stems.tissue[t].NRemobilisable + stolons.tissue[t].NRemobilisable;
                    if (t == 0) NLuxTissue += roots.tissue[t].NRemobilisable;
                    if (NLuxTissue > Epsilon)
                    {
                        double Nusedup = Math.Min(NLuxTissue, remainingNdemand);
                        fracRemob = MathUtility.Divide(Nusedup, NLuxTissue, 0.0);

                        leaves.tissue[t].DoRemobiliseN(fracRemob);
                        stems.tissue[t].DoRemobiliseN(fracRemob);
                        stolons.tissue[t].DoRemobiliseN(fracRemob);
                        if (t == 0) roots.tissue[t].DoRemobiliseN(fracRemob);

                        NLuxury2NewGrowth += Nusedup;
                        remainingNdemand -= Nusedup;
                        if (remainingNdemand <= Epsilon) t = 0;
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
            double glfFactor = 1.0 - myShootRootGlfFactor * (1.0 - Math.Pow(glfMin, 1.0 / myShootRootGlfFactor));

            // get the current shoot/root ratio (partition will try to make this value closer to targetSR)
            double currentSR = MathUtility.Divide(AboveGroundLiveWt, roots.DMGreen, 1000000.0);

            // get the factor for the reproductive season of perennials (increases shoot allocation during spring)
            double reproFac = 1.0;
            if (UsingReproSeasonFactor && !isAnnual)
                reproFac = CalcReproductiveGrowthFactor();

            // get today's target SR
            double targetSR = myTargetShootRootRatio * reproFac;

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
        double targetFLeaf = myFractionLeafMaximum;
        if ((myFractionLeafMinimum < myFractionLeafMaximum) && (AboveGroundLiveWt > myFractionLeafDMThreshold))
        {
            // compute new target fLeaf
            double fLeafAux = (AboveGroundLiveWt - myFractionLeafDMThreshold) / (myFractionLeafDMFactor - myFractionLeafDMThreshold);
            fLeafAux = Math.Pow(fLeafAux, myFractionLeafExponent);
            targetFLeaf = myFractionLeafMinimum + (myFractionLeafMaximum - myFractionLeafMinimum) / (1 + fLeafAux);
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
            if ((dGrowthRoot > Epsilon) && (rootDepth < myRootDepthMaximum))
            {
                double tempFactor = GFTemperature(Tmean);
                dRootDepth = myRootElongationRate * tempFactor;
                rootDepth = Math.Min(myRootDepthMaximum, Math.Max(myRootDepthMinimum, rootDepth + dRootDepth));
                layerBottomRootZone = GetRootZoneBottomLayer();
            }
            else
            {
                // no root growth, depth does not change
                dRootDepth = 0.0;
            }

            // do root distribution
            if (dRootDepth > Epsilon)
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
            layerBottomRootZone = -1;
        }
    }

    /// <summary>Calculates the plant height, as function of DM</summary>
    /// <returns>Plant height</returns>
    internal double HeightfromDM()
    {
        double TodaysHeight = myPlantHeightMaximum - myPlantHeightMinimum;
        double standingDM = (leaves.DMTotal + stems.DMTotal);

        if (standingDM <= myPlantHeightMassForMax)
        {
            double massRatio = standingDM / myPlantHeightMassForMax;
            double heightF = myPlantHeightExponent - (myPlantHeightExponent * massRatio) + massRatio;
            heightF *= Math.Pow(massRatio, myPlantHeightExponent - 1);
            TodaysHeight *= heightF;
        }

        return TodaysHeight + myPlantHeightMinimum;
    }

    /// <summary>Calculates the LAI values for green and dead material</summary>
    internal void EvaluateLAI()
    {
        // Get the amount of green tissue of leaves (converted from kg/ha to kg/m2)
        double greenTissue = leaves.DMGreen / 10000;

        // Get a proportion of green tissue from stolons
        greenTissue += stolons.DMGreen * myStolonEffectOnLAI / 10000;

        // Consider some green tissue from stems (if DM is low)
        if (AboveGroundLiveWt < myShootMaxEffectOnLAI)
        {
            double shootFactor = myMaxStemEffectOnLAI * Math.Sqrt(1.0 - (AboveGroundLiveWt / myShootMaxEffectOnLAI));
            greenTissue += stems.DMGreen * shootFactor / 10000;
        }
        // Resilience after unfavoured conditions
        // Consider cover will be bigger for the same amount of DM when DM is low due to
        // - light extinction coefficient will be bigger - plant leaves will be more horizontal than in dense high swards
        // - more parts will turn green for photosynthesis (?)
        // - quick response of plant shoots to favoured conditions after release of stress
        // » maybe specific leaf area should be reduced (RCichota2014)

        greenLAI = greenTissue * mySpecificLeafArea;

        deadLAI = 0.0001 * leaves.tissue[3].DM * mySpecificLeafArea;
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
    internal double RemoveDMOld(double amountToRemove)
    {
        // save current state
        double preRemovalDMShoot = AboveGroundWt;
        double preRemovalNShoot = AboveGroundN;

        // get the weights for each pool, consider preference and available DM
        double tempPrefGreen = myPreferenceForGreenOverDead + (amountToRemove / StandingWt);
        double tempPrefDead = 1.0 + (myPreferenceForGreenOverDead * amountToRemove / StandingWt);
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
        if (Math.Abs(dmdefoliated - amountToRemove) > Epsilon)
            throw new Exception("  AgPasture - removal of DM resulted in loss of mass balance");

        return dmdefoliated;
    }

    /// <summary>Removes a given amount of DM (and N) from this plant.</summary>
    /// <param name="amountToRemove">The DM amount to remove (kg/ha)</param>
    /// <returns>The DM amount actually removed (kg/ha)</returns>
    internal double RemoveDM(double amountToRemove)
    {
        // get existing DM and N amounts
        double preRemovalDMShoot = AboveGroundWt;
        double preRemovalNShoot = AboveGroundN;

        if (amountToRemove > Epsilon)
        {
            // Compute the fraction of each tissue to be removed
            double[] fracRemoving = new double[5];
            if (amountToRemove - HarvestableWt > -Epsilon)
            {
                // All existing DM is removed
                for (int i = 0; i < 5; i++)
                    fracRemoving[i] = 1.0;
            }
            else
            {
                // Initialise the fractions to be removed (these will be normalised later)
                fracRemoving[0] = leaves.DMGreenHarvestable * myPreferenceForGreenOverDead * myPreferenceForLeafOverStems;
                fracRemoving[1] = stems.DMGreenHarvestable * myPreferenceForGreenOverDead;
                fracRemoving[2] = stolons.DMGreenHarvestable * myPreferenceForGreenOverDead;
                fracRemoving[3] = leaves.DMDeadHarvestable * myPreferenceForLeafOverStems;
                fracRemoving[4] = stems.DMDeadHarvestable;

                // Get fraction potentially removable (maximum fraction of each tissue in the removing amount)
                double[] fracRemovable = new double[5];
                fracRemovable[0] = leaves.DMGreenHarvestable / amountToRemove;
                fracRemovable[1] = stems.DMGreenHarvestable / amountToRemove;
                fracRemovable[2] = stolons.DMGreenHarvestable / amountToRemove;
                fracRemovable[3] = leaves.DMDeadHarvestable / amountToRemove;
                fracRemovable[4] = stems.DMDeadHarvestable / amountToRemove;

                // Normalise the fractions of each tissue to be removed, they should add to one
                double totalFrac = fracRemoving.Sum();
                for (int i = 0; i < 5; i++)
                    fracRemoving[i] = Math.Min(fracRemovable[i], fracRemoving[i] / totalFrac);

                // Iterate until sum of fractions to remove is equal to one
                //  The initial normalised fractions are based on preference and existing DM. Because the value of fracRemoving is limited
                //   to fracRemovable, the sum of fracRemoving may not be equal to one, as it should be. We need to iterate adjusting the
                //   values of fracRemoving until we get a sum close enough to one. The previous values are used as weighting factors for
                //   computing new ones at each iteration.
                int count = 1;
                totalFrac = fracRemoving.Sum();
                while (1.0 - totalFrac > Epsilon)
                {
                    count += 1;
                    for (int i = 0; i < 5; i++)
                        fracRemoving[i] = Math.Min(fracRemovable[i], fracRemoving[i] / totalFrac);
                    totalFrac = fracRemoving.Sum();
                    if (count > 1000)
                    {
                        Console.WriteLine(" AgPasture could not remove on graze all the DM required for " + speciesName);
                        break;
                    }
                }
                //mySummary.WriteMessage(this, " AgPasture " + Name + " needed " + count + " iterations to solve partition of removed DM");
            }

            // Get digestibility of DM being harvested (do this before updating pools)
            double greenDigestibility = (leaves.DigestibilityLive * fracRemoving[0]) + (stems.DigestibilityLive * fracRemoving[1])
                                        + (stolons.DigestibilityLive * fracRemoving[2]);
            double deadDigestibility = (leaves.DigestibilityDead * fracRemoving[3]) + (stems.DigestibilityDead * fracRemoving[4]);
            digestDefoliated = greenDigestibility + deadDigestibility;

            // Update the various tissues (DM, N and N remobilisable)
            int t;
            // Leaves
            double fracRemaining = Math.Max(0.0, 1.0 - MathUtility.Divide(amountToRemove * fracRemoving[0], leaves.DMGreen, 0.0));
            for (t = 0; t < 3; t++)
            {
                leaves.tissue[t].DM *= fracRemaining;
                leaves.tissue[t].Namount *= fracRemaining;
                leaves.tissue[t].NRemobilisable *= fracRemaining;
            }
            fracRemaining = Math.Max(0.0, 1.0 - MathUtility.Divide(amountToRemove * fracRemoving[3], leaves.DMDead, 0.0));
            leaves.tissue[t].DM *= fracRemaining;
            leaves.tissue[t].Namount *= fracRemaining;
            leaves.tissue[t].NRemobilisable *= fracRemaining;

            // Stems
            fracRemaining = Math.Max(0.0, 1.0 - MathUtility.Divide(amountToRemove * fracRemoving[1], stems.DMGreen, 0.0));
            for (t = 0; t < 3; t++)
            {
                stems.tissue[t].DM *= fracRemaining;
                stems.tissue[t].Namount *= fracRemaining;
                stems.tissue[t].NRemobilisable *= fracRemaining;
            }
            fracRemaining = Math.Max(0.0, 1.0 - MathUtility.Divide(amountToRemove * fracRemoving[4], stems.DMDead, 0.0));
            stems.tissue[t].DM *= fracRemaining;
            stems.tissue[t].Namount *= fracRemaining;
            stems.tissue[t].NRemobilisable *= fracRemaining;

            // Stolons
            fracRemaining = Math.Max(0.0, 1.0 - MathUtility.Divide(amountToRemove * fracRemoving[2], stolons.DMGreen, 0.0));
            for (t = 0; t < 3; t++)
            {
                stolons.tissue[t].DM *= fracRemaining;
                stolons.tissue[t].Namount *= fracRemaining;
                stolons.tissue[t].NRemobilisable *= fracRemaining;
            }
        }

        // Set outputs and check balance
        dmdefoliated = preRemovalDMShoot - AboveGroundWt;
        fractionDefoliated = dmdefoliated / preRemovalDMShoot;
        Ndefoliated = preRemovalNShoot - AboveGroundN;
        if (Math.Abs(dmdefoliated - amountToRemove) > Epsilon)
            throw new Exception("  AgPasture " + speciesName + " - removal of DM resulted in loss of mass balance");
        else
            Console.WriteLine(" Biomass removed from " + speciesName + " by grazing: " + dmdefoliated.ToString("#0.0") + "kg/ha");

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
        double termActual = (CO2 + myCO2EffectScaleFactor) / CO2;
        double termReference = (myReferenceCO2 + myCO2EffectScaleFactor) / myReferenceCO2;
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
            if (leaves.DMGreen > Epsilon)
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
        if (CO2 <= myReferenceCO2)
            return 1.0;

        double factorCO2 = Math.Pow((myCO2EffectOffsetFactor - myReferenceCO2) / (CO2 - myReferenceCO2), myCO2EffectExponent);
        double Fn = (myCO2EffectMinimum + factorCO2) / (1.0 + factorCO2);
        return Fn;
    }

    /// <summary>Canopy conductance decline to elevated [CO2]</summary>
    /// <returns>Effects on canopy conductance</returns>
    private double ConductanceCO2Effects()
    {
        if (Math.Abs(CO2 - myReferenceCO2) < 0.5)
            return 1.0;
        //Hard coded here, not used, should go to Micromet!
        double Gmin = 0.2; //Fc = Gmin when CO2->unlimited
        double Gmax = 1.25; //Fc = Gmax when CO2 = 0;
        double beta = 2.5; //curvature factor,

        double Fc = Gmin + (Gmax - Gmin) * (1 - Gmin) * Math.Pow(myReferenceCO2, beta) /
                    ((Gmax - 1) * Math.Pow(CO2, beta) + (1 - Gmin) * Math.Pow(myReferenceCO2, beta));
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
            if (MetFile.MaxT > myHeatFullTemperature)
            {
                // very high temperature, full stress
                heatFactor = 0.0;
                accumTHeat = 0.0;
            }
            else if (MetFile.MaxT > myHeatOnsetTemperature)
            {
                // high temperature, add some stress
                heatFactor = highTempStress * (myHeatFullTemperature - MetFile.MaxT) / (myHeatFullTemperature - myHeatOnsetTemperature);
                accumTHeat = 0.0;
            }
            else
            {
                // cool temperature, same stress as yesterday
                heatFactor = highTempStress;
            }

            // check recovery factor
            double recoveryFactor = 0.0;
            if (MetFile.MaxT <= myHeatOnsetTemperature)
                recoveryFactor = (1 - heatFactor) * (accumTHeat / myHeatRecoverySumDD);

            // accumulate temperature
            accumTHeat += Math.Max(0.0, myHeatRecoveryTReference - Tmean);

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
            if (MetFile.MinT < myColdFullTemperature)
            {
                // very low temperature, full stress
                coldFactor = 0.0;
                accumTCold = 0.0;
            }
            else if (MetFile.MinT < myColdOnsetTemperature)
            {
                // low temperature, add some stress
                coldFactor = lowTempStress * (MetFile.MinT - myColdFullTemperature) / (myColdOnsetTemperature - myColdFullTemperature);
                accumTCold = 0.0;
            }
            else
            {
                // warm temperature, same stress as yesterday
                coldFactor = lowTempStress;
            }

            // check recovery factor
            double recoveryFactor = 0.0;
            if (MetFile.MinT >= myColdOnsetTemperature)
                recoveryFactor = (1 - coldFactor) *(accumTCold / myColdRecoverySumDD);

            // accumulate temperature
            accumTCold += Math.Max(0.0, Tmean - myColdRecoveryTReference);

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
        if (myPhotosyntheticPathway == "C4")
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
        double growthTmax = myGrowthToptimum + ((myGrowthToptimum - myGrowthTminimum) / myGrowthTEffectExponent);
        if ((T > myGrowthTminimum) && (T < growthTmax))
        {
            double val1 = Math.Pow((T - myGrowthTminimum), myGrowthTEffectExponent) * (growthTmax - T);
            double val2 = Math.Pow((myGrowthToptimum - myGrowthTminimum), myGrowthTEffectExponent) * (growthTmax - myGrowthToptimum);
            result = val1 / val2;
            if (result < Epsilon) result = 0.0;
        }

        return result;
    }

    /// <summary>Temperature effects on photosynthesis for C4 plants</summary>
    /// <param name="T">Temperature</param>
    /// <returns>Growth factor</returns>
    public double GFTempC4(double T)
    {
        double result;
        if (T <= myGrowthTminimum)
        {
            result = 0.0;
        }
        else if (T >= myGrowthToptimum)
        {
            result = 1.0;
        }
        else
        {
            double Tmax = myGrowthToptimum + (myGrowthToptimum - myGrowthTminimum) / myGrowthTEffectExponent;
            double val1 = Math.Pow((T - myGrowthTminimum), myGrowthTEffectExponent) * (Tmax - T);
            double val2 = Math.Pow((myGrowthToptimum - myGrowthTminimum), myGrowthTEffectExponent) * (Tmax - myGrowthToptimum);
            result = val1 / val2;
            if (result < Epsilon) result = 0.0;
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
            double baseEffect = 1.0 - Math.Exp(-Math.Pow(Tmean / myRespirationTReference, myRespirationExponent));
            result = baseEffect / scalef;
        }

        return result;
    }

    /// <summary>Effect of temperature on tissue turnover rate</summary>
    /// <returns>Turnover factor</returns>
    private double TemperatureEffectOnTissueTurnover()
    {
        double result;
        if (Tmean <= myTurnoverTemperatureMin)
            result = 0.0;
        else if (Tmean < myTurnoverTemperatureRef)
            result = Math.Pow((Tmean - myTurnoverTemperatureMin) / (myTurnoverTemperatureRef - myTurnoverTemperatureMin), myTurnoverTemperatureExponent);
        else
            result = 1.0;

        return result;
    }

    /// <summary>Effect of water stress on tissue turnover rate</summary>
    /// <returns>Turnover factor</returns>
    private double MoistureEffectOnTissueTurnover()
    {
        double result = 1.0;
        if (Math.Min(glfWater, glfWLogging) < myTurnoverDroughtThreshold)
            result = 1.0 + (myTurnoverDroughtEffectMax - 1.0) * ((myTurnoverDroughtThreshold - Math.Min(glfWater, glfWLogging)) / myTurnoverDroughtThreshold);

        return Math.Max(1.0, Math.Min(myTurnoverDroughtEffectMax, result));
    }

    /// <summary>Effect of defoliation on stolon/root turnover rate.</summary>
    /// <remarks>
    /// This approach spreads the effect over a few days after a defoliation, starting large and decreasing with time.
    /// It is assumed that a defoliation of 100% of harvestable material will result in a full decay of stolons.
    /// </remarks>
    /// <returns>A factor for adjusting tissue turnover (0-1)</returns>
    private double DefoliationEffectOnTissueTurnover()
    {
        double defoliationEffect = 0.0;
        cumDefoliationFactor = Math.Min(1.0, cumDefoliationFactor + fractionDefoliated);
        if (cumDefoliationFactor > Epsilon)
        {
            double todaysFactor = Math.Pow(cumDefoliationFactor, myTurnoverDefoliationCoefficient + 1.0);
            todaysFactor /= (myTurnoverDefoliationCoefficient + 1.0);
            if (cumDefoliationFactor - todaysFactor < myTurnoverDefoliationEffectMin)
            {
                defoliationEffect = cumDefoliationFactor;
                cumDefoliationFactor = 0.0;
            }
            else
            {
                defoliationEffect = cumDefoliationFactor - todaysFactor;
                cumDefoliationFactor = todaysFactor;
            }
        }

        // clear fraction defoliated after use
        fractionDefoliated = 0.0;

        return defoliationEffect;
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
        get { return (1.0 - Math.Exp(-myLightExtinctionCoefficient * greenLAI)); }
    }

    /// <summary>Gets the dead tissues cover</summary>
    internal double coverDead
    {
        get { return (1.0 - Math.Exp(-myLightExtinctionCoefficient * deadLAI)); }
    }

    /// <summary>Gets the total plant cover</summary>
    internal double coverTotal
    {
        get { return (1.0 - (Math.Exp(-myLightExtinctionCoefficient * totalLAI))); }
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
        double depthFirstStage = Math.Min(myRootDepthMaximum, myRootDistributionDepthParam);
        double rootDeepDepthParam = 1.0;
        double xFac;
        for (int layer = 0; layer < nLayers; layer++)
        {
            if (xf.Length > 1)
                xFac = xf[layer];
            else
                xFac = 1.0;
            depthBottom += dlayer[layer];
            if (depthTop >= myRootDepthMaximum)
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
                result[layer] = Math.Pow(myRootDepthMaximum * rootDeepDepthParam - Math.Max(depthTop, depthFirstStage), myRootDistributionExponent + 1)
                              - Math.Pow(myRootDepthMaximum * rootDeepDepthParam - Math.Min(depthBottom, myRootDepthMaximum), myRootDistributionExponent + 1);
                result[layer] /= (myRootDistributionExponent + 1) * Math.Pow(myRootDepthMaximum * rootDeepDepthParam - depthFirstStage, myRootDistributionExponent);
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
        double topLayersDepth = 0.0;
        double cumProportion = 0.0;
        double[] result = new double[nLayers];

        // Get the total weight over the root zone, first layers totally within the root zone
        for (int layer = 0; layer < layerBottomRootZone; layer++)
        {
            cumProportion += targetRootAllocation[layer];
            topLayersDepth += dlayer[layer];
        }
        // Then consider layer at the bottom of the root zone
        double layerFrac = Math.Min(1.0, (myRootDepthMaximum - topLayersDepth) / (rootDepth - topLayersDepth));
        cumProportion += targetRootAllocation[layerBottomRootZone] * Math.Min(1.0, Math.Max(0.0, layerFrac));

        // Normalise the weights to be a fraction, adds up to one
        if (cumProportion > Epsilon)
        {
            for (int layer = 0; layer < layerBottomRootZone; layer++)
                result[layer] = targetRootAllocation[layer] / cumProportion;
            result[layerBottomRootZone] = targetRootAllocation[layerBottomRootZone] * layerFrac / cumProportion;
        }

        return result;
    }

    /// <summary>Computes how much of the layer is actually explored by roots</summary>
    /// <param name="layer">The layer to be analysed</param>
    /// <returns>The fraction of layer explored by roots</returns>
    internal double LayerFractionWithRoots(int layer)
    {
        double depthToTopOfLayer = 0.0;
        for (int i = 0; i < layer; i++)
            depthToTopOfLayer += dlayer[i];
        double fractionInLayer = (rootDepth - depthToTopOfLayer) / dlayer[layer];

        return Math.Min(1.0, Math.Max(0.0, fractionInLayer));
    }

    /// <summary>Finds the layer at the bottom of the root zone</summary>
    /// <returns>The layer at bottom of root zone</returns>
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

    /// <summary>Calculates the average digestibility of standing herbage</summary>
    /// <returns>The digestibility</returns>
    internal void EvaluateDigestibility()
    {
        double result = 0.0;
        if (StandingWt > Epsilon)
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
                if (DMTotal > Epsilon)
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
                if (DMGreen > Epsilon)
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
                if (DMDead > Epsilon)
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

                if (tissueCN > Epsilon)
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

                if (tissueCN > Epsilon)
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
