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
    #region Static variables, for parameters common for all species  ------------------------------

    /// <summary>Some Description</summary>
    internal static Clock Clock;

    /// <summary>Some Description</summary>
    internal static MetFile MetFile;

    /// <summary>Some Description</summary>
    internal static double CO2 = 380;

    /// <summary>Some Description</summary>
    internal static double swardInterceptedRadn;                          //total Radn intercepted by pasture

    /// <summary>Some Description</summary>
    internal static double swardCoverGreen;

    /// <summary>Some Description</summary>
    internal static double swardLightExtCoeff;                    //k of mixed pasture

    #endregion

    #region Main parameters  ----------------------------------------------------------------------

    //// - General parameters  --------------------------------------------------------------------

    /// <summary>Some Description</summary>
    internal string speciesName;

    /// <summary>Some Description</summary>
    internal string micrometType;

    /// <summary>Some Description</summary>
    internal bool isLegume;        //Legume (0=no,1=yes)

    /// <summary>Some Description</summary>
    internal string photoPath;       //Photosynthesis pathways: 3=C3, 4=C4; //no consideration for CAM(=3)

    /// <summary>Previous state</summary>
    internal SpeciesState prevState = new SpeciesState();              //for remembering the state of previous day

    //// - annual species parameters - not fully implemented  ----------------------------------------

    /// <summary>Some Description</summary>
    internal bool isAnnual;        //Species type (1=annual,0=perennial)

    /// <summary>Some Description</summary>
    internal int dayEmerg;         //Earlist day of emergence (for annuals only)

    /// <summary>Some Description</summary>
    internal int monEmerg;        //Earlist month of emergence (for annuals only)

    /// <summary>Some Description</summary>
    internal int dayAnth;            //Earlist day of anthesis (for annuals only)

    /// <summary>Some Description</summary>
    internal int monAnth;            //Earlist month of anthesis (for annuals only)

    /// <summary>Some Description</summary>
    internal int daysToMature;    //Days from anthesis to maturity (for annuals only)

    /// <summary>Some Description</summary>
    internal int daysEmgToAnth;   //Days from emergence to Anthesis (calculated, annual only)

    /// <summary>Some Description</summary>
    internal int phenoStage = 1;  //pheno stages: 0 - pre_emergence, 1 - vegetative, 2 - reproductive

    /// <summary>Some Description</summary>
    internal double phenoFactor = 1;

    /// <summary>Some Description</summary>
    internal int daysfromEmergence = 0;   //days

    /// <summary>Some Description</summary>
    internal int daysfromAnthesis = 0;    //days

    /// <summary>Some Description</summary>
    internal int dRootDepth;        //Daily root growth (mm)

    /// <summary>Some Description</summary>
    internal int maxRootDepth;    //Maximum root depth (mm)

    internal bool bSown = false;
    private double DDSfromSowing = 0.0;
    private double DDSfromEmergence = 0.0;
    private double DDSfromAnthesis = 0.0;
    //// ------------------------------------------------------------------------------------------

    //// - Specific parameters  -------------------------------------------------------------------

    //// > Plant growth >>>

    /// <summary>Some Description</summary>
    internal double Pm;                    //reference leaf co2 mg/m^2/s maximum

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

    /// <summary>Some Description</summary>
    internal double lightExtCoeff;    //Light extinction coefficient

    /// <summary>Some Description</summary>
    internal double growthTmin;   //Minimum temperature (grtmin) - originally 0

    /// <summary>Some Description</summary>
    internal double growthTopt;   //Optimum temperature (grtopt) - originally 20

    /// <summary>Some Description</summary>
    internal double growthTref;

    /// <summary>Some Description</summary>
    internal double growthTq;        //Temperature n (grtemn) --fyl: q curvature coefficient, 1.5 for c3 & 2 for c4 in IJ

    /// <summary>Some Description</summary>
    internal bool usingHeatStress = false;

    /// <summary>Some Description</summary>
    internal double heatOnsetT;            //onset tempeature for heat effects

    /// <summary>Some Description</summary>
    internal double heatFullT;            //full temperature for heat effects

    /// <summary>Some Description</summary>
    internal double heatTq;

    /// <summary>Some Description</summary>
    internal double heatSumT;            //temperature sum for recovery - sum of (25-mean)

    /// <summary>Some Description</summary>
    internal double heatRecoverT;

    /// <summary>Some Description</summary>
    internal double highTempStress = 1.0;  //fraction of growth rate due to high temp. effect

    /// <summary>Some Description</summary>
    private double accumTHeat = 0.0;          //accumulated temperature from previous heat strike = sum of '25-MeanT'(>0)

    /// <summary>Some Description</summary>
    private double heatFactor = 1.0;

    /// <summary>Some Description</summary>
    internal bool usingColdStress = false;

    /// <summary>Some Description</summary>
    internal double coldOnsetT;          //onset tempeature for cold effects

    /// <summary>Some Description</summary>
    internal double coldFullT;            //full tempeature for cold effects

    /// <summary>Some Description</summary>
    internal double coldTq;

    /// <summary>Some Description</summary>
    internal double coldSumT;            //temperature sum for recovery - sum of means

    /// <summary>Some Description</summary>
    internal double coldRecoverT;

    /// <summary>Some Description</summary>
    internal double lowTempStress = 1.0;   //fraction of growth rate due to low temp. effect

    /// <summary>Some Description</summary>
    private double accumTCold = 0.0;       //accumulated temperature from previous cold strike = sum of MeanT (>0)

    /// <summary>Some Description</summary>
    private double coldFactor = 1.0;

    /// <summary>Some Description</summary>
    internal double referenceCO2 = 380;                  //ambient CO2 concentration

    /// <summary>Some Description</summary>
    internal double CO2PmaxScale;

    /// <summary>Some Description</summary>
    internal double CO2NScale;

    /// <summary>Some Description</summary>
    internal double CO2NMin;

    /// <summary>Some Description</summary>
    internal double CO2NCurvature;

    //// > DM partition >>>

    /// <summary>Some Description</summary>
    internal double targetSRratio;       //Shoot-Root ratio maximum

    /// <summary>Some Description</summary>
    internal double maxRootFraction;       //Root DM allocation maximum (to be deprecated)

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
    internal double fStolon;            //Fixed growth partition to stolon (0-1)

    /// <summary>Some Description</summary>
    internal double specificLeafArea;                //Specific leaf area (m2/kg dwt)

    /// <summary>Some Description</summary>
    internal double specificRootLength;

    //// > DM turnover and senescence  >>>

    /// <summary>Some Description</summary>
    internal double liveLeavesPerTiller;

    /// <summary>Some Description</summary>
    internal double refTissueTurnoverRate;    //Decay coefficient between live and dead

    /// <summary>Some Description</summary>
    internal double facGrowingTissue;

    /// <summary>Some Description</summary>
    internal double refTurnoverRateStolon;

    /// <summary>Some Description</summary>
    internal double refLitteringRate;    //Decay coefficient between dead and litter

    /// <summary>Some Description</summary>
    internal double rateRootSen;      //Decay reference root senescence rate (%/day)

    /// <summary>Some Description</summary>
    internal double massFluxTmin;            //grfxt1    Mass flux minimum temperature

    /// <summary>Some Description</summary>
    internal double massFluxTopt;            //grfxt2    Mass flux optimum temperature

    /// <summary>Some Description</summary>
    internal double massFluxTq;

    /// <summary>Some Description</summary>
    internal double massFluxW0;            //grfw1        Mass flux scale factor at GLFwater=0 (must be > 1)

    /// <summary>Some Description</summary>
    internal double massFluxWopt;         //grfw2        Mass flux optimum temperature

    /// <summary>Some Description</summary>
    internal double exponentGLFW2dead;

    /// <summary>Some Description</summary>
    internal double stockParameter;   //Stock influence parameter

    /// <summary>Some Description</summary>
    internal static double stockingRate = 0.0;  //stocking rate affacting transfer of dead to little (default as 0 for now)

    /// <summary>Some Description</summary>
    internal double Kappa2 = 0.0;

    /// <summary>Some Description</summary>
    internal double Kappa3 = 0.0;

    /// <summary>Some Description</summary>
    internal double Kappa4 = 0.0;

    //// > Digestibility and quality  >>>

    /// <summary>Some Description</summary>
    internal double digestLive;   //Digestibility of live plant material (0-1)

    /// <summary>Some Description</summary>
    internal double digestDead;   //Digestibility of dead plant material (0-1)

    //// > N fixation  >>>

    /// <summary>Some Description</summary>
    internal double MaxFix;   //N-fix fraction when no soil N available, read in later

    /// <summary>Some Description</summary>
    internal double MinFix;   //N-fix fraction when soil N sufficient

    //// > DM amounts, for each tissue and pool

    /// <summary>Some Description</summary>
    internal double dmleaf1;    //leaf 1 (kg/ha)

    /// <summary>Some Description</summary>
    internal double dmleaf2;    //leaf 2 (kg/ha)

    /// <summary>Some Description</summary>
    internal double dmleaf3;    //leaf 3 (kg/ha)

    /// <summary>Some Description</summary>
    internal double dmleaf4;    //leaf dead (kg/ha)

    /// <summary>Some Description</summary>
    internal double dmstem1;    //sheath and stem 1 (kg/ha)

    /// <summary>Some Description</summary>
    internal double dmstem2;    //sheath and stem 2 (kg/ha)

    /// <summary>Some Description</summary>
    internal double dmstem3;    //sheath and stem 3 (kg/ha)

    /// <summary>Some Description</summary>
    internal double dmstem4;    //sheath and stem dead (kg/ha)

    /// <summary>Some Description</summary>
    internal double dmstol1;    //stolon 1 (kg/ha)

    /// <summary>Some Description</summary>
    internal double dmstol2;    //stolon 2 (kg/ha)

    /// <summary>Some Description</summary>
    internal double dmstol3;    //stolon 3 (kg/ha)

    /// <summary>Some Description</summary>
    internal double dmroot;    //root (kg/ha)

    /// <summary>Some Description</summary>
    internal double dmgreenmin; // minimum grenn dm

    /// <summary>Some Description</summary>
    internal double dmdeadmin; // minimum dead dm

    /// <summary>Some Description</summary>
    internal double dmgreen;

    /// <summary>Some Description</summary>
    internal double dmdead;

    /// <summary>Some Description</summary>
    internal double dmleaf;

    /// <summary>Some Description</summary>
    internal double dmstem;

    /// <summary>Some Description</summary>
    internal double dmleaf_green;

    /// <summary>Some Description</summary>
    internal double dmstem_green;

    /// <summary>Some Description</summary>
    internal double dmstol_green;

    /// <summary>Some Description</summary>
    internal double dmstol;

    /// <summary>Some Description</summary>
    internal double dmshoot;

    //// > Nc - N concentration  >>>

    /// <summary>Some Description</summary>
    internal double NcstemFr;   //stem Nc as % of leaf Nc

    /// <summary>Some Description</summary>
    internal double NcstolFr;   //stolon Nc as % of leaf Nc

    /// <summary>Some Description</summary>
    internal double NcrootFr;   //root Nc as % of leaf Nc

    /// <summary>Some Description</summary>
    internal double NcRel2;     //N concentration in tissue 2 relative to tissue 1

    /// <summary>Some Description</summary>
    internal double NcRel3;     //N concentration in tissue 3 relative to tissue 1

    /// <summary>Some Description</summary>
    internal double Ncleaf1;    //leaf 1  (critical N %)

    /// <summary>Some Description</summary>
    internal double Ncleaf2;    //leaf 2

    /// <summary>Some Description</summary>
    internal double Ncleaf3;    //leaf 3

    /// <summary>Some Description</summary>
    internal double Ncleaf4;    //leaf dead

    /// <summary>Some Description</summary>
    internal double Ncstem1;    //sheath and stem 1

    /// <summary>Some Description</summary>
    internal double Ncstem2;    //sheath and stem 2

    /// <summary>Some Description</summary>
    internal double Ncstem3;    //sheath and stem 3

    /// <summary>Some Description</summary>
    internal double Ncstem4;    //sheath and stem dead

    /// <summary>Some Description</summary>
    internal double Ncstol1;    //stolon 1

    /// <summary>Some Description</summary>
    internal double Ncstol2;    //stolon 2

    /// <summary>Some Description</summary>
    internal double Ncstol3;    //stolon 3

    /// <summary>Some Description</summary>
    internal double Ncroot;        //root

    /// <summary>Some Description</summary>
    internal double NcleafOpt;    //leaf   (critical N %)

    /// <summary>Some Description</summary>
    internal double NcstemOpt;    //sheath and stem

    /// <summary>Some Description</summary>
    internal double NcstolOpt;    //stolon

    /// <summary>Some Description</summary>
    internal double NcrootOpt;    //root

    /// <summary>Some Description</summary>
    internal double NcleafMax;    //leaf  (critical N %)

    /// <summary>Some Description</summary>
    internal double NcstemMax;    //sheath and stem

    /// <summary>Some Description</summary>
    internal double NcstolMax;    //stolon

    /// <summary>Some Description</summary>
    internal double NcrootMax;    //root

    /// <summary>Some Description</summary>
    internal double NcleafMin;

    /// <summary>Some Description</summary>
    internal double NcstemMin;

    /// <summary>Some Description</summary>
    internal double NcstolMin;

    /// <summary>Some Description</summary>
    internal double NcrootMin;

    //// > N amount in each pool  >>>

    /// <summary>Some Description</summary>
    internal double Nleaf1 = 0.0;

    /// <summary>Some Description</summary>
    internal double Nleaf2 = 0.0;

    /// <summary>Some Description</summary>
    internal double Nleaf3 = 0.0;

    /// <summary>Some Description</summary>
    internal double Nleaf4 = 0.0;

    /// <summary>Some Description</summary>
    internal double Nstem1 = 0.0;

    /// <summary>Some Description</summary>
    internal double Nstem2 = 0.0;

    /// <summary>Some Description</summary>
    internal double Nstem3 = 0.0;

    /// <summary>Some Description</summary>
    internal double Nstem4 = 0.0;

    /// <summary>Some Description</summary>
    internal double Nstol1 = 0.0;

    /// <summary>Some Description</summary>
    internal double Nstol2 = 0.0;

    /// <summary>Some Description</summary>
    internal double Nstol3 = 0.0;

    /// <summary>Some Description</summary>
    internal double Nroot = 0.0;

    /// <summary>Some Description</summary>
    internal double Nshoot;    //above-ground total N (kg/ha)

    /// <summary>Some Description</summary>
    internal double Nleaf;    //leaf N

    /// <summary>Some Description</summary>
    internal double Nstem;    //stem N

    /// <summary>Some Description</summary>
    internal double Ngreen;    //live N

    /// <summary>Some Description</summary>
    internal double Ndead;    //in standing dead (kg/ha)

    /// <summary>Some Description</summary>
    internal double Nstolon;    //stolon

    //// > LAI  >>>

    /// <summary>Some Description</summary>
    internal double greenLAI; //sum of 3 pools

    /// <summary>Some Description</summary>
    internal double deadLAI;  //pool dmleaf4

    /// <summary>Some Description</summary>
    internal double totalLAI;

    //// Root depth and distribution

    /// <summary>Some Description</summary>
    internal bool usingSpeciesRoot;

    /// <summary>Some Description</summary>
    internal double rootDepth;       //current root depth (mm)

    /// <summary>Some Description</summary>
    internal int rootDistributionMethod = 2;

    /// <summary>Some Description</summary>
    internal double expoLinearDepthParam = 0.12;

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
    internal double[] rootFraction;

    /// <summary>Root length density (mm/mm3)</summary>
    internal double[] RLD
    {
        get
        {
            double[] Result = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
            {
                Result[layer] = (dmroot * 0.1) * rootFraction[layer] * specificRootLength;   // m/m2
                Result[layer] /= dlayer[layer] * 1000;    // mm/mm3
            }

            return Result;
        }
    }

    ////  >> Plant height  >>>

    /// <summary>Some Description</summary>
    internal bool usingSpeciesHeight;

    /// <summary>Some Description</summary>
    internal double height;

    /// <summary>Some Description</summary>
    internal double MaxPlantHeight;

    /// <summary>Some Description</summary>
    internal double MassForMaxHeight;

    /// <summary>Some Description</summary>
    internal double ExponentHeightFromMass;

    /// <summary>Some Description</summary>
    internal double MinimumHeight;

    //// > N and C cycling  >>>

    /// <summary>Some Description</summary>
    internal double Nremob = 0.0;       //N remobiliesd N during senescing

    /// <summary>Some Description</summary>
    internal double Cremob = 0.0;

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
    internal double newGrowthN = 0.0;    //N plant-soil

    /// <summary>Some Description</summary>
    internal double NdemandLux = 0.0;      //N demand for new growth, with luxury uptake

    /// <summary>Some Description</summary>
    internal double NdemandOpt = 0.0;

    /// <summary>Some Description</summary>
    internal double Nfix = 0.0;         //N fixed by legumes

    /// <summary>Some Description</summary>
    internal double NLuxury2 = 0.0;               // luxury N (above Nopt) potentially remobilisable

    /// <summary>Some Description</summary>
    internal double NLuxury3 = 0.0;               // luxury N (above Nopt)potentially remobilisable

    /// <summary>Some Description</summary>
    internal double NFastRemob2 = 0.0;   // amount of luxury N remobilised from tissue 2

    /// <summary>Some Description</summary>
    internal double NFastRemob3 = 0.0;   // amount of luxury N remobilised from tissue 3

    //// > Harvest variables  >>>

    /// <summary>Some Description</summary>
    internal double dmdefoliated;

    /// <summary>Some Description</summary>
    internal double Ndefoliated;

    /// <summary>Some Description</summary>
    internal double digestHerbage;

    /// <summary>Some Description</summary>
    internal double digestDefoliated;

    //// > Water and N uptake  >>>

    /// <summary>Plant soil available water</summary>
    internal double[] soilAvailableWater;

    /// <summary>Plant water demand</summary>
    internal double WaterDemand = 0.0;

    /// <summary>Amount of soil water taken up</summary>
    internal double WaterUptake = 0.0;

    /// <summary>Amount of soil water taken up</summary>
    internal double[] soilWaterUptake;

    /// <summary>Some Description</summary>
    internal double waterStressFactor;

    /// <summary>Some Description</summary>
    internal double soilSatFactor;

    /// <summary>Amount of NH4 available for uptake</summary>
    internal double[] soilAvailableNH4;

    /// <summary>Amount of NO3 available for uptake</summary>
    internal double[] soilAvailableNO3;

    /// <summary>N demand from soil</summary>
    internal double soilNdemand = 0.0;

    /// <summary>Amount of N taken up</summary>
    internal double soilNuptake = 0.0;

    /// <summary>Some Description</summary>
    internal double soilNH4Uptake;
    /// <summary>Some Description</summary>
    internal double soilNO3Uptake;

    /// <summary>Some Description</summary>
    internal double NdilutCoeff;

    //// > growth limiting factors  >>>

    /// <summary>Some Description</summary>
    internal double glfWater = 1.0;  //from water stress

    /// <summary>Some Description</summary>
    internal double glfTemp = 1.0;   //from temperature

    /// <summary>Some Description</summary>
    internal double glfN = 1.0;      //from N deficit

    /// <summary>Some Description</summary>
    internal double RadnFactor;

    /// <summary>Some Description</summary>
    internal double CO2Factor;

    /// <summary>Some Description</summary>
    internal double NcFactor;

    /// <summary>Some Description</summary>
    internal double GLFSFertility;

    /// <summary>Some Description</summary>
    internal double GLFGeneric;

    /// <summary>Some Description</summary>
    internal double ExtremeTempStress;

    //// > Growth deltas  >>>

    /// <summary>Some Description</summary>
    internal double dGrowthPot = 0.0;     //daily growth potential

    /// <summary>Some Description</summary>
    internal double dGrowthW = 0.0;       //daily growth with water-deficit incorporated

    /// <summary>Some Description</summary>
    internal double dGrowth = 0.0;        //daily growth

    /// <summary>Some Description</summary>
    internal double dGrowthRoot = 0.0;    //daily root growth

    /// <summary>Some Description</summary>
    internal double dGrowthHerbage = 0.0; //daily growth shoot

    /// <summary>Some Description</summary>
    internal double dLitter = 0.0;        //daily litter production

    /// <summary>Some Description</summary>
    internal double dNLitter = 0.0;       //N in dLitter

    /// <summary>Some Description</summary>
    internal double dRootSen = 0.0;       //daily root sennesce

    /// <summary>Some Description</summary>
    internal double dNrootSen = 0.0;     //N in dRootSen

    /// <summary>Some Description</summary>
    internal double IL;

    /// <summary>Some Description</summary>
    internal double PotPhoto;

    /// <summary>Some Description</summary>
    internal double Pgross;

    /// <summary>Some Description</summary>
    internal double Resp_m;

    /// <summary>Some Description</summary>
    internal double Resp_g;

    //// > Partitioning and turnover rates  >>>

    /// <summary>actual fraction of new growth added to shoot</summary>
    internal double fShoot = 1.0;

    /// <summary>Actual fraction of shoot growth added to leaves</summary>
    internal double fLeaf;

    /// <summary>Some Description</summary>
    internal double gama = 0.0;    // from tissue 1 to 2, then 3 then 4

    /// <summary>Some Description</summary>
    internal double gamaS = 0.0;    // for stolons

    /// <summary>Some Description</summary>
    internal double gamaD = 0.0;    // from dead to litter

    /// <summary>Some Description</summary>
    internal double gamaR = 0.0;    // for roots (to dead/FOM)

    //// > Intercepted radiation  >>>

    /// <summary>Some Description</summary>
    internal double intRadnFrac;     //fraction of Radn intercepted by this species = intRadn/Radn

    /// <summary>Some Description</summary>
    internal double interceptedRadn;         //Intercepted Radn by this species (MJ/day)

    //// > Soil layering  >>>
    /// <summary>Some Description</summary>
    internal double[] dlayer;

    #endregion

    #region Internal constants  ----------------------------------------------------------------------

    //// - Constants  -----------------------------------------------------------------------------

    /// <summary>Some Description</summary>
    const double CarbonFractionDM = 0.4;            //DM to C converion

    /// <summary>Some Description</summary>
    const double N2Protein = 6.25;      //this is for plants... (higher amino acids)

    /// <summary>Some Description</summary>
    const double ProteinCNr = 3.5;     //C:N in remobilised material

    #endregion

    #region Initialisation methods  ---------------------------------------------------------------

    /// <summary>
    /// Refresh variables
    /// </summary>
    internal void DailyRefresh()
    {
        dmdefoliated = 0.0;
        Ndefoliated = 0.0;
        digestDefoliated = 0.0;
    }

    /// <summary>
    /// Reset variables
    /// </summary>
    internal void ResetZero()
    {
        //Reset dm pools
        dmleaf1 = dmleaf2 = dmleaf3 = dmleaf4 = 0.0;    //(kg/ha)
        dmstem1 = dmstem2 = dmstem3 = dmstem4 = 0.0;    //sheath and stem
        dmstol1 = dmstol2 = dmstol3 = 0.0;
        dmroot = 0.0;

        dmdefoliated = 0.0;

        //Reset N pools
        Nleaf1 = Nleaf2 = Nleaf3 = Nleaf4 = 0.0;
        Nstem1 = Nstem2 = Nstem3 = Nstem4 = 0.0;
        Nstol1 = Nstol2 = Nstol3 = Nroot = 0.0;

        phenoStage = 0;
    }

    #endregion

    #region Annual species  -----------------------------------------------------------------------

    /// <summary>
    /// Set germination
    /// </summary>
    internal void SetInGermination()
    {
        bSown = true;
        phenoStage = 0; //before germination
    }

    /// <summary>
    /// Emergence to anthesys
    /// </summary>
    /// <returns>Number of days</returns>
    internal int CalcDaysEmgToAnth()
    {
        daysEmgToAnth = 0;
        int numbMonths = monAnth - monEmerg;  //emergence & anthesis in the same calendar year: monEmerg < monAnth
        if (monEmerg >= monAnth)              //...across the calendar year
            numbMonths += 12;

        daysEmgToAnth = (int)(30.5 * numbMonths + (dayAnth - dayEmerg));

        return daysEmgToAnth;
    }

    /// <summary>
    /// Set state
    /// </summary>
    private void SetEmergentState()
    {
        dmleaf1 = 10.0;   //(kg/ha)
        dmleaf2 = 20.0;
        dmleaf3 = 20.0;
        dmleaf4 = 0.0;
        if (!isLegume)
        {
            dmstem1 = 5.0;
            dmstem2 = 10.0;
            dmstem3 = 0.0;
            dmstem4 = 0.0;
            dmroot = 50.0;
        }
        else
        {
            dmstol1 = 5.0;
            dmstol2 = 10.0;
            dmstol3 = 0.0;
            dmroot = 25.0;
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
        UpdateAggregatedVariables();

        dGrowthPot = 0.0;        // daily growth potential
        dGrowthW = 0.0;          // daily growth considering only water deficit
        dGrowth = 0.0;           // daily growth actual
        dGrowthRoot = 0.0;       // daily root growth
        fShoot = 1.0;            // actual fraction of dGrowth allocated to shoot
    }

    /// <summary>
    /// Get phenology state
    /// </summary>
    /// <returns>Phenology index</returns>
    internal int Phenology()
    {
        const double DDSEmergence = 150;   // to be an input parameter
        double meanT = 0.5 * (MetFile.MaxT + MetFile.MinT);

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

    /// <summary>
    /// Annuals phenology
    /// </summary>
    /// <returns>true or false</returns>
    internal bool annualPhenology()
    {
        if (Clock.month == monEmerg && Clock.day_of_month == dayEmerg)
            phenoStage = 1;         //vegetative stage
        else if (Clock.month == monAnth && Clock.day_of_month == dayAnth)
            phenoStage = 2;         //reproductive

        if (phenoStage == 0)        //before emergence
        {
            dGrowthPot = 0.0;
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
                dGrowthPot = 0.0;
                return false;       // Flag no growth after mature
            }
            return true;
        }
        return true;
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
            //considering root distribution change, here?
        }

        return rootDepth;  // no root depth change for pereniel pasture
    }

    /// <summary>
    /// Reduction of growth in annual species, related to phenology
    /// </summary>
    /// <returns>Pot growth</returns>
    public double annualSpeciesReduction()
    {
        double rFactor = 1;  // reduction factor of annual species
        if (phenoStage == 1 && daysfromEmergence < 60)  //decline at the begining due to seed bank effects ???
        {
            rFactor = 0.5 + 0.5 * daysfromEmergence / 60;
        }
        else if (phenoStage == 2)                       //decline of photosynthesis when approaching maturity
        {
            rFactor = 1.0 - MathUtility.Divide(daysfromAnthesis, daysToMature, 0.0);
        }
        dGrowthPot *= rFactor;
        return dGrowthPot;
    }

    #endregion

    #region Plant growth and DM partition  --------------------------------------------------------

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

        // skip growth is plant hasn't germinated yet
        if (phenoStage == 0 || greenLAI == 0)
            return dGrowthPot = 0.0;

        //Add temp effects to Pm
        double Tmean = (MetFile.MaxT + MetFile.MinT) / 2;
        double Tday = Tmean + 0.5 * (MetFile.MaxT - Tmean);

        double glfT = GFTemperature(Tmean);
        CO2Factor = PCO2Effects();
        NcFactor = PmxNeffect();

        double Pm_mean = Pm * glfT * CO2Factor * NcFactor;

        glfT = GFTemperature(Tday);
        double Pm_day = Pm * glfT * CO2Factor * NcFactor;

        double tau = 3600 * MetFile.day_length;                //conversion of hour to seconds
        IL = swardLightExtCoeff * 1.33333 * 0.5 * swardInterceptedRadn * 1000000 / tau;
        double IL2 = IL / 2;                      //IL for early & late period of a day

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

        PotPhoto = 10000 * carbon_m2;                 //10000: 'kg/m^2' =>'kg/ha'

        //Add extreme temperature effects;
        ExtremeTempStress = HeatEffect() * ColdEffect();      // in practice only one temp stress factor is < 1
        Pgross = PotPhoto * ExtremeTempStress;

        // Consider generic growth limiting factor
        Pgross *= GLFGeneric;

        // Radiation effects (for reporting purposes only)
        RadnFactor = ((0.25 * Pl2) + (0.75 * Pl1)) / ((0.25 * Pm_mean) + (0.75 * Pm_day));

        // Respiration, maintenance and growth
        double Teffect = 0.0;
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
                Teffect = Tmean / growthTopt;        // Using growthTopt as reference, and set a maximum rise to 1.25
                if (Teffect > 1.25) Teffect = 1.25;
                Teffect *= GFTemperature(growthTopt);  // Added by RCichota,oct/2014 - after changes in temp function needed this to make the function continuous
            }   //The extreme high temperature (heat) effect is added separately
        }

        double LiveDM = (dmgreen + dmroot) * CarbonFractionDM;       //converting DM to C    (kgC/ha)
        Resp_m = maintRespiration * Teffect * NcFactor * LiveDM;
        Resp_g = Pgross * (1 - growthEfficiency);

        // ** C budget is not explicitly done here as in EM
        Cremob = 0.0;                     // Nremob* C2N_protein;    // No carbon budget here
        // Nu_remob[elC] := C2N_protein * Nu_remob[elN];
        // need to substract CRemob from dm turnover?

        // Net potential growth (C) of the day (excluding growth respiration)
        dGrowthPot = Pgross + Cremob - Resp_g - Resp_m;
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

        // skip growth is plant hasn't germinated yet
        if (phenoStage == 0 || greenLAI == 0)
            isGrowing = false;

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

            //Temperature effects
            double glfTmean = GFTemperature(0.5 * (MetFile.MinT + MetFile.MaxT));

            //Potential photosynthetic rate at dawn/dusk (first and last quarter of the day)
            double Pm1 = Pm * glfTmean * CO2Factor * NcFactor;

            //Potential photosynthetic rate at brigth light (half of sunligth length, middle of day)
            double Pm2 = Pm * glfTemp * CO2Factor * NcFactor;

            //Sunlight length, converted to seconds
            double tau = 3600 * MetFile.day_length;

            //Photosynthetic active radiation - include dusk/dawn effect
            double interceptedPAR = interceptedRadn * fractionPAR * (4.0 / 3.0);
            interceptedPAR *= 1000000 / tau;          // converted from MJ/m2.day to J/m2.s

            //Intercepted irradiance (J/m2 leaf/s)
            IL = lightExtCoeff * interceptedPAR;

            //Photosynthesis per leaf area under full irradiance at the top of the canopy
            double Pl1 = SingleLeafPhotosynthesis(0.5 * interceptedPAR, Pm1);   // early and late parts of the day
            double Pl2 = SingleLeafPhotosynthesis(interceptedPAR, Pm2);         // main part of the day

            // Radiation effects (for reporting purposes only)
            RadnFactor = ((0.25 * Pl1) + (0.75 * Pl2)) / ((0.25 * Pm1) + (0.75 * Pm2));

            //Canopy photosynthesis - Upscaling from 'per LAI' to 'per ground area'
            double carbon_m2 = 0.5 * (Pl1 + Pl2);     // mgCO2/m2 leaf/s
            carbon_m2 *= coverGreen / lightExtCoeff;  // mgCO2/m2.s - land area
            carbon_m2 *= 0.000001;                    // kgCO2/m2.s
            carbon_m2 *= tau;                         // kgCO2/m2.day
            carbon_m2 *= 12.0 / 44.0;                 // kgC/m2.day
            PotPhoto = 10000 * carbon_m2;             // kgC/ha.day

            //Add extreme temperature effects;
            double TempStress = HeatEffect() * ColdEffect();      // in practice only one temp stress factor is < 1
            Pgross = PotPhoto * TempStress;

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
    /// <param name="IL">Istantaneous intercepted radiation (depend on time of day)</param>
    /// <param name="Pmc">Max photosyntehsis rate, given T, CO2 and N concentration</param>
    /// <returns></returns>
    private double SingleLeafPhotosynthesis(double IL, double Pmc)
    {
        double photoAux1 = alphaPhoto * IL + Pmc;
        double photoAux2 = 4 * thetaPhoto * alphaPhoto * IL * Pmc;
        double Pl = (0.5 / thetaPhoto) * (photoAux1 - Math.Sqrt(Math.Pow(photoAux1, 2) - photoAux2));
        return Pl;
    }

    /// <summary>
    /// Compute plant respiration, growth and maintenance
    /// </summary>
    /// <returns>Amount of C lost by respiration</returns>
    internal void DailyPlantRespiration()
    {
        double LiveDM = (dmgreen + dmroot) * CarbonFractionDM;       //converting DM to C    (kgC/ha)
        double Teffect = 0.0;
        if (LiveDM > 0.0)
        {
            double Tmean = 0.5 * (MetFile.MaxT + MetFile.MinT);
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
                    Teffect = Tmean / growthTopt;        // Using growthTopt as reference, and set a maximum rise to 1.25
                    if (Teffect > 1.25) Teffect = 1.25;
                    Teffect *= GFTemperature(growthTopt);  // Added by RCichota,oct/2014 - after changes in temp function needed this to make the function continuous
                }   //The extreme high temperature (heat) effect is added separately
            }
        }

        // maintenance respiration
        Resp_m = maintRespiration * Teffect * NcFactor * LiveDM;

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
        Cremob = 0.0;                     // Nremob* C2N_protein;    // No carbon budget here
        // Nu_remob[elC] := C2N_protein * Nu_remob[elN];
        // need to substract CRemob from dm rutnover?

        // Net potential growth (C) of the day (excluding growth respiration)
        dGrowthPot = Pgross + Cremob - Resp_g - Resp_m;
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
        dGrowthW = dGrowthPot * Math.Pow(glfWater, waterStressFactor);

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
            gfnit = glfN;                           //legume no dilution, but reducing more DM (therefore LAI)
        else
            gfnit = Math.Pow(glfN, NdilutCoeff);    // more DM growth than N limited, due to dilution (typically NdilutCoeff = 0.5)

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

        if (dGrowth > 0.0)                  // if no net growth, then skip "partition" part
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
                throw new Exception("  AgPasture - Mass balance lost on partition of new growth");

            // Allocate the partitioned growth to the 1st tissue pools
            dmleaf1 += toLeaf * dGrowth;
            dmstem1 += toStem * dGrowth;
            dmstol1 += toStol * dGrowth;
            dmroot += toRoot * dGrowth;
            dGrowthHerbage = (toLeaf + toStem + toStol) * dGrowth;

            // Partitioing N, based on DM and [N] in each plant part
            double Nsum = (toLeaf * NcleafMax)
                        + (toStem * NcstemMax)
                        + (toStol * NcstolMax)
                        + (toRoot * NcrootMax);
            if (Nsum > 0.0)
            {
                double toLeafN = toLeaf * NcleafMax / Nsum;
                double toStemN = toStem * NcstemMax / Nsum;
                double toStolN = toStol * NcstolMax / Nsum;
                double toRootN = toRoot * NcrootMax / Nsum;

                // Allocate new N to the 1st tissue pools
                Nleaf1 += toLeafN * newGrowthN;
                Nstem1 += toStemN * newGrowthN;
                Nstol1 += toStolN * newGrowthN;
                Nroot += toRootN * newGrowthN;
            }

            // fraction of Nremob not used on growth that is added to dead tissue (remaining goes into litter)
            double leftoverNremob = Nremob * Kappa4;
            if (leftoverNremob > 0.0)
            {
                double DMsum = dmleaf4 + dmstem;
                Nleaf4 += leftoverNremob * MathUtility.Divide(dmleaf4, DMsum, 0.0);
                Nstem4 += leftoverNremob * MathUtility.Divide(dmstem4, DMsum, 0.0);
            }

            // check whether luxury N was remobilised during N balance
            if (NFastRemob2 + NFastRemob3 > 0.0)
            {
                // partition any used N into plant parts (by N content)
                Nsum = Nleaf2 + Nstem2 + Nstol2;
                if (NFastRemob2 > 0.0 && Nsum > 0.0)
                {
                    Nleaf2 -= NFastRemob2 * Nleaf2 / Nsum;
                    Nstem2 -= NFastRemob2 * Nstem2 / Nsum;
                    Nstol2 -= NFastRemob2 * Nstol2 / Nsum;
                }
                Nsum = Nleaf3 + Nstem3 + Nstol3;
                if (NFastRemob3 > 0.0 && Nsum > 0.0)
                {
                    Nleaf3 -= NFastRemob3 * Nleaf3 / Nsum;
                    Nstem3 -= NFastRemob3 * Nstem3 / Nsum;
                    Nstol3 -= NFastRemob3 * Nstol3 / Nsum;
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
        double gftt = GFTempTissue();

        // Get the moisture factor for tissue turnover
        double gfwt = GFWaterTissue();

        // Get the moisture factor for littering rate
        double gfwL = Math.Pow(glfWater, exponentGLFW2dead);

        // Consider the number of leaves
        double gftleaf = 3.0 / liveLeavesPerTiller;       // three refers to the number of stages used in the model

        // Leaf and stems turnover rate
        gama = refTissueTurnoverRate * gftt * gfwt * gftleaf;

        // Stolons turnover rate (legumes)
        if (isLegume)
            gamaS = refTurnoverRateStolon * gftt * gfwt * gftleaf;

        // Littering rate
        gamaD = refLitteringRate * gfwL * digestDead / 0.4;

        // Adjust littering rate due to stock trampling
        gamaD += stockParameter * stockingRate;

        // Roots turnover rate
        gamaR = gftt * (2 - glfWater) * rateRootSen;

        if (gama == 0.0)
        { //if gama ==0 due to gftt or gfwt, then skip "turnover" part
            dLitter = 0.0;
            dNLitter = 0.0;
            dRootSen = 0.0;
            dNrootSen = 0.0;
        }
        else
        {
            if (isAnnual)
            {  // Adjust turnover rate for annuals
                if (phenoStage == 1)
                { //vegetative
                    double Kv = (double)daysfromEmergence / daysEmgToAnth;
                    gama *= Kv;
                    gamaR *= Kv;
                }
                else if (phenoStage == 2)
                { //reproductive
                    double Kr = (double)daysfromAnthesis / daysToMature;
                    //gama = 1 - (1 - gama) * (1 - Kr * Kr);
                    gama *= 1 - (Kr * Kr);
                    gama += Kr * Kr;
                }
            }

            // Get daily defoliation factor
            double Fd = 0.0;
            if (prevState.dmdefoliated + prevState.dmshoot > 0.0)
                Fd = MathUtility.Divide(prevState.dmdefoliated, prevState.dmdefoliated + prevState.dmshoot, 0.0);

            // Increase stolon senescence if there was defoliation
            if (isLegume)
                gamaS += Fd * (1 - gamaS);

            // If today's turnover will result in a dmgreen < dmgreen_minimum, then adjust the rates,
            // Possibly will have to skip this for annuals to allow them to die - phenololgy-related?
            double dmgreenToBe = dmgreen + dGrowth - (gama * (prevState.dmleaf3 + prevState.dmstem3 + prevState.dmstol3));
            if (dmgreenToBe < dmgreenmin)
            {
                if (gama > 0.0)
                {
                    if (dmgreen + dGrowth < dmgreenmin)
                    {
                        gama = 0.0;
                        gamaS = 0.0;
                        //  gamad = 0.0;     -RCichota: Littering should not be affected
                        gamaR = 0.0;
                    }
                    else
                    {
                        double gama_adj = MathUtility.Divide(dmgreen + dGrowth - dmgreenmin,
                                                             prevState.dmleaf3 + prevState.dmstem3 + prevState.dmstol3, 0.0);
                        gamaR *= gama_adj / gama;
                        // gamaD *= gama_adj / gama;     -RCichota: Littering should not be affected
                        gama = gama_adj;
                    }
                }
            }

            // consider a minimum for roots too
            if (dmroot < 0.5 * dmgreenmin)
            {
                gamaR = 0.0;
            }

            //// Do the actual turnover, update DM and N

            // Leaves
            double DMfrom1to2 = facGrowingTissue * gama * prevState.dmleaf1;
            double DMfrom2to3 = gama * prevState.dmleaf2;
            double DMfrom3to4 = gama * prevState.dmleaf3;
            double DMfrom4toL = gamaD * prevState.dmleaf4;

            dmleaf1 += 0.0 - DMfrom1to2;  // DM in was considered in PartitionDMGrown()
            dmleaf2 += DMfrom1to2 - DMfrom2to3;
            dmleaf3 += DMfrom2to3 - DMfrom3to4;
            dmleaf4 += DMfrom3to4 - DMfrom4toL;
            dGrowthHerbage -= DMfrom4toL;
            dLitter = DMfrom4toL;

            double Nfrom1to2 = Ncleaf1 * DMfrom1to2;
            double Nfrom2to3 = Ncleaf2 * DMfrom2to3;
            double Nfrom3to4 = NcleafMin * DMfrom3to4;
            double Nfrom4toL = Ncleaf4 * DMfrom4toL;
            Nleaf3Remob = (Ncleaf3 - NcleafMin) * DMfrom3to4;

            Nleaf1 += 0.0 - Nfrom1to2;   // N in was considered in PartitionDMGrown()
            Nleaf2 += Nfrom1to2 - Nfrom2to3;
            Nleaf3 += Nfrom2to3 - Nfrom3to4 - Nleaf3Remob;
            Nleaf4 += Nfrom3to4 - Nfrom4toL;
            dNLitter = Nfrom4toL;

            if (dmleaf1 != 0)
            { Ncleaf1 = Nleaf1 / dmleaf1; }
            if (dmleaf2 != 0)
            { Ncleaf2 = Nleaf2 / dmleaf2; }
            if (dmleaf3 != 0)
            { Ncleaf3 = Nleaf3 / dmleaf3; }
            if (dmleaf4 != 0)
            { Ncleaf4 = Nleaf4 / dmleaf4; }

            // Stems
            DMfrom1to2 = facGrowingTissue * gama * prevState.dmstem1;
            DMfrom2to3 = gama * prevState.dmstem2;
            DMfrom3to4 = gama * prevState.dmstem3;
            DMfrom4toL = gamaD * prevState.dmstem4;

            dmstem1 += 0.0 - DMfrom1to2;    // DM in was considered in PartitionDMGrown()
            dmstem2 += DMfrom1to2 - DMfrom2to3;
            dmstem3 += DMfrom2to3 - DMfrom3to4;
            dmstem4 += DMfrom3to4 - DMfrom4toL;
            dGrowthHerbage -= DMfrom4toL;
            dLitter += DMfrom4toL;

            Nfrom1to2 = Ncstem1 * DMfrom1to2;
            Nfrom2to3 = Ncstem2 * DMfrom2to3;
            Nfrom3to4 = NcstemMin * DMfrom3to4;
            Nfrom4toL = Ncstem4 * DMfrom4toL;
            Nstem3Remob = (Ncstem3 - NcstemMin) * DMfrom3to4;

            Nstem1 += 0.0 - Nfrom1to2;    // N in was considered in PartitionDMGrown()
            Nstem2 += Nfrom1to2 - Nfrom2to3;
            Nstem3 += Nfrom2to3 - Nfrom3to4 - Nstem3Remob;
            Nstem4 += Nfrom3to4 - Nfrom4toL;
            dNLitter += Nfrom4toL;

            if (dmstem1 != 0)
            { Ncstem1 = Nstem1 / dmstem1; }
            if (dmstem2 != 0)
            { Ncstem2 = Nstem2 / dmstem2; }
            if (dmstem3 != 0)
            { Ncstem3 = Nstem3 / dmstem3; }
            if (dmstem4 != 0)
            { Ncstem4 = Nstem4 / dmstem4; }

            // Stolons
            if (isLegume)
            {
                DMfrom1to2 = facGrowingTissue * gamaS * prevState.dmstol1;
                DMfrom2to3 = gamaS * prevState.dmstol2;
                double DMfrom3toL = gamaS * prevState.dmstol3;

                dmstol1 += 0.0 - DMfrom1to2;      // DM in was considered in PartitionDMGrown()
                dmstol2 += DMfrom1to2 - DMfrom2to3;
                dmstol3 += DMfrom2to3 - DMfrom3toL;
                dGrowthHerbage -= DMfrom3toL;
                dLitter += DMfrom3toL;

                Nfrom1to2 = Ncstol1 * DMfrom1to2;
                Nfrom2to3 = Ncstol2 * DMfrom2to3;
                Nfrom3to4 = 0.5 * (Ncstol3 + NcstolMin) * DMfrom3toL;
                Nstol3Remob = 0.5 * (Ncstol3 - NcstolMin) * DMfrom3toL;

                Nstol1 += 0.0 - Nfrom1to2;    // N in was considered in PartitionDMGrown()
                Nstol2 += Nfrom1to2 - Nfrom2to3;
                Nstol3 += Nfrom2to3 - Nfrom3to4 - Nstol3Remob;
                dNLitter += Nfrom3to4;

                if (dmstol1 != 0)
                { Ncstol1 = Nstol1 / dmstol1; }
                if (dmstol2 != 0)
                { Ncstol2 = Nstol2 / dmstol2; }
                if (dmstol3 != 0)
                { Ncstol3 = Nstol3 / dmstol3; }
            }

            // Roots
            dRootSen = gamaR * prevState.dmroot;
            dmroot -= dRootSen;
            NrootRemob = 0.5 * (Ncroot - NcrootMin) * dRootSen;
            dNrootSen = Ncroot * dRootSen - NrootRemob;
            Nroot -= Ncroot * dRootSen;
            if (dmroot != 0)
            { Ncroot = Nroot / dmroot; }

            // fraction of Nremob not used in growth that is added to litter
            double leftoverNremob = Nremob * (1 - Kappa4);
            dNLitter += leftoverNremob;
            //The leftover 'Nremob' of previous day (if>0) indicates more N should go to litter in previous day, so do it now

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
            Cremob = 0.0;  // not explicitly considered
        }
    }

    /// <summary>
    /// Update aggregated variables
    /// </summary>
    internal void UpdateAggregatedVariables()   //update DM, N
    {
        //// - DM  -------------------------------------------------------
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

        //// - N  --------------------------------------------------------
        Nleaf = Nleaf1 + Nleaf2 + Nleaf3 + Nleaf4;
        Nstem = Nstem1 + Nstem2 + Nstem3 + Nstem4;
        Nstolon = Nstol1 + Nstol2 + Nstol3;

        Nshoot = Nleaf + Nstem + Nstolon;

        Ngreen = Nleaf1 + Nleaf2 + Nleaf3
               + Nstem1 + Nstem2 + Nstem3
               + Nstol1 + Nstol2 + Nstol3;
        Ndead = Nleaf4 + Nstem4;

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
            // New growth
            rootFraction = RootGrowthDistribution(dGrowth);
            // After senescence
            rootFraction = RootGrowthDistribution(-dRootSen);
        }
    }

    /// <summary>
    /// Compute the LAI value of green material
    /// </summary>
    /// <returns>green LAI</returns>
    internal double GreenLAI()
    {
        double myLAI = (0.0001 * dmleaf_green * specificLeafArea)
                     + (0.0001 * dmstol * 0.3 * specificLeafArea);
        // 0.0001: kg/ha->kg/m2; SLA: m2/kg - assuming Mass2GLA = 0.3*SLA
        // Resilience after unfavoured conditions
        // Consider cover will be bigger for the same amount of DM when DM is low due to
        // - light extinction coefficient will be bigger - plant leaves will be more horizontal than in dense high swards
        // - more parts will turn green for photosysnthesis (?)
        // - quick response of plant shoots to favoured conditions after release of stress
        // » Specific leaf area should be reduced (RCichota2014)
        if (!isLegume && dmgreen < 1000)
        {
            myLAI += 0.0001 * dmstem_green * specificLeafArea * Math.Sqrt((1000 - dmgreen) / 1000);
        }

        return myLAI;
    }

    /// <summary>
    /// Compute the LAI value of dead material
    /// </summary>
    /// <returns>dead LAI</returns>
    internal double DeadLAI()
    {
        double myLAI = 0.0001 * dmleaf4 * specificLeafArea;
        return myLAI;
    }

    /// <summary>
    /// Store state of various pools
    /// </summary>
    internal void SetPrevPools()
    {
        prevState.dmleaf1 = dmleaf1;
        prevState.dmleaf2 = dmleaf2;
        prevState.dmleaf3 = dmleaf3;
        prevState.dmleaf4 = dmleaf4;
        prevState.dmstem1 = dmstem1;
        prevState.dmstem2 = dmstem2;
        prevState.dmstem3 = dmstem3;
        prevState.dmstem4 = dmstem4;
        prevState.dmstol1 = dmstol1;
        prevState.dmstol2 = dmstol2;
        prevState.dmstol3 = dmstol3;
        prevState.dmroot = dmroot;
        prevState.dmleaf_green = dmleaf_green;
        prevState.dmstem_green = dmstem_green;
        prevState.dmstol_green = dmstol_green;
        prevState.dmleaf = dmleaf;
        prevState.dmstem = dmstem;
        prevState.dmstol = dmstol;
        prevState.dmshoot = dmshoot;
        prevState.dmgreen = dmgreen;
        prevState.dmdead = dmdead;

        // RCichota May 2014: moved pS.dmdefoliated to be stored at the time of a removal (it is zeroed at the end of process)
    }

    #endregion

    #region Functions  ----------------------------------------------------------------------------

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
        double Gmin = 0.2;      //Fc = Gmin when CO2->unlimited
        double Gmax = 1.25;     //Fc = Gmax when CO2 = 0;
        double beta = 2.5;      //curvature factor,

        double Fc = Gmin + (Gmax - Gmin) * (1 - Gmin) * Math.Pow(referenceCO2, beta) /
        ((Gmax - 1) * Math.Pow(CO2, beta) + (1 - Gmin) * Math.Pow(referenceCO2, beta));
        return Fc;
    }

    /// <summary>
    /// Species N demand for potential growth (soilNdemand)
    /// </summary>
    /// <returns>Amount of N demanded</returns>
    internal double CalcNdemand()
    {
        fShoot = NewGrowthToShoot();
        double fL = UpdatefLeaf(); //to consider more dm to leaf when DM is lower?
        fLeaf = maxFLeaf;

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
        //Ndemand *= NCO2Effects();       //luxury uptake not reduced

        //even with sufficient soil N available
        if (isLegume)
            Nfix = MinFix * NdemandOpt;
        else
            Nfix = 0.0;

        return Nfix;
    }

    /// <summary>
    /// New fraction to leaves
    /// </summary>
    /// <returns>fLeaf</returns>
    private double UpdatefLeaf()
    {
        double fL = maxFLeaf;   //fraction of shoot that goes to leaf

        if ((minFLeaf < maxFLeaf) && (dmgreen > dmMaxFLeaf))
        {
            double dmAux = Math.Pow((dmgreen - dmMaxFLeaf) / (dmReferenceFLeaf - dmMaxFLeaf), exponentFLeaf);
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

        double Nleaf_green = 0.0;
        double effect = 1.0;
        if (!isAnnual)  //  &&and FVegPhase and ( VegDay < 10 ) ) then  // need this or it never gets going
        {
            Nleaf_green = Nleaf1 + Nleaf2 + Nleaf3;
            if (dmleaf_green > 0.0)
            {
                double Ncleaf_green = Nleaf_green / dmleaf_green;
                if (Ncleaf_green < NcleafOpt * Fn)     //Fn
                {
                    if (Ncleaf_green > NcleafMin)
                    {
                        //effect = Math.Min(1.0, Ncleaf_green / NcleafOpt*Fn);
                        effect = Math.Min(1.0, MathUtility.Divide(Ncleaf_green - NcleafMin, NcleafOpt * Fn - NcleafMin, 0.0));
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
    /// Cost of N fixation (not implemented)
    /// </summary>
    /// <returns>Cost</returns>
    private double NFixCost()
    {
        double costF = 1.0;    //  reduction of net production as cost of N-fixing
        if (!isLegume || Nfix == 0 || NdemandLux == 0)      //  happens when plant has no growth
        { return costF; }

        double actFix = Nfix / NdemandLux;
        costF = 1 - 0.24 * (actFix - MinFix) / (MaxFix - MinFix);
        if (costF < 0.76)
            costF = 0.76;
        return costF;
    }

    /// <summary>
    /// Calcualte the fraction of new growth allocated to shoot
    /// </summary>
    /// <returns>fShoot</returns>
    private double NewGrowthToShoot()
    {
        // shoot/root ratio for today's DM partition
        double todaysSR = targetSRratio;

        if (prevState.dmroot > 0.00001)
        {
            double fac = 1.0;                   //day-to-day fraction of reduction
            //double minF = allocationSeasonF;    //default = 0.8;
            double doy = Clock.day_of_month + (int)((Clock.month - 1) * 30.5);
            // NOTE: the type for doy has to be double or the divisions below will be rounded (to int) and thus be [slightly] wrong

            double doyC = startHighAllocation;             // Default as in South-hemisphere: 232
            int doyEoY = 365 + (DateTime.IsLeapYear(Clock.year) ? 1 : 0);
            int[] ReproSeasonIntval = new int[3]; // { 35, 60, 30 };
            double allocationIncrease = allocationSeasonF;
            ReproSeasonIntval[0] = (int)(durationHighAllocation * shoulderHighAllocation * 1.17);
            ReproSeasonIntval[1] = (int)durationHighAllocation;
            ReproSeasonIntval[2] = (int)(durationHighAllocation * shoulderHighAllocation);

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
                    doyIniPlateau += 183 * (paramALatFunction - (paramALatFunction * myB) + myB) * Math.Pow(myB, paramALatFunction - 1.0);
                }

                // compute the duration of the three phases (onset, plateau, and outset)
                double maxPlateauPeriod = doyEoY - 2 * maxShoulderLatFunction;
                ReproSeasonIntval[1] = (int)(minPlateauLatFunction + (maxPlateauPeriod - minPlateauLatFunction) * Math.Pow(1 - Math.Abs(MetFile.Latitude) / 90, paramBLatFunction));
                ReproSeasonIntval[0] = (int)Math.Min(maxShoulderLatFunction, ReproSeasonIntval[1] * onsetFacLatFunction);
                ReproSeasonIntval[2] = (int)Math.Min(maxShoulderLatFunction, ReproSeasonIntval[1] * outsetFacLatFunction);
                if (ReproSeasonIntval.Sum() > doyEoY)
                    throw new Exception("Error when calculating period with high DM allocation, greater then one year");

                doyC = doyIniPlateau - ReproSeasonIntval[0];
                // compute the factor to augment allocation
                allocationIncrease = allocationMax;
                if (Math.Abs(MetFile.Latitude) < referenceLatitude)
                {
                    double myB = Math.Abs(MetFile.Latitude) / referenceLatitude;
                    allocationIncrease *= (paramCLatFunction - (paramCLatFunction * myB) + myB) * Math.Pow(myB, paramCLatFunction - 1.0);
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

            // update todays shoot/root partition
            todaysSR = fac * targetSRratio;

            // get the soil related growth limiting factor (the smaller this is the higher the allocation of DM to roots)
            double GFmin = Math.Min(glfWater, glfN);

            // get the current shoot/root ratio (the smaller this is the higher the allocation of DM to shoot)
            double presentSR = MathUtility.Divide(dmgreen, prevState.dmroot, 1000.0);

            // update todays shoot/root partition
            todaysSR *= GFmin * todaysSR / presentSR;

            // compute fraction to shoot
            fShoot = todaysSR / (1.0 + todaysSR);
        }
        else
        {
            fShoot = 1.0;  // this should not happen (might happen if plant is dead)
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
        double gft3 = 0.0;
        double growthTmax = growthTopt + (growthTopt - growthTmin) / growthTq;
        if (T > growthTmin && T < growthTmax)
        {
            double val1 = Math.Pow((T - growthTmin), growthTq) * (growthTmax - T);
            double val2 = Math.Pow((growthTref - growthTmin), growthTq) * (growthTmax - growthTref);
            gft3 = val1 / val2;

            if (gft3 < 0.0) gft3 = 0.0;
            //if (gft3 > 1.0) gft3 = 1.0;
        }
        return gft3;
    }

    /// <summary>
    /// Temperature effects on photosynthesis for C4 plants
    /// </summary>
    /// <param name="T">Temperature</param>
    /// <returns>GLFTemp</returns>
    public double GFTempC4(double T)
    {
        double gft4 = 0.0;          // Assign value 0 for the case of T < Tmin

        if (T > growthTmin)         // same as GFTempC3 for [Tmin,Topt], but T as Topt if T > Topt
        {
            if (T > growthTopt)
                T = growthTopt;

            double Tmax = growthTopt + (growthTopt - growthTmin) / growthTq;
            double val1 = Math.Pow((T - growthTmin), growthTq) * (Tmax - T);
            double val2 = Math.Pow((growthTref - growthTmin), growthTq) * (Tmax - growthTref);
            gft4 = val1 / val2;

            if (gft4 < 0.0) gft4 = 0.0;
            //if (gft4 > 1.0) gft4 = 1.0;
        }
        return gft4;
    }

    /// <summary>
    /// Heat effects on photosynthesis
    /// </summary>
    /// <returns>Heat effect</returns>
    private double HeatEffect()
    {
        if (usingHeatStress)
        {
            // check heat stress factor
            if (MetFile.MaxT > heatFullT)
            {
                heatFactor = 0.0;
                accumTHeat = 0.0;
            }
            else if (MetFile.MaxT > heatOnsetT)
            {
                heatFactor = highTempStress * (heatFullT - MetFile.MaxT) / (heatFullT - heatOnsetT);
                accumTHeat = 0.0;
            }

            // check recovery factor
            double recoveryFactor = 0.0;
            if (MetFile.MaxT <= heatOnsetT)
                recoveryFactor = (1 - heatFactor) * Math.Pow(accumTHeat / heatSumT, heatTq);

            // accumulate temperature
            double meanT = 0.5 * (MetFile.MaxT + MetFile.MinT);
            accumTHeat += Math.Max(0.0, heatRecoverT - meanT);
            //// TODO: move this to the beginning, so todays temperature is taken into account (faster recovery)

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
            // check cold stress factor
            if (MetFile.MinT < coldFullT)
            {
                coldFactor = 0.0;
                accumTCold = 0.0;
            }
            else if (MetFile.MinT < coldOnsetT)
            {
                coldFactor = lowTempStress * (MetFile.MinT - coldFullT) / (coldOnsetT - coldFullT);
                accumTCold = 0.0;
            }

            // check recovery factor
            double recoveryFactor = 0.0;
            if (MetFile.MinT >= coldOnsetT)
                recoveryFactor = (1 - coldFactor) * Math.Pow(accumTCold / coldSumT, coldTq);

            // accumulate temperature
            double meanT = 0.5 * (MetFile.MaxT + MetFile.MinT);
            accumTCold += Math.Max(0.0, meanT - coldRecoverT);

            // cold stress
            lowTempStress = Math.Min(1.0, coldFactor + recoveryFactor);

            return lowTempStress;
        }
        else
            return 1.0;
    }

    /// <summary>
    /// Effect of water stress on tissue turnover rate
    /// </summary>
    /// <returns>Moisture effect</returns>
    private double GFWaterTissue()
    {
        double gfwt = 1.0;

        if (glfWater < massFluxWopt)
            gfwt = 1 + (massFluxW0 - 1.0) * ((massFluxWopt - glfWater) / massFluxWopt);

        if (gfwt < 1.0) gfwt = 1.0;
        if (gfwt > massFluxW0) gfwt = massFluxW0;
        return gfwt;
    }

    /// <summary>
    ///  Effect of temperature on tissue turnover rate
    /// </summary>
    /// <returns>Temp effect</returns>
    private double GFTempTissue()
    {
        double T = (MetFile.MaxT + MetFile.MinT) / 2;

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

    /// <summary>
    /// Remove plant DM
    /// </summary>
    /// <param name="AmountToRemove">The DM amount to remove</param>
    /// <param name="PrefGreen">Preference level for green tissue</param>
    /// <param name="PrefDead">Preference level for dead tissue</param>
    /// <returns>Amount removed</returns>
    internal double RemoveDM(double AmountToRemove, double PrefGreen, double PrefDead)
    {
        // check existing amount and what is harvestable
        double PreRemovalDM = dmshoot;
        double PreRemovalN = Nshoot;
        double AmountRemovable = Math.Max(0.0, dmleaf_green + dmstem_green - dmgreenmin)
                               + Math.Max(0.0, dmleaf4 + dmstem4 - dmdeadmin);

        // get the weights for each pool, consider preference and available DM
        double FractionNotRemoved = 0.0;
        if (AmountRemovable > 0.0)
            FractionNotRemoved = Math.Max(0.0, (AmountRemovable - AmountToRemove) / AmountRemovable);

        double TempPrefGreen = PrefGreen + (PrefDead * (1 - FractionNotRemoved));
        double TempPrefDead = PrefDead + (PrefGreen * (1 - FractionNotRemoved));
        double TempRemovableGreen = Math.Max(0.0, dmleaf_green + dmstem_green - dmgreenmin);
        double TempRemovableDead = Math.Max(0.0, dmleaf4 + dmstem4 - dmdeadmin);

        // get partiton between dead and live materials
        double TempTotal = TempRemovableGreen * TempPrefGreen
                         + TempRemovableDead * TempPrefDead;
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
        digestDefoliated = calcDigestibility();

        // update the various pools
        dmleaf1 *= FractionRemainingGreen;
        dmleaf2 *= FractionRemainingGreen;
        dmleaf3 *= FractionRemainingGreen;
        dmleaf4 *= FractionRemainingDead;
        dmstem1 *= FractionRemainingGreen;
        dmstem2 *= FractionRemainingGreen;
        dmstem3 *= FractionRemainingGreen;
        dmstem4 *= FractionRemainingDead;
        //No stolon removed

        // N remove
        Nleaf1 *= FractionRemainingGreen;
        Nleaf2 *= FractionRemainingGreen;
        Nleaf3 *= FractionRemainingGreen;
        Nleaf4 *= FractionRemainingDead;
        Nstem1 *= FractionRemainingGreen;
        Nstem2 *= FractionRemainingGreen;
        Nstem3 *= FractionRemainingGreen;
        Nstem4 *= FractionRemainingDead;

        //Nremob is also removed proportionally (not sensitive?)
        double PreRemovalNRemob = Nremob;
        Nremob *= FractionRemainingGreen;

        // update Luxury N pools
        NLuxury2 *= FractionRemainingGreen;
        NLuxury3 *= FractionRemainingGreen;

        // Update the variables with aggregated data and plant parts (dmshoot, LAI, etc)
        UpdateAggregatedVariables();

        // check balance and set outputs
        double NremobRemove = PreRemovalNRemob - Nremob;
        dmdefoliated = PreRemovalDM - dmshoot;
        prevState.dmdefoliated = dmdefoliated;
        Ndefoliated = PreRemovalN - Nshoot;
        if (Math.Abs(dmdefoliated - AmountToRemove) > 0.00001)
            throw new Exception("  AgPasture - removal of DM resulted in loss of mass balance");

        return Ndefoliated;
    }

    /// <summary>
    /// Calculate the average plant digestibility (above ground)
    /// </summary>
    /// <returns>digestibility</returns>
    internal double calcDigestibility()
    {
        if ((dmleaf + dmstem) <= 0.0)
        {
            digestHerbage = 0.0;
            return digestHerbage;
        }

        double CNp = 3.5;                           //CN ratio of protein
        double CNw = 100;                           //CN ratio of cell wall

        //Live
        double digestibilityLive = 0.0;
        if (dmgreen > 0.0 & Ngreen > 0.0)
        {
            double fSugar = 0.5 * dGrowth / dmgreen;    //dmgreen: live shoots including leaves/stems/stolons
            double CNlive = 0.4 * dmgreen / Ngreen;                                //CN ratio of live shoots
            double fProteinLive = (CNw / CNlive - (1 - fSugar)) / (CNw / CNp - 1); //Fraction of protein in liveing shoots
            double fWallLive = 1 - fSugar - fProteinLive;                          //Fraction of cell wall in living shoots
            digestibilityLive = fSugar + fProteinLive + digestLive * fWallLive;
        }

        //Dead
        double digestibilityDead = 0.0;
        double standingDead = dmleaf4 + dmstem4;        //Not including stolons here for stolons are not grazed
        if (standingDead > 0.0 && Ndead > 0.0)
        {
            double CNdead = 0.4 * dmdead / Ndead;                       //CN ratio of standing dead;
            double fProteinDead = (CNw / CNdead - 1) / (CNw / CNp - 1); //Fraction of protein in standing dead
            double fWallDead = 1 - fProteinDead;                        //Fraction of cell wall in standing dead
            digestibilityDead = fProteinDead + digestDead * fWallDead;
        }

        double deadFrac = MathUtility.Divide(standingDead, dmleaf + dmstem, 0.0);
        digestHerbage = (1 - deadFrac) * digestibilityLive + deadFrac * digestibilityDead;

        return digestHerbage;
    }

    /// <summary>
    /// Find the layer at the bottom of the root zone
    /// </summary>
    /// <returns>layer at bottom of root zone</returns>
    internal int RootZoneBottomLayer()
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

    /// <summary>
    /// Calculate the plant height, as function of DM
    /// </summary>
    /// <returns>Plant height</returns>
    internal double HeightfromDM()
    {
        double TodaysHeight = MaxPlantHeight;
        double standingDM = (dmleaf + dmstem);

        if (standingDM <= MassForMaxHeight)
        {
            double massRatio = standingDM / MassForMaxHeight;
            double heightF = ExponentHeightFromMass
                           - (ExponentHeightFromMass * massRatio)
                           + massRatio;
            heightF *= Math.Pow(massRatio, ExponentHeightFromMass - 1);
            TodaysHeight *= heightF;
        }

        return Math.Max(TodaysHeight, MinimumHeight);
    }

    #endregion
}

////-----------------------------------------------------------------------------------------------
/// <summary>
/// Stores the values of pool status of previous day
/// </summary>
public class SpeciesState
{
    /// <summary>DM of leaf at stage 1</summary>
    public double dmleaf1;
    /// <summary>DM of leaf at stage 2</summary>
    public double dmleaf2;
    /// <summary>DM of leaf at stage 3</summary>
    public double dmleaf3;
    /// <summary>DM of leaf at stage 4</summary>
    public double dmleaf4;
    /// <summary>DM of stem at stage 1</summary>
    public double dmstem1;
    /// <summary>DM of stem at stage 2</summary>
    public double dmstem2;
    /// <summary>DM of stem at stage 3</summary>
    public double dmstem3;
    /// <summary>DM of stem at stage 4</summary>
    public double dmstem4;
    /// <summary>DM of stolon at stage 1</summary>
    public double dmstol1;
    /// <summary>DM of stolon at stage 2</summary>
    public double dmstol2;
    /// <summary>DM of stolon at stage 3</summary>
    public double dmstol3;
    /// <summary>DM of roots</summary>
    public double dmroot;

    /// <summary>DM of leaves</summary>
    public double dmleaf;
    /// <summary>DM of stems and sheath</summary>
    public double dmstem;
    /// <summary>DM of stolons</summary>
    public double dmstol;
    /// <summary>DM of green leaves</summary>
    public double dmleaf_green;
    /// <summary>DM of green stems</summary>
    public double dmstem_green;
    /// <summary>DM of green stolons</summary>
    public double dmstol_green;
    /// <summary>DM above ground</summary>
    public double dmshoot;
    /// <summary>DM green above ground</summary>
    public double dmgreen;
    /// <summary>DM dead above ground</summary>
    public double dmdead;
    /// <summary>Total plant DM</summary>
    public double dmtotal;
    /// <summary>DM defoliated</summary>
    public double dmdefoliated;
    /// <summary>N remobilsed from senesced tissue</summary>
    public double Nremob;

    /// <summary>The constructor</summary>
    public SpeciesState() { }
}

////-----------------------------------------------------------------------------------------------
/// <summary>
/// Basic values defining species state
/// </summary>
public class SpeciesStateSettings
{
    /// <summary>DM of shoot</summary>
    public double ShootDM = 0.0;

    /// <summary>DM of roots</summary>
    public double RootDM = 0.0;

    /// <summary>Depth of roots</summary>
    public double RootDepth = 0.0;

    /// <summary> Fractions of DM for each biomass pool </summary>
    public double[] DMFraction;

    /// <summary> Concentration of N for each biomass pool, including roots </summary>
    public double[] NConcentration;

    /// <summary> Constructor, initialise some variables </summary>
    public SpeciesStateSettings()
    {
        Array.Resize(ref DMFraction, 11);
        Array.Resize(ref NConcentration, 12);
    }
}