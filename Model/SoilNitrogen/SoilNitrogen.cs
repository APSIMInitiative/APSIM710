using System;
using System.Reflection;
using System.Collections.Generic;
using System.Xml.Serialization;
using System.Xml;
using System.Linq;
using ModelFramework;
using CSGeneral;

/// <summary>
/// Computes the soil C and N processes
/// </summary>
/// <remarks>
/// A more-or-less direct port of the Fortran SoilN model  -  Ported by Eric Zurcher Sept/Oct 2010
/// Code tidied up by RCichota on Aug/Sep-2012 (updates in Feb/2014): 
///     - modified how some variables are handled (substitute 'get's by [input]s or [link]s), 
///     - added some of the parameters/constants to the xml (so they can be changed/tested)
///     - added/updated some output variables (set some as properties), with correspondent reduction of internal variables
///     - moved the variables to a partial class (.Variables) and added 'region's to ease access/readability
///     - updated error checks and messages
///     - moved all soilTemp code to a separate class (the idea is to eliminate it in the future)
/// </remarks>
public partial class SoilNitrogen
{
    #region >>  Events which we publish

    /// <summary>
    /// Event to communicate other modules of C and/or N changes to/from outside the simulation
    /// </summary>
    [Event]
    public event ExternalMassFlowDelegate ExternalMassFlow;

    /// <summary>
    /// Event to communicate other modules that solutes have been added to the simulation (owned by SoilNitrogen)
    /// </summary>
    [Event]
    public event NewSoluteDelegate NewSolute;

    /// <summary>
    /// Event to comunicate other modules (SurfaceOM) that residues have been decomposed
    /// </summary>
    [Event]
    public event SurfaceOrganicMatterDecompDelegate actualResidueDecompositionCalculated;

    #endregion events published

    #region >>  Setup events handlers and methods

    /// <summary>
    /// Sets the commands for the initial setup
    /// </summary>
    [EventHandler]
    public void OnInitialised()
    {
        // Variable handling when using APSIMX
#if (APSIMX == true)
        initDone = false;
        ApsimFile.Soil Soil = (ApsimFile.Soil)Paddock.Get("Soil");
        dlayer = Soil.Thickness;
        bd = Soil.Water.BD;
        sat_dep = MathUtility.Multiply(Soil.Water.SAT, Soil.Thickness);
        dul_dep = MathUtility.Multiply(Soil.Water.DUL, Soil.Thickness);
        ll15_dep = MathUtility.Multiply(Soil.Water.LL15, Soil.Thickness);
        sw_dep = MathUtility.Multiply(Soil.SW, Soil.Thickness);
        oc = Soil.OC;
        ph = Soil.Analysis.PH;
        salb = Soil.SoilWater.Salb;
        no3ppm = Soil.NO3;
        nh4ppm = Soil.NH4;

        fbiom = Soil.SoilOrganicMatter.FBiom;
        finert = Soil.SoilOrganicMatter.FInert;
        soil_cn = Soil.SoilOrganicMatter.SoilCN;
        root_wt = Soil.SoilOrganicMatter.RootWt;
        root_cn = Soil.SoilOrganicMatter.RootCN;
        enr_a_coeff = Soil.SoilOrganicMatter.EnrACoeff;
        enr_b_coeff = Soil.SoilOrganicMatter.EnrBCoeff;
        Clock.Tick += new TimeDelegate(OnTick);
        Clock.Process += new NullTypeDelegate(OnProcess);
        //Clock.Post += new NullTypeDelegate(OnPost);

        initDone = true;
#endif

        // set the size of arrays
        ResizeLayeredVariables(dlayer.Length);

        // check few initialisation parameters
        CheckParameters();

        // check the initial values of some basic variables
        CheckInitialVariables();

        // set the variables up with their the initial values
        SetInitialValues();

        // initialise soil temperature
        if (usingSimpleSoilTemp)
        {
            simpleST = new simpleSoilTemp(MetFile.Latitude, MetFile.tav, MetFile.amp, MetFile.MinT, MetFile.MaxT);
            Tsoil = simpleST.SoilTemperature(Clock.Today, MetFile.MinT, MetFile.MaxT, MetFile.Radn, salb, dlayer, bd, ll15_dep, sw_dep);
        }

        // notifify apsim about solutes
        AdvertiseMySolutes();

        // print SoilN report
        WriteSummaryReport();
    }

    /// <summary>
    /// Sets the commands for reseting the module to the initial setup
    /// </summary>
    [EventHandler(EventName = "reset")]
    public void OnReset()
    {
        isResetting = true;

        // Save the present C and N status
        StoreStatus();

        // reset the size of arrays
        ResizeLayeredVariables(dlayer.Length);

        // reset C and N variables to their initial state
        oc = reset_oc;
        no3ppm = reset_no3ppm;
        nh4ppm = reset_nh4ppm;
        ureappm = reset_ureappm;

        // perform initialisation and setup
        SetInitialValues();

        // reset soil temperature
        if (usingSimpleSoilTemp)
        {
            simpleST = new simpleSoilTemp(MetFile.Latitude, MetFile.tav, MetFile.amp, MetFile.MinT, MetFile.MaxT);
            Tsoil = simpleST.SoilTemperature(Clock.Today, MetFile.MinT, MetFile.MaxT, MetFile.Radn, salb, dlayer, bd, ll15_dep, sw_dep);
        }

        // get the changes of state and publish (let other component to know)
        SendDeltaState();

        // print SoilN report
        WriteSummaryReport();

        isResetting = false;
    }

    /// <summary>
    /// Checks general initialisation parameters, and let user know of some settings
    /// </summary>
    private void CheckParameters()
    {
        Console.WriteLine();
        Console.WriteLine("        - Reading/checking parameters");

        SoilNParameterSet = SoilNParameterSet.Trim();
        Console.WriteLine("          - Using " + SoilNParameterSet + " soil mineralisation specification");

        // check whether soil temperature is present. If not, check whether the basic params for simpleSoilTemp have been supplied
        if (SimpleSoilTempAllowed)
            usingSimpleSoilTemp = (ave_soil_temp == null);

        if (usingSimpleSoilTemp)
        {
            if (MetFile.Latitude == null)
                throw new Exception("Value for latitude was not supplied");
            if (MetFile.tav == null)
                throw new Exception("Value for TAV was not supplied");
            if (MetFile.amp == null)
                throw new Exception("Value for AMP was not supplied");
        }

        int nLayers = dlayer.Length;

        // check whether ph is supplied, use a default if not - might be better to throw an exception?
        usingSimpleSoilpH = (ph == null);
        if (usingSimpleSoilpH)
        {
            ph = new double[nLayers];
            for (int layer = 0; layer < nLayers; ++layer)
                ph[layer] = defaultInipH;
            Console.WriteLine("          - Soil pH was not supplied, the value " + defaultInipH.ToString("0.00") + " will be used for all layers");
        }

        // Check if all fom values have been supplied
        if (fract_carb.Length != fom_types.Length)
            throw new Exception("Number of \"fract_carb\" different to \"fom_type\"");
        if (fract_cell.Length != fom_types.Length)
            throw new Exception("Number of \"fract_cell\" different to \"fom_type\"");
        if (fract_lign.Length != fom_types.Length)
            throw new Exception("Number of \"fract_lign\" different to \"fom_type\"");

        // Check if all C:N values have been supplied. If not use average C:N ratio in all pools
        if (fomPoolsCNratio == null || fomPoolsCNratio.Length < 3)
        {
            fomPoolsCNratio = new double[3];
            for (int i = 0; i < 3; i++)
                fomPoolsCNratio[i] = iniFOM_CNr;
        }

        // Check if initial fom depth has been supplied, if not assume that initial fom is distributed over the whole profile
        if (iniFOM_depth <= epsilon)
            iniFOM_depth = SumDoubleArray(dlayer);

        // store the inital layer structure
        // Calculate conversion factor from kg/ha to ppm (mg/kg)
        // Convert minimum values for urea, nh4 and no3 from ppm to kg/ha
        reset_dlayer = new double[nLayers];
        convFactor = new double[nLayers];
        urea_min = new double[nLayers];
        nh4_min = new double[nLayers];
        no3_min = new double[nLayers];
        for (int layer = 0; layer < nLayers; ++layer)
        {
            reset_dlayer[layer] = dlayer[layer];
            convFactor[layer] = MathUtility.Divide(100.0, bd[layer] * dlayer[layer], 0.0);
            urea_min[layer] = MathUtility.Divide(ureappm_min, convFactor[layer], 0.0);
            nh4_min[layer] = MathUtility.Divide(nh4ppm_min, convFactor[layer], 0.0);
            no3_min[layer] = MathUtility.Divide(no3ppm_min, convFactor[layer], 0.0);
        }
    }

    /// <summary>
    /// Checks whether initial values for OM and mineral N were given and make sure all layers have valid values
    /// </summary>
    /// <remarks>
    /// Initial OC values are mandatory, but not for all layers. Zero is assumed for layers not set.
    /// Initial values for mineral N are optional, assume zero if not given
    /// The inital FOM values are given as a total amount which is distributed using an exponential function.
    /// In this procedure the fraction of total FOM that goes in each layer is also computed
    /// </remarks>
    private void CheckInitialVariables()
    {
        int nLayers = dlayer.Length;

        // ensure that array for initial OC have a value for each layer
        if (reset_oc.Length < nLayers)
            Console.WriteLine("           + Values supplied for the initial OC content do not cover all layers - zero will be assumed");
        else if (reset_oc.Length > nLayers)
            Console.WriteLine("           + More values were supplied for the initial OC content than the number of layers - excess will ignored");
        Array.Resize(ref reset_oc, nLayers);

        // ensure that array for initial urea content have a value for each layer
        if (reset_ureappm == null)
            Console.WriteLine("           + No values were supplied for the initial content of urea - zero will be assumed");
        else if (reset_ureappm.Length < nLayers)
            Console.WriteLine("            + Values supplied for the initial content of urea do not cover all layers - zero will be assumed");
        else if (reset_ureappm.Length > nLayers)
            Console.WriteLine("           + More values were supplied for the initial content of urea than the number of layers - excess will ignored");
        Array.Resize(ref reset_ureappm, nLayers);

        // ensure that array for initial content of NH4 have a value for each layer
        if (reset_nh4ppm == null)
            Console.WriteLine("           + No values were supplied for the initial content of nh4 - zero will be assumed");
        else if (reset_nh4ppm.Length < nLayers)
            Console.WriteLine("            + Values supplied for the initial content of nh4 do not cover all layers - zero will be assumed");
        else if (reset_nh4ppm.Length > nLayers)
            Console.WriteLine("           + More values were supplied for the initial content of nh4 than the number of layers - excess will ignored");
        Array.Resize(ref reset_nh4ppm, nLayers);

        // ensure that array for initial content of NO3 have a value for each layer
        if (reset_no3ppm == null)
            Console.WriteLine("           + No values were supplied for the initial content of no3 - zero will be assumed");
        else if (reset_no3ppm.Length < nLayers)
            Console.WriteLine("           + Values supplied for the initial content of no3 do not cover all layers - zero will be assumed");
        else if (reset_no3ppm.Length > nLayers)
            Console.WriteLine("           + More values were supplied for the initial content of no3 than the number of layers - excess will ignored");
        Array.Resize(ref reset_no3ppm, nLayers);

        // compute initial FOM distribution in the soil (FOM fraction)
        FOMiniFraction = new double[nLayers];
        double totFOMfraction = 0.0;
        int deepestLayer = getCumulativeIndex(iniFOM_depth, dlayer);
        double cumDepth = 0.0;
        double FracLayer = 0.0;
        for (int layer = 0; layer <= deepestLayer; layer++)
        {
            FracLayer = Math.Min(1.0, MathUtility.Divide(iniFOM_depth - cumDepth, dlayer[layer], 0.0));
            cumDepth += dlayer[layer];
            FOMiniFraction[layer] = FracLayer * Math.Exp(-iniFOM_coefficient * Math.Min(1.0, MathUtility.Divide(cumDepth, iniFOM_depth, 0.0)));
        }

        // distribute FOM through layers
        totFOMfraction = SumDoubleArray(FOMiniFraction);
        for (int layer = 0; layer <= deepestLayer; layer++)
        {
            FOMiniFraction[layer] /= totFOMfraction;
        }

        // initialise some residue decompositon variables
        residueName = new string[1] { "none" };
        pot_c_decomp = new double[1] { 0.0 };
    }

    /// <summary>
    /// Performs the initial setup and calculations
    /// </summary>
    /// <remarks>
    /// This procedure is also used onReset
    /// </remarks>
    private void SetInitialValues()
    {
        // general variables
        int nLayers = dlayer.Length;

        // convert and set C an N values over the profile
        for (int layer = 0; layer < nLayers; layer++)
        {
            // convert the amounts of mineral N
            double iniUrea = MathUtility.Divide(reset_ureappm[layer], convFactor[layer], 0.0);         // convert from ppm to kg/ha
            double iniNH4 = MathUtility.Divide(reset_nh4ppm[layer], convFactor[layer], 0.0);
            double iniNO3 = MathUtility.Divide(reset_no3ppm[layer], convFactor[layer], 0.0);

            // calculate total soil C
            double Soil_OC = reset_oc[layer] * 10000;					    // = (oc/100)*1000000 - convert from % to ppm
            Soil_OC = MathUtility.Divide(Soil_OC, convFactor[layer], 0.0);  //Convert from ppm to kg/ha

            // calculate inert soil C
            double InertC = finert[layer] * Soil_OC;

            // calculate microbial biomass C and N
            double BiomassC = MathUtility.Divide((Soil_OC - InertC) * fbiom[layer], 1.0 + fbiom[layer], 0.0);
            double BiomassN = MathUtility.Divide(BiomassC, biom_cn, 0.0);

            // calculate C and N values for active humus
            double HumusC = Soil_OC - BiomassC;
            double HumusN = MathUtility.Divide(HumusC, hum_cn, 0.0);

            // distribute C over fom pools
            double[] fomPool = new double[3];
            fomPool[0] = iniFOM_wt * FOMiniFraction[layer] * fract_carb[FOMtypeID_reset] * defaultFOMCarbonContent;
            fomPool[1] = iniFOM_wt * FOMiniFraction[layer] * fract_cell[FOMtypeID_reset] * defaultFOMCarbonContent;
            fomPool[2] = iniFOM_wt * FOMiniFraction[layer] * fract_lign[FOMtypeID_reset] * defaultFOMCarbonContent;

            // set the initial values across patches
            _urea[layer] = iniUrea;
            _nh4[layer] = iniNH4;
            _no3[layer] = iniNO3;
            inert_c[layer] = InertC;
            biom_c[layer] = BiomassC;
            biom_n[layer] = BiomassN;
            hum_c[layer] = HumusC;
            hum_n[layer] = HumusN;
            fom_c_pool1[layer] = fomPool[0];
            fom_c_pool2[layer] = fomPool[1];
            fom_c_pool3[layer] = fomPool[2];
            fom_n_pool1[layer] = MathUtility.Divide(fomPool[0], fomPoolsCNratio[0], 0.0);
            fom_n_pool2[layer] = MathUtility.Divide(fomPool[1], fomPoolsCNratio[1], 0.0);
            fom_n_pool3[layer] = MathUtility.Divide(fomPool[2], fomPoolsCNratio[2], 0.0);
        }

        initDone = true;

        // Store today's values (needed for deltas and NEW sysbal component)
        StoreStatus();
    }

    /// <summary>
    /// Sets the size of arrays (with nLayers)
    /// </summary>
    /// <remarks>
    /// This is used during initialisation and whenever the soil profile changes (thus not often at all)
    /// </remarks>
    /// <param name="nLayers">The number of layers</param>
    private void ResizeLayeredVariables(int nLayers)
    {
        // Amounts - N
        Array.Resize(ref _nh4, nLayers);
        Array.Resize(ref _no3, nLayers);
        Array.Resize(ref _urea, nLayers);
        Array.Resize(ref TodaysInitialNO3, nLayers);
        Array.Resize(ref TodaysInitialNH4, nLayers);
        Array.Resize(ref fom_n_pool1, nLayers);
        Array.Resize(ref fom_n_pool2, nLayers);
        Array.Resize(ref fom_n_pool3, nLayers);
        Array.Resize(ref biom_n, nLayers);
        Array.Resize(ref hum_n, nLayers);

        // Amounts - C
        Array.Resize(ref fom_c_pool1, nLayers);
        Array.Resize(ref fom_c_pool2, nLayers);
        Array.Resize(ref fom_c_pool3, nLayers);
        Array.Resize(ref inert_c, nLayers);
        Array.Resize(ref biom_c, nLayers);
        Array.Resize(ref hum_c, nLayers);

        // deltas
        Array.Resize(ref dlt_c_res_to_biom, nLayers);
        Array.Resize(ref dlt_c_res_to_hum, nLayers);
        Array.Resize(ref dlt_c_res_to_atm, nLayers);
        Array.Resize(ref dlt_res_nh4_min, nLayers);
        Array.Resize(ref dlt_res_no3_min, nLayers);

        Array.Resize(ref dlt_urea_hydrolysis, nLayers);
        Array.Resize(ref dlt_nitrification, nLayers);
        Array.Resize(ref dlt_n2o_nitrif, nLayers);
        Array.Resize(ref dlt_no3_dnit, nLayers);
        Array.Resize(ref dlt_n2o_dnit, nLayers);
        Array.Resize(ref dlt_fom_n_min, nLayers);
        Array.Resize(ref dlt_biom_n_min, nLayers);
        Array.Resize(ref dlt_hum_n_min, nLayers);
        Array.Resize(ref nh4_deficit_immob, nLayers);
        for (int i = 0; i < 3; i++)
        {
            Array.Resize(ref dlt_c_fom_to_biom[i], nLayers);
            Array.Resize(ref dlt_c_fom_to_hum[i], nLayers);
            Array.Resize(ref dlt_c_fom_to_atm[i], nLayers);
            Array.Resize(ref dlt_n_fom[i], nLayers);
        }
        Array.Resize(ref dlt_biom_c_hum, nLayers);
        Array.Resize(ref dlt_biom_c_atm, nLayers);
        Array.Resize(ref dlt_hum_c_biom, nLayers);
        Array.Resize(ref dlt_hum_c_atm, nLayers);

        Array.Resize(ref InhibitionFactor_Nitrification, nLayers);
    }

    /// <summary>
    /// Clear (zero out) the values of variables storing deltas
    /// </summary>
    /// <remarks>
    /// This is used to zero out the variables that need reseting every day, those that are not necessarily computed everyday
    /// </remarks>
    private void ClearDeltaVariables()
    {
        // residue decomposition
        Array.Clear(pot_c_decomp, 0, pot_c_decomp.Length);
        // this is also cleared onPotentialResidueDecompositionCalculated, but it is here to ensure it will be reset every timestep

        // miscelaneous
        Array.Clear(InhibitionFactor_Nitrification, 0, InhibitionFactor_Nitrification.Length);
        dlt_n_loss_in_sed = 0.0;
        dlt_c_loss_in_sed = 0.0;
    }


    /// <summary>
    /// Notifies any interested module about this module's ownership of solute information.
    /// </summary>
    private void AdvertiseMySolutes()
    {

        if (NewSolute != null)
        {
            string[] solute_names;
            if (useOrganicSolutes)
            {
                solute_names = new string[7] { "urea", "nh4", "no3", "org_c_pool1", "org_c_pool2", "org_c_pool3", "org_n" };
            }
            else
            { // don't publish the organic solutes
                solute_names = new string[3] { "urea", "nh4", "no3" };
            }

            NewSoluteType SoluteData = new NewSoluteType();
            SoluteData.solutes = solute_names;

            NewSolute.Invoke(SoluteData);
        }
    }

    /// <summary>
    /// Store today's initial N amounts
    /// </summary>
    private void StoreStatus()
    {
        for (int layer = 0; layer < dlayer.Length; layer++)
        {
            // store these values so they may be used to compute daily deltas
            TodaysInitialNH4[layer] = _nh4[layer];
            TodaysInitialNO3[layer] = _no3[layer];
        }
        TodaysInitialN = SumDoubleArray(nit_tot);
        TodaysInitialC = SumDoubleArray(carbon_tot);
    }

    /// <summary>
    /// Calculates variations in C an N, and publishes MassFlows to APSIM
    /// </summary>
    private void SendDeltaState()
    {

        double dltN = SumDoubleArray(nit_tot) - TodaysInitialN;
        double dltC = SumDoubleArray(carbon_tot) - TodaysInitialC;

        SendExternalMassFlowN(dltN);
        SendExternalMassFlowC(dltC);
    }

    /// <summary>
    /// Writes in the summaryfile a report about setup and status of SoilNitrogen
    /// </summary>
    private void WriteSummaryReport()
    {

        string myMessage = "";
        Console.WriteLine();
        if (usingSimpleSoilTemp)
            myMessage = "   - Soil temperature calculated internally";
        else
            myMessage = "   - Soil temperature supplied by apsim";
        Console.WriteLine("        " + myMessage);
        if (usingSimpleSoilpH)
            myMessage = "   - Soil pH was not supplied, default value will be used";
        else
            myMessage = "   - Soil pH supplied by apsim";
        Console.WriteLine("        " + myMessage);
        Console.WriteLine();

        Console.Write(@"
                      Soil Profile Properties
          ------------------------------------------------
           Layer    pH    OC     NO3     NH4    Urea
                         (%) (kg/ha) (kg/ha) (kg/ha)
          ------------------------------------------------
");
        for (int layer = 0; layer < dlayer.Length; ++layer)
        {
            Console.WriteLine("          {0,4:d1}     {1,4:F2}  {2,4:F2}  {3,6:F2}  {4,6:F2}  {5,6:F2}",
            layer + 1, ph[layer], oc[layer], no3[layer], nh4[layer], urea[layer]);
        }
        Console.WriteLine("          ------------------------------------------------");
        Console.WriteLine("           Totals              {0,6:F2}  {1,6:F2}  {2,6:F2}",
                  SumDoubleArray(no3), SumDoubleArray(nh4), SumDoubleArray(urea));
        Console.WriteLine("          ------------------------------------------------");
        Console.WriteLine();
        Console.Write(@"
                  Initial Soil Organic Matter Status
          ---------------------------------------------------------
           Layer      Hum-C   Hum-N  Biom-C  Biom-N   FOM-C   FOM-N
                    (kg/ha) (kg/ha) (kg/ha) (kg/ha) (kg/ha) (kg/ha)
          ---------------------------------------------------------
");

        double TotalFomC = 0.0;
        for (int layer = 0; layer < dlayer.Length; ++layer)
        {
            TotalFomC += fom_c[layer];
            Console.WriteLine("          {0,4:d1}   {1,10:F1}{2,8:F1}{3,8:F1}{4,8:F1}{5,8:F1}{6,8:F1}",
            layer + 1, hum_c[layer], hum_n[layer], biom_c[layer], biom_n[layer], fom_c[layer], fom_n[layer]);
        }
        Console.WriteLine("          ---------------------------------------------------------");
        Console.WriteLine("           Totals{0,10:F1}{1,8:F1}{2,8:F1}{3,8:F1}{4,8:F1}{5,8:F1}",
            SumDoubleArray(hum_c), SumDoubleArray(hum_n), SumDoubleArray(biom_c),
            SumDoubleArray(biom_n), TotalFomC, SumDoubleArray(fom_n));
        Console.WriteLine("          ---------------------------------------------------------");
        Console.WriteLine();
    }

    #endregion setup events

    #region >>  Process events handlers and methods

    #region »   Recurrent processes (each timestep)

    /// <summary>
    /// Sets the commands for each timestep - at very beginning of of it
    /// </summary>
    /// <param name="time">The date time</param>
    [EventHandler(EventName = "tick")]
    public void OnTick(TimeType time)
    {
        if (initDone)
        {
            // store some initial values, so they may be for mass balance
            StoreStatus();
            // clear variables holding deltas
            ClearDeltaVariables();
        }
    }

    /// <summary>
    /// Sets the commands for each timestep - at the main process phase
    /// </summary>
    [EventHandler(EventName = "process")]
    public void OnProcess()
    {

        // update soil temperature
        if (usingSimpleSoilTemp)
            Tsoil = simpleST.SoilTemperature(Clock.Today, MetFile.MinT, MetFile.MaxT, MetFile.Radn, salb, dlayer, bd, ll15_dep, sw_dep);
        else
            Tsoil = ave_soil_temp;

        // calculate C and N processes
        EvaluateProcesses();

        // send actual decomposition back to surface OM
        if (!isPondActive)
            SendActualResidueDecompositionCalculated();
    }

    /// <summary>
    /// Performs the soil C and N balance processes, at APSIM timestep.
    /// </summary>
    /// <remarks>
    /// The processes considered, in order, are:
    ///     - Decomposition of surface residues
    ///     - Urea hydrolysis
    ///     - Denitrification + N2O production
    ///     - SOM mineralisation (humus then m. biomass)
    ///     - Decomposition of FOM
    ///     - Nitrification + N2O production
    /// Note: potential surface organic matter decomposition is given by SurfaceOM module, only N balance is considered here
    ///  If there is a pond then surfaceOM is inactive, the decomposition of OM is done wholly by the pond module
    ///  Also, different parameters are used for some processes when pond is active
    /// </remarks>
    private void EvaluateProcesses()
    {
        int nLayers = dlayer.Length;    // number of soil layers

        // 1. surface residues decomposition
        // 1.1. clear dome deltas
        Array.Clear(dlt_c_res_to_biom, 0, dlt_c_res_to_biom.Length);
        Array.Clear(dlt_c_res_to_hum, 0, dlt_c_res_to_hum.Length);
        Array.Clear(dlt_c_res_to_atm, 0, dlt_c_res_to_atm.Length);
        Array.Clear(dlt_res_nh4_min, 0, dlt_res_nh4_min.Length);
        Array.Clear(dlt_res_no3_min, 0, dlt_res_no3_min.Length);
        // 1.2. get the amounts of C decomposed
        if (isPondActive)
        {
            // There is a pond in the system, the POND module will decompose residues - not SoilNitrogen
            //   the pond module computes the amounts of C added to the soil, here these are added to the top layer
            //   with the C:N ratio of the respective SOM pools (Not sure how N balance is kept here)

            // zero deltas by assigning new array
            dlt_c_decomp = new double[1][];
            dlt_n_decomp = new double[1][];
            dlt_c_decomp[0] = new double[nLayers];
            dlt_n_decomp[0] = new double[nLayers];

            dlt_c_res_to_biom[0] += pond_biom_C;   // humic material from breakdown of residues in pond
            dlt_c_res_to_hum[0] += pond_hum_C;     // biom material from breakdown of residues in pond
        }
        else
        {
            // compute the C and N balance for residue decomposition 
            DecomposeResidues();
        }

        // take each layer in turn and compute the soil C and N processes
        for (int layer = 0; layer < nLayers; layer++)
        {
            // 1.3. add/remove C and N from residue decomposition into appropriate pools

            // organic C pools
            biom_c[layer] += dlt_c_res_to_biom[layer];
            hum_c[layer] += dlt_c_res_to_hum[layer];

            // organic N balance
            hum_n[layer] = MathUtility.Divide(hum_c[layer], hum_cn, 0.0);
            biom_n[layer] = MathUtility.Divide(biom_c[layer], biom_cn, 0.0);

            // update soil mineral N
            _nh4[layer] += dlt_res_nh4_min[layer];
            _no3[layer] += dlt_res_no3_min[layer];

            // 2. get the urea hydrolysis
            dlt_urea_hydrolysis[layer] = UreaHydrolysis(layer);

            // update soil mineral N
            _urea[layer] -= dlt_urea_hydrolysis[layer];
            _nh4[layer] += dlt_urea_hydrolysis[layer];

            // 3. get the denitrification
            dlt_no3_dnit[layer] = Denitrification(layer);

            // update soil mineral N
            _no3[layer] -= dlt_no3_dnit[layer];

            // N2O loss to atmosphere due to denitrification
            double N2N2O = Denitrification_Nratio(layer);
            dlt_n2o_dnit[layer] = dlt_no3_dnit[layer] / (N2N2O + 1.0);

            // 4. transformations of soil organic matter pools
            // 4.1. mineralisation of humic pool
            MineraliseHumus(layer);

            // 4.2. mineralisation of m. biomass pool
            MineraliseMBiomass(layer);

            // 5. decomposition of FOM pools
            DecomposeFOM(layer);

            // update SOM pools
            biom_c[layer] += dlt_hum_c_biom[layer] - dlt_biom_c_hum[layer] - dlt_biom_c_atm[layer] +
                           dlt_c_fom_to_biom[0][layer] + dlt_c_fom_to_biom[1][layer] + dlt_c_fom_to_biom[2][layer];
            hum_c[layer] += dlt_biom_c_hum[layer] - dlt_hum_c_biom[layer] - dlt_hum_c_atm[layer] +
                           dlt_c_fom_to_hum[0][layer] + dlt_c_fom_to_hum[1][layer] + dlt_c_fom_to_hum[2][layer];

            biom_n[layer] = MathUtility.Divide(biom_c[layer], biom_cn, 0.0);
            hum_n[layer] = MathUtility.Divide(hum_c[layer], hum_cn, 0.0);

            // update FOM pools
            fom_c_pool1[layer] -= (dlt_c_fom_to_biom[0][layer] + dlt_c_fom_to_hum[0][layer] + dlt_c_fom_to_atm[0][layer]);
            fom_c_pool2[layer] -= (dlt_c_fom_to_biom[1][layer] + dlt_c_fom_to_hum[1][layer] + dlt_c_fom_to_atm[1][layer]);
            fom_c_pool3[layer] -= (dlt_c_fom_to_biom[2][layer] + dlt_c_fom_to_hum[2][layer] + dlt_c_fom_to_atm[2][layer]);

            fom_n_pool1[layer] -= dlt_n_fom[0][layer];
            fom_n_pool2[layer] -= dlt_n_fom[1][layer];
            fom_n_pool3[layer] -= dlt_n_fom[2][layer];

            // update soil mineral N after mineralisation/immobilisation
            // starts with nh4
            _nh4[layer] += dlt_hum_n_min[layer] + dlt_biom_n_min[layer] + dlt_fom_n_min[layer];
            if (_nh4[layer] < nh4_min[layer])
            {
                nh4_deficit_immob[layer] = nh4_min[layer] - _nh4[layer];
                _nh4[layer] = nh4_min[layer];
            }
            else
                nh4_deficit_immob[layer] = 0.0;

            // now change no3
            _no3[layer] -= nh4_deficit_immob[layer];
            if (_no3[layer] < no3_min[layer])
                throw new Exception("N immobilisation resulted in mineral N in layer(" + (layer + 1).ToString() + ") to go below minimum");
            // note: tests for adequate mineral N for immobilisation have been made so this no3 should not go below no3_min


            // 6. get the nitrification of ammonium-N
            dlt_nitrification[layer] = Nitrification(layer);

            // N2O loss to atmosphere during nitrification
            dlt_n2o_nitrif[layer] = N2OProducedDuringNitrification(layer);

            // update soil mineral N
            _no3[layer] += dlt_nitrification[layer] - dlt_nh4_dnit[layer];
            _nh4[layer] -= dlt_nitrification[layer];

            // 7. check whether mineral values are ok
            if (_urea[layer] < urea_min[layer] || _urea[layer] > 10000.0)
                throw new Exception("Value for urea(" + layer + ") is out of range");
            if (_nh4[layer] < nh4_min[layer] || _nh4[layer] > 10000.0)
                throw new Exception("Value for NH4(" + layer + ") is out of range");
            if (_no3[layer] < no3_min[layer] || _no3[layer] > 10000.0)
                throw new Exception("Value for NO3(" + layer + ") is out of range");
        }
    }

    #endregion recurrent processes

    #region »   Sporadic processes (not necessarily every timestep)

    /// <summary>
    /// Set the commands for writing a summary report to the summaryfile
    /// </summary>
    [EventHandler(EventName = "sum_report")]
    public void OnSum_report()
    {
        WriteSummaryReport();
    }

    /// <summary>
    /// Passes the information about the potential decomposition of surface residues
    /// </summary>
    /// <remarks>
    /// This information is passed by a residue/SurfaceOM module
    /// </remarks>
    /// <param name="SurfaceOrganicMatterDecomp">Data about the potential decomposition of each residue type on soil surface</param>
    [EventHandler(EventName = "PotentialResidueDecompositionCalculated")]
    public void OnPotentialResidueDecompositionCalculated(SurfaceOrganicMatterDecompType SurfaceOrganicMatterDecomp)
    {

        // number of residues being considered
        int nResidues = SurfaceOrganicMatterDecomp.Pool.Length;

        // zero variables by assigning new array
        residueName = new string[nResidues];
        residueType = new string[nResidues];
        pot_c_decomp = new double[nResidues];
        pot_n_decomp = new double[nResidues];
        pot_p_decomp = new double[nResidues];

        // store potential decomposition into appropriate variables
        for (int residue = 0; residue < nResidues; residue++)
        {
            residueName[residue] = SurfaceOrganicMatterDecomp.Pool[residue].Name;
            residueType[residue] = SurfaceOrganicMatterDecomp.Pool[residue].OrganicMatterType;
            pot_c_decomp[residue] = SurfaceOrganicMatterDecomp.Pool[residue].FOM.C;
            pot_n_decomp[residue] = SurfaceOrganicMatterDecomp.Pool[residue].FOM.N;
            // this P decomposition is needed to formulate data required by SOILP - struth, this is very ugly
            pot_p_decomp[residue] = SurfaceOrganicMatterDecomp.Pool[residue].FOM.P;
        }
    }

    /// <summary>
    /// Sends back to SurfaceOM the information about residue decomposition
    /// </summary>
    private void SendActualResidueDecompositionCalculated()
    {
        // Note:
        //      - Potential decomposition was given to this module by a residue/surfaceOM module. This module evaluated
        //          whether the conditions (C-N balance) allowed the decompostion to happen.
        //		- Now we explicitly tell the sender module the actual decomposition rate for each of its residues.
        //      - If there wasn't enough mineral N to decompose, the rate will be reduced to zero !!  - MUST CHECK THE VALIDITY OF THIS

        if (actualResidueDecompositionCalculated != null && SumDoubleArray(pot_c_decomp) >= epsilon)
        {
            int nLayers = dlayer.Length;
            int nResidues = residueName.Length;
            SurfaceOrganicMatterDecompType SOMDecomp = new SurfaceOrganicMatterDecompType();
            SOMDecomp.Pool = new SurfaceOrganicMatterDecompPoolType[nResidues];

            soilp_dlt_org_p = new double[nLayers];
            double[] c_summed_layer = new double[nLayers];


            for (int residue = 0; residue < nResidues; residue++)
            {
                double c_summed = SumDoubleArray(dlt_c_decomp[residue]);
                if (Math.Abs(c_summed) < epsilon)
                    c_summed = 0.0;
                double n_summed = SumDoubleArray(dlt_n_decomp[residue]);
                if (Math.Abs(n_summed) < epsilon)
                    n_summed = 0.0;

                // pack up the structure to return decompositions to SurfaceOrganicMatter
                SOMDecomp.Pool[residue] = new SurfaceOrganicMatterDecompPoolType();
                SOMDecomp.Pool[residue].FOM = new FOMType();
                SOMDecomp.Pool[residue].Name = residueName[residue];
                SOMDecomp.Pool[residue].OrganicMatterType = residueType[residue];
                SOMDecomp.Pool[residue].FOM.amount = 0.0F;
                SOMDecomp.Pool[residue].FOM.C = (float)c_summed;
                SOMDecomp.Pool[residue].FOM.N = (float)n_summed;
                SOMDecomp.Pool[residue].FOM.P = 0.0F;
                SOMDecomp.Pool[residue].FOM.AshAlk = 0.0F;
                // Note: The values for 'amount', 'P', and 'AshAlk' will not be collected by SurfaceOrganicMatter, so send zero as default.

                // dsg 131004 soilp needs some stuff - very ugly process - needs to be streamlined
                //  create some variables which soilp can "get" - layer based arrays independant of residues
                //for (int layer = 0; layer < nLayers; layer++)
                //{
                //soilp_dlt_res_c_atm[layer] += _dlt_res_c_atm[layer][residue];
                //soilp_dlt_res_c_hum[layer] += _dlt_res_c_hum[layer][residue];
                //soilp_dlt_res_c_biom[layer] += _dlt_res_c_biom[layer][residue];
                //c_summed_layer[layer] += dlt_res_c_decomp[layer];
                //}
                //RCichota: have put this as properties
            }

            // dsg 131004  calculate the old dlt_org_p (from the old Decomposed event sent by residue2) for getting by soilp
            double act_c_decomp = 0.0;
            double tot_pot_c_decomp = SumDoubleArray(pot_c_decomp);
            double tot_pot_p_decomp = SumDoubleArray(pot_p_decomp);
            for (int layer = 0; layer < nLayers; layer++)
            {
                act_c_decomp = dlt_c_res_to_biom[layer] + dlt_c_res_to_hum[layer] + dlt_c_res_to_atm[layer];
                soilp_dlt_org_p[layer] = tot_pot_p_decomp *
                    MathUtility.Divide(act_c_decomp, tot_pot_c_decomp, 0.0);
            }

            // raise the event
            actualResidueDecompositionCalculated.Invoke(SOMDecomp);
        }
    }

    /// <summary>
    /// Passes the instructions to incorporate FOM to the soil - simple FOM
    /// </summary>
    /// <remarks>
    /// In this event, the FOM is given as a single amount, so it will be assumed that the CN ratios of all fractions are equal
    /// Notes:
    ///     - if N (or CNR) values are not given, no FOM will not be added to soil
    ///     - if both CNR and N values are given, CNR is used and N is overwritten
    /// </remarks>
    /// <param name="inFOMdata">Data about the FOM to be added to the soil</param>
    [EventHandler(EventName = "IncorpFOM")]
    public void OnIncorpFOM(FOMLayerType inFOMdata)
    {
        int nLayers = dlayer.Length;

        // get the total amount to be added
        double totalCAmount = 0.0;
        double totalNAmount = 0.0;
        double amountCnotAdded = 0.0;
        double amountNnotAdded = 0.0;
        for (int layer = 0; layer < inFOMdata.Layer.Length; layer++)
        {
            if (layer < nLayers)
            {
                if (inFOMdata.Layer[layer].FOM.amount >= epsilon)
                {
                    inFOMdata.Layer[layer].FOM.C = inFOMdata.Layer[layer].FOM.amount * (float)defaultFOMCarbonContent;
                    if (inFOMdata.Layer[layer].CNR > epsilon)
                    {   // we have C:N info - note that this has precedence over N amount
                        totalCAmount += inFOMdata.Layer[layer].FOM.C;
                        inFOMdata.Layer[layer].FOM.N = inFOMdata.Layer[layer].FOM.C / inFOMdata.Layer[layer].CNR;
                        totalNAmount += inFOMdata.Layer[layer].FOM.N;
                    }
                    else if (inFOMdata.Layer[layer].FOM.N > epsilon)
                    {   // we have N info
                        totalCAmount += inFOMdata.Layer[layer].FOM.C;
                        totalNAmount += inFOMdata.Layer[layer].FOM.N;
                    }
                    else
                    {   // no info for N
                        amountCnotAdded += inFOMdata.Layer[layer].FOM.C;
                    }
                }
                else if (inFOMdata.Layer[layer].FOM.N >= epsilon)
                {   // no info for C
                    amountNnotAdded += inFOMdata.Layer[layer].FOM.N;
                }
            }
            else
                Console.WriteLine(" IncorpFOM: information passed contained more layers than the soil, these will be ignored");
        }

        // If any FOM was passed, make the partition into FOM pools
        if (totalCAmount >= epsilon)
        {
            // check whether a valid FOM type was given
            fom_type = 0;   // use the default if fom_type was not given
            for (int i = 0; i < fom_types.Length; i++)
            {
                if (inFOMdata.Type == fom_types[i])
                {
                    fom_type = i;
                    break;
                }
            }

            // Pack the handled values of fom, so they can be added to the soil
            FOMPoolType myFOMPoolData = new FOMPoolType();
            myFOMPoolData.Layer = new FOMPoolLayerType[inFOMdata.Layer.Length];

            // Now partition the .C and .N amounts into FOM pools
            for (int layer = 0; layer < inFOMdata.Layer.Length; layer++)
            {
                if (layer < nLayers)
                {
                    myFOMPoolData.Layer[layer] = new FOMPoolLayerType();
                    myFOMPoolData.Layer[layer].Pool = new FOMType[3];
                    myFOMPoolData.Layer[layer].Pool[0] = new FOMType();
                    myFOMPoolData.Layer[layer].Pool[1] = new FOMType();
                    myFOMPoolData.Layer[layer].Pool[2] = new FOMType();

                    if (inFOMdata.Layer[layer].FOM.C > epsilon)
                    {
                        myFOMPoolData.Layer[layer].nh4 = 0.0F;
                        myFOMPoolData.Layer[layer].no3 = 0.0F;
                        myFOMPoolData.Layer[layer].Pool[0].C = (float)(inFOMdata.Layer[layer].FOM.C * fract_carb[fom_type]);
                        myFOMPoolData.Layer[layer].Pool[1].C = (float)(inFOMdata.Layer[layer].FOM.C * fract_cell[fom_type]);
                        myFOMPoolData.Layer[layer].Pool[2].C = (float)(inFOMdata.Layer[layer].FOM.C * fract_lign[fom_type]);

                        myFOMPoolData.Layer[layer].Pool[0].N = (float)(inFOMdata.Layer[layer].FOM.N * fract_carb[fom_type]);
                        myFOMPoolData.Layer[layer].Pool[1].N = (float)(inFOMdata.Layer[layer].FOM.N * fract_cell[fom_type]);
                        myFOMPoolData.Layer[layer].Pool[2].N = (float)(inFOMdata.Layer[layer].FOM.N * fract_lign[fom_type]);
                    }
                }
            }

            // actually add the FOM to soil
            IncorporateFOM(myFOMPoolData);
        }
        else
            writeMessage("IncorpFOM: action was not carried out because no amount was given");

        // let the user know of any issues
        if ((amountCnotAdded >= epsilon) | (amountNnotAdded >= epsilon))
        {
            writeMessage("IncorpFOM - Warning Error: The amounts of " + amountCnotAdded.ToString("#0.00") +
            "kgC/ha and " + amountCnotAdded.ToString("#0.00") + "were not added because some information was missing");
        }
    }

    /// <summary>
    /// Passes the instructions to incorporate FOM to the soil - FOM pools
    /// </summary>
    /// <remarks>
    /// In this event, the FOM amount is given already partitioned by pool
    /// </remarks>
    /// <param name="inFOMPoolData">Data about the FOM to be added to the soil</param>
    [EventHandler(EventName = "IncorpFOMPool")]
    public void OnIncorpFOMPool(FOMPoolType inFOMPoolData)
    {
        // get the total amount to be added
        double totalCAmount = 0.0;
        double totalNAmount = 0.0;
        double amountCnotAdded = 0.0;
        double amountNnotAdded = 0.0;
        for (int layer = 0; layer < inFOMPoolData.Layer.Length; layer++)
        {
            if (layer < dlayer.Length)
            {
                for (int pool = 0; pool < 3; pool++)
                {
                    int teste1 = inFOMPoolData.Layer.Rank;
                    int teste2 = inFOMPoolData.Layer.Length;
                    int teste3 = inFOMPoolData.Layer.GetLength(0);
                    if (inFOMPoolData.Layer[layer].Pool[pool].C >= epsilon)
                    {   // we have both C and N, can add
                        totalCAmount += inFOMPoolData.Layer[layer].Pool[pool].C;
                        totalNAmount += inFOMPoolData.Layer[layer].Pool[pool].N;
                    }
                    else
                    {   // some data is mising, cannot add
                        amountCnotAdded += inFOMPoolData.Layer[layer].Pool[pool].C;
                        amountNnotAdded += inFOMPoolData.Layer[layer].Pool[pool].N;
                    }
                }
            }
            else
                Console.WriteLine(" IncorpFOMPool: information passed contained more layers than the soil, these will be ignored");
        }

        // add FOM to soil layers, if given
        if (totalCAmount >= epsilon)
            IncorporateFOM(inFOMPoolData);
        else
            writeMessage("IncorpFOMPool: action was not carried out because no amount was given");

        // let the user know of any issues
        if ((amountCnotAdded >= epsilon) | (amountNnotAdded >= epsilon))
        {
            writeMessage("IncorpFOMPool - Warning Error: The amounts of " + amountCnotAdded.ToString("#0.00") +
            "kgC/ha and " + amountCnotAdded.ToString("#0.00") + "were not added because some information was missing");
        }
    }

    /// <summary>
    /// Gets the data about incoming FOM, add to the patch's FOM pools
    /// </summary>
    /// <remarks>
    /// The FOM amount is given already partitioned by pool
    /// </remarks>
    /// <param name="FOMPoolData"></param>
    public void IncorporateFOM(FOMPoolType FOMPoolData)
    {
        for (int layer = 0; layer < FOMPoolData.Layer.Length; layer++)
        {
            fom_c_pool1[layer] += FOMPoolData.Layer[layer].Pool[0].C;
            fom_c_pool2[layer] += FOMPoolData.Layer[layer].Pool[1].C;
            fom_c_pool3[layer] += FOMPoolData.Layer[layer].Pool[2].C;

            fom_n_pool1[layer] += FOMPoolData.Layer[layer].Pool[0].N;
            fom_n_pool2[layer] += FOMPoolData.Layer[layer].Pool[1].N;
            fom_n_pool3[layer] += FOMPoolData.Layer[layer].Pool[2].N;

            _nh4[layer] += FOMPoolData.Layer[layer].nh4;
            _no3[layer] += FOMPoolData.Layer[layer].no3;
        }
    }

    /// <summary>
    /// Passes the information about changes in the soil profile
    /// </summary>
    /// <remarks>
    /// The event is primarily used to account for the effects of soil erosion
    /// It is assumed that if there are any changes in the soil profile the module doing it will let us know.
    ///     this will be done by setting both 'soil_loss' and 'n_reduction' (ProfileReductionAllowed) to a non-default value
    /// </remarks>
    /// <param name="NewProfile">Data about the new soil profile</param>
    [EventHandler(EventName = "new_profile")]
    public void OnNew_profile(NewProfileType NewProfile)
    {
        // Are these changes mainly (only) due to by erosion? what else??
        //  this event happens on initialisation, but no actions is needed here at that point.

        // check for variations in the soil profile. and update the C and N amounts appropriately
        if (soil_loss > epsilon && ProfileReductionAllowed)
        {
            double[] new_dlayer = new double[NewProfile.dlayer.Length];
            for (int layer = 0; layer < new_dlayer.Length; layer++)
                new_dlayer[layer] = (double)NewProfile.dlayer[layer];

            // RCichota: we must be carefull here, dlayer is now read as an [Input], so it will be updated on the fly
            //  It is possible thus that comparing our dlayer with NewProfile.dlayer will result in no differences...
            //  I have set up a 'reset_dlayer' which holds the original dlayer and will check the changes in the soil
            //   profile againt it.  
            //      Need to verify when dlayer (and the other soil vars) change in relation to this event.


            // There have been some soil loss, launch the UpdateProfile
            UpdateProfile(new_dlayer);

            // don't we need to update the other variables (at least their size)???
        }
    }

    /// <summary>
    /// Check whether profile has changed and move values between layers
    /// </summary>
    /// <param name="new_dlayer">New values for dlayer</param>
    private void UpdateProfile(double[] new_dlayer)
    {
        dlt_n_loss_in_sed = 0.0;
        dlt_c_loss_in_sed = 0.0;

        // How to decide:
        // If bedrock is lower than lowest  profile depth, we won't see any change in profile (layer structure),
        //  even if there is erosion. If bedrock is reached, than the profile is actually reduced.

        // Ideally we should test both soil_loss and dlayer for changes to cater for
        // manager control. But, the latter means we have to fudge enr for the loss from top layer.

        // move pools
        // EJZ: Why aren't no3 and urea moved????  - RCichota: added urea and no3.  What happens with this when there is reset???
        dlt_n_loss_in_sed += MoveLayers(ref _urea, new_dlayer);
        dlt_n_loss_in_sed += MoveLayers(ref _nh4, new_dlayer);
        dlt_n_loss_in_sed += MoveLayers(ref _no3, new_dlayer);
        dlt_c_loss_in_sed += MoveLayers(ref inert_c, new_dlayer);
        dlt_c_loss_in_sed += MoveLayers(ref biom_c, new_dlayer);
        dlt_n_loss_in_sed += MoveLayers(ref biom_n, new_dlayer);
        dlt_c_loss_in_sed += MoveLayers(ref hum_c, new_dlayer);
        dlt_n_loss_in_sed += MoveLayers(ref hum_n, new_dlayer);
        dlt_n_loss_in_sed += MoveLayers(ref fom_n_pool1, new_dlayer);
        dlt_n_loss_in_sed += MoveLayers(ref fom_n_pool2, new_dlayer);
        dlt_n_loss_in_sed += MoveLayers(ref fom_n_pool3, new_dlayer);
        dlt_c_loss_in_sed += MoveLayers(ref fom_c_pool1, new_dlayer);
        dlt_c_loss_in_sed += MoveLayers(ref fom_c_pool2, new_dlayer);
        dlt_c_loss_in_sed += MoveLayers(ref fom_c_pool3, new_dlayer);
    }

    /// <summary>
    /// Move the values of a given variable between layers, from bottom to top
    /// </summary>
    /// <remarks>
    /// Used when there is some soil lost due to erosion
    ///     // Changed from subroutine to function returning amount of profile loss
    /// </remarks>
    /// <param name="SoilProperty">Variable to move layers</param>
    /// <param name="new_dlayer">new dlayer array</param>
    /// <returns>Amount of C or N lost because of changes in profile</returns>
    private double MoveLayers(ref double[] SoilProperty, double[] new_dlayer)
    {
        double layer_loss = 0.0;
        double layer_gain = 0.0;
        int lowest_layer = dlayer.Length;
        int lowestLayer = reset_dlayer.Length - 1;
        int new_lowest_layer = new_dlayer.Length;

        double TodaysInitialAmount = SumDoubleArray(SoilProperty);
        double SoilLossThickness = soil_loss / (bd[0] * 10);  //  10 = 1000/10000, converts loss to kg/m2, then to L/m2 = mm
        double AmountGainedBottom = 0.0;
        double AmountLostTop = 0.0;

        // check whether soil loss is not bigger than any layer
        for (int layer = 0; layer < lowestLayer; layer++)
        {
            SoilLossThickness = soil_loss * (bd[layer] * 10);
            if (dlayer[layer] < SoilLossThickness)
            {
                double LayerDeviation = ((SoilLossThickness / dlayer[layer]) - 1) * 100;
                throw new Exception("Soil loss is greater than the thickness of layer(" + layer.ToString() + "), by " + LayerDeviation.ToString("#0.00") +
                    "%.\nConstrained to this layer. Re-mapping of SoilN pools will be incorrect.");
            }
        }

        // initialise layer loss from below profile same as bottom layer
        if (Math.Abs(SumDoubleArray(reset_dlayer) - SumDoubleArray(new_dlayer)) < epsilon)
        {
            // Soil profile structure did not change, update values among layers
            // Assuming soil was lost in the surface, move values from the bottom up (assumes the properties of the soil below our profile are the same as the bottom layer)
            AmountGainedBottom = SoilProperty[lowestLayer] * Math.Min(1.0, SoilLossThickness / dlayer[lowestLayer]);
        }
        else if (SumDoubleArray(reset_dlayer) < SumDoubleArray(new_dlayer))
        {
            // soil profile has been increased (assume material has been added to the surface
            AmountGainedBottom = 0.0;
            // check for changes in layer structure
            if (reset_dlayer.Length < new_dlayer.Length)
            {
                throw new Exception("Number of soil layers was increased, this option has not been coded in SoilNitrogen");
            }
        }
        else
        {
            // soil profile has been reduced
            AmountGainedBottom = 0.0;

            // check for changes in layer structure
            if (new_dlayer.Length < reset_dlayer.Length)
            {
                // we have less layers than before.  The layer structure is kept, from top down, thus in fact the bottom layer merges with the second bottom etc.

                // merge the bottom layers
                for (int layer = lowestLayer; layer >= new_dlayer.Length; layer--)
                {
                    SoilProperty[layer - 1] += SoilProperty[layer];
                }
                Array.Resize(ref SoilProperty, new_dlayer.Length);  // isn't this also done later on? no, but I think it should (do a ResizeArrays)
            }
            else
            {
                // the thickness of some layer(s) changed 
                throw new Exception("Soil loss has reduced soil profile, but did not change number of layers, this option has not been coded in SoilNitrogen");
            }
        }

        layer_gain = AmountGainedBottom;

        // now move from bottom layer to top
        for (int layer = new_lowest_layer - 1; layer >= 0; layer--)
        {
            // this layer gains what the lower layer lost
            SoilLossThickness = soil_loss * (bd[layer] * 10);
            layer_loss = SoilProperty[layer] * Math.Min(1.0, SoilLossThickness / dlayer[layer]);
            SoilProperty[layer] += layer_gain - layer_loss;
            layer_gain = layer_loss;
        }

        // now adjust top layer using enrichment equation
        double enr = enr_a_coeff * Math.Pow(soil_loss * 1000, -1.0 * enr_b_coeff);
        enr = Math.Max(1.0, Math.Min(enr, enr_a_coeff));

        AmountLostTop = layer_loss * enr;
        SoilProperty[0] = Math.Max(0.0, SoilProperty[0] + layer_loss - AmountLostTop);

        // check mass balance
        double UpdatedAmount = SumDoubleArray(SoilProperty);
        TodaysInitialAmount += AmountGainedBottom - AmountLostTop;
        if (!MathUtility.FloatsAreEqual(UpdatedAmount, TodaysInitialAmount))
        {
            throw new Exception("N mass balance out");
        }
        // why don't we test AmountLostTop against Soil_lost??
        return AmountLostTop;
    }

    /// <summary>
    /// Passes the information about changes in mineral N made by other modules
    /// </summary>
    /// <param name="NitrogenChanges">The variation (delta) for each mineral N form</param>
    [EventHandler(EventName = "NitrogenChanged")]
    public void OnNitrogenChanged(NitrogenChangedType NitrogenChanges)
    {
        // check whether there are significant values, if so pass them to appropriate dlt
        if (hasSignificantValues(NitrogenChanges.DeltaUrea, epsilon))
            dlt_urea = NitrogenChanges.DeltaUrea;
        if (hasSignificantValues(NitrogenChanges.DeltaNH4, epsilon))
            dlt_nh4 = NitrogenChanges.DeltaNH4;
        if (hasSignificantValues(NitrogenChanges.DeltaNO3, epsilon))
            dlt_no3 = NitrogenChanges.DeltaNO3;
    }

    /// <summary>
    /// Get the information about urine being added
    /// </summary>
    /// <param name="UrineAdded">Urine deposition data (includes urea N amount, volume, area affected, etc)</param>
    [EventHandler(EventName = "AddUrine")]
    public void OnAddUrine(AddUrineType UrineAdded)
    {
        // Starting with the minimalist version. To be updated by Val's group to include a urine patch algorithm
        _urea[0] += UrineAdded.Urea;
    }

    /// <summary>
    /// Comunicate other components that C amount in the soil has changed
    /// </summary>
    /// <param name="dltC">C changes</param>
    private void SendExternalMassFlowC(double dltC)
    {
        ExternalMassFlowType massBalanceChange = new ExternalMassFlowType();
        if (Math.Abs(dltC) < epsilon)
            dltC = 0.0;     // don't bother with values that are too small
        massBalanceChange.FlowType = dltC >= epsilon ? "gain" : "loss";
        massBalanceChange.PoolClass = "soil";
        massBalanceChange.N = (float)Math.Abs(dltC);
        ExternalMassFlow.Invoke(massBalanceChange);
    }

    /// <summary>
    /// Comunicate other components that N amount in the soil has changed
    /// </summary>
    /// <param name="dltN">N changes</param>
    private void SendExternalMassFlowN(double dltN)
    {
        ExternalMassFlowType massBalanceChange = new ExternalMassFlowType();
        if (Math.Abs(dltN) < epsilon)
            dltN = 0.0;
        massBalanceChange.FlowType = dltN >= epsilon ? "gain" : "loss";
        massBalanceChange.PoolClass = "soil";
        massBalanceChange.N = (float)Math.Abs(dltN);
        ExternalMassFlow.Invoke(massBalanceChange);
    }

    #endregion sporadic processes

    #endregion processes events

    #region >>  The soil C and N processes

    #region »   OM processes

    /// <summary>
    /// Calculate rate of nitrogen mineralization/immobilization of surface residues
    /// </summary>
    /// <remarks>
    /// This will test to see whether adequate mineral nitrogen is available to sustain potential rate of decomposition of
    /// surface residues, which was somputed by SurfaceOM. It aslo calculates net rate of nitrogen mineralization/immobilization
    /// </remarks>
    private void DecomposeResidues()
    {
        int nLayers = dlayer.Length;                            // number of layers in the soil
        int nResidues = residueName.Length;                     // number of residues being considered
        double[] no3_available = new double[nLayers];           // no3 available for mineralisation
        double[] nh4_available = new double[nLayers];           // nh4 available for mineralisation
        int min_layer = getCumulativeIndex(min_depth, dlayer);  // soil layer down to which N is available for mineralisation
        double[] fracLayer = FractionLayer(min_depth);          // fraction of each layer that is within mineralisation depth
        double[] dlt_c_to_biom = new double[nResidues];         // C mineralized converted to biomass
        double[] dlt_c_to_hum = new double[nResidues];          // C mineralized converted to humus

        // 1. zero deltas by assigning new array
        dlt_c_decomp = new double[nResidues][];
        dlt_n_decomp = new double[nResidues][];
        for (int residue = 0; residue < nResidues; residue++)
        {
            dlt_c_decomp[residue] = new double[nLayers];
            dlt_n_decomp[residue] = new double[nLayers];
        }

        // check whether there is any potential residue decompostion
        if (SumDoubleArray(pot_c_decomp) > epsilon)
        {  // there is some decomposition, verify C-N balance

            // 2. get the available mineral N in the soil close to surface (mineralisation depth)
            for (int layer = 0; layer <= min_layer; layer++)
            {
                no3_available[layer] = Math.Max(0.0, _no3[layer] - no3_min[layer]) * fracLayer[layer];
                nh4_available[layer] = Math.Max(0.0, _nh4[layer] - nh4_min[layer]) * fracLayer[layer];
            }

            // 3. get the potential transfers to m. biomass and humic pools
            for (int residue = 0; residue < nResidues; residue++)
            {
                dlt_c_to_biom[residue] = pot_c_decomp[residue] * ef_res * fr_res_biom;
                dlt_c_to_hum[residue] = pot_c_decomp[residue] * ef_res * (1.0 - fr_res_biom);
            }

            // 4. test whether there is adequate N available to meet immobilization demand

            // 4.1. potential N demanded for conversion of FOM into soil OM
            double n_demand = MathUtility.Divide(SumDoubleArray(dlt_c_to_biom), biom_cn, 0.0) +
                              MathUtility.Divide(SumDoubleArray(dlt_c_to_hum), hum_cn, 0.0);
            // 4.2. total available N for this process
            double n_min_available = SumDoubleArray(nh4_available) + SumDoubleArray(no3_available);
            double n_available = n_min_available + SumDoubleArray(pot_n_decomp);

            // 4.3. factor to reduce mineralization rate if insufficient N is available
            double ReductionFactor = 1.0;
            if (n_demand > n_available)
            {
                ReductionFactor = MathUtility.Divide(n_min_available,
                                                      n_demand - SumDoubleArray(pot_n_decomp), 0.0);
                ReductionFactor = Math.Max(0.0, Math.Min(1.0, ReductionFactor));
            }

            // 5. partition the additions of C and N to layers
            double dlt_n_decomp_tot = 0.0;
            double dlt_c_atm = 0.0;
            double fractionIntoLayer = 1.0;
            for (int layer = 0; layer <= min_layer; layer++)
            {
                // 5.1. fraction of mineralised stuff going in this layer
                fractionIntoLayer = MathUtility.Divide(dlayer[layer] * fracLayer[layer], min_depth, 0.0);

                // 5.2. adjust C and N amounts for each residue and add to soil OM pools
                for (int residue = 0; residue < nResidues; residue++)
                {
                    dlt_c_decomp[residue][layer] = pot_c_decomp[residue] * ReductionFactor * fractionIntoLayer;
                    dlt_n_decomp[residue][layer] = pot_n_decomp[residue] * ReductionFactor * fractionIntoLayer;
                    dlt_n_decomp_tot += dlt_n_decomp[residue][layer];

                    dlt_c_res_to_biom[layer] += dlt_c_to_biom[residue] * ReductionFactor * fractionIntoLayer;
                    dlt_c_res_to_hum[layer] += dlt_c_to_hum[residue] * ReductionFactor * fractionIntoLayer;
                    dlt_c_atm = pot_c_decomp[residue] * Math.Max(0.0, 1 - ef_res);
                    dlt_c_res_to_atm[layer] += dlt_c_atm * ReductionFactor * fractionIntoLayer;
                }
            }

            // 6. get the net N mineralised/immobilised (hg/ha) - positive means mineralisation, negative is immobilisation
            double dlt_mineral_n = dlt_n_decomp_tot - n_demand * ReductionFactor;

            // 7. partition mineralised/immobilised N into mineral forms
            if (dlt_mineral_n > epsilon)
            {
                // 7.1. we have mineralisation into NH4, distribute it over the layers
                for (int layer = 0; layer <= min_layer; layer++)
                {
                    fractionIntoLayer = MathUtility.Divide(dlayer[layer] * fracLayer[layer], min_depth, 0.0);
                    dlt_res_nh4_min[layer] = dlt_mineral_n * fractionIntoLayer;
                }
            }
            else if (dlt_mineral_n < -epsilon)
            {
                // 7.2. we have immobilisation, soak up any N required from NH4 then NO3
                for (int layer = 0; layer <= min_layer; layer++)
                {
                    dlt_res_nh4_min[layer] = -Math.Min(nh4_available[layer], Math.Abs(dlt_mineral_n));
                    dlt_mineral_n -= dlt_res_nh4_min[layer];
                }

                for (int layer = 0; layer <= min_layer; layer++)
                {
                    dlt_res_no3_min[layer] = -Math.Min(no3_available[layer], Math.Abs(dlt_mineral_n));
                    dlt_mineral_n -= dlt_res_no3_min[layer];
                }

                // 7.3. check that there is no remaining immobilization demand
                if (Math.Abs(dlt_mineral_n) >= epsilon)
                    throw new Exception("Value for remaining immobilization is out of range");
            }
            // else, there is no net N transformation
        }
        // else, there is no residue decomposition
    }

    /// <summary>
    /// Calculate the transformations of the the soil humic pool, mineralisation (+ve) or immobilisation (-ve)
    /// </summary>
    /// <remarks>
    /// It is assumed that the inert_C component of the humic pool is not subject to mineralisation
    /// some constants have different values when there's a pond, as anaerobic conditions dominate
    /// </remarks>
    /// <param name="layer">the node number representing soil layer for which calculations will be made</param>
    private void MineraliseHumus(int layer)
    {
        // index = 1 for aerobic conditions, 2 for anaerobic conditions
        int index = (!isPondActive) ? 1 : 2;

        // get the soil temperature factor
        double stf = 1.0;
        if (usingNewFunctions)
            stf = SoilTempFactor(layer, index, TempFactorData_MinerSOM);
        else if (SoilNParameterSet == "rothc")
            stf = RothcTF(layer, index);
        else
            stf = TF(layer, index);

        // get the soil water factor
        double swf = 1.0;
        if (usingNewFunctions)
            swf = SoilMoistFactor(layer, index, MoistFactorData_MinerSOM);
        else
            swf = WF(layer, index);

        // compute the mineralization amounts of C and N from the humic pool
        double dlt_c_min_tot = (hum_c[layer] - inert_c[layer]) * rd_hum[index - 1] * stf * swf;
        double dlt_n_min_tot = MathUtility.Divide(dlt_c_min_tot, hum_cn, 0.0);

        // distribute the mineralised N and C
        dlt_hum_c_biom[layer] = dlt_c_min_tot * ef_hum;
        dlt_hum_c_atm[layer] = dlt_c_min_tot * (1.0 - ef_hum);

        // calculate net mineralization
        dlt_hum_n_min[layer] = dlt_n_min_tot - MathUtility.Divide(dlt_hum_c_biom[layer], biom_cn, 0.0);
    }

    /// <summary>
    /// Calculate the transformations of the soil biomass pool, mineralisation (+ve) or immobilisation (-ve)
    /// </summary>
    /// <param name="layer">the node number representing soil layer for which calculations will be made</param>
    private void MineraliseMBiomass(int layer)
    {
        // index = 1 for aerobic and 2 for anaerobic conditions
        int index = (!isPondActive) ? 1 : 2;

        // get the soil temperature factor
        double stf = 1.0;
        if (usingNewFunctions)
            stf = SoilTempFactor(layer, index, TempFactorData_MinerSOM);
        else if (SoilNParameterSet == "rothc")
            stf = RothcTF(layer, index);
        else
            stf = TF(layer, index);

        // get the soil water factor
        double swf = 1.0;
        if (usingNewFunctions)
            swf = SoilMoistFactor(layer, index, MoistFactorData_MinerSOM);
        else
            swf = WF(layer, index);

        // compute the mineralization amounts of C and N from the m. biomass pool
        double dlt_n_min_tot = biom_n[layer] * rd_biom[index - 1] * stf * swf;
        double dlt_c_min_tot = dlt_n_min_tot * biom_cn;

        // distribute the mineralised N and C
        dlt_biom_c_hum[layer] = dlt_c_min_tot * ef_biom * (1.0 - fr_biom_biom);
        dlt_biom_c_atm[layer] = dlt_c_min_tot * (1.0 - ef_biom);

        // calculate net mineralization
        dlt_biom_n_min[layer] = dlt_n_min_tot - MathUtility.Divide(dlt_biom_c_hum[layer], hum_cn, 0.0) -
                           MathUtility.Divide((dlt_c_min_tot - dlt_biom_c_atm[layer] - dlt_biom_c_hum[layer]), biom_cn, 0.0);
    }

    /// <summary>
    /// Calculate the decomposition of the soil Fresh OM, mineralisation (+ve) or immobilisation (-ve)
    /// </summary>
    /// <remarks>
    /// - parameters are given in pairs, for aerobic and anaerobic conditions (with pond)
    /// </remarks>
    /// <param name="layer">the node number representing soil layer for which calculations will be made</param>
    private void DecomposeFOM(int layer)
    {
        // index = 1 for aerobic and 2 for anaerobic conditions
        int index = (!isPondActive) ? 1 : 2;

        // get total available mineral N (kg/ha)
        double mineralN_available = Math.Max(0.0, (_no3[layer] - no3_min[layer]) + (_nh4[layer] - nh4_min[layer]));

        // ratio of C in fresh OM to N available for decay
        double cnr = MathUtility.Divide(fom_c[layer], fom_n[layer] + mineralN_available, 0.0);

        // calculate the C:N ratio factor
        double cnrf = 1.0;
        if (usingNewFunctions)
            CNratioFactor(layer, index, cnrf_optcn, cnrf_coeff);
        else
            cnrf = Math.Max(0.0, Math.Min(1.0, Math.Exp(-cnrf_coeff * (cnr - cnrf_optcn) / cnrf_optcn)));

        // get the soil temperature factor
        double stf = 1.0;
        if (usingNewFunctions)
            stf = SoilTempFactor(layer, index, TempFactorData_DecompFOM);
        else if (SoilNParameterSet == "rothc")
            stf = RothcTF(layer, index);
        else
            stf = TF(layer, index);

        // get the soil water factor
        double swf = 1.0;
        if (usingNewFunctions)
            swf = SoilMoistFactor(layer, index, MoistFactorData_DecompFOM);
        else
            swf = WF(layer, index);

        // calculate gross amount of C & N released due to mineralisation of the fresh organic matter.
        if (fom_c[layer] >= fom_min)
        {
            double dlt_n_fom_gross_miner = 0.0; // amount of fresh organic N mineralized across fpools (kg/ha)
            double dlt_c_fom_gross_miner = 0.0; // total C mineralized (kg/ha) summed across fpools
            double[] dlt_n_gross_decomp = new double[3]; // amount of fresh organic N mineralized in each pool (kg/ha)
            double[] dlt_c_gross_decomp = new double[3]; // amount of C mineralized (kg/ha) from each pool

            // C:N ratio of fom
            double fom_cn = MathUtility.Divide(fom_c[layer], fom_n[layer], 0.0);

            // get the decomposition of carbohydrate-like, cellulose-like and lignin-like fractions (fpools) in turn.
            for (int fractn = 0; fractn < 3; fractn++)
            {
                // get the max decomposition rate for each fpool
                double drate = FractRDFom(fractn)[index - 1] * cnrf * stf * swf;

                // calculate the gross amount of fresh organic carbon mineralised (kg/ha)
                dlt_c_gross_decomp[fractn] = drate * FractFomC(fractn)[layer];

                // calculate the gross amount of N released from fresh organic matter (kg/ha)
                dlt_n_gross_decomp[fractn] = drate * FractFomN(fractn)[layer];

                // sum up values
                dlt_c_fom_gross_miner += dlt_c_gross_decomp[fractn];
                dlt_n_fom_gross_miner += dlt_n_gross_decomp[fractn];
            }

            // calculate potential transfers of C mineralised to biomass
            double dlt_c_biom_tot = dlt_c_fom_gross_miner * ef_fom * fr_fom_biom;

            // calculate potential transfers of C mineralised to humus
            double dlt_c_hum_tot = dlt_c_fom_gross_miner * ef_fom * (1.0 - fr_fom_biom);

            // test whether there is adequate N available to meet immobilisation demand
            double n_demand = MathUtility.Divide(dlt_c_biom_tot, biom_cn, 0.0) +
                              MathUtility.Divide(dlt_c_hum_tot, hum_cn, 0.0);
            double n_available = mineralN_available + dlt_n_fom_gross_miner;

            // factor to reduce mineralisation rates if insufficient N to meet immobilisation demand
            double reductionFactor = 1.0;
            if (n_demand > n_available)
                reductionFactor = Math.Max(0.0, Math.Min(1.0, MathUtility.Divide(mineralN_available, n_demand - dlt_n_fom_gross_miner, 0.0)));

            // now adjust carbon transformations etc. and similarly for N pools
            for (int fractn = 0; fractn < 3; fractn++)
            {
                double dlt_c_act_decomp = dlt_c_gross_decomp[fractn] * reductionFactor;
                dlt_c_fom_to_biom[fractn][layer] = dlt_c_act_decomp * ef_fom * fr_fom_biom;
                dlt_c_fom_to_hum[fractn][layer] = dlt_c_act_decomp * ef_fom * (1.0 - fr_fom_biom);
                dlt_c_fom_to_atm[fractn][layer] = dlt_c_act_decomp * (1.0 - ef_fom);
                dlt_n_fom[fractn][layer] = dlt_n_gross_decomp[fractn] * reductionFactor;
            }
            dlt_fom_n_min[layer] = (dlt_n_fom_gross_miner - n_demand) * reductionFactor;
        }
        else
        {
            // only reset the delta variables
            for (int fractn = 0; fractn < 3; fractn++)
            {
                dlt_c_fom_to_biom[fractn][layer] = 0.0;
                dlt_c_fom_to_hum[fractn][layer] = 0.0;
                dlt_c_fom_to_atm[fractn][layer] = 0.0;
                dlt_n_fom[fractn][layer] = 0.0;
            }
            dlt_fom_n_min[layer] = 0.0;
        }
    }

    #endregion OM processes

    #region »   N processes

    /// <summary>
    /// Calculate the amount of urea converted to NH4 via hydrolysis (kgN/ha)
    /// </summary>
    /// <remarks>
    /// - very small amounts of urea are hydrolysed promptly, regardless the hydrolysis settings
    /// - parameters are given in pairs, for aerobic and anaerobic conditions (with pond)
    /// </remarks>
    /// <param name="layer">the node number representing soil layer for which calculations will be made</param>
    /// <returns>delta N coverted from urea into NH4</returns>
    private double UreaHydrolysis(int layer)
    {
        // index = 1 for aerobic and 2 for anaerobic conditions
        int index = (!isPondActive) ? 1 : 2;

        double result;

        if (_urea[layer] < 0.1)
            // urea amount is too small, all will be hydrolised
            result = _urea[layer];
        else
        {
            // get the soil temperature factor
            double stf = 1.0;
            if (usingNewFunctions)
                stf = SoilTempFactor(layer, index, TempFactorData_UHydrol);
            else
                stf = Math.Max(0.0, Math.Min(1.0, (Tsoil[layer] / 40.0) + 0.20));

            // get the soil water factor
            double swf = 1.0;
            if (usingNewFunctions)
                swf = SoilMoistFactor(layer, index, MoistFactorData_UHydrol);
            else
                swf = Math.Max(0.0, Math.Min(1.0, WF(layer, index) + 0.20));

            // note (jngh) oc & ph are not updated during simulation
            //   mep    following equation would be better written using oc(layer) = (hum_C(layer) + biom_C(layer))

            // potential fraction of urea being hydrolysed
            double pot_hydrol_rate = 0.0;
            double MinHydrol = 0.0;
            if (usingNewFunctions)
            {
                double totalC = (hum_c[layer] + biom_c[layer]) * convFactor[layer] / 10000;  // (100/1000000) = convert to ppm and then to %
                pot_hydrol_rate = potHydrol_parmA + potHydrol_parmB * totalC +
                     potHydrol_parmC * ph[layer] + potHydrol_parmD * totalC * ph[layer];
                MinHydrol = potHydrol_min;
            }
            else
            {
                pot_hydrol_rate = -1.12 + 1.31 * reset_oc[layer] + 0.203 * ph[layer] - 0.155 * reset_oc[layer] * ph[layer];
                MinHydrol = 0.25;
            }

            pot_hydrol_rate = Math.Max(MinHydrol, Math.Min(1.0, pot_hydrol_rate));

            // actual amount hydrolysed;
            result = Math.Max(0.0, Math.Min(_urea[layer], pot_hydrol_rate * _urea[layer] * Math.Min(swf, stf)));
        }

        return result;
    }

    /// <summary>
    /// Calculate the amount of NH4 converted to NO3 via nitrification
    /// </summary>
    /// <remarks>
    /// - This routine is much simplified from original CERES code
    /// - pH effect on nitrification is not used as pH is not simulated
    /// - parameters are given in pairs, for aerobic and anaerobic conditions (with pond)
    /// </remarks>
    /// <param name="layer">the node number representing soil layer for which calculations will be made</param>
    /// <returns>delta N coverted from NH4 into NO3</returns>
    private double Nitrification(int layer)
    {
        // index = 1 for aerobic and 2 for anaerobic conditions
        int index = (!isPondActive) ? 1 : 2;

        // get the soil temperature factor
        double stf = 1.0;
        if (usingNewFunctions)
            stf = SoilTempFactor(layer, index, TempFactorData_Nitrif);
        else
            stf = TF(layer, index);

        // get the soil water factor
        double swf = 1.0;
        if (usingNewFunctions)
            swf = SoilMoistFactor(layer, index, MoistFactorData_Nitrif);
        else
            swf = WFNitrf(layer, index);

        // get the soil pH factor
        double phf = 1.0;
        if (usingNewFunctions)
            phf = SoilpHFactor(layer, index, pHFactorData_Nitrif);
        else
            phf = pHFNitrf(layer);

        // get most limiting factor
        double pni = Math.Min(swf, Math.Min(stf, phf));
        // NOTE: factors to adjust rate of nitrification are used combined, with phf removed to match CERES v1
        //          well so what is pH factor doing here??

        // get the potential rate of nitrification for layer
        double nh4ppm = _nh4[layer] * convFactor[layer];
        double opt_nitrif_rate_ppm = MathUtility.Divide(nitrification_pot * nh4ppm, nh4ppm + nh4_at_half_pot, 0.0);

        // get the actual rate of nitrification
        double nitrif_rate = opt_nitrif_rate_ppm * pni * Math.Max(0.0, 1.0 - InhibitionFactor_Nitrification[layer]);
        nitrif_rate = MathUtility.Divide(nitrif_rate, convFactor[layer], 0.0);      // convert back to kg/ha

        return nitrif_rate;
    }

    /// <summary>
    /// Calculate the amount of N2O produced during nitrification
    /// </summary>
    /// <param name="layer">the soil layer index for which calculations will be made</param>
    /// <returns>delta N coverted into N2O during nitrification</returns>
    /// <returns></returns>
    private double N2OProducedDuringNitrification(int layer)
    {
        double result = dlt_nitrification[layer] * dnit_nitrf_loss;
        return result;
    }

    /// <summary>
    /// Calculate amount of NO3 transformed via denitrification
    /// </summary>
    /// <remarks>
    /// - parameters are given in pairs, for aerobic and anaerobic conditions (with pond)
    /// </remarks>
    /// <param name="layer">the soil layer index for which calculations will be made</param>
    /// <returns>delta N coverted from NO3 into gaseous forms</returns>
    private double Denitrification(int layer)
    {
        // Notes:
        //     Denitrification will happend whenever: 
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

        int index = 1; // denitrification calcs are not different whether there is pond or not. use 1 as default

        // get the soil temperature factor
        double stf = 1.0;
        if (usingNewFunctions)
            stf = SoilTempFactor(layer, index, TempFactorData_Denit);
        else
            stf = Math.Max(0.0, Math.Min(1.0, 0.1 * Math.Exp(0.046 * Tsoil[layer])));
        // This is an empirical dimensionless function to account for the effect of temperature.
        // The upper limit of 1.0 means that optimum denitrification temperature is 50 oC and above.  At 0 oC it is 0.1 of optimum, and at -20 oC is about 0.04.

        // get the soil water factor
        double swf = 1.0;
        if (usingNewFunctions)
            swf = SoilMoistFactor(layer, index, MoistFactorData_Denit);
        else
            swf = WFDenit(layer);

        // get available carbon from soil organic pools
        double totalC = (hum_c[layer] + fom_c[layer]) * convFactor[layer];
        double active_c = 0.0;
        if (usingNewFunctions)
            active_c = actC_parmA + actC_parmB * totalC;
        else
            active_c = 0.0031 * totalC + 24.5;
        // Note: CM V2 had active_c = fom_C_conc + 0.0031*hum_C_conc + 24.5
        // Note: Ceres wheat has active_c = 0.4* fom_C_pool1 + 0.0031 * 0.58 * hum_C_conc + 24.5

        // calculate denitrification rate  - kg/ha
        double denit_rate = dnit_rate_coeff * active_c * swf * stf * _no3[layer];

        return denit_rate;
    }

    /// <summary>
    /// Calculate the N2 to N2O ratio during denitrification
    /// </summary>
    /// <remarks>
    /// parameters are given in pairs, for aerobic and anaerobic conditions (with pond)
    /// </remarks>
    /// <param name="layer">the soil layer index for which calculations will be made</param>
    /// <returns>The ratio between N2 and N2O (0-1)</returns>
    private double Denitrification_Nratio(int layer)
    {
        //int index = 1; // denitrification calcs are not different whether there is pond or not. use 1 as default

        // the water filled pore space (%)
        double WFPS = sw_dep[layer] / sat_dep[layer] * 100.0;

        // CO2 production today (kgC/ha)
        double CO2_prod = co2_atm[layer];

        // calculate the terms for the formula from Thornburn et al (2010)
        bool didInterpolate;
        double result = 0.0;
        if (usingNewFunctions)
        {
            double CO2effect = 0.0;
            if (CO2_prod > epsilon)
                CO2effect = Math.Exp(N2N2O_parmB * (_no3[layer] / CO2_prod));
            CO2effect = Math.Max(N2N2O_parmA, CO2effect);
            double WFPSeffect = MathUtility.LinearInterpReal(WFPS, WFPSFactorData_Denit.xVals, WFPSFactorData_Denit.yVals, out didInterpolate);
            result = dnit_k1 * CO2effect * WFPSeffect;
        }
        else
        {
            double RtermA = 0.16 * dnit_k1;
            double RtermB = 0.0;
            if (CO2_prod > 0.0)
                RtermB = dnit_k1 * Math.Exp(-0.80 * (_no3[layer] / CO2_prod));
            double RtermC = 0.1;
            double RtermD = MathUtility.LinearInterpReal(WFPS, dnit_wfps, dnit_n2o_factor, out didInterpolate);
            result = Math.Max(RtermA, RtermB) * Math.Max(RtermC, RtermD);
        }

        return result;
    }

    #endregion

    #region >>  Environmental factors

    #region Old factors

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

        if (Tsoil[layer] > 0.0)
        {
            if (opt_temp[index - 1] == 0.0)
                return 0.0;
            else
                return Math.Max(0.0, Math.Min(1.0,
                    (Tsoil[layer] * Tsoil[layer]) / Math.Pow(opt_temp[index - 1], 2.0)));
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
        double t = Math.Min(Tsoil[layer], opt_temp[layer]);
        return 47.9 / (1.0 + Math.Exp(106.0 / (t + 18.3)));
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

    #endregion  old factors

    #region »  New factors

    /// <summary>
    /// Calculate a temperature factor (0-1) for C and N processes
    /// </summary>
    /// <param name="layer">The soil layer to calculate</param>
    /// <param name="index">Parameter indication whether pond exists</param>
    /// <param name="Parameters">Parameter data</param>
    /// <returns>Temperature limiting factor (0-1)</returns>
    private double SoilTempFactor(int layer, int index, BentStickData Parameters)
    {
        // + Assumptions
        //     index = 0 for aerobic conditions, 1 for anaerobic

        index -= 1;  // use this untill can change the whole code. (index used to be [1-2]
        if (index > Parameters.xValueForOptimum.Length - 1)
            throw new Exception("SoilNitrogen.SoilTempFactor - invalid value for \"index\" parameter");

        double Toptimum = Parameters.xValueForOptimum[index];
        double Fzero = Parameters.yValueAtZero[index];
        double CurveN = Parameters.CurveExponent[index];
        double AuxV = Math.Pow(Fzero, 1 / CurveN);
        double Tzero = Toptimum * AuxV / (AuxV - 1);
        double beta = 1 / (Toptimum - Tzero);

        return Math.Min(1.0, Math.Pow(beta * Math.Max(0.0, Tsoil[layer] - Tzero), CurveN));
    }

    /// <summary>
    /// Calculate a soil moist factor (0-1) for C and N processes
    /// </summary>
    /// <param name="layer">The soil layer to calculate</param>
    /// <param name="index">Parameter indication whether pond exists</param>
    /// <param name="Parameters">Parameter data</param>
    /// <returns>Soil moisture limiting factor (0-1)</returns>
    private double SoilMoistFactor(int layer, int index, BrokenStickData Parameters)
    {
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

    /// <summary>
    /// Calculate a water filled pore space factor for denitrification processes
    /// </summary>
    /// <param name="layer">The soil layer to calculate</param>
    /// <param name="index">Parameter indication whether pond exists</param>
    /// <param name="Parameters">Parameter data</param>
    /// <returns>limiting factor due to water filled pore space (0-1)</returns>
    private double WaterFilledPoreSpaceFactor(int layer, int index, BrokenStickData Parameters)
    {
        // + Assumptions
        //     index = 0 for aerobic conditions, 1 for anaerobic

        index -= 1;  // use this untill can change the whole code. (index used to be [1-2]
        if (index == 0)
        {
            bool didInterpolate;

            // get the WFPS value (%)
            double WFPS = sw_dep[layer] / sat_dep[layer] * 100.0;

            // get the WFPS factor
            return MathUtility.LinearInterpReal(WFPS, Parameters.xVals, Parameters.yVals, out didInterpolate);
        }
        else if (index == 1) // if pond is active
            return 1.0;
        else
            throw new Exception("SoilNitrogen.SoilMoistFactor - invalid value for \"index\" parameter");
    }

    /// <summary>
    /// Calculate a pH factor for C and N processes
    /// </summary>
    /// <param name="layer">The soil layer to calculate</param>
    /// <param name="index">Parameter indication whether pond exists</param>
    /// <param name="Parameters">Parameter data</param>
    /// <returns>Soil pH limiting factor (0-1)</returns>
    private double SoilpHFactor(int layer, int index, BrokenStickData Parameters)
    {
        bool DidInterpolate;
        return MathUtility.LinearInterpReal(ph[layer], Parameters.xVals, Parameters.yVals, out DidInterpolate);
    }

    /// <summary>
    /// Calculate a C:N ratio factor for C and N processes
    /// </summary>
    /// <param name="layer">The soil layer to calculate</param>
    /// <param name="index">Parameter indication whether pond exists</param>
    /// <param name="OptCN">The optimum CN ration, below which there is no limitations</param>
    /// <param name="rateCN">A rate factor to increase limitation as function of increasing CN ratio</param>
    /// <returns>The CN ratio limiting factor</returns>
    private double CNratioFactor(int layer, int index, double OptCN, double rateCN)
    {
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

    #endregion New factors

    #endregion Envmt factors

    #endregion C and N processes

    #region >>  Auxiliary functions

    /// <summary>
    /// Print a message to the summaryfile
    /// </summary>
    /// <param name="myMessage">The message to be printed below the date and module info</param>
    private void writeMessage(string myMessage)
    {
        Console.WriteLine(Clock.Today.ToString("dd MMMM yyyy") + " (Day of year=" + Clock.Today.DayOfYear.ToString() + "), SoilNitrogen:");
        Console.WriteLine("     " + myMessage);
    }

    /// <summary>
    /// Computes the fraction of each layer that is between the surface and a given depth
    /// </summary>
    /// <param name="maxDepth">The depth down to which the fractions are computed</param>
    /// <returns>An array with the fraction (0-1) of each layer that is between the surface and maxDepth</returns>
    private double[] FractionLayer(double maxDepth)
    {
        double cumDepth = 0.0;
        double[] result = new double[dlayer.Length];
        int maxLayer = getCumulativeIndex(maxDepth, dlayer);
        for (int layer = 0; layer <= maxLayer; layer++)
        {
            result[layer] = Math.Min(1.0, MathUtility.Divide(maxDepth - cumDepth, dlayer[layer], 0.0));
        }
        return result;
    }

    /// <summary>
    /// Find the index at which the cumulative amount is equal or greater than a given value
    /// </summary>
    /// <param name="sumTarget">The target value being sought</param>
    /// <param name="anArray">The array to analyse</param>
    /// <returns>The index of the array item at which the sum is equal or greater than the target</returns>
    private int getCumulativeIndex(double sumTarget, double[] anArray)
    {
        double cum = 0.0f;
        for (int i = 0; i < anArray.Length; i++)
        {
            cum += anArray[i];
            if (cum >= sumTarget)
                return i;
        }
        return anArray.Length - 1;
    }

    /// <summary>
    /// Check whether there is at least one considerable/significant value in the array
    /// </summary>
    /// <param name="anArray">The array to analyse</param>
    /// <param name="MinValue">The minimum considerable value</param>
    /// <returns>True if there is any value greater than the minimum, false otherwise</returns>
    private bool hasSignificantValues(double[] anArray, double MinValue)
    {
        bool result = false;
        if (anArray != null)
        {
            foreach (double Value in anArray)
            {
                if (Math.Abs(Value) >= MinValue)
                {
                    result = true;
                    break;
                }
            }
        }
        return result;
    }

    /// <summary>
    /// Checks whether the variable is significantly negative, consider thresholds
    /// </summary>
    /// <remarks>
    /// Three levels are considered when analying a negative value, these are defined by the warning and the fatal threshold value:
    ///  (1) If the variable is negative but the value is smaller (in absolute terms) than the warning threshold, the deviation is considered irrelevant;
    ///  (2) If the value of the variable is greater than the warning threshold, then a warning message is given, but its value is still considered OK;
    ///  (3) If the variable value is greater than the fatal threshold then a fatal error is raised and the calculation stops.
    /// </remarks>
    /// <param name="TheVariable">variable to be tested</param>
    /// <param name="layer">layer to which the variable belongs to</param>
    /// <param name="VariableName">name of the variable</param>
    /// <returns>A number from zero (value is ok), to 3, simulation cannot continue</returns>
    private byte CheckNegativeValues(double TheValue, int layer, string VariableName)
    {
        // Note: the layer number and the variable name are passed only so that they can be added to the error message

        byte result = 0;
        if (TheValue < FatalNegativeThreshold)
        {
            result = 3;
            throw new Exception("Attempt to change " + VariableName + "[" + (layer + 1).ToString() +
                "] to a value below the fatal threshold, " +
                FatalNegativeThreshold.ToString());
        }
        else if (TheValue < WarningNegativeThreshold)
        {
            result = 2;
            Console.WriteLine(Clock.Today.ToShortDateString() + " - Attempt to change " + VariableName + "[" +
                (layer + 1).ToString() + "] to a value below the warning threshold, " +
                WarningNegativeThreshold.ToString());
            Console.WriteLine("  The value will be reset to minimum value");
        }
        else if (TheValue < -epsilon)
            result = 1;

        return result;
    }

    /// <summary>
    /// Calculate the sum of all values of an array of doubles
    /// </summary>
    /// <param name="anArray">The array of values</param>
    /// <returns>The sum</returns>
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

    #endregion Aux functions
}

public class SoilTypeDefinition
{
    [Param]
    protected XmlNode SoilTypeDefinitionXML;
}