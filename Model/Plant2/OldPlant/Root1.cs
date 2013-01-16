﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CSGeneral;
using ModelFramework;

public class Root1 : BaseOrgan1, BelowGround
{
   
    #region Parameters read from XML file and links to other functions.
    [Link]
    public Plant15 Plant;

    [Link]
    public Component My;

    [Link]
    Function RootAdvanceFactorTemp = null;

    [Link]
    Function RootAdvanceFactorWaterStress = null;

    [Link]
    Function SWFactorRootDepth = null;

    [Link]
    Function SWFactorRootLength = null;

    [Link]
    Function RootDepthRate = null;

    [Link]
    Population1 Population = null;

    [Link]
    Function RelativeRootRate = null;

    [Link]
    Function DMSenescenceFraction = null;

    [Link]
    CompositeBiomass TotalLive = null;

    [Link]
    Function GrowthStructuralFractionStage = null;

    [Link]
    object NUptakeFunction = null;

    [Param]
    double NConcentrationCritical = 0;

    [Param]
    double NConcentrationMinimum = 0;

    [Param]
    double NConcentrationMaximum = 0;

    [Param]
    double InitialRootDepth = 0;

    [Param]
    double DieBackFraction = 0;

    [Param]
    private double[] cl = null;

    [Param]
    public double[] ll = null;

    [Param]
    public double[] kl = null;

    [Param]
    public double[] xf = null;

    [Param]
    public bool ModifyKL;

    [Param(IsOptional = true)]
    double ClA = 0;

    [Param(IsOptional = true)]
    double ClB = 0;

    [Param(IsOptional = true)]
    double ESPA = 0;

    [Param(IsOptional = true)]
    double ESPB = 0;

    [Param(IsOptional = true)]
    double ECA = 0;

    [Param(IsOptional = true)]
    double ECB = 0;

    [Param]
    double NDeficitUptakeFraction = 1.0;

    [Param]
    double NSenescenceConcentration = 0;

    [Param]
    string NSupplyPreference = "";

    [Param]
    double SenescenceDetachmentFraction = 0;

    [Param]
    double InitialWt = 0;

    [Param]
    double InitialNConcentration = 0;

    [Param]
    private double SpecificRootLength = 0;

    #endregion

    #region Variables we need from other modules
    [Input]
    public double[] dlayer = null;

    [Input]
    public double[] bd = null;

    [Input]
    public double[] sw_dep = null;

    [Input]
    public double[] dul_dep = null;

    [Input]
    public double[] ll15_dep = null;

    [Input]
    public double[] sat_dep = null;
    
    [Input(IsOptional = true)]
    public double[] esp = null;

    [Input(IsOptional = true)]
    public double[] ec = null;

    [Input(IsOptional = true)]
    public double[] no3 = null;

    [Input(IsOptional = true)]
    public double[] nh4 = null;

    [Input(IsOptional = true)]
    double swim3 = double.MinValue;
    #endregion

    #region Events we're going to publish at some point.
    [Event]
    public event FOMLayerDelegate IncorpFOM;

    [Event]
    public event WaterChangedDelegate WaterChanged;

    [Event]
    public event NitrogenChangedDelegate NitrogenChanged;
    #endregion

    #region Private variables
    private bool SwimIsPresent = false;
    private double[] dlt_sw_dep;
    private double[] sw_avail;
    private double[] sw_avail_pot;
    private double[] sw_supply;
    private double[] dlt_no3gsm;
    private double[] dlt_nh4gsm;
    private double[] no3gsm_uptake_pot;
    private double[] nh4gsm_uptake_pot;
    private double dltRootDepth;
    private double[] dltRootLength;
    private double[] dltRootLengthSenesced;
    private double[] dltRootLengthDead;
    private double[] ll_dep;
    private double[] RootLength;
    private double[] no3gsm_min;
    private double[] nh4gsm_min;
    private bool HaveModifiedKLValues = false;
    private double[] RootLengthSenesced;
    private double dlt_n_senesced_retrans;           // plant N retranslocated to/from (+/-) senesced part to/from <<somewhere else??>> (g/m^2)
    private double dlt_n_senesced_trans;


    private double _DMGreenDemand;
    private double _NDemand;
    private double _SoilNDemand;
    private double NMax; 
    private double sw_demand;
    private double n_conc_crit = 0;
    private double n_conc_max = 0;
    private double n_conc_min = 0;
    #endregion

    #region Public interface defined by Organ1
    public override string Name { get { return My.Name; } }
    public override Biomass Senescing { get; protected set; }
    public override Biomass Retranslocation { get; protected set; }
    public override Biomass Growth { get; protected set; }
    public override Biomass Detaching { get; protected set; }

    // Soil water
    public override double SWSupply { get { return MathUtility.Sum(sw_supply); } }
    public override double SWDemand { get { return sw_demand; } }
    public override double SWUptake { get { return -MathUtility.Sum(dlt_sw_dep); } }
    public override void DoSWDemand(double Supply) { }
    public override void DoSWUptake(double SWDemand)
    {
        // Firstly grow roots.
        //  the layer with root front
        int layer = FindLayerNo(RootDepth);

        dltRootDepth = RootDepthRate.Value * RootAdvanceFactorTemp.Value *
                        Math.Min(RootAdvanceFactorWaterStress.Value, SWFactorRootDepth.Value) *
                        xf[layer];

        // prevent roots partially entering layers where xf == 0
        int deepest_layer;
        for (deepest_layer = xf.Length - 1;
            deepest_layer >= 0 &&
            (xf[deepest_layer] <= 0.0 || getModifiedKL(deepest_layer) <= 0.0);
            deepest_layer--)
            ; /* nothing */

        int RootLayerMax = deepest_layer + 1;
        double RootDepthMax = MathUtility.Sum(dlayer, 0, deepest_layer + 1, 0.0);
        dltRootDepth = MathUtility.Constrain(dltRootDepth, double.MinValue, RootDepthMax - RootDepth);

        if (dltRootDepth < 0.0)
            throw new Exception("negative root growth??");

        Util.Debug("Root.dltRootDepth=%f", dltRootDepth);
        Util.Debug("Root.root_layer_max=%i", RootLayerMax);
        Util.Debug("Root.root_depth_max=%f", RootDepthMax);

        // potential extractable sw
        DoPotentialExtractableSW();

        // actual extractable sw (sw-ll)
        DoSWAvailable();

        DoSWSupply();
    
        if (SwimIsPresent)
        {
            My.Get("uptake_water_" + Plant.CropType, out dlt_sw_dep);
            dlt_sw_dep = MathUtility.Multiply_Value(dlt_sw_dep, -1);   // make them negative numbers.
        }
        else
            DoWaterUptakeInternal(SWDemand);
        Util.Debug("Root.dlt_sw_dep=%f", MathUtility.Sum(dlt_sw_dep));
    }


    // dry matter
    public override double DMSupply { get { return 0.0; } }
    public override double DMRetransSupply { get { return 0; } }
    public override double dltDmPotRue { get { return 0.0; } }
    public override double DMGreenDemand { get { return _DMGreenDemand; } }
    public override double DMDemandDifferential { get { return 0; } }
    public override void DoDMDemand(double DMSupply)
    {
        _DMGreenDemand = Math.Max(0.0, DMSupply);   //Just ask for all you can get for now - NIH.
    }
    public override void DoDmRetranslocate(double dlt_dm_retrans_to_fruit, double demand_differential_begin) { }
    public override void GiveDmGreen(double Delta)
    {
        Growth.StructuralWt += Delta * GrowthStructuralFractionStage.Value;
        Growth.NonStructuralWt += Delta * (1.0 - GrowthStructuralFractionStage.Value);
        Util.Debug("Root.Growth.StructuralWt=%f", Growth.StructuralWt);
        Util.Debug("Root.Growth.NonStructuralWt=%f", Growth.NonStructuralWt);
    }
    public override void DoSenescence()
    {
        double fraction_senescing = MathUtility.Constrain(DMSenescenceFraction.Value, 0.0, 1.0);

        Senescing.StructuralWt = (Live.StructuralWt + Growth.StructuralWt + Retranslocation.StructuralWt) * fraction_senescing;
        Senescing.NonStructuralWt = (Live.NonStructuralWt + Growth.NonStructuralWt + Retranslocation.NonStructuralWt) * fraction_senescing;
        Util.Debug("Root.Senescing.StructuralWt=%f", Senescing.StructuralWt);
        Util.Debug("Root.Senescing.NonStructuralWt=%f", Senescing.NonStructuralWt);
    }
    public override void DoDetachment()
    {
        Detaching = Dead * SenescenceDetachmentFraction;
        Util.Debug("Root.Detaching.Wt=%f", Detaching.Wt);
        Util.Debug("Root.Detaching.N=%f", Detaching.N);
    }

    // nitrogen
    [Output]
    public override double NDemand { get { return _NDemand; } }
    [Output]
    public override double NSupply
    {
        get
        {
            int deepest_layer = FindLayerNo(RootDepth);
            return MathUtility.Sum(no3gsm_uptake_pot, 0, deepest_layer + 1, 0) +
                   MathUtility.Sum(nh4gsm_uptake_pot, 0, deepest_layer + 1, 0);
        }
    }
    public override double NUptake
    {
        get
        {
            int deepest_layer = FindLayerNo(RootDepth);
            return -MathUtility.Sum(dlt_no3gsm, 0, deepest_layer + 1, 0)
                    - MathUtility.Sum(dlt_nh4gsm, 0, deepest_layer + 1, 0);
        }
    }
    public override double SoilNDemand { get { return _SoilNDemand; } }
    public override double NCapacity
    {
        get
        {
            return MathUtility.Constrain(NMax - NDemand, 0.0, double.MaxValue);
        }
    }
    public override double NDemandDifferential { get { return MathUtility.Constrain(NDemand - Growth.N, 0.0, double.MaxValue); } }
    public override double AvailableRetranslocateN
    {
        get
        {
            double N_min = n_conc_min * Live.Wt;
            double N_avail = MathUtility.Constrain(Live.N - N_min, 0.0, double.MaxValue);
            double n_retrans_fraction = 1.0;
            return (N_avail * n_retrans_fraction);
        }
    }
    public override double DltNSenescedRetrans { get { return dlt_n_senesced_retrans; } }
    public override void DoNDemand(bool IncludeRetransloation)
    {

        double TopsDMSupply = 0;
        double TopsDltDmPotRue = 0;
        foreach (Organ1 Organ in Plant.Tops)
        {
            TopsDMSupply += Organ.DMSupply;
            TopsDltDmPotRue += Organ.dltDmPotRue;
        }

        if (IncludeRetransloation)
            Util.CalcNDemand(TopsDMSupply, TopsDltDmPotRue, n_conc_crit, n_conc_max, Growth, Live, Retranslocation.N, NDeficitUptakeFraction,
                      ref _NDemand, ref NMax);
        else
            Util.CalcNDemand(TopsDMSupply, TopsDltDmPotRue, n_conc_crit, n_conc_max, Growth, Live, 0.0, NDeficitUptakeFraction,
                      ref _NDemand, ref NMax);
        Util.Debug("Root.NDemand=%f", _NDemand);
        Util.Debug("Root.NMax=%f", NMax);
    }
    public override void DoNDemand1Pot(double dltDmPotRue)
    {
        Biomass OldGrowth = Growth;
        Growth.StructuralWt = dltDmPotRue * MathUtility.Divide(Live.Wt, TotalLive.Wt, 0.0);
        Util.Debug("Root.Growth.StructuralWt=%f", Growth.StructuralWt);
        Util.CalcNDemand(dltDmPotRue, dltDmPotRue, n_conc_crit, n_conc_max, Growth, Live, Retranslocation.N, 1.0,
                   ref _NDemand, ref NMax);
        Growth.StructuralWt = 0.0;
        Growth.NonStructuralWt = 0.0;
        Util.Debug("Root.NDemand=%f", _NDemand);
        Util.Debug("Root.NMax=%f", NMax);
    }
    public override void DoSoilNDemand()
    {
        _SoilNDemand = NDemand - dlt_n_senesced_retrans;
        _SoilNDemand = MathUtility.Constrain(_SoilNDemand, 0.0, double.MaxValue);
        Util.Debug("Root.SoilNDemand=%f", _SoilNDemand);
    }
    public override void DoNSupply()
    {
        if (NUptakeFunction is NUptake3)
        {
            double[] no3gsm = MathUtility.Multiply_Value(no3, Conversions.kg2gm / Conversions.ha2sm); 
            double[] nh4gsm = MathUtility.Multiply_Value(nh4, Conversions.kg2gm / Conversions.ha2sm);

            (NUptakeFunction as NUptake3).DoNUptake(RootDepth, no3gsm, nh4gsm,
                                             bd, dlayer, sw_avail, sw_avail_pot, no3gsm_min, nh4gsm_min,
                                             ref no3gsm_uptake_pot, ref nh4gsm_uptake_pot);
        }
        else
            throw new NotImplementedException();
    }

    public override void DoNRetranslocate(double NSupply, double GrainNDemand)
    {
        if (GrainNDemand >= NSupply)
        {
            // demand greater than or equal to supply
            // retranslocate all available N
            Retranslocation.StructuralN = AvailableRetranslocateN;
        }
        else
        {
            // supply greater than demand.
            // Retranslocate what is needed
            Retranslocation.StructuralN = GrainNDemand * MathUtility.Divide(AvailableRetranslocateN, NSupply, 0.0);
        }
        Util.Debug("Root.Retranslocation.N=%f", Retranslocation.N);
    }
    public override void DoNSenescence()
    {
        double green_n_conc = MathUtility.Divide(Live.N, Live.Wt, 0.0);
        double dlt_n_in_senescing_part = Senescing.Wt * green_n_conc;
        double sen_n_conc = Math.Min(NSenescenceConcentration, green_n_conc);

        double SenescingN = Senescing.Wt * sen_n_conc;
        Senescing.StructuralN = MathUtility.Constrain(SenescingN, double.MinValue, Live.N);

        dlt_n_senesced_trans = dlt_n_in_senescing_part - Senescing.N;
        dlt_n_senesced_trans = MathUtility.Constrain(dlt_n_senesced_trans, 0.0, double.MaxValue);

        Util.Debug("Root.SenescingN=%f", SenescingN);
        Util.Debug("Root.dlt.n_senesced_trans=%f", dlt_n_senesced_trans);
    }
    public override void DoNSenescedRetranslocation(double navail, double n_demand_tot)
    {
        dlt_n_senesced_retrans = navail * MathUtility.Divide(NDemand, n_demand_tot, 0.0);
        Util.Debug("Root.dlt.n_senesced_retrans=%f", dlt_n_senesced_retrans);
    }
    public override void DoNPartition(double GrowthN)
    {
        Growth.StructuralN = GrowthN;
    }
    public override void DoNFixRetranslocate(double NFixUptake, double nFixDemandTotal)
    {
        Growth.StructuralN += NFixUptake * MathUtility.Divide(NDemandDifferential, nFixDemandTotal, 0.0);
    }
    public override void DoNConccentrationLimits()
    {
        n_conc_crit = NConcentrationCritical;
        n_conc_min = NConcentrationMinimum;
        n_conc_max = NConcentrationMaximum;
        Util.Debug("Root.n_conc_crit=%f", n_conc_crit);
        Util.Debug("Root.n_conc_min=%f", n_conc_min);
        Util.Debug("Root.n_conc_max=%f", n_conc_max);
    }
    public override void ZeroDltNSenescedTrans()
    {
        dlt_n_senesced_trans = 0;
    }
    public override void DoNUptake(double PotNFix)
    {
        //if (SwimIsPresent)
        //{
        //    My.Get("uptake_no3_" + Plant.CropType, out dlt_no3gsm);
        //    MathUtility.Multiply_Value(dlt_no3gsm, -Conversions.kg2gm/Conversions.ha2sm);   // convert units and make them negative.
        //}
        //else

        double n_demand = 0.0;
        foreach (Organ1 Organ in Plant.Organ1s)
            n_demand += Organ.SoilNDemand;

        int deepest_layer = FindLayerNo(RootDepth);

        double ngsm_supply = MathUtility.Sum(no3gsm_uptake_pot, 0, deepest_layer + 1, 0)
                           + MathUtility.Sum(nh4gsm_uptake_pot, 0, deepest_layer + 1, 0);


        if (NSupplyPreference == "fixation")
            n_demand = MathUtility.Constrain(n_demand - PotNFix, 0.0, double.MaxValue);

        // get actual change in N contents
        Util.ZeroArray(dlt_no3gsm);
        Util.ZeroArray(dlt_nh4gsm);

        double scalef;
        if (n_demand > ngsm_supply)
        {
            scalef = 0.99999f;      // avoid taking it all up as it can
            // cause rounding errors to take
            // no3 below zero.
        }
        else
            scalef = MathUtility.Divide(n_demand, ngsm_supply, 0.0);

        for (int layer = 0; layer <= deepest_layer; layer++)
        {
            // allocate nitrate
            double no3gsm_uptake = no3gsm_uptake_pot[layer] * scalef;
            dlt_no3gsm[layer] = -no3gsm_uptake;

            // allocate ammonium
            double nh4gsm_uptake = nh4gsm_uptake_pot[layer] * scalef;
            dlt_nh4gsm[layer] = -nh4gsm_uptake;
        }

        Util.Debug("Root.dlt_no3gsm=%f", MathUtility.Sum(dlt_no3gsm));
        Util.Debug("Root.dlt_nh4gsm=%f", MathUtility.Sum(dlt_nh4gsm));
    }


    // cover
    public override double CoverGreen { get { return 0; } protected set { } }
    public override double CoverSen { get { return 0; } protected set { } }
    public override void DoPotentialRUE() { }
    public override double interceptRadiation(double incomingSolarRadiation) { return 0; }
    public override void DoCover() { }

    // update
    public override void Update()
    {
        // send off detached roots before root structure is updated by plant death
        DisposeDetachedMaterial(Detaching, RootLength);

        Live = Live + Growth - Senescing;

        Dead = Dead - Detaching + Senescing;
        Live = Live + Retranslocation;
        Live.StructuralN = Live.N + dlt_n_senesced_retrans;

        Biomass dying = Live * Population.DyingFractionPlants;
        Live = Live - dying;
        Dead = Dead + dying;
        Senescing = Senescing + dying;

        Util.Debug("Root.Green.Wt=%f", Live.Wt);
        Util.Debug("Root.Green.N=%f", Live.N);
        Util.Debug("Root.Senesced.Wt=%f", Dead.Wt);
        Util.Debug("Root.Senesced.N=%f", Dead.N);
        Util.Debug("Root.Senescing.Wt=%f", Senescing.Wt);
        Util.Debug("Root.Senescing.N=%f", Senescing.N);

        RootDepth += dltRootDepth;

        for (int layer = 0; layer < dlayer.Length; layer++)
            RootLength[layer] += dltRootLength[layer];

        for (int layer = 0; layer < dlayer.Length; layer++)
        {
            RootLength[layer] -= dltRootLengthSenesced[layer];
            RootLengthSenesced[layer] += dltRootLengthSenesced[layer];
        }
        // Note that movement and detachment of C is already done, just
        // need to maintain relationship between length and mass
        // Note that this is not entirely accurate.  It links live root
        // weight with root length and so thereafter dead(and detaching)
        // root is assumed to have the same distribution as live roots.
        for (int layer = 0; layer < dlayer.Length; layer++)
        {
            dltRootLengthDead[layer] = RootLength[layer] * Population.DyingFractionPlants;
            RootLength[layer] -= dltRootLengthDead[layer];
            RootLengthSenesced[layer] += dltRootLengthDead[layer];
        }

        double CumDepth = MathUtility.Sum(dlayer);
        if (RootDepth < 0 || RootDepth > CumDepth)
            throw new Exception("Invalid root depth: " + RootDepth.ToString());

        Util.Debug("root.RootDepth=%f", RootDepth);
        Util.Debug("root.RootLength=%f", MathUtility.Sum(RootLength));
        Util.Debug("root.RootLengthSenesced=%f", MathUtility.Sum(RootLengthSenesced));

        UpdateWaterAndNBalance();
    }

    #endregion

    #region Public interface specific to Root
    [Output("RootDepth")]
    public double RootDepth = 0;
    public double SWAvailRatio
    {
        get
        {
            if (MathUtility.Sum(sw_avail_pot) == 0)
                return 1.0;
            else
                return MathUtility.Sum(MathUtility.Divide(sw_avail, sw_avail_pot));
        }
    }
    public double WetRootFraction
    {
        get
        {
            if (RootDepth > 0.0)
            {
                double[] RootFr = RootDist(1.0, RootLength);

                double wet_root_fr = 0.0;
                for (int layer = 0; layer != RootFr.Length; layer++)
                    wet_root_fr = wet_root_fr + WFPS(layer) * RootFr[layer];
                return wet_root_fr;
            }
            else
                return 0.0;
        }
    }
    double[] FASWLayered
    {
        get
        {
            double[] FASW = new double[dlayer.Length];
            for (int i = 0; i < dlayer.Length; i++)
            {
                FASW[i] = MathUtility.Divide(sw_dep[i] - ll_dep[i], dul_dep[i] - ll_dep[i], 0.0);
                FASW[i] = MathUtility.Constrain(FASW[i], 0.0, 1.0);
            }
            return FASW;
        }
    }
    public double FASW
    {
        get
        {
            //  the layer with root front
            int layer = FindLayerNo(RootDepth);
            int deepest_layer = dlayer.Length - 1;

            double CumDepth = MathUtility.Sum(dlayer, 0, layer + 1, 0.0);

            double rootdepth_in_layer = dlayer[layer] - (CumDepth - RootDepth);
            rootdepth_in_layer = MathUtility.Constrain(rootdepth_in_layer, 0.0, dlayer[layer]);

            double weighting_factor = MathUtility.Divide(rootdepth_in_layer, dlayer[layer], 0.0);
            int next_layer = Math.Min(layer + 1, deepest_layer);

            double fasw1 = FASWLayered[layer];
            double fasw2 = FASWLayered[next_layer];

            fasw1 = Math.Min(1.0, Math.Max(0.0, fasw1));
            fasw2 = Math.Min(1.0, Math.Max(0.0, fasw2));

            return weighting_factor * fasw2 + (1.0 - weighting_factor) * fasw1;
        }
    }
    /// <summary>
    /// Root length density - needed by SWIM
    /// </summary>
    [Output("RootLengthDensity")]
    [Units("mm/mm^3")]
    public double[] RootLengthDensity { get { return MathUtility.Divide(RootLength, dlayer); } }
    /// <summary>
    /// Calculate the extractable soil water in the root zone (mm).
    /// </summary>
    internal double ESWInRootZone
    {
        get
        {
            double ESW = 0;
            int deepest_layer = FindLayerNo(RootDepth);
            for (int layer = 0; layer <= deepest_layer; layer++)
                ESW += MathUtility.Constrain(sw_dep[layer] - ll_dep[layer], 0.0, double.MaxValue);
            return ESW;
        }
    }

    /// <summary>
    /// Return the index of the layer corresponding to the given depth
    /// </summary>
    internal int FindLayerNo(double depth)
    {
        int i;
        double progressive_sum = 0.0;

        for (i = 0; i < dlayer.Length; i++)
        {
            progressive_sum = progressive_sum + dlayer[i];
            if (progressive_sum >= depth)
                break;
        }
        if (i != 0 && i == dlayer.Length)
            return (i - 1); // last element in array
        return i;
    }

    /// <summary>
    /// Calculate the increase in root length density in each rooted
    /// layer based upon soil hospitality, moisture and fraction of
    /// layer explored by roots.
    /// </summary>
    internal void RootLengthGrowth()
    {
        Util.ZeroArray(dltRootLength);

        double depth_today = RootDepth + dltRootDepth;
        int deepest_layer = FindLayerNo(depth_today);

        double[] rlv_factor = new double[dlayer.Length];    // relative rooting factor for all layers


        double rlv_factor_tot = 0.0;
        for (int layer = 0; layer <= deepest_layer; layer++)
        {
            double branching_factor = RelativeRootRate.Values[layer];

            rlv_factor[layer] = SWFactorRootLength.Values[layer] *
                                branching_factor *                                   // branching factor
                                xf[layer] *                                          // growth factor
                                MathUtility.Divide(dlayer[layer], RootDepth, 0.0);   // space weighting factor

            rlv_factor[layer] = MathUtility.Constrain(rlv_factor[layer], 1e-6, double.MaxValue);
            rlv_factor_tot += rlv_factor[layer];
        }

        double dlt_length_tot = Growth.Wt / Conversions.sm2smm * SpecificRootLength;

        for (int layer = 0; layer <= deepest_layer; layer++)
            dltRootLength[layer] = dlt_length_tot * MathUtility.Divide(rlv_factor[layer], rlv_factor_tot, 0.0);
        Util.Debug("Root.dltRootLength=%f", MathUtility.Sum(dltRootLength));
    }

    /// <summary>
    /// Calculate root length senescence based upon changes in senesced root
    /// biomass and the specific root length.
    /// </summary>
    internal void DoSenescenceLength()
    {
        Util.ZeroArray(dltRootLengthSenesced);
        double senesced_length = Senescing.Wt / Conversions.sm2smm * SpecificRootLength;
        dltRootLengthSenesced = RootDist(senesced_length, RootLength);
        Util.Debug("Root.dltRootLengthSenesced=%f", MathUtility.Sum(dltRootLengthSenesced));
    }

    /// <summary>
    /// Write a summary to the summary file.
    /// </summary>
    internal void WriteSummary()
    {
        Console.WriteLine("                        Root Profile");
        Console.WriteLine("         -----------------------------------------------");
        Console.WriteLine("          Layer       Kl           Lower    Exploration");
        Console.WriteLine("          Depth     Factor         Limit      Factor");
        Console.WriteLine("          (mm)         ()        (mm/mm)       (0-1)");
        Console.WriteLine("         -----------------------------------------------");

        double dep_tot, esw_tot;                      // total depth of soil & ll

        dep_tot = esw_tot = 0.0;
        for (int layer = 0; layer < dlayer.Length; layer++)
        {
            Console.WriteLine(string.Format("     {0,9:F1}{1,10:F3}{2,15:F3}{3,12:F3}",
                              dlayer[layer],
                              getModifiedKL(layer),
                              MathUtility.Divide(ll_dep[layer], dlayer[layer], 0.0),
                              xf[layer]));
            dep_tot += dlayer[layer];
            esw_tot += dul_dep[layer] - ll_dep[layer];
        }
        Console.WriteLine("         -----------------------------------------------");
        if (HaveModifiedKLValues)
            Console.WriteLine("         **** KL's have been modified using either CL, EC or ESP values.");

        Console.WriteLine(string.Format("         Extractable SW: {0,5:F0}mm in {1,5:F0}mm total depth ({2,3:F0}%).",
                                        esw_tot,
                                        dep_tot,
                                        Conversions.fract2pcnt * MathUtility.Divide(esw_tot, dep_tot, 0.0)));
    }

    #endregion

    #region Event handlers
    [EventHandler]
    public void OnInitialised()
    {
        SwimIsPresent = swim3 > 0;
        if (SwimIsPresent)
            Console.WriteLine("Using SWIM3 for Soil Water Uptake.");

        Senescing = new Biomass();
        Retranslocation = new Biomass();
        Growth = new Biomass();
        Detaching = new Biomass();
        dlt_sw_dep = new double[dlayer.Length];
        sw_avail = new double[dlayer.Length];
        sw_avail_pot = new double[dlayer.Length];
        sw_supply = new double[dlayer.Length];

        dlt_no3gsm = new double[dlayer.Length];
        dlt_nh4gsm = new double[dlayer.Length];
        no3gsm_uptake_pot = new double[dlayer.Length];
        nh4gsm_uptake_pot = new double[dlayer.Length];
        dltRootLength = new double[dlayer.Length];
        dltRootLengthSenesced = new double[dlayer.Length];
        dltRootLengthDead = new double[dlayer.Length];
        no3gsm_min = new double[dlayer.Length];
        nh4gsm_min = new double[dlayer.Length];
        RootLength = new double[dlayer.Length];
        RootLengthSenesced = new double[dlayer.Length];

        ll_dep = MathUtility.Multiply(ll, dlayer);
        Util.ZeroArray(no3gsm_min);
        Util.ZeroArray(nh4gsm_min);
    }
    public override void OnPrepare()
    {
        Growth.Clear();
        Senescing.Clear();
        Detaching.Clear();
        Retranslocation.Clear();

        dlt_n_senesced_retrans = 0.0;
        dlt_n_senesced_trans = 0.0;

        _DMGreenDemand = 0.0;
        _NDemand = 0.0;
        _SoilNDemand = 0.0;
        NMax = 0.0;
        sw_demand = 0.0; dltRootDepth = 0.0;
        Util.ZeroArray(dltRootLength);
        Util.ZeroArray(dltRootLengthSenesced);
        Util.ZeroArray(dltRootLengthDead);
        Util.ZeroArray(dlt_sw_dep);
        Util.ZeroArray(sw_avail);
        Util.ZeroArray(sw_avail_pot);
        Util.ZeroArray(sw_supply);
        Util.ZeroArray(dlt_no3gsm);
        Util.ZeroArray(dlt_nh4gsm);
        Util.ZeroArray(no3gsm_uptake_pot);
        Util.ZeroArray(nh4gsm_uptake_pot);
    }
    public override void OnHarvest(HarvestType Harvest, BiomassRemovedType BiomassRemoved)
    {
        Biomass Dead;
        Dead = Live * DieBackFraction;

        // however dead roots have a given N concentration
        Dead.StructuralN = Dead.Wt * NSenescenceConcentration;

        Live = Live - Dead;
        Dead = Dead + Dead;

        int i = Util.IncreaseSizeOfBiomassRemoved(BiomassRemoved);

        // Unlike above ground parts, no roots go to surface residue module.
        BiomassRemoved.dm_type[i] = Name;
        BiomassRemoved.fraction_to_residue[i] = 0.0F;
        BiomassRemoved.dlt_crop_dm[i] = 0.0F;
        BiomassRemoved.dlt_dm_n[i] = 0.0F;
        BiomassRemoved.dlt_dm_p[i] = 0.0F;
    }
    public override void OnEndCrop(BiomassRemovedType BiomassRemoved)
    {
        DisposeDetachedMaterial(Live, RootLength);
        DisposeDetachedMaterial(Dead, RootLengthSenesced);
        Dead.Clear();
        Live.Clear();
    }

    [EventHandler]
    public void OnPhaseChanged(PhaseChangedType PhenologyChange)
    {
        if (PhenologyChange.NewPhaseName == "GerminationToEmergence")
            RootDepth = InitialRootDepth;
        else if (PhenologyChange.NewPhaseName == "EmergenceToEndOfJuvenile")
        {
            Live.StructuralWt = InitialWt * Population.Density;
            Live.StructuralN = InitialNConcentration * Live.StructuralWt;

            // initial root length (mm/mm^2)
            double initial_root_length = Live.Wt / Conversions.sm2smm * SpecificRootLength;

            // initial root length density (mm/mm^3)
            double rld = MathUtility.Divide(initial_root_length, RootDepth, 0.0);

            int deepest_layer = FindLayerNo(RootDepth);

            for (int layer = 0; layer <= deepest_layer; layer++)
                RootLength[layer] = rld * dlayer[layer] * RootProportion(layer, RootDepth);

            Util.Debug("Root.InitGreen.StructuralWt=%f", Live.StructuralWt);
            Util.Debug("Root.InitGreen.StructuralN=%f", Live.StructuralN);
            Util.Debug("Root.InitRootLength=%f", MathUtility.Sum(RootLength));
        }
    }

    #endregion

    #region Private functionality

    /// <summary>
    /// Dispose of detached material from dead & senesced roots into FOM pool
    /// </summary>
    private void DisposeDetachedMaterial(Biomass BiomassToDisposeOf, double[] RootLength)
    {
        if (BiomassToDisposeOf.Wt > 0.0)
        {
            // DM
            double[] dlt_dm_incorp = RootDist(BiomassToDisposeOf.Wt * Conversions.gm2kg / Conversions.sm2ha, RootLength);

            // Nitrogen
            double[] dlt_N_incorp = RootDist(BiomassToDisposeOf.N * Conversions.gm2kg / Conversions.sm2ha, RootLength);

            // Phosporous
            //double[] dlt_P_incorp = RootDist(BiomassToDisposeOf.P * Conversions.gm2kg / Conversions.sm2ha);

            FOMLayerType IncorpFOMData = new FOMLayerType();
            IncorpFOMData.Type = Plant.CropType;
            Util.Debug("Root.IncorpFOM.Type=%s", IncorpFOMData.Type.ToLower());
            IncorpFOMData.Layer = new FOMLayerLayerType[dlt_dm_incorp.Length];
            for (int i = 0; i != dlt_dm_incorp.Length; i++)
            {
                IncorpFOMData.Layer[i] = new FOMLayerLayerType();
                IncorpFOMData.Layer[i].FOM = new FOMType();
                IncorpFOMData.Layer[i].FOM.amount = (float)dlt_dm_incorp[i];
                IncorpFOMData.Layer[i].FOM.N = (float)dlt_N_incorp[i];
                //IncorpFOMData.Layer[i].FOM.P = (float)dlt_P_incorp[i];
                IncorpFOMData.Layer[i].FOM.C = (float)0.0;
                IncorpFOMData.Layer[i].FOM.AshAlk = (float)0.0;
                IncorpFOMData.Layer[i].CNR = 0;
                IncorpFOMData.Layer[i].LabileP = 0;
                Util.Debug("Root.IncorpFOM.FOM.amount=%f2", IncorpFOMData.Layer[i].FOM.amount);
                Util.Debug("Root.IncorpFOM.FOM.N=%f", IncorpFOMData.Layer[i].FOM.N);

            }
            IncorpFOM.Invoke(IncorpFOMData);
        }
        else
        {
            // no roots to incorporate
        }
    }

    /// <summary>
    /// Calculate a modified KL value as per:
    ///    Hochman et. al. (2007) Simulating the effects of saline and sodic subsoils on wheat
    ///       crops growing on Vertosols. Australian Journal of Agricultural Research, 58, 802–810
    /// Will use one of CL, ESP and EC in that order to modified KL.
    /// </summary>
    private double getModifiedKL(int i)
    {
        if (ModifyKL)
        {
            double KLFactor = 1.0;
            if (cl != null && cl.Length > 1)
                KLFactor = Math.Min(1.0, ClA * Math.Exp(ClB * cl[i]));

            else if (esp.Length > 1)
                KLFactor = Math.Min(1.0, ESPA * Math.Exp(ESPB * esp[i]));

            else if (ec.Length > 1)
                KLFactor = Math.Min(1.0, ECA * Math.Exp(ECB * ec[i]));

            if (KLFactor != 1.0)
                HaveModifiedKLValues = true;

            return kl[i] * KLFactor;
        }
        else
            return kl[i];
    }

    /// <summary>
    /// Return potential available soil water from each layer in the root zone.
    /// </summary>
    private void DoPotentialExtractableSW()
    {
        Util.ZeroArray(sw_avail_pot);

        int deepest_layer = FindLayerNo(RootDepth);
        for (int layer = 0; layer <= deepest_layer; layer++)
            sw_avail_pot[layer] = dul_dep[layer] - ll_dep[layer];

        // correct bottom layer for actual root penetration
        sw_avail_pot[deepest_layer] = sw_avail_pot[deepest_layer] * RootProportion(deepest_layer, RootDepth);

        Util.Debug("Root.deepest_layer=%i", deepest_layer);
        Util.Debug("Root.sw_avail_pot[deepest_layer]=%f", sw_avail_pot[deepest_layer]);
    }

    /// <summary>
    /// Return actual water available for extraction from each layer in the
    /// soil profile by the crop (mm water)
    /// </summary>
    private void DoSWAvailable()
    {
        Util.ZeroArray(sw_avail);

        int deepest_layer = FindLayerNo(RootDepth);
        for (int layer = 0; layer <= deepest_layer; layer++)
        {
            sw_avail[layer] = sw_dep[layer] - ll_dep[layer];
            sw_avail[layer] = MathUtility.Constrain(sw_avail[layer], 0.0, double.MaxValue);
        }
        // correct bottom layer for actual root penetration
        sw_avail[deepest_layer] = sw_avail[deepest_layer] * RootProportion(deepest_layer, RootDepth);
        Util.Debug("Root.sw_avail[deepest_layer]=%f", sw_avail[deepest_layer]);
    }

    /// <summary>
    /// Return potential water uptake from each layer of the soil profile
    /// by the crop (mm water). This represents the maximum amount in each
    /// layer regardless of lateral root distribution but takes account of
    /// root depth in bottom layer.
    /// </summary>
    private void DoSWSupply()
    {
        Util.ZeroArray(sw_supply);

        int deepest_layer = FindLayerNo(RootDepth);
        double sw_avail;
        for (int i = 0; i <= deepest_layer; i++)
        {
            sw_avail = (sw_dep[i] - ll_dep[i]);
            sw_supply[i] = sw_avail * getModifiedKL(i);
            sw_supply[i] = MathUtility.Constrain(sw_supply[i], 0.0, double.MaxValue);
        }
        //now adjust bottom layer for depth of root
        sw_supply[deepest_layer] = sw_supply[deepest_layer] * RootProportion(deepest_layer, RootDepth);
        Util.Debug("Root.sw_supply[deepest_layer]=%f", sw_supply[deepest_layer]);
    }

    /// <summary>
    /// Calculate todays daily water uptake by this root system
    /// </summary>
    private void DoWaterUptakeInternal(double sw_demand)
    {
        int deepest_layer = FindLayerNo(RootDepth);
        double sw_supply_sum = MathUtility.Sum(sw_supply, 0, deepest_layer + 1, 0.0);

        if ((sw_supply_sum < 0.0) || (sw_demand < 0.0))
        {
            //we have no uptake - there is no demand or potential
            Util.ZeroArray(dlt_sw_dep);
        }
        else
        {
            // get actual uptake
            Util.ZeroArray(dlt_sw_dep);
            if (sw_demand < sw_supply_sum)
            {
                // demand is less than what roots could take up.
                // water is non-limiting.
                // distribute demand proportionately in all layers.
                for (int layer = 0; layer <= deepest_layer; layer++)
                {
                    dlt_sw_dep[layer] = -1.0 * MathUtility.Divide(sw_supply[layer], sw_supply_sum, 0.0) * sw_demand;
                }
            }
            else
            {
                // water is limiting - not enough to meet demand so take
                // what is available (potential)
                for (int layer = 0; layer <= deepest_layer; layer++)
                {
                    dlt_sw_dep[layer] = -1 * sw_supply[layer];
                }
            }
        }
    }

    /// <summary>
    /// Returns the proportion of layer that has roots in it (0-1).
    ///     Each element of "dlayr" holds the height of  the
    ///     corresponding soil layer.  The height of the top layer is
    ///     held in "dlayr"(1), and the rest follow in sequence down
    ///     into the soil profile.  Given a root depth of "root_depth",
    ///     this function will return the proportion of "dlayr"("layer")
    ///     which has roots in it  (a value in the range 0..1).
    /// </summary>
    private double RootProportion(int layer, double RootDepth)
    {
        double depth_to_layer_bottom = MathUtility.Sum(dlayer, 0, layer + 1, 0.0);
        double depth_to_layer_top = depth_to_layer_bottom - dlayer[layer];
        double depth_to_root = Math.Min(depth_to_layer_bottom, RootDepth);
        double depth_of_root_in_layer = Math.Max(0.0, depth_to_root - depth_to_layer_top);
        return (MathUtility.Divide(depth_of_root_in_layer, dlayer[layer], 0.0));
    }

    /// <summary>
    /// Distribute root material over profile based upon root
    /// length distribution.
    /// </summary>
    private double[] RootDist(double root_sum, double[] RootLength)
    {
        int deepest_layer = FindLayerNo(RootDepth);

        double root_length_sum = MathUtility.Sum(RootLength, 0, deepest_layer + 1, 0.0);

        double[] RootArray = new double[RootLength.Length];
        for (int layer = 0; layer <= deepest_layer; layer++)
            RootArray[layer] = root_sum * MathUtility.Divide(RootLength[layer], root_length_sum, 0.0);
        return RootArray;
    }

    /// <summary>
    /// 
    /// </summary>
    private double WFPS(int layer)
    {
        double wfps = MathUtility.Divide(sw_dep[layer] - ll15_dep[layer],
                                        sat_dep[layer] - ll15_dep[layer], 0.0);
        return MathUtility.Constrain(wfps, 0.0, 1.0);
    }

    /// <summary>
    /// Update the water and N balance.
    /// </summary>
    private void UpdateWaterAndNBalance()
    {
        NitrogenChangedType NitrogenUptake = new NitrogenChangedType();
        NitrogenUptake.DeltaNO3 = MathUtility.Multiply_Value(dlt_no3gsm, Conversions.gm2kg / Conversions.sm2ha);
        NitrogenUptake.DeltaNH4 = MathUtility.Multiply_Value(dlt_nh4gsm, Conversions.gm2kg / Conversions.sm2ha);
        Util.Debug("Root.NitrogenUptake.DeltaNO3=%f", MathUtility.Sum(NitrogenUptake.DeltaNO3));
        Util.Debug("Root.NitrogenUptake.DeltaNH4=%f", MathUtility.Sum(NitrogenUptake.DeltaNH4));
        NitrogenChanged.Invoke(NitrogenUptake);

        // Send back delta water and nitrogen back to APSIM.
        if (!SwimIsPresent)
        {
            WaterChangedType WaterUptake = new WaterChangedType();
            WaterUptake.DeltaWater = dlt_sw_dep;
            Util.Debug("Root.WaterUptake=%f", MathUtility.Sum(WaterUptake.DeltaWater));
            WaterChanged.Invoke(WaterUptake);

        }
    }

    #endregion

}

