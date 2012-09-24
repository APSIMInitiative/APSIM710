using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CSGeneral;

public class Root1 : Organ1, BelowGround
{
    [Link]
    Plant15 Plant = null;

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
    
    [Link]
    object NUptakeFunction = null;

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
    public double[] cl = null;

    [Input(IsOptional = true)]
    public double[] esp = null;

    [Input(IsOptional = true)]
    public double[] ec = null;

    [Input(IsOptional = true)]
    public double[] no3 = null;

    [Input(IsOptional = true)]
    public double[] nh4 = null;

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

    [Param(IsOptional = true)]
    string UptakeSource = "calc";

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
    private double SpecificRootLength;

    [Event]
    public event FOMLayerDelegate IncorpFOM;

    // ***********
    [Link]
    CompositeBiomass WholePlantGreen = null;

    [Link]
    Function GrowthStructuralFractionStage = null;

    [Event]
    public event WaterChangedDelegate WaterChanged;

    [Event]
    public event NitrogenChangedDelegate NitrogenChanged;

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
    private double RootDepth = 0;
    private bool HaveModifiedKLValues = false;
    private double[] RootLengthSenesced;
    private struct Dlt
    {
        public double dm_pot_rue;
        public double n_senesced_retrans;           // plant N retranslocated to/from (+/-) senesced part to/from <<somewhere else??>> (g/m^2)
        public double n_senesced_trans;
        public double height;                       // growth upwards (mm)
        public double width;                        // growth outwards (mm)
    }
    public class CoverStruct
    {
        public double Green;
        public double Sen;
        public double Total { get { return 1.0 - (1.0 - Green) * (1.0 - Sen); } }
    }
    private Dlt dlt = new Dlt();
    private CoverStruct _Cover = new CoverStruct();
    private Biomass _Green = new Biomass();
    private Biomass _Senesced = new Biomass();
    private Biomass _Growth = new Biomass();
    private Biomass _Senescing = new Biomass();
    private Biomass _Detaching = new Biomass();
    private Biomass _Retranslocation = new Biomass();
    private Biomass GreenRemoved = new Biomass();
    private Biomass SenescedRemoved = new Biomass();
    private double dm_pot_rue;
    private double n_senesced_retrans;
    private double n_senesced_trans;
    private double height;
    private double width;
    private double _DMGreenDemand;
    private double _NCapacity;
    private double _NDemand;
    private double _SoilNDemand;
    private double NMax;
    private double PDemand;
    private double sw_demand_te;
    private double sw_demand;
    private double _NCrit = 0;
    private double _NMin = 0;
    private double n_conc_crit = 0;
    private double n_conc_max = 0;
    private double n_conc_min = 0;
    private double radiationInterceptedGreen;

    internal override Biomass Green { get { return _Green; } }
    internal override Biomass Senesced { get { return _Senesced; } }
    internal override Biomass Senescing { get { return _Senescing; } }
    internal override Biomass Retranslocation { get { return _Retranslocation; } }
    internal override Biomass Growth { get { return _Growth; } }
    internal override Biomass Detaching { get { return _Detaching; } }

    internal override double NCrit { get { return n_conc_crit * Green.Wt; } }
    internal override double NMin { get { return n_conc_min * Green.Wt; } }
    internal override double NDemand { get { return _NDemand; } }
    internal override double SoilNDemand { get { return _SoilNDemand; } }
    internal double NSupply
    {
        get
        {
            int deepest_layer = FindLayerNo(RootDepth);
            return MathUtility.Sum(no3gsm_uptake_pot, 0, deepest_layer+1, 0) +
                   MathUtility.Sum(nh4gsm_uptake_pot, 0, deepest_layer+1, 0);
        }
    }
    internal override double SWDemand { get { return sw_demand; } }
    internal override double DMGreenDemand { get { return _DMGreenDemand; } }
    internal override double DltNSenescedRetrans { get { return dlt.n_senesced_retrans; } }

    protected void ZeroDeltas()
    {
        _Growth.Clear();
        _Senescing.Clear();
        _Detaching.Clear();
        Retranslocation.Clear();
        GreenRemoved.Clear();
        SenescedRemoved.Clear();

        dlt.dm_pot_rue = 0.0;
        dlt.n_senesced_retrans = 0.0;
        dlt.n_senesced_trans = 0.0;
        dlt.height = 0.0;
        dlt.width = 0.0;

        _DMGreenDemand = 0.0;
        _NCapacity = 0.0;
        _NDemand = 0.0;
        _SoilNDemand = 0.0;
        NMax = 0.0;
        PDemand = 0.0;
        sw_demand_te = 0.0;
        sw_demand = 0.0;
    }

    internal override void OnPrepare()
    {
        ZeroDeltas();
        dltRootDepth = 0.0;
        ZeroArray(dltRootLength);
        ZeroArray(dltRootLengthSenesced);
        ZeroArray(dltRootLengthDead);

        ZeroArray(dlt_sw_dep);

        ZeroArray(sw_avail);
        ZeroArray(sw_avail_pot);
        ZeroArray(sw_supply);

        ZeroArray(dlt_no3gsm);
        ZeroArray(dlt_nh4gsm);
        ZeroArray(no3gsm_uptake_pot);
        ZeroArray(nh4gsm_uptake_pot);
    }
    internal override void OnHarvest(HarvestType Harvest, BiomassRemovedType BiomassRemoved)
    {
        Biomass Dead;
        Dead = Green * DieBackFraction;

        // however dead roots have a given N concentration
        Dead.StructuralN = Dead.Wt * NSenescenceConcentration;

        _Green = Green - Dead;
        _Senesced = Senesced + Dead;

        int i = IncreaseSizeOfBiomassRemoved(BiomassRemoved);

        // Unlike above ground parts, no roots go to surface residue module.
        BiomassRemoved.dm_type[i] = Name;
        BiomassRemoved.fraction_to_residue[i] = 0.0F;
        BiomassRemoved.dlt_crop_dm[i] = 0.0F;
        BiomassRemoved.dlt_dm_n[i] = 0.0F;
        BiomassRemoved.dlt_dm_p[i] = 0.0F;
    }

    internal override void DoPotentialRUE() { }
    internal override void DoSWDemand(double Supply) { }
    internal override double DMSupply { get { return 0.0; } }
    internal override double dltDmPotRue { get { return dm_pot_rue; } }

    internal override double NCapacity
    {
        get
        {
            return MathUtility.Constrain(NMax - NDemand, 0.0, double.MaxValue);
        }
    }
    internal override double NDemandDifferential { get { return MathUtility.Constrain(NDemand - Growth.N, 0.0, double.MaxValue); } }
    /// <summary>
    /// Calculate N available for transfer to grain (g/m^2)
    /// </summary>
    internal override double AvailableRetranslocateN
    {
        get
        {
            double N_min = n_conc_min * Green.Wt;
            double N_avail = MathUtility.Constrain(Green.N - N_min, 0.0, double.MaxValue);
            double n_retrans_fraction = 1.0;
            return (N_avail * n_retrans_fraction);
        }
    }
    internal override void DoNRetranslocate(double NSupply, double GrainNDemand)
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
    [Output]
    internal double NUptake
    {
        get
        {
            int deepest_layer = FindLayerNo(RootDepth);
            return  -MathUtility.Sum(dlt_no3gsm, 0, deepest_layer + 1, 0)
                    - MathUtility.Sum(dlt_nh4gsm, 0, deepest_layer + 1, 0);
        }
    }
    internal override void DoNDemand1Pot(double dltDmPotRue)
    {
        Biomass OldGrowth = _Growth;
        _Growth.StructuralWt = dltDmPotRue * MathUtility.Divide(Green.Wt, WholePlantGreen.Wt, 0.0);
        Util.Debug("Root.Growth.StructuralWt=%f", _Growth.StructuralWt);
        CalcNDemand(dltDmPotRue, dltDmPotRue, n_conc_crit, n_conc_max, _Growth, Green, Retranslocation.N, 1.0,
                   ref _NDemand, ref NMax);
        _Growth.StructuralWt = 0.0;
        _Growth.NonStructuralWt = 0.0;
        Util.Debug("Root.NDemand=%f", _NDemand);
        Util.Debug("Root.NMax=%f", NMax);
    }

    internal override void DoNDemand(bool IncludeRetransloation)
    {

        double TopsDMSupply = 0;
        double TopsDltDmPotRue = 0;
        foreach (Organ1 Organ in Plant.Tops)
        {
            TopsDMSupply += Organ.DMSupply;
            TopsDltDmPotRue += Organ.dltDmPotRue;
        }

        if (IncludeRetransloation)
            CalcNDemand(TopsDMSupply, TopsDltDmPotRue, n_conc_crit, n_conc_max, _Growth, Green, Retranslocation.N, NDeficitUptakeFraction,
                      ref _NDemand, ref NMax);
        else
            CalcNDemand(TopsDMSupply, TopsDltDmPotRue, n_conc_crit, n_conc_max, _Growth, Green, 0.0, NDeficitUptakeFraction,
                      ref _NDemand, ref NMax);
        Util.Debug("Root.NDemand=%f", _NDemand);
        Util.Debug("Root.NMax=%f", NMax);
    }

    internal override void DoSoilNDemand()
    {
        _SoilNDemand = NDemand - dlt.n_senesced_retrans;
        _SoilNDemand = MathUtility.Constrain(_SoilNDemand, 0.0, double.MaxValue);
        Util.Debug("Root.SoilNDemand=%f", _SoilNDemand);
    }

    internal override void ZeroDltDmGreen()
    {
        _Growth.StructuralWt = 0;
        _Growth.NonStructuralWt = 0;
    }
    internal override void ZeroDltNSenescedTrans()
    {
        dlt.n_senesced_trans = 0;
    }
    internal override void GiveDmGreen(double Delta)
    {
        _Growth.StructuralWt += Delta * GrowthStructuralFractionStage.Value;
        _Growth.NonStructuralWt += Delta * (1.0 - GrowthStructuralFractionStage.Value);
        Util.Debug("Root.Growth.StructuralWt=%f", _Growth.StructuralWt);
        Util.Debug("Root.Growth.NonStructuralWt=%f", _Growth.NonStructuralWt);
    }


// ******

    [Output]
    public int RootLayerMax;

    [Output]
    public double RootDepthMax;

    public double SWSupply { get { return MathUtility.Sum(sw_supply); } }
    public double WaterUptake { get { return -MathUtility.Sum(dlt_sw_dep); } }
    private double[] no3gsm { get { return MathUtility.Multiply_Value(no3, Conversions.kg2gm / Conversions.ha2sm); } }
    private double[] nh4gsm { get { return MathUtility.Multiply_Value(nh4, Conversions.kg2gm / Conversions.ha2sm); } }

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
                double[] RootFr = RootDist(1.0);

                double wet_root_fr = 0.0;
                for (int layer = 0; layer != RootFr.Length; layer++)
                    wet_root_fr = wet_root_fr + WFPS(layer) * RootFr[layer];
                return wet_root_fr;
            }
            else
                return 0.0;
        }
    }

    public double[] RootLengthDensity
    {
        get
        {
            double[] rld = MathUtility.Divide(RootLength, dlayer);
            return MathUtility.Divide_Value(rld, Population.Density);
        }
    }

    


    void Initialise()
    {
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
        ZeroArray(no3gsm_min);
        ZeroArray(nh4gsm_min);
    }


    [EventHandler]
    public void OnPhaseChanged(PhaseChangedType PhenologyChange)
    {
        if (PhenologyChange.NewPhaseName == "GerminationToEmergence")
            RootDepth = InitialRootDepth;
        else if (PhenologyChange.NewPhaseName == "EmergenceToEndOfJuvenile")
        {
            Green.StructuralWt = InitialWt * Population.Density;
            Green.StructuralN = InitialNConcentration * Green.StructuralWt;

            // initial root length (mm/mm^2)
            double initial_root_length = Green.Wt / Conversions.sm2smm * SpecificRootLength;

            // initial root length density (mm/mm^3)
            double rld = MathUtility.Divide(initial_root_length, RootDepth, 0.0);

            int deepest_layer = FindLayerNo(RootDepth);

            for (int layer = 0; layer <= deepest_layer; layer++)
                RootLength[layer] = rld * dlayer[layer] * RootProportion(layer, RootDepth);

            Util.Debug("Root.InitGreen.StructuralWt=%f", Green.StructuralWt);
            Util.Debug("Root.InitGreen.StructuralN=%f", Green.StructuralN);
            Util.Debug("Root.InitRootLength=%f", MathUtility.Sum(RootLength));
        }
    }
    public double FASW
    {
        get
        {
            //  the layer with root front
            int layer = FindLayerNo(RootDepth);
            int deepest_layer = dlayer.Length - 1;

            double CumDepth = MathUtility.Sum(dlayer, 0, layer+1, 0.0);

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

    public void DoPlantRootDepth()
    {
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

        RootLayerMax = deepest_layer + 1;
        RootDepthMax = MathUtility.Sum(dlayer, 0, deepest_layer + 1, 0.0);
        dltRootDepth = MathUtility.Constrain(dltRootDepth, double.MinValue, RootDepthMax - RootDepth);

        if (dltRootDepth < 0.0)
            throw new Exception("negative root growth??");

        Util.Debug("Root.dltRootDepth=%f", dltRootDepth);
        Util.Debug("Root.root_layer_max=%i", RootLayerMax);
        Util.Debug("Root.root_depth_max=%f", RootDepthMax);
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
    /// calculate the fraction of available soil water at the given depth (mm)
    /// </summary>
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

    /// <summary>
    /// Calculate a modified KL value as per:
    ///    Hochman et. al. (2007) Simulating the effects of saline and sodic subsoils on wheat
    ///       crops growing on Vertosols. Australian Journal of Agricultural Research, 58, 802–810
    /// Will use one of CL, ESP and EC in that order to modified KL.
    /// </summary>
    double getModifiedKL(int i)
    {
        if (ModifyKL)
        {
            double KLFactor = 1.0;
            if (cl.Length > 1)
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

    internal void DoWaterUptake(double TopsSWDemand)
    {
        DoWaterSupply();
        if (UptakeSource == "apsim")
        {
            throw new NotImplementedException();
            //doWaterUptakeExternal(uptake_source, crop_type);
        }
        else if (UptakeSource == "swim3")
        {
            throw new NotImplementedException();
            //doWaterUptakeExternal(uptake_source, crop_type);
        }
        else
            DoWaterUptakeInternal(TopsSWDemand);
    }

    /// <summary>
    /// Calculate today's daily water supply from this root system
    /// based on the KL approach 
    /// </summary>
    void DoWaterSupply()
    {
        // potential extractable sw
        DoPotentialExtractableSW();

        // actual extractable sw (sw-ll)
        DoSWAvailable();

        DoSWSupply();
    }

    /// <summary>
    /// Return potential available soil water from each layer in the root zone.
    /// </summary>
    void DoPotentialExtractableSW()
    {
        ZeroArray(sw_avail_pot);

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
    void DoSWAvailable()
    {
        ZeroArray(sw_avail);

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
    void DoSWSupply()
    {
        ZeroArray(sw_supply);

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
    void DoWaterUptakeInternal(double sw_demand)
    {
        int deepest_layer = FindLayerNo(RootDepth);
        double sw_supply_sum = MathUtility.Sum(sw_supply, 0, deepest_layer + 1, 0.0);

        if ((sw_supply_sum < 0.0) || (sw_demand < 0.0))
        {
            //we have no uptake - there is no demand or potential
            ZeroArray(dlt_sw_dep);
        }
        else
        {
            // get actual uptake
            ZeroArray(dlt_sw_dep);
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
        Util.Debug("Root.dlt_sw_dep=%f", MathUtility.Sum(dlt_sw_dep));
    }

    /// <summary>
    /// Nitrogen supply.
    /// </summary>
    public void DoNitrogenSupply()
    {
        if (NUptakeFunction is NUptake3)
            (NUptakeFunction as NUptake3).DoNUptake(RootDepth, no3gsm, nh4gsm, 
                                             bd, dlayer, sw_avail, sw_avail_pot, no3gsm_min, nh4gsm_min, 
                                             ref no3gsm_uptake_pot, ref nh4gsm_uptake_pot);
                                             
        else
            throw new NotImplementedException();
    }

    internal override void DoDMDemand(double DMSupply)
    {
        _DMGreenDemand = Math.Max(0.0, DMSupply);   //Just ask for all you can get for now - NIH.
    }

    internal override void DoSenescence()
    {
        double fraction_senescing = MathUtility.Constrain(DMSenescenceFraction.Value, 0.0, 1.0);

        _Senescing.StructuralWt = (Green.StructuralWt + _Growth.StructuralWt + Retranslocation.StructuralWt) * fraction_senescing;
        _Senescing.NonStructuralWt = (Green.NonStructuralWt + _Growth.NonStructuralWt + Retranslocation.NonStructuralWt) * fraction_senescing;
        Util.Debug("Root.Senescing.StructuralWt=%f", _Senescing.StructuralWt);
        Util.Debug("Root.Senescing.NonStructuralWt=%f", _Senescing.NonStructuralWt);
    }

    internal override void DoNSenescence()
    {
        double green_n_conc = MathUtility.Divide(Green.N, Green.Wt, 0.0);
        double dlt_n_in_senescing_part = _Senescing.Wt * green_n_conc;
        double sen_n_conc = Math.Min(NSenescenceConcentration, green_n_conc);

        double SenescingN = _Senescing.Wt * sen_n_conc;
        _Senescing.StructuralN = MathUtility.Constrain(SenescingN, double.MinValue, Green.N);

        dlt.n_senesced_trans = dlt_n_in_senescing_part - _Senescing.N;
        dlt.n_senesced_trans = MathUtility.Constrain(dlt.n_senesced_trans, 0.0, double.MaxValue);

        Util.Debug("Root.SenescingN=%f", SenescingN);
        Util.Debug("Root.dlt.n_senesced_trans=%f", dlt.n_senesced_trans);
    }
    internal override void DoNSenescedRetranslocation(double navail, double n_demand_tot)
    {
        dlt.n_senesced_retrans = navail * MathUtility.Divide(NDemand, n_demand_tot, 0.0);
        Util.Debug("Root.dlt.n_senesced_retrans=%f", dlt.n_senesced_retrans);
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
    double RootProportion(int layer, double RootDepth)
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
    double[] RootDist(double root_sum)
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
    double WFPS(int layer)
    {
        double wfps = MathUtility.Divide(sw_dep[layer] - ll15_dep[layer],
                                        sat_dep[layer] - ll15_dep[layer], 0.0);
        return MathUtility.Constrain(wfps, 0.0, 1.0);
    }



    /// <summary>
    /// Calculate the increase in root length density in each rooted
    /// layer based upon soil hospitality, moisture and fraction of
    /// layer explored by roots.
    /// </summary>
    internal void RootLengthGrowth()
    {
        ZeroArray(dltRootLength);

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

        double dlt_length_tot = _Growth.Wt / Conversions.sm2smm * SpecificRootLength;

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
        ZeroArray(dltRootLengthSenesced);
        double senesced_length = _Senescing.Wt / Conversions.sm2smm * SpecificRootLength;
        dltRootLengthSenesced = RootDist(senesced_length);
        Util.Debug("Root.dltRootLengthSenesced=%f", MathUtility.Sum(dltRootLengthSenesced));
    }

    internal void DoNUptake(double PotNFix)
    {
        if (UptakeSource == "apsim")
        {
            throw new NotImplementedException();
            //doWaterUptakeExternal(uptake_source, crop_type);
        }
        else if (UptakeSource == "swim3")
        {
            throw new NotImplementedException();
            //doWaterUptakeExternal(uptake_source, crop_type);
        }
        else
            DoNUptakeInternal(PotNFix);
    }


    void DoNUptakeInternal(double PotNFix)
    {
        double n_demand = 0.0;
        foreach (Organ1 Organ in Plant.Organ1s)
            n_demand += Organ.SoilNDemand;

        int deepest_layer = FindLayerNo(RootDepth);

        double ngsm_supply = MathUtility.Sum(no3gsm_uptake_pot, 0, deepest_layer + 1, 0)
                           + MathUtility.Sum(nh4gsm_uptake_pot, 0, deepest_layer + 1, 0);


        if (NSupplyPreference == "fixation")
            n_demand = MathUtility.Constrain(n_demand - PotNFix, 0.0, double.MaxValue);

        // get actual change in N contents
        ZeroArray(dlt_no3gsm);
        ZeroArray(dlt_nh4gsm);

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

    internal override void DoDetachment()
    {
        _Detaching = Senesced * SenescenceDetachmentFraction;
        Util.Debug("Root.Detaching.Wt=%f", _Detaching.Wt);
        Util.Debug("Root.Detaching.N=%f", _Detaching.N);
    }

    /// <summary>
    /// Dispose of detached material from dead & senesced roots into FOM pool
    /// </summary>
    internal void DisposeDetachedMaterial()
    {
        if (_Detaching.Wt > 0.0)
        {
            // DM
            double[] dlt_dm_incorp = RootDist(_Detaching.Wt * Conversions.gm2kg / Conversions.sm2ha);

            // Nitrogen
            double[] dlt_N_incorp = RootDist(_Detaching.N * Conversions.gm2kg / Conversions.sm2ha);

            // Phosporous
            //double[] dlt_P_incorp = RootDist(_Detaching.P * Conversions.gm2kg / Conversions.sm2ha);

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
    /// Update Daily State
    /// </summary>
    internal override void Update()
    {
        _Green = Green + Growth - _Senescing;

        _Senesced = Senesced - _Detaching + _Senescing;
        _Green = Green + Retranslocation;
        _Green.StructuralN = Green.N + dlt.n_senesced_retrans;

        Biomass dying = Green * Population.DyingFractionPlants;
        _Green = Green - dying;
        _Senesced = Senesced + dying;
        _Senescing = _Senescing + dying;

        Util.Debug("Root.Green.Wt=%f", _Green.Wt);
        Util.Debug("Root.Green.N=%f", _Green.N);
        Util.Debug("Root.Senesced.Wt=%f", _Senesced.Wt);
        Util.Debug("Root.Senesced.N=%f", _Senesced.N);
        Util.Debug("Root.Senescing.Wt=%f", _Senescing.Wt);
        Util.Debug("Root.Senescing.N=%f", _Senescing.N);

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
    }

    public void UpdateWaterBalance()
    {
        // Send back delta water and nitrogen back to APSIM.
        if (UptakeSource == "calc")
        {
            WaterChangedType WaterUptake = new WaterChangedType();
            WaterUptake.DeltaWater = dlt_sw_dep;
            Util.Debug("Root.WaterUptake=%f", MathUtility.Sum(WaterUptake.DeltaWater));
            WaterChanged.Invoke(WaterUptake);

            NitrogenChangedType NitrogenUptake = new NitrogenChangedType();
            NitrogenUptake.DeltaNO3 = MathUtility.Multiply_Value(dlt_no3gsm, Conversions.gm2kg / Conversions.sm2ha);
            NitrogenUptake.DeltaNH4 = MathUtility.Multiply_Value(dlt_nh4gsm, Conversions.gm2kg / Conversions.sm2ha);
            Util.Debug("Root.NitrogenUptake.DeltaNO3=%f", MathUtility.Sum(NitrogenUptake.DeltaNO3));
            Util.Debug("Root.NitrogenUptake.DeltaNH4=%f", MathUtility.Sum(NitrogenUptake.DeltaNH4));
            NitrogenChanged.Invoke(NitrogenUptake);
        }
        else
            throw new NotImplementedException();
    }

    internal override void DoNConccentrationLimits()
    {
        n_conc_crit = NConcentrationCritical;
        n_conc_min = NConcentrationMinimum;
        n_conc_max = NConcentrationMaximum;
        Util.Debug("Root.n_conc_crit=%f", n_conc_crit);
        Util.Debug("Root.n_conc_min=%f", n_conc_min);
        Util.Debug("Root.n_conc_max=%f", n_conc_max);
    }

    internal void WriteSummary()
    {
        if (dlt_sw_dep == null)
            Initialise();

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

}

