using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CSGeneral;

public class Stem1 : Organ1, AboveGround
{
    [Link]
    Plant15 Plant = null;

    [Link]
    Function HeightFunction = null;

    [Link]
    Population1 Population = null;

    [Link]
    CompositeBiomass TotalGreen = null;

    [Link]
    Function GrowthStructuralFractionStage = null;

    [Link]
    Function DMSenescenceFraction = null;

    [Link]
    Leaf1 Leaf = null;

    [Link]
    Function NConcentrationCritical = null;

    [Link]
    Function NConcentrationMinimum = null;

    [Link]
    Function NConcentrationMaximum = null;

    [Link]
    Function RetainFraction = null;

    [Param]
    double NDeficitUptakeFraction = 1.0;

    [Param]
    double NSenescenceConcentration = 0;

    [Param]
    double SenescenceDetachmentFraction = 0;

    [Param]
    double InitialWt = 0;

    [Param]
    double InitialNConcentration = 0;

    [Output]
    double FractionHeightRemoved = 0;

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
    public override double SWDemand { get { return sw_demand; } }
    internal override double DMGreenDemand { get { return _DMGreenDemand; } }
    internal override double DMRetransSupply
    {
        get
        {
            return MathUtility.Constrain(Green.NonStructuralWt, 0.0, double.MaxValue);
        }
    }
    internal override double NCapacity
    {
        get
        {
            return MathUtility.Constrain(NMax - NDemand, 0.0, double.MaxValue);
        }
    }
    internal override double NDemandDifferential { get { return MathUtility.Constrain(NDemand - Growth.N, 0.0, double.MaxValue); } }
    internal override double DltNSenescedRetrans { get { return dlt.n_senesced_retrans; } }

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
            Retranslocation.StructuralN = -AvailableRetranslocateN;
        }
        else
        {
            // supply greater than demand.
            // Retranslocate what is needed
            Retranslocation.StructuralN = -GrainNDemand * MathUtility.Divide(AvailableRetranslocateN, NSupply, 0.0);
        }
        Util.Debug("Stem.Retranslocation.N=%f", Retranslocation.N);
    }
    protected void ZeroDeltas()
    {
        Growth.Clear();
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

    internal override double interceptRadiation(double incomingSolarRadiation)
    {
        radiationInterceptedGreen = _Cover.Green * incomingSolarRadiation;
        return _Cover.Total * incomingSolarRadiation;
    }

    internal override void DoPotentialRUE() { }
    internal override void DoSWDemand(double Supply) { }
    internal override double DMSupply { get { return 0.0; } }
    internal override double dltDmPotRue { get { return dm_pot_rue; } }

    internal override void DoNDemand1Pot(double dltDmPotRue)
    {
        Biomass OldGrowth = Growth;
        Growth.StructuralWt = dltDmPotRue * MathUtility.Divide(Green.Wt, TotalGreen.Wt, 0.0);
        Util.Debug("Stem.Growth.StructuralWt=%f", _Growth.StructuralWt);

        CalcNDemand(dltDmPotRue, dltDmPotRue, n_conc_crit, n_conc_max, Growth, Green, Retranslocation.N, 1.0,
                   ref _NDemand, ref NMax);
        Growth.StructuralWt = 0.0;
        Growth.NonStructuralWt = 0.0;
        Util.Debug("Stem.NDemand=%f", _NDemand);
        Util.Debug("Stem.NMax=%f", NMax);
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
            CalcNDemand(TopsDMSupply, TopsDltDmPotRue, n_conc_crit, n_conc_max, Growth, Green, Retranslocation.N, NDeficitUptakeFraction,
                      ref _NDemand, ref NMax);
        else
            CalcNDemand(TopsDMSupply, TopsDltDmPotRue, n_conc_crit, n_conc_max, Growth, Green, 0.0, NDeficitUptakeFraction,
                      ref _NDemand, ref NMax);
        Util.Debug("Stem.NDemand=%f", _NDemand);
        Util.Debug("Stem.NMax=%f", NMax);
    }

    internal override void DoSoilNDemand()
    {
        _SoilNDemand = NDemand - dlt.n_senesced_retrans;
        _SoilNDemand = MathUtility.Constrain(_SoilNDemand, 0.0, double.MaxValue);
        Util.Debug("Stem.SoilNDemand=%f", _SoilNDemand);
    }

    internal override void ZeroDltDmGreen()
    {
        Growth.StructuralWt = 0;
        Growth.NonStructuralWt = 0;
    }
    internal override void ZeroDltNSenescedTrans() 
    {
        dlt.n_senesced_trans = 0;
    }


    internal override void GiveDmGreen(double Delta)
    {
        Growth.StructuralWt += Delta * GrowthStructuralFractionStage.Value;
        Growth.NonStructuralWt += Delta * (1.0 - GrowthStructuralFractionStage.Value);
        Util.Debug("Stem.Growth.StructuralWt=%f", _Growth.StructuralWt);
        Util.Debug("Stem.Growth.NonStructuralWt=%f", _Growth.NonStructuralWt);
    }


    private double DeltaHeight;
    private double _Height = 0;
    private double _Width = 0;

    // soilwat needs height for its E0 calculation.
    [Output("Height")][Units("mm")]
    public double Height { get { return _Height; } }
    public double Width { get { return _Width; } }

    public double GreenWtPerPlant { get {return MathUtility.Divide(Green.Wt, Population.Density, 0.0); } }

    internal override void OnPrepare()
    {
        ZeroDeltas();
    }

    internal override void OnHarvest(HarvestType Harvest, BiomassRemovedType BiomassRemoved)
    {
        // Some biomass is removed according to harvest height
        FractionHeightRemoved = MathUtility.Divide(Harvest.Height, Height, 0.0);

        double chop_fr_green = (1.0 - RetainFraction.Value);
        double chop_fr_sen = (1.0 - RetainFraction.Value);

        double dlt_dm_harvest = Green.Wt * chop_fr_green
                             + Senesced.Wt * chop_fr_sen;

        double dlt_n_harvest = Green.N * chop_fr_green
                            + Senesced.N * chop_fr_sen;

        //double dlt_p_harvest = Green.P * chop_fr_green
        //                    + Senesced.P * chop_fr_sen;

        _Senesced = Senesced * RetainFraction.Value;
        _Green = Green * RetainFraction.Value;

        _Height = MathUtility.Constrain(Harvest.Height, 1.0, double.MaxValue);

        int i = IncreaseSizeOfBiomassRemoved(BiomassRemoved);
        BiomassRemoved.dm_type[i] = Name;
        BiomassRemoved.fraction_to_residue[i] = (float)(1.0 - Harvest.Remove);
        BiomassRemoved.dlt_crop_dm[i] = (float)(dlt_dm_harvest * Conversions.gm2kg / Conversions.sm2ha);
        BiomassRemoved.dlt_dm_n[i] = (float)(dlt_n_harvest * Conversions.gm2kg / Conversions.sm2ha);
        //BiomassRemoved.dlt_dm_p[i] = (float)(dlt_p_harvest * Conversions.gm2kg / Conversions.sm2ha);
    }

    internal override void OnEndCrop(BiomassRemovedType BiomassRemoved)
    {
        base.OnEndCrop(BiomassRemoved);

        int i = IncreaseSizeOfBiomassRemoved(BiomassRemoved);
        BiomassRemoved.dm_type[i] = Name;
        BiomassRemoved.fraction_to_residue[i] = 1.0F;
        BiomassRemoved.dlt_crop_dm[i] = (float)((Green.Wt + Senesced.Wt) * Conversions.gm2kg / Conversions.sm2ha);
        BiomassRemoved.dlt_dm_n[i] = (float)((Green.N + Senesced.N) * Conversions.gm2kg / Conversions.sm2ha);
        //BiomassRemoved.dlt_dm_p[i] = (float)((Green.P + Senesced.P) * Conversions.gm2kg / Conversions.sm2ha);

        Senesced.Clear();
        Green.Clear();
    }


    [EventHandler]
    public void OnPhaseChanged(PhaseChangedType PhenologyChange)
    {
        if (PhenologyChange.NewPhaseName == "EmergenceToEndOfJuvenile")
        {
            Green.StructuralWt = InitialWt * Population.Density;
            Green.StructuralN = InitialNConcentration * Green.StructuralWt;
            Util.Debug("Stem.InitGreen.StructuralWt=%f", Green.StructuralWt);
            Util.Debug("Stem.InitGreen.StructuralN=%f", Green.StructuralN);
        }
    }
    internal void Morphology()
    {
        DeltaHeight = MathUtility.Constrain(HeightFunction.Value - _Height, 0.0, double.MaxValue);
        Util.Debug("Stem.DeltaHeight=%f", DeltaHeight); 
    }

    internal override void DoSenescence()
    {
        double fraction_senescing = MathUtility.Constrain(DMSenescenceFraction.Value, 0.0, 1.0);

        _Senescing.StructuralWt = (Green.StructuralWt + Growth.StructuralWt + Retranslocation.StructuralWt) * fraction_senescing;
        _Senescing.NonStructuralWt = (Green.NonStructuralWt + Growth.NonStructuralWt + Retranslocation.NonStructuralWt) * fraction_senescing;
        Util.Debug("Stem.Senescing.StructuralWt=%f", _Senescing.StructuralWt);
        Util.Debug("Stem.Senescing.NonStructuralWt=%f", _Senescing.NonStructuralWt);
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

        Util.Debug("Stem.SenescingN=%f", SenescingN);
        Util.Debug("Stem.dlt.n_senesced_trans=%f", dlt.n_senesced_trans);
    }
    internal override void DoNSenescedRetranslocation(double navail, double n_demand_tot)
    {
        dlt.n_senesced_retrans = navail * MathUtility.Divide(NDemand, n_demand_tot, 0.0);
        Util.Debug("Stem.dlt.n_senesced_retrans=%f", dlt.n_senesced_retrans);
    }

    internal override void DoDetachment()
    {
        _Detaching = Senesced * SenescenceDetachmentFraction;
        Util.Debug("Stem.Detaching.Wt=%f", _Detaching.Wt);
        Util.Debug("Stem.Detaching.N=%f", _Detaching.N);
    }

    internal override void Update()
    {
        Growth.StructuralN += Leaf.NSenescedTrans;
        _Green = Green + Growth - _Senescing;

        _Senesced = Senesced - _Detaching + _Senescing;
        _Green = Green + Retranslocation;
        _Green.StructuralN = Green.N + dlt.n_senesced_retrans;

        Biomass dying = Green * Population.DyingFractionPlants;
        _Green = Green - dying;
        _Senesced = Senesced + dying;
        _Senescing = _Senescing + dying;
        _Height += DeltaHeight;

        Util.Debug("Stem.Green.Wt=%f", _Green.Wt);
        Util.Debug("Stem.Green.N=%f", _Green.N);
        Util.Debug("Stem.Senesced.Wt=%f", _Senesced.Wt);
        Util.Debug("Stem.Senesced.N=%f", _Senesced.N);
        Util.Debug("Stem.Senescing.Wt=%f", _Senescing.Wt);
        Util.Debug("Stem.Senescing.N=%f", _Senescing.N);
    }

    internal override void DoNConccentrationLimits()
    {
        n_conc_crit = NConcentrationCritical.Value;
        n_conc_min = NConcentrationMinimum.Value;
        n_conc_max = NConcentrationMaximum.Value;
        Util.Debug("Stem.n_conc_crit=%f", n_conc_crit);
        Util.Debug("Stem.n_conc_min=%f", n_conc_min);
        Util.Debug("Stem.n_conc_max=%f", n_conc_max);
    }
}

