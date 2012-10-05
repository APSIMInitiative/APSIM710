using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CSGeneral;
using ModelFramework;

public class Stem1 : Organ1, AboveGround
{
    [Link]
    Plant15 Plant = null;

    [Link]
    public Component My;

    public string Name { get { return My.Name; } }


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

    public Biomass Green { get { return _Green; } set { } }
    public  Biomass Senesced { get { return _Senesced; } }
    public  Biomass Senescing { get { return _Senescing; } }
    public  Biomass Retranslocation { get { return _Retranslocation; } }
    public  Biomass Growth { get { return _Growth; } }
    public  Biomass Detaching { get { return _Detaching; } }

    public  double NCrit { get { return n_conc_crit * Green.Wt; } }
    public  double NMin { get { return n_conc_min * Green.Wt; } }
    public  double NDemand { get { return _NDemand; } }
    public  double SoilNDemand { get { return _SoilNDemand; } }
    public double SWDemand { get { return sw_demand; } }
    public double SWSupply { get { return 0; } }
    public double SWUptake { get { return 0; } }
    public void DoSWUptake(double SWDemand) { }
    public double DMGreenDemand { get { return _DMGreenDemand; } }
    public  double DMRetransSupply
    {
        get
        {
            return MathUtility.Constrain(Green.NonStructuralWt, 0.0, double.MaxValue);
        }
    }
    public  double NCapacity
    {
        get
        {
            return MathUtility.Constrain(NMax - NDemand, 0.0, double.MaxValue);
        }
    }
    public  double NDemandDifferential { get { return MathUtility.Constrain(NDemand - Growth.N, 0.0, double.MaxValue); } }
    public  double DltNSenescedRetrans { get { return dlt.n_senesced_retrans; } }
    public double NSupply { get { return 0; } }
    public double NUptake { get { return 0; } }


    public  double CoverGreen {get { return 0; } }
    public  double CoverSen { get { return 0; } }

    public  void DoDMDemand(double DMSupply) { }
    public  void DoDmRetranslocate(double dlt_dm_retrans_to_fruit, double demand_differential_begin) { }
    public  void DoCover() { }
    public  double DMDemandDifferential { get { return 0; } }
    public  void DoNPartition(double GrowthN)
    {
        Growth.StructuralN = GrowthN;
    }
    public  void DoNFixRetranslocate(double NFixUptake, double nFixDemandTotal)
    {
        Growth.StructuralN += NFixUptake * MathUtility.Divide(NDemandDifferential, nFixDemandTotal, 0.0);
    }

    /// <summary>
    /// Calculate N available for transfer to grain (g/m^2)
    /// </summary>
    public  double AvailableRetranslocateN
    {
        get
        {
            double N_min = n_conc_min * Green.Wt;
            double N_avail = MathUtility.Constrain(Green.N - N_min, 0.0, double.MaxValue);
            double n_retrans_fraction = 1.0;
            return (N_avail * n_retrans_fraction);
        }
    }
    public  void DoNRetranslocate(double NSupply, double GrainNDemand)
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
    public void DoNUptake(double PotNFix) { }

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

    public  double interceptRadiation(double incomingSolarRadiation)
    {
        radiationInterceptedGreen = _Cover.Green * incomingSolarRadiation;
        return _Cover.Total * incomingSolarRadiation;
    }

    public  void DoPotentialRUE() { }
    public  void DoSWDemand(double Supply) { }
    public  double DMSupply { get { return 0.0; } }
    public  double dltDmPotRue { get { return dm_pot_rue; } }

    public  void DoNDemand1Pot(double dltDmPotRue)
    {
        Biomass OldGrowth = Growth;
        Growth.StructuralWt = dltDmPotRue * MathUtility.Divide(Green.Wt, TotalGreen.Wt, 0.0);
        Util.Debug("Stem.Growth.StructuralWt=%f", _Growth.StructuralWt);

        Util.CalcNDemand(dltDmPotRue, dltDmPotRue, n_conc_crit, n_conc_max, Growth, Green, Retranslocation.N, 1.0,
                   ref _NDemand, ref NMax);
        Growth.StructuralWt = 0.0;
        Growth.NonStructuralWt = 0.0;
        Util.Debug("Stem.NDemand=%f", _NDemand);
        Util.Debug("Stem.NMax=%f", NMax);
    }

    public  void DoNDemand(bool IncludeRetransloation)
    {

        double TopsDMSupply = 0;
        double TopsDltDmPotRue = 0;
        foreach (Organ1 Organ in Plant.Tops)
        {
            TopsDMSupply += Organ.DMSupply;
            TopsDltDmPotRue += Organ.dltDmPotRue;
        }

        if (IncludeRetransloation)
            Util.CalcNDemand(TopsDMSupply, TopsDltDmPotRue, n_conc_crit, n_conc_max, Growth, Green, Retranslocation.N, NDeficitUptakeFraction,
                      ref _NDemand, ref NMax);
        else
            Util.CalcNDemand(TopsDMSupply, TopsDltDmPotRue, n_conc_crit, n_conc_max, Growth, Green, 0.0, NDeficitUptakeFraction,
                      ref _NDemand, ref NMax);
        Util.Debug("Stem.NDemand=%f", _NDemand);
        Util.Debug("Stem.NMax=%f", NMax);
    }

    public  void DoSoilNDemand()
    {
        _SoilNDemand = NDemand - dlt.n_senesced_retrans;
        _SoilNDemand = MathUtility.Constrain(_SoilNDemand, 0.0, double.MaxValue);
        Util.Debug("Stem.SoilNDemand=%f", _SoilNDemand);
    }
    public void DoNSupply() { }

    public  void ZeroDltNSenescedTrans() 
    {
        dlt.n_senesced_trans = 0;
    }


    public  void GiveDmGreen(double Delta)
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

    public  void OnPrepare()
    {
        ZeroDeltas();
    }

    public  void OnHarvest(HarvestType Harvest, BiomassRemovedType BiomassRemoved)
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

        int i = Util.IncreaseSizeOfBiomassRemoved(BiomassRemoved);
        BiomassRemoved.dm_type[i] = Name;
        BiomassRemoved.fraction_to_residue[i] = (float)(1.0 - Harvest.Remove);
        BiomassRemoved.dlt_crop_dm[i] = (float)(dlt_dm_harvest * Conversions.gm2kg / Conversions.sm2ha);
        BiomassRemoved.dlt_dm_n[i] = (float)(dlt_n_harvest * Conversions.gm2kg / Conversions.sm2ha);
        //BiomassRemoved.dlt_dm_p[i] = (float)(dlt_p_harvest * Conversions.gm2kg / Conversions.sm2ha);
    }

    public  void OnEndCrop(BiomassRemovedType BiomassRemoved)
    {
        int i = Util.IncreaseSizeOfBiomassRemoved(BiomassRemoved);
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

    public  void DoSenescence()
    {
        double fraction_senescing = MathUtility.Constrain(DMSenescenceFraction.Value, 0.0, 1.0);

        _Senescing.StructuralWt = (Green.StructuralWt + Growth.StructuralWt + Retranslocation.StructuralWt) * fraction_senescing;
        _Senescing.NonStructuralWt = (Green.NonStructuralWt + Growth.NonStructuralWt + Retranslocation.NonStructuralWt) * fraction_senescing;
        Util.Debug("Stem.Senescing.StructuralWt=%f", _Senescing.StructuralWt);
        Util.Debug("Stem.Senescing.NonStructuralWt=%f", _Senescing.NonStructuralWt);
    }
    public  void DoNSenescence()
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
    public  void DoNSenescedRetranslocation(double navail, double n_demand_tot)
    {
        dlt.n_senesced_retrans = navail * MathUtility.Divide(NDemand, n_demand_tot, 0.0);
        Util.Debug("Stem.dlt.n_senesced_retrans=%f", dlt.n_senesced_retrans);
    }

    public  void DoDetachment()
    {
        _Detaching = Senesced * SenescenceDetachmentFraction;
        Util.Debug("Stem.Detaching.Wt=%f", _Detaching.Wt);
        Util.Debug("Stem.Detaching.N=%f", _Detaching.N);
    }

    public  void Update()
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

    public  void DoNConccentrationLimits()
    {
        n_conc_crit = NConcentrationCritical.Value;
        n_conc_min = NConcentrationMinimum.Value;
        n_conc_max = NConcentrationMaximum.Value;
        Util.Debug("Stem.n_conc_crit=%f", n_conc_crit);
        Util.Debug("Stem.n_conc_min=%f", n_conc_min);
        Util.Debug("Stem.n_conc_max=%f", n_conc_max);
    }
}

