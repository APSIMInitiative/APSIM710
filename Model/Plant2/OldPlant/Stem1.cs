using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CSGeneral;
using ModelFramework;

public class Stem1 : Organ1, AboveGround
{
    #region Parameters read from XML file and links to other functions.
    [Link]
    Plant15 Plant = null;

    [Link]
    public Component My;

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
    #endregion

    #region Private variables
    public double dlt_n_senesced_retrans;           // plant N retranslocated to/from (+/-) senesced part to/from <<somewhere else??>> (g/m^2)
    public double dlt_n_senesced_trans;
    public double dlt_height;                       // growth upwards (mm)
    public double dlt_width;                        // growth outwards (mm)

    private Biomass GreenRemoved = new Biomass();
    private Biomass SenescedRemoved = new Biomass();
    private double _DMGreenDemand;
    private double _NDemand;
    private double _SoilNDemand;
    private double NMax;
    private double sw_demand;
    private double n_conc_crit = 0;
    private double n_conc_max = 0;
    private double n_conc_min = 0;
    private double DeltaHeight;

    #endregion

    #region Public interface defined by Organ1
    public string Name { get { return My.Name; } }
    public Biomass Green { get; private set; }
    public Biomass Senesced { get; private set; }
    public Biomass Senescing { get; private set; }
    public Biomass Retranslocation { get; private set; }
    public Biomass Growth { get; private set; }
    public Biomass Detaching { get; private set; }

    // Soil water
    public double SWSupply { get { return 0; } }
    public double SWDemand { get { return sw_demand; } }
    public double SWUptake { get { return 0; } }
    public void DoSWDemand(double Supply) { }
    public void DoSWUptake(double SWDemand) { }

    // dry matter
    public double DMSupply { get { return 0.0; } }
    public double DMRetransSupply
    {
        get
        {
            return MathUtility.Constrain(Green.NonStructuralWt, 0.0, double.MaxValue);
        }
    }
    public double dltDmPotRue { get { return 0; } }
    public double DMGreenDemand { get { return _DMGreenDemand; } }
    public double DMDemandDifferential { get { return 0; } }
    public void DoDMDemand(double DMSupply) { }
    public void DoDmRetranslocate(double dlt_dm_retrans_to_fruit, double demand_differential_begin) { }
    public void GiveDmGreen(double Delta)
    {
        Growth.StructuralWt += Delta * GrowthStructuralFractionStage.Value;
        Growth.NonStructuralWt += Delta * (1.0 - GrowthStructuralFractionStage.Value);
        Util.Debug("Stem.Growth.StructuralWt=%f", Growth.StructuralWt);
        Util.Debug("Stem.Growth.NonStructuralWt=%f", Growth.NonStructuralWt);
    }
    public void DoSenescence()
    {
        double fraction_senescing = MathUtility.Constrain(DMSenescenceFraction.Value, 0.0, 1.0);

        Senescing.StructuralWt = (Green.StructuralWt + Growth.StructuralWt + Retranslocation.StructuralWt) * fraction_senescing;
        Senescing.NonStructuralWt = (Green.NonStructuralWt + Growth.NonStructuralWt + Retranslocation.NonStructuralWt) * fraction_senescing;
        Util.Debug("Stem.Senescing.StructuralWt=%f", Senescing.StructuralWt);
        Util.Debug("Stem.Senescing.NonStructuralWt=%f", Senescing.NonStructuralWt);
    }
    public void DoDetachment()
    {
        Detaching = Senesced * SenescenceDetachmentFraction;
        Util.Debug("Stem.Detaching.Wt=%f", Detaching.Wt);
        Util.Debug("Stem.Detaching.N=%f", Detaching.N);
    }
    
    // nitrogen
    public double NDemand { get { return _NDemand; } }
    public double NSupply { get { return 0; } }
    public double NUptake { get { return 0; } }
    public double SoilNDemand { get { return _SoilNDemand; } }
    public double NCapacity
    {
        get
        {
            return MathUtility.Constrain(NMax - NDemand, 0.0, double.MaxValue);
        }
    }
    public double NDemandDifferential { get { return MathUtility.Constrain(NDemand - Growth.N, 0.0, double.MaxValue); } }
    public double AvailableRetranslocateN
    {
        get
        {
            double N_min = n_conc_min * Green.Wt;
            double N_avail = MathUtility.Constrain(Green.N - N_min, 0.0, double.MaxValue);
            double n_retrans_fraction = 1.0;
            return (N_avail * n_retrans_fraction);
        }
    }
    public double DltNSenescedRetrans { get { return dlt_n_senesced_retrans; } }
    public void DoNDemand(bool IncludeRetransloation)
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
    public void DoNDemand1Pot(double dltDmPotRue)
    {
        Biomass OldGrowth = Growth;
        Growth.StructuralWt = dltDmPotRue * MathUtility.Divide(Green.Wt, TotalGreen.Wt, 0.0);
        Util.Debug("Stem.Growth.StructuralWt=%f", Growth.StructuralWt);

        Util.CalcNDemand(dltDmPotRue, dltDmPotRue, n_conc_crit, n_conc_max, Growth, Green, Retranslocation.N, 1.0,
                   ref _NDemand, ref NMax);
        Growth.StructuralWt = 0.0;
        Growth.NonStructuralWt = 0.0;
        Util.Debug("Stem.NDemand=%f", _NDemand);
        Util.Debug("Stem.NMax=%f", NMax);
    }
    public void DoSoilNDemand()
    {
        _SoilNDemand = NDemand - dlt_n_senesced_retrans;
        _SoilNDemand = MathUtility.Constrain(_SoilNDemand, 0.0, double.MaxValue);
        Util.Debug("Stem.SoilNDemand=%f", _SoilNDemand);
    }
    public void DoNSupply() { }
    public void DoNRetranslocate(double NSupply, double GrainNDemand)
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
    public void DoNSenescence()
    {
        double green_n_conc = MathUtility.Divide(Green.N, Green.Wt, 0.0);
        double dlt_n_in_senescing_part = Senescing.Wt * green_n_conc;
        double sen_n_conc = Math.Min(NSenescenceConcentration, green_n_conc);

        double SenescingN = Senescing.Wt * sen_n_conc;
        Senescing.StructuralN = MathUtility.Constrain(SenescingN, double.MinValue, Green.N);

        dlt_n_senesced_trans = dlt_n_in_senescing_part - Senescing.N;
        dlt_n_senesced_trans = MathUtility.Constrain(dlt_n_senesced_trans, 0.0, double.MaxValue);

        Util.Debug("Stem.SenescingN=%f", SenescingN);
        Util.Debug("Stem.dlt.n_senesced_trans=%f", dlt_n_senesced_trans);
    }
    public void DoNSenescedRetranslocation(double navail, double n_demand_tot)
    {
        dlt_n_senesced_retrans = navail * MathUtility.Divide(NDemand, n_demand_tot, 0.0);
        Util.Debug("Stem.dlt.n_senesced_retrans=%f", dlt_n_senesced_retrans);
    }
    public void DoNPartition(double GrowthN)
    {
        Growth.StructuralN = GrowthN;
    }
    public void DoNFixRetranslocate(double NFixUptake, double nFixDemandTotal)
    {
        Growth.StructuralN += NFixUptake * MathUtility.Divide(NDemandDifferential, nFixDemandTotal, 0.0);
    }
    public void DoNConccentrationLimits()
    {
        n_conc_crit = NConcentrationCritical.Value;
        n_conc_min = NConcentrationMinimum.Value;
        n_conc_max = NConcentrationMaximum.Value;
        Util.Debug("Stem.n_conc_crit=%f", n_conc_crit);
        Util.Debug("Stem.n_conc_min=%f", n_conc_min);
        Util.Debug("Stem.n_conc_max=%f", n_conc_max);
    }
    public void ZeroDltNSenescedTrans()
    {
        dlt_n_senesced_trans = 0;
    }
    public void DoNUptake(double PotNFix) { }
    

    //cover
    public double CoverGreen { get { return 0; } }
    public double CoverSen { get { return 0; } }
    public void DoPotentialRUE() { }
    public double interceptRadiation(double incomingSolarRadiation) { return 0; }
    public void DoCover() { }

    // update
    public void Update()
    {
        Growth.StructuralN += Leaf.NSenescedTrans;
        Green = Green + Growth - Senescing;

        Senesced = Senesced - Detaching + Senescing;
        Green = Green + Retranslocation;
        Green.StructuralN = Green.N + dlt_n_senesced_retrans;

        Biomass dying = Green * Population.DyingFractionPlants;
        Green = Green - dying;
        Senesced = Senesced + dying;
        Senescing = Senescing + dying;
        Height += DeltaHeight;

        Util.Debug("Stem.Green.Wt=%f", Green.Wt);
        Util.Debug("Stem.Green.N=%f", Green.N);
        Util.Debug("Stem.Senesced.Wt=%f", Senesced.Wt);
        Util.Debug("Stem.Senesced.N=%f", Senesced.N);
        Util.Debug("Stem.Senescing.Wt=%f", Senescing.Wt);
        Util.Debug("Stem.Senescing.N=%f", Senescing.N);
    }
    #endregion

    #region Public interface specific to Stem
    public  double NCrit { get { return n_conc_crit * Green.Wt; } }
    public  double NMin { get { return n_conc_min * Green.Wt; } }
    
    [Output("Height")]
    [Units("mm")]
    public double Height { get; private set; }  // soilwat needs height for its E0 calculation.
    public double Width { get; private set; }

    public double FractionHeightRemoved { get; private set; }
    public double GreenWtPerPlant { get { return MathUtility.Divide(Green.Wt, Population.Density, 0.0); } }

    internal void Morphology()
    {
        DeltaHeight = MathUtility.Constrain(HeightFunction.Value - Height, 0.0, double.MaxValue);
        Util.Debug("Stem.DeltaHeight=%f", DeltaHeight);
    }



    #endregion

    #region Event handlers

    [EventHandler]
    public void OnInitialised()
    {
        Green = new Biomass();
        Senesced = new Biomass();
        Senescing = new Biomass();
        Retranslocation = new Biomass();
        Growth = new Biomass();
        Detaching = new Biomass();
    }

    public  void OnPrepare()
    {
        Growth.Clear();
        Senescing.Clear();
        Detaching.Clear();
        Retranslocation.Clear();
        GreenRemoved.Clear();
        SenescedRemoved.Clear();

        dlt_n_senesced_retrans = 0.0;
        dlt_n_senesced_trans = 0.0;
        dlt_height = 0.0;
        dlt_width = 0.0;

        _DMGreenDemand = 0.0;
        _NDemand = 0.0;
        _SoilNDemand = 0.0;
        NMax = 0.0;
        sw_demand = 0.0;
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

        Senesced = Senesced * RetainFraction.Value;
        Green = Green * RetainFraction.Value;

        Height = MathUtility.Constrain(Harvest.Height, 1.0, double.MaxValue);

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

    #endregion

}

