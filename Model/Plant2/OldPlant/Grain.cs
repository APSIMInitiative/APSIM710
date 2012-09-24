using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CSGeneral;

public class Grain : Organ1, AboveGround, Reproductive
{
    [Link]
    SWStress SWStress = null;

    [Link]
    NStress NStress = null;

    [Link]
    Function TempStress = null;

    [Link]
    Stem1 Stem = null;

    [Link]
    Root1 Root = null;

    [Link]
    Phenology Phenology = null;

    [Link]
    Population1 Population = null;

    [Link]
    Function GrainGrowthPeriod = null;

    [Link]
    Function ReproductivePeriod = null;

    [Link]
    Function RelativeGrainFill = null;

    [Link]
    Function RelativeGrainNFill = null;

    [Link]
    Function DMSenescenceFraction = null;

    [Link]
    Function NConcentrationCritical = null;

    [Link]
    Function NConcentrationMinimum = null;

    [Link]
    Function NConcentrationMaximum = null;

    [Param]
    double InitialWt = 0;

    [Param]
    double InitialNConcentration = 0;

    [Param]
    double GrainsPerGramStem = 0;

    [Param]
    double PotentialGrainFillingRate = 0;

    [Param]
    double PotentialGrainGrowthRate = 0;

    [Param]
    double MinimumGrainNFillingRate = 0;

    [Param]
    double CriticalGrainFillingRate = 0;

    [Param]
    double GrainMaxDailyNConc = 0;

    [Param]
    double PotentialGrainNFillingRate = 0;

    [Param]
    double MaxGrainSize = 0;

    [Param]
    double NSenescenceConcentration = 0;

    [Param]
    double SenescenceDetachmentFraction = 0;

    [Param]
    public double WaterContentFraction = 0;

    // ***********
    [Link]
    CompositeBiomass WholePlantGreen = null;

    [Link]
    Function GrowthStructuralFractionStage = null;

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
    internal double NDemand2 { get { return MathUtility.Constrain(NDemand - dlt.n_senesced_retrans - Growth.N, 0.0, double.MaxValue); } }
    internal override double SoilNDemand { get { return _SoilNDemand; } }
    internal override double SWDemand { get { return sw_demand; } }
    internal override double DMGreenDemand { get { return _DMGreenDemand; } }
    internal override double DMDemandDifferential
    {
        get
        {
            return MathUtility.Constrain(DMGreenDemand - _Growth.Wt, 0.0, Double.MaxValue);
        }
    }
    internal override double NCapacity { get  { return 0.0; } }
    internal override double NDemandDifferential { get { return MathUtility.Constrain(NDemand - Growth.N, 0.0, double.MaxValue); } }
    internal override double AvailableRetranslocateN { get { return 0.0; } }
    internal override double DltNSenescedRetrans { get { return dlt.n_senesced_retrans; } }

    internal override void DoNRetranslocate(double NSupply, double GrainNDemand)
    {
        if (GrainNDemand >= NSupply)
        {
            // demand greater than or equal to supply
            // retranslocate all available N
            Retranslocation.StructuralN = NSupply;
        }
        else
        {
            // supply greater than demand. Retranslocate what is needed
            Retranslocation.StructuralN = GrainNDemand;
        }
        Util.Debug("meal.Retranslocation.N=%f", Retranslocation.N);
    }

    [Output("Yield")]
    [Units("kg/ha")]
    public double Yield { get { return Green.Wt * 10; } }  // convert to kg/ha

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
    }
    internal override void OnHarvest(HarvestType Harvest, BiomassRemovedType BiomassRemoved)
    {
        int i = IncreaseSizeOfBiomassRemoved(BiomassRemoved);
        BiomassRemoved.dm_type[i] = "meal";
        BiomassRemoved.fraction_to_residue[i] = 0.0F;
        BiomassRemoved.dlt_crop_dm[i] = (float)((Green.Wt + Senesced.Wt) * Conversions.gm2kg / Conversions.sm2ha);
        BiomassRemoved.dlt_dm_n[i] = (float)((Green.N + Senesced.N) * Conversions.gm2kg / Conversions.sm2ha);
        //BiomassRemoved.dlt_dm_p[i] = (float)((Green.P + Senesced.P) * Conversions.gm2kg / Conversions.sm2ha);

        Green.Clear();
        Senesced.Clear();
    }

    [EventHandler]
    public void OnPhaseChanged(PhaseChangedType PhenologyChange)
    {
        if (PhenologyChange.NewPhaseName == "EmergenceToEndOfJuvenile")
        {
            Green.StructuralWt = InitialWt * Population.Density;
            Green.StructuralN = InitialNConcentration * Green.StructuralWt;
            Util.Debug("meal.InitGreen.StructuralWt=%f", Green.StructuralWt);
            Util.Debug("meal.InitGreen.StructuralN=%f", Green.StructuralN);
        }
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
        Util.Debug("meal.Growth.StructuralWt=%f", _Growth.StructuralWt);
        Util.Debug("meal.Growth.NonStructuralWt=%f", _Growth.NonStructuralWt);
    }


    private double Dlt_dm_stress_max = 0;
    public double GrainNo = 0;
    private double DltDMGrainDemand;
    private static double N_grain_demand;

    internal override void DoNDemand1Pot(double dltDmPotRue)
    {
        // no n demand for grain
    }

    internal void doProcessBioDemand()
    {
        DoDMDemandStress();
        DoGrainNumber();
        DoDMDemandGrain();
    }

    private void DoGrainNumber()
    {
        if (Phenology.OnDayOf("emergence"))
        {
            // seedling has just emerged.
            GrainNo = 0.0;
        }
        else if (Phenology.OnDayOf("flowering"))
        {
            // we are at first day of grainfill.
            GrainNo = GrainsPerGramStem * Stem.Green.Wt;
        }
        else
        {
            // no changes
        }
        Util.Debug("Grian.GrainNumber=%f", GrainNo);
    }


    /// <summary>
    ///  Find maximum stress on daily dm production (0-1)
    /// </summary>
    void DoDMDemandStress()
    {
        double RueReduction;          // Effect of non-optimal N and Temp conditions on RUE (0-1)

        RueReduction = Math.Min(TempStress.Value, NStress.Photo);
        Dlt_dm_stress_max = SWStress.Photo * RueReduction;
        Util.Debug("Grain.Dlt_dm_stress_max=%f", Dlt_dm_stress_max);
    }


    void DoDMDemandGrain()
       {
       if (GrainGrowthPeriod.Value == 1)
          {
          // Perform grain filling calculations

              if (Phenology.InPhase("StartGrainFillToEndGrainFill"))
                DltDMGrainDemand = GrainNo * PotentialGrainFillingRate * RelativeGrainFill.Value;
          else
             {
             // we are in the flowering to grainfill phase
                 DltDMGrainDemand = GrainNo * PotentialGrainGrowthRate * RelativeGrainFill.Value;
              }
           // check that grain growth will not result in daily n conc below minimum conc
           // for daily grain growth
          double nfact_grain_fill = Math.Min(1.0, NStress.Grain *PotentialGrainNFillingRate / MinimumGrainNFillingRate);
          DltDMGrainDemand = DltDMGrainDemand * nfact_grain_fill;



          // Check that growth does not exceed maximum grain size
          double max_grain = GrainNo * MaxGrainSize;

          double max_dlt = Math.Max(max_grain - Green.Wt, 0.0);
          DltDMGrainDemand = Math.Min(DltDMGrainDemand, max_dlt);
          _DMGreenDemand = Math.Max(DltDMGrainDemand, 0.0);
          }
       else
           DltDMGrainDemand = 0.0;
       Util.Debug("Grain.Dlt_dm_grain_demand=%f", DltDMGrainDemand);
       }

    internal override void DoSenescence()
    {
        double fraction_senescing = MathUtility.Constrain(DMSenescenceFraction.Value, 0.0, 1.0);

        _Senescing.StructuralWt = (Green.StructuralWt + _Growth.StructuralWt + Retranslocation.StructuralWt) * fraction_senescing;
        _Senescing.NonStructuralWt = (Green.NonStructuralWt + _Growth.NonStructuralWt + Retranslocation.NonStructuralWt) * fraction_senescing;
        Util.Debug("meal.Senescing.StructuralWt=%f", _Senescing.StructuralWt);
        Util.Debug("meal.Senescing.NonStructuralWt=%f", _Senescing.NonStructuralWt);

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

        Util.Debug("meal.SenescingN=%f", SenescingN);
        Util.Debug("meal.dlt.n_senesced_trans=%f", dlt.n_senesced_trans);
    }
    internal override void DoNSenescedRetranslocation(double navail, double n_demand_tot)
    {
        dlt.n_senesced_retrans = navail * MathUtility.Divide(NDemand, n_demand_tot, 0.0);
        Util.Debug("meal.dlt.n_senesced_retrans=%f", dlt.n_senesced_retrans);
    }

    public double DltDmPotentialGrain 
    { 
        get
        {
            return DltDMGrainDemand; // oilPart->removeEnergy(DltDMGrainDemand);
        }
    }

    internal void DoNDemandGrain()
    {
        double Tav;
        double grain_growth;

        // default case
        double gN_grain_demand1 = 0.0;
        double gN_grain_demand2 = 0.0;
        N_grain_demand = 0.0;
        if (ReproductivePeriod.Value == 1)
        {
            // we are in grain filling stage

            gN_grain_demand1 = GrainNo
                           * PotentialGrainNFillingRate * NStress.Grain
                           * RelativeGrainNFill.Value;

            gN_grain_demand2 = Math.Min(GrainNo * PotentialGrainNFillingRate * RelativeGrainNFill.Value, Root.NSupply);
            N_grain_demand = Math.Max(gN_grain_demand1, gN_grain_demand2);
            N_grain_demand = gN_grain_demand1;

        }

        if (GrainGrowthPeriod.Value == 1)
        {
            // during grain C filling period so make sure that C filling is still
            // going on otherwise stop putting N in now

            grain_growth = MathUtility.Divide(_Growth.Wt + Retranslocation.Wt, GrainNo, 0.0);
            if (grain_growth < CriticalGrainFillingRate)
            {
                //! grain filling has stopped - stop n flow as well
                N_grain_demand = 0.0;
            }
            double dailyNconc = MathUtility.Divide(N_grain_demand, (_Growth.Wt + Retranslocation.Wt), 1.0);
            if (dailyNconc > GrainMaxDailyNConc) 
                N_grain_demand = (_Growth.Wt + Retranslocation.Wt) * GrainMaxDailyNConc;
        }
        Util.Debug("Grain.N_grain_demand=%f", N_grain_demand);
    }

    internal override void DoNDemand(bool IncludeRetranslocation)
    {
        _NDemand = N_grain_demand;
        Util.Debug("Grain.NDemand=%f", _NDemand);
    }

    internal override void DoSoilNDemand()
    {
        _SoilNDemand = NDemand - dlt.n_senesced_retrans;
        _SoilNDemand = MathUtility.Constrain(_SoilNDemand, 0.0, double.MaxValue);
        Util.Debug("meal.SoilNDemand=%f", _SoilNDemand);
    }
    internal override void DoDetachment()
    {
        _Detaching = Senesced * SenescenceDetachmentFraction;
        Util.Debug("meal.Detaching.Wt=%f", _Detaching.Wt);
        Util.Debug("meal.Detaching.N=%f", _Detaching.N);
    }

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

        //if (HIStressSensitivePeriod.Value == 1)
        //{

        //}

        double dlt_grain_no_lost = GrainNo * Population.DyingFractionPlants;
        GrainNo -= dlt_grain_no_lost;

        Util.Debug("meal.Green.Wt=%f", _Green.Wt);
        Util.Debug("meal.Green.N=%f", _Green.N);
        Util.Debug("meal.Senesced.Wt=%f", _Senesced.Wt);
        Util.Debug("meal.Senesced.N=%f", _Senesced.N);
        Util.Debug("meal.Senescing.Wt=%f", _Senescing.Wt);
        Util.Debug("meal.Senescing.N=%f", _Senescing.N);
        Util.Debug("meal.GrainNo=%f", GrainNo);
    }

    internal override void DoNConccentrationLimits()
    {
        n_conc_crit = NConcentrationCritical.Value;
        n_conc_min = NConcentrationMinimum.Value;
        n_conc_max = NConcentrationMaximum.Value;
        Util.Debug("meal.n_conc_crit=%f", n_conc_crit);
        Util.Debug("meal.n_conc_min=%f", n_conc_min);
        Util.Debug("meal.n_conc_max=%f", n_conc_max);
    }


    internal override void DoDmRetranslocate(double DMAvail, double DMDemandDifferentialTotal)
    {
        Retranslocation.NonStructuralWt = DMAvail * MathUtility.Divide(DMDemandDifferential, DMDemandDifferentialTotal, 0.0);
        Util.Debug("meal.Retranslocation=%f", Retranslocation.NonStructuralWt);
    }

    internal void WriteCultivarInfo()
    {
        Console.WriteLine(string.Format("   grains_per_gram_stem           = {0,10:F1} (/g)", GrainsPerGramStem));
        Console.WriteLine(string.Format("   potential_grain_filling_rate   = {0,10:F4} (g/grain/day)", PotentialGrainFillingRate));
        Console.WriteLine(string.Format("   potential_grain_growth_rate    = {0,10:F4} (g/grain/day)", PotentialGrainGrowthRate));
        Console.WriteLine(string.Format("   max_grain_size                 = {0,10:F4} (g)", MaxGrainSize));
    }
}

