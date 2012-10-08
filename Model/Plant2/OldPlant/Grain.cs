using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CSGeneral;
using ModelFramework;

public class Grain : Organ1, AboveGround, Reproductive
{
    #region Parameters read from XML file and links to other functions.
    [Link]
    public Component My;

    [Link]
    Plant15 Plant = null;

    [Link]
    SWStress SWStress = null;

    [Link]
    NStress NStress = null;

    [Link]
    Function TempStress = null;

    [Link]
    Stem1 Stem = null;

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

    [Link]
    Function GrowthStructuralFractionStage = null;

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

    #endregion

    #region Private variables
    public double dlt_dm_pot_rue;
    public double dlt_n_senesced_retrans;           // plant N retranslocated to/from (+/-) senesced part to/from <<somewhere else??>> (g/m^2)
    public double dlt_n_senesced_trans;
    public double dlt_height;                       // growth upwards (mm)
    public double dlt_width;                        // growth outwards (mm)
    private Biomass GreenRemoved = new Biomass();
    private Biomass SenescedRemoved = new Biomass();
    private double _DMGreenDemand;
    private double _NDemand;
    private double _SoilNDemand;
    private double sw_demand;
    private double n_conc_crit = 0;
    private double n_conc_max = 0;
    private double n_conc_min = 0;
    private double DltDMGrainDemand;
    private double N_grain_demand;
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
    public double DMSupply { get { return 0; } }
    public double DMRetransSupply { get { return 0; } }
    public double dltDmPotRue { get { return 0; } }
    public double DMGreenDemand { get { return _DMGreenDemand; } }
    public double DMDemandDifferential
    {
        get
        {
            return MathUtility.Constrain(DMGreenDemand - Growth.Wt, 0.0, Double.MaxValue);
        }
    }
    public void DoDMDemand(double DMSupply) { }
    public void DoDmRetranslocate(double DMAvail, double DMDemandDifferentialTotal)
    {
        Retranslocation.NonStructuralWt = DMAvail * MathUtility.Divide(DMDemandDifferential, DMDemandDifferentialTotal, 0.0);
        Util.Debug("meal.Retranslocation=%f", Retranslocation.NonStructuralWt);
    }
    public void GiveDmGreen(double Delta)
    {
        Growth.StructuralWt += Delta * GrowthStructuralFractionStage.Value;
        Growth.NonStructuralWt += Delta * (1.0 - GrowthStructuralFractionStage.Value);
        Util.Debug("meal.Growth.StructuralWt=%f", Growth.StructuralWt);
        Util.Debug("meal.Growth.NonStructuralWt=%f", Growth.NonStructuralWt);
    }
    public void DoSenescence()
    {
        double fraction_senescing = MathUtility.Constrain(DMSenescenceFraction.Value, 0.0, 1.0);

        Senescing.StructuralWt = (Green.StructuralWt + Growth.StructuralWt + Retranslocation.StructuralWt) * fraction_senescing;
        Senescing.NonStructuralWt = (Green.NonStructuralWt + Growth.NonStructuralWt + Retranslocation.NonStructuralWt) * fraction_senescing;
        Util.Debug("meal.Senescing.StructuralWt=%f", Senescing.StructuralWt);
        Util.Debug("meal.Senescing.NonStructuralWt=%f", Senescing.NonStructuralWt);

    }
    public void DoDetachment()
    {
        Detaching = Senesced * SenescenceDetachmentFraction;
        Util.Debug("meal.Detaching.Wt=%f", Detaching.Wt);
        Util.Debug("meal.Detaching.N=%f", Detaching.N);
    }

    // nitrogen
    public  double NDemand { get { return _NDemand; } }
    public double NSupply { get { return 0; } }
    public double NUptake { get { return 0; } }
    public  double SoilNDemand { get { return _SoilNDemand; } }
    public  double NCapacity { get  { return 0.0; } }
    public  double NDemandDifferential { get { return MathUtility.Constrain(NDemand - Growth.N, 0.0, double.MaxValue); } }
    public  double AvailableRetranslocateN { get { return 0.0; } }
    public  double DltNSenescedRetrans { get { return dlt_n_senesced_retrans; } }
    public void DoNDemand(bool IncludeRetranslocation)
    {
        _NDemand = N_grain_demand;
        Util.Debug("Grain.NDemand=%f", _NDemand);
    }
    public void DoNDemand1Pot(double dltDmPotRue)
    {
        // no n demand for grain
    }
    public void DoSoilNDemand()
    {
        _SoilNDemand = NDemand - dlt_n_senesced_retrans;
        _SoilNDemand = MathUtility.Constrain(_SoilNDemand, 0.0, double.MaxValue);
        Util.Debug("meal.SoilNDemand=%f", _SoilNDemand);
    }
    public void DoNSupply() { }
    public void DoNRetranslocate(double NSupply, double GrainNDemand)
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
    public void DoNSenescence()
    {
        double green_n_conc = MathUtility.Divide(Green.N, Green.Wt, 0.0);
        double dlt_n_in_senescing_part = Senescing.Wt * green_n_conc;
        double sen_n_conc = Math.Min(NSenescenceConcentration, green_n_conc);

        double SenescingN = Senescing.Wt * sen_n_conc;
        Senescing.StructuralN = MathUtility.Constrain(SenescingN, double.MinValue, Green.N);

        dlt_n_senesced_trans = dlt_n_in_senescing_part - Senescing.N;
        dlt_n_senesced_trans = MathUtility.Constrain(dlt_n_senesced_trans, 0.0, double.MaxValue);

        Util.Debug("meal.SenescingN=%f", SenescingN);
        Util.Debug("meal.dlt.n_senesced_trans=%f", dlt_n_senesced_trans);
    }
    public void DoNSenescedRetranslocation(double navail, double n_demand_tot)
    {
        dlt_n_senesced_retrans = navail * MathUtility.Divide(NDemand, n_demand_tot, 0.0);
        Util.Debug("meal.dlt.n_senesced_retrans=%f", dlt_n_senesced_retrans);
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
        Util.Debug("meal.n_conc_crit=%f", n_conc_crit);
        Util.Debug("meal.n_conc_min=%f", n_conc_min);
        Util.Debug("meal.n_conc_max=%f", n_conc_max);
    }
    public void ZeroDltNSenescedTrans()
    {
        dlt_n_senesced_trans = 0;
    }
    public void DoNUptake(double PotNFix) { }

    // cover
    public  double CoverGreen { get { return 0; } }
    public  double CoverSen { get { return 0; } }
    public  void DoPotentialRUE() { }
    public  double interceptRadiation(double incomingSolarRadiation) { return 0; }
    public  void DoCover() { }

    // update
    public void Update()
    {
        Green = Green + Growth - Senescing;

        Senesced = Senesced - Detaching + Senescing;
        Green = Green + Retranslocation;
        Green.StructuralN = Green.N + dlt_n_senesced_retrans;

        Biomass dying = Green * Population.DyingFractionPlants;
        Green = Green - dying;
        Senesced = Senesced + dying;
        Senescing = Senescing + dying;

        //if (HIStressSensitivePeriod.Value == 1)
        //{

        //}

        double dlt_grain_no_lost = GrainNo * Population.DyingFractionPlants;
        GrainNo -= dlt_grain_no_lost;

        Util.Debug("meal.Green.Wt=%f", Green.Wt);
        Util.Debug("meal.Green.N=%f", Green.N);
        Util.Debug("meal.Senesced.Wt=%f", Senesced.Wt);
        Util.Debug("meal.Senesced.N=%f", Senesced.N);
        Util.Debug("meal.Senescing.Wt=%f", Senescing.Wt);
        Util.Debug("meal.Senescing.N=%f", Senescing.N);
        Util.Debug("meal.GrainNo=%f", GrainNo);
    }

    #endregion

    #region Public interface specific to Grain
    [Output("Yield")]
    [Units("kg/ha")]
    public double Yield { get { return Green.Wt * 10; } }  // convert to kg/ha

    [Output]
    public double GrainNo { get; private set; }


    internal double NDemand2 { get { return MathUtility.Constrain(NDemand - dlt_n_senesced_retrans - Growth.N, 0.0, double.MaxValue); } }
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
    void DoDMDemandStress()
    {
        double RueReduction;          // Effect of non-optimal N and Temp conditions on RUE (0-1)

        RueReduction = Math.Min(TempStress.Value, NStress.Photo);
        double Dlt_dm_stress_max = SWStress.Photo * RueReduction;
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
            double nfact_grain_fill = Math.Min(1.0, NStress.Grain * PotentialGrainNFillingRate / MinimumGrainNFillingRate);
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
    public double DltDmPotentialGrain
    {
        get
        {
            return DltDMGrainDemand; // oilPart->removeEnergy(DltDMGrainDemand);
        }
    }
    internal void DoNDemandGrain()
    {
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

            // calculate total N supply
            double NSupply = 0;
            foreach (Organ1 Organ in Plant.Organ1s)
                NSupply += Organ.NSupply;

            gN_grain_demand2 = Math.Min(GrainNo * PotentialGrainNFillingRate * RelativeGrainNFill.Value, NSupply);
            N_grain_demand = Math.Max(gN_grain_demand1, gN_grain_demand2);
            N_grain_demand = gN_grain_demand1;

        }

        if (GrainGrowthPeriod.Value == 1)
        {
            // during grain C filling period so make sure that C filling is still
            // going on otherwise stop putting N in now

            grain_growth = MathUtility.Divide(Growth.Wt + Retranslocation.Wt, GrainNo, 0.0);
            if (grain_growth < CriticalGrainFillingRate)
            {
                //! grain filling has stopped - stop n flow as well
                N_grain_demand = 0.0;
            }
            double dailyNconc = MathUtility.Divide(N_grain_demand, (Growth.Wt + Retranslocation.Wt), 1.0);
            if (dailyNconc > GrainMaxDailyNConc)
                N_grain_demand = (Growth.Wt + Retranslocation.Wt) * GrainMaxDailyNConc;
        }
        Util.Debug("Grain.N_grain_demand=%f", N_grain_demand);
    }
    internal void WriteCultivarInfo()
    {
        Console.WriteLine(string.Format("   grains_per_gram_stem           = {0,10:F1} (/g)", GrainsPerGramStem));
        Console.WriteLine(string.Format("   potential_grain_filling_rate   = {0,10:F4} (g/grain/day)", PotentialGrainFillingRate));
        Console.WriteLine(string.Format("   potential_grain_growth_rate    = {0,10:F4} (g/grain/day)", PotentialGrainGrowthRate));
        Console.WriteLine(string.Format("   max_grain_size                 = {0,10:F4} (g)", MaxGrainSize));
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

        dlt_dm_pot_rue = 0.0;
        dlt_n_senesced_retrans = 0.0;
        dlt_n_senesced_trans = 0.0;
        dlt_height = 0.0;
        dlt_width = 0.0;

        _DMGreenDemand = 0.0;
        _NDemand = 0.0;
        _SoilNDemand = 0.0;
        sw_demand = 0.0;
    }
    public  void OnHarvest(HarvestType Harvest, BiomassRemovedType BiomassRemoved)
    {
        int i = Util.IncreaseSizeOfBiomassRemoved(BiomassRemoved);
        BiomassRemoved.dm_type[i] = "meal";
        BiomassRemoved.fraction_to_residue[i] = 0.0F;
        BiomassRemoved.dlt_crop_dm[i] = (float)((Green.Wt + Senesced.Wt) * Conversions.gm2kg / Conversions.sm2ha);
        BiomassRemoved.dlt_dm_n[i] = (float)((Green.N + Senesced.N) * Conversions.gm2kg / Conversions.sm2ha);
        //BiomassRemoved.dlt_dm_p[i] = (float)((Green.P + Senesced.P) * Conversions.gm2kg / Conversions.sm2ha);

        Green.Clear();
        Senesced.Clear();
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
            Util.Debug("meal.InitGreen.StructuralWt=%f", Green.StructuralWt);
            Util.Debug("meal.InitGreen.StructuralN=%f", Green.StructuralN);
        }
    }

    #endregion

}

