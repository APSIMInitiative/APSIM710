using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;
using ModelFramework;

public class LeafCohort
{
 #region Class Data Members
    public double _Population = 0;

    [Param]
    public double Age = 0;

    [Param]
    public double Rank = 0;

    [Param]
    public double Area = 0;

    protected double PotentialAreaGrowth = 0;
    protected double MaxLiveArea = 0;
    public Biomass Live = new Biomass();
    public Biomass Dead = new Biomass();

    protected double GrowthDuration = 0;
    protected double LagDuration = 0;
    protected double SenescenceDuration = 0;
    protected double SpecificLeafAreaMax = 0;
    protected double MaximumNConc = 0;
    protected double MinimumNConc = 0;
    protected double StructuralNConc = 0;
    protected double InitialNConc = 0;

    public double LiveArea = 0;
    public double DeadArea = 0;
    public double MaxArea = 0;
    public double DeltaWt = 0;

    public double StructuralNDemand = 0;
    public double MetabolicNDemand = 0;
    public double NonStructuralNDemand = 0;

    [Link]
    public Leaf Leaf = null;

    [Link(NamePath = "Population")]
    public Population PopulationFunction = null;

    [Link(NamePath = "MaxArea")]
    public Function MaxAreaFunction;

    [Link(NamePath = "GrowthDuration")]
    public Function GrowthDurationFunction;

    [Link(NamePath = "LagDuration")]
    public Function LagDurationFunction;

    [Link(NamePath = "SenescenceDuration")]
    public Function SenescenceDurationFunction;

    [Link(NamePath = "SpecificLeafAreaMax")]
    public Function SpecificLeafAreaMaxFunction;

    [Link(NamePath = "MaximumNConc")]
    public Function MaximumNConcFunction;

    [Link(NamePath = "MinimumNConc")]
    public Function MinimumNConcFunction;

    [Link(NamePath = "StructuralNConc", IsOptional = true)]
    public Function StructuralNConcFunction;

    [Link(NamePath = "InitialNConc", IsOptional = true)]
    public Function InitialNConcFunction;

    [Link]
    public Paddock MyPaddock;
#endregion
    
 #region arbitration methods
    virtual public double DMDemand
    {
        get
        {
            if (IsGrowing)
                return PotentialAreaGrowth / SpecificLeafAreaMax;
            else
                return 0.0;
        }
    }
    virtual public double DMSinkCapacity
    {
        get { return 0.0; }
    }
    virtual public double DMExcessAllocation
    { set { } }
    virtual public double NDemand
    {
        get
        {
            double NDeficit = Math.Max(0.0, MaximumNConc * Live.Wt - Live.N);
            return NDeficit;
        }
    }
    virtual public double DMAllocation
    {
        set
        {
            if (value < -0.00001)
                throw new Exception("Leaf cohort allocation -ve DM value");
            Live.StructuralWt += value;
            //LiveArea += Math.Min(PotentialAreaGrowth,value * SpecificLeafAreaMax);
            LiveArea += PotentialAreaGrowth;
            LiveArea = Math.Min(LiveArea, Live.StructuralWt * SpecificLeafAreaMax);
            DeltaWt = value;
        }
    }
    virtual public double NAllocation
    {
        set
        {
            double StructN = Live.StructuralWt * StructuralNConc;
            double Ndmd = StructN - Live.StructuralN;
            if (Ndmd < 0)
                Ndmd = 0.0;
            if (Ndmd > value)
                Ndmd = value;

            Live.StructuralN += Ndmd;
            Live.NonStructuralN += value - Ndmd;

            //Live.StructuralN += value/3.0;
            //Live.NonStructuralN += 2.0*value/3.0;
        }
    }
    virtual public double NRetranslocationSupply
    {
        get
        {
            double Nf = NFac();
            //return Live.NonStructuralN *Nf;
            return Live.NonStructuralN;
        }
    }
    virtual public double NRetranslocation
    {
        set
        {
            if (value > Live.NonStructuralN)
                throw new Exception("A leaf cohort cannot supply that amount for N retranslocation");
            Live.NonStructuralN = Live.NonStructuralN - value;
        }
    }
 #endregion

 #region Cohort phenology
    public double NodeAge
    {
        get { return Age; }
    }
    public bool Finished
    {
        get
        {
            return IsInitialised && Age > (GrowthDuration + LagDuration + SenescenceDuration);
        }
    }
    public bool IsGrowing
    {
        get { return (Age < GrowthDuration); }
    }
    public bool IsGreen
    {
        get { return (Age < (GrowthDuration + LagDuration + SenescenceDuration)); }
    }
    public bool IsSenescing
    {
        get { return (Age < (GrowthDuration + LagDuration + SenescenceDuration) && (Age > (GrowthDuration + LagDuration))); }
    }
    public bool IsNotSenescing
    {
        get { return (Age < (GrowthDuration + LagDuration)); }
    }
    public bool ShouldBeDead
    {
        get { return (Age > (GrowthDuration + LagDuration + SenescenceDuration)); }
    }
    public bool IsAlive
    {
        get { return ((Age >= 0) && (Age < (GrowthDuration + LagDuration + SenescenceDuration))); }
    }
    public bool IsInitialised
    {
        get { return _Population > 0; }
    }
    public bool IsDead
    {
        get
        {
            return MathUtility.FloatsAreEqual(LiveArea, 0.0) && !MathUtility.FloatsAreEqual(DeadArea, 0.0);
        }
    }
    public bool IsFullyExpanded
    {
        get
        {
            return (IsInitialised && Age > GrowthDuration);
        }
    }
 #endregion

 #region Cohort leaf expansion 
    public double MaxSize
    {
        get
        {
            return MaxLiveArea / Population;
        }
    }
    public double Population
    {
        get
        {
            return _Population;
        }
    }
    public double Size
    {
        get
        {
            if (IsInitialised)
                return LiveArea / Population;
            else
                return 0;
        }
    }

    /// <summary>
    /// Potential delta LAI
    /// </summary>
    /// <param name="TT">thermal-time</param>
    /// <returns>(mm2 leaf/cohort position/m2 soil/day)</returns>
    virtual public double PotentialAreaGrowthFunction(double TT) 
    {
        double leafSizeDelta = SizeFunction(Age + TT) - SizeFunction(Age); //mm2 of leaf expanded in one day at this cohort (Today's minus yesterday's Area/cohort)
        double growth = Population * leafSizeDelta; // Daily increase in leaf area for that cohort position in a per m2 basis (mm2/m2/day)
        return growth;                              // FIXME-EIT Unit conversion to m2/m2 could happen here and population could be considered at higher level only (?)
    }

    /// <summary>
    /// Potential average leaf size for today per cohort (no stress)
    /// </summary>
    /// <param name="TT">Thermal-time accumulation since cohort initiation</param>
    /// <returns>Average leaf size (mm2/leaf)</returns>
    protected double SizeFunction(double TT)
    {
        double alpha = -Math.Log((1 / 0.99 - 1) / (MaxArea / (MaxArea * 0.01) - 1)) / GrowthDuration;
        double leafsize = MaxArea / (1 + (MaxArea / (MaxArea * 0.01) - 1) * Math.Exp(-alpha * TT));
        return leafsize;

    }
 #endregion

 #region Leaf cohort functions

    /// <summary>
    /// The default constuctor that will be called by the APSIM infrastructure.
    /// </summary>
    public LeafCohort()
    {
    }
    /// <summary>
    /// Returns a clone of this object
    /// </summary>
    public virtual LeafCohort Clone()
    {
        LeafCohort NewLeaf = (LeafCohort) this.MemberwiseClone();
        NewLeaf.Live = new Biomass();
        NewLeaf.Dead = new Biomass();
        return NewLeaf;
    }


    virtual public void DoInitialisation()
    {
        // _Population will equal 0 when the APSIM infrastructure creates this object.
        // It will already be set if PLANT creates this object and population is passed
        // in as an argument in the constructor below. Confusing isn't it?
        if (_Population == 0)
            _Population = PopulationFunction.Value * Leaf.PrimaryBudNo;
        MaxArea = MaxAreaFunction.Value; 
        GrowthDuration = GrowthDurationFunction.Value;
        LagDuration = LagDurationFunction.Value;
        SenescenceDuration = SenescenceDurationFunction.Value;
        SpecificLeafAreaMax = SpecificLeafAreaMaxFunction.Value;
        MaximumNConc = MaximumNConcFunction.Value;
        MinimumNConc = MinimumNConcFunction.Value;
        if (StructuralNConcFunction != null)
            StructuralNConc = StructuralNConcFunction.Value;

        if (InitialNConcFunction != null)
            InitialNConc = InitialNConcFunction.Value;

        Age = Area / MaxArea * GrowthDuration;
        LiveArea = Area * _Population;
        Live.StructuralWt = LiveArea / SpecificLeafAreaMax;
        Live.StructuralN = Live.StructuralWt * InitialNConc;

    }

    [EventHandler]
    public void OnInitialised()
    {
        MyPaddock.Subscribe(Leaf.InitialiseStage, DoInitialisation);
    }

   // virtual public void DoStartSet(double TT)
   // {
   // }
    virtual public void DoPotentialGrowth(double TT)
    {
        if (IsInitialised)
            PotentialAreaGrowth = PotentialAreaGrowthFunction(TT);
    }
    virtual public void DoActualGrowth(double TT)
    {
        if (IsInitialised)
        {
            if (MaxLiveArea < LiveArea)
                MaxLiveArea = LiveArea;
            //Calculate Senescence
            double FractionSenescing = 0;
            double AreaSenescing = 0;
            double TTInSenPhase = Math.Max(0.0, Age + TT - LagDuration - GrowthDuration);

            if (TTInSenPhase > 0)
            {

                if (Rank == 10)
                { }

                double LeafDuration = GrowthDuration + LagDuration + SenescenceDuration;
                double RemainingTT = Math.Max(0, LeafDuration - Age);

                if (RemainingTT == 0)
                    FractionSenescing = 1;
                else
                    FractionSenescing = Math.Min(1, Math.Min(TT, TTInSenPhase) / RemainingTT);

                if ((FractionSenescing > 1) || (FractionSenescing < 0))
                {
                    throw new Exception("Bad Fraction Senescing");
                }
                // Update State Variables
                AreaSenescing = LiveArea * FractionSenescing;
                DeadArea = DeadArea + AreaSenescing;
                LiveArea = LiveArea - AreaSenescing;

                double Senescing = FractionSenescing * Live.StructuralWt;
                Live.StructuralWt -= Senescing;
                Dead.StructuralWt += Senescing;

                Senescing = FractionSenescing * Live.NonStructuralWt;
                Live.NonStructuralWt -= Senescing;
                Dead.NonStructuralWt += Senescing;

                Senescing = FractionSenescing * Live.NonStructuralN;
                Live.NonStructuralN -= Senescing;
                Dead.NonStructuralN += Senescing;

                Senescing = FractionSenescing * Live.StructuralN;
                Live.StructuralN -= Senescing;
                Dead.StructuralN += Senescing;
            }

            Age = Age + TT;
        }
    }
    private double NFac()
    {
        if (IsInitialised)
        {
            double Nconc = Live.NConc;
            double value = Math.Min(1.0, Math.Max(0.0, (Nconc - MinimumNConc) / (MaximumNConc - MinimumNConc)));
            return value;
        }
        else
            return 0;
    }
    virtual public void DoKill(double fraction)
    {
        if (IsInitialised)
        {
            double change;
            change = LiveArea * fraction;
            LiveArea -= change;
            DeadArea += change;

            change = Live.StructuralWt * fraction;
            Live.StructuralWt -= change;
            Dead.StructuralWt += change;

            change = Live.NonStructuralWt * fraction;
            Live.NonStructuralWt -= change;
            Dead.NonStructuralWt += change;

            change = Live.StructuralN * fraction;
            Live.StructuralN -= change;
            Dead.StructuralN += change;

            change = Live.NonStructuralN * fraction;
            Live.NonStructuralN -= change;
            Dead.NonStructuralN += change;
        }
    }
    virtual public void DoFrost(double fraction)
    {
        if (IsInitialised)
            DoKill(fraction);
    }    
    public double FractionExpanded
    {
        get
        {
            if (IsInitialised && Age < GrowthDuration)
                return Age / GrowthDuration;
            else
                return 1.0;
        }
    }
#endregion
    

    


}
   
