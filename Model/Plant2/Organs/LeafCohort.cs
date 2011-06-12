using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

public class LeafCohort
{
 #region Class Data Members
    protected double _Population = 0;
    protected double Age = 0;
    protected double Rank = 0;
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
            return Age > (GrowthDuration + LagDuration + SenescenceDuration);
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
            return (Age > GrowthDuration);
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
            return LiveArea / Population;
        }
    }
    virtual public double PotentialAreaGrowthFunction(double TT) // Potential delta LAI
    {
        //return MaxArea*Population * Math.Min(TT, Math.Max(0, GrowthDuration - Age)) / GrowthDuration;

        double growth = Population * (SizeFunction(Age + TT) - SizeFunction(Age));
        return growth;
    }
    protected double SizeFunction(double TT)
    {
        double alpha = -Math.Log((1 / 0.99 - 1) / (MaxArea / (MaxArea * 0.01) - 1)) / GrowthDuration;
        double leafsize = MaxArea / (1 + (MaxArea / (MaxArea * 0.01) - 1) * Math.Exp(-alpha * TT));
        return leafsize;

    }
 #endregion

 #region Leaf cohort functions
    public LeafCohort(double popn, double age, double rank, Function ma, Function gd, Function ld, Function sd, Function sla, double InitialArea, Function CNC, Function MNC, Function SNC, Function INC)
    {
        _Population = popn;
        Rank = rank;
        Age = age;
        MaxArea = ma.Value;
        GrowthDuration = gd.Value;
        LagDuration = ld.Value;
        SenescenceDuration = sd.Value;
        SpecificLeafAreaMax = sla.Value;
        MaximumNConc = CNC.Value;
        MinimumNConc = MNC.Value;
        if (SNC != null)
            StructuralNConc = SNC.Value;

        if (INC != null)
            InitialNConc = INC.Value;

        //if (InitialArea != 0)
        //   if (InitialArea < MaxArea)
        //      Age = GrowthDuration * InitialArea / MaxArea;
        //   else
        //      Age = GrowthDuration;


        LiveArea = InitialArea * Population;
        Live.StructuralWt = LiveArea / SpecificLeafAreaMax;
        Live.StructuralN = Live.StructuralWt * InitialNConc;

    }
    virtual public void DoStartSet(double TT)
    {
    }
    virtual public void DoPotentialGrowth(double TT)
    {
        PotentialAreaGrowth = PotentialAreaGrowthFunction(TT);
    }
    virtual public void DoActualGrowth(double TT)
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
    private double NFac()
    {
        double Nconc = Live.NConc;
        double value = Math.Min(1.0, Math.Max(0.0, (Nconc - MinimumNConc) / (MaximumNConc - MinimumNConc)));
        return value;
    }
    virtual public void DoKill(double fraction)
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
    virtual public void DoFrost(double fraction)
    {
        DoKill(fraction);
    }    
    public double FractionExpanded
    {
        get
        {
            if (Age < GrowthDuration)
                return Age / GrowthDuration;
            else
                return 1.0;
        }
    }
#endregion
    

    


}
   
