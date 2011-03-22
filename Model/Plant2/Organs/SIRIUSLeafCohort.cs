using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

class SIRIUSLeafCohort
{
 #region Class Data Members
    private double _Population = 0;
    private double Age = 0;
    private double Rank = 0;
    private double PotentialAreaGrowth = 0;
    private double SenescedFrac = 0;
    public double PotentialSize = 0;
    public double PotentialArea = 0; 
    public double LiveArea = 0;
    public double MaxLiveArea = 0;
    public double DeadArea = 0;
    public Biomass Live = new Biomass();
    public Biomass Dead = new Biomass();

    public double MaxArea = 0;
    private double GrowthDuration = 0;
    private double LagDuration = 0;
    private double SenescenceDuration = 0;
    private double SpecificLeafAreaMax = 0;
    private double MaximumNConc = 0;
    private double MinimumNConc = 0;
    private double InitialNConc = 0;
    private double StructuralFraction = 0;
    public double Nreallocated = 0;
    public double DMretranslocated = 0;
    private double NReallocationFactor = 0;
    private double NRetranslocationRate = 0;
    public double DeltaWt = 0;
    public double DeltaStructuralWt = 0;
    public double DeltaNonStructuralWt = 0;
    public double LeafStartNRetranslocationSupply = 0;
    public double LeafStartNReallocationSupply = 0;
    public double LeafStartDMRetranslocationSupply = 0;
    public double LeafStartStructuralN = 0;
    public double LeafStartNonStructuralN = 0;
    public double LeafStartStructuralWt = 0;
    public double LeafStartNonStructuralWt = 0;
    public double PotentialDMAllocation = 0;
 #endregion
    
 #region Phenology state variables
    // leaf age and potential leaf size functions
    public double NodeAge
    {
        get { return Age; }
    }
    public bool   Finished
    {
        get
        {
            return Age > (GrowthDuration + LagDuration + SenescenceDuration);
        }
    }
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
    public bool   IsGrowing
    {
        get { return (Age < GrowthDuration); }
    }
    public bool   IsGreen
    {
        get { return (Age < (GrowthDuration + LagDuration + SenescenceDuration)); }
    }
    public bool   IsSenescing
    {
        get { return (Age < (GrowthDuration + LagDuration + SenescenceDuration) && (Age > (GrowthDuration + LagDuration))); }
    }
    public bool   IsAlive
    {
        get { return ((Age >= 0) && (Age < (GrowthDuration + LagDuration + SenescenceDuration))); }
    }
    public bool   ShouldBeDead
    {
        get { return (Age > (GrowthDuration + LagDuration + SenescenceDuration)); }
    }
    public bool   IsDead
    {
        get
        {
            return MathUtility.FloatsAreEqual(LiveArea, 0.0) && !MathUtility.FloatsAreEqual(DeadArea, 0.0);
        }
    }
    public bool   IsFullyExpanded
    {
        get
        {
            return (Age > GrowthDuration);
        }
    }
    public bool   IsNotSenescing
    {
        get { return (Age < (GrowthDuration + LagDuration)); }
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

 #region Biomass state variables and deltas
    // variables passed up to leaf parent for arbitrator variables and passed back down to update biomass and 
    // N after arbitration
    private double NFac()
    {
        double Nconc = Live.NConc;
        double value = Math.Min(1.0, Math.Max(0.0, (Nconc - MinimumNConc) / (MaximumNConc - MinimumNConc)));
        return value;
    }
    public double DMDemand
     
    {
        get
        {
            if (IsGrowing)
                return PotentialAreaGrowth / SpecificLeafAreaMax;
            else
                return 0.0;
        }
    }
    public double NDemand
    {
        get
        {
            double NDeficit = Math.Max(0.0, MaximumNConc * (Live.Wt + PotentialDMAllocation) - Live.N);
            if (IsNotSenescing)
                return Math.Max(0.0, NDeficit);
            else
                return 0.0;
        }
    }
    public double DMRetranslocationSupply
    {
        get
        {
            return LeafStartNonStructuralWt;
        }
    }
    public double DMRetranslocation
    {
        set
        {
            if (value > LeafStartNonStructuralWt)
                throw new Exception("A leaf cohort cannot supply that amount for N retranslocation");
            Live.NonStructuralWt = Live.NonStructuralWt - value;
            DMretranslocated = value;
        }
    }
    public double DMPotentialAllocation
    {
        set
        {
            PotentialDMAllocation = value;
        }
    }
    public double DMAllocation
    {
        set
        {
            if (value < 0)
                throw new Exception("Leaf cohort allocation -ve DM value");
            if (value == 0)
            { } //do nothing
            else
            {
                DeltaWt = value;
                DeltaStructuralWt = value * StructuralFraction;
                DeltaNonStructuralWt = value * (1 - StructuralFraction);
                Live.StructuralWt += DeltaStructuralWt;
                Live.NonStructuralWt += DeltaNonStructuralWt;
            }
            //LiveArea += PotentialAreaGrowth;
            LiveArea += value * SpecificLeafAreaMax;
            LiveArea = Math.Min(LiveArea, Live.Wt * SpecificLeafAreaMax);
        }
    }
    public double NAllocation
    {
        set
        {
            if (value < -0.000000001)
                throw new Exception("Leaf cohort given negative N allocation");
            if (value == 0.00)
            { } //do nothing
            else
            {
                if (IsGrowing)
                {
                    double StructN = DeltaStructuralWt * MinimumNConc;
                    Live.StructuralN += Math.Min(value, StructN);
                    Live.NonStructuralN += Math.Max(0.0, value - StructN);
                }
                else
                    Live.NonStructuralN += value;
            }
        }
    }
    public double NReallocationSupply()
    {
        return SenescedFrac * LeafStartNonStructuralN * NReallocationFactor;
    }
    public double NReallocation
    {
        set
        {
            if (value - LeafStartNonStructuralN > 0.00000000001)
                throw new Exception("A leaf cohort cannot supply that amount for N reallocation");
            if (value < -0.0000000001)
                throw new Exception("Leaf cohort given negative N reallocation");
            Live.NonStructuralN -= value;
            Nreallocated = value;
        }
    }
    public double NRetranslocationSupply()
    {
        {
            double Nretrans = (LeafStartNonStructuralN - LeafStartNReallocationSupply - LeafStartNonStructuralWt * MinimumNConc ) * NRetranslocationRate;
            return Math.Max(0.0,Nretrans);
        }
    }
    public double NRetranslocation
    {
        set
        {
            if (value - LeafStartNRetranslocationSupply > 0.00000000001)
                throw new Exception("A leaf cohort cannot supply that amount for N retranslocation");
            if (value < -0.0000000001)
                throw new Exception("Leaf cohort given negative N retranslocation");
            Live.NonStructuralN -= value;
        }
    }
 #endregion

 #region Leaf Cohort Functions
    // Functions used to produce leaf cohort behaviour
    public SIRIUSLeafCohort(double popn, double age, double rank, Function ma, Function gd, Function ld, Function sd, Function sla, double InitialArea, Function CNC, Function MNC, Function INC, Function SF, Function NRF, Function NRR)    
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
        InitialNConc = INC.Value; //This is redundant now, will remove HEB
        StructuralFraction = SF.Value;
        LiveArea = InitialArea * Population;
        Live.StructuralWt = (LiveArea / SpecificLeafAreaMax) * StructuralFraction;
        Live.NonStructuralWt = (LiveArea / SpecificLeafAreaMax) * (1-StructuralFraction);
        Live.StructuralN = Live.StructuralWt * MinimumNConc;
        Live.NonStructuralN = Live.NonStructuralWt * MaximumNConc;
        NReallocationFactor = NRF.Value;
        NRetranslocationRate = NRR.Value;
    }
    public void DoStartSet(double TT)
    {
        SenescedFrac = FractionSenescing(TT);
        LeafStartStructuralN = Live.StructuralN;
        LeafStartNonStructuralN = Live.NonStructuralN;
        LeafStartStructuralWt = Live.StructuralWt;
        LeafStartNonStructuralWt = Live.NonStructuralWt;
        LeafStartDMRetranslocationSupply = DMRetranslocationSupply;
        LeafStartNReallocationSupply = NReallocationSupply();
        LeafStartNRetranslocationSupply = NRetranslocationSupply();
        PotentialAreaGrowth = PotentialAreaGrowthFunction(TT);
        PotentialArea = PotentialArea + PotentialAreaGrowth - (PotentialArea * FractionSenescing(TT));
        PotentialSize = PotentialArea / Population;
        //zero set detlas
        DeltaWt = 0;
        DeltaStructuralWt = 0;
        DeltaNonStructuralWt = 0;
    }
    public void DoPotentialGrowth(double TT)
    {
        // All these bits have been moved into dostartset which is invoked before arbitration
    }
    public void DoActualGrowth(double TT)
    {
        double AreaSenescing = LiveArea * SenescedFrac;
        double StructuralWtSenescing = SenescedFrac * Live.StructuralWt;
        double StructuralNSenescing = SenescedFrac * Live.StructuralN;
        double NonStructuralWtSenescing = SenescedFrac * Live.NonStructuralWt;
        double NonStructuralNSenescing = SenescedFrac * LeafStartNonStructuralN;
        //double AreaSenescing = (StructuralWtSenescing + NonStructuralWtSenescing) * SpecificLeafAreaMax;

        DeadArea = DeadArea + AreaSenescing;
        LiveArea = LiveArea - AreaSenescing;

        Live.StructuralWt -= StructuralWtSenescing;
        Dead.StructuralWt += StructuralWtSenescing;

        Live.StructuralN -= StructuralNSenescing;
        Dead.StructuralN += StructuralNSenescing;

        Live.NonStructuralN -= (NonStructuralNSenescing - Nreallocated); //Seness NonStructuralN that is not reallocated
        Dead.NonStructuralN += (NonStructuralNSenescing - Nreallocated); //dont pass reallocated N into dead pool

        Live.NonStructuralWt -= NonStructuralWtSenescing;
        Dead.NonStructuralWt += NonStructuralWtSenescing;

        Age = Age + TT;
    }
    public void DoKill(double fraction)
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
    public void DoFrost(double fraction)
    {
        DoKill(fraction);
    }
    public double FractionSenescing(double TT)
    {
        double fracSen = 0;
        double TTInSenPhase = Math.Max(0.0, Age + TT - LagDuration - GrowthDuration);
        if (TTInSenPhase > 0)
        {

            if (Rank == 10)
            { }

            double LeafDuration = GrowthDuration + LagDuration + SenescenceDuration;
            double RemainingTT = Math.Max(0, LeafDuration - Age);

            if (RemainingTT == 0)
                fracSen = 1;
            else
                fracSen = Math.Min(1, Math.Min(TT, TTInSenPhase) / RemainingTT);
            if ((fracSen > 1) || (fracSen < 0))
            {
                throw new Exception("Bad Fraction Senescing");
            }
        }
        else
        {
            fracSen = 0;
        }
        return fracSen;
    }
    public double PotentialAreaGrowthFunction(double TT) // Potential delta LAI
    {
        double growth = Population * (SizeFunction(Age + TT) - SizeFunction(Age));
        return growth;
    }
    private double SizeFunction(double TT)
    {
        double alpha = -Math.Log((1 / 0.99 - 1) / (MaxArea / (MaxArea * 0.01) - 1)) / GrowthDuration;
        double leafsize = MaxArea / (1 + (MaxArea / (MaxArea * 0.01) - 1) * Math.Exp(-alpha * TT));
        return leafsize;
    }
 #endregion
}
   
