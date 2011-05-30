using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

class SIRIUSLeafCohort : LeafCohort
{
 #region Class Data Members
    public SIRIUSLeaf ParentLeafOrgan = null;
    private double SenescedFrac = 0;
    public double PotentialSize = 0;
    public double PotentialArea = 0; 
    private double StructuralFraction = 0;
    public double Nreallocated = 0;
    public double DMretranslocated = 0;
    private double NReallocationFactor = 0;
    private double NRetranslocationRate = 0;
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
    
 #region Arbitrator method calls
    public override double NDemand
    {
        get
        {
            double CoverAbove = ParentLeafOrgan.CoverAboveCohort(Rank); // Fixme.  This is throwing an error at the moment
            double NDeficit = Math.Max(0.0, MaximumNConc * (Live.Wt + PotentialDMAllocation) - Live.N);
            if (IsNotSenescing)// && CoverAbove < 0.9) // Assuming a leaf will have no demand if it is senescing and will have no demand if it is is shaded conditions
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
    public override double DMAllocation
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
    public override double NAllocation
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
    public override double NRetranslocationSupply
    {
        get
        {
            double Nretrans = (LeafStartNonStructuralN - LeafStartNReallocationSupply - LeafStartNonStructuralWt * MinimumNConc) * NRetranslocationRate;
            return Math.Max(0.0, Nretrans);
        }
    }
    public override double NRetranslocation
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
    public SIRIUSLeafCohort(double popn, double age, double rank, Function ma, Function gd, Function ld, Function sd, Function sla, double InitialArea, Function CNC, Function MNC, Function INC, Function SF, Function NRF, Function NRR, SIRIUSLeaf Parent)    
        :base(popn, age,rank,ma,gd,ld,sd,sla,InitialArea,CNC, MNC, null, null)
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
        ParentLeafOrgan = Parent;
    }
    public override void DoStartSet(double TT)
    {
        SenescedFrac = FractionSenescing(TT);
        LeafStartNonStructuralN = Live.NonStructuralN;
        LeafStartNonStructuralWt = Live.NonStructuralWt;
        LeafStartDMRetranslocationSupply = DMRetranslocationSupply;
        LeafStartNReallocationSupply = NReallocationSupply();
        LeafStartNRetranslocationSupply = NRetranslocationSupply;
        PotentialAreaGrowth = PotentialAreaGrowthFunction(TT);
        PotentialArea = PotentialArea + PotentialAreaGrowth - (PotentialArea * FractionSenescing(TT));
        PotentialSize = PotentialArea / Population;
        //zero set detlas
        DeltaWt = 0;
        DeltaStructuralWt = 0;
        DeltaNonStructuralWt = 0;
    }
    public override void DoPotentialGrowth(double TT)
    {
        // All these bits have been moved into dostartset which is invoked before arbitration
    }
    public override void DoActualGrowth(double TT)
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
 #endregion
}
   
