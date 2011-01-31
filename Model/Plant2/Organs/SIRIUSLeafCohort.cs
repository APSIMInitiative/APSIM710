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
    public double PotentialSize = 0;
    public double PotentialArea = 0; 
    public double LiveArea = 0;
    public double MaxLiveArea = 0;
    public double DeadArea = 0;
    public double NReallocationSupply = 0; 
    public Biomass Live = new Biomass();
    public Biomass Dead = new Biomass();

    public double MaxArea = 0;
    private double GrowthDuration = 0;
    private double LagDuration = 0;
    private double SenescenceDuration = 0;
    private double SpecificLeafAreaMax = 0;
    private double MaximumNConc = 0;
    private double MinimumNConc = 0;
    private double StructuralNConc = 0;
    private double InitialNConc = 0;
    private double StructuralFraction = 0;

    #endregion
    public double Nretranslocated = 0;
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
    public        SIRIUSLeafCohort(double popn, double age, double rank, Function ma, Function gd, Function ld, Function sd, Function sla, double InitialArea, Function CNC, Function MNC, Function SNC, Function INC, Function SF)
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
        StructuralNConc = SNC.Value;
        InitialNConc = INC.Value;
        StructuralFraction = SF.Value;
        //if (InitialArea != 0)
        //   if (InitialArea < MaxArea)
        //      Age = GrowthDuration * InitialArea / MaxArea;
        //   else
        //      Age = GrowthDuration;


        LiveArea = InitialArea * Population;
        Live.StructuralWt = LiveArea / SpecificLeafAreaMax;
        Live.StructuralN = Live.StructuralWt * INC.Value;

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
    public bool ShouldBeDead
    {
        get { return (Age > (GrowthDuration + LagDuration + SenescenceDuration)); }
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
    public void DoPotentialGrowth(double TT)
    {
        PotentialAreaGrowth = PotentialAreaGrowthFunction(TT);

        PotentialArea = PotentialArea + PotentialAreaGrowth - (PotentialArea * FractionSenescing(TT));
        PotentialSize = PotentialArea / Population;
        //SenescedFrac = FractionSenescing(TT); 
    }
    public double PotentialAreaGrowthFunction(double TT) // Potential delta LAI
    {
        //return MaxArea*Population * Math.Min(TT, Math.Max(0, GrowthDuration - Age)) / GrowthDuration;

        double growth = Population * (SizeFunction(Age + TT) - SizeFunction(Age));
        return growth;
    }
    private double SizeFunction(double TT)
    {
        double alpha = -Math.Log((1 / 0.99 - 1) / (MaxArea / (MaxArea * 0.01) - 1)) / GrowthDuration;
        double leafsize = MaxArea / (1 + (MaxArea / (MaxArea * 0.01) - 1) * Math.Exp(-alpha * TT));
        return leafsize;

    }
    public double DMDemand(double TT)
    {
        return PotentialAreaGrowth / SpecificLeafAreaMax;
    }
    public double NDemand()
    {
        double NDeficit = Math.Max(0.0, MaximumNConc * Live.Wt - Live.N);
        return NDeficit;
    }
    public double DMAllocation
    {
        set
        {
            if (value < 0)
                throw new Exception("Leaf cohort allocation -ve DM value");
            //Live.StructuralWt += value;
            {
                Live.StructuralWt += value * StructuralFraction;
                Live.NonStructuralWt += value * (1 - StructuralFraction);
            }
            //LiveArea += Math.Min(PotentialAreaGrowth,value * SpecificLeafAreaMax);
            LiveArea += PotentialAreaGrowth;
            LiveArea = Math.Min(LiveArea, Live.Wt * SpecificLeafAreaMax);
        }
    }
    public double NAllocation
    {
        set
        {
            double StructN = Live.StructuralWt * MinimumNConc;
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
    public void DoActualGrowth(double TT)
    {
        if (MaxLiveArea < LiveArea)
            MaxLiveArea = LiveArea;
        
        //Calculate Senescence
        double LiveNonStructuralNConc = 0;
        double MaxNonStructuralWt = 0;
        double NremobSenFrac = 0;
        if (Live.NonStructuralN > 0 & Live.NonStructuralWt > 0)
        {
            LiveNonStructuralNConc = Live.NonStructuralN / Live.NonStructuralWt;
            MaxNonStructuralWt = Live.NonStructuralN / MinimumNConc;
        }
             
        if (LiveNonStructuralNConc >= MinimumNConc)
            NremobSenFrac = 0;
        else if (Live.NonStructuralWt <= MaxNonStructuralWt)
            NremobSenFrac = 0;
        else
            NremobSenFrac = Math.Max(0.0, (Live.NonStructuralWt - MaxNonStructuralWt) / Live.NonStructuralWt); 


        double FracSen = 0;
        FracSen = FractionSenescing(TT);//Math.Max(FractionSenescing(TT), NremobSenFrac);
        double AreaSenescing = 0;
        
        // Update State Variables
        AreaSenescing = LiveArea * FracSen;
        DeadArea = DeadArea + AreaSenescing;
        LiveArea = LiveArea - AreaSenescing;

        double Senescing = FracSen * Live.StructuralWt;
        Live.StructuralWt -= Senescing;
        Dead.StructuralWt += Senescing;

        Senescing = FracSen * Live.NonStructuralWt;
        Live.NonStructuralWt -= Senescing;
        Dead.NonStructuralWt += Senescing;

        Senescing = FracSen * Live.NonStructuralN;
        Live.NonStructuralN -= Senescing;
        Dead.NonStructuralN += Senescing;

        Senescing = FracSen * Live.StructuralN;
        Live.StructuralN -= Senescing;
        Dead.StructuralN += Senescing;

        Age = Age + TT;
        NReallocationSupply = FractionSenescing(TT) * Live.NonStructuralN;
    }
    private double NFac()
    {
        double Nconc = Live.NConc;
        double value = Math.Min(1.0, Math.Max(0.0, (Nconc - MinimumNConc) / (MaximumNConc - MinimumNConc)));
        return value;
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
    public double NRetranslocationSupply
    {
      get
        {
            double Nretrans = Live.NonStructuralN; 
            if (Nretrans < 0)
                return 0;
            else
                return Nretrans;
        }
    }
    public double NRetranslocation
    {
        set
        {
            if (value > Live.NonStructuralN)
                throw new Exception("A leaf cohort cannot supply that amount for N retranslocation");
            Live.NonStructuralN = Live.NonStructuralN - value;
            Nretranslocated = value;
        }
    }
    public double DMRetranslocationSupply
    {
        get
        {
         return Live.NonStructuralWt;
        }
    }
    public double DMRetranslocation
    {
        set
        {
            if (value > Live.NonStructuralWt)
                throw new Exception("A leaf cohort cannot supply that amount for N retranslocation");
            Live.NonStructuralWt = Live.NonStructuralWt - value;
        }
    }
}
   
