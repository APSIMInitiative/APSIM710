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
    private double PotentialArea = 0;
    private double PotentialDMAllocation = 0;
    private double Nreallocated = 0;
    private double StructuralDMDemand = 0;
    private double NonStructuralDMDemand = 0;    
    
    private double NReallocationFactor = 0;
    private double NRetranslocationRate = 0;
    private double Fw = 0;
    new double SpecificLeafAreaMax = 0;
    private double SpecificLeafAreaMin = 0;
    public double SLA = 0.0;
    private double NonStructuralNConc = 0;
    
    private double DeltaPotentialArea = 0;
    private double DeltaWaterConstrainedArea = 0;
    private double DeltaStructuralWt = 0;
    private double DeltaNonStructuralWt = 0;
    
    public double LeafStartNRetranslocationSupply = 0;
    public double LeafStartNReallocationSupply = 0;
    public double LeafStartDMRetranslocationSupply = 0;
    public double LeafStartNonStructuralN = 0;
    public double LeafStartStructuralWt = 0;
    public double LeafStartNonStructuralWt = 0;
    public double LeafStartArea = 0;
    
    private double StructuralFraction = 0; //this may be redundant
    private double DMretranslocated = 0;  //this may be redundant
    private double LeafStartStructuralN = 0;             //this is redundant 
 #endregion
    
 #region Arbitrator method calls
    public override double DMDemand
    {
        get
        {
            if (IsGrowing)
            {
                StructuralDMDemand = DeltaPotentialArea / SpecificLeafAreaMax;  //Work out how much DM would be needed to grow to potantial size
                double PotentialNonStructuralDMDemand = (StructuralDMDemand * (1/StructuralFraction)) - StructuralDMDemand; //Non-structural DM is a fixed proporiton of DM demand assuming leaves are growing at potential rate
                //StructuralDMDemand = DeltaWaterConstrainedArea / SpecificLeafAreaMax;
                double MaximumDM = (LeafStartArea + DeltaWaterConstrainedArea) / SpecificLeafAreaMin;  //Water stressed leaves can't get any thicker than this
                double ThicknessConstrainedDMdelta = MaximumDM - LeafStartNonStructuralWt - LeafStartStructuralWt - StructuralDMDemand;  //So todays maximum delta will be this
                NonStructuralDMDemand = Math.Min(PotentialNonStructuralDMDemand, ThicknessConstrainedDMdelta);  //So the actual demand will be the lesser of what is needed to grown non-structural DM at (1-StructuralFraction) is the leaves are not water stressed.  
                //If leaf area growth is constrained by water stress leaves get thicker until they reach their maximum thickness and then leaf biomass demand will be constrained so leaves don't exceed maximum thickness.  
                return StructuralDMDemand + NonStructuralDMDemand;
            }
            else
                return 0.0;
        }
    }
    public override double DMSinkCapacity
   {
       get
       {
              double MaximumDM = (LeafStartArea + DeltaWaterConstrainedArea) / SpecificLeafAreaMin;
              return Math.Max(0.0, MaximumDM - DMDemand - LeafStartNonStructuralWt - LeafStartStructuralWt);
       }
   }
    public override double DMExcessAllocation
    {
        set
        {
            Live.NonStructuralWt += value;
        }
    }
    public override double NDemand
    {
        get
        {
            //double StructuralNDemand = StructuralDMDemand * StructuralNConc * (1 / (1 - StructuralFraction));  //assumes structural N has the minimum N content
            //double MaxNonStructuralN = NonStructuralNConc * (Live.NonStructuralWt + NonStructuralDMDemand);  //Works out how much NonStructural N would be in the leaf if it got to MaxN
            //double NonStructuralNDemand = Math.Max(0.0, MaxNonStructuralN - LeafStartNonStructuralN); //Non-structural N demand is that required to bring the leaf up to its Max N conc
            double CoverAbove = ParentLeafOrgan.CoverAboveCohort(Rank); // Fixme.  This is throwing an error at the moment
            double NDeficit = Math.Max(0.0, MaximumNConc * (Live.Wt + PotentialDMAllocation) - Live.N);
            if (IsNotSenescing)  // && CoverAbove < 0.9) // Assuming a leaf will have no demand if it is senescing and will have no demand if it is is shaded conditions
                return NDeficit;
            //return StructuralNDemand + NonStructuralNDemand;
            else
                return 0.0;
        }
    }
    public double DMRetranslocationSupply
    {
        get
        {
            return 0;// LeafStartNonStructuralWt;
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
            if (value < -0.0000000001)
                throw new Exception("Leaf cohort allocation -ve DM value");
            if (value == 0)
            { } //do nothing
            else
            {
                DeltaWt = value;
                DeltaStructuralWt = Math.Min(value, NonStructuralDMDemand);  //Biomass partitioned to Structural first in proportion to leafarea growth
                double NonStructuralAllocation = Math.Max(0.0,value - NonStructuralDMDemand);  //Any surpless is partitioned to NonStructual not in proportion to leaf area growth.  
                                                                                              //Net effect is if the crop is water stressed leaves will get thicker and if it is growth (light of Nitrogen) stressed leaves will get thicker
                DeltaNonStructuralWt = NonStructuralAllocation;
                Live.StructuralWt += DeltaStructuralWt;
                Live.MetabolicWt += DeltaNonStructuralWt;
            }
            
            //Grow leaves after DM allocated
            double DeltaActualArea = Math.Min(DeltaWaterConstrainedArea, value * SpecificLeafAreaMax);
            LiveArea += DeltaActualArea;
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
                    double StructN = DeltaStructuralWt * StructuralNConc;
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
            double Nretrans = (LeafStartNonStructuralN - LeafStartNReallocationSupply - LeafStartStructuralN) * NRetranslocationRate;
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
    public SIRIUSLeafCohort(double popn, double age, double rank, Function ma, Function gd, Function ld, Function sd, Function sla, double InitialArea, Function CNC, Function MNC, Function INC, Function SF, Function NRF, Function NRR, Function Stressfact, Function SLAmin, SIRIUSLeaf Parent)
        : base(popn, age, rank, ma, gd, ld, sd, sla, InitialArea, CNC, MNC, null, null)
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
        //MinimumNConc = MNC.Value;
        StructuralNConc = MNC.Value;
        //NonStructuralNConc = (CNC.Value - MNC.Value) * StructuralFraction * (1/(1-StructuralFraction)); //Sets up Nonstructural N conc so that leaf N conc is able to get to Maximum N conc
        InitialNConc = INC.Value; //This is redundant now, will remove HEB
        StructuralFraction = SF.Value;
        LiveArea = InitialArea * Population;
        Live.StructuralWt = LiveArea / SpecificLeafAreaMax; //Get initial Structural wt from initial area
        Live.NonStructuralWt = LiveArea / (SpecificLeafAreaMax - SpecificLeafAreaMin);  //Start leaves off at Max thickness
        Live.StructuralN = Live.StructuralWt * MinimumNConc;
        Live.NonStructuralN = Live.NonStructuralWt * NonStructuralNConc;
        NReallocationFactor = NRF.Value;
        NRetranslocationRate = NRR.Value;
        Fw = Stressfact.Value;
        SpecificLeafAreaMin = SLAmin.Value;
        ParentLeafOrgan = Parent;
    }
    public override void DoPotentialGrowth(double TT)
    {
        SenescedFrac = FractionSenescing(TT);
        LeafStartArea = LiveArea;
        LeafStartStructuralWt = Live.StructuralWt;
        LeafStartNonStructuralWt = Live.NonStructuralWt;
        LeafStartNonStructuralN = Live.NonStructuralN;
        LeafStartDMRetranslocationSupply = DMRetranslocationSupply;
        LeafStartNReallocationSupply = NReallocationSupply();
        LeafStartNRetranslocationSupply = NRetranslocationSupply;
        DeltaPotentialArea = PotentialAreaGrowthFunction(TT); //Calculate delta leaf area in the absence of water stress
        DeltaWaterConstrainedArea = DeltaPotentialArea * Fw; //Reduce potential growth for water stress
        PotentialSize = PotentialArea / Population;
        SLA = 0;
        if (Live.Wt > 0)
            SLA = LiveArea / Live.Wt;
        

        //zero set detlas
        DeltaWt = 0;
        DeltaStructuralWt = 0;
        DeltaNonStructuralWt = 0;
        
        
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
   
