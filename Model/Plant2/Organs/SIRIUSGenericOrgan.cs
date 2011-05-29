using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

public class SIRIUSGenericOrgan : GenericOrgan, AboveGround
{

    #region Class data members
    private double SenescedFrac = 0;
    private double StartNRetranslocationSupply = 0;
    private double StartNReallocationSupply = 0;
    private double StartNonStructuralN = 0;
    private double StartNonStructuralWt = 0;
    private double PotentialDMAllocation = 0;
    #endregion

    public override void DoStartSet()
    {
        StartNonStructuralN = Live.NonStructuralN;
        StartNonStructuralWt = Live.NonStructuralWt;
        StartNReallocationSupply = NReallocationSupply;
        StartNRetranslocationSupply = NRetranslocationSupply;
    }

 #region Arbitrator methods
    public override double DMPotentialAllocation
    {
        set
        {
            if (DMDemand == 0)
                if (value < 0.000000000001) { }//All OK
                else
                    throw new Exception("Invalid allocation of potential DM in " + Name);
            PotentialDMAllocation = value;
        }
    }
    public override double DMRetranslocationSupply
    {
        get
        {
            return StartNonStructuralWt;
        }
    }
    public override double DMRetranslocation
    {
        set
        {
            if (value > StartNonStructuralWt)
                throw new Exception("Retranslocation exceeds nonstructural biomass in organ: " + Name);
            Live.NonStructuralWt -= value;
        }
    }
    public override double NDemand
    {
        get
        {
            //Calculate N demand based on how much N is needed to grow nodule wt
            Function MaximumNConc = Children["MaximumNConc"] as Function;
            Function NitrogenDemandPhase = Children["NitrogenDemandPhase"] as Function;
            double NDeficit = Math.Max(0.0, MaximumNConc.Value * (Live.Wt + PotentialDMAllocation) - Live.N);
            //Only allow organ to express demand if organ is growing
            return NDeficit * NitrogenDemandPhase.Value;
        }
    }
    public override double NAllocation
    {
        set
        {
            if (value > 0)
            {
                Function MinimumNConc = Children["MinimumNConc"] as Function;
                double StructuralNRequirement = Math.Max(0.0, Live.StructuralWt * MinimumNConc.Value - Live.StructuralN);
                double StructuralAllocation = Math.Min(StructuralNRequirement, value);
                Live.StructuralN += StructuralAllocation;
                Live.NonStructuralN += Math.Max(0.0, value - StructuralAllocation);
            }
        }
    }
    public override double NReallocationSupply
    {
        get
        {
            Function NReallocationFactor = Children["NReallocationFactor"] as Function;
            return SenescedFrac * StartNonStructuralN * NReallocationFactor.Value; //FIXME organ won't reallocate at the moment because I havent written code to asign a value to SenFrac
        }
    }
    public override double NReallocation
    {
        set
        {
            if (MathUtility.IsGreaterThan(value, StartNonStructuralN))
                throw new Exception("N Reallocation exceeds nonstructural nitrogen in organ: " + Name);
            if (value < -0.000000001)
                throw new Exception("-ve N Reallocation requested from " + Name);
            Live.NonStructuralN -= value;
        }
    }
    public override double NRetranslocationSupply
    {
        get
        {
            Function MinimumNConc = Children["MinimumNConc"] as Function;
            Function NRetranslocationRate = Children["NRetranslocationRate"] as Function;
            double LabileN = Math.Max(0, StartNonStructuralN - StartNonStructuralWt * MinimumNConc.Value);
            double Nretrans = (LabileN - StartNReallocationSupply) * NRetranslocationRate.Value;
            return Nretrans;
        }
    }
    public override double NRetranslocation
    {
        set
        {
            if (MathUtility.IsGreaterThan(value, StartNonStructuralN - StartNRetranslocationSupply))
                throw new Exception("N Retranslocation exceeds nonstructural nitrogen in organ: " + Name);
            if (value < -0.000000001)
                throw new Exception("-ve N Retranslocation requested from " + Name);
            Live.NonStructuralN -= value;
        }
    }
    public override double MaxNconc
    {
        get
        {
            Function MaximumNConc = Children["MaximumNConc"] as Function;
            return MaximumNConc.Value;
        }
    }
    public override double MinNconc
    {
        get
        {
            Function MinimumNConc = Children["MinimumNConc"] as Function;
            return MinimumNConc.Value;
        }
    }
    #endregion
}