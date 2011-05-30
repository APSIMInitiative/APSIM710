using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

public class SIRIUSGenericOrgan : GenericOrgan, AboveGround
{

 #region Optional IDE input field
    [Link(IsOptional.Yes)]
    Function SenescenceRate = null;
    [Link(IsOptional.Yes)]
    Function NReallocationFactor = null;
    [Link(IsOptional.Yes)]
    Function NRetranslocationFactor = null;
    [Link(IsOptional.Yes)]
    Function NitrogenDemandSwitch = null;
 #endregion

 #region Class data members
    private double _SenescenceRate = 0;
    private double StartNRetranslocationSupply = 0;
    private double StartNReallocationSupply = 0;
    private double StartNonStructuralN = 0;
    private double StartNonStructuralWt = 0;
    private double PotentialDMAllocation = 0;
 #endregion

 #region Organ functions
    public override void DoPotentialGrowth()
    {
        _SenescenceRate = 0;
        if (SenescenceRate != null) //Default of zero means no senescence
            _SenescenceRate = SenescenceRate.Value;
        StartNonStructuralN = Live.NonStructuralN;
        StartNonStructuralWt = Live.NonStructuralWt;
        StartNReallocationSupply = NReallocationSupply;
        StartNRetranslocationSupply = NRetranslocationSupply;
    }
    //public override void DoStartSet()
    //{
      /*  _SenescenceRate = 0;
        if (SenescenceRate != null) //Default of zero means no senescence
            _SenescenceRate = SenescenceRate.Value; 
        StartNonStructuralN = Live.NonStructuralN;
        StartNonStructuralWt = Live.NonStructuralWt;
        StartNReallocationSupply = NReallocationSupply;
        StartNRetranslocationSupply = NRetranslocationSupply;*/
    //}
    public override void DoActualGrowth()
    {
        base.DoActualGrowth();

        Live.StructuralWt *= (1.0 - _SenescenceRate);
        Live.NonStructuralWt *= (1.0 - _SenescenceRate);
        Live.StructuralN *= (1.0 - _SenescenceRate);
        Live.NonStructuralN *= (1.0 - _SenescenceRate);
    }
 #endregion

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
            double _NitrogenDemandSwitch = 1;
            if (NitrogenDemandSwitch != null) //Default of 1 means demand is always truned on!!!!
                _NitrogenDemandSwitch = NitrogenDemandSwitch.Value; 
            Function MaximumNConc = Children["MaximumNConc"] as Function;
            double NDeficit = Math.Max(0.0, MaximumNConc.Value * (Live.Wt + PotentialDMAllocation) - Live.N);
            return NDeficit * _NitrogenDemandSwitch;
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
            double _NReallocationFactor = 0;
            if (NReallocationFactor != null) //Default of zero means N reallocation is truned off
                _NReallocationFactor = NReallocationFactor.Value;
            return _SenescenceRate * StartNonStructuralN * _NReallocationFactor; 
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
            double _NRetranslocationFactor = 0;
            if (NRetranslocationFactor != null) //Default of zero means retranslocation is turned off
                _NRetranslocationFactor = NRetranslocationFactor.Value;
            Function MinimumNConc = Children["MinimumNConc"] as Function;
            double LabileN = Math.Max(0, StartNonStructuralN - StartNonStructuralWt * MinimumNConc.Value);
            double Nretrans = (LabileN - StartNReallocationSupply) * _NRetranslocationFactor;
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