using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

public class Nodule : SIRIUSGenericOrgan, BelowGround
{
    [Link]
    Plant Plant = null;

  #region Class Data Members
    public double RespiredWt = 0;
    public double PropFixationDemand = 0;
    public double _NFixed = 0;
  #endregion

 public override void DoStartSet()
    { }
    
 #region Fixation methods
    public override double NFixation
    {
        set
        {
            _NFixed = value;
        }
    }
    [Output]
    public double RespiredWtFixation
    {
        get
        {
            return RespiredWt;
        }
    }
    [Output]
    public override double NFixationSupply
    {
        get
        {
            Function SpecificNitrogenaseActivity = Children["SpecificNitrogenaseActivity"] as Function;
            Function FT = Children["FT"] as Function;
            Function FW = Children["FW"] as Function;
            Function FWlog = Children["FWlog"] as Function;
            return Live.StructuralWt * SpecificNitrogenaseActivity.Value * Math.Min(FT.Value, Math.Min(FW.Value, FWlog.Value));
        }
    }
    public override double DMRespired
    {
        set
        //This is the DM that is consumed to fix N.  this is calculated by the arbitrator and passed to the nodule to report
        {
            RespiredWt = value;
        }
    }
    [Output]
    public double NFixed
    {
        get
        {
            return _NFixed;
        }
    }
 #endregion

 #region Arbitrator methods
    //public override double DMRetranslocationSupply { get { return 0; } }
    /*public override double DMRetranslocation
    {
        set
        {
            if (value > 0)
                throw new Exception(Name + " cannot supply retranslocation");
        }
    }*/
   /* public override double DMAllocation
    {
        set
        {
           // allocating structural DM to nodule organ
            Live.StructuralWt += value; 
        }
    }*/
   // public override double NRetranslocationSupply { get { return 0; } }
   /*public override double NRetranslocation
    {
        set
        {
            if (value > 0)
                throw new Exception(Name + " cannot supply N retranslocation");
        }
    }*/
    //public override double NReallocationSupply { get { return 0; } }
    /*public override double NReallocation
    {
        set
        {
            if (value > 0)
                throw new Exception(Name + " cannot supply N reallocation");
        }
    }*/
 #endregion




}

