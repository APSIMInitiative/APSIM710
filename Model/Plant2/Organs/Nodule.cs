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
}

