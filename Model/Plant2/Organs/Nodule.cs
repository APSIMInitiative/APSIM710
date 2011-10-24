using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

public class Nodule : SIRIUSGenericOrgan, BelowGround
{
  #region Class Data Members
    public double RespiredWt = 0;
    public double PropFixationDemand = 0;
    public double _NFixed = 0;

    [Link]
    Function FixationMetabolicCost = null;

    [Link]
    Function SpecificNitrogenaseActivity = null;

    [Link]
    Function FT = null;

    [Link]
    Function FW = null;

    [Link]
    Function FWlog = null;

  #endregion
        
 #region Fixation methods

    public override double NFixationCost
    {
        get
        {
            return FixationMetabolicCost.Value;
        }
    }
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

