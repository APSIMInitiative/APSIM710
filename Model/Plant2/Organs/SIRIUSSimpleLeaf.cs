using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

public class SIRIUSSimpleLeaf : SIRIUSLeaf
{
 
    //Get Methods to provide Leaf Status
    [Output]
    [Units("g/m^2")]
    public override double DMDemand
    {
        get
        {

            return 1.0;
        }
    }

    //public override double DMSinkCapacity
    //{
    //    get
    //    {

    //        return DMDemand;
    //    }
    //}
}
   
