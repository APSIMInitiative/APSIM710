using System;
using System.Collections.Generic;
using System.Text;

public class SoilTemperatureFunction : Function
{
    #region Class Data Members
    [Link]
    private XYPairs XYPairs = null;   // Temperature effect on Growth Interpolation Set

    [Input]
    [Units("oC")]
    double maxt_soil_surface = 0;

    [Input]
    [Units("oC")]
    double mint_soil_surface = 0;
    #endregion

    [Output]
    [Units("deg.day")]
    public override double Value
    {
        get
        {
            return AirTemperatureFunction.Linint3hrlyTemp(maxt_soil_surface, mint_soil_surface, XYPairs);
        }
    }
}
   
