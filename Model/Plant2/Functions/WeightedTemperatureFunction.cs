using System;
using System.Collections.Generic;
using System.Text;

public class WeightedTemperatureFunction : Function
{
    #region Class Data Members
    [Link]
    private XYPairs XYPairs = null;   // Temperature effect on Growth Interpolation Set

    [Param]
    private double MaximumTemperatureWeighting = 0.0;

    [Input]
    double MaxT = 0;

    [Input]
    double MinT = 0;
    #endregion

    [Output]
    [Units("0-1")]
    public override double Value
    {
        get
        {
            double Tav = MaximumTemperatureWeighting * MaxT + (1 - MaximumTemperatureWeighting) * MinT;
            return XYPairs.ValueIndexed(Tav);
        }
    }

}
   
