using System;
using System.Collections.Generic;
using System.Text;
[Description("Returns the temperature of the surface soil layer with the weighting: " + 
             "0.25*DayBeforeYesterday + 0.5*Yesterday + 0.25*Today")]
public class SoilTemperatureWeightedFunction : Function
{
    #region Class Data Members

    private double DayBeforeYesterday = 0;
    private double Yesterday = 0;
    private double Today = 0;
    [Link]
    private XYPairs XYPairs = null;   // Temperature effect on Growth Interpolation Set

    [Input(IsOptional=true)]
    [Units("oC")]
    double maxt_soil_surface = 0;

    #endregion

    /// <summary>
    /// EventHandler for OnPrepare.
    /// </summary>
    [EventHandler]
    public void OnPrepare()
    {
        DayBeforeYesterday = Yesterday;
        Yesterday = Today;
        Today = maxt_soil_surface;
    }


    [Output]
    [Units("deg.day")]
    public override double Value
    {
        get
        {
            double WeightedTemperature = 0.25 * DayBeforeYesterday + 0.5 * Yesterday + 0.25 * Today;
            return XYPairs.ValueIndexed(WeightedTemperature);
        }
    }
}
   
