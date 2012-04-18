using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;
using ModelFramework;

[Description("Returns the value of todays photoperiod calculated using the specified latitude and twilight sun angle threshold.  If variable called ClimateControl.PhotoPeriod can be found this will be used instead")]
class PhotoperiodFunction : Function
{
    [Param]
    private double Twilight = 0;
    [Input]
    private double Latitude = 0;
    [Input]
    public DateTime Today;

    [Link]
    private Paddock MyPaddock = null;

    [Output]
    public override double Value
    {
        get
        {
            double val;
            //If simulatation environment has a variable called ClimateControl.PhotoPeriod will use that other wise calculate from day and location
            if (MyPaddock.Get("ClimateControl.PhotoPeriod", out val))  //FIXME.  If climatecontrol does not contain a variable called photoperiod it still returns a value of zero.
            {
                double CCPP = Convert.ToDouble(val);
                if (CCPP > 0.0)
                    return Convert.ToDouble(val);
                else
                    return MathUtility.DayLength(Today.DayOfYear, Twilight, Latitude);
            }
            else
                return MathUtility.DayLength(Today.DayOfYear, Twilight, Latitude);
        }
    }

}
