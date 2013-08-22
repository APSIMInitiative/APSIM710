using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

[Description("Returns the difference between today's and yesterday's photoperiods in hours.")]
class PhotoperiodDeltaFunction : Function
{
    [Param]
    private double Twilight = 0;
    [Output]
    public override double Value
    {
        get
        {
            double PhotoperiodToday = MathUtility.DayLength(Clock.day, Twilight, MetData.Latitude);
            double PhotoperiodYesterday = MathUtility.DayLength(Clock.day - 1, Twilight, MetData.Latitude);
            double PhotoperiodDelta = PhotoperiodToday - PhotoperiodYesterday;
            return PhotoperiodDelta;
        }
    }

}
