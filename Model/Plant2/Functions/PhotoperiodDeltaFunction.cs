using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

[Description("Returns the difference between today's and yesterday's photoperiods in hours.")]
class PhotoperiodDeltaFunction : Function
{
    [Param]
    private double Twilight = 0;
    [Input]
    private double Latitude = 0;
    [Input]
    public double Day = 0;

    [Output]
    public override double Value
    {
        get
        {
            double PhotoperiodToday = MathUtility.DayLength(Day, Twilight, Latitude);
            double PhotoperiodYesterday = MathUtility.DayLength(Day - 1, Twilight, Latitude);
            double PhotoperiodDelta = PhotoperiodToday - PhotoperiodYesterday;
            return PhotoperiodDelta;
        }
    }

}
