using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using ModelFramework;
using CSGeneral;

[Description("Adds the value of all childern functions to the previous days accumulation between start and end phases")]
public class TranspirationFunction : Function
{
    //Class members
    public double AccumulatedValue = 0;
    
    [Link]
    Arbitrator Arbitrator = null;

    [Link]
    Leaf Leaf = null;

    [Param]
    private double TranspirationEfficiencyCoefficient = 0.005;

    [Param]
    private double SVPfrac = 0.75;

    public double TranspirationDemand = 0;

    [EventHandler]
    public void OnNewMet(NewMetType NewMet)
    {
        double PotentialBiomass = Leaf.Photosynthesis.PotentialGrowth(Leaf.RadIntTot);
        TranspirationDemand = PotentialBiomass * (TranspirationEfficiencyCoefficient / DayTimeAverageVPD);
    }
    
    public double DayTimeAverageVPD
    {
        get
        {
           
            double VPDmint = MetUtility.svp((float)MetData.MinT) - MetData.vp;
            VPDmint = Math.Max(VPDmint, 0.0);

            double VPDmaxt = MetUtility.svp((float)MetData.MaxT) - MetData.vp;
            VPDmaxt = Math.Max(VPDmaxt, 0.0);

            return SVPfrac * VPDmaxt + (1 - SVPfrac) * VPDmint;
        }
    }
    [Output]
    public override double Value
    {
        get
        {
            return TranspirationDemand;
        }
    }


}

