using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

[Description("This must be renamed DMDemandFunction for the source code to recoginise it!!!!.  This function returns the specified proportion of total DM supply.  The organ may not get this proportion if the sum of demands from other organs exceeds DM supply")]
public class BerryDryMatterDemandFunction : Function
{
   // [Link]
    //Function PartitionFraction = null;
    [Param]
    private double Wo1 = 1.0;
    [Param]
    private double Wf1 = 1.0;
   [Param]
    private double Mu1 = 1.0;
   [Param]
   private double Wo2 = 1.0;
   [Param]
   private double Wf2 = 1.0;
   [Param]
   private double Mu2 = 1.0;
    [Link]
    Phenology Phenology = null;
    [Link]
    Function TT = null;
    private double yesterdaysDM = 0;
    private double AccTT = 0;
    [Output]
    public override double Value
    {
        get

        {
            if (Phenology.CurrentPhaseName == "BerryDevelopment")
            {
                AccTT += TT.Value;
                double TodaysDM = 0;
                TodaysDM = Wf1 / (1 + (Wf1 - Wo1) / Wo1 * Math.Exp(-Mu1 * AccTT));
                double returnValue = TodaysDM - yesterdaysDM;
                yesterdaysDM = TodaysDM;
                return returnValue;
             }
            if (Phenology.CurrentPhaseName == "Senescent")
            {
                AccTT += TT.Value;
                double TodaysDM = 0;
                TodaysDM = Wf1 / (1 + (Wf1 - Wo1) / Wo1 * Math.Exp(-Mu1 * AccTT)) + Wf2 / (1 + (Wf2 - Wo2) / Wo2 * Math.Exp(-Mu2 * AccTT));
                double returnValue = TodaysDM - yesterdaysDM;
                yesterdaysDM = TodaysDM;
                return returnValue;
            }
            else
                return 0;
        }
    }
    [EventHandler]
    public void OnPrune()
    {
        AccTT = 0;
        yesterdaysDM = 0;
    }
}



