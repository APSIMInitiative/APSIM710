using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

[Description("This must be renamed DMDemandFunction for the source code to recoginise it!!!!  This function calculates DM demand beyond the start stage as the product of current organ wt (g), relative growth rate and the specified organ number.")]
public class RelativeGrowthRateDemandFunction : Function
{
    [Param]
    double InitialWt = 0;

    [Param]
    string InitialStageName = "";

    [Link]
    Function RelativeGrowthRate = null;

    [Link]
    Function OrganNumber = null;

    [Link]
    Phenology Phenology = null;

    [Link]
    Biomass Live = null;

    double StartWt = 0;

    public override double Value
    {
        get
        {
            if (Phenology.OnDayOf(InitialStageName) && StartWt == 0)
                StartWt = InitialWt;                                   //This is to initiate mass so relative growth rate can kick in
            double CurrentOrganWt = Math.Max(StartWt, Live.Wt/OrganNumber.Value);  
            double OrganDemand = CurrentOrganWt * RelativeGrowthRate.Value;
            return OrganDemand * OrganNumber.Value;
        }
    }

}



