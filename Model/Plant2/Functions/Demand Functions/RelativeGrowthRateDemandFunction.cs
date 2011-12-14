using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

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



