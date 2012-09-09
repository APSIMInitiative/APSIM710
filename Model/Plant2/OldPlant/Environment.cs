using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CSGeneral;

public class Environment
{


    [Input]
    public double MaxT;

    [Input]
    public double MinT;

    [Input]
    public DateTime Today;

    [Input(IsOptional=true)]
    public double CO2 = 350;

    public double MeanT { get { return (MaxT + MinT) / 2.0; } }

    public double VPD
    {
        get
        {
            const double SVPfrac = 0.75;

            return SVPfrac * (CSGeneral.MetUtility.svp(MaxT) - CSGeneral.MetUtility.svp(MinT)) / 10;
        }
    }

}

