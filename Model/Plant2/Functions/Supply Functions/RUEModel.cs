using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

public class RUEModel
{
    [Link]
    Plant Plant = null;

    [Link]
    Function RUE = null;

    [Link]
    Function Fco2 = null;

    [Link]
    Function Fn = null;

    [Link]
    Function Ft = null;

    [Link]
    Function Fw = null;

    [Link]
    Function Fvpd = null;


    #region Class Data Members
    [Input]
    public NewMetType MetData;

    [Event]
    public event NewPotentialGrowthDelegate NewPotentialGrowth;
    #endregion

    #region Associated variables
    [Output]
    public double VPD
    {
        get
        {
            const double SVPfrac = 0.66;

            double VPDmint = MetUtility.svp((float)MetData.mint) - MetData.vp;
            VPDmint = Math.Max(VPDmint, 0.0);

            double VPDmaxt = MetUtility.svp((float)MetData.maxt) - MetData.vp;
            VPDmaxt = Math.Max(VPDmaxt, 0.0);

            return SVPfrac * VPDmaxt + (1 - SVPfrac) * VPDmint;
        }
    }

    #endregion

    /// <summary>
    /// Total plant "actual" radiation use efficiency (for the day) corrected by reducing factors (g biomass/MJ global solar radiation) CHCK-EIT 
    /// </summary>
    [Output("RueAct")]
    [Units("gDM/MJ")]
    private double RueAct
    {
        get
        {
            double RueReductionFactor = Math.Min(Ft.Value, Math.Min(Fn.Value, Fvpd.Value)) * Fw.Value * Fco2.Value;
            return RUE.Value * RueReductionFactor;
        }
    }
    /// <summary>
    /// Daily growth increment of total plant biomass
    /// </summary>
    /// <param name="RadnInt">intercepted radiation</param>
    /// <returns>g dry matter/m2 soil/day</returns>
    public double Growth(double RadnInt)
    {
        return RadnInt * RueAct;
    }

    private void PublishNewPotentialGrowth()
    {
        // Send out a NewPotentialGrowthEvent.
        if (NewPotentialGrowth != null)
        {
            NewPotentialGrowthType GrowthType = new NewPotentialGrowthType();
            GrowthType.sender = Plant.Name;
            GrowthType.frgr = (float)Math.Min(Ft.Value, Math.Min(Fn.Value, Fvpd.Value));
            NewPotentialGrowth.Invoke(GrowthType);
        }
    }
    [EventHandler]
    public void OnPrepare()
    {
        PublishNewPotentialGrowth();
    }
}
