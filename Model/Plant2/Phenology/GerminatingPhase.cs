using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;


class GerminatingPhase : Phase
{
    [Input]
    private double ESW = 0;

    /// <summary>
    /// Do our timestep development
    /// </summary>
    public override double DoTimeStep(double PropOfDayToUse)
    {

        bool CanGerminate = !Phenology.OnDayOf("Sowing") && ESW > 0;

        if (CanGerminate)
            return 0.00001;
        else
            return 0;
    }

    /// <summary>
    /// Return a fraction of phase complete.
    /// </summary>
    public override double FractionComplete
    {
        get
        {
            return 1;
        }
    }
}
