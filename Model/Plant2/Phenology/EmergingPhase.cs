using System;
using System.Collections.Generic;
using System.Text;

class EmergingPhase : GenericPhase
{
    [Param]
    private double ShootLag = 0;
    [Param]
    private double ShootRate = 0;

    private double SowDepth;

    [EventHandler]
    public void OnSow(SowPlant2Type Sow)
    {
        SowDepth = Sow.Depth;
    }

    /// <summary>
    /// Return the target to caller. Can be overridden by derived classes.
    /// </summary>
    protected override double CalcTarget()
    {
        return ShootLag + SowDepth * ShootRate;
    }


}
