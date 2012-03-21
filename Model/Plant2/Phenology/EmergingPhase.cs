using System;
using System.Collections.Generic;
using System.Text;

class EmergingPhase : GenericPhase
{
    [Link]
    Plant Plant;

    [Param]
    private double ShootLag = 0;
    [Param]
    private double ShootRate = 0;


    /// <summary>
    /// Return the target to caller. Can be overridden by derived classes.
    /// </summary>
    protected override double CalcTarget()
    {
        return ShootLag + Plant.SowingData.Depth * ShootRate;
    }

}
