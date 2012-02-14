using System;
using System.Collections.Generic;
using System.Text;


public class LeafDeathPhase : Phase
{
    [Link]
    private Leaf Leaf = null;

    private double DeadNodeNoAtStart;
    bool First = true;

    /// <summary>
    /// Do our timestep development
    /// </summary>
    public override double DoTimeStep(double PropOfDayToUse)
    {
        base.DoTimeStep(PropOfDayToUse);
        
        if (First)
        {
            DeadNodeNoAtStart = Leaf.DeadCohortNo;
            First = false;
        }

        if (Leaf.DeadCohortNo >= Leaf.FinalLeafNo)
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
            double F = (Leaf.DeadCohortNo - DeadNodeNoAtStart) / (Leaf.FinalLeafNo - DeadNodeNoAtStart);
            if (F < 0) F = 0;
            if (F > 1) F = 1;
            return F;
        }
    }

}


      
      
