using System;
using System.Collections.Generic;
using System.Text;

public class LeafAppearancePhase : Phase
{
    [Link]
    Leaf Leaf = null;

    [Link]
    Function ThermalTime = null;

    private double CumulativeTT;
    private double CohortNoAtStart;
    bool First = true;

    [Param]
    private double RemainingLeaves = 0;

    [Output]
    public double TTInPhase { get { return CumulativeTT; } }


    /// <summary>
    /// Reset phase
    /// </summary>
    public override void ResetPhase()
    {
        CumulativeTT = 0;
        CohortNoAtStart = 0;
    }


    /// <summary>
    /// Do our timestep development
    /// </summary>
    public override double DoTimeStep(double PropOfDayToUse)
    {
        if (First)
        {
            CohortNoAtStart = Leaf.ExpandedCohortNo;
            First = false;
        }

        // Accumulate thermal time.
        CumulativeTT += ThermalTime.Value;

        if (Leaf.ExpandedCohortNo >= (int)(Leaf.FinalLeafNo - RemainingLeaves))
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
            double F = (Leaf.ExpandedNodeNo - CohortNoAtStart) / ((Leaf.FinalLeafNo-RemainingLeaves) - CohortNoAtStart);
            if (F < 0) F = 0;
            if (F > 1) F = 1;
            return F;
        }
    }


}


      
      
