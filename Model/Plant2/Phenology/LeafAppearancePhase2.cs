using System;
using System.Collections.Generic;
using System.Text;

public class LeafAppearancePhase2 : Phase
{
    [Link]
    Leaf Leaf = null;

    private double CumulativeTT;
    private double NodeNoAtStart;
    bool First = true;

    [Param]
    private double RemainingLeaves = 0;

    [Output]
    public double TTInPhase { get { return CumulativeTT; } }


    /// <summary>
    /// Initialise everything
    /// </summary>
    public override void Initialising()
    {
        CumulativeTT = 0;
        NodeNoAtStart = 0;
    }


    /// <summary>
    /// Do our timestep development
    /// </summary>
    public override double DoTimeStep(double PropOfDayToUse)
    {
        if (First)
        {
            NodeNoAtStart = Leaf.CohortNo;
            First = false;
        }

        // Accumulate thermal time.
        Function TT = Children["ThermalTime"] as Function;
        CumulativeTT += TT.Value;

        if (Leaf.CohortNo >= (int)(Leaf.FinalNodeNo - RemainingLeaves))
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
            double F = (Leaf.CohortNo - NodeNoAtStart) / (Leaf.FinalNodeNo - NodeNoAtStart);
            if (F < 0) F = 0;
            if (F > 1) F = 1;
            return F;
        }
    }


}


      
      
