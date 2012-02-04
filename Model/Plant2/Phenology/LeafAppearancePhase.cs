using System;
using System.Collections.Generic;
using System.Text;

public class LeafAppearancePhase : Phase
{
    [Link]
    Leaf Leaf = null;

    [Link]
    Function ThermalTime = null;

    //private double CumulativeTT;
    private double CohortNoAtStart;
    bool First = true;

    [Param]
    private double RemainingLeaves = 0;

    private double FractionCompleteYesterday = 0;

    /// <summary>
    /// Reset phase
    /// </summary>
    public override void ResetPhase()
    {
       _TTinPhase = 0;
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

        //
        FractionCompleteYesterday = FractionComplete;

        // Accumulate thermal time.
        _TTForToday = ThermalTime.Value * PropOfDayToUse;
        _TTinPhase += _TTForToday;

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
            return Math.Max(F, FractionCompleteYesterday); //Set to maximum of FractionCompleteYesterday so on days where final leaf number increases phenological stage is not wound back.
        }
    }


}


      
      
