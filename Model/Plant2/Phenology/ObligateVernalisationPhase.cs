using System;
using System.Collections.Generic;
using System.Text;


public class ObligateVernalisationPhase : Phase
{
    [Link]
    Function JuvenileDevelopmentIndex = null;

    [Link]
    Function Target = null;


    //Class Parameters
    private double ExtentOfVernYesterday;
    private double ExtentOfVern;

    [Output]
    public double CumulativeVern { get { return ExtentOfVern; } }
    
    /// <summary>
    /// Reset phase
    /// </summary>
    public override void ResetPhase() { ExtentOfVernYesterday = 0; }

    /// <summary>
    /// Do our timestep development
    /// </summary>
    public override double DoTimeStep(double PropOfDayToUse)
    {
        // Calculate the Vern for today.
        ExtentOfVern = JuvenileDevelopmentIndex.Value * PropOfDayToUse;

        // Accumulate Vernalisation
        //_CumulativeVern += VernForToday;

        // Get the Target TT
        double Target = CalcTarget();

        // Work out if we've reached our target. 
        // If we have then return the left over day to our caller.
        double PropOfDayUnused = 0.0;
        if (ExtentOfVern > Target)
        {
            double LeftOverValue = ExtentOfVern - Target;
            if (ExtentOfVern > 0.0)
            {
                double PropOfValueUnused = LeftOverValue / (ExtentOfVern - ExtentOfVernYesterday);
                PropOfDayUnused = PropOfValueUnused * PropOfDayToUse;
            }
            else
                PropOfDayUnused = 1.0;
        }
        if (ExtentOfVern == Target)
            PropOfDayUnused = 1.0;

        ExtentOfVernYesterday = ExtentOfVern;
        return PropOfDayUnused;
    }

    /// <summary>
    /// Return the target to caller. Can be overridden by derived classes.
    /// </summary>
    protected virtual double CalcTarget()
    {
        return Target.Value;
    }

    /// <summary>
    /// Return a fraction of phase complete.
    /// </summary>
    public override double FractionComplete
    {
        get
        {
            return ExtentOfVern / CalcTarget();
        }
    }
}


      
      
