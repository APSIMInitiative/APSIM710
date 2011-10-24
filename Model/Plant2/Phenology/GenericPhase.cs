using System;
using System.Collections.Generic;
using System.Text;


public class GenericPhase : Phase
{
    private double _CumulativeTT;
    [Output]
    public double CumulativeTT { get { return _CumulativeTT; } }

    [Link]
    public Function ThermalTime = null;

    [Link(IsOptional.Yes)]
    public Function Stress = null;

    [Link(IsOptional.Yes)]
    public Function Target = null;


     /// <summary>
    /// Reset phase
    /// </summary>
    public override void ResetPhase() { _CumulativeTT = 0; }

    /// <summary>
    /// Do our timestep development
    /// </summary>
    public override double DoTimeStep(double PropOfDayToUse)
    {
        // Calculate the TT for today.      
        double TTForToday = ThermalTime.Value * PropOfDayToUse;

        // Reduce the TT for today if a "stress" function is present.
        if (Stress != null)
            TTForToday *= Stress.Value;

        // Accumulate the TT
        _CumulativeTT += TTForToday;

        // Get the Target TT
        double Target = CalcTarget();

        // Work out if we've reached our target. 
        // If we have then return the left over day to our caller.
        double PropOfDayUnused = 0.0;
        if (_CumulativeTT > Target)
        {
            double LeftOverValue = _CumulativeTT - Target;
            if (TTForToday > 0.0)
            {
                double PropOfValueUnused = LeftOverValue / TTForToday;
                PropOfDayUnused = PropOfValueUnused * PropOfDayToUse;
            }
            else
                PropOfDayUnused = 1.0;
            _CumulativeTT = Target;
        }

        return PropOfDayUnused;
    }

    /// <summary>
    /// Return the target to caller. Can be overridden by derived classes.
    /// </summary>
    protected virtual double CalcTarget()
    {
        if (Target == null)
            throw new Exception("Cannot find target for phase: " + Name);
        return Target.Value;
    }

    /// <summary>
    /// Return a fraction of phase complete.
    /// </summary>
    public override double FractionComplete
    {
        get
        {
            return _CumulativeTT / CalcTarget();
        }
    }

}


      
      
