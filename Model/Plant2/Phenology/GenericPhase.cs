using System;
using System.Collections.Generic;
using System.Text;


public class GenericPhase : Phase
{
    [Link]
    public Function ThermalTime = null;

    [Link(IsOptional=true)]
    public Function Stress = null;

    [Link(IsOptional=true)]
    public Function Target = null;

    /// <summary>
    /// Do our timestep development
    /// </summary>
    public override double DoTimeStep(double PropOfDayToUse)
    {
        // Calculate the TT for today.      
        _TTForToday = ThermalTime.Value * PropOfDayToUse;

        // Reduce the TT for today if a "stress" function is present.
        if (Stress != null)
            _TTForToday *= Stress.Value;

        // Accumulate the TT
        _TTinPhase += _TTForToday;

        // Get the Target TT
        double Target = CalcTarget();

        // Work out if we've reached our target. 
        // If we have then return the left over day to our caller.
        double PropOfDayUnused = 0.0;
        if (_TTinPhase > Target)
        {
            double LeftOverValue = _TTinPhase - Target;
            if (_TTForToday > 0.0)
            {
                double PropOfValueUnused = LeftOverValue / _TTForToday;
                PropOfDayUnused = PropOfValueUnused * PropOfDayToUse;
            }
            else
                PropOfDayUnused = 1.0;
            _TTinPhase = Target;
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
            return _TTinPhase / CalcTarget();
        }
    }

}


      
      
