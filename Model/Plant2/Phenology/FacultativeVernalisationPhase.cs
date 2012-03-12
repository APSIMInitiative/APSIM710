using System;
using System.Collections.Generic;
using System.Text;


public class FacultativeVernalisationPhase : Phase
{
    [Link]
    Function Target = null;

    [Link]
    Structure Structure = null;

    [Link]
    VernalisationSIRIUS VernalisationSIRIUS = null;
    
    //Class Parameters
    private double ExtentOfDevelopmentYesterday;
    private double ExtentOfDevelopment;
    public double _JuvenileDevelopmentIndex = 0;
    public bool InitialLoop = true;

    /// <summary>
    /// Reset phase
    /// </summary>
    public override void ResetPhase() { ExtentOfDevelopmentYesterday = 0; }

    /// <summary>
    /// This function increments vernalisation units accumulated in phase 
    /// and returns a non-zero value if the phase target is met today so
    /// the phenology class knows to progress to the next phase and how
    /// much tt to pass it on the first day.
    /// </summary>
    public double JuvenileDevelopmentIndex
    {
        get
        {
            return _JuvenileDevelopmentIndex;
        }
    }
    
    public void UpdateJuvenileDevelopmentIndex()
    {
            if (Structure.MainStemPrimordiaNo == 0)
                _JuvenileDevelopmentIndex = VernalisationSIRIUS.AccumulatedVernalisation;
            else if ((VernalisationSIRIUS.AccumulatedVernalisation >= 1) || (Structure.MainStemPrimordiaNo >= Structure.AttainableFinalNodeNumber))
                _JuvenileDevelopmentIndex = 1.0;
            else
                _JuvenileDevelopmentIndex = Math.Max(Structure.MainStemPrimordiaNo / Structure.AttainableFinalNodeNumber, VernalisationSIRIUS.AccumulatedVernalisation);
        }

    public override double DoTimeStep(double PropOfDayToUse)
    {
        // Calculate the Vern for today.
        UpdateJuvenileDevelopmentIndex();
        ExtentOfDevelopment = JuvenileDevelopmentIndex * PropOfDayToUse;
        
        // Get the Target TT
        double Target = CalcTarget();

        double PropOfDayUnused = 0.0;
        if (ExtentOfDevelopment > Target)
        {
            double LeftOverValue = ExtentOfDevelopment - Target;
            if (ExtentOfDevelopment > 0.0)
            {
                double PropOfValueUnused = LeftOverValue / (ExtentOfDevelopment - ExtentOfDevelopmentYesterday);
                PropOfDayUnused = PropOfValueUnused * PropOfDayToUse;
            }
            else
                PropOfDayUnused = 1.0;
        }
        if (ExtentOfDevelopment == Target)
            PropOfDayUnused = 1.0;

        ExtentOfDevelopmentYesterday = ExtentOfDevelopment;
        //AccumulatedVernalisation += Vernalisation.Value;
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
            return ExtentOfDevelopment / CalcTarget();
        }
    }

 

}


      
      
