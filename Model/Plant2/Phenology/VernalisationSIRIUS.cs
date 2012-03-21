using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

public class VernalisationSIRIUS
{
    [Link]
    Function Vernalisation = null;

    [Param]
    private double VernalisationType = 0;

    [Output]
    public double AccumulatedVernalisation = 0;
    
    
    /// <summary>
    /// Trap the NewMet event.
    /// </summary>
    [EventHandler]
    public void OnNewMet(NewMetType NewMet)
    {
        AccumulatedVernalisation += Vernalisation.Value;
        AccumulatedVernalisation = Math.Min(AccumulatedVernalisation, 1.0);
    }

    /// <summary>
    /// Initialise everything - gets called at sowing as well.
    /// </summary>
    [EventHandler] 
    public void OnInitialised()
    {
        AccumulatedVernalisation = VernalisationType;
    }
}
   
