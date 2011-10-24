using System;
using System.Collections.Generic;
using System.Text;


abstract public class Phase
{
    [Param]
    public string Start;

    [Param]
    public string End;

    [Link]
    public ModelEnvironment ModelEnvironment;

    public string Name { get { return ModelEnvironment.Name; } }

    abstract public double DoTimeStep(double PropOfDayToUse);
    abstract public double FractionComplete { get; }

    [EventHandler]
    public void OnInitialised() { ResetPhase(); }

    public virtual void ResetPhase() { }

}
   
