using System;
using System.Collections.Generic;
using System.Text;
using ModelFramework;


abstract public class Phase
{
    [Param]
    public string Start;

    [Param]
    public string End;

    [Link]
    public Component My;

    public string Name { get { return My.Name; } }

    abstract public double DoTimeStep(double PropOfDayToUse);
    abstract public double FractionComplete { get; }

    [EventHandler]
    public void OnInitialised() { ResetPhase(); }

    public virtual void ResetPhase() { }

}
   
