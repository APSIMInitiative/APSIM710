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

    [Link]
    public Phenology Phenology = null;

    protected double _TTForToday = 0;
    [Output]
    public double TTForToday { get { return _TTForToday; } }
    protected double _TTinPhase = 0;
    [Output]
    public double TTinPhase { get { return _TTinPhase; } }
    
    public string Name { get { return My.Name; } }

    abstract public double DoTimeStep(double PropOfDayToUse);
    abstract public double FractionComplete { get; }

    [EventHandler]
    public void OnInitialised() { ResetPhase(); }
    public virtual void ResetPhase() { _TTinPhase = 0; }
        
}
   
