using System;
using System.Collections.Generic;
using System.Text;
using ModelFramework;

[Description("Base class from which other functions inherit")]
abstract public class Function
{
    abstract public double Value { get; }
    virtual public string ValueString { get { return Value.ToString(); } }
    virtual public double[] Values { get { return new double[1] { Value }; } }

    virtual public void UpdateVariables(string initial) { }
   // virtual public double AttainableFinalNodeNumber { get { return 0; } }
   // virtual public double VernalisationFinalNodeNumber { get { return 0; } }
  //  virtual public double PhotoperiodFinalNodeNumber { get { return 0; } }
  //  virtual public double TerminatedFinalNodeNumber { get { return 0; } }
   // virtual public double CommitHaunStage { get { return 0; } }
   // virtual public double TargetFinalNodeNumber { get { return 0; } }
    virtual public void SetFinalNodeNumber() { }

    public double _VernalisationIndex = 0;

    [Link]
    public Component My = null;

    public string Name { get { return My.Name; } }
}
