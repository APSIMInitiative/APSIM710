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
    
    [Link]
    public Component My = null;

    [Link]
    public MetFile MetData = null;

    [Link]
    public Clock Clock = null;

    public string Name { get { return My.Name; } }
}
