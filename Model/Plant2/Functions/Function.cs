using System;
using System.Collections.Generic;
using System.Text;
using ModelFramework;

abstract public class Function
{
    abstract public double Value { get; }
    virtual public string ValueString { get { return Value.ToString(); } }

    [Link]
    public Component My = null;

    public string Name { get { return My.Name; } }
}
