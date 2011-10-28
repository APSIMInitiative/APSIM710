using System;
using System.Collections.Generic;
using System.Text;

abstract public class Function
{
    abstract public double Value { get; }
    virtual public string ValueString { get { return Value.ToString(); } }

    [Link]
    public ModelEnvironment ModelEnvironment = null;

    public string Name { get { return ModelEnvironment.Name; } }
}
