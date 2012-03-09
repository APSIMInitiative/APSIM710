using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;

/// <summary>
/// Abstract variable class for encapsulating an APSIM variable, be it input, output, param or state.
/// </summary>
abstract class VariableBase
{
    public string Name;
    private VariableBase Output;
    public bool IsOptional;
    internal void ConnectTo(VariableBase Out)
    {
        Output = Out;
    }


    internal void UpdateValue()
    {
        if (Output == null)
        {
            if (!IsOptional)
                throw new Exception("Cannot find a value for [Input] variable: " + Name);
        }
        else
            Value = Output.Value;
    }
    public bool IsConnected { get { return Output != null; } }

    public abstract Type Type { get; }
    public abstract object Value { get; set; }

}


/// <summary>
/// A derived variable class for encapsulating a reflected FieldInfo.
/// </summary>
class FieldVariable : VariableBase
{
    private FieldInfo Info;
    object Model;
    public FieldVariable(FieldInfo FI, object M)
    {
        IsOptional = false;
        Info = FI;
        Model = M;
        Name = Info.Name;
    }

    public override object Value
    {
        get { return Info.GetValue(Model); }
        set { Info.SetValue(Model, value); }
    }
    public override Type Type
    {
        get { return Info.FieldType; }
    }
}

/// <summary>
/// A derived variable class for encapsulating a reflected PropertyInfo.
/// </summary>
class PropertyVariable : VariableBase
{
    private PropertyInfo Info;
    object Model;
    public PropertyVariable(PropertyInfo FI, object M)
    {
        Info = FI;
        Model = M;
        Name = Info.Name;
    }

    public override object Value
    {
        get { return Info.GetValue(Model, null); }
        set { Info.SetValue(Model, value, null); }
    }
    public override Type Type
    {
        get { return Info.PropertyType; }
    }
}

/// <summary>
/// A simple string variable class for holding a string value. Used with [Param] variables.
/// </summary>
class StringVariable<T> : VariableBase
{
    private T Val;
    public StringVariable(string name, T value)
    {
        Name = name;
        Val = value;
    }


    public override object Value
    {
        get { return Val; }
        set { new NotImplementedException("A StringVariable is not settable"); }
    }
    public override Type Type
    {
        get { return Val.GetType(); }
    }
}