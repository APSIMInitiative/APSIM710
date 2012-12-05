using System;
using System.Collections.Generic;
using System.Text;
using System.Xml.Serialization;


[AttributeUsage(AttributeTargets.Field | AttributeTargets.Property)]
[XmlType("ApsimX.Input")]
public class Input : System.Attribute
{
    private bool Optional = false;

    public bool IsOptional
    {
        get { return Optional; }
        set { Optional = value; }
    }
}

[XmlType("ApsimX.Param")]
[AttributeUsage(AttributeTargets.Field | AttributeTargets.Property)]
public class Param : System.Attribute
{
    public string Alias = null;
    private bool _Optional = false;
    private double _MinVal = Double.NaN;
    private double _MaxVal = Double.NaN;

    public string Name
    {
        get { return Alias; }
        set { Alias = value; }
    }

    public bool IsOptional
    {
        get { return _Optional; }
        set { _Optional = value; }
    }

    public double MinVal
    {
        get { return _MinVal; }
        set { _MinVal = value; }
    }

    public double MaxVal
    {
        get { return _MaxVal; }
        set { _MaxVal = value; }
    }
}

[XmlType("ApsimX.Output")]
[AttributeUsage(AttributeTargets.Field | AttributeTargets.Property)]
public class Output : System.Attribute
{
    private bool _Immutable = false;
    public Output()
    {
    }

    public bool Immutable
    {
        get { return _Immutable; }
        set { _Immutable = value; }
    }


    // Deprecated.
    public Output(string Text)
    {
    }
}

[XmlType("ApsimX.EventHandler")]
[AttributeUsage(AttributeTargets.Method)]
public class EventHandler : System.Attribute
{
    private string _Name;
    public string EventName
    {
        get { return _Name; }
        set { _Name = value; }
    }
}

[XmlType("ApsimX.Link")]
[AttributeUsage(AttributeTargets.Field, AllowMultiple = true)]
public class Link : Attribute
{
    private String _Path = null;
    private bool _IsOptional = false;

    public string NamePath
    {
        get { return _Path; }
        set { _Path = value; }
    }
    public bool IsOptional
    {
        get { return _IsOptional; }
        set { _IsOptional = value; }
    }

}

[XmlType("ApsimX.Description")]
[AttributeUsage(AttributeTargets.Class | AttributeTargets.Field | AttributeTargets.Property)]
public class Description : System.Attribute
{
    private string _Text;
    public Description(string Text)
    {
        _Text = Text;
    }
    public string ToString()
    {
        return _Text;
    }
}

[XmlType("ApsimX.Units")]
[AttributeUsage(AttributeTargets.Field | AttributeTargets.Property)]
public class Units : System.Attribute
{
    public Units(string text)
    {
    }
}

[XmlType("ApsimX.Event")]
[AttributeUsage(AttributeTargets.Event)]
public class Event : System.Attribute
{

}
