using System;

/// <summary>
/// These classes define the attributes used to provide metadata for the 
/// APSIM Component properties and events.
/// </summary>

[AttributeUsage(AttributeTargets.Method)]
public class EventHandler : Attribute
{
}

[AttributeUsage(AttributeTargets.Event)]
public class Event : Attribute
{
}


[AttributeUsage(AttributeTargets.Field | AttributeTargets.Property, AllowMultiple = true)]
public class Param : Attribute
{
    public String Name;
    public Boolean Optional;
    public Double MinVal;
    public Double MaxVal;
    public Param()
    {
        Name = "";
        Optional = false;
		MinVal = MaxVal = Double.NaN;  // Using Nan to indicate min. and max. values not set
    }
    public Param(Boolean IsOptional)
	{
			Name = "";
			Optional = IsOptional;
			MinVal = MaxVal = Double.NaN;  // Using Nan to indicate min. and max. values not set
    }
    public Param(String sName)
    {
        Name = sName;
        Optional = false;
		MinVal = MaxVal = Double.NaN;
    }
    public Param(String Name, bool IsOptional)
    {
        this.Name = Name;
        Optional = IsOptional;
        MinVal = MaxVal = Double.NaN;
    }
}

[AttributeUsage(AttributeTargets.Field | AttributeTargets.Property, AllowMultiple = true)]
public class Input : Attribute
{
    public bool Optional;
    public Input()
    {
        Optional = false;
    }
    public Input(bool IsOptional)
    {
        Optional = IsOptional;
    }
}

[AttributeUsage(AttributeTargets.Field | AttributeTargets.Property, AllowMultiple = true)]
public class Output : Attribute
{
    public String Name;
    public Output()
    {
        Name = "";
    }
    public Output(String sName)
    {
        Name = sName;
    }
}

[AttributeUsage(AttributeTargets.Field, AllowMultiple = true)]
public class Writable : Attribute
{
}

[AttributeUsage(AttributeTargets.Field | AttributeTargets.Property, AllowMultiple = true)]
public class Units : Attribute
{
    private String St;
    public Units(String Units)
    {
        St = Units;
    }
    public override String ToString()
    {
        return St;
    }
}

[AttributeUsage(AttributeTargets.Field | AttributeTargets.Property | AttributeTargets.Class, AllowMultiple = true)]
public class Description : Attribute
{
    private String St;

    public Description(String Description)
    {
        St = Description;
    }

    public override String ToString()
    {
        return St;
    }
}


[AttributeUsage(AttributeTargets.Class, AllowMultiple = true)]
public class Model : Attribute
{
}

public enum IsOptional {Yes, No};

[AttributeUsage(AttributeTargets.Field, AllowMultiple = true)]
public class Link : Attribute
{
    public String _Path;
    public IsOptional _IsOptional;
    public Link() { _Path = null; _IsOptional = IsOptional.No; }
    public Link(IsOptional isOptional) { _Path = null; _IsOptional = isOptional; }
    public Link(String Path) { _Path = Path; _IsOptional = IsOptional.No; }
    public Link(String Path, IsOptional isOptional) { _Path = Path; _IsOptional = isOptional; }
}	


