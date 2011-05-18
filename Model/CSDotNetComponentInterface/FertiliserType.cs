using System;
using System.Runtime.InteropServices;

//------ Fertiliser ------
public class FertiliserType
{
    private Component Comp;
    public FertiliserType(Component c)
    {
        Comp = c;
    }
    Variable Variable(String VariableName)
    {
        return Comp.Variable(VariableName);
    }

    void Publish(String EventName, ApsimType Data)
    {
        Comp.Publish(EventName, Data);
    }

    public Single fertiliser
    {
        get
        {
            return Comp.Variable("fertiliser").ToSingle();
        }
    }
    public String name
    {
        get
        {
            return Comp.Variable("name").ToString();
        }
    }
    void Apply(double Amount, double Depth, String Type)
    {
        FertiliserApplicationType Fertiliser = new FertiliserApplicationType();
        Fertiliser.Amount = (float)Amount;
        Fertiliser.Depth = (float)Depth;
        Fertiliser.Type = Type;
        Comp.Publish("Apply", Fertiliser);
    }
}
