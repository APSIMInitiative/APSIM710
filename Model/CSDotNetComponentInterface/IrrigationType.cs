using System;
using System.Runtime.InteropServices;

//------ Fertiliser ------
public class IrrigationType
{
    private Component Comp;
    public IrrigationType(Component c)
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

    public Single irrigation
    {
        get
        {
            return Comp.Variable("irrigation").ToSingle();
        }
    }

    public Single irrig_tot
    {
        get
        {
            return Comp.Variable("irrig_tot").ToSingle();
        }
    }

    public Single allocation
    {
        get
        {
            return Comp.Variable("allocation").ToSingle();
        }
    }

    public Single irrigation_efficiency
    {
        get
        {
            return Comp.Variable("irrigation_efficiency").ToSingle();
        }
        set
        {
            Comp.Variable("irrigation_efficiency").Set(value);
        }
    }

    public String name
    {
        get
        {
            return Comp.Variable("name").ToString();
        }
    }
    void Apply(double Amount)
    {
        IrrigationApplicationType Irrigation = new IrrigationApplicationType();
        Irrigation.Amount = (float)Amount;
        Comp.Publish("Apply2", Irrigation);
    }
}
