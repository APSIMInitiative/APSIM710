using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using CSGeneral;

[Description("Returns the age (in years) of the crop")]
class AgeCalculatorFunction : Function
{
    private int _Age = 0;

    [EventHandler]
    private void OnTick(TimeType T)
    {
        _Age = _Age + 1;
    }
    [Output]
    [Units("y")]
    public override double Value
    {
        get
        {
            return _Age/365.25;
        }
    }
    [Output("Age")]
    [Units("y")]
    public double Age
    {
        get
        {
            return _Age / 365.25;
        }
    }

}