using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using CSGeneral;

[Description("Takes the value of the child as the x value and returns the y value from a sigmoid of the form y = Xmax * 1/1+exp(-(x-Xo)/b)")]
public class SigmoidFunction2 : Function
{
    [Link]
    public Function Ymax = null;
    [Link]
    public Function XValue = null;
  
    [Param]
    private double Xo = 1.0;
    [Param]
    private double b = 1.0;
    
    
    [Output]
    public override double Value
    {
        get
        {

            try
            {
                return Ymax.Value * 1 / (1 + Math.Exp(-(XValue.Value - Xo) / b));
            }
            catch (Exception E)
            {
                throw new Exception("Error with values to Sigmoid function");
            }
         }
    }

}

