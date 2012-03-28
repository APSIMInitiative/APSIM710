using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;

/// <summary>
/// Calculate partitioning of daily growth based upon allometric relationship
/// </summary>
[Description("This must be renamed DMDemandFunction for the source code to recoginise it!!!!. Plant allometry is often described using a simple power function (y=kX^p).  This function returns the demand for DM that would be required to return the size of a pool to that calculated using a good old fashioned allometric relationship using the standard power function (y=kx^p)")]
public class AllometricDemandFunction : Function
{
    [Link]
    Plant Plant = null;

    [Param]
    double Const = 0.0; 
    [Param]
    double Power = 0.0;
    [Param]
    string XProperty = null;
    [Param]
    string YProperty = null;


    [Output]
    public override double Value
    {
        get
        {
            double returnValue = 0.0;
            double XValue = Plant.GetObject(XProperty);
            double YValue = Plant.GetObject(YProperty);
            double Target = Const * Math.Pow(XValue, Power);
            returnValue = Math.Max(0.0,Target - YValue);

            return returnValue;
        }
    }

}

