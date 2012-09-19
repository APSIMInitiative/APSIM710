using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using ModelFramework;

/// <summary>
/// Calculate partitioning of daily growth based upon allometric relationship
/// </summary>
[Description("This must be renamed DMDemandFunction for the source code to recoginise it!!!!. Plant allometry is often described using a simple power function (y=kX^p).  This function returns the demand for DM that would be required to return the size of a pool to that calculated using a good old fashioned allometric relationship using the standard power function (y=kx^p)")]
public class AllometricDemandFunction : Function
{
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
            double XValue;
            if (!My.Get(XProperty, out XValue))
                throw new Exception("Cannot find variable: " + XProperty + " in function: " + My.Name);
            double YValue;
            if (!My.Get(YProperty, out YValue))
                throw new Exception("Cannot find variable: " + YProperty + " in function: " + My.Name);

            double Target = Const * Math.Pow(XValue, Power);
            returnValue = Math.Max(0.0,Target - YValue);

            return returnValue;
        }
    }

}

