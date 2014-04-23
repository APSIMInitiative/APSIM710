using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using CSGeneral;
using System.Collections;
using ModelFramework;

[Description("returns a y value that corresponds to the position of the value of XProperty in the specified xy matrix")]
public class LinearInterpolationFunction : Function
{
    [Link]
    XYPairs XYPairs = null;

    [Param]
    private string XProperty = "";

    [Output]
    public override double Value
    {
        get
        {
            string PropertyName = XProperty;
            string ArraySpec;
            bool ArrayFound = PropertyName.Contains("[");
            if (ArrayFound)
                ArraySpec = StringManip.SplitOffBracketedValue(ref PropertyName, '[', ']');
            double XValue = 0;
            try
            {
                object v = Util.GetVariable(XProperty, My);
                if (v == null)
                    throw new Exception("Cannot find value for " + Name + " XProperty: " + XProperty);
                XValue = Convert.ToDouble(v);
            }
            catch (IndexOutOfRangeException)
            {
            }
            return XYPairs.ValueIndexed(XValue);
        }
    }

    public double ValueForX(double XValue)
    {
        return XYPairs.ValueIndexed(XValue);
    }

    public override double[] Values
    {
        get
        {
            string PropertyName = XProperty;

            object v = null;
            My.GetObject(XProperty, out v);
            if (v == null)
                throw new Exception("Cannot find value for " + Name + " XProperty: " + XProperty);
            if (v is Array)
            {
                Array A = v as Array;
                double[] ReturnValues = new double[A.Length];
                for (int i = 0; i < A.Length; i++)
                {
                    double XValue = Convert.ToDouble(A.GetValue(i));
                    ReturnValues[i] = XYPairs.ValueIndexed(XValue);
                }
                return ReturnValues;
            }
            else
            {
                double XValue = Convert.ToDouble(v);
                return new double[1] { XYPairs.ValueIndexed(XValue) };
            }
        }
    }

}

