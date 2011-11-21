using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using CSGeneral;
using System.Collections;



public class LinearInterpolationFunction : Function
{
    [Link]
    Plant Plant = null;

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
            string ArraySpec = StringManip.SplitOffBracketedValue(ref PropertyName, '(', ')');
            double XValue;
            if (ArraySpec == "")
            {
                object v = ExpressionFunction.Evaluate(Plant, XProperty);
                if (v == null)
                    throw new Exception("Cannot find value for "+ Name +" XProperty: " + XProperty);
                XValue = Convert.ToDouble(v);
            }
            else
            {
                string[] ArraySpecBits = ArraySpec.Split("=".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
                if (ArraySpecBits.Length != 2)
                    throw new Exception("Invalid depth specifier in generic funciton. Specifier = " + XProperty);
                int Index;
                if (ArraySpecBits[0].ToLower() == "depth")
                {
                    double Depth = Convert.ToDouble(ExpressionFunction.Evaluate(Plant, ArraySpecBits[1]));

                    // Assume dlayer comes from the same object as the depth variable.
                    int PosLastPeriod = ArraySpecBits[1].LastIndexOf('.');
                    if (PosLastPeriod == -1)
                        throw new Exception("Invalid depth specifier in generic funciton. Specifier = " + XProperty);
                    string ObjectName = ArraySpecBits[1].Substring(0, PosLastPeriod);
                    double[] dlayer = (double[])ExpressionFunction.Evaluate(Plant, ObjectName + ".dlayer");
                    Index = LayerIndex(Depth, dlayer);
                }
                else if (ArraySpecBits[0].ToLower() == "index")
                {
                    if (!Int32.TryParse(ArraySpecBits[1], out Index))
                        Index = (int)Convert.ToDouble(ExpressionFunction.Evaluate(Plant, ArraySpecBits[1]));
                }
                else
                    throw new Exception("Invalid depth specifier in generic funciton. Specifier = " + XProperty);

                double[] Values = (double[])ExpressionFunction.Evaluate(Plant, PropertyName);
                if (Values.Length == 0 || Index >= Values.Length)
                    throw new Exception("Index is outside the bounds of array " + PropertyName + " in generic function. Specifier = " + XProperty);
                XValue = Values[Index];
            }
            return XYPairs.ValueIndexed(XValue);
        }
    }

    static private int LayerIndex(double depth, double[] dlayer)
    {
        double CumDepth = 0;
        for (int i = 0; i < dlayer.Length; i++)
        {
            CumDepth = CumDepth + dlayer[i];
            if (CumDepth >= depth) { return i; }
        }
        throw new Exception("Depth deeper than bottom of soil profile in GenericFunction.");
    }




}

