using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using CSGeneral;
using System.Collections;

[Description("returns a y value that corresponds to the position of the value of XProperty in the specified xy matrix")]
public class LinearInterpolationFunction : Function
{
    [Link]
    Plant Plant = null;

    [Link]
    XYPairs XYPairs = null;

    [Param]
    private string XProperty = "";

    private bool LookedForInternalVariable = false;
    private MemberInfo Member = null;
    private object Target = null;


    [Output]
    public override double Value
    {
        get
        {
            string PropertyName = XProperty;
            string ArraySpec = StringManip.SplitOffBracketedValue(ref PropertyName, '[', ']');
            double XValue;
            if (ArraySpec == "")
            {
                // Simple type
                if (!LookedForInternalVariable)
                {
                    Plant.GetMemberInfo(XProperty, Plant, out Member, out Target);
                }
                object v = null;

                if (Member != null)
                {
                    if (Member is FieldInfo)
                        v = (Member as FieldInfo).GetValue(Target);
                    else
                        v = (Member as PropertyInfo).GetValue(Target, null);
                }
                else
                    v = Plant.GetObject(XProperty);
                if (v == null)
                    throw new Exception("Cannot find value for "+ Name +" XProperty: " + XProperty);
                XValue = Convert.ToDouble(v);
            }
            else
                XValue = Convert.ToDouble(Plant.GetObject(XProperty));

            return XYPairs.ValueIndexed(XValue);
        }
    }

}

