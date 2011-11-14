using System;
using System.Collections.Generic;
using System.Text;
using System.Collections;
using System.Reflection;
using CSGeneral;


public class CompositeBiomass : Biomass
{
    [Link]
    Plant Plant = null;

    [Link]
    ModelEnvironment ModelEnvironment = null;

    [Param]
    private string[] Propertys = null;

    [Output]
    [Units("g/m^2")]
    override public double NonStructuralN
    {
        get
        {
            double Value = 0;
            foreach (string PropertyName in Propertys)
                Value += GetValue(PropertyName + ".NonStructuralN");
            return Value;
        }

        set { throw new Exception("Cannot set NonStructuralN in CompositeBiomass"); }
    }



    [Output]
    [Units("g/m^2")]
    override public double StructuralN
    {
        get
        {
            double Value = 0;
            foreach (string PropertyName in Propertys)
                Value += GetValue(PropertyName + ".StructuralN");
            return Value;
        }
        set { throw new Exception("Cannot set StructuralN in CompositeBiomass"); }
    }
    [Output]
    [Units("g/m^2")]
    override public double NonStructuralWt
    {
        get
        {
            double Value = 0;
            foreach (string PropertyName in Propertys)
                Value += GetValue(PropertyName + ".NonStructuralWt");
            return Value;
        }
        set { throw new Exception("Cannot set NonStructuralWt in CompositeBiomass"); }
    }
    [Output]
    [Units("g/m^2")]
    override public double StructuralWt
    {
        get
        {
            double Value = 0;
            foreach (string PropertyName in Propertys)
                Value += GetValue(PropertyName + ".StructuralWt");
            return Value;
        }
        set { throw new Exception("Cannot set StructuralWt in CompositeBiomass"); }
    }
    [Output]
    [Units("g/m^2")]
    override public double MetabolicN
    {
        get
        {
            double Value = 0;
            foreach (string PropertyName in Propertys)
                Value += GetValue(PropertyName + ".MetabolicN");
            return Value;
        }

        set { throw new Exception("Cannot set MetabolicN in CompositeBiomass"); }
    }
    [Output]
    [Units("g/m^2")]
    override public double MetabolicWt
    {
        get
        {
            double Value = 0;
            foreach (string PropertyName in Propertys)
                Value += GetValue(PropertyName + ".MetabolicWt");
            return Value;
        }

        set { throw new Exception("Cannot set MetabolicWt in CompositeBiomass"); }
    }

    override public void Clear()
    {
        // This is called in OnCut - for now do nothing.
    }

    private double GetValue(string PropertyName)
    {
        int PosBracket = PropertyName.IndexOf('[');
        if (PosBracket == -1)
            return Convert.ToDouble(ExpressionFunction.Evaluate(Plant, PropertyName));
        else
        {
            object ArrayObject = ModelEnvironment.Link<object>(Plant.FullName + "." + PropertyName.Substring(0, PosBracket));
            if (ArrayObject != null)
            {
                string RemainderOfPropertyName = PropertyName;
                string ArraySpecifier = StringManip.SplitOffBracketedValue(ref RemainderOfPropertyName, '[', ']');
                int PosRemainder = PropertyName.IndexOf("].");
                if (PosRemainder == -1)
                    throw new Exception("Invalid name path found in CompositeBiomass. Path: " + PropertyName);
                RemainderOfPropertyName = PropertyName.Substring(PosRemainder + 2);

                IList Array = (IList)ArrayObject;
                int ArrayIndex;
                if (int.TryParse(ArraySpecifier, out ArrayIndex))
                    return Convert.ToDouble(GetValueOfField(RemainderOfPropertyName, Array[ArrayIndex]));
                
                else
                {
                    double Sum = 0.0;
                    for (int i = 0; i < Array.Count; i++)
                    {
                        object Obj = GetValueOfField(RemainderOfPropertyName, Array[i]);
                        if (Obj == null)
                            throw new Exception("Cannot evaluate: " + RemainderOfPropertyName);

                        if (ArraySpecifier == "" || Utility.IsOfType(Array[i].GetType(), ArraySpecifier))
                        {
                            Sum += Convert.ToDouble(Obj);
                        }
                    }
                    return Sum;
                }
            }
        return 0;
        }
    }


    /// <summary>
    /// Return the value (using Reflection) of the specified property on the specified object.
    /// Returns null if not found.
    /// </summary>
    private static object GetValueOfField(string PropertyName, object I)
    {
        string[] Bits = PropertyName.Split(".".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
        for (int i = 0; i < Bits.Length; i++)
        {

            FieldInfo FI = I.GetType().GetField(Bits[i], BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Public);
            if (FI == null)
            {
                PropertyInfo PI = I.GetType().GetProperty(Bits[i], BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Public);
                if (PI == null)
                    return null;
                else
                    I = PI.GetValue(I, null);
            }
            else
                I = FI.GetValue(I);
        }
        return I;
    }
}
 