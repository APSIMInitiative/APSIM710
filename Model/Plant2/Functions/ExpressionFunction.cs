using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Text;
using System.Reflection;
using CSGeneral;
using System.Data;
using EBMath;

/// <summary>
/// Evaluate a mathematical expression using the EvaluateExpression dll. 
/// Obs: Expression can contain variable names from Plant2
/// </summary>

public class ExpressionFunction : Function
{
    [Link]
    static Plant Plant = null;

    [Param]
    private string Expression = "";

    private ExpressionEvaluator fn = new ExpressionEvaluator();
    private bool parsed = false;

    [Output]
    public override double Value
    {
        get
        {
            if (!parsed)
            {
                Parse(fn, Expression);
                parsed = true;
            }
            FillVariableNames(fn);
            Evaluate(fn);
            return fn.Result;
        }
    }

    private static void Parse(ExpressionEvaluator fn, string ExpressionProperty)
    {
        fn.Parse(ExpressionProperty.Trim());
        fn.Infix2Postfix();
    }

    private static void FillVariableNames(ExpressionEvaluator fn)
    {
        ArrayList varUnfilled = fn.Variables;
        ArrayList varFilled = new ArrayList();
        Symbol symFilled;
        foreach (Symbol sym in varUnfilled)
        {
            symFilled.m_name = sym.m_name;
            symFilled.m_type = EBMath.Type.Variable;
            symFilled.m_valueString = "";
            symFilled.m_value = 0;
            object Value = ExpressionFunction.GetPropertyValueFromPlant(Plant, sym.m_name.Trim());
            if (Value is string)
                symFilled.m_valueString = Value.ToString();
            else
                symFilled.m_value = Convert.ToDouble(Value);
            varFilled.Add(symFilled);
        }
        fn.Variables = varFilled;
    }

    private static void Evaluate(ExpressionEvaluator fn)
    {
        fn.EvaluatePostfix();
        if (fn.Error)
        {
            throw new Exception(fn.ErrorDescription);
        }
    }

    /// <summary>
    /// Return the value of the specified property as an object. The PropertyName
    /// is relative to the RelativeTo argument (usually Plant).
    /// Format:
    ///    [function(]VariableName[)]
    /// Where:
    ///     function is optional and can be one of Sum, subtract, multiply, divide, max, min
    ///     VariableName is the name of a Plant variable. It can also include an array. Array can
    ///                  have a filter inside of square brackets. Filter can be an array index (0 based)
    ///                  or be the name of a class or base class.
    /// e.g. Leaf.MinT
    ///      sum(Leaf.Leaves[].Live.Wt)              - sums all live weights of all objects in leaves array.
    ///      subtract(Leaf.Leaves[].Live.Wt)         - subtracts all live weights of all objects in leaves array.
    ///      Leaf.Leaves[1].Live.Wt                  - returns the live weight of the 2nd element of the leaves array.
    ///      sum(Leaf.Leaves[AboveGround].Live.Wt)   - returns the live weight of the 2nd element of the leaves array.
    /// </summary>
    public static object Evaluate(Plant P, string Expression)
    {
        Plant = P;
        ExpressionEvaluator fn = new ExpressionEvaluator();
        Parse(fn, Expression);
        FillVariableNames(fn);
        Evaluate(fn);
        if (fn.Results != null)
            return fn.Results;
        else
            return fn.Result;
    }

    /// <summary>
    /// Return the value of the specified property as an object. The PropertyName
    /// is relative to the RelativeTo argument (usually Plant).
    /// Format:
    ///     PropertyName is the name of a Plant variable. It can also include an array. Array can
    ///                  have a filter inside of square brackets. Filter can be an array index (0 based)
    ///                  or be the name of a class or base class.
    /// e.g. Leaf.MinT
    ///      Leaf.Leaves[].Live.Wt              - returns all live weights of all objects in leaves array.
    ///      Leaf.Leaves[1].Live.Wt             - returns the live weight of the 2nd element of the leaves array.
    ///      Organs[AboveGround].Live.Wt        - returns all the live weights for all above ground organs.
    /// </summary>
    private static object GetPropertyValueFromPlant(object RelativeTo, string PropertyName)
    {
        while (PropertyName.Contains("."))
        {
            int PosPeriod = PropertyName.IndexOf('.');
            object O = null;
            string NameToLookFor = PropertyName.Substring(0, PosPeriod);
            string ArraySpecifier = null;
            if (NameToLookFor.Contains("[") && NameToLookFor.Contains("]"))
                ArraySpecifier = StringManip.SplitOffBracketedValue(ref NameToLookFor, '[', ']');

            if (RelativeTo is Instance)
            {
                // Try and look in the children list first.
                Instance Inst = (Instance)RelativeTo;
                if (Inst.Children.Contains(NameToLookFor))
                    O = Inst.Find(NameToLookFor);
            }
            if (O == null)
            {
                // Look for a field or property
                O = GetValueOfField(NameToLookFor, RelativeTo);
            }
            if (O == null)
                throw new Exception("Cannot find property: " + PropertyName);

            // Go handle arrays.
            if (ArraySpecifier != null)
            {
                IList Array = (IList)O;
                int ArrayIndex;
                if (int.TryParse(ArraySpecifier, out ArrayIndex))
                {
                    RelativeTo = Array[ArrayIndex];
                }
                else
                {
                    PropertyName = PropertyName.Substring(PosPeriod + 1);
                    string Value = "";
                    for (int i = 0; i < Array.Count; i++)
                    {
                        object Obj = GetPropertyValueFromPlant(Array[i], PropertyName);
                        if (Obj == null)
                            throw new Exception("Cannot evaluate: " + PropertyName);

                        if (ArraySpecifier == "" || Utility.IsOfType(Array[i].GetType(), ArraySpecifier))
                        {
                            if (Value != "")
                                Value += ",";
                            Value += Obj.ToString();
                        }
                    }
                    return Value;
                }

            }
            RelativeTo = O;
            PropertyName = PropertyName.Substring(PosPeriod + 1);
        }

        return GetValueOfField(PropertyName, RelativeTo);
    }

    /// <summary>
    /// Return the value (using Reflection) of the specified property on the specified object.
    /// Returns null if not found.
    /// </summary>
    private static object GetValueOfField(string PropertyName, object I)
    {
        FieldInfo FI = I.GetType().GetField(PropertyName, BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Public);
        object v = null;
        if (FI == null)
        {
            PropertyInfo PI = I.GetType().GetProperty(PropertyName, BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Public);
            if (PI != null)
                v = PI.GetValue(I, null);
        }
        else
            v = FI.GetValue(I);
        return v;
    }
}



