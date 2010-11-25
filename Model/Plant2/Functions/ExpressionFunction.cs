using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Text;
using System.Reflection;
using CSGeneral;
using System.Data;
using EB.Math;

/// <summary>
/// Evaluate a mathematical expression using the EvaluateExpression dll. 
/// Obs: Expression can contain variable names from Plant2
/// </summary>

public class ExpressionFunction : Function
{
    [Param]
    private string ExpressionProperty = "";

    private ExpressionEvaluator fn = new ExpressionEvaluator();
    private bool parsed = false;

    [Output]
    public override double Value
    {
        get
        {
            Parse();
            FillVariableNames();
            Evaluate();
            return fn.Result;
        }
    }
    
    private void Parse()
    {
        if (!parsed)
        {
            fn.Parse(ExpressionProperty.Trim());
            fn.Infix2Postfix();
            parsed = true;
        }
    }

    private void FillVariableNames()
    {
        ArrayList varUnfilled = fn.Variables;
        ArrayList varFilled = new ArrayList();
        Symbol symFilled;
        foreach (Symbol sym in varUnfilled)
        {
            symFilled.m_name = sym.m_name;
            symFilled.m_type = EB.Math.Type.Variable;
            symFilled.m_value = Convert.ToDouble(GenericFunction.GetPropertyValueFromPlant((Plant)Root, sym.m_name.Trim()));
            varFilled.Add(symFilled);
        }
        fn.Variables = varFilled;
    }

    private void Evaluate()
    {
      fn.EvaluatePostfix();
      if(fn.Error)
      {
          throw new Exception(fn.ErrorDescription);
      }
    }
}

