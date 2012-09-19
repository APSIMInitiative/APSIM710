﻿using System;
using System.Collections.Generic;
using System.Text;
using System.Collections;
using System.Reflection;
using ModelFramework;


public class ArrayBiomass
{
    [Link]
    Component My = null;

    [Param]
    private string[] Propertys = null;

    [Param]
    private string ArraySize = null;
    private int ArraySizeNumber = -1;

    [Output]
    [Units("g/m^2")]
    public double[] NonStructuralN
    {
        get
        {
            return AddValuesToList(".NonStructuralN");
        }

        set { throw new Exception("Cannot set NonStructuralN in ArrayBiomass"); }
    }
    [Output]
    [Units("g/m^2")]
    public double[] StructuralN
    {
        get
        {
            return AddValuesToList(".StructuralN");
        }

        set { throw new Exception("Cannot set StructuralN in ArrayBiomass"); }
    }
    [Output]
    [Units("g/m^2")]
    public double[] MetabolicN
    {
        get
        {
            return AddValuesToList(".MetabolicN");
        }

        set { throw new Exception("Cannot set MetabolicN in ArrayBiomass"); }
    }
    [Output]
    [Units("g/m^2")]
    public double[] NonStructuralWt
    {
        get
        {
            return AddValuesToList(".NonStructuralWt");
        }
        set { throw new Exception("Cannot set NonStructuralWt in ArrayBiomass"); }
    }
    [Output]
    [Units("g/m^2")]
    public double[] StructuralWt
    {
        get
        {
            return AddValuesToList(".StructuralWt");
        }

        set { throw new Exception("Cannot set StructuralWt in ArrayBiomass"); }
    }
    [Output]
    [Units("g/m^2")]
    public double[] MetabolicWt
    {
        get
        {
            return AddValuesToList(".MetabolicWt");
        }

        set { throw new Exception("Cannot set MetabolicWt in ArrayBiomass"); }
    }
    [Output]
    [Units("g/g")]
    public double[] NConc
    {
        get
        {
            return AddValuesToList(".NConc");
        }
    }
    [Output]
    [Units("g/g")]
    public double[] StructuralNConc
    {
        get
        {
            return AddValuesToList(".StructuralNConc");
        }
    }
    [Output]
    [Units("g/g")]
    public double[] NonStructuralNConc
    {
        get
        {
            return AddValuesToList(".NonStructuralNConc");
        }
    }
    [Output]
    [Units("g/g")]
    public double[] MetabolicNConc
    {
        get
        {
            return AddValuesToList(".MetabolicNConc");
        }
    }


    /// <summary>
    /// Helper method to go find the value(s) of a property and put into a List<double>
    /// </summary>
    private double[] AddValuesToList(string SubPropertyName)
    {
        if (ArraySizeNumber == -1)
            ArraySizeNumber = Convert.ToInt32(ExpressionFunction.Evaluate( ArraySize, My));

        double[] Values = new double[ArraySizeNumber];
        int i = 0;
        foreach (string PropertyName in Propertys)
        {
            object Obj = Util.GetVariable(PropertyName + SubPropertyName, My);
            if (Obj == null)
                throw new Exception("Cannot find: " + PropertyName + " in ArrayBiomass: " + My.Name);

            if (Obj is IEnumerable)
            {
                foreach (object Value in Obj as IEnumerable)
                {
                    Values[i] = Convert.ToDouble(Value);
                    i++;
                }
            }
            else
            {
                Values[i] = Convert.ToDouble(Obj);
                i++;
            }

                
        }
        return Values;
    }



}
