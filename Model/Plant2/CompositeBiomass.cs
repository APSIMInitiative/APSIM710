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
                Value += Plant.GetObject(PropertyName + ".NonStructuralN");
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
                Value += Plant.GetObject(PropertyName + ".StructuralN");
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
                Value += Plant.GetObject(PropertyName + ".NonStructuralWt");
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
                Value += Plant.GetObject(PropertyName + ".StructuralWt");
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
                Value += Plant.GetObject(PropertyName + ".MetabolicN");
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
                Value += Plant.GetObject(PropertyName + ".MetabolicWt");
            return Value;
        }

        set { throw new Exception("Cannot set MetabolicWt in CompositeBiomass"); }
    }

    override public void Clear()
    {
        // This is called in OnCut - for now do nothing.
    }
}
 