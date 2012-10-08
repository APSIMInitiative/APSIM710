using System;
using System.Collections.Generic;
using System.Text;
using System.Collections;
using System.Reflection;
using CSGeneral;
using ModelFramework;


public class CompositeBiomass : Biomass
{
    [Link]
    Component My = null;

    [Param]
    private string[] Propertys = null;

    /// <summary>
    ///  Update this biomass object.
    /// </summary>
    public void Update()
    {
        base.Clear();

        foreach (string PropertyName in Propertys)
        {
            object v = Util.GetVariable(PropertyName, My);
            if (v == null)
                throw new Exception("Cannot find: " + PropertyName + " in composite biomass: " + My.Name);

            if (v is IEnumerable)
            {
                foreach (object i in v as IEnumerable)
                {
                    if (!(i is Biomass))
                        throw new Exception("Elements in the array: " + PropertyName + " are not Biomass objects in composition biomass: " + My.Name);
                    Add(i as Biomass);
                }
            }
            else
            {

                if (!(v is Biomass))
                    throw new Exception("Property: " + PropertyName + " is not a Biomass object in composition biomass: " + My.Name);
                Add(v as Biomass);
            }
        }
    }

    [Output]
    [Units("g/m^2")]
    override public double NonStructuralN
    {
        get { Update();  return base.NonStructuralN; }
        set { throw new Exception("Cannot set NonStructuralN in CompositeBiomass"); }
    }
    [Output]
    [Units("g/m^2")]
    override public double StructuralN
    {
        get { Update(); return base.StructuralN; }
        set { throw new Exception("Cannot set StructuralN in CompositeBiomass"); }
    }
    [Output]
    [Units("g/m^2")]
    override public double NonStructuralWt
    {
        get { Update(); return base.NonStructuralWt; }
        set { throw new Exception("Cannot set NonStructuralWt in CompositeBiomass"); }
    }
    [Output]
    [Units("g/m^2")]
    override public double StructuralWt
    {
        get { Update(); return base.StructuralWt; }
        set { throw new Exception("Cannot set StructuralWt in CompositeBiomass"); }
    }
    [Output]
    [Units("g/m^2")]
    override public double MetabolicN
    {
        get { Update(); return base.MetabolicN; }
        set { throw new Exception("Cannot set MetabolicN in CompositeBiomass"); }
    }
    [Output]
    [Units("g/m^2")]
    override public double MetabolicWt
    {
        get { Update(); return base.MetabolicWt; }
        set { throw new Exception("Cannot set MetabolicWt in CompositeBiomass"); }
    }

    override public void Clear()
    {
        // This is called in OnCut - for now do nothing.
    }



}
 