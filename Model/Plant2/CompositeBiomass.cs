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


    class CompositeInfo
    {
        public Plant.Info Info;
        public string ArrayFieldName = null;
        public string ArraySpecifier = "";
        public object Value { get { return Info.Value; } }

    }
    private List<CompositeInfo> Infos = new List<CompositeInfo>();

    /// <summary>
    ///  Cannot use the OnInitialised event handler because our parent plant hasn't yet been initialised.
    /// </summary>
    [EventHandler]
    public bool HaveBeenInitialised()
    {
        if (Plant.SowingData != null && Infos.Count == 0)
        {
            foreach (string PropertyName in Propertys)
            {
                string ArrayFieldName = "", ArraySpecifier;
                string PropertyNameMinusArray = PropertyName;
                int PosRemainder = PropertyNameMinusArray.IndexOf("].");
                if (PosRemainder != -1)
                {
                    ArrayFieldName = PropertyNameMinusArray.Substring(PosRemainder + 2);
                    PropertyNameMinusArray = PropertyNameMinusArray.Remove(PosRemainder+1);
                }

                ArraySpecifier = StringManip.SplitOffBracketedValue(ref PropertyNameMinusArray, '[', ']');
                Plant.Info I = Plant.GetMemberInfo(PropertyNameMinusArray, Plant);
                if (I == null)
                    throw new Exception("In ComponentBiomass, cannot find property: " + PropertyName);
                Infos.Add(new CompositeInfo { Info = I, ArrayFieldName = ArrayFieldName, ArraySpecifier = ArraySpecifier });
            }
        }
        return Plant.SowingData != null && Infos.Count > 0;
    }

    private Biomass CompositeBiomassObject
    {
        get
        {
            Biomass ReturnBiomass = new Biomass();
            if (HaveBeenInitialised())
            {
                foreach (CompositeInfo I in Infos)
                {
                    object Value = I.Value;
                    if (Value is Array)
                    {
                        foreach (Biomass b in Value as Array)
                            ReturnBiomass += b;
                    }
                    else if (Value is Biomass)
                        ReturnBiomass += Value as Biomass;
                    else if (Value is IList)
                    {
                        // Could be a list of any type of object.
                        if (I.ArrayFieldName != null)
                        {
                            foreach (object Obj in Value as IList)
                            {
                                if (I.ArraySpecifier == "" || Utility.IsOfType(Obj.GetType(), I.ArraySpecifier))
                                {
                                    Plant.Info ObjectInfo = Plant.GetMemberInfo(I.ArrayFieldName, Obj);
                                    if (ObjectInfo == null)
                                        throw new Exception("Cannot find field: " + I.ArrayFieldName + " in type: " + Obj.GetType().Name);

                                    ReturnBiomass += ObjectInfo.Value as Biomass;
                                }
                            }
                        }
                        else
                        {
                            foreach (Biomass b in Value as IList)
                                ReturnBiomass += b;
                        }


                    }

                }
            }
            return ReturnBiomass;
        }
    }

    [Output]
    [Units("g/m^2")]
    override public double NonStructuralN
    {
        get { return CompositeBiomassObject.NonStructuralN; }
        set { throw new Exception("Cannot set NonStructuralN in CompositeBiomass"); }
    }
    [Output]
    [Units("g/m^2")]
    override public double StructuralN
    {
        get { return CompositeBiomassObject.StructuralN; }
        set { throw new Exception("Cannot set StructuralN in CompositeBiomass"); }
    }
    [Output]
    [Units("g/m^2")]
    override public double NonStructuralWt
    {
        get { return CompositeBiomassObject.NonStructuralWt; }
        set { throw new Exception("Cannot set NonStructuralWt in CompositeBiomass"); }
    }
    [Output]
    [Units("g/m^2")]
    override public double StructuralWt
    {
        get { return CompositeBiomassObject.StructuralWt; }
        set { throw new Exception("Cannot set StructuralWt in CompositeBiomass"); }
    }
    [Output]
    [Units("g/m^2")]
    override public double MetabolicN
    {
        get { return CompositeBiomassObject.MetabolicN; }
        set { throw new Exception("Cannot set MetabolicN in CompositeBiomass"); }
    }
    [Output]
    [Units("g/m^2")]
    override public double MetabolicWt
    {
        get { return CompositeBiomassObject.MetabolicWt; }
        set { throw new Exception("Cannot set MetabolicWt in CompositeBiomass"); }
    }

    override public void Clear()
    {
        // This is called in OnCut - for now do nothing.
    }




}
 