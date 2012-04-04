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

    Biomass ReturnBiomass = new Biomass();

    class CompositeInfo
    {
        public object Target;
        public MemberInfo Member;

        public object Value
        {
            get
            {
                if (Member is FieldInfo)
                    return (Member as FieldInfo).GetValue(Target);
                else
                    return (Member as PropertyInfo).GetValue(Target, null);
            }
        }
        
        public string ArrayFieldName = null;
        public string ArraySpecifier = "";
    }
    private List<CompositeInfo> Infos = new List<CompositeInfo>();

    /// <summary>
    ///  Cannot use the OnInitialised event handler because our parent plant hasn't yet been initialised.
    /// </summary>
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
                object Target;
                MemberInfo Member;
                Plant.GetMemberInfo(PropertyNameMinusArray, Plant, out Member, out Target);
                if (Member == null)
                    throw new Exception("In ComponentBiomass, cannot find property: " + PropertyName);
                Infos.Add(new CompositeInfo { Target = Target, 
                                              Member = Member,
                                              ArrayFieldName = ArrayFieldName, 
                                              ArraySpecifier = ArraySpecifier });
            }
        }
        return Plant.SowingData != null && Infos.Count > 0;
    }

    private Biomass CompositeBiomassObject
    {
        get
        {
            ReturnBiomass.Clear();
            if (HaveBeenInitialised())
            {
                foreach (CompositeInfo I in Infos)
                {
                    object Value = I.Value;
                    if (Value is Array)
                    {
                        foreach (Biomass b in Value as Array)
                            AddToReturnBiomass(b);
                    }
                    else if (Value is Biomass)
                        AddToReturnBiomass(Value as Biomass);
                    else if (Value is IList)
                    {
                        // Could be a list of any type of object.
                        if (I.ArrayFieldName != null)
                        {
                            foreach (object Obj in Value as IList)
                            {
                                if (I.ArraySpecifier == "" || Utility.IsOfType(Obj.GetType(), I.ArraySpecifier))
                                {
                                    object MemberValue = Plant.GetValueOfMember(I.ArrayFieldName, Obj);
                                    if (MemberValue == null)
                                        throw new Exception("Cannot find field: " + I.ArrayFieldName + " in type: " + Obj.GetType().Name);

                                    AddToReturnBiomass(MemberValue as Biomass);
                                }
                            }
                        }
                        else
                        {
                            foreach (Biomass b in Value as IList)
                                AddToReturnBiomass(b);
                        }


                    }

                }
            }
            return ReturnBiomass;
        }
    }

    private void AddToReturnBiomass(Biomass a)
    {
        ReturnBiomass.StructuralWt += a.StructuralWt;
        ReturnBiomass.NonStructuralWt += a.NonStructuralWt;
        ReturnBiomass.MetabolicWt += a.MetabolicWt;
        ReturnBiomass.StructuralN += a.StructuralN;
        ReturnBiomass.NonStructuralN += a.NonStructuralN;
        ReturnBiomass.MetabolicN += a.MetabolicN;
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
 