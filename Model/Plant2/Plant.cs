using System;
using System.Collections.Generic;
using System.Text;
using ModelFramework;
using VBMet;
using CSGeneral;
using System.Reflection;
using System.Collections;


public class Plant
{
    [Link(IsOptional=true)]
    Phenology Phenology = null;

    [Link(IsOptional=true)]
    Arbitrator Arbitrator = null;

    [Link(IsOptional = true)]
    Structure Structure = null;

    [Link]
    Component My = null;

    public string Name { get { return My.Name; } }
    public SowPlant2Type SowingData;

    private List<Organ> _Organs = new List<Organ>();
    [Output]
    public List<Organ> Organs
    {
        // Public property to return our organs to caller. Used primarily for unit testing.
        get { return _Organs; }
    }
    
    [Input(IsOptional = true)]
    Single swim3 = 0;

 #region Outputs
    [Output("Crop_Type")]
    [Param]
    public string CropType = "";
    [Output]
    private double WaterSupplyDemandRatio = 0;
    [Output]
    public string plant_status
    {
        get
        {
            // What should be returned here?
            // The old "plant" component returned either "out", "alive"
            // How to determine "dead"?
            return "alive";
        }
    }
    [Output]
    [Units("mm")]
    private double WaterDemand   // Needed for SWIM2
    {
        get
        {
            double Demand = 0;
            foreach (Organ o in Organs)
                Demand += o.WaterDemand;
            return Demand;
        }
    }

    public string FullName
    {
        get { return My.FullName; }
    }
 #endregion

 #region Plant functions
    private void DoPhenology()
    {
        if (Phenology != null)
            Phenology.DoTimeStep();
    }
    public void DoPotentialGrowth()
    {
        if (Structure != null)
            Structure.DoPotentialGrowth();
        foreach (Organ o in Organs)
            o.DoPotentialGrowth();
    }
    private void DoWater()
    {
        if (swim3 == 0)
        {
            double Supply = 0;
            double Demand = 0;
            foreach (Organ o in Organs)
            {
                Supply += o.WaterSupply;
                Demand += o.WaterDemand;
            }

            if (Demand > 0)
                WaterSupplyDemandRatio = Supply / Demand;
            else
                WaterSupplyDemandRatio = 1;

            double fraction = 1;
            if (Demand > 0)
                fraction = Math.Min(1.0, Supply / Demand);

            foreach (Organ o in Organs)
                if (o.WaterDemand > 0)
                    o.WaterAllocation = fraction * o.WaterDemand;

            double FractionUsed = 0;
            if (Supply > 0)
                FractionUsed = Math.Min(1.0, Demand / Supply);

            foreach (Organ o in Organs)
                o.DoWaterUptake(FractionUsed * Supply);
        }
        else
        {
            double Uptake = 0;
            double Demand = 0;
            double Supply = 0;
            foreach (Organ o in Organs)
            {
                Supply += o.WaterSupply;
                Uptake += o.WaterUptake;
                Demand += o.WaterDemand;
            }
            // It is REALLY dodgy that we need to do this at all
            if (Demand > 0)
                WaterSupplyDemandRatio = Supply / Demand;
            else
                WaterSupplyDemandRatio = 1;

            double fraction = 1;
            if (Demand > 0)
                fraction = Uptake / Demand;
            if (fraction > 1.001)
                throw new Exception("Water uptake exceeds total crop demand.");

            foreach (Organ o in Organs)
                if (o.WaterDemand > 0)
                    o.WaterAllocation = fraction * o.WaterDemand;

            //throw new Exception("Cannot talk to swim3 yet");
        }
    }
    public void DoActualGrowth()
    {
        foreach (Organ o in Organs)
            o.DoActualGrowth();
    }
#endregion

 #region Low level variable finding routines
    /// <summary>
    /// Return an internal plant object. PropertyName can use dot and array notation (square brackets).
    /// e.g. Leaf.MinT
    ///      Leaf.Leaves[].Live.Wt              - sums all live weights of all objects in leaves array.
    ///      Leaf.Leaves[1].Live.               - returns the live weight of the 2nd element of the leaves array.
    ///      Leaf.Leaves[Leaf.NodeNo].Live.Wt)  - returns the live weight of the leaf as specified by Leaf.NodeNo
    /// </summary>
    public double GetObject(string PropertyName)
    {
        int PosBracket = PropertyName.IndexOf('[');
        if (PosBracket == -1)
            return Convert.ToDouble(GetValueOfVariable(PropertyName));
        else
        {
            object ArrayObject = GetValueOfVariable(PropertyName.Substring(0, PosBracket));
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
                    return Convert.ToDouble(GetValueOfMember(RemainderOfPropertyName, Array[ArrayIndex]));

                else if (ArraySpecifier.Contains("."))
                {
                    int Index;
                    if (My.Get(ArraySpecifier, out Index))
                    {
                        if (Index < 0 || Index >= Array.Count)
                            throw new Exception("Invalid index of " + Index.ToString() + " found while indexing into variable: " + PropertyName);
                        return Convert.ToDouble(GetValueOfMember(RemainderOfPropertyName, Array[Index]));
                    }
                }
                else
                {
                    double Sum = 0.0;
                    for (int i = 0; i < Array.Count; i++)
                    {
                        object Obj = GetValueOfMember(RemainderOfPropertyName, Array[i]);
                        if (Obj == null)
                            throw new Exception("Cannot evaluate: " + RemainderOfPropertyName);

                        if (ArraySpecifier == "" || Utility.IsOfType(Array[i].GetType(), ArraySpecifier))
                            Sum += Convert.ToDouble(Obj);
                    }
                    return Sum;
                }
            }
            else
                throw new Exception("Cannot find property: " + PropertyName);
        return 0;
        }
    }


    private object GetValueOfVariable(string VariableName)
    {
        
        //// Look internally
        //Info I = GetMemberInfo(VariableName, this);
        //if (I != null)
        //{
        //    VariableCache.Add(VariableName, I);
        //    return I.Value;
        //}

        // Look externally
        object Data;
        if (My.Get(VariableName, out Data))
            return Data;
        
        // Still didn't find it
        throw new Exception("Cannot find variable: " + VariableName);
    }




    internal static object GetValueOfMember(string PropertyName, object Target)
    {
        object ReturnTarget;
        MemberInfo ReturnMember;
        GetMemberInfo(PropertyName, Target, out ReturnMember, out ReturnTarget);
        if (ReturnMember is FieldInfo)
            return (ReturnMember as FieldInfo).GetValue(ReturnTarget);
        else
            return (ReturnMember as PropertyInfo).GetValue(ReturnTarget, null);
    }

    /// <summary>
    /// Return the value (using Reflection) of the specified property on the specified object.
    /// Returns null if not found.
    /// </summary>
    internal static void GetMemberInfo(string PropertyName, object Target, out MemberInfo ReturnMember, out object ReturnTarget)
    {
        FieldInfo FI;
        PropertyInfo PI;
        string[] Bits = PropertyName.Split(".".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
        for (int i = 0; i < Bits.Length - 1; i++)
        {
            // First check for organs.
            bool Found = false;
            if (Target is Plant)
            {
                foreach (Organ O in (Target as Plant).Organs)
                {
                    if (O.Name == Bits[i])
                    {
                        Found = true;
                        Target = O;
                        break;
                    }
                }
            }

            if (!Found)
            {
                FI = Target.GetType().GetField(Bits[i], BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Public);
                if (FI == null)
                {
                    PI = Target.GetType().GetProperty(Bits[i], BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Public);
                    if (PI == null)
                    {
                        Target = null;
                        break;
                    }
                    else
                        Target = PI.GetValue(Target, null);
                }
                else
                    Target = FI.GetValue(Target);
            }
        }

        if (Target == null)
            ReturnMember = null;
        else
        {
            // By now we should have a target - go get the field / property.
            FI = Target.GetType().GetField(Bits[Bits.Length - 1], BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Public);
            if (FI == null)
            {
                PI = Target.GetType().GetProperty(Bits[Bits.Length - 1], BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Public);
                if (PI == null)
                    ReturnMember = null;
                else
                    ReturnMember = PI;
            }
            else
                ReturnMember = FI;
        }
        ReturnTarget = Target;
    }

 #endregion

 #region Event handlers and publishers
    [Event]
    public event NewCropDelegate NewCrop;
    [Event]
    public event NullTypeDelegate Sowing;
    [Event]
    public event NullTypeDelegate Cutting;
    [Event]
    public event NewCropDelegate CropEnding;
    [Event]
    public event BiomassRemovedDelegate BiomassRemoved;

    [EventHandler]
    public void OnSow(SowPlant2Type Sow)
    {
        SowingData = Sow;

        // Go through all our children and find all organs.
        Organs.Clear();
        foreach (object ChildObject in My.ChildrenAsObjects)
        {
            Organ Child = ChildObject as Organ;
            if (Child != null)
                Organs.Add(Child);
        }

        if (NewCrop != null)
        {
            NewCropType Crop = new NewCropType();
            Crop.crop_type = CropType;
            Crop.sender = Name;
            NewCrop.Invoke(Crop);
        } 
        
        if (Sowing != null)
            Sowing.Invoke();

        // tell all our children about sow
        foreach (Organ Child in Organs)
            Child.OnSow(Sow);
    }
    [EventHandler]
    public void OnProcess()
    {
        DoPhenology();
        DoPotentialGrowth();
        DoWater();
        if (Arbitrator != null)
            Arbitrator.DoArbitrator(Organs);
        DoActualGrowth();
    }
    [EventHandler]
    public void OnHarvest()
    {
        // tell all our children about sow
        foreach (Organ Child in Organs)
            Child.OnHarvest();
    }
    [EventHandler]
    public void OnEndCrop()
    {
        NewCropType Crop = new NewCropType();
        Crop.crop_type = CropType;
        Crop.sender = Name;
        if (CropEnding != null)
            CropEnding.Invoke(Crop);

        BiomassRemovedType BiomassRemovedData = new BiomassRemovedType();
        BiomassRemovedData.crop_type = CropType;
        BiomassRemovedData.dm_type = new string[Organs.Count];
        BiomassRemovedData.dlt_crop_dm = new float[Organs.Count];
        BiomassRemovedData.dlt_dm_n = new float[Organs.Count];
        BiomassRemovedData.dlt_dm_p = new float[Organs.Count];
        BiomassRemovedData.fraction_to_residue = new float[Organs.Count];
        int i = 0;
        foreach (Organ O in Organs)
        {
            if (O is AboveGround)
            {
                BiomassRemovedData.dm_type[i] = O.Name;
                BiomassRemovedData.dlt_crop_dm[i]=(float)(O.Live.Wt + O.Dead.Wt)*10f;
                BiomassRemovedData.dlt_dm_n[i] = (float)(O.Live.N + O.Dead.N) * 10f;
                BiomassRemovedData.dlt_dm_p[i] = 0f;
                BiomassRemovedData.fraction_to_residue[i] = 1f;
            }
            else
            {
                BiomassRemovedData.dm_type[i] = O.Name;
                BiomassRemovedData.dlt_crop_dm[i] = 0f;
                BiomassRemovedData.dlt_dm_n[i] = 0f;
                BiomassRemovedData.dlt_dm_p[i] = 0f;
                BiomassRemovedData.fraction_to_residue[i] = 0f;
            }
            i++;
        }
        BiomassRemoved.Invoke(BiomassRemovedData);

        // tell all our children about sow
        foreach (Organ Child in Organs)
            Child.OnEndCrop();
    }
    [EventHandler]
    private void OnCut()
    {
        Cutting.Invoke();
    }
 #endregion

}
   
