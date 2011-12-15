using System;
using System.Collections.Generic;
using System.Text;
using ModelFramework;
using VBMet;
using CSGeneral;
using System.Reflection;
using System.Collections;

[Model]
public class Plant
{
    [Link(IsOptional=true)]
    Phenology Phenology = null;

    [Link(IsOptional=true)]
    Arbitrator Arbitrator = null;

    [Link]
    Component My = null;

    public string Name { get { return My.Name; } }


    private List<Organ> _Organs = new List<Organ>();
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
            {
                if (o.WaterDemand > 0)
                    o.WaterAllocation = fraction * o.WaterDemand;
            }

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
            {
                if (o.WaterDemand > 0)
                    o.WaterAllocation = fraction * o.WaterDemand;
            }

            //throw new Exception("Cannot talk to swim3 yet");
        }
    }
    private void DoArbitrator()
    {
        if (Arbitrator != null)
        {
            Arbitrator.DoDMSetup(Organs);
            Arbitrator.DoPotentialDMAllocation(Organs);
            Arbitrator.DoNutrientSetup(Organs);
            Arbitrator.DoNutrientReAllocation(Organs);
            Arbitrator.DoNutrientUptake(Organs);
            Arbitrator.DoNutrientFixation(Organs);
            Arbitrator.DoNutrientRetranslocation(Organs);
            Arbitrator.DoActualDMAllocation(Organs);
            Arbitrator.DoNutrientAllocation(Organs);
            Arbitrator.DoDM(Organs);
        }
    }
    public void DoActualGrowth()
    {
        foreach (Organ o in Organs)
            o.DoActualGrowth();
    }
    public object GetPlantVariable(string VariablePath)
    {
        double ValueFromGet;
        if (!My.Get(VariablePath, out ValueFromGet) ||
            ValueFromGet == Double.NaN)
            return null;
        return ValueFromGet;

    }

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
            return Convert.ToDouble(ExpressionFunction.Evaluate(this, PropertyName));
        else
        {
            object ArrayObject = My.LinkByName(PropertyName.Substring(0, PosBracket));
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
                
                else if (ArraySpecifier.Contains("."))
                {
                    object I = GetPlantVariable(ArraySpecifier);
                    if (I != null)
                    {
                        int Index = Convert.ToInt32(I);
                        if (Index < 0 || Index >= Array.Count)
                            throw new Exception("Invalid index of " + Index.ToString() + " found while indexing into variable: " + PropertyName);
                        return Convert.ToDouble(GetValueOfField(RemainderOfPropertyName, Array[Index]));
                    }
                }
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
    [EventHandler]
    public void OnSow(SowPlant2Type Sow)
    {
        // Go through all our children and find all organs.
        foreach (object ChildObject in My.ChildrenAsObjects)
        {
            Organ Child = ChildObject as Organ;
            if (Child != null)
                Organs.Add((Organ)Child);
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
    }
    [EventHandler]
    public void OnProcess()
    {
        DoPhenology();
        DoPotentialGrowth();
        DoWater();
        DoArbitrator();
        DoActualGrowth();
    }
    [EventHandler]
    private void OnEndCrop()
    {
        NewCropType Crop = new NewCropType();
        Crop.crop_type = CropType;
        Crop.sender = Name;
        CropEnding.Invoke(Crop);
    }
    [EventHandler]
    private void OnCut()
    {
        Cutting.Invoke();
    }
 #endregion

}
   
