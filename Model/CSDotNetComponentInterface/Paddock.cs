using System;
using System.Text;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Runtime.InteropServices;

public class Paddock : Component
{
    // --------------------------------------------------------------------
    /// <summary>
    /// Encapsulates an APSIM paddock in a simulation.
    /// </summary>
    /// <param name="Nam">Name of the hosting component (a member of a paddock e.g. Plant2) </param>
    /// <param name="component">The component itself</param>
    // --------------------------------------------------------------------
    public Paddock(String Nam, ApsimComponent component)
        : base(Nam, component)
    {
    }
    // --------------------------------------------------------------------
    /// <summary>
    /// 
    /// </summary>
    /// <param name="In">Instance of a root/leaf/shoot/phenology</param>
    // --------------------------------------------------------------------
    public Paddock(Instance In)
        : base(In)
    {
        if (Types.Instance.TypeNames.Length == 0)
            PlugIns.LoadAll();
    }
    public String Name
    {
        get
        {
            return base.ParentCompName; //name of the component (Paddock)
        }
    }
    // --------------------------------------------------------------------
    /// <summary>
    /// Return a list of all child paddock components to caller.
    /// </summary>
    // --------------------------------------------------------------------
    public TypedMultiList<Paddock> SubPaddocks
    {
        get
        {
            TypedMultiList<Paddock> Children = new TypedMultiList<Paddock>();
            foreach (KeyValuePair<uint, String> pair in HostComponent.SiblingComponents)
            {
                Paddock C = new Paddock(pair.Value, HostComponent);
                if (C.IsOfType("paddock") || C.IsOfType("ProtocolManager"))
                    Children.Add(C);
            }
            return Children;
        }
    }
    //=========================================================================
    /// <summary>
    /// Returns a component that is a child of the paddock
    /// </summary>
    //=========================================================================
    public Component Component(String TypeToFind)
    {
        TypedList<Component> Children = new TypedList<Component>();
        foreach (KeyValuePair<uint, String> pair in HostComponent.SiblingComponents)
        {
            Component C = new Component(pair.Value, HostComponent); //??
            if (C.IsOfType(TypeToFind))
                return C;
        }
        return null;
    }
    //=========================================================================
    /// <summary>
    /// Return a child component of the paddock by name.
    /// </summary>
    /// <param name="NameToFind"></param>
    /// <returns></returns>
    //=========================================================================
    public Component ComponentByName(String NameToFind)
    {
        Component C = null;
        foreach (KeyValuePair<uint, String> pair in HostComponent.SiblingComponents)
        {
            if (pair.Value.ToLower() == NameToFind.ToLower())
            {
                C = new Component(pair.Value, HostComponent);   //??
            }
        }
        return C;
    }
    //=========================================================================
    /// <summary>
    /// 
    /// </summary>
    /// <param name="EventName"></param>
    /// <param name="Data"></param>
    //=========================================================================
    public override void Publish(String EventName, ApsimType Data)
    {
        HostComponent.Publish(EventName, Data);
    }
    /// <summary>
    /// Return the SoilWaterType representing the soilwat component in the paddock
    /// </summary>
    public SoilWat SoilWater
    {
        get
        {
            Component SoilToReturn;
            SoilToReturn = Component("soilwat");
            if (SoilToReturn == null)
                SoilToReturn = Component("swim2");

            if (SoilToReturn != null)
                return new SoilWat(SoilToReturn);
            return null;
        }
    }
    public SoilNitrogenType SoilNitrogen
    {
        get
        {
            Component SoilToReturn;
            SoilToReturn = Component("soiln");
            if (SoilToReturn != null)
                return new SoilNitrogenType(SoilToReturn);
            return null;
        }
    }
    // --------------------------------------------------------------------
    /// <summary>
    /// Return a list of all child crops to caller.
    /// </summary>
    // --------------------------------------------------------------------
    public List<CropType> Crops
    {
        get
        {
            List<CropType> Children = new List<CropType>();
            foreach (KeyValuePair<uint, String> pair in HostComponent.SiblingComponents)
            {
                Component ChildComponent = new Component(pair.Value, HostComponent);
                if (ChildComponent.IsCrop() || ChildComponent.IsOfType("Plant") || ChildComponent.IsOfType("Plant2")) 
                    Children.Add(new CropType(ChildComponent));
            }
            return Children; 
        }
    }
    // --------------------------------------------------------------------
    /// <summary>
    /// 
    /// </summary>
    // --------------------------------------------------------------------
    public FertiliserType Fertiliser
    {
        get
        {
            Component C = Component("fertiliser");
            if (C != null)
                return new FertiliserType(C);
            else
                return null;
        }
    }
    // --------------------------------------------------------------------
    /// <summary>
    /// 
    /// </summary>
    // --------------------------------------------------------------------
    public IrrigationType Irrigation
    {
        get
        {
            Component C = Component("irrigation");
            if (C != null)
                return new IrrigationType(C);
            else
                return null;
        }
    }
}

/// <summary>
/// 
/// </summary>
public class MyPaddock
{
    private static Paddock _Singleton = null;
    public Paddock this[String ComponentName]
    {
        get
        {
            if (_Singleton == null)
                _Singleton = new Paddock(ComponentName, null);
            return _Singleton;
        }
    }
}