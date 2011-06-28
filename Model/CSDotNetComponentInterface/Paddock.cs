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
    public new String Name
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
    public Component ComponentByType(String TypeToFind)
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
    /// <summary>
    /// Returns a reference to a variable.
    /// <param name="VariableName"></param>
    /// </summary>
    public override Variable Variable(String VariableName)
    {
        return new Variable(HostComponent, VariableName);
    }
    //=========================================================================
    /// <summary>
    /// Publish a notification event i.e. one that doesn't have any data 
    /// associated with it. This event is broadcast to all components within scope.
    /// </summary>
    //=========================================================================
    public override void Publish(String EventName)
    {
        HostComponent.Publish(EventName, null);
    }
    //=========================================================================
    /// <summary>
    /// Publish an event that has associated data. This event is broadcast to all components within scope.
    /// </summary>
    /// <param name="EventName"></param>
    /// <param name="Data"></param>
    //=========================================================================
    public override void Publish(String EventName, ApsimType Data)
    {
        HostComponent.Publish(EventName, Data);
    }
    //=========================================================================
    /// <summary>
    /// Create a component of the specified name
    /// </summary>
    /// <param name="ComponentName"></param>
    /// <returns></returns>
    //=========================================================================
    public Component CreateComponent(String ComponentName)
    {
        return new Component(ComponentName, HostComponent);
    }
    // --------------------------------------------------------------------
    /// <summary>
    /// Return a list of all child crops to caller.
    /// </summary>
    // --------------------------------------------------------------------
    public List<Component> Crops
    {
        get
        {
            List<Component> Children = new List<Component>();
            foreach (KeyValuePair<uint, String> pair in HostComponent.SiblingComponents)
            {
                Component ChildComponent = new Component(pair.Value, HostComponent);
                if (ChildComponent.IsCrop() || ChildComponent.IsOfType("Plant") || ChildComponent.IsOfType("Plant2"))
                    Children.Add(ChildComponent);
            }
            return Children; 
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