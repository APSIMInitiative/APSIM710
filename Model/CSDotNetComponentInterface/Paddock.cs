using System;
using System.Text;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Runtime.InteropServices;
using ModelFramework;
using CMPServices;

public class Paddock : Component
{
    public Dictionary<uint, TComp> ChildComponents;  
    // --------------------------------------------------------------------
    /// <summary>
    /// Encapsulates an APSIM paddock in a simulation.
    /// </summary>
    /// <param name="Nam">Name of the paddock or system</param>
    /// <param name="component">The hosting component</param>
    // --------------------------------------------------------------------
    public Paddock(String Nam, ApsimComponent component)
        : base(Nam, component)
    {
        ChildComponents = new Dictionary<uint, TComp>();
        queryChildComponents(Nam);
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
            return base.Name(); //name of the component (Paddock)
        }
    }
    // --------------------------------------------------------------------
    /// <summary>
    /// 
    /// </summary>
    /// <param name="FQN"></param>
    /// <returns></returns>
    // --------------------------------------------------------------------
    protected int queryChildComponents(String FQN)
    {
        String sSearchName = FQN + ".*";    //search comp.*

        List<TComp> comps = new List<TComp>();
        HostComponent.Host.queryCompInfo(sSearchName, TypeSpec.KIND_COMPONENT, ref comps);
        ChildComponents.Clear();
        for (int i = 0; i < comps.Count; i++)
        {
            ChildComponents.Add(comps[i].compID, comps[i]);
        }
        return ChildComponents.Count;
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
            foreach (KeyValuePair<uint, TComp> pair in ChildComponents)
            {
                if (pair.Value.CompClass.ToLower() == "paddock" || pair.Value.CompClass.ToLower() == "protocolmanager")
                {
                    Paddock C = new Paddock(pair.Value.name, HostComponent);
                    Children.Add(C);
                }
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
        Component C = null;
        TypedList<Component> Children = new TypedList<Component>();
        foreach (KeyValuePair<uint, TComp> pair in ChildComponents)
        {
            if (pair.Value.CompClass.ToLower() == TypeToFind.ToLower())
               C = new Component(pair.Value.name, HostComponent);
        }
        return C;
    }
    //=========================================================================
    /// <summary>
    /// Return a child component of the paddock by unqualified name.
    /// </summary>
    /// <param name="NameToFind">Unqualified name</param>
    /// <returns></returns>
    //=========================================================================
    public Component ComponentByName(String NameToFind)
    {
        Component C = null;
        foreach (KeyValuePair<uint, TComp> pair in ChildComponents) 
        {
            if (TRegistrar.unQualifiedName(pair.Value.name).ToLower() == NameToFind.ToLower())
            {
                C = new Component(pair.Value.name, HostComponent);
                return C;
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
            foreach (KeyValuePair<uint, TComp> pair in ChildComponents) 
            {
                Component ChildComponent = new Component(pair.Value.name, HostComponent);
                if (ChildComponent.IsOfType("Plant") || ChildComponent.IsOfType("Plant2"))
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
