using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Runtime.InteropServices;

/// <summary>
/// --------------------------------------------------------------------
/// Returns the singleton instance of a reflection class that is
/// capable of returning metadata about the structure of the simulation.
/// --------------------------------------------------------------------
/// </summary>
public class Component : TypedItem
{
    protected String TypeName;
    protected ApsimComponent HostComponent;
    
    protected String ParentCompName;            //Name of the parent component in the simulation
    protected String CompName;                  //Name of the actual component
    // --------------------------------------------------------------------
    /// <summary>
    /// 
    /// </summary>
    /// <param name="TypeNameToMatch"></param>
    /// <returns></returns>
    // --------------------------------------------------------------------
    public override bool IsOfType(String TypeNameToMatch)
    {
        return TypeName.ToLower() == TypeNameToMatch.ToLower();
    }
    public bool IsCrop()
    {
        // Turn the fully qualified name (e.g. .MasterPM.Paddock1.wheat) into a
        // crop name (e.g. wheat)
        String CropName = CompName;
        int PosLastPeriod = CropName.LastIndexOf('.');
        if (PosLastPeriod != -1)
            CropName = CropName.Substring(PosLastPeriod + 1);
        if (Types.Instance.IsCrop(CropName))
            return true;
        else
            return Types.Instance.IsCrop(TypeName);
    }
    // --------------------------------------------------------------------
    /// <summary>
    /// Constructor
    /// </summary>
    /// <param name="In">Instance of a root/leaf/shoot/phenology</param>
    // --------------------------------------------------------------------
    public Component(Instance In)
    {
        //get the name of the owner component of the Instance (e.g. Plant2)
        CompName = In.ParentComponent().GetName();
        ParentCompName = "";
        if (CompName.LastIndexOf('.') > -1)
            ParentCompName = CompName.Substring(0, CompName.LastIndexOf('.')); //e.g. Paddock
        //get the name of the host component for the calling object
        HostComponent = In.ParentComponent();   //e.g. Plant2
        TypeName = HostComponent.CompClass;     //type of Plant2
    }
    // --------------------------------------------------------------------
    /// <summary>
    /// Constructor
    /// </summary>
    /// <param name="Nam">Name of the actual component</param>
    /// <param name="component">The apsim component that hosts this object</param>
    // --------------------------------------------------------------------
    public Component(String Nam, ApsimComponent component)
    {
        CompName = Nam;
        ParentCompName = "";
        if (CompName.LastIndexOf('.') > -1)
            ParentCompName = CompName.Substring(0, CompName.LastIndexOf('.'));
        HostComponent = component;
        TypeName = HostComponent.CompClass;
    }
    // --------------------------------------------------------------------
    /// <summary>
    /// Return a list of all sibling components to caller.
    /// </summary>
    // --------------------------------------------------------------------
    public TypedList<Component> ComponentList
    {
        get
        {
            TypedList<Component> Children = new TypedList<Component>();
            foreach (KeyValuePair<uint, String> pair in HostComponent.SiblingComponents)
            {
                Component C = new Component(pair.Value, HostComponent);
                Children.Add(C);
            }
            return Children;
        }
    }
    // --------------------------------------------------------------------
    /// <summary>
    /// 
    /// </summary>
    /// <param name="VariableName"></param>
    /// <returns></returns>
    // --------------------------------------------------------------------
    public VariableType Variable(String VariableName)
    {
        return new VariableType(HostComponent, CompName, VariableName);
    }
    /// <summary>
    /// 
    /// </summary>
    /// <param name="EventName"></param>
    /// <param name="Data"></param>
    public virtual void Publish(String EventName, ApsimType Data)
    {
        HostComponent.Publish(CompName + "." + EventName, Data);
    }
    /// <summary>
    /// 
    /// </summary>
    /// <param name="EventName"></param>
    /// <param name="F"></param>
    public virtual void Subscribe(String EventName, RuntimeEventHandler.NullFunction F)
    {
        RuntimeEventHandler Event = new RuntimeEventHandler(EventName, F);
        HostComponent.Subscribe(Event);
    }
}

