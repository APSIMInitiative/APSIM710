using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using System.Collections.Specialized;
using System.IO;

using CMPServices;
using ModelFramework;
using CSGeneral;
using ApsimFile;

/// <summary>
/// Represents a [Link]. Main responsability is to 
/// resolve the linkages at runtime of a simulation.
/// Links can be to APSIM components (which are by default matched using type only) or
/// they can be links to Instance objects (which are by default matched using type ane name).
/// e.g. 
///    APSIM linkages:
///    [Link] Paddock MyPaddock;                      // links to the current paddock (owner).
///    [Link] Component MyComponent;                  // links to the current component.
///    [Link] SoilWat MySoil;                         // links to the component in scope that has the type 'SoilWat'
///    [Link("wheat2")] Wheat W;                      // links to the sibling component named 'wheat2'
/// !!!!to be tested!!!!!   [Link(".simulation.paddock1.wheat2")] Wheat W; // links to a specific component with the full path '.simulation.paddock1.wheat2'
/// e.g. 
///    Instance linkages (cannot specify a link path for instance linkages):
///    [Link] Function ThermalTime;        // links to an object with type Function and name ThermalTime
///    [Link] Leaf Leaf;                   // links to an object with type Leaf and name Leaf
/// <summary>
public class LinkField
{
    private FieldInfo Field;
    private Link LinkAttr;
    private Instance In;
    ApsimComponent Comp;
    static Assembly ProbeInfo;

    #region public
    //----------------------------------------------------------------------
    /// <summary>
    /// 
    /// </summary>
    /// <param name="_In"></param>
    /// <param name="_Field"></param>
    /// <param name="_LinkAttr"></param>
    //----------------------------------------------------------------------
    public LinkField(Instance _In, FieldInfo _Field, Link _LinkAttr)
    {
        In = _In;
        Field = _Field;
        LinkAttr = _LinkAttr;
        Comp = In.ParentComponent();
    }
    /// <summary>
    /// Creates any classes that are of type [Link] in the model
    /// </summary>
    public void Resolve()
    {
        if (Field.GetValue(In) == null)
        {
            Object ReferencedObject = null;

            // Load in the probe info assembly.
            //ProbeInfo = Assembly.LoadFile(Path.Combine(Configuration.ApsimDirectory(), "Model", "CSDotNetProxies.dll"));
            ProbeInfo = Assembly.LoadFile(Path.Combine(Configuration.ApsimDirectory(), "Model", "DotNetProxies.dll"));

            String TypeToFind = Field.FieldType.Name;
            String NameToFind;
            if (LinkAttr._Path == null)
                NameToFind = Field.Name;
            else
                NameToFind = LinkAttr._Path;

            if (IsAPSIMType(TypeToFind))
            {
                if (LinkAttr._Path == null)
                    NameToFind = null; // default is not to use name.
                string SystemName = Comp.Name.Substring(0, Math.Max(Comp.Name.LastIndexOf('.'), 0));
                ReferencedObject = FindApsimObject(TypeToFind, NameToFind, SystemName, Comp);
            }
            else
            {
                // Link is an Instance link - use name and type to find the object.
                ReferencedObject = FindInstanceObject(NameToFind, TypeToFind);
            }

            // Set the value of the [Link] field to the newly created object.
            if (ReferencedObject == null)
            {
                if (LinkAttr._IsOptional != IsOptional.Yes)
                    throw new Exception("Cannot find [Link] for type: " + TypeToFind + " " + NameToFind + " in object: " + In.Name);
            }
            else
                Field.SetValue(In, ReferencedObject);
        }

    }
    //-------------------------------------------------------------------------
    /// <summary>
    /// Return a object that matches the specified type and name.
    /// <param name="TypeToFind">The type to find. [Type.]ProxyClass. Would be the paddock type when
    /// when the NameToFind is expected to be a child of the paddock.</param>
    /// <param name="NameToFind">Name of the component. Can be null. </param>
    /// <param name="SystemName"></param>
    /// <param name="Comp"></param>
    /// </summary>
    //-------------------------------------------------------------------------
    public static Object FindApsimObject(String TypeToFind, String NameToFind, string SystemName, ApsimComponent Comp)
    {
        Object ReferencedObject;
        if (NameToFind != null && NameToFind.Contains("."))
            ReferencedObject = GetSpecificApsimComponent(NameToFind, Comp);
        else
            ReferencedObject = FindApsimComponent(NameToFind, TypeToFind, SystemName, Comp);
        return ReferencedObject;
    }
    #endregion
    //----------------------------------------------------------------------
    /// <summary>
    /// Search for an Instance object in scope that has the specified name and type.
    /// Returns null if not found.
    /// <param name="NameToFind"></param>
    /// <param name="TypeToFind">The type to find. [Type.]ProxyClass.</param>
    /// </summary>
    //----------------------------------------------------------------------
    private Object FindInstanceObject(String NameToFind, String TypeToFind)
    {
        // Check our children first.
        foreach (NamedItem Child in In.Children)
        {
            if (NameToFind.ToLower() == Child.Name.ToLower() && Utility.IsOfType(Child.GetType(), TypeToFind))
                return Child;
        }

        // Check our siblings, our parent's siblings and our parent's, parent's siblings etc.
        Instance Parent = In.Parent;
        while (Parent != null)
        {
            if (Parent.Name.ToLower() == TypeToFind.ToLower())
                return Parent;
            foreach (NamedItem Sibling in Parent.Children)
            {
                if (NameToFind.ToLower() == Sibling.Name.ToLower() && Utility.IsOfType(Sibling.GetType(), TypeToFind))
                    return Sibling;
            }
            Parent = Parent.Parent;
        }
        return null;
    }

    #region protected
    //----------------------------------------------------------------------
    /// <summary>
    /// Returns true if the specified typename is an APSIM type.
    /// <param name="TypeToFind">The type to find. [Type.]ProxyClass.</param>
    /// </summary>
    //----------------------------------------------------------------------
    protected bool IsAPSIMType(String TypeToFind)
    {
        if (TypeToFind == "Paddock" || TypeToFind == "Component")
            return true;
        else
            return ProbeInfo.GetType("ModelFramework." + TypeToFind) != null;  
    }
    //----------------------------------------------------------------------
    /// <summary>
    /// Create a DotNetProxy class to represent an Apsim component.
    /// <param name="TypeToFind">The type to find. [Type.]ProxyClass.</param>
    /// <param name="FQN"></param>
    /// <param name="Comp"></param>
    /// </summary>
    //----------------------------------------------------------------------
    protected static Object CreateDotNetProxy(String TypeToFind, String FQN, ApsimComponent Comp)
    {
        Type ProxyType;
        if (TypeToFind == "Paddock" || TypeToFind == "Component")
            ProxyType = Assembly.GetExecutingAssembly().GetType("ModelFramework." + TypeToFind);
        else
            ProxyType = ProbeInfo.GetType("ModelFramework." + TypeToFind);
        if (ProxyType == null)
            throw new Exception("Cannot find proxy reference: " + TypeToFind);
        Object[] Parameters = new Object[2];
        Parameters[0] = FQN;
        Parameters[1] = Comp;
        return Activator.CreateInstance(ProxyType, Parameters);
    }
    //----------------------------------------------------------------------
    /// <summary>
    /// Go find an Apsim component IN SCOPE that matches the specified name and type.
    /// IN SCOPE means a component that is a sibling (or in the system above ?????)
    /// </summary>
    /// <param name="NameToFind">Name of the component.</param>
    /// <param name="TypeToFind">The type to find. [Type.]ProxyClass</param>
    /// <param name="SystemName"></param>
    /// <param name="Comp"></param>
    //----------------------------------------------------------------------
    protected static Object FindApsimComponent(String NameToFind, String TypeToFind, string SystemName, ApsimComponent Comp)
    {
        if ((TypeToFind == "Paddock") && (NameToFind == null) && (SystemName.Length > 0)) //if the parent is a paddock/system
            return CreateDotNetProxy(TypeToFind, SystemName, Comp);
        if (TypeToFind == "Component" && NameToFind == null)
            return CreateDotNetProxy(TypeToFind, Comp.Name, Comp);

        //Get a list of siblings.
        String sSearchName = SystemName + ".*";    //search parent.*
        List<TComp> comps = new List<TComp>();
        Comp.Host.queryCompInfo(sSearchName, TypeSpec.KIND_COMPONENT, ref comps);

        //using sibling components find the one to create.
        String SiblingShortName = "";

        // The TypeToFind passed in as [Type.]DotNetProxy . We need to convert this to a Component Type
        // e.g. TypeToFind = Plant2.Lucerne2, Component Type = Plant2, ProxyClass = Lucerne2
        // Query DotNetProxies for metadata about the TypeToFind class. Find [ComponentType()] attribute 
        String compClass = "";
        if (TypeToFind != null)
        {
            Type ProxyType;
            ProxyType = ProbeInfo.GetType("ModelFramework." + unQualifiedName(TypeToFind));
            if (ProxyType != null)
            {
                Attribute[] attribs = Attribute.GetCustomAttributes(ProxyType);
                foreach (Attribute attrib in attribs)
                {
                    if (attrib.GetType() == typeof(ComponentTypeAttribute))
                        compClass = ((ComponentTypeAttribute)attrib).ComponentClass;
                    if (compClass.ToLower() != unQualifiedName(TypeToFind).ToLower())
                        compClass += "." + unQualifiedName(TypeToFind);                //e.g Plant2.Lucerne2
                }
            }
            else
            {
                //this TypeToFind may have been Plant2 which means we search through the components
                //to get the full Type.DotNetProxy type
                foreach (TComp pair in comps)
                {
                    int firstPeriod = pair.CompClass.IndexOf('.');
                    if (firstPeriod == -1)
                        firstPeriod = pair.CompClass.Length;
                    String SiblingType = pair.CompClass.Substring(0, firstPeriod);
                    if (SiblingType.ToLower() == TypeToFind.ToLower())
                    {
                        return CreateDotNetProxy(unQualifiedName(pair.CompClass), pair.name, Comp);
                    }
                }
            }
            //for each sibling of this component
            foreach (TComp pair in comps)
            {
                String SiblingType = pair.CompClass;
                if (SiblingType.ToLower() == compClass.ToLower())
                {
                    SiblingShortName = unQualifiedName(pair.name).ToLower();
                    if (NameToFind == null || NameToFind.ToLower() == SiblingShortName)
                        return CreateDotNetProxy(unQualifiedName(TypeToFind), pair.name, Comp);
                }
            }
        }
        else
        {
            //TypeToFind==null but the name is used. So then we can use the compClass 
            //that was found for the component using queryInfo before.
            foreach (TComp pair in comps)                                   //for each sibling of this component
            {
                SiblingShortName = unQualifiedName(pair.name).ToLower();
                if (NameToFind.ToLower() == SiblingShortName)
                {
                    compClass = unQualifiedName(pair.CompClass);
                    if (compClass.Length > 0)
                    {
                        //find the proxytype for this component class 
                        return CreateDotNetProxy(compClass, pair.name, Comp);
                    }
                }
            }
        }

        // If we get this far then we didn't find the APSIM component.
        return null;
    }
    //============================================================================
    /// <summary>
    /// Extracts the last section of a FQN
    /// </summary>
    /// <param name="sFQN">Fully qualified name.</param>
    /// <returns>The unqualified name.</returns>
    //============================================================================
    public static string unQualifiedName(string sFQN)
    {
        int lastDot = sFQN.LastIndexOf(".");
        return sFQN.Substring(lastDot + 1);
    }
    //----------------------------------------------------------------------
    /// <summary>
    /// Go find a component with the specified name. NameToFind can be either
    /// a relative or absolute address.
    /// </summary>
    //----------------------------------------------------------------------
    protected static Object GetSpecificApsimComponent(String NameToFind, ApsimComponent Comp)
    {
        if (NameToFind.Contains(".MasterPM."))
            return CreateDotNetProxy(NameToFind, NameToFind, Comp);   // absolute reference.
        else
        {
            // relative reference.
            String OurName = Comp.GetName();
            String ParentName = "";
            int PosLastPeriod = OurName.LastIndexOf('.');
            if (PosLastPeriod == -1)
                throw new Exception("Invalid component name found: " + OurName);
            ParentName = OurName.Substring(0, PosLastPeriod);
            return CreateDotNetProxy(NameToFind, ParentName + "." + NameToFind, Comp);
        }
    }
    #endregion
}






 

