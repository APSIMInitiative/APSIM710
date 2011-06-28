using System;
using System.Collections.Generic;
using System.Reflection;
using System.Xml;
using CSGeneral;

// --------------------------------------------------------------------
/// <summary>
/// 
/// </summary>
// --------------------------------------------------------------------
public class Factory
{
    private Instance _Root;
    private List<FactoryProperty> RegisteredProperties;
    private List<EvntHandler> RegisteredEventHandlers;
    private List<FactoryEvent> RegisteredEvents;
    private List<LinkField> Links;
    private Assembly CallingAssembly;
    // --------------------------------------------------------------------
    /// <summary>
    /// 
    /// </summary>
    // --------------------------------------------------------------------
    public Factory()
    {
        RegisteredProperties = new List<FactoryProperty>();
        RegisteredEventHandlers = new List<EvntHandler>();
        RegisteredEvents = new List<FactoryEvent>();
        Links = new List<LinkField>();
    }
    public Instance Root
    {
        get { return _Root; }
    }
    public List<FactoryProperty> Properties
    {
        get { return RegisteredProperties; }
    }
    public List<EvntHandler> EventHandlers
    {
        get { return RegisteredEventHandlers; }
    }
    public List<FactoryEvent> Events
    {
        get { return RegisteredEvents; }
    }
    // --------------------------------------------------------------------
    /// <summary>
    /// Clear all the current settings for this Factory.
    /// </summary>
    // --------------------------------------------------------------------
    public void Clear()
    {
        RegisteredProperties.Clear();
        RegisteredEventHandlers.Clear();
        RegisteredEvents.Clear();
    }
    // --------------------------------------------------------------------
    /// <summary>
    /// Create instances (and populate their fields and properies of all 
    /// classes as specified by the Xml passed in. The newly created root
    /// instance can be retrieved by the 'Root' property.
    /// </summary>
    /// <param name="Xml"></param>
    /// <param name="AssemblyWithTypes"></param>
    /// <param name="HostComponent"></param>
    // --------------------------------------------------------------------
    public void Create(String Xml, Assembly AssemblyWithTypes, ApsimComponent ParentComponent)
    {
        CallingAssembly = AssemblyWithTypes;
        XmlDocument Doc = new XmlDocument();
        Doc.LoadXml(Xml);
        RemoveShortCuts(Doc.DocumentElement);
        _Root = CreateInstance(Doc.DocumentElement, null, null, ParentComponent);
    }
    // --------------------------------------------------------------------
    /// <summary>
    /// Resolve all links and initialise all Instances
    /// </summary>
    // --------------------------------------------------------------------
    public void Initialise()
    {
        ResolveLinks();
        CallInitialisedOnAll(_Root);
    }
    // --------------------------------------------------------------------
    /// <summary>
    /// Create an instance of a the 'Instance' class based on the 
    /// Node type passed in information. Then populate the instance based
    /// on the child XML nodes.
    /// </summary>
    /// <param name="Node"></param>
    /// <param name="Parent"></param>
    /// <param name="ParentInstance"></param>
    /// <param name="HostComponent"></param>
    /// <returns></returns>
    // --------------------------------------------------------------------
    private Instance CreateInstance(XmlNode Node,
                                  XmlNode Parent,
                                  Instance ParentInstance,
                                  ApsimComponent ParentComponent)
    {
        Type ClassType = CallingAssembly.GetType(Node.Name);
        if (ClassType == null)
            throw new Exception("Cannot find a class called: " + Node.Name);
        Instance CreatedInstance = (Instance)(Activator.CreateInstance(ClassType));

        if (CreatedInstance.GetType().IsSubclassOf(typeof(DerivedInstance)))
        {
            ((DerivedInstance)CreatedInstance).Initialise(Node, ParentInstance, ParentComponent);
        }
        else if (CreatedInstance != null)
        {
            CreatedInstance.Initialise(XmlHelper.Name(Node), ParentInstance, ParentComponent);
            GetAllProperties(CreatedInstance, Parent);
            GetAllEventHandlers(CreatedInstance);
            GetAllEvents(CreatedInstance);
            PopulateParams(CreatedInstance, Node, ParentComponent);
        }
        else
        {
            throw new Exception("Class " + Node.Name + " must be derived from the \"Instance\" class");
        }
        
        return CreatedInstance;
    }
    // --------------------------------------------------------------------
    /// <summary>
    /// Go through all reflected fields and properties that are tagged
    /// with a 'Param' or 'Input' attribute and add them to our list
    /// of registered properties.
    /// </summary>
    /// <param name="Obj"></param>
    /// <param name="Parent"></param>
    // --------------------------------------------------------------------
    private void GetAllProperties(Instance Obj, XmlNode Parent)
    {
        foreach (FieldInfo Property in Obj.GetType().GetFields(BindingFlags.FlattenHierarchy | BindingFlags.Instance | BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic))
        {
            bool AddProperty = false;
            bool IsOutput = false;
            Object[] Attributes = Property.GetCustomAttributes(false);
            foreach (Object Attr in Attributes)
            {
                IsOutput = (IsOutput || (Attr.GetType() == typeof(Output)) );
                if (Attr.GetType() == typeof(Param))
                    AddProperty = true;
                if (Attr.GetType() == typeof(Input))
                    AddProperty = true;
                if (Attr.GetType() == typeof(Output))
                    AddProperty = true;
                if (Attr.GetType() == typeof(Link))
                {
                    LinkField LinkF = new LinkField(Obj, Property, (Link)Attr);
                    Links.Add(LinkF);
                }
            }
            if (AddProperty)
            {
                FactoryProperty NewProperty = new FactoryProperty(new ReflectedField(Property, Obj), Parent);
                if (IsOutput)
                    RemoveRegisteredOutput(NewProperty.OutputName);
                RegisteredProperties.Add(NewProperty);
            }
        }
        foreach (PropertyInfo Property in Obj.GetType().GetProperties(BindingFlags.FlattenHierarchy | BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic))
        {
            bool AddProperty = false;
            bool IsOutput = false;
            Object[] Attributes = Property.GetCustomAttributes(false);
            foreach (Object Attr in Attributes)
            {
                IsOutput = (IsOutput || (Attr.GetType() == typeof(Output)) );
                if (Attr.GetType() == typeof(Param))
                    AddProperty = true;
                if (Attr.GetType() == typeof(Input))
                    AddProperty = true;
                if (Attr.GetType() == typeof(Output))
                    AddProperty = true;
            }
            if (AddProperty)
            {
                FactoryProperty NewProperty = new FactoryProperty(new ReflectedProperty(Property, Obj), Parent);
                if (IsOutput)
                    RemoveRegisteredOutput(NewProperty.OutputName);
                RegisteredProperties.Add(NewProperty);
            }
        }
    }
    // --------------------------------------------------------------------
    /// <summary>
    /// Remove the specified [output] from the list of registered properties.
    /// Duplicates can happen when an [output] in a base class is
    /// overridden in a derived class [output]. In this case we want the last
    /// duplicate and it superseeds previous ones (base classes)
    /// </summary>
    /// <param name="OutputName"></param>
    // --------------------------------------------------------------------
    private void RemoveRegisteredOutput(String OutputName)
    {
        for (int i = 0; i != RegisteredProperties.Count; i++)
        {
            if (RegisteredProperties[i].IsOutput &&
                String.Compare(RegisteredProperties[i].OutputName, OutputName) == 0)
            {
                RegisteredProperties.RemoveAt(i);
                return;
            }
        }
    }
    // --------------------------------------------------------------------
    /// <summary>
    /// Goes through the model looking for all event handlers that start
    /// with 'On' and are marked 'EventHandler'
    /// </summary>
    /// <param name="Obj"></param>
    // --------------------------------------------------------------------
    private void GetAllEventHandlers(Instance Obj)
    {
        foreach (MethodInfo Method in Obj.GetType().GetMethods(BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic))
        {
            Object[] Attributes = Method.GetCustomAttributes(false);
            foreach (Object Attr in Attributes)
            {

                if ( (Attr.GetType() == typeof(EventHandler)) &&
                    Method.Name.Length > 2 &&
                    Method.Name.Substring(0, 2) == "On")
                    RegisteredEventHandlers.Add(new FactoryEventHandler(Method, Obj));
            }
        }
    }
    // --------------------------------------------------------------------
    /// <summary>
    /// Goes through the model looking for all events
    /// </summary>
    /// <param name="Obj"></param>
    // --------------------------------------------------------------------
    private void GetAllEvents(Instance Obj)
    {
        foreach (EventInfo Event in Obj.GetType().GetEvents(BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic))
        {
            Object[] Attributes = Event.GetCustomAttributes(false);
            foreach (Object Attr in Attributes)
            {
                if (Attr.GetType() == typeof(Event)) 
                    RegisteredEvents.Add(new FactoryEvent(Event, Obj));
            }
        }
    }
    // --------------------------------------------------------------------
    /// <summary>
    /// Go through all child XML nodes for the node passed in and set
    /// the corresponding property values in the Obj instance passed in.
    /// </summary>
    /// <param name="Obj"></param>
    /// <param name="Node"></param>
    /// <param name="HostComponent"></param>
    // --------------------------------------------------------------------
    private void PopulateParams(Instance Obj, XmlNode Node, ApsimComponent ParentComponent)
    {
        foreach (XmlNode Child in Node.ChildNodes)
        {
            if (Child.GetType() != typeof(XmlComment) ) //(XmlComment)(Child) == null)
            {
                Type t = CallingAssembly.GetType(Child.Name);
                if ((t != null) && t.IsSubclassOf(typeof(Instance)))
                {
                    // Create a child instance - indirect recursion.
                    Instance ChildInstance = CreateInstance(Child, Child, Obj, ParentComponent);
                    Obj.Add(ChildInstance);

                    // See if there is an array of the right type with the plural version of our child name
                    // e.g. if Child is LeafCohort, look for a LeafCohorts member.
                    String PluralName = CalcParentName(Child) + "s";
                    foreach (FactoryProperty Property in RegisteredProperties)
                    {
                        if (Property.FQN == PluralName)
                        {
                            // Found a list - see if it is the right type.
                            if (Property.TypeName.Contains("List"))
                            {
                                // It is the right type so add our newly created child to the list.
                                Property.AddToList(ChildInstance);
                            }
                            break;
                        }
                    }
                }
                else if (Child.Name == "Memo")
                {
                    // Ignore memo fields.
                }
                else if (!Child.HasChildNodes && Child.InnerText == "")
                    throw new Exception("Cannot have a blank value for property: " + Child.Name);
                else if (Child.HasChildNodes)
                {
                    FactoryProperty Parameter = FindProperty(Child);
                    if (Parameter == null)
                        throw new Exception("Cannot set value of property: " + Child.Name + " in object: " + Obj.InstanceName + ". The property must have either a [Param] or [Input] attribute.");
                    Parameter.Name = XmlHelper.Name(Child);
                    Parameter.Set(Child);  // set the value of the simple property.
                }
            }
        }
        // Look for an XmlNode param. If found then given it our current 'Node'.
        foreach (FactoryProperty Property in RegisteredProperties)
        {
            if (Property.TypeName == "XmlNode")
            {
                Property.Set(Node);
            }
        }
    }
    // --------------------------------------------------------------------
    /// <summary>
    /// Go through all our registered properties and look for the one that
    /// has the specified name. Returns null if not found.
    /// </summary>
    /// <param name="Child"></param>
    /// <returns></returns>
    // --------------------------------------------------------------------
    private FactoryProperty FindProperty(XmlNode Child)
    {
        String FQN = CalcParentName(Child);
        foreach (FactoryProperty Property in RegisteredProperties)
        {
            if (Property.FQN.ToLower() == FQN.ToLower())
                return Property;
        }

        // Go look for the plural version - property might be an array.
        FQN = FQN + "s";
        foreach (FactoryProperty Property in RegisteredProperties)
        {
            if (Property.FQN.ToLower() == FQN.ToLower())
                return Property;
        }
        return null;
    }
    // --------------------------------------------------------------------
    /// <summary>
    /// Check the members of the array.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    /// <param name="Property"></param>
    /// <param name="ErrorString"></param>
    // --------------------------------------------------------------------
    protected void CheckArray<T>(FactoryProperty Property, String ErrorString)
    {
        T[] arrayObj = (T[])Property.Get;
        for (int i = 0; i < arrayObj.Length; i++)
        {
            Double val = (Double)Convert.ChangeType(arrayObj[i], typeof(Double));
            if (!Double.IsNaN(Property.ParamMinVal) &&
                val < Property.ParamMinVal)
                ErrorString += "The value provided for element " + i +
                               " of parameter " + Property.FQN +
                               " is less than its allowed minimum (" +
                               val + " vs. " +
                               Property.ParamMinVal + ")\n";
            if (!Double.IsNaN(Property.ParamMaxVal) &&
                val > Property.ParamMaxVal)
                ErrorString += "The value provided for element " + i +
                                " of parameter " + Property.FQN +
                                " is greater than its allowed maximum (" +
                                val + " vs. " +
                                Property.ParamMaxVal + ")\n";
        }
    }
    // --------------------------------------------------------------------
    /// <summary>
    /// Check for parameters in the model that haven't been given a value and throw if any 
    /// are found. Also do range checking, if applicable.
    /// </summary>
    // --------------------------------------------------------------------
    public void CheckParameters()
    {
        String Errors = "";
        String RangeErrors = "";
        for (int i = 0; i != RegisteredProperties.Count; i++)
        {
            FactoryProperty Property = RegisteredProperties[i];
            if (Property.IsParam)
            {
                if (!Property.HasAsValue && !Property.OptionalParam)
                {
                    if (Errors != "")
                        Errors += ", ";
                    Errors += Property.FQN;
                }
                // Is there a tidier way to do this?
                if (Property.HasAsValue &&
                   (!Double.IsNaN(Property.ParamMinVal) ||
                    !Double.IsNaN(Property.ParamMaxVal)))
                {
                    if (Property.TypeName == "Double" ||
                        Property.TypeName == "Single" ||
                        Property.TypeName == "Int32")
                    {
                        double val = Convert.ToDouble(Property.Get.ToString());
                        if (!Double.IsNaN(Property.ParamMinVal) &&
                            val < Property.ParamMinVal)
                            RangeErrors += "The value provided for parameter " + Property.FQN +
                             " is less than its allowed minimum (" +
                             Property.Get.ToString() + " vs. " +
                             Property.ParamMinVal + ")\n";
                        if (!Double.IsNaN(Property.ParamMaxVal) &&
                            val > Property.ParamMaxVal)
                            RangeErrors += "The value provided for parameter " + Property.FQN +
                               " is greater than its allowed maximum (" +
                               Property.Get.ToString() + " vs. " +
                               Property.ParamMaxVal + ")\n";
                    }
                    else if (Property.TypeName == "Double[]")
                        CheckArray<double>(Property, RangeErrors);
                    else if (Property.TypeName == "Single[]")
                        CheckArray<float>(Property, RangeErrors);
                    else if (Property.TypeName == "Int32[]")
                        CheckArray<int>(Property, RangeErrors);
                }
            }
        }
        if (Errors != "")
            throw new Exception("The following parameters haven't been initialised: " + Errors);
        if (RangeErrors != "")
            throw new Exception("In " + Root.InstanceName + ", the following parameters are outside their allowable ranges:\n" + RangeErrors);
    }
    // --------------------------------------------------------------------
    /// <summary>
    /// Check for parameters in the model that
    /// haven't been given a value and throw if any 
    /// are found.
    /// Also do range checking, if applicable.
    /// </summary>
    // --------------------------------------------------------------------
/*    public void ThrowOnUnInitialisedParameters()
    {
        String Errors = "";
        for (int i = 0; i != RegisteredProperties.Count; i++)
        {
            FactoryProperty Property = RegisteredProperties[i];
            if (Property.IsParam && !Property.HasAsValue)
            {
                if (Errors != "")
                    Errors += ", ";
                Errors += Property.FQN;
            }
        }
        if (Errors != "")
            throw new Exception("The following parameters haven't been initialised: " + Errors);
    } */
    // --------------------------------------------------------------------      
    /// <summary>
    /// Remove any shortcut nodes in the children of
    /// the specified node.
    /// </summary>
    /// <param name="Node"></param>
    // --------------------------------------------------------------------
    private void RemoveShortCuts(XmlNode Node)
    {
        String ShortCutPath = XmlHelper.Attribute(Node, "shortcut");
        if (ShortCutPath != "")
        {
            // Shortcut strings will be a full path e.g. /FrenchBean/Model/Plant/Phenology/ThermalTime
            // But our Node->OwnerDocument->DocumentElemen is <Plant> 
            // So we need to find /Plant/ and remove everything on the path before that. This way
            // we'll end up with a relative path e.g. Plant/Phenology/ThermalTime
            int PosPlant = ShortCutPath.IndexOf("/Plant/");
            if (PosPlant == -1)
                throw new Exception("Invalid shortcut path: " + ShortCutPath);
            ShortCutPath = ShortCutPath.Remove(0, PosPlant + 7); // Get rid of the /Plant/ string.

            XmlNode ReferencedNode = XmlHelper.Find(Node.OwnerDocument.DocumentElement, ShortCutPath);
            if (ReferencedNode == null)
                throw new Exception("Cannot find short cut node: " + ShortCutPath);
            XmlNode NewNode = ReferencedNode.CloneNode(true);
            Node.ParentNode.ReplaceChild(NewNode, Node);
            if (XmlHelper.Attribute(Node, "name") != "")
                XmlHelper.SetName(NewNode, XmlHelper.Name(Node));
        }

        for (int i = 0; i < Node.ChildNodes.Count; i++)
            RemoveShortCuts(Node.ChildNodes[i]);

    }
    // --------------------------------------------------------------------
    /// <summary>
    /// Resolve all [Links].
    /// </summary>
    // --------------------------------------------------------------------
    public void ResolveLinks()
    {
        for (int i = 0; i < Links.Count; i++)
            Links[i].Resolve();
    }
    // --------------------------------------------------------------------
    /// <summary>
    /// Calculate a parent name
    /// </summary>
    /// <param name="Node"></param>
    /// <returns></returns>
    // --------------------------------------------------------------------
    private String CalcParentName(XmlNode Node)
    {
        String ParentName = "";
        if (Node == null || Node.ParentNode == null || Node.ParentNode.ParentNode == null)
            return "";

        ParentName = CalcParentName(Node.ParentNode);
        return ParentName + XmlHelper.Name(Node);
    }
    // --------------------------------------------------------------------
    /// <summary>
    /// Do a depth first walk of the Instance tree, calling each Instance's Initialised method.
    /// </summary>
    // --------------------------------------------------------------------
    private void CallInitialisedOnAll(Instance Obj)
    {
        foreach (Instance Child in Obj.Children)
            CallInitialisedOnAll(Child);

        Obj.Initialised();
    }
}


