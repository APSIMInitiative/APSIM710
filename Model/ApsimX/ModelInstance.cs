using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using System.Reflection;
using CSGeneral;
using ModelFramework;
using System.IO;
using ApsimFile;

/// <summary>
/// Encapculates an instance of a model (e.g. wheat, sorghum, manager).
/// </summary>
internal class ModelInstance
{
    public VariableBase ModelAPI;
    public List<ModelInstance> Children = new List<ModelInstance>();
    public List<VariableBase> Inputs = new List<VariableBase>();
    public List<VariableBase> Outputs = new List<VariableBase>();
    public List<VariableBase> Params = new List<VariableBase>();
    public List<VariableBase> States = new List<VariableBase>();
    public List<LinkField> Refs = new List<LinkField>();
    public List<EventPublisher> Publishers = new List<EventPublisher>();
    public List<EventSubscriber> Subscribers = new List<EventSubscriber>();

    public Type ClassType;
    public XmlNode Node;
    public ModelInstance Parent;
    public object TheModel;
    private string _FullName;


    /// <summary>
    /// Create instances of all objects specified by the XmlNode. Returns the top 
    /// level instance.
    /// </summary>
    public static ModelInstance CreateModelInstance(XmlNode Node)
    {
        ModelInstance RootInstance = new ModelInstance(Node, null, Assembly.GetExecutingAssembly());
        RootInstance.Initialise();
        return RootInstance;
    }

    /// <summary>
    /// Internal recursive constructor to create instances of all objects specified by the 
    /// XmlNode. The parent instance is also passed in. Returns the top level instance.
    /// </summary>
    internal ModelInstance(XmlNode _Node, ModelInstance ParentInstance, Assembly Assembly)
    {
        if (ParentInstance != null)
            _FullName = ParentInstance.FullName + "." + XmlHelper.Name(_Node);
        else
        {
            // Root element.
            MethodInfo OnTickMethod = GetType().GetMethod("OnTick");
            Subscribers.Add(new EventSubscriber(OnTickMethod, this));
            _FullName = "";

        }

        Node = _Node;
        ClassType = GetClassType(Node.Name, Assembly);

        if (ClassType == null)
            throw new Exception("Cannot find a model class called: " + Node.Name);

        Parent = ParentInstance;
        TheModel = Activator.CreateInstance(ClassType);
        CollectVariablesAndEvents();

        if (!ModelHasXMLParam())
        {
            foreach (XmlNode Child in XmlHelper.ChildNodes(Node, ""))
            {
                // If the model has a [Param] with the same name as this child xml node
                // then don't go and try create a nested simulation object for this xml node.
                if (!ModelHasParam(Child.Name) && Child.Name.ToLower() != "summaryfile")
                    Children.Add(new ModelInstance(Child, this, Assembly));
            }
        }
    }

    /// <summary>
    /// Initialise this model instance by resolving all variable, events and links.
    /// </summary>
    internal void Initialise()
    {
        ConnectInputsAndOutputs();
        ConnectEvents();
        ResolveRefs();
        CheckAllInputs();
        UpdateValues();
    }


    /// <summary>
    /// Property to return a unqualified name.
    /// </summary>
    public string Name
    {
        get
        {
            int PosPeriod = _FullName.LastIndexOf('.');
            if (PosPeriod != -1)
                return _FullName.Substring(PosPeriod + 1);
            else
                return _FullName;
        }
    }

    /// <summary>
    /// Property to return a fully qualified name.
    /// </summary>
    public string FullName
    {
        get
        {
            return _FullName;
        }
    }

    public ModelInstance ParentPaddock
    {
        get
        {
            ModelInstance P = Parent;
            while (P != null && P.ClassType.Name != "Area" && P.ClassType.Name != "Simulation")
                P = P.Parent;
            return P;
        }
    }


    /// <summary>
    /// Resolve all [Ref] pointers.
    /// </summary>
    public void ResolveRefs()
    {
        foreach (LinkField Ref in Refs)
        {
            object ModelReference = FindModel(Ref);
            if (ModelReference == null)
            {
                if (!Ref.Optional)
                    throw new Exception("Cannot find ref: " + Ref.Name);
            }
            else
            {
                if (ModelReference.GetType().ToString().Contains("Generic.List"))
                    Ref.Values = ModelReference;
                else
                    Ref.Value = ModelReference;
            }
        }
        foreach (ModelInstance Child in Children)
            Child.ResolveRefs();
    }

    public ModelInstance Root
    {
        get
        {
            ModelInstance I = this;
            while (I.Parent != null)
                I = I.Parent;
            return I;
        }
    }
    /// <summary>
    /// Go find a model instance using the specified NameToFind. This name can
    /// have path information. Returns null if not found.
    ///   A leading . indicates an absolute address. e.g. .simulation.paddock1.soilwat
    ///   No dot indicates a child.                  e.g. leaf 
    /// </summary>
    public ModelInstance FindModelInstance(string NameToFind)
    {
        ModelInstance I = this;

        string[] Paths = NameToFind.Split(".".ToCharArray());
        int PathIndex = 0;
        while (PathIndex < Paths.Length)
        {
            string Path = Paths[PathIndex];
            if (Path == "")
            {
                // Must be a reference to the root node.
                I = Root;
            }

            else
            {
                // Find a child node.
                int ChildIndex = 0;
                while (ChildIndex < I.Children.Count && I.Children[ChildIndex].Name.ToLower() != Path.ToLower())
                    ChildIndex++;
                if (ChildIndex == I.Children.Count)
                    return null;
                I = I.Children[ChildIndex];
            }

            PathIndex++;
        }
        return I;
    }

    public object FindModel(LinkField Link)
    {
        if (Link.Info.FieldType.Name == "Paddock")
            return new Paddock(ParentPaddock);

        if (Link.Info.FieldType.Name == "Component" && Link.Name == "My")
            return new ModelFramework.Component(this);

        if (Link.Name == "*")
            return Children;

        ModelInstance I = FindModelInstance(Link.Name);
        if (I == null)
            return I;
        else
            return I.TheModel;
    }
    public object FindModel(string NameToFind)
    {
        if (NameToFind == "*")
            return Children;

        ModelInstance I = FindModelInstance(NameToFind);
        if (I == null && !NameToFind.Contains(".") && Parent != null)
        {
            // No dot was specified so try our parent.
            return Parent.FindModel(NameToFind);
        }
        if (I != null)
            return I.TheModel;
        else
            return null;
    }

    public object FindModelByType(string TypeToFind)
    {
        foreach (ModelInstance Child in Children)
        {
            if (Child.ClassType.ToString().ToLower() == TypeToFind.ToLower())
                return Child.TheModel;
        }

        // Not found - try our parent.
        if (Parent != null)
            return Parent.FindModelByType(TypeToFind);
        else
            return null;
    }

    /// <summary>
    /// Find a specific output that matches "NameToFind". This method will search the
    /// instance passed in plus all child instances.
    /// </summary>

    public VariableBase FindOutput(string NameToFind)
    {
        if (NameToFind.Contains("."))
        {
            // absolute address.
            int PosLastPeriod = NameToFind.LastIndexOf('.');
            ModelInstance I = FindModelInstance(NameToFind.Substring(0, PosLastPeriod));
            if (I == null)
                throw new Exception("Cannot find output variable: " + NameToFind);
            return I.FindOutput(NameToFind.Substring(PosLastPeriod + 1));
        }
        else
        {
            Predicate<VariableBase> equals = V => V.Name.ToLower() == NameToFind.ToLower();

            // See if we have the output
            VariableBase V1 = Outputs.Find(equals);
            if (V1 != null)
                return V1;

            // If we get this far, then we haven't found it - check our children.
            foreach (ModelInstance Child in Children)
            {
                V1 = Child.Outputs.Find(equals);
                if (V1 != null)
                    return V1;
            }

            // If we get this far, then we haven't found the output so go check our parent.
            if (Parent == null)
                return null;
            else
                return Parent.FindOutput(NameToFind);
        }
    }

    /// <summary>
    /// Update all [Input] values for this model instance and all child instances.
    /// </summary>
    public void UpdateValues()
    {
        foreach (VariableBase Input in Inputs)
            Input.UpdateValue();
        foreach (ModelInstance Child in Children)
            Child.UpdateValues();
    }

    public void OnTick()
    {
        UpdateValues();

    }

    internal void GetAllOutputNames(ref List<string> VariableNames)
    {
        foreach (VariableBase Output in Outputs)
            VariableNames.Add(Name + "." + Output.Name);
        foreach (ModelInstance Child in Children)
            Child.GetAllOutputNames(ref VariableNames);
    }

    internal void GetAllOutputValues(ref List<object> Values)
    {
        foreach (VariableBase Output in Outputs)
            Values.Add(Output.Value);
        foreach (ModelInstance Child in Children)
            Child.GetAllOutputValues(ref Values);
    }

    internal bool ModelHasParam(string ParamName)
    {
        foreach (VariableBase Param in Params)
        {
            if (Param.Name.ToLower() == ParamName.ToLower())
                return true;
        }
        return false;
    }

    internal bool ModelHasXMLParam()
    {
        foreach (VariableBase Param in Params)
        {
            if (Param is FieldVariable && (Param as FieldVariable).Value is XmlNode)
                return true;
        }
        return false;
    }



    /// <summary>
    /// Add a new model based on the model description passed in and the assembly. This will replace this 
    /// instance.
    /// </summary>
    internal void AddModel(XmlNode ModelDescription, Assembly ModelAssembly)
    {

        Children.Clear();
        Inputs.Clear();
        Outputs.Clear();
        Params.Clear();
        States.Clear();
        Refs.Clear();
        Publishers.Clear();
        Subscribers.Clear();

        Node = ModelDescription;
        ClassType = GetClassType(Node.Name, ModelAssembly);
        if (ClassType == null)
            throw new Exception("Cannot find a model class called: " + Node.Name);

        TheModel = Activator.CreateInstance(ClassType);
        CollectVariablesAndEvents();

        if (!ModelHasXMLParam())
        {
            foreach (XmlNode Child in XmlHelper.ChildNodes(Node, ""))
            {
                // If the model has a [Param] with the same name as this child xml node
                // then don't go and try create a nested simulation object for this xml node.
                if (!ModelHasParam(Child.Name) && Child.Name.ToLower() != "summaryfile")
                    Children.Add(new ModelInstance(Child, this, ModelAssembly));
            }
        }

        Initialise();

        // See if the newly created instance has an Initialised event handler. If so then call it.
        foreach (EventSubscriber Subscriber in Subscribers)
        {
            if (Subscriber.Name == "Initialised")
                Subscriber.Invoke();
        }

    }

    /// <summary>
    /// Collect all variables and events from the specified instance and all child instances.
    /// </summary>
    private void CollectVariablesAndEvents()
    {
        // Collection all fields.
        foreach (FieldInfo Field in GetAllFields(TheModel.GetType(), BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic))
        {
            foreach (Attribute Attribute in Field.GetCustomAttributes(false))
            {
                if (Attribute is Link)
                {
                    Link LinkAttribute = (Link)Attribute;
                    if (LinkAttribute.NamePath == null)
                        LinkAttribute.NamePath = Field.Name;
                    Refs.Add(new LinkField(TheModel, LinkAttribute.NamePath, Field, LinkAttribute.IsOptional));
                }
                else
                {
                    FieldVariable Var = new FieldVariable(Field, TheModel);
                    if (Var.Type.ToString() == "ModelAPIInterface")
                        ModelAPI = Var;
                    else if (Attribute is Input)
                        Inputs.Add(Var);
                    else if (Attribute is Output)
                        Outputs.Add(Var);
                    else
                        States.Add(Var);

                    // If this is a param then go find a value for it.
                    if (Attribute is Param)
                    {
                        // Get the parameter name taking any alias' into account.
                        string ParamName = Field.Name;
                        Param p = (Param)Attribute;
                        if (p.Alias != null)
                            ParamName = p.Alias;
                        Var.Name = ParamName;

                        // Get the parameter value.
                        if (ParamName.ToLower() == "name")
                            Var.Value = Name;
                        else if (Field.FieldType.Name == "XmlNode")
                            Var.Value = Node;
                        else
                        {
                            XmlNode ParamNode = XmlHelper.Find(Node, ParamName);
                            string ParamValue = null;
                            if (ParamNode != null)
                                ParamValue = ParamNode.InnerXml;
                            if (ParamValue == null)
                                throw new Exception("Cannot find a parameter value for: " + Field.Name + " for " + Name);
                            VariableBase ConvertedVariable = TypeConverter.CreateConverterIfNecessary(new StringVariable(ParamName, ParamValue), Var);
                            Var.Value = ConvertedVariable.Value;
                        }
                        Params.Add(Var);
                    }
                }
            }
        }
        // Collect all properties.
        foreach (PropertyInfo Property in TheModel.GetType().GetProperties(BindingFlags.FlattenHierarchy | BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic))
        {
            foreach (object Attribute in Property.GetCustomAttributes(false))
            {
                if (Attribute is Link)
                {
                }
                else
                {
                    PropertyVariable Var = new PropertyVariable(Property, TheModel);
                    if (Attribute is Input)
                        Inputs.Add(Var);
                    else if (Attribute is Output)
                        Outputs.Add(Var);
                    else
                        States.Add(Var);

                    // If this is a param then go find a value for it.
                    if (Attribute is Param)
                    {
                        string ParamValue = XmlHelper.Value(Node, Property.Name);
                        if (ParamValue == "")
                            throw new Exception("Cannot find a parameter value for: " + Property.Name);

                        VariableBase ConvertedVariable = TypeConverter.CreateConverterIfNecessary(new StringVariable(Property.Name, ParamValue), Var);
                        Var.Value = ConvertedVariable.Value;
                    }
                }
            }
        }
        // Look for all events in the model
        foreach (EventInfo Event in TheModel.GetType().GetEvents(BindingFlags.Instance | BindingFlags.Public))
            Publishers.Add(new EventPublisher(Event, TheModel));

        // Look for all event handlers in the model
        foreach (MethodInfo EventHandler in TheModel.GetType().GetMethods(BindingFlags.Instance | BindingFlags.Public))
        {
            foreach (object Attribute in EventHandler.GetCustomAttributes(false))
                if (Attribute.ToString() == "EventHandler")
                    Subscribers.Add(new EventSubscriber(EventHandler, TheModel));
        }

        // Now go and do the same for all child models.
        foreach (ModelInstance Child in Children)
        {
            Child.CollectVariablesAndEvents();
        }
    }
    
    /// <summary>
    /// Go find and return the specified ClassName either in the current executing asssembly or
    /// in a dll of the same name as the class.
    /// </summary>
    private static Type GetClassType(string ClassName, Assembly Assembly)
    {
        int PosPeriod = ClassName.IndexOf('.');
        if (PosPeriod == -1)
        {
            Type T = Assembly.GetType(ClassName, false, true);
            if (T == null)
            {
                string AssemblyFileName = Path.Combine(Configuration.ApsimBinDirectory(), ClassName + "X.dll");
                if (File.Exists(AssemblyFileName))
                {
                    Assembly Dll = Assembly.LoadFile(AssemblyFileName);
                    return Dll.GetType(ClassName, false, true);
                }
            }
            return T;
        }
        else
        {
            // This is used in unit tests.
            string AssemblyFileName = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location) + "\\" + ClassName.Substring(0, PosPeriod) + ".dll";
            if (!File.Exists(AssemblyFileName))
                throw new Exception("Cannot find assembly file: " + AssemblyFileName);
            Assembly Dll = Assembly.LoadFile(AssemblyFileName);
            return Dll.GetType(ClassName.Substring(PosPeriod + 1));
        }
    }
    
    /// <summary>
    /// Return all fields. The normal .NET reflection doesn't return private fields in base classes.
    /// This function does.
    /// </summary>
    private static List<FieldInfo> GetAllFields(Type type, BindingFlags flags)
    {
        if (type == typeof(Object)) return new List<FieldInfo>();

        var list = GetAllFields(type.BaseType, flags);
        // in order to avoid duplicates, force BindingFlags.DeclaredOnly
        list.AddRange(type.GetFields(flags | BindingFlags.DeclaredOnly));
        return list;
    }

    /// <summary>
    /// Connect all inputs to outputs for the specified instance and all child instances.
    /// </summary>
    private void ConnectInputsAndOutputs()
    {
        foreach (VariableBase Input in Inputs)
        {
            VariableBase Output = FindOutput(Input.Name);
            if (Output != null)
            {
                Output = TypeConverter.CreateConverterIfNecessary(Output, Input);
                Input.ConnectTo(Output);
            }
        }
        foreach (ModelInstance Child in Children)
        {
            Child.ConnectInputsAndOutputs();
        }
    }

    /// <summary>
    /// Connect all inputs to outputs for the specified instance and all child instances.
    /// </summary>
    private void ConnectEvents()
    {
        foreach (EventSubscriber Subscriber in Subscribers)
        {
            EventPublisher Publisher = Root.FindEventPublisher(Subscriber.Name);
            if (Publisher != null)
            {
                Publisher.ConnectTo(Subscriber);
            }
        }
        foreach (ModelInstance Child in Children)
        {
            Child.ConnectEvents();
        }
    }

    /// <summary>
    /// Recurse through all model instances and find an event publisher with the specified name.
    /// Returns null if not found.
    /// </summary>
    private EventPublisher FindEventPublisher(string EventName)
    {
        foreach (EventPublisher Publisher in Publishers)
        {
            if (Publisher.Name.ToLower() == EventName.ToLower())
                return Publisher;
        }

        foreach (ModelInstance Child in Children)
        {
            EventPublisher Publisher = Child.FindEventPublisher(EventName);
            if (Publisher != null)
                return Publisher;
        }
        return null;
    }
    
    /// <summary>
    /// Check to make sure all inptus are connected to an output.
    /// </summary>
    private void CheckAllInputs()
    {
        foreach (VariableBase Var in Inputs)
        {
            if (!Var.IsConnected)
                throw new Exception("Cannot find an input value for: " + Var.Name + " in " + Name);
        }
        foreach (ModelInstance Child in Children)
            Child.CheckAllInputs();
    }


    /// <summary>
    /// Return a list of crops.
    /// </summary>
    public List<ModelInstance> Crops
    {
        get
        {
            // Perhaps to implement this, it should use reflection to interogate the class to see if it
            // is a crop.
            return new List<ModelInstance>();
        }
    }

    internal bool Get<T>(string NamePath, out T Data) where T: new()
    {
        VariableBase V = FindOutput(NamePath);
        if (V != null)
        {
            Data = (T)Convert.ChangeType(V.Value, typeof(T));
            return true;
        }
        else
        {
            Data = new T();
            return false;
        }
    }

    internal bool Get<T>(string NamePath, out T[] Data) where T : new()
    {
        VariableBase V = FindOutput(NamePath);
        if (V != null)
        {
            Data = (T[])Convert.ChangeType(V.Value, typeof(T));
            return true;
        }
        else
        {
            Data = null;
            return false;
        }
    }

    internal bool Get(string NamePath, out string Data)
    {
        VariableBase V = FindOutput(NamePath);
        if (V != null)
        {
            Data = V.Value.ToString();
            return true;
        }
        else
        {
            Data = "";
            return false;
        }
    }

    internal bool Get(string NamePath, out string[] Data)
    {
        VariableBase V = FindOutput(NamePath);
        if (V != null)
        {
            Data = (string[]) new ToStringArray(V).Value;
            return true;
        }
        else
        {
            Data = null;
            return false;
        }
    }
}
