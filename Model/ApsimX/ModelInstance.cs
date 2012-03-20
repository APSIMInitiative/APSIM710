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
    public static string Title;
    private string _FullName;
    private List<string> ComponentOrder = null;


    /// <summary>
    /// Create instances of all objects specified by the XmlNode. Returns the top 
    /// level instance.
    /// </summary>
    public static ModelInstance CreateModelInstance(XmlNode Node)
    {
        ModelInstance RootInstance = new ModelInstance(Node, null, Assembly.GetExecutingAssembly());
        RootInstance.SortChildren();
        RootInstance.Initialise();
        return RootInstance;
    }

    private int CompareModelInstances(ModelInstance x, ModelInstance y)
    {
        int xIndex = StringManip.IndexOfCaseInsensitive(ComponentOrder, x.ClassType.Name);
        int yIndex = StringManip.IndexOfCaseInsensitive(ComponentOrder, y.ClassType.Name);
        if (xIndex == yIndex)
            return 0;
        if (xIndex == -1)
            return 1;
        if (yIndex == -1)
            return -1;
        if (xIndex < yIndex)
            return -1;
        else
            return 1;
    }

    /// <summary>
    /// Sort all child instances according to the proper component order.
    /// </summary>
    private void SortChildren()
    {
        if (ComponentOrder == null)
            ComponentOrder = Configuration.Instance.ComponentOrder();

        Children.Sort(CompareModelInstances);
        foreach (ModelInstance Child in Children)
            Child.SortChildren();
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
            // Root element - register an "OnTick" handler and a "Title" output variable.
            MethodInfo OnTickMethod = GetType().GetMethod("OnTick");
            Subscribers.Add(new EventSubscriber(null, OnTickMethod, this));
            FieldInfo TitleFieldInfo = GetType().GetField("Title");
            Outputs.Add(new FieldVariable(TitleFieldInfo, TheModel));

            _FullName = "";
            Title = XmlHelper.Name(_Node);
        }
        Node = _Node;
        ClassType = GetClassType(Node.Name, Assembly);
        if (ClassType == null)
            throw new Exception("Cannot find a model class called: " + Node.Name);

        Parent = ParentInstance;
        TheModel = Activator.CreateInstance(ClassType);
        CollectVariablesAndEvents();

        // If this model doesn't have a [Param] of type XmlNode then go through all children
        // and assume that those that aren't a [Param] must be a nested model.
        if (!ModelHasXMLParam())
        {
            foreach (XmlNode Child in XmlHelper.ChildNodes(Node, ""))
            {
                bool Enabled = XmlHelper.Attribute(Child, "enabled") != "no";

                // If the model has a [Param] with the same name as this child xml node
                // then don't go and try create a nested simulation object for this xml node.

                if (!(Child is XmlWhitespace) && !ModelHasParam(Child.Name) && Enabled)
                    TryCreateModelInstance(Child);
            }
        }
    }

    /// <summary>
    /// Try and create model instance(s) for the specified XmlNode. Two ways that this method
    /// works. The first (and simplest) is that it uses the Node.Name as a class name and tries to
    /// locate that in the currently executing assembly. If this works then it will create the model instance
    /// and it to the children collection.
    /// If the first method fails then it will use the Node.Name as a class name, try and find an XML file with
    /// that class name, open it, look for a AtModelRunTime node and then follow the "instructions" inside the
    /// node.
    /// </summary>
    private void TryCreateModelInstance(XmlNode ModelParameterisation)
    {
        string XmlFileName = Path.Combine(Configuration.ApsimBinDirectory(), ModelParameterisation.Name + "X.xml");
        if (File.Exists(XmlFileName))
        {
            XmlDocument XmlDoc = new XmlDocument();
            XmlDoc.Load(XmlFileName);
            XmlNode RunTimeInstructions = XmlHelper.Find(XmlDoc.DocumentElement, "MetaData/RunTimeInstructions");
            if (RunTimeInstructions == null)
                throw new Exception("Cannot determine how to create an instance of class: " + ModelParameterisation.Name);
            ProcessInstructions(RunTimeInstructions, ModelParameterisation);
        }
        else
        {
            Assembly CurrentAssembly = Assembly.GetExecutingAssembly();
            Type ModelClassType = GetClassType(ModelParameterisation.Name, CurrentAssembly);
            if (ModelClassType != null)
                Children.Add(new ModelInstance(ModelParameterisation, this, CurrentAssembly));
        }
    }

    private XmlNode ProcessInstructions(XmlNode InstructionNode, XmlNode ModelParameterisation)
    {
        XmlNode ModelDescription = null;
        foreach (XmlNode Instruction in InstructionNode.ChildNodes)
        {
            if (Instruction.Name == "InstantiateModel")
            {
                if (ModelDescription != null)
                {
                    // This must be the second model instantiation - add the previous one to "Children"
                    CreateNewModelInstance(ModelParameterisation, ModelDescription);
                }
                ModelDescription = Instruction;
            }
            else if (Instruction.Name == "Override")
            {
                Override(ModelDescription, Instruction);
            }
            else if (Instruction.Name == "OverrideIf")
            {
                string Condition = XmlHelper.Attribute(Instruction, "condition");
                if (!Condition.Contains("="))
                    throw new Exception("Invalid <overrideif> condition: " + Condition);
                int PosEquals = Condition.IndexOf('=');
                string VariableName = Condition.Substring(0, PosEquals).Replace(".", "/");
                string VariableValue = Condition.Substring(PosEquals + 1);
                XmlNode VariableNode = XmlHelper.Find(ModelDescription, VariableName);
                if (VariableNode == null)
                    throw new Exception("Cannot find referenced node in <overrideif>: " + VariableName);
                if (VariableNode.InnerText.ToLower() == VariableValue.ToLower())
                    Override(ModelDescription, Instruction);
            }
        }
        CreateNewModelInstance(ModelParameterisation, ModelDescription);
        return ModelDescription;
    }

    private void CreateNewModelInstance(XmlNode ModelParameterisation, XmlNode ModelDescription)
    {
        string InnerContents = ModelDescription.InnerXml;
        if (ModelParameterisation.Name == "soil")
            InnerContents = Soil.ReplaceSoilMacros(ModelParameterisation, ModelDescription.InnerXml);
        ApsimFile.Macro M = new Macro();
        InnerContents = M.Go(ModelParameterisation, InnerContents);

        ModelDescription.InnerXml = InnerContents;

        string ClassName = XmlHelper.Attribute(ModelDescription, "ClassName");
        if (ClassName == "")
            throw new Exception("Cannot find a ClassName attribute on InstantiateModel XML element: " + ModelDescription.Name);
        ModelDescription = XmlHelper.ChangeType(ModelDescription, ClassName);
        string AssemblyFileName = XmlHelper.Attribute(ModelDescription, "Assembly");
        if (AssemblyFileName == "")
            throw new Exception("Cannot find an assembly attribute on InstantiateModel XML element: " + ModelDescription.Name);
        AssemblyFileName = Path.Combine(Configuration.ApsimBinDirectory(), AssemblyFileName);
        Assembly ModelAssembly = Assembly.LoadFile(AssemblyFileName);
        if (ModelAssembly == null)
            throw new Exception("Cannot find assembly: " + AssemblyFileName);
        Children.Add(new ModelInstance(ModelDescription, this, ModelAssembly));
    }

    /// <summary>
    /// Perform an override on the specified modeldescription using the specified instruction.
    /// </summary>
    private static void Override(XmlNode ModelDescription, XmlNode Instruction)
    {
        String ReferencedNodeName = XmlHelper.Attribute(Instruction, "NodeToOverride").Replace(".", "/");
        XmlNode ReferencedNode = ModelDescription;
        if (ReferencedNodeName != "")
            ReferencedNode = XmlHelper.Find(ModelDescription, ReferencedNodeName);
        if (ReferencedNode == null)
            throw new Exception("Cannot find referenced node: " + ReferencedNodeName);
        foreach (XmlNode NodeToOverride in Instruction.ChildNodes)
        {
            XmlNode NodeToRemove = XmlHelper.Find(ReferencedNode, XmlHelper.Name(NodeToOverride));
            if (NodeToRemove == null)
                throw new Exception("Cannot override node: " + XmlHelper.Name(NodeToOverride));
            XmlNode NewNode = ReferencedNode.OwnerDocument.ImportNode(NodeToOverride, true);
            ReferencedNode.InsertAfter(NewNode, NodeToRemove);
            ReferencedNode.RemoveChild(NodeToRemove);
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

    public void OnTick(TimeType t)
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

    /// <summary>
    /// Returns true if this model has a [Param] with the specified name.
    /// </summary>
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
                    {
                        Input i = (Input)Attribute;
                        Var.IsOptional = i.IsOptional;
                        Inputs.Add(Var);
                    }
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
                            {
                                if (ClassType.Assembly.GetType(Field.FieldType.Name) != null)
                                {
                                    // we have found a nested param - treat it as a model.
                                    ModelInstance NewModelInstance = new ModelInstance(ParamNode, this, ClassType.Assembly);
                                    Children.Add(NewModelInstance);
                                    Var.Value = NewModelInstance.TheModel;
                                }
                                else
                                {
                                    ParamValue = ParamNode.InnerXml;
                                    if (ParamValue == null)
                                    {
                                        if (!p.IsOptional)
                                            throw new Exception("Cannot find a parameter value for: " + Field.Name + " for " + Name);
                                    }
                                    else
                                        Var.Value = TypeConverter.Convert<string>(ParamValue, Var.Type);
                                }
                            }
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
                    {
                        Input i = (Input)Attribute;
                        Var.IsOptional = i.IsOptional;
                        Inputs.Add(Var);
                    }
                    else if (Attribute is Output)
                        Outputs.Add(Var);
                    else
                        States.Add(Var);

                    // If this is a param then go find a value for it.
                    if (Attribute is Param)
                    {
                        Param p = (Param)Attribute;
                        string ParamValue = XmlHelper.Value(Node, Property.Name);
                        if (ParamValue == "")
                        {
                            if (!p.IsOptional)
                                throw new Exception("Cannot find a parameter value for: " + Property.Name);
                        }
                        else
                            Var.Value = TypeConverter.Convert<string>(ParamValue, Var.Type);
                        Params.Add(Var);
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
                {
                    EventHandler Handler = (EventHandler)Attribute;
                    Subscribers.Add(new EventSubscriber(Handler.EventName, EventHandler, TheModel));
                }
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
                string AssemblyFileName;
                if (ClassName == "surfaceom")
                    AssemblyFileName = "SurfaceOrganicMatter";
                else
                    AssemblyFileName = ClassName;
                AssemblyFileName = Path.Combine(Configuration.ApsimBinDirectory(), AssemblyFileName + "X.dll");
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
                Output = TypeConverter.CreateConverterIfNecessary(Output, Input.Type);
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
            if (!Var.IsConnected && !Var.IsOptional)
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
            Data = (T[]) TypeConverter.CreateConverterIfNecessary(V, typeof(T[])).Value;
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
            if (V.Type.IsArray)
            {
                string st = "";
                Array a = (Array) V.Value;
                foreach (object o in a)
                    st += "   " + o.ToString();
                Data = st;
            }
            else
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


    internal bool Set<T>(string NamePath, T Data)
    {
        VariableBase V = FindOutput(NamePath);
        if (V != null)
        {
            V.Value = Data;
            return true;
        }
        else
            return false;
    }
    internal bool Set<T>(string NamePath, T[] Data)
    {
        VariableBase V = FindOutput(NamePath);
        if (V != null)
        {
            V.Value = TypeConverter.Convert(Data, V.Type);
            return true;
        }
        else
            return false;
    }
}
