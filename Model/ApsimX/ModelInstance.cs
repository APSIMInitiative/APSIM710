using System;
using System.Collections;
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
    public List<ModelInstance> Children = new List<ModelInstance>();
    public List<ClassVariable> Inputs = new List<ClassVariable>();
    public List<ClassVariable> Outputs = new List<ClassVariable>();
    public List<ClassVariable> Params = new List<ClassVariable>();
    public List<ClassVariable> States = new List<ClassVariable>();
    public List<LinkField> Refs = new List<LinkField>();
    public List<EventPublisher> Publishers = new List<EventPublisher>();
    public List<EventSubscriber> Subscribers = new List<EventSubscriber>();
    

    public Type ClassType;
    public XmlNode Node;
    public ModelInstance Parent;
    public object TheModel;
    public static string Title;
    public static XmlNode SoilNode;
    public static Simulation RootSimulation;
    private string _FullName;
    private List<string> ComponentOrder = null;
    private bool Enabled = true;
    private Dictionary<string, ClassVariable> VariableCache = new Dictionary<string, ClassVariable>();

    /// <summary>
    /// Create instances of all objects specified by the XmlNode. Returns the top 
    /// level instance.
    /// </summary>
    public static ModelInstance CreateModelInstance(XmlNode Node)
    {
        string InstanceName = XmlHelper.Name(Node);
        ModelInstance RootInstance = new ModelInstance(InstanceName, Node, null, Assembly.GetExecutingAssembly());
        RootInstance.SortChildren();
        RootInstance.Initialise();
        RootSimulation = (Simulation) RootInstance.TheModel;
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

        Utility.StableSort<ModelInstance>(Children, CompareModelInstances);
        foreach (ModelInstance Child in Children)
            Child.SortChildren();
    }

    /// <summary>
    /// Internal recursive constructor to create instances of all objects specified by the 
    /// XmlNode. The parent instance is also passed in. Returns the top level instance.
    /// </summary>
    internal ModelInstance(string InstanceName, XmlNode _Node, ModelInstance ParentInstance, Assembly Assembly)
    {
        if (ParentInstance != null)
        {
            if (!ParentInstance.Enabled)
                Enabled = false;
            else
                Enabled = XmlHelper.Attribute(_Node, "enabled").ToLower() != "no";

            _FullName = ParentInstance.FullName + "." + InstanceName;
        }
        else
        {
            // Root element - register an "OnTick" handler and a "Title" output variable.
            MethodInfo OnTickMethod = GetType().GetMethod("OnTick");
            Subscribers.Add(new EventSubscriber(null, OnTickMethod, this));
            FieldInfo TitleFieldInfo = GetType().GetField("Title");
            Outputs.Add(new ClassVariable(TitleFieldInfo, TheModel));

            _FullName = "";
            Title = InstanceName;
        }

        // resolve any shortcuts.
        Node = _Node;
        string ShortCutPath = XmlHelper.Attribute(Node, "shortcut");
        if (ShortCutPath != "")
        {
            Node = XmlHelper.Find(Node.OwnerDocument.DocumentElement, ShortCutPath);
            if (Node == null)
                throw new Exception("Cannot find shortcut: " + ShortCutPath);
        }
        ClassType = GetClassType(Node.Name, Assembly);
        if (ClassType == null)
            throw new Exception("Cannot find a model class called: " + Node.Name);

        Parent = ParentInstance;


        CreateInstanceOfModel();
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
    private void TryCreateModelInstance(XmlNode ModelParameterisation, Assembly CurrentAssembly)
    {
        string XmlFileName = Path.Combine(Configuration.ApsimBinDirectory(), ModelParameterisation.Name + "X.xml");
        if (File.Exists(XmlFileName))
        {
            XmlDocument XmlDoc = new XmlDocument();
            XmlDoc.Load(XmlFileName);
            XmlNode RunTimeInstructions = XmlHelper.Find(XmlDoc.DocumentElement, "Model");
            if (RunTimeInstructions == null)
                throw new Exception("Cannot determine how to create an instance of class: " + ModelParameterisation.Name);

            ProcessInstructions(RunTimeInstructions, ModelParameterisation);
        }
        else
        {
            Type ModelClassType = GetClassType(ModelParameterisation.Name, CurrentAssembly);
            if (ModelClassType != null)
            {
                string InstanceName = XmlHelper.Name(ModelParameterisation);
                Children.Add(new ModelInstance(InstanceName, ModelParameterisation, this, CurrentAssembly));
            }
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
        string ClassName = XmlHelper.Attribute(ModelDescription, "ClassName");
        if (ClassName == "")
            throw new Exception("Cannot find a ClassName attribute on InstantiateModel XML element: " + ModelDescription.Name);

        string InnerContents = ModelDescription.InnerXml;

        // replace [InstanceName] with the name of this instance.
        InnerContents = InnerContents.Replace("[InstanceName]", XmlHelper.Name(ModelParameterisation));

        if (ModelParameterisation.Name == "soil" || ClassName == "Plant")
        {
            if (ModelParameterisation.Name == "soil")
                SoilNode = ModelParameterisation;
            InnerContents = Soil.ReplaceSoilMacros(SoilNode, InnerContents);
        }
        ApsimFile.Macro M = new Macro();
        InnerContents = M.Go(ModelParameterisation, InnerContents);

        ModelDescription.InnerXml = InnerContents;

        ModelDescription = XmlHelper.ChangeType(ModelDescription, ClassName);

        string AssemblyFileName = XmlHelper.Attribute(ModelDescription, "Assembly");
        if (AssemblyFileName == "")
            throw new Exception("Cannot find an assembly attribute on InstantiateModel XML element: " + ModelDescription.Name);
        AssemblyFileName = Path.Combine(Configuration.ApsimBinDirectory(), AssemblyFileName);
        if (!File.Exists(AssemblyFileName))
            throw new Exception("Cannot find assemble: " + AssemblyFileName);
        Assembly ModelAssembly = Assembly.LoadFile(AssemblyFileName);
        if (ModelAssembly == null)
            throw new Exception("Cannot find assembly: " + AssemblyFileName);

        string InstanceName = XmlHelper.Name(ModelParameterisation);
        Children.Add(new ModelInstance(InstanceName, ModelDescription, this, ModelAssembly));
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
        if (!Enabled)
            foreach (EventSubscriber Subscriber in Subscribers)
                Subscriber.Enabled = false;
        ResolveRefs();
        UpdateValues();
        foreach (ModelInstance Child in Children)
            Child.Initialise();
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
            object ModelReference = null;
            if (Ref.Info.FieldType.Name == "Paddock")
                ModelReference = new Paddock(ParentPaddock);

            else if (Ref.Info.FieldType.Name == "Component" && Ref.Name == "My")
                ModelReference = new ModelFramework.Component(this);

            else
            {
                ModelReference = FindModelByName(Ref.Name);
                if (ModelReference == null)
                    ModelReference = FindModelByType(Ref.Info.FieldType.Name);
            }

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

    public object FindModelByName(string NameToFind)
    {
        ModelInstance I = FindModelInstance(NameToFind);
        if (I == null && !NameToFind.Contains(".") && Parent != null)
        {
            // No dot was specified so try our parent.
            return Parent.FindModelByName(NameToFind);
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

    public ClassVariable FindOutput(string NameToFind)
    {
        if (VariableCache.ContainsKey(NameToFind))
            return VariableCache[NameToFind];

        ClassVariable V = FindOutputInternal(NameToFind);
        if (V != null)
            VariableCache.Add(NameToFind, V);
        return V;
    }

    private ClassVariable FindOutputInternal(string NameToFind)
    {
        if (NameToFind.Contains("."))
        {
            // absolute address.
            int PosLastPeriod = NameToFind.LastIndexOf('.');
            ModelInstance I = FindModelInstance(NameToFind.Substring(0, PosLastPeriod));
            if (I == null)
                return null;
            // See if we have the output
            NameToFind = NameToFind.Substring(PosLastPeriod + 1);
            foreach (ClassVariable V in I.Outputs)
                if (string.Equals(V.Name, NameToFind, StringComparison.CurrentCultureIgnoreCase))
                    return V;
            return null;
        }
        else
        {
            // If we get this far, then we haven't found it - check our children.
            ClassVariable V1 = FindOutputInChildren(NameToFind);
            if (V1 != null)
                return V1;

            // If we get this far, then we haven't found the output so go check our parent.
            if (Parent == null)
                return null;
            else
                return Parent.FindOutputInternal(NameToFind);
        }
    }


    private ClassVariable FindOutputInChildren(string NameToFind)
    {
        // See if we have the output
        foreach (ClassVariable V in Outputs)
            if (V.Name.ToLower() == NameToFind.ToLower())
                return V;

        foreach (ModelInstance Child in Children)
        {
            ClassVariable V1 = Child.FindOutputInChildren(NameToFind);
            if (V1 != null)
                return V1;
        }
        return null;
    }



    /// <summary>
    /// Update all [Input] values for this model instance and all child instances.
    /// </summary>
    public void UpdateValues()
    {
        foreach (ClassVariable Input in Inputs)
            Input.UpdateValue();
    }

    public void OnTick(TimeType t)
    {
        if (Enabled)
        {
            UpdateValues();
            foreach (ModelInstance Child in Children)
                Child.OnTick(t);
        }
    }

    public void CreateInstanceOfModel()
    {
        TheModel = Activator.CreateInstance(ClassType);

        CollectVariablesAndEvents();

        // If this model doesn't have a [Param] of type XmlNode then go through all children
        // and assume that those that aren't a [Param] must be a nested model.
        if (!ModelHasXMLParam())
        {
            foreach (XmlNode Child in XmlHelper.ChildNodes(Node, ""))
            {
                bool ChildEnabled = XmlHelper.Attribute(Child, "enabled") != "no";

                // If the model has a [Param] with the same name as this child xml node
                // then don't go and try create a nested simulation object for this xml node.

                if (!(Child is XmlWhitespace) && !ModelHasParam(Child.Name) && ChildEnabled)
                    TryCreateModelInstance(Child, ClassType.Assembly);
            }
        }
    }


    public void Activate()
    {
        if (!Enabled)
        {
            // We're going to tear everything down and create a new, fresh model.
            //TearDownModel();
            EnableEvents();
            UpdateAllValues();
            //CreateInstanceOfModel();
            //Root.ResolveAllRefsAndEvents();
            //PublishToChildren("Initialised");
        }
    }

    private void UpdateAllValues()
    {
        UpdateValues();
        foreach (ModelInstance Child in Children)
            Child.UpdateValues();
    }


    /// <summary>
    /// Need to tear down entire model and reinstate a fresh one.
    /// </summary>
    public void Deactivate()
    {
        if (Enabled)
        {
            Enabled = false;
            DisableEvents();
            //TearDownModel();
            //CreateInstanceOfModel();
            //Initialise();
            //Root.ResolveAllRefsAndEvents();
            //PublishToChildren("Initialised");
        }
    }

    private void EnableEvents()
    {
        Enabled = true;
        foreach (EventSubscriber Subscriber in Subscribers)
            Subscriber.Enabled = true;
        foreach (ModelInstance Child in Children)
            Child.EnableEvents();
    }

    private void DisableEvents()
    {
        foreach (EventSubscriber Subscriber in Subscribers)
            Subscriber.Enabled = false;
        foreach (ModelInstance Child in Children)
            Child.DisableEvents();
    }

    void ResolveAllRefsAndEvents()
    {
        VariableCache.Clear();
        ConnectInputsAndOutputs();
        ResolveRefs();
        if (Enabled)
            ConnectEvents();
        UpdateValues();
        foreach (ModelInstance Child in Children)
            Child.ResolveAllRefsAndEvents();
    }
    void TearDownModel()
    {
        Inputs.Clear();
        Outputs.Clear();
        Params.Clear();
        States.Clear();
        Refs.Clear();
        Root.DisconnectAllEvents();
        Subscribers.Clear();
        Publishers.Clear();
        VariableCache.Clear();
        foreach (ModelInstance Child in Children)
        {
            Child.TearDownModel();
            Child.Parent = null;
        }
        Children.Clear();
    }

    internal void GetAllOutputNames(ref List<string> VariableNames)
    {
        foreach (ClassVariable Output in Outputs)
            VariableNames.Add(Name + "." + Output.Name);
        foreach (ModelInstance Child in Children)
            Child.GetAllOutputNames(ref VariableNames);
    }

    internal void GetAllOutputValues(ref List<object> Values)
    {
        foreach (ClassVariable Output in Outputs)
            Values.Add(Output.Value);
        foreach (ModelInstance Child in Children)
            Child.GetAllOutputValues(ref Values);
    }

    /// <summary>
    /// Returns true if this model has a [Param] with the specified name.
    /// </summary>
    internal bool ModelHasParam(string ParamName)
    {
        foreach (ClassVariable Param in Params)
        {
            if (Param.Name.ToLower() == ParamName.ToLower())
                return true;
        }
        return false;
    }

    internal bool ModelHasXMLParam()
    {
        foreach (ClassVariable Param in Params)
        {
            if (Param.Type == typeof(XmlNode))
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
                    Children.Add(new ModelInstance(XmlHelper.Name(Child), Child, this, ModelAssembly));
            }
        }

        Initialise();

        // See if the newly created instance has an Initialised event handler. If so then call it.
        PublishToChildren("Initialised");
    }

    /// <summary>
    /// Collect all variables and events from the specified instance and all child instances.
    /// </summary>
    private void CollectVariablesAndEvents()
    {
        // Collection all fields.
        foreach (FieldInfo Field in GetAllFields(TheModel.GetType(), BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic))
            ProcessClassMember(Field);
        
        // Collect all properties.
        foreach (PropertyInfo Property in TheModel.GetType().GetProperties(BindingFlags.FlattenHierarchy | BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic))
            ProcessClassMember(Property);
        
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
                    AddSubscriber(Handler.EventName, EventHandler, TheModel);
                }
        }

        // Now go and do the same for all child models.
        foreach (ModelInstance Child in Children)
        {
            Child.CollectVariablesAndEvents();
        }
    }

    private void ProcessClassMember(MemberInfo Member)
    {
        // Exclude delegates.
        if (Member is FieldInfo)
        {
            bool IsDelegate = (Member as FieldInfo).FieldType.BaseType.Name == "MulticastDelegate";
            if (IsDelegate)
                return;
        }
        // First off look for [Link]
        object[] LinkAttributes = Member.GetCustomAttributes(typeof(Link), false);
        if (LinkAttributes.Length != 0)
        {
            if (Member is PropertyInfo)
                throw new Exception("Not allowed to have a link on a property. Name is: " + Member.Name);

            Link LinkAttribute = LinkAttributes[0] as Link;
            if (LinkAttribute.NamePath == null)
                LinkAttribute.NamePath = Member.Name;
            Refs.Add(new LinkField(TheModel, LinkAttribute.NamePath, Member as FieldInfo, LinkAttribute.IsOptional));
            return;
        }

        // Not a link so it must be a normal class variable.
        ClassVariable Variable = new ClassVariable(Member, TheModel);

        if (Variable.IsInput)
            Inputs.Add(Variable);
        else if (Variable.IsOutput)
            Outputs.Add(Variable);

        if (Variable.IsParam)
        {
            string ParamName = Variable.Name;
            object ParamValue = null;

            // Get the parameter value.
            if (ParamName.ToLower() == "name")
                ParamValue = Name;
            else if (Variable.Type.Name == "XmlNode")
                ParamValue = Node;
            else
            {
                List<XmlNode> ParamNodes = XmlHelper.ChildNodesByName(Node, ParamName);
                if (ParamNodes.Count > 0)
                {
                    if (Variable.Type.Name == "List`1")
                    {
                        IList L = (IList)Activator.CreateInstance(Variable.Type);
                        ParamValue = L;

                        foreach (XmlNode ParamNode in ParamNodes)
                        {
                            ModelInstance NewModelInstance = new ModelInstance(XmlHelper.Name(ParamNode), ParamNode, this, ClassType.Assembly);
                            Children.Add(NewModelInstance);
                            L.Add(NewModelInstance.TheModel);
                        }
                    }
                    else if (ClassType.Assembly.GetType(Variable.Type.Name) != null)
                    {
                        // we have found a nested param - treat it as a model.
                        ModelInstance NewModelInstance = new ModelInstance(XmlHelper.Name(ParamNodes[0]), ParamNodes[0], this, ClassType.Assembly);
                        Children.Add(NewModelInstance);
                        ParamValue = NewModelInstance.TheModel;
                    }
                    else
                    {
                        // Simple parameter value from XML.
                        string ParamStringValue = ParamNodes[0].InnerXml;
                        if (ParamStringValue != null)
                            ParamValue = TypeConverter.Convert(ParamName, ParamStringValue, Variable.Type);
                    }
                }
                else if (Variable.Type.IsArray && ParamName[ParamName.Length - 1] == 's')
                {
                    // We have encountered a [Param] that is an array e.g.
                    // [Param] string[] XYs;
                    // The name "XYs" is converted to a non plural name: XY
                    // and all child values of this XML Node with a type of "XY" will be found and 
                    // inserted into a newly created array.
                    string ParamNameNoPlural = ParamName.Substring(0, ParamName.Length - 1);
                    List<string> Values = XmlHelper.Values(Node, ParamNameNoPlural);
                    if (Values.Count > 0)
                        ParamValue = TypeConverter.Convert(ParamName, Values.ToArray(), Variable.Type);
                }
            }
            if (ParamValue == null && !Variable.IsOptional)
                throw new Exception("Cannot find a parameter value for: " + Variable.Name + " for " + Name);
            else if (ParamValue != null)
                Variable.Value = ParamValue;
            
            Params.Add(Variable);
        }

        if (Variable.IsState)
            States.Add(Variable);
    }

    internal void AddSubscriber(string EventName, MethodInfo EventHandler, object Model)
    {
        Subscribers.Add(new EventSubscriber(EventName, EventHandler, Model));
    }

    /// <summary>
    /// Publish an event with no data.
    /// </summary>
    internal void Publish(string EventName)
    {
        int PosPeriod = EventName.LastIndexOf('.');
        if (PosPeriod != -1)
        {
            string ModelName = EventName.Substring(0, PosPeriod);
            ModelInstance i = FindModelInstance(EventName.Substring(0, PosPeriod));
            if (i == null)
                throw new Exception("Cannot publish event to model: " + ModelName + ". Model not found");
            i.PublishToChildren(EventName.Substring(PosPeriod + 1));
            return;
        }
        PublishToChildren(EventName);
        if (Parent != null)
            Parent.Publish(EventName);
    }

    internal void PublishToChildren(string EventName)
    {
        foreach (EventSubscriber Subscriber in Subscribers)
        {
            if (Subscriber.Name.ToLower() == EventName.ToLower())
                Subscriber.Invoke();
        }
        foreach (ModelInstance Child in Children)
            Child.PublishToChildren(EventName);
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
        foreach (ClassVariable Input in Inputs)
        {
            ClassVariable Output = FindOutput(Input.Name);
            if (Output != null)
            {
                Input.ConnectTo(Output);
            }
        }
    }

    /// <summary>
    /// Connect all inputs to outputs for the specified instance and all child instances.
    /// </summary>
    internal void ConnectEvents()
    {
        foreach (EventSubscriber Subscriber in Subscribers)
        {
            EventPublisher Publisher = Root.FindEventPublisher(Subscriber.Name);
            if (Publisher != null)
                Publisher.ConnectTo(Subscriber);
        }
    }

    /// <summary>
    /// Connect all inputs to outputs for the specified instance
    /// </summary>
    internal void DisconnectAllEvents()
    {
        foreach (EventSubscriber Subscriber in Subscribers)
            Subscriber.Disconnect();

        foreach (ModelInstance Child in Children)
            Child.DisconnectAllEvents();
    }

    /// <summary>
    /// Recurse through all model instances and find an event publisher with the specified name.
    /// Returns null if not found.
    /// </summary>
    private EventPublisher FindEventPublisher(string EventName)
    {
        foreach (EventPublisher Publisher in Publishers)
        {
            if (string.Equals(Publisher.Name, EventName, StringComparison.CurrentCultureIgnoreCase))
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
        ClassVariable V = FindOutput(NamePath);
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
        ClassVariable V = FindOutput(NamePath);
        if (V != null)
        {
            Data = (T[]) TypeConverter.Convert(NamePath, V.Value, typeof(T[]));
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
        ClassVariable V = FindOutput(NamePath);
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

    internal bool Get(string NamePath, out object Data)
    {
        ClassVariable V = FindOutput(NamePath);
        if (V != null)
        {
            Data = V.Value;
            return true;
        }
        else
        {
            Data = null;
            return false;
        }
    }

    internal bool Get(string NamePath, out string[] Data)
    {
        ClassVariable V = FindOutput(NamePath);
        if (V != null)
        {
            Data = (string[]) TypeConverter.Convert(NamePath, V.Type, typeof(string[]));
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
        ClassVariable V = FindOutput(NamePath);
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
        ClassVariable V = FindOutput(NamePath);
        if (V != null)
        {
            V.Value = TypeConverter.Convert(NamePath, Data, V.Type);
            return true;
        }
        else
            return false;
    }

    /// <summary>
    /// Checkpoint the specified ModelInstance and all children by writing their values to the specified node
    /// Captures the values of all state variables.
    /// </summary>
    internal void Checkpoint(XmlNode ParentNode)
    {
        XmlNode Node = ParentNode.AppendChild(ParentNode.OwnerDocument.CreateElement(Name));
        foreach (ClassVariable State in States)
            SerialiseObject(State.Name, State.Type, State.Value, Node);

        foreach (ModelInstance Child in Children)
            Child.Checkpoint(Node);

    }

    private static void SerialiseObject(string Name, Type Type, object Value, XmlNode ParentNode)
    {
        bool WriteBlank = Value == null;// ||
                            //(Type.IsArray && (Value as Array).Length == 0) ||
                            //(Type.Name == "List`1" && (Value as IList).Count == 0);

        if (WriteBlank)
            XmlHelper.SetValue(ParentNode, Name, "");
        else
        {
            if (!TypeConverter.CanHandleType(Type))
            {
                // We don't know about this type - ask class to serialise.
                MethodInfo Serialise = Value.GetType().GetMethod("Serialise");
                if (Serialise != null)
                    Serialise.Invoke(Value, null);
                else
                {
                    // Class doesn't have a serialise method so try and go through each member and write to node.
                    XmlNode Node = ParentNode.AppendChild(ParentNode.OwnerDocument.CreateElement(Name));
                    foreach (FieldInfo Field in GetAllFields(Value.GetType(), BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic))
                        SerialiseObject(Field.Name, Field.FieldType, Field.GetValue(Value), Node);
                }
            }
            else
                XmlHelper.SetValue(ParentNode, Name, TypeConverter.Convert(Name, Value, typeof(string)) as string);
        }
    }

    /// <summary>
    /// Use the specified checkpoint XML node to initialise all objects.
    /// </summary>
    /// <param name="ParentNode"></param>
    internal void RestoreFromCheckpoint(XmlNode ParentNode)
    {
        foreach (XmlNode ChildNode in ParentNode.ChildNodes)
        {
            // See if this node is a state.
            ClassVariable FoundState = null;
            foreach (ClassVariable State in States)
                if (State.Name == ChildNode.Name)
                {
                    FoundState = State;
                    break;
                }

            if (FoundState == null)
            {
                // Not a state - so it must be a child.
                ModelInstance FoundChild = null;
                foreach (ModelInstance Child in Children)
                    if (Child.Name == ChildNode.Name)
                    {
                        FoundChild = Child;
                        break;
                    }

                if (FoundChild == null)
                    throw new Exception("Cannot find class: " + ChildNode.Name + " while restoring from a checkpoint");

                FoundChild.RestoreFromCheckpoint(ChildNode);
            }
            else
            {
                FoundState.Value = DeSerialiseObject(FoundState.Name, FoundState.Type, FoundState.Value, ChildNode);

            }


        }
    }

    /// <summary>
    /// "Value" is what the model currently has.
    /// </summary>
    /// <param name="Name"></param>
    /// <param name="Type"></param>
    /// <param name="Value"></param>
    /// <param name="Node"></param>
    /// <returns></returns>
    private object DeSerialiseObject(string Name, Type Type, object Value, XmlNode Node)
    {
        // Found a state - set its value.
        string st = Node.InnerText;
        if (st == "" && Node.InnerXml == "")
            return null;

        else
        {
            if (!TypeConverter.CanHandleType(Type))
            {
                // We don't know about this type - ask class to deserialise.
                MethodInfo DeSerialise = Type.GetMethod("DeSerialise");
                if (DeSerialise != null)
                    return DeSerialise.Invoke(Value, null);
                else
                {
                    if (Type.Name == "List`1")
                    {
                        int size = Convert.ToInt32(XmlHelper.Value(Node, "_size"));
                        if (size != 0)
                            throw new NotImplementedException();
                        if (Value != null)
                        {
                            (Value as IList).Clear();
                            return Value;
                        }
                        else
                            throw new NotImplementedException();
                    }
                    else if (Type.IsArray)
                    {
                        throw new NotImplementedException();
                    }

                    // Class doesn't have a deserialise method so try and go through each member and deserialise manually
                    foreach (FieldInfo Field in GetAllFields(Value.GetType(), BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic))
                    {
                        // Need to find correct child XML node.
                        XmlNode ChildNode = XmlHelper.Find(Node, Field.Name);
                        if (ChildNode == null)
                            throw new Exception("Cannot find child node: " + Field.Name + " in checkpoint under: " + Node.Name);
                        Field.SetValue(Value, DeSerialiseObject(Field.Name, Field.FieldType, Field.GetValue(Value), ChildNode));
                    }
                    return Value;
                }
            }
            else
                return TypeConverter.Convert(Name, st, Type);

        }
        
    }

    internal void OutputDiagnostics()
    {
        if (Inputs.Count > 0)
            Console.WriteLine("Component: " + FullName);

        foreach (ClassVariable Input in Inputs)
            Console.WriteLine("    " + Input.Name);
        foreach (ModelInstance Child in Children)
            Child.OutputDiagnostics();

    }

}
