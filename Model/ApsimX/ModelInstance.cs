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
using System.Xml.Serialization;

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
    public string ShortCutPath;
    public static Simulation RootSimulation;
    private string _FullName;
    private List<string> ComponentOrder = null;
    private bool Enabled = true;
    private Dictionary<string, ClassVariable> VariableCache = new Dictionary<string, ClassVariable>();



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
    public void SortChildren()
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
    internal ModelInstance(string InstanceName, XmlNode _Node, ModelInstance ParentInstance, Type _ClassType)
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

            _FullName = InstanceName;
            Title = InstanceName;
        }

        // resolve any shortcuts.
        Node = _Node;
        ShortCutPath = XmlHelper.Attribute(Node, "shortcut");
        if (ShortCutPath != "")
        {
            Node = XmlHelper.Find(Node, ShortCutPath);
            if (Node == null)
                throw new Exception("Cannot find shortcut: " + ShortCutPath);
        }
        ClassType = _ClassType;
        if (ClassType == null)
            throw new Exception("Cannot find a model class called: " + Node.Name);

        Parent = ParentInstance;

        TheModel = Activator.CreateInstance(ClassType);

        CollectVariablesAndEvents();
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
        ResolveRefs(IncludeChildren: false);
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
    public void ResolveRefs(bool IncludeChildren)
    {
        foreach (LinkField Ref in Refs)
        {
            if (Ref.Info.FieldType.Name == "Paddock")
                Ref.Value = new Paddock(ParentPaddock);

            else if (Ref.Info.FieldType.Name == "Component" && Ref.Name == "My")
                Ref.Value = new ModelFramework.Component(this);

            else
            {
                ModelInstance[] Instances = FindModelsByName(Ref.Name);

                if (Instances == null)
                    Instances = FindModelsByType(Ref.Info.FieldType.Name);

                if (Instances == null)
                {
                    if (!Ref.Optional)
                        throw new Exception("Cannot find ref: " + Ref.Name);
                }
                else
                {
                    if (Ref.Info.FieldType.IsArray)
                    {
                        // An link which is an array has been encountered.
                        // e.g. [Link] LeafCohort[] InitialLeaves;
                        int[] args = new int[] {Instances.Length};
                        Array A = Array.CreateInstance(Ref.Info.FieldType.GetElementType(), Instances.Length) as Array;
                        for (int i = 0; i < Instances.Length; i++)
                            A.SetValue(Instances[i].TheModel, i);
                        Ref.Value = A;
                    }
                    else
                        Ref.Value = Instances[0].TheModel;
                }
            }
        }
        if (IncludeChildren)
            foreach (ModelInstance Child in Children)
                Child.ResolveRefs(IncludeChildren);
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

    public ModelInstance FindModelInstance(string NameToFind)
    {
        ModelInstance[] Instances = FindModelInstances(NameToFind);
        if (Instances == null || Instances.Length == 0)
            return null;
        else
            return Instances[0];
    }

    /// <summary>
    /// Go find a model instance using the specified NameToFind. This name can
    /// have path information. Returns null if not found.
    ///   A leading . indicates an absolute address. e.g. .simulation.paddock1.soilwat
    ///   No dot indicates a child.                  e.g. leaf 
    /// </summary>
    public ModelInstance[] FindModelInstances(string NameToFind)
    {
        List<ModelInstance> Instances = new List<ModelInstance>();

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
                Instances.Clear();
                foreach (ModelInstance Instance in I.Children)
                {
                    string NameNoArray = Instance.Name;
                    StringManip.SplitOffBracketedValue(ref NameNoArray, '[', ']');
                    if (string.Equals(NameNoArray, Path, StringComparison.CurrentCultureIgnoreCase))
                        Instances.Add(Instance);
                }
                if (Instances.Count == 0)
                    return null;
                
                I = Instances[0];
            }

            PathIndex++;
        }
        return Instances.ToArray();
    }

    public ModelInstance[] FindModelsByName(string NameToFind)
    {
        ModelInstance[] I = FindModelInstances(NameToFind);
        if (I == null && !NameToFind.Contains(".") && Parent != null)
        {
            // No dot was specified so try our parent.
            return Parent.FindModelsByName(NameToFind);
        }
        return I;
    }

    public ModelInstance[] FindModelsByType(string TypeToFind)
    {
        List<ModelInstance> Instances = null;
        foreach (ModelInstance Child in Children)
        {
            if (string.Equals(Child.ClassType.ToString(), TypeToFind, StringComparison.CurrentCultureIgnoreCase))
            {
                if (Instances == null)
                    Instances = new List<ModelInstance>();
                Instances.Add(Child);
            }
        }
        if (Instances != null)
            return Instances.ToArray();

        // Not found - try our parent.
        if (Parent != null)
            return Parent.FindModelsByType(TypeToFind);
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


    public ClassVariable FindParamOrState(string NameToFind)
    {
        foreach (ClassVariable Param in Params)
            if (string.Equals(Param.Name, NameToFind, StringComparison.CurrentCultureIgnoreCase))
                return Param;
        foreach (ClassVariable State in States)
            if (string.Equals(State.Name, NameToFind, StringComparison.CurrentCultureIgnoreCase))
                return State;

        return null;
    }

    public ClassVariable FindParam(string NameToFind)
    {
        foreach (ClassVariable Param in Params)
            if (string.Equals(Param.Name, NameToFind, StringComparison.CurrentCultureIgnoreCase))
                return Param;

        return null;
    }

    public LinkField FindLink(string NameToFind)
    {
        foreach (LinkField Link in Refs)
            if (string.Equals(Link.Name, NameToFind, StringComparison.CurrentCultureIgnoreCase))
                return Link;

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

    public void Activate()
    {
        if (!Enabled)
        {
            // We're going to tear everything down and create a new, fresh model.
            //TearDownModel();
            EnableEvents();
            //ResolveAllRefsAndEvents();
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
            Subscriber.Connect();
        foreach (ModelInstance Child in Children)
            Child.EnableEvents();
    }

    private void DisableEvents()
    {
        foreach (EventSubscriber Subscriber in Subscribers)
            Subscriber.Disconnect();
        foreach (ModelInstance Child in Children)
            Child.DisableEvents();
    }

    void ResolveAllRefsAndEvents()
    {
        VariableCache.Clear();
        ConnectInputsAndOutputs();
        ResolveRefs(IncludeChildren: false);
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

    internal ClassVariable ModelHasXMLParam()
    {
        foreach (ClassVariable Param in Params)
        {
            if (Param.Type == typeof(XmlNode))
                return Param;
        }
        return null;
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
        ClassType = XmlSerialiser.GetClassType(Node.Name, ModelAssembly);
        if (ClassType == null)
            throw new Exception("Cannot find a model class called: " + Node.Name);

        TheModel = Activator.CreateInstance(ClassType);
        CollectVariablesAndEvents();

        //if (!ModelHasXMLParam())
        //{
        //    foreach (XmlNode Child in XmlHelper.ChildNodes(Node, ""))
        //    {
        //        // If the model has a [Param] with the same name as this child xml node
        //        // then don't go and try create a nested simulation object for this xml node.
        //        if (!ModelHasParam(Child.Name) && Child.Name.ToLower() != "summaryfile")
        //            Children.Add(new ModelInstance(XmlHelper.Name(Child), Child, this, ModelAssembly));
        //    }
        //}

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
            Type BaseType = (Member as FieldInfo).FieldType.BaseType;
            bool IsDelegate = BaseType != null && BaseType.Name == "MulticastDelegate";
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
            Params.Add(Variable);

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
        for (int i = 0; i < Subscribers.Count; i++)
        {
            if (Subscribers[i].Name.ToLower() == EventName.ToLower())
                Subscribers[i].Invoke();
        }
        foreach (ModelInstance Child in Children)
            Child.PublishToChildren(EventName);
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
    internal string Checkpoint()
    {
        StringWriter Writer = new StringWriter();
        XmlSerialiser.Serialise(this, Writer, true);
        return Writer.ToString();
    }

    /// <summary>
    /// Use the specified checkpoint XML node to recreate all objects - runs slower than
    /// the InitialiseFromCheckpoint method
    /// </summary>
    internal void RestoreFromCheckpoint(string Checkpoint)
    {
        Root.DisconnectAllEvents();

        StringReader In = new StringReader(Checkpoint);
        ModelInstance NewInstance = XmlSerialiser.Deserialise(In, null);
        Parent.Children.Remove(this);
        NewInstance.Parent = Parent;
        Parent.Children.Add(NewInstance);
        Parent.SortChildren();

        // restoring from checkpoint doesn't do links.
        Root.ResolveAllRefsAndEvents();
    }

    /// <summary>
    /// Use the specified checkpoint XML node to initialise all objects.
    /// </summary>
    internal void InitialiseFromCheckpoint(string Checkpoint)
    {
        StringReader In = new StringReader(Checkpoint);
        XmlSerialiser.Deserialise(In, Parent, false);
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

    /// <summary>
    ///  Run all simulations found from this model instance, looking through all children.
    /// </summary>
    public void Run()
    {
        if (TheModel is Simulation)
        {
            UpdateValues();
            do
                PublishToChildren("Timestep");
            while (true);
        }
        foreach (ModelInstance Child in Children)
            Child.Run();
    }

}
