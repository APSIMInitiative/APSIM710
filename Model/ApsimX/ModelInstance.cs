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
public class ModelInstance
{
    private List<ModelInstance> _Children = new List<ModelInstance>();
    private List<ClassVariable> _Inputs = new List<ClassVariable>();
    private List<ClassVariable> _Outputs = new List<ClassVariable>();
    private List<ClassVariable> _Params = new List<ClassVariable>();
    private List<ClassVariable> _States = new List<ClassVariable>();
    private List<ClassLink> _Links = new List<ClassLink>();
    private List<EventPublisher> _Publishers = new List<EventPublisher>();
    private List<EventSubscriber> _Subscribers = new List<EventSubscriber>();
    private Dictionary<string, ClassVariable> VariableCache = new Dictionary<string, ClassVariable>();
    private int ExecutionOrder = Int32.MaxValue;
    private string _FullName;
    private bool _Enabled = true;
    private Type _ClassType;
    private ModelInstance _Parent;
    private object _TheModel;
    private string _ShortcutPath;
    private static bool _Terminate;

    /// <summary>
    /// Internal constructor to create an instance of a model wrapper as specified by the 
    /// XmlNode. The parent instance is also passed in.
    /// </summary>
    internal ModelInstance(string Name, bool Enabled, string ShortcutPath, ModelInstance Parent, Type ClassType)
    {
        _Enabled = Enabled;
        _ShortcutPath = ShortcutPath;
        _Parent = Parent;
        _ClassType = ClassType;

        if (Parent != null)
        {
            if (!Parent._Enabled)
                _Enabled = false;
            _FullName = Parent.FullName + "." + Name;
        }
        else
        {
            _FullName = Name;
            Subscribe("Tick", GetType().GetMethod("OnTick", BindingFlags.NonPublic | BindingFlags.Instance), this);
            Subscribe("Terminate", GetType().GetMethod("OnTerminate", BindingFlags.NonPublic | BindingFlags.Instance), this);
        }

        _TheModel = Activator.CreateInstance(ClassType);
        CollectVariablesAndEvents();

        // See if we have an execution order state variable.
        ClassVariable ExecutionOrderVar = States.Find(var => var.Name == "ExecutionOrder");
        if (ExecutionOrderVar != null)
            ExecutionOrder = Convert.ToInt32(ExecutionOrderVar.Value);
    }

    #region Public properties
    /// <summary>
    /// Returns a list of child ModelInstances.
    /// </summary>
    public List<ModelInstance> Children { get { return _Children; } }

    /// <summary>
    /// Returns a list of child ModelInstances that have been sorted according to component order.
    /// </summary>
    public List<ModelInstance> ChildrenSorted
    {
        get
        {
            // For speed reasons check to see if any child has an ExecutionOrder. If not then
            // just return the normal children list. Otherwise return a sorted list.
            if (Children.Find(var => var.ExecutionOrder != Int32.MaxValue) == null)
                return Children;
            else
            {
                List<ModelInstance> SortedChildren = Children;
                Utility.StableSort<ModelInstance>(SortedChildren,
                                                  (x, y) => x.ExecutionOrder.CompareTo(y.ExecutionOrder));
                return SortedChildren;
            }
        }
    }

    /// <summary>
    /// Returns a list of variables decorated with [Input]
    /// </summary>
    public List<ClassVariable> Inputs { get { return _Inputs; } }

    /// <summary>
    /// Returns a list of variables decorated with [Output]
    /// </summary>
    public List<ClassVariable> Outputs { get { return _Outputs; } }

    /// <summary>
    /// Returns a list of variables decorated with [Param]
    /// </summary>
    public List<ClassVariable> Params { get { return _Params; } }

    /// <summary>
    /// Returns a list of variables not decorated with any attribute - state variables.
    /// </summary>
    public List<ClassVariable> States { get { return _States; } }

    /// <summary>
    /// Returns a list of variables decorated with [Link]
    /// </summary>
    public List<ClassLink> Links { get { return _Links; } }

    /// <summary>
    /// Returns a list of variables decorated with [Event] - events published.
    /// </summary>
    public List<EventPublisher> Publishers { get { return _Publishers; } }

    /// <summary>
    /// Returns a list of variables decorated with [EventHandler]
    /// </summary>
    public List<EventSubscriber> Subscribers { get { return _Subscribers; } }

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
    /// Return the 'Type' of the encapsulated model.
    /// </summary>
    public Type Type { get { return _ClassType; } }

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

    /// <summary>
    /// Property to return the parent ModelInstance. Returns null if no parent.
    /// </summary>
    public ModelInstance Parent { get { return _Parent; } }

    /// <summary>
    /// Return true if the model is enabled. When enabling or disabling a model, it will also enable / disable 
    /// all child models.
    /// </summary>
    public bool Enabled
    {
        get
        {
            return _Enabled;
        }
        set
        {
            if (value != _Enabled)
            {
                _Enabled = value;
                foreach (EventSubscriber Subscriber in Subscribers)
                    Subscriber.Enabled = _Enabled;
                foreach (ModelInstance Child in Children)
                    Child.Enabled = _Enabled;
            }
        }
    }

    /// <summary>
    /// Return the shortcut path for this ModelInstance or "" if not a shortcut.
    /// </summary>
    public string ShortcutPath { get { return _ShortcutPath; } }

    /// <summary>
    /// Return the actual 'Model' that this ModelInstance wraps.
    /// </summary>
    public object TheModel { get { return _TheModel; } }

    /// <summary>
    /// Return the Root ModelInstance i.e. the ModelInstance that doesn't have a Parent.
    /// </summary>
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
    #endregion

    /// <summary>
    /// Initialise this model instance by resolving all variable, events and links.
    /// </summary>
    internal void Initialise()
    {
        ConnectInputsAndOutputs();
        ConnectEvents();
        ResolveLinks();
        UpdateValues(Deep: false);
        foreach (ModelInstance Child in Children)
            Child.Initialise();
    }

    /// <summary>
    /// Resolve all [Link] pointers.
    /// </summary>
    private void ResolveLinks()
    {
        foreach (ClassLink Ref in Links)
        {
            if (Ref.Info.FieldType.Name == "Paddock")
            {
                ModelInstance ParentPaddock = Parent;
                while (ParentPaddock != null && ParentPaddock.Type.Name != "Area" && ParentPaddock.Type.Name != "Simulation")
                    ParentPaddock = ParentPaddock.Parent;
                Ref.Value = new Paddock(ParentPaddock);
            }

            else if (Ref.Info.FieldType.Name == "Component" && Ref.Name == "My")
                Ref.Value = new ModelFramework.Component(this);

            else if (Ref.Info.FieldType.IsArray)
            {
                // Find matching children.
                List<ModelInstance> Instances = Children.FindAll(Instance => Ref.Info.Name == StringManip.RemoveAfter(Instance.Name, '['));

                // An link which is an array has been encountered.
                // e.g. [Link] LeafCohort[] InitialLeaves;
                int[] args = new int[] { Instances.Count };
                Array A = Array.CreateInstance(Ref.Info.FieldType.GetElementType(), Instances.Count) as Array;
                for (int i = 0; i < Instances.Count; i++)
                    A.SetValue(Instances[i].TheModel, i);
                Ref.Value = A;
            }
            else
            {
                ModelInstance Instance = InScope.FindModel(Ref.Name, this);
                if (Instance == null)
                    Instance = InScope.FindModel(I => I.Type.Name == Ref.Info.FieldType.Name, this);
                else if (!Utility.IsOfType(Instance.Type, Ref.Info.FieldType.Name))
                    Instance = null;
                if (Instance == null)
                {
                    if (!Ref.Optional)
                        throw new Exception("Cannot find ref: " + Ref.Name);
                }
                else
                    Ref.Value = Instance.TheModel;
            }
        }
    }

    /// <summary>
    /// Find a specific output that matches "NameToFind". For speed reasons, we'll look in a
    /// variable cache first. This cache makes a big difference to runtime speed.
    /// </summary>
    private ClassVariable FindOutput(string NameToFind)
    {
        if (VariableCache.ContainsKey(NameToFind))
            return VariableCache[NameToFind];

        ClassVariable V = InScope.FindVariable(NameToFind, this, InScope.Outputs);

        if (V != null)
            VariableCache.Add(NameToFind, V);
        return V;
    }

    /// <summary>
    /// Update all [Input] values for this model instance
    /// </summary>
    private void UpdateValues(bool Deep)
    {
        foreach (ClassVariable Input in Inputs)
            Input.UpdateValue();
        if (Deep)
            foreach (ModelInstance Child in Children)
                Child.UpdateValues(Deep);
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
        Links.Clear();
        Publishers.Clear();
        Subscribers.Clear();

        _ClassType = XmlSerialiser.GetClassType(ModelDescription.Name, ModelAssembly);
        if (Type == null)
            throw new Exception("Cannot find a model class called: " + ModelDescription.Name);

        _TheModel = Activator.CreateInstance(Type);
        CollectVariablesAndEvents();

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
        foreach (FieldInfo Field in Utility.GetAllFields(TheModel.GetType(), BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic))
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
                    Subscribe(Handler.EventName, EventHandler, TheModel);
                }
        }

        // Now go and do the same for all child models.
        foreach (ModelInstance Child in Children)
        {
            Child.CollectVariablesAndEvents();
        }
    }

    /// <summary>
    /// Process the specified MemberInfo and put it into different Lists. e.g. Params, States, Inputs etc.
    /// </summary>
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
            Links.Add(new ClassLink(TheModel, LinkAttribute.NamePath, Member as FieldInfo, LinkAttribute.IsOptional));
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

    /// <summary>
    /// Subscribe to the specified event.
    /// </summary>
    internal void Subscribe(string EventName, MethodInfo EventHandler, object Model)
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
            ModelInstance i = InScope.FindModel(ModelName, this);
            if (i == null)
                throw new Exception("Cannot publish event to model: " + ModelName + ". Model not found");
            i.PublishToChildren(EventName.Substring(PosPeriod + 1));
            return;
        }
        PublishToChildren(EventName);
        if (Parent != null)
            Parent.Publish(EventName);
    }

    /// <summary>
    /// Publish
    /// </summary>
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

        foreach (ModelInstance Child in ChildrenSorted)
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
        NewInstance._Parent = Parent;
        Parent.Children.Add(NewInstance);

        // restoring from checkpoint doesn't do links.
        Root.Initialise();
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
    /// We have subscribed to the Tick event so that we can update all [Input] values in all models.
    /// </summary>
    private void OnTick(TimeType t)
    {
       UpdateValues(Deep: true);
    }

    /// <summary>
    /// A component has terminated the simulation. Flag this so that we can stop the timestep loop.
    /// </summary>
    private void OnTerminate()
    {
        _Terminate = true;
    }

    /// <summary>
    ///  Run all simulations found from this model instance, looking through all children.
    /// </summary>
    public void Run()
    {
        if (TheModel is Simulation)
        {
            _Terminate = false;
            UpdateValues(Deep: true);
            PublishToChildren("Initialised");

            do
                PublishToChildren("Timestep");
            while (_Terminate == false);
        }
        foreach (ModelInstance Child in Children)
            Child.Run();
    }


}
