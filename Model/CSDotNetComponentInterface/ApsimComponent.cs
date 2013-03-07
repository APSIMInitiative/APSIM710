using System;
using System.Xml;
using System.Reflection;
using System.Text;
using System.Collections.Generic;
using System.CodeDom.Compiler;
using System.Runtime.InteropServices;
using System.IO;
using CSGeneral;
using CMPServices;
using ApsimFile;

namespace ModelFramework
{
    /// <summary>
    /// Extends the TPropertyInfo class by adding a delegate pointing to a function which can be used
    /// to read or write the property.
    /// This is NOT meant to be used to any great extent, and is largely for backwards compatability,
    /// allowing for registration of new outputs on the fly
    /// </summary>
    class TExtendedPropertyInfo : TPropertyInfo
    {
        public propertyDelegate pDelegate;
        public TExtendedPropertyInfo(String sDDMLType, String sShortDescr, String sFullDescr, String sBaseType, propertyDelegate pDeleg)
            : base(sDDMLType, sShortDescr, sFullDescr, sBaseType)
        {
            pDelegate = pDeleg;
        }
    }
    /// <summary>
    /// Defines a delegate type for callbacks to read or write a manually-added property.
    /// </summary>
    /// <param name="propID">The propertyID</param>
    /// <param name="value">Receives the new value (if a read operation); holds the value to be used in a write (request set) operation(</param>
    /// <param name="isSetRequest">If true, the call is a request to set the property; if false, a request to read it.</param>
    /// <returns></returns>
    public delegate bool propertyDelegate(int propID, ref TPropertyInfo value, bool isSetRequest);

    // --------------------------------------------------------------------
    /// <summary>
    /// This is the main host for an Apsim component such as Plant2.
    /// </summary>
    // --------------------------------------------------------------------
    class ApsimComponent : Instance
    {
        private bool IsScript;
        private bool IsPlant;
        public Instance ModelInstance;
        private StringBuilder Contents;
        private Assembly modelAssembly;
        private bool Init1;
        public bool Init2Received;
        private Factory Fact;
        private int instanceNumber;
        public uint ComponentID;
        private bool DriverFound = false;
        //private String Name;
        private String DllFileName;
        private Dictionary<int, TExtendedPropertyInfo> RegistrationsPropStatic;     //component properties (for plant2 they are 'static' properties and will not be deregistered)
        private Dictionary<int, FactoryProperty> RegistrationsProp;         //component instance properties
        private Dictionary<int, FactoryProperty> RegistrationsDriver;
        private Dictionary<FactoryProperty, int> ReverseRegDriver;
        private Dictionary<int, ApsimType> RegistrationsDriverExtra;
        private Dictionary<String, int> RegistrationsDriverExtraLookup; //allows the lookup by name for the Get() function
        private Dictionary<int, ApsimType> RegistrationsEvent;
        
        private Dictionary<int, ApsimType> RegistrationsSet;
        public Dictionary<uint, TComp> SiblingComponents;  //includes itself
        private XmlNode InitData;
        private bool EndCropToday;

        protected Boolean haveWrittenToStdOutToday;
        protected TimeType tick;
        public TAPSIMHost Host;

        //Event id's for events that are handled in this class
        public const int INIT2INDEX = 9999999;
        protected int SOWINDEX = Int32.MaxValue;        //ensure these are unique/unused values
        protected int PREPAREINDEX = Int32.MaxValue;    //ensure these are unique/unused values
        protected int ENDCROPINDEX = Int32.MaxValue;
        protected int POSTINDEX = Int32.MaxValue;

        public String GetName() { return Name; }
        public uint GetId() { return ComponentID; }
        public Boolean DoingAGetDescription() { return Init1 == true; }
        // --------------------------------------------------------------------
        /// <summary>
        /// Normal constructor
        /// </summary>
        /// <param name="ModelAssembly"></param>
        /// <param name="instanceNumber"></param>
        // --------------------------------------------------------------------
        public ApsimComponent(Assembly ModelAssembly, int instancenumber, TAPSIMHost host)
        {
            Init1 = true;
            Init2Received = false;
            EndCropToday = false;
            instanceNumber = instancenumber;
            modelAssembly = ModelAssembly;
            DllFileName = ModelAssembly.Location;
            RegistrationsPropStatic = new Dictionary<int, TExtendedPropertyInfo>();
            RegistrationsProp = new Dictionary<int, FactoryProperty>();
            RegistrationsDriver = new Dictionary<int, FactoryProperty>();
            ReverseRegDriver = new Dictionary<FactoryProperty, int>();
            RegistrationsDriverExtra = new Dictionary<int, ApsimType>();
            RegistrationsDriverExtraLookup = new Dictionary<String, int>();
            RegistrationsEvent = new Dictionary<int, ApsimType>();
            RegistrationsSet = new Dictionary<int, ApsimType>();
            SiblingComponents = new Dictionary<uint, TComp>();  //id, comp
            Fact = new Factory();
            Host = host;
            IsScript = false;
            IsPlant = false;

            //The AssemblyResolve event is called when the common language runtime tries to bind to the assembly and fails.
            //This is used particularly when the in-memory compiled Script is trying to resolve it's references.
            AppDomain currentDomain = AppDomain.CurrentDomain;
            currentDomain.AssemblyResolve += new ResolveEventHandler(currentDomain_AssemblyResolve);
        }
        // --------------------------------------------------------------------
        /// <summary>
        /// Called by APSIM to create ourselves
        /// </summary>
        /// <param name="dllFileName"></param>
        /// <param name="compID"></param>
        /// <param name="parentID"></param>
        /// <param name="callbackArg"></param>
        /// <param name="callback"></param>
        // --------------------------------------------------------------------
        public void createInstance(String dllFileName,
                            uint compID,
                            uint parentID)
        {
            Name = "APSIM Component";   //default to something
            ComponentID = compID;

            // Load in the reflectiion dll in case our host component want's to use it.
            DllFileName = dllFileName;
            tick = new TimeType();
            tick.startday = tick.startsec = 0;
            haveWrittenToStdOutToday = false;
        }
        // -----------------------------------
        /// <summary>
        /// Called by APSIM to delete ourselves
        /// </summary>
        // -----------------------------------
        public void deleteInstance()
        {

        }
        // --------------------------------------------------------------------
        /// <summary>
        /// Store the SDML description for this component. Usually during init1.
        /// </summary>
        /// <param name="script"></param>
        // --------------------------------------------------------------------
        public void setScript(String script)
        {
            Contents = new StringBuilder(script.Length);
            Contents.Append(script);
        }
        // --------------------------------------------------------------------
        /// <summary>
        /// Called by APSIM/engine to perform some action
        /// </summary>
        /// <param name="message"></param>
        // --------------------------------------------------------------------
        public void Init1Handler()
        {
            try
            {
                if (Init1)
                {
                    Init1 = false;

                    XmlDocument Doc = new XmlDocument();
                    Doc.LoadXml(Contents.ToString());
                    String _compClass = XmlHelper.Attribute(Doc.DocumentElement, "class");
                    if (_compClass.Length > 0)
                        CompClass = _compClass;
                    InitData = XmlHelper.Find(Doc.DocumentElement, "initdata");
                    IsPlant = CompClass.Contains("Plant2");
                    IsScript = (XmlHelper.FindByType(InitData, "text") != null);

                    PREPAREINDEX = Host.eventCount();
                    registerEvent(null, "Prepare", "<type/>", PREPAREINDEX, TypeSpec.KIND_SUBSCRIBEDEVENT, 0, 0);

                    if (IsScript)
                    {
                        // Create an XML model that we can pass to BuildObjects.
                        XmlDocument NewDoc = new XmlDocument();
                        XmlNode ScriptNode = NewDoc.AppendChild(NewDoc.CreateElement("Manager2"));
                        ScriptNode.InnerXml = InitData.InnerXml;
                        // Build all necessary objects
                        BuildObjects(ScriptNode, modelAssembly);
                    }
                    else if (IsPlant)
                    {
                        SowPlant2Type sow = new SowPlant2Type();
                        int RegistrationIndex = Host.eventCount();
                        SOWINDEX = RegistrationIndex;
                        registerEvent(null, "Sow", sow.DDML(), SOWINDEX, TypeSpec.KIND_SUBSCRIBEDEVENT, 0, 0);
                        ENDCROPINDEX = SOWINDEX + 1;
                        registerEvent(null, "EndCrop", "<type/>", ENDCROPINDEX, TypeSpec.KIND_SUBSCRIBEDEVENT, 0, 0);
                        POSTINDEX = SOWINDEX + 2;
                        registerEvent(null, "Post", "<type/>", POSTINDEX, TypeSpec.KIND_SUBSCRIBEDEVENT, 0, 0);
                        //need a 'static' property here so other components know something of plant
                        int propertyID = RegisterProperty("plant_status", "<type kind=\"string\"/>", true, false, false, "Plant status", "out, alive, dead", null);
                        TExtendedPropertyInfo staticProperty;
                        if (RegistrationsPropStatic.TryGetValue(propertyID, out staticProperty))
                        {
                            staticProperty.setValue("out"); //default to no crop in
                        }
                    }
                    else
                    {
                        // Insert any other parameters found under the <InitData> section into the model description.
                        if (InitData.ChildNodes.Count == 0)
                            throw new Exception("No nodes found under <InitData> for dll: " + DllFileName);
                        foreach (XmlNode Parameter in InitData.ChildNodes)
                        {
                            if (XmlHelper.ChildNodes(Parameter, "").Count == 0)
                            {
                                // simple parameter - insert into model description
                                InsertParameterIntoModel(Parameter, InitData.ChildNodes[0]);
                            }
                        }
                        if (InitData.ChildNodes[0].Name == "PerformInstructions")
                        {
                             XmlNode ModelDescription = null;
                             PerformInstructions(InitData.ChildNodes[0], ref ModelDescription);
                             BuildObjects(ModelDescription, modelAssembly);
                        }
                        else
                            BuildObjects(InitData.ChildNodes[0], modelAssembly);
                    }
                    //process any init1's in the component
                    for (int i = 0; i != Fact.EventHandlers.Count; i++)
                    {
                        EvntHandler Event = Fact.EventHandlers[i];
                        if (String.Compare(Event.EventName, "Init1", true) == 0)
                        {
                            Event.Invoke(null);
                        }
                    }
                }
            }
            catch (System.Exception err)
            {
                String msg = "";
                if (err is System.Reflection.ReflectionTypeLoadException)
                {
                    var typeLoadException = err as ReflectionTypeLoadException;
                    var loaderExceptions = typeLoadException.LoaderExceptions;
                    msg = loaderExceptions[0].ToString();
                }
                SendFatalError(err.Message + "\n" + msg);
            }
        }
        // --------------------------------------------------------------------------
        /// <summary>
        /// Send a fatal error to the system.
        /// </summary>
        /// <param name="Line"></param>
        // --------------------------------------------------------------------------
        public void SendFatalError(String Line)
        {
            Line += "\nComponent name: " + TRegistrar.unQualifiedName(Name) + "\n";
            Host.sendError(Line, true);
        }
        // --------------------------------------------------------------------------
        /// <summary>
        /// Send a non fatal error to the simulation system.
        /// </summary>
        /// <param name="Line"></param>
        // --------------------------------------------------------------------------
        public void Warning(String Line)
        {
            Host.sendError(Line, false);
        }

        public uint lastEventPublisher;
        // --------------------------------------------------------------------------
        /// <summary>
        /// Executes an event in the Apsim component. Handles some events here
        /// directly and others are passed onto the component directly through
        /// the call to Event.unpack() -> Invoke()
        /// </summary>
        /// <param name="RegistrationIndex">Event ID</param>
        /// <param name="messageData">The message data that was passed with the event.</param>
        /// <returns>0 when it succeeds</returns>
        // --------------------------------------------------------------------------
        public int handleEvent(int RegistrationIndex, TTypedValue messageData, uint publisherID)
        {
            int result = 0;
            try
            {
                lastEventPublisher = publisherID;
                if (RegistrationIndex == INIT2INDEX)
                {
                    querySiblingComponents(Name);
                    Init2Received = true;
                    if (ModelInstance != null)
                    {
                        Console.WriteLine();
                        Console.Write("------- " + ModelInstance.Name + " Initialisation ");
                        int col = ModelInstance.Name.Length + 24;
                        String line = new String('-', 80 - col - 1);
                        Console.Write(line);
                        Console.WriteLine();
                    }
                    GetAllInputs();
                    if (!IsPlant)                   //plant will do this at sow time in BuildObjects()
                        Fact.Initialise();
                    for (int i = 0; i != Fact.EventHandlers.Count; i++)
                    {
                        EvntHandler Event = Fact.EventHandlers[i];
                        if (String.Compare(Event.EventName, "Init2", true) == 0)
                        {
                            GetAllInputs();
                            Event.Invoke(messageData);
                        }
                    }
                }
                else if (RegistrationIndex == SOWINDEX)
                    OnSow(messageData);
                else if (RegistrationIndex == ENDCROPINDEX)
                    OnEndCrop(messageData);
                else if (RegistrationIndex == POSTINDEX)
                    OnPost(messageData);
                else if (RegistrationIndex == PREPAREINDEX)
                    OnPrepare(messageData);
                else
                {
                    //execute other event
                    bool IsEvent = RegistrationsEvent[RegistrationIndex].GetType() == typeof(FactoryEventHandler);
                    if (IsEvent && Init2Received)
                    {
                        GetAllInputs();
                    }
                    ApsimType Event;
                    if (RegistrationsEvent.TryGetValue(RegistrationIndex, out Event))
                    {
                        try
                        {
                            if (Event.GetType() == typeof(FactoryEventHandler))
                                ((FactoryEventHandler)Event).unpack(messageData);   //unpacks and Invoke()
                            else if (Event.GetType() == typeof(RuntimeEventHandler))
                                ((RuntimeEventHandler)Event).Invoke(null);
                        }
                        catch (System.Exception err)
                        {
                            String details = Name + "." + ((EvntHandler)Event).EventName;
                            if (err.InnerException != null)
                                SendFatalError(err.InnerException.Message + " " + details);
                            else
                                SendFatalError(err.Message + " for event in handleEvent() + " + details);
                            result = 1;
                        }
                    }
                }
            }
            catch (System.Exception err)
            {
                if (err.InnerException != null)
                    SendFatalError(err.InnerException.Message);
                else
                    SendFatalError(err.Message + " for event " + messageData.Name + " in handleEvent()");
                result = 1;
            }
            lastEventPublisher = 0;
            return result;
        }
        // ----------------------------------------------
        /// <summary>
        /// Add an owned property to this component. This will 
        /// exist in a list that is not deregistered when Plant2
        /// deregisters it's properties at endcrop.
        /// See onQueryValue() for accessing this value.
        /// </summary>
        /// <param name="sName"></param>
        /// <param name="sDDML"></param>
        /// <param name="read"></param>
        /// <param name="write"></param>
        /// <param name="init"></param>
        /// <param name="sDescr"></param>
        /// <param name="sFullDescr"></param>
        // ----------------------------------------------
        public int RegisterProperty(String sName, String sDDML, Boolean read, Boolean write, Boolean init, String sDescr, String sFullDescr, propertyDelegate pDeleg)
        {
            int RegistrationIndex = Host.propertyCount(); //Registrations.Count;
            TExtendedPropertyInfo property = new TExtendedPropertyInfo(sDDML, sDescr, sFullDescr, "", pDeleg);
            property.Name = sName;
            RegistrationsPropStatic.Add(RegistrationIndex, property);
            Host.addProperty(sName, RegistrationIndex, true, false, false, sDDML, sDescr, sFullDescr);

            return RegistrationIndex;
        }
        // ----------------------------------------------
        /// <summary>
        /// Look for all properties and register them.
        /// </summary>
        // ----------------------------------------------
        public void RegisterAllProperties()
        {
            Boolean doReg = true;
            for (int i = 0; i != Fact.Properties.Count; i++)
            {
                FactoryProperty Property = Fact.Properties[i];
                doReg = true;
                if (IsPlant && (String.Compare(Property.Name, "plant_status", true) == 0))  //don't (re)register plant_status for Plant2
                    doReg = false;
                if (Property.IsOutput && doReg)
                {
                    int RegistrationIndex = Host.propertyCount(); //Registrations.Count;
                    RegistrationsProp.Add(RegistrationIndex, Property);
                    Host.addProperty(Property.OutputName, RegistrationIndex, !Property.WriteOnly, !Property.ReadOnly, false, Property.DDML(), "", "");
                }
            }
        }
        // ----------------------------------------------
        /// <summary>
        /// Look for all event handlers and register them. 
        /// Avoid registering EndCrop because they are routed from here
        /// when the ENDCROPINDEX event is handled by onEndCrop(). This avoids
        /// having ambiguous EndCrop event handlers in the registry when the Manager sends
        /// EndCrop published event.
        /// </summary>
        // ----------------------------------------------
        public void RegisterAllEventHandlers()
        {
            // Dont allow prepare registrations to go to ProtocolManager because we've already 
            // registered for this. The ProtocolManager has a problem where: when a plant2 crop 
            // is sown during prepare of the manager model, it registeres for prepare. The PM 
            // won't call the crops prepare handler until the day after sowing ie. it doesn't
            // see new prepare event subscriptions while in the middle of propagating 
            // the prepare event to all modules. Because of this, the class handles prepare
            // separately.

            for (int i = 0; i != Fact.EventHandlers.Count; i++)
                if (!string.Equals(Fact.EventHandlers[i].EventName, "EndCrop", StringComparison.CurrentCultureIgnoreCase) &&
                    !string.Equals(Fact.EventHandlers[i].EventName, "Prepare", StringComparison.CurrentCultureIgnoreCase))
                {
                    ExpandEventName(Fact.EventHandlers[i]);
                    Subscribe(Fact.EventHandlers[i]);
                }
        }
        // ----------------------------------------------
        /// <summary>
        /// Go through all [input] variables in model
        /// and get the most recent values from the simulation.
        /// </summary>
        // ----------------------------------------------
        public void GetAllInputs()
        {
            foreach (FactoryProperty Property in Fact.Properties)
            {
                if (Property.IsInput)
                {
                    //bool found = false;
                    int driverId;
                    if (ReverseRegDriver.TryGetValue(Property, out driverId))
                    {
                        Host.sendDriverRequest(driverId, 0);
                    }
                    else
                    //foreach (KeyValuePair<int, FactoryProperty> pair in RegistrationsDriver)
                    //{
                    //    if (pair.Value.Equals(Property))
                    //    {
                    //        Host.sendDriverRequest(pair.Key, 0);
                    //        found = true;
                    //    }
                    //}
                    //if (!found)
                    {
                        int RegistrationIndex = Host.driverCount(); //Registrations.Count;
                        RegistrationsDriver.Add(RegistrationIndex, Property);
                        ReverseRegDriver.Add(Property, RegistrationIndex);
                        Host.registerDriver(Property.Name, RegistrationIndex, Property.OptionalInput ? 0 : 1, 1, Property.Units, false, Property.DDML(), Property.Name, "", 0); //register it
                        Host.sendDriverRequest(RegistrationIndex, 0);
                    }
                }
            }
        }
        // ----------------------------------------------
        /// <summary>
        /// Assign the returned driver value.
        /// </summary>
        /// <param name="driverID"></param>
        /// <param name="providerID"></param>
        /// <param name="aValue"></param>
        /// <returns></returns>
        // ----------------------------------------------
        public Boolean assignDriver(int driverID, uint providerID, TTypedValue aValue)
        {
            Boolean result = false;
            FactoryProperty Property;
            ApsimType Data;
            if (RegistrationsDriver.TryGetValue(driverID, out Property))
            {
                Property.setValue(aValue);
                result = true;
            }
            else if (RegistrationsDriverExtra.TryGetValue(driverID, out Data))
            {
                ((TypeInterpreter)Data).setValue(aValue);
                result = true;
            }
            DriverFound = true;
            return result;
        }
        // ----------------------------------------------
        /// <summary>
        /// Do a getvalue for a specified ApsimType variable. Register the driving
        /// variable if it has not been registered before.
        /// </summary>
        /// <param name="PropertyName"></param>
        /// <param name="Data"></param>
        // ----------------------------------------------
        public Boolean Get(String PropertyName, ApsimType Data, bool isOptional)
        {
            //search for any driver that is registered with this PropertyName
            int RegistrationIndex = 0;
            if (!RegistrationsDriverExtraLookup.TryGetValue(PropertyName.ToLower(), out RegistrationIndex))
            {
                // If it's not already registered, add registration now
                RegistrationIndex = Math.Max(1, Host.driverCount());
                Host.registerDriver(PropertyName, RegistrationIndex, isOptional ? 0 : 1, 1, "", false, Data.DDML(), PropertyName, "", 0); //register it
                RegistrationsDriverExtraLookup.Add(PropertyName.ToLower(), RegistrationIndex);
            }
            // Set the data destination so AssignDriver will know where to put it
            RegistrationsDriverExtra[RegistrationIndex] = Data;
            DriverFound = false;
            Host.sendDriverRequest(RegistrationIndex, 0);
            //no need to deregister now 
            return DriverFound && Data != null;  //true if no errors
        }
        // ----------------------------------------------
        /// <summary>
        /// Do a requestset
        /// </summary>
        /// <param name="PropertyName">Name of the property</param>
        /// <param name="Data">The value data</param>
        // ----------------------------------------------
        public bool Set(String PropertyName, ApsimType Data)
        {
            //a list here might be used to cache the DDMLValue and avoid re-parsing.
            //this would be useful to speed it up if Set() is used frequently
            TDDMLValue dataVal = new TDDMLValue(Data.DDML(), "");
            byte[] msgData;
            Data.pack(out msgData);
            dataVal.setData(msgData, msgData.Length, 0);
            return Host.doSet(PropertyName, Data.DDML(), dataVal);    //checks for previously registered setters
        }
        // ----------------------------------------------
        /// <summary>
        /// Publish an event
        /// </summary>
        /// <param name="EventName"></param>
        /// <param name="Data"></param>
        // ----------------------------------------------
        public void Publish(String EventName, ApsimType Data)
        {
            int RegistrationIndex = Host.getEventID(EventName, TypeSpec.KIND_PUBLISHEDEVENT);
            if (RegistrationIndex < 0)  //if not registered yet
            {
                RegistrationIndex = Host.eventCount();
                RegistrationsEvent.Add(RegistrationIndex, (ApsimType)Data);
                Host.registerEvent(EventName, Data.DDML(), RegistrationIndex, TypeSpec.KIND_PUBLISHEDEVENT, 0, 0);
            }
            byte[] msgData;
            Data.pack(out msgData);
            Host.sendPublishEvent(RegistrationIndex, msgData, false);
        }
        // ------------------------------------------------------------------------
        /// <summary>
        /// Registers all the event handlers in the cmp component. 
        /// </summary>
        /// <param name="Event">Event to be considered</param>
        // ------------------------------------------------------------------------
        public void ExpandEventName(EvntHandler Event)
        {
            string name = Event.EventName;
            int openPos = name.IndexOf('[');
            int closePos = name.IndexOf(']');
            while (openPos >= 0 && closePos > openPos)
            {
                string target = name.Substring(openPos + 1, closePos - openPos - 1);
                string newText = "";
                for (int i = 0; i != Fact.Properties.Count; i++)
                {
                    FactoryProperty prop = Fact.Properties[i];
                    if (prop.IsParam && prop.HasAsValue && prop.Name == target)
                    {
                        newText = prop.Get.ToString();
                        break;
                    }
                }
                if (newText == "")
                    Warning("No substitution string available for [" + target + "]");
                name = name.Remove(openPos, closePos - openPos + 1);
                name = name.Insert(openPos, newText);
                openPos = name.IndexOf('[');
                closePos = name.IndexOf(']');
                Event.EventName = name;
            }
        }
        // ------------------------------------------------------------------------
        /// <summary>
        /// Registers all the event handlers in the cmp component. 
        /// </summary>
        /// <param name="Event"></param>
        // ------------------------------------------------------------------------
        public void Subscribe(EvntHandler Event)
        {
            int RegistrationIndex = Host.eventCount();
            registerEvent(Event, Event.EventName, Event.DDML(), RegistrationIndex, TypeSpec.KIND_SUBSCRIBEDEVENT, 0, 0);
        }
        // ------------------------------------------------------------------------
        /// <summary>
        /// Register an event. Add it to the local list of event handlers and then register it.
        /// </summary>
        /// <param name="Event">The event handler. This is the event handler in the specific cmp component. 
        /// If the event is handled here first then use null.</param>
        /// <param name="sName"></param>
        /// <param name="sDDML"></param>
        /// <param name="iEventID"></param>
        /// <param name="iKind"></param>
        /// <param name="destID"></param>
        /// <param name="toAck"></param>
        // ------------------------------------------------------------------------
        public void registerEvent(EvntHandler Event, string sName, string sDDML, int iEventID,
                             int iKind, uint destID,
                             int toAck)
        {
            RegistrationsEvent.Add(iEventID, Event);
            Host.registerEvent(sName, sDDML, iEventID, iKind, destID, toAck);
        }
        // ------------------------------------------------------------------------
        /// <summary>
        /// Look for all event publishers and register them.
        /// </summary>
        // ------------------------------------------------------------------------
        public void TrapAllEvents()
        {
            int eventID = Host.eventCount();
            for (int i = 0; i != Fact.Events.Count; i++)
            {
                FactoryEvent Event = Fact.Events[i];
                Event.ID = eventID;
                RegistrationsEvent.Add(Event.ID, Event);
                Host.addEvent(Event.EventName, Event.ID, TypeSpec.KIND_PUBLISHEDEVENT, Event.DDML(), "", "", 0);
                eventID++;
                Event.OnFired += new FactoryEvent.FactoryEventFired(OnPublish);
            }
        }
        // ----------------------------------------------
        /// <summary>
        /// The model has published an event - go and publish it to APSIM.
        /// </summary>
        /// <param name="Event"></param>
        // ----------------------------------------------
        public void OnPublish(FactoryEvent Event)
        {
            //ensures the event msg data is copied into the Host eventList[]
            byte[] msgData;
            Event.Data.pack(out msgData);
            Host.sendPublishEvent(Event.ID, msgData, true);
        }
        // ----------------------------------------------
        /// <summary>
        /// Build all objects based on the XML passed in.
        /// </summary>
        /// <param name="XML"></param>
        // ----------------------------------------------
        public void BuildObjects(XmlNode XML, Assembly modelAssembly)
        {
            Fact.Clear();   //clear this so it can be reinitialised
            Fact.Create(XML.OuterXml, modelAssembly, this);
            ModelInstance = (Instance)(Fact.Root);
            if (Init2Received)
            {
                GetAllInputs();
                Fact.Initialise();  //resolve links can only happen after init2 
            } 
            String InstanceName = Name;
            if (InstanceName.Contains("."))
            {
                InstanceName = InstanceName.Substring(InstanceName.LastIndexOf('.') + 1);
            }
            ModelInstance.Name = InstanceName;
            RegisterAllProperties();
            Fact.CheckParameters(this);
            RegisterAllEventHandlers();
            TrapAllEvents();
        }
        // ----------------------------------------------
        /// <summary>
        /// Perform all instructions found in the
        /// specified XML node.
        /// </summary>
        /// <param name="Node"></param>
        /// <param name="ModelDescription"></param>
        // ----------------------------------------------
        private void PerformInstructions(XmlNode Node, ref XmlNode ModelDescription)
        {
            foreach (XmlNode Instruction in Node.ChildNodes)
            {
                if (Instruction.Name == "Construct")
                {
                    ModelDescription = XmlHelper.Find(InitData, Instruction.InnerText);
                    if (ModelDescription == null)
                        throw new Exception("Cannot find referenced node: " + Instruction.InnerText);
                }
                if (Instruction.Name == "ConstructModel")
                {
                    if (XmlHelper.ChildNodes(Instruction, "").Count == 0)
                    {
                        if (ModelDescription == null)
                            throw new Exception("Cannot find referenced node: " + Instruction.InnerText);
                    }
                    ModelDescription = XmlHelper.ChildNodes(Instruction, "")[0];
                }
                else if (Instruction.Name == "Goto")
                {
                    XmlNode ReferencedNode = XmlHelper.Find(InitData, Instruction.InnerText);
                    if (ReferencedNode == null)
                        throw new Exception("Cannot find cultivar node: " + Instruction.InnerText);
                    PerformInstructions(ReferencedNode, ref ModelDescription); // recursion
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
        }

        /// <summary>
        /// Perform an override on the specified modeldescription using the specified instruction.
        /// </summary>
        private static void Override(XmlNode ModelDescription, XmlNode Instruction)
        {
            String ReferencedNodeName = XmlHelper.Name(Instruction).Replace(".", "/");
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
        // ----------------------------------------------
        /// <summary>
        /// A sowing event has occurred.
        /// </summary>
        /// <param name="messageData"></param>
        // ----------------------------------------------
        public void OnSow(TTypedValue messageData)
        {
            SowPlant2Type Sow = new SowPlant2Type();
            Sow.unpack(messageData);

            if (Sow.Cultivar == null)
                throw new Exception("Cannot find cultivar on sow line");

            if (ModelInstance != null)
                throw new Exception("Plant already in ground while trying to sow another crop");

            // Now locate the cultivar parameter section and perform all instructions found there.
            XmlNode CultivarNode = XmlHelper.Find(InitData, Sow.Cultivar);
            if (CultivarNode == null)
                throw new Exception("Cannot find cultivar information for: " + Sow.Cultivar);
            XmlNode ModelDescription = null;
            PerformInstructions(CultivarNode, ref ModelDescription);

            // Insert any other parameters found under the <InitData> section into the model description.
            foreach (XmlNode Parameter in InitData.ChildNodes)
            {
                if (XmlHelper.ChildNodes(Parameter, "").Count == 0)
                {
                    // simple parameter - insert into model description
                    InsertParameterIntoModel(Parameter, ModelDescription);
                }
            }

            //XmlHelper.SetName(ModelDescription, TRegistrar.unQualifiedName(Name));
            BuildObjects(ModelDescription, modelAssembly);
            GetAllInputs();
            CallEventHandlers("Sow", Sow);  //call multiple Sow events
        }
        //============================================================================
        /// <summary>
        /// Call each event handler with the name EventName (of SowPlant2Type) (should this be ApsimType?).
        /// </summary>
        /// <param name="EventName">Name of the event.</param>
        /// <param name="Data">The message data.</param>
        //============================================================================
        public void CallEventHandlers(String EventName, SowPlant2Type Data)
        {
            try
            {
                for (int i = 0; i != Fact.EventHandlers.Count; i++)
                {
                    EvntHandler Event = Fact.EventHandlers[i];
                    if (String.Compare(Event.EventName, EventName, true) == 0)
                        Event.Invoke(Data);
                }
            }
            catch (Exception err)
            {
                if (err.InnerException != null)
                    SendFatalError(err.InnerException.Message + "\n");
                else
                    SendFatalError(err.Message + "\n");
            }
        }
        //============================================================================
        /// <summary>
        /// ?
        /// </summary>
        /// <param name="Parameter"></param>
        /// <param name="Node"></param>
        //============================================================================
        public Boolean InsertParameterIntoModel(XmlNode Parameter, XmlNode Node)
        {
            Boolean found = false;
            if (Node.Name.Equals(Parameter.Name, StringComparison.OrdinalIgnoreCase))
            {
                Node.InnerText = Parameter.InnerText;
                found = true;
            }
            else
            {
                int i = 0;
                while (!found && (i < Node.ChildNodes.Count))
                {
                    XmlNode Child = Node.ChildNodes[i];
                    found = InsertParameterIntoModel(Parameter, Child);
                    i++;
                }
                //NH - replaced this code 
                // foreach (XmlNode Child in Node.ChildNodes)
                //     InsertParameterIntoModel(Parameter, Child);
            }
            return found;
        }
        // ----------------------------------------------
        /// <summary>
        /// A harvest event has occurred.
        /// </summary>
        /// <param name="messageData"></param>
        // ----------------------------------------------
        public void OnEndCrop(TTypedValue messageData)
        {
            EndCropToday = true;
            CallEventHandlers("EndCrop", null);  //call multiple EndCrop events
        }
        // ----------------------------------------------
        /// <summary>
        /// A Post message has been received. See if we need to do an endcrop.
        /// </summary>
        /// <param name="messageData"></param>
        // ----------------------------------------------
        public void OnPost(TTypedValue messageData)
        {
            if (EndCropToday)
            {
                EndCropToday = false;
                //deregister all specific Plant2 properties and events
                foreach (KeyValuePair<int, FactoryProperty> pair in RegistrationsDriver)
                {
                    Host.delDriver(pair.Key);
                }
                RegistrationsDriver.Clear();
                ReverseRegDriver.Clear();
                //Events
                List<int> keys = new List<int>();   //list of event items to be removed
                foreach (KeyValuePair<int, ApsimType> pair in RegistrationsEvent)
                {
                    //don't unregister the Sow event
                    if ((pair.Key != SOWINDEX) && (pair.Key != ENDCROPINDEX) && (pair.Key != POSTINDEX) && (pair.Key != PREPAREINDEX))
                    {
                        keys.Add(pair.Key);
                    }
                }
                //now remove the registrations from the dictionary
                for (int i = 0; i < keys.Count; i++)
                {
                    RegistrationsEvent.Remove(keys[i]);
                    Host.delEvent(keys[i]);
                }
                //Properties
                foreach (KeyValuePair<int, FactoryProperty> pair in RegistrationsProp)
                {
                    Host.delProp(pair.Key);
                }
                RegistrationsProp.Clear();
                ModelInstance = null;
            }
        }

        // ----------------------------------------------
        /// <summary>
        /// A Post message has been received. See if we need to do an endcrop.
        /// </summary>
        /// <param name="messageData"></param>
        // ----------------------------------------------
        public void OnPrepare(TTypedValue messageData)
        {
            if (ModelInstance != null)
            {
                GetAllInputs();
                CallEventHandlers("Prepare", null);
            }

        }

        // -----------------------------------------------------------------------
        /// <summary>
        /// Handler for all QueryValue messages.
        /// </summary>
        /// <param name="propertyID">Registration ID of the property</param>
        /// <param name="requestorID">Requesting entity</param>
        /// <param name="aValue">Return value is stored here.</param>
        /// <returns></returns>
        // -----------------------------------------------------------------------
        public Boolean onQueryValue(int propertyID, uint requestorID, ref TPropertyInfo aValue)
        {
            Boolean result = false;
            FactoryProperty Property;
            if (RegistrationsProp.TryGetValue(propertyID, out Property))
            {
                Property.pack(aValue);
                result = true;
            }
            else
            {
                //the propertyID wasn't found in the model instance's registered list so search the list
                //of 'static' properties that may have been registered in this class.
                TExtendedPropertyInfo staticProperty;
                if (RegistrationsPropStatic.TryGetValue(propertyID, out staticProperty))
                {
                    if (staticProperty.pDelegate != null)
                      result = staticProperty.pDelegate.Invoke(propertyID, ref aValue, false);
                    else
                      result = ReadStaticProperty("plant_status", staticProperty, aValue);
                }
            }
            return result;
        }
        // -----------------------------------------------------------------------
        /// <summary>
        /// When requesting a value of an owned property in onQueryValue() and it is not found in the 
        /// normally registered list of model instance properties, this function will attempt to set it
        /// from a local value in the RegistrationsPropStatic list after firstly checking the
        /// model instance.fact list of properties. Plant2 is stopped from registering plant_status.
        /// </summary>
        /// <param name="sName">Name of the property</param>
        /// <param name="staticProperty">The local property found in RegistrationsPropStatic list</param>
        /// <param name="value">The return value is stored here.</param>
        /// <returns>True if the return value has been set.</returns>
        // -----------------------------------------------------------------------
        protected Boolean ReadStaticProperty(String sName, TPropertyInfo staticProperty, TPropertyInfo value)
        {
            Boolean result = false;
            if (String.Compare(staticProperty.Name, sName, true) == 0)
            {
                Boolean found = false;
                if (ModelInstance != null)                                              //if the instance exists (plant2)
                {
                    //attempt to find a matching property in the instance
                    int i = 0;
                    while (!found && (i != Fact.Properties.Count))
                    {
                        if ((String.Compare(Fact.Properties[i].Name, sName, true) == 0) && (Fact.Properties[i].IsOutput))
                        {
                            Fact.Properties[i].pack(value);
                            result = true;
                            found = true;
                        }
                        i++;
                    }
                }
                else
                {
                    //use a local value for this property
                    if (!found)
                    {
                        value.setValue(staticProperty);
                        result = true;
                    }
                }
            }
            return result;
        }
        // -----------------------------------------------------------------------
        /// <summary>
        /// Handler for all QuerySetValue messages.
        /// </summary>
        /// <param name="driverID"></param>
        /// <param name="providerID"></param>
        /// <param name="aValue"></param>
        /// <returns></returns>
        // -----------------------------------------------------------------------
        public Boolean onQuerySetValue(int propertyID, TTypedValue aValue)
        {
            Boolean result = false;
            FactoryProperty Property;
            if (RegistrationsProp.TryGetValue(propertyID, out Property))
            {
                Property.setValue(aValue);
                result = true;
            }
            return result;
        }
        // -----------------------------------------------------------------------
        /// <summary>
        /// Use queryInfo message to find all the sibling components for the one
        /// specified by FQN. Populate the SiblingComponents list.
        /// </summary>
        /// <param name="FQN"></param>
        /// <returns></returns>
        // -----------------------------------------------------------------------
        public int querySiblingComponents(String FQN)
        {
            String sSearchName = "*";
            if (FQN.Contains("."))                                                //if this component has a parent then
                sSearchName = FQN.Substring(0, FQN.LastIndexOf('.')) + ".*";    //search parent.*

            List<TComp> comps = new List<TComp>();
            Host.queryCompInfo(sSearchName, TypeSpec.KIND_COMPONENT, ref comps);
            SiblingComponents.Clear();
            for (int i = 0; i < comps.Count; i++)
            {
                string sReturnName = comps[i].name;
                String buf = sReturnName;
                int numOfOccurence = sReturnName.Length - buf.Replace(".", "").Length;
                buf = FQN;
                int numOfOccurReq = FQN.Length - buf.Replace(".", "").Length;
                if (numOfOccurence == numOfOccurReq)       //ensure sibling of the request string
                    SiblingComponents.Add(comps[i].compID, comps[i]);
            }
            return SiblingComponents.Count;
        }
        // -----------------------------------------------------------------------
        /// <summary>
        /// Accessing property of the TAbstractComponent
        /// </summary>
        // -----------------------------------------------------------------------
        public String CompClass
        {
            get { return Host.CompClass; }
            set {Host.CompClass = value; }
        }
        // -----------------------------------------------
        /// <summary>
        /// Called by probe tool to return info about model
        /// </summary>
        /// <param name="InitD"></param>
        /// <returns></returns>
        // -----------------------------------------------
        public String GetDescription(XmlNode InitD)
        {
            InitData = InitD;
            String Name = XmlHelper.Name(InitData.ParentNode);
            if (DllFileName.Length < 1)
                DllFileName = XmlHelper.Attribute(InitData.ParentNode, "executable");
            StringBuilder Desc = new StringBuilder("<describecomp>\r\n");

            String company = "unknown";
            String product = Name;
            String version = "1.0";
            Assembly assembly = Assembly.LoadFrom(DllFileName);
            if (assembly != null)
            {
                object[] attributes = assembly.GetCustomAttributes(typeof(AssemblyCompanyAttribute), false);
                if (attributes.Length > 0)
                    company = (attributes[0] as AssemblyCompanyAttribute).Company;
                if (CompClass.Length == 0)
                {
                    attributes = assembly.GetCustomAttributes(typeof(AssemblyProductAttribute), false);
                    if (attributes.Length > 0)
                        product = (attributes[0] as AssemblyProductAttribute).Product;
                }
                else
                    product = CompClass;
                version = assembly.GetName().Version.Major.ToString() + "." + assembly.GetName().Version.Minor.ToString();
            }
            Desc.Append("   <executable>" + DllFileName + "</executable>\r\n");
            Desc.Append("   <class>" + product + "</class>\r\n");
            Desc.Append("   <version>" + version + "</version>\r\n");
            Desc.Append("   <author>" + company + "</author>\r\n");

            XmlNode ModelDescription = null;
            XmlNode CultivarNode = XmlHelper.FindByType(InitData, "Cultivar");
            if (CultivarNode != null)
                PerformInstructions(CultivarNode, ref ModelDescription);
            else if (InitData.ChildNodes.Count > 0)
                ModelDescription = InitData.ChildNodes[0];  //always expects the Model node at [0] !

            if (ModelDescription != null)
            {
                Fact = new Factory();
                Fact.Create(ModelDescription.OuterXml, modelAssembly, this);
                ModelInstance = (Instance)(Fact.Root);

                // get description for all properties.
                for (int i = 0; i != Fact.Properties.Count; i++)
                {
                    String St = Fact.Properties[i].GetDescription();
                    if (St != "")
                        Desc.Append(St);
                }
                List<String> EventsAlreadyProcessed = new List<String>();

                // get description for all events.
                for (int i = 0; i != Fact.Events.Count; i++)
                {
                    if (!EventsAlreadyProcessed.Contains(Fact.Events[i].EventName))
                    {
                        String St = Fact.Events[i].GetDescription();
                        if (St != "")
                            Desc.Append(St);
                        EventsAlreadyProcessed.Add(Fact.Events[i].EventName);
                    }
                }

                // EventsAlreadyProcessed.Clear();

                // get description for all event handlers.
                for (int i = 0; i != Fact.EventHandlers.Count; i++)
                {
                    if (!EventsAlreadyProcessed.Contains(Fact.EventHandlers[i].EventName))
                    {
                        String St = Fact.EventHandlers[i].GetDescription();
                        if (St != "")
                            Desc.Append(St);
                        EventsAlreadyProcessed.Add(Fact.EventHandlers[i].EventName);
                    }
                }

            }
            Desc.Append("</describecomp>\r\n");
            return Desc.ToString();
        }
        // -----------------------------------------------
        /// <summary>
        /// When the domain fails to load a referenced assembly this event
        /// is called. Attempt to find the assembly from known paths.
        /// This is used particularly when the in-memory compiled Script is trying to resolve it's references.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="args"></param>
        /// <returns>The assembly that was found.</returns>
        // -----------------------------------------------
        public Assembly currentDomain_AssemblyResolve(object sender, ResolveEventArgs args)
        {
            //This handler is called only when the common language runtime tries to bind to the assembly and fails.
            //Retrieve the list of referenced assemblies in an array of AssemblyName.
            Assembly MyAssembly, ExecutingAssembly;
            String sAssemblyPath = "";

            ExecutingAssembly = Assembly.GetExecutingAssembly();
            String argDllName = args.Name.Substring(0, args.Name.IndexOf(",")) + ".dll";
            //see if it matches this assembly
            if (ExecutingAssembly.FullName.Substring(0, ExecutingAssembly.FullName.IndexOf(",")) == args.Name.Substring(0, args.Name.IndexOf(",")))
            {
                sAssemblyPath = Path.Combine(Path.GetDirectoryName(ExecutingAssembly.Location), argDllName);
            }
            //if it is not this assembly then look in the same path as this assembly
            if (sAssemblyPath.Length == 0)
            {
                String sSearchName = Path.Combine(Path.GetDirectoryName(ExecutingAssembly.Location), argDllName);
                if (File.Exists(sSearchName))
                {
                    sAssemblyPath = sSearchName;
                }
                else
                {
                    sSearchName = Path.Combine(Configuration.ApsimBinDirectory(), argDllName);
                    if (File.Exists(sSearchName))
                    {
                        sAssemblyPath = sSearchName;
                    }
                }
            }
			
            if (sAssemblyPath != "")
                MyAssembly = Assembly.LoadFrom(sAssemblyPath);  //Load the assembly from the specified path.
            else
                MyAssembly = null;
			
            return MyAssembly;                              //Return the loaded assembly.
        }
    }
}