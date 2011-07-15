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
    // --------------------------------------------------------------------
    /// <summary>
    /// This is the main host for an Apsim component such as Plant2.
    /// </summary>
    // --------------------------------------------------------------------
    public class ApsimComponent : Instance
    {
        private bool IsScript;
        private bool IsPlant;
        private Instance Model;
        private StringBuilder Contents;
        private Assembly modelAssembly;
        private bool Init1;
        public bool Init2Received;
        private Factory Fact;
        private int instanceNumber;
        public uint ComponentID;
        //private String Name;
        private String DllFileName;
        private Dictionary<int, FactoryProperty> RegistrationsProp;
        private Dictionary<int, FactoryProperty> RegistrationsDriver;
        private Dictionary<FactoryProperty, int> ReverseRegDriver;
        private Dictionary<int, ApsimType> RegistrationsDriverExtra;
        private Dictionary<String, int> RegistrationsDriverExtraLookup; //allows the lookup by name for the Get() function
        private Dictionary<int, ApsimType> RegistrationsEvent;
        private Dictionary<int, ApsimType> RegistrationsSet;
        public Dictionary<uint, TComp> SiblingComponents;  //includes itself
        public Dictionary<uint, TComp> SeniorComponents;   //parent and parent's siblings
        public String CompClass;
        private XmlNode InitData;
        private bool EndCropToday;

        protected Boolean haveWrittenToStdOutToday;
        protected TimeType tick;
        protected TAPSIMHost Host;

        //Event id's for events that are handled in this class
        public const int INIT2INDEX = 9999999;
        protected int SOWINDEX = Int32.MaxValue;        //ensure these are unique/unused values
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
            RegistrationsProp = new Dictionary<int, FactoryProperty>();
            RegistrationsDriver = new Dictionary<int, FactoryProperty>();
            ReverseRegDriver = new Dictionary<FactoryProperty, int>();
            RegistrationsDriverExtra = new Dictionary<int, ApsimType>();
            RegistrationsDriverExtraLookup = new Dictionary<String, int>();
            RegistrationsEvent = new Dictionary<int, ApsimType>();
            RegistrationsSet = new Dictionary<int, ApsimType>();
            SiblingComponents = new Dictionary<uint, TComp>();  //id, comp
            SeniorComponents = new Dictionary<uint, TComp>();  //id, comp
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
                    InitData = XmlHelper.Find(Doc.DocumentElement, "initdata");
                    IsPlant = (XmlHelper.FindByType(InitData, "Plant") != null);
                    IsScript = (XmlHelper.FindByType(InitData, "text") != null);

                    if (IsScript)
                    {
                        String ScriptClassName = "";
                        Assembly CompiledAssembly = CompileScript(XmlHelper.FindByType(InitData, "text"));
                        modelAssembly = CompiledAssembly;
                        foreach (Type t in CompiledAssembly.GetTypes())
                        {
                            if ((t.BaseType != null) && (t.BaseType.Name == "Instance"))
                                ScriptClassName = t.Name;
                        }
                        if (ScriptClassName == "")
                            throw new Exception("Cannot find a script class inherited from 'Instance'");

                        // Create an XML model that we can pass to BuildObjects.
                        XmlDocument NewDoc = new XmlDocument();
                        XmlNode ScriptNode = NewDoc.AppendChild(NewDoc.CreateElement(ScriptClassName));
                        XmlNode UINode = XmlHelper.Find(InitData, "ui");
                        if (UINode != null)
                        {
                            foreach (XmlNode Child in XmlHelper.ChildNodes(UINode, ""))
                            {
                                if (XmlHelper.Attribute(Child, "type").ToLower() != "category")
                                    XmlHelper.SetValue(ScriptNode, Child.Name, Child.InnerText);
                            }
                        }
                        // Build all necessary objects
                        BuildObjects(ScriptNode);
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
                        BuildObjects(InitData.ChildNodes[0]);
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
        public int handleEvent(int RegistrationIndex, byte[] messageData)
        {
            int result = 0;
            try
            {
                if (RegistrationIndex == INIT2INDEX)
                {
                    if (!IsPlant)                   //plant will do this at sow time in BuildObjects()
                        Fact.Initialise();
                    Init2Received = true;
                    if (Model != null)
                    {
                        Console.WriteLine();
                        Console.Write("------- " + Model.Name + " Initialisation ");
                        int col = Model.Name.Length + 24;
                        String line = new String('-', 80 - col - 1);
                        Console.Write(line);
                        Console.WriteLine();
                    }
                    for (int i = 0; i != Fact.EventHandlers.Count; i++)
                    {
                        EvntHandler Event = Fact.EventHandlers[i];
                        if (String.Compare(Event.EventName, "Init2") == 0)
                            Event.Invoke(messageData);
                    }
                }
                else if (RegistrationIndex == SOWINDEX)
                    OnSow(messageData);
                else if (RegistrationIndex == ENDCROPINDEX)
                    OnEndCrop(messageData);
                else if (RegistrationIndex == POSTINDEX)
                    OnPost(messageData);
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
                            ((FactoryEventHandler)Event).unpack(messageData);   //unpacks and Invoke()
                        }
                        catch (System.Exception err)
                        {
                            SendFatalError(err.InnerException.Message);
                            result = 1;
                        }
                    }
                }
            }
            catch (System.Exception err)
            {
                SendFatalError(err.Message);
                result = 1;
            }
            return result;
        }
        // ----------------------------------------------
        /// <summary>
        /// Look for all properties and register them.
        /// </summary>
        /// <param name="F"></param>
        // ----------------------------------------------
        public void RegisterAllProperties(Factory F)
        {
            for (int i = 0; i != F.Properties.Count; i++)
            {
                FactoryProperty Property = F.Properties[i];
                if (Property.IsOutput)
                {
                    int RegistrationIndex = Host.propertyCount(); //Registrations.Count;
                    RegistrationsProp.Add(RegistrationIndex, Property);
                    Host.addProperty(Property.OutputName, RegistrationIndex, true, true, false, Property.DDML(), "", "");
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
        /// <param name="F"></param>
        // ----------------------------------------------
        public void RegisterAllEventHandlers(Factory F)
        {
            for (int i = 0; i != F.EventHandlers.Count; i++)
                if (String.Compare(F.EventHandlers[i].EventName, "EndCrop", true) != 0)
                    Subscribe(F.EventHandlers[i]);
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
            Host.sendDriverRequest(RegistrationIndex, 0);
            //no need to deregister now 
            return (Data != null);  //true if no errors
        }
        // ----------------------------------------------
        /// <summary>
        /// Do a requestset
        /// </summary>
        /// <param name="PropertyName"></param>
        /// <param name="Data"></param>
        // ----------------------------------------------
        public void Set(String PropertyName, ApsimType Data)
        {
            ////NH - would be better to search through the list of registrations and not reregister
            int RegistrationIndex = RegistrationsSet.Count;
            RegistrationsSet.Add(RegistrationIndex, Data);
            ////       CISet(ComponentI, PropertyName, "", myCallback,
            ////             instanceNumber, RegistrationIndex, Data.DDML());
            // /////////        Registrations.RemoveAt(RegistrationIndex);
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
            int RegistrationIndex = Host.getEventID(EventName);
            if (RegistrationIndex < 0)  //if not registered yet
            {
                RegistrationIndex = Host.eventCount();
                RegistrationsEvent.Add(RegistrationIndex, (ApsimType)Data);
                Host.registerEvent(EventName, Data.DDML(), RegistrationIndex, TypeSpec.KIND_PUBLISHEDEVENT, 0, 0);
            }
            Host.sendPublishEvent(RegistrationIndex, false);
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
        /// <param name="F"></param>
        // ------------------------------------------------------------------------
        public void TrapAllEvents(Factory F)
        {
            int eventID = Host.eventCount();
            for (int i = 0; i != F.Events.Count; i++)
            {
                FactoryEvent Event = F.Events[i];
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
        public void BuildObjects(XmlNode XML)
        {
            Fact.Clear();   //clear this so it can be reinitialised
            Fact.Create(XML.OuterXml, modelAssembly, this);
            if (Init2Received)
                Fact.Initialise();  //resolve links can only happen after init2 
            Model = (Instance)(Fact.Root);
            String InstanceName = Name;
            if (InstanceName.Contains("."))
            {
                InstanceName = InstanceName.Substring(InstanceName.LastIndexOf('.') + 1);
            }
            Model.Name = InstanceName;
            RegisterAllProperties(Fact);
            RegisterAllEventHandlers(Fact);
            TrapAllEvents(Fact);
            Fact.CheckParameters();
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
                else if (Instruction.Name == "Goto")
                {
                    XmlNode ReferencedNode = XmlHelper.Find(InitData, Instruction.InnerText);
                    if (ReferencedNode == null)
                        throw new Exception("Cannot find cultivar node: " + Instruction.InnerText);
                    PerformInstructions(ReferencedNode, ref ModelDescription); // recursion
                }
                else if (Instruction.Name == "Override")
                {
                    String ReferencedNodeName = XmlHelper.Name(Instruction).Replace(".", "/");
                    XmlNode ReferencedNode = XmlHelper.Find(ModelDescription, ReferencedNodeName);
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
            }
        }
        // ----------------------------------------------
        /// <summary>
        /// A sowing event has occurred.
        /// </summary>
        /// <param name="messageData"></param>
        // ----------------------------------------------
        public void OnSow(byte[] messageData)
        {
            SowPlant2Type Sow = new SowPlant2Type();
            Sow.unpack(messageData);

            if (Sow.Cultivar == null)
                throw new Exception("Cannot find cultivar on sow line");

            if (Model != null)
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

            BuildObjects(ModelDescription);
            GetAllInputs();
            CallEventHandlers("Sow", Sow);  //call multiple Sow events
        }
        //============================================================================
        /// <summary>
        /// Call each event handler with the name EventName (of SowPlant2Type).
        /// </summary>
        /// <param name="EventName"></param>
        /// <param name="Data"></param>
        //============================================================================
        public void CallEventHandlers(String EventName, SowPlant2Type Data)
        {
            for (int i = 0; i != Fact.EventHandlers.Count; i++)
            {
                EvntHandler Event = Fact.EventHandlers[i];
                if (String.Compare(Event.EventName, EventName) == 0)
                    Event.Invoke(Data);
            }
        }
        /// <summary>
        /// ?
        /// </summary>
        /// <param name="Parameter"></param>
        /// <param name="Node"></param>
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
        public void OnEndCrop(byte[] messageData)
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
        public void OnPost(byte[] messageData)
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
                    if ((pair.Key != SOWINDEX) && (pair.Key != ENDCROPINDEX) && (pair.Key != POSTINDEX))
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
                Model = null;
            }
        }
        // -----------------------------------------------------------------------
        /// <summary>
        /// Handler for all QueryValue messages.
        /// </summary>
        /// <param name="propertyID"></param>
        /// <param name="requestorID"></param>
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
        /// Compile the script. Can be VB or C#
        /// </summary>
        /// <param name="Node"></param>
        /// <returns></returns>
        // -----------------------------------------------------------------------
        public Assembly CompileScript(XmlNode Node)
        {
            // Get the language associated with the file extension.
            if (CodeDomProvider.IsDefinedExtension(".cs"))
            {
                bool VB = Node.InnerText.IndexOf("Inherits ") != -1;
                String language;
                if (VB)
                    language = CodeDomProvider.GetLanguageFromExtension(".vb");
                else
                    language = CodeDomProvider.GetLanguageFromExtension(".cs");

                if ((language.Length > 0) && CodeDomProvider.IsDefinedLanguage(language))
                {
                    CodeDomProvider provider = CodeDomProvider.CreateProvider(language);
                    if (provider != null)
                    {
                        CompilerParameters _params = new CompilerParameters();
                        _params.GenerateInMemory = true;            //in memory only
                        _params.TreatWarningsAsErrors = false;
                        _params.WarningLevel = 2;
                        _params.ReferencedAssemblies.Add("System.dll");
                        if (VB)
                            _params.ReferencedAssemblies.Add("Microsoft.VisualBasic.dll");
                        _params.ReferencedAssemblies.Add(Types.GetProbeInfoDLLFileName());
                        System.Reflection.Assembly currentAssembly = System.Reflection.Assembly.GetExecutingAssembly();
                        _params.ReferencedAssemblies.Add(currentAssembly.Location);   // Reference the current assembly from within dynamic one

                        foreach (string val in XmlHelper.ValuesRecursive(Node.ParentNode, "reference"))
                            if (File.Exists(val))
                                _params.ReferencedAssemblies.Add(val);
                            else if (File.Exists(RuntimeEnvironment.GetRuntimeDirectory() + val))
                                _params.ReferencedAssemblies.Add(RuntimeEnvironment.GetRuntimeDirectory() + val);
                            else
                                _params.ReferencedAssemblies.Add(Path.GetDirectoryName(DllFileName) + "\\" + val);

                        String[] source = new String[1];
                        source[0] = Node.InnerText;
                        CompilerResults results = provider.CompileAssemblyFromSource(_params, source);
                        String Errors = "";
                        foreach (CompilerError err in results.Errors)
                        {
                            if (Errors != "")
                                Errors += "\r\n";

                            Errors += err.ErrorText + ". Line number: " + err.Line.ToString();
                        }
                        if (Errors != "")
                            throw new Exception(Errors);

                        return results.CompiledAssembly;
                    }
                }
            }
            return null;
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

            Desc.Append("   <executable>" + DllFileName + "</executable>\r\n");
            Desc.Append("   <class>" + Name + "</class>\r\n");
            Desc.Append("   <version>1.0</version>\r\n");
            Desc.Append("   <author>APSRU</author>\r\n");

            XmlNode ModelDescription = null;
            XmlNode CultivarNode = XmlHelper.FindByType(InitData, "Cultivar");
            if (CultivarNode != null)
                PerformInstructions(CultivarNode, ref ModelDescription);
            else if (InitData.ChildNodes.Count > 0)
                ModelDescription = InitData.ChildNodes[0];

            if (ModelDescription != null)
            {
                Fact = new Factory();
                Fact.Create(ModelDescription.OuterXml, modelAssembly, this);
                Model = (Instance)(Fact.Root);

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
            MyAssembly = Assembly.LoadFrom(sAssemblyPath);  //Load the assembly from the specified path.

            return MyAssembly;                              //Return the loaded assembly.
        }
    }
}