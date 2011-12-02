using System;
using System.Collections;
using System.Reflection;
using System.Text;
using System.Xml;

namespace CMPServices
{
    //==============================================================================
    /// <summary>
    /// This is the required ancestor class for any Common Modelling Protocol
    /// compliant version of a logic component that uses the mixed mode piwrapper dll.
    /// This class is replaced by <see cref="TBaseComp"/> when using a fully managed code component
    /// without the piwrapper e.g. when using the fully .net version of the engine.
    /// </summary>
    //==============================================================================
    public abstract class TFarmwiseInstance : TAbstractComponent
    {
        //objects used in the reflection process to access the
        //wrapper object and it's methods
        private Assembly FWrapperAssembly;
        private System.Type FWrapperType;
        private Object FWrapObject;
        private MethodInfo FMiPassMessage;
        private MethodInfo FMiBuildMessage;
        private MethodInfo FMiSendMessage;
        private MethodInfo FMiGetChildComp;

        private TEventsManager eventsManager;     //Pointer to the events manager
        private string init1Error;                //temporary storage of error message from init1

        /// <summary>
        /// keeping track of drivers requested
        /// </summary>
        protected ArrayList driverIDList;         
        internal TCheckPointTracer checkPointTracer; //allows the following of the checkpoint process
        /// <summary>
        /// The object that is used for tracking the routing of messages.
        /// </summary>
        protected TMsgDirector msgDirector;       
        /// <summary>
        /// Ref to the wrapper
        /// </summary>
        protected IntPtr pWrapper;
        /// <summary>
        /// Checkpoint driver ID.
        /// </summary>
        protected int checkPointDriverID;

        //==============================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="pBaseCompWrapper">Pointer to the TBaseComponentWrapper from within the PI Wrapper module.</param>
        //==============================================================================
        protected TFarmwiseInstance(IntPtr pBaseCompWrapper)
            : base()
        {
            pWrapper = pBaseCompWrapper;
            FModulePathName = getModulePath();

            checkPointTracer = new TCheckPointTracer();
            checkPointDriverID = -1;
            msgDirector = new TMsgDirector();
        }
        //==============================================================================
        /// <summary>
        /// Usual method of construction for this object.
        /// </summary>
        /// <param name="pBaseCompWrapper">Pointer to the TBaseComponentWrapper from within the PI Wrapper module.</param>
        /// <param name="sType"></param>
        /// <param name="sVersion"></param>
        /// <param name="sAuthor"></param>
        //==============================================================================
        protected TFarmwiseInstance(IntPtr pBaseCompWrapper, string sType, string sVersion, string sAuthor)
            : base()
        {
            init1Error = "";
            pWrapper = pBaseCompWrapper;           //store the ptr to the managing wrapper in the class var
            FModulePathName = getModulePath();

            FType = sType;
            FVersion = sVersion;
            FAuthor = sAuthor;

            checkPointTracer = new TCheckPointTracer();
            checkPointDriverID = -1;
            msgDirector = new TMsgDirector();
            eventsManager = new TEventsManager(this);

            addEvent("error", EVTERROR, TypeSpec.KIND_PUBLISHEDEVENT, TypeSpec.typeERROR, "Error event", "", 0);    //Publish the standard "error" event
        }
        //==============================================================================
        /// <summary>
        /// Returns the full path and name of this dll.
        /// Child classes can reimplement this if other path info is required.
        /// </summary>
        /// <returns>The full path and name of this dll.</returns>
        //==============================================================================
        protected virtual string getModulePath()
        {
            return System.Reflection.Assembly.GetExecutingAssembly().Location;
        }
        //============================================================================
        /// <summary>
        /// Generate a "deregister" message.
        /// </summary>
        /// <param name="lKind">Kind of entity</param>
        /// <param name="lCodeID"></param>
        //============================================================================
        protected override void sendDeregistration(int lKind, int lCodeID)
        {
            //iWrapperID iMsgKind     DestComp iParam1 iParam2 iParam3 szParam1 szParam2 valPtr iValSize toAck
            PassMessage(pWrapper, Msgs.MSG_DEREGISTER, 0, lKind, lCodeID, 0, null, null, null, 0, 0);
        }
        //============================================================================
        /// <summary>
        /// Sends an error message to the subscribing components. When the error is
        /// fatal, the system will terminate the simulation.
        /// It is the responsibility of the protocol to handle terminating the           
        /// simulation in the case of a fatal error.
        /// </summary>
        /// <param name="sMessage">Message string</param>
        /// <param name="bFatal">True if fatal and system should terminate.</param>
        //============================================================================
        public override void sendError(String sMessage, bool bFatal)
        {
            uint newMsgID;
            IntPtr msg = new IntPtr(0);

            newMsgID = BuildMsg(pWrapper, Msgs.MSG_ERROR, FParentID, (bFatal ? 1 : 0), 0, 0, sMessage, null, null, 0, 0, ref msg);
            SendMsg(pWrapper, msg); 
        }
        //============================================================================
        /// <summary>
        /// Generate a "register" message.
        /// </summary>
        /// <param name="sName">Name of the entity to be registered.</param>
        /// <param name="sDDML">DDML type description of the entity.</param>
        /// <param name="lCodeID"></param>
        /// <param name="lKind">Kind of entity</param>
        /// <param name="destID">Attach the entity to this source</param>
        /// <param name="readable">Readable</param>
        /// <param name="writeable">Writeable</param>
        /// <param name="toAck">Acknowledge this registration 0/1.</param>
        //============================================================================
        protected override void sendRegistration(String sName, String sDDML, int lCodeID,
           int lKind, uint destID, bool readable, bool writeable, int toAck)
        {
            int lParam1;

            if (lKind != TypeSpec.KIND_OWNED_R)
                lParam1 = lKind;
            else if (readable && !writeable)
                lParam1 = TypeSpec.KIND_OWNED_R;
            else if (!readable && writeable)
                lParam1 = TypeSpec.KIND_OWNED_W;
            else if (readable && writeable)
                lParam1 = TypeSpec.KIND_OWNED_RW;
            else
            {
                string errorMsg = String.Format("{0}: Attempt to register property [{1}] that is neither readable nor writeable in sendRegistration()", FName, sName);
                throw (new ApplicationException(errorMsg));
            }
            //iWrapperID iMsgKind iDestComp iParam1 iParam2 iParam3 szParam1 szParam2 valPtr iValSize
            PassMessage(pWrapper, Msgs.MSG_REGISTER, 0, lParam1, lCodeID, destID, sName, sDDML, null, 0, toAck);
        }
        //==============================================================================
        /// <summary>
        /// Use reflection to get the win32/managed C++ wrapper object and also the methods
        /// of the object that will be called from here.
        /// Stores the values in class properties.
        /// </summary>
        //==============================================================================
        protected void getWrapperObject()
        {
            FWrapperAssembly = Assembly.LoadFrom(getModulePath());
            FWrapperType = FWrapperAssembly.GetType("PICompInt32.WrapperClass");

            if (FWrapperType.Equals(null) == false)
            {
                FWrapObject = Activator.CreateInstance(FWrapperType, null);
                FMiPassMessage = FWrapperType.GetMethod("PassMessage");
                FMiBuildMessage = FWrapperType.GetMethod("BuildMessage");
                FMiSendMessage = FWrapperType.GetMethod("SendMsg");
                FMiGetChildComp = FWrapperType.GetMethod("GetChildComp");
            }
        }
        //==============================================================================
        /// <summary>
        /// Calls a managed wrapper class in the mixed mode dll
        /// </summary>
        /// <param name="iWrapperID"></param>
        /// <param name="iMsgKind"></param>
        /// <param name="iDestComp"></param>
        /// <param name="iParam1"></param>
        /// <param name="iParam2"></param>
        /// <param name="iParam3"></param>
        /// <param name="sParam1"></param>
        /// <param name="sParam2"></param>
        /// <param name="valPtr"></param>
        /// <param name="iValSize"></param>
        /// <param name="toAck"></param>
        /// <returns></returns>
        //==============================================================================
        protected uint PassMessage(IntPtr iWrapperID, int iMsgKind, uint iDestComp, int iParam1, int iParam2, uint iParam3,
           string sParam1, string sParam2, byte[] valPtr, int iValSize, int toAck)
        {
            Object[] argArray = new Object[11];
            uint newMsgID = 0;

            try
            {
                try
                {
                    if (Convert.ReferenceEquals(FWrapObject, null) == true)
                    {
                        getWrapperObject();
                    }
                    //now call the passmessage()
                    if (Convert.ReferenceEquals(FMiPassMessage, null) == false)
                    {
                        argArray[0] = iWrapperID;
                        argArray[1] = (Object)iMsgKind;    //box these variables
                        argArray[2] = (Object)iDestComp;
                        argArray[3] = (Object)iParam1;
                        argArray[4] = (Object)iParam2;
                        argArray[5] = (Object)iParam3;
                        argArray[6] = sParam1;
                        argArray[7] = sParam2;
                        argArray[8] = (Object)valPtr; //matches unsigned char __gc []   parameter
                        argArray[9] = (Object)iValSize;
                        argArray[10] = (Object)toAck;
                        newMsgID = (uint)(FMiPassMessage.Invoke(FWrapObject, argArray));
                    }
                    else
                    {
                        throw (new System.NullReferenceException("External method PassMessage() set to null reference"));
                    }
                }
                catch (Exception e)
                {
                    Console.WriteLine(FName + ": Error in TFarmwiseInstance.PassMessage() " + e.Message);
                }
            }
            catch (Exception e)
            {
                Console.WriteLine(FName + ": Error in TFarmwiseInstance.PassMessage() " + e.Message);
            }
            return newMsgID;
        }
        //==============================================================================
        /// <summary>
        /// Publishes an event to the simulation.
        /// </summary>
        /// <param name="iEventID">ID of the event being published</param>
        /// <param name="bAcknowledge">Acknowledgement flag</param>
        //==============================================================================
        public void sendPublishEvent(int iEventID, bool bAcknowledge)
        {
            int prmSize;
            uint iMsgID;
            IntPtr msg = new IntPtr(0);

            TEventInfo eventInfo = (TEventInfo)eventList[iEventID];
            prmSize = (int)eventInfo.sizeBytes();
            byte[] prmPtr = new byte[prmSize];
            eventInfo.getData(ref prmPtr);             //gets a copy of the datablock

            iMsgID = BuildMsg(pWrapper, Msgs.MSG_PUBLISHEVENT, 0, iEventID, Convert.ToInt16(bAcknowledge), 0, eventInfo.sDDML, "", prmPtr, prmSize, Convert.ToInt16(bAcknowledge), ref msg);

            if ((iEventID == EVTERROR) && eventInfo.member("fatal").asBool())
            {
                bFatalErrorSent = true;
                iFatalErrorID = iMsgID;
            }

            SendMsg(pWrapper, msg);
        }
        //==============================================================================
        /// <summary>
        /// Calls a managed wrapper class in the mixed mode dll to build a message that
        /// the wrapper owns and returns an IntPtr to here.
        /// </summary>
        /// <param name="iWrapperID">Wrapper</param>
        /// <param name="iMsgKind"></param>
        /// <param name="iDestComp"></param>
        /// <param name="iParam1"></param>
        /// <param name="iParam2"></param>
        /// <param name="iParam3"></param>
        /// <param name="sParam1"></param>
        /// <param name="sParam2"></param>
        /// <param name="valPtr"></param>
        /// <param name="iValSize"></param>
        /// <param name="toAck"></param>
        /// <param name="msg"></param>
        /// <returns></returns>
        //==============================================================================
        protected uint BuildMsg(IntPtr iWrapperID, int iMsgKind, uint iDestComp, int iParam1, int iParam2, uint iParam3,
           string sParam1, string sParam2, byte[] valPtr, int iValSize, int toAck, ref IntPtr msg)
        {
            Object[] argArray = new Object[12];
            uint newMsgID = 0;

            try
            {
                try
                {
                    if (Convert.ReferenceEquals(FWrapObject, null) == true)
                    {
                        getWrapperObject();
                    }
                    //now call the buildMessage()
                    if (Convert.ReferenceEquals(FMiBuildMessage, null) == false)
                    {
                        argArray[0] = iWrapperID;
                        argArray[1] = (Object)iMsgKind;    //box these variables
                        argArray[2] = (Object)iDestComp;
                        argArray[3] = (Object)iParam1;
                        argArray[4] = (Object)iParam2;
                        argArray[5] = (Object)iParam3;
                        argArray[6] = sParam1;
                        argArray[7] = sParam2;
                        argArray[8] = valPtr; 
                        argArray[9] = (Object)iValSize;
                        argArray[10] = (Object)toAck; 
                        argArray[11] = msg;
                        newMsgID = (UInt32)(FMiBuildMessage.Invoke(FWrapObject, argArray));
                        msg = (IntPtr)argArray[11];
                    }
                    else
                    {
                        throw (new System.NullReferenceException("External method BuildMessage() set to null reference"));
                    }
                }
                catch (Exception e)
                {
                    Console.WriteLine(FName + ": Error in BuildMessage() " + e.Message);
                }
            }
            catch (Exception e)
            {
                Console.WriteLine(FName + ": Error in BuildMessage() " + e.Message);
            }
            return newMsgID;
        }

        //==============================================================================
        /// <summary>
        /// Calls a managed wrapper class in the mixed mode dll and sends a message that
        /// was constructed within the wrapper.
        /// </summary>
        /// <param name="iWrapperID"></param>
        /// <param name="msg"></param>
        //==============================================================================
        protected void SendMsg(IntPtr iWrapperID, IntPtr msg)
        {
            Object[] argArray = new Object[2];
            try
            {
                try
                {
                    if (Convert.ReferenceEquals(FWrapObject, null) == true)
                    {
                        getWrapperObject();
                    }

                    //now call the SendMsg()
                    if (Convert.ReferenceEquals(FMiSendMessage, null) == false)
                    {
                        argArray[0] = iWrapperID;
                        argArray[1] = msg;
                        FMiSendMessage.Invoke(FWrapObject, argArray);
                    }
                    else
                    {
                        throw (new System.NullReferenceException("External method SendMsg() set to null reference"));
                    }
                }
                catch (Exception e)
                {
                    Console.WriteLine(FName + ": Error in SendMsg() " + e.Message);
                }
            }
            catch (Exception e)
            {
                Console.WriteLine(FName + ": Error in SendMsg() " + e.Message);
            }
        }
        //==============================================================================
        /// <summary>
        /// Get a child component of this system by index.
        /// </summary>
        /// <param name="iWrapperID">Wrapper</param>
        /// <param name="index">Use this index in the child list</param>
        /// <param name="childName">FQName of the child component</param>
        /// <returns>Component ID of the child</returns>
        //==============================================================================
        protected uint GetChildComp(IntPtr iWrapperID, int index, ref string childName)
        {
            Object[] argArray = new Object[3];
            uint compID = 0;

            try
            {
                try
                {
                    if (Convert.ReferenceEquals(FWrapObject, null) == true)
                    {
                        getWrapperObject();
                    }
                    //now call the GetChildComp()
                    if (Convert.ReferenceEquals(FMiGetChildComp, null) == false)
                    {
                        argArray[0] = iWrapperID;
                        argArray[1] = (Object)((UInt32)index);
                        argArray[2] = childName;
                        compID = (UInt32)(FMiGetChildComp.Invoke(FWrapObject, argArray));
                        childName = argArray[2].ToString();
                    }
                    else
                    {
                        throw (new System.NullReferenceException("External method GetChildComp() set to null reference"));
                    }
                }
                catch (Exception e)
                {
                    Console.WriteLine(FName + ": Error in GetChildComp() " + e.Message);
                }
            }
            catch (Exception e)
            {
                Console.WriteLine(FName + ": Error in GetChildComp() " + e.Message);
            }
            return compID;
        }
        //============================================================================
        /// <summary>
        /// The Simulation system uses this function to set the value of a driving property.
        /// You must override this function in the child class to ensure that the
        /// system can set the value of a predefined driving property.
        /// This instance of the function should be called as the default when the driverID does not
        /// match any of the predefined driver ID's in the overriden function.
        /// If you don't define this function in the child class, this instance will be used.
        /// </summary>
        /// <param name="driverID">Local id of the driving property.</param>
        /// <param name="providerID">Component ID of the provider of this value.</param>
        /// <param name="aValue">Typed Value containing the definition and values.</param>
        //============================================================================
        public virtual void assignDriver(int driverID, uint providerID, TTypedValue aValue)
        {
            TDriverInfo driver;

            if (driverID <= driverList.Count)
            {
                driver = (TDriverInfo)driverList[driverID];
                if (driver != null)
                {
                    try
                    {
                        driver.copyFrom(aValue);
                        driverList[driverID] = driver;
                    }
                    catch (Exception e)
                    {
                        String errorMsg = String.Format("Cannot copy data for driving property {0} in assignDriver(): {1}", driverID, e.Message);
                        sendError(errorMsg, true);
                    }
                }
                else
                {
                    String errorMsg = String.Format("Cannot assign driving property {0} in assignDriver()", driverID);
                    sendError(errorMsg, true);
                }
            }
        }
        //==============================================================================
        /// <summary>
        /// Initialises the value of one of the default properties.
        /// </summary>
        /// <param name="propertyID">Local id of the property.</param>
        /// <param name="aValue">Typed Value containing the definition and values.</param>
        /// <returns>True if a default property was initialised.</returns>
        //==============================================================================
        public virtual bool initDefProperty(int propertyID, TTypedValue aValue)
        {
            bool result = false;
            TPropertyInfo property;

            if (propertyID == propertyNameToID("published_events"))
                result = true;
            else if (propertyID == propertyNameToID("subscribed_events"))
                result = true;
            else if (propertyID == propertyNameToID("driver_connections"))
                result = true;

            if (result)
            {
                property = propertyList[propertyID];
                property.copyFrom(aValue);
                propertyList[propertyID] = property;
            }

            return result;
        }

        //============================================================================
        /// <summary>
        /// Read the standard properties. If the property is a user component property then
        /// call the virtual function readProperty().
        /// This is used to read any standard, default, or user defined property.
        /// </summary>
        /// <param name="propertyID">Local ID of the property.</param>
        /// <param name="requestorID">Component ID of the component requesting this value.</param>
        /// <param name="aValue">The property in the property list.</param>
        //============================================================================
        public override void readFromPropertyList(int propertyID, uint requestorID, ref TPropertyInfo aValue)
        {
            try
            {
                switch (propertyID)
                {                                  //Populate aValue with current value
                    case PROP_NAME: aValue.setValue(FName);           //These are the standard properties
                        break;
                    case PROP_TYPE: aValue.setValue(FType);
                        break;
                    case PROP_VERSION: aValue.setValue(FVersion);
                        break;
                    case PROP_AUTHOR: aValue.setValue(FAuthor);
                        break;
                    case PROP_ACTIVE: aValue.setValue(FActive);
                        break;
                    case PROP_STATE: { } //see doQueryProperty() and getStateProperty()
                        break;
                    //default properties are already stored in the aValue ptr from the propertyList
                    case PROP_PUBEVENT: //published_events
                        break;
                    case PROP_SUBEVENT: //subscribed_events
                        break;
                    case PROP_DRVCONN: //driver_connections
                        break;

                    default: readProperty(propertyID, requestorID, ref aValue); //Non-standard properties - overloaded in child classes
                        break;
                }
            }
            catch (Exception e)
            {
                String errorMsg = String.Format("{0}\n Error in readFromPropertyList(). Property: {1}", e.Message, propertyID);
                sendError(errorMsg, true);
            }
        }
        //============================================================================
        /// <summary>
        /// Allows the simulation system to read the value of a local property.
        /// You must override this function in the child class to ensure that the system
        /// can read the local property value.
        /// </summary>
        /// <param name="propertyID">Local ID of the property.</param>
        /// <param name="requestorID">Component ID of the component requesting this value.</param>
        /// <param name="aValue"></param>
        //============================================================================
        public override void readProperty(int propertyID, uint requestorID, ref TPropertyInfo aValue)
        {
        }
        //==============================================================================
        /// <summary>
        /// Get the value of the 'state' property. Retrieving this property is different
        /// for systems and components. Systems need to send getValue messages to
        /// children. However a component can send the returnValue msg immediately.
        /// </summary>
        /// <param name="requestedByID">The requesting component.</param>
        /// <param name="queryMsgID">ID of the query message.</param>
        /// <param name="replyTo">Reply to this component.</param>
        //==============================================================================
        protected override void getStateProperty(uint requestedByID, uint queryMsgID, uint replyTo)
        {
            //if this is a system then it will need to get child information for this property
            //See doComplete() for where the 'state' value is composed and sent back to the requesting component
            if (FSystem)
            {
                uint newMsgID;
                TIDSpec driverItem;
                IntPtr msg;
                string newDriverName;
                string childName = "";
                int newDriverID = 0;

                ArrayList msgList = new ArrayList();
                driverIDList.Clear();

                TTrunkMsg qryValueMsg = msgDirector.addMsgTrunk(replyTo, queryMsgID, Msgs.MSG_QUERYVALUE, 0, 0);
                //find all the child components for this system
                int i = 0;
                uint childID = GetChildComp(pWrapper, i, ref childName);
                while (childID != 0)
                {
                    newDriverName = childName + ".state";
                    if (!getDriverByName(newDriverName, ref newDriverID))
                    {
                        newDriverID = driverList.Count;    //assume array index is the ID
                        string buf = "";
                        MakeDDML("state", TTypedValue.STYPE_STR, false, "", ref buf);
                        //do the registration of the new driver
                        addDriver(newDriverName, driverList.Count, 1, 1, "", false, buf, "", "", childID);
                    }

                    TDriverInfo driver = (TDriverInfo)driverList[newDriverID];
                    driver.iSourceID = childID;

                    //store the details for this driver so that I can send getValue msgs and store them appropriately when returned.
                    driverItem = new TIDSpec();
                    driverItem.compID = childID;
                    driverItem.itemID = (uint)newDriverID;
                    driverItem.name = newDriverName;
                    driverIDList.Add(driverItem);

                    msg = new IntPtr(0);
                    newMsgID = buildGetValueMsg(newDriverID, ref msg);
                    msgDirector.addMsgBranch(replyTo, queryMsgID, FParentID, newMsgID);   //getValues always go to the parent
                    msgList.Add(msg);

                    i++;
                    childID = GetChildComp(pWrapper, i, ref childName);
                }

                // if we had no children, finish now. Otherwise we'll finish when last reply
                // is received from our children
                if (msgList.Count == 0)
                    replySystemStateValue(replyTo, queryMsgID);

                //now send off all the msgs
                int it;
                it = 0;
                while (it != msgList.Count)
                {
                    msg = (IntPtr)msgList[it];
                    SendMsg(pWrapper, msg);                                        //now the msg for the state variable
                    it++;
                }
            }
            else
            {   //send the property value back immediately
                byte[] valueData;

                TPropertyInfo aValue = (TPropertyInfo)propertyList[propertyNameToID("state")];     //Find a TTypedValue to work with

                char active = FActive ? 'T' : 'F';
                StringBuilder buf = new StringBuilder();
                buf.Append("<component name=\"" + FName + "\" active=\"" + active + "\">\n" + getStateText());   //wrap in <component>
                buf.Append("</component>\n");
                aValue.setValue(buf.ToString());

                int valSize = (int)aValue.sizeBytes();
                valueData = new byte[valSize];
                aValue.getData(ref valueData);                          //makes a copy into a block that has been allocated

                //send a replyValue message to the router that sent it here
                sendReplyValue(queryMsgID, replyTo, aValue.sDDML, valueData, valSize);
            }
        }

        //============================================================================
        /// <summary>
        /// Called by the sequencer object to execute component logic.
        /// You must override this function in the child class to ensure that the system
        /// can execute the component logic.
        /// </summary>
        /// <param name="eventID">Local Event ID to be executed.</param>
        /// <param name="iState">State number to be executed within this event.</param>
        /// <param name="publisherID">Component ID of the publisher of this event.</param>
        /// <param name="aParams">Event parameter values.</param>
        /// <returns>The result (guard condition) for the state begin executed.</returns>
        //============================================================================
        public override int processEventState(int eventID, int iState, uint publisherID, TTypedValue aParams)
        {
            return 0;
        }
        //==============================================================================
        /// <summary>
        /// When a returnInfo arrives this function does some initial processing before
        /// being sent on to the component logic.
        /// </summary>
        /// <param name="queryMsgID">ID of the queryInfo message that this is in response to.</param>
        /// <param name="sReqName">Name of the entity requested.</param>
        /// <param name="sReturnName">Name of the entity returned.</param>
        /// <param name="ownerID">ID of the owning component of the entity.</param>
        /// <param name="entityID">ID of the entity found.</param>
        /// <param name="iKind">Kind of entity.</param>
        /// <param name="sDDML">DDML type of the entity.</param>
        //==============================================================================
        public virtual void preProcessEntityInfo(uint queryMsgID, string sReqName, string sReturnName, uint ownerID,
                                                 uint entityID, int iKind, string sDDML)
        {
        }
        //============================================================================
        /// <summary>
        /// Called by the simulation system to initialise this component. This function
        /// registers the driving properties, events, and properties and then calls the
        /// #initialise() routine.
        /// </summary>
        /// <param name="msgFrom">Sender ID of this message.</param>
        /// <param name="msgTo">ID of the recipient component.</param>
        /// <param name="iStage">Initialisation stage 1 or 2.</param>
        /// <param name="sFQN">fully qualified name of this component.</param>
        /// <param name="sInitData">Contains the text from the initdata section of the script.</param>
        //============================================================================
        public void doInitialise(uint msgFrom, uint msgTo, int iStage, string sFQN, string sInitData)
        {
            TCompParser compParser;
            TSDMLValue initValue;
            TDriverInfo dvrInfo;
            TEventInfo evInfo;
            TSetterProperty setterInfo;
            int i;
           
            try
            {
                if (iStage == 1)
                {
                    init1Error = "";
                    FMyID = msgTo;         //store the ID chosen for this component
                    FParentID = msgFrom;   //store the parent's (system) ID. init1's alway s come from the owner.

                    //small section to allow the child class to use doInit1() in the same way it would if
                    //it was based on a TBaseComp
                    TMsgHeader fakeMsg = new TMsgHeader();
                    fakeMsg.from = msgFrom;
                    fakeMsg.to = msgTo;
                    doInit1(fakeMsg, sInitData, sFQN); //call stub

                    //register the events
                    // We do this first, so that the error event is registered early on.
                    // Otherwise, even our own exception handler won't work properly.
                    // But because components might conceivably (if improbably) call addEvent
                    // during either initDefProperty or initProperty, we need to use a
                    // separate "register now" flag for events, to ensure those events
                    // will be properly registered.
                    for (i = 0; i < eventList.Count; i++)
                    {
                        if (eventList[i] != null)
                        {
                            evInfo = (TEventInfo)eventList[i];
                            sendRegistration(evInfo.Name, evInfo.sDDML, i, evInfo.iKind, evInfo.destID, false, false, 0);
                        }
                    }
                    bRegisterEventsNow = true;

                    compParser = new TCompParser(sInitData);

                    // FName = compParser->instanceName();
                    FName = sFQN;
                    FActive = true;//**** CompParser.isActive;
                    //this is set by the component constructor         FSystem = compParser->isSystem();

                    uint inits = compParser.initCount();
                    for (i = 1; i <= inits; i++)
                    {
                        initValue = new TSDMLValue(compParser.initText((uint)i), "");   //get the init
                        int propID = propertyNameToID(initValue.Name);
                        if (!initDefProperty(propID, initValue))                    //and pass it to initProperty() for
                            initProperty(propID, initValue);                        //handling by component-specific logic
                    }

                    //register all the driven inputs
                    for (i = 0; i < driverList.Count; i++)
                    {                               //Register all previously defined entities
                        if (driverList[i] != null)
                        {
                            dvrInfo = (TDriverInfo)driverList[i];
                            sendRegistration(dvrInfo.Name, dvrInfo.sDDML, i, TypeSpec.KIND_DRIVER, dvrInfo.iSourceID, false, false, 0);
                        }
                    }

                    //register the properties
                    registerProperties();

                    //register the setters
                    for (i = 0; i < setPropertyList.Count; i++)
                    {
                        if (setPropertyList[i] != null)
                        {
                            setterInfo = (TSetterProperty)setPropertyList[i];
                            sendRegistration(setterInfo.Name, setterInfo.sDDML, i, TypeSpec.KIND_REQUESTSET, setterInfo.destCompID, false, false, 0);
                        }
                    }

                    bRegisterNow = true;

                } // iStage=1

                if (init1Error.Length > 0)
                {
                    string errorMsg = string.Format("Error in doIntialise() at stage 1: {0}", init1Error);
                    sendError(errorMsg, true);
                }
                else
                    initialise(iStage);                                                      //Component-specific logic
            }
            catch (Exception e)
            {
                if (iStage == 1)
                {
                    init1Error = e.Message;
                }
                else
                {
                    string errorMsg = string.Format("Error in doIntialise() at stage 2: {0}", e.Message);
                    sendError(errorMsg, true);
                }
            }

        }
        /// <summary>
        /// stub to simulate the handler used in the TBaseComp class (full .net implementation)
        /// </summary>
        /// <param name="msg"></param>
        /// <param name="SDML"></param>
        /// <param name="FQN"></param>
        protected virtual void doInit1(TMsgHeader msg, string SDML, string FQN)
        {
        }
        //============================================================================      
        /// <summary>
        /// Initialise the component.
        /// </summary>
        /// <param name="iStage">Init stage 1 or 2</param>
        //============================================================================
        public abstract void initialise(int iStage);
        //============================================================================
        /// <summary>
        /// Tests the number of values returned for a driving property against the
        /// valid range stored in the TDriverInfo structure. If it fails, then
        /// an error is published to the simulation.
        /// </summary>
        /// <param name="driverID">Local ID of the driver.</param>
        //============================================================================
        protected void CheckDriverCount(int driverID)
        {
            TDriverInfo driverInfo;
            bool bValid;
            StringBuilder sMessage;

            driverInfo = (TDriverInfo)driverList[driverID];

            bValid = false;
            if ((driverInfo.iConnCount >= driverInfo.iMinConn)
               && ((driverInfo.iMaxConn == NO_MAXCONN) || (driverInfo.iConnCount <= driverInfo.iMaxConn)))
                bValid = true;

            if (!bValid)
            {                                           //if Simlation structure is invalid report the error
                sMessage = new StringBuilder("driving property ");
                sMessage.Append(driverInfo.Name);
                sMessage.Append("' ");

                if (driverInfo.iConnCount == 0)  //Suit the error message to the valid range
                {
                    sMessage.Append("is required but not present");
                }
                else if (driverInfo.iConnCount < driverInfo.iMinConn)
                {
                    sMessage.Append("has only ");
                    string buf = string.Format("{0} {1} {2} {3}", driverInfo.iConnCount, "values", driverInfo.iMinConn, "value(s) required");
                    sMessage.Append(buf);
                }
                else
                {
                    sMessage.Append("is ambiguous (");
                    string buf = string.Format("{0} {1} {2} {3}", driverInfo.iConnCount, "values present, max. ", driverInfo.iMaxConn, "value(s) permitted)");
                    sMessage.Append(buf);
                }

                sendError(sMessage.ToString(), true);
            }
        }
        //============================================================================
        // Definition and registration of entities
        //============================================================================
        // * For this purpose, entities are split up into drivers, (owned) properties
        //  and events.
        // * Registration is done at two times, depending on whether an entity is
        //   defined before or after the component is initialised.
        //   1. Before doInitialise is called, the bRegisterNow flag is FALSE and
        //      registration is kept until doInitialise().
        //   2. Subsequently, bRegisterNow is TRUE and registration is done immediately
        //============================================================================
        /// <summary>
        /// Adds a setting property to the internal list and registers it.
        /// </summary>
        /// <param name="sName">local name for this property.</param>
        /// <param name="iPropertyID">local property ID for this setter property.</param>
        /// <param name="destName">name of the destination property. Can be a FQN or a UQN.</param>
        /// <param name="destCompID">optional ID of the destination component.</param>
        /// <param name="sUnit">units of the property.</param>
        /// <param name="bIsArray">make this DDML type an array.</param>
        /// <param name="sType">DDML type for this property.</param>
        //============================================================================
        protected void addSetterProperty(string sName, int iPropertyID,
           string destName, uint destCompID,
           string sUnit, bool bIsArray, string sType)
        {
            TSetterProperty newProperty;

            try
            {
                newProperty = createSetterProperty(sName, iPropertyID, destName, destCompID, sUnit, bIsArray, sType);

                while (setPropertyList.Count <= iPropertyID)        // We are going to index directly into list later
                    setPropertyList.Add(null);

                if (setPropertyList[iPropertyID] != null)
                {
                    if (bRegisterNow)
                        sendDeregistration(TypeSpec.KIND_REQUESTSET, iPropertyID);
                    setPropertyList[iPropertyID] = null;
                }

                setPropertyList[iPropertyID] = newProperty;  //store the new property

                if (bRegisterNow)
                    sendRegistration(destName, newProperty.sDDML, iPropertyID, TypeSpec.KIND_REQUESTSET, destCompID, false, false, 0);
            }
            catch (Exception e)
            {
                string errorMsg = string.Format("addSetterProperty(): {0}", e.Message);
                sendError(errorMsg, true);
            }
        }
        //============================================================================
        /// <summary>
        /// Create a setting property
        /// </summary>
        /// <param name="sName">local name for this property.</param>
        /// <param name="iPropertyID">local property ID for this setter property.</param>
        /// <param name="destName">name of the destination property. Can be a FQN or a UQN.</param>
        /// <param name="destCompID">optional ID of the destination component.</param>
        /// <param name="sUnit">units of the property.</param>
        /// <param name="bIsArray">make this DDML type an array.</param>
        /// <param name="sType">DDML type for this property.</param>
        /// <returns>The new setter property.</returns>
        //============================================================================
        protected TSetterProperty createSetterProperty(string sName, int iPropertyID,
                                                        string destName, uint destCompID,
                                                        string sUnit, bool bIsArray, string sType)
        {
            string sDDMLType = "";

            MakeDDML(sName, sType, bIsArray, sUnit, ref sDDMLType);

            TSetterProperty newProperty = new TSetterProperty(sDDMLType, sType);
            newProperty.Name = sName;
            newProperty.destName = destName;                //FQN of the destination property to set
            newProperty.destCompID = destCompID;
            newProperty.regID = (uint)iPropertyID;

            return newProperty;
        }
        //============================================================================
        /// <summary>
        /// Removes a setting property from the internal list and deregisters it.
        /// </summary>
        /// <param name="iPropertyID">Local ID of the property.</param>
        //============================================================================
        protected void deleteSetterProperty(int iPropertyID)
        {
            if (iPropertyID < setPropertyList.Count)
            {
                if (setPropertyList[iPropertyID] != null)
                {
                    if (bRegisterNow)
                        sendDeregistration(TypeSpec.KIND_REQUESTSET, iPropertyID);
                    setPropertyList[iPropertyID] = null;
                }
            }
        }
        //==============================================================================
        /// <summary>
        /// Convenience routine for setting up default values and valid ranges of properties.
        /// </summary>
        /// <param name="iPropertyID">ID of the local property</param>
        /// <param name="dDefault">Default value</param>
        //==============================================================================
        protected void setPropertyDefault(int iPropertyID, double dDefault)
        {
            if (dDefault != TInitValue.NO_RANGE_D)
            {
                TPropertyInfo prop = propertyList[iPropertyID];
                prop.setDefault(dDefault);
                propertyList[iPropertyID] = prop;
            }
        }
        //============================================================================
        /// <summary>
        /// Convenience routine for setting up default values and valid ranges of properties.
        /// </summary>
        /// <param name="iPropertyID">ID of the local property</param>
        /// <param name="iDefault">Default value</param>
        //============================================================================
        protected void setPropertyDefault(int iPropertyID, int iDefault)
        {
            if (iDefault != TInitValue.NO_RANGE_I4)
            {
                TPropertyInfo prop = propertyList[iPropertyID];
                prop.setDefault(iDefault);
                propertyList[iPropertyID] = prop;
            }
        }

        //============================================================================
        /// <summary>
        /// Convenience routine for setting up default values and valid ranges of properties.
        /// </summary>
        /// <param name="iPropertyID">ID of the local property</param>
        /// <param name="bDefault">Default value</param>
        //============================================================================
        protected void setPropertyDefault(int iPropertyID, bool bDefault)
        {
            TPropertyInfo prop = propertyList[iPropertyID];
            prop.setDefault(bDefault);
            propertyList[iPropertyID] = prop;
        }
        //============================================================================
        /// <summary>
        /// Convenience routine for setting up default values and valid ranges of properties.
        /// </summary>
        /// <param name="iPropertyID">ID of the local property</param>
        /// <param name="sDefault">Default value</param>
        //============================================================================
        protected void setPropertyDefault(int iPropertyID, string sDefault)
        {
            TPropertyInfo prop = propertyList[iPropertyID];
            prop.setDefault(sDefault);
            propertyList[iPropertyID] = prop;
        }
        //============================================================================
        /// <summary>
        /// Convenience routine for setting up default values and valid ranges of properties
        /// </summary>
        /// <param name="iPropertyID">ID of the local property</param>
        /// <param name="iDefault">Default value</param>
        /// <param name="iMinVal">Min integer value</param>
        /// <param name="iMaxVal">Max integer value</param>
        //============================================================================
        protected void setPropertyRange(int iPropertyID, int iDefault, int iMinVal, int iMaxVal)
        {
            TPropertyInfo aProperty = propertyList[iPropertyID];
            if (iDefault != TInitValue.NO_RANGE_I4)
                aProperty.setDefault(iDefault);
            if (iMinVal != TInitValue.NO_RANGE_I4)
                aProperty.setMin(iMinVal);
            if (iMaxVal != TInitValue.NO_RANGE_I4)
                aProperty.setMax(iMaxVal);
            propertyList[iPropertyID] = aProperty;
        }
        //============================================================================
        /// <summary>
        /// Convenience routine for setting up default values and valid ranges of properties
        /// </summary>
        /// <param name="iPropertyID">ID of the local property</param>
        /// <param name="dDefault">Default value</param>
        /// <param name="dMinVal">Min value</param>
        /// <param name="dMaxVal">Max value</param>
        //============================================================================
        protected void setPropertyRange(int iPropertyID, double dDefault, double dMinVal, double dMaxVal)
        {
            TPropertyInfo aProperty = propertyList[iPropertyID];
            if (dDefault.CompareTo(TInitValue.NO_RANGE_D) != 0)
                aProperty.setDefault(dDefault);
            if (dMinVal.CompareTo(TInitValue.NO_RANGE_D) != 0)
                aProperty.setMin(dMinVal);
            if (dMaxVal.CompareTo(TInitValue.NO_RANGE_D) != 0)
                aProperty.setMax(dMaxVal);
            propertyList[iPropertyID] = aProperty;
        }
        //============================================================================
        /// <summary>
        /// Adds an event to the local event list and registers it. The child object
        /// should call this function to add events.
        /// </summary>
        /// <param name="sName">Name of the event.</param>
        /// <param name="eventID">Local ID of the event.</param>
        /// <param name="iKind">Published or Subscribed.</param>
        /// <param name="sType">DDML type of the event.</param>
        /// <param name="sShortDescr">Short description text</param>
        /// <param name="sFullDescr">Full description text</param>
        /// <param name="destID">ID of the destination component.</param>
        //============================================================================
        public void addEvent(string sName, int eventID, int iKind, string sType, String sShortDescr, String sFullDescr, uint destID)
        {
            TEventInfo newEvent;    //TDDMLValue descendant
            string sDDMLType = "";

            try
            {
                if (sType.Length > 0)
                    MakeDDML(sName, sType, false, "", ref sDDMLType);
                else
                    sDDMLType = TypeSpec.TYPEEMPTY;

                newEvent = new TEventInfo(sDDMLType, sShortDescr, sFullDescr, "");
                newEvent.Name = sName;
                newEvent.iKind = iKind;

                if (iKind == TypeSpec.KIND_PUBLISHEDEVENT)
                {
                    newEvent.destID = destID;
                }
                else                       //if the event is a subscribing event
                {
                    newEvent.destID = 0;    //then the destination component = 0
                }

                while (eventList.Count <= eventID)         //We are going to index directly into List later
                    eventList.Add(null);

                if (eventList[eventID] != null)
                {
                    if (bRegisterEventsNow)
                        sendDeregistration(((TEventInfo)eventList[eventID]).iKind, eventID);
                }
                eventList[eventID] = newEvent;

                eventsManager.addEventType(sName, eventID);

                if (bRegisterEventsNow)
                    sendRegistration(sName, sDDMLType, eventID, iKind, newEvent.destID, false, false, 0);
            }
            catch (Exception e)
            {
                String errorMsg = String.Format("addEvent(): {0}", e.Message);
                sendError(errorMsg, true);
            }
        }
        //============================================================================
        /// <summary>
        /// Send an acknowledgement message
        /// </summary>
        /// <param name="msgTo">Component ID to which complete is sent.</param>
        /// <param name="msgID">ID of the message being acknowledged.</param>
        //============================================================================
        public override void sendComplete(uint msgTo, uint msgID)
        {
            PassMessage(pWrapper, Msgs.MSG_COMPLETE, msgTo, (int)msgID, 0, 0, null, null, null, 0, 0);
        }
        //============================================================================
        /// <summary>
        /// Responds to the termination message from the system. Tells the statemachine
        /// that it should terminate when it arrives at DONE. If no events are running
        /// then a complete() message is sent from here.
        /// </summary>
        /// <param name="termMsgFrom">Sender's ID of the termination message.</param>
        /// <param name="termMsgID">Message ID of the termination message.</param>
        //============================================================================
        public virtual void doAtTermination(uint termMsgFrom, uint termMsgID)
        {
            try
            {
                terminate();                                                   //Component-specific termination logic
                // Tell the state machine that when it
                if (eventsManager.reqTerminate(termMsgFrom, termMsgID))
                {      // arrives at DONE, it should terminate
                    sendComplete(termMsgFrom, termMsgID);                       // If it has no events being executed
                }                                                              //  then send "complete" from here
            }
            catch (Exception e)
            {
                string errorMsg = string.Format("doAtTermination(): {0}", e.Message);
                sendError(errorMsg, true);
            }
        }
        //============================================================================
        /// <summary>
        /// Responds to a MSG_NOTIFYDELETE
        /// </summary>
        /// <param name="termMsgFrom">Sender's ID of the termination message.</param>
        /// <param name="termMsgID">Message ID of the termination message.</param>
        //============================================================================
        public void doAtDeletion(uint termMsgFrom, uint termMsgID)
        {
            try
            {
                terminate();                                                   //Component-specific termination logic
                // Tell the state machine that when it
                if (eventsManager.reqTerminate(termMsgFrom, termMsgID))
                {      //  arrives at DONE, it should terminate
                    sendComplete(termMsgFrom, termMsgID);                       // If it has no events being executed
                }
            }
            catch (Exception e)
            {
                string errorMsg = string.Format("doAtTermination(): {0}", e.Message);
                sendError(errorMsg, true);
            }
        }
        //============================================================================
        /// <summary>
        /// Called by the exported function onEvent(). This function begins the
        /// execution of an event by telling the events manager to beginEvent().
        /// If the requiresAck == 1 then a complete(event) will to be sent
        /// </summary>
        /// <param name="msgFrom">The component ID of the sender (or router) of the event.</param>
        /// <param name="publBy">The originator component ID of the published event.</param>
        /// <param name="eventID">Local event ID.</param>
        /// <param name="msgID">Incoming message ID.</param>
        /// <param name="requiresAck">Requires acknowledgement.</param>
        /// <param name="prmDDML">DDML description of this event.</param>
        /// <param name="prmData">Parameter data values.</param>
        /// <param name="prmSize">Size of parameter data block.</param>
        //============================================================================
        public void doEvent(uint msgFrom, uint publBy, int eventID, uint msgID, bool requiresAck, string prmDDML,
                            byte[] prmData, uint prmSize)
        {
            TEventInfo eventInfo;
            TDDMLValue subsParams;

            try
            {
                if (!eventsManager.isToTerminate()) //check that this component has not been requested to terminate
                {
                    eventInfo = (TEventInfo)eventList[eventID];
                    if (eventInfo != null)
                    {
                        if (eventInfo.isVariant || !eventInfo.checkCompatibility(publBy, prmDDML))
                        {
                            if (ConvertApsimVariant(eventInfo, publBy, prmDDML, ref prmData, ref prmSize))
                            {
                                eventInfo.isVariant = true;
                            }
                            else
                            {
                                string errorMsg = string.Format("{0}: Type of value passed for event {1} ({2}) parameter is incompatible.", FName, eventID, eventInfo.sDescr);
                                throw (new TypeMisMatchException(errorMsg));
                            }
                        }
                        subsParams = eventInfo.sourceValue(publBy);
                        if (subsParams != null)
                            subsParams.setData(prmData, (int)prmSize, 0);
                        eventsManager.beginEvent(eventID, msgFrom, publBy, msgID, subsParams, requiresAck);
                    }
                    else if (requiresAck)
                    {
                        sendComplete(msgFrom, msgID);
                    }
                }
            }
            catch (Exception e)
            {
                string errorMsg = string.Format("doEvent(): {0}", e.Message);
                sendError(errorMsg, true);
            }

        }
        //============================================================================
        /// <summary>
        /// Receives value data for a nominated driving property and
        /// <ol>
        ///  <li>places it in an existing driver</li>
        ///  <li>calls assignDriver to allow the component logic to access the value</li>
        /// </ol>
        /// Note: the DDML information is passed here for later use in type-checking,
        ///       unit conversion etc
        /// </summary>
        /// <param name="driverID">Local ID of the driving property.</param>
        /// <param name="providerID">Component ID of the provider of the values.</param>
        /// <param name="sDDML">DDML type description.</param>
        /// <param name="valPtr">The value block.</param>
        /// <param name="valSize">Size of value block.</param>
        //============================================================================
        public void doSetDriver(uint driverID, uint providerID, string sDDML, byte[] valPtr, uint valSize)
        {
            TDriverInfo aDriver;
            TSourcedInfo passValue;

            if (providerID != FMyID || driverID == checkPointDriverID)
            {
                try
                {
                    aDriver = (TDriverInfo)driverList[(int)driverID];
                    if (!aDriver.checkCompatibility(providerID, sDDML))
                    {
                        string errorMsg = string.Format("{0}: Type of value passed for driving variable {1} ({2}) is incompatible.\n {3} <- {4}", FName, driverID, aDriver.sDescr, aDriver.sDDML, sDDML);
                        throw (new TypeMisMatchException(errorMsg));
                    }

                    passValue = aDriver.sourceValue(providerID);
                    passValue.setData(valPtr, (int)valSize, 0);
                    aDriver.iConnCount++;
                    driverList[(int)driverID] = aDriver;   //store changes
                    assignDriver((int)driverID, providerID, passValue);
                }
                catch (Exception e)
                {
                    string errorMsg = string.Format("doSetDriver(): {0}", e.Message);
                    sendError(errorMsg, true);
                }
            }
        }
        //============================================================================
        /// <summary>
        /// Defines a processing state within a subscribed event.
        /// </summary>
        /// <param name="eventID">Local ID of the event.</param>
        /// <param name="iStateID">State number.</param>
        /// <param name="iStateType">Type of state; Logic or Non logic</param>
        //============================================================================
        protected void defineEventState(int eventID, int iStateID, int iStateType)
        {
            eventsManager.defineEventState(eventID, iStateID, iStateType);
        }
        //============================================================================
        /// <summary>
        /// Defines a transition between processing states within a subscribed event
        /// </summary>
        /// <param name="eventID">Local ID of the event.</param>
        /// <param name="iStateID">State number.</param>
        /// <param name="iCondition">Guard condition to trigger this transition.</param>
        /// <param name="iToState">State to be executed because of the guard condition.</param>
        /// <param name="replaceAll">Replace all existing transitions for this state.</param>
        //============================================================================
        protected void defineEventTransition(int eventID, int iStateID, int iCondition, int iToState, bool replaceAll)
        {
            eventsManager.defineEventTransition(eventID, iStateID, iCondition, iToState, replaceAll);
        }
        //==============================================================================
        /// <summary>
        /// Sends a replySetValueSuccess message.
        /// </summary>
        /// <param name="reqMsgID">The original message ID of the querySetValue msg.</param>
        /// <param name="replyTo">Send this message to..</param>
        /// <param name="success">The success flag.</param>
        //==============================================================================
        protected void sendReplySetValueSuccess(uint reqMsgID, uint replyTo, bool success)
        {
            PassMessage(pWrapper, Msgs.MSG_REPLYSET, replyTo, (int)reqMsgID, success ? 1 : 0, 0, null, null, null, 0, 0);
        }
        //============================================================================
        /// <summary>
        /// The simulation uses this function set the value of an owned property from
        /// a querySetValue message.
        /// </summary>
        /// <param name="msgID">ID of the incoming message.</param>
        /// <param name="replyTo">Reply to this component.</param>
        /// <param name="propertyID">Local ID of the property.</param>
        /// <param name="sDDML">DDML type description of the property.</param>
        /// <param name="valPtr">The value data.</param>
        /// <param name="valSize">Size of the value data.</param>
        //============================================================================
        public void doResetProperty(uint msgID, uint replyTo, uint propertyID, string sDDML, byte[] valPtr, uint valSize)
        {
            TPropertyInfo ownedValue;
            TDDMLValue resetValue;
            bool result;
            CompPropPair dictKey = new CompPropPair(replyTo, propertyID);

            try
            {
                if (((int)propertyID >= propertyList.Count) || (propertyList[(int)propertyID] == null))
                {
                    string errorMsg = string.Format("{0}: Attempt to write to undefined property (ID: {1}) in doResetProperty()", FName, (int)propertyID);
                    throw (new ApplicationException(errorMsg));
                }

                if ((int)propertyID == propertyNameToID("state")) //if trying to set the State of this component
                {
                    writeStateProperty(msgID, replyTo, sDDML, valPtr, valSize);//then sends it's own success msg
                }
                else
                {
                    ownedValue = (TPropertyInfo)propertyList[(int)propertyID];
                    if (!ownedValue.bWrite)
                    {
                        string errorMsg = string.Format("{0}: Attempt to write to read-only property \" {1} \" in doResetProperty()", FName, ownedValue.Name);
                        throw (new ApplicationException(errorMsg));
                    }

                    // See whether we have a cached DDMLvalue, to avoid the expense of parsing
                    if (!resetDict.TryGetValue(dictKey, out resetValue))
                    {
                        resetValue = new TDDMLValue(sDDML, "");
                        if (ownedValue.canAssignFrom(resetValue) == TTypedValue.ctBAD)    //Check for type compatibility 
                        {
                            string errorMsg = string.Format("{0}: Attempt to write value of wrong type to property \" {1} \" in doResetProperty()", FName, ownedValue.Name);
                            throw (new TypeMisMatchException(errorMsg));
                        }
                        resetDict.Add(dictKey, resetValue);
                    }
                    resetValue.setData(valPtr, (int)valSize, 0);               //Now carry out the reset
                    result = writeProperty((int)propertyID, resetValue);
                    sendReplySetValueSuccess(msgID, replyTo, result);     //sends the success? msg
                }
            }
            catch (Exception e)
            {
                string errorMsg = string.Format("doResetProperty(): {0} ", e.Message);
                sendError(errorMsg, true);
            }
        }
        //==============================================================================
        /// <summary>
        /// Set the value of the state property.
        /// </summary>
        /// <param name="reqMsgID">Incoming message ID.</param>
        /// <param name="replyTo">Reply to this component.</param>
        /// <param name="sDDML">DDML of the property.</param>
        /// <param name="valPtr">Data value.</param>
        /// <param name="valSize">Size of the data block.</param>
        //==============================================================================
        public void writeStateProperty(uint reqMsgID, uint replyTo, string sDDML, byte[] valPtr, uint valSize)
        {
            TCompParser compParser;
            TCompParser sdmlChild;
            TSDMLValue initValue;
            TDDMLValue compDef;
            string childName = "";
            string newSetterName;
            int newSetterID;
            TSetterProperty newSetter;
            IntPtr msg;
            uint newMsgID;
            ArrayList msgList;
            TTrunkMsg qrySetMsg;
            bool found;
            bool success = true;   //init to succeed

            msgList = new ArrayList();

            compDef = new TDDMLValue(sDDML, "");
            compDef.setData(valPtr, (int)valSize, 0);
            //init all the inits from the initsection
            compParser = new TCompParser(compDef.asStr());
            try
            {
                uint inits = compParser.initCount();
                for (uint i = 1; i <= inits; i++)
                {
                    TSDMLParser sdmlParser = new TSDMLParser(compParser.initNode(i));   //Parse the init data into TTypedValues
                    initValue = new TSDMLValue(sdmlParser, sdmlParser.rootNode(), "");       //and pass it to initProperty() for
                    if (!initDefProperty(propertyNameToID(initValue.Name), initValue))
                        initProperty(propertyNameToID(initValue.Name), initValue);     //handling by component-specific logic
                }
            }
            catch (Exception)
            {
                success = false;
            }

            if (FSystem)
            { //if this is a system then
                if (success)
                {
                    qrySetMsg = msgDirector.addMsgTrunk(replyTo, reqMsgID, Msgs.MSG_QUERYSET, propertyNameToID("state"), 0); //source of the querySetValue
                    //initialise all the child components
                    uint childID;
                    int i;
                    //get all the components of the system and send the initsections to the children
                    XmlNode childNode = compParser.firstChildCompNode();  //get the first child
                    while (childNode != null)
                    {
                        sdmlChild = new TCompParser(childNode);
                        i = 0;
                        found = false;
                        childID = GetChildComp(pWrapper, i, ref childName);
                        while (!found && (childID != 0))
                        {
                            if (childName.ToLower() == sdmlChild.InstanceName.ToLower())
                            {
                                found = true;
                                newSetterName = childName + ".state";
                                newSetterID = 0;
                                if (!getSetterByName(newSetterName, ref newSetterID))
                                {
                                    newSetterID = setPropertyList.Count;   //next id. Assume array index is the ID.
                                    string buf = "";
                                    MakeDDML("state", TTypedValue.STYPE_STR, false, "", ref buf);
                                    //do the registration of the new setter
                                    addSetterProperty(newSetterName, newSetterID, newSetterName, 0, "", false, buf);
                                }

                                newSetter = (TSetterProperty)setPropertyList[newSetterID];
                                newSetter.destName = newSetterName;   //store the fqn of the dest state property
                                newSetter.regID = (uint)newSetterID;

                                newSetter.setValue(compParser.getText(childNode)); //store the <component> xml in the setter property

                                msg = new IntPtr(0);
                                newMsgID = buildRequestSetMsg(newSetterID, (TTypedValue)setPropertyList[newSetterID], ref msg);
                                msgList.Add(msg);
                                msgDirector.addMsgBranch(replyTo, reqMsgID, FMyID, (uint)newSetterID);   //querySet msg used to set a Setter property
                            }
                            else
                            {
                                i++;
                                childID = GetChildComp(pWrapper, i, ref childName);
                            }
                        }
                        childNode = compParser.nextSiblingCompNode();  //go to the next child component
                    }

                    // if we had no children, finish now. Otherwise we'll finish when last reply
                    // is received from our children
                    if (msgList.Count < 1)
                        sendReplySetValueSuccess(reqMsgID, replyTo, success);

                    //now send off all the msgs
                    int it;
                    it = 0;
                    while (it != msgList.Count)
                    {
                        msg = (IntPtr)msgList[it];
                        SendMsg(pWrapper, msg);                                        //now the msg for the state variable
                        it++;
                    }
                    //waits for all the notifySetValueSuccess replies and then sends a success value back to replyTo.
                }
            }
            else  //send the reply immediately
                sendReplySetValueSuccess(reqMsgID, replyTo, success);
        }
        //==============================================================================
        /// <summary>
        /// Search the Setter variable list for the Setter specified by setterName.
        /// </summary>
        /// <param name="setterName">FQN of the Setter.</param>
        /// <param name="setterID">Return the Setter ID.</param>
        /// <returns>True if the Setter is found.</returns>
        //==============================================================================
        protected bool getSetterByName(string setterName, ref int setterID)
        {
            TSetterProperty setter;
            bool setterFound = false;

            int i = 0;
            while ((!setterFound) && (i < setPropertyList.Count))
            {
                setter = (TSetterProperty)setPropertyList[i];
                if ((setter != null) && (setter.Name.ToLower() == setterName.ToLower()))
                {
                    setterFound = true;
                    setterID = i;
                }
                else
                {
                    i++;
                }
            }
            return setterFound;
        }
        //==============================================================================
        /// <summary>
        /// Build a requestSetvalue message.
        /// </summary>
        /// <param name="iLocalSetPropID">Local identifier for the property.</param>
        /// <param name="Value">Value to which to set the property.</param>
        /// <param name="msg">Pointer to the newly created message.</param>
        /// <returns>The new message ID.</returns>
        //==============================================================================
        protected uint buildRequestSetMsg(int iLocalSetPropID, TTypedValue Value, ref IntPtr msg)
        {
            int valSize;
            uint msgID;
            TSetterProperty localProp = null;

            if (iLocalSetPropID < setPropertyList.Count)
            {
                localProp = (TSetterProperty)setPropertyList[iLocalSetPropID];
            }
            if (localProp == null)
            {
                string errorMsg = string.Format("Attempt to set an undefined property (ID: {0}) in buildRequestSetMsg()", iLocalSetPropID);
                throw (new ApplicationException(errorMsg));
            }

            valSize = (int)Value.sizeBytes();
            byte[] valPtr = new byte[valSize];
            Value.getData(ref valPtr);

            msgID = BuildMsg(pWrapper, Msgs.MSG_REQUESTSET, 0, iLocalSetPropID, 0, 0, localProp.sDDML, "", valPtr, valSize, 0, ref msg);

            return msgID;
        }
        //==============================================================================
        /// <summary>
        /// The virtual function to handle the message notifySetValueSuccess.
        /// When reimplemented by the child it should firstly call this function.
        /// </summary>
        /// <param name="msgFrom"></param>
        /// <param name="propertyID"></param>
        /// <param name="bSuccess"></param>
        //==============================================================================
        protected virtual void processNotifySet(uint msgFrom, int propertyID, bool bSuccess)
        {
            if (checkPointTracer.isCheckPointing(propertyID))     //if this is a restore checkpoint setter
            {
                checkPointTracer.clearDriverProcess(propertyID);   //clear the tracing
                sendPauseSim(false);                               //resume the simulation
            }
        }
        //==============================================================================
        /// <summary>
        /// Handles an incoming notifySetValueSuccess message. MSG_NOTIFYSET
        /// Calls the virtual function processNotifySet()
        /// </summary>
        /// <param name="msgFrom"></param>
        /// <param name="propertyID"></param>
        /// <param name="bSuccess"></param>
        //==============================================================================
        public void doSetSuccess(uint msgFrom, uint propertyID, bool bSuccess)
        {
            if (FSystem)
            { //only systems would have branched out messages
                //check if this is a notifySetValueSuccess for the restoring of a checkpoint.
                if (msgDirector.isABranch(Msgs.MSG_QUERYSET, FMyID, propertyID))
                {
                    TTrunkMsg queryMsg = new TTrunkMsg();
                    msgDirector.getBranch(Msgs.MSG_QUERYSET, FMyID, propertyID, ref queryMsg);

                    uint count = msgDirector.pruneBranch(Msgs.MSG_QUERYSET, FMyID, propertyID);
                    if (count == 0)
                    {
                        //if any of the returns have bSuccess false then send a failure

                        uint returnTo = queryMsg.returnToCompID;
                        uint queryMsgID = queryMsg.inMsgID;
                        msgDirector.removeTrunk(returnTo, queryMsgID);
                        //send a replySetValueSuccess to the originator of the querySet
                        sendReplySetValueSuccess(queryMsgID, returnTo, bSuccess);
                    }
                }
                else
                {
                    processNotifySet(msgFrom, (int)propertyID, bSuccess);
                }
            }
            else
            {
                processNotifySet(msgFrom, (int)propertyID, bSuccess);
            }
        }
        //============================================================================
        /// <summary>
        /// Sends a pause or resume to the simulation.
        /// </summary>
        /// <param name="bPause">True if pause.</param>
        //============================================================================
        protected void sendPauseSim(bool bPause)
        {
            if (bPause)
                PassMessage(pWrapper, Msgs.MSG_PAUSE, 1, 0, 0, 0, "", "", null, 0, 0);
            else
                PassMessage(pWrapper, Msgs.MSG_RESUME, 1, 0, 0, 0, "", "", null, 0, 0);
        }
        //============================================================================
        /// <summary>
        /// Called by the exported function #onReturnInfo()
        /// </summary>
        /// <param name="msgFrom">Source of this message.</param>
        /// <param name="queryMsgID">Message ID of the queryInfo() message.</param>
        /// <param name="ownerID">Component ID of the owning component.</param>
        /// <param name="entityID">ID of the entity requested.</param>
        /// <param name="iEntityKind">Kind of entity.</param>
        /// <param name="sEntityName">Name of the entity.</param>
        /// <param name="sDDML">DDML type description of the entity.</param>
        //============================================================================
        public void doInfoReturned(uint msgFrom, uint queryMsgID, uint ownerID, uint entityID, uint iEntityKind,
                                   string sEntityName, string sDDML)
        {
            TQueryStore query;

            try
            {
                int i = 0;
                //find the entity that was requested - by the queryInfo msg ID
                query = (TQueryStore)queryList[i];
                while ((i < queryList.Count) && (query.iSentMsgID != queryMsgID))
                {
                    i++;
                    query = (TQueryStore)queryList[i];
                }
                if (i == queryList.Count)
                {
                    string errorMsg = string.Format("{0}: Stored query information not found in doInfoReturned()", FName);
                    sendError(errorMsg, true);
                }
                else
                {
                    preProcessEntityInfo(queryMsgID, query.sName, sEntityName, ownerID, entityID, (int)iEntityKind, sDDML);
                    processEntityInfo(query.sName, sEntityName, ownerID, entityID, (int)iEntityKind, sDDML);
                }
            }
            catch (Exception e)
            {
                string errorMsg = string.Format("doInfoReturned(): {0} ", e.Message);
                sendError(errorMsg, true);
            }
        }
        //==============================================================================
        /// <summary>
        /// Restore the checkpoint script to the component. Start by pausing the
        /// simulation.
        /// </summary>
        /// <param name="compName">The FQN of the component to be restored.</param>
        /// <param name="sdml">The SDML script that will initialise the component/system.</param>
        //==============================================================================
        protected void restoreCheckPoint(string compName, string sdml)
        {
            //pause the simulation
            IntPtr msg = new IntPtr(0);
            uint newMsgID = BuildMsg(pWrapper, Msgs.MSG_PAUSE, 0, 0, 0, 0, "", "", null, 0, 1, ref msg);
            //log the checkpoint by driverID, msgID and the driver name which is always the name of the 'state' variable
            string setVarName = compName + ".state";
            checkPointTracer.logCheckPointRestore(newMsgID, Msgs.MSG_PAUSE, setVarName, sdml);  //a restore checkpoint
            SendMsg(pWrapper, msg);       //send a Pause and wait for the Complete(pause)
        }
        //============================================================================
        /// <summary>
        /// Executes the checkpoint process. A Pause is sent to the simulation and when
        /// the Complete(Pause) returns a driving property is registered that connects
        /// to the required component.
        /// </summary>
        /// <param name="compName">The FQN of the component to be checkpointed.</param>
        //============================================================================
        protected void doCheckPoint(string compName)
        {
            //pause the simulation
            IntPtr msg = new IntPtr(0);
            uint newMsgID = BuildMsg(pWrapper, Msgs.MSG_PAUSE, 0, 0, 0, 0, "", "", null, 0, 1, ref msg);
            //log the checkpoint by driverID, msgID and the driver name which is always the name of the 'state' variable
            string driverName = compName + ".state";
            checkPointTracer.logCheckPoint(newMsgID, Msgs.MSG_PAUSE, driverName, false);
            SendMsg(pWrapper, msg);       //send a Pause and wait for the Complete(pause)

        }
        //============================================================================
        /// <summary>
        /// Request the value of a driving property.
        /// Determines if the request is from an Event. If it is then we will keep
        /// track of the msgID-eventID pair for when a Complete is returned
        /// </summary>
        /// <param name="driverID">Local ID of the driver being requested.</param>
        /// <param name="eventID"></param>
        //============================================================================
        public void sendDriverRequest(int driverID, int eventID)
        {
            try
            {
                IntPtr msg = new IntPtr();
                uint msgID = buildGetValueMsg(driverID, ref msg);
                if (eventID != 0)
                {                           //if this request is triggered from an event then
                    eventsManager.addRequest(eventID, msgID);  //keep track of the call
                }
                SendMsg(pWrapper, msg);
            }
            catch (Exception e)
            {
                string errorMsg = string.Format("sendDriverRequest(): {0}", e.Message);
                sendError(errorMsg, true);
            }
        }
        //==============================================================================
        /// <summary>
        /// Construct a GetValue message for a driving variable.
        /// </summary>
        /// <param name="driverID">ID of the driving variable.</param>
        /// <param name="msg">The created message.</param>
        /// <returns>The message ID of the new message.</returns>
        //==============================================================================
        protected uint buildGetValueMsg(int driverID, ref IntPtr msg)
        {
            uint msgID = BuildMsg(pWrapper, Msgs.MSG_GETVALUE, 0, driverID, 0, 0, "", "", null, 0, 1, ref msg);

            TDriverInfo driverInfo = (TDriverInfo)driverList[driverID];
            if (!driverInfo.bRequestActive)
            {                                        //Nested requests are not allowed
                driverInfo.bRequestActive = true;
                driverInfo.iRequestMsg = msgID;
                driverInfo.iConnCount = 0;
                driverList[driverID] = driverInfo;
            }
            else
            {
                string errorMsg = string.Format("{0}: Attempt to nest requests for driving property {1} in buildGetValueMsg().", FName, driverInfo.Name);
                throw (new ApplicationException(errorMsg));
            }
            return msgID;
        }
        //============================================================================
        /// <summary>
        /// Attempt to set the value of another component's owned property.
        /// </summary>
        /// <param name="iLocalSetPropID">local identifier for the property</param>
        /// <param name="Value">value to which to set the property</param>
        //============================================================================
        protected void sendReqSetValue(int iLocalSetPropID, TTypedValue Value)
        {
            IntPtr msg = new IntPtr(0);

            buildRequestSetMsg(iLocalSetPropID, Value, ref msg);
            SendMsg(pWrapper, msg);
        }
        //============================================================================
        /// <summary>
        /// Not implemented.
        /// </summary>
        /// <param name="sFQN"></param>
        /// <param name="bActivate"></param>
        //============================================================================
        protected void sendActivateComp(string sFQN, bool bActivate)
        {

        }
        //============================================================================
        /// <summary>
        /// Sends a message to the simulation to terminate.
        /// </summary>
        //============================================================================
        protected void sendEndSimulation()
        {
            PassMessage(pWrapper, Msgs.MSG_TERMINATE, 0, 0, 0, 0, "", "", null, 0, 0);
        }
        //============================================================================
        /// <summary>
        /// Handles the complete message that is sent via the wrapper prot.dll
        /// If this is a Complete to an earlier request from a TMEvent, then the event
        /// manager can use this Complete to resume the event processing. If the message
        /// ID is not found in the event manager's list then it is ignored.
        /// </summary>
        /// <param name="msgID">ID of the original message that this Complete is sent for.</param>
        //============================================================================
        public void doComplete(uint msgID)
        {
            bool bFound;
            TDriverInfo driver;
            TQueryStore query;

            try
            {
                eventsManager.completeRequest(msgID);
                //Remove stored information about previously sent queryInfo messages
                bFound = false;
                int i = 0;
                while (!bFound && (i < queryList.Count))
                {
                    query = (TQueryStore)queryList[i];
                    bFound = (query.iSentMsgID == msgID);
                    if (bFound)
                    {
                        queryList.RemoveAt(i);
                    }
                    else
                        i++;
                }
                int itd = 0;
                while (itd < driverList.Count)
                {                                                            //If this "complete" is for a driving
                    driver = (TDriverInfo)driverList[itd];                   //  property request, then check for
                    bFound = (driver != null) && driver.bRequestActive && (driver.iRequestMsg == msgID); // the correct number of answers
                    if (bFound)
                    {
                        CheckDriverCount(itd);                                 //index of the driver=driver ID
                        driver.bRequestActive = false;

                        //determine if this is a Complete for state variables in a queryValue(state) process (when a system)
                        if (msgDirector.isABranch(Msgs.MSG_QUERYVALUE, FParentID, msgID))
                        {
                            TTrunkMsg srcMsg = new TTrunkMsg();
                            if (msgDirector.getBranch(Msgs.MSG_QUERYVALUE, FParentID, msgID, ref srcMsg))
                            {
                                if (msgDirector.pruneBranch(Msgs.MSG_QUERYVALUE, FParentID, msgID) == 0)   //if this is the last 'state' value from the children then
                                {
                                    replySystemStateValue(srcMsg.returnToCompID, srcMsg.inMsgID);
                                }
                            }
                        }
                    }
                    itd++;
                }

                if (bFatalErrorSent && (msgID == iFatalErrorID))      // Once a fatal ERROR event has been
                    sendEndSimulation();                               //   acknowledged, send "terminate"

                //Processing checkpointing for this component doing the checkpointing.
                if (checkPointTracer.isCheckPointMsg(msgID))
                    processCheckPointing(msgID);
                else if (checkPointTracer.isCheckPointRestoreMsg(msgID))
                    processCheckPointRestore(msgID);
            }
            catch (Exception e)
            {
                string errorMsg = string.Format("doComplete(): {0}", e.Message);
                sendError(errorMsg, true);
            }
        }
        //==============================================================================
        /// <summary>
        /// Send a replyValue(state) for this system.
        /// </summary>
        /// <param name="returnToCompId">The ID fo the component to receive the reply</param>
        /// <param name="msgID">The ID of the msg that is completed.</param>
        //==============================================================================
        protected void replySystemStateValue(uint returnToCompId, uint msgID)
        {
            TDriverInfo stateDriver;
            int valSize;

            TIDSpec driverItem;

            //append all the 'state' drivers
            TPropertyInfo aValue = (TPropertyInfo)propertyList[propertyNameToID("state")];     //Find a TTypedValue to work with

            char active = FActive ? 'T' : 'F';
            string systemState = getStateText();
            systemState = "<system name=\"" + FName + "\" active=\"" + active + "\">\n" + systemState;                          //wrap in <component>

            //now for each 'state' driver, append it to this component's state
            int it = 0;
            while (it < driverIDList.Count)
            {
                driverItem = (TIDSpec)driverIDList[it];
                stateDriver = (TDriverInfo)driverList[(int)driverItem.itemID];
                systemState += stateDriver.asStr();
                it++;
            }

            systemState += "</system>\n";
            aValue.setValue(systemState);

            valSize = (int)aValue.sizeBytes();
            byte[] valueData = new byte[valSize];
            aValue.getData(ref valueData);                 //makes a copy into a block that has been allocated

            //send a replyValue to router
            sendReplyValue(msgID, returnToCompId, aValue.sDDML, valueData, valSize);

            //remove the trunk msg
            msgDirector.removeTrunk(returnToCompId, msgID);
        }

        //==============================================================================
        /// <summary>
        /// Continues the checkpointing process for a component that is doing the
        /// checkpointing. Called by doComplete().
        /// </summary>
        /// <param name="msgID">The ID of the msg that is completed.</param>
        //==============================================================================
        protected void processCheckPointing(uint msgID)
        {
            string driverName;
            bool driverFound;
            int driverID = 0;
            uint newMsgID;

            uint msgType = checkPointTracer.getMsgType(msgID);  //find the message in the checkpoint process
            if (msgType == Msgs.MSG_PAUSE)
            {  //if done checkpoint pause
                //firstly check if this driving variable is registered
                driverName = checkPointTracer.getDriver(msgID);
                driverFound = getDriverByName(driverName, ref driverID);

                //if the driver is not registered then
                if (!driverFound)
                {                                //if I need a new driver
                    driverID = driverList.Count;                   //default to the last index in the list

                    TDriverInfo newDriver = createDriver(driverName, driverID, 1, 1, "", false, TTypedValue.STYPE_STR, "", "", 0);

                    while (driverList.Count <= driverID)           //keep the driver list indexed by driverID
                        driverList.Add(null);
                    driverList[driverID] = newDriver;

                    //now register the new driver
                    IntPtr msg = new IntPtr(0);
                    newMsgID = BuildMsg(pWrapper, Msgs.MSG_REGISTER, 0, TypeSpec.KIND_DRIVER, driverID, 0, driverName, TTypedValue.STYPE_STR, null, 0, 1, ref msg);
                    //log the checkpoint by driverID, msgID
                    checkPointTracer.updateMsg(msgID, newMsgID, Msgs.MSG_REGISTER, driverID);  //allows me to track this process from the doComplete()
                    SendMsg(pWrapper, msg);                            //when registration is complete, the getvalue is sent
                }
                else
                {
                    //do a driver request using getValue for the matching driver ID
                    // unsigned driverID = checkPointTracer->getDriverID(msgID);
                    IntPtr msg = new IntPtr(0);
                    newMsgID = buildGetValueMsg(driverID, ref msg);
                    checkPointDriverID = driverID;
                    checkPointTracer.updateMsg(msgID, newMsgID, Msgs.MSG_GETVALUE, driverID);
                    SendMsg(pWrapper, msg);
                }
            }
            if (msgType == Msgs.MSG_REGISTER)
            {
                //do a driver request using getValue for the matching driver ID
                driverID = checkPointTracer.getDriverID(msgID);
                IntPtr msg = new IntPtr(0);
                newMsgID = buildGetValueMsg(driverID, ref msg);
                checkPointDriverID = driverID;
                checkPointTracer.updateMsg(msgID, newMsgID, Msgs.MSG_GETVALUE, driverID);
                SendMsg(pWrapper, msg);
            }
            if (msgType == Msgs.MSG_GETVALUE)
            {                     //this is a reply to my getValue
                driverID = checkPointTracer.getDriverID(msgID);
                checkPointTracer.clearDriverProcess(driverID); //clear the checkpoint tracer
                storeCheckPoint(driverID);                      //stores the checkpoint value using the overridden function
                deleteDriver(driverID);
                checkPointDriverID = -1;
                sendPauseSim(false);                            //resume the simulation
            }
        }
        //==============================================================================
        /// <summary>
        /// Continues the process of restoring a checkpoint.
        /// </summary>
        /// <param name="msgID">The ID of the msg that is completed.</param>
        //==============================================================================
        protected void processCheckPointRestore(uint msgID)
        {
            string setterName;
            bool setterNameFound;
            int setterID = 0;
            uint newMsgID;

            uint msgType = checkPointTracer.getMsgType(msgID);  //find the message in the checkpoint process
            if (msgType == Msgs.MSG_PAUSE)
            {  //if done checkpoint pause
                //firstly check if this Setter variable is registered
                setterName = checkPointTracer.getDriver(msgID);
                setterNameFound = getSetterByName(setterName, ref setterID);

                //if the setter is not registered then
                if (!setterNameFound)
                {
                    setterID = setPropertyList.Count;               //default to the last index in the list

                    TSetterProperty newSetter = createSetterProperty(setterName, setterID, setterName, 0, "", false, TTypedValue.STYPE_STR);

                    while (setPropertyList.Count <= setterID)        // We are going to index directly into list later
                        setPropertyList.Add(null);
                    //store the SDML for the restore value. It will be sent in the requestSetValue msg
                    newSetter.setValue(checkPointTracer.getValue(msgID));
                    setPropertyList[setterID] = newSetter;

                    //now register the new setter
                    IntPtr msg = new IntPtr(0);
                    newMsgID = BuildMsg(pWrapper, Msgs.MSG_REGISTER, 0, TypeSpec.KIND_REQUESTSET, setterID, 0, setterName, TTypedValue.STYPE_STR, null, 0, 1, ref msg);
                    //log the checkpoint by setterID, msgID
                    checkPointTracer.updateMsg(msgID, newMsgID, Msgs.MSG_REGISTER, setterID);  //allows me to track this process from the doComplete()
                    SendMsg(pWrapper, msg);                            //when registration is complete, the requestSetValue is sent
                }
                else
                {
                    //do a requestSetValue using for the matching setter ID
                    setterID = checkPointTracer.getDriverID(msgID);
                    IntPtr msg = new IntPtr(0);
                    newMsgID = buildRequestSetMsg(setterID, (TSetterProperty)setPropertyList[setterID], ref msg);
                    checkPointTracer.updateMsg(msgID, newMsgID, Msgs.MSG_REQUESTSET, setterID);
                    SendMsg(pWrapper, msg);
                }
            }
            else if (msgType == Msgs.MSG_REGISTER)
            {
                //do a requestSetValue using for the matching setter ID
                setterID = checkPointTracer.getDriverID(msgID);
                IntPtr msg = new IntPtr(0);
                ((TSetterProperty)setPropertyList[setterID]).setValue(checkPointTracer.getValue(msgID));
                newMsgID = buildRequestSetMsg(setterID, (TSetterProperty)setPropertyList[setterID], ref msg);
                checkPointTracer.updateMsg(msgID, newMsgID, Msgs.MSG_REQUESTSET, setterID);
                SendMsg(pWrapper, msg);
            }
        }
        //============================================================================
        /// <summary>
        /// Sends a reply value message to the replyTo component.
        /// </summary>
        /// <param name="queryMsgID"></param>
        /// <param name="replyTo"></param>
        /// <param name="sParamType"></param>
        /// <param name="aParams"></param>
        /// <param name="paramsSize"></param>
        //============================================================================
        protected override void sendReplyValue(uint queryMsgID, uint replyTo, string sParamType, byte[] aParams, int paramsSize)
        {
            IntPtr msg = new IntPtr(0);

            BuildMsg(pWrapper, Msgs.MSG_REPLYVALUE, replyTo, (int)queryMsgID, 0, 0, sParamType, "", aParams, paramsSize, 0, ref msg);
            SendMsg(pWrapper, msg);

        }
        //==============================================================================
        /// <summary>
        /// Search the driver list for the driver specified by driverName.
        /// </summary>
        /// <param name="driverName">FQN of the driver.</param>
        /// <param name="driverID">Return the driver ID.</param>
        /// <returns>True if the driver is found.</returns>
        //==============================================================================
        protected bool getDriverByName(string driverName, ref int driverID)
        {
            TDriverInfo driver;
            bool driverFound = false;

            int i = 0;
            while ((!driverFound) && (i < driverList.Count))
            {
                driver = (TDriverInfo)driverList[i];
                if ((driver != null) && (driver.Name.ToLower() == driverName.ToLower()))
                {
                    driverFound = true;
                    driverID = i;           //the driver that was found
                }
                else
                    i++;
            }
            return driverFound;
        }
        //============================================================================
        /// <summary>
        /// Called when the component is made active or inactive
        /// </summary>
        /// <param name="bActiveNow"></param>
        //============================================================================
        protected void doActivation(bool bActiveNow)
        {
            FActive = bActiveNow;
        }
        //============================================================================
        /// <summary>
        /// Use the event handling system to execute a piece of purely internal logic.
        /// Typically used to process "init2".
        /// The event must be a published event.
        /// </summary>
        /// <param name="eventID"></param>
        //============================================================================
        protected void executeInternalEvent(int eventID)
        {
            TEventInfo EventInfo;

            try
            {
                EventInfo = (TEventInfo)eventList[eventID];
                if ((EventInfo.iKind != TypeSpec.KIND_PUBLISHEDEVENT) || (EventInfo.sDDML.ToLower() != TypeSpec.TYPEEMPTY.ToLower()))
                {
                    string errorMsg = string.Format("{0}: Exception in executeInternalEvent()", FName);
                    throw (new ApplicationException(errorMsg));
                }
                else
                    doEvent(0, 0, eventID, 0, false, "", null, 0);
            }
            catch (Exception e)
            {
                string errorMsg = string.Format("executeInternalEvent(): {0}\nInvalid attempt to generate internal event # {1}", e.Message, eventID);
                sendError(errorMsg, true);
            }
        }
        //============================================================================
        /// <summary>
        /// Submit a request for information about any entities matching a given kind 
        /// and name. The name may be qualified and may contain wildcards.
        /// </summary>
        /// <param name="sName">Name of entity to query. Can contain wildcards for the
        /// property of event name.</param>
        /// <param name="iKind">Kind of entity to query.</param>
        /// <param name="eventID"></param>
        //============================================================================
        protected void sendQueryInfo(string sName, int iKind, int eventID)
        {
            uint msgID;
            IntPtr msg = new IntPtr(0);

            msgID = buildQueryInfo(sName, iKind, ref msg);

            if (eventID != 0)
                eventsManager.addRequest(eventID, msgID);

            SendMsg(pWrapper, msg);
        }
        //==============================================================================
        /// <summary>
        /// Name of entity to query. Can contain wildcards for the
        /// property of event name.
        /// </summary>
        /// <param name="sName">Kind of entity to query.</param>
        /// <param name="iKind"></param>
        /// <param name="msg">Ref to the new message.</param>
        /// <returns></returns>
        //==============================================================================
        protected uint buildQueryInfo(string sName, int iKind, ref IntPtr msg)
        {
            uint newMsgID;

            newMsgID = BuildMsg(pWrapper, Msgs.MSG_QUERYINFO, 0, iKind, 0, 0, sName, null, null, 0, 1, ref msg);

            TQueryStore queryStore = new TQueryStore();
            queryStore.iSentMsgID = newMsgID;
            queryStore.sName = sName;
            queryList.Add(queryStore);

            return newMsgID;
        }
        /// <summary>
        /// 
        /// </summary>
        /// <param name="sScriptName"></param>
        public virtual void createInitScript(string sScriptName)
        {
        }
        /// <summary>
        /// 
        /// </summary>
        /// <param name="sScriptName"></param>
        public virtual void deleteInitScript(string sScriptName)
        {
        }
    }
}
