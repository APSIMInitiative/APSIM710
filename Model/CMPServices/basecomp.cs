using System;
using System.Collections.Generic;
using System.Text;
using System.Collections;
using System.Runtime.InteropServices;
using System.Xml;

namespace CMPServices
{
    /// <summary>
    /// Used as the callback into the CMP engine
    /// </summary>
    /// <param name="inMsg"></param>
    
    //[UnmanagedFunctionPointer(CallingConvention.StdCall)]
    public delegate void MessageFromLogic(TMsgHeader inMsg);

    //============================================================================
    /// <summary>
    /// The base component for a non system model component in the CMP framework
    /// that does not need the mixed mode piwrapper. See <see cref="TFarmwiseInstance"/>.
    /// This is the case when using the full .net engine.
    /// </summary>
    //============================================================================
    public abstract class TBaseComp : TAbstractComponent
    {
        /// <summary>
        /// List of messages that have been sent and await acknowledgement
        /// </summary>
        protected TMessageHdrList ackList;              
        /// <summary>
        /// track queryInfo msg's
        /// </summary>
        protected TQueryInfoTracker queryInfoTracker;   
        /// <summary>
        /// 
        /// </summary>
        protected TMessageInterpreter interpreter;
        /// <summary>
        /// The events manager
        /// </summary>
        private TEventsManager eventsManager;           
        /// <summary>
        /// temporary storage of error message from init1
        /// </summary>
        private string init1Error;                      

        /// <summary>
        /// Keeping track of drivers requested
        /// </summary>
        protected List<TIDSpec> driverIDList;           
        internal TCheckPointTracer checkPointTracer;    //allows the following of the checkpoint process
        /// <summary>
        /// The object that is used for tracking the routing of messages.
        /// </summary>
        protected TMsgDirector msgDirector;             
        /// <summary>
        /// ID of the driver used for checkpointing
        /// </summary>
        protected int checkPointDriverID;
        /// <summary>
        /// Callback to the simulation engine
        /// </summary>
        protected MessageFromLogic msgDestFunction = null;
        private TInitScriptManager FScriptManager;
        /// <summary>
        /// The parser object constructed from the component SDML
        /// </summary>
        protected TCompParser FSDMLComp;

        //============================================================================
        /// <summary>
        /// Constructor for a TBaseComp. Adds a published error event.
        /// </summary>
        /// <param name="ID">ID of this component.</param>
        /// <param name="parentID">ID of the parent component.</param>
        /// <param name="msgCallBack">Delegate function to be called in the simulation engine.</param>
        /// <param name="sType">Class type of the component.</param>
        /// <param name="sVersion">Version number string.</param>
        /// <param name="sAuthor">Author's name.</param>
        //============================================================================
        protected TBaseComp(uint ID, uint parentID, MessageFromLogic msgCallBack,  
                         string sType, string sVersion, string sAuthor)
            : base()
        {
            FMyID = ID;
            FParentID = parentID;
            FScriptManager = null;  //will only be created when needed
            FSDMLComp = null;

            //could use System.Reflection.Assembly.GetExecutingAssembly().Location : to find the callback into the engine
            msgDestFunction = msgCallBack;  //store a ref to the delegate class function here
            
            driverIDList = new List<TIDSpec>();
            ackList = new TMessageHdrList();                //tracks the complete msg's
            queryInfoTracker = new TQueryInfoTracker();     //tracks queryInfo msg's
            interpreter = new TMessageInterpreter(FMyID);
            interpreter.initMsgIDCounter(FMyID * 1000000);     //ensure msgID's are interesting

            init1Error = "";
            FModulePathName = getModulePath();

            checkPointTracer = new TCheckPointTracer();
            checkPointDriverID = -1;
            msgDirector = new TMsgDirector();

            FType = sType;
            FVersion = sVersion;
            FAuthor = sAuthor;

            eventsManager = new TEventsManager(this);

            addEvent("error", EVTERROR, TypeSpec.KIND_PUBLISHEDEVENT, TypeSpec.typeERROR, "", "", 0);    //Publish the standard "error" event
        }
        //============================================================================
        /// <summary>
        /// Main message handler for the component.
        /// </summary>
        /// <param name="msg">Incoming message.</param>
        //============================================================================       
        public virtual void handleMessage(TMsgHeader msg)
        {
            uint dataSize;
            int id;
            uint iCompID;
            uint iPropertyID;
            bool bSuccess;
            
            uint msgID = msg.msgID;           //store some message details in local vars
            uint msgFrom = msg.from;

            switch (msg.msgType)
            {
                case Msgs.MSG_COMMENCE:
                    {
                        doCommence(msgID);
                    }
                    break;
                case Msgs.MSG_COMPLETE: //msg that signifies completion
                    {
                        interpreter.loadMessage(msg);    //take ownership of this msg
                        uint ackID = (uint)interpreter.getIntField(Msgs.MSG_COMPLETE_ACKID);
                        doComplete(msg.from, ackID);
                    }
                    break;
                case Msgs.MSG_EVENT:
                    {
                        interpreter.loadMessage(msg);    //take ownership of this msg
                        id = interpreter.getIntField(Msgs.MSG_EVENT_ID);
                        byte[] dataPtr;
                        dataSize = interpreter.getValueField(Msgs.MSG_EVENT_PARAMS, out dataPtr);
                        uint publBy = (uint)interpreter.getIntField(Msgs.MSG_EVENT_PUBLISHEDBY);
                        string DDML = interpreter.getTextField(Msgs.MSG_EVENT_TYPE);
                        doEvent(msgFrom, publBy, id, msgID, (msg.toAck > 0), DDML, dataPtr, dataSize);
                    }
                    break;
                case Msgs.MSG_INIT1:
                    {
                        interpreter.loadMessage(msg);    //take ownership of this msg
                        string SDML = interpreter.getTextField(Msgs.MSG_INIT1_SDML);
                        string FQN = interpreter.getTextField(Msgs.MSG_INIT1_FQN);
                        doInit1(interpreter.getMsg(), SDML, FQN);     //do init1 processing
                        sendComplete(msgFrom, msgID);                 //logic component now sends a complete 
                    }
                    break;
                case Msgs.MSG_INIT2:
                    {
                        handleInit2(msg);   //after processing sends a Complete
                        sendComplete(msg.from, msg.msgID);     //non system logic component now sends a complete
                    }
                    break;
                //get driving property value
                case Msgs.MSG_QUERYVALUE:   //request comes from another system
                    {
                        handleValueRequest(msg);
                    }
                    break;
                case Msgs.MSG_NOTIFYDELETE:
                    {
                        doAtDeletion(msgFrom, msgID);
                    }
                    break;
                case Msgs.MSG_NOTIFYSET:
                    {
                        interpreter.loadMessage(msg);    //take ownership of this msg
                        iPropertyID = (uint)interpreter.getIntField(Msgs.MSG_NOTIFYSET_ID);
                        bSuccess = interpreter.getBoolField(Msgs.MSG_NOTIFYSET_SUCCESS);
                        doSetSuccess(msgFrom, iPropertyID, bSuccess);  //send it to the component dll
                    }
                    break;
                case Msgs.MSG_NOTIFYTERMINATION:
                    {
                        doAtTermination(msgFrom, msgID);
                    }
                    break;
                case Msgs.MSG_PAUSE:
                    {
                        doPause(msgFrom, msgID);
                    }
                    break;
                //Alter owned property
                case Msgs.MSG_QUERYSET:
                    {
                        handleQuerySetValue(msg);
                    }
                    break;
                case Msgs.MSG_RESUME:
                    {
                        doResume(msgID);
                    }
                    break;
                case Msgs.MSG_RETURNVALUE:
                    {
                        interpreter.loadMessage(msg);    //take ownership of this msg
                        iCompID = (uint)interpreter.getIntField(Msgs.MSG_RETURNVALUE_COMPID);
                        iPropertyID = (uint)interpreter.getIntField(Msgs.MSG_RETURNVALUE_ID);
                        string DDML = interpreter.getTextField(Msgs.MSG_RETURNVALUE_TYPE);
                        byte[] dataPtr;
                        dataSize = interpreter.getValueField(Msgs.MSG_RETURNVALUE_VALUE, out dataPtr);
                        doSetDriver(iPropertyID, iCompID, DDML, dataPtr, dataSize);     //this is always the destination
                    }
                    break;
                case Msgs.MSG_RETURNINFO:
                    {
                        handleReturnInfo(msg);
                    }
                    break;
            }
        }
        //============================================================================
        /// <summary>
        /// Sends a <see cref="Msgs.MSG_REGISTER">MSG_REGISTER</see> message that will register
        /// an item in the simulation.
        /// </summary>
        /// <param name="sName">Name of the item.</param>
        /// <param name="sDDML">DDML type of the item.</param>
        /// <param name="lCodeID">Local ID of the item.</param>
        /// <param name="lKind">Kind of item to register.</param>
        /// <param name="destID">Optional integer ID for a destination component.
        ///	
        /// <para>If kind=<see cref="TypeSpec.KIND_DRIVER">See TypeSpec.KIND_DRIVER</see> or <see cref="TypeSpec.KIND_DRIVER">TypeSpec.KIND_REQUESTSET</see>, 
        /// identifies a component that owns a property corresponding to the driving property or property set request being registered. 
        /// Value requests for the driving property must be routed to the nominated component only.</para>
        ///	
        /// <para>If kind=<see cref="TypeSpec.KIND_PUBLISHEDEVENT">TypeSpec.KIND_PUBLISHEDEVENT</see>, identifies a component that subscribes to the event being published. 
        /// When the event is published, it must be routed to the nominated component only.</para>
        /// 
        ///	<para>If kind is <see cref="TypeSpec.KIND_OWNED_R">TypeSpec.KIND_OWNED_R</see>, 
        /// <see cref="TypeSpec.KIND_OWNED_W">TypeSpec.KIND_OWNED_W</see>, 
        /// <see cref="TypeSpec.KIND_OWNED_RW">TypeSpec.KIND_OWNED_RW</see> or 
        /// <see cref="TypeSpec.KIND_SUBSCRIBEDEVENT">TypeSpec.KIND_SUBSCRIBEDEVENT</see>, destID must be zero.</para> 
        /// 
        /// <para>If destID=0, this field is ignored and routing depends upon the protocol implementation.</para>
        /// </param>
        /// <param name="readable">Readable flag.</param>
        /// <param name="writeable">Writeable flag.</param>
        /// <param name="toAck">Acknowledge this message 1=true.</param>
        //============================================================================
        protected override void sendRegistration(string sName, string sDDML, int lCodeID,
                                        int lKind, uint destID,
                                        bool readable, bool writeable, int toAck)
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
            TMsgHeader msg = buildRegisterMsg(sName, sDDML, lCodeID, lParam1, destID);
            sendMessage(msg);
        }
        //============================================================================
        /// <summary>
        /// Generate a "deregister" message for the item and send it.
        /// </summary>
        /// <param name="lKind">Kind of item to deregister. <see cref="TypeSpec.KIND_DRIVER">See TypeSpec.KIND_DRIVER</see></param>
        /// <param name="lCodeID">Local ID of the item.</param>
        /// <seealso cref="sendRegistration">See sendRegistration()</seealso>
        //============================================================================
        protected override void sendDeregistration(int lKind, int lCodeID)
        {
            interpreter.setField(Msgs.MSG_DEREGISTER_KIND, lKind);  //the kind of entity to deregister
            interpreter.setField(Msgs.MSG_DEREGISTER_ID, lCodeID);  //its ID
            TMsgHeader msg = interpreter.createMessage(Msgs.MSG_DEREGISTER, FParentID);

            sendMessage(msg);
        }
        //============================================================================
        /// <summary>
        /// Sends a MSG_REQUESTSET to attempt setting a property of another component.
        /// Registers the 'setter' property first if it is not already registered.
        /// </summary>
        /// <param name="sPropFQN">FQN of the property to set.</param>
        /// <param name="sDDML">DDML type string.</param>
        /// <param name="value">The value of the property. (Can be more than just a scalar)</param>
        //============================================================================
        protected void sendRequestSet(String sPropFQN, String sDDML, TTypedValue value)
        {
            TMsgHeader msg;
            int newSetterID = 0;

            if (!getSetterByName(sPropFQN, ref newSetterID))
            {
                newSetterID = setPropertyList.Count; //next id. Assume array index is the ID.
                //do the registration of the new setter
                addSetterProperty(sPropFQN, newSetterID, sPropFQN, 0, "", false, sDDML);
            }
            setPropertyList[newSetterID].destName = sPropFQN; //store fqn of the dest state property
            setPropertyList[newSetterID].regID = (uint)newSetterID;
            setPropertyList[newSetterID].setValue(value); //store the value
            msg = buildRequestSetMsg(newSetterID, (TTypedValue)setPropertyList[newSetterID]);
            if (msg.msgID != 0)
                sendMessage(msg);
        }
        //============================================================================
        /// <summary>
        /// Attempt to set the value of another component's owned property.
        /// Assumes that the Setter property has already been registered.
        /// </summary>
        /// <param name="localSetPropID">Local identifier for the Setter property.</param>
        /// <param name="Value">Value to which to set the property.</param>
        //============================================================================
        public void sendReqSetValue(int localSetPropID, TTypedValue Value)
        {
            TMsgHeader msg = buildRequestSetMsg(localSetPropID, setPropertyList[localSetPropID]);
            if (msg.msgID != 0)
                sendMessage(msg);
        }
        //============================================================================
        /// <summary>
        /// Sends an error message to our owning system.
        /// </summary>
        /// <param name="sMessage">Message that gets logged.</param>
        /// <param name="bFatal">True if fatal. Usually terminates simulation.</param>
        //============================================================================
        public override void sendError(string sMessage, bool bFatal)
        {
            interpreter.setField(Msgs.MSG_ERROR_FATAL, bFatal);  
            interpreter.setField(Msgs.MSG_ERROR_MESSAGE, sMessage);  //its ID
            TMsgHeader msg = interpreter.createMessage(Msgs.MSG_ERROR, FParentID);
            sendMessage(msg);
            /*
            //send the error to the system if a wrapper exists
            if ( (eventList.Count > 0) && (msgDestFunction != null) )
            {
                eventList[EVTERROR].member("fatal").setValue(bFatal);
                string sMsg = "(" + FName + ") " + sMessage;
                eventList[EVTERROR].member("message").setValue(sMsg);
                sendPublishEvent(EVTERROR, bFatal);
            }
            else
                if (bFatal)
                    throw (new ApplicationException(sMessage));//MessageBox.Show(sMessage);
            */
        }
        //============================================================================
        /// <summary>
        /// Sends a reply value message to the replyTo component.
        /// </summary>
        /// <param name="queryMsgID">ID of queryValue message</param>
        /// <param name="replyTo">Reply to this component.</param>
        /// <param name="sParamType">DDML type.</param>
        /// <param name="aParams">Data for the returning value.</param>
        /// <param name="paramsSize">Size in bytes of the data.</param>
        //============================================================================
        protected override void sendReplyValue(uint queryMsgID, uint replyTo, string sParamType, byte[] aParams, int paramsSize)
        {
            interpreter.setField(Msgs.MSG_REPLYVALUE_QUERYID, queryMsgID);        //ID of queryValue
            interpreter.setField(Msgs.MSG_REPLYVALUE_TYPE, sParamType);           //DDML type
            interpreter.setField(Msgs.MSG_REPLYVALUE_VALUE, aParams, (uint)paramsSize);
            TMsgHeader msg = interpreter.createMessage(Msgs.MSG_REPLYVALUE, replyTo);

            sendMessage(msg);
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
            return System.Reflection.Assembly.GetAssembly(this.GetType()).Location; 
            // GetEntryAssembly().Location; // GetCallingAssembly().Location;// GetExecutingAssembly().Location;
        }
        //============================================================================
        /// <summary>
        /// Get the name of the wrapper dll that is used for all communication with
        /// this component. Since this .net component never uses a wrapper, it will
        /// return the name of this component.
        /// </summary>
        /// <returns>Full path name of the wrapper dll.</returns>
        //============================================================================
        public virtual string wrapperDll()
        {
            return getModulePath();
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
        /// Adds an event to the local event list and registers it. The descendant object
        /// should call this function to add events.
        /// </summary>
        /// <param name="sName">Name of the event.</param>
        /// <param name="eventID">Local ID of the event.</param>
        /// <param name="iKind">Published or Subscribed.</param>
        /// <param name="sType">The type. Can be DDML or the type name e.g. TTypedValue.STYPE_STR</param>
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
                String errorMsg = String.Format(FName + " addEvent(): {0}", e.Message);
                sendError(errorMsg, true);
            }
        }
        //==============================================================================
        /// <summary>
        /// Used by a Sequencer service to start the simulation. Override in child class.
        /// </summary>
        /// <param name="msgID">Message ID</param>
        //==============================================================================
        public virtual void doCommence(uint msgID)
        {
        }

        //============================================================================
        /// <summary>
        /// Handles the complete message that is sent via the wrapper prot.dll
        /// If this is a Complete to an earlier request from a TMEvent, then the event
        /// manager can use this Complete to resume the event processing. If the message
        /// ID is not found in the event manager's list then it is ignored.
        /// </summary>
        /// <param name="msgFrom">Source of the Complete.</param>
        /// <param name="origMsgID">ID of the original message that this Complete is sent for.</param>
        //============================================================================
        protected virtual void doComplete(uint msgFrom, uint origMsgID)
        {
            try
            {
                eventsManager.completeRequest(origMsgID);
                //Remove stored information about previously sent queryInfo messages
                foreach (TQueryStore query in queryList)
                {
                    if (query.iSentMsgID == origMsgID)
                    {
                        queryList.Remove(query);
                        break;
                    }
                }
                foreach (TDriverInfo driver in driverList)
                    {                                                            
                    //  If this "complete" is for a driving
                    //  property request, then check for
                    //  the correct number of answers
                    if ((driver != null) && driver.bRequestActive && (driver.iRequestMsg == origMsgID))
                    {
                        CheckDriverCount(driver);                                 //index of the driver=driver ID
                        driver.bRequestActive = false;

                        //determine if this is a Complete for state variables in a queryValue(state) process (when a system)
                        if (msgDirector.isABranch(Msgs.MSG_QUERYVALUE, FParentID, origMsgID))
                        {
                            TTrunkMsg srcMsg = new TTrunkMsg();
                            if (msgDirector.getBranch(Msgs.MSG_QUERYVALUE, FParentID, origMsgID, ref srcMsg))
                            {
                                if (msgDirector.pruneBranch(Msgs.MSG_QUERYVALUE, FParentID, origMsgID) == 0)   //if this is the last 'state' value from the children then
                                {
                                    replySystemStateValue(srcMsg.returnToCompID, srcMsg.inMsgID);
                                }
                            }
                        }
                        break;
                    }
                }

                if (bFatalErrorSent && (origMsgID == iFatalErrorID))      // Once a fatal ERROR event has been
                    sendEndSimulation();                               //   acknowledged, send "terminate"

                //Processing checkpointing for this component doing the checkpointing.
                if (checkPointTracer.isCheckPointMsg(origMsgID))
                    processCheckPointing(origMsgID);
                else if (checkPointTracer.isCheckPointRestoreMsg(origMsgID))
                    processCheckPointRestore(origMsgID);
            }
            catch (Exception e)
            {
                string errorMsg = string.Format(FName + " doComplete(): {0}", e.Message);
                sendError(errorMsg, true);
            }
            finaliseSentMsg(origMsgID);       //take sent msg off list
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
        protected void doEvent(uint msgFrom, uint publBy, int eventID, uint msgID, bool requiresAck, string prmDDML,
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
                                string errorMsg = string.Format("{0}: Type of value passed for event ({1}) parameter is incompatible.", FName, eventID);
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
                string errorMsg = string.Format(FName + " doEvent(): {0}", e.Message);
                sendError(errorMsg, true);
            }

        }

        //============================================================================
        /// <summary>
        /// Called by the parent component to initialise this component. This function
        /// registers the driving properties, events, and properties and then calls the
        /// initialise(1 or 2) routine.
        /// </summary>
        /// <param name="msg">The Init1 message.</param>
        /// <param name="SDML">SDML script for this component.</param>
        /// <param name="FQN">Fully qualified name of this component.</param>
        //============================================================================
        protected virtual void doInit1(TMsgHeader msg, string SDML, string FQN)
        {
            if (FSDMLComp == null)
                FSDMLComp = new TCompParser(SDML);   //Parser for init script
            FSystem = FSDMLComp.IsSystem;

            FName = FQN;
            FVersion = FSDMLComp.Version;

            try
            {
                TDriverInfo dvrInfo;
                TEventInfo evInfo;
                int i;

                try
                {
                    init1Error = "";
                    FMyID = msg.to;         //store the ID chosen for this component
                    FParentID = msg.from;   //store the parent's (system) ID. init1's alway s come from the owner.

                    FActive = true;//**** CompParser.isActive;
                    //this is set by the component constructor         FSystem = compParser->isSystem();

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

                    initAllInits(FSDMLComp);

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

                    bRegisterNow = true;

                    if (init1Error.Length > 0)
                    {
                        string errorMsg = string.Format(FName + " Error in doInit1(): {0}", init1Error);
                        sendError(errorMsg, true);
                    }
                    else
                        initialise(1);                                                      //Component-specific logic
                }
                catch (Exception e)
                {
                    init1Error = e.Message;
                }
            }
            catch (Exception error)
            {
                sendError(FName + " TBaseComp::doInit1(): " + error.Message, true);  
            }
        }
        //============================================================================
        /// <summary>
        /// Calls the virtual function initialise(2) to execute user's component code.
        /// </summary>
        /// <param name="msg">Init2 message.</param>
        //============================================================================
        protected void doInit2(TMsgHeader msg)
        {
            try
            {
                initialise(2);
            }
            catch (Exception e)
            {
                string errorMsg = string.Format(FName + " Error calling initialise(2): {0}", e.Message);
                sendError(errorMsg, true);
            }
        }
        //============================================================================      
        /// <summary>
        /// Handle init2 messages in this non system component.
        /// </summary>
        /// <param name="msg">Init2 message.</param>
        //============================================================================      
        protected virtual void handleInit2(TMsgHeader msg)
        {
            doInit2(msg);
        }
        //============================================================================      
        /// <summary>
        /// Initialise the component. Will be overridden by derived classes to execute
        /// custom component code for the respective init stage. This is called after
        /// initProperty() is called.
        /// </summary>
        /// <param name="iStage">Init stage 1 or 2</param>
        //============================================================================
        public abstract void initialise(int iStage);
        //============================================================================
        /// <summary>
        /// Called in response to a MSG_GETVALUE -> MSG_QUERYVALUE request.
        /// </summary>
        /// <param name="msg">Incoming QueryValue message.</param>
        //============================================================================
        protected void handleValueRequest(TMsgHeader msg)
        {
            uint iReplyTo = msg.from;          //the router

            interpreter.loadMessage(msg);
            //expect the wrapper to trigger a returnValue message
            uint iPropertyID = (uint)interpreter.getIntField(Msgs.MSG_QUERYVALUE_ID);
            uint requestedByID = (uint)interpreter.getIntField(Msgs.MSG_QUERYVALUE_REQBY);       //source of the request

            doQueryProperty(iPropertyID, requestedByID, msg.msgID, iReplyTo);   //logic dll then sends a ReplyValue
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
                String errorMsg = String.Format(FName + " {0}\n Error in readFromPropertyList(). Property: {1}", e.Message, propertyID);
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
        protected void doAtDeletion(uint termMsgFrom, uint termMsgID)
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
                string errorMsg = string.Format(FName + " doAtDeletion(): {0}", e.Message);
                sendError(errorMsg, true);
            }
        }
        //==============================================================================
        /// <summary>
        /// Handles an incoming notifySetValueSuccess message. MSG_NOTIFYSET
        /// Calls the virtual function processNotifySet()
        /// </summary>
        /// <param name="msgFrom">ID of message sender.</param>
        /// <param name="propertyID">The property ID.</param>
        /// <param name="bSuccess">True if successful.</param>
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
        /// Responds to the termination message from the system. Tells the statemachine
        /// that it should terminate when it arrives at DONE. If no events are running
        /// then a complete() message is sent from here.
        /// </summary>
        /// <param name="termMsgFrom">Sender's ID of the termination message.</param>
        /// <param name="termMsgID">Message ID of the termination message.</param>
        //============================================================================
        protected virtual void doAtTermination(uint termMsgFrom, uint termMsgID)
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
                string errorMsg = string.Format(FName + " doAtTermination(): {0}", e.Message);
                sendError(errorMsg, true);
            }
        }
        //==============================================================================
        /// <summary>
        /// Used by a Sequencer service to pause the simulation. Override in child class.
        /// </summary>
        /// <param name="msgFrom">Component source for this message.</param>
        /// <param name="msgID">Message ID</param>
        //==============================================================================
        public virtual void doPause(uint msgFrom, uint msgID)
        {
        }
        //==============================================================================
        /// <summary>
        /// Handles a querySet message.
        /// </summary>
        /// <param name="msg">Incoming querySet message.</param>
        //==============================================================================
        protected virtual void handleQuerySetValue(TMsgHeader msg)
        {
            uint iPropertyID;
            uint iReplyTo;
            uint msgID;
            uint dataSize;
            string DDML;

            interpreter.loadMessage(msg);    //take ownership of this msg
            iReplyTo = msg.from;
            msgID = msg.msgID;
            iPropertyID = (uint)interpreter.getIntField(Msgs.MSG_QUERYSET_ID);           // Parse the request message
            byte[] dataPtr;
            dataSize = interpreter.getValueField(Msgs.MSG_QUERYSET_VALUE, out dataPtr);
            DDML = interpreter.getTextField(Msgs.MSG_QUERYSET_TYPE);

            doResetProperty(msgID, iReplyTo, iPropertyID, DDML, dataPtr, dataSize);
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

                    while (resetList.Count <= (int)propertyID)                 //Make sure that we have a TTypedValue
                        resetList.Add(null);                               //available for storing the reset
                    if (resetList[(int)propertyID] != null)              //values. We can't use propertyList[]
                        resetValue = resetList[(int)propertyID];    //   because the type need only be
                    else
                    {                                                     //   compatible, not identical
                        resetValue = new TDDMLValue(sDDML, "");
                        if (ownedValue.isSameType(resetValue) == TTypedValue.ctBAD)    //Check for type compatibility 
                        {
                            string errorMsg = string.Format("{0}: Attempt to write value of wrong type to property \" {1} \" in doResetProperty()", FName, ownedValue.Name);
                            throw (new TypeMisMatchException(errorMsg));
                        }
                    }
                    resetValue.setData(valPtr, (int)valSize, 0);               //Now carry out the reset
                    resetList[(int)propertyID] = resetValue;
                    result = writeProperty((int)propertyID, resetValue);
                    sendReplySetValueSuccess(msgID, replyTo, result);     //sends the success? msg
                }
            }
            catch (Exception e)
            {
                string errorMsg = string.Format(FName + " doResetProperty(): {0} ", e.Message);
                sendError(errorMsg, true);
            }
        }
        //==============================================================================
        /// <summary>
        /// Used by a Sequencer service to restart the simulation. Override in child class.
        /// </summary>
        /// <param name="msgID">Message ID</param>
        //==============================================================================
        public virtual void doResume(uint msgID)
        {
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
            TDDMLValue passValue;

            if (providerID != FMyID || driverID == checkPointDriverID)
            {
                try
                {
                    aDriver = (TDriverInfo)driverList[(int)driverID];
                    if (!aDriver.checkCompatibility(providerID, sDDML))
                    {
                        string errorMsg = string.Format("{0}: Type of value passed for driving variable {1} is incompatible.", FName, driverID);
                        throw (new TypeMisMatchException(errorMsg));
                    }

                    passValue = aDriver.sourceValue(providerID);
                    passValue.setData(valPtr, (int)valSize, 0);
                    assignDriver((int)driverID, providerID, passValue);
                    driverList[(int)driverID].iConnCount++;
                }
                catch (Exception e)
                {
                    string errorMsg = string.Format(FName + " doSetDriver(): {0}", e.Message);
                    sendError(errorMsg, true);
                }
            }
        }
        //============================================================================
        /// <summary>
        /// Handles the returnInfo from a queryInfo message.
        /// </summary>
        /// <param name="msg">Incoming returnInfo message.</param>
        //============================================================================
        protected void handleReturnInfo(TMsgHeader msg)
        {

            interpreter.loadMessage(msg);
            uint queryID = (uint)interpreter.getIntField(Msgs.MSG_RETURNINFO_QUERYID);
            uint compID = (uint)interpreter.getIntField(Msgs.MSG_RETURNINFO_COMPID);
            uint entityID = (uint)interpreter.getIntField(Msgs.MSG_RETURNINFO_ID);
            uint kind = (uint)interpreter.getIntField(Msgs.MSG_RETURNINFO_KIND);

            string sFQN = interpreter.getTextField(Msgs.MSG_RETURNINFO_NAME);
            string sDDML = interpreter.getTextField(Msgs.MSG_RETURNINFO_TYPE);

            doInfoReturned(msg.from, queryID, compID, entityID, kind, sFQN, sDDML);
        }
        //============================================================================
        /// <summary>
        /// Called when a MSG_RETURNINFO msg arrives
        /// </summary>
        /// <param name="msgFrom">Source of this message.</param>
        /// <param name="queryMsgID">Message ID of the queryInfo() message.</param>
        /// <param name="ownerID">Component ID of the owning component.</param>
        /// <param name="entityID">ID of the entity requested.</param>
        /// <param name="iEntityKind">Kind of entity.</param>
        /// <param name="sEntityName">Name of the entity.</param>
        /// <param name="sDDML">DDML type description of the entity.</param>
        //============================================================================
        protected virtual void doInfoReturned(uint msgFrom, uint queryMsgID, uint ownerID, uint entityID, uint iEntityKind,
                                   string sEntityName, string sDDML)
        {
            TQueryStore query;

            try
            {
                int i = 0;
                //find the entity that was requested - by the queryInfo msg ID
                query = queryList[i];
                while ((i < queryList.Count) && (query.iSentMsgID != queryMsgID))
                {
                    i++;
                    query = queryList[i];
                }
                if (i == queryList.Count)
                {
                    string errorMsg = string.Format(FName + " {0}: Stored query information not found in doInfoReturned()", FName);
                    sendError(errorMsg, true);
                }
                else
                {
                    processEntityInfo(query.sName, sEntityName, ownerID, entityID, (int)iEntityKind, sDDML);
                }
            }
            catch (Exception e)
            {
                string errorMsg = string.Format(FName + " doInfoReturned(): {0} ", e.Message);
                sendError(errorMsg, true);
            }
        }
        //============================================================================
        /// <summary>
        /// Tests the number of values returned for a driving property against the
        /// valid range stored in the TDriverInfo structure. If it fails, then
        /// an error is published to the simulation.
        /// </summary>
        /// <param name="driverInfo">Driver to be checked.</param>
        //============================================================================
        protected void CheckDriverCount(TDriverInfo driverInfo)
        {
            //TDriverInfo driverInfo;
            bool bValid;
            StringBuilder sMessage;

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
                driverItem = driverIDList[it];
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
        //============================================================================
        /// <summary>
        /// Sends a message to the simulation to terminate.
        /// </summary>
        //============================================================================
        protected void sendEndSimulation()
        {
            //create and send the msg to the parent
            TMsgHeader newMsg = interpreter.createMessage(Msgs.MSG_TERMINATE, FParentID);
            sendMessage(newMsg);
        }
        //==============================================================================
        /// <summary>
        /// Continues the checkpointing process for a component that is doing the
        /// checkpointing. Called by doComplete().
        /// </summary>
        /// <param name="msgID">The ID of the msg that is completed.</param>
        //==============================================================================
        private void processCheckPointing(uint msgID)
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
                    TMsgHeader msg = buildRegisterMsg(driverName, TTypedValue.STYPE_STR, driverID, TypeSpec.KIND_DRIVER, 0);
                    //log the checkpoint by driverID, msgID
                    checkPointTracer.updateMsg(msgID, msg.msgID, Msgs.MSG_REGISTER, driverID);  //allows me to track this process from the doComplete()
                    sendMessage(msg);                            //when registration is complete, the getvalue is sent
                }
                else
                {
                    //do a driver request using getValue for the matching driver ID
                    // unsigned driverID = checkPointTracer->getDriverID(msgID);
                    TMsgHeader msg = new TMsgHeader();
                    newMsgID = buildGetValueMsg(driverID, ref msg);
                    checkPointDriverID = driverID;
                    checkPointTracer.updateMsg(msgID, newMsgID, Msgs.MSG_GETVALUE, driverID);
                    sendMessage(msg);
                }
            }
            if (msgType == Msgs.MSG_REGISTER)
            {
                //do a driver request using getValue for the matching driver ID
                driverID = checkPointTracer.getDriverID(msgID);
                TMsgHeader msg = new TMsgHeader();
                newMsgID = buildGetValueMsg(driverID, ref msg);
                checkPointDriverID = driverID;
                checkPointTracer.updateMsg(msgID, newMsgID, Msgs.MSG_GETVALUE, driverID);
                sendMessage(msg);
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
        private void processCheckPointRestore(uint msgID)
        {
            string setterName;
            bool setterNameFound;
            int setterID = 0;

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
                    TMsgHeader msg = buildRegisterMsg(setterName, TTypedValue.STYPE_STR, setterID, TypeSpec.KIND_REQUESTSET, 0);
                    //log the checkpoint by setterID, msgID
                    checkPointTracer.updateMsg(msgID, msg.msgID, Msgs.MSG_REGISTER, setterID);  //allows me to track this process from the doComplete()
                    sendMessage(msg);                            //when registration is complete, the requestSetValue is sent
                }
                else
                {
                    //do a requestSetValue using for the matching setter ID
                    setterID = checkPointTracer.getDriverID(msgID);
                    TMsgHeader msg = buildRequestSetMsg(setterID, setPropertyList[setterID]);
                    checkPointTracer.updateMsg(msgID, msg.msgID, Msgs.MSG_REQUESTSET, setterID);
                    sendMessage(msg);
                }
            }
            else if (msgType == Msgs.MSG_REGISTER)
            {
                //do a requestSetValue using for the matching setter ID
                setterID = checkPointTracer.getDriverID(msgID);
                setPropertyList[setterID].setValue(checkPointTracer.getValue(msgID));
                TMsgHeader msg = buildRequestSetMsg(setterID, setPropertyList[setterID]);
                checkPointTracer.updateMsg(msgID, msg.msgID, Msgs.MSG_REQUESTSET, setterID);
                sendMessage(msg);
            }
        }
        //============================================================================
        /// <summary>
        /// Removes the msg(msgID) from the list of msgs that are to be acknowledged.
        /// </summary>
        /// <param name="msgID"></param>
        //============================================================================
        protected void finaliseSentMsg(uint msgID)
        {
            ackList.removeMsg(msgID);
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
            interpreter.setField(Msgs.MSG_COMPLETE_ACKID, msgID);
            TMsgHeader ackMsg = interpreter.createMessage(Msgs.MSG_COMPLETE, msgTo);  //send back to the sender
            sendMessage(ackMsg);
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
        /// Initialises the value of a property.
        /// You must override this function in the child class to ensure that the
        /// system can initialise the value of an owned property. This is called
        /// during the init 1 stage and is called before initialise(1) is called.
        /// </summary>
        /// <param name="propertyID">Local id of the property.</param>
        /// <param name="aValue">Typed Value containing the definition and values.</param>
        //============================================================================
        public abstract void initProperty(int propertyID, TTypedValue aValue);
        //============================================================================
        /// <summary>
        /// Virtual function that the child class can override for cleanup tasks
        /// when the component receives a terminate message.
        /// </summary>
        //============================================================================
        public virtual void terminate()
        {

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
            // Construct a reply
            interpreter.setField(Msgs.MSG_REPLYSET_REQID, reqMsgID);    
            interpreter.setField(Msgs.MSG_REPLYSET_OK, success);
            interpreter.createMessage(Msgs.MSG_REPLYSET, replyTo);

            sendMessage(interpreter.getMsg());

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
        /// Set the value of the state property.
        /// </summary>
        /// <param name="reqMsgID">Incoming message ID.</param>
        /// <param name="replyTo">Reply to this component.</param>
        /// <param name="sDDML">DDML of the property.</param>
        /// <param name="valPtr">Data value.</param>
        /// <param name="valSize">Size of the data block.</param>
        //==============================================================================
        protected virtual void writeStateProperty(uint reqMsgID, uint replyTo, string sDDML, byte[] valPtr, uint valSize)
        {
            TCompParser compParser;
            TDDMLValue compDef;

            compDef = new TDDMLValue(sDDML, "");
            compDef.setData(valPtr, (int)valSize, 0);

            //init all the inits from the initsection
            compParser = new TCompParser(compDef.asStr());
            bool success = initAllInits(compParser);

            //send the reply immediately
            sendReplySetValueSuccess(reqMsgID, replyTo, success);

        }
        //============================================================================
        /// <summary>
        /// Initialise all the init properties from the TCompParser object.
        /// </summary>
        /// <param name="compParser">An instance of the TcompParser.</param>
        /// <returns>True if the initialisation was successful.</returns>
        //============================================================================
        protected Boolean initAllInits(TCompParser compParser)
        {
            TSDMLValue initValue;
            bool success = true;

            try
            {
                uint inits = compParser.initCount();
                for (uint i = 1; i <= inits; i++)
                {
                    initValue = new TSDMLValue(compParser.initText(i), "");     //create a TSDMLValue from the script section    
                    int propID = propertyNameToID(initValue.Name);
                    if (!initDefProperty(propID, initValue))                    //and pass it to initProperty() for
                        initProperty(propID, initValue);                        //handling by component-specific logic
                }
            }
            catch (Exception)
            {
                success = false;
            }
            return success;
        }
        //============================================================================
        /// <summary>
        /// Allows the simulation system to set the value of a local property.
        /// You must override this function in the child class to ensure that the system
        /// can set the local property value.
        /// </summary>
        /// <param name="propertyID">Local ID of the property.</param>
        /// <param name="aValue">Value to use</param>
        /// <returns></returns>
        //============================================================================
        public virtual bool writeProperty(int propertyID, TTypedValue aValue)
        {
            return false;
        }
        //============================================================================
        /// <summary>
        /// The Simulation system uses this function to set the value of a driving property.
        /// You must override this function in the child class to ensure that the
        /// system can set the value of a predefined driving property.
        /// This function should be called as the default when the driverID does not
        /// match any of the predefined driver ID's.
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
                        String errorMsg = String.Format("Cannot copy data for driving property {0} in assignDriver(): {1} " + FName, driverID, e.Message);
                        sendError(errorMsg, true);
                    }
                }
                else
                {
                    String errorMsg = String.Format("Cannot assign driving property {0} in assignDriver() " + FName, driverID);
                    sendError(errorMsg, true);
                }
            }
        }
        //============================================================================
        /// <summary>
        /// Place holder for derived classes to implement a handler for returnInfo message.
        /// </summary>
        /// <param name="sReqName">Name of the entity requested.</param>
        /// <param name="sReturnName">Name of the entity returned.</param>
        /// <param name="ownerID">ID of the owning component of the entity.</param>
        /// <param name="entityID">ID of the entity found.</param>
        /// <param name="iKind">Kind of entity.</param>
        /// <param name="sDDML">DDML type of the entity.</param>
        //============================================================================
        public virtual void processEntityInfo(string sReqName, string sReturnName, uint ownerID,
                                              uint entityID, int iKind, string sDDML)
        {

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
        /// Function that implements the storage of the checkpoint. This is the
        /// place to write the checkpoint to a file.
        /// Access the driver using: ((TDriverInfo)driverList[driverID]).asStr()
        /// To be overridden in the child object.
        /// </summary>
        /// <param name="driverID">Driving property ID.</param>
        //============================================================================
        protected virtual void storeCheckPoint(int driverID)
        {
            //child object may write this driver value to a file.
            //write this driver value to a file.
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
                interpreter.createMessage(Msgs.MSG_PAUSE, FParentID);
            else
                interpreter.createMessage(Msgs.MSG_RESUME, FParentID);

            sendMessage(interpreter.getMsg());
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
                setter = setPropertyList[i];
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
        /// <param name="sType">The type. Can be DDML or the type name e.g. TTypedValue.STYPE_STR</param>
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
        /// <param name="sType">The type. Can be DDML or the type name e.g. TTypedValue.STYPE_STR</param>
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
                string errorMsg = string.Format(FName + " addSetterProperty(): {0}", e.Message);
                sendError(errorMsg, true);
            }
        }
        //==============================================================================
        /// <summary>
        /// Constructs a MSG_REGISTER msg.
        /// </summary>
        /// <param name="sName">Name of the entity.</param>
        /// <param name="sDDML">DDML type.</param>
        /// <param name="entityID">Entity ID.</param>
        /// <param name="kind">Kind of entity.</param>
        /// <param name="destID">Destination component ID.</param>
        /// <returns>The message.</returns>
        //==============================================================================
        protected TMsgHeader buildRegisterMsg(string sName, string sDDML, int entityID, int kind, uint destID)
        {
            interpreter.setField(Msgs.MSG_REGISTER_KIND, kind);    //the kind of entity
            interpreter.setField(Msgs.MSG_REGISTER_ID, entityID);    //the entity ID
            interpreter.setField(Msgs.MSG_REGISTER_DESTID, destID); //the destID
            interpreter.setField(Msgs.MSG_REGISTER_NAME, sName);    //the entity name
            interpreter.setField(Msgs.MSG_REGISTER_TYPE, sDDML);    //the entity DDML
            interpreter.createMessage(Msgs.MSG_REGISTER, FParentID);//send to the parent component

            return interpreter.getMsg();
        }
        //==============================================================================
        /// <summary>
        /// Construct a GetValue message for a driving variable.
        /// </summary>
        /// <param name="driverID">ID of the driving variable.</param>
        /// <param name="msg">The created message.</param>
        /// <returns>The message ID of the new message.</returns>
        //==============================================================================
        protected uint buildGetValueMsg(int driverID, ref TMsgHeader msg)
        {
            interpreter.setField(Msgs.MSG_GETVALUE_ID, driverID);      //the driver ID code
            msg = interpreter.createMessage(Msgs.MSG_GETVALUE, FParentID);
            msg.toAck = 1;  //always

            TDriverInfo driverInfo = (TDriverInfo)driverList[driverID];
            if (!driverInfo.bRequestActive)
            {                                        //Nested requests are not allowed
                driverInfo.bRequestActive = true;
                driverInfo.iRequestMsg = msg.msgID;
                driverInfo.iConnCount = 0;
                driverList[driverID] = driverInfo;
            }
            else
            {
                string errorMsg = string.Format("{0}: Attempt to nest requests for driving property {1} in buildGetValueMsg().", FName, driverInfo.Name);
                sendError(errorMsg, true);
                //throw (new ApplicationException(errorMsg));
            }
            return msg.msgID;
        }
        //==============================================================================
        /// <summary>
        /// Build a requestSetvalue message.
        /// </summary>
        /// <param name="iLocalSetPropID">Local identifier for the property.</param>
        /// <param name="Value">Value to which to set the property.</param>
        /// <returns>The new message. If this function fails then the returned message.msgID==0</returns>
        //==============================================================================
        protected TMsgHeader buildRequestSetMsg(int iLocalSetPropID, TTypedValue Value)
        {
            uint valSize;
            TSetterProperty localProp = null;
            TMsgHeader newMsg = new TMsgHeader();
            newMsg.msgID = 0;   //default to fault

            if (iLocalSetPropID < setPropertyList.Count)
            {
                localProp = setPropertyList[iLocalSetPropID];
            }
            if (localProp != null)
            {
                valSize = Value.sizeBytes();
                byte[] valPtr = new byte[valSize];
                Value.getData(ref valPtr);

                //destination is the owning system
                interpreter.setField(Msgs.MSG_REQUESTSET_ID, iLocalSetPropID);      //local reg property ID
                interpreter.setField(Msgs.MSG_REQUESTSET_TYPE, localProp.sDDML);
                interpreter.setField(Msgs.MSG_REQUESTSET_VALUE, valPtr, valSize);
                newMsg = interpreter.createMessage(Msgs.MSG_REQUESTSET, FParentID);
            }
            else
            {
                string errorMsg = string.Format("Attempt to set an undefined property (ID: {0}) in buildRequestSetMsg()", iLocalSetPropID);
                sendError(errorMsg, true);
                //throw (new ApplicationException(errorMsg));
            }
            return newMsg;
        }
        //============================================================================
        /// <summary>
        /// Send a message through the simulation 
        /// </summary>
        /// <param name="msg"></param>
        //============================================================================
        protected void sendMessage(TMsgHeader msg)
        {
            if ( (msg.to != 0) && (msg.from == FMyID) ) {
                addToSentMsgList(msg);
            } 
            sendMessageToEngine(msg);   //send to the parent system (passed through prot.dll)
        }
        //============================================================================
        /// <summary>
        /// Add a msg to the sent msg list. It will only be added if the toAck flag is
        /// set. This list is used to complete the process of sending a message. When
        /// a message COMPLETE arrives, the component consults this
        /// list before triggering other processes.
        /// The list contains a copy of the original message header.
        /// </summary>
        /// <param name="msg">Message to add to the sent message list.</param>
        //============================================================================
        protected void addToSentMsgList(TMsgHeader msg)
        {
            if (msg.toAck > 0)
            {
                ackList.addMsgToList(msg);
            }
        }
        //==============================================================================
        /// <summary>
        /// Send a message up to the owning system so it can be routed throughout
        /// the simulation. 
        /// </summary>
        /// <param name="msg">Message that will be sent to the engine.</param>
        //==============================================================================
        protected virtual void sendMessageToEngine(TMsgHeader msg)
        {
            msgDestFunction(msg);   //send to the CMP engine (passed to prot_cs.dll)               
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
            uint prmSize;

            TEventInfo eventInfo = (TEventInfo)eventList[iEventID];
            prmSize = eventInfo.sizeBytes();
            byte[] prmPtr = new byte[prmSize];
            eventInfo.getData(ref prmPtr);             //gets a copy of the datablock

            interpreter.setField(Msgs.MSG_PUBLISHEVENT_ID, iEventID);           //the source's event ID
            interpreter.setField(Msgs.MSG_PUBLISHEVENT_TYPE, eventInfo.sDDML);  //the parameter DDML
            interpreter.setField(Msgs.MSG_PUBLISHEVENT_PARAMS, prmPtr, prmSize);
            TMsgHeader msg = interpreter.createMessage(Msgs.MSG_PUBLISHEVENT, FParentID);
            msg.toAck = Convert.ToUInt16(bAcknowledge);

            if ((iEventID == EVTERROR) && eventInfo.member("fatal").asBool())
            {
                bFatalErrorSent = true;
                iFatalErrorID = msg.msgID;
            }

            sendMessage(msg);
        }
        //==============================================================================
        /// <summary>
        /// Publishes an event to the simulation. Sets the event's data parameters
        /// firstly with paramData.
        /// </summary>
        /// <param name="iEventID">ID of the event being published</param>
        /// <param name="paramData">Parameter data to be sent with this event</param>
        /// <param name="bAcknowledge">Acknoledgement flag</param>
        //==============================================================================
        public void sendPublishEvent(int iEventID, byte[] paramData, bool bAcknowledge)
        {
            TEventInfo eventInfo = (TEventInfo)eventList[iEventID];
            if (eventInfo != null)
            {
                if (paramData != null)
                    eventInfo.setData(paramData, paramData.Length, 0);
                sendPublishEvent(iEventID, bAcknowledge);
            }
            else
                Console.WriteLine("sendPublishEvent(): Cannot publish event: " + iEventID.ToString());
        }
        //============================================================================
        /// <summary>
        /// Builds a queryInfo request and returns ownership of it to the caller
        /// </summary>
        /// <param name="sName">Name of the entity to query.</param>
        /// <param name="iKind">Entity kind.</param>
        /// <param name="destCompID">Query the entity from this component.</param>
        /// <returns>The queryInfo message header.</returns>
        //============================================================================
        protected TMsgHeader buildQueryInfo(string sName, uint iKind, uint destCompID)
        {
            interpreter.setField(Msgs.MSG_QUERYINFO_NAME, sName);
            interpreter.setField(Msgs.MSG_QUERYINFO_KIND, iKind);
            interpreter.createMessage(Msgs.MSG_QUERYINFO, destCompID);

            return interpreter.getMsg();
        }
        //============================================================================
        /// <summary>
        /// Sends an Event message to the nominated component
        /// </summary>
        /// <param name="destCompID">Destination component ID.</param>
        /// <param name="publisherID">Event published by.</param>
        /// <param name="eventID">ID of the event.</param>
        /// <param name="toAck">Acknowledge. 1=ack</param>
        /// <param name="sParamType">DDML type of the parameters.</param>
        /// <param name="param">The parameter data.</param>
        /// <param name="paramsSize">Byte size of the parameter data.</param>
        //============================================================================
        protected void sendEvent(uint destCompID, uint publisherID, uint eventID,
                               uint toAck, string sParamType, byte[] param, uint paramsSize)
        {
            sendMessage(buildSendEvent(destCompID, publisherID, eventID, toAck, sParamType, param, paramsSize));
        }
        //============================================================================
        /// <summary>
        /// Constructs and returns an event message to the caller
        /// </summary>
        /// <param name="destCompID">Destination component ID.</param>
        /// <param name="publisherID">Event published by.</param>
        /// <param name="eventID">ID of the event.</param>
        /// <param name="toAck">Acknowledge. 1=ack</param>
        /// <param name="sParamType">DDML type of the parameters.</param>
        /// <param name="param">The parameter data.</param>
        /// <param name="paramsSize">Byte size of the parameter data.</param>
        /// <returns></returns>
        //============================================================================
        protected TMsgHeader buildSendEvent(uint destCompID, uint publisherID, uint eventID,
                                                   uint toAck, string sParamType, byte [] param, uint paramsSize)
        {
            TMsgHeader msg;

            interpreter.setField(Msgs.MSG_EVENT_ID, eventID);
            interpreter.setField(Msgs.MSG_EVENT_PUBLISHEDBY, publisherID);
            interpreter.setField(Msgs.MSG_EVENT_TYPE, sParamType);
            interpreter.setField(Msgs.MSG_EVENT_PARAMS, param, paramsSize);
            msg = interpreter.createMessage(Msgs.MSG_EVENT, destCompID); //get ownership of the msg
            msg.toAck = toAck;                 //set the event acknowledge flag

            return msg;                         //return the msg for the caller to own
        }
        //============================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="destCompID">Destination of this request.</param>
        /// <param name="propID">Property ID requested.</param>
        /// <param name="reqByID">Requested by.</param>
        /// <returns>The message</returns>
        //============================================================================
        protected TMsgHeader buildQueryValue(uint destCompID, uint propID, uint reqByID)
        {
            TMsgHeader newMsg;

            interpreter.setField(Msgs.MSG_QUERYVALUE_ID, propID);
            interpreter.setField(Msgs.MSG_QUERYVALUE_REQBY, reqByID);
            newMsg = interpreter.createMessage(Msgs.MSG_QUERYVALUE, destCompID); //get ownership of the msg

            return newMsg;
        }
        //============================================================================
        /// <summary>
        /// Sends a returnValue message to the calling component
        /// </summary>
        /// <param name="destID">The destination component</param>
        /// <param name="valueFromCompID">The property value originates from this component</param>
        /// <param name="returnPropID">ID of the property in the destination component</param>
        /// <param name="DDMLType">The DDML type string</param>
        /// <param name="valueData"></param>
        /// <param name="iDataSize">Size of the data</param>
        //============================================================================
        protected void sendReturnValue(uint destID, uint valueFromCompID, uint returnPropID,
                                       string DDMLType, byte[] valueData, uint iDataSize)
        {
            interpreter.setField(Msgs.MSG_RETURNVALUE_COMPID, valueFromCompID);
            interpreter.setField(Msgs.MSG_RETURNVALUE_ID, returnPropID);
            interpreter.setField(Msgs.MSG_RETURNVALUE_TYPE, DDMLType);
            interpreter.setField(Msgs.MSG_RETURNVALUE_VALUE, valueData, iDataSize);

            TMsgHeader msg = interpreter.createMessage(Msgs.MSG_RETURNVALUE, destID);
            sendMessage(msg);
        }
        //============================================================================
        /// <summary>
        /// Send a notifySetValueSuccess message.
        /// </summary>
        /// <param name="sendTo">Destination component.</param>
        /// <param name="propID">Property that was set.</param>
        /// <param name="success">Success flag.</param>
        //============================================================================
        protected void sendNotifySetValueSuccess(uint sendTo, uint propID, bool success)
        {
            interpreter.setField(Msgs.MSG_NOTIFYSET_ID, propID);
            interpreter.setField(Msgs.MSG_NOTIFYSET_SUCCESS, success);

            interpreter.createMessage(Msgs.MSG_NOTIFYSET, sendTo);
            sendMessage(interpreter.getMsg());
        }
        //============================================================================
        /// <summary>
        /// Returns the TMsgHeader of the original msgID from the ackList
        /// If it is not in the list then TMessageHdrList.Null is returned
        /// </summary>
        /// <param name="msgID">Message ID.</param>
        /// <returns>The message or TMessageHdrList.Null.</returns>
        //============================================================================
        protected TMsgHeader querySentMsgList(uint msgID)
        {
            return ackList.queryMsg(msgID);
        }
        //============================================================================
        /// <summary>
        /// Returns the msgType of the msgID from the ackList
        /// If it is not in the list then -1 (invalid) is returned
        /// </summary>
        /// <param name="msgID">Message ID.</param>
        /// <returns>The message type. See <see cref="Msgs"/></returns>
        //============================================================================
        protected int getSentMsgType(uint msgID)
        {
            return ackList.getMsgType(msgID);
        }
        //============================================================================
        /// <summary>
        /// Returns the owner of a property/event/component
        /// e.g. own.er.name returns own.er
        ///      name        returns ""
        /// </summary>
        /// <param name="sFQN">Fully qualified name of the entity.</param>
        /// <returns>The owner of a property/event/component</returns>
        //============================================================================
        static public string qualifiedOwnerName(string sFQN)
        {
            if (sFQN.Contains("."))
            {
                int lastDot = sFQN.LastIndexOf(".");
                return sFQN.Substring(0, lastDot);
            }
            else
                return "";
        }
        //============================================================================
        /// <summary>
        /// Defines a processing state within a subscribed event.
        /// </summary>
        /// <param name="eventID">Local ID of the event.</param>
        /// <param name="iStateID">State number.</param>
        /// <param name="iStateType">Type of state; Logic or Non logic</param>
        //============================================================================
        public void defineEventState(int eventID, int iStateID, int iStateType)
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
        /// <param name="replaceAll">Replace all current transitions</param>
        //============================================================================
        public void defineEventTransition(int eventID, int iStateID, int iCondition, int iToState, bool replaceAll)
        {
            eventsManager.defineEventTransition(eventID, iStateID, iCondition, iToState, replaceAll);
        }
        //============================================================================
        /// <summary>
        /// Request the value of a driving property.
        /// Determines if the request is from an Event. If it is then we will keep
        /// track of the msgID-eventID pair for when a Complete is returned
        /// </summary>
        /// <param name="driverID">Local ID of the driver being requested.</param>
        /// <param name="eventID">Event ID.</param>
        //============================================================================
        public void sendDriverRequest(int driverID, int eventID)
        {
            try
            {
                TMsgHeader msg = new TMsgHeader();
                uint msgID = buildGetValueMsg(driverID, ref msg);
                if (eventID != 0)
                {                           //if this request is triggered from an event then
                    eventsManager.addRequest(eventID, msgID);  //keep track of the call
                }
                sendMessage(msg);
            }
            catch (Exception e)
            {
                string errorMsg = string.Format(FName + " sendDriverRequest(): {0}", e.Message);
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
        protected void sendQueryInfo(string sName, uint iKind, int eventID)
        {
            TMsgHeader msg = buildQueryInfo(sName, iKind, FParentID);

            TQueryStore queryStore = new TQueryStore();
            queryStore.iSentMsgID = msg.msgID;
            queryStore.sName      = sName;
            queryList.Add(queryStore);

            if (eventID != 0)
                eventsManager.addRequest(eventID, msg.msgID);

            sendMessage(msg);
        }
        //============================================================================
        /// <summary>
        /// Creates a new init script container. If one by this name already exists, then
        /// the existing one is removed.
        /// </summary>
        /// <param name="sScriptName">Name of the new script.</param>
        //============================================================================
        public virtual void createInitScript(string sScriptName)
        {
            if (FScriptManager == null)
                FScriptManager = new TInitScriptManager(true);
            FScriptManager.createInitScript(sScriptName);
        }
        //============================================================================
        /// <summary>
        /// Deletes the script instance specified by sScriptName.
        /// </summary>
        /// <param name="sScriptName">Name of the script.</param>
        //============================================================================
        public virtual void deleteInitScript(string sScriptName) 
        {
            if (FScriptManager != null)
                FScriptManager.deleteInitScript(sScriptName);
        }
        //============================================================================
        /// <summary>
        /// Get the text of the init script.
        /// </summary>
        /// <param name="sScriptName">Name of the script.</param>
        //============================================================================
        public virtual String textFromInitScript(string sScriptName)
        {
            String sScriptText = "";
            if (FScriptManager != null)
                FScriptManager.textFromInitScript(sScriptName, ref sScriptText);
            return sScriptText;
        }
        //============================================================================
        /// <summary>
        /// Convert an init script into the relevant properties.
        /// </summary>
        /// <param name="sScriptName">Name of the script</param>
        /// <param name="sScriptText">XML text of the init script</param>
        //============================================================================
        public virtual void textToInitScript(string sScriptName, string sScriptText)
        {
            if (FScriptManager != null)
                FScriptManager.textToInitScript(sScriptName, sScriptText);
        }
        //============================================================================
        /// <summary>
        /// Add the property. If it exists then overwrite it.
        /// </summary>
        /// <param name="sScriptName">Name of the script.</param>
        /// <param name="sPropertyName">Name of the property.</param>
        /// <param name="sTypeDDML">DDML type.</param>
        /// <param name="pValueData">Data block.</param>
        //============================================================================
        public virtual void valueToInitScript(string sScriptName, string sPropertyName, string sTypeDDML, Byte[] pValueData)
        {
            if (FScriptManager != null)
                FScriptManager.valueToInitScript(sScriptName, sPropertyName, sTypeDDML, pValueData);
        }
        //============================================================================
        /// <summary>
        /// Retrieve the property by name (case insensitive).
        /// </summary>
        /// <param name="sScriptName">Name of the script.</param>
        /// <param name="sPropertyName">Property name.</param>
        /// <param name="sTypeDDML">DDML type string.</param>
        /// <param name="pValueData">Data block.</param>
        //============================================================================
        public virtual void valueFromInitScript(string sScriptName, string sPropertyName, ref string sTypeDDML, ref Byte[] pValueData)
        {
            if (FScriptManager != null)
                FScriptManager.valueFromInitScript(sScriptName, sPropertyName, ref sTypeDDML, ref pValueData);
        }
    }
}