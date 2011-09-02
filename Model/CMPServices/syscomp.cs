using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;


namespace CMPServices
{
    //============================================================================
    /// <summary>
    /// A component reference. Used in lists.
    /// </summary>
    //============================================================================
    public struct TComp {
        /// <summary>
        /// ID component
        /// </summary>
        public uint compID;    
        /// <summary>
        /// FQ Component name
        /// </summary>
        public string name;    
        /// <summary>
        /// True if system
        /// </summary>
        public bool isSystem;
        /// <summary>
        /// Component type/class
        /// </summary>
        public String CompClass;
    }
    //============================================================================
    /// <summary>
    /// Holds information about a manually specified connection 
    /// </summary>
    //============================================================================
    internal struct TManualConnect {
        /// <summary>
        /// ID of component
        /// </summary>
        public uint compID;    
        /// <summary>
        /// kind of connection (driver or published event)
        /// </summary>
        public uint kind;      
        /// <summary>
        /// unqualified name of the connected entity
        /// </summary>
        public string name;    
        /// <summary>
        /// Connections
        /// </summary>
        public TSDMLValue connects;
    } 
    //============================================================================
    /// <summary>
    /// Object that represents all systems in the simulation system.
    /// </summary>
    //============================================================================
    public abstract class TSysComp : TBaseComp {
        private TMsgHeader notifyTerminateMsgHdr;   //temp storage of the msg details
        private TActiveTracker setActiveList;       //list of requests to set child comps to active/deactivate
        private static TComp NullChild = new TComp();

        /// <summary>
        /// True if the init1 process complete.
        /// </summary>
        protected bool init1LogicComplete;          
        /// <summary>
        /// True if the init2 process complete.
        /// </summary>
        protected bool init2LogicComplete;          
        /// <summary>
        /// Original message ID of request
        /// </summary>
        protected TMsgHeader init1MsgHdr;           
        /// <summary>
        /// Original message ID of request
        /// </summary>
        protected TMsgHeader init2MsgHdr;           
        /// <summary>
        /// store the ID of the sequencer component
        /// </summary>
        protected uint sequencerID;                 
        /// <summary>
        /// set when in the Init1 stage, unset when all init1's complete
        /// </summary>
        protected bool doingStartup;                
        /// <summary>
        /// this object knows that termination is in progress
        /// </summary>
        protected bool mustTerminate;               
        /// <summary>
        /// keep count of the init1 responses
        /// </summary>
        protected int init1AcksRemain;              
        /// <summary>
        /// keep count of the init2 responses
        /// </summary>
        protected int init2AcksRemain;              
        /// <summary>
        /// Child component list.
        /// </summary>
        protected List<TComp> compList;
        /// <summary>
        /// List of all the children as SDMLComponents
        /// </summary>
        protected List<TCompParser> sdmlChildList;  
        /// <summary>
        /// List of all manual connections
        /// </summary>
        private List<TManualConnect> manualConnectList; 

        /// <summary>
        /// Registrar that maintains and manages registration details
        /// </summary>
        protected TRegistrar registrar;             

        //============================================================================
        /// <summary>
        /// A System component base class.
        /// </summary>
        /// <param name="ID">Component ID.</param>
        /// <param name="parentID">Parent ID</param>
        /// <param name="msgCallBack">Callback to the simulation engine.</param>
        /// <param name="sType">Class name string.</param>
        /// <param name="sVersion">Version name string.</param>
        /// <param name="sAuthor">Author name.</param>
        //============================================================================
        protected TSysComp(uint ID, uint parentID, MessageFromLogic msgCallBack, string sType, string sVersion, string sAuthor)
            : base(ID, parentID, msgCallBack, sType, sVersion, sAuthor)
        {
            setActiveList = new TActiveTracker();
            sdmlChildList = new List<TCompParser>();
            manualConnectList = new List<TManualConnect>();
            registrar = new TRegistrar(ID);
            compList = new List<TComp>();
        }

        //============================================================================
        /// <summary>
        /// Overrides version in TBaseComp
        /// </summary>
        /// <param name="msg">Incoming message</param>
        //============================================================================
        public override void handleMessage(TMsgHeader msg)
        {
            uint iCompID;
            uint iPropertyID;
            uint dataSize;
            string sType;
            string sName;
            string FQN;

            uint msgID = msg.msgID;           //store some message details in local vars
            uint msgFrom = msg.from;
            uint toAck = msg.toAck;

            switch (msg.msgType)
            {
                case Msgs.MSG_ACTIVATE:
                    {
                        //8 Oct 2002 - to be completed.
                        //The deactivate logic has been implemented but not tested.
                    }
                    break;
                case Msgs.MSG_ADD:
                    {
                        interpreter.loadMessage(msg);         //take ownership of this msg
                        String SDML = interpreter.getTextField(Msgs.MSG_ADD_SDML);
                        doAddComponent(SDML);
                    }
                    break;
                case Msgs.MSG_ERROR:                //when at the destination of the simulation component it has it's own handler             
                    {
                        if (msg.to == FMyID)
                        {
                            if (FMyID != FParentID)  //If we're not our own parent
                            {
                                msg.to = FParentID;  //redirect the message up the line
                                sendMessage(msg);
                            }
                        }
                        else
                            sendMessage(msg);
                    }
                    break;
                //msg that signifies completion
                case Msgs.MSG_COMPLETE:
                    {
                        if (msg.to == FMyID)
                        {                         //if this is the destination
                            interpreter.loadMessage(msg);         //take ownership of this msg
                            uint ackID = (uint)interpreter.getIntField(Msgs.MSG_COMPLETE_ACKID);
                            //determine what has been completed
                            doComplete(msgFrom, ackID);              //using the original msg id
                        }
                        else
                            sendMessage(msg);          //send on to the destination
                    }
                    break;
                case Msgs.MSG_EVENT:
                    {
                        if (msg.to == FMyID)
                        {                         //if this is the destination (do same as base class)
                            interpreter.loadMessage(msg);    //take ownership of this msg
                            int id = interpreter.getIntField(Msgs.MSG_EVENT_ID);
                            byte[] dataPtr;
                            dataSize = interpreter.getValueField(Msgs.MSG_EVENT_PARAMS, out dataPtr);
                            uint publBy = (uint)interpreter.getIntField(Msgs.MSG_EVENT_PUBLISHEDBY);
                            String DDML = interpreter.getTextField(Msgs.MSG_EVENT_TYPE);
                            doEvent(msgFrom, publBy, id, msgID, (msg.toAck > 0), DDML, dataPtr, dataSize);
                        }
                        else
                            sendMessage(msg);          //send on to the destination
                    }
                    break;
                case Msgs.MSG_DEACTIVATE:
                    {                             //deactivate this system and children
                        interpreter.loadMessage(msg);         //take ownership of this msg
                        FQN = interpreter.getTextField(Msgs.MSG_DEACTIVATE_COMP);
                        doDeactivateComponent(msgFrom, msgID, toAck, FQN);
                    }
                    break;
                case Msgs.MSG_DELETE:
                    {
                        interpreter.loadMessage(msg);         //take ownership of this msg
                        sName = interpreter.getTextField(Msgs.MSG_DELETE_COMP);
                        doDeleteComponent(msgFrom, msgID, toAck, sName);
                    }
                    break;
                case Msgs.MSG_DEREGISTER:
                    {
                        handleDeregistration(msg);
                    }
                    break;
                //getValue. Get a driving property value
                case Msgs.MSG_GETVALUE:
                    {                               //request from a component to get a driving value
                        interpreter.loadMessage(msg);         //take ownership of this msg
                        uint propID = (uint)interpreter.getIntField(Msgs.MSG_GETVALUE_ID); //property id of requesting component
                        routeValueRequest(msgID, msgFrom, propID, toAck);
                    }
                    break;
                case Msgs.MSG_INIT1:
                    {                                  // ?? MSG_INIT1_INSTARTUP
                        interpreter.loadMessage(msg);                  //take ownership of this msg
                        init1MsgHdr.msgID = msgID;                      //store so an ack can be sent later
                        init1MsgHdr.from = msgFrom;
                        doingStartup = interpreter.getBoolField(Msgs.MSG_INIT1_INSTARTUP);

                        String SDML = interpreter.getTextField(Msgs.MSG_INIT1_SDML); //get the SDML string
                        FQN = interpreter.getTextField(Msgs.MSG_INIT1_FQN);   //get the FQN
                        doInit1(interpreter.getMsg(), SDML, FQN);
                        //it acknowledges this once all COMPLETE's have returned
                        }
                    break;
                case Msgs.MSG_INIT2:
                    {
                        init2MsgHdr.msgID = msgID;                      //store so an ack can be sent later
                        init2MsgHdr.from = msgFrom;
                        handleInit2(msg); //interpreter.getMsg()); //also cleans up msg
                        //acknowledges this once all COMPLETE's have returned
                    }
                    break;
                case Msgs.MSG_NOTIFYDELETE:
                    {
                        doNotifyDelete(msgFrom, msgID, toAck);
                    }
                    break;
                case Msgs.MSG_NOTIFYREG:
                    {      //notified that a registration has changed in another system
                        interpreter.loadMessage(msg);         //take ownership of this msg
                        //store the values from the incoming msg
                        bool registered = interpreter.getBoolField(Msgs.MSG_NOTIFYREG_REG);

                        int kind = interpreter.getIntField(Msgs.MSG_NOTIFYREG_KIND);
                        uint ownerID = (uint)interpreter.getIntField(Msgs.MSG_NOTIFYREG_OWNER);
                        uint regID = (uint)interpreter.getIntField(Msgs.MSG_NOTIFYREG_ID);

                        sName = interpreter.getTextField(Msgs.MSG_NOTIFYREG_NAME);
                        sType = interpreter.getTextField(Msgs.MSG_NOTIFYREG_TYPE);

                        doRegistrChange(registered, kind, ownerID, regID, sName, sType, msgFrom);
                    }
                    break;
                //notifySetValueSuccess
                case Msgs.MSG_NOTIFYSET:
                    {
                        if (msg.to == FMyID)
                        {                    //if this is the destination
                            interpreter.loadMessage(msg);         //take ownership of this msg
                            uint setID = (uint)interpreter.getIntField(Msgs.MSG_NOTIFYSET_ID);
                            bool success = interpreter.getBoolField(Msgs.MSG_NOTIFYSET_SUCCESS);
                            doSetSuccess(msgFrom, setID, success);  //send it to the component dll
                        }
                        else
                            sendMessage(msg);    //send on to the originator of requestSetValue
                    }
                    break;
                case Msgs.MSG_NOTIFYTERMINATION:
                    {
                        notifyTerminateMsgHdr.msgID = msgID;      //store so an ack can be sent later
                        notifyTerminateMsgHdr.from = msgFrom;
                        doNotifyTerminate(msgFrom, msgID);
                    }
                    break;
                case Msgs.MSG_PAUSE:
                    {
                        handlePause(msg);             //passes it on up to the parent. Overriden in TSimulation
                    }
                    break;
                case Msgs.MSG_PUBLISHEVENT:
                    {  //route the event to all the connected components
                        uint eventID;

                        interpreter.loadMessage(msg);         //take ownership of this msg
                        eventID = (uint)interpreter.getIntField(Msgs.MSG_PUBLISHEVENT_ID);
                        byte[] data;
                        uint paramSize = interpreter.getValueField(Msgs.MSG_PUBLISHEVENT_PARAMS, out data); //allocates mem
                        sType = interpreter.getTextField(Msgs.MSG_PUBLISHEVENT_TYPE);

                        //interpreter->deleteMessage(msg);
                        //send out events that are subscribed to this published event
                        int subscribers = doPublishEvent(msgID, msgFrom, eventID, toAck, sType, data, paramSize);

                        //if there were no subscribers to this published event then send a complete anyway
                        if (subscribers == 0)
                        {
                            //check if this was an error event and send it to stderr if it was.
			                //this will happen during init or init2 when there are no connections.
			                TDDMLValue param =  new TDDMLValue(sType, "");
                            if (param.isRecord())
                            {
                                if ((param.member(1) != null) && (param.member(1).Name == "fatal"))
                                {
                                    param.setData(data, (int)paramSize, 0); //store the data
                                    string errorMsg = param.member("message").asStr();
                                    System.Console.Error.WriteLine(errorMsg); 
                                }
                            }
                            if (toAck == 1)
			                    sendComplete(msgFrom, msgID);
                        }
                    }
                    break;
                case Msgs.MSG_QUERYINFO:
                    {
                        interpreter.loadMessage(msg);         //take ownership of this msg
                        int kind = interpreter.getIntField(Msgs.MSG_QUERYINFO_KIND);
                        // We save a local copy of the name, rather than just referencing the
                        // interpreter's pointer, since the call to doQueryInfo might result
                        // in messages being sent, which would override the interpreter's contents,
                        // and we need to keep the original name for our call to routeQueryInfo
                        String queryName = interpreter.getTextField(Msgs.MSG_QUERYINFO_NAME);
                        //check for entities of KIND that are registered here.
                        //I could do some checks on the name to see if it was possible that they could be here
                        doQueryInfo(msgFrom, msgID, kind, queryName);       //searches and returns returnInfo msg's

                        //Then forward on the message to get info from other systems and from any child systems
                        int forwardedCount = routeQueryInfo(msgFrom, msgID, kind, toAck, queryName);

                        if (forwardedCount == 0)    //if this system has no one to forward queries onto then send a complete from here
                            sendComplete(msgFrom, msgID);
                    }
                    break;
                case Msgs.MSG_QUERYSET:
                    {
                        if (msg.to == FMyID)                  //if this is the destination
                            handleQuerySetValue(msg);
                        else
                            sendMessage(msg);    //send on to destination
                    }
                    break;
                //request a driving property value
                case Msgs.MSG_QUERYVALUE:
                    {                    //request comes from another system
                        if (msg.to == FMyID)
                        {                 //if this is the destination
                            /* TODO : Trap the request for the 'state' property.
                                      I think I will have problems with a system logic dll! */
                            handleValueRequest(msg); //sends msg to the wrapper of the component
                        }
                        else
                            sendMessage(msg);    //send on to component
                    }
                    break;
                case Msgs.MSG_REGISTER:
                    {
                        //do a registration using the registrar
                        interpreter.loadMessage(msg);         //take ownership of this msg
                        int kind = interpreter.getIntField(Msgs.MSG_REGISTER_KIND);
                        uint regID = (uint)interpreter.getIntField(Msgs.MSG_REGISTER_ID);
                        uint destCompID = (uint)interpreter.getIntField(Msgs.MSG_REGISTER_DESTID);

                        sName = interpreter.getTextField(Msgs.MSG_REGISTER_NAME);
                        sType = interpreter.getTextField(Msgs.MSG_REGISTER_TYPE);

                        //do registration (and connections if not in startup)
                        doRegistration(kind, msgFrom, msgID, regID, destCompID, sName, sType, (int)msg.toAck);
                    }
                    break;
                case Msgs.MSG_REPLYSET:
                    {
                        if (msg.to == FMyID)
                        {                     //if this is the originator of the querySetValue
                            interpreter.loadMessage(msg);         //take ownership of this msg
                            uint queryMsgID = (uint)interpreter.getIntField(Msgs.MSG_REPLYSET_REQID);
                            bool success = interpreter.getBoolField(Msgs.MSG_REPLYSET_OK);
                            doReplySetValueSuccess(msgFrom, queryMsgID, success);  //do the routing back
                        }
                        else
                            sendMessage(msg);       //send on to originator
                    }
                    break;
                case Msgs.MSG_REPLYVALUE:
                    {
                        if (msg.to == FMyID)                      //if this is the originator of the queryValue
                            handleReplyValue(msg);  //route back to originator of getValue
                        else
                            sendMessage(msg);       //send on to originator of the queryValue
                    }
                    break;
                case Msgs.MSG_REQUESTCOMPID:
                    {
                        interpreter.loadMessage(msg);         //take ownership of this msg
                        FQN = interpreter.getTextField(Msgs.MSG_REQUESTCOMPID_NAME);
                        uint ownerID = (uint)interpreter.getIntField(Msgs.MSG_REQUESTCOMPID_OWNER);
                        doCompIDRequest(ownerID, FQN);
                    }
                    break;
                //requestSetValue
                case Msgs.MSG_REQUESTSET:
                    {
                        handleRequestSetValue(msg);
                    }
                    break;
                case Msgs.MSG_RESUME:
                    {
                        handleResume(msg);               //passes it on up to the parent. Overriden in TSimulation
                    }
                    break;
                case Msgs.MSG_RETURNCOMPID:
                    {
                        interpreter.loadMessage(msg);         //take ownership of this msg
                        uint compID = (uint)interpreter.getIntField(Msgs.MSG_RETURNCOMPID_ID);
                        //store this id as a one of the decendant (grand)children
                        registrar.addOwnerComp(msg.to, compID);
                        if (msg.to == FMyID)
                        {                  //if this is the destination
                            FQN = interpreter.getTextField(Msgs.MSG_RETURNCOMPID_FQN);
                            doReturnComponentID(FQN, compID);
                        }
                        else
                        {                                 //else it is for someone else
                            sendMessage(msg); //send it on
                        }
                    }
                    break;
                case Msgs.MSG_RETURNINFO:
                    {
                        if (msg.to == FMyID)                   //if this is the destination
                            handleReturnInfo(msg);
                        else                                   //else it is for someone else
                            sendMessage(msg); //send it on
                    }
                    break;
                case Msgs.MSG_RETURNVALUE:
                    {
                        if (msg.to == FMyID)
                        {            //if this is the destination
                            interpreter.loadMessage(msg);         //take ownership of this msg
                            iCompID = (uint)interpreter.getIntField(Msgs.MSG_RETURNVALUE_COMPID);
                            iPropertyID = (uint)interpreter.getIntField(Msgs.MSG_RETURNVALUE_ID);
                            String DDML = interpreter.getTextField(Msgs.MSG_RETURNVALUE_TYPE);
                            byte[] data;
                            dataSize = interpreter.getValueField(Msgs.MSG_RETURNVALUE_VALUE, out data);
                            doSetDriver(iPropertyID, iCompID, DDML, data, dataSize);     //this is the destination
                        }
                        else
                            sendMessage(msg);         //send on to the destination
                    }
                    break;
                case Msgs.MSG_TERMINATE:
                    {
                        doReqTerminate();
                        
                    }
                    break;
            }

        }
        //==============================================================================
        /// <summary>
        /// Send a message up to the owning system so it can be routed throughout
        /// the simulation. 
        /// </summary>
        /// <param name="msg">Message to send</param>
        //==============================================================================
        protected override void sendMessageToEngine(TMsgHeader msg)
        {
            msgDestFunction(msg);   //send to the CMP engine (passed to prot_cs.dll)               
        }
        /*
        //============================================================================
        /// <summary>
        /// Handles the complete msg that has been sent from the logic component back
        /// through here to the originator.
        /// </summary>
        /// <param name="msgID">The msgID of the message from the logic dll.</param>
        /// <returns>True if this message should be forwarded on to the destination.</returns>
        //============================================================================
        protected bool interpretLogicCompleteMsg(uint msgID)
        {
            bool bForward = true;               //default to continue to destination

            if (init1MsgHdr.msgID == msgID)
            {   //if this was an init1
                init1LogicComplete = true;
                if (init1AcksRemain > 0)         //if not all the init1s are finished
                    bForward = false;             //wait till all the complete's come back from children
            }
            else if (init2MsgHdr.msgID == msgID)
            {
                init2LogicComplete = true;       //this was an init2
                if (init2AcksRemain > 0)         //if not all the init2s are finished
                    bForward = false;             //wait till all the complete's come back from children
            }
            else if (notifyTerminateMsgHdr.msgID == msgID)  //this was a notifyterminate
            {    
                bForward = (compList.Count == 0);      //if we have children, wait till all the completes come back
            }
            return bForward;
        } */
        //============================================================================
        /// <summary>
        /// Requests the master system for a unique registration ID for an owned
        /// component that is about to be created
        /// </summary>
        /// <param name="ownerID">Owning component ID.</param>
        /// <param name="compName">Component name.</param>
        //============================================================================
        protected void sendCompIDRequest(uint ownerID, string compName)
        {
            interpreter.setField(Msgs.MSG_REQUESTCOMPID_OWNER, ownerID);
            interpreter.setField(Msgs.MSG_REQUESTCOMPID_NAME, compName);

            interpreter.createMessage(Msgs.MSG_REQUESTCOMPID, FParentID);
            TMsgHeader newMsg = interpreter.getMsg();
            sendMessage(newMsg);
        }
        //============================================================================
        /// <summary>
        /// Requests an owned component to carry out the first stage of initialisation
        /// </summary>
        /// <param name="compID">Component ID.</param>
        /// <param name="sFQN">Fully qualified name of the component.</param>
        /// <param name="SDML">SDML script for the component.</param>
        //============================================================================
        protected void sendInit1(uint compID, string sFQN, string SDML)
        {
            interpreter.setField(Msgs.MSG_INIT1_FQN, sFQN);
            interpreter.setField(Msgs.MSG_INIT1_SDML, SDML);
            interpreter.setField(Msgs.MSG_INIT1_INSTARTUP, true);   //in startup
            interpreter.createMessage(Msgs.MSG_INIT1, compID);
            TMsgHeader newMsg = interpreter.getMsg();
            sendMessage(newMsg);
        }
        //============================================================================
        /// <summary>
        /// Sends an init2 to each child.
        /// </summary>
        /// <param name="msgFrom">Sender ID.</param>
        //============================================================================
        protected void sendInit2ToEachChild(uint msgFrom)
        {
            List<TMsgHeader> msgList = new List<TMsgHeader>();   //message header list object
            TMsgHeader newMsg;
            TComp child;

            // We use a reverse iterator here, since sendMsgsFromList sends its messages
            // in reverse order
            for (int i = compList.Count - 1; i >= 0; i--)   //for each child
            {  
                child = compList[i];
                newMsg = buildInit2(child.compID);          //new msg
                msgList.Add(newMsg);
            }
            sendMsgsFromList(msgList);       //send all init2 messages. Component sends back a Complete msg
        }
        //============================================================================
        /// <summary>
        /// Requests an owned component to carry out the second stage of initialisation
        /// </summary>
        /// <param name="compID">Component ID</param>
        /// <returns>The Init2 message.</returns>
        //============================================================================
        protected TMsgHeader buildInit2(uint compID)
        {
            interpreter.createMessage(Msgs.MSG_INIT2, compID);
            return interpreter.getMsg();
        }
        //============================================================================
        /// <summary>
        /// Send an Init2 message.
        /// </summary>
        /// <param name="compID">Component ID.</param>
        //============================================================================
        protected void sendInit2(uint compID)
        {
            sendMessage(buildInit2(compID));
        }
        //============================================================================
        /// <summary>
        /// Sends a notifyTerminate message to a component
        /// </summary>
        /// <param name="ID">ID of the destination.</param>
        //============================================================================
        protected void sendNotifyTerminate(uint ID)
        {
            TMsgHeader newMsg;

            //create and send the msg to a child
            interpreter.createMessage(Msgs.MSG_NOTIFYTERMINATION, ID);
            newMsg = interpreter.getMsg();
            sendMessage(newMsg);
        }
        //============================================================================
        /// <summary>
        /// Routes an incoming queryInfo msg to any child systems other than the
        /// originator. Sends queryInfo also to the parent.
        /// </summary>
        /// <param name="msgFrom">Source of the message.</param>
        /// <param name="msgID">ID of the incoming message.</param>
        /// <param name="kind">Kind of entity requested.</param>
        /// <param name="toAck">Acknowledge.</param>
        /// <param name="sName">Name of the entity to find.</param>
        /// <returns>The count of outgoing queryInfo messages.</returns>
        //============================================================================
        protected int routeQueryInfo(uint msgFrom, uint msgID, int kind, uint toAck, string sName)
        {
            List<TMsgHeader> msgList = new List<TMsgHeader>();      //temporary store of outgoing msg's
            int forwardedCount = 0;             //keep a count of the forward queryInfo's
            TComp child;

            msgDirector.addMsgTrunk(msgFrom, msgID, Msgs.MSG_QUERYINFO, 0, (int)toAck);    //record the incoming msg details

            int i = 0;
            while (i < compList.Count)
            {
                child = compList[i];
                if (child.isSystem)
                {
                    //send queryInfo's to any child systems. They will not send them back here.
                    if (child.compID != msgFrom)
                    {   //ensure it doesn't go to the originator
                        uint childSysID = child.compID;
                        TMsgHeader queryInfoMsg = buildQueryInfo(sName, (uint)kind, childSysID);
                        forwardedCount++;
                        msgDirector.addMsgBranch(msgFrom, msgID, queryInfoMsg.to, queryInfoMsg.msgID);         //record this outgoing queryInfo

                        //I will make a list of copies of these msgs and then send the copies later
                        msgList.Add(queryInfoMsg);                           //stores a ptr to the msg and it's data
                    }
                }
                i++;
            }

            //send it out to the rest of the simulation
            if (canInfoBeFoundElsewhere(kind, sName))
            {   //check if the entity may be found outside this subsystem
                //now do a message for the parent
                //if this is not the top of the tree and the request is not from parent
                if ((FParentID != FMyID) && (msgFrom != FParentID))
                {
                    forwardedCount++;
                    TMsgHeader parentMsg = buildQueryInfo(sName, (uint)kind, FParentID);  //send the message to the parent
                    msgDirector.addMsgBranch(msgFrom, msgID, parentMsg.to, parentMsg.msgID);         //record this outgoing queryInfo
                    msgList.Add(parentMsg);                                    //stores a ptr to the msg and it's data
                }
            }

            //now send them all
            sendMsgsFromList(msgList);

            if (forwardedCount == 0)   //if the queryInfo was not forwarded on anywhere
                msgDirector.removeTrunk(msgFrom, msgID);    //remove any logging of it

            return forwardedCount;              //return the number of messages forwarded

        }
        //============================================================================
        /// <summary>
        /// When an event is published from a child component, it is then routed to
        /// all the components that have a subscription to that event.
        /// This is done using the Registrar because it keeps a list of all the connections
        /// that are used for a published event.
        /// The events may be sent requiring acknowledgement. If it needs an ack then
        /// the Complete msg will need to be received from all the events before a
        /// Complete can be sent back from here.
        /// </summary>
        /// <param name="inMsgID">Incoming message ID.</param>
        /// <param name="publisherID">Publisher ID.</param>
        /// <param name="eventID">Event ID.</param>
        /// <param name="toAck">Acknowledge. 1=ack</param>
        /// <param name="sParamType">DDML parameter type.</param>
        /// <param name="param">Data.</param>
        /// <param name="paramsSize">Byte size of the data.</param>
        /// <returns>Count of events published</returns>
        //============================================================================
        protected int doPublishEvent(uint inMsgID, uint publisherID, uint eventID,
                                     uint toAck, string sParamType, byte[] param, uint paramsSize)
        {

            int i;
            TEntityList connects;
            int eventsCount;
            TIDSpec anEvent;
            TMsgHeader msg;
            List<TMsgHeader> msgList = new List<TMsgHeader>();      //temporary store of outgoing msg's

            eventsCount = 0;
            connects = registrar.getPubEventConnections(publisherID, eventID);
            if ( (connects != null) && (connects.count() > 0))
            {
                if (toAck == 1)
                {                                   //if this published event needs acknowledgment then
                    msgDirector.addMsgTrunk(publisherID, inMsgID, Msgs.MSG_PUBLISHEVENT, 0, (int)toAck);    //record the incoming msg details
                }

                //for (i = 1; i <= connects.count(); i++)
                for (i = connects.count(); i > 0;  i--)
                {
                    anEvent = connects.getEntity(i);

                    // Last minute check, to resolve any named (e.g., manual) connections that
                    // haven't yet been resolved to IDs.
                    // It would be possible to always do the resolution here, rather than
                    // within resolvePublishedEvents, but I'm not making that change now
                    if (((anEvent.compID < 1) /* || (event->itemID < 1) */) &&
                        (anEvent.name.Length > 0))
                    {    //if the connection has only a name then
                        //send out queryInfo for the subscriber's name. The name would be FQN
                        msg = buildQueryInfo(anEvent.name, TypeSpec.KIND_SUBSCRIBEDEVENT, FMyID);
                        //Keep a record of the request so that the system knows what to do
                        // with the returned values in the handleReturnInfo()
                        queryInfoTracker.addQueryMsg(msg.msgID, publisherID, anEvent.name, eventID, TQueryInfoTracker.ANY_DEST_COMP, TQueryInfoTracker.UPD_PUBEVENT_CONN);

                        sendMessage(msg);                                  //this system will then handle returnInfo's
                    } //endif

                    //build the message and get ownership of it
                    msg = buildSendEvent(anEvent.compID, publisherID, anEvent.itemID, toAck, sParamType, param, paramsSize);
                    if (toAck == 1)
                    {                               //if this published event needs acknowledgment then
                        msgDirector.addMsgBranch(publisherID, inMsgID, msg.to, msg.msgID);  //stores the outgoing details
                    }
                    //if this is asynchronous then send the message

                    eventsCount++;
                    //this is synchronous so I will need to make a list of these guys and then send them all later
                    msgList.Add(msg);                  //stores a ptr to the msg and it's data
                }

                //now send them all
                sendMsgsFromList(msgList);                      //only used if more than one msg begin sent
            }
            return eventsCount;
        }
        //============================================================================
        /// <summary>
        /// In addition to beginComponentDelete, if the acknowledged message is:
        /// <ul>
        ///  <li><b>init1:</b> If this is the last init1 message sent to owned components to
        ///         be acknowledged, then the init1 message from the system that
        ///         manages this system is acknowledged. </li>
        ///  <li><b>init2:</b> If this is the last init2 message sent to owned components to
        ///         be acknowledged, then the init2 message from the system that
        ///         manages this system is acknowledged. </li>
        ///  <li><b>Terminate:</b>
        ///  <ul><li>(a) The component that sent the acknowledgement is deleted. </li>
        ///      <li>(b) If this is the last terminate message acknowledged, then
        ///              the terminate message from the system that manages this
        ///              system is acknowledged.</li>
        ///  </ul></li>
        ///  </ul>
        /// </summary>
        /// <param name="msgFrom">Source of the Complete.</param>
        /// <param name="origMsgID">The original message ID.</param>
        //============================================================================
        protected override void doComplete(uint msgFrom, uint origMsgID)
        {
            uint compID;
            TComp child;

            switch (getSentMsgType(origMsgID))
            {
                case Msgs.MSG_INIT1:
                    {
                        finaliseSentMsg(origMsgID);                           //take init1's off sent msg list
                        init1AcksRemain--;
                        if ((init1AcksRemain == 0) /*&& init1LogicComplete*/)
                        { //if all the init1s are finished
                            doingStartup = false;                              //finished the Init1 stage
                            sendComplete(init1MsgHdr.from, init1MsgHdr.msgID); //send a Complete(init1) to the parent
                        }
                    }
                    break;
                case Msgs.MSG_INIT2:
                    {
                        finaliseSentMsg(origMsgID);                           //take init2's off sent msg list
                        init2AcksRemain--;
                        if ((init2AcksRemain == 0) /*&& init2LogicComplete*/)
                        {   //no more expected
                            sendComplete(init2MsgHdr.from, init2MsgHdr.msgID); //send a complete(Init2) to the caller
                        }
                    }
                    break;

                case Msgs.MSG_EVENT:
                    {
                        finaliseSentMsg(origMsgID);                     //take sent msg off list

                        //determine if this complete is sent back to the publisher
                        doCompleteEvent(msgFrom, origMsgID);
                    }
                    break;

                case Msgs.MSG_PUBLISHEVENT:
                    {
                        finaliseSentMsg(origMsgID);                     //take sent msg off list
                        //handleMessage() has already forwarded this of to the logic dll

                        //when a publishEvent has completed, then there must be further processing to do
                    }
                    break;

                case Msgs.MSG_NOTIFYTERMINATION:
                    {
                        //here we delete the component that responded
                        TMsgHeader msg;
                        querySentMsgList(origMsgID, out msg);
                        uint destCompID = msg.to;
                        finaliseSentMsg(origMsgID);             //take sent msg of list

                        if (sdmlChildList.Count > 0)
                        {
                            //remove the sdmlComp from the child list
                            //this keeps track of how many components are left to delete
                            int i = 0;
                            while (i < compList.Count)
                            {
                                child = compList[i];
                                if (child.compID == destCompID)
                                {
                                    //remove the TSDMLComponent from the child list
                                    removeSDMLChild(TRegistrar.unQualifiedName(child.name));
                                    i = compList.Count;   //terminate loop
                                }
                                else
                                    i++;
                            }
                        }
                        //when all the messages have been acknowledged, then send a COMPLETE to the parent
                        if (sdmlChildList.Count == 0)
                        {   //no components left to delete
                            sendComplete(notifyTerminateMsgHdr.from, notifyTerminateMsgHdr.msgID);
                        }
                    }
                    break;

                case Msgs.MSG_PAUSE:
                    {
                        //When a pause ack arrives, I will check what type of message it is in response to
                        finaliseSentMsg(origMsgID);                   //take sent msg off list
                        doCompletePause(msgFrom, origMsgID);
                    } //end case pause
                    break;

                case Msgs.MSG_NOTIFYDELETE:
                    {                         //do the deregistration of the component or system
                        int regoKind = TypeSpec.KIND_COMPONENT;                //get the kind of deregistration
                        int regoID = 0;
                        TMsgHeader msg;
                        querySentMsgList(origMsgID, out msg);
                        compID = msg.to;
                        finaliseSentMsg(origMsgID);                   //take the sent notifyAboutToDelete msg off list

                        //determine which child this comes from and delete the child
                        string sName;
                        sName = TRegistrar.unQualifiedName(findCompFQN(compID));

                        removeSDMLChild(sName);

                        registrar.deregistration(regoKind, 0, compID);  //give it to the registrar

                        //send the notifyRegistrationChange to all the assoc systems
                        broadcastRegoChange(false, regoKind, compID, regoID, "", "", FMyID);

                        sendPauseSim(false); //sends resume to the parent. Arrives at the simulation->sequencer

                        //If the original message sent to this system was a notifyAboutToDelete then
                        //this system must delete all it's children and then send a complete.
                        //This is the case when this is the system being deleted.
                        //If this is the case, then a msg trunk/branch set will exist in the msgDirector.
                        if (msgDirector.isABranch(Msgs.MSG_NOTIFYDELETE, msgFrom, origMsgID))
                        {  //if this system is to be deleted
                            if (sdmlChildList.Count == 0)
                            {                          //if all the children have been deleted then
                                //send a complete(notifyAboutToDelete) to the caller
                                TTrunkMsg notifyMsg = new TTrunkMsg();
                                msgDirector.getBranch(Msgs.MSG_NOTIFYDELETE, msgFrom, origMsgID, ref notifyMsg);
                                sendComplete(notifyMsg.returnToCompID, notifyMsg.inMsgID);
                                msgDirector.removeTrunk(notifyMsg.returnToCompID, notifyMsg.inMsgID);  //cleanup
                            }
                            else
                                msgDirector.pruneBranch(Msgs.MSG_NOTIFYDELETE, msgFrom, origMsgID);
                        }
                    }
                    break;

                case Msgs.MSG_QUERYINFO:
                    {
                        doCompleteQueryInfo(msgFrom, origMsgID);
                        queryInfoTracker.finaliseMsg(origMsgID);
                        finaliseSentMsg(origMsgID);                     //take sent msg off list
                    }
                    break;

                default:
                    {
                        base.doComplete(msgFrom, origMsgID);
                    }
                    break;
            }
            return;

        }
        //============================================================================
        /// <summary>
        /// Handles the complete() that arrives after all returnInfo's have been sent
        /// back here
        /// </summary>
        /// <param name="msgFrom">Message from.</param>
        /// <param name="origMsgID">Original message ID.</param>
        //============================================================================
        protected virtual void doCompleteQueryInfo(uint msgFrom, uint origMsgID)
        {
            TTrunkMsg queryInfoMsg = new TTrunkMsg();
            bool foundBranch = msgDirector.getBranch(Msgs.MSG_QUERYINFO, msgFrom, origMsgID, ref queryInfoMsg);   //get the initiator
            if (foundBranch)    //if this was a rebroadcast msg then
            {
                uint queryInfoMsgID = queryInfoMsg.inMsgID;                 //store the initial msg id
                uint senderID = queryInfoMsg.returnToCompID;                //store the sender id
                //need to determine if I am ready to send a complete to the queryInfo sender
                uint remainingMsgs = msgDirector.pruneBranch(Msgs.MSG_QUERYINFO, msgFrom, origMsgID);
                if (remainingMsgs == 0)
                {
                    msgDirector.removeTrunk(senderID, queryInfoMsgID);     //remove the original msg from this tree
                    sendComplete(senderID, queryInfoMsgID);              //send a Complete(queryInfo)
                }
            }
            else
            {    //else we are back at the originator and do other handling of the Complete()
                //if this system component has a logic dll, and the queryInfo came from this component ID, then
                TMsgHeader msg;
                if (querySentMsgList(origMsgID, out msg) == true)
                {
                    base.doComplete(msg.from, msg.msgID);

                    //the queryInfo message may be required to pass on it's completion
                    //to another component. This is the case when a registration of a
                    //driving property or published event is done. The registration may require an
                    //acknowledgement. This can be done at this point.

                    //-firstly check to see if an ack is required to a third party-
                    if (msgDirector.isABranch(Msgs.MSG_REGISTER, FMyID, origMsgID))
                    {     //if ack required
                        TTrunkMsg regMsg = new TTrunkMsg();
                        msgDirector.getBranch(Msgs.MSG_REGISTER, FMyID, origMsgID, ref regMsg);   //get the src of the query msg
                        uint branchesRemaining = msgDirector.pruneBranch(Msgs.MSG_REGISTER, FMyID, origMsgID);
                        if (branchesRemaining < 1)
                        {     //if the notifyRegchange has completed (other virtual branch)
                            msgDirector.removeTrunk(regMsg.returnToCompID, regMsg.inMsgID);
                            sendComplete(regMsg.returnToCompID, regMsg.inMsgID);       //send a complete back to the comp requesting the registration
                        }
                    }
                }
            }
        }
        //============================================================================
        /// <summary>
        /// When a pause ack arrives, I will check what type of message it is in
        /// response to.
        /// </summary>
        /// <param name="msgFrom">Message from.</param>
        /// <param name="origMsgID">Original message ID.</param>
        //============================================================================
        protected virtual void doCompletePause(uint msgFrom, uint origMsgID)
        {
            //Simulation paused because of a deleteComponent?
            //if the pause was triggered by a MSG_DELETE
            if (msgDirector.isABranch(Msgs.MSG_DELETE, msgFrom, origMsgID))
            {
                TTrunkMsg delMsg = new TTrunkMsg();
                msgDirector.getBranch(Msgs.MSG_DELETE, msgFrom, origMsgID, ref delMsg);
                //now send a notifyAboutToDelete to the component
                //returnToCompID is used to direct it to the component to delete
                interpreter.createMessage(Msgs.MSG_NOTIFYDELETE, delMsg.returnToCompID);
                msgDirector.removeTrunk(delMsg.returnToCompID, delMsg.inMsgID);  //cleanup

                TMsgHeader notifyDeleteMsg = interpreter.getMsg();
                sendMessage(notifyDeleteMsg);                    //send notifyAboutToDelete
            }
            else
            {
                //Has the simulation paused because of a deactivateComponent?
                //This message would be coming from the sequencer
                //Check for details about the source of the pause
                if (msgDirector.isABranch(Msgs.MSG_DEACTIVATE, msgFrom, origMsgID))
                {
                    TTrunkMsg deactivateMsg = new TTrunkMsg();
                    msgDirector.getBranch(Msgs.MSG_DEACTIVATE, msgFrom, origMsgID, ref deactivateMsg);
                    uint childID = deactivateMsg.returnToCompID;      //get the component id that is to be deactivated
                    //I will assume that it is always a child component/system that is being deactivated
                    uint propertyRegID = registrar.getLocalPropertyID("active", childID);   //get the property ID of ACTIVE
                    msgDirector.removeTrunk(childID, deactivateMsg.inMsgID);  //cleanup

                    if (propertyRegID > 0)
                    {                               //if the 'active' property is in the registry
                        //Send a querySetValue(active, false) to it
                        TDDMLValue typedValue = new TDDMLValue("<type kind=\"boolean\"/>", "");
                        typedValue.setValue(false);             //set to false
                        //get the data
                        byte[] data = new byte[typedValue.sizeBytes()];
                        typedValue.getData(ref data);

                        //construct the message
                        TMsgHeader newMsg = buildQuerySetValue(childID, propertyRegID, "<type kind=\"boolean\"/>", data, typedValue.sizeBytes());

       // //To Do:      setActiveList.setActive(newMsg.msgID, childID, INACTIVE, true);  //deactivate and then resume

                        sendMessage(newMsg);
                        //The component/system will then respond with a replySetValueSuccess
                    }
                }
            }
        }
        //============================================================================
        /// <summary>
        /// Passes this pause message on up to the simulation.
        /// </summary>
        /// <param name="msg">Pause message.</param>
        //============================================================================
        protected virtual void handlePause(TMsgHeader msg)
        {
            msg.to = FParentID;
            sendMessage(msg);
        }
        //============================================================================
        /// <summary>
        /// Handle a Resume message.
        /// </summary>
        /// <param name="msg">Message</param>
        //============================================================================
        protected virtual void handleResume(TMsgHeader msg)
        {
            msg.to = FParentID;
            sendMessage(msg);
        }
        //============================================================================
        /// <summary>
        /// An addComponent message causes this system to issue a requestComponentID.
        /// </summary>
        /// <param name="SDML">SDML script for the component.</param>
        //============================================================================
        protected void doAddComponent(string SDML)
        {
            string sName;
            TCompParser sdmlChild = new TCompParser(SDML);
            sName = sdmlChild.InstanceName;   //get the name of the component

            sdmlChildList.Add(sdmlChild);     //add the child to the list of sdml children
            //This is used when the returnComponentID arrives

            sendCompIDRequest(FMyID, partiallyQualifyName(sName));       //send a requestComponentID onto the simulation system
        }
        //============================================================================
        /// <summary>
        /// Finds the ID of the component to delete and then sends a pause to the
        /// sequencer via the simulation.
        /// </summary>
        /// <param name="msgID">Message ID.</param>
        /// <param name="msgFrom">Message from.</param>
        /// <param name="toAck">Acknowledge. 1=Ack</param>
        /// <param name="sCompToDelete">Component name to delete.</param>
        //============================================================================
        protected void doDeleteComponent(uint msgID, uint msgFrom, uint toAck, string sCompToDelete)
        {
            TMsgHeader newMsg;
            uint compIDToDelete;

            //find the component ID that needs to be deleted by looking through the children
            compIDToDelete = findCompID(sCompToDelete, true);

            if ((compIDToDelete > 0) && (compIDToDelete != UInt32.MaxValue))
            {    //if the component is found in the child list
                //request the simulation to be paused
                interpreter.createMessage(Msgs.MSG_PAUSE, 1);    //send to the sequencer (sim) ****need to refine this
                newMsg = interpreter.getMsg();
                newMsg.toAck = 1;                           //acknowledge this pause message

                //keep a record of this routing so that when the COMPLETE arrives,
                //a notifyAboutToDelete can be sent to the component.

                //store the ID of the component to delete as the (alias) originator of the delete msg (for routing)
                msgFrom = compIDToDelete;
                msgDirector.addMsgTrunk(msgFrom, msgID, Msgs.MSG_DELETE, 0, (int)toAck);       //store incoming details
                msgDirector.addMsgBranch(msgFrom, msgID, newMsg.to, newMsg.msgID);   //store the pause msg details

                sendMessage(newMsg);
            }
        }
        //============================================================================
        /// <summary>
        /// Check any registrations in this system that are of the KIND and match
        /// the name. When there is a match, send a returnInfo message to the source
        /// of the query.
        /// The name may be qualified (see section 8.1). A property or event name may
        /// be "*", which matches all names ("component.*" is permitted)
        /// When the search is complete, then a COMPLETE msg is also sent if ack is
        /// </summary>
        /// <param name="srcCompID">Source of the QueryInfo.</param>
        /// <param name="queryMsgID">QueryInfo msg ID.</param>
        /// <param name="kind">Kind of entity.</param>
        /// <param name="sFQN">Name of the entity being searched for. May be fully qualified.</param>
        //============================================================================
        protected void doQueryInfo(uint srcCompID, uint queryMsgID, int kind, string sFQN)
        {
            string searchName;
            string ownerName;
            uint entityOwnerID;               //compID that owns the entity being searched for
            TEntityList infoList = null;
            TIDSpec entity;

            //access the registrar to determine the match to event/property names

            searchName = TRegistrar.unQualifiedName(sFQN);
            ownerName = qualifiedOwnerName(sFQN);

            // Component and system names are handled outside of the registrar
            if (kind == TypeSpec.KIND_COMPONENT || kind == TypeSpec.KIND_SYSTEM)
            {
                //check if no owner OR I am the owner OR wildcard as owner
                System.Globalization.CultureInfo ci = new System.Globalization.CultureInfo("en-AU");
                if ((ownerName.Length == 0) || (ownerName.ToLower(ci) == FName.ToLower(ci)) || (ownerName == "*"))
                {
                    infoList = new TEntityList();
                    //match searchName against Unqualified Names of the children
                    for (int i = 0; i < compList.Count; i++)
                    {
                        TComp child = compList[i];
                        if ( ((searchName == "*") || (searchName.ToLower(ci) == TRegistrar.unQualifiedName(child.name).ToLower(ci))) //if '*' then match any child
                              && ((kind == TypeSpec.KIND_COMPONENT) || child.isSystem) ) 
                        {  // If kind is KIND_SYSTEM, match only systems
                            uint regID = findCompID(child.name, true);
                            if (regID == UInt32.MaxValue)
                                regID = 0;
                            infoList.add(FMyID, regID, child.name, child.isSystem ? TypeSpec.KIND_SYSTEM : TypeSpec.KIND_COMPONENT, " ", true); //add FQN and other details to the list
                        }
                    }
                }
            }
            else
            {
                //get the ID of the ownerName (it is a child of this system)
                entityOwnerID = findCompID(ownerName, true);    //if ownerName == "" -> entityOwnerID = 0

                if (entityOwnerID != UInt32.MaxValue)
                { //if the entity has some hope of being found here
                    //do a search for all entities of KIND for this compID
                    infoList = new TEntityList();
                    //search the registry. Handles wildcard entity names.
                    registrar.findEntities(entityOwnerID, ownerName, searchName, kind, ref infoList, "");
                }
            }

            if (infoList != null)
            {
                //then send a returnInfo for every match with the FQN from entity->name
                for (int i = 1; i <= infoList.count(); i++)
                {  //1-x indexed
                    entity = infoList.getEntity(i);
                    TMsgHeader newMsg = buildReturnInfo(queryMsgID, entity.compID, entity.itemID, entity.name, entity.sType, entity.kind, srcCompID);
                    sendMessage(newMsg);
                }
            }
        }
        //============================================================================
        /// <summary>
        /// Handles the request to deactivate a child component, given it's
        /// unqualified name.
        /// </summary>
        /// <param name="msgID">Message ID.</param>
        /// <param name="msgFrom">Message from.</param>
        /// <param name="toAck">Acknowledge.</param>
        /// <param name="sFQN">FQN of the component.</param>
        //============================================================================
        protected void doDeactivateComponent(uint msgID, uint msgFrom, uint toAck, string sFQN)
        {
            TMsgHeader newMsg;
            uint compIDToDeactivate;

            System.Globalization.CultureInfo ci = new System.Globalization.CultureInfo("en-AU");
            if (FName.ToLower(ci) == sFQN.ToLower(ci))
            {   //if this is the component to deactivate
                compIDToDeactivate = FMyID;          //store the component ID
            }
            else
            {
                //find the component ID that needs to be deactivated by looking through the children
                compIDToDeactivate = findCompID(sFQN, true);
            }

            if ((compIDToDeactivate > 0) && (compIDToDeactivate != UInt32.MaxValue))
            {         //if the component is found
                //request the simulation to be paused
                interpreter.createMessage(Msgs.MSG_PAUSE, 1);    //send to the sequencer (sim)
                newMsg = interpreter.getMsg();
                newMsg.toAck = 1;                           //acknowledge this pause message

                //keep a record of this routing so that when the COMPLETE arrives,
                //a querySetValue can be sent to the component.

                //use the deactivate msg as the trunk message, but use the deactivate compID so I can
                //retrieve it when handling the COMPLETE
                msgDirector.addMsgTrunk(compIDToDeactivate, msgID, Msgs.MSG_DEACTIVATE, 0, (int)toAck);        //store incoming details
                //the pause msg is the branch
                msgDirector.addMsgBranch(compIDToDeactivate, msgID, newMsg.to, newMsg.msgID);      //store the pause msg details

                sendMessage(newMsg);    //send the pause
            }
        }
        //============================================================================
        /// <summary>
        /// If this System component is told to delete itself, it needs to cascade the
        /// notifyAboutToDeletes to it's children so that they are deregistered also.
        ///
        /// Store the details of the incoming request so that a COMPLETE can be sent
        /// later.
        /// </summary>
        /// <param name="msgFrom">Message from.</param>
        /// <param name="msgID">Message ID.</param>
        /// <param name="toAck">Acknowledge. 1=Ack</param>
        //============================================================================
        protected virtual void doNotifyDelete(uint msgFrom, uint msgID, uint toAck)
        {
            TComp child;
            TMsgHeader msgCopy;
            List<TMsgHeader> msgList = new List<TMsgHeader>();

            //store details of the notifyAboutToDelete msg because this system
            //will need to return a COMPLETE once all the children have sent their
            //complete(notifyAboutToDelete)'s back here
            msgDirector.addMsgTrunk(msgFrom, msgID, Msgs.MSG_NOTIFYDELETE, 0, (int)toAck);

            int i = 0;
            while (i < compList.Count)
            {
                child = compList[i];
                interpreter.createMessage(Msgs.MSG_NOTIFYDELETE, child.compID);
                msgCopy = interpreter.getMsg();           //get a copy

                msgDirector.addMsgBranch(msgFrom, msgID, msgCopy.to, msgCopy.msgID);  //stores the outgoing details
                msgList.Add(msgCopy);            //stores a ptr to the msg and it's data
                i++;
            }
            //now send them all
            sendMsgsFromList(msgList);
        }
        //============================================================================
        /// <summary>
        /// Qualifies the component name and passes it up the tree of systems
        /// </summary>
        /// <param name="iOwnerID">Owner ID.</param>
        /// <param name="sCompName">Component name.</param>
        //============================================================================
        protected virtual void doCompIDRequest(uint iOwnerID, string sCompName)
        {
            sendCompIDRequest(iOwnerID, partiallyQualifyName(sCompName));  //send on up the tree
        }
        //============================================================================
        /// <summary>
        /// Sends a requestTerminate
        /// </summary>
        //============================================================================
        protected virtual void doReqTerminate()
        {
            sendEndSimulation(); //send msg directly to parent (finally the simulation)
        }
        //============================================================================
        /// <summary>
        /// Send notifyTerminate to all children
        /// </summary>
        /// <param name="msgFrom">Message from.</param>
        /// <param name="msgID">The message ID.</param>
        //============================================================================
        protected void doNotifyTerminate(uint msgFrom, uint msgID)
        {
            
            TComp child;
            mustTerminate = true;   //this object knows that termination is in progress

            //send on for component handling before termination
            doAtTermination(msgFrom, msgID);

            int i = 0;
            while (i < compList.Count)
            {
                child = compList[i];
                sendNotifyTerminate(child.compID);
                i++;
            }
        }
        //============================================================================
        /// <summary>
        /// In response to a GET_VALUE msg.
        /// Sends MSG_QUERYVALUE messages to each source for the value of a
        /// component's driving property. The set of sources is looked up from the
        /// registrar.
        /// The connections retrieved from the registrar will have their matchesRule
        /// attribute set by the rule specified in the registrar. (e.g. nearest)
        /// </summary>
        /// <param name="msgID">Incoming GetValue msg ID.</param>
        /// <param name="drvCompID">Component ID of the calling component.</param>
        /// <param name="propID">Driving variable property ID.</param>
        /// <param name="toAck">Do an acknowledge.</param>
        //============================================================================
        protected void routeValueRequest(uint msgID, uint drvCompID, uint propID, uint toAck)
        {
            TEntityList connectList;
            TIDSpec destProp;
            uint reqFrom;            //request the property value from this component
            uint srcProperty;
            List<TMsgHeader> msgList = new List<TMsgHeader>();              //temporary store of outgoing msg's
            TMsgHeader queryValueMsg;

            connectList = registrar.getDriverConnections(drvCompID, propID);
            if ( (connectList != null) && (connectList.count() > 0))
            {                                  //if there are some connections
                //Now store the number of responses expected for this getValue request
                //This information is used when returnValue msg's arrive.
                msgDirector.addMsgTrunk(drvCompID, msgID, Msgs.MSG_GETVALUE, (int)propID, (int)toAck);    //record the incoming msg details

                for (int i = 1; i <= connectList.count(); i++)
                {
                    destProp = connectList.getEntity(i);

                    // Last minute check, to resolve any named (e.g., manual) connections that
                    // haven't yet been resolved to IDs.
                    // It would be possible to always do the resolution here, rather than
                    // within resolveDrivingProperties, but I'm not making that change now
                    if (((destProp.compID < 1) /* || (destProp->itemID < 1) */ ) &&
                           (destProp.name.Length > 0))
                    {    //if the connection has only a name then
                        //send out queryInfo for the property name. The name would be FQN
                        TMsgHeader msg = buildQueryInfo(destProp.name, TypeSpec.KIND_OWNED_R, FMyID);
                        //Keep a record of the request so that the system knows what to do
                        // with the returned values in the handleReturnInfo()
                        queryInfoTracker.addQueryMsg(msg.msgID, drvCompID, destProp.name, propID, TQueryInfoTracker.ANY_DEST_COMP, TQueryInfoTracker.UPD_DRVPROPERTY_CONN);

                        sendMessage(msg);                                  //this system will then handle returnInfo's
                    }  //endif

                    if (destProp.matchesRule)
                    {    //if this connection is to be used
                        reqFrom = destProp.compID;    //send a queryValue message
                        srcProperty = destProp.itemID;

                        //directed to reqFrom component
                        queryValueMsg = buildQueryValue(reqFrom, srcProperty, drvCompID);

                        msgDirector.addMsgBranch(drvCompID, msgID, reqFrom, queryValueMsg.msgID);  //record this outgoing query Info

                        //I will make a list of copies of these msgs and then send the copies later
                        msgList.Add(queryValueMsg);              //stores a ptr to the msg and it's data
                    }
                }
                if (msgList.Count > 0)
                {
                    sendMsgsFromList(msgList);                            //now send them all
                }
                else
                {   //else there are no values to be requested because no useful connections found
                    msgDirector.removeTrunk(drvCompID, msgID);
                    sendComplete(drvCompID, msgID);
                }
            }
            else
            {   //else there are no values to be requested
                sendComplete(drvCompID, msgID);
            }
        }
        //============================================================================
        /// <summary>
        /// When a replyValue message arrives to this object that sent a queryValue,
        /// send the msg to the component that sent
        /// getValue and then see if it is time to send a COMPLETE to the component.
        /// </summary>
        /// <param name="msg">Message.</param>
        //============================================================================
        protected virtual void handleReplyValue(TMsgHeader msg)
        {
            int count;
            uint queryMsgID;
            uint msgFrom = msg.from;

            //load this message and extract the fields
            interpreter.loadMessage(msg);
            queryMsgID = (uint)interpreter.getIntField(Msgs.MSG_REPLYVALUE_QUERYID);

            TTrunkMsg queryValueMsg = new TTrunkMsg();
            bool foundBranch = msgDirector.getBranch(Msgs.MSG_GETVALUE, msgFrom, queryMsgID, ref queryValueMsg);
            if (foundBranch)    //if this msg was sent from here
            {   
                uint getValueMsgID = queryValueMsg.inMsgID;                  //store the initial msg ID
                uint requesterID = queryValueMsg.returnToCompID;             //store the sender ID
                uint propID = (uint)queryValueMsg.propID;                          //ID of driving property

                count = (int)msgDirector.pruneBranch(Msgs.MSG_GETVALUE, msgFrom, queryMsgID);
                if (count == 0)
                    msgDirector.removeTrunk(requesterID, getValueMsgID);

                string sDDML = interpreter.getTextField(Msgs.MSG_REPLYVALUE_TYPE);
                uint valueSize;
                byte[] data;
                valueSize = interpreter.getValueField(Msgs.MSG_REPLYVALUE_VALUE, out data); //allocates mem

                //send on to the component that requested it
                uint valueFromCompID = msg.from;
                sendReturnValue(requesterID, valueFromCompID, propID, sDDML, data, valueSize);

                //when the number of returns = the number of queryValue messages sent (for the incoming getValue msgID)
                if (count == 0)
                {
                    sendComplete(requesterID, getValueMsgID);
                }
            }
            else
            {      // ACK! We don't know about this message.
                sendError("Error in routing of replyValue message", true); //send fatal error message
            }
        }
        //============================================================================
        /// <summary>
        /// Handles the request to set an owned property for a component.
        /// The registar is queried to find the connection for the property.
        /// If there is no connection then a failure is returned.
        /// </summary>
        /// <param name="msg">Message.</param>
        //============================================================================
        protected void handleRequestSetValue(TMsgHeader msg)
        {
            TEntityList connectList;
            TIDSpec destProp;
            int propID;
            uint requesterID;
            uint targetID = 0;
            uint targetProp = 0;
            TMsgHeader newMsg;
            string sType;

            uint dataSize;
            uint inMsgID;
            uint nMatched;

            interpreter.loadMessage(msg);
            //get the property id that has previously been registered
            propID = interpreter.getIntField(Msgs.MSG_REQUESTSET_ID);

            //identify the component that owns the property to be set
            requesterID = msg.from;      //source of the requestSet
            inMsgID = msg.msgID;
            //   prop = registrar->setterPropertyByID(requesterID, propID);

            nMatched = 0;
            connectList = registrar.getReqSetConnections(requesterID, (uint)propID); //return the list of connections
            if (connectList != null)
            {
                for (int i = 1; i <= connectList.count(); i++)
                {
                    destProp = connectList.getEntity(i);
                    if (destProp.matchesRule)
                    {
                        ++nMatched;
                        targetID = destProp.compID;
                        targetProp = destProp.itemID;
                    }
                }
            }
            if (nMatched == 0)
            {         //if the property does not have a connection then
                interpreter.setField(Msgs.MSG_NOTIFYSET_ID, propID);      //send a failure
                interpreter.setField(Msgs.MSG_NOTIFYSET_SUCCESS, false);
                interpreter.createMessage(Msgs.MSG_NOTIFYSET, requesterID);
                newMsg = interpreter.getMsg();
            }
            else if (nMatched > 1)
            {
                sendError("Request set value had multiple destinations", true); //send fatal error message
                return;
            }
            else
            {   //send a querySetValue message to the component
                //get the type
                sType = interpreter.getTextField(Msgs.MSG_REQUESTSET_TYPE);
                //get the data
                byte[] dataPtr;
                dataSize = interpreter.getValueField(Msgs.MSG_REQUESTSET_VALUE, out dataPtr);

                //create the querySetValue msg
                newMsg = buildQuerySetValue(targetID, targetProp, sType, dataPtr, dataSize);

                //Need to track this message being routed so the notifySetValueSuccess gets routed back to the originator
                msgDirector.addMsgTrunk(requesterID, inMsgID, Msgs.MSG_REQUESTSET, propID, 0);    //record the incoming msg details
                msgDirector.addMsgBranch(requesterID, inMsgID, targetID, newMsg.msgID);
            }
            sendMessage(newMsg);
        }
        //============================================================================
        /// <summary>
        /// Handles the request to set an owned property for this system
        ///
        /// This is a bit tricky because if this was a set active/deactive then I need
        /// to do something other than passing the value on to the logic dll. I will
        /// need to send this on to any children.
        /// </summary>
        /// <param name="msg">Message.</param>
        //============================================================================
        protected override void handleQuerySetValue(TMsgHeader msg)
        {

            TComp child;
            TMsgHeader newMsg;
            uint msgFrom;
            uint msgID;
            uint msgType;

            //Test if this is setting the 'active' property.

            int propID = propertyNameToID("active");   //get the property ID from the list

            msgFrom = msg.from;
            msgID = msg.msgID;
            msgType = msg.msgType;
            interpreter.loadMessage(msg);

            //get the ID of the property to be set -from the msg
            int setID = interpreter.getIntField(Msgs.MSG_QUERYSET_ID);
            if (propID == setID)
            {  //if the ID == propID then
                //now branch all these requests off to the children
                List<TMsgHeader> msgList = new List<TMsgHeader>();  //temporary store of outgoing msg's
                uint valueSize;
                byte[] data;
                valueSize = interpreter.getValueField(Msgs.MSG_QUERYSET_VALUE, out data); //allocates mem
                string sDDML = interpreter.getTextField(Msgs.MSG_QUERYSET_TYPE);

                //retrieve the boolean value that is being transmitted in the msg
                TDDMLValue active = new TDDMLValue(sDDML, "");
                active.setData(data, (int)valueSize, 0); //store the data -> ptr
                uint setTo = TActiveTracker.ACTIVE;
                if (active.asBool() == false)
                    setTo = TActiveTracker.INACTIVE;

                //record the original request to set the 'active' property
                msgDirector.addMsgTrunk(msgFrom, msgID, (int)msgType, propID, 0);

                //get the first child
                int i = 0;
                for (i = 0; i < compList.Count; i++)
                {  //for each child
                    child = compList[i];
                    uint childPropID = registrar.getLocalPropertyID("active", child.compID);   //child's active property
                    //construct a msg to the child
                    newMsg = buildQuerySetValue(child.compID, childPropID, sDDML, data, valueSize);

                    //record the request in the setActive object
                    setActiveList.setActive(newMsg.msgID, newMsg.to, setTo, false);

                    msgDirector.addMsgBranch(msgFrom, msgID, newMsg.to, newMsg.msgID);

                    //add it to the msg list
                    //this is synchronous so I will need to make a list of these guys and then send them all later
                    msgList.Add(newMsg);                      //stores a ptr to the msg and it's data
                }

                //send all the activate/deactivate querySetValue's to the children
                sendMsgsFromList(msgList);

                //Attempt to set this system's active property. Send the message directly to the logic dll
                newMsg = buildQuerySetValue(FMyID, (uint)propertyNameToID("active"), sDDML, data, valueSize);

                msgDirector.addMsgBranch(msgFrom, msgID, newMsg.to, newMsg.msgID);
                //record the request in the setActive object
                setActiveList.setActive(newMsg.msgID, newMsg.to, setTo, false);
                //      msgToLogic(newMsg);
                //when the logic dll replies with a replySetValueSuccess I will trap it
            }
            else
            {      //setting some other property so send the msg to the logic dll
                base.handleQuerySetValue(msg);
            }
        }
        //============================================================================
        /// <summary>
        /// When a replySetValueSuccess msg comes back to this originator of the
        /// querySetValue, I need to route back the success info the the original
        /// component.
        /// 
        /// When activating/deactivating a component a querySetValue was sent.
        /// 
        /// If the deactivate was applied to more than one component, (children) then
        /// this system will need to send on back a replySetValueSuccess to the
        /// initiator once all the responses are back from the children. Then this
        /// system needs to attempt to set it's own active property. If one of them
        /// fails, I will need to set a fatal error.
        /// </summary>
        /// <param name="msgFrom">Message from.</param>
        /// <param name="queryMsgID">Message ID of the query.</param>
        /// <param name="success">Success</param>
        //============================================================================
        protected void doReplySetValueSuccess(uint msgFrom, uint queryMsgID, bool success)
        {
            if (msgDirector.isABranch(Msgs.MSG_REQUESTSET, msgFrom, queryMsgID))
            {        //if a routed querySet msg
                TTrunkMsg requestMsg = new TTrunkMsg();
                msgDirector.getBranch(Msgs.MSG_REQUESTSET, msgFrom, queryMsgID, ref requestMsg);
                uint returnTo = requestMsg.returnToCompID;
                uint reqMsgID = requestMsg.inMsgID;   //requesting msg ID
                uint reqPropID = (uint)requestMsg.propID;   //the setterID


                //determine if it was a set 'active' to a child of this system
                /*      unsigned propID = propertyNameToID("active");   //get the property ID from the list
                      if (reqPropID == propID) {                      //if I was setting this system's 'active' property
                         if (success) {
                            int setTo = setActiveList->getLastSetActive(queryMsgID, msgFrom);
                            if (setTo > -1) {                            //if this was a set 'active'
                               setActiveList->removeSetActive(queryMsgID, msgFrom);
                            }
                            if (setTo == INACTIVE) {   //if deactivate
                               registrar->deregistration(KIND_COMPONENT, 0, msgFrom);      //Deregister the component
                               //send the notifyRegistrationChange to all the assoc systems
                               broadcastRegoChange(false, KIND_COMPONENT, msgFrom, 0, "", "", FMyID);
                            }
                            else {
                */
                /* TODO : examine what if activate? */
                /*            }

                            int repliesRemaining = msgDirector->pruneBranch(MSG_REQUESTSET, msgFrom, queryMsgID);  //cleanup
                            if (repliesRemaining == 0) {     //all the querySetValue msgs have succeeded
                               msgDirector->removeTrunk(returnTo, reqMsgID);
                               sendReplySetValueSuccess(returnTo, reqMsgID, true);
                            }
                         }
                         else {   //failed to set 'active' property
                            std::string name;
                            findCompFQN(msgFrom, name);
                            std::string buf = "Cannot set the 'active' property of the component " + name;
                            sendError(buf.c_str(), True); //send fatal error message
                         }
                      } */
                //      else {   //this was any other normal property (not 'active')
                msgDirector.removeTrunk(returnTo, reqMsgID);  //cleanup

                //send off the notifySetValueSuccess
                sendNotifySetValueSuccess(returnTo, reqPropID, success);
                //      }
            }
            else
            { //not a routed querySet, so I will test if this was a de/activate comp querySet to a child
                if (success)
                {
                    int setTo = setActiveList.getLastSetActive(queryMsgID, msgFrom);
                    if (setTo > -1)
                    {                            //if this was a set 'active'
                        setActiveList.removeSetActive(queryMsgID, msgFrom);
                    }
                    if (setTo == TActiveTracker.INACTIVE)
                    {   //if deactivate
                        registrar.deregistration(TypeSpec.KIND_COMPONENT, 0, msgFrom);      //Deregister the component
                        //send the notifyRegistrationChange to all the assoc systems
                        broadcastRegoChange(false, TypeSpec.KIND_COMPONENT, msgFrom, 0, "", "", FMyID);

                        sendPauseSim(false); //send a resume
                    }
                    else
                    {
                        /* TODO : examine what if activate? */
                    }
                }
                else
                {
                    string name;
                    name = findCompFQN(msgFrom);
                    string buf = "Cannot set the 'active' property of the component" + name;
                    sendError(buf, true); //send fatal error message
                }
            }
        }
        //============================================================================
        /// <summary>
        /// Handles the processing of a complete() sent in response to an event msg.
        ///  Looks through the msgDirector's list of event msgs's that were sent out
        /// in response to a publishevent. If all the event messages have sent back
        /// a complete(), then a complete is sent on to the original publishevent
        /// sender.
        /// </summary>
        /// <param name="msgFrom">Message from.</param>
        /// <param name="origMsgID">Original message ID.</param>
        //============================================================================
        protected void doCompleteEvent(uint msgFrom, uint origMsgID)
        {
            TTrunkMsg publishMsg = new TTrunkMsg();
            bool foundBranch = msgDirector.getBranch(Msgs.MSG_PUBLISHEVENT, msgFrom, origMsgID, ref publishMsg);   //get the initiator
            if (foundBranch)    //if this was a rebroadcast publishEvent then
            {   
                uint publishMsgID = publishMsg.inMsgID;                   //store the initial msg id
                int toAck = publishMsg.toAck;
                uint senderID = publishMsg.returnToCompID;                //store the sender id
                //need to determine if I am ready to send a complete to the publishEvent sender
                uint remainingMsgs = msgDirector.pruneBranch(Msgs.MSG_PUBLISHEVENT, msgFrom, origMsgID);
                if (remainingMsgs == 0)
                {
                    msgDirector.removeTrunk(senderID, publishMsgID);     //remove the original msg from this tree
                    if (toAck == 1)                                       //if it requested a complete() then
                        sendComplete(senderID, publishMsgID);           //send a Complete(publishevent)
                }
            }
            else
            { //else
                //do other handling of the Complete(event)
            }
        }
        //============================================================================
        /// <summary>
        /// Handles the registration of properties and events. If the registration is
        /// done after startup, then a notification of the registration change is
        /// broadcast.
        /// If this registration needs to be acknowledged then I will need to record
        /// that so if there are queryInfo's done I need to wait for their returns.
        /// </summary>
        /// <param name="kind">Kind of entity to register.</param>
        /// <param name="ownerID">Component owning the property being registered.</param>
        /// <param name="msgID">Registration msg ID.</param>
        /// <param name="regID">Property/event ID being registered.</param>
        /// <param name="destCompID">Source to connect to.</param>
        /// <param name="sName">Name of property/event.</param>
        /// <param name="sType">DDML type description.</param>
        /// <param name="toAck">Acknowledge the registration message 0/1.</param>
        //============================================================================
        protected void doRegistration(int kind, uint ownerID, uint msgID, uint regID,
                              uint destCompID, string sName, string sType, int toAck)
        {
            TMsgHeader queryMsg;                                        //queryInfo msg that may be sent for connections
            bool queryInfoRequired = false;                             //
            uint queryCompID = 0, queryMsgID = 0;                       //local copies for use after a msg is sent
            uint connectToEntityType = TypeSpec.KIND_OWNED_R;           //this property can be connected to this type of entity
            int whenReturnDo = TQueryInfoTracker.UPD_SETPROPERTY_CONN;  //after a complete(queryInfo), do this task

            //firstly get the FQN of the owner component
            string FQN;
            FQN = findCompFQN(ownerID);
            //do the registration
            registrar.registration(kind, FQN, ownerID, regID, destCompID, sName, sType);

            if (!doingStartup)
            {                                     //if not in startup mode then
                switch (kind)
                {
                    //if this is a driving property then
                    case TypeSpec.KIND_DRIVER:
                        {
                            bool hasManual = false;
                            for (int i = 0; i < manualConnectList.Count; i++)
                            {
                                TManualConnect manConn = manualConnectList[i];
                                if (manConn.kind == kind && manConn.compID == ownerID &&
                                    manConn.name == sName)
                                {
                                    registrar.replacePropertyConnections(ownerID, sName, manConn.connects);
                                    hasManual = true;
                                }
                            }
                            if (!hasManual)
                            {
                                queryInfoRequired = true;  //flag that a queryInfo is needed to resolve connections
                                connectToEntityType = TypeSpec.KIND_OWNED_R;
                                whenReturnDo = TQueryInfoTracker.ADD_DRVPROPERTY_CONN;
                            }
                            break;
                        }

                    //if this is a published event then
                    case TypeSpec.KIND_PUBLISHEDEVENT:
                        {
                            bool hasManual = false;
                            for (int i = 0; i < manualConnectList.Count; i++)
                            {
                                TManualConnect manConn = manualConnectList[i];
                                if (manConn.kind == kind && manConn.compID == ownerID &&
                                    manConn.name == sName)
                                {
                                    registrar.replacePubEventConnections(ownerID, sName, manConn.connects);
                                    hasManual = true;
                                }
                            }
                            if (!hasManual)
                            {
                                queryInfoRequired = true;  //flag that a queryInfo is needed to resolve connections
                                connectToEntityType = TypeSpec.KIND_SUBSCRIBEDEVENT;
                                whenReturnDo = TQueryInfoTracker.ADD_PUBEVENT_CONN;
                            }
                            break;
                        }

                    case TypeSpec.KIND_REQUESTSET:
                        {
                            queryInfoRequired = true;  //flag that a queryInfo is needed to resolve connections
                            connectToEntityType = TypeSpec.KIND_OWNED_RW;
                            whenReturnDo = TQueryInfoTracker.UPD_SETPROPERTY_CONN;
                            break;
                        }

                    //if this is an owned property then
                    case TypeSpec.KIND_OWNED_R:
                    case TypeSpec.KIND_OWNED_W:
                    case TypeSpec.KIND_OWNED_RW:
                    //if this is a subscribed event then
                    case TypeSpec.KIND_SUBSCRIBEDEVENT:
                    default:;
                        break;
                }

                if (queryInfoRequired)
                {            //if I need to do a queryInfo
                    queryMsg = buildQueryInfo(sName, connectToEntityType, FMyID); //queryInfo is always sent to the owner
                    queryCompID = queryMsg.to;      //keep a local copy
                    queryMsgID = queryMsg.msgID;    //keep a local copy
                    //Keep a record of the request so that the system knows what to do
                    // with the returned values in the handleReturnInfo()
                    queryInfoTracker.addQueryMsg(queryMsg.msgID, ownerID, sName, regID, destCompID, whenReturnDo);

                    if (toAck > 0)
                    {  //if this registration needs to be acknowledged
                        //record the registration msg and the fact that it branched a queryInfo
                        msgDirector.addMsgTrunk(ownerID, msgID, Msgs.MSG_REGISTER, 2, 1);
                        msgDirector.addMsgBranch(ownerID, msgID, queryCompID, queryMsgID); //for the queryInfo
                        msgDirector.addMsgBranch(ownerID, msgID, queryCompID, queryMsgID); //virtual branch for the notifyRegchange
                    }
                    sendMessage(queryMsg);                //this system will then handle returnInfo's and the complete
                }

                //do any registration changes locally and then
                //send the notifyRegistrationChange to all the assoc systems
                String itemName = sName;
                if (!sName.Contains(".")) //if the name is unqualified then
                {
                    itemName = findCompFQN(ownerID) + "." + sName; //form a fqn for the property/event
                }
                doRegistrChange(true, kind, ownerID, regID, itemName, sType, ownerID);

                //determine whether a complete(registration) should be sent now
                if (toAck > 0)
                {
                    if (!queryInfoRequired)                //simple reply. No complete(queryInfo) required.
                        sendComplete(ownerID, msgID);       //send a complete back to the comp requesting the registration
                    else
                    {
                        if (queryInfoRequired)
                        {            //if I did a queryInfo
                            //store the fact that the notifyRegistration has been done for this registration msg
                            uint branchesRemaining = msgDirector.pruneBranch(Msgs.MSG_REGISTER, queryCompID, queryMsgID);  //remove the virtual branch
                            if (branchesRemaining < 1)
                            {     //if the queryInfo has completed
                                msgDirector.removeTrunk(ownerID, msgID);
                                sendComplete(ownerID, msgID);       //send a complete back to the comp requesting the registration
                            }
                        }
                    }
                } //endif acknowledge
            }  //endif not in startup
        }
        //============================================================================
        /// <summary>
        /// When a notifyRegistrationChange msg arrives, it could be a registration
        /// or deregistration. I may need to do something and also forward the msg
        /// on to any other child systems.
        /// </summary>
        /// <param name="registered">True if this entity is registered.</param>
        /// <param name="kind">Kind of entity.</param>
        /// <param name="ownerID">Owner ID.</param>
        /// <param name="regID">Registration ID.</param>
        /// <param name="sName">Name.</param>
        /// <param name="sType">DDML type.</param>
        /// <param name="msgFrom">Message from.</param>
        //============================================================================
        protected void doRegistrChange(bool registered, int kind, uint ownerID, uint regID,
                                       string sName, string sType, uint msgFrom)
        {
            if (registered)             //if this is a registration
            {                         
                //*****************
                //handle the registration change here for this system
                //if this system has items that can be connected to the new item then
                //do the auto connections
                //*****************
                // See if we have a published event or readable property that can connect
                // to a subscribed event or driver that we are managing
                if (!doingStartup &&
                     ((kind == TypeSpec.KIND_SUBSCRIBEDEVENT) ||
                     (kind == TypeSpec.KIND_OWNED_R) ||
                     (kind == TypeSpec.KIND_OWNED_RW) ||
                     (kind == TypeSpec.KIND_OWNED_W)))
                {
                    string searchName;
                    string ownerName;
                    uint entityOwnerID;               //compID that owns the entity being searched for
                    TEntityList infoList;
                    TIDSpec entity;

                    //access the registrar to determine the match to event/property names
                    searchName = TRegistrar.unQualifiedName(sName);
                    ownerName = qualifiedOwnerName(sName);

                    //get the ID of the ownerName (it is a child of this system)
                    entityOwnerID = findCompID(ownerName, true);    //if ownerName == "" -> entityOwnerID = 0
                    if (entityOwnerID == UInt32.MaxValue)
                        entityOwnerID = 0;

                    //do a search for all entities of KIND for this compID
                    infoList = new TEntityList();

                    //search the registry. Handles wildcard entity names.
                    if (kind == TypeSpec.KIND_SUBSCRIBEDEVENT)
                    {
                        if ((entityOwnerID == 0) && (ownerName.Length < 1))                //if the event is unqualified then
                            ownerName = findCompFQN(ownerID);   //get the FQN of the owner of the subscr event
                        registrar.findEntities(entityOwnerID, ownerName, searchName, TypeSpec.KIND_PUBLISHEDEVENT, ref infoList, sType);
                        string ownerFQN = findCompFQN(ownerID);
                        for (int i = 1; i <= infoList.count(); i++)
                        {  //1-x indexed
                            entity = infoList.getEntity(i);
                            registrar.addPubEventConnection(entity.compID, entity.itemID, ownerID, ownerFQN, regID, entity.sType);
                        }
                    }
                    if ((kind == TypeSpec.KIND_OWNED_R) || (kind == TypeSpec.KIND_OWNED_RW))
                    {
                        if ((entityOwnerID == 0) && (ownerName.Length < 1))                //if the property is unqualified then
                            ownerName = findCompFQN(ownerID);   //get the FQN of the owner of the newly registered property
                        entityOwnerID = 0; //searching for something with no specific owner
                        registrar.findEntities(entityOwnerID, ownerName, searchName, TypeSpec.KIND_DRIVER, ref infoList, sType);
                        for (int i = 1; i <= infoList.count(); i++)
                        {  //1-x indexed
                            entity = infoList.getEntity(i);
                            // We need the FQN of the owner of the driving property
                            // We obtain it via a query info
                            TMsgHeader msg = buildQueryInfo(searchName, TypeSpec.KIND_OWNED_R, FMyID); //queryInfo is always sent to the owner
                            //Keep a record of the request so that the system knows what to do
                            // with the returned values in the handleReturnInfo()
                            queryInfoTracker.addQueryMsg(msg.msgID, entity.compID, sName, entity.itemID, ownerID, TQueryInfoTracker.ADD_DRVPROPERTY_CONN);
                            sendMessage(msg);
                            //           registrar->addPropertyConnection(entity->compID, entity->itemID, ownerID, regID, entity->name);
                        }
                    }
                    // A new writeable property might match the requirement of a requestSet
                    if ((kind == TypeSpec.KIND_OWNED_RW) || (kind == TypeSpec.KIND_OWNED_W))
                    {
                        if ((entityOwnerID == 0) && (ownerName.Length < 1))                //if the property is unqualified then
                            ownerName = findCompFQN(ownerID);   //get the FQN of the owner of the newly registered property
                        infoList.clear();
                        entityOwnerID = 0; //searching for something with no specific owner
                        registrar.findEntities(entityOwnerID, ownerName, searchName, TypeSpec.KIND_REQUESTSET, ref infoList, sType);
                        for (int i = 1; i <= infoList.count(); i++)
                        {  //1-x indexed
                            entity = infoList.getEntity(i);
                            // We need the FQN of the owner of the new writeable property
                            //  We obtain it via a query info
                            TMsgHeader msg = buildQueryInfo(searchName, TypeSpec.KIND_OWNED_W, FMyID); //queryInfo is always sent to the owner
                            //Keep a record of the request so that the system knows what to do
                            // with the returned values in the handleReturnInfo()
                            queryInfoTracker.addQueryMsg(msg.msgID, entity.compID, sName, entity.itemID, ownerID, TQueryInfoTracker.ADD_SETPROPERTY_CONN);
                            sendMessage(msg);
                            // registrar->updateSetterConnection(entity->compID, entity->itemID, ownerID, regID, searchName);
                        }
                    }
                }
            }
            else
            {                                    //deregister the property/event/component
                //do the deregistration
                if ((kind == TypeSpec.KIND_COMPONENT) || (kind == TypeSpec.KIND_SYSTEM))
                    regID = ownerID;
                registrar.deregistration(kind, ownerID, regID);      //give it to the registrar
            }
            //send the notifyRegistrationChange to all the assoc systems
            broadcastRegoChange(registered, kind, ownerID, (int)regID, sName, sType, msgFrom);
        }
        //============================================================================
        /// <summary>
        /// This is the destination for the deregistration message.
        /// Do the deregistration and then broadcast the notifyRegistrationChange
        /// </summary>
        /// <param name="msg">The deregistration message.</param>
        //============================================================================
        protected void handleDeregistration(TMsgHeader msg)
        {
            int kind;
            uint ownerID, regID;
            uint msgFrom;   //source of deregistration msg


            interpreter.loadMessage(msg);
            //store the values from the incoming deregistration msg
            kind = interpreter.getIntField(Msgs.MSG_DEREGISTER_KIND);
            ownerID = msg.from;
            regID = (uint)interpreter.getIntField(Msgs.MSG_DEREGISTER_ID);
            msgFrom = msg.from;                                  //store the source of the dereg msg

            // Don't we need a call to handleRegistrChange here instead???
            doRegistrChange(false, kind, ownerID, regID, "", "", msgFrom);
            //send the notifyRegistrationChange to all the assoc systems
            //// broadcastRegoChange(false, kind, ownerID, regID, "", "", msgFrom);

            //deregister
            if ((kind == TypeSpec.KIND_COMPONENT) || (kind == TypeSpec.KIND_SYSTEM))
                regID = ownerID;
            registrar.deregistration(kind, ownerID, regID);   //give it to the registrar
        }
        //============================================================================
        /// <summary>
        /// Broadcast the change notification to other systems.
        /// srcID is used to ensure that this doesn't get sent back to where
        /// it came from.
        /// </summary>
        /// <param name="registered">True if this is a registration.</param>
        /// <param name="kind">property, method, or type of component to de/register.</param>
        /// <param name="ownerID">component that owns the property or method. Or the component to de/register.</param>
        /// <param name="regID">registration id of the property or event.</param>
        /// <param name="sName">unqualf'd name of the property/event/component.</param>
        /// <param name="sType">DDL type or component type/class.</param>
        /// <param name="srcID">id of the system sending the broadcast.</param>
        //============================================================================
        protected void broadcastRegoChange(bool registered, int kind, uint ownerID,
                                           int regID, string sName, string sType, uint srcID)
        {
            uint sendto;
            TComp child;

            //Send a notifyRegistrationChange msg to the parent system and all child systems
            //(except the one that just sent the registration msg).

            //send it to the parent if it this is not the top simulation and is not the source
            //now create the new notifyRegistrationChange msg
            if ((FParentID != FMyID) && (FParentID != srcID))
                sendNotifyRegoChange(registered, kind, ownerID, regID, sName, sType, FParentID, 0);

            //now send a msg to each child system
            for (int i = 0; i < compList.Count; i++)
            {   //for each child
                child = compList[i];
                if (child.isSystem)
                {  //if the child is a system
                    sendto = child.compID;
                    //// We normally don't want to send this back to a child that sent
                    //// it to us, but the case where the child is the originator is
                    //// a special case.
                    if (srcID != sendto || sendto == ownerID)
                    {
                        //now create the new notifyRegistrationChange msg
                        sendNotifyRegoChange(registered, kind, ownerID, regID, sName, sType, sendto, 0);
                    }
                }
            }
        }
        //============================================================================
        /// <summary>
        /// Send a querySet message.
        /// </summary>
        /// <param name="destCompID">Destination component ID.</param>
        /// <param name="propID">Property ID.</param>
        /// <param name="sDDML">DDML type.</param>
        /// <param name="valPtr">Data value.</param>
        /// <param name="valSize">Byte size of the data value.</param>
        //============================================================================
        protected void sendQuerySetValue(uint destCompID, uint propID,
                                         string sDDML, byte[] valPtr, uint valSize)
        {
            sendMessage(buildQuerySetValue(destCompID, propID, sDDML, valPtr, valSize));
        }
        //============================================================================
        /// <summary>
        /// Builds a querySetValue message.
        /// </summary>
        /// <param name="destCompID">Destination component ID.</param>
        /// <param name="propID">Property ID.</param>
        /// <param name="sDDML">DDML type.</param>
        /// <param name="valPtr">Data value.</param>
        /// <param name="valSize">Byte size of the data value.</param>
        /// <returns>QuerySet message.</returns>
        //============================================================================
        protected TMsgHeader buildQuerySetValue(uint destCompID, uint propID,
                                                       string sDDML, byte[] valPtr, uint valSize)
        {
            TMsgHeader newMsg;

            interpreter.setField(Msgs.MSG_QUERYSET_ID, propID);
            interpreter.setField(Msgs.MSG_QUERYSET_TYPE, sDDML);
            interpreter.setField(Msgs.MSG_QUERYSET_VALUE, valPtr, valSize);
            interpreter.createMessage(Msgs.MSG_QUERYSET, destCompID);
            newMsg = interpreter.getMsg();        //get ownership of the msg

            return newMsg;
        }
        //============================================================================
        /// <summary>
        /// Construct a MSG_RETURNINFO message.
        /// </summary>
        /// <param name="queryMsgID">Query message ID.</param>
        /// <param name="compID">Component ID</param>
        /// <param name="regID">Registration ID</param>
        /// <param name="sFQN">Fully qualified name of the entity.</param>
        /// <param name="sType">DDML type of the entity.</param>
        /// <param name="kind">Kind of entity.</param>
        /// <param name="destCompID">Destination component.</param>
        /// <returns>The created ReturnInfo message.</returns>
        //============================================================================
        protected TMsgHeader buildReturnInfo(uint queryMsgID, uint compID, uint regID,
                                             string sFQN, string sType, int kind, uint destCompID)
        {
            interpreter.setField(Msgs.MSG_RETURNINFO_QUERYID, queryMsgID);
            interpreter.setField(Msgs.MSG_RETURNINFO_COMPID, compID);
            interpreter.setField(Msgs.MSG_RETURNINFO_ID, regID);
            interpreter.setField(Msgs.MSG_RETURNINFO_NAME, sFQN);
            interpreter.setField(Msgs.MSG_RETURNINFO_TYPE, sType);
            interpreter.setField(Msgs.MSG_RETURNINFO_KIND, kind);
            interpreter.createMessage(Msgs.MSG_RETURNINFO, destCompID);

            return interpreter.getMsg();
        }
        //============================================================================
        /// <summary>
        /// Create and send a notifyRegistrationChange
        /// </summary>
        /// <param name="registered">True if registered.</param>
        /// <param name="kind">Kind of entity.</param>
        /// <param name="ownerID">Owner ID.</param>
        /// <param name="regID">Registration ID.</param>
        /// <param name="sName">Name of the entity.</param>
        /// <param name="sType">DDML type of the entity.</param>
        /// <param name="sendto">Send the message to.</param>
        /// <param name="toAck">Acknowledge (1==true)</param>
        //============================================================================
        protected void sendNotifyRegoChange(bool registered, int kind, uint ownerID,
                                int regID, string sName, string sType, uint sendto, int toAck)
        {
            TMsgHeader newMsg;

            //now create the new notifyRegistrationChange msg
            interpreter.setField(Msgs.MSG_NOTIFYREG_REG, registered);
            interpreter.setField(Msgs.MSG_NOTIFYREG_KIND, kind);
            interpreter.setField(Msgs.MSG_NOTIFYREG_OWNER, ownerID);
            interpreter.setField(Msgs.MSG_NOTIFYREG_ID, regID);
            interpreter.setField(Msgs.MSG_NOTIFYREG_NAME, sName);
            interpreter.setField(Msgs.MSG_NOTIFYREG_TYPE, sType);
            interpreter.createMessage(Msgs.MSG_NOTIFYREG, sendto);

            newMsg = interpreter.getMsg();
            newMsg.toAck = (uint)toAck;
            sendMessage(newMsg);             //send
        }
        //============================================================================
        /// <summary>
        /// Handles the returned component ID's and proceeds to create the component.
        /// Using the returned FQN, the system searches for the correct sdml to
        /// use for creating the component.
        ///
        /// Assumes that this is the destination for the component ID
        /// The check is done before this function is called
        /// </summary>
        /// <param name="sFQN">FQN of the new child component.</param>
        /// <param name="newID">ID of the new child component.</param>
        //============================================================================
        protected void doReturnComponentID(string sFQN, uint newID)
        {
            bool found;
            TCompParser sdmlChild;

            //using the FQN find the child sdml and send it Init1
            found = false;
            int i = 0;
            while (!found && !mustTerminate && (i < sdmlChildList.Count))
            {   //while comp not found in list
                sdmlChild = sdmlChildList[i];
                if (sdmlChild.InstanceName == TRegistrar.unQualifiedName(sFQN))
                {        //if comp found
                    found = true;

                    //keep info about this new child component
                    TComp comp = new TComp();
                    comp.compID = newID;
                    comp.name = sFQN;
                    comp.isSystem = sdmlChild.IsSystem;
                    comp.CompClass = sdmlChild.CompClass;
                    compList.Add(comp);

                    if (TRegistrar.unQualifiedName(sFQN).ToLower() == "sequencer" ||
                        TRegistrar.unQualifiedName(sFQN).ToLower() == "clock" )
                        sequencerID = newID;

                    sendInit1(newID, sFQN, sdmlChild.getXML());   //sends init1 which creates TBaseComponentWrapper
                }
                else
                    i++; //next comp
            }
        }
        //============================================================================
        /// <summary>
        /// Carries out the second stage of initialisation logic
        /// As this is a system it will send messages to all the children.
        /// overrides TBaseComp.handleInit2()
        /// </summary>
        /// <param name="msg">Copy of incoming Init2 message.</param>
        //============================================================================
        protected override void handleInit2(TMsgHeader msg)                    
        {
            uint msgFrom;

            msgFrom = msg.from;
            TMsgHeader msgToForward = msg;
            TCompParser sdmlChild;

            for (int i = 0; i < sdmlChildList.Count; i++)
            {   //while comp not found in list
                sdmlChild = sdmlChildList[i];
                addManualConnects(findCompID(sdmlChild.InstanceName, false), sdmlChild);
            }
            addManualConnects(0, FSDMLComp);

            // We begin by resolving all manual connections
            resolveManualConnections();    // resolve manual connections

            // Next we resolve the "automatic" connections
            resolvePublishedEvents();
            resolveDrivingProperties();
            resolvePropertySet();                        //requestSet connections to properties

            /* TODO : Check this */
            //   if (!connectEvents && !connectDrivers) {     //if there are no published events/properties
            //      sendComplete(msgFrom, msgID);             //send the Complete(Init2) from here
            //   }

            init2AcksRemain = sdmlChildList.Count;      //keep track of how many Completes should be returned
            base.doInit2(msgToForward);                 //call the initialise(2) of the user's component logic
            if (sdmlChildList.Count < 1)
                sendComplete(msgFrom, msg.msgID);       //non system logic component now sends a complete
            interpreter.incMsgIDCounter(1);             //Fudge this to make id's line up with native engine
            sendInit2ToEachChild(msgFrom);              //a new init2 msg to each child
        }
        //utility functions
        //============================================================================
        /// <summary>
        /// Prefixes a name with this component's name.
        /// e.g. xx.yyy becomes -  me.xx.yyy
        /// </summary>
        /// <param name="sName">Name to qualify</param>
        /// <returns>A qualified name</returns>
        //============================================================================
        protected virtual string partiallyQualifyName(string sName)
        {
            return TRegistrar.unQualifiedName(FName) + "." + sName; //this component's unqualified name + .sName
        }
        //============================================================================
        /// <summary>
        /// Return a ref to the child in the compList that matches the name specified.
        /// </summary>
        /// <param name="name">Name of the child component.</param>
        /// <returns>NullChild if not found</returns>
        //============================================================================
        protected TComp getChild(string name)
        {
            TComp child = NullChild;    

            int i = 0;
            while (i < compList.Count)
            {
                if (name == compList[i].name)
                {
                    child = compList[i];
                    i = compList.Count;  //terminate the loop
                }
                else
                    i++;
            }
            return child;
        }
        //============================================================================
        /// <summary>
        /// Looks through the list of child components and tries to match the szFQN.
        /// Since the TBaseComponents are name using FQN's I need to know what type
        /// of match I am doing. If the szName is FQN then I can do a match with
        /// the FQN of the TBaseComponent. Otherwise I only match the short name.
        /// </summary>
        /// <param name="sName">Name of component to find.</param>
        /// <param name="matchFQN">Do a match to the FQN name.</param>
        /// <returns>Returns the component ID if a match is found.
        /// 0 if the component is name == "". Returns Uint32.MaxValue
        /// if the component is not found.</returns>
        //============================================================================
        protected virtual uint findCompID(string sName, bool matchFQN)
        {
            uint compID = 0;
            if (sName.Length > 0)
            {
                compID = UInt32.MaxValue;  // return this if we have a name, but no match...
                string sCompName;
                bool found;
                TComp child;

                System.Globalization.CultureInfo ci = new System.Globalization.CultureInfo("en-AU");
                int i = 0;
                found = false;
                while (!found && (i < compList.Count))
                {
                    child = compList[i];
                    sCompName = child.name;
                    if (!matchFQN)
                        sCompName = TRegistrar.unQualifiedName(sCompName);
                    
                    if ( (sName.ToLower(ci) == sCompName.ToLower(ci)) && (sCompName.Length > 0))
                    {
                        found = true;
                        compID = child.compID;
                    }
                    i++;
                }
            }
            return compID;
        }
        //============================================================================
        /// <summary>
        /// Looks through the list of child components and tries to match the szFQN.
        /// Returns the component ID if a match is found, else 0 if no match.
        /// Since the TBaseComponents are name using FQN's I need to know what type
        /// of match I am doing. If the szName is FQN then I can do a match with
        /// the FQN of the TBaseComponent. Otherwise I only match the short name.
        /// </summary>
        /// <param name="compID">Component ID</param>
        /// <returns>The fully qualified name.</returns>
        //============================================================================
        protected string findCompFQN(uint compID)
        {
            string buf = "";
            TComp child;

            int i = 0;
            while (i < compList.Count)
            {
                child = compList[i];
                if (child.compID == compID)
                {
                    buf = child.name;
                    i = compList.Count;                   //terminate loop
                }
                else
                    i++;
            }
            return buf;
        }
        //============================================================================
        /// <summary>
        /// Used to resolve the subscribed events that are connections to the published
        /// events registered in this system.
        /// Checks through the connections already described from the initdata section.
        /// For each of these partially described connections, it does a queryInfo
        /// to get the compID and the eventID to set the values for the connection item.
        /// I am also sending out queryInfo's for the published event to find any other
        /// subscribers. (I'm not sure if this is necessary 23/7 )
        /// </summary>
        /// <returns></returns>
        //============================================================================
        private bool resolvePublishedEvents()
        {
            bool processed = false;
            int i, y;
            TPubEvent pubEvent;
            TMsgHeader msg;
            uint reqRegID, reqCompID; //stored id's to use when tracing back the returnInfo msg's
            TIDSpec anEvent;

            //Send out queryInfo for all the published events to determine which
            //events subscribe to the published events stored by this system
            i = 1;
            pubEvent = registrar.publishedEvent(i);
            if (pubEvent != null)     //if there are queryInfo's to be sent
            {                                 
                processed = true;
                while (pubEvent != null)
                {
                    reqCompID = pubEvent.compID;
                    reqRegID = pubEvent.regID;

                    TEntityList connects = pubEvent.connects;
                    //Some published events will have manual connections at this stage that
                    //were described in the initdata section.
                    if (connects.count() > 0)
                    {
                        //If some of the connections are partly described with just the name, then
                        //I need to get the regID and compID of the source of the connection
                        for (y = 1; y <= connects.count(); y++)
                        {                  //for each connection
                            anEvent = connects.getEntity(y);
                            if ((anEvent.compID < 1) /* || (event->itemID < 1) */ )
                            {    //if the connection has only a name then
                                //send out queryInfo for the subscriber's name. The name would be FQN
                                msg = buildQueryInfo(anEvent.name, TypeSpec.KIND_SUBSCRIBEDEVENT, FMyID);
                                //Keep a record of the request so that the system knows what to do
                                // with the returned values in the handleReturnInfo()
                                queryInfoTracker.addQueryMsg(msg.msgID, reqCompID, pubEvent.name, reqRegID, TQueryInfoTracker.ANY_DEST_COMP, TQueryInfoTracker.UPD_PUBEVENT_CONN);

                                sendMessage(msg);                                  //this system will then handle returnInfo's
                            }  //endif
                        }                                                        //next connection
                    }
                    else if (pubEvent.autoConn == true)
                    {   //if continue auto connection
                        //send out queryInfo's for pubEvent->name. The name could be qualified
                        uint destCompID = 0;   //needs to be set to the optional destination compID
                        autoConnectPubEvent(reqCompID, reqRegID, destCompID, pubEvent.name, pubEvent.sType);
                    }
                    i++;
                    pubEvent = registrar.publishedEvent(i);
                }
            }
            return processed;
        }
        //============================================================================
        /// <summary>
        /// Used to resolve the connections to driving properties
        /// </summary>
        /// <returns></returns>
        //============================================================================
        private bool resolveDrivingProperties()
        {
            bool processed = false;
            int i;
            TMsgHeader msg;
            TDrvProperty drvProperty;
            uint reqRegID, reqCompID; //stored id's to use when tracing back the returnInfo msg's
            TIDSpec property;

            //Send out queryInfo with Acknowledge required for all the driving properties to determine which
            //owned properties match them
            i = 1;
            drvProperty = registrar.drivingProperty(i);
            if (drvProperty != null)
            {                                 //if there are queryInfo's to be sent
                processed = true;
                while (drvProperty != null)
                {
                    reqCompID = drvProperty.compID;
                    reqRegID = drvProperty.regID;

                    TEntityList connects = drvProperty.connects;
                    //Some driving properties will have manual connections at this stage that
                    //were described in the initdata section.
                    if (connects.count() > 0)
                    {
                        //If some of the connections are partly described with just the name, then
                        //I need to get the regID and compID of the source of the connection
                        for (int c = 1; c <= connects.count(); c++)
                        {
                            property = connects.getEntity(c);
                            if ((property.compID < 1) /* || (property->itemID < 1) */ )
                            {    //if the connection has only a name then
                                //send out queryInfo for the property name. The name would be FQN
                                msg = buildQueryInfo(property.name, TypeSpec.KIND_OWNED_R, FMyID);
                                //Keep a record of the request so that the system knows what to do
                                // with the returned values in the handleReturnInfo()
                                queryInfoTracker.addQueryMsg(msg.msgID, reqCompID, property.name, reqRegID, TQueryInfoTracker.ANY_DEST_COMP, TQueryInfoTracker.UPD_DRVPROPERTY_CONN);
                                sendMessage(msg);                                  //this system will then handle returnInfo's
                            }  //endif
                        }
                    }
                    else if (drvProperty.autoConn == true)
                    {   //if continue auto connection
                        //send out queryInfo's for drvProperty->name. Assumes that name is unqualified
                        msg = buildQueryInfo(drvProperty.name, TypeSpec.KIND_OWNED_R, FMyID);
                        //Keep a record of the request so that the system knows what to do
                        // with the returned values in the handleReturnInfo()
                        queryInfoTracker.addQueryMsg(msg.msgID, reqCompID, drvProperty.name, reqRegID, TQueryInfoTracker.ANY_DEST_COMP, TQueryInfoTracker.ADD_DRVPROPERTY_CONN);
                        sendMessage(msg);                //this system will then handle returnInfo's
                    }
                    i++;
                    drvProperty = registrar.drivingProperty(i);
                }
            }
            return processed;
        }
        //============================================================================
        /// <summary>
        /// Components that register a 'setter property' specify the destination by
        /// FQN address.
        /// </summary>
        /// <returns></returns>
        //============================================================================
        private bool resolvePropertySet()
        {
            bool processed = false;
            int i;
            TMsgHeader msg;
            TReqSetProperty setProperty;
            uint reqRegID, reqCompID; //stored id's to use when tracing back the returnInfo msg's

            //Send out queryInfo with Acknowledge required for all the properties to determine which
            //owned properties match them
            i = 1;
            setProperty = registrar.setterProperty(i);
            if (setProperty != null)
            {                                 //if there are queryInfo's to be sent
                processed = true;
                while (setProperty != null)
                {
                    reqCompID = setProperty.compID;
                    reqRegID = setProperty.regID;

                    //send out queryInfo's for setterProperty->name. Assumes that name is unqualified?
                    msg = buildQueryInfo(setProperty.name, TypeSpec.KIND_OWNED_W, FMyID);
                    //Keep a record of the request so that the system knows what to do
                    // with the returned values in the handleReturnInfo()
                    queryInfoTracker.addQueryMsg(msg.msgID, reqCompID, setProperty.name, reqRegID, TQueryInfoTracker.ANY_DEST_COMP, TQueryInfoTracker.UPD_SETPROPERTY_CONN);
                    sendMessage(msg);                //this system will then handle returnInfo's

                    i++;
                    setProperty = registrar.setterProperty(i);
                }
            }
            return processed;
        }
        //============================================================================
        /// <summary>
        /// Called at the start of Init2
        /// Attempts to set up manual connections for child components
        /// </summary>
        /// <returns></returns>
        //============================================================================
        protected bool resolveManualConnections()
        {
            bool changed = false;
            if (manualConnectList.Count > 0)
            {
                for (int i = 0; i < manualConnectList.Count; i++)
                {
                    TManualConnect manConn = manualConnectList[i];
                    if (manConn.kind == TypeSpec.KIND_DRIVER)
                    {
                        changed |= registrar.replacePropertyConnections(manConn.compID, manConn.name, manConn.connects);
                    }
                    else if (manConn.kind == TypeSpec.KIND_PUBLISHEDEVENT)
                    {
                        changed |= registrar.replacePubEventConnections(manConn.compID, manConn.name, manConn.connects);
                    }
                }
            }
            return changed;
        }
        //============================================================================
        /// <summary>
        /// Store on the manual connection list information about manual connections
        /// that will later need to be established.
        /// </summary>
        /// <param name="compID">ID of component being examined</param>
        /// <param name="sdmlComp"></param>
        //============================================================================
        private void addManualConnects(uint compID, TCompParser sdmlComp)
        {
            string entity;
            string buf;
            string connBuf;
            bool doAdd;
            string pathBuf;
            bool doPaths = compID == 0;
            uint origCompID = compID;

            // Search for manually connected driving properties
            entity = sdmlComp.initTextByName(sdmlComp.DriverArrayName);
            if (entity.Length > 1)
            {
                TSDMLValue sdmlValue = new TSDMLValue(entity, "");
                for (uint i = 1; i <= sdmlValue.count(); i++)
                { //for each driver
                    TTypedValue arrayItem = sdmlValue.item(i);
                    buf = arrayItem.member("name").asStr();
                    preProcessName(ref buf, origCompID);
                    doAdd = false;

                    if (doPaths)
                    {
                        pathBuf = qualifiedOwnerName(buf);
                        if (pathBuf.Length > 0)
                        {
                            compID = findCompID(TRegistrar.unQualifiedName(pathBuf), false);
                            doAdd = compID != UInt32.MaxValue;
                        }
                    }
                    else
                        doAdd = (buf.Contains(".") == false);
                    if (doAdd)
                    {
                        TManualConnect manConnect = new TManualConnect();
                        manConnect.compID = compID;
                        manConnect.kind = TypeSpec.KIND_DRIVER;
                        manConnect.name = TRegistrar.unQualifiedName(buf);
                        manConnect.connects = new TSDMLValue(arrayItem.member("connects"));
                        for (uint c = 1; c <= manConnect.connects.count(); c++)
                        {
                            connBuf = manConnect.connects.item(c).asStr();
                            if (preProcessName(ref connBuf, origCompID))
                            {
                                manConnect.connects.item(c).setValue(connBuf);
                            }
                        }
                        manualConnectList.Add(manConnect);
                    }
                }

            }

            // Now do it all over again for published events
            entity = sdmlComp.initTextByName(sdmlComp.PubEventArrayName);
            if (entity.Length > 1)
            {
                TSDMLValue sdmlValue = new TSDMLValue(entity, "");
                for (uint i = 1; i <= sdmlValue.count(); i++)
                { //for each driver
                    TTypedValue arrayItem = sdmlValue.item(i);
                    buf = arrayItem.member("name").asStr();
                    preProcessName(ref buf, origCompID);
                    doAdd = false;

                    if (doPaths)
                    {
                        pathBuf = qualifiedOwnerName(buf);
                        if (pathBuf.Length > 0)
                        {
                            compID = findCompID(TRegistrar.unQualifiedName(pathBuf), false);
                            doAdd = (compID != UInt32.MaxValue);
                        }
                    }
                    else
                        doAdd = (buf.Contains(".") == false);
                    if (doAdd)
                    {
                        TManualConnect manConnect = new TManualConnect();
                        manConnect.compID = compID;
                        manConnect.kind = TypeSpec.KIND_PUBLISHEDEVENT;
                        manConnect.name = TRegistrar.unQualifiedName(buf);
                        manConnect.connects = new TSDMLValue(arrayItem.member("connects"));
                        for (uint c = 1; c <= manConnect.connects.count(); c++)
                        {
                            connBuf = manConnect.connects.item(c).asStr();
                            if (preProcessName(ref connBuf, origCompID))
                            {
                                manConnect.connects.item(c).setValue(connBuf);
                            }
                        }
                        manualConnectList.Add(manConnect);
                    }
                }
            }
        }
        //============================================================================
        /// <summary>
        /// Does a queryInfo to itself which then gets routed off to other systems.
        /// </summary>
        /// <param name="ownerID"></param>
        /// <param name="regID"></param>
        /// <param name="destCompID"></param>
        /// <param name="sName"></param>
        /// <param name="sType"></param>
        //============================================================================
        private void autoConnectPubEvent(uint ownerID, uint regID,
                               uint destCompID, string sName, string sType)
        {
            TMsgHeader queryMsg;

            //send out queryInfo's for szName. The name could be qualified
            queryMsg = buildQueryInfo(sName, TypeSpec.KIND_SUBSCRIBEDEVENT, FMyID); //this system will then route queryInfo's
            //Keep a record of the request so that the system knows what to do
            // with the returned values in the handleReturnInfo()
            queryInfoTracker.addQueryMsg(queryMsg.msgID, ownerID, sName, regID, destCompID, TQueryInfoTracker.ADD_PUBEVENT_CONN);
            sendMessage(queryMsg);                //this system will then handle returnInfo's
        }
        //============================================================================
        /// <summary>
        /// Remove an TSDMLComponent from the child list
        /// </summary>
        /// <param name="sName"></param>
        //============================================================================
        protected void removeSDMLChild(string sName)
        {
            TCompParser sdmlChild;

            //remove the TSDMLComponent from the child list
            int i = 0;
            while (i < sdmlChildList.Count)
            {
                sdmlChild = sdmlChildList[i];
                if (sdmlChild.InstanceName == sName)
                {
                    sdmlChildList.RemoveAt(i);
                    i = sdmlChildList.Count;    //terminate loop
                }
                else
                    i++;
            }
        }
        //============================================================================
        /// <summary>
        /// Determine if the entity could be found outside this sub-system.
        /// </summary>
        /// <param name="kind">Kind of entity to find.</param>
        /// <param name="sFQN">Name of the entity.</param>
        /// <returns>True if this entity could be found outside this sub-system.</returns>
        //============================================================================
        protected bool canInfoBeFoundElsewhere(int kind, string sFQN)
        {
            string ownerName;
            bool keepLooking = true;
            TComp child;

            //access the registrar to determine the match to event/property names
            ownerName = qualifiedOwnerName(sFQN);

            if (ownerName.Length > 0)
            {  //if it has an owner then keep checking
                System.Globalization.CultureInfo ci = new System.Globalization.CultureInfo("en-AU");
                if (ownerName.ToLower(ci) == FName.ToLower(ci))
                {   //if the owner is me
                    keepLooking = true;
                }
                else
                {
                    //check if it belongs to a child of this system
                    int i = 0;
                    while (keepLooking && (i < compList.Count))
                    {
                        child = compList[i];
                        //if the child FQN matches the first part of the owner
                        if ( (child.name.Length <= ownerName.Length) && (ownerName.Substring(0, child.name.Length) == child.name) )
                        {   
                            keepLooking = false;
                        }
                        i++;
                    }
                }
            }
            return keepLooking;
        }
        //============================================================================
        /// <summary>
        /// Sends all the messages contained in the msgList
        /// </summary>
        /// <param name="msgList"></param>
        //============================================================================
        protected void sendMsgsFromList(List<TMsgHeader> msgList)  
        {
            TMsgHeader msg;

            while (msgList.Count > 0)
            {
                msg = msgList[msgList.Count - 1];
                sendMessage(msg);
                msgList.RemoveAt(msgList.Count - 1);
            }
        }
        //============================================================================
        /// <summary>
        /// <ol>
        /// <li>Used with setup of manual connections.</li>
        /// <li>Allows a component, or ancestor of a component, to be specified by prefixed</li>
        /// <li>periods. One period indicates "self", two indicates the parent, three the</li>
        /// <li>grand-parent, etc.</li>
        /// <li>This function expands a name (of an event or property) containing prefixed</li>
        /// <li>periods into a fully qualified name.</li></ol>
        /// </summary>
        /// <param name="aName">Reference to the string to be examined and (possibly) modified</param>
        /// <param name="compID">Provides ID of reference component; if 0, "this" is used</param>
        /// <returns>Retuns true iff the name string was modified</returns>
        //============================================================================
        protected bool preProcessName(ref string aName, uint compID)
        {
            bool changed = false;
            char prefixChar = '.';
            int nLevels = 0;

            while (aName[nLevels] == prefixChar)
                nLevels++;
            if (nLevels > 0)
            {
                aName = aName.Substring(nLevels);
                string compName;
                if (compID == 0)
                    compName = FName;   //FQN
                else
                    compName = findCompFQN(compID);
                int endAt = compName.Length;

                //determine how far back to go up the name tree
                while ( (--nLevels > 0) && (endAt > 0) )
                {
                    endAt = compName.LastIndexOf('.', endAt - 1);
                }

                if (endAt > 0)
                    aName = compName.Substring(0, endAt) + '.' + aName;
                changed = true;
            }
            return changed;
        }
        //============================================================================
        /// <summary>
        /// <ol>
        /// <li>Sets initial values as for TBaseComponent.</li>
        /// <li>Sets up connections specified in the SDML script</li>
        /// <li>Starts the process of initialising child components</li></ol>
        /// </summary>
        /// <param name="msg">Copy of the Init1 message.</param>
        /// <param name="SDML"></param>
        /// <param name="FQN"></param>
        //============================================================================
        protected override void doInit1(TMsgHeader msg, string SDML, string FQN) //!< overrides TBaseComponent
        {
            string sSDML;
            TCompParser sdmlChild;

            FSDMLComp = new TCompParser(SDML);//Parser for init script
            FSystem = FSDMLComp.IsSystem;

            FName = FQN;
            FVersion = FSDMLComp.Version;

            //add all the sdml children to this system component's list
            sSDML = FSDMLComp.firstChildComp();                //get the first child
            while (sSDML.Length > 0)
            {
                sdmlChild = new TCompParser(sSDML);
                //Add this sdml child to a class list for later searching
                sdmlChildList.Add(sdmlChild);
                sSDML = FSDMLComp.nextSiblingComp();            //go to the next child component
            }
            init1AcksRemain = sdmlChildList.Count;         //require this many acks to init1

            //now send the init1 msg to the logic dll for this system
            //clear the property list so it can be reloaded when the dll registers it's properties
            //propertyListClear(true);                     //remove all properties
            base.doInit1(msg, SDML, FQN);                  //sends this msg to the base component for registrations 
            if (sdmlChildList.Count < 1)                   //test this just in case
                sendComplete(msg.from, msg.msgID);
            interpreter.incMsgIDCounter(1);                //Fudge this to make id's line up with native engine

            //now create each of these child components/systems
            int i = 0;
            while (i < sdmlChildList.Count && !mustTerminate)
            {
                sdmlChild = sdmlChildList[i];

                string qualifiedName = partiallyQualifyName(sdmlChild.InstanceName);
                sendCompIDRequest(FMyID, qualifiedName);    //get a unique id
                //When this return message is handled, the component gets created
                //see handleReturnComponentID()

                //Each child will send a complete(init1) and when this system
                //gets all of them it will send a complete(init1) to the parent
                if (sdmlChildList.Count > 0)
                {
                    i++;
                }
            }
        }
        //============================================================================
        /// <summary>
        /// Called by the function handleReturnInfo()
        /// </summary>
        /// <param name="msgFrom">Source of this message.</param>
        /// <param name="queryMsgID">Message ID of the queryInfo() message.</param>
        /// <param name="ownerID">Component ID of the owning component.</param>
        /// <param name="entityID">ID of the entity requested.</param>
        /// <param name="iEntityKind">Kind of entity.</param>
        /// <param name="sEntityName">Name of the entity.</param>
        /// <param name="sDDML">DDML type description of the entity.</param>
        //============================================================================
        protected override void doInfoReturned(uint msgFrom, uint queryMsgID, uint ownerID,
                                  uint entityID, uint iEntityKind,
                                  string sEntityName, string sDDML)
        {
            int task;
            MsgTask theTask;

            //The returnInfo has returned with a purpose defined when the queryInfo
            //was sent. Use theTask->doOnReturn to find the original message intention.
            theTask = queryInfoTracker.findMsg(queryMsgID); //need the message info for later
            if (!queryInfoTracker.IsNull(theTask))
                task = theTask.doOnReturn;
            else
                task = 0;        //execute the default: option

            string ownerFQN = findCompFQN(ownerID);

            switch (task)
            {
                case TQueryInfoTracker.ADD_PUBEVENT_CONN:
                    {  //add a subscribing event connection to the published event (doesn't have a name)
                        if ((theTask.destCompID == TQueryInfoTracker.ANY_DEST_COMP) || (ownerID == theTask.destCompID))
                        {
                            registrar.addPubEventConnection(theTask.requestingCompID,
                                                             theTask.requestingRegID,
                                                             ownerID,
                                                             ownerFQN,
                                                             entityID,
                                                             sDDML);
                        }
                    }
                    break;
                case TQueryInfoTracker.UPD_PUBEVENT_CONN:
                    {  //update a subscribing event connection (by name) for the published event
                        registrar.updatePubEventConnection(theTask.requestingCompID,
                                                            theTask.requestingRegID,
                                                            ownerID,
                                                            sEntityName,
                                                            entityID);
                    }
                    break;
                case TQueryInfoTracker.ADD_DRVPROPERTY_CONN:
                    {  //add a property connection to the driving property
                        if ((theTask.destCompID == TQueryInfoTracker.ANY_DEST_COMP) || (ownerID == theTask.destCompID))
                        {
                            registrar.addPropertyConnection(theTask.requestingCompID,
                                                             theTask.requestingRegID,
                                                             ownerID,
                                                             entityID, sEntityName);
                        }
                    }
                    break;
                case TQueryInfoTracker.UPD_DRVPROPERTY_CONN:
                    {  //update a property connection for the driving property (by name)
                        registrar.updatePropertyConnection(theTask.requestingCompID,
                                                            theTask.requestingRegID,
                                                            ownerID,
                                                            sEntityName,
                                                            entityID);
                    }
                    break;
                case TQueryInfoTracker.ADD_SETPROPERTY_CONN:
                    {
                        if (ownerID == theTask.destCompID)
                            registrar.updateSetterConnection(theTask.requestingCompID,
                                                            theTask.requestingRegID,
                                                            ownerID,
                                                            entityID,
                                                            sEntityName);
                    }
                    break;
                case TQueryInfoTracker.UPD_SETPROPERTY_CONN:
                    {
                        registrar.updateSetterConnection(theTask.requestingCompID,
                                                          theTask.requestingRegID,
                                                          ownerID,
                                                          entityID,
                                                          sEntityName);
                    }
                    break;

                //This message has another purpose. It may need to be routed back to a component
                //that was broadcasting a queryInfo.
                default:
                    {
                        //if this is the case, then I should have a record of it in the message director
                        if (msgDirector.isABranch(Msgs.MSG_QUERYINFO, msgFrom, queryMsgID))
                        {        //if this was a rebroadcast msg then
                            TTrunkMsg queryInfoMsg = new TTrunkMsg();
                            msgDirector.getBranch(Msgs.MSG_QUERYINFO, msgFrom, queryMsgID, ref queryInfoMsg);   //get the initiator
                            uint queryInfoMsgID = queryInfoMsg.inMsgID;                 //store the initial msg id
                            uint senderID = queryInfoMsg.returnToCompID;                //store the sender id

                            ///// This branch will be pruned upon receipt of the COMPLETE msg.
                            ///// Don't do it here, since there might be multiple returninfos,
                            ///// and we'll need to be able to handle all of them.
                            ///// msgDirector.pruneBranch(MSG_QUERYINFO, msgFrom, queryMsgID);
                            //send a returnInfo message to the caller with details from this returnInfo
                            TMsgHeader returnMsg = buildReturnInfo(queryInfoMsgID, ownerID, entityID, sEntityName, sDDML, (int)iEntityKind, senderID);
                            sendMessage(returnMsg);
                        }
                        else
                        { //now send the msg to the wrapper component for this system
                            
                        }
                    }
                    break;
            }
        }
        //============================================================================
        /// <summary>
        /// Get the child component's ID
        /// </summary>
        /// <param name="index">The index of the component in the child list.</param>
        /// <param name="name">The component name.</param>
        /// <returns>The component ID. Returns zero if no component found.</returns>
        //============================================================================
        public uint getChildComp(uint index, ref string name)
        {
            TComp child;
            uint i = 0;
            uint compID = 0;

            if (compList.Count > index)
            {
                int it = 0;
                while ((it < compList.Count) && (compID == 0))
                {
                    if (index == i)
                    {
                        child = compList[it];
                        name = child.name;
                        compID = child.compID;
                    }
                    it++;
                    i++;
                }
            }
            else
                name = "";

            return compID; ;
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
        protected override void writeStateProperty(uint reqMsgID, uint replyTo, string sDDML, byte[] valPtr, uint valSize)
        {
            TCompParser compParser;
            TCompParser sdmlChild;
            TDDMLValue compDef;
            string childName = "";
            string newSetterName;
            int newSetterID;
            TSetterProperty newSetter;
            TMsgHeader msg;
            List<TMsgHeader> msgList;
            bool found;

            msgList = new List<TMsgHeader>();

            compDef = new TDDMLValue(sDDML, "");
            compDef.setData(valPtr, (int)valSize, 0);
            //init all the inits from the initsection
            compParser = new TCompParser(compDef.asStr());

            bool success = initAllInits(compParser);

            //this is a system
            if (success)
            {
                msgDirector.addMsgTrunk(replyTo, reqMsgID, Msgs.MSG_QUERYSET, propertyNameToID("state"), 0); //source of the querySetValue
                //initialise all the child components
                uint childID;
                uint i;
                //get all the components of the system and send the initsections to the children
                XmlNode childNode = compParser.firstChildCompNode();  //get the first child
                while (childNode != null)
                {
                    sdmlChild = new TCompParser(childNode);
                    i = 0;
                    found = false;
                    childID = getChildComp(i, ref childName);
                    while (!found && (childID != 0))
                    {
                        System.Globalization.CultureInfo ci = new System.Globalization.CultureInfo("en-AU");
                        if (childName.ToLower(ci) == sdmlChild.InstanceName.ToLower(ci))
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

                            newSetter = setPropertyList[newSetterID];
                            newSetter.destName = newSetterName;   //store the fqn of the dest state property
                            newSetter.regID = (uint)newSetterID;

                            newSetter.setValue(compParser.getText(childNode)); //store the <component> xml in the setter property

                            msg = buildRequestSetMsg(newSetterID, (TTypedValue)setPropertyList[newSetterID]);
                            msgList.Add(msg);
                            msgDirector.addMsgBranch(replyTo, reqMsgID, FMyID, (uint)newSetterID);   //querySet msg used to set a Setter property
                        }
                        else
                        {
                            i++;
                            childID = getChildComp(i, ref childName);
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
                    msg = msgList[it];
                    sendMessage(msg);                                        //now the msg for the state variable
                    it++;
                }
                //waits for all the notifySetValueSuccess replies and then sends a success value back to replyTo.
            }
        }
        //============================================================================
        /// <summary>
        /// Initialise procedure for a component. Overriden by derived classes.
        /// </summary>
        /// <param name="iStage">Stage 1 or 2 of the initialisation.</param>
        //============================================================================
        public abstract override void initialise(int iStage);
        //============================================================================
        /// <summary>
        /// Initialise the owned property. Overriden by derived classes.
        /// </summary>
        /// <param name="propertyID">Property ID.</param>
        /// <param name="aValue">Value.</param>
        //============================================================================
        public abstract override void initProperty(int propertyID, TTypedValue aValue);
        //============================================================================
        /// <summary>
        /// Returns the data describing the connections from the registrar
        /// </summary>
        /// <param name="type"></param>
        //============================================================================
        public List<TConnectEntity> getConnectList(TConnectType type)
        {
            return registrar.getConnectList(type);
        }
        //============================================================================
        /// <summary>
        /// Returns the data describing the connections from the registrar
        /// </summary>
        /// <param name="connectData">Data for a typeEntityList of ddmlvalue</param>
        /// <param name="type"></param>
        //============================================================================
        public void getConnectionList(out byte[] connectData, TConnectType type)
        {
            registrar.getConnectionList(out connectData, type);
        }
    }

}
