using System;
using System.Xml;
using System.Text;
using System.Reflection;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Runtime.InteropServices;
using CSGeneral;
using CMPServices;


public class CMPComponentInterface
{
    //------ GetValue ------
    struct GetValueType
    {
        int ID;
    }
    private delegateMsgCallback _callengine;
    private TMessageInterpreter interpreter;
    public string componentType = "APSRU";
    public string version = "2.0";
    public string author = "APSRU";
    public int active = 1;
    public string state = "";
    protected int propID;
    public CMPComponentInterface(delegateMsgCallback callback, uint componentid, uint parentid, string _dllName)
    {
        propID = 1;
        componentID = componentid;
        parentID = parentid;
        dllName = _dllName;
        //initMessageFactory();
        errorHasOccurred = false;
        //simScript = null;
        //init1 = null;
        tick = new TimeType();
        tick.startday = tick.startsec = 0; tickID = 0;
        haveWrittenToStdOutToday = false;
        _callengine = callback;
        interpreter = new TMessageInterpreter(componentID);
        interpreter.initMsgIDCounter(componentID * 100000);     //ensure msgID's are interesting
        List<TMsgHeader> messages = new List<TMsgHeader>();
    }
    protected int nextID()
    {
        return ++propID;
    }
    // -----------------------------------------------------------------------
    /// <summary>
    /// Get the value of a variable from another component.
    /// </summary>
    /// <param name="name"></param>
    /// <param name="units"></param>
    /// <param name="optional"></param>
    /// <param name="data"></param>
    /// <returns></returns>
    // -----------------------------------------------------------------------
    public Boolean get(String name, String units, Boolean optional, ref TDDMLValue data)
    {
        clearMessages();

        // see if we have an array specifier.   //// TODO: fix the ability to use array specifiers
        ////   ArraySpecifier* arraySpecifier = ArraySpecifier::create(name);
        String nameWithoutArraySpec = name;
        ////   if (arraySpecifier != NULL)
        ////      nameWithoutArraySpec = arraySpecifier->variableName();

        int id = nameToRegistrationID(nameWithoutArraySpec, RegistrationKind.getReg);
        Boolean alreadyRegistered = (id != 0);
        if (!alreadyRegistered)
            id = RegisterWithPM(nameWithoutArraySpec, units, "", RegistrationKind.getReg, data.asDDML());

        interpreter.setField(Msgs.MSG_GETVALUE_ID, id);      //the driver ID code
        TMsgHeader msg = interpreter.createMessage(Msgs.MSG_GETVALUE, parentID);
        msg.toAck = 1;  //always
        sendMessage(msg);

        String errorMsg = "";
        if (messages.Count == 0)
            errorMsg = "No component responded to a 'get' for variable: " + name;

        else if (messages.Count == 1)
        {
            interpreter.loadMessage(messages[0]);    //take ownership of this msg
            int iCompID = interpreter.getIntField(Msgs.MSG_RETURNVALUE_COMPID);
            int iPropertyID = interpreter.getIntField(Msgs.MSG_RETURNVALUE_ID);
            string DDML = interpreter.getTextField(Msgs.MSG_RETURNVALUE_TYPE);
            byte[] dataPtr = new byte[1];
            uint dataSize = interpreter.getValueField(Msgs.MSG_RETURNVALUE_VALUE, ref dataPtr);

            data.setData(dataPtr, dataSize, 0);

            ////if (arraySpecifier != NULL)
                ////arraySpecifier->summariseData(returnMessageData, returnValue.ddml);
        }
        else if (messages.Count > 1)
            errorMsg = "Too many components responded to a 'get' for variable: " + name;

        if (errorMsg != "")
        {
            if (!optional)
                throw (new ApplicationException(errorMsg));
            return false;
        }
        else
            return true;
    }
    // -----------------------------------------------------------------------
    /// <summary>
    /// Set the value of a variable in another component.
    /// </summary>
    /// <param name="name"></param>
    /// <param name="units"></param>
    /// <param name="data"></param>
    // -----------------------------------------------------------------------
    public void set(String name, String units, ref TDDMLValue data)
    {
        clearMessages();
        int id = nameToRegistrationID(name, RegistrationKind.setReg);
        bool alreadyRegistered = (id != 0);
        if (!alreadyRegistered)
            id = RegisterWithPM(name, units, "", RegistrationKind.setReg, data.asDDML());

        TMsgHeader msg = buildRequestSetMsg(id, data);
        if (msg.msgID != 0)
            sendMessage(msg);
    }
/*      bool read(const std::string& name, Convertable* value, bool optional);
      bool read(const std::string& name, std::vector<Convertable*> values, bool optional);

      void publish(const std::string& name, Packable* data);
      void subscribe(const std::string& eventName, Packable* handler);
      void query(const std::string& pattern, std::vector<QueryMatch>& matches);

      void setSearchOrder(const std::vector<std::string> &list) {simSectionsToSearch = list;};
      void getSearchOrder(std::vector<std::string> &list) {list = simSectionsToSearch;};
      bool readFiltered(const std::string& filterName, std::vector<std::string> &values);
      bool readAll(std::vector<std::string> &names, std::vector<std::string> &values);
      bool readScripts(std::map<std::string, std::string> &scripts);
    */
    // -----------------------------------------------------------------------
    /// <summary>
    /// Export a variable. The variable passed in is stored directly
    /// in our map so the assumption is that we are now owners.
    /// ie. don't delete this data object elsewhere!
    /// </summary>
    /// <param name="name"></param>
    /// <param name="units"></param>
    /// <param name="description"></param>
    /// <param name="writable"></param>
    // -----------------------------------------------------------------------
    public void expose(String name, String units, String description, Boolean writable, String sDDML)
    //,Packable* variable);
    {

        RegistrationKind kind = RegistrationKind.respondToGetReg;
        if (writable)
            kind = RegistrationKind.respondToGetSetReg;
        if (nameToRegistrationID(name, RegistrationKind.respondToGetReg) == 0)
            RegisterWithPM(name, units, description, kind, sDDML);

    }
    // -----------------------------------------------------------------------
    /// <summary>
    /// Write a message to the summary stream.
    /// </summary>
    /// <param name="msg"></param>
    // -----------------------------------------------------------------------
    public void write(String msg)
    {
        if (!haveWrittenToStdOutToday)
        {
            if (tick.startday != 0)
            {
                TTimeValue gDate = new TTimeValue(tick.startday, (uint)tick.startsec, tick.startsecpart);
                Console.WriteLine("(Day of year=" + gDate.dayOfYear() + ")" + ", ");
            }
            Console.WriteLine(getName() + ": ");
            haveWrittenToStdOutToday = true;
        }
        Console.WriteLine("     " + msg);
    }
    String getName()
    {
        return name;
    }
    String getFQName()
    {
        return (pathName + "." + name);
    }
    String getExecutableFileName()
    {
        return dllName;
    }
    public String getInitData()
    {
        return simScript.DocumentElement.OuterXml;
    }
      
    // internal stuff.
    // -----------------------------------------------------------------------
    /// <summary>
    /// Called for all incoming messages.
    /// </summary>
    /// <param name="message"></param>
    // -----------------------------------------------------------------------
    private void messageToLogic(TMsgHeader message)
    {
        // We need to keep track of bits of the message because the FARMWI$E infrastructure
        // doesn't guarantee that a message is still valid at the end of this method.
        // eg. it deletes the Init1 message before we get to test the ack flag at the bottom.
        bool ack = message.toAck == 1;
        uint msgID = message.msgID;
        uint msgFrom = message.from;
        interpreter.loadMessage(message);    //take ownership of this msg
        try
        {
            switch (message.msgType)
            {
                case Msgs.MSG_EVENT:        onEvent(message); break;
                case Msgs.MSG_INIT1:
                    {
                        String SDML = interpreter.getTextField(Msgs.MSG_INIT1_SDML);
                        String FQN = interpreter.getTextField(Msgs.MSG_INIT1_FQN);
                        onInit1(SDML, FQN);     //do init1 processing
                    }
                    break;
                case Msgs.MSG_INIT2:        onInit2(message); break;
                case Msgs.MSG_QUERYVALUE:   onQueryValue(message); break;
                case Msgs.MSG_QUERYSET:     onQuerySetValue(message); break;
                case Msgs.MSG_RETURNVALUE:
                case Msgs.MSG_RETURNINFO:
                case Msgs.MSG_NOTIFYSET:
                case Msgs.MSG_REPLYSET:
                    {
                        TMsgHeader copyMsg = new TMsgHeader();
                        copyMsg = message;
                        messages.Add(copyMsg);
                    } break;
            }
            // if acknowledgement is required, then give it.
            if (ack)
            {
                sendComplete(msgFrom, msgID);
            }
        }
        catch (Exception err)
        {
            error(err.Message, true);
        }
    }

    public void error(String errorMessage, bool isFatal)
    {
        String sMsg = errorMessage + "\n";
        sMsg += "Component name: " + name + "\n";
        //use the new error msg
        interpreter.setField(Msgs.MSG_ERROR_FATAL, isFatal);
        interpreter.setField(Msgs.MSG_ERROR_MESSAGE, sMsg);  //its ID
        TMsgHeader msg = interpreter.createMessage(Msgs.MSG_ERROR, parentID);
        sendMessage(msg);

        if (isFatal)
        {
            terminate(); //may not be necessary ???
            errorHasOccurred = true;
        }
    }
    /// <summary>
    /// 
    /// </summary>
    /// <param name="dllName"></param>
    /// <returns></returns>
    String getDescription(String dllName)
    {
        StringBuilder returnString = new StringBuilder();
        try
        {
            returnString.Append("<describecomp>\n");
            returnString.Append("<executable>" + dllName + "</executable>\n");
            returnString.Append("<class>" + name + "</class>\n");
            returnString.Append("<version>1.0</version>\n");
            returnString.Append("<author>APSRU</author>\n");

            foreach (KeyValuePair<String, Reg> reg in regNames)
            {
                RegistrationKind Kind = reg.Value.kind;
                if (Kind == RegistrationKind.respondToGetReg)
                    returnString.Append(getPropertyDescription(reg, "read"));
                else if (Kind == RegistrationKind.respondToSetReg)
                    returnString.Append(getPropertyDescription(reg, "write"));
                else if (Kind == RegistrationKind.respondToGetSetReg)
                    returnString.Append(getPropertyDescription(reg, "both"));
                else if (Kind == RegistrationKind.respondToEventReg)
                    returnString.Append(getEventDescription(reg, "subscribed"));
                else if (Kind == RegistrationKind.eventReg)
                    returnString.Append(getEventDescription(reg, "published"));
                else if (Kind == RegistrationKind.getReg)
                {
                    returnString.Append("   <driver name=\"");
                    returnString.Append(getRegName(reg));
                    returnString.Append("\">\n");
                    returnString.Append(reg.Value.ddml); 
                    returnString.Append("\n</driver>\n");
                }
            }
        }
        catch (Exception err)
        {
            returnString.Append("<ERROR>" + err.Message + "</ERROR>\n");
        }
        returnString.Append("</describecomp>\n");
        return returnString.ToString();
    }
    /*
      // tell the system about an event we will issue later
      void notifyFutureEvent(const std::string& name);

      void deRegisterAll();
     */
    //private ScienceAPI2 scienceAPI;
    private uint componentID;
    private uint parentID;
    private string name;
    private string pathName;
    private string dllName;
    private Boolean errorHasOccurred;
    private XmlDocument simScript;  
    //////private List<String> simSectionsToSearch;
    /////private String currentClass1;
    /////private String currentClass2;
    //private Packable* init1;
    //private std::vector<Packable*> init2;

    protected enum RegistrationKind
    {
        getReg = 1, respondToGetReg = 2,
        setReg = 9, respondToSetReg = 3,
        eventReg = 5, respondToEventReg = 6,
        respondToGetSetReg = 4
    }
    protected struct Reg
    {
        public TDDMLValue data;
        public RegistrationKind kind;
        public String ddml;
        public int regID;
    }
	private List<TMsgHeader> messages;
     /* //typedef std::multimap<std::string, RegistrationKind> NameToRegKindMap;
      //typedef std::multimap<std::string, std::string> NameToDDMLMap;
    */
      private Dictionary<String, Reg> regNames = new Dictionary<String, Reg>();
      //NameToRegKindMap regKinds;
      //NameToDDMLMap regDDMLs;
    
    private int tickID;
    private TimeType tick;
    private bool haveWrittenToStdOutToday;
    // -----------------------------------------------------------------------
    /// <summary>
    /// Clear all messages in our message list.
    /// </summary>
    // -----------------------------------------------------------------------
    private void clearMessages()
    {
        messages.Clear();
    }
   // -----------------------------------------------------------------------
   // Register something with our PM given the specified information.
   // The data passed in is stored directly in our map so the assumption
   // is that we are now owners.
   // ie. don't delete this data object elsewhere!
   // -----------------------------------------------------------------------
    private int RegisterWithPM(String name, String units, String description, RegistrationKind regKind, String sDDML)
    //,Packable* data);
    {
        String ddml = sDDML;
        if (units != "")
            addAttributeToXML(ddml, "unit=\"" + units + "\"");
        if (description != "")
            addAttributeToXML(ddml, "description=\"" + description + "\"");

        // Add new object to our map.
        String fullRegName = name + regKind.ToString();
        Reg reg = new Reg();
        reg.data = new TDDMLValue(sDDML, "");
        reg.kind = regKind;
        reg.ddml = ddml;
        reg.regID = nextID();
        regNames.Add(fullRegName, reg);

        SendRegisterMsg(name, ddml, reg.regID, (int)regKind, 0);
        return reg.regID;
    }
    /// <summary>
    /// Return a registration id for the specified name. 0 if not found.
    /// </summary>
    /// <param name="name"></param>
    /// <param name="regKind"></param>
    /// <returns>0 if not found</returns>
    private int nameToRegistrationID(String name, RegistrationKind regKind)
    {
        String FullRegName = name + regKind.ToString();
        if (regNames.ContainsKey(FullRegName))
        {
            return regNames[FullRegName].regID;
        }
        else
            return 0;
    }
    /// <summary>
    /// Find the registered property by ID.
    /// </summary>
    /// <param name="ID"></param>
    /// <returns></returns>
    private String IDToPropertyName(int ID)
    {
        foreach (KeyValuePair<String, Reg> pair in regNames)
        {
            if (pair.Value.regID == ID)
            {
                return pair.Key;
            }
        }
        return "";
    }
    /// <summary>
    /// Send a message to the engine
    /// </summary>
    /// <param name="message"></param>
    public void sendMessage(TMsgHeader message)
    {
        TNativeMsgHeader msgPtr = new TNativeMsgHeader();
        uint nBytes = message.nDataBytes;
        try
        {
            msgPtr.version = message.version;
            msgPtr.msgType = message.msgType;
            msgPtr.from = message.from;
            msgPtr.to = message.to;
            msgPtr.msgID = message.msgID;
            msgPtr.toAck = message.toAck;
            msgPtr.nDataBytes = nBytes;

            if (nBytes == 0)
            {
                msgPtr.dataPtr = IntPtr.Zero;
            }
            else
            {
                msgPtr.dataPtr = Marshal.AllocHGlobal((int)nBytes);
                Marshal.Copy(message.dataPtr, 0, msgPtr.dataPtr, (int)nBytes);
            }
            _callengine(ref componentID, ref msgPtr);
        }
        finally
        {
            if (nBytes > 0)
                Marshal.FreeHGlobal(msgPtr.dataPtr);
        }
    }

    public void onInit1(String SDML, String FQN)
    {
        // get instance name from fqn.
        pathName = "";
        name = FQN;
        int posPeriod = FQN.LastIndexOf(".");
        if (posPeriod > -1)
            pathName = FQN.Substring(0, posPeriod);

        expose("name", "", "", false, "<type kind=\"string\"/>");
        expose("type", "", "", false, "<type kind=\"string\"/>");
        expose("version", "", "", false, "<type kind=\"string\"/>");
        expose("author", "", "", false, "<type kind=\"string\"/>");
        expose("active", "", "", false, "<type kind=\"integer4\"/>");
        expose("state", "", "", false, "<type kind=\"string\"/>");

        simScript = new XmlDocument();
        simScript.LoadXml(SDML);
    }
    /// <summary>
    /// Handler for Init2 message.
    /// </summary>
    /// <param name="message"></param>
    public void onInit2(TMsgHeader message)
    {
        if ((tickID = nameToRegistrationID("tick", RegistrationKind.respondToEventReg)) == 0) 
   	    {
   	        // We need a tick to determine when to write the "day = ..." heading to a summary file. 
   	        tickID = RegisterWithPM("tick", "", "", RegistrationKind.respondToEventReg, "<type/>");
        }
    }
    /// <summary>
    /// Handler for all QueryValue messages.
    /// </summary>
    /// <param name="message"></param>
    public void onQueryValue(TMsgHeader message)
    {
        String sDDML = "";
        uint iReplyTo = message.from;          //the router

        interpreter.loadMessage(message);
        //expect the wrapper to trigger a returnValue message
        int iPropertyID = interpreter.getIntField(Msgs.MSG_QUERYVALUE_ID);
        int requestedByID = interpreter.getIntField(Msgs.MSG_QUERYVALUE_REQBY);       //source of the request

        //find the property by ID in the regNames list
        String propKey = IDToPropertyName(iPropertyID);
        if (propKey.Length > 0)
        {
            Reg r = regNames[propKey];
            byte[] data = new byte[r.data.sizeBytes()];
            r.data.getData(ref data);   //get the value
            sDDML = r.ddml; 
            sendReplyValue(message.msgID, (uint)requestedByID, sDDML, data, data.Length);
        }
        else
        {
            string errorMsg = string.Format("onQueryValue(): Cannot find property {0}", iPropertyID);
            error(errorMsg, true);
        }
    }
    // -----------------------------------------------------------------------
    /// <summary>
    /// Handler for all QuerySetValue messages.
    /// </summary>
    /// <param name="message"></param>
    public void onQuerySetValue(TMsgHeader message)
    {
        int iPropertyID;
        uint iReplyTo;
        uint msgID;
        uint dataSize;
        string DDML;

        interpreter.loadMessage(message);    //take ownership of this msg
        iReplyTo = message.from;
        msgID = message.msgID;
        iPropertyID = interpreter.getIntField(Msgs.MSG_QUERYSET_ID);           // Parse the request message
        byte[] dataPtr = new byte[1];
        dataSize = interpreter.getValueField(Msgs.MSG_QUERYSET_VALUE, ref dataPtr);
        DDML = interpreter.getTextField(Msgs.MSG_QUERYSET_TYPE);

        //find the property by ID in the regNames list
        String propKey = IDToPropertyName(iPropertyID);
        if (propKey.Length > 0)
        {
            Reg r = regNames[propKey];
            r.data.setData(dataPtr, dataSize, 0); // change the value of the variable.
            sendReplySetValueSuccess(msgID, iReplyTo, true);
        }
        else
            sendReplySetValueSuccess(msgID, iReplyTo, false);
    }
    // -----------------------------------------------------------------------
    /// <summary>
    /// Handler for all Event messages.
    /// </summary>
    /// <param name="message"></param>
    // -----------------------------------------------------------------------
    public void onEvent(TMsgHeader message)
    {
        interpreter.loadMessage(message);    //take ownership of this msg
        int id = interpreter.getIntField(Msgs.MSG_EVENT_ID);
        byte[] dataPtr = new byte[1];
        uint dataSize = interpreter.getValueField(Msgs.MSG_EVENT_PARAMS, ref dataPtr);
        uint publBy = (uint)interpreter.getIntField(Msgs.MSG_EVENT_PUBLISHEDBY);
        string DDML = interpreter.getTextField(Msgs.MSG_EVENT_TYPE);
        /*  TODO implement event handling
        Reg* reg = (Reg*) cmpevent.ID;
        Packable& data = *(reg->data);

        if (cmpevent.ID == tickID)
        {
            unpack(messageData, tick);
            messageData.reset();
            unpack(messageData, cmpevent);
            haveWrittenToStdOutToday = false;
        }
        // unpack the data - this will unpack and then call the function.
        data.unpack(messageData, cmpevent.ddml);
        */
    }
/*      bool readFromSection(XMLNode::iterator initData,
                           XMLNode::iterator sectionData,
                           const std::string& parName,
                           Convertable* value);
      void readAndDemangleScripts(std::map<std::string, std::string> &scripts, XMLNode::iterator &data);
      void replaceManagerMacros(std::string& contents, XMLNode ui);
    */
    /// <summary>
    /// Terminate the simulation.
    /// </summary>
    private void terminate()
    {
        //create and send the msg to the parent
        interpreter.createMessage(Msgs.MSG_TERMINATE, parentID);
        TMsgHeader newMsg = interpreter.getMsg();
        sendMessage(newMsg);
    }

    private String getPropertyDescription(KeyValuePair<String, Reg> reg, String access)
    {
        StringBuilder returnString = new StringBuilder("   <property name=\"");
        returnString.Append(getRegName(reg));
        returnString.Append("\" access=\"" + access + "\" init=\"F\">\n");
        returnString.Append(reg.Value.ddml); 
        
        returnString.Append("\n</property>\n");

        return returnString.ToString();
    }
    String getEventDescription(KeyValuePair<String, Reg> reg, String published)
    {
        StringBuilder returnString = new StringBuilder("   <event name=\"");
        returnString.Append(getRegName(reg));
        returnString.Append("\" kind=\"" + published + "\">\n");
        XmlDocument Doc = new XmlDocument();
        Doc.LoadXml(reg.Value.ddml);
        returnString.Append(Doc.InnerXml);
        returnString.Append("\n</event>\n");
        return returnString.ToString();
    }
    private String getRegName(KeyValuePair<String, Reg> reg)
    {
        string name = reg.Key;
        ///////name.erase(name.length() - 1);   NH ??????
        return name;
    }
    protected void SendRegisterMsg(string sName, string sDDML, int entityID, int kind, uint destID)
    {
        interpreter.setField(Msgs.MSG_REGISTER_KIND, kind);    //the kind of entity
        interpreter.setField(Msgs.MSG_REGISTER_ID, entityID);    //the entity ID
        interpreter.setField(Msgs.MSG_REGISTER_DESTID, destID); //the destID
        interpreter.setField(Msgs.MSG_REGISTER_NAME, sName);    //the entity name
        interpreter.setField(Msgs.MSG_REGISTER_TYPE, sDDML);    //the entity DDML
        interpreter.createMessage(Msgs.MSG_REGISTER, parentID);//send to the parent component

        sendMessage(interpreter.getMsg());
    }
    //============================================================================
    /// <summary>
    /// Send an acknowledgement message
    /// </summary>
    /// <param name="msgTo">Component ID to which complete is sent.</param>
    /// <param name="msgID">ID of the message being acknowledged.</param>
    //============================================================================
    public void sendComplete(uint msgTo, uint msgID)
    {
        interpreter.setField(Msgs.MSG_COMPLETE_ACKID, msgID);
        interpreter.createMessage(Msgs.MSG_COMPLETE, msgTo);  //send back to the sender
        TMsgHeader ackMsg = interpreter.getMsg();
        sendMessage(ackMsg);
    }
    // -----------------------------------------------------------------------
    /// <summary>
    /// Add an attribute (e.g. unit="g/m2") to the end of an
    /// xml string (e.g. "<type name="biomass"/>). Copied from General.dll
    /// </summary>
    /// <param name="XML"></param>
    /// <param name="Attribute"></param>
    // -----------------------------------------------------------------------
    protected void addAttributeToXML(String XML, String Attribute)
    {
        int PosEnd = XML.LastIndexOf("/>");
        if (PosEnd == -1)
            throw (new ApplicationException("Invalid XML string: " + XML));

        XML.Insert(PosEnd, " " + Attribute);
    }
    protected void sendReplyValue(uint queryMsgID, uint replyTo, string sParamType, byte[] aParams, int paramsSize)
    {
        interpreter.setField(Msgs.MSG_REPLYVALUE_QUERYID, queryMsgID);        //ID of queryValue
        interpreter.setField(Msgs.MSG_REPLYVALUE_TYPE, sParamType);           //DDML type
        interpreter.setField(Msgs.MSG_REPLYVALUE_VALUE, aParams, (uint)paramsSize);
        TMsgHeader msg = interpreter.createMessage(Msgs.MSG_REPLYVALUE, replyTo);

        sendMessage(msg);
    }
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

        valSize = Value.sizeBytes();
        byte[] valPtr = new byte[valSize];
        Value.getData(ref valPtr);

        //destination is the owning system
        interpreter.setField(Msgs.MSG_REQUESTSET_ID, iLocalSetPropID);      //local reg property ID
        interpreter.setField(Msgs.MSG_REQUESTSET_TYPE, localProp.sDDML);
        interpreter.setField(Msgs.MSG_REQUESTSET_VALUE, valPtr, valSize);
        newMsg = interpreter.createMessage(Msgs.MSG_REQUESTSET, parentID);
        return newMsg;
    }

    /*
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
        interpreter.createMessage(Msgs.MSG_PUBLISHEVENT, FParentID);
        TMsgHeader msg = interpreter.getMsg();
        msg.toAck = Convert.ToUInt16(bAcknowledge);

        if ((iEventID == EVTERROR) && eventInfo.member("fatal").asBool())
        {
            bFatalErrorSent = true;
            iFatalErrorID = msg.msgID;
        }

        sendMessage(msg);
    }*/

}
