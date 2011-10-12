using System;
using System.Text;
using System.Xml;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Collections.Generic;
using ModelFramework;
using CSGeneral;
using CMPServices;

//=========================================================================
/// <summary>
/// This class creates an instance of the Apsim Component class that is used
/// to host an Apsim .net component. TAPSIMHost descends from TBaseComp
/// which is a non system component. TBaseComp 
/// </summary>
//=========================================================================
[ComVisible(true)]
public class TAPSIMHost : TBaseComp
{
    private int tickID;
    const int smEXECUTE = 1;
    private int drvCOMPCLASS = -1;

    protected ApsimComponent Comp;
    protected Dictionary<String, List<TIDSpec>> entityInfoList; //<searchstring, entityinfo list>
    protected Dictionary<String, List<TComp>> compInfoList;

    //=========================================================================
    /// <summary>
    /// Create an instance of a component here
    /// </summary>
    /// <param name="compID"></param>
    /// <param name="parentCompID"></param>
    /// <param name="messageCallback"></param>
    //=========================================================================
    public TAPSIMHost(uint compID, uint parentCompID, MessageFromLogic messageCallback,
                      String _STYPE, String _SVERSION, String _SAUTHOR)
        : base(compID, parentCompID, messageCallback, _STYPE, _SVERSION, _SAUTHOR)
    {
        //create an APSIM component here
        Assembly Assemb = Assembly.GetCallingAssembly();    //get the cmp component that uses this library
        Comp = new ApsimComponent(Assemb, (int)compID, this);

        Comp.CompClass = _STYPE;
        Comp.createInstance(Assemb.Location, compID, parentCompID);
        FModuleName = getModulePath();

        entityInfoList = new Dictionary<string, List<TIDSpec>>();
        compInfoList = new Dictionary<string, List<TComp>>();
    }
    //=========================================================================
    /// <summary>
    /// 
    /// </summary>
    /// <param name="iStage"></param>
    //=========================================================================
    public override void initialise(int iStage)
    {
        if (iStage == 1)
        {
            Comp.Init1Handler();   //do any APSIM related tasks
        }
        if (iStage == 2)
        {
            Comp.Init2Received = true;
            // We need a tick to determine when to write the "day = ..." heading to a summary file. 
            tickID = eventList.Count;
            addEvent("tick", tickID, TypeSpec.KIND_SUBSCRIBEDEVENT, TTimeStep.typeTIMESTEP, "", "", 0);  //subscribe "tick" event
            DefineSimpleTranistions(tickID);

            Comp.handleEvent(ApsimComponent.INIT2INDEX, null);
        }   
    }
    public void queryEntityInfo(String searchPattern, uint findKind, ref List<TIDSpec> entityList)
    {
        entityInfoList.Add(searchPattern, entityList);  //keep track of the entity list
        sendQueryInfo(searchPattern, findKind, 0);
    }
    public void queryCompInfo(String searchPattern, uint findKind, ref List<TComp> componentList)
    {
        compInfoList.Add(searchPattern, componentList);    //keep track of the component list
        sendQueryInfo(searchPattern, findKind, 0);
    }
    //=========================================================================
    /// <summary>
    /// Override the doComplete so that the list of queried entity lists can be cleaned up.
    /// See queryEntityInfo().
    /// </summary>
    /// <param name="msgFrom">Source of the Complete.</param>
    /// <param name="origMsgID">ID of the original message that this Complete is sent for.</param>
    //=========================================================================
    protected override void doComplete(uint msgFrom, uint origMsgID)
    {
        //Remove the dictionary items from previously sent queryInfo messages in queryEntityInfo()
        for (int i = 0; i < queryList.Count; i++)
        {
            TQueryStore query = queryList[i];
            if (query.iSentMsgID == origMsgID)
            {
                if (compInfoList.ContainsKey(query.sName))
                {
                    compInfoList.Remove(query.sName);
                }
                else if (entityInfoList.ContainsKey(query.sName))
                {
                    entityInfoList.Remove(query.sName);
                }
                break;
            }
        }
        base.doComplete(msgFrom, origMsgID);    //execute default behaviour
    }
    //=========================================================================
    /// <summary>
    /// A handler for returnInfo message.
    /// </summary>
    /// <param name="sReqName">Name of the entity requested.</param>
    /// <param name="sReturnName">Name of the entity returned.</param>
    /// <param name="ownerID">ID of the owning component of the entity.</param>
    /// <param name="entityID">ID of the entity found.</param>
    /// <param name="iKind">Kind of entity.</param>
    /// <param name="sDDML">DDML type of the entity.</param>
    //=========================================================================
    public override void processEntityInfo(string sReqName, string sReturnName, uint ownerID, uint entityID, int iKind, string sDDML)
    {
        base.processEntityInfo(sReqName, sReturnName, ownerID, entityID, iKind, sDDML);
        
        //queryInfo's have been sent for sibling component names
        if ( (iKind == TypeSpec.KIND_COMPONENT) || (iKind == TypeSpec.KIND_SYSTEM) )
        {
            if (compInfoList.ContainsKey(sReqName))
            {
                List<TComp> comps;
                if (compInfoList.TryGetValue(sReqName, out comps))
                {
                    TComp foundComp = new TComp();
                    foundComp.compID = entityID;
                    foundComp.name = sReturnName;
                    if (entityID == Comp.ComponentID)                       //if it is this component then take a shortcut
                    {
                        foundComp.CompClass = FType;                         //use the local value  
                    }
                    else
                        foundComp.CompClass = sDDML;
                    foundComp.isSystem = (iKind == TypeSpec.KIND_SYSTEM || foundComp.CompClass.ToLower() == "paddock" || foundComp.CompClass.ToLower() == "protocolmanager");
                    comps.Add(foundComp);

                    if (foundComp.CompClass.Length <= 1)    // if type information was absent, do a GetValue to find it's type 
                    {
                        //now do a getvalue to find the class type of the component
                        drvCOMPCLASS = driverCount();
                        registerDriver(sReturnName + ".type", drvCOMPCLASS, 1, 1, "-", false, "<type kind=\"string\"></type>", "", "", 0);
                        sendDriverRequest(drvCOMPCLASS, 0);
                    }
                }
            }
            else
            { 
            }
        }
    }
    //=========================================================================
    /// <summary>
    /// Handle the QuerySetValue message.
    /// </summary>
    /// <param name="propertyID"></param>
    /// <param name="aValue"></param>
    /// <returns></returns>
    //=========================================================================
    public override bool writeProperty(int propertyID, TTypedValue aValue)
    {
        return Comp.onQuerySetValue(propertyID, aValue);
    }
    //=========================================================================
    /// <summary>
    /// 
    /// </summary>
    /// <param name="propertyID"></param>
    /// <param name="aValue"></param>
    //=========================================================================
    public override void initProperty(int propertyID, TTypedValue aValue)
    {
        sendError("simulation: Invalid property ID in initProperty()", true);
    }
    //=========================================================================
    /// <summary>
    /// Std properties are already taken care of in the parent class.
    /// Now do a look up of the specialised component properties.
    /// Parent class then sends this value in a ReplyValue msg.
    /// </summary>
    /// <param name="propertyID"></param>
    /// <param name="requestorID"></param>
    /// <param name="aValue">Return value is stored here.</param>
    //=========================================================================
    public override void readProperty(int propertyID, uint requestorID, ref TPropertyInfo aValue)
    {
        Comp.onQueryValue(propertyID, requestorID, ref aValue);
    }
    //=========================================================================
    /// <summary>
    /// Assign a driver value.
    /// </summary>
    /// <param name="driverID"></param>
    /// <param name="providerID">Component that provided the value.</param>
    /// <param name="aValue">Returned value.</param>
    //=========================================================================
    public override void assignDriver(int driverID, uint providerID, TTypedValue aValue)
    {
        if (driverID == drvCOMPCLASS)   //if this driver was a comp.type 
        {
            if (compInfoList.Count == 1)
            {
                foreach (KeyValuePair<String, List<TComp>> pair in compInfoList)
                {
                    List<TComp> comps = pair.Value;
                    for (int i = 0; i < comps.Count; i++)
                    {
                        TComp comp = comps[i];
                        if (comp.compID == providerID)
                        {
                            comp.CompClass = aValue.asString();                 //store the class type
                            comps[i] = comp;
                        }
                    }
                }
            }
            else
            {
                throw new Exception("More requests using compInfoList<> than expected in assignDriver()");
            }
        }
        else
        {
            if (!Comp.assignDriver(driverID, providerID, aValue))
                base.assignDriver(driverID, providerID, aValue);
        }
    }
    //=========================================================================
    /// <summary>
    /// 
    /// </summary>
    /// <param name="eventID"></param>
    /// <param name="iState"></param>
    /// <param name="publisherID"></param>
    /// <param name="aParams"></param>
    /// <returns></returns>
    //=========================================================================
    public override int processEventState(int eventID, int iState, uint publisherID, TTypedValue aParams)
    {
        int iCondition = 0;
        if (eventID == tickID)
        {
            //execute tick event
        }
        else {
            byte[] data = new byte[aParams.sizeBytes()];
            aParams.getData(ref data);
            Comp.handleEvent(eventID, data);
        }
        ////return base.processEventState(eventID, iState, publisherID, aParams);
        return iCondition;
    }
    //============================================================================
    /// <summary>
    /// Override the init1 so that the Apsim component can have access to the SDML script.
    /// </summary>
    /// <param name="msg"></param>
    /// <param name="SDML"></param>
    /// <param name="FQN"></param>
    //============================================================================
    protected override void doInit1(TMsgHeader msg, string SDML, string FQN)
    {
        Comp.ComponentID = msg.to;  //ensure this is set
        Comp.Name = FQN;
        Comp.setScript(SDML);
        base.doInit1(msg, SDML, FQN);
    }
    //=========================================================================
    /// <summary>
    /// 
    /// </summary>
    /// <param name="eventID"></param>
    //=========================================================================
    public void DefineSimpleTranistions(int eventID)
    {
        defineEventTransition(eventID, TStateMachine.IDLE, 0, smEXECUTE, true);   //idle -> smExecute
        defineEventState(eventID, smEXECUTE, TStateMachine.LOGIC);
        defineEventTransition(eventID, smEXECUTE, 0, TStateMachine.DONE, false);   //smExecute -> done
    }
    //============================================================================
    /// <summary>
    /// Adds an event, sets up the state transitions and registers it in the simulation.
    /// </summary>
    /// <param name="sName">Name of the item.</param>
    /// <param name="sDDML">DDML type of the item.</param>
    /// <param name="iEventID">Local ID of the item.</param>
    /// <param name="iKind">Kind of item to register.</param>
    /// <param name="destID">Optional integer ID for a destination component.
    ///	
    /// <para>If kind=<see cref="TypeSpec.KIND_PUBLISHEDEVENT">TypeSpec.KIND_PUBLISHEDEVENT</see>, identifies a component that subscribes to the event being published. 
    /// When the event is published, it must be routed to the nominated component only.</para>
    /// 
    /// <see cref="TypeSpec.KIND_SUBSCRIBEDEVENT">TypeSpec.KIND_SUBSCRIBEDEVENT</see>, destID must be zero.</para> 
    /// 
    /// <para>If destID=0, this field is ignored and routing depends upon the protocol implementation.</para>
    /// </param>
    /// <param name="toAck">Acknowledge this message 1=true.</param>
    //============================================================================
    public void registerEvent(string sName, string sDDML, int iEventID,
                         int iKind, uint destID,
                         int toAck)
    {
        addEvent(sName, iEventID, iKind, sDDML, sName, "", destID);
        DefineSimpleTranistions(iEventID);
    }
    //============================================================================
    /// <summary>
    /// Register a new driving variable
    /// </summary>
    /// <param name="sName"></param>
    /// <param name="driverID"></param>
    /// <param name="iMinConn"></param>
    /// <param name="iMaxConn"></param>
    /// <param name="sUnit"></param>
    /// <param name="bIsArray"></param>
    /// <param name="sType"></param>
    /// <param name="sShortDescr"></param>
    /// <param name="sFullDescr"></param>
    /// <param name="sourceID"></param>
    //============================================================================
    public void registerDriver(string sName, int driverID, int iMinConn, int iMaxConn,
                               string sUnit, bool bIsArray, string sType, String sShortDescr, String sFullDescr, uint sourceID)
    {
        addDriver(sName, driverID, iMinConn, iMaxConn, sUnit, bIsArray, sType, sShortDescr, sFullDescr, sourceID);
    }
    //============================================================================
    /// <summary>
    /// Deregister event
    /// </summary>
    //============================================================================
    public void delEvent(int eventID)
    {
        deleteEvent(eventID);
    }
    public void delDriver(int driverID)
    {
        deleteDriver(driverID);
    }
    public void delProp(int propID)
    {
        deleteProperty(propID);
    }
    //============================================================================
    /// <summary>
    /// Return the number of properties in the registered property list.
    /// </summary>
    /// <returns></returns>
    //============================================================================
    public int propertyCount()
    {
        return propertyList.Count;
    }
    public int driverCount()
    {
        return driverList.Count;
    }
    public int eventCount()
    {
        return eventList.Count;
    }
    //=========================================================================
    /// <summary>
    /// 
    /// </summary>
    //=========================================================================
    public void deleteInstance()
    {
        Comp.deleteInstance();
    }
    //=========================================================================
    /// <summary>
    /// 
    /// </summary>
    /// <param name="context"></param>
    /// <returns></returns>
    //=========================================================================
    public override string description(string context)
    {
        StringBuilder str = new StringBuilder("");
        try
        {
            if (context.Length < 1)
            {
                str.Append(base.description(""));
            }
            else
            {
                XmlDocument Doc = new XmlDocument();
                Doc.LoadXml(context);
                if (Doc != null)
                {
                    String ExecutableFileName = XmlHelper.Attribute(Doc.DocumentElement, "executable");
                    FType = XmlHelper.Attribute(Doc.DocumentElement, "class");
                    Assembly Assemb = Assembly.LoadFile(ExecutableFileName);        // Load the assembly.
                    ApsimComponent Comp = new ApsimComponent(Assemb, 0, this);
                    XmlNode InitData = XmlHelper.Find(Doc.DocumentElement, "initdata");
                    str.Append(Comp.GetDescription(InitData));
                }
            }
        }
        catch (Exception err)
        {
            Console.WriteLine(err.Message);
        }
        return str.ToString();
    }
    //=========================================================================
    /// <summary>
    /// Create an init script manager. 
    /// *** For Apsim: need to determine what to implement for the init script manager.
    /// </summary>
    /// <param name="sScriptName"></param>
    //=========================================================================
    public override void createInitScript(string sScriptName)
    {
        base.createInitScript(sScriptName);
    }
    //=========================================================================
    /// <summary>
    /// Deletes the script instance. 
    /// </summary>
    /// <param name="sScriptName"></param>
    //=========================================================================
    public override void deleteInitScript(string sScriptName)
    {
        base.deleteInitScript(sScriptName);
    }
    //=========================================================================
    /// <summary>
    /// Get the event ID of the named event.
    /// </summary>
    /// <param name="eventName">Name of the event</param>
    /// <param name="kind">The event type e.g. TypeSpec.KIND_PUBLISHEDEVENT</param>
    /// <returns></returns>
    //=========================================================================
    public int getEventID(String eventName, int kind)
    {
        int eventID = -1;
        for (int i = 0; i < eventList.Count; i++)
        {
            if (eventList[i] != null)
            {
                if ((eventList[i].Name.ToLower() == eventName.ToLower()) && ( ((TEventInfo)eventList[i]).iKind == kind) ) 
                    eventID = i; //indexed as in array
            }
        }
        return eventID;
    }
    public String CompClass
    {
        set { FType = value; }
        get { return FType; }
    }
}


