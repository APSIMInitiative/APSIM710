using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Text;

namespace CMPServices
{

    //==============================================================================
    // TSourcedInfo, TDriverInfo, TPropertyInfo, TEventInfo class declarations
    //==============================================================================
    //==============================================================================
    /// <summary>
    /// Ancestor class that does type checking of source values for drivers and events.
    /// </summary>
    //==============================================================================
    public class TSourcedInfo : TDDMLValue
    {
        /// <summary>
        /// 
        /// </summary>
        private List<TSourcedInfo> sourceValues;  //List of sourced values. Used as a cache for type defs.
        /// <summary>
        /// DDML type definition.
        /// </summary>
        public String sDDML;             
        //============================================================================
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="sDDMLType">The DDML type</param>
        /// <param name="sBaseType">Optional base type</param>
        //============================================================================
        public TSourcedInfo(String sDDMLType, String sBaseType)
            : base(sDDMLType, sBaseType)
        {
            sDDML = sDDMLType;
            sourceValues = new List<TSourcedInfo>();
        }
        //============================================================================
        /// <summary>
        /// On the first time a value is provided to a driving property, this method
        /// does the following: 
        /// <ol>
        ///   <li>Tests the type of the value passed to the component for compatibility.</li>
        /// If
        ///   <ul>
        ///    <li>(a) the type is identical, does nothing.</li>
        ///    <li>(b) the type is different but compatible, creates a new TTypedValue
        ///            corresponding to the type of the passed value.</li>
        ///    <li>(c) the type is incompatible, returns FALSE.</li>
        ///   </ul>
        ///   <li>Stores the TTypedValue to be used with values from the current provider
        /// in the Sources[] array where it can be accessed quickly.</li>
        /// </ol>
        ///     
        /// <b>Note: </b>
        ///  Different providers may provide values with different types, as long as
        ///  they are all compatible.
        ///  It is assumed the type of driver values from a given provider will remain
        ///  stable through the simulation.
        /// </summary>
        /// <param name="sourceCompID">ID of the component used as a source.</param>
        /// <param name="sPassedDDML">DDML type description.</param>
        /// <returns>True if they are compatible</returns>
        //============================================================================
        public bool checkCompatibility(uint sourceCompID, String sPassedDDML)
        {
            bool result = false;

            if ((sourceCompID < sourceValues.Count) && (sourceValue(sourceCompID) != null)) //Already tested this ddlvalue
            {
                result = true;
            }
            else
            {
                int iOldCount = sourceValues.Count;
                if (sourceCompID >= iOldCount)
                {
                    for (int i = iOldCount; i <= sourceCompID; i++)
                    {
                        sourceValues.Add(null);
                    }
                }
                TSourcedInfo source = new TSourcedInfo(sPassedDDML, "");

                switch (isSameType(source))
                {
                    case TTypedValue.ctCOMP:         //Parameter order is important! ctCOMP
                        {                             //means "sourceValue is compatible with this"
                            addSourceValue(sourceCompID, source);
                            result = true;
                        }
                        break;
                    case TTypedValue.ctSAME:
                        {                             //means "sourceValue is compatible with this"
                            addSourceValue(sourceCompID, this);
                            result = true;
                        }
                        break;
                    default:                      //incompatible
                        {
                            result = false;
                        }
                        break;
                }
            }
            return result;
        }
        //============================================================================
        /// <summary>
        /// Searches through the source list and returns the one found.
        /// </summary>
        /// <param name="sourceCompID"></param>
        /// <returns>The TSourcedInfo value from the sourceValues list.</returns>
        //============================================================================
        public TSourcedInfo sourceValue(uint sourceCompID)
        {
            return sourceValues[(int)sourceCompID];
        }

        //============================================================================
        /// <summary>
        /// Create and add a new DDMLValue source item to the sourceValues list.
        /// </summary>
        /// <param name="sourceCompID">ID of the component source.</param>
        /// <param name="source">The ref to the DDMLValue</param>
        //============================================================================
        public void addSourceValue(uint sourceCompID, TDDMLValue source)
        {
            while (sourceValues.Count <= (int)sourceCompID)         //We are going to index directly into List later
                sourceValues.Add(null);
            sourceValues[(int)sourceCompID] = (TSourcedInfo)source;
        }
    }

    //==============================================================================
    /// <summary>
    /// A driving property of a component.
    /// </summary>
    //==============================================================================
    public class TDriverInfo : TSourcedInfo
    {
        /// <summary>
        /// Optional source component
        /// </summary>
        public uint iSourceID;        
        /// <summary>
        /// Min expected connections
        /// </summary>
        public int iMinConn;          
        /// <summary>
        /// Max expected connections
        /// </summary>
        public int iMaxConn;         
        /// <summary>
        /// assumes that only one request at a time will be active
        /// </summary>
        public bool bRequestActive;   
        /// <summary>
        /// id of the request message
        /// </summary>
        public uint iRequestMsg;      
        /// <summary>
        /// 
        /// </summary>
        public int iConnCount;        //...
        /// <summary>
        /// Short description
        /// </summary>
        public String sDescr;
        /// <summary>
        /// Full description
        /// </summary>
        public String sDescription; 
        /// <summary>
        /// 
        /// </summary>
        /// <param name="sDDMLType"></param>
        /// <param name="sShortDescr">Short description text</param>
        /// <param name="sFullDescr">Full description text</param>
        /// <param name="sBaseType"></param>
        public TDriverInfo(String sDDMLType, String sShortDescr, String sFullDescr, String sBaseType)
            : base(sDDMLType, sBaseType)
        {
            sDescr = sShortDescr;
            sDescription = sFullDescr;
        }
    }

    //==============================================================================
    /// <summary>
    /// A standard component property item.
    /// </summary>
    //==============================================================================
    public class TPropertyInfo : TInitValue
    {
        /// <summary>
        /// DDML type definition
        /// </summary>
        public String sDDML;     
        /// <summary>
        /// Readable
        /// </summary>
        public bool bRead;       
        /// <summary>
        /// Writeable
        /// </summary>
        public bool bWrite;      
        /// <summary>
        /// An init value
        /// </summary>
        public bool bInit;
        /// <summary>
        /// Short description
        /// </summary>
        public String sDescr;
        /// <summary>
        /// Full description
        /// </summary>
        public String sDescription; 

        //==============================================================================
        /// <summary>
        /// Constructor for a property
        /// </summary>
        /// <param name="sDDMLType">DDML type</param>
        /// <param name="sShortDescr">Short description text</param>
        /// <param name="sFullDescr">Full description text</param>
        /// <param name="sBaseType">Base type. See <see cref="TTypedValue.sTYPECODES"/></param>
        //==============================================================================
        public TPropertyInfo(String sDDMLType, String sShortDescr, String sFullDescr, String sBaseType)
            : base(sDDMLType, sBaseType)
        {
            sDDML = sDDMLType;
            sDescr = sShortDescr;
            sDescription = sFullDescr;
        }
    }
    //==============================================================================
    /// <summary>
    /// Setter property
    /// </summary>
    //==============================================================================
    public class TSetterProperty : TDDMLValue
    {
        /// <summary>
        /// Name of the destination entity
        /// </summary>
        public String destName;
        /// <summary>
        /// Destination component ID
        /// </summary>
        public uint destCompID;
        /// <summary>
        /// Registered ID of this Setter property
        /// </summary>
        public uint regID;
        /// <summary>
        /// DDML type.
        /// </summary>
        public String sDDML;

        //==============================================================================
        /// <summary>
        /// Construct a Setter property.
        /// </summary>
        /// <param name="sDDMLType">DDML type</param>
        /// <param name="sBaseType">Base type. See <see cref="TTypedValue.sTYPECODES"/></param>
        //==============================================================================
        public TSetterProperty(String sDDMLType, String sBaseType)
            : base(sDDMLType, sBaseType)
        {
            sDDML = sDDMLType;
        }
    }

    //==============================================================================
    /// <summary>
    /// Used to store the local event information
    /// </summary>
    //==============================================================================
    public class TEventInfo : TSourcedInfo
    {
        public uint destID;    //optional source component
        public int iKind;      //published or subscribed
        /// <summary>
        /// Short description
        /// </summary>
        public String sDescr;
        /// <summary>
        /// Full description
        /// </summary>
        public String sDescription;
        public bool isVariant;

        public TEventInfo(String sDDMLType, String sShortDescr, String sFullDescr, String sBaseType)
            : base(sDDMLType, sBaseType)
        {
            sDescr = sShortDescr;
            sDescription = sFullDescr;
            isVariant = false;
        }
    }

    //==============================================================================
    /// <summary>
    /// Records the msgID and name of an entity being queryied with a queryInfo message.
    /// </summary>
    //==============================================================================
    public struct TQueryStore
    {
        /// <summary>
        /// 
        /// </summary>
        public uint iSentMsgID;    //ID of the sent message.
        /// <summary>
        /// 
        /// </summary>
        public String sName;       //Name of the entity being queried.
    }

    //==============================================================================
    /// <summary>
    /// Property definition. Used to store the component's properties.
    /// </summary>
    //==============================================================================
    internal struct TPropertyDef
    {
        /// <summary>
        /// Name of the property.
        /// </summary>
        public String sName;        
        /// <summary>
        /// DDML type definition of the property.
        /// </summary>
        public String sType;
        /// <summary>
        /// Short description
        /// </summary>
        public String sDescr;
        /// <summary>
        /// Full description
        /// </summary>
        public String sDescription;
    }
    //==============================================================================
    /// <summary>
    /// Base class for all components.
    /// </summary>
    //==============================================================================
    public abstract class TAbstractComponent
    {
        /// <summary>
        /// published error event ID
        /// </summary>
        public const int EVTERROR = 0;       
        /// <summary>
        /// subscribing error event ID
        /// </summary>
        public const int EVTERROR_S = 1;     
        /// <summary>
        /// Has no connection limit
        /// </summary>
        public const int NO_MAXCONN = -1;    

        //Property IDs are indexed starting from 0
        /// <summary>
        /// Standard Name property
        /// </summary>
        protected const int PROP_NAME = 0;      
        /// <summary>
        /// Standard Type property
        /// </summary>
        protected const int PROP_TYPE = 1;      
        /// <summary>
        /// Standard Version property
        /// </summary>
        protected const int PROP_VERSION = 2;   
        /// <summary>
        /// Standard Author property
        /// </summary>
        protected const int PROP_AUTHOR = 3;    
        /// <summary>
        /// Standard Active property
        /// </summary>
        protected const int PROP_ACTIVE = 4;    
        /// <summary>
        /// Standard State property
        /// </summary>
        protected const int PROP_STATE = 5;     
        /// <summary>
        /// Count of standard properties defined in TAbstractComponent
        /// </summary>
        protected const int NO_STD_PROPERTIES = 6; 

        /// <summary>
        /// default published events property
        /// </summary>
        protected const int PROP_PUBEVENT = 6;  
        /// <summary>
        /// default subscribed events property
        /// </summary>
        protected const int PROP_SUBEVENT = 7; 
        /// <summary>
        /// default driver connections property
        /// </summary>
        protected const int PROP_DRVCONN = 8;  
        /// <summary>
        /// Count of default properties
        /// </summary>
        protected const int NO_DEF_PROPERTIES = 3; 
        /// <summary>
        /// Start indexing the user's properties from this one (NO_STD_PROPERTIES + NO_DEF_PROPERTIES)
        /// </summary>
        public const int PROP_START_INDEX = NO_STD_PROPERTIES + NO_DEF_PROPERTIES;

        /// <summary>
        /// FQN component name partly provided by simulation writer
        /// </summary>
        protected String FName;              
        /// <summary>
        /// Type of this component
        /// </summary>
        protected String FType;              
        /// <summary>
        /// Version number
        /// </summary>
        protected String FVersion;           
        /// <summary>
        /// Author's name
        /// </summary>
        protected String FAuthor;            
        /// <summary>
        /// This component's ID
        /// </summary>
        protected uint FMyID;                
        /// <summary>
        /// The parent system ID
        /// </summary>
        protected uint FParentID;            
        /// <summary>
        /// Name of the dll. (Short name when in logic dll).
        /// </summary>
        protected String FModuleName;        
        /// <summary>
        /// Full path name of the dll
        /// </summary>
        protected String FModulePathName;    

        /// <summary>
        /// component is active
        /// </summary>
        protected bool FActive;              
        /// <summary>
        /// True if this can be a system component
        /// </summary>
        protected bool FSystem;              
        /// <summary>
        /// flag to protect the standard properties during processing
        /// </summary>
        protected bool FGuardProperties;     

        /// <summary>
        /// flag the sending of a fatal error
        /// </summary>
        protected bool bFatalErrorSent;      
        /// <summary>
        /// store the msg ID
        /// </summary>
        protected uint iFatalErrorID;        

        //      protected TTypesDatabase typesDB;      //Interface to the DDML database
        /// <summary>
        /// List of owned properties
        /// </summary>
        protected Collection<TPropertyInfo> propertyList;  
        /// <summary>
        /// List of events
        /// </summary>
        protected List<TTypedValue> eventList;       
        /// <summary>
        /// List of driving properties.
        /// </summary>
        protected List<TDriverInfo> driverList;      
        /// <summary>
        /// List of setter properties.
        /// </summary>
        protected List<TSetterProperty> setPropertyList; 
        /// <summary>
        /// List of queries sent from the component.
        /// </summary>
        protected List<TQueryStore> queryList;       
        /// <summary>
        /// ...
        /// </summary>
        protected List<TDDMLValue> resetList;       
        /// <summary>
        /// Used to determine when property registration occurs
        /// </summary>
        protected bool bRegisterNow;         
        /// <summary>
        /// Used to determine when event registration occurs
        /// </summary>
        protected bool bRegisterEventsNow;   

        private TPropertyDef[] STD_CMP_PROPERTIES;  //Array of standard properties
        private TPropertyDef[] DEF_CMP_PROPERTIES;  //Array of default properties

        //============================================================================
        /// <summary>
        /// The topmost base class for component types in the CMP simulation system.
        /// </summary>
        //============================================================================
        protected TAbstractComponent()
        {
            STD_CMP_PROPERTIES = new TPropertyDef[NO_STD_PROPERTIES];
            DEF_CMP_PROPERTIES = new TPropertyDef[NO_DEF_PROPERTIES];

            propertyList = new Collection<TPropertyInfo>();
            eventList = new List<TTypedValue>();
            driverList = new List<TDriverInfo>();
            setPropertyList = new List<TSetterProperty>();
            queryList = new List<TQueryStore>();
            resetList = new List<TDDMLValue>();

            FName = "";
            FSystem = false;                    //descendent will set this if required
            FModuleName = "";
            FModulePathName = "";
            bRegisterNow = false;               //defer the registration
            bRegisterEventsNow = false;

            STD_CMP_PROPERTIES[PROP_NAME].sName = "name";
            STD_CMP_PROPERTIES[PROP_NAME].sType = TTypedValue.STYPE_STR;
            STD_CMP_PROPERTIES[PROP_NAME].sDescr = "Component name";
            STD_CMP_PROPERTIES[PROP_NAME].sDescription = "The name of the component or submodel";
            STD_CMP_PROPERTIES[PROP_TYPE].sName = "type";
            STD_CMP_PROPERTIES[PROP_TYPE].sType = TTypedValue.STYPE_STR;
            STD_CMP_PROPERTIES[PROP_TYPE].sDescr = "Component class type";
            STD_CMP_PROPERTIES[PROP_TYPE].sDescription = "";
            STD_CMP_PROPERTIES[PROP_VERSION].sName = "version";
            STD_CMP_PROPERTIES[PROP_VERSION].sType = TTypedValue.STYPE_STR;
            STD_CMP_PROPERTIES[PROP_VERSION].sDescr = "Model version";
            STD_CMP_PROPERTIES[PROP_VERSION].sDescription = "";
            STD_CMP_PROPERTIES[PROP_AUTHOR].sName = "author";
            STD_CMP_PROPERTIES[PROP_AUTHOR].sType = TTypedValue.STYPE_STR;
            STD_CMP_PROPERTIES[PROP_AUTHOR].sDescr = "";
            STD_CMP_PROPERTIES[PROP_AUTHOR].sDescription = "The component author or maintainer";
            STD_CMP_PROPERTIES[PROP_ACTIVE].sName = "active";
            STD_CMP_PROPERTIES[PROP_ACTIVE].sType = TTypedValue.STYPE_STR;
            STD_CMP_PROPERTIES[PROP_ACTIVE].sDescr = "";
            STD_CMP_PROPERTIES[PROP_ACTIVE].sDescription = "";
            STD_CMP_PROPERTIES[PROP_STATE].sName = "state";
            STD_CMP_PROPERTIES[PROP_STATE].sType = TTypedValue.STYPE_STR;
            STD_CMP_PROPERTIES[PROP_STATE].sDescr = "";
            STD_CMP_PROPERTIES[PROP_STATE].sDescription = "";

            DEF_CMP_PROPERTIES[PROP_PUBEVENT - NO_STD_PROPERTIES].sName = "published_events";      //[0]
            DEF_CMP_PROPERTIES[PROP_PUBEVENT - NO_STD_PROPERTIES].sType = TypeSpec.TYPECONNECTED;
            DEF_CMP_PROPERTIES[PROP_PUBEVENT - NO_STD_PROPERTIES].sDescr = "";      
            DEF_CMP_PROPERTIES[PROP_PUBEVENT - NO_STD_PROPERTIES].sDescription = "";      
            DEF_CMP_PROPERTIES[PROP_SUBEVENT - NO_STD_PROPERTIES].sName = "subscribed_events";
            DEF_CMP_PROPERTIES[PROP_SUBEVENT - NO_STD_PROPERTIES].sType = TypeSpec.TYPESUBEVENTS;
            DEF_CMP_PROPERTIES[PROP_SUBEVENT - NO_STD_PROPERTIES].sDescr = "";
            DEF_CMP_PROPERTIES[PROP_SUBEVENT - NO_STD_PROPERTIES].sDescription = "";
            DEF_CMP_PROPERTIES[PROP_DRVCONN - NO_STD_PROPERTIES].sName = "driver_connections";
            DEF_CMP_PROPERTIES[PROP_DRVCONN - NO_STD_PROPERTIES].sType = TypeSpec.TYPECONNECTED;
            DEF_CMP_PROPERTIES[PROP_DRVCONN - NO_STD_PROPERTIES].sDescr = "";
            DEF_CMP_PROPERTIES[PROP_DRVCONN - NO_STD_PROPERTIES].sDescription = "";

            int i;
            FGuardProperties = false;
            for (i = 0; i < NO_STD_PROPERTIES; i++)
                addProperty(STD_CMP_PROPERTIES[i].sName, i, true, false, false, "", false, STD_CMP_PROPERTIES[i].sType, 
                            STD_CMP_PROPERTIES[i].sDescr, STD_CMP_PROPERTIES[i].sDescription);
            FGuardProperties = true;
            for (i = 0; i < NO_DEF_PROPERTIES; i++)
                addProperty(DEF_CMP_PROPERTIES[i].sName, NO_STD_PROPERTIES + i, true, false, true, "", false, DEF_CMP_PROPERTIES[i].sType, 
                            DEF_CMP_PROPERTIES[i].sDescr, DEF_CMP_PROPERTIES[i].sDescription);

        }
        //============================================================================
        /// <summary>
        /// Generate a "deregister" message for the item and send it.
        /// </summary>
        /// <param name="lKind">Kind of item to deregister. <seealso cref="TypeSpec.KIND_DRIVER">See TypeSpec.KIND_DRIVER</seealso></param>
        /// <param name="lCodeID">Local ID of the item.</param>
        /// <seealso cref="sendRegistration">See sendRegistration()</seealso>
        //============================================================================
        protected abstract void sendDeregistration(int lKind, int lCodeID);
        //============================================================================
        /// <summary>
        /// Sends a <see cref="Msgs.MSG_REGISTER">MSG_REGISTER</see> message that will register
        /// an item in the simulation.
        /// </summary>
        /// <param name="sName">Name of the item.</param>
        /// <param name="sDDML">DDML type of the item.</param>
        /// <param name="lCodeID">Local ID of the item.</param>
        /// <param name="lKind">Kind of item to register.</param>
        /// <param name="destID">Send this message to this component.</param>
        /// <param name="readable">Readable flag.</param>
        /// <param name="writeable">Writeable flag.</param>
        /// <param name="toAck">Acknowledge this message 1=true.</param>
        //============================================================================
        protected abstract void sendRegistration(String sName, String sDDML, int lCodeID,
                                                 int lKind, uint destID, bool readable, bool writeable, int toAck);

        //============================================================================
        /// <summary>
        /// Send an error msg
        /// </summary>
        /// <param name="sMessage">Error message string.</param>
        /// <param name="bFatal">True if this is a fatal error.</param>
        //============================================================================
        public abstract void sendError(String sMessage, bool bFatal);

        //============================================================================
        /// <summary>
        /// Add a property to the local property list. It is then registered. The
        /// child object should use this function to add properties.
        /// </summary>
        /// <param name="sName">Property name.</param>
        /// <param name="propertyID">Local ID of the property.</param>
        /// <param name="bRead">Is readable.</param>
        /// <param name="bWrite">Is writeable.</param>
        /// <param name="bInit">An initial value variable.</param>
        /// <param name="sUnit">Units name.</param>
        /// <param name="bIsArray">Is an array.</param>
        /// <param name="sType">The type. Can be DDML or the type name e.g. TTypedValue.STYPE_STR</param>
        /// <param name="sShortDescr">Short description text</param>
        /// <param name="sFullDescr">Full description text</param>
        /// <returns>The new TInitValue</returns>
        // A.D.Moore Jun 2001
        //============================================================================
        protected TInitValue addProperty(String sName, int propertyID, bool bRead, bool bWrite,
                                         bool bInit, String sUnit, bool bIsArray, String sType, String sShortDescr, String sFullDescr)
        {
            String sDDMLType = "";

            MakeDDML(sName, sType, bIsArray, sUnit, ref sDDMLType);
            return addProperty(sName, propertyID, bRead, bWrite, bInit, sDDMLType, sShortDescr, sFullDescr);    
        }
        //============================================================================
        /// <summary>
        /// Add a property to the local property list. It is then registered. The
        /// child object should use this function to add properties.
        /// </summary>
        /// <param name="sName">Property name.</param>
        /// <param name="propertyID">Local ID of the property.</param>
        /// <param name="bRead">Is readable.</param>
        /// <param name="bWrite">Is writeable.</param>
        /// <param name="bInit">An initial value variable.</param>
        /// <param name="sDDMLType">The DDML type fully specified.</param>
        /// <param name="sShortDescr">Short description text</param>
        /// <param name="sFullDescr">Full description text</param>
        /// <returns>The new TInitValue</returns>
        //============================================================================
        public TInitValue addProperty(String sName, int propertyID, bool bRead, bool bWrite,
                                         bool bInit, String sDDMLType, String sShortDescr, String sFullDescr)
        {
            TPropertyInfo newProperty = null;
            uint destID;     //must be zero
            String errorMsg;

            try
            {
                destID = 0;           //properties have no source component connection
                if ((FGuardProperties) && (propertyID < NO_STD_PROPERTIES))
                {
                    errorMsg = String.Format("{0}: Cannot override standard property in addproperty().", FName);
                    throw (new ApplicationException(errorMsg));
                }

                newProperty = new TPropertyInfo(sDDMLType, sShortDescr, sFullDescr, "");
                newProperty.Name = sName;
                newProperty.bRead = bRead;
                newProperty.bWrite = bWrite;
                newProperty.bInit = bInit;

                while (propertyList.Count <= propertyID)         //We are going to index directly into
                    propertyList.Add(null);                       //propertyList later

                if (propertyList[propertyID] != null)
                {
                    if (bRegisterNow)
                        sendDeregistration(TypeSpec.KIND_OWNED, propertyID);
                    propertyList[propertyID] = null;
                }

                propertyList[propertyID] = newProperty;

                if (bRegisterNow)
                    sendRegistration(sName, sDDMLType, propertyID, TypeSpec.KIND_OWNED, destID, bRead, bWrite, 0);
            }
            catch (Exception excep)
            {
                errorMsg = String.Format("addProperty(): {0}.", excep.Message);
                sendError(errorMsg, true);
            }

            return newProperty;
        }
        //============================================================================
        /// <summary>
        /// Register all of the component's properties.
        /// </summary>
        //============================================================================
        protected void registerProperties()
        {
            int i;

            for (i = 0; i < propertyList.Count; i++)
            {
                if (propertyList[i] != null)
                {
                    TPropertyInfo propInfo = propertyList[i];
                    //if the property is not readable or writeable then don't register it
                    if ((propInfo.bRead) || (propInfo.bWrite))
                    {
                        //register with 0 destination
                        sendRegistration(propInfo.Name, propInfo.sDDML, i, TypeSpec.KIND_OWNED, 0, propInfo.bRead, propInfo.bWrite, 0);
                    }
                }
            }
        }
        //============================================================================
        /// <summary>
        /// Returns value data to the simulation system for a nominated owned
        /// property, by:
        /// <ol>
        ///  <li>locating a previously constructed property from the property list</li>
        ///  <li>calling setValue() to populate it</li>
        ///  <li>passing a message back to the wrapper dll</li>
        /// </ol>
        /// </summary>
        /// <param name="propertyID">Local ID of the property.</param>
        /// <param name="requestedByID">ID of the component requesting this property.</param>
        /// <param name="queryMsgID">ID of the query message.</param>
        /// <param name="replyTo">Component to reply to.</param>
        //============================================================================
        public virtual void doQueryProperty(uint propertyID, uint requestedByID, uint queryMsgID, uint replyTo)
        {
            try
            {
                if ((int)propertyID == propertyNameToID("state"))
                {
                    //systems will not send a replyValue immediately so I don't want to go into readFromPropertyList()
                    getStateProperty(requestedByID, queryMsgID, replyTo);
                }
                else if (propertyID < propertyList.Count)
                {
                    TPropertyInfo aValue = propertyList[(int)propertyID];                     //Find a TTypedValue to work with
                    //read any standard or default property and any user defined property
                    readFromPropertyList((int)propertyID, requestedByID, ref aValue);

                    int valSize = (int)aValue.sizeBytes();
                    byte[] valueData = new byte[valSize];
                    aValue.getData(ref valueData);                         //makes a copy into a block that has been allocated

                    //send a replyValue message to the router that sent it here
                    sendReplyValue(queryMsgID, replyTo, aValue.sDDML, valueData, valSize);
                }
            }
            catch (Exception e)
            {
                string errorMsg = string.Format("doQueryProperty(): {0}", e.Message);
                sendError(errorMsg, true);
            }
        }
        //============================================================================
        /// <summary>
        /// Get the value of the 'state' property. Used during checkpointing and QueryValue handling.                               
        /// Read the standard properties. If the propertyID is a user property, readProperty() is called.
        /// </summary>
        /// <param name="requestedByID">The requesting component.</param>
        /// <param name="queryMsgID">ID of the query message.</param>
        /// <param name="replyTo">Reply to this component.</param>
        //============================================================================  
        protected virtual void getStateProperty(uint requestedByID, uint queryMsgID, uint replyTo)
        {
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
        protected abstract void sendReplyValue(uint queryMsgID, uint replyTo, string sParamType, byte[] aParams, int paramsSize);

        //============================================================================
        /// <summary>
        /// Empties the property list excluding the standard properties.
        /// </summary>
        /// <param name="removeDefaults">Keeps the std and default properties if false.</param>
        /// <param name="all">Clear all properties.</param>
        //============================================================================
        public void propertyListClear(bool removeDefaults, bool all)
        {
            uint remain = NO_STD_PROPERTIES;

            if (all)
            {
                remain = 0;
            }
            else
            {
                if (!removeDefaults)
                    remain = NO_STD_PROPERTIES + NO_DEF_PROPERTIES; //keep the defaults
            }

            while (propertyList.Count > remain)
            {
                propertyList.RemoveAt(propertyList.Count - 1);
            }
        }
        //============================================================================
        /// <summary>
        /// Converts the type-definition data for properties etc into proper DDML.
        /// </summary>
        /// <param name="sName">Name of the DDML type.</param>
        /// <param name="sType">The type. Can be DDML or the type name e.g. TTypedValue.STYPE_STR</param>
        /// <param name="bIsArray">Is an array.</param>
        /// <param name="sUnit">Unit string.</param>
        /// <param name="ddml">Buffer to hold the return value.</param>
        /// <returns>The DDML string.</returns>
        //============================================================================
        protected String MakeDDML(String sName, String sType, bool bIsArray, string sUnit, ref string ddml)
        {
            StringBuilder newDDML = new StringBuilder();

            if (sType.Length < 1)
            {                              // The null string is used for "no
                newDDML.Append(TypeSpec.TYPEEMPTY);  //   parameters" in events
            }
            else
                if (sType.Substring(0, TypeSpec.TYPEDDML.Length) == TypeSpec.TYPEDDML)
                {            // Fully-defined types
                    /* #ifdef ALLOW_AUTO_ARRAYS
                       if (bIsArray && strlen(szType) > 7 && !strcmp(szType + strlen(szType) - 7 , "</type>")) {
                          ddml = "<type array=\"T\"><element";
                          ddml.append(szType + 5, strlen(szType) - 12);
                          ddml += "</element></type>";
                       }
                       else
        #endif */

                    newDDML.Append(sType);
                    // Remove multiple spaces
                    while (newDDML.ToString().Contains("  "))
                      newDDML.Replace("  ", " ");
                }
                else
                {                                                // Construct the DDML text for scalars
                    newDDML.Append("<type name=\"");
                    newDDML.Append(sName);
                    newDDML.Append("\" kind=\"");
                    newDDML.Append(sType);
                    newDDML.Append("\"");
                    if (bIsArray)
                        newDDML.Append(" array=\"T\"");
                    if (sUnit != "-")
                    {
                        newDDML.Append(" unit=\"");
                        newDDML.Append(sUnit);
                        newDDML.Append("\"");
                    }
                    newDDML.Append("/>");
                }

            ddml = newDDML.ToString();
            return ddml;   //returns the new ddml string
        }

        //============================================================================
        /// <summary>
        /// Removes a property from the local property list. It is then deregistered. The
        /// child object should use this function to delete properties.
        /// </summary>
        /// <param name="propertyID">Local ID of the property.</param>
        //============================================================================
        protected void deleteProperty(int propertyID)
        {
            if (propertyID < propertyList.Count)
            {
                if (propertyList[propertyID] != null)
                {
                    if (bRegisterNow)
                        sendDeregistration(TypeSpec.KIND_OWNED, propertyID);
                    propertyList[propertyID] = null;
                }
            }
            // Now trim the list, if possible
            int i = propertyList.Count;
            while (i > 0 && propertyList[i - 1] == null)
            {
                propertyList.RemoveAt(i - 1);
                i--;
            }
        }
        //============================================================================
        /// <summary>
        /// Removes an event from the local event list and registers it. The child object
        /// should call this function to delete events.
        /// </summary>
        /// <param name="eventID">Local ID of the event.</param>
        //============================================================================
        protected void deleteEvent(int eventID)
        {
            if (eventID < eventList.Count)
            {
                TEventInfo anEvent = (TEventInfo)eventList[eventID];
                if (anEvent != null)
                {
                    if (bRegisterNow)
                        sendDeregistration(anEvent.iKind, eventID);
                    eventList[eventID] = null;
                }
            }
            // Now trim the list, if possible
            int i = eventList.Count;
            while (i > 0 && eventList[i - 1] == null)
            {
                eventList.RemoveAt(i - 1);
                i--;
            }
        }
        //============================================================================
        /// <summary>
        /// Finds the integer ID corresponding to a local property name. Case
        /// insensitive search.
        /// </summary>
        /// <param name="sName">Name of the property.</param>
        /// <returns>Returns -1 if not found.</returns>
        //============================================================================
        protected int propertyNameToID(String sName)
        {
            TPropertyInfo info;
            int foundAt = -1; //not found
            int i = 0;

            while ((foundAt < 0) && (i < propertyList.Count))
            {
                info = propertyList[i];
                if ((info != null) && (info.Name.ToLower() == sName.ToLower()))
                    foundAt = i;
                else
                    i++;
            }

            return foundAt;
        }
        //============================================================================
        /// <summary>
        /// Adds a driving property to the local list of drivers. The child object
        /// should use this function to add driving variables. Registration is
        /// performed here.
        /// </summary>
        /// <param name="sName">Name of the driving property.</param>
        /// <param name="driverID">Local ID of the driving property.</param>
        /// <param name="iMinConn">Minimum connections permitted.</param>
        /// <param name="iMaxConn">Maximum connections permitted.</param>
        /// <param name="sUnit">Unit name.</param>
        /// <param name="bIsArray">True if this is an array.</param>
        /// <param name="sType">The type. Can be DDML or the type name e.g. TTypedValue.STYPE_STR</param>
        /// <param name="sShortDescr">Short description text</param>
        /// <param name="sFullDescr">Full description text</param>
        /// <param name="sourceID">Component ID of the source value.</param>
        //============================================================================
        protected void addDriver(string sName, int driverID, int iMinConn, int iMaxConn,
                                 string sUnit, bool bIsArray, string sType, String sShortDescr, String sFullDescr, uint sourceID)
        {
            TDriverInfo newDriver;

            try
            {
                newDriver = createDriver(sName, driverID, iMinConn, iMaxConn, sUnit, bIsArray, sType, sShortDescr, sFullDescr, sourceID);

                while (driverList.Count <= driverID)                                      //We are going to index directly into
                    driverList.Add(null);                                                   //driverList later

                if (driverList[driverID] != null)
                {
                    if (bRegisterNow)
                        sendDeregistration(TypeSpec.KIND_DRIVER, driverID);
                    driverList[driverID] = null;
                }
                driverList[driverID] = newDriver;

                if (bRegisterNow)
                    sendRegistration(sName, newDriver.sDDML, driverID, TypeSpec.KIND_DRIVER, newDriver.iSourceID, false, false, 0);
            }
            catch (Exception e)
            {
                string errorMsg = string.Format("addDriver(): {0}", e.Message);
                sendError(errorMsg, true);
            }

        }
        //============================================================================
        /// <summary>
        /// Gets the state text for the component.
        /// </summary>
        /// <returns>The string describing the component state.</returns>
        //============================================================================
        protected string getStateText()
        {
            TSDMLValue sdmlWriter;
            string CR = "\r\n";

            StringBuilder sBuf = new StringBuilder("<executable name=\"");
            sBuf.Append(FModulePathName);
            sBuf.Append("\" version=\"1.0\"/>\r\n");
            sBuf.Append("<initdata>\r\n");
            sBuf.Append("<![CDATA[\r\n");
            sBuf.Append("<initsection>\r\n");

            //write the properties
            sdmlWriter = new TSDMLValue("<type/>", "");

            //list out the properties
            for (int i = 0; i < propertyList.Count; i++)
            {
                TPropertyInfo property = propertyList[i];
                if (property != null)
                {
                    if ((property.bInit) /* && (property->bRead) */ )
                    { //get the SDML description of this property
                        readFromPropertyList(i, 0, ref property);     //store the value of this property in the propInfo
                        sBuf.Append(sdmlWriter.getText(property, 2, 2));
                        sBuf.Append(CR);
                    }
                }
            }
            sBuf.Append("</initsection>\r\n");
            sBuf.Append("]]>\r\n");
            sBuf.Append("</initdata>\r\n)");

            return sBuf.ToString();
        }
        //============================================================================
        /// <summary>
        /// Read the standard properties. If the propertyID is a user property, readProperty() is called.
        /// </summary>
        /// <param name="propertyID">The property ID to find.</param>
        /// <param name="requestorID">The requesting component.</param>
        /// <param name="aValue">Ref to the property.</param>
        //============================================================================  
        public virtual void readFromPropertyList(int propertyID, uint requestorID, ref TPropertyInfo aValue)
        {
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
        public abstract void readProperty(int propertyID, uint requestorID, ref TPropertyInfo aValue);
        //============================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="sName">Name of the driving property.</param>
        /// <param name="driverID">Local ID of the driving property.</param>
        /// <param name="iMinConn">Minimum connections permitted.</param>
        /// <param name="iMaxConn">Maximum connections permitted.</param>
        /// <param name="sUnit">Unit name.</param>
        /// <param name="bIsArray">True if this is an array.</param>
        /// <param name="sType">The type. Can be DDML or the type name e.g. TTypedValue.STYPE_STR</param>
        /// <param name="sShortDescr">Short description text</param>
        /// <param name="sFullDescr">Full description text</param>
        /// <param name="sourceID">Component ID of the source value.</param>
        /// <returns>The new driver</returns>
        //============================================================================
        internal TDriverInfo createDriver(string sName, int driverID, int iMinConn, int iMaxConn,
                                          string sUnit, bool bIsArray, string sType, 
                                          String sShortDescr, String sFullDescr, uint sourceID)
        {
            TDriverInfo newDriver;
            string sDDMLType = "";

            MakeDDML(sName, sType, bIsArray, sUnit, ref sDDMLType);

            newDriver = new TDriverInfo(sDDMLType, sShortDescr, sFullDescr, "");
            newDriver.Name = sName;
            newDriver.iMinConn = iMinConn;
            newDriver.iMaxConn = iMaxConn;
            newDriver.iSourceID = sourceID;
            newDriver.bRequestActive = false;

            return newDriver;
        }
        //============================================================================
        /// <summary>
        /// Removes a driving property from the local list of drivers. The child object
        /// should use this function to delete driving variables. Deregistration is
        /// performed here.
        /// </summary>
        /// <param name="driverID">Local ID of the driver.</param>
        //============================================================================
        protected void deleteDriver(int driverID)
        {
            if (driverID < driverList.Count)
            {
                if (driverList[driverID] != null)
                {
                    if (bRegisterNow)
                        sendDeregistration(TypeSpec.KIND_DRIVER, driverID);
                    driverList[driverID] = null;
                }
                // Now trim the list, if possible
                int i = driverList.Count;
                while (i > 0 && driverList[i - 1] == null)
                {
                    driverList.RemoveAt(i - 1);
                    i--;
                }
            }
        }
        //============================================================================
        /// <summary>
        /// Gets the component description.
        /// </summary>
        /// <returns>The string of the component description comforming to the CMP specification.</returns>
        //============================================================================
        public virtual string description(string context)
        {
            StringBuilder sBuf = new StringBuilder("");
            //construct the description
            sBuf.Append("<describecomp>");
            sBuf.Append("<executable>");
            sBuf.Append(FModulePathName);
            sBuf.Append("</executable>");
            sBuf.Append("<class>");
            sBuf.Append(FType);
            sBuf.Append("</class>");
            sBuf.Append("<version>");
            sBuf.Append(FVersion);
            sBuf.Append("</version>");
            sBuf.Append("<author>");
            sBuf.Append(FAuthor);
            sBuf.Append("</author>");
            if (FSystem)
                sBuf.Append("<system/>");

            //write the properties
            TPropertyInfo property;
            int it = 0;
            while (it < propertyList.Count)
            {
                property = propertyList[it];
                if (property != null)
                {
                    sBuf.Append("<property name=\"");
                    sBuf.Append(property.Name);
                    sBuf.Append("\" descr=\"");
                    sBuf.Append(property.sDescr);
                    sBuf.Append("\" access=\"");
                    if ((property.bWrite) && (property.bRead))
                    {
                        sBuf.Append("both");
                    }
                    else
                    {
                        if (property.bWrite)
                            sBuf.Append("write");
                        else if (property.bRead)
                            sBuf.Append("read");          //default is "read"
                    }
                    sBuf.Append("\" init=\"");
                    if (property.bInit)
                        sBuf.Append("T\">");
                    else
                        sBuf.Append("F\">");

                    //now write the type info
                    sBuf.Append(writeTypeInfo(property));
                    if (property.sDescription.Length > 0)
                        sBuf.Append("<description>" + property.sDescription + "</description>");
                    else
                        sBuf.Append("<description/>");
                    sBuf.Append("</property>");
                }
                it++;
            }

            //write the list of driving variables
            TDriverInfo driver;
            int itd = 0;
            while (itd < driverList.Count)
            {
                driver = driverList[itd];
                if (driver != null)
                {
                    sBuf.Append("<driver name=\"");
                    sBuf.Append(driver.Name);
                    sBuf.Append("\" minsrc=\"");
                    sBuf.Append(driver.iMinConn);
                    sBuf.Append("\" maxsrc=\"");
                    sBuf.Append(driver.iMaxConn);
                    sBuf.Append("\">");
                    //now write the type info
                    sBuf.Append(writeTypeInfo(driver));
                    if (driver.sDescription.Length > 0)
                        sBuf.Append("<description>" + driver.sDescription + "</description>");
                    else
                        sBuf.Append("<description/>");
                    sBuf.Append("</driver>");
                }
                itd++;
            }

            //now for the events
            TEventInfo anEvent;
            int ite = 0;
            while (ite < eventList.Count)
            {
                anEvent = (TEventInfo)eventList[ite];
                if (anEvent != null)
                {
                    sBuf.Append("<event name=\"");
                    sBuf.Append(anEvent.Name);
                    sBuf.Append("\" descr=\"");
                    sBuf.Append(anEvent.sDescr);
                    sBuf.Append("\" kind=\"");
                    if (anEvent.iKind == TypeSpec.KIND_SUBSCRIBEDEVENT)
                        sBuf.Append("subscribed\">");
                    else
                        sBuf.Append("published\">");        //default is "published"
                    if (anEvent.isScalar())
                    {
                        sBuf.Append("<field name=\"");
                        sBuf.Append(anEvent.Name);
                        sBuf.Append("\" descr=\"\"");
                        sBuf.Append(writeFieldInfo(anEvent));  //recurse into the fields
                        sBuf.Append("</field>");
                    }
                    else
                    {
                        for (int i = 1; i <= anEvent.count(); i++)
                        {
                            sBuf.Append("<field name=\"");
                            sBuf.Append(anEvent.member((uint)i).Name);
                            sBuf.Append("\" descr=\"\"");
                            sBuf.Append(writeFieldInfo((TDDMLValue)anEvent.member((uint)i)));
                            sBuf.Append("</field>");
                        }
                    }
                    if (anEvent.sDescription.Length > 0)
                        sBuf.Append("<description>" + anEvent.sDescription + "</description>");
                    else
                        sBuf.Append("<description/>");
                    sBuf.Append("</event>");
                }
                ite++;
            }
            sBuf.Append("</describecomp>");

            return sBuf.ToString();
        }
        //============================================================================
        /// <summary>
        /// Formats the type information for this DDMLValue into xml format for the
        /// component description text.
        /// </summary>
        /// <param name="attr">The property/driver.</param>
        /// <returns>The string buffer containing the type details in xml text.</returns>
        //============================================================================
        protected string writeTypeInfo(TTypedValue attr)
        {
            string sBuf = "<type" + writeFieldInfo(attr) + "</type>";
            return sBuf;
        }
        //============================================================================
        /// <summary>
        /// Formats the xml text for the &lt;field&gt; or &lt;element&gt; description of the typed   
        /// value stored as a TPropertyInfo or TDriverInfo.
        /// </summary>
        /// <param name="attr">The property/driver.</param>
        /// <returns>The string buffer containing the type details in xml text.</returns>
        //============================================================================
        protected string writeFieldInfo(TTypedValue attr)
        {
            string elementType;
            StringBuilder sBuf = new StringBuilder("");

            sBuf.Append(" kind=\"");
            sBuf.Append(attr.typeName());
            sBuf.Append("\"");
            if (attr.isArray())
                sBuf.Append(" array=\"T\" count=\"" + attr.count().ToString() + "\"");

            if ((attr.units().Length > 0) && (attr.units()[0] != '-'))
            {
                sBuf.Append(" unit=\"");
                sBuf.Append(attr.units());
                sBuf.Append("\"");
            }

            sBuf.Append(">");

            //now nest into the fields/elements
            if (!attr.isScalar())
            {
                uint iFirst = 1;
                // Special handling for non-scalar arrays of length 0, to ensure type definition is output
                if (attr.isArray() && attr.count() == 0 && attr.baseType() == TTypedValue.TBaseType.ITYPE_DEF)
                    iFirst = 0;
                for (uint i = iFirst; i <= attr.count(); i++)
                {   //for each child
                    if (attr.isArray())
                    {
                        elementType = "element";
                        sBuf.Append("<");
                        sBuf.Append(elementType);
                        sBuf.Append(writeFieldInfo(attr.member(i)));
                        sBuf.Append("</" + elementType + ">");
                    }
                    else if (attr.isRecord())
                    {
                        elementType = "field";
                        sBuf.Append("<");
                        sBuf.Append(elementType);
                        sBuf.Append(" name=\"");
                        sBuf.Append((attr.member(i)).Name);
                        sBuf.Append("\" descr=\"\"");
                        sBuf.Append(writeFieldInfo(attr.member(i)));
                        sBuf.Append("</" + elementType + ">");
                    }
                }
            }

            //if a scalar or an array of scalars then write the defval|minval|maxval elements
            if ((attr.isScalar()) || (attr.isArray() && (attr.baseType() != TTypedValue.TBaseType.ITYPE_DEF)) ) {
                TInitValue attribute = attr as TInitValue;
                if (attribute != null)
                {
                    if (attribute.getDefault() != null)
                    {
                        sBuf.Append("<defval>" + attribute.getDefault().asStr() + "</defval>");
                    }
                    if (attribute.getMax() != null)
                    {
                        if (attribute.getMax().asStr().Length != 0)
                        {
                            sBuf.Append("<maxval>" + attribute.getMax().asStr() + "</maxval>");
                        }
                    }
                    if (attribute.getMin() != null)
                    {
                        if (attribute.getMin().asStr().Length != 0)
                        {
                            sBuf.Append("<minval>" + attribute.getMin().asStr() + "</minval>");
                        }
                     }
                }
            }

            return sBuf.ToString();
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
        public virtual int processEventState(int eventID, int iState, uint publisherID, TTypedValue aParams)
        {
            return 0;
        }
        //============================================================================
        /// <summary>
        /// Send an acknowledgement message
        /// </summary>
        /// <param name="msgTo">Component ID to which complete is sent.</param>
        /// <param name="msgID">ID of the message being acknowledged.</param>
        //============================================================================
        public virtual void sendComplete(uint msgTo, uint msgID)
        {

        }

        //============================================================================
        /// <summary>
        /// A bit ugly, but some of the Fortran components (like the manager) still
        /// generate events with the associated data in the form of an Apsim variant.
        /// This routine attempts to repackage that data into a more acceptable form.
        /// </summary>
        /// <param name="dest">The TEventInfo object associated with the event.</param>
        /// <param name="publBy">The originator component ID of the published event.</param>
        /// <param name="prmDDML">DDML description of this event.</param>
        /// <param name="prmData">Parameter data values.</param>
        /// <param name="prmSize">Size of parameter data block.</param>
        //============================================================================
        internal bool ConvertApsimVariant(TEventInfo dest, uint publBy, string prmDDML, ref byte[] prmData, ref uint prmSize)
        {
            TDDMLValue src = new TDDMLValue(prmDDML, "");
            src.setData(prmData, (int)prmSize, 0);
            for (uint idx = 1; idx < src.count(); idx += 5)
            {
                int field = 1 + (int)(idx / 5);
                string fieldName = src.item(idx).Name.ToLower();
                if (fieldName != "param" + field.ToString() + "_name")
                    return false;
                string name = src.item(idx).asString().ToLower();
                TTypedValue destField = dest.member(name);
                if (destField == null)
                    continue; // Perhaps this should raise an exception?
                TTypedValue value = null;
                if (src.item(idx + 4).count() > 0)
                {
                    if (destField.isArray())
                        value = src.item(idx + 4);
                    else
                        value = src.item(idx + 4).item(1);
                }
                if (name == "sender" || name == "sender_id")
                    continue;
                destField.setValue(value);
            }
            prmSize = dest.sizeBytes();
            prmData = new byte[prmSize];
            dest.getData(ref prmData);                 //makes a copy into a block that has been allocated
            dest.addSourceValue(publBy, dest);
            return true;
        }

    }
}
