using System;
using System.Collections.Generic;
using System.Text;

namespace CMPServices
{
    //=========================================================================
    /// <summary>
    /// 
    /// </summary>
    public enum TConnectType {
        /// <summary>
        /// Driving property
        /// </summary>
        ctDriver,
        /// <summary>
        /// Event
        /// </summary>
        ctEvent,
        /// <summary>
        /// Request set
        /// </summary>
        ctReqSet
    };
    //=========================================================================
    /// <summary>
    /// Base class for the various connectable entities
    /// </summary>
    //=========================================================================
    public class TConnectEntity
    {
        /// <summary>
        /// ID of owning component
        /// </summary>
        public uint compID;     
        /// <summary>
        /// FQN of the owner
        /// </summary>
        public string ownerFQN; 
        /// <summary>
        /// property or event name; whether fully-qualified depends on context
        /// </summary>
        public string name;
        /// <summary>
        ///
        /// </summary>
        public string uqname;
        /// <summary>
        /// property or event registration ID
        /// </summary>
        public uint regID;      
        /// <summary>
        /// DDML type (including unit)
        /// </summary>
        public string sType;    
        /// <summary>
        /// Constructor
        /// </summary>
        public TConnectEntity()
        {
            sType = "";
        }
    }
    //=========================================================================
    /// <summary>
    /// 
    /// </summary>
    //=========================================================================
    internal class TLocProperty : TConnectEntity
    {
        public bool readable;   //whether the property is readable
        public bool writable;   //whether the property is writeable
        public bool chkpoint;   //whether the property is included in checkpoints
    }
    //=========================================================================
    /// <summary>
    /// Subscribed event class. 
    /// </summary>
    //=========================================================================
    public class TSubEvent : TConnectEntity
    {
    }
    //=========================================================================
    /// <summary>
    /// Driving property.
    /// </summary>
    //=========================================================================
    public class TDrvProperty : TConnectEntity
    {
        /// <summary>
        /// optional source component ID
        /// </summary>
        public uint destID;     
        /// <summary>
        /// user-defined or automatic connection
        /// </summary>
        public bool autoConn;   
        /// <summary>
        /// Connections: list of TIDSpec* denoting (components,properties)
        /// that are sources for the property's value(s) (item type = TIDSpec)
        /// </summary>
        public TEntityList connects;    
             
        /// <summary>
        /// Driving property constructor
        /// </summary>
        public TDrvProperty() {
            autoConn = true;
        }
    }
    //=========================================================================
    /// <summary>
    /// Published event description.
    /// </summary>
    //=========================================================================
    public class TPubEvent : TConnectEntity
    {
        /// <summary>
        /// Optional component ID destination for this event
        /// </summary>
        public uint destID;             
        /// <summary>
        /// User-defined or automatic connection
        /// </summary>
        public bool autoConn;           
        /// <summary>
        /// Connections: list of TIDSpec denoting (components,events)
        /// that have subscribed to the event
        /// </summary>
        public TEntityList connects;    
             
        /// <summary>
        /// Constructor. autoConn defaults to true.
        /// </summary>
        public TPubEvent() {
            autoConn = true;
        }
    }
    //=========================================================================
    /// <summary>
    /// A Setter property description.
    /// </summary>
    //=========================================================================
    public class TReqSetProperty : TConnectEntity
    {
        /// <summary>
        /// ID of target component.
        /// </summary>
        public uint destID;             
        /// <summary>
        /// Connections: list of TIDSpec denoting (components,properties)
        /// that are possible targets for the setter's value(s)
        /// </summary>
        public TEntityList connects;    
                                        
        /// <summary>
        /// Default constructor.
        /// </summary>
        public TReqSetProperty()
        { 
        }
    }
    //=========================================================================
    /// <summary>
    /// Registrar class. Used by system components to manage the registration
    /// processes.
    /// </summary>
    //=========================================================================
    public class TRegistrar
    {
        private List<TConnectEntity> drvPropList;   //driving property list
        private List<TConnectEntity> ownedPropList; //owned property list
        private List<TConnectEntity> pubEventList;  //published event list
        private List<TConnectEntity> propSetList;   //list of property connections for requestSet msg's
        private List<TConnectEntity> subEventList;  //subscribed event list

        //                child, parent
        private Dictionary<uint, uint> compOwnerList;  //list of component owners

        //=========================================================================
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="compID">Component ID of the owner.</param>
        //=========================================================================
        public TRegistrar(uint compID)
        {
            drvPropList = new List<TConnectEntity>();
            ownedPropList = new List<TConnectEntity>();
            pubEventList = new List<TConnectEntity>();
            propSetList = new List<TConnectEntity>();
            subEventList = new List<TConnectEntity>();
            compOwnerList = new Dictionary<uint, uint>();
        }
        //=========================================================================
        /// <summary>
        /// Tests the two values to determine if the test value is as near as the
        /// nearest value.
        /// </summary>
        /// <param name="nearest">Test value for nearest.</param>
        /// <param name="test">Test value.</param>
        /// <returns>True if the test value is nearer than the nearest value.</returns>
        //=========================================================================
        static private bool asNear(uint nearest, uint test)
        {
            uint nearestUp;
            uint nearestDown;
            uint testUp;
            uint testDown;
            bool result = false;

            nearestUp = nearest / 1000;
            nearestDown = nearest % 1000;

            testUp = test / 1000;
            testDown = test % 1000;

            if ((testUp <= nearestUp) && (testDown <= nearestDown))   //rule for testing nearest
                result = true;

            return result;
        }
        //=========================================================================
        /// <summary>
        /// Calculate the distance when moving up and down the tree.
        /// a.b.c.d.g.t.u -> a.b.c.d.e.f  = 002001
        /// a.b.c.d.e.f -> a.b.a.s.d.f = 003003
        /// </summary>
        /// <param name="src">The start point of the test.</param>
        /// <param name="dest">The end point of the test.</param>
        /// <returns>An integer value representing the count of traversals up
        /// and down the tree. Where the integer digits aaabbb -> aaa=trav up, bbb trav down.</returns>
        //=========================================================================
        static private uint calcDistance(string src, string dest)
        {
            int i;
            uint h = 0;   //traversals up from src
            uint d = 0;   //traversals up from dest
            int lastdot = 0;

            //find the last dot separator at which point the strings match
            i = 0;
            while ((i < src.Length && i < dest.Length) && (src.Substring(i, 1).ToLower() == dest.Substring(i, 1).ToLower()))
            {
                if (src[i] == '.')
                    lastdot = i;
                i++;
            }

            //look down each path from this point
            i = lastdot;
            while (i < src.Length)
            {
                if (++i < src.Length && src.Substring(i, 1) == ".")
                    h++;
            }

            i = lastdot;
            while (i < dest.Length)
            {
                if (++i < dest.Length && dest.Substring(i, 1) == ".")
                    d++;
            }

            return (h * 1000 + d);
        }
        //=========================================================================
        /// <summary>
        /// Scans through all the connections for this driving variable and sets
        /// the matchesRule flag.
        /// </summary>
        /// <param name="drvProperty"></param>
        //=========================================================================
        private void recalcDrvConnections(ref TDrvProperty drvProperty)
        {
            uint nearest = 999999;
            uint guess;
            TIDSpec prop;
            string sDriverFQN;

            if (drvProperty.compID == 1)
            {
                return;  // Kludge. Don't do this for the simulation root...
            }

            if (drvProperty.ownerFQN.Length > 0)
                sDriverFQN = drvProperty.ownerFQN + "." + drvProperty.name;
            else
                sDriverFQN = drvProperty.name;

            //This rule finds the equal nearest connections

            //find the minimum distance from the drv to connection
            for (int i = 1; i <= drvProperty.connects.count(); i++) //for each connection
            {
                prop = drvProperty.connects.getEntity(i);
                guess = calcDistance(sDriverFQN, prop.name);
                if (guess > 0 && asNear(nearest, guess))    //if it is <= the minimum
                {
                    nearest = guess;                                //set the min
                }  //endif
            }
            //set all the matchesRule flags
            for (int i = 1; i <= drvProperty.connects.count(); i++) //for each connection
            {
                prop = drvProperty.connects.getEntity(i);
                guess = calcDistance(sDriverFQN, prop.name);
                if (guess > 0 && asNear(nearest, guess))    //if it is <= the minimum
                {
                    prop.matchesRule = true;                       //set matchesRule
                }
                else
                    prop.matchesRule = false;
                drvProperty.connects.storeEntity(prop, i);  //now store the updated property ref 
            }

            // Move all rule matches to the front of the list
            int lastMatchPos = 0;
            for (int i = 1; i <= drvProperty.connects.count(); i++)  //for each connection
            {
                prop = drvProperty.connects.getEntity(i);
                if (prop.matchesRule)
                    drvProperty.connects.swap(i, ++lastMatchPos);
            }
        }
        //=========================================================================
        /// <summary>
        /// Scans through all the connections for this requestSet variable and sets
        /// the matchesRule flag.
        /// </summary>
        /// <param name="setProperty"></param>
        //=========================================================================
        private void recalcReqSetConnections(ref TReqSetProperty setProperty)
        {
            // Hmm. Is this right? What about the case where a
            uint nearest = 999999;
            uint guess;
            TIDSpec prop;
            string sSetterFQN;

            if (setProperty.ownerFQN.Length > 0)
                sSetterFQN = setProperty.ownerFQN + "." + setProperty.name;
            else
                sSetterFQN = setProperty.name;

            //This rule finds the equal nearest connections

            //find the minimum distance from the drv to connection
            for (int i = 1; i <= setProperty.connects.count(); i++)   //for each connection
            {
                prop = setProperty.connects.getEntity(i);
                guess = calcDistance(sSetterFQN, prop.name);
                if (guess > 0 && asNear(nearest, guess))    //if it is <= the minimum
                {
                    nearest = guess;                                //set the min
                }  //endif
            }
            //set all the matchesRule flags
            for (int i = 1; i <= setProperty.connects.count(); i++)   //for each connection
            {
                prop = setProperty.connects.getEntity(i);
                guess = calcDistance(sSetterFQN, prop.name);
                if (guess > 0 && asNear(nearest, guess))    //if it is <= the minimum
                {
                    prop.matchesRule = true;                       //set matchesRule
                }
                else
                    prop.matchesRule = false;
                setProperty.connects.storeEntity(prop, i);  //now store the updated property ref 
            }
        }
        //=========================================================================
        /// <summary>
        /// Deregister a component.
        /// </summary>
        /// <param name="compID"></param>
        //=========================================================================
        private void deregComponent(uint compID)
        {
            removeConnectEntities(ref drvPropList, compID);
            removeConnectEntities(ref ownedPropList, compID);
            removeConnectEntities(ref pubEventList, compID);
            removeConnectEntities(ref subEventList, compID);
            removeConnectEntities(ref propSetList, compID);

            // We should probably also go through the connectLists of drvProperty, pubEvent,
            // and reqSetProperty, and clean those up as well...
        }
        //=========================================================================
        /// <summary>
        /// Removes all connectable entities (property or event) idenitified by
        /// compID from the list
        /// </summary>
        /// <param name="list"></param>
        /// <param name="compID"></param>
        //=========================================================================
        static private void removeConnectEntities(ref List<TConnectEntity> list, uint compID)
        {
            foreach (TConnectEntity entity in list)
            {
                if (entity.compID == compID)
                    list.Remove(entity);
            }
        }
        //=========================================================================
        /// <summary>
        /// Removes the connectable entity (property or event) idenitified by
        /// compID and regID from the list
        /// </summary>
        /// <param name="list"></param>
        /// <param name="compID"></param>
        /// <param name="regID"></param>
        //=========================================================================
        static protected void removeConnectEntity(ref List<TConnectEntity> list, uint compID, uint regID)
        {
            TConnectEntity entity;
            int i = 0;
            while (i < list.Count - 1)
            {
                entity = list[i];
                if ((entity.compID == compID) && (entity.regID == regID))
                {
                    list.RemoveAt(i);
                    i = list.Count; //terminate loop
                }
                i++;
            }
        }
        //=========================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="list"></param>
        //=========================================================================
        static protected void clearConnectEntitiesList(ref List<TConnectEntity> list)
        {
            list.Clear();
        }
        //=========================================================================
        /// <summary>
        /// Returns a connectable entities (property or event) from a list, if it
        /// matches compID and regID. Returns NULL if not found.
        /// </summary>
        /// <param name="list"></param>
        /// <param name="compID"></param>
        /// <param name="regID"></param>
        /// <param name="foundIdx"></param>
        /// <returns></returns>
        //=========================================================================
        static protected TConnectEntity getConnectEntity(List<TConnectEntity> list, uint compID, uint regID, ref int foundIdx)
        {
            TConnectEntity entity;
            TConnectEntity result = null;

            int i = 0;
            foundIdx = i;
            while ((result == null) && (i < list.Count))
            {
                entity = list[i];
                if ((entity.compID == compID) && (entity.regID == regID))
                {
                    foundIdx = i;
                    result = entity;
                }
                else
                    i++;
            }
            return result;
        }
        //=========================================================================
        /// <summary>
        /// Returns a connectable entities (property or event) from a list, if it
        /// matches compID and entityName (case insensitive). Returns NULL if not found.
        /// </summary>
        /// <param name="list"></param>
        /// <param name="compID"></param>
        /// <param name="entityName"></param>
        /// <param name="foundIdx"></param>
        /// <returns></returns>
        //=========================================================================
        static protected TConnectEntity getConnectEntity(List<TConnectEntity> list, uint compID, string entityName, ref int foundIdx)
        {
            TConnectEntity entity;
            TConnectEntity result = null;
            int i = 0;
            foundIdx = i;
            while ((result == null) && (i < list.Count))
            {
                entity = list[i];
                if ((entity.compID == compID) && (String.Compare(entity.uqname, entityName, true) == 0))
                {
                    result = entity;
                    foundIdx = i;
                }
                i++;
            }
            return result;
        }
        //============================================================================
        /// <summary>
        /// Extracts the last section of a FQN as the component name
        /// </summary>
        /// <param name="sFQN">Fully qualified name.</param>
        /// <returns>The unqualified name.</returns>
        //============================================================================
        public static string unQualifiedName(string sFQN)
        {
            if (sFQN.Contains("."))
            {
                int lastDot = sFQN.LastIndexOf(".");
                return sFQN.Substring(lastDot + 1);
            }
            else
                return sFQN;
        }
        //============================================================================
        /// <summary>
        /// Tests two DDML type strings to see whether they are compatible.
        /// This is used by the findEntities function to check for type compatibility
        ///
        /// Parts of this may need further checking - specifically, the check can be
        /// asymetrical for records, when one type is a superset of the other. I probably
        /// don't have this logic quite right in all cases in findEntities.
        ///
        /// Return value: 0: Not compatible
        ///               1: A compatible with B
        ///               2: B compatible with A
        ///               3: A and B compatible with each other
        /// </summary>
        /// <param name="sTypeA"></param>
        /// <param name="sTypeB"></param>
        /// <returns></returns>
        //============================================================================
        static protected int getCompatibleType(string sTypeA, string sTypeB)
        {

            // Do the check only if both strings are present; other return "compatible"

            int result = 0;
            if ( (sTypeA.Equals(null) || sTypeA.Length < 1 ||
                  sTypeB.Equals(null) || sTypeB.Length < 1) ||
                  (sTypeA == sTypeB) )
            {
                result = 3;
            }
            else
            {
                result = 0;
                TDDMLValue valueA = null;
                TDDMLValue valueB = null;
                try
                {
                    valueA = new TDDMLValue(sTypeA, TTypedValue.TBaseType.ITYPE_DEF);
                    valueB = new TDDMLValue(sTypeB, TTypedValue.TBaseType.ITYPE_DEF);
                    int check1 = valueA.canAssignFrom(valueB);
                    if (check1 == TTypedValue.ctSAME)
                    {
                        result = 3;
                    }
                    else
                    {
                        if (check1 == TTypedValue.ctCOMP)
                        {
                            result = 1;
                        }
                        if (valueB.canAssignFrom(valueA) != TTypedValue.ctBAD)
                        {
                            result |= 2;
                        }
                    }
                }
                catch (Exception excep)
                {
                    throw (new ApplicationException(excep.Message));
                }
            }
            return result;
        }
        //============================================================================
        /// <summary>
        /// Returns information about a driving property.
        /// </summary>
        /// <param name="idx">Uses index 1-x</param>
        /// <returns></returns>
        //============================================================================
        public TDrvProperty drivingProperty(int idx)
        {
            TDrvProperty result = null;

            if ((idx > 0) && (idx <= drvPropList.Count))
                result = (TDrvProperty)drvPropList[idx - 1];

            return result;
        }
        //============================================================================
        /// <summary>
        /// Returns information about a published event as a TPubEvent.
        /// </summary>
        /// <param name="idx">Uses index 1-x</param>
        /// <returns></returns>
        //============================================================================
        public TPubEvent publishedEvent(int idx)
        {
            TPubEvent result = null;

            if ((idx > 0) && (idx <= pubEventList.Count))
                result = (TPubEvent)pubEventList[idx - 1];

            return result;
        }
        //============================================================================
        /// <summary>
        /// Returns information about a subscribed event. The result contains:
        /// </summary>
        /// <param name="idx">Uses index 1-x</param>
        /// <returns></returns>
        //============================================================================
        public TSubEvent subscribedEvent(int idx)
        {
            TSubEvent result = null;

            if ((idx > 0) && (idx <= subEventList.Count))
                result = (TSubEvent)subEventList[idx - 1];

            return result;
        }
        //============================================================================
        /// <summary>
        /// Returns information about a property setting connection. Used during a
        /// requestSet msg operation.
        /// </summary>
        /// <param name="idx">Uses index 1-x</param>
        /// <returns></returns>
        //============================================================================
        public TReqSetProperty setterProperty(int idx)
        {
            TReqSetProperty result = null;

            if ((idx > 0) && (idx <= propSetList.Count))
                result = (TReqSetProperty)propSetList[idx - 1];

            return result;
        }
        //============================================================================
        /// <summary>
        /// Returns information about a property setting connection. Used during a
        /// requestSet msg operation.
        /// </summary>
        /// <param name="ownerID"></param>
        /// <param name="regID">registration ID of the property setter in the calling component.</param>
        /// <returns></returns>
        //============================================================================
        public TReqSetProperty setterPropertyByID(uint ownerID, uint regID)
        {
            int foundAt = 0;
            TReqSetProperty result = (TReqSetProperty)(getConnectEntity(propSetList, ownerID, regID, ref foundAt));
            return result;
        }
        //============================================================================
        /// <summary>
        /// Carries out registration of a property or event
        /// </summary>
        /// <param name="kind"></param>
        /// <param name="ownerFQN"></param>
        /// <param name="ownerID"></param>
        /// <param name="regID"></param>
        /// <param name="destCompID"></param>
        /// <param name="sName"></param>
        /// <param name="sType"></param>
        //============================================================================
        public void registration(int kind, string ownerFQN, uint ownerID, uint regID,
                                 uint destCompID, string sName, string sType)
        {
            TConnectEntity newEntity;

            // Create a new connectable entity of the suitable type, fill in any
            // specialised fields, and store item in the list
            switch (kind)
            {
                case TypeSpec.KIND_DRIVER:
                    {
                        TDrvProperty drvProperty = new TDrvProperty();
                        drvProperty.destID = destCompID;            //optional source
                        drvProperty.connects = new TEntityList();   //new list of connections to this driving property
                        newEntity = drvProperty;
                        storeEntityFields(ownerFQN, ownerID, regID, sName, sType, ref newEntity);   //now fill in the those fields common to all types
                        insertIntoList(drvPropList, newEntity);
                    }
                    break;
                case TypeSpec.KIND_OWNED_R:
                case TypeSpec.KIND_OWNED_W:
                case TypeSpec.KIND_OWNED_RW:
                    {
                        TLocProperty locProperty = new TLocProperty();
                        locProperty.readable = (kind == TypeSpec.KIND_OWNED_R) || (kind == TypeSpec.KIND_OWNED_RW);
                        locProperty.writable = (kind == TypeSpec.KIND_OWNED_W) || (kind == TypeSpec.KIND_OWNED_RW);
                        locProperty.chkpoint = true;
                        newEntity = locProperty;
                        storeEntityFields(ownerFQN, ownerID, regID, sName, sType, ref newEntity);   //now fill in the those fields common to all types
                        insertIntoList(ownedPropList, newEntity);
                    }
                    break;
                case TypeSpec.KIND_PUBLISHEDEVENT:
                    {
                        TPubEvent pubEvent = new TPubEvent();
                        pubEvent.destID = destCompID;   //optional destination
                        if (sType.Length < 2)
                            sType = "<type/>";
                        pubEvent.connects = new TEntityList();  //new list of connections to this driving property
                        newEntity = pubEvent;
                        storeEntityFields(ownerFQN, ownerID, regID, sName, sType, ref newEntity);   //now fill in the those fields common to all types
                        insertIntoList(pubEventList, newEntity);
                    }
                    break;
                case TypeSpec.KIND_SUBSCRIBEDEVENT:
                    {
                        TSubEvent subEvent = new TSubEvent();
                        newEntity = subEvent;
                        storeEntityFields(ownerFQN, ownerID, regID, sName, sType, ref newEntity);   //now fill in the those fields common to all types
                        insertIntoList(subEventList, newEntity);
                    }
                    break;
                case TypeSpec.KIND_REQUESTSET:
                    {
                        TReqSetProperty setProperty = new TReqSetProperty();
                        setProperty.destID = destCompID;      //optional target
                        setProperty.connects = new TEntityList();     //new list of connections to this driving property
                        newEntity = setProperty;
                        storeEntityFields(ownerFQN, ownerID, regID, sName, sType, ref newEntity);   //now fill in the those fields common to all types
                        insertIntoList(propSetList, newEntity);
                    }
                    break;
                default:
                    {
                        string errorMsg = String.Format("Invalid registration: {0} kind = {1}", sName, kind);
                        throw (new ApplicationException(errorMsg));
                    }
            }
        }

        //============================================================================
        // insertIntoList
        //
        // Insert the newItem into the entityList in order. Case insensitive
        // comparison with uqname field.
        //============================================================================
        private int insertIntoList(List<TConnectEntity> entityList, TConnectEntity newItem)
        {
            TConnectEntity entity;
            bool found = false;

            //find the insertion point
            int idx = entityList.FindIndex(
                delegate(TConnectEntity item)
                {
                    return String.Compare(item.uqname, newItem.uqname, true) > 0;   //item that will follow
                }
            );

            int i = Math.Max(idx, 0);
            while (!found && (i < entityList.Count))
            {
                entity = entityList[i];
                if (String.Compare(entity.uqname, newItem.uqname, true) > 0)
                {
                    found = true;
                }
                else
                    i++;  // Only increment if no element found yet
            }
            entityList.Insert(i, newItem);

            return 0;
        }

        //============================================================================
        /// <summary>
        /// Store common field values in a TConnectEntity object.
        /// </summary>
        /// <param name="ownerFQN">Owning component FQN</param>
        /// <param name="ownerID">Owning component ID</param>
        /// <param name="regID"></param>
        /// <param name="sName"></param>
        /// <param name="sType">DDML type</param>
        /// <param name="newEntity"></param>
        //============================================================================
        private static void storeEntityFields(string ownerFQN, uint ownerID, uint regID, string sName, string sType, ref TConnectEntity newEntity)
        {
            newEntity.name = sName.ToLower();   //always case-insensitive matching
            newEntity.uqname = unQualifiedName(newEntity.name);
            newEntity.compID = ownerID;         //owning component
            newEntity.ownerFQN = ownerFQN;
            newEntity.regID = regID;
            newEntity.sType = sType;            //DDML type
        }
        //============================================================================
        /// <summary>
        /// Carries out deregistration of a property, event or component.
        /// </summary>
        /// <param name="regoKind"></param>
        /// <param name="ownerID"></param>
        /// <param name="regID">The registration ID of the property or event. This is also the
        /// ID a component when a component is being de/registered.</param>
        //============================================================================
        public void deregistration(int regoKind, uint ownerID, uint regID)
        {
            int i, p;

            switch (regoKind)
            {                    //select the kind of deregistration
                case TypeSpec.KIND_DRIVER:
                    {
                        removeConnectEntity(ref drvPropList, ownerID, regID);
                    }
                    break;
                case TypeSpec.KIND_OWNED_R:
                case TypeSpec.KIND_OWNED_W:
                case TypeSpec.KIND_OWNED_RW:
                    {
                        removeConnectEntity(ref ownedPropList, ownerID, regID);

                        // we also need to remove any connections to this property from
                        // the driver list
                        if ((regoKind == TypeSpec.KIND_OWNED_R) || (regoKind == TypeSpec.KIND_OWNED_RW))
                        {
                            for (p = 0; p < drvPropList.Count; p++)
                            {
                                TDrvProperty drvProperty = (TDrvProperty)drvPropList[p];
                                if ((drvProperty != null) && (drvProperty.connects != null))
                                {
                                    i = 1;
                                    while (i <= drvProperty.connects.count())
                                    {
                                        TIDSpec propID = drvProperty.connects.getEntity(i);
                                        if ((propID.compID == ownerID) && (propID.itemID == regID))
                                        {
                                            drvProperty.connects.erase(i);
                                            if (drvProperty.connects.count() > 0)
                                                recalcDrvConnections(ref drvProperty);
                                            drvPropList[p] = drvProperty; //store changes
                                        }
                                        else
                                            i++;
                                    }
                                }
                            }
                        }

                        // finally, we need to remove any connections to the property from
                        // the requesetSet list
                        if ((regoKind == TypeSpec.KIND_OWNED_W) || (regoKind == TypeSpec.KIND_OWNED_RW))
                        {
                            for (p = 0; p < propSetList.Count; p++)
                            {
                                TReqSetProperty propConn = (TReqSetProperty)propSetList[p];
                                if ((propConn != null) && (propConn.connects != null))
                                {
                                    i = 1;
                                    while (i < propConn.connects.count())
                                    {
                                        TIDSpec propID = propConn.connects.getEntity(i);
                                        if ((propID.compID == ownerID) && (propID.itemID == regID))
                                        {
                                            propConn.connects.erase(i);
                                            if (propConn.connects.count() > 0)
                                                recalcReqSetConnections(ref propConn);
                                            propSetList[p] = propConn;  //store changes
                                        }
                                        else
                                            i++;
                                    }
                                }
                            }
                        }
                        break;
                    }
                case TypeSpec.KIND_PUBLISHEDEVENT:
                    {
                        removeConnectEntity(ref pubEventList, ownerID, regID);
                        break;
                    }
                case TypeSpec.KIND_SUBSCRIBEDEVENT:
                    {
                        removeConnectEntity(ref subEventList, ownerID, regID);

                        // we also need to remove any connections to this subscriber from
                        // the published event list
                        for (p = 0; p < pubEventList.Count; p++)
                        {
                            TPubEvent pubEvent = (TPubEvent)pubEventList[p];
                            if ((pubEvent != null) && (pubEvent.connects != null))
                            {
                                i = 1;
                                while (i <= pubEvent.connects.count())
                                {
                                    TIDSpec eventRef = pubEvent.connects.getEntity(i);
                                    if ((eventRef.compID == ownerID) && (eventRef.itemID == regID))
                                    {
                                        pubEvent.connects.erase(i);
                                        pubEventList[p] = pubEvent; //store changes
                                    }
                                    else
                                        i++;
                                }
                            }
                        }
                        break;
                    }
                case TypeSpec.KIND_COMPONENT:
                    {
                        //remove any items from all lists that belong to the sending component
                        //regoID is the assumed to be the component to deregister
                        deregComponent(regID);
                        break;
                    }
                case TypeSpec.KIND_SYSTEM:
                    {
                        //remove any items from all lists that belong to the sending component
                        deregComponent(regID);

                        //remove any items belonging to any children of the system being deregistered
                        //Note: They are to be sent the notifyAboutAboutToDelete so that they get deregistered
                        break;
                    }
                case TypeSpec.KIND_REQUESTSET:
                    {
                        removeConnectEntity(ref propSetList, ownerID, regID);
                        break;
                    }

                default:
                    {
                        string errorMsg = String.Format("Invalid deregistration: kind = {0}", regoKind);
                        throw (new ApplicationException(errorMsg));
                    }
            }

        }
        //============================================================================
        /// <summary>
        /// Get the property registration ID for a property name belonging to a
        /// component compID.
        /// </summary>
        /// <param name="name">Property name</param>
        /// <param name="compID">Component ID</param>
        /// <returns>Returns the registration ID.</returns>
        //============================================================================
        public uint getLocalPropertyID(string name, uint compID)
        {
            uint regID = 0;
            int foundAt = 0;
            TLocProperty locProperty = (TLocProperty)getConnectEntity(ownedPropList, compID, name, ref foundAt);
            if (!locProperty.Equals(null))
            {
                regID = locProperty.regID;
            }
            return regID;
        }
        //============================================================================
        /// <summary>
        /// Replaces connections to the driving property's list. The ID info needs
        /// to be determined at init2 stage.
        /// The driver value needs to be an unqualified name. The connection source name
        /// needs to be a FQN.
        /// If this routine is called, it is due to a manual connection. In that
        /// case, the autoconn property for this driver needs to be turned off.  
        /// Constructs the init object using xml
        /// </summary>
        /// <param name="compID">The component ID.</param>
        /// <param name="driverName">Name of the driver to search for.</param>
        /// <param name="connects">TypedValue array of (strings) connection names.</param>
        /// <returns></returns>
        //============================================================================
        public bool replacePropertyConnections(uint compID, string driverName, TTypedValue connects)
        {
            bool changed = false;
            int foundAt = 0;
            TDrvProperty drvProperty = (TDrvProperty)getConnectEntity(drvPropList, compID, driverName, ref foundAt);

            if ((drvProperty != null) && (drvProperty.connects != null) && drvProperty.autoConn)
            {
                drvProperty.connects.clear();
                //now add the connection details
                drvProperty.autoConn = false;   //this manual connection stops further resolving
                for (uint i = 1; i <= connects.count(); i++)
                {
                    if (connects.item(i).asStr().Length > 0)
                    {
                        changed = true;
                        drvProperty.connects.add(0, 0, connects.item(i).asStr(), 0, "", true);
                    }
                }
                drvPropList[foundAt] = drvProperty; //store the changed property
            }
            return changed;
        }
        //============================================================================
        /// <summary>
        /// Adds a connection to the appropriate member of the list of driving properties
        /// </summary>
        /// <param name="driverCompID"></param>
        /// <param name="driverPropertyID"></param>
        /// <param name="srcCompID"></param>
        /// <param name="srcPropertyID"></param>
        /// <param name="srcFQN"></param>
        /// <param name="srcDDML"></param>
        //============================================================================
        public void addPropertyConnection(uint driverCompID, uint driverPropertyID, uint srcCompID,
                                          uint srcPropertyID, string srcFQN, string srcDDML)
        {
            int foundAt = 0;
            TDrvProperty drvProperty = (TDrvProperty)getConnectEntity(drvPropList, driverCompID, driverPropertyID, ref foundAt);
            if ((drvProperty != null) && (drvProperty.connects != null) && drvProperty.autoConn &&
                     (getCompatibleType(drvProperty.sType, srcDDML) & 1) == 1)

            {
                drvProperty.connects.add(srcCompID, srcPropertyID, srcFQN, 0, "", true);
                //recalc the connections that match the connection rule
                if (drvProperty.connects.count() > 1)
                    recalcDrvConnections(ref drvProperty);
                drvPropList[foundAt] = drvProperty; //store changes
            }

            return;
        }
        //============================================================================
        /// <summary>
        /// Replace event connections to the published event list at compid, event name
        /// The event value needs to be a short name. The connection source name
        /// needs to be a FQN.
        /// </summary>
        /// <param name="compID"></param>
        /// <param name="eventName"></param>
        /// <param name="connects"></param>
        /// <returns></returns>
        //============================================================================
        public bool replacePubEventConnections(uint compID, string eventName, TTypedValue connects)
        {
            bool changed = false;
            int foundAt = 0;
            TPubEvent pubEvent = (TPubEvent)getConnectEntity(pubEventList, compID, eventName, ref foundAt);
            if ((pubEvent != null) && (pubEvent.connects != null) && pubEvent.autoConn)
            {
                // This is used only for manualy connections, so disable autoconnection
                pubEvent.autoConn = false;
                //now add the connection details
                TEntityList connectList = pubEvent.connects;
                connectList.clear();
                for (uint i = 1; i <= connects.count(); i++)    //for each source
                {
                    changed = true;
                    connectList.add(0, 0, connects.item(i).asStr(), 0, "", true);
                }
                pubEvent.connects = connectList;        //store changes
                pubEventList[foundAt] = pubEvent;       //store event item changes
            }
            return changed;

        }
        //============================================================================
        /// <summary>
        /// Add a event connection to the published event list at compid, eventid
        /// </summary>
        /// <param name="compID">Source of the published event</param>
        /// <param name="eventID"></param>
        /// <param name="destCompID">ID of the connect target component</param>
        /// <param name="destCompFQN">FQN of the component</param>
        /// <param name="destEventID"></param>
        /// <param name="sDDML"></param>
        //============================================================================
        public void addPubEventConnection(uint compID, uint eventID, uint destCompID, string destCompFQN, uint destEventID, string sDDML)
        {
            int foundAt = 0;
            TPubEvent pubEvent = (TPubEvent)getConnectEntity(pubEventList, compID, eventID, ref foundAt);

            //now, if it's compatible, and if the destination ID is wildcarded
            //or matches, add the connection details
            if (((pubEvent != null) && pubEvent.autoConn) &&
                ((pubEvent.destID == 0) || (pubEvent.destID == destCompID)) &&
                getCompatibleType(sDDML, pubEvent.sType) == 3)
            {
                pubEvent.connects.add(destCompID, destEventID, destCompFQN, TypeSpec.KIND_SUBSCRIBEDEVENT, sDDML, true);
                pubEventList[foundAt] = pubEvent;
            }
            return;
        }
        //============================================================================
        /// <summary>
        /// Updates the published event connections (subscriber) that has had only 
        /// part details from the SDML script.
        /// Searches by compID, eventID, destEventName
        /// </summary>
        /// <param name="compID">The published event owner</param>
        /// <param name="eventID">The published event</param>
        /// <param name="destCompID">Component that owns the subscribing event</param>
        /// <param name="destEventName">Name of the subscribing event</param>
        /// <param name="destEventID">id of the subscribing event</param>
        //============================================================================
        public void updatePubEventConnection(uint compID, uint eventID, uint destCompID, string destEventName, uint destEventID)
        {
            TIDSpec anEvent;

            //get the list of connections to the published event
            int foundAt = 0;
            TPubEvent pubEvent = (TPubEvent)getConnectEntity(pubEventList, compID, eventID, ref foundAt);
            if (!pubEvent.Equals(null))
            {
                TEntityList connections = pubEvent.connects;

                //now find the connection by name
                bool found = false;
                int i = 1;
                while (!found && (i <= connections.count()))
                {
                    anEvent = connections.getEntity(i);
                    if (anEvent.name == destEventName.ToLower())
                    {
                        found = true;
                        //update the destCompID and the destEventID
                        anEvent.compID = destCompID;
                        anEvent.itemID = destEventID;
                        connections.storeEntity(anEvent, i);                    //store changes
                    }
                    i++;
                }
                pubEvent.connects = connections;    //store changed connections
                pubEventList[foundAt] = pubEvent;
            }
            return;
        }
        //============================================================================
        /// <summary>
        /// Updates the drving property connection to other owned properties
        /// Searches by compID, propID, srcPropName
        /// </summary>
        /// <param name="compID">The driving property owner</param>
        /// <param name="propID">The driving property ID</param>
        /// <param name="srcCompID">Component that owns the source property</param>
        /// <param name="srcPropName">Name of the owned property connected to</param>
        /// <param name="srcPropID">id of the owned property connected to</param>
        //============================================================================
        public void updatePropertyConnection(uint compID, uint propID, uint srcCompID, string srcPropName, uint srcPropID)
        {
            bool found;
            TIDSpec property;

            //get the list of connections to the driving property
            int foundAt = 0;
            TDrvProperty drvProperty = (TDrvProperty)getConnectEntity(drvPropList, compID, propID, ref foundAt);
            if (!drvProperty.Equals(null))
            {
                TEntityList connections = drvProperty.connects;

                //now find the connection by name
                found = false;
                int i = 1;
                while (!found && (i <= connections.count()))
                {
                    property = connections.getEntity(i);
                    if (property.name == srcPropName.ToLower())
                    {
                        found = true;
                        //update the srcCompID and the srcPropID
                        property.compID = srcCompID;
                        property.itemID = srcPropID;
                        connections.storeEntity(property, i);                    //store changes
                    }
                    i++;
                }
                drvProperty.connects = connections;                             //store changed connections
                drvPropList[foundAt] = drvProperty;
            }
            return;
        }
        //============================================================================
        /// <summary>
        /// Updates the registered setter properties by adding a connection to a property
        /// found elswhere in the system.
        /// </summary>
        /// <param name="setterCompID"></param>
        /// <param name="setterPropertyID"></param>
        /// <param name="destCompID"></param>
        /// <param name="destPropertyID"></param>
        /// <param name="destFQName"></param>
        //============================================================================
        public void updateSetterConnection(uint setterCompID, uint setterPropertyID, uint destCompID, uint destPropertyID, string destFQName)
        {
            int foundAt = 0;
            TReqSetProperty setProperty = (TReqSetProperty)getConnectEntity(propSetList, setterCompID, setterPropertyID, ref foundAt);

            if ((setProperty != null) && (setProperty.connects != null))
            {
                // Make sure we haven't already registered this connection....
                bool alreadySet = false;
                for (int i = 1; !alreadySet && (i <= setProperty.connects.count()); i++)
                { //for each connection
                    TIDSpec oldProp = setProperty.connects.getEntity(i);
                    if ((oldProp.compID == destCompID) && (oldProp.itemID == destPropertyID))
                        alreadySet = true;
                }
                if (!alreadySet)
                {
                    TIDSpec propID = setProperty.connects.add(destCompID, destPropertyID, destFQName, 0, "", true);
                    //recalc the connections that match the connection rule
                    if (setProperty.connects.count() > 1)
                        recalcReqSetConnections(ref setProperty);
                    propSetList[foundAt] = setProperty; //store changes
                }
            }
            return;
        }
        //============================================================================
        /// <summary>
        /// Look for the entities that are registered and match - ownerID, searchName
        /// and kind. If the ownerID is 0 then matching to ownerID is ignored.
        /// If the searchName is a wildcard (*) then we need to match any entity.
        /// Returns a list of EntityRef structures containing the details of every
        /// match.
        /// </summary>
        /// <param name="searchNameOwnerID">The owner of the item described by searchName</param>
        /// <param name="searchNameOwner">The FQN of the owner of the item described by searchName</param>
        /// <param name="searchName">The unqualified name of the item to find</param>
        /// <param name="kind"></param>
        /// <param name="infoList">The list of items found</param>
        /// <param name="sType"></param>
        /// <returns></returns>
        //============================================================================
        public TEntityList findEntities(uint searchNameOwnerID, String searchNameOwner, String searchName, int kind, ref TEntityList infoList, string sType)
        {
            bool useWildcard;
            string sFQN;
            string entityName;    // pointer to the unqualifed name
            string lowerSearchName = "";
            TConnectEntity entity;
            List<TConnectEntity> searchList = null;
            int propKind;
            int compatDir = 0;  // direction for checking compatability
            // if 0, no check
            // if 1, then entity type against szType
            // if 2, then szType against entity type
            // if 3, then both

            useWildcard = (searchName == "*");
            switch (kind)
            {
                case TypeSpec.KIND_DRIVER:
                    searchList = drvPropList;
                    compatDir = 1;
                    break;
                case TypeSpec.KIND_OWNED_W:
                case TypeSpec.KIND_OWNED_RW:
                case TypeSpec.KIND_OWNED_R:
                    searchList = ownedPropList;
                    compatDir = 2;
                    break;
                case TypeSpec.KIND_PUBLISHEDEVENT:
                    searchList = pubEventList;
                    compatDir = 3;
                    break;
                case TypeSpec.KIND_SUBSCRIBEDEVENT:
                    searchList = subEventList;
                    compatDir = 3;
                    break;
                case TypeSpec.KIND_COMPONENT:
                case TypeSpec.KIND_SYSTEM:
                    searchList = null;
                    compatDir = 0;
                    break;
                case TypeSpec.KIND_REQUESTSET:
                    searchList = propSetList;
                    compatDir = 2;
                    break;
            }

            if (!searchList.Equals(null))
            {
                lowerSearchName = searchName.ToLower();
                int i = 0;
                bool done = false;
                while (!done && (i < searchList.Count))
                {
                    entity = searchList[i];
                    entityName = entity.uqname;
                    //check if the unqualified names are a match
                    if (useWildcard || (lowerSearchName == entityName))  //if '*' or matches the searchname
                    {
                        //check if we need to match the ownerID
                        if (searchNameOwnerID == 0 || ((searchNameOwnerID != 0) && (entity.compID == searchNameOwnerID)))
                        {
                            if (entity.ownerFQN.Length > 0)
                                sFQN = entity.ownerFQN + '.' + entityName;
                            else
                                sFQN = entity.name;

                            // We need full kind information for owned properties
                            // We can't just rely on the type of the request
                            propKind = kind;
                            if ((kind == TypeSpec.KIND_OWNED_W) ||
                                (kind == TypeSpec.KIND_OWNED_RW) ||
                                (kind == TypeSpec.KIND_OWNED_R))
                            {
                                TLocProperty locProperty = (TLocProperty)entity;
                                if (!locProperty.Equals(null))
                                {
                                    //determine the property type
                                    if (locProperty.readable)
                                    {
                                        if (locProperty.writable)
                                            propKind = TypeSpec.KIND_OWNED_RW;
                                        else
                                            propKind = TypeSpec.KIND_OWNED_R;
                                    }
                                    else
                                    {
                                        if (locProperty.writable)
                                            propKind = TypeSpec.KIND_OWNED_W;
                                    }
                                }
                            }
                            int compat = getCompatibleType(entity.sType, sType);
                            bool OKtoAdd = ((((compatDir & 1) != 1) ||
                                           ((compat & 1) == 1)) &&
                                           (((compatDir & 2) != 2) ||
                                           ((compat & 2) == 2)));

                            // Entity names are allowed to be fully or partly qualified
                            // If we're looking for connections, reject those that don't match
                            // - if we find 'sow' and the full name of the published event is 'plant2.sow' then we
                            // need to match 'plant2.sow' to the (FQN)searchname
                            if ((sType.Length > 0) && (entity.name.IndexOf('.') != -1)) //if the item found is qualified
                            {
                                String FQNSearchName = searchNameOwner + "." + searchName;
                                int start = FQNSearchName.Length - entity.name.Length;     //determine the matching part of the string
                                OKtoAdd &= (start >= 0) && (String.Compare(entity.name.Substring(start), FQNSearchName, true) == 0);
                            }
                            if (OKtoAdd)
                            {
                                infoList.add(entity.compID, entity.regID, sFQN, propKind, entity.sType, true);
                            }
                        }//endif
                    }//endif
                    //decide if there is need to keep searching this ordered list
                    if (!useWildcard && String.Compare(searchName, entityName, true) < 0)
                    {
                        done = true;
                    }
                    i++;
                }
            }
            return infoList;
        }
        //============================================================================
        /// <summary>
        /// Maintain the list of parent-child ids
        /// Add a reference/link from the parent component ID to the child/sub child ID
        /// </summary>
        /// <param name="ownerID"></param>
        /// <param name="decendantChildID"></param>
        //============================================================================
        public void addOwnerComp(uint ownerID, uint decendantChildID)
        {
            if (compOwnerList.ContainsKey(decendantChildID) == false)
                compOwnerList.Add(decendantChildID, ownerID);
            return;
        }
        //============================================================================
        /// <summary>
        /// Searches the list of parent-child relationships (one->many)
        /// Returns the ID of the parent of the decendantChildID
        /// If the child is not found then 0 is returned
        /// </summary>
        /// <param name="decendantChildID"></param>
        /// <returns></returns>
        //============================================================================
        public uint getParentOf(uint decendantChildID)
        {
            uint owner;
            if (compOwnerList.TryGetValue(decendantChildID, out owner))
                return owner;

            return 0;
        }
        //============================================================================
        /// <summary>
        /// Searches the list of parent-child relationships to find the immediate
        /// child of myID that is an ancestor of decendantChildID.
        /// </summary>
        /// <param name="myID"></param>
        /// <param name="decendantChildID"></param>
        /// <returns>If the child is not found then 0 is returned</returns>
        //============================================================================
        public uint getMyChildTheAncestorOf(uint myID, uint decendantChildID)
        {
            uint childID = 0;
            uint ownerID;

            ownerID = getParentOf(decendantChildID);
            if (ownerID == myID)             //if the myID is the parent
                childID = decendantChildID;   //store the child name destination
            else
            {
                //otherwise we need to search the family tree of ID's
                while ((ownerID != myID) && (ownerID > 0))
                {
                    childID = ownerID;
                    ownerID = getParentOf(childID);
                }
            }
            return childID;
        }
        //============================================================================
        /// <summary>
        /// Returns the connection list for the published event ID
        /// </summary>
        /// <param name="compID"></param>
        /// <param name="eventID"></param>
        /// <returns>The connection list for the published event ID</returns>
        //============================================================================
        public TEntityList getPubEventConnections(uint compID, uint eventID)
        {
            TEntityList connectList = null;
            int foundAt = 0;
            TPubEvent pubEvent = (TPubEvent)getConnectEntity(pubEventList, compID, eventID, ref foundAt);
            if (!pubEvent.Equals(null))
            {
                connectList = pubEvent.connects;
            }
            return connectList;
        }
        //============================================================================
        /// <summary>
        /// Returns the list of connections to a driving property.
        /// </summary>
        /// <param name="driverCompID"></param>
        /// <param name="driverPropertyID"></param>
        /// <returns></returns>
        //============================================================================
        public TEntityList getDriverConnections(uint driverCompID, uint driverPropertyID)
        {
            TEntityList connectList = null;
            int foundAt = 0;
            TDrvProperty drvProperty = (TDrvProperty)getConnectEntity(drvPropList, driverCompID, driverPropertyID, ref foundAt);
            if (!drvProperty.Equals(null))
            {
                connectList = drvProperty.connects;
            }
            return connectList;
        }
        //============================================================================
        /// <summary>
        /// Returns the list of connections to a requestSet property.
        /// </summary>
        /// <param name="setterCompID"></param>
        /// <param name="setterPropertyID"></param>
        /// <returns></returns>
        //============================================================================
        public TEntityList getReqSetConnections(uint setterCompID, uint setterPropertyID)
        {
            TEntityList connectList = null;
            int foundAt = 0;
            TReqSetProperty setProperty = (TReqSetProperty)getConnectEntity(propSetList, setterCompID, setterPropertyID, ref foundAt);
            if (!setProperty.Equals(null))
            {
                connectList = setProperty.connects;
            }
            return connectList;
        }
        /// <summary>
        /// Structure used to describe a connected entity
        /// </summary>
        public const String typeEntityList = "<type name=\"entity_list\" array=\"T\">" +
                               "<element>" +
							     "<field name=\"compID\" kind=\"integer4\"/>" +
							     "<field name=\"ownerFQN\" kind=\"string\"/>" +
							     "<field name=\"entityName\" kind=\"string\"/>"+
							     "<field name=\"regID\" kind=\"integer4\"/>"+
							     "<field name=\"DDML\" kind=\"string\"/>" +
							     "<field name=\"destID\" kind=\"integer4\"/>"+
								 "<field name=\"autoConn\" kind=\"boolean\"/>"+
								 "<field name=\"connects\" array=\"T\">"+
								   "<element>"+
    								 "<field name=\"compID\" kind=\"integer4\"/>"+
	   							     "<field name=\"itemID\" kind=\"integer4\"/>"+
								     "<field name=\"name\" kind=\"string\"/>"+
								     "<field name=\"matchesRule\" kind=\"boolean\"/>"+
								   "</element>"+
								 "</field>"+
							   "</element>"+
							 "</type>";
        //============================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="type"></param>
        /// <returns></returns>
        //============================================================================
        public List<TConnectEntity> getConnectList(TConnectType type)
        {
            switch (type)
            {
                case TConnectType.ctDriver: return drvPropList; 
                case TConnectType.ctEvent: return pubEventList; 
                case TConnectType.ctReqSet: return propSetList; 
                default: return null;
            }
        }
        //============================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="connectData"></param>
        /// <param name="type"></param>
        //============================================================================
        public void getConnectionList(out byte[] connectData, TConnectType type)
        {
            connectData = null;

            List<TConnectEntity> entityList = getConnectList(type);

            TDDMLValue listValues = new TDDMLValue(typeEntityList, "");
            uint nValues = (uint)entityList.Count;
            listValues.setElementCount(nValues);
            for (uint i = 0; i < nValues; ++i)
            {
                TTypedValue entityVal = listValues.item(i + 1);
                TConnectEntity entity = entityList[(int)i];
                entityVal.member("compID").setValue((int)entity.compID);
                entityVal.member("ownerFQN").setValue(entity.ownerFQN);
                entityVal.member("entityName").setValue(entity.name);
                entityVal.member("regID").setValue((int)entity.regID);
                entityVal.member("DDML").setValue(entity.sType);

                if (type == TConnectType.ctDriver)
                {
                    TDrvProperty drvProp = (TDrvProperty)entity;
                    if (drvProp != null)
                    {
                        entityVal.member("destID").setValue((int)drvProp.destID);
                        entityVal.member("autoConn").setValue(drvProp.autoConn);
                        uint nConnects = (uint)drvProp.connects.count();
                        entityVal.member("connects").setElementCount(nConnects);
                        for (uint j = 1; j <= nConnects; ++j)
                        {
                            TTypedValue connection = entityVal.member("connects").item(j);
                            TIDSpec connectItem = drvProp.connects.getEntity((int)j);
                            connection.member("compID").setValue((int)connectItem.compID);
                            connection.member("itemID").setValue((int)connectItem.itemID);
                            connection.member("name").setValue(connectItem.name);
                            connection.member("matchesRule").setValue(connectItem.matchesRule);
                        }
                    }
                }
                else if (type == TConnectType.ctEvent)
                {
                  TPubEvent pubEvent = (TPubEvent)entity;
                  if (pubEvent != null) {
                      entityVal.member("destID").setValue((int)pubEvent.destID);
                      entityVal.member("autoConn").setValue(pubEvent.autoConn);
                      uint nConnects = (uint)pubEvent.connects.count();
                      entityVal.member("connects").setElementCount(nConnects);
                      for (uint j = 1; j <= nConnects; ++j) {
                        TTypedValue connection = entityVal.member("connects").item(j);
                        TIDSpec connectItem = pubEvent.connects.getEntity((int)j);
                        connection.member("compID").setValue((int)connectItem.compID);
                        connection.member("itemID").setValue((int)connectItem.itemID);
                        connection.member("name").setValue(connectItem.name);
                      }
                  }
                }
                else if (type == TConnectType.ctReqSet)
                {
                    TReqSetProperty propSet = (TReqSetProperty)entity;
                    if (propSet != null)
                    {
                        entityVal.member("destID").setValue((int)propSet.destID);
                        uint nConnects = (uint)propSet.connects.count();
                        entityVal.member("connects").setElementCount(nConnects);
                        for (uint j = 1; j <= nConnects; ++j)
                        {
                            TTypedValue connection = entityVal.member("connects").item(j);
                            TIDSpec connectItem = propSet.connects.getEntity((int)j);
                            connection.member("compID").setValue((int)connectItem.compID);
                            connection.member("itemID").setValue((int)connectItem.itemID);
                            connection.member("name").setValue(connectItem.name);
                            connection.member("matchesRule").setValue(connectItem.matchesRule);
                        }
                    }
                }
            }
            uint valSize = listValues.sizeBytes();
            byte[] valueData = new byte[valSize];
            listValues.getData(ref valueData);
            connectData = valueData;
        }
    }
}
