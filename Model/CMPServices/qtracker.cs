using System;
using System.Collections.Generic;
using System.Text;

namespace CMPServices
{
    //=========================================================================    
    /// <summary>
    /// Records the details about the purpose for a message that is recorded
    /// by a TQueryInfoTracker.
    /// </summary>
    //=========================================================================    
    public struct MsgTask {
        /// <summary>
        /// Message ID
        /// </summary>
        public uint msgID;              
        /// <summary>
        /// ID of requesting component
        /// </summary>
        public uint requestingCompID; 
        /// <summary>
        /// name of the entity requested
        /// </summary>
        public string requestingEntity; 
        /// <summary>
        /// ID of the entity requested
        /// </summary>
        public uint requestingRegID;  
        /// <summary>
        /// request from this component
        /// </summary>
        public uint destCompID;        
        /// <summary>
        /// Task to do when the returnInfo arrives. See <see cref="TQueryInfoTracker.UPD_PUBEVENT_CONN"/>
        /// </summary>
        public int doOnReturn;          
    }
    //=========================================================================    
    /// <summary>
    /// Used to keep track of the queryInfo messages that are sent asynchronously.
    /// When a returnInfo message returns, the component must know what to do with
    /// the returned information. This class just keeps a list of the queryInfo
    /// msg details that are required to determine how to treat the returnInfo.
    /// 
    /// Using the onReturnTask() in a switch() block, the determined task can
    /// be selected from the list of constant id's (PUBEVENT_CONN ...).
    ///
    /// When the task has been selected, call finaliseMsg() to remove outdated
    /// items from the list of pending jobs.
    /// </summary>
    //=========================================================================    
    public class TQueryInfoTracker
    {
        //list of resulting actions to take from queryInfo's (used in TSysComp)
        /// <summary>
        /// Update a published event connection.
        /// </summary>
        public const int UPD_PUBEVENT_CONN = 1;
        /// <summary>
        /// Add a publised event connection.
        /// </summary>
        public const int ADD_PUBEVENT_CONN = 2;
        /// <summary>
        /// Update a driving property connection.
        /// </summary>
        public const int UPD_DRVPROPERTY_CONN = 3;
        /// <summary>
        /// Add a driving property connection.
        /// </summary>
        public const int ADD_DRVPROPERTY_CONN = 4;
        /// <summary>
        /// Update a Setter property connection.
        /// </summary>
        public const int UPD_SETPROPERTY_CONN = 5;
        /// <summary>
        /// Add a Setter property connection.
        /// </summary>
        public const int ADD_SETPROPERTY_CONN = 6;

        /// <summary>
        /// Use any component as the destination.
        /// </summary>
        public const uint ANY_DEST_COMP = 0;
        private List<MsgTask> sentQueryList;    //List of sent queryInfo's
        /// <summary>
        /// Value of an undefined MsgsTask.
        /// </summary>
        public static MsgTask Null;
        //=========================================================================    
        /// <summary>
        /// Constructor
        /// </summary>
        //=========================================================================    
        public TQueryInfoTracker()
        {
            sentQueryList = new List<MsgTask>();
        }
        //=========================================================================    
        /// <summary>
        /// Adds details of the queryInfo msg to the list of msg's
        /// </summary>
        /// <param name="msgID">Message ID.</param>
        /// <param name="reqCompID">ID of the requesting component.</param>
        /// <param name="reqName">Name of the requesting component.</param>
        /// <param name="reqRegID"></param>
        /// <param name="destCompID"></param>
        /// <param name="doOnReturn">Do this task on return. See <see cref="UPD_PUBEVENT_CONN"/></param>
        //=========================================================================    
        public void addQueryMsg(uint msgID, uint reqCompID, string reqName,
                           uint reqRegID, uint destCompID, int doOnReturn)
        {
            MsgTask queryMsg = new MsgTask();
            queryMsg.msgID = msgID;               //queryInfo msg ID
            queryMsg.requestingCompID = reqCompID;
            queryMsg.requestingEntity = reqName;  //name of entity doing the requesting (e.g. driving property)
            queryMsg.requestingRegID = reqRegID;
            queryMsg.destCompID = destCompID;
            queryMsg.doOnReturn = doOnReturn;     //used to determine the which task to perform when a returnInfo arrives

            sentQueryList.Add(queryMsg);
        }
        //=========================================================================    
        /// <summary>
        /// Used to delete the msg details stored previously about a queryInfo msg.
        /// </summary>
        /// <param name="msgID">Message ID</param>
        //=========================================================================    
        public void finaliseMsg(uint msgID)
        {
            bool found = false;
            MsgTask queryMsg;

            if (sentQueryList.Count > 0)
            {
                int i = 0;
                while ((!found) && (i < sentQueryList.Count))   //while more msgs
                {
                    queryMsg = sentQueryList[i];
                    if (queryMsg.msgID == msgID)    //if found
                    {
                        found = true;
                        sentQueryList.RemoveAt(i);   //delete the list item
                    }
                    else
                        i++;
                }
            }
        }
        //=========================================================================    
        /// <summary>
        /// When a returnInfo msg arrives, this method can be used to do a lookup and
        /// determine which task was previously defined to be carried out now.
        /// The return value is expected to be used in a switch() block.
        /// </summary>
        /// <param name="msgID">Message ID</param>
        /// <returns>Returns an integer value that matches one of the task constants. See <see cref="UPD_PUBEVENT_CONN"/>
        /// Returns 0 if no matching message is found.</returns>
        //=========================================================================    
        public int onReturnTask(uint msgID)
        {
            int task = 0;   //default when nothing found
            MsgTask msg;

            msg = (MsgTask)findMsg(msgID);
            if (!IsNull(msg))
                task = msg.doOnReturn;      //store the identifier

            return task;
        }
        //=========================================================================    
        /// <summary>
        /// Does a search through the list of msg's
        /// </summary>
        /// <param name="msgID">Message ID</param>
        /// <returns>Returns the MsgTask item or TQueryInfoTracker.Null if not found.</returns>
        //=========================================================================    
        public MsgTask findMsg(uint msgID)
        {
            bool found = false;
            MsgTask msg = Null;
            MsgTask queryMsg;

            if (sentQueryList.Count > 0)
            {
                int i = 0;
                while ((!found) && (i < sentQueryList.Count))   //while more msgs
                {                
                    queryMsg = sentQueryList[i];
                    if (queryMsg.msgID == msgID)
                    {   //if found
                        found = true;
                        msg = sentQueryList[i];
                    }
                    else
                        i++;
                }
            }
            return msg;
        }
        //=========================================================================    
        /// <summary>
        /// Find the name of the requesting entity.
        /// </summary>
        /// <param name="msgID">Message ID</param>
        /// <returns>The name of the requesting entity. "" if not found.</returns>
        //=========================================================================    
        private string msgRequestingEntity(uint msgID)
        {
            string sName = "";
            MsgTask msg;

            msg = findMsg(msgID);
            if (!IsNull(msg))
                sName = msg.requestingEntity;      //store the identifier

            return sName;
        }
        //=========================================================================    
        /// <summary>
        /// Get the requesting component ID
        /// </summary>
        /// <param name="msgID">Message ID</param>
        /// <returns>The requesting component ID. 0 if not found.</returns>
        //=========================================================================    
        private uint msgRequestingComp(uint msgID)
        {
            uint compID = 0;
            MsgTask msg;

            msg = findMsg(msgID);
            if (!IsNull(msg))
                compID = msg.requestingCompID;      //store the requesting component ID

            return compID;
        }
        //=========================================================================    
        /// <summary>
        /// Get the requesting registration ID.
        /// </summary>
        /// <param name="msgID">Message ID.</param>
        /// <returns>0 if not found.</returns>
        //=========================================================================    
        private uint msgRequestingID(uint msgID)
        {
            uint regID = 0;
            MsgTask msg;

            msg = findMsg(msgID);
            if (!IsNull(msg))
                regID = msg.requestingRegID;      //store the requesting component ID

            return regID;
        }
        //=========================================================================    
        /// <summary>
        /// Return the ID of the "destination" component
        /// </summary>
        /// <param name="msgID">Message ID</param>
        /// <returns>Return the ID of the "destination" component</returns>
        //=========================================================================    
        private uint msgRequestedComp(uint msgID)
        {
            uint compID = 0;
            MsgTask msg;

            msg = findMsg(msgID);
            if (!IsNull(msg))
                compID = msg.destCompID;      //return the ID of the "destination" component

            return compID;
        }
        //============================================================================
        /// <summary>
        /// Determine if the MsgTask is defined.
        /// </summary>
        /// <param name="task">The MsgTask item.</param>
        /// <returns>True is the task==NULL</returns>
        //============================================================================
        public Boolean IsNull(MsgTask task)
        {
            return task.Equals(Null);
        }
    }
}
