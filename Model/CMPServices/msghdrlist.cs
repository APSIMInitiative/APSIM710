using System;
using System.Collections.Generic;
using System.Text;

namespace CMPServices
{
    //============================================================================
    /// <summary>
    /// Container class for a list of TMsgHeader objects.
    /// </summary>
    //============================================================================
    public class TMessageHdrList
    {
        private List<TMsgHeader> msgList;         //list of messages
        private uint activeCount;
        private static TMsgHeader Null;
        //============================================================================
        /// <summary>
        /// Creates a TMsgHeaderList.
        /// </summary>
        //============================================================================
        public TMessageHdrList()
        {
            msgList = new List<TMsgHeader>();
            activeCount = 0;
            Null = new TMsgHeader();
        }
        //============================================================================
        /// <summary>
        /// Delete all messages in the list.
        /// </summary>
        //============================================================================
        protected virtual void removeAll()
        {
            msgList.Clear();
            activeCount = 0;
        }
        //============================================================================
        /// <summary>
        /// Add a message to the list which is a copy of the original message header.
        /// </summary>
        /// <param name="msg">Message to copy and insert into the list.</param>
        //============================================================================
        public virtual void addMsgToList(TMsgHeader msg)
        {
            TMsgHeader aMsg = new TMsgHeader();
            aMsg = msg;   //copy
            aMsg.dataPtr = null;
            msgList.Add(aMsg);

            ++activeCount;
        }
        //============================================================================
        /// <summary>
        /// Removes the msg (by msgID) from the list of msgs.
        /// </summary>
        /// <param name="msgID"></param>
        //============================================================================
        public virtual void removeMsg(uint msgID)
        {
            int i = msgList.Count - 1;
            while (i >= 0)
            {
                {
                    if (msgList[i].msgID == msgID)
                    {
                        msgList.RemoveAt(i);
                        --activeCount;
                        i = 0; //terminate loop
                    }
                    i--;
                }
            }
        }
        //============================================================================
        /// <summary>
        /// Returns the msgType of the msgID list.
        /// If it is not in the list then -1 (invalid) is returned
        /// </summary>
        /// <param name="msgID">Message ID.</param>
        /// <returns>The message type number or -1 if the message is not in the list. See <see cref="Msgs"/> class.</returns>
        //============================================================================
        public int getMsgType(uint msgID)
        {
            for (int i = msgList.Count - 1; i >= 0; i--)
            {
                if (msgList[i].msgID == msgID)
                    return msgList[i].msgType;
            }
            return -1;
        }
        //============================================================================
        /// <summary>
        /// Returns the msg header info of the original msgID from the list
        /// If it is not in the list then TMessageHdrList.Null is returned
        /// </summary>
        /// <param name="msgID">Message ID</param>
        /// <returns>The message header or TMessageHdrList.Null</returns>
        //============================================================================
        public TMsgHeader queryMsg(uint msgID)
        {
            int i = 0;
            while (i < msgList.Count)
            {
                if (msgList[i].msgID == msgID)
                    return msgList[i];
                i++;
            }
            return Null;
        }
        //============================================================================
        /// <summary>
        /// Gets the message that has been stored in this list by index. 
        /// </summary>
        /// <param name="index">1->n</param>
        /// <returns>The message header or TMessageHdrList.Null</returns>
        //============================================================================
        public TMsgHeader getMsg(int index)
        {
            if ((index > 0) && (index <= msgList.Count))
                return msgList[index - 1];
            else
                return Null;
        }
        //============================================================================
        /// <summary>
        /// Return the count of messages in the list.
        /// </summary>
        /// <returns>Count</returns>
        //============================================================================
        public uint msgCount() 
        { 
            return activeCount; 
        }
        //============================================================================
        /// <summary>
        /// Test if the msg is defined.
        /// </summary>
        /// <param name="msg"></param>
        /// <returns>True if msg == TMessageHdrList.Null</returns>
        //============================================================================
        public Boolean IsNull(TMsgHeader msg)
        {
            return msg.Equals(Null);
        }
    }
}
