using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Text;
using CMPServices;

namespace CMPServices
{
    //=========================================================================    
    /// <summary>
    /// The msg field structure is made up of various types
    /// </summary>
    //=========================================================================    
    internal struct TMsgSpec {
        public uint toAck;        //acknowledge
        public int [] fld;       //how many fields allowed in this msg
        public TMsgSpec(uint ack, int [] field) {
            toAck = ack;
            fld = field;
        }
    }
    //=========================================================================    
    /// <summary>
    /// Used as array item when calling field inits before a message is created 
    /// an item that is used to point to a field in a proposed message.
    /// </summary>
    //=========================================================================    
    internal struct TMsgFld {
        public TTypedValue.TBaseType fieldType;    //type of field
        public int size;         //length in bytes of ptrVal
        public int intVal;
        public bool boolVal;
        public String strVal;
        public byte [] ptrVal;
    }
    //=========================================================================    
    /// <summary>
    /// Each message has a header giving details of the message. 
    /// </summary>
    //=========================================================================    
    [StructLayout(LayoutKind.Explicit, Pack = 1)]
    public struct TMsgHeader
    {
        /// <summary>
        /// msg structure version
        /// </summary>
        [FieldOffset(0)]
        public UInt16 version;      
        /// <summary>
        /// msg type 1-32
        /// </summary>
        [FieldOffset(2)]
        public UInt16 msgType;      
        /// <summary>
        /// component id
        /// </summary>
        [FieldOffset(4)]
        public UInt32 from;         
        /// <summary>
        /// component id
        /// </summary>
        [FieldOffset(8)]
        public UInt32 to;           
        /// <summary>
        /// ID of this msg
        /// </summary>
        [FieldOffset(12)]
        public UInt32 msgID;        
        /// <summary>
        /// Acknowlege receipt 1=Ack
        /// </summary>
        [FieldOffset(16)]
        public UInt32 toAck;        
        /// <summary>
        /// Size of dataPtr block
        /// </summary>
        [FieldOffset(20)]
        public UInt32 nDataBytes;   
        /// <summary>
        /// Message data bytes
        /// </summary>
        [FieldOffset(24)]
        public byte[] dataPtr;      
    }
    //=========================================================================
    /// <summary>
    /// The messsage structure that matches the byte layout of the native code.
    /// With the IntPtr dataPtr field that is required by native code.
    /// </summary>
    //=========================================================================
    [StructLayout(LayoutKind.Explicit, Pack = 1)]
    public struct TNativeMsgHeader
    {
        /// <summary>
        /// msg structure version
        /// </summary>
        [FieldOffset(0)]
        public UInt16 version;
        /// <summary>
        /// msg type 1-32
        /// </summary>
        [FieldOffset(2)]
        public UInt16 msgType;
        /// <summary>
        /// component id
        /// </summary>
        [FieldOffset(4)]
        public UInt32 from;
        /// <summary>
        /// component id
        /// </summary>
        [FieldOffset(8)]
        public UInt32 to;
        /// <summary>
        /// ID of this msg
        /// </summary>
        [FieldOffset(12)]
        public UInt32 msgID;
        /// <summary>
        /// Acknowlege receipt 1=Ack
        /// </summary>
        [FieldOffset(16)]
        public UInt32 toAck;
        /// <summary>
        /// Size of dataPtr block
        /// </summary>
        [FieldOffset(20)]
        public UInt32 nDataBytes;
        /// <summary>
        /// Message data bytes
        /// </summary>
        [FieldOffset(24)]
        public IntPtr dataPtr;
    }
    //=========================================================================
    /// <summary>
    /// TMessageInterpreter is used to build messages that will be sent within the simulation 
    /// and also interprets (splits message block) incoming messages.
    /// </summary>
    //=========================================================================
    public class TMessageInterpreter
    {
        // Specifications for the data types contained in the messages. -1 = empty
        private TMsgSpec[] msgSpec = new TMsgSpec[32] {
            new TMsgSpec(0, new int[Msgs.MAX_FLDS] { (int)TTypedValue.TBaseType.ITYPE_STR,  -1, -1, -1, -1, -1 }) ,  // MSG_ACTIVATE
            new TMsgSpec(0, new int[Msgs.MAX_FLDS] { (int)TTypedValue.TBaseType.ITYPE_STR,  -1, -1, -1, -1, -1 }) ,  // MSG_ADD
            new TMsgSpec(0, new int[Msgs.MAX_FLDS] { (int)TTypedValue.TBaseType.ITYPE_BOOL, (int)TTypedValue.TBaseType.ITYPE_STR, -1, -1, -1, -1 }) ,  // MSG_ERROR
            new TMsgSpec(0, new int[Msgs.MAX_FLDS] { -1,                  -1, -1, -1, -1, -1 }) ,  // MSG_COMMENCE
            new TMsgSpec(0, new int[Msgs.MAX_FLDS] { (int)TTypedValue.TBaseType.ITYPE_INT4, -1, -1, -1, -1, -1 }) ,  // MSG_COMPLETE
            new TMsgSpec(0, new int[Msgs.MAX_FLDS] { (int)TTypedValue.TBaseType.ITYPE_STR,  -1, -1, -1, -1, -1 }) ,  // MSG_DEACTIVATE
            new TMsgSpec(0, new int[Msgs.MAX_FLDS] { (int)TTypedValue.TBaseType.ITYPE_STR,  -1, -1, -1, -1, -1 }) ,  // MSG_DELETE
            new TMsgSpec(0, new int[Msgs.MAX_FLDS] { (int)TTypedValue.TBaseType.ITYPE_INT4, (int)TTypedValue.TBaseType.ITYPE_INT4, -1, -1, -1, -1 }) ,  // MSG_DEREGISTER
            new TMsgSpec(0, new int[Msgs.MAX_FLDS] { (int)TTypedValue.TBaseType.ITYPE_INT4, (int)TTypedValue.TBaseType.ITYPE_INT4, (int)TTypedValue.TBaseType.ITYPE_STR, (int)TTypedValue.TBaseType.ITYPE_DEF, -1, -1}) ,  // MSG_EVENT
            new TMsgSpec(1, new int[Msgs.MAX_FLDS] { (int)TTypedValue.TBaseType.ITYPE_INT4, -1, -1, -1, -1, -1 }) ,  // MSG_GETVALUE
            new TMsgSpec(1, new int[Msgs.MAX_FLDS] { (int)TTypedValue.TBaseType.ITYPE_STR,  (int)TTypedValue.TBaseType.ITYPE_STR, (int)TTypedValue.TBaseType.ITYPE_BOOL,  -1, -1, -1 }) ,  // MSG_INIT1
            new TMsgSpec(1, new int[Msgs.MAX_FLDS] { -1,                  -1, -1, -1, -1, -1 }) ,  // MSG_INIT2
            new TMsgSpec(1, new int[Msgs.MAX_FLDS] { -1,                  -1, -1, -1, -1, -1 }) ,  // MSG_NOTIFYDELETE
            new TMsgSpec(0, new int[Msgs.MAX_FLDS] { (int)TTypedValue.TBaseType.ITYPE_BOOL, (int)TTypedValue.TBaseType.ITYPE_INT4, (int)TTypedValue.TBaseType.ITYPE_INT4, (int)TTypedValue.TBaseType.ITYPE_INT4, (int)TTypedValue.TBaseType.ITYPE_STR, (int)TTypedValue.TBaseType.ITYPE_STR }) ,  // MSG_NOTIFYREG
            new TMsgSpec(0, new int[Msgs.MAX_FLDS] { (int)TTypedValue.TBaseType.ITYPE_INT4, (int)TTypedValue.TBaseType.ITYPE_BOOL, -1, -1, -1, -1 }) ,  // MSG_NOTIFYSET
            new TMsgSpec(1, new int[Msgs.MAX_FLDS] { -1,                  -1, -1, -1, -1, -1 }) ,  // MSG_NOTIFYTERMINATION
            new TMsgSpec(0, new int[Msgs.MAX_FLDS] { -1,                  -1, -1, -1, -1, -1 }) ,  // MSG_PAUSE
            new TMsgSpec(0, new int[Msgs.MAX_FLDS] { (int)TTypedValue.TBaseType.ITYPE_INT4, (int)TTypedValue.TBaseType.ITYPE_STR,  (int)TTypedValue.TBaseType.ITYPE_DEF, -1, -1, -1 }) ,  // MSG_PUBLISHEVENT
            new TMsgSpec(1, new int[Msgs.MAX_FLDS] { (int)TTypedValue.TBaseType.ITYPE_STR,  (int)TTypedValue.TBaseType.ITYPE_INT4, -1, -1, -1, -1 }) ,  // MSG_QUERYINFO
            new TMsgSpec(0, new int[Msgs.MAX_FLDS] { (int)TTypedValue.TBaseType.ITYPE_INT4, (int)TTypedValue.TBaseType.ITYPE_STR,  (int)TTypedValue.TBaseType.ITYPE_DEF, -1, -1, -1 }) ,  // MSG_QUERYSET
            new TMsgSpec(0, new int[Msgs.MAX_FLDS] { (int)TTypedValue.TBaseType.ITYPE_INT4, (int)TTypedValue.TBaseType.ITYPE_INT4, -1, -1, -1, -1 }) ,  // MSG_QUERYVALUE
            new TMsgSpec(0, new int[Msgs.MAX_FLDS] { (int)TTypedValue.TBaseType.ITYPE_INT4, (int)TTypedValue.TBaseType.ITYPE_INT4, (int)TTypedValue.TBaseType.ITYPE_INT4, (int)TTypedValue.TBaseType.ITYPE_STR,  (int)TTypedValue.TBaseType.ITYPE_STR, -1}) ,  // MSG_REGISTER
            new TMsgSpec(0, new int[Msgs.MAX_FLDS] { (int)TTypedValue.TBaseType.ITYPE_STR,  -1, -1, -1, -1, -1 }) ,  // MSG_REINSTATE
            new TMsgSpec(0, new int[Msgs.MAX_FLDS] { (int)TTypedValue.TBaseType.ITYPE_INT4, (int)TTypedValue.TBaseType.ITYPE_BOOL, -1, -1, -1, -1 }) ,  // MSG_REPLYSET
            new TMsgSpec(0, new int[Msgs.MAX_FLDS] { (int)TTypedValue.TBaseType.ITYPE_INT4, (int)TTypedValue.TBaseType.ITYPE_STR,  (int)TTypedValue.TBaseType.ITYPE_DEF, -1, -1, -1 }) ,  // MSG_REPLYVALUE
            new TMsgSpec(0, new int[Msgs.MAX_FLDS] { (int)TTypedValue.TBaseType.ITYPE_INT4, (int)TTypedValue.TBaseType.ITYPE_STR,  -1, -1, -1, -1 }) ,  // MSG_REQUESTCOMPID
            new TMsgSpec(0, new int[Msgs.MAX_FLDS] { (int)TTypedValue.TBaseType.ITYPE_INT4, (int)TTypedValue.TBaseType.ITYPE_STR,  (int)TTypedValue.TBaseType.ITYPE_DEF, -1, -1, -1 }) ,  // MSG_REQUESTSETVALUE
            new TMsgSpec(0, new int[Msgs.MAX_FLDS] { -1,                  -1, -1, -1, -1, -1 }) ,  // MSG_RESUME
            new TMsgSpec(0, new int[Msgs.MAX_FLDS] { (int)TTypedValue.TBaseType.ITYPE_STR,  (int)TTypedValue.TBaseType.ITYPE_INT4, -1, -1, -1, -1 }) ,  // MSG_RETURNCOMPID
            new TMsgSpec(0, new int[Msgs.MAX_FLDS] { (int)TTypedValue.TBaseType.ITYPE_INT4, (int)TTypedValue.TBaseType.ITYPE_INT4, (int)TTypedValue.TBaseType.ITYPE_INT4, (int)TTypedValue.TBaseType.ITYPE_STR,  (int)TTypedValue.TBaseType.ITYPE_STR, (int)TTypedValue.TBaseType.ITYPE_INT4 }) ,  // MSG_RETURNINFO
            new TMsgSpec(0, new int[Msgs.MAX_FLDS] { (int)TTypedValue.TBaseType.ITYPE_INT4, (int)TTypedValue.TBaseType.ITYPE_INT4, (int)TTypedValue.TBaseType.ITYPE_STR,  (int)TTypedValue.TBaseType.ITYPE_DEF, -1, -1 }) ,  // MSG_RETURNVALUE
            new TMsgSpec(0, new int[Msgs.MAX_FLDS] { -1,                  -1, -1, -1, -1, -1 })     // MSG_TERMINATE
        };

        private System.Text.ASCIIEncoding ascii;
        private uint ownerCompID;             //Owner of this interpreter
        private uint runningID;               //an ID number that is incremented every time a new message
        //is created
        private TMsgFld[] initArray;         //array of fields in the msg block
        private int newMsgBlockSize;          //the expected size that the new message block is to be

        private TMsgHeader msg;             //the message header

        //=========================================================================    
        /// <summary>
        /// 
        /// </summary>
        /// <param name="compID"></param>
        //=========================================================================    
        public TMessageInterpreter(uint compID)
        {
            ownerCompID = compID;
            runningID = 0;
            newMsgBlockSize = 0;

            initArray = new TMsgFld[Msgs.MAX_FLDS];
            //initialise the initArray to not have any fields yet
            for (int i = 0; i < Msgs.MAX_FLDS; i++)
            {
                initArray[i].fieldType = TTypedValue.TBaseType.ITYPE_EMPTY;
                initArray[i].size = 0;
            }
            ascii = new System.Text.ASCIIEncoding();
        }
        //=========================================================================    
        /// <summary>
        /// Init the starting message id.
        /// </summary>
        /// <param name="initValue">Use this starting value</param>
        //=========================================================================    
        public void initMsgIDCounter(uint initValue)
        {
            runningID = initValue;
        }
        //=========================================================================    
        /// <summary>
        /// Allow the incrementing of the message id
        /// </summary>
        /// <param name="amount"></param>
        //=========================================================================    
        public void incMsgIDCounter(uint amount)
        {
            runningID += amount;
        }
        //=========================================================================    
        /// <summary>
        /// Construct a message with fields appropriate to the nominated message.
        /// All integer fields will be set to zero, all text fields to null, all
        /// boolean fields to false and all pointers to null. If the interpreter is
        /// already handling a message, it is deleted.
        /// </summary>
        /// <param name="msgType">Type of message. See Msgs class</param>
        /// <param name="destAddress"></param>
        /// <returns></returns>
        //=========================================================================    
        public TMsgHeader createMessage(int msgType, uint destAddress)
        {
            int iFld, iFldType;
            int ptr = 0;    //index in byte array

            // determine a unique id for this message
            // avoid using an ID of 0, since this is used else to flag empty buffers
            if (++runningID == 0)
                ++runningID;
            msg = new TMsgHeader();
            msg.version = 256;                      //protocol version
            msg.msgType = (UInt16)msgType;          //id of msg type
            msg.from = ownerCompID;                 //id of this component
            msg.to = destAddress;                   //sending msg to a component
            msg.msgID = runningID;                  //unique id
            msg.toAck = msgSpec[msgType - 1].toAck;   //decide whether to acknowledge
            msg.nDataBytes = (uint)newMsgBlockSize; //init field
            msg.dataPtr = new byte[msg.nDataBytes];

            //use all the memory that the initArray points to
            iFld = 1;
            iFldType = msgSpec[msg.msgType - 1].fld[iFld - 1];
            byte[] buf;
            int i;
            while ((iFldType != -1) && (iFld <= Msgs.MAX_FLDS))
            {  //while more fields
                switch (iFldType)
                {                                   //select the field type
                    case (int)TTypedValue.TBaseType.ITYPE_INT4:
                        {
                            msg.dataPtr[ptr++] = (Byte)((uint)initArray[iFld - 1].intVal);
                            msg.dataPtr[ptr++] = (Byte)(((uint)initArray[iFld - 1].intVal) >> 8);
                            msg.dataPtr[ptr++] = (Byte)(((uint)initArray[iFld - 1].intVal) >> 16);
                            msg.dataPtr[ptr++] = (Byte)(((uint)initArray[iFld - 1].intVal) >> 24);
                            initArray[iFld - 1].fieldType = TTypedValue.TBaseType.ITYPE_EMPTY;
                            initArray[iFld - 1].size = 0;
                        }
                        break;
                    case (int)TTypedValue.TBaseType.ITYPE_BOOL:
                        {
                            buf = BitConverter.GetBytes(initArray[iFld - 1].boolVal);
                            for (i = 0; i < TTypedValue.typeSize[(int)TTypedValue.TBaseType.ITYPE_BOOL]; i++)
                                msg.dataPtr[ptr++] = buf[i];
                            initArray[iFld - 1].fieldType = TTypedValue.TBaseType.ITYPE_EMPTY;
                            initArray[iFld - 1].size = 0;
                        }
                        break;
                    case (int)TTypedValue.TBaseType.ITYPE_STR:
                        {
                            //copy the chars to the data block and set the DIM=x bytes
                            int asize = initArray[iFld - 1].size;
                            msg.dataPtr[ptr++] = (Byte)((uint)asize);
                            msg.dataPtr[ptr++] = (Byte)(((uint)asize) >> 8);
                            msg.dataPtr[ptr++] = (Byte)(((uint)asize) >> 16);
                            msg.dataPtr[ptr++] = (Byte)(((uint)asize) >> 24);
                            Array.Copy(ascii.GetBytes(initArray[iFld - 1].strVal), 0, msg.dataPtr, ptr, asize);
                            ptr += asize;
                            //for (int i = 0; i < asize; i++)
                            //  msg.dataPtr[ptr++] = initArray[iFld - 1].ptrVal[i]; 
                            initArray[iFld - 1].fieldType = TTypedValue.TBaseType.ITYPE_EMPTY;
                            initArray[iFld - 1].size = 0;
                        }
                        break;
                    case (int)TTypedValue.TBaseType.ITYPE_DEF:
                        {
                            //now copy the datablock from the typed value into the ptr
                            Array.Copy(initArray[iFld - 1].ptrVal, 0, msg.dataPtr, ptr, initArray[iFld - 1].size);
                            ptr += initArray[iFld - 1].size;
                            initArray[iFld - 1].fieldType = TTypedValue.TBaseType.ITYPE_EMPTY;
                            initArray[iFld - 1].size = 0;
                        }
                        break;
                }
                iFld++;                                               //next field
                if (iFld <= Msgs.MAX_FLDS)                                 //if there is another field
                    iFldType = msgSpec[msg.msgType - 1].fld[iFld - 1];    //get it's type
            }
            newMsgBlockSize = 0;

            return msg;
        }
        //=========================================================================    
        /// <summary>
        /// Sets the value in the field denoted by fieldID to the nominated value.
        /// FieldID corresponds to a fieldname in the message. If the field is not
        /// found in the message, then false is returned.
        /// </summary>
        /// <param name="fieldID"></param>
        /// <param name="value"></param>
        /// <returns>True if successful</returns>
        //=========================================================================    
        public bool setField(int fieldID, uint value)
        {
            return setField(fieldID, (int)value);
        }
        //=========================================================================    
        /// <summary>
        /// Sets the value in the field denoted by fieldID to the nominated value.
        /// FieldID corresponds to a fieldname in the message. If the field is not
        /// found in the message, then false is returned.
        /// </summary>
        /// <param name="fieldID"></param>
        /// <param name="value"></param>
        /// <returns></returns>
        //=========================================================================    
        public bool setField(int fieldID, int value)
        {
            bool result;
            int iMsg;
            int iFld;

            //Validate the field number with the field type expected for this message
            iMsg = fieldID / 1000;
            iFld = fieldID % 1000;
            result = (msgSpec[iMsg - 1].fld[iFld - 1] == (int)TTypedValue.TBaseType.ITYPE_INT4);

            if (result)
            {
                newMsgBlockSize -= freeInitArrayItem(iFld);         //ensure this array item is cleaned up first
                //store the int in a list of ptrs (initArray)
                initArray[iFld - 1].intVal = value;            //store the int value
                initArray[iFld - 1].size = (int)TTypedValue.typeSize[(int)TTypedValue.TBaseType.ITYPE_INT4];
                initArray[iFld - 1].fieldType = TTypedValue.TBaseType.ITYPE_INT4;//store the type of ptr
                newMsgBlockSize += (int)TTypedValue.typeSize[(int)TTypedValue.TBaseType.ITYPE_INT4];
            }
            return result;
        }
        //=========================================================================    
        /// <summary>
        /// Sets the value in the field denoted by fieldID to the nominated value.
        /// FieldID corresponds to a fieldname in the message. If the field is not
        /// found in the message, then false is returned.
        /// </summary>
        /// <param name="fieldID"></param>
        /// <param name="value"></param>
        /// <returns></returns>
        //=========================================================================    
        public bool setField(int fieldID, bool value)
        {
            bool result;
            int iMsg;
            int iFld;

            //Validate the field number with the field type expected for this message
            iMsg = fieldID / 1000;
            iFld = fieldID % 1000;
            result = (msgSpec[iMsg - 1].fld[iFld - 1] == (int)TTypedValue.TBaseType.ITYPE_BOOL);

            if (result)
            {
                newMsgBlockSize -= freeInitArrayItem(iFld);         //ensure this array item is cleaned up first
                //store the int in a list of ptrs (initArray)
                initArray[iFld - 1].boolVal = value;                //store the bool value
                initArray[iFld - 1].size = (int)TTypedValue.typeSize[(int)TTypedValue.TBaseType.ITYPE_BOOL];
                initArray[iFld - 1].fieldType = TTypedValue.TBaseType.ITYPE_BOOL;//store the type of ptr
                newMsgBlockSize += (int)TTypedValue.typeSize[(int)TTypedValue.TBaseType.ITYPE_BOOL];
            }
            return result;
        }
        //=========================================================================    
        /// <summary>
        /// Sets the value in the field denoted by fieldID to the nominated value.
        /// FieldID corresponds to a fieldname in the message. If the field is not
        /// found in the message, then false is returned.
        /// </summary>
        /// <param name="fieldID"></param>
        /// <param name="value"></param>
        /// <returns></returns>
        //=========================================================================    
        public bool setField(int fieldID, string value)
        {
            bool result;
            int iMsg;
            int iFld;

            //Validate the field number with the field type expected for this message
            iMsg = fieldID / 1000;
            iFld = fieldID % 1000;
            result = (msgSpec[iMsg - 1].fld[iFld - 1] == (int)TTypedValue.TBaseType.ITYPE_STR);

            if (result)
            {

                int memFreed = freeInitArrayItem(iFld);                     //ensure this array item is cleaned up first
                if (memFreed > 0)                                           //if bytes were freed then
                    newMsgBlockSize -= (int)TTypedValue.typeSize[(int)TTypedValue.TBaseType.ITYPE_INT4] + memFreed;   //adjust total calculation
                //store the char[]
                //                initArray[iFld - 1].ptrVal = ascii.GetBytes(value);
                initArray[iFld - 1].strVal = value;
                initArray[iFld - 1].size = value.Length;
                initArray[iFld - 1].fieldType = TTypedValue.TBaseType.ITYPE_STR;         //store the type of value                 
                newMsgBlockSize += (int)(TTypedValue.typeSize[(int)TTypedValue.TBaseType.ITYPE_INT4] + value.Length); //block includes DIM bytes and not '\0'
            }
            return result;
        }
        //=========================================================================    
        /// <summary>
        /// Sets the value in the field denoted by fieldID to the nominated value.
        /// FieldID corresponds to a fieldname in the message. If the field is not
        /// found in the message, then false is returned.
        /// </summary>
        /// <param name="fieldID"></param>
        /// <param name="value"></param>
        /// <param name="iSize"></param>
        /// <returns></returns>
        //=========================================================================    
        public bool setField(int fieldID, byte[] value, uint iSize)
        {
            bool result;
            int iMsg;
            int iFld;

            //Validate the field number with the field type expected for this message
            iMsg = fieldID / 1000;
            iFld = fieldID % 1000;
            result = (msgSpec[iMsg - 1].fld[iFld - 1] == (int)TTypedValue.TBaseType.ITYPE_DEF);

            if (result)
            {
                newMsgBlockSize -= freeInitArrayItem(iFld);         //ensure this array item is cleaned up first
                //store the int in a list of ptrs (initArray)

                initArray[iFld - 1].ptrVal = value;                 //store the []
                initArray[iFld - 1].size = (int)iSize;
                initArray[iFld - 1].fieldType = TTypedValue.TBaseType.ITYPE_DEF;//store the type of ptr
                newMsgBlockSize += (int)iSize;
            }
            return result;
        }
        //=========================================================================
        /// <summary>
        /// Clear the temporary array item.
        /// </summary>
        /// <param name="iFld">Index of the array item, 1->n</param>
        /// <returns>The amount of memory freed in bytes.</returns>
        //=========================================================================
        private int freeInitArrayItem(int iFld)
        {
            int memFreed = 0;

            TMsgFld field = initArray[iFld - 1];

            if ((field.fieldType != TTypedValue.TBaseType.ITYPE_EMPTY) && (field.size > 0))
            {
                TMsgFld afield = initArray[iFld - 1];
                memFreed = afield.size;
                afield.size = 0;
                if ((afield.ptrVal != null) && (afield.fieldType == TTypedValue.TBaseType.ITYPE_DEF))
                {
                    int len = afield.ptrVal.Length;
                    Array.Clear(afield.ptrVal, 0, len);
                }
                afield.fieldType = TTypedValue.TBaseType.ITYPE_EMPTY;
                initArray[iFld - 1] = afield;
            }

            return memFreed;
        }
        //=========================================================================
        /// <summary>
        /// Determines the offset for a particular field in the msg data block. Adds
        /// all the item sizes and uses the char[] block information to determine the
        /// size of the char[] strings.
        /// </summary>
        /// <param name="fieldID">Field ID e.g. 14001</param>
        /// <returns>The offset count from the start of the data block to find the
        /// field data.</returns>
        //=========================================================================
        private uint calcOffset(int fieldID)
        {
            int iFieldNo;
            int iFld;        //field number along the block
            int iFldType;
            uint offset = 0;  //ptr offset counter
            int strSize;

            if ((!msg.Equals(null)) && (msg.dataPtr.Length > 0))
            {        //check that a message and data block exist
                iFieldNo = fieldID % 1000;

                for (iFld = 1; iFld < iFieldNo; iFld++)
                {
                    iFldType = msgSpec[msg.msgType - 1].fld[iFld - 1];
                    if (iFldType == (int)TTypedValue.TBaseType.ITYPE_STR)
                    {
                        strSize = BitConverter.ToInt32(msg.dataPtr, (int)offset);    //get the size of the char string
                        offset += TTypedValue.typeSize[(int)TTypedValue.TBaseType.ITYPE_INT4] + (uint)strSize;  //add 4byte size DIM + length of string
                    }
                    else
                    {
                        offset += TTypedValue.typeSize[iFldType];
                    }
                }
            }
            if (msg.nDataBytes < offset)
            {
                string errorMsg = string.Format("Error determining offset of field {0} from msg {1}", fieldID, msg.msgType);
                throw (new ApplicationException(errorMsg));
            }
            return offset;
        }
        //=========================================================================
        /// <summary>
        /// Requests the integer value of a field in the message.
        /// </summary>
        /// <param name="fieldID">ID of the field.</param>
        /// <returns>The integer value found in this field.</returns>
        //=========================================================================
        public int getIntField(int fieldID)
        {
            int iValue = 0;
            int msgType;
            int iFld;
            int byteIndex;

            msgType = fieldID / 1000;          //fieldID / 1000 gives the message id
            iFld = fieldID % 1000;

            if ((msgType == msg.msgType)                              // Check that the field corresponds to this msg
              && (msgSpec[msgType - 1].fld[iFld - 1] == (int)TTypedValue.TBaseType.ITYPE_INT4) // Validate the type of this field within the message
              && (msg.dataPtr.Length > 0))
            {
                //at this point we need to calc the total offset in the dataptr for this int
                byteIndex = (int)calcOffset(fieldID);
                iValue = BitConverter.ToInt32(msg.dataPtr, byteIndex);    //get the int at this position
            }
            else
            {
                string errorMsg = string.Format("Cannot retrieve integer field {0} from msg {1}", iFld, msg.msgType);
                throw (new ApplicationException(errorMsg));
            }
            return iValue;
        }
        //=========================================================================
        /// <summary>
        /// Request the boolean value from this field.
        /// </summary>
        /// <param name="fieldID"></param>
        /// <returns>The boolean value found at this field id.</returns>
        //=========================================================================
        public bool getBoolField(int fieldID)
        {
            bool bValue = false;
            int byteIndex;
            int msgType;
            int iFld;

            msgType = fieldID / 1000;          //fieldID / 1000 gives the message id
            iFld = fieldID % 1000;
            if ((msgType == msg.msgType)                           // Check that the field corresponds to this msg
              && (msgSpec[msgType - 1].fld[iFld - 1] == (int)TTypedValue.TBaseType.ITYPE_BOOL)  // Validate the type of this field within the message
              && (msg.dataPtr.Length > 0))
            {
                //at this point we need to calc the total offset in the dataptr for this char[]
                byteIndex = (int)calcOffset(fieldID);
                bValue = BitConverter.ToBoolean(msg.dataPtr, byteIndex);         //get the bool at this position
            }
            else
            {
                string errorMsg = string.Format("Cannot retrieve boolean field {0} from msg {1}", iFld, msg.msgType);
                throw (new ApplicationException(errorMsg));
            }
            return bValue;
        }
        //=========================================================================
        /// <summary>
        /// Get the text string found at this field id.
        /// </summary>
        /// <param name="fieldID">Message field id value. e.g. 9003</param>
        /// <returns>The string value</returns>
        //=========================================================================
        public string getTextField(int fieldID)
        {
            int byteIndex;
            int msgType;
            int iFld;
            string strField;

            msgType = fieldID / 1000;          //fieldID / 1000 gives the message id
            iFld = fieldID % 1000;
            if ((msgType == msg.msgType)                           // Check that the field corresponds to this msg
              && (msgSpec[msgType - 1].fld[iFld - 1] == (int)TTypedValue.TBaseType.ITYPE_STR)   // Validate the type of this field within the message
              && (msg.dataPtr.Length > 0))
            {
                //at this point we need to calc the total offset in the dataptr for this char[]
                byteIndex = (int)calcOffset(fieldID);
                int strSize = BitConverter.ToInt32(msg.dataPtr, byteIndex);                      //get the size of the char string
                byteIndex += (int)TTypedValue.typeSize[(int)TTypedValue.TBaseType.ITYPE_INT4];                   //jump past the DIM=x 4 bytes
                strField = ascii.GetString(msg.dataPtr, byteIndex, strSize);
            }
            else
            {
                string errorMsg = string.Format("Cannot retrieve string field {0} from msg {1}", iFld, msg.msgType);
                throw (new ApplicationException(errorMsg));
            }
            return strField;
        }
        //=========================================================================
        /// <summary>
        /// Initialise with an incoming msg. This interpreter now has a copy of the message.
        /// </summary>
        /// <param name="inMsg"></param>
        /// <returns></returns>
        //=========================================================================
        public bool loadMessage(TMsgHeader inMsg)
        {
            int iFld;
            bool result;

            if (!inMsg.Equals(null))  //if there is a message header
            {
                //need to do a cleanup in case the interpreter has any existing msg's
                for (iFld = 1; (iFld <= Msgs.MAX_FLDS) && (initArray[iFld - 1].fieldType != TTypedValue.TBaseType.ITYPE_EMPTY); iFld++)
                    if (initArray[iFld - 1].size > 0)
                        freeInitArrayItem(iFld);
                //take ownership of the msg header and data block
                msg = inMsg;      //now I own the msg because I will delete it later
                result = true;
            }
            else
                result = false;

            return result;
        }
        //=========================================================================
        /// <summary>
        /// Ref to msg
        /// </summary>
        /// <returns>Ref to msg</returns>
        //=========================================================================
        public TMsgHeader getMsg()
        {
            return msg;
        }
        //=========================================================================
        /// <summary>
        /// Get the data array field in the message.
        /// Always assumes the variant is the last field!!!
        /// </summary>
        /// <param name="fieldID"></param>
        /// <param name="data"></param>
        /// <returns></returns>
        //=========================================================================
        public uint getValueField(int fieldID, out byte[] data)
        {
            int msgType;
            int iFld;
            uint variantSize = 0;
            uint cursor;

            msgType = fieldID / 1000;                                 // fieldID/1000 gives the message ID
            iFld = fieldID % 1000;
            if ((msgType == msg.msgType)                           // Check that the field corresponds to this msg
              && (msgSpec[msgType - 1].fld[iFld - 1] == (int)TTypedValue.TBaseType.ITYPE_DEF)   // Validate the type of this field within the message
              && (msg.dataPtr.Length > 0))
            {
                //at this point we need to calc the total offset in the dataptr for this variant
                cursor = calcOffset(fieldID);
                variantSize = msg.nDataBytes - cursor;   //always assume the variant is the last field!!!
                data = new byte[variantSize];             //allocate memory. 
                Array.Copy(msg.dataPtr, cursor, data, 0, variantSize);
            }
            else
            {
                string errorMsg = string.Format("Cannot retrieve variant field {0} from msg {1}", iFld, msg.msgType);
                throw (new ApplicationException(errorMsg));
            }
            return variantSize;
        }
        //=========================================================================
        /// <summary>
        /// Converts a message that has an IntPtr data field into a managed
        /// code version of a message.
        /// </summary>
        /// <param name="src">The unmanaged version of the message.</param>
        /// <returns>The new managed message structure.</returns>
        //=========================================================================
        public static TMsgHeader NativeMsgToManagedMsg(ref TNativeMsgHeader src)
        {
            TMsgHeader msgPtr = new TMsgHeader();
            uint nBytes = src.nDataBytes;

            msgPtr.version = src.version;
            msgPtr.msgType = src.msgType;
            msgPtr.from = src.from;
            msgPtr.to = src.to;
            msgPtr.msgID = src.msgID;
            msgPtr.toAck = src.toAck;
            msgPtr.nDataBytes = nBytes;

            if (nBytes == 0)
            {
                msgPtr.dataPtr = null;
            }
            else
            {
                msgPtr.dataPtr = new Byte[nBytes];
                Marshal.Copy(src.dataPtr, msgPtr.dataPtr, 0, (int)nBytes);
            }
            return msgPtr;
        }

        //=========================================================================
        /// <summary>
        /// Casts an unsigned integer to an IntPtr
        /// Shouldn't be necessary, but the Mono VB compiler doesn't handle this
        /// as it should.
        /// Currently assumes 32-bit pointers
        /// </summary>
        /// <param name="ptrVal">The unsigned value to be treated as a pointer.</param>
        /// <returns>The value cast to as IntPtr.</returns>
        //=========================================================================
        public static IntPtr PtrCast(uint ptrVal)
        {
            return (IntPtr)ptrVal;
        }
    }
}
