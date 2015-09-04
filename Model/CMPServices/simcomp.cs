using System;
using System.Collections.Generic;
using System.Text;

namespace CMPServices
{
    //============================================================================
    /// <summary>
    /// Specialised version of TBaseComp that applies to a component that is the
    /// simulation component.
    /// </summary>
    //============================================================================
    public abstract class TSimComp : TSysComp
    {
        private uint FNewComponentID;        //unique running list of new component ID's

        //============================================================================
        /// <summary>
        /// Constructor for the Simulation base type. 
        /// </summary>
        /// <param name="ID">Component ID.</param>
        /// <param name="parentID">Parent component ID.</param>
        /// <param name="msgCallBack">Callback to the simulation engine.</param>
        /// <param name="sType">Class type.</param>
        /// <param name="sVersion">Version string.</param>
        /// <param name="sAuthor">Author name.</param>
        //============================================================================
        protected TSimComp(uint ID, uint parentID, MessageFromLogic msgCallBack, string sType, string sVersion, string sAuthor)
            : base(ID, parentID, msgCallBack, sType, sVersion, sAuthor)
        {
            FNewComponentID = 1;  //increment from here
            FParentID = 1;       //has no parent so point to itself
        }
        //============================================================================
        /// <summary>
        /// Overrides version in TBaseComponent
        /// </summary>
        /// <param name="msg">Incoming message.</param>
        //============================================================================
        public override void handleMessage(TMsgHeader msg)  
        {
            base.handleMessage(msg);  //common messages
        }
        //============================================================================
        /// <summary>
        /// If there is a sequencer component, sends a commence message to it to begin
        /// the simulation.
        /// </summary>
        //============================================================================
        protected void sendCommence()
        {
            TMsgHeader newMsg = interpreter.createMessage(Msgs.MSG_COMMENCE, sequencerID);
            // Normally the entire simulation runs "inside" the handling of this
            // message, so this seems a good place to catch any wayward exceptions
            try
            {
                sendMessage(newMsg);
            }
            catch (Exception e)
            {
                sendError("Exception during execution: sendCommence() " + e.Message, true);
            }
        }
        //============================================================================
        /// <summary>
        /// Sends the unique component ID to the system that owns the component.
        /// </summary>
        /// <param name="ownerID">Owning component ID.</param>
        /// <param name="sCompFQN">Component name.</param>
        /// <param name="newID">New component ID.</param>
        //============================================================================
        protected void sendComponentID(uint ownerID, string sCompFQN, uint newID)
        {
            registrar.addOwnerComp(ownerID, newID);

            interpreter.setField(Msgs.MSG_RETURNCOMPID_FQN, sCompFQN);
            interpreter.setField(Msgs.MSG_RETURNCOMPID_ID, newID);
            interpreter.createMessage(Msgs.MSG_RETURNCOMPID, ownerID);
            TMsgHeader newMsg = interpreter.getMsg();              //take control of this msg

            sendMessage(newMsg);
        }
        //============================================================================
        /// <summary>
        /// In addition to beginComponentDelete and Terminate,
        /// <ul>
        /// <li><b>init1:</b> If this is the last init1 message sent to owned components to be
        ///        acknowledged, then init2 is sent to all components managed by the
        ///        master system.</li>
        /// <li><b>init2:</b> If this is the last init2 message sent to owned components to be
        ///        acknowledged, then commence is sent (either to the master PM or
        ///        the sequencer service). </li>
        /// </ul>
        /// </summary>
        /// <param name="msgFrom">Source of the Complete.</param>
        /// <param name="origMsgID">The original message ID.</param>
        //============================================================================
        protected override void doComplete(uint msgFrom, uint origMsgID)
        {
            TComp child;

            switch (getSentMsgType(origMsgID))
            {
                case Msgs.MSG_INIT1:
                    {
                        init1AcksRemain--;
                        finaliseSentMsg(origMsgID);
                        if ((init1AcksRemain == 0) /*&& init1LogicComplete*/)
                        { //if all the init1s are finished
                            doingStartup = false;                                 //finished the Init1 stage
                            sendComplete(init1MsgHdr.from, init1MsgHdr.msgID);    //send a Complete(init1) to ourself
                            sendInit2(FMyID);                                     //send it to myself so it will be handled like any other system
                        }
                    }
                    break;
                case Msgs.MSG_INIT2:
                    {
                        finaliseSentMsg(origMsgID);                             //take init2's off sent msg list
                        init2AcksRemain--;
                        if ((init2AcksRemain == 0) /*&& init2LogicComplete*/)
                        {   //no more expected
                            resolveManualConnections();                         // resolve manual connections
                            sendComplete(init2MsgHdr.from, init2MsgHdr.msgID);    //send a Complete(init1) to ourself
                            if (!mustTerminate)
                              sendCommence();                                     //send commence to the sequencer (required here for asynchronous)
                        }
                    }
                    break;
                case Msgs.MSG_NOTIFYTERMINATION:
                    {
                        //here we delete the component that responded
                        TMsgHeader msg;
                        querySentMsgList(origMsgID, out msg);
                        uint compID = msg.to;
                        finaliseSentMsg(origMsgID);

                        if (sdmlChildList.Count > 0)
                        {
                            //remove the sdmlComp from the child list
                            //this keeps track of how many components are left to delete
                            int i = 0;
                            while (i < compList.Count)
                            {
                                child = compList[i];
                                if (child.compID == compID)
                                {
                                    //remove the TSDMLComponent from the child list
                                    removeSDMLChild(child.name);
                                    i = compList.Count;   //terminate loop
                                }
                                else
                                    i++;
                            }
                        }
                        //when all the messages have been acknowledged this simulation is done
                    }
                    break;

                default:
                    base.doComplete(msgFrom, origMsgID);
                    break;
            }
        }
        //============================================================================
        /// <summary>
        /// Pauses the simulation msg back to the sender
        /// </summary>
        /// <param name="msg">Incoming Pause message.</param>
        //============================================================================
        protected override void handlePause(TMsgHeader msg)
        {
            //sequencer sends a COMPLETE msg once received
            pauseSimulation(msg);   //sends this msg off to the sequencer
        }
        //============================================================================
        /// <summary>
        /// Responds to the resume message by calling the sequencer
        /// </summary>
        /// <param name="msg">Incoming Resume message.</param>
        //============================================================================
        protected override void handleResume(TMsgHeader msg)
        {
            resumeSimulation();
        }
        //============================================================================
        /// <summary>
        /// Generates a unique component ID and sends it back to the system that
        /// owns the component (iOwnerID).
        /// </summary>
        /// <param name="iOwnerID"></param>
        /// <param name="sCompName"></param>
        //============================================================================
        protected override void doCompIDRequest(uint iOwnerID, string sCompName)
        {
            sendComponentID(iOwnerID, sCompName, makeComponentID());
        }
        //============================================================================
        /// <summary>
        /// Overrides the function in TSysComp.
        /// This is the response to the MSG_TERMINATE.
        /// Sends terminate messages to all components managed by the simulation
        /// </summary>
        //============================================================================
        protected override void doReqTerminate()
        {
            //create and send the msg to all children
            //they respond with complete() messages and are then deleted
            TComp child;
            mustTerminate = true;   //this object knows that termination is in progress

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
        /// Sends a pause message to the sequencer
        /// </summary>
        /// <param name="msg"></param>
        //============================================================================
        protected void pauseSimulation(TMsgHeader msg)
        {
            msg.to = sequencerID;  //redirect to the sequencer
            sendMessage(msg);
        }
        //============================================================================
        /// <summary>
        /// Sends a resume message to the sequencer
        /// </summary>
        //============================================================================
        protected void resumeSimulation()
        {
            interpreter.createMessage(Msgs.MSG_RESUME, sequencerID);
            sendMessage(interpreter.getMsg());
        }
        //============================================================================
        /// <summary>
        /// Overrides the function in the CompSys parent so that any child of this
        /// simulation is not qualified with "simulation."
        /// </summary>
        /// <param name="sName">Name to qualify</param>
        /// <returns></returns>
        //============================================================================
        protected override string partiallyQualifyName(string sName)
        {
            return sName;
        }
        //============================================================================
        /// <summary>
        /// Generates a unique component ID
        /// </summary>
        /// <returns>The component ID</returns>
        //============================================================================
        protected uint makeComponentID()
        {
            return ++FNewComponentID;
        }
        //============================================================================
        /// <summary>
        /// Looks through the list of child components and tries to match the szFQN.
        /// Returns the component ID if a match is found.
        /// Overrides the function in TSysComponent. Because the 'simulation' object
        /// registers it's properties in it's own registrar, I will be searching
        /// for the ID of 'simulation' in the child list but it will not be there.
        /// However I need to find the ID of the 'simulation'.
        /// </summary>
        /// <param name="sName"></param>
        /// <param name="matchFQN"></param>
        /// <returns></returns>
        //============================================================================
        protected override uint findCompID(string sName, bool matchFQN)
        {
            uint compID;
            string sCompName;

            compID = base.findCompID(sName, matchFQN);

            if (compID == UInt32.MaxValue)
            {  //if not found
                //if the name is the same name as this simulation object
                sCompName = FName;
                if (!matchFQN)
                    sCompName = TRegistrar.unQualifiedName(sCompName);

                if ((String.Compare(sName, sCompName, true) == 0) && (sCompName.Length > 0))
                {
                    compID = FMyID;
                }
            }
            return compID;
        }
        //============================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="iStage"></param>
        //============================================================================
        public abstract override void initialise(int iStage);
        //============================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="propertyID"></param>
        /// <param name="aValue"></param>
        //============================================================================
        public abstract override void initProperty(int propertyID, TTypedValue aValue);
 

    }
}
