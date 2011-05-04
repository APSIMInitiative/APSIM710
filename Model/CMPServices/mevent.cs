using System;
using System.Collections.Generic;

namespace CMPServices
{
    //=======================================================================
    /// <summary>
    /// Each TEventsManager contains a number of events.
    /// TMEvent encapsulates the logic of a Simulation Modelling Component event.
    /// Each of these events can be in a number of states. This is why TMEvent is
    /// descended from a TStateMachine. An TMEvent may be a logic event or non-logic
    /// event. Logic events can have a number of states that could be branched to
    /// to any of the next states depending on the result value from the state
    /// that has just been processed. This is what the transition list in the
    /// TStateMachine is used for.
    /// </summary>
    //=======================================================================
    internal class TMEvent : TStateMachine
    {
        private const bool SYNCHRONOUS = true;
        private TDDMLValue FParams;      //DDML value parameters for this event
        private string eventName;      //Name of this event
        private int eventID;           //Local event ID
        private TAbstractComponent parentComp;  //points to the owning TAbstractComponent
        private TEventsManager parentMgr;      //points to the owning TEventsManager
        private List<uint> requestList; //list of outstanding requests made during this event process
        private bool waiting;          //waiting state flag

        /// <summary>
        /// Component ID of the publisher
        /// </summary>
        public uint iPublisher;          
        public bool bNotify;
        public uint iNotifyTo;
        public uint iNotifyMsg;

        //=======================================================================
        /// <summary>
        /// Construct a TMEvent.
        /// </summary>
        /// <param name="parentC">owning TBaseComp</param>
        /// <param name="parentM">owning TEventsManager</param>
        /// <param name="sName">Event name</param>
        /// <param name="iEventID">Event ID as stored in the local component.</param>
        //=======================================================================
        public TMEvent(Object parentC, Object parentM, string sName, int iEventID)
            : base()
        {
            eventName = sName;
            eventID = iEventID;
            parentComp = (TAbstractComponent)parentC;
            parentMgr = (TEventsManager)parentM;

            requestList = new List<uint>();

            //add a DONE state so that when this 'end of loop' is executed, a Complete msg is sent
            base.defineState(DONE, 0, "Done", LOGIC);
            defineTransition(DONE, 0, IDLE);
            createDefaultTransitions();

            waiting = false;             //not waiting for msg returns
        }

        //=======================================================================
        /// <summary>
        /// A copy constructor for the TMEvent.
        /// </summary>
        /// <param name="anEvent">Event to copy</param>
        //=======================================================================
        public TMEvent(TMEvent anEvent)
            : base(anEvent)
        {
            requestList = new List<uint>();
            //copy all the attributes of 'event'
            eventName = anEvent.eventName;
            parentComp = anEvent.parentComp;
            parentMgr = anEvent.parentMgr;
            eventID = anEvent.eventID;
            FParams = anEvent.FParams;     //should I be making a copy? (probably not. It is owned by the comp)
            waiting = false;             //not waiting for msg returns
            nextEventState = anEvent.nextEventState;
        }

        //============================================================================
        /// <summary>
        /// Begins the event processing. Assumes that the event is IDLE.
        /// </summary>
        /// <param name="evParams">Parameters that have been sent for this event.</param>
        //============================================================================
        public void beginEvent(TDDMLValue evParams)
        {
            if (getCurrentState() == IDLE)
            {
                FParams = evParams;               //store the event parameters from the doEvent --who owns this? (the comp)

                startMachine();                    //set currentState to the next state
                nextEventState = getCurrentState();  //set the state to resume from

                resume();                         //restarts the state processing at the nextEventState
            }
            else
            {
                throw (new ApplicationException("Error in beginEvent(). You cannot begin a running event!"));
            }
        }

        //============================================================================
        /// <summary>
        /// Restarts the state processing at the nextEventState.
        /// </summary>
        //============================================================================
        public void resume()
        {
            if (SYNCHRONOUS)
            {
                while ((getCurrentState() != IDLE))
                {
                    nextEventState = processCurrState();
                    setCurrentState(nextEventState);
                }
                if (getCurrentState() == DONE)
                    setCurrentState(IDLE);
            } /*
         else 
         { 
            if (nextEventState != IDLE) 
            {
               do 
               {
                  setCurrentState( nextEventState );
                  nextEventState = processCurrState();

                  //Don't allow states that are non logic to automatically go to the next state
                  //Non logic states include requests that need to wait till a return comes
                  //back from the system. This is handled in the message handling of the
                  //component.
               } while ( (nextEventState != IDLE) && (currentStateType() != NONLOGIC) );

               if (nextEventState == IDLE)
                  setCurrentState(IDLE);
            }
         } */
        }
        //============================================================================
        /// <summary>
        /// Sets the event state to IDLE.
        /// </summary>
        //============================================================================
        protected void endEvent()
        {
            setCurrentState(IDLE);
        }
        //============================================================================
        /// <summary>
        /// Get event ID.
        /// </summary>
        /// <returns>The event ID of this object.</returns>
        //============================================================================
        public int getEventID()
        {
            return eventID;
        }
        //============================================================================
        /// <summary>
        /// Executes a state for this event.
        /// </summary>
        /// <param name="state">State to be processed.</param>
        /// <returns>Guard condition from the state being processed..</returns>
        //============================================================================
        protected override int doStateActivity(TState state)
        {
            uint termMsgID = 0;
            uint termMsgFrom = 0;
            int guardCondition;
            int result = -9;  //invalid

            try
            {
                if (state.stateNo == DONE)
                {
                    if (bNotify)                              //Acknowledge completion of this event
                        parentComp.sendComplete(iNotifyTo, iNotifyMsg);

                    // If the events manager has been notified that the simulation is
                    if (parentMgr.isToTerminate())
                    {
                        parentMgr.getTerminationMsgDetails(ref termMsgFrom, ref termMsgID);  //terminating, acknowledge the
                        parentComp.sendComplete(termMsgFrom, termMsgID);             //notifyTerminate message here  
                    }
                    result = 0;                                                    //Always return 0: DONE -> IDLE
                }
                else
                {    //call the routine from the parentComp
                    guardCondition = parentComp.processEventState(eventID, state.stateNo, iPublisher, FParams);
                    result = guardCondition;

                }
            }
            catch (Exception)
            {
                setCurrentState(TStateMachine.DONE);
                throw;                    //reraise this exception
            }

            return result;
        }
        //============================================================================
        /// <summary>
        /// Stores a request item. This is used to keep track of outgoing requests
        /// that are being made during this event processing.
        /// </summary>
        /// <param name="msgID">Message ID of the request</param>
        //============================================================================
        public void addRequest(uint msgID)
        {
            requestList.Add(msgID);

            waiting = true;
        }
        //============================================================================
        /// <summary>
        /// Used to decrement the count of system requests awaiting returns.
        /// </summary>
        /// <param name="msgID">Message ID to be removed from the request list.</param>
        /// <returns>The number of requests remaining in the request list.</returns>
        //============================================================================
        public int completeRequest(uint msgID)
        {
            bool found = false;

            //remove the request from the list
            int i = 0;
            while ((!found) && (i < requestList.Count))
            {
                if (requestList[i] == msgID)
                {
                    requestList.RemoveAt(i);
                    found = true;
                }
                else
                    i++;
            }

            if (requestList.Count == 0)    //if the list is empty then
                waiting = false;             //the event is not waiting

            return requestList.Count;
        }

        //============================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <returns>The waiting flag value.</returns>
        //============================================================================
        public bool IsWaiting()
        {
            return waiting;
        }
        //============================================================================
        /// <summary>
        /// Set the state of the waiting flag.
        /// </summary>
        /// <param name="condition">True or false.</param>
        //============================================================================
        public void setWaiting(bool condition)
        {
            waiting = condition;
        }

    }
}
