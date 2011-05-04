using System;
using System.Collections;
using System.Collections.Generic;

namespace CMPServices
{
    //=======================================================================
    /// <summary>
    /// TStateMachine handles a number of states and transitions.
    /// </summary>
    //=======================================================================
    public class TStateMachine
    {
        private const int MAXTRANS = 10;
        /// <summary>
        /// Idle State. This state is added during construction.
        /// </summary>
        public const int IDLE = 0;
        /// <summary>
        /// Completed State 
        /// </summary>
        public const int DONE = 10000;
        /// <summary>
        /// Failure guard (transition) condition
        /// </summary>
        public const int FAIL = MAXTRANS;// index in transitions[0..MAXTRANS]
        /// <summary>
        /// Type of state. Used to indicate that the state is
        /// used for data retrieval from another part of the system.
        /// </summary>
        public const int NONLOGIC = 0;
        /// <summary>
        /// Type of state. Used to indicate that the state is
        /// used for internal logic processing.
        /// </summary>
        public const int LOGIC = 1;
        //=======================================================================
        /// <summary>
        /// Description of a state.
        /// </summary>
        //=======================================================================
        public class TState
        {
            /// <summary>
            /// State number
            /// </summary>
            public int stateNo;
            /// <summary>
            /// logic/nonlogic
            /// </summary>
            public int stateType;
            /// <summary>
            /// execute this event number
            /// </summary>
            public uint eventID;
            /// <summary>
            /// event name to process
            /// </summary>
            public String sEventName;
            /// <summary>
            /// nested states
            /// </summary>
            public TStateMachine subStates;
            /// <summary>
            /// List of transitions (TTransition)
            /// </summary>
            internal int[] transitions = new int[MAXTRANS + 1];   //[condition] = tostate
        }
        //=======================================================================
        /// <summary>
        /// List of TStates in this TStateMachine.
        /// </summary>
        //=======================================================================
        protected TState[] stateList = new TState[DONE + 1];
        /// <summary>
        /// current state number
        /// </summary>
        protected int FCurrState;           
        /// <summary>
        /// next state number
        /// </summary>
        protected int nextEventState;       
        /// <summary>
        /// paused flag
        /// </summary>
        public bool isPaused;

        /// <summary>
        /// If watercast simulation use watercast models
        /// </summary>
        protected static bool isWaterCastSim = false;


        /// <summary>
        /// Run a WaterCast Simulation
        /// </summary>
        public static void SetWaterCastModel()
        {
            isWaterCastSim = true;
        }

        /// <summary>
        /// Obtain if the sim is a watercast model
        /// </summary>
        public static bool WaterCastModel
        {
            get { return isWaterCastSim; }
        }

        //=======================================================================
        /// <summary>
        /// Default constructor.
        /// </summary>
        //=======================================================================
        public TStateMachine()
        {
            for (int i = 0; i <= DONE; i++)
                stateList[i] = null;

            FCurrState = IDLE;     //init to beginning state
            nextEventState = IDLE; //init to something harmless

            //subclassed objects will add more states
            defineState(IDLE, 0, "Idle", LOGIC);
            defineTransition(IDLE, 0, IDLE); //loop until user inserts a state
            setCurrentState(IDLE);
        }
        //=======================================================================
        /// <summary>
        /// Copy constructor
        /// </summary>
        /// <param name="SrcMachine">The state machine to copy.</param>
        //=======================================================================
        public TStateMachine(TStateMachine SrcMachine)
        {
            //copy the state list items
            copyStates(SrcMachine.stateList);
            FCurrState = SrcMachine.FCurrState;
        }
        //============================================================================
        /// <summary>
        /// Accept the definition of a state and store it into the stateList ordered by the iState.
        /// </summary>
        /// <param name="iState">State ID.</param>
        /// <param name="iType">Logic/Non Logic state</param>
        /// <returns></returns>
        //============================================================================
        public TState defineState(int iState, int iType)
        {
            return defineState(iState, 0, "State " + iState.ToString(), iType);
        }
        //============================================================================
        /// <summary>
        /// Accept the definition of a state and store it into the stateList ordered by the iState.
        /// </summary>
        /// <param name="iState">State ID.</param>
        /// <param name="eventID">Associated event ID.</param>
        /// <param name="sEvent">Name of the associated event.</param>
        /// <param name="iType">Logic/Non Logic state</param>
        /// <returns>The new TState.</returns>
        //============================================================================
        public virtual TState defineState(int iState, uint eventID, String sEvent, int iType)
        {
            TState state = null;

            //construct a state and init the fields describing it
            if (iState >= 0)
            {
                state = new TState();
                for (int t = 0; t <= MAXTRANS; t++)    
                    state.transitions[t] = DONE;
                state.stateNo = iState;
                state.eventID = eventID;
                state.subStates = null;
                if (sEvent.Length > 0)
                {
                    state.sEventName = sEvent;
                }
                state.stateType = iType;
                stateList[iState] = state;
            }
            return state;
        }
        //============================================================================
        /// <summary>
        /// Overloaded function that allows complete replacement of any existing transitions.
        /// </summary>
        /// <param name="iState">State number: 0 &lt;= iState &lt;= n where where 0=IDLE and 99999=DONE.</param>
        /// <param name="iCondition">Guard condition.</param>
        /// <param name="iTo">State to execute next.</param>
        /// <param name="replaceAll">Replace all current transitions</param>
        //============================================================================
        public void defineTransition(int iState, int iCondition, int iTo, bool replaceAll)
        {
            if (replaceAll)
            {
                TState state = stateList[iState]; 
                if (state != null)
                {
                    for (int t = 0; t <= MAXTRANS; t++)
                        state.transitions[t] = DONE;
                }
            }
            defineTransition(iState, iCondition, iTo);
        }
        //============================================================================
        /// <summary>
        /// Define a transition for a state object.
        /// Non Logic states can only have one transition.
        /// </summary>
        /// <param name="iState">State number: 0 &lt;= iState &lt;= n where where 0=IDLE and 99999=DONE.</param>
        /// <param name="iCondition">Guard condition.</param>
        /// <param name="iTo">State to execute next.</param>
        // N.Herrmann Aug 2006
        //============================================================================
        public void defineTransition(int iState, int iCondition, int iTo)
        {
            stateList[iState].transitions[iCondition] = iTo; 
        }
        //============================================================================
        /// <summary>
        /// Set all the transitions to be defaulted to the next state in the state list.
        /// Links the last state back to the first state. This creates the loop.
        /// This is possible because the list is always in sorted order by stateNo.
        /// </summary>
        // N.Herrmann May 2003
        //============================================================================
        public void createDefaultTransitions()
        {
            int lastState = 0;
            for (int i = 1; i <= DONE; i++)
            {
                if (stateList[i] != null)
                {
                    stateList[lastState].transitions[0] = i;
                    lastState = i;
                }
            }
            stateList[lastState].transitions[0] = IDLE; //ensure that the last state links back to IDLE
        }
        //============================================================================
        /// <summary>
        /// Starts the machine by moving to the state after IDLE
        /// </summary>
        //============================================================================
        public void startMachine()
        {
            TState state = stateList[IDLE]; 
            if (state != null)
            {
                setCurrentState(state.transitions[0]); //next state
                nextEventState = FCurrState;
            }
            else
                throw (new ApplicationException("Cannot start state machine in startMachine()"));
        }
        //============================================================================
        /// <summary>
        /// Set the pause flag so that the sequencer will not execute the next state
        /// (In a synchronous environment the pause will have no effect because the
        /// system is still in an event execution and it is only when this event is
        /// complete that the sequencer has control again.)
        /// </summary>
        // N.Herrmann Aug 2001
        //============================================================================
        public void pause()
        {
            isPaused = true;
        }
        //============================================================================
        /// <summary>
        /// Set the current state.
        /// </summary>
        /// <param name="iState">State number.</param>
        /// <returns>A ref to the state being requested. NULL if not found.</returns>
        // N.Herrmann May 2003
        //============================================================================
        public TState setCurrentState(int iState)
        {
            TState state = stateList[iState];
            if (state != null)
            {
                FCurrState = iState;
            }
            return state;
        }
        //============================================================================
        /// <summary>
        /// Returns the type of the current state. 
        /// </summary>
        /// <returns>Returns the type of the current state.</returns>
        //============================================================================
        public int currentStateType()
        {
            TState state = stateList[FCurrState]; 
            if (state != null)
                return state.stateType;
            else
                return -1;  //unknown
        }

        //============================================================================
        /// <summary>
        /// Get the current state.
        /// </summary>
        /// <returns>The current state referenced by FCurrState field.</returns>
        //============================================================================
        public int getCurrentState()
        {
            return FCurrState;
        }
        //============================================================================
        /// <summary>
        /// Call the routine for processing this state.
        /// </summary>
        /// <returns>The state number to be processed next for this event.</returns>
        // N.Herrmann May 2003
        //============================================================================
        public int processCurrState()
        {
            int iGuardCondition;

            TState state = stateList[FCurrState];
            iGuardCondition = doStateActivity(state);              //execute the event state
            if (state != null)
                return state.transitions[iGuardCondition];         // look up the transition
            else
            {
                String buf = String.Format("{0} {1,2} {2} {3,2}", "Cannot find next state in processCurrState() for state:",
                                           state, "guard condition:", iGuardCondition);
                throw (new ApplicationException(buf));
            }
        }
        //============================================================================
        /// <summary>
        /// Overidden in the subclassed objects
        /// </summary>
        /// <param name="state"></param>
        /// <returns>Guard condition from the state being processed.</returns>
        // N.Herrmann May 2003
        //============================================================================
        protected virtual int doStateActivity(TState state)
        {
            return 0;
        }
        //============================================================================
        /// <summary>
        /// Copy a srcList state list into the local stateList
        /// </summary>
        /// <param name="srcStateList"></param>
        // N.Herrmann May 2001, May 2003
        //============================================================================
        protected void copyStates(TState[] srcStateList)
        {
            for (int i = 0; i <= DONE; i++)
                stateList[i] = srcStateList[i];
        }

        //============================================================================
        /// <summary>
        /// Search for the TState in the state list that has the state number = iStateNo.
        /// </summary>
        /// <param name="iStateNo">State number.</param>
        /// <returns>The TState found to correspond to iStateNo.</returns>
        // N.Herrmann May 2003
        //============================================================================
        protected TState findState(int iStateNo)
        {
            return stateList[iStateNo];
        }
        //============================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="iState"></param>
        /// <param name="state"></param>
        //============================================================================
        protected void storeState(int iState, TState state)
        {
            stateList[iState] = state;
        }
    }
}
