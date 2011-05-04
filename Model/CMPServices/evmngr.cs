using System;
using System.Collections;

namespace CMPServices
{
   //============================================================================
   /// <summary>
   /// Each TBaseComp will have a TEventsManager. The Events Manager is
   /// used to initialise and coordinate the state machine that handles the event
   /// processing.
   /// </summary>
   //============================================================================
   internal class TEventsManager
   {
      private Object parentComp;
      private ArrayList eventList;       //list of ptrs to TEvents
      private ArrayList requestList;     //requests made to the system for this (logic) event
      private uint terminateMsgFrom;
      private uint terminateMsgID;
      private bool needToTerminate;
      
      private struct RequestRecord
      {
         public int eventID;      //Event ID of current process
         public uint msgID;       //Outgoing msgID
      }
      //============================================================================
      /// <summary>
      /// Constructor
      /// </summary>
      /// <param name="parentComponent">Pointer to the parent component (TBaseComp).</param>
      //============================================================================
      public TEventsManager(Object parentComponent)
      {
         eventList = new ArrayList();
         requestList = new ArrayList();
         parentComp = parentComponent;

         needToTerminate = false;  //do not terminate yet
      }
      //============================================================================
      /// <summary>
      /// Starts an execution of an event. If an event object exists and is idle, it
      /// is started. To allow for threaded processing, a new event object will be
      /// created if none are found to be idle.
      /// </summary>
      /// <param name="iRegID">ID of the event.</param>
      /// <param name="iSender">Component ID requesting the event.</param>
      /// <param name="iPublisher">Component that published this event.</param>
      /// <param name="iMsgID">Message ID of this event msg.</param>
      /// <param name="dataParams">Param data.</param>
      /// <param name="bNotify"> </param>
      //============================================================================
      public void beginEvent(int iRegID, uint iSender, uint iPublisher, uint iMsgID, TDDMLValue dataParams, bool bNotify)
      {
         bool found = false;
         TMEvent anEvent = null;
         TMEvent eventToCopy = null;
         
         string errorMsg;

         int i = 0;
         //look through the list of defined events to see if one is available to run
         while ( (!found) && (i < eventList.Count) ) 
         {
            anEvent = (TMEvent)eventList[i];
            if ((uint)(anEvent.getEventID()) == iRegID)    //if the event is the correct type
            {               
               found = (anEvent.getCurrentState() == TStateMachine.IDLE);     //if it is idle, use this event
               if (!found)
                  eventToCopy = anEvent;
            }
            i++;
         }

         //create a copy of a non idle event if necessary
         if (!found) 
         {
            if (eventToCopy == null) 
            {
               errorMsg = String.Format("No event registered for {0}", iRegID);
               throw (new ApplicationException(errorMsg));
            }
            anEvent = new TMEvent(eventToCopy);
         }

         anEvent.iPublisher = iPublisher;
         anEvent.bNotify = bNotify;          // set up info for later acknowledgement
         anEvent.iNotifyTo = iSender;
         anEvent.iNotifyMsg = iMsgID;
         anEvent.beginEvent(dataParams);     //now activate the event
      }

      //============================================================================
      /// <summary>
      /// Used by the TBaseComp to define the states belonging to an event.
      /// </summary>
      /// <param name="iRegID">Event ID.</param>
      /// <param name="iState">State number.</param>
      /// <param name="iType">Type of state - LOGIC/NONLOGIC</param>
      //============================================================================
      public void defineEventState(int iRegID, int iState, int iType)
      {
         int i;
         TMEvent anEvent;

         //check if the event has been created
         i = eventIdx(iRegID);
         if (i > -1)    //if the event is defined then
         {  
            anEvent = (TMEvent)eventList[i];
            anEvent.defineState(iState, 0, "", iType);
         }
      }

      //============================================================================
      /// <summary>
      /// Used by the TBaseComp to define the transitions belonging to an event.
      /// </summary>
      /// <param name="iRegID">Event ID.</param>
      /// <param name="iState">State number.</param>
      /// <param name="iResult">Guard condition.</param>
      /// <param name="iTo">State number to transit to.</param>
      /// <param name="replaceAll">Replace all current transitions</param>
      //============================================================================
      public void defineEventTransition(int iRegID, int iState, int iResult, int iTo, bool replaceAll)
      {
         int i;
         TMEvent anEvent;

         //check if the event has been created
         i = eventIdx(iRegID);
         if (i > -1) 
         {  //if the event is defined then
            anEvent = (TMEvent)eventList[i];
            anEvent.defineTransition(iState, iResult, iTo, replaceAll);
         }
      }
      //============================================================================
      /// <summary>
      /// Adds a new event to the this event manager's list.
      /// </summary>
      /// <param name="sName">Name of the event.</param>
      /// <param name="iRegID">Event ID.</param>
      //============================================================================
      public void addEventType(string sName, int iRegID)
      {
         TMEvent anEvent;

         anEvent = new TMEvent(parentComp, this, sName, iRegID);
         eventList.Add(anEvent);
      }
      //============================================================================
      /// <summary>
      /// Find the event index in the local array of events.
      /// </summary>
      /// <param name="eventID"></param>
      /// <returns>The event index in the array. -1 => not found</returns>
      //============================================================================
      private int eventIdx(int eventID)
      {
         int result = -1;  //init to not found

         //check if the event has been created
         bool found = false;
         int i = 0;
         while ( (!found) && (i <= eventList.Count - 1) ) 
         {
            if ( ((TMEvent)eventList[i]).getEventID() == eventID) 
            {
               found = true;
               result = i;
            }
            else
               i++;
         }
         return result;
      }

      //============================================================================
      /// <summary>
      /// Tracks the requests made to the rest of the system.
      /// </summary>
      /// <param name="eventID">Event ID.</param>
      /// <param name="msgID">Message ID.</param>
      //============================================================================
      public void addRequest(int eventID, uint msgID)
      {
         RequestRecord request;
         int eventIndex;

         request = new RequestRecord();
         request.eventID = eventID;
         request.msgID = msgID;
         requestList.Add(request);

         //add the msgID to the request list of the event
         eventIndex = eventIdx(eventID);
         if (eventIndex >= 0)
         {
            TMEvent anEvent = (TMEvent)eventList[eventIndex];
            anEvent.addRequest(msgID);
            eventList[eventIndex] = anEvent;
         }
      }

      //============================================================================
      /// <summary>
      /// Informs the event that sent the msgID, that a return has occured. The
      /// number of remaining requests required by the event is checked. If no returns
      /// are still pending then the event is resumed.
      /// </summary>
      /// <param name="msgID">Message ID.</param>
      //============================================================================
      public void completeRequest(uint msgID)
      {
         int anEventIdx;
         bool found;
         int remainingRequests;

         //scan through the request list and find the event that triggered it
         int i = 0;
         found = false;
         while ( (!found) && (i < requestList.Count) ) 
         {
            RequestRecord request = (RequestRecord)requestList[i];
            if (request.msgID == msgID) 
            {                            //if the msg request if found
               found = true;
               anEventIdx = eventIdx(request.eventID);                   //get the TMEvent belonging to the request
               if (anEventIdx >= 0) 
               {
                  TMEvent anEvent = (TMEvent)eventList[anEventIdx];

                  //now delete the request from the the Event Manager's requestList
                  requestList.RemoveAt(i);
                  remainingRequests = anEvent.completeRequest(msgID);    //tell the event that the request is complete
               if (remainingRequests < 1) //if there are no pending requests then
                  {                         
                     //event.resumeEvent;                                    //resume the event
                     anEvent.setWaiting(false);
                  }
                  eventList[anEventIdx] = anEvent;
               }
            }
            else
            {
               i++;        //next event
            }
         }
      }
      //============================================================================
      /// <summary>
      /// Tell this event manager to terminate.
      /// </summary>
      /// <param name="termMsgFrom">Component ID of requester.</param>
      /// <param name="termMsgID">Message ID of the termination message.</param>
      /// <returns>Returns True if the events manager is ready to terminate.</returns>
      //============================================================================
      public bool reqTerminate(uint termMsgFrom, uint termMsgID)
      {
         bool sendCompleteNow;
         
         int i;

         //return the values stored from the notifyTerminate
         terminateMsgID = termMsgID;
         terminateMsgFrom = termMsgFrom;

         needToTerminate = true;
         // this means that when it arrives at the DONE state it should send
         //a complete(notifyTermination) and not respond to any more execution events

         //if this event manager has no active events (all DONE) then
         //send the complete from here.

         //look through the list of events. If all of them are DONE then send the complete()
         sendCompleteNow = true;
         i = 0;
         int anEventIdx = eventIdx(i);
         while (anEventIdx >= 0) 
         {
            //keep checking if any are not DONE
            sendCompleteNow = (sendCompleteNow && (((TMEvent)eventList[i]).getCurrentState() == TStateMachine.IDLE) );
            i++;
            anEventIdx = eventIdx(i);
         }

         return sendCompleteNow;
      }

      //============================================================================
      /// <summary>
      /// Returns the state of this state machine. If this events manager has received
      /// a notifyTerminate then this will return True.
      /// </summary>
      /// <returns>True if this events manager has been told to terminate.</returns>
      //============================================================================
      public bool isToTerminate()
      {
         return needToTerminate;

      }
      //============================================================================
      /// <summary>
      /// Return the values stored from the notifyTerminate message
      /// </summary>
      /// <param name="termMsgFrom">Component ID of the requester.</param>
      /// <param name="termMsgID">Message ID of the terminate message.</param>
      //============================================================================
      public void getTerminationMsgDetails(ref uint termMsgFrom, ref uint termMsgID)
      {
         termMsgID = terminateMsgID;
         termMsgFrom = terminateMsgFrom;

      }

	}
}
