using System;
using System.Collections;

namespace CMPServices
{
   //=======================================================================
   /// <summary>
   /// TCheckPointProcess is an item that describes an individual checkpoint process.
   /// </summary>
   //=======================================================================
   internal struct TCheckPointProcess 
   {
      /// <summary>
      /// Id Of The Driving Variable For This Process
      /// </summary>
      public int stateDriverID;        
      /// <summary>
      /// FQName of the driving variable
      /// </summary>
      public string stateDriver;       
      /// <summary>
      /// Msg Type For The Following Msg
      /// </summary>
      public uint msgType;             
      /// <summary>
      /// Msg Id Of The Pause Message
      /// </summary>
      public uint msgID;               
      /// <summary>
      /// The SDML value of the setter property in the restore process
      /// </summary>
      public string propertyValue;     
      /// <summary>
      /// True if this is a restore checkpoint process
      /// </summary>
      public bool isRestoreProcess;    
   }
   //=======================================================================
   /// <summary>
   /// Summary description for chktracer.
   /// Declaration of the TCheckPointTracer class. This object is used to track
   /// the checkpoint process.
   /// <b>Assumptions:</b>
   ///              Each process is uniquely identified by the driving property
   ///              that will be set. e.g. Only one checkpoint will be done at
   ///              any one time on a component/system.
   ///              More than one checkpoint could be underway at any one time.
   ///              Message ID's are unique within this component.
   /// </summary>
   //=======================================================================
   internal class TCheckPointTracer
   {
      protected ArrayList checkPointList;

      public TCheckPointTracer()
      {
         checkPointList = new ArrayList();
      }

      //==============================================================================
      /// <summary>
      /// Stores a msg ID of one of the messages in the checkpoint process.
      /// a checkpoint process is begining.
      /// </summary>
      /// <param name="msgID">Message ID of the message used when checkpointing.</param>
      /// <param name="msgType">type of message.</param>
      /// <param name="driverName">Name of the driving property.</param>
      /// <param name="restoring">True if this is a restore process.</param>
      /// <returns>The TCheckPointProcess item that was added to the list.</returns>
      //==============================================================================
      public TCheckPointProcess logCheckPoint(uint msgID, uint msgType, string driverName, bool restoring)
      {
         TCheckPointProcess checkPoint = new TCheckPointProcess();

         checkPoint.stateDriverID = 9999999;
         checkPoint.stateDriver = driverName;
         checkPoint.msgID = msgID;
         checkPoint.msgType = msgType;
         checkPoint.isRestoreProcess = restoring;
         checkPointList.Add(checkPoint);

         return checkPoint;
      }

      //==============================================================================
      /// <summary>
      /// Stores a msg ID of one of the messages in the checkpoint restore process.
      /// </summary>
      /// <param name="msgID">Message ID.</param>
      /// <param name="msgType">Type of the message.</param>
      /// <param name="driverName">Name of the driver.</param>
      /// <param name="driverValue">Used to store the checkpoint SDML string that is used
      /// to restore the component.</param>
      /// <returns>The TCheckPointProcess item that was added to the list.</returns>
      //==============================================================================
	  public TCheckPointProcess logCheckPointRestore(uint msgID, uint msgType, string driverName, string driverValue)
      {
		 TCheckPointProcess checkPoint = new TCheckPointProcess();

		 checkPoint.stateDriverID = 9999999;
		 checkPoint.stateDriver = driverName;
		 checkPoint.msgID = msgID;
		 checkPoint.msgType = msgType;
		 checkPoint.isRestoreProcess = true;
		 checkPoint.propertyValue = driverValue;
		 checkPointList.Add(checkPoint);

		 return checkPoint;
	  }
	  //==============================================================================
	  /// <summary>
	  /// Get the message type of the message.
	  /// </summary>
	  /// <param name="msgID">Message ID.</param>
	  /// <returns>The type of message. See <see cref="Msgs"/> class</returns>
	  //==============================================================================
	  public uint getMsgType(uint msgID)
	  {
         uint msgType;
         TCheckPointProcess checkPoint = new TCheckPointProcess();
         
         msgType = 0;   //init to invalid msg type

         if (getProcess(msgID, ref checkPoint))
         {
            msgType = checkPoint.msgType;
         }

         return msgType;
      }

      //==============================================================================
      /// <summary>
      /// Looks through the checkpoint list to see if this msgID is part of a
      /// checkpoint process.
      /// </summary>
      /// <param name="msgID">Key used to search the list.</param>
      /// <returns>Returns True if the message ID is found.</returns>
      //==============================================================================
      public bool isCheckPointMsg(uint msgID)
      {
         TCheckPointProcess checkPoint;
         bool bFound;
         int i;
         
         bFound = false;

         i = 0;
         while ( (!bFound) && (i != checkPointList.Count) ) 
         {
            checkPoint = (TCheckPointProcess)checkPointList[i];
            if ( (checkPoint.msgID == msgID) && (!checkPoint.isRestoreProcess) )
            {
               bFound = true;
            }
            else {
               i++;
            }
         }

         return bFound;
      }

      //==============================================================================
      /// <summary>
      /// Looks through the checkpoint list to see if this msgID is part of a
      /// checkpoint restore process.
      /// </summary>
      /// <param name="msgID">Key used to search the list.</param>
      /// <returns>True if the message ID is found.</returns>
      //==============================================================================
      public bool isCheckPointRestoreMsg(uint msgID) 
      {
         TCheckPointProcess checkPoint;
         bool bFound;
         int i;

         bFound = false;

         i = 0;
         while ( (!bFound) && (i != checkPointList.Count) ) 
         {
            checkPoint = (TCheckPointProcess)checkPointList[i];
            if ( (checkPoint.msgID == msgID) && (checkPoint.isRestoreProcess) ) 
            {
               bFound = true;
            }
            else 
            {
               i++;
            }
         }

         return bFound;
      }

      //==============================================================================
      /// <summary>
      /// Looks through the checkpoint list to see if this driver is being used in a
      /// checkpoint process.
      /// </summary>
      /// <param name="driverID">Driving variable ID.</param>
      /// <returns>Returns True if this driver is being checkpointed now.</returns>
      //==============================================================================
      public bool isCheckPointing(int driverID)
      {
         TCheckPointProcess checkPoint;
         bool bFound;
         int i;

         bFound = false;
         i = 0;
         while ( (!bFound) && (i < checkPointList.Count) ) 
         {
            checkPoint = (TCheckPointProcess)checkPointList[i];
            if (checkPoint.stateDriverID == driverID) 
            {
               bFound = true;
            }
            else 
            {
               i++;
            }
         }
         return bFound;
      }

      //==============================================================================
      /// <summary>
      /// Finds the driving property ID associated with the msgID.
      /// </summary>
      /// <param name="msgID">Key used to search the list.</param>
      /// <returns>If the checkpoint process for msgID is not found, the return value is 9999999. 
      /// If found, the return value will be the driving property ID.</returns>
      //==============================================================================
      public int getDriverID(uint msgID) 
      {
         TCheckPointProcess checkPoint = new TCheckPointProcess();
         int driverID;

         driverID = 9999999;

         if (getProcess(msgID, ref checkPoint))
         {
            driverID = checkPoint.stateDriverID;
         }
         return driverID;
      }

      //==============================================================================
      /// <summary>
      /// Get the name of the driving variable.
      /// </summary>
      /// <param name="msgID">Key used to search the list.</param>
      /// <returns></returns>
      //==============================================================================
      public string getDriver(uint msgID) 
      {
         TCheckPointProcess checkPoint = new TCheckPointProcess();

         string result = "";

         if (getProcess(msgID, ref checkPoint)) 
         {
            result = checkPoint.stateDriver;
         }
         return result;
      }

      //==============================================================================
      /// <summary>
      /// Find the value for the property used in the checkpoint (restore) process.
      /// </summary>
      /// <param name="msgID">Key used to search the list.</param>
      /// <returns>The property value name stored for this checkpoint restore process.</returns>
      //==============================================================================
      public string getValue(uint msgID) 
      {
         TCheckPointProcess checkPoint = new TCheckPointProcess();
      
         string result = "";

         if (getProcess(msgID, ref checkPoint))
         {
            result = checkPoint.propertyValue;
         }
         return result;
      }

      //==============================================================================
      /// <summary>
      /// Protected function to find the checkpoint process by msgID used.
      /// </summary>
      /// <param name="msgID">Key used to search the list.</param>
      /// <param name="checkPointFound">The checkpoint found.</param>
      /// <returns>The checkpoint process.</returns>
      //==============================================================================
      protected bool getProcess(uint msgID, ref TCheckPointProcess checkPointFound) 
      {
         TCheckPointProcess checkPoint;
         bool bFound = false;
         int i;

         i = 0;
         while ( (!bFound) && (i < checkPointList.Count) ) 
         {
            checkPoint = (TCheckPointProcess)checkPointList[i];
            if (checkPoint.msgID == msgID) 
            {
               checkPointFound = checkPoint;
               bFound = true;
            }
            else 
            {
               i++;
            }
         }

         return bFound;
      }

      //==============================================================================
      /// <summary>
      /// Identifies the checkpoint process by driverID and sets the associated
      /// message ID to msgID.
      /// </summary>
      /// <param name="msgID">ID of the msg to find.</param>
      /// <param name="newMsgID">ID of the message that replaces msgID.</param>
      /// <param name="msgType">Type of message to store.</param>
      /// <param name="driverID">Driver ID to store. Key used to find the checkpoint process item.</param>
      //==============================================================================
      public void updateMsg(uint msgID, uint newMsgID, uint msgType, int driverID) 
      {
         TCheckPointProcess checkPoint;

         bool bFound = false;
         int i;

         i = 0;
         while ( (!bFound) && (i < checkPointList.Count) ) 
         {
			checkPoint = (TCheckPointProcess)checkPointList[i];
            if (checkPoint.msgID == msgID) 
			{
			   checkPoint.msgID = newMsgID;
               checkPoint.stateDriverID = driverID;
			   checkPoint.msgType = msgType;
			   checkPointList[i] = checkPoint;   //store the changed values
               bFound = true;
            }
            else 
            {
               i++;
            }
         }
      }
      //==============================================================================
      /// <summary>
      /// Removes the checkpoint process from the list of checkpoints.
      /// - driverID Key used to find the checkpoint process item.
      /// </summary>
      /// <param name="driverID"></param>
      //==============================================================================
      public void clearDriverProcess(int driverID) 
      {
         TCheckPointProcess checkPoint;
         bool bFound;
         int i;

         bFound = false;
         i = 0;
         while ( (!bFound) && (i < checkPointList.Count) ) 
         {
            checkPoint = (TCheckPointProcess)checkPointList[i];
            if (checkPoint.stateDriverID == driverID) 
            {
               bFound = true;
               checkPointList.Remove(i);
            }
            else 
            {
               i++;
            }
         }
      }

      //==============================================================================
      /// <summary>
      /// Removes the checkpoint process from the list of checkpoints.
      /// </summary>
      /// <param name="msgID">Key used to find the checkpoint process item.</param>
      //==============================================================================
      public void clearMsgProcess(uint msgID) 
      {
         TCheckPointProcess checkPoint;
         bool bFound;
         int i;
         
         bFound = false;
         i = 0;
         while ( (!bFound) && (i < checkPointList.Count) ) 
         {
            checkPoint = (TCheckPointProcess)checkPointList[i];
            if (checkPoint.msgID == msgID) 
            {
               bFound = true;
               checkPointList.Remove(i);
            }
            else 
            {
               i++;
            }
         }
      }
	}
}
