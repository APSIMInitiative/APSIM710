//---------------------------------------------------------------------------
#ifndef MessageH
#define MessageH

#include <General/platform.h>
// ------------------------------------------------------------------
//  Short description:
//    This class encapsulates a protocol message.

//  Notes:

//  Changes:
//    dph 22/2/2000
//    dph 20/4/2001 made protocol 1.0 compliant

// ------------------------------------------------------------------
struct Message
   {
   enum Type
      {
      ActivateComponent = 1,
      AddComponent = 2,
      Error = 3,
      Commence = 4,
      Complete = 5,
      DeactivateComponent = 6,
      DeleteComponent = 7,
      Deregister = 8,
      Event = 9,
      GetValue = 10,
      Init1 = 11,
      Init2 = 12,
      NotifyAboutToDelete = 13,
      NotifyRegistrationChange = 14,
      NotifySetValueSuccess = 15,
      NotifyTermination = 16,
      PauseSimulation = 17,
      PublishEvent = 18,
      QueryInfo = 19,
      QuerySetValue = 20,
      QueryValue = 21,
      Register = 22,
      ReinstateCheckpoint = 23,
      ReplySetValueSuccess = 24,
      ReplyValue = 25,
      RequestComponentID = 26,
      RequestSetValue = 27,
      ResumeSimulation = 28,
      ReturnComponentID = 29,
      ReturnInfo = 30,
      ReturnValue = 31,
      TerminateSimulation = 32,

      ApsimGetQuery = 40,
      ApsimSetQuery = 41,
      ApsimChangeOrder = 42,
      };

   short int version;
   short int messageType;
   int from;
   int to;
   int messageID;
   int toAcknowledge;
   int nDataBytes;
   char* dataPtr;
   };



// ------------------------------------------------------------------
// Initialise the message factory.
// ------------------------------------------------------------------
void EXPORT initMessageFactory(void);

// ------------------------------------------------------------------
// Shutdown the message factory.
// ------------------------------------------------------------------
void EXPORT shutDownMessageFactory(void);

// ------------------------------------------------------------------
// Creates a new message that can hold a given number of bytes for data.
// ------------------------------------------------------------------
Message EXPORT & constructMessage(Message::Type messageType,
                          unsigned int fromID,
                          unsigned int toID,
                          bool acknowledgementRequired,
                          unsigned int numDataBytes);

// ------------------------------------------------------------------
// Delete the specified message that was created by constructMessage
// ------------------------------------------------------------------
void EXPORT deleteMessage(Message& message);

// ------------------------------------------------------------------
// Create a new message from the specified one and return a pointer
// to it. The caller should delete the memory when finished by
// calling deleteClonedMessage below
// ------------------------------------------------------------------
Message EXPORT * cloneMessage(const Message& from);

// ------------------------------------------------------------------
// Delete a message that was created via the cloneMessage routine above.
// ------------------------------------------------------------------
void EXPORT deleteClonedMessage(Message* msg);


#endif
