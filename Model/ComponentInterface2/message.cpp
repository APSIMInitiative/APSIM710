#include <string.h>
#include <stdexcept>
#include <General/platform.h>

using namespace std;

#include "message.h"

static const unsigned MAX_NUM_MESSAGES = 20;
static const unsigned MAX_MESSAGE_SIZE = 15000;

#ifdef _MSC_VER
  __declspec(thread) int runningMessageID = 0;
  __declspec(thread) unsigned int nextFreeMessageID = 0;
  __declspec(thread) Message** messages = NULL;
#else
  thread_local int runningMessageID = 0;
  thread_local unsigned int nextFreeMessageID = 0;
  thread_local Message** messages = NULL;
#endif
          
void EXPORT initMessageFactory(void)
   // ------------------------------------------------------------------
   // Initialise the message factory.
   // ------------------------------------------------------------------
   {
	   if (messages == NULL) {
		   messages = new Message*[MAX_NUM_MESSAGES];
           for (unsigned messageI = 0; messageI < MAX_NUM_MESSAGES; messageI++)
             messages[messageI] = (Message*) new char[MAX_MESSAGE_SIZE];
		   runningMessageID = 0;
		   nextFreeMessageID = 0;
	   }
   }

void EXPORT shutDownMessageFactory(void)
   // ------------------------------------------------------------------
   // Shutdown the message system.
   // ------------------------------------------------------------------
   {
   nextFreeMessageID = 0;
   if (messages != NULL && messages[0] != NULL)
      {
      for (unsigned messageI = 0; messageI < MAX_NUM_MESSAGES; messageI++)
         {
         delete [] messages[messageI];
         messages[messageI] = NULL;
         }
      }
   delete[] messages;
   messages = NULL; 
   }


Message EXPORT &constructMessage(Message::Type messageType,
                                  unsigned int fromID,
                                  unsigned int toID,
                                  bool acknowledgementRequired,
                                  unsigned int numDataBytes)
   // ------------------------------------------------------------------
   // Creates a new message.
   // ------------------------------------------------------------------
   {
   Message* message;
   if (nextFreeMessageID >= MAX_NUM_MESSAGES)
      {
      throw runtime_error("Internal error: Too many messages sent from component.");
      }
   else if (numDataBytes > MAX_MESSAGE_SIZE - sizeof(Message))
      message = (Message*) new char[sizeof(Message) + numDataBytes];

   else
      {
      message = messages[nextFreeMessageID];
      nextFreeMessageID++;
      }

   message->version       = 256;
   message->messageType   =  messageType;
   message->from          = (unsigned int)fromID;
   message->to            = (unsigned int)toID;
   message->messageID     = ++runningMessageID;
   message->toAcknowledge = acknowledgementRequired;
   message->nDataBytes    = numDataBytes;
   if (numDataBytes > 0)
      message->dataPtr = ((char*)&message->dataPtr) + sizeof(char*);

   else
      message->dataPtr = NULL;
   return *message;
   }

void EXPORT deleteMessage(Message& message)
   // ------------------------------------------------------------------
   // Delete the specified message that was created by constructMessage
   // ------------------------------------------------------------------
   {
   if (message.nDataBytes > (int) (MAX_MESSAGE_SIZE - sizeof(Message)))
      delete [] &message;
   else
      nextFreeMessageID--;
   }

// ------------------------------------------------------------------
// Create a new message from the specified one and return a pointer
// to it. The caller should delete the memory when finished by
// calling deleteClonedMessage below
// ------------------------------------------------------------------
Message EXPORT * cloneMessage(const Message& from)
   {
   Message* message = (Message*) new char[sizeof(Message) + from.nDataBytes];
   memcpy(message, &from, sizeof(Message));
   message->dataPtr = ((char*)&message->dataPtr) + sizeof(char*);
   memcpy(message->dataPtr, from.dataPtr, from.nDataBytes);
   return message;
   }

// ------------------------------------------------------------------
// Delete a message that was created via the clonedMessage routine above.
// ------------------------------------------------------------------
void EXPORT deleteClonedMessage(Message* msg)
   {
   delete [] (char*) msg;
   }
   
