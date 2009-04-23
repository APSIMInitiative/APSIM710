#include <stdio.h>
#include <stdexcept>
#include <general/platform.h>

#include "message.h"

using namespace std;

static int runningMessageID = 0;

static const unsigned MAX_NUM_MESSAGES = 20;
static const unsigned MAX_MESSAGE_SIZE = 5000;
static Message* messages[MAX_NUM_MESSAGES]
   = {NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL};
unsigned nextFreeMessage = 0;


          
void EXPORT initMessageFactory(void)
   // ------------------------------------------------------------------
   // Initialise the message factory.
   // ------------------------------------------------------------------
   {
   if (messages[0] == NULL)
      {
      for (unsigned messageI = 0; messageI < MAX_NUM_MESSAGES; messageI++)
         messages[messageI] = (Message*) new char[MAX_MESSAGE_SIZE];
      nextFreeMessage = 0;
      }
   }

void EXPORT shutDownMessageFactory(void)
   // ------------------------------------------------------------------
   // Shutdown the message system.
   // ------------------------------------------------------------------
   {
   if (messages[0] != NULL)
      {
      for (unsigned messageI = 0; messageI < MAX_NUM_MESSAGES; messageI++)
         {
         delete [] messages[messageI];
         messages[messageI] = NULL;
         }
      }
   }


Message& EXPORT constructMessage(Message::Type messageType,
                                  unsigned int fromID,
                                  unsigned int toID,
                                  bool acknowledgementRequired,
                                  unsigned int numDataBytes)
   // ------------------------------------------------------------------
   // Creates a new message.
   // ------------------------------------------------------------------
   {
   Message* message;
   if (nextFreeMessage >= MAX_NUM_MESSAGES)
      {
      throw runtime_error("Internal error: Too many messages sent from component.");
      }
   else if (numDataBytes > MAX_MESSAGE_SIZE - sizeof(Message))
      message = (Message*) new char[sizeof(Message) + numDataBytes];

   else
      {
      message = messages[nextFreeMessage];
      nextFreeMessage++;
      }

   message->version       = 256;
   message->messageType   =  messageType;
   message->from          = (unsigned int)fromID;
   message->to            = (unsigned int)toID;
   message->messageID     = ++runningMessageID;
   message->toAcknowledge = acknowledgementRequired;
   message->nDataBytes    = numDataBytes;
   if (numDataBytes > 0)
      message->dataPtr = ((char*)&message->dataPtr) + 4;

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
      nextFreeMessage--;
   }

// ------------------------------------------------------------------
// Create a new message from the specified one and return a pointer
// to it. The caller should delete the memory when finished by
// calling deleteClonedMessage below
// ------------------------------------------------------------------
Message* EXPORT cloneMessage(const Message& from)
   {
   Message* message = (Message*) new char[sizeof(Message) + from.nDataBytes];
   memcpy(message, &from, sizeof(Message));
   message->dataPtr = ((char*)&message->dataPtr) + 4;
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
   
