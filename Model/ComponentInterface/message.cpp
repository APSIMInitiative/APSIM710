#include <stdio.h>
#include <stdexcept>
using namespace std;
#include "message.h"

namespace protocol {

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

// ------------------------------------------------------------------
//  Short description:
//    Initialise all static messages

//  Notes:

//  Changes:
//    dph 14/5/2001

// ------------------------------------------------------------------
void initMessages(void)
   {
	   if (messages == NULL) {
		   messages = new Message*[MAX_NUM_MESSAGES];
           for (unsigned messageI = 0; messageI < MAX_NUM_MESSAGES; messageI++)
             messages[messageI] = (Message*) new char[MAX_MESSAGE_SIZE];
		   runningMessageID = 0;;
		   nextFreeMessageID = 0;
	   }
}


// ------------------------------------------------------------------
//  Short description:
//    Delete all static messages

//  Notes:

//  Changes:
//    dph 14/5/2001

// ------------------------------------------------------------------
void deleteMessages(void)
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
   


// ------------------------------------------------------------------
//  Short description:
//    Creates a new message.

//  Notes:

//  Changes:
//    dph 14/5/2001

// ------------------------------------------------------------------
Message EXPORT * constructMessage(MessageType messageType,
                                  unsigned int fromID,
                                  unsigned int toID,
                                  bool acknowledgementRequired,
                                  unsigned int numDataBytes)
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
      message->dataPtr = ((char*)&(message->dataPtr)) + sizeof(void*);

   else
      message->dataPtr = NULL;
   return message;
   }

void EXPORT deleteMessage(Message* message)
   {
   if (message->nDataBytes > MAX_MESSAGE_SIZE - sizeof(Message))
      delete [] message;
   else
      nextFreeMessageID--;
   }


} // namespace protocol

