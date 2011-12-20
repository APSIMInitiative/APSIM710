#include <stdio.h>
#include <stdexcept>
#include <boost/thread/tss.hpp>
using namespace std;
#include "message.h"

namespace protocol {

static const unsigned MAX_NUM_MESSAGES = 20;
static const unsigned MAX_MESSAGE_SIZE = 15000;

void freeMsgPtr(Message** aMsgPtr) {delete [] aMsgPtr;} // Requires use of array delete
boost::thread_specific_ptr<int> runningMessageIDPtr;
boost::thread_specific_ptr<unsigned> nextFreeMessagePtr;
boost::thread_specific_ptr<Message*> msgPtr(freeMsgPtr);
// ------------------------------------------------------------------
//  Short description:
//    Initialise all static messages

//  Notes:

//  Changes:
//    dph 14/5/2001

// ------------------------------------------------------------------
void initMessages(void)
   {
	   if (msgPtr.get() == 0) {
		   msgPtr.reset(new Message*[MAX_NUM_MESSAGES]);
		   Message** messages = msgPtr.get();
           for (unsigned messageI = 0; messageI < MAX_NUM_MESSAGES; messageI++)
             messages[messageI] = (Message*) new char[MAX_MESSAGE_SIZE];
		   runningMessageIDPtr.reset(new int(0));
		   nextFreeMessagePtr.reset(new unsigned(0));
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
   Message** messages = msgPtr.get();
   *nextFreeMessagePtr = 0;
   if (messages != NULL && messages[0] != NULL)
      {
      for (unsigned messageI = 0; messageI < MAX_NUM_MESSAGES; messageI++)
         {
         delete [] messages[messageI];
         messages[messageI] = NULL;
         }
      }
   msgPtr.reset(NULL); 
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
   Message** messages = msgPtr.get();
   unsigned & nextFreeMessage = *nextFreeMessagePtr;
   int & runningMessageID = *runningMessageIDPtr;
   if (nextFreeMessage > MAX_NUM_MESSAGES)
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
   return message;
   }

void EXPORT deleteMessage(Message* message)
   {
   unsigned & nextFreeMessage = *nextFreeMessagePtr;
   if (message->nDataBytes > MAX_MESSAGE_SIZE - sizeof(Message))
      delete [] message;
   else
      nextFreeMessage--;
   }


} // namespace protocol

