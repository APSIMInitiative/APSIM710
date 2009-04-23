#include <stdlib.h>

#include "Transport.h"
#include <ComponentInterface/Interfaces.h>
#include <ComponentInterface/message.h>

#include <ComponentInterface/MessageDataExt.h>
#include <general/platform.h>
using namespace protocol;

namespace protocol {
Transport transport;    // singleton global transport layer.
}

// ------------------------------------------------------------------
// Return the singleton global transport layer.
// ------------------------------------------------------------------
Transport& EXPORT Transport::getTransport(void)
   {
   return transport;
   }
// ------------------------------------------------------------------
//  Short description:
//    add the specified component to the address book so that
//    we can send messages to that component later.

//  Notes:

//  Changes:
//    dph 10/5/2001

// ------------------------------------------------------------------
void Transport::addComponent(unsigned int id,
                             const std::string& name,
                             IComputation* computation)
   {
   for (unsigned int i = addressBook.size(); i <= id; i++)
      {
      addressBook.push_back(NULL);
      }
   addressBook[id] = computation;
   }
// ------------------------------------------------------------------
//  Short description:
//     Deliver the specified message to the specified component,
//     and then go looking for other messages to deliver.

//  Notes:

//  Changes:
//    dph 10/5/2001

// ------------------------------------------------------------------
void Transport::deliverMessage(Message* message)
   {

   if (messageHook != NULL)
      messageHook->callback(message);

   addressBook[message->to]->messageToLogic(message);

   if (messageHook != NULL)
      messageHook->callback(NULL);

   }



// ------------------------------------------------------------------
//  Short description:
//     Hook the transport message flow.

//  Notes:

//  Changes:
//    dph 10/5/2001

// ------------------------------------------------------------------
namespace protocol {
void EXPORT setMessageHook(IMessageHook* hook)
   {
   transport.setMessageHook(hook);
   }
}

