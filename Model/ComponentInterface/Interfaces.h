#ifndef InterfacesH
#define InterfacesH

//#include "IConfiguration.h"
#include <ComponentInterface/MessageData.h>

#include <stdexcept>
#include <string>

namespace protocol {

class Message;
class EXPORT IMessageHook
   {
   public:
      virtual ~IMessageHook() {}
      virtual void callback(const protocol::Message* message) = 0;
   };

// ------------------------------------------------------------------
//  Short description:
//    Interface for a simulation.  This is the top-level starting point
//    for all simulations.

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
class EXPORT ISimulation
   {
   public:
      virtual ~ISimulation(void) { };

      // go execute the simulation
      virtual void go(const std::string& simFilename) = 0;

      // set a hook into the message stream.
      virtual void setMessageHook(IMessageHook* hook) = 0;

   };


// ------------------------------------------------------------------
//  Short description:
//    Interface for a computation.  A computation is looks after
//    all communications with a component executable.  In APSIM
//    terms this is a DLL.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
class EXPORT IComputation
   {
   public:
      virtual ~IComputation(void) { };

      virtual bool isOk(void) const = 0;
      virtual void messageToLogic(const Message* Message) const = 0;
      virtual std::string getExecutable(void) = 0;
   };
// ------------------------------------------------------------------
//  Short description:
//     Encapsulates the transport layer for the simulation.  Its sole
//     purpose is to deliver messages to components.  The transport
//     layer has no knowledge of the simulation structure.

//  Changes:
//    dph 10/5/2001

// ------------------------------------------------------------------
class EXPORT ITransport
   {
   public:
      virtual ~ITransport(void) { }
      virtual void deliverMessage(Message* message) = 0;
      virtual void addComponent(unsigned int id,
                                const std::string& name,
                                IComputation* computation) = 0;
      virtual void setMessageHook(IMessageHook* hook) = 0;
   };

class EXPORT IData
   {
   // -----------------------------------------------------------
   // This interface describes the necessary methods for any data
   // that can be transported from one module to another in APSIM
   // -----------------------------------------------------------
   public:
      virtual ~IData() { }
      virtual IData* Clone() const = 0;
		virtual void Pack(protocol::MessageData& message) = 0;
		virtual void Unpack(protocol::MessageData & message) = 0;
		virtual std::string DDML() = 0;
      virtual unsigned Size() = 0;
   };

class INamedData : public IData
   {
   // -----------------------------------------------------------
   // This interface encapsulates a named piece of data.
   // -----------------------------------------------------------
   public:
      virtual void SetName(const std::string& Name) = 0;
   };


} // namespace protocol
#endif

