//---------------------------------------------------------------------------
#ifndef LogComponentH
#define LogComponentH

// ------------------------------------------------------------------
//  Short description:
//     Encapsulates the APSIM LOG module

//  Notes:

//  Changes:
//    DPH 25/7/2001

// ------------------------------------------------------------------
class LogComponent : public protocol::Component,
                     public protocol::IMessageHook
   {
   public:
      LogComponent(void);
      ~LogComponent(void);
      virtual void doInit1(const protocol::Init1Data&);
      virtual void callback(const protocol::Message* message);
      virtual bool respondToSet(unsigned int& fromID, protocol::QuerySetValueData& setValueData);
   private:
      ofstream out;
      int previousNesting;
      int nesting;
      unsigned debug_outputID;

//      typedef std::pair<unsigned, unsigned> key;
//      typedef std::map<key, std::string> Registrations;
//      struct RegComponent
//         {
//         Registrations registrations;
//         };
//      typedef std::map<unsigned int, RegComponent> Components;
//      Components components;

      const std::string queryCodeToString (const protocol::SimulationInformationKind type);
      void writeMessage(const std::string& toName,
                        const protocol::Message* message,
                        std::ostream& out);
      void writeMessageData(const protocol::Message* message);
      void writeRegistrationData(const protocol::Message* message, EventTypeCode kind);
      void storeRegistration(const protocol::Message* message);
      void writeVariant(const protocol::Variant& variant);
      std::string formatType(std::string RegistrationTypeString);

   };

#endif
