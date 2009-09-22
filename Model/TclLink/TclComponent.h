//---------------------------------------------------------------------------
#ifndef TclComponentH
#define TclComponentH

// ------------------------------------------------------------------
// This component acts as the interface between an instance of a
// Tcl interpreter and an APSIM simulation.
// ------------------------------------------------------------------
struct ApsimGetQueryData;

class TclComponent : public protocol::Component,
                     public protocol::IMessageHook
   {
   public:
      TclComponent(void);
      ~TclComponent(void);
      virtual void doInit1(const protocol::Init1Data&);
      virtual void doInit2(void);
      virtual void respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);
      virtual void respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData);
      virtual bool respondToSet(unsigned int& fromID, protocol::QuerySetValueData& setValueData);
      virtual void onApsimGetQuery(unsigned int fromID,protocol::ApsimGetQueryData& apsimGetQueryData);
      
      int apsimGet( Tcl_Interp *interp, const string &varname, bool optional);
      bool apsimSet(Tcl_Interp *interp, const string &varname, Tcl_Obj *obj);

      void registerApsimVariable(Tcl_Interp *interp, const string &name) ;
      unsigned int registerEvent(string &eventName, string &script);
      void unRegisterEvent(unsigned int id);
      void catchMessages(string &command);

      std::string getXML(void) {return componentData->getXML();};
      
   private:

      typedef std::multimap<unsigned, std::string, std::less<unsigned> >   UInt2StringMap;
      UInt2StringMap rules;
      Tcl_Interp *Interp;

      void callback(const protocol::Message* message);
      std::string messageCallbackCommand;

      bool hasFatalError;
      unsigned errorID;
   };
#endif
