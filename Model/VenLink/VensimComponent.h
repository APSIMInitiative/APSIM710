//---------------------------------------------------------------------------
#ifndef VensimComponentH
#define VensimComponentH
#include <windows.h>
#include <map>

// ------------------------------------------------------------------
// This component acts as the interface between an instance of a
// VENSIM model and an APSIM simulation.
// ------------------------------------------------------------------
class VensimComponent : public protocol::Component
   {
   public:
      VensimComponent(void);
      ~VensimComponent(void);
      virtual void doInit1(const protocol::Init1Data&);
      virtual void doInit2(void);
      virtual void respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);
      virtual void respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData);
      virtual bool respondToSet(unsigned int& fromID, protocol::QuerySetValueData& setValueData);

      int vensim_command(string &command);
      int vensim_command(const char *command);
      int vensim_get_varnames(const char* filter,
                              int vartype,
                              char* buf,
                              int buflength);
      int vensim_get_val(string &name, float* value);

   private:
      HINSTANCE vensimLibrary;
      typedef std::map<unsigned, std::string> VariableMap;
      VariableMap variables;
      std::string fileName;
      unsigned processID;

      bool findLibrary(void);
      void doTimestep(void);
      void registerModelVariables(void);
   };


#endif
