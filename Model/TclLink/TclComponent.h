//---------------------------------------------------------------------------
#ifndef TclComponentH
#define TclComponentH

#include <string>
#include <map>

class Variant;

// ------------------------------------------------------------------
// This component acts as the interface between an instance of a
// Tcl interpreter and an APSIM simulation.
// ------------------------------------------------------------------
typedef std::map<std::string, std::string >   Name2RuleMap;

class TclComponent {
   public:
      TclComponent(ScienceAPI2& scienceapi);
      virtual void onError(void);
      virtual void onInit2(void);

      void exposeReadable(const std::string &variableName, const std::string &units);
      void exposeReadWrite(const std::string &variableName, const std::string &units);
      	
      void respondToGet(const std::string &variableName, std::vector<std::string> &result);
      void respondToSet(const std::string &variableName, std::vector<std::string> &value);

      void subscribeNull(const std::string &event, const std::string &script);
      void subscribeVariant(const std::string &event, const std::string &script);
      void onNullEventCallback(const std::string &);
      void onVariantEventCallback(const std::string &, Variant &);

      int apsimGet( Tcl_Interp *interp, const std::string &varname, bool optional);
      bool apsimSet(Tcl_Interp *interp, const std::string &varname, Tcl_Obj *obj);

      ScienceAPI2& apsimAPI;

   private:
      Tcl_Interp *Interp;
      Name2RuleMap rules;
      bool hasFatalError;
   };

#endif

