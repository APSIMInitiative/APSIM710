//---------------------------------------------------------------------------
#ifndef RComponentH
#define RComponentH

#include <string>
#include <map>

// ------------------------------------------------------------------
// This component acts as the interface between an instance of a
// R interpreter and an APSIM simulation.
// There are two parts: the apsim component that is linked to the Apsim 
// Infrastructure (ComponentInterface2), and the embedding dll that is linked 
// to the embedding engine RInside (http://dirk.eddelbuettel.com/code/rinside.html)
//
// On windows, RInside uses the gnu stdc++ (g++) implementation, and cannot be compiled with MSVC -
// hence the two dlls. This is an advantage however as it allows us to load the  
// R dll from the information in the system registry - it's not normally on the
// system PATH. The embedding dll is stored in apsim's runtime dll package. Building it is 
// difficult.
//
// On unix, everything is built with gcc, and the calls from apsim -> RInside are simpler. 
// ------------------------------------------------------------------
typedef std::map<std::string, std::string >   Name2RuleMap;

class RComponent 
   {
   public:
      RComponent(ScienceAPI2& scienceapi);
      ~RComponent(void);
      virtual void onError(void);
      virtual void onInit2(void);
      void subscribe(const std::string &event, const std::string &type, const std::string &handler );
      void onRuleCallback(const std::string &);
      void expose(const std::string &variableName,const std::string &units);
      	
      void respondToGet(const std::string &variableName, std::vector<std::string> &result);
      void respondToSet(const std::string &variableName, std::vector<std::string> &value);

      ScienceAPI2& apsimAPI;
   private:
      Name2RuleMap rules;
      bool hasFatalError;
      Name2RuleMap apsimVariables;
      void importVariables(void);
   };

bool isNumeric(const std::string &s); 

#endif

