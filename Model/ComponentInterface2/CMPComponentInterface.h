#ifndef CMPComponentInterfaceH
#define CMPComponentInterfaceH
#include <General/platform.h>
#include <string>
#include <map>
#include <ComponentInterface2/Interfaces.h>
#include <ComponentInterface2/CMPData.h>
#include <ComponentInterface2/ScienceAPI2.h>
#include <General/xml.h>

struct Message;

typedef EXPORT void (STDCALL CallbackType)(const uintptr_t *compInst, Message& message);

class EXPORT CMPComponentInterface
   {
   public:
      CMPComponentInterface(uintptr_t* callbackarg, CallbackType* callback, unsigned componentID, unsigned parentID, const std::string &dllName);
      ~CMPComponentInterface();

      bool get(const std::string& name, const std::string& units, bool optional, Packable* data);
      void set(const std::string& name, const std::string& units, Packable* data);
      bool read(const std::string& name, Convertable* value, bool optional);
      bool read(const std::string& name, std::vector<Convertable*> values, bool optional);

      void publish(const std::string& name, Packable* data);
      void subscribe(const std::string& eventName, Packable* handler);
      void query(const std::string& pattern, std::vector<QueryMatch>& matches);
      void queryVariable(const std::string& pattern, std::vector<QueryMatch>& matches);

      void setSearchOrder(const std::vector<std::string> &list) {simSectionsToSearch = list;};
      void getSearchOrder(std::vector<std::string> &list) {list = simSectionsToSearch;};
      bool readFiltered(const std::string& filterName, std::vector<std::string> &values);
      bool readAll(std::vector<std::string> &names, std::vector<std::string> &values);
      bool readScripts(std::map<std::string, std::string> &scripts);

      // Export a variable. The variable passed in is stored directly
      // in our map so the assumption is that we are now owners.
      // ie. don't delete this data object elsewhere!
      void expose(const std::string& name,
                  const std::string& units,
                  const std::string& description,
                  bool writable,
                  Packable* variable);

      void write(const std::string& msg);

      std::string getName(void);
      std::string getFQName(void);
      std::string getExecutableFileName(void);
      const std::string getInitData(void) {return simScript->documentElement().write();}

      // internal stuff.
      void messageToLogic(const Message& message);

      void error(const std::string& errorMessage, bool isFatal);
      std::string getDescription(const std::string& dllName);

      // tell the system about an event we will issue later
      void notifyFutureEvent(const std::string& name);

      void deRegisterAll();
   private:
      ScienceAPI2* scienceAPI;
      uintptr_t* callbackArg;
      unsigned componentID;
      unsigned parentID;
      std::string name;
      std::string type;
      std::string pathName;
      std::string dllName;
      CallbackType* messageCallback;
      bool errorHasOccurred;
      XMLDocument* simScript;
      std::vector<std::string> simSectionsToSearch;
      std::string currentClass1;
      std::string currentClass2;
      Packable* init1;
      std::vector<Packable*> init2;

      enum RegistrationKind {getReg=1,         respondToGetReg=2,
                             setReg=9,         respondToSetReg=3,
                             eventReg=5,       respondToEventReg=6,
                                               respondToGetSetReg=4};
      struct Reg
         {
         Packable* data;
         RegistrationKind kind;
         std::string ddml;
         int regId;
         };

      typedef std::multimap<std::string, Reg*> NameToRegMap;
      typedef std::vector<Message*> Messages;
      typedef std::vector<Reg*> Regs;
      //typedef std::multimap<std::string, RegistrationKind> NameToRegKindMap;
      //typedef std::multimap<std::string, std::string> NameToDDMLMap;

      NameToRegMap regNames;
      //NameToRegKindMap regKinds;
      //NameToDDMLMap regDDMLs;
      Messages messages;
      Regs Registrations;

      uintptr_t tickID;
      TimeType tick;
      bool haveWrittenToStdOutToday;

      void clearMessages();
      int RegisterWithPM(const std::string& Name, const std::string& Units,
                         const std::string& Description,
                         RegistrationKind regKind,
                         Packable* Data);

      int nameToRegistrationID(const std::string& name,
                               RegistrationKind regKind);

      void sendMessage(Message& message);

      void onInit1(const Message& message);
      void onInit2(const Message& message);
      void onQueryValue(const Message& message);
      void onQuerySetValue(const Message& message);
      void onEvent(const Message& message);
      bool readFromSection(XMLNode::iterator initData,
                           XMLNode::iterator sectionData,
                           const std::string& parName,
                           Convertable* value);
      void readAndDemangleScripts(std::map<std::string, std::string> &scripts, XMLNode::iterator &data);
      void replaceManagerMacros(std::string& contents, XMLNode ui);

      void terminate();
      std::string getPropertyDescription(NameToRegMap::iterator reg, const string& access);
      std::string getEventDescription(NameToRegMap::iterator reg, const string& published);
      std::string getRegName(NameToRegMap::iterator reg);
   };

#endif
