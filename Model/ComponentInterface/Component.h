//---------------------------------------------------------------------------
#ifndef ComponentH
#define ComponentH

#include <map>
#include <vector>
#include <functional>

#include <General/stl_functions.h>
#include <General/TreeNodeIterator.h>
#include <General/xml.h>
#include <General/path.h>
#include <General/platform.h>
#include <General/string_functions.h>

#include <ApsimShared/ApsimComponentData.h>
#include <ApsimShared/ApsimRegistry.h>
#include <ApsimShared/FApsimComponentData.h>

#include <ComponentInterface/Messages.h>
#include <ComponentInterface/MessageDataExt.h>
#include <ComponentInterface/ApsimVariant.h>
#include "DataTypes.h"
#include <ComponentInterface/Variable.h>
#include <ComponentInterface/VariableRef.h>
#include <ComponentInterface/FunctionReturningValue.h>
#include <ComponentInterface/FunctionTakingValue.h>
#include <ComponentInterface/Interfaces.h>

class ScienceAPI;

namespace protocol {

// forward declarations of our friends.
class Component;
class Variants;
struct QueryValueData;

extern "C" void EXPORT STDCALL messageToLogic (uintptr_t* instanceNumber,
                                                  Message* message,
                                                  bool* processed);
typedef EXPORT void (STDCALL CallbackType)(const uintptr_t *compInst, Message *message);
extern "C" void EXPORT STDCALL createInstance(const char* dllFileName,
                                         const unsigned int* compID,
                                         const unsigned int* parentID,
                                         uintptr_t* instanceNumber,
                                         const uintptr_t* callbackArg,
                                         CallbackType* callback);
void callCallback(const uintptr_t* callbackArg,
                   CallbackType* messageCallback,
                   Message* message);

// ------------------------------------------------------------------
// Abstract class to send variables to the rest of the system
// via sendVariable()
// ------------------------------------------------------------------
class EXPORT baseInfo {
  protected:
   bool                   myIsArray;
   int                    myLength;
   protocol::DataTypeCode myType;
   std::string            myName;
   std::string            myUnits;
   std::string            myDescription;
  public:
   baseInfo()
      {
      myLength = 0;  myIsArray = false;
      myType = protocol::DTunknown;
      };
   virtual ~baseInfo() {};
   virtual void sendVariable(Component *, QueryValueData&) {};
   std::string name(void) {return myName;};
};

// ------------------------------------------------------------------
// A class to wrap a variable for reporting/manager/etc. Keeps a pointer
// to memory region of scalar or array object, and knows how to send this
// to the system via sendVariable when asked.
// ------------------------------------------------------------------
class EXPORT varInfo : public baseInfo {
  private:
   void *myPtr;
  public:
   varInfo(const char *name, DataTypeCode type, int length, void *ptr, const char *units, const char *desc) {
      myName = name;
      myType = type;
      myLength = length;
      myIsArray = length > 1;
      myPtr = ptr;
      myUnits = units;
      myDescription = desc;
   };

   ~varInfo() {};
   void sendVariable(Component *, QueryValueData&);

  };
class EXPORT varVectorFloatInfo : public baseInfo {
  private:
      const std::vector<float>& myPtr;
  public:
   varVectorFloatInfo(const char *name, DataTypeCode type, const std::vector<float>& ptr, const char *units, const char *desc)
      : myPtr(ptr)
      {
      myName = name;
      myType = type;
      myLength = (int) ptr.size();
      myIsArray = true;
      myUnits = units;
      myDescription = desc;
   };
   void sendVariable(Component *, QueryValueData&);
  };
class EXPORT stringInfo : public baseInfo {
  private:
   std::string *myPtr;
  public:
   stringInfo(const char *name, std::string *ptr, const char *units, const char *desc) {
      myName = name;
      myType = DTstring;
      myLength = 1;
      myIsArray = 0;
      myPtr = ptr;
      myUnits = units;
      myDescription = desc;
   };
   ~stringInfo() {};
   void sendVariable(Component *, QueryValueData&);
};

// Same as above, but stores pointers to function calls, not memory regions.
class EXPORT fnInfo : public baseInfo {
  private:
    std::function<void(Component *, QueryValueData &)> myFn; 
  public:
    fnInfo(const char *name,
           protocol::DataTypeCode type, bool isArray,
           std::function<void(Component *, QueryValueData &)> fn,
           const char *units, const char *desc) {
      myFn = fn;
      myName = name;
      myType = type;
      myLength = -1;          // length is variable..
      myIsArray = isArray;
      myUnits = units;
      myDescription = desc;
   };
   ~fnInfo() {};
   void sendVariable(Component *s, QueryValueData &qd) { myFn(s, qd); };
};

// Same as above, but stores pointers to function calls, not memory regions.
class EXPORT fnSetInfo : public baseInfo {
  private:
    std::function<bool(Component *, QuerySetValueData &)> myFn;
  public:
    fnSetInfo(const char *name,
           protocol::DataTypeCode type, bool isArray,
           std::function<bool(Component *, QuerySetValueData &)> fn,
           const char *units, const char *desc) {
      myFn = fn;
      myName = name;
      myType = type;
      myLength = -1;          // length is variable..
      myIsArray = isArray;
      myUnits = units;
      myDescription = desc;
   };
   ~fnSetInfo() {};
   bool callSetter(Component *s, QuerySetValueData &qd) {return myFn(s, qd);}

};
typedef std::map<unsigned, baseInfo*>   UInt2InfoMap;
typedef std::map<unsigned, fnSetInfo*>   UInt2SetInfoMap;
typedef std::map<std::string, unsigned> NameToUIntMap;
typedef std::function<void(unsigned &, unsigned &, protocol::Variant &)> pfcall;
typedef std::multimap<unsigned, pfcall, std::less<unsigned> >   UInt2EventMap;

// ------------------------------------------------------------------
// Manages a single instance of an APSIM Component
// ------------------------------------------------------------------
class EXPORT Component
   {
   public:
      Component(void);
      virtual ~Component(void);

      virtual std::string getType(void) {return fileTailNoExtension(dllName);};
      std::string getName(void) {return name;};
      std::string getClassType(void) {return type;};
      std::string getFQName(void);// {return (pathName + "." + name);};
      std::string getFQContainerName(void) {return (pathName);};
      std::string getDllName(void) {return dllName;};
      unsigned int getId(void) {return componentID;};
      ScienceAPI& scienceAPI() {return *api;}

      // Add a registration.
      unsigned int addRegistration(EventTypeCode kind,
                               int destID,
                               const char *name,
                               const char *ddml)
         {
         return (addRegistration(kind, destID, name, ddml, string("")));
         };

      unsigned int addRegistration(EventTypeCode kind,
                               int destID,
                               const std::string& name,
                               const std::string& ddml)
         {
         return (addRegistration(kind, destID, name, ddml, string("")));
         };

      unsigned int addRegistration(EventTypeCode kind,
                               int destID,
                               const FString& name,
                               const FString& ddml)
         {
         return (addRegistration(kind,
                                 destID,
                                 asString(name),
                                 asString(ddml),
                                 string("")));
         };

      unsigned int addRegistration(EventTypeCode kind,
                               int destID,
                               const FString& name,
                               const Type& type,
                               const FString& alias)
         {
         return (addRegistration(kind,
                                 destID,
                                 asString(name),
                                 asString(type.getTypeString()),
                                 asString(alias)));
         };

      unsigned int addRegistration(EventTypeCode kind,
                               int destID,
                               const std::string& name,
                               const std::string& ddml,
                               const std::string& alias);

      void deleteRegistration(EventTypeCode kind,
                              unsigned int regID);

	  ApsimRegistration *getRego(unsigned int regID)
 	    {
          if (regID <= Registrations.size())
            return Registrations[regID - 1];
		  else
 	        return NULL;
 	    }

	  std::string getProperty(const std::string &a, const std::string &b) const
         {
         return componentData->getProperty(a,b);
         }
      std::string findProperty(const std::string& name)
         {
         return componentData->findProperty(name);
         }

      void getProperties(const std::string &section,
                         std::vector<std::string> &names,
                         std::vector<std::string> &values) const
         {
         componentData->getProperties(section, names, values);
         }

      void getMultipleProperties(const std::string& sectionName,
                                 const std::string& variableName,
                                 std::vector<std::string>& values);


      // Notify system of an error.
      void error(const char *msg, bool isFatal);
      void error(const string& msg, bool isFatal)
         {
         error(msg.c_str(), isFatal);
         }
      void error(const FString& msg, bool isFatal)
         {
         error(asString(msg), isFatal);
         }

      // Terminate simulation
      void terminateSimulation(void);

      // Get the value of a specific variable.  Return true if a value was
      // returned.
      bool getVariable(unsigned int variableID,
                       Variant **value,
                       bool optional = false);

      // Get the multiple values of a specific variable.  Return true
      // if some values are returned.
      bool getVariables(unsigned int variableID, Variants **values);

      // Set the value of a variable.  Returns true if value was changed.
      template <class T>
      bool setVariable(unsigned int variableID, const T& data)
         {
         return setVariable(variableID, 0, data);
         }
      template <class T>
      bool setVariable(unsigned int variableID, bool isOptional, const T& data)
         {
         setVariableSuccess = false;
         sendMessage(newRequestSetValueMessage(componentID, parentID,
                                               variableID,
                                               DDML(data),
                                               data));
         if (!isOptional && !setVariableSuccess)
            setVariableError(variableID);
         return setVariableSuccess;
         }

      // Publish an event.
      void publishNull(unsigned int eventID)
         {
         int null = 0;
         sendMessage(newPublishEventMessage(componentID,
                                            parentID,
                                            eventID,
                                            "<type/>",
                                            null));
         }
      template <class T>
      void publish(unsigned int eventID, T& data)
         {
         sendMessage(newPublishEventMessage(componentID,
                                            parentID,
                                            eventID,
                                            DDML(data),
                                            data));
         }
      // Publish an event.
      template <class T>
      void publishArray(unsigned int eventID, T data[], unsigned int numValues)
         {
         sendMessage(newPublishEventMessage(componentID,
                                            parentID,
                                            eventID,
                                            DDML(data),
                                            data, numValues));
         }

      // Write a line to the summary file.
      void writeString(const FString& st);
      void writeString(const std::string& st);
      void writeString(const char *st);
      void writeStdErr(const std::string& st);

      // Send a variable to another component.
      template <class T>
      void sendVariable(QueryValueData& queryValueData,
                        const T& value)
         {
         sendMessage(newReplyValueMessage(componentID,
                                          queryValueData.fromID,
                                          queryValueData.queryID,
                                          DDML(value),
                                          value));
         }

      template <class T>
      void sendVariable(QueryValueData& queryValueData,
                        const std::vector<T> &values)
         {
         sendMessage(newReplyValueMessage(componentID,
                                          queryValueData.fromID,
                                          queryValueData.queryID,
                                          DDML(values),
                                          values));
         }

      // convert to and from a compname to an ID.
      bool componentNameToID(const std::string& name, int& compID);
      bool componentIDToName(int compID, std::string& name);

      // send a change order message.
      void changeComponentOrder(const FStrings& names);
      std::string getDescription(void);

      // override these methods if necessary.
      virtual void doInit1(const Init1Data&);
      virtual void doInit2(void) { }
      virtual void doCommence(void) { }
      virtual void respondToEvent(unsigned int& fromID, unsigned int& eventID, Variant& variant);
      virtual void respondToGet(unsigned int& fromID, QueryValueData& queryData);
      virtual bool respondToSet(unsigned int& fromID, QuerySetValueData& setValueData);
      virtual void notifyTermination(void) { }
      virtual void messageToLogic(/*const*/ Message* message);

      // Put here so that unit tests can call it.
      //void setup(const char *dllname,
      //           const unsigned int componentid,
      //           const unsigned int parentid,
      //           const uintptr_t* callbackarg,
      //           void* messagecallback);
      void setup(const char *dllname,
                 const unsigned int componentid,
                 const unsigned int parentid,
                 const uintptr_t* callbackarg,
                 CallbackType messagecallback);

   protected:
      int componentID;
      int parentID;
      ApsimComponentData* componentData;
      ::ScienceAPI* api;
      bool beforeInit2;
      bool beforeCommence;
      bool haveWrittenToStdOutToday;

      // ********* LOW LEVEL ROUTINES - should not be used by regular components.
      virtual void onRequestComponentIDMessage(unsigned int fromID, RequestComponentIDData& data) { }
      virtual void onRequestSetValueMessage(unsigned int fromID, RequestSetValueData& data) { }
      virtual void onReturnComponentIDMessage(ReturnComponentIDData& data) { }
      virtual void onRegisterMessage(unsigned int fromID, protocol::RegisterData& registerData) { }
      virtual void onDeregisterMessage(unsigned int fromID, protocol::DeregisterData& registerData) { }
      virtual void onTerminateSimulationMessage(void) { }
      virtual void onGetValueMessage(unsigned int fromId, GetValueData& getValueData) { }
      virtual void onPublishEventMessage(unsigned int fromID, PublishEventData& publishEventData) { }
      virtual void onQueryInfoMessage(unsigned int fromID, unsigned int messageID, QueryInfoData& queryInfo) { }
      virtual void onQueryValueMessage(unsigned int fromID, QueryValueData& queryData);
      virtual void onCompleteMessage(CompleteData& completeData);
      virtual void onApsimGetQuery(unsigned int fromID, ApsimGetQueryData& apsimGetQueryData) { }
      virtual bool onApsimSetQuery(ApsimSetQueryData& apsimSetQueryData) {return false;}
      virtual void onApsimChangeOrderData(unsigned int fromID, protocol::MessageData& messageData) { }
      virtual void onQuerySetValueMessage(unsigned fromID, QuerySetValueData& querySetData, unsigned msgID);
      virtual void onReplyValueMessage(unsigned fromID, ReplyValueData replyValueData) { }
      virtual void onReplySetValueSuccess(unsigned fromID, ReplySetValueSuccessData& replySetData, unsigned msgID) { };

      virtual void onError(ErrorData& errorData) { };

      // Send a message
      void sendMessage(Message* message)
         {
         bool doAck = message->toAcknowledge != 0;
         if (doAck)
            {
            completeIDs.push_back(message->messageID);
            completeFound = false;
            }
         if (messageCallback != NULL)
            (*messageCallback)(callbackArg, message);
         if (doAck)
            waitForComplete();
         deleteMessage(message);
         }

	  typedef std::vector<ApsimRegistration*> Regs;
      Regs Registrations;

      ApsimRegistration *getRegistration(int fromID, unsigned int regID);
      EventTypeCode getRegistrationType(unsigned int regID);
      void getRegistrationName(int fromID, unsigned int regID, std::string &);
      bool getSetVariableSuccess(void) {return setVariableSuccess;}
      void setVariableError(unsigned int regID);
      void writeStringToStream(const std::string& lines, std::ostream& out,
                               const std::string& componentName);

   private:
      std::string name;
      std::string pathName;
      std::string dllName;
      std::string type;
      std::string version;
      std::string author;
      int active;
      std::string state;

      typedef std::map<unsigned int, Variants*> getVariableResponses;
      getVariableResponses myGetVariableResponses;

      unsigned int tickID;
      bool setVariableSuccess;
      std::vector<unsigned> completeIDs;
      bool completeFound;
      bool sendTickToComponent;
      protocol::TimeType tick;
      unsigned currentMsgID;

      UInt2InfoMap getVarMap;                  // List of variables we can send to system
      UInt2SetInfoMap setVarMap;                  // List of variables we can send to system
      UInt2EventMap eventMap;                  // List of events we handle
      NameToUIntMap reverseGetVarMap;          

      const uintptr_t* callbackArg;
      CallbackType* messageCallback;

      void addReturnValueMessage(ReturnValueData &returnData);
      void clearReturnInfos(void);
      void waitForComplete(void);


      friend class TypeConverter;
      friend void EXPORT STDCALL messageToLogic (unsigned* instanceNumber,
                                            Message* message,
                                            bool* processed);
      friend void EXPORT STDCALL createInstance(const char* dllFileName,
                                           const unsigned int* compID,
                                           const unsigned int* parentID,
                                           unsigned int* instanceNumber,
                                           const unsigned int* callbackArg,
                                           CallbackType* callback);
      unsigned int getReg(const char *systemName,
                          DataTypeCode type,
                          bool isArray,
                          const char *units,
                          const char *desc);

      unsigned int getReg(const char *systemName,
                          std::string &DDML);

 protected:
      std::vector<ReturnInfoData*> returnInfos;

 public:
      // Get a variable from the system (into basic C datatypes)
      template <class T>
      bool getVariable(unsigned int regId,
                       T& value,
                       double lower,
                       double upper,
                       bool isOptional = false)
         {
         protocol::Variant *variant = NULL;
         if (!getVariable(regId, &variant, isOptional))
            {
            return false;
            }
         else
            {
            ApsimRegistration *regItem = getRegistration(componentID, regId);
            TypeConverter* typeConverter;
            getTypeConverter(regItem->getName().c_str(),
                             variant->getType(),
                             regItem->getDDML().c_str(),
                             typeConverter);
            protocol::ArraySpecifier* arraySpec = protocol::ArraySpecifier::create(regItem);
            bool ok = variant->unpack(typeConverter,
                                      arraySpec,
                                      value);
			if (arraySpec) delete arraySpec;
			delete typeConverter;
            if (!ok)
               {
               string buffer= "Unpack failed.\n"
                              "VariableName: ";
               buffer += regItem->getName();
               error(buffer, true);
               return false;
               }
            if (value < lower || value > upper)
               {
               string msg = string("Bound check warning while getting variable.\n"
                                   "Variable  : ") + regItem->getName() + string("\n"
                                   "Condition : ") + ftoa(lower, 2) + string(" <= ") +
                                    typeto_str(value) + string(" <= ") + ftoa(upper, 2);
               error(msg, false);
               }
            }
         return true;
         }

      template <class T>
      bool getVariable(unsigned int regId,
                       std::vector<T>& values,
                       double lower,
                       double upper,
                       bool isOptional = false)
         {
         values.clear();
         protocol::Variant *variant = NULL;
         if (!getVariable(regId, &variant, isOptional))
            {
            return false;
            }
         else
            {
            ApsimRegistration *regItem = getRegistration(componentID, regId);
			TypeConverter* typeConverter;
            getTypeConverter(regItem->getName().c_str(),
                             variant->getType(),
                             regItem->getDDML().c_str(),
                             typeConverter);
            protocol::ArraySpecifier* arraySpec = protocol::ArraySpecifier::create(regItem);
            bool ok = variant->unpack(typeConverter,
                                      arraySpec,
                                      values);
			if (arraySpec) delete arraySpec;
			delete typeConverter;
			if (!ok)
               {
               string buffer= "Unpack failed.\n"
                              "VariableName: ";
               buffer += regItem->getName();
               error(buffer, true);
               return false;
               }

            for (unsigned int i = 0; i < values.size(); i++)
               {
               if (values[i] < lower || values[i] > upper)
                   {
                   string msg = string("Bound check warning while getting variable.\n"
                                       "Variable  : ") + regItem->getName() + string("(") + itoa(i+1) +  string(")\n"
                                       "Condition : ") + ftoa(lower, 2) + string(" <= ") +
                                typeto_str(values[i]) + string(" <= ") + ftoa(upper, 2);
                   error(msg, false);
                   }
               }
            }
         return (values.size() > 0);
         }

   // Read variable permutations
   bool readParameter(const FString& sectionName,
                      const FString& variableName,
                      FString& variableValue,
                      bool optional);

   std::string readParameter(const std::string& sectionName,
                             const std::string& variableName)
       {
       std::string valueString = componentData->getProperty(sectionName, variableName);

       // remove any Units specifier "(..)":
       size_t posBracket = valueString.find('(');
       if (posBracket != std::string::npos)
         valueString = valueString.substr(0,posBracket);

       // And any whitespace...
       stripLeadingTrailing(valueString, " \t\n\r");
       return valueString;
       };


      // Search a list of "sections" for a parameter.
    std::string readParameter(const std::vector<std::string> &sectionNames,
                              const std::string& variableName)
       {
       std::string result;
       for (unsigned int i = 0; i < sectionNames.size(); i++)
         if ((result = readParameter(sectionNames[i], variableName)) != "")
            return result;
       return result;
       };


   template <class T>
   bool readParameter(const std::string &sectionName,
                      const std::string &variableName,
                      T &value,
                      double lower,
                      double upper,
                      bool optional=false)
      {
      // 1. Get the variable as string
      string datastring = readParameter(sectionName, variableName);

      if (datastring.size() == 0)
         {
         if (!optional)
             {
             string msg = string("Cannot find a parameter.\n"
                                 "Parameter name = ") + variableName;
             msg += "\nSection name = " + sectionName;
             error(msg, true);
             }
         return false;
         }

      // 2. Convert it
      try { value = strto_type<T> (datastring, value); }
      catch(...)
         {
         string msg = string("Problem converting variable to ") +
                             string(typeid(T).name()) + string(" type.\n"
                             "Parameter name = ") + variableName + string("\n"
                             "Value          = '") + datastring + string("'");
         error(msg, true);
         return false;
         }

      // 3. Check bounds
      if (value < lower || value > upper)
         {
         string msg = string(
                    "Bound check warning while reading parameter.\n"
                    "Variable  : ") + variableName + string("\n"
                    "Condition : ") + ftoa(lower, 2) + string(" <= ") +
                    datastring + string(" <= ") + ftoa(upper, 2);
         error(msg, false);
         }

      return true;
      }

   template <class T>
   bool readParameter(const std::string &sectionName,
                      const std::string &variableName,
                      std::vector <T> &values,
                      double lower,
                      double upper,
                      bool optional=false)
      {
      values.clear();

      // 1. Get the variable as string
      string datastring = readParameter(sectionName, variableName);
      if (datastring == "")
         {
         if (!optional)
             {
             string msg = string("Cannot find a parameter.\n"
                                 "Parameter name = ") + variableName;
             msg += "\nSection name = " + sectionName;
             error(msg, true);
             }
         return false;
         }

      // 2. Convert it
      std::vector <string> value_strings;
      splitIntoValues (datastring," ",value_strings);
      for (int i=0; i!=value_strings.size();i++)
        {
        T value;
        try { value = strto_type<T>(value_strings[i], value); }
        catch(...)
           {
           string msg = string("Problem converting variable to ") +
                               string(typeid(T).name()) + string(" type.\n"
                               "Parameter name = ") + variableName + string("\n"
                               "Value          = '") + value_strings[i] + string("'\n");
           error(msg, true);
           return false;
           }
        values.push_back(value);

        // 3. Check bounds
        if (value < lower || value > upper)
            {
            //NB. array index reported as origin 1 indexing!!!
            string msg = string(
                    "Bound check warning while reading parameter.\n"
                    "Variable  : ") + variableName + string("(") + itoa(i+1) +  string(")\n"
                    "Condition : ") + ftoa(lower, 2) + string(" <= ") +
                    value_strings[i] + string(" <= ") + ftoa(upper, 2);
            error(msg, false);
            }
        }
      return (value_strings.size() > 0);
      };

       // C arrays
      template <class T>
      bool readParameter(const std::string &sect, const std::string &name,
                         T *valarray, int &numvals,
                         double lower, double upper, bool isoptional = false)
         {
         std::vector<T> vv;
         bool result = readParameter(sect, name, vv, lower, upper, isoptional);
         for (unsigned int i = 0; i < vv.size(); i++)
             valarray[i] = vv[i];
         numvals = vv.size();
         return result;
         };

      template <class T>
      bool readParameter(const std::vector<std::string> &sections,
                         const std::string &variableName,
                         T &value,
                         double lower,
                         double upper,
                         bool optional=false)
         {
         for (unsigned int i = 0; i < sections.size(); i++)
           if (readParameter(sections[i], variableName, value, lower, upper, true))
              return true;

         if (!optional)
            {
            std::string msg = string("Cannot find a parameter.\n"
                                     "Parameter name = ") + variableName;
            for (unsigned i = 0; i != sections.size(); i++)
                msg += "\nSection name = " + sections[i];
            error(msg, true);
            }
         return false;
         };

      template <class T>
      bool readParameter(const std::vector<std::string> &sects,
                         const std::string &name,
                         T *v, int &numvals,
                         double lower, double upper,
                         bool isOptional = false)
         {
         for (unsigned int i = 0; i < sects.size(); i++)
           if (readParameter(sects[i], name,  v, numvals, lower, upper, true))
              return true;

         if (!isOptional)
            {
            std::string msg = string("Cannot find a parameter.\n"
                                 "Parameter name = ") + name;
            for (unsigned i = 0; i != sects.size(); i++)
                msg += "\nSection name = " + sects[i];
            error(msg, true);
            }
         return false;
         };

      template <class T>
      bool readParameter(const std::vector<std::string> &sections,
                         const std::string &variableName,
                         std::vector <T> &values,
                         double lower,
                         double upper,
                         bool isOptional=false)
         {
         for (unsigned int i = 0; i < sections.size(); i++)
           if (readParameter(sections[i], variableName, values, lower, upper, true))
              return true;

         if (!isOptional)
            {
            string msg = string("Cannot find a parameter in any of the files/sections\n"
                                 "specified in the control file.\n"
                                 "Parameter name = ") + variableName;
            for (unsigned i = 0; i != sections.size(); i++)
                msg += "\nSection name = " + sections[i];
            error(msg, true);
            }
         return false;
         };

      void AddToGetVarMap(unsigned int id, baseInfo* v)
      {
          getVarMap.insert(UInt2InfoMap::value_type(id, v));
          reverseGetVarMap.insert(NameToUIntMap::value_type(v->name(), id));
      }

      // Add Variables to the Get list.
      // Function
      void addGettableVar(const char *systemName,
                          DataTypeCode type,
                          bool isArray,
                          std::function<void(Component *, QueryValueData &)> ptr,
                          const char *units,
                          const char *desc)
          {
          // Get a system ID for it
          unsigned int id = getReg(systemName, type, isArray, units, desc);
          // Add to variable map
          fnInfo *v = new fnInfo(systemName, type, isArray, ptr, units, desc);
          AddToGetVarMap(id, v);
          };
     // Function with DDML
     void addGettableVar(const char *systemName,
                          std::string &ddml,
                          std::function<void(Component *, QueryValueData &)> ptr,
                          const char *units,
                          const char *desc)
          {
          // Get a system ID for it
          unsigned int id = getReg(systemName, ddml);
          // Add to variable map
          fnInfo *v = new fnInfo(systemName, protocol::DTunknown, 0, ptr, units, desc);
          AddToGetVarMap(id, v);
          };


      // scalar
      template <class T>
      void addGettableVar(const char *systemName,
                          T &value,
                          const char *units,
                          const char *desc)
          {
          DataTypeCode type = dataTypeCodeOf(value);
          unsigned int id = getReg(systemName, type, false, units, desc);
          varInfo *v = new varInfo(systemName, type, 1, &value, units, desc);
          AddToGetVarMap(id, v);
          };
      // Special case for stl strings
      void addGettableVar(const char *systemName,
                          std::string &value,
                          const char *units,
                          const char *desc)
          {
          unsigned int id = getReg(systemName, DTstring, false, units, desc);
          stringInfo *v = new stringInfo(systemName, &value, units, desc);
          AddToGetVarMap(id, v);
          };

      // C array
      template <class T>
      void addGettableVar(const char *systemName,
                          int length,
                          T *value,
                          const char *units,
                          const char *desc)
          {
          DataTypeCode type = dataTypeCodeOf(* value);
          unsigned int id = getReg(systemName, type, 1, units, desc);
          varInfo *v = new varInfo(systemName, type, length, value, units, desc);
          AddToGetVarMap(id, v);
          };

      // vector
      template <class T>
      void addGettableVar(const char *systemName,
                          const std::vector<T> &value,
                          const char *units,
                          const char *desc)
           {
          DataTypeCode type = dataTypeCodeOf(value);
          unsigned int id = getReg(systemName, type, true, units, desc);
          varVectorFloatInfo *v = new varVectorFloatInfo(systemName, type, value, units, desc);
          AddToGetVarMap(id, v);
           };
      // scalar
      void addSettableVar(const char *systemName,
                          DataTypeCode type,
                          const char * ddml,
                          std::function<bool(Component *, QuerySetValueData &)> ptr,
                          const char *units,
                          const char *desc)
          {
          unsigned int id = addRegistration(::respondToSet, componentID, systemName, ddml);
          fnSetInfo *fn = new fnSetInfo(systemName, type, false, ptr, units, desc);
          setVarMap.insert(UInt2SetInfoMap::value_type(id, fn));
          };

      // Read-write vector
      void addRWVectorVar(const char *systemName,
                          const std::vector<float> &value,
                          std::function<bool(Component *, QuerySetValueData &)> ptr,
                          const char *units,
                          const char *desc)
           {
          DataTypeCode type = dataTypeCodeOf(value);
          std::string ddml = "<type kind=\"single\" array=\"T\"";
          if (strlen(units) > 0)
	      {
             ddml += " unit=\"";
             ddml += units;
             ddml += "\"";
	      }
          if (strlen(desc) > 0)
          {
             ddml += " description=\"";
             ddml += desc;
             ddml += "\"";
          }
          ddml += "/>";
          unsigned int id = addRegistration(::respondToGetSet, componentID, systemName, ddml.c_str());
          varVectorFloatInfo *v = new varVectorFloatInfo(systemName, type, value, units, desc);
          AddToGetVarMap(id, v);
          fnSetInfo *fn = new fnSetInfo(systemName, type, false, ptr, units, desc);
          setVarMap.insert(UInt2SetInfoMap::value_type(id, fn));
           };

      void removeGettableVar(const char *systemName);


      // Add a procedure to be called when events occur
      unsigned int addEvent(const char *systemName,
                            std::function<void(unsigned &, unsigned &, protocol::Variant &)> ptr,
                            const char* DDML);

   }; // end class Component
} // end namespace protocol

protocol::Component* createComponent(void);

#endif
