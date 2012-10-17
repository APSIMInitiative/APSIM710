//---------------------------------------------------------------------------
#ifndef ApsimRegistrationH
#define ApsimRegistrationH

#include <string.h>
#include <stdint.h>
#include <General/platform.h>
#include <General/string_functions.h>
#include <ApsimShared/ApsimRegistrationType.h>
#include <iostream>

class EXPORT  ApsimRegistration
   {
   public:
      // ------------------------------------------------------------------
      // constructors and destructors
      // ------------------------------------------------------------------
      ApsimRegistration(EventTypeCode _type,
                        const std::string& _registrationName,
                        const std::string& _ddml,
                        int  _destinationID,
                        int _componentID) :
              type(_type),
              registrationName(ToLower(_registrationName)),
              ddml(_ddml),
              componentID(_componentID),
              destinationID(_destinationID)
        {
			setName(registrationName);
        };

      virtual ~ApsimRegistration(void) {};

      // ------------------------------------------------------------------
      // Return true if this registration item matches the specified parameters.
      // There are 3 main cases (eg for "wheat sow")
      // ... wheat          sow - relative path
      // ... .paddock.wheat sow - absolute path
      // ... *.wheat        sow - all paths

      // ------------------------------------------------------------------
      virtual bool isMatch(const ApsimRegistration *rhs)
         {
         return (type == rhs->type &&
                 isMatch(registrationName, rhs->registrationName) );
         }

	  virtual bool isMatch(EventTypeCode rhsType,
				   const std::string& rhsName)
         {
         return (type == rhsType &&
                 isMatch(registrationName, rhsName) );
         }
      virtual bool matchSubscriberType(const ApsimRegistration *rhs);
      // ------------------------------------------------------------------
      // Return the type string of the registration to caller.
	  // ------------------------------------------------------------------
	  const std::string getType(void) const {return typeCodeToString(type);}
	  EventTypeCode getTypeCode(void) const {return type;}
	  const std::string& getName(void) const {return registrationName;};
	  const std::string& getNameWithoutBrackets(void) const {return registrationNameWithoutBrackets;}
	  const std::string& getDDML(void) const {return ddml;};
	  int getComponentID(void) const {return componentID;};
      int getDestinationID(void) const {return destinationID;};
	  void setName(const std::string& name) { 
		registrationName = ToLower(name);
        size_t pos = registrationName.find('(');
        if (pos != std::string::npos)
           {
           arraySpecifier = registrationName.substr(pos);
//           registrationName = registrationName.substr(0, pos);
           }

        pos = registrationName.rfind('(');
        if (pos != std::string::npos)
           {
           registrationNameWithoutBrackets = registrationName.substr(0, pos);
           }
        else
           registrationNameWithoutBrackets = registrationName;
	  };

      virtual uintptr_t getRegID(void) const = 0;

      EventTypeCode opposite();

   protected:
      EventTypeCode type;
      std::string registrationName;
      std::string registrationNameWithoutBrackets;
      std::string arraySpecifier;
      std::string ddml;
      int componentID;
      int destinationID;

      // Match two strings, catering for wildcards in either.
      bool isMatch(const std::string& a, const std::string& b)
         {
         return(a == b);   // *FIXME*!!!
         }
   };

class EXPORT  NativeRegistration : public ApsimRegistration
   {
   public:
      NativeRegistration(EventTypeCode _type,
                              const std::string& _registrationName,
                              const std::string& _ddml,
                              int  _destinationID,
                              int _componentID) :
        ApsimRegistration(_type, _registrationName,_ddml, _destinationID, _componentID)
        {
        };
     uintptr_t getRegID(void) const  {return (uintptr_t) this;};
   };

class EXPORT  ForeignRegistration : public ApsimRegistration
   {
   private:
     unsigned int foreignID;
   public:
	  ForeignRegistration(EventTypeCode _type,
                              const std::string& _registrationName,
                              const std::string& _ddml,
                              int  _destinationID,
                              int _componentID,
                              unsigned int _foreignID) :
        ApsimRegistration(_type, _registrationName, _ddml, _destinationID, _componentID),
        foreignID(_foreignID)
        {
        };
     uintptr_t getRegID(void) const {return foreignID;}

   };


#endif
