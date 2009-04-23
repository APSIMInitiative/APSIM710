//---------------------------------------------------------------------------
#ifndef RegistrationItemH
#define RegistrationItemH
#include "Variants.h"
#include "ArraySpecifier.h"
#include "Messages.h"
#include <ApsimShared/FString.h>

namespace protocol {

// ------------------------------------------------------------------
// This class encapsulates a single registration item plus any
// return value variants if it is a getVariable registration.
// ------------------------------------------------------------------
class RegistrationItem
   {
   public:
      // ------------------------------------------------------------------
      // constructors and destructors
      // ------------------------------------------------------------------
      RegistrationItem(Component* parent, RegistrationType kind,
                       const FString& name, const Type& type,
                       const FString& componentName);
      ~RegistrationItem(void)
         {
         delete [] originalName;
         delete [] name;
         delete [] type;
         delete [] componentName;
         delete arraySpecifier;
         }

      // ------------------------------------------------------------------
      // Return true if this registration item matches the specified parameters.
      // ------------------------------------------------------------------
      bool isMatch(RegistrationType rhsKind, const FString& rhsName,
                   const FString& rhsComponentName)
         {
         return (kind == rhsKind && rhsName == originalName
                 && rhsComponentName == componentName);
         }

      // ------------------------------------------------------------------
      // Return the destination component ID or 0 if not found.
      // ------------------------------------------------------------------
      char* getComponentName(void) {return componentName;}

      // ------------------------------------------------------------------
      // Return the type string of the registration to caller.
      // ------------------------------------------------------------------
      const char* getType(void) const {return type;}
      void setType(const Type& type);

      const char *getName(void) const {return name;};
      RegistrationType getKind(void) {return kind;}
      Variants& getVariants(void) {return variants;}

      void empty(void) {variants.empty();}
      unsigned countResponses(void) {return variants.size();}

      // ------------------------------------------------------------------
      // Add a return value message to this registration item.
      // ------------------------------------------------------------------
      void addReturnValueMessage(unsigned int fromID, ReturnValueData& returnValueData);

   private:
      Component* parent;
      RegistrationType kind;
      char* name;
      char* type;
      char* componentName;
      char* originalName;

      Variants variants;
      bool haveCreatedTypeConverter;
      bool isError;
      ArraySpecifier* arraySpecifier;


   };

} // namespace protocol
#endif
