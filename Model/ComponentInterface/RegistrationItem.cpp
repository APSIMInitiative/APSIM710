//---------------------------------------------------------------------------
#include <stdexcept>
#include "RegistrationItem.h"

using namespace protocol;

// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
RegistrationItem::RegistrationItem(Component* p, RegistrationType k,
                                   const FString& n, const Type& t,
                                   const FString& comp)
   : variants(p, t), kind(k), haveCreatedTypeConverter(false), parent(p)
   {
   // create a buffer for name string and point our name object to it.
   name = new char[n.length()+1];
   strcpy(name, "");
   strncat(name, n.f_str(), n.length());

   // create a buffer for name string and point our name object to it.
   originalName = new char[n.length()+1];
   strcpy(originalName, "");
   strncat(originalName, n.f_str(), n.length());

   // create a buffer for type string and point our type object to it.
   type = NULL;
   setType(t);

   // create a buffer for component name string and point our component name object to it.
   componentName = new char[comp.length()+1];
   strcpy(componentName, "");
   strncat(componentName, comp.f_str(), comp.length());

   arraySpecifier = ArraySpecifier::create(name, type);

   isError = true;
   }
// ------------------------------------------------------------------
// Add a return value message to this registration item.
// ------------------------------------------------------------------
void RegistrationItem::addReturnValueMessage(unsigned int fromID,
                                             ReturnValueData& returnValueData)
   {
   if (!haveCreatedTypeConverter)
      {
      TypeConverter* converter;
      isError = !getTypeConverter(name, returnValueData.variant.getType(),
                                  FString(type), converter);
      if (!isError)
         variants.setTypeConverter(converter);
      haveCreatedTypeConverter = true;
      }
   if (!isError)
      {
      returnValueData.variant.setFromId(returnValueData.fromID);
      returnValueData.variant.setArraySpecifier(arraySpecifier);
      variants.addVariant(returnValueData.variant);
      }
   }
// ------------------------------------------------------------------
// Set the type of the registration.
// ------------------------------------------------------------------
void RegistrationItem::setType(const Type& t)
   {
   delete [] type;

   // create a buffer for type string and point our type object to it.
   type = new char[t.getTypeString().length()+20];
   strcpy(type, "");
   strncat(type, t.getTypeString().f_str(), t.getTypeString().length());
   haveCreatedTypeConverter = false;
   }

