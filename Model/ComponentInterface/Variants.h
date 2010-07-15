 //---------------------------------------------------------------------------

#ifndef VariantsH
#define VariantsH
#include "ProtocolVector.h"
#include "Variant.h"
namespace protocol {
class TypeConverter;

// ------------------------------------------------------------------
//  Short description:
//     This class encapsulates one or more variant variables as
//     returned by the returnValue message.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
class Variants
   {
   public:
      Variants(Component* parent)
         {
         }
      ~Variants(void)
         {
         empty();
         }
      unsigned int size(void) const {return variants.size();}
      void empty(void)
         {
         for (unsigned int i = 0; i < variants.size(); i++)
            delete variants[i];
         variants.empty();
         }
      void addVariant(const Variant& variant)
         {
         variants.push_back(new Variant(variant));
         }

      Variant* getVariant(unsigned int variantIndex)
         {
         if (variantIndex < variants.size())
            return variants[variantIndex];
         else
            return NULL;
         }

   private:
      vector<Variant*> variants;
   };

} // namespace protocol
#endif
