//---------------------------------------------------------------------------

#ifndef ArraySpecifierH
#define ArraySpecifierH

#include <ApsimShared/ApsimRegistration.h>
#include "ProtocolVector.h"
#include "Type.h"
#include <stdexcept>
namespace protocol {

//---------------------------------------------------------------------------
// This class encapsulates the array indexing and summing support in all
// APSIM components.  It allows:
//    name() - return sum of array (for backwards compatibility)
//    sum(name) - return sum of array
//    name(n) - return nth element of array
//    name(2-5) - return sum of elements 2 to 5 of array.
//    name(2:4) - return elements 2 to 4 of array.
//    sum(name(2-5)) - sum elements 2 to 5.
//---------------------------------------------------------------------------
class EXPORT ArraySpecifier
   {
   public:
      ArraySpecifier(unsigned num);
      ArraySpecifier(unsigned number1, unsigned number2, bool doSum);

      //---------------------------------------------------------------------------
      // return the lower and upper bounds of the array.
      //---------------------------------------------------------------------------
      unsigned getLowerBound(void);
      unsigned getUpperBound(void);

      //---------------------------------------------------------------------------
      // Convert the array of numbers in the specified messageData
      // to the required element(s).
      //---------------------------------------------------------------------------
      void convert(MessageData& messageData, DataTypeCode typeCode);

      //---------------------------------------------------------------------------
      // Create an array specifier if necessary.  The caller assumes ownership
      // of the returned arraySpecifier and should delete it accordingly.
      // NB This routine will also change name and registeredType if it creates
      // an array specifier.  Name will have the array specification eg. (2-5)
      // removed and registeredType will become and array e.g. array="T"
      //---------------------------------------------------------------------------
      static ArraySpecifier* create(ApsimRegistration *);

   private:
      unsigned number1;
      unsigned number2;
      bool doSum;


      //---------------------------------------------------------------------------
      // Convert the specified array of numbers to the required element(s).
      // returns true if all ok.
      //---------------------------------------------------------------------------
      template <class T>
      bool convertArray(MessageData& messageData, vector<T>& values)
         {
         messageData >> values;

         unsigned start = number1;
         unsigned end = number2;
         if (start == 0)
            {
            end = values.size();
            start = 0;
            }
         else
            start--;
         if (doSum)
            {
            T sum = 0;
            for (unsigned i = start; i < end; ++i)
               sum += values[i];
            values.empty();
            values.push_back(sum);
            }
         else
            {
            unsigned index = 0;
            for (unsigned i = start; i < end; ++i)
               {
               if (i >= values.size())
                  throw std::runtime_error("Invalid array index.");
               values[index++] = values[i];
               }

            while (values.size() > end-start)
               values.erase(values.size()-1);
            }

         messageData.reset();
         messageData << values;
         messageData.reset();
         return true;
         }

   };



} // namespace protocol

#endif
