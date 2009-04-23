//---------------------------------------------------------------------------
#ifndef ArraySpecifierH
#define ArraySpecifierH

#include <string>
#include <vector>

class MessageData;
//---------------------------------------------------------------------------
// This class encapsulates the array indexing and summing support in all
// APSIM components.  Array spec can be:
//    name() - return sum of array (for backwards compatibility)
//    sum(name) - return sum of array
//    name(n) - return nth element of array
//    name(2-5) - return elements 2 to 5 of array.
//    name(2:4) - return elements 2 to 4 of array.
//    sum(name(2-5)) - sum elements 2 to 5.
//---------------------------------------------------------------------------
class ArraySpecifier
   {
   public:
      //---------------------------------------------------------------------------
      // Create an array specifier if necessary.  The caller assumes ownership
      // of the returned arraySpecifier and should delete it accordingly.
      // Returns NULL if no array specified found.
      //---------------------------------------------------------------------------
      static ArraySpecifier* create(const std::string& name);

      std::string variableName() {return varName;}
      void adornVariableName(std::string& varName);
      unsigned length(void) {return 0;};

      void summariseData(MessageData& messageData, const std::string& ddml);
   private:
      int startIndex;
      int endIndex;
      std::string functionName;
      std::string varName;
      bool oldStyleSum;

      void parseFunctionName(const std::string& functionName);

      //----------------------------------------------------------------------
      // Process the specified array and get rid of the values that don't
      // match the array specified passed into the constructor.
      //----------------------------------------------------------------------
      template <class T>
      void processArray(std::vector<T>& values)
         {
         if (values.size() > 0)
            {
            int start = startIndex;
            int end = endIndex;
            if (start == -1)
               start = 0;
            else
               start--;   // 1 based to zero based indexing
            if (end == -1)
               end = (int) values.size()-1;
            else
               end--;     // 1 base to zero based indexing.


            unsigned index = 0;
            for (int i = start; i <= end; i++)
               {
               if (i >= (int)values.size())
                  throw std::runtime_error("Invalid array index.");
               values[index++] = values[i];
               }

            values.erase(values.begin()+index, values.end());

            if (functionName == "sum")
               {
               T sum = 0;
               for (unsigned i = 0; i != values.size(); i++)
                  sum += values[i];
               values.erase(values.begin(), values.end());
               values.push_back(sum);
               }
            }
         }

   };



#endif
