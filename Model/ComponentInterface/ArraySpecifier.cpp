//---------------------------------------------------------------------------
#pragma hdrstop
#include <stdio.h>
#include <string>
#include <stdexcept>
#ifdef __WIN32__
   #include <stdlib>
#endif

#include "ArraySpecifier.h"
using namespace protocol;

//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
ArraySpecifier::ArraySpecifier(unsigned number)
   : number1(number), number2(number)
   {
   doSum = (number == 0); 
   }
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
ArraySpecifier::ArraySpecifier(unsigned num1, unsigned num2, bool sum)
   : number1(num1), number2(num2)
   {
   doSum = sum;
   }
//---------------------------------------------------------------------------
// return the lower bounds of the array.
//---------------------------------------------------------------------------
unsigned ArraySpecifier::getLowerBound(void)
   {
   if (doSum)
      return 1;
   else
      return number1;
   }
//---------------------------------------------------------------------------
// return the upper bounds of the array.
//---------------------------------------------------------------------------
unsigned ArraySpecifier::getUpperBound(void)
   {
   if (doSum)
      return 1;
   else
      return number2;
   }

//---------------------------------------------------------------------------
// Convert the array of numbers in the specified messageData
// to the required element(s).
//---------------------------------------------------------------------------
void ArraySpecifier::convert(MessageData& messageData, DataTypeCode typeCode)
   {
   if (typeCode == DTint4)
      {
      vector<int> values;
      convertArray(messageData, values);
      }
   else if (typeCode == DTsingle)
      {
      vector<float> values;
      convertArray(messageData, values);
      }
   else if (typeCode == DTdouble)
      {
      vector<double> values;
      convertArray(messageData, values);
      }
   }

//---------------------------------------------------------------------------
// Create an array specifier if necessary.  The caller assumes ownership
// of the returned arraySpecifier and should delete it accordingly.
// NB This routine will also change name and registeredType if it creates
// an array specifier.  Name will have the array specification eg. (2-5)
// removed and registeredType will become and array e.g. array="T"
//---------------------------------------------------------------------------
ArraySpecifier* ArraySpecifier::create(ApsimRegistration *reg)
   {
   std::string indices;
   bool doSum;
   if (reg->getName().substr(0,4) == "sum(")
      {
      indices = reg->getName().substr(5); // remove the sum(
      doSum = true;
      }
   else 
      {
      indices = reg->getName();
      doSum = false;
      }

   unsigned pos;
   if ((pos = indices.find('(')) != std::string::npos)
        indices = indices.substr(pos+1);

   // We potentially have an array specifier. see if it is
   // 1. blank           e.g. ()
   // 2. number          e.g. (5)
   // 3. number-number   e.g. (2-5)
   // 4. number:number   e.g. (2:5)
   int number1=0, number2=0;
   if (sscanf(indices.c_str(), "%d:%d", &number1, &number2) == 2)
      return(new ArraySpecifier(number1, number2, doSum));
   else if (sscanf(indices.c_str(), "%d-%d", &number1, &number2) == 2)
      return(new ArraySpecifier(number1, number2, doSum));
   else if (sscanf(indices.c_str(), "%d", &number1) == 1)
      return(new ArraySpecifier(number1));
   else if (indices.size() > 0 && indices[0] == ')')
      return(new ArraySpecifier(0));

   // Need to catch the case: sum(sw) ie. no range specified
   if (doSum) return(new ArraySpecifier(0));

   return NULL;
   }

