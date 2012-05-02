#ifndef FortranArrayH
#define FortranArrayH

#include <string>
#include <General/string_functions.h>
#include <stdexcept>
class MessageData;

template <class T>
class FortranArray
   {
   public:
      T* Data;
      int& NumValues;
      int MaxValues;
      FortranArray(T* data, int& numvalues, int& maxValues)
         : Data(data), NumValues(numvalues), MaxValues(maxValues)
         {}
      void readFromString(const std::string& st)
         {
         std::vector<std::string> values;
         split(st, " ", values);
         NumValues = values.size();
         if (NumValues > MaxValues)
            throw std::runtime_error("Too many values in FORTRAN array");
         for (int i = 0; i != NumValues; i++)
            Data[i] = values[i];
         }
      void push_back(T& value)
         {
         Data[NumValues] = value;
         NumValues++;
         if (NumValues > MaxValues)
            throw std::runtime_error("Too many values in FORTRAN array");
         }
   };

template <class T> void unpack(MessageData& messageData, FortranArray<T>& value)
   {
   ::unpack(messageData, value.NumValues);
   for (int i = 0; i != value.NumValues; i++)
      ::unpack(messageData, value.Data[i]);
   }
template <class T> void pack(MessageData& messageData, const FortranArray<T>& value)
   {
   ::pack(messageData, value.NumValues);
   for (int i = 0; i != value.NumValues; i++)
      ::pack(messageData, value.Data[i]);

   }
template <class T> unsigned memorySize(const FortranArray<T>& value)
   {
   return 4 + value.NumValues * ::memorySize(value.Data[0]);
   }
template <class T> std::string DDML(const FortranArray<T>& value)
   {
   std::string ddml = ::DDML(value.Data[0]);
   addAttributeToXML(ddml, "array=\"T\"");
   return ddml;
   }

template <class TO>
void Convert(bool source, FortranArray<TO>& toVector)
   {
   TO to;
   ::Convert(source, to);
   toVector.push_back(to);
   }
template <class TO>
void Convert(int source, FortranArray<TO>& toVector)
   {
   TO to;
   ::Convert(source, to);
   toVector.push_back(to);
   }
template <class TO>
void Convert(float source, FortranArray<TO>& toVector)
   {
   TO to;
   ::Convert(source, to);
   toVector.push_back(to);
   }
template <class TO>
void Convert(double source, FortranArray<TO>& toVector)
   {
   TO to;
   ::Convert(source, to);
   toVector.push_back(to);
   }
template <class TO>
void Convert(const std::string& source, FortranArray<TO>& toVector)
   {
   TO to;
   ::Convert(source, to);
   toVector.push_back(to);
   }

template <class T>
void performBoundCheck(const std::string& name, const FortranArray<T>& value, T lower, T upper)
   {
   for (int i = 0; i != value.NumValues; i++)
      {
      ::performBoundCheck(name, value.Data[i], lower, upper);
      }
   }

/*template <class FROM, class TO>
void Convert(const FROM& source, FortranArray<TO>& toVector)
   {
   TO to;
   ::Convert(source, to);
   toVector.push_back(to);
   } */
template <class FROM, class TO>
void Convert(const std::vector<FROM>& source, FortranArray<TO>& toVector)
   {
   for (unsigned i = 0; i != source.size(); i++)
      {
      TO to;
      ::Convert(source[i], to);
      toVector.push_back(to);
      }
   }

#endif
