#ifndef CMPDataH
#define CMPDataH

#include <string>
#include <vector>
#include <stdexcept>
#include <typeinfo>
#include <boost/function.hpp>
#include <ComponentInterface2/Interfaces.h>
#include <ComponentInterface2/DataTypes.h>
#include <ComponentInterface2/BuiltIns.h>

void EXPORT getKindAndArray(const std::string& ddml, std::string& kind, bool& isArray);


template <class T>
class PackableWrapper : public Packable
   {
   // -------------------------------------------------------------------
   // A simple wrapper class for wrapping something and implementing a
   // Packable interface.
   // -------------------------------------------------------------------
   protected:
      T variable;
   public:
      PackableWrapper(T value) : variable(value)  {}
      virtual unsigned memorySize()               {return ::memorySize(variable);}
      virtual void pack(MessageData& messageData) {::pack(messageData, variable);}
      virtual void unpack(MessageData& messageData, const std::string& ddml)
                                                  {::unpack(messageData, variable);}
      virtual std::string ddml()                  {return DDML(variable);}
   };

template <class T>
class PackableWrapper2 : public Packable
   {
   // -------------------------------------------------------------------
   // A simple wrapper class for wrapping something and implementing a
   // Packable interface.
   // -------------------------------------------------------------------
   protected:
      T variable;
   public:
      PackableWrapper2(T value) : variable(value)  {}
      virtual unsigned memorySize()               {return ::memorySize(variable);}
      virtual void pack(MessageData& messageData) {::pack(messageData, variable);}
      virtual void unpack(MessageData& messageData, const std::string& ddml)
                                                  {::unpack(messageData, ddml, variable);}
      virtual std::string ddml()                  {return DDML(variable);}
   };

// -------------------------------------------------------------------
// A wrapper class for passing builtin types via the CMP
// -------------------------------------------------------------------
template <class T>
class CMPBuiltIn : public PackableWrapper<T>, public Convertable
   {
   public:
      CMPBuiltIn(T value) : PackableWrapper<T>(value) {}

      virtual void from(const std::vector<std::string>& values)
         {
         Convert(values, variable);
         }

      virtual void unpack(MessageData& messageData, const std::string& sourceDDML)
         {
         bool sourceIsArray;
         string sourceKind;
         getKindAndArray(sourceDDML, sourceKind, sourceIsArray);
         if (sourceKind == "")
            throw runtime_error("Cannot convert from a structure to a scalar");
         else
            {
            if (sourceIsArray)
               {
               if (sourceKind == "integer4")
                  {
                  std::vector<int> value;
                  if (typeid(variable) == typeid(std::vector<int>))
                     ::unpack(messageData, variable);
                  else
                     {
                     ::unpack(messageData, value);
                     Convert(value, variable);
                     }
                  }
               else if (sourceKind == "single")
                  {
                  std::vector<float> value;
                  if (typeid(variable) == typeid(std::vector<float>))
                     ::unpack(messageData, variable);
                  else
                     {
                     ::unpack(messageData, value);
                     Convert(value, variable);
                     }
                  }
               else if (sourceKind == "double")
                  {
                  std::vector<double> value;
                  if (typeid(variable) == typeid(std::vector<double>))
                     ::unpack(messageData, variable);
                  else
                     {
                     ::unpack(messageData, value);
                     Convert(value, variable);
                     }
                  }
               else if (sourceKind == "string")
                  {
                  std::vector<std::string> value;
                  if (typeid(variable) == typeid(std::vector<std::string>))
                     ::unpack(messageData, variable);
                  else
                     {
                     ::unpack(messageData, value);
                     Convert(value, variable);
                     }
                  }
               else if (sourceKind == "boolean")
                  {
                  std::vector<bool> value;
                  if (typeid(variable) == typeid(std::vector<bool>))
                     ::unpack(messageData, variable);
                  else
                     {
                     ::unpack(messageData, value);
                     Convert(value, variable);
                     }
                  }
               else
                  throw runtime_error("Bad kind found in ddml: " + sourceDDML);
               }
            else
               {
               if (sourceKind == "integer4")
                  {
                  if (typeid(variable) == typeid(int))
                     ::unpack(messageData, variable);
                  else
                     {
                     int value;
                     ::unpack(messageData, value);
                     Convert(value, variable);
                     }
                  }
               else if (sourceKind == "single")
                  {
                  if (typeid(variable) == typeid(float))
                     ::unpack(messageData, variable);
                  else
                     {
                     float value;
                     ::unpack(messageData, value);
                     Convert(value, variable);
                     }
                  }
               else if (sourceKind == "double")
                  {
                  if (typeid(variable) == typeid(double))
                     ::unpack(messageData, variable);
                  else
                     {
                     double value;
                     ::unpack(messageData, value);
                     Convert(value, variable);
                     }
                  }
               else if (sourceKind == "string")
                  {
                  if (typeid(variable) == typeid(std::string))
                     ::unpack(messageData, variable);
                  else
                     {
                     std::string value;
                     ::unpack(messageData, value);
                     Convert(value, variable);
                     }
                  }
               else if (sourceKind == "boolean")
                  {
                  if (typeid(variable) == typeid(bool))
                     ::unpack(messageData, variable);
                  else
                     {
                     bool value;
                     ::unpack(messageData, value);
                     Convert(value, variable);
                     }
                  }
               else
                  throw runtime_error("Bad kind found in ddml: " + sourceDDML);
               }
            }
         }

   };

// -------------------------------------------------------------------
// bounding function templates + a bounded version of CMPBuiltIn
// -------------------------------------------------------------------
void performBoundCheck(const std::string& name, int value, int lower, int upper);
void performBoundCheck(const std::string& name, float value, float lower, float upper);
void performBoundCheck(const std::string& name, double value, double lower, double upper);
template <class T>
void performBoundCheck(const std::string& name, const std::vector<T> &values, T lower, T upper)
   {
   for (unsigned i = 0; i != values.size(); i++)
      performBoundCheck(name, values[i], lower, upper);
   }

template <class T, class B>
class CMPBuiltInBounded : public CMPBuiltIn<T>
   {
   private:
      B lowerBound;
      B upperBound;
      std::string name;
   public:
      CMPBuiltInBounded(const std::string& variableName,  T value, B lower, B upper)
         : lowerBound(lower),
           upperBound(upper),
           name(variableName),
           CMPBuiltIn<T>(value) { }

      virtual void from(const std::vector<std::string>& values)
         {
         CMPBuiltIn<T>::from(values);
         performBoundCheck(name, variable, lowerBound, upperBound);
         }
      virtual void unpack(MessageData& messageData, const std::string& sourceDDML)
         {
         CMPBuiltIn<T>::unpack(messageData, sourceDDML);
         performBoundCheck(name, variable, lowerBound, upperBound);
         }
   };

// -------------------------------------------------------------------
// A wrapper class for CMP events, gets and sets that take a single
// data item as an arguemnt.
// -------------------------------------------------------------------
template <class FT, class T>
class Method : public Packable
   {
   private:
      FT f;
      T variable;
   public:
      Method(FT& fn) : f(fn) { }

      virtual unsigned memorySize()
         {
         f(variable);
         return ::memorySize(variable);
         }
      virtual void pack(MessageData& messageData)
         {
         // Assumes that memorySize was called immediately before this method.
         // No need to call it again.
         ::pack(messageData, variable);
         }
      virtual void unpack(MessageData& messageData, const std::string& sourceDDML)
         {
         ::unpack(messageData, variable);
         f(variable);
         }
      virtual std::string ddml()
         {
         return DDML(variable);
         }
   };

// -------------------------------------------------------------------
// A wrapper class for CMP events, gets and sets that take a single
// data item as an arguemnt.
// -------------------------------------------------------------------
template <class FT, class T>
class DualMethod : public Packable
   {
   private:
      FT getter;
      FT setter;
      T variable;
   public:
      DualMethod(FT& get, FT& set) : getter(get), setter(set) { }

      virtual unsigned memorySize()
         {
         getter(variable);
         return ::memorySize(variable);
         }
      virtual void pack(MessageData& messageData)
         {
         // Assumes that memorySize was called immediately before this method.
         // No need to call it again.
         ::pack(messageData, variable);
         }
      virtual void unpack(MessageData& messageData, const std::string& sourceDDML)
         {
         ::unpack(messageData, variable);
         setter(variable);
         }
      virtual std::string ddml()
         {
         return DDML(variable);
         }
   };
// -------------------------------------------------------------------
// A wrapper class for CMP events that take no data ie. null events.
// -------------------------------------------------------------------
class NullMethod : public Packable
   {
   private:
      boost::function0<void> fn;
      Null null;
   public:
      NullMethod(boost::function0<void>& fn)
         {
         this->fn = fn;
         }
      virtual unsigned memorySize()
         {
         return 0;
         }
      virtual void pack(MessageData& messageData)
         {
         throw std::runtime_error("Cannot call pack on a NullMethod");
         }
      virtual void unpack(MessageData& messageData, const std::string& sourceDDML)
         {
         fn();
         }
      virtual std::string ddml()
         {
         return DDML(null);
         }
   };
#endif
