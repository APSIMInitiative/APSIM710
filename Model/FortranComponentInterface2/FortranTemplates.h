#ifndef FortranTemplatesH
#define FortranTemplatesH

#include <ComponentInterface2/Interfaces.h>
#include <FortranComponentInterface2/FortranComponentWrapper.h>

// -------------------------------------------------------------------
// A wrapper class for CMP events, gets and sets that take a single
// data item as an arguemnt.
// -------------------------------------------------------------------
template <class FT, class T>
class FortranMethod : public Packable
   {
   private:
      FT* f;
      T variable;
      FortranComponentWrapper* componentInterface;
   public:
      FortranMethod(FortranComponentWrapper* componentI, FT* fn)
         {
         f = fn;
         componentInterface = componentI;
         }
      virtual unsigned memorySize()
         {
         componentInterface->swapInstanceIn();
         f(&variable);
         componentInterface->swapInstanceOut();
         return ::memorySize(variable);
         }
      virtual void pack(MessageData& messageData)
         {
         componentInterface->swapInstanceIn();
         f(&variable);
         ::pack(messageData, variable);
         componentInterface->swapInstanceOut();
         }
      virtual void unpack(MessageData& messageData, const std::string& sourceDDML)
         {
         componentInterface->swapInstanceIn();
         ::unpack(messageData, sourceDDML, variable);
         f(&variable);
         componentInterface->swapInstanceOut();
         }
      virtual std::string ddml() {return DDML(variable);}

   };

template <class FT, class T>
class FortranDualMethod : public Packable
   {
   private:
      FT* getter;
      FT* setter;
      T* variable;
      FortranComponentWrapper* componentInterface;
   public:
      FortranDualMethod(FortranComponentWrapper* componentI, FT* getterFn, FT *setterFn)
         {
         getter = getterFn;
         setter = setterFn;
         componentInterface = componentI;
         variable = new T;
         }
      ~FortranDualMethod() {delete variable;}
      virtual unsigned memorySize()
         {
         componentInterface->swapInstanceIn();
         getter(variable);
         componentInterface->swapInstanceOut();
         return ::memorySize(*variable);
         }
      virtual void pack(MessageData& messageData)
         {
         componentInterface->swapInstanceIn();
         ::pack(messageData, *variable);
         componentInterface->swapInstanceOut();
         }
      virtual void unpack(MessageData& messageData, const std::string& sourceDDML)
         {
         componentInterface->swapInstanceIn();
         ::unpack(messageData, sourceDDML, *variable);
         setter(variable);
         componentInterface->swapInstanceOut();
         }
      virtual std::string ddml() {return DDML(*variable);}

   };

template <class FT, class T>
class FortranNamedDualMethod : public Packable
   {
   private:
      std::string apsimName;
      FT getter;
      FT setter;
      T variable;
      FortranComponentWrapper* componentInterface;
   public:
       FortranNamedDualMethod(FortranComponentWrapper* componentI, const std::string & name, T initvalue, FT getterFn, FT setterFn)
         {
         apsimName = name;
         getter = getterFn;
         setter = setterFn;
         componentInterface = componentI;
         variable = initvalue;
         }
      virtual unsigned memorySize()
         {
         componentInterface->swapInstanceIn();
         getter(apsimName.c_str(), &variable, apsimName.length());
         componentInterface->swapInstanceOut();
         return ::memorySize(variable);
         }
      virtual void pack(MessageData& messageData)
         {
         componentInterface->swapInstanceIn();
         ::pack(messageData, variable);
         componentInterface->swapInstanceOut();
         }
      virtual void unpack(MessageData& messageData, const std::string& sourceDDML)
         {
         componentInterface->swapInstanceIn();
         ::unpack(messageData, /*sourceDDML, */ variable);
         setter(apsimName.c_str(), &variable, apsimName.length());
         componentInterface->swapInstanceOut();
         }
      virtual std::string ddml() {return DDML(variable);}

   };

// -------------------------------------------------------------------
// A wrapper class for CMP events, gets and sets that take a single
// data item as an arguemnt.
// -------------------------------------------------------------------
template <class FT>
class FortranNullMethod : public Packable
   {
   private:
      FT* f;
      FortranComponentWrapper* componentInterface;
   public:
      FortranNullMethod(FortranComponentWrapper* componentI, FT* fn)
         {
         componentInterface = componentI;
         f = fn;
         }
      virtual unsigned memorySize()
         {
         return 0;
         }
      virtual void pack(MessageData& messageData)
         {
         throw std::runtime_error("Cannot call pack on a FortranNullMethod");
         }
      virtual void unpack(MessageData& messageData, const std::string& sourceDDML)
         {
         componentInterface->swapInstanceIn();
         f();
         componentInterface->swapInstanceOut();
         }
      virtual std::string ddml() {return "<type/>";}
   };
#endif

