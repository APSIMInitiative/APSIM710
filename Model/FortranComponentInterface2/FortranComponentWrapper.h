#ifndef FortranComponentWrapper2H
#define FortranComponentWrapper2H
#include <General/platform.h>
#include <stack>

class ScienceAPI;
class CMPComponentInterface;

class FortranComponentWrapper
   {
   public:
      FortranComponentWrapper(ScienceAPI* scienceAPI,
                              CMPComponentInterface* componentInterface,
                              void* dllHandle);
      ~FortranComponentWrapper();
      ScienceAPI& scienceAPI() {return scienceapi;}
      CMPComponentInterface& componentInterface() {return *componentinterface;}

      void swapInstanceIn();
      void swapInstanceOut();

   private:
      struct CommonBlock
         {
         const char* id;
         unsigned int idSize;
         const char* g;
         unsigned int gSize;
         const char* p;
         unsigned int pSize;
         const char* c;
         unsigned int cSize;
         unsigned int dummy1;
         unsigned int dummy2;
         unsigned int dummy3;
         unsigned int dummy4;
         unsigned int dummy5;
         unsigned int dummy6;
         unsigned int dummy7;
         unsigned int dummy8;
         unsigned int dummy9;
         unsigned int dummy10;
         };

      std::stack<FortranComponentWrapper *> callStack;
      ScienceAPI& scienceapi;
      CMPComponentInterface* componentinterface;
      void* dllHandle;
      typedef void (STDCALL NullMethod)();
      typedef void (STDCALL BoolMethod)(const int* doAllocate);
      BoolMethod* allocDealloc;
      CommonBlock *realCommonBlock;
      CommonBlock ourCommonBlock;

      void onInit1();
   };

#endif

