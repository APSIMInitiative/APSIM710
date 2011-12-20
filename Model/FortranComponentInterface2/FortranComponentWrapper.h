#ifndef FortranComponentWrapper2H
#define FortranComponentWrapper2H
#include <General/platform.h>
#include <stack>

class ScienceAPI2;
class CMPComponentInterface;

class FortranComponentWrapper
   {
   public:
      FortranComponentWrapper(ScienceAPI2* scienceAPI,
                              CMPComponentInterface* componentInterface,
                              void* dllHandle);
      ~FortranComponentWrapper();
      ScienceAPI2& scienceAPI() {return scienceapi;}
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
      ScienceAPI2& scienceapi;
      CMPComponentInterface* componentinterface;
      void* dllHandle;
      typedef void (STDCALL NullMethod)();
      typedef void (STDCALL BoolMethod)(const int* doAllocate);
      BoolMethod* allocDealloc;
      CommonBlock *realCommonBlock;
      CommonBlock ourCommonBlock;
      std::stack<CommonBlock> savedCommonBlocks;

      void onInit1();
   };

#endif

