#ifdef __WIN32__
#include <windows.h>

//extern "C" void TkWinXCleanup(HINSTANCE);
//extern "C" void TkWinXInit(HINSTANCE);
//extern "C" void TclWinInit(HINSTANCE);

//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*) {

    switch (reason) {
      case DLL_PROCESS_ATTACH:
        {
        //TclWinInit(hinst);
        //TkWinXInit(hinst);
        break;
        }
      case DLL_PROCESS_DETACH:
        {
        //TkWinXCleanup(hinst);
        break;
        }
      default:
        // Threads?
        return 0;
    }
    return 1;
}
//---------------------------------------------------------------------------

#endif
