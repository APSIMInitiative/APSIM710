#ifndef PlatformH
#define PlatformH
#ifdef __WIN32__
   #define EXPORT  __declspec(dllexport)
   #define STDCALL __stdcall
   #ifdef _MSC_VER
     #pragma warning( disable : 4068 4251 4275 4290 4996)
  #endif
#else
   #define EXPORT
   #define STDCALL __attribute__((stdcall))
#endif
#endif
