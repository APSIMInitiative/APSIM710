#ifdef __WIN32__
   #define EXPORT  __declspec(dllexport)
   #define STDCALL __stdcall
#else
   #define EXPORT
   #define STDCALL
#endif
