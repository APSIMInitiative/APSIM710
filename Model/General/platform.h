#ifdef __WIN32__
   #define EXPORT  __declspec(dllexport)
 #ifdef _MSC_VER   
   #define STDCALL
 #else
   #define STDCALL __stdcall
 #endif  
#else
   #define EXPORT
   #define STDCALL
#endif
