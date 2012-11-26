//---------------------------------------------------------------------------
#ifndef REmbedderH
#define REmbedderH

#ifdef WIN32
  // On Windows, RInside provides a get/setenv that doesnt talk with the windows equivalent. Use it.
  extern "C" int setenv(const char *env_var, const char *env_val, int dummy);
  #define STDCALL __attribute__((stdcall))
#else
  #define STDCALL 
#endif

// C++ Callbacks from R 
typedef void (*V_CHAR4_FN)(const char*, const char*, char*, void *);
extern V_CHAR4_FN apsimCallback;

class RInside;
extern RInside *ptrR;

#endif
