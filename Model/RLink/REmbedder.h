//---------------------------------------------------------------------------
#ifndef REmbedderH
#define REmbedderH

#ifdef WIN32
  // On Windows, RInside provides a get/setenv that doesnt talk with the windows equivalent. Use it.
  extern "C" int setenv(const char *env_var, const char *env_val, int dummy);
#endif

class RInside;
extern RInside *ptrR;

void apsimFatalError( std::string message );


#endif
