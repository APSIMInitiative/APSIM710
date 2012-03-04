//---------------------------------------------------------------------------
// The windows build of this file is built with gcc. It has no apsim infrastructure calls. We cannot
// pass c++ classes as arguments - the name mangling and memory managers are incompatible.
// It requires RTools (from CRAN) to compile.

#include <RInside.h>

#ifdef WIN32
  // On Windows, RInside provides a get/setenv that doesnt talk with the windows equivalent. Use it.
  extern "C" int setenv(const char *env_var, const char *env_val, int dummy);

  #define STDCALL __attribute__((stdcall))

#else

  #define STDCALL 

#endif


// C++ Callbacks from R 
typedef void STDCALL (*V_CHAR_FN)(const char*);
static V_CHAR_FN apsimCallback = NULL;

extern "C" void apsimPublishNull( std::string eventName ){
  if (apsimCallback != NULL) 
    apsimCallback(eventName.c_str());
}

RCPP_MODULE(apsim){
  using namespace Rcpp ;
  function( "apsimPublishNull", &apsimPublishNull ) ;
}  

///////////////////START
static RInside *R = NULL;
// These "Embedded_xxxx" routines are called via dlsym() from the apsim component
// Return false on error
extern "C" bool EmbeddedR_Start(const char *R_Home, const char *UserLibs)
{
   int argc = 0;
   char **argv = NULL;
   if (R_Home != NULL) { setenv("R_HOME", R_Home, 1) ; }
   if (UserLibs != NULL) { setenv("R_LIBS_USER", UserLibs, 1); }
   // TMPDIR??
   // R_USER??

   try {
     R = new RInside(argc, argv, true); 
   } catch (std::exception& ex) {
        std::cerr << "R Exception: " << ex.what() << std::endl; std::cerr.flush();
		return false; 
   } 	 

   try {
     (*R)["apsim"] = LOAD_RCPP_MODULE(apsim) ;
   } catch (std::exception& ex) {
        std::cerr << "R Exception: " << ex.what() << std::endl; std::cerr.flush();
		return false; 
   } 	 
  
   return true;
}

extern "C" bool EmbeddedR_Stop(void)
{
  if (R == NULL)
     return false;

  delete R;

  R = NULL;
  return true;
}

extern "C" bool EmbeddedR_SetComponent(void *fPtr)
{
  apsimCallback = (V_CHAR_FN) fPtr;
  return true;
}

extern "C" bool EmbeddedR_Eval(const char *cmd)
{
  if (R == NULL)
     return false;

  R->parseEval(cmd);
  return true;
}

extern "C" bool EmbeddedR_BoolEval(const char *cmd, bool *result)
{
  *result = false;
  if (R == NULL)
     return false;

  *result = R->parseEval(cmd);
  return true;
}

extern "C" bool EmbeddedR_IntEval(const char *cmd, int *result)
{
  *result = 0;
  if (R == NULL)
     return false;

  *result = R->parseEval(cmd);
  return true;
}

extern "C" bool EmbeddedR_GetVector(const char *variable, char *result, 
                                    unsigned int resultlen, unsigned int elemwidth, 
									unsigned int *numReturned)
{
  *numReturned = 0;
  if (R == NULL)
     return false;
  try 
    {
    Rcpp::CharacterVector resultVec = (*R)[variable];
    char *ptr = result;
    for (int i = 0; i < resultVec.size() && ptr  < result + resultlen; i++)
        {
        const char *stringrep = resultVec[i];
        strncpy(ptr, stringrep, elemwidth);
        ptr[elemwidth-1] = '\0';
        ptr += elemwidth;
        (*numReturned)++;
        }		
    return true;
    } 
  catch(std::exception& ex) {
    std::cerr << "R Exception: " << ex.what() << std::endl;
  }
  return false;
}

extern "C" bool EmbeddedR_SimpleCharEval(const char *cmd, char *buf, int buflen)
{
  buf[0] = '\0';
  if (R == NULL)
     return false;

  std::string result = R->parseEval(cmd);
  strncpy(buf, result.c_str(), buflen);
  return true;
}

#ifndef WIN32
// for gcc builds under unix
bool StartR (const char *R_Home, const char *UserLibs) {return (EmbeddedR_Start(R_Home, UserLibs));}
void StopR(void) {EmbeddedR_Stop();}

int IntREval(const char *s)
  {
  int result = 0;
  if (R != NULL) 
	 result = R->parseEval(s);
  return result;
  }
  
bool BoolREval(const char *s) 
  {
  bool result = false;
  if (R != NULL) 
     result = R->parseEval(s);
  return result;
  }

void REvalQ(const char *s) 
  {
  if (R != NULL)
     R->parseEval(s);
  }

void RGetVector(const char *variable, std::vector<std::string> &result)
  {
  result.clear();
  if (R != NULL) 
     {
     Rcpp::CharacterVector resultVec = (*R)[variable];
     for (int i = 0; i < resultVec.size(); i++)
       result.push_back(std::string(resultVec[i]));
     }
  }

std::string SimpleREval(const char *s)
  {
  if (R != NULL)
     {
     std::string result = R->parseEval(s);
     return result;
     }
  return "";  
  }

#endif
