//---------------------------------------------------------------------------
// This is built with gcc. No apsim infrastructure calls. C linkage only
//#include <Rcpp.h>
#include <RInside.h>

#ifdef WIN32
   // on Windows, RInside provides a get/setenv that doesnt talk with the windows equivalent
   extern "C" int setenv(const char *env_var, const char *env_val, int dummy);
#endif

static RInside *R = NULL;
typedef void __attribute__((stdcall)) (*V_CHAR_FN)(const char*);
static V_CHAR_FN apsimCallback = NULL;

// C++ Callbacks.
extern "C" void apsimPublishNull( std::string eventName ){
  if (apsimCallback != NULL) 
    apsimCallback(eventName.c_str());
}

RCPP_MODULE(apsim){
  using namespace Rcpp ;
  function( "apsimPublishNull", &apsimPublishNull ) ;
}  

///////////////////START
// Return false on error
extern "C" bool EmbeddedR_Start(const char *R_Home, const char *UserLibs)
{
   int argc = 0;
   char **argv = NULL;
   //std::cout << "EmbeddedR_Start  called" << std::endl; std::cout.flush();
   if (R_Home != NULL) { setenv("R_HOME", R_Home, 1) ; /*std::cout << "R Home is " << R_Home << std::endl; std::cout.flush(); */}
   if (UserLibs != NULL) { setenv("R_LIBS_USER", UserLibs, 1); /*std::cout << "R_UserLibs is " << UserLibs << std::endl; std::cout.flush();*/ }
   // TMPDIR??
   // R_USER??

   try {
     R = new RInside(argc, argv, true); 
   } catch (std::exception& ex) {
        std::cerr << "R Exception: " << ex.what() << std::endl; std::cerr.flush();
		return false; 
   } 	 
   //printf("EmbeddedR_Start RInside called OK\n");
   try {
     (*R)["apsim"] = LOAD_RCPP_MODULE(apsim) ;
   } catch (std::exception& ex) {
        std::cerr << "R Exception: " << ex.what() << std::endl; std::cerr.flush();
		return false; 
   } 	 
   //std::cout << "EmbeddedR_Start RCPP modules called OK\n";
  
   return true;
}

extern "C" bool EmbeddedR_Stop(const char *)
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
