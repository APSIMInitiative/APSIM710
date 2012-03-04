#include <string.h>
#include <stdlib.h>
#include <iostream>

#ifdef __WIN32__
#include <windows.h>
#endif

#include <General/platform.h>
#include <General/string_functions.h>
#include <General/stristr.h>
#include <General/path.h>
#include <General/dll.h>

#include <ComponentInterface2/ScienceAPI2.h>

#include "RComponent.h"

using namespace std;
static void *RDLLHandle = NULL;
static RComponent *currentComponent = NULL;

extern "C" EXPORT RComponent  * STDCALL createComponent(ScienceAPI2& scienceAPI)
   {
   return new RComponent(scienceAPI);
   }

extern "C" void EXPORT STDCALL deleteComponent(RComponent* component)
   {
   if (component)
     delete component;
   }

extern "C" void EXPORT STDCALL componentCallback(const char *s)
   {
	if (currentComponent) currentComponent->publishNull(string(s));
   }

#ifdef __WIN32__
bool GetStringRegKey(const std::string &strKeyName,const std::string &strValueName, std::string &strValue)
{
   strValue = "";
   HKEY  keys[] = {HKEY_LOCAL_MACHINE, HKEY_CURRENT_USER};
   for (int i = 0; i < 2; i++) 
     {
     HKEY CurrentKey = keys[i];
     HKEY hKey;
     LONG lRes = RegOpenKeyEx(CurrentKey, strKeyName.c_str(), 0, KEY_READ, &hKey);
     if (lRes == ERROR_SUCCESS) 
        {
        char szBuffer[1024];
        DWORD dwBufferSize = sizeof(szBuffer);
        ULONG nError;
        nError = RegQueryValueEx(hKey, strValueName.c_str(), 0, NULL, (LPBYTE)szBuffer, &dwBufferSize);
        if (ERROR_SUCCESS == nError)
	      {
          strValue = szBuffer;
	      return true;
	      }
        }
     }	
   return false;
}

//--------------------------- R Embedder DLL initialisation
typedef bool (*B_VOID_FN)(void);
typedef bool (*B_CHAR_FN)(const char*);
typedef bool (*B_CHAR2_FN)(const char*, const char*);
typedef bool (*B_VOIDPTR_FN)(void *);
typedef bool (*B_BOOL_FN)(const char*, bool *);
typedef bool (*B_INT_FN)(const char*, int *);
typedef bool (*B_VEC_FN)(const char*, char *, unsigned int, unsigned int, unsigned int *);
typedef bool (*B_2CHAR_FN)(const char*, char*, int);

B_CHAR2_FN   R_StartFn;
B_VOIDPTR_FN R_SetCallbackFn;
B_CHAR_FN    R_EvalCharFn;
B_BOOL_FN    R_EvalBoolFn;
B_INT_FN     R_EvalIntFn;
B_VEC_FN     R_GetVecFn;
B_2CHAR_FN   R_EvalCharSimpleFn;

// Load and initialise the dll. Call once.
bool StartR (const char *R_Home, const char *UserLibs, const char *exeName)
   {
    RDLLHandle = loadDLL(exeName);
    if (RDLLHandle == NULL) 
      throw std::runtime_error(string("Can't load R DLL ") + exeName);     

    R_StartFn = (B_CHAR2_FN) dllProcAddress(RDLLHandle, "EmbeddedR_Start");
    if (R_StartFn == NULL) 
       goto baddll; 

    if (!R_StartFn(R_Home, UserLibs )) 
       throw std::runtime_error(string("R_Start failed in ") + exeName);     

    R_SetCallbackFn = (B_VOIDPTR_FN) dllProcAddress(RDLLHandle, "EmbeddedR_SetComponent");
    if (!R_SetCallbackFn((void *)&componentCallback)) 
       throw std::runtime_error(string("R_SetComponent failed in ") + exeName);     

    if (NULL == (R_EvalCharFn = (B_CHAR_FN) dllProcAddress(RDLLHandle, "EmbeddedR_Eval"))) goto baddll; 
    if (NULL == (R_EvalBoolFn = (B_BOOL_FN) dllProcAddress(RDLLHandle, "EmbeddedR_BoolEval"))) goto baddll; 
    if (NULL == (R_EvalIntFn = (B_INT_FN) dllProcAddress(RDLLHandle, "EmbeddedR_IntEval"))) goto baddll;
    if (NULL == (R_GetVecFn = (B_VEC_FN) dllProcAddress(RDLLHandle, "EmbeddedR_GetVector"))) goto baddll;
    if (NULL == (R_EvalCharSimpleFn = (B_2CHAR_FN) dllProcAddress(RDLLHandle, "EmbeddedR_SimpleCharEval"))) goto baddll;
    
	return 1; 
baddll:
    throw std::runtime_error(string("DLL missing symbol in ") + exeName);     
    }

// Delete an interpreter
void StopR(void)
   {
   B_VOID_FN R_StopFn;
   R_StopFn = (B_VOID_FN) dllProcAddress(RDLLHandle, "EmbeddedR_Stop");
   if (R_StopFn != NULL) R_StopFn();
   }

// Quietly evaluate something   
void REvalQ(const char *s) 
{
   if (R_EvalCharFn != NULL) R_EvalCharFn(s);
}

bool BoolREval(const char *s) 
{
   bool result = false;
   if (R_EvalBoolFn != NULL) R_EvalBoolFn(s, &result);
   return (result);
}

int IntREval(const char *s) 
{
   int result = 0;
   if (R_EvalIntFn != NULL) R_EvalIntFn(s, &result);
   return (result);
}

void RGetVector(const char *s, std::vector<std::string> &result)
{
   const int numWidth = 16;
   result.clear();
   
   char *buffer = new char [2048 * numWidth];
   memset(buffer, 0, sizeof(buffer));
   unsigned int numReturned = 0;
   if (R_GetVecFn != NULL) R_GetVecFn (s, buffer, sizeof(buffer), numWidth, &numReturned);
   result.resize(numReturned);
   char *ptr = buffer;
   for (int i = 0; i < numReturned; i++) 
     {
	 result[i] = ptr;
	 ptr += numWidth;
	 }
   delete [] buffer;	 
}

// Evaluate something and return as concantenated string
std::string SimpleREval(const char *s) 
{
   char buffer[1024];
   if (R_EvalCharSimpleFn != NULL) R_EvalCharSimpleFn(s, buffer, sizeof(buffer));
   return (std::string(buffer));
}
#else

// unix/gcc equivalents are simpler.
extern int IntREval(const char *s) ;
extern bool BoolREval(const char *s); 
extern void REvalQ(const char *s) ;
extern void StopR(void);
extern bool StartR (const char *R_Home, const char *UserLibs);
extern void RGetVector(const char *s, std::vector<std::string> &result);
extern std::string SimpleREval(const char *s);

#endif

// ------------------------------------------------------------------
// Initialise and destroy the component. 
// ------------------------------------------------------------------
RComponent::RComponent(ScienceAPI2 & api) : apsimAPI(api)
   {
   hasFatalError = false;
   apsimAPI.subscribe("error", nullFunction(&RComponent::onError));
   apsimAPI.subscribe("init2", nullFunction(&RComponent::onInit2));
   currentComponent = this;
   }

RComponent::~RComponent(void)
   {
   StopR();
   RDLLHandle = NULL;
   }

void RComponent::onInit2(void)
   {
   apsimAPI.write("RLink Initialisation\n");
   if (RDLLHandle != NULL) throw std::runtime_error("R has already been loaded");

   rules.clear();
   apsimAPI.readScripts(rules);
 
   string apsimDLL = apsimAPI.getExecutableFileName();
   replaceAll(apsimDLL, "\\", "/"); 
#ifdef __WIN32__
   string installPath, userlibs;
   // Load R.dll first so that the embedder dll resolves to the loaded version and not something unknown
   string versionString;
   if (!GetStringRegKey("Software\\R-core\\R", "Current Version", versionString))
     if (!GetStringRegKey("Software\\R-core\\R32", "Current Version", versionString))
         throw std::runtime_error("No R version info");
   
   string installPathKey = "Software\\R-core\\R\\" + versionString ;
   if (!GetStringRegKey(installPathKey, "InstallPath", installPath))
     {
	 installPathKey = "Software\\R-core\\R32\\" + versionString ;
      if (!GetStringRegKey(installPathKey, "InstallPath", installPath))
         throw std::runtime_error("No R install info in " + installPathKey);
     }		
   replace(installPath.begin(), installPath.end(), '\\', '/'); 
   apsimAPI.write("Loading R from " + installPath + "\n");
   string Rdll = installPath + "/bin/i386/R.dll";
   if (loadDLL(Rdll) == NULL) throw std::runtime_error("Can't load R DLL " + Rdll);  

   apsimAPI.write("Embedding R in " + fileDirName(apsimDLL) + "\n");
   string EXE = fileDirName(apsimDLL) + "/REmbed.dll";

   char *p = getenv("USERPROFILE");
   if (p != NULL) {
	  userlibs = p;
      replace(userlibs.begin(), userlibs.end(), '\\', '/');
	  userlibs += "/Documents/R/win-library/";
	  vector<string> vnums;
	  split(versionString, ".", vnums);
      userlibs += vnums[0];
	  userlibs += ".";
	  userlibs += vnums[1];
   }

   if (!StartR(installPath.c_str(), 
               userlibs.c_str(), 
		       EXE.c_str()))
		throw std::runtime_error("Cant Start R");
#else
   string EXE = fileDirName(apsimDLL) + "/REmbed.so";;
   StartR(NULL, NULL);
#endif

   // write copyright notice(s).
   apsimAPI.write(SimpleREval("R.version.string") + "\n");
   apsimAPI.write("Copyright (C) 2011 The R Foundation for Statistical Computing\n");

   for (map<string,string>::iterator i = rules.begin(); i != rules.end(); i++) 
       {
	   if  (i->first != "my variables" && i->first != "apsim variables") 
	      {
          string msg;
          msg = "--->Section: "; msg += i->first; msg += "\n";
          msg += i->second; msg += "\n";
          apsimAPI.write(msg);
          if (i->first != "init") 
             apsimAPI.subscribe(i->first, namedNullFunction(i->first, &RComponent::onNullEventCallback));
          }
	   }

   apsimAPI.write("--->End\n");

   // Expose variables
   vector<string> rawVariables; vector<string> variables;
   Split_string(rules["my variables"], "\r\n", rawVariables); 
   for (vector<string>::iterator i = rawVariables.begin(); i != rawVariables.end(); i++)
       {
	   unsigned int p = i->find('#');
	   if (p != string::npos) 
	      *i = i->substr(0, p);
       stripLeadingTrailing(*i, " ");
	   if (*i != "") 
	      variables.push_back(*i);
       }
   if (variables.size() > 0) 
      {
	  apsimAPI.write("--->Exported R Variables:\n");
      for (vector<string>::iterator i = variables.begin(); i != variables.end(); i++)
	      {
          apsimAPI.write(*i + "\n");
          expose(*i);
		  }
      }
   // find the apsim variables we want before every event
   variables.clear();
   Split_string(rules["apsim variables"], "\r\n", variables); 
   if (variables.size() > 0) 
      {
      apsimAPI.write("--->Imported APSIM Variables:\n");
      for (vector<string>::iterator i = variables.begin(); i != variables.end(); i++)
        {
	    vector<string> words;
        SplitStringHonouringQuotes(*i, " ", words);
	    if (words.size() == 1)
	       {
		   apsimVariables[words[0]] = words[0]; 
		   apsimAPI.write(words[0] + "\n"); 
		   }
	    else if (words.size() == 3 && words[1] == "as")
	       {
 	   	   stripLeadingTrailing(words[0], "\"");
	   	   apsimVariables[words[0]] = words[2]; 
	   	   apsimAPI.write(words[2] + " <- " + words[0] + "\n"); 
	   	   }
	    else if (words.size() > 0) 
	   	   throw std::runtime_error("Cant make sense of '" + *i + "'");
        }	  
      }

	  // Do the init rule if specified..
   string initRule = rules["init"];
   if (initRule != "")
      {
      importVariables();	 
      REvalQ(initRule.c_str());
      }
   }

void RComponent::onError(void)
   {
   hasFatalError = true;
   }

// Allow apsim to read (and/ or write) one of our variables. 
void RComponent::expose(const std::string &variableName)
   {
   apsimAPI.exposeFunction2(variableName, "", "", 
                           StringArrayFunction2(variableName, &RComponent::respondToGet),
                           StringArrayFunction2(variableName, &RComponent::respondToSet));
   }

void RComponent::subscribeNull(const std::string &event, const std::string &script) 
   {
   rules[event] += script;
   apsimAPI.subscribe(event, namedNullFunction(event, &RComponent::onNullEventCallback));
   }

void RComponent::publishNull(const std::string &event) 
   {
   if (!hasFatalError) apsimAPI.publish(event);
   }

// Deal with an event coming from the system to us
void RComponent::onNullEventCallback(const std::string &s)
   {
   if (!hasFatalError) 
     {
     string rule = rules[s];
     if (rule != "")
       {
       importVariables();	 
       REvalQ(rule.c_str());
	   }
     }
   }

// ------------------------------------------------------------------
// Return a variable to caller. Everything is a string.
// ------------------------------------------------------------------
void RComponent::respondToGet(const std::string &variableName, std::vector<std::string> &result)
   {
   result.clear(); 
   RGetVector(variableName.c_str(), result);
   }

// ------------------------------------------------------------------
// Set the value of a variable for the specified variable name.
// ------------------------------------------------------------------
void RComponent::respondToSet(const std::string &variableName, std::vector<std::string> &value)
   {
   string cmd = variableName;
   cmd += "<-";
   if (value.size() > 1) 
      {	
      cmd += "c(";
      for (std::vector<string>::iterator p = value.begin(); p != value.end(); p++)
	      {
		  if (p != value.begin()) cmd += ",";
		  //cmd += "'";
		  cmd += *p;
		  //cmd += "'";
		  }
      cmd += ")";
      }
   else
      cmd += value[0];
   REvalQ(cmd.c_str());
   }
   
void RComponent::importVariables(void) 
{
  for (Name2RuleMap::iterator p = apsimVariables.begin(); p != apsimVariables.end(); p++)
     {
	 vector<string> resultArray;
     apsimAPI.get(p->first, "", 1, resultArray);
	 if (resultArray.size() > 0) 
	    respondToSet(p->second, resultArray);
	 }
}
  
#if 0
int apsimRegisterEvent(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[])
{
	 return (apsimSubscribeNull(cd, interp, objc, objv));
}

#endif
