// Ugliness mostly due to inability to include tk.h into apsim framework..

#ifdef __WIN32__
#include <windows.h>
#endif

#include <string.h>
#include <tcl.h>
#include <tk.h>
#include <stdexcept>

#ifdef __WIN32__
extern "C" void TclWinInit(HINSTANCE);
extern "C" void TkWinXCleanup(HINSTANCE);
extern "C" void TkWinXInit(HINSTANCE);
#endif

extern int apsimGetProc(ClientData , Tcl_Interp *, int , Tcl_Obj * CONST []);
extern int apsimGetOptionalProc(ClientData , Tcl_Interp *, int , Tcl_Obj * CONST []);
extern int apsimSetProc(ClientData , Tcl_Interp *, int , Tcl_Obj * CONST []);
extern int apsimRegisterGetSetProc(ClientData , Tcl_Interp *, int , Tcl_Obj * CONST []);
extern int apsimSendMessageProc(ClientData , Tcl_Interp *, int , Tcl_Obj * CONST []);
extern int apsimSendRawMessageProc(ClientData , Tcl_Interp *, int , Tcl_Obj * CONST []);
extern int apsimWriteToSummaryFileProc(ClientData , Tcl_Interp *, int , Tcl_Obj * CONST []);
extern int apsimRegisterEvent(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[]);
extern int apsimSubscribeNull(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[]);
extern int apsimSubscribeVariant(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[]);
//extern int apsimUnRegisterEvent(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[]);
//extern int apsimCatchMessages(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[]);
extern int apsimGetComponentXML(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[]);
extern int apsimGetChildren(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[]);
extern int apsimGetFQName(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[]);

static char GlobalDllName[4096];

Tcl_Interp *NewInterp (Tcl_Interp *topLevel, ClientData cd, const char *interpName)
   {
   Tcl_Interp *interp;
   if (topLevel == NULL) {
      interp = Tcl_CreateInterp();
      if (interp == NULL) { throw std::runtime_error("CreateInterp failed");}
   } else {
      if ((interp = Tcl_CreateSlave(topLevel, interpName, 0))== NULL) { 
         char msg[1024];
         strcpy(msg,"CreateSlave '");
         strcat(msg,interpName);
         strcat(msg,"' failed.");
         throw std::runtime_error(msg);
      }
   }
   Tcl_Preserve((ClientData) interp);
   Tcl_InitMemory(interp);

   Tcl_FindExecutable(GlobalDllName); // This is ignored anyway - they use GetModule()!!

   Tcl_VarEval(interp, "set tcl_library [file join [file dirname ", GlobalDllName, "] TclLink/lib/tcl[info tclversion]]", NULL);
   Tcl_VarEval(interp, "set tk_library [file join [file dirname ", GlobalDllName, "] TclLink/lib/tk[info tclversion]]", NULL);

   Tcl_SetVar(interp, "argv", "", TCL_GLOBAL_ONLY);
   Tcl_SetVar(interp, "argc", "0", TCL_GLOBAL_ONLY);

   Tcl_DString argString;
   Tcl_ExternalToUtfDString(NULL, GlobalDllName, -1, &argString);
   Tcl_SetVar(interp, "argv0", Tcl_DStringValue(&argString), TCL_GLOBAL_ONLY);
   Tcl_SetVar(interp, "tcl_interactive", "0", TCL_GLOBAL_ONLY);
   Tcl_DStringFree(&argString);

   if (Tcl_Init(interp) != TCL_OK) {
#ifdef __WIN32__
      MessageBox(0, interp->result, "Error in Tcl Startup", MB_ICONSTOP); 
#else
      fprintf(stderr, "Error in Tcl Startup\n%s", interp->result);
#endif
      return NULL;
   }

   Tcl_StaticPackage(interp, "Tk", Tk_Init, Tk_SafeInit);
   if (Tk_Init(interp) != TCL_OK) {
#ifdef __WIN32__
      MessageBox(0, interp->result, "Error in Tk Startup", MB_ICONSTOP); 
#else
      fprintf(stderr, "Error in Tk Startup\n%s", interp->result);
#endif
      return NULL;
   }

   Tcl_CreateObjCommand(interp, "apsimGet", apsimGetProc, cd, NULL);
   Tcl_CreateObjCommand(interp, "apsimGetOptional", apsimGetOptionalProc, cd, NULL);
   Tcl_CreateObjCommand(interp, "apsimSet", apsimSetProc, cd, NULL);
   Tcl_CreateObjCommand(interp, "apsimRegisterGetSet", apsimRegisterGetSetProc, cd, NULL);
   Tcl_CreateObjCommand(interp, "apsimSendMessage", apsimSendMessageProc, cd, NULL);
   Tcl_CreateObjCommand(interp, "apsimSendRawMessage", apsimSendRawMessageProc, cd, NULL);
   Tcl_CreateObjCommand(interp, "apsimWriteToSummaryFile", apsimWriteToSummaryFileProc, cd, NULL);
   Tcl_CreateObjCommand(interp, "apsimRegisterEvent", apsimRegisterEvent, cd, NULL);
   Tcl_CreateObjCommand(interp, "apsimSubscribeNull", apsimSubscribeNull, cd, NULL);
   Tcl_CreateObjCommand(interp, "apsimSubscribeVariant", apsimSubscribeVariant, cd, NULL);
//   Tcl_CreateObjCommand(interp, "apsimUnRegisterEvent", apsimUnRegisterEvent, cd, NULL);
//   Tcl_CreateObjCommand(interp, "apsimCatchMessages", apsimCatchMessages, cd, NULL);
   Tcl_CreateObjCommand(interp, "apsimGetComponentXML", apsimGetComponentXML, cd, NULL);
   Tcl_CreateObjCommand(interp, "apsimGetChildren", apsimGetChildren, cd, NULL);
   Tcl_CreateObjCommand(interp, "apsimGetFQName", apsimGetFQName, cd, NULL);
   return interp;
   }

void StartTcl (const char *dllName)
   {
   const char *q=dllName;
   char *p= GlobalDllName; 
   while (*q != '\0') {
       if (*q == '\\') {
         *p = '/';
       } else {
         *p = *q;
       }
       p++, q++;
   }
   *p = '\0';
   }

void StopTcl(Tcl_Interp *interp)
   {
   //MessageBox(0, "TCl Stop", "TCl Stop", MB_ICONSTOP);
   if (!Tcl_InterpDeleted(interp))
       {
       Tk_Window t = Tk_MainWindow(interp);
       if (t != NULL) { Tk_DestroyWindow(t); }
   
       Tcl_Eval(interp, "exit");
       if (!Tcl_InterpDeleted(interp))
           {
           Tcl_DeleteInterp(interp);
           }
       Tcl_Release((ClientData) interp);
       }
   Tcl_Finalize();
   //TkWinXCleanup(hinst);
   }
