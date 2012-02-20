//---------------------------------------------------------------------------
#ifndef REmbedderH
#define REmbedderH

extern "C" bool R_Start(void);        // Return false on error
extern "C" bool R_Eval(const char *); 

int apsimGetProc(ClientData , Tcl_Interp *, int , Tcl_Obj * CONST []);
int apsimGetOptionalProc(ClientData , Tcl_Interp *, int , Tcl_Obj * CONST []);
int apsimSetProc(ClientData , Tcl_Interp *, int , Tcl_Obj * CONST []);
int apsimRegisterGetSetProc(ClientData , Tcl_Interp *, int , Tcl_Obj * CONST []);
int apsimSendMessageProc(ClientData , Tcl_Interp *, int , Tcl_Obj * CONST []);
int apsimSendRawMessageProc(ClientData , Tcl_Interp *, int , Tcl_Obj * CONST []);
int apsimWriteToSummaryFileProc(ClientData , Tcl_Interp *, int , Tcl_Obj * CONST []);
int apsimRegisterEvent(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[]);
int apsimSubscribeNull(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[]);
int apsimSubscribeVariant(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[]);
//int apsimUnRegisterEvent(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[]);
//int apsimCatchMessages(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[]);
int apsimGetComponentXML(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[]);
int apsimGetChildren(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[]);
int apsimGetFQName(ClientData cd, Tcl_Interp *interp, int objc, Tcl_Obj * CONST objv[]);


#endif