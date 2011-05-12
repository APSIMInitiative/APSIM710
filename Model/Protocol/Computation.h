//---------------------------------------------------------------------------
#ifndef computationH
#define computationH
#include <ComponentInterface/Interfaces.h>
#include <stdexcept>

#include <General/platform.h>

#ifdef MONO
#include <mono/jit/jit.h>
#include <mono/metadata/assembly.h>
#include <mono/metadata/debug-helpers.h>
#else
 #ifdef __WIN32__
#include <mscoree.h>
#include "mscorlib.h"
 #endif
#endif

enum CompilationMode {Invalid, Native, CLR, Mixed };
CompilationMode IsManaged(const char * filename);

namespace protocol {

typedef EXPORT void (STDCALL CallbackType)(const unsigned int *compInst, Message *message);
typedef void (STDCALL createInstanceProcType)(const char* ,
                                          const unsigned int* ,
                                          const unsigned int* ,
                                          const int* ,
                                          const int* ,
                                          CallbackType* );
typedef void (STDCALL deleteInstanceProcType)(const int*);
typedef void (STDCALL messageToLogicProcType)(const int* ,
                                          const Message* ,
                                          bool* );


// ------------------------------------------------------------------
//  Short description:
//    Encapsulates a component "computation".  A computation is a
//    DLL Handle + an instance number.  All calls to the DLL go through
//    this class.

//  Notes:

//  Changes:
//    dph 22/2/2000
//    dph 12/7/2001 modified to make protocol compliant 1.0

// ------------------------------------------------------------------
class EXPORT Computation : public IComputation
   {
   public:
      Computation(const std::string& name,
                  const std::string& fileName,
                  const std::string& componentInterfaceExecutable,
                  unsigned int componentId,
                  unsigned int parentId) throw (std::runtime_error);
      ~Computation(void);

      bool isOk(void) const  {return (createInstanceProc != NULL &&
                                      deleteInstanceProc != NULL &&
                                      messageToLogicProc != NULL);}
      virtual void messageToLogic(const Message* message) const
         {
			 if (isManaged) {
				 messageToManagedLogic((Message*)message);
			 }
			 else
			 {
               bool messageProcessed;
               (*messageToLogicProc) (&instanceNo, message, &messageProcessed);
			 }
         }
      unsigned getInstanceNo(void) const {return instanceNo;}
      std::string getExecutable(void) {return executableFileName;}

   private:
      int instanceNo;
      void* handle;
	  std::string executableFileName;

	  bool isManaged;

#ifdef MONO

	  static MonoDomain *domain;
	  MonoMethod* handleMsgMethod;
      MonoMethod* GetMonoMethod(MonoObject* classInstance, const char* methodName);
	  MonoObject* classInstance;

#else
 #ifdef __WIN32__
	  HMODULE mscoree;
	  static ICorRuntimeHost *pHost;
	  VARIANT vtobj;
	  void* typePtr;
	  mscorlib::_ObjectHandlePtr pObjectHandle; 
 #endif
#endif

	  void InitNETFrameworks();

	  void CreateManagedInstance(const std::string& filename,
		                         unsigned int componentId,
                                 unsigned int parentId);
	  void messageToManagedLogic(Message* message) const;

      void createInstance(const std::string& filename,
                          unsigned int componentId,
                          unsigned int parentId);
      virtual void deleteInstance(void) const;

      createInstanceProcType *createInstanceProc;
      deleteInstanceProcType *deleteInstanceProc;
      messageToLogicProcType *messageToLogicProc;
      
      bool loadComponent(const std::string& filename,
                         std::string componentInterfaceExecutable) throw (std::runtime_error);
      void unloadComponent(void);

   };

} // namespace protocol
#endif
