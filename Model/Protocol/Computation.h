//---------------------------------------------------------------------------
#ifndef computationH
#define computationH
#include <ComponentInterface/Interfaces.h>
#include <stdexcept>

#include <General/platform.h>

namespace protocol {
typedef EXPORT STDCALL void (CallbackType)(const unsigned int *compInst, Message *message);
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
         bool messageProcessed;
         (*messageToLogicProc) (&instanceNo, message, &messageProcessed);
         }
      unsigned getInstanceNo(void) const {return instanceNo;}
      std::string getExecutable(void) {return executableFileName;}

   private:
      int instanceNo;
      void* handle;
      std::string executableFileName;

      void createInstance(const std::string& filename,
                          unsigned int componentId,
                          unsigned int parentId);
      virtual void deleteInstance(void) const;

      void EXPORT STDCALL (*createInstanceProc)(const char* dllFileName,
                                          const unsigned int* componentID,
                                          const unsigned int* parentID,
                                          const int* anInstanceNo,
                                          const int* callbackArg,
                                          CallbackType* callback);

      void EXPORT STDCALL (*deleteInstanceProc)(const int* anInstanceNo);
      void EXPORT STDCALL (*messageToLogicProc)(const int* anInstanceNo,
                                          const Message* messageHeader,
                                          bool* bProcessed);
      bool loadComponent(const std::string& filename,
                         std::string componentInterfaceExecutable) throw (std::runtime_error);
      void unloadComponent(void);

   };

} // namespace protocol
#endif
