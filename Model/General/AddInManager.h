#ifndef AddInManagerH
#define AddInManagerH
#include <vector>
#include <string>
#include <stdexcept>
#include <general\path.h>

//---------------------------------------------------------------------------
// This exception is thrown by the AddInManager in response to an invalid
// AddIn.
//---------------------------------------------------------------------------
class InvalidAddInError : public std::runtime_error
   {
   public:
      InvalidAddInError(const std::string& msg)
         : std::runtime_error(msg)
         {}
      InvalidAddInError(const std::string& file, const std::string& ePoint)
         : std::runtime_error("Invalid add-in DLL: " + file
                              + ". Cannot find entry point: " + ePoint)
         {}
   };

//---------------------------------------------------------------------------
// This class encapsulates the idea of an addin manager.  It keeps track
// of addin DLL's, their handles and the classes that the DLL's create.
//---------------------------------------------------------------------------
template <class T>
class AddInManager
   {
   public:
      //---------------------------------------------------------------------------
      // destructor
      //---------------------------------------------------------------------------
      ~AddInManager(void)
         {
         unload();
         }

      //---------------------------------------------------------------------------
      // load all addins.
      //---------------------------------------------------------------------------
      void load(const std::vector<std::string>& filenames) throw(InvalidAddInError)
         {
         // get the path of this executable.
         Path appPath(Application->ExeName.c_str());

         // Loop through all addin filenames, load the DLL, call the DLL to create an
         // instance of an AddIn and store in our list of addins.
         for (unsigned i = 0; i < filenames.size(); i++)
            {
            Path addinPath(appPath);
            addinPath.Append_path(filenames[i].c_str());

            HINSTANCE handle = LoadLibrary(addinPath.Get_path().c_str());

            // give an intelligent error msg if this fails...
            if (handle == NULL)
               {
               PVOID pMsgBuf;
               FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
                             NULL,
                             GetLastError(),
                             MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                             (PTSTR) &pMsgBuf,
                             0,
                             NULL);
               string msg = reinterpret_cast <PCHAR> (pMsgBuf);
               msg += "\nDll: " + addinPath.Get_path();
               LocalFree(pMsgBuf);
               throw InvalidAddInError(msg);
               }
            else
               {
               handles.push_back(handle);

               // look for createAddIn and a deleteAddIn entry points
               T*   __stdcall (*createAddInProc) (void);
               void __stdcall (*deleteAddInProc) (T*);

               (FARPROC) createAddInProc = GetProcAddress(handle, "createAddIn");
               (FARPROC) deleteAddInProc = GetProcAddress(handle, "deleteAddIn");
               if (createAddInProc == NULL)
                  throw InvalidAddInError(filenames[i], "createAddIn");
               if (deleteAddInProc == NULL)
                  throw InvalidAddInError(filenames[i], "deleteAddIn");

               addins.push_back( (*createAddInProc)());
               }
            }
         }

      //---------------------------------------------------------------------------
      // unload all addins.
      //---------------------------------------------------------------------------
      void unload(void) throw()
         {
         for (unsigned i = 0; i < handles.size(); i++)
            {
            void __stdcall (*deleteAddInProc) (T*);
            (FARPROC) deleteAddInProc = GetProcAddress(handles[i], "deleteAddIn");
            if (deleteAddInProc != NULL)
               (*deleteAddInProc)(addins[i]);
            }
         addins.erase(addins.begin(), addins.end());
         handles.erase(handles.begin(), handles.end());
         }

      //---------------------------------------------------------------------------
      // Return the number of add-ins loaded.
      //---------------------------------------------------------------------------
      unsigned getNumAddIns(void) const
         {return addins.size();}

      //---------------------------------------------------------------------------
      // Return an add-in to caller.
      //---------------------------------------------------------------------------
      T* getAddIn(unsigned index) const throw(std::range_error)
         {
         if (index > getNumAddIns())
            throw std::range_error("Invalid index into AddInManager::getAddIn method");
         return addins[index];
         }

      //---------------------------------------------------------------------------
      // Return a HINSTANCE for the specified add-in.
      //---------------------------------------------------------------------------
      HINSTANCE getAddInHandle(unsigned index) const throw(std::range_error)
         {
         if (index > getNumAddIns())
            throw std::range_error("Invalid index into AddInManager::getAddInHandle method");
         return handles[index];
         }

   private:
      typedef std::vector<T*> AddIns;
      typedef std::vector<HINSTANCE> Handles;
      AddIns addins;
      Handles handles;

   };

#endif 