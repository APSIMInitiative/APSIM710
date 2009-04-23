//---------------------------------------------------------------------------
#pragma hdrstop
#include "ClsFact.h"
#include <shlobj.h>
#include <olectl.h>
#include <string>
#include <exception>
#include <general\path.h>
#include <general\inifile.h>
#include <general\stream_functions.h>
#include <ApsimShared\ApsimVersion.h>
#include <forms.hpp>

HINSTANCE hInst;
extern ULONG g_DllRefCount;
using namespace std;

//---------------------------------------------------------------------------
// generated with CoCreateGuid()
//---------------------------------------------------------------------------
const char *szCLSID = "{5637FAA5-A5A4-42DD-A9F2-AE08F204B5";

__declspec(dllexport) HRESULT STDMETHODCALLTYPE DllCanUnloadNow(void)
   {
   return (g_DllRefCount > 0 ? S_FALSE : S_OK);
   }

//---------------------------------------------------------------------------
// work out a registry key for this DLL.
//---------------------------------------------------------------------------
string getKey(void)
   {
   return "Apsim" + getApsimVersion();
   }
string getClsid(void)
   {
   string version = getApsimVersion();
   replaceAll(version, ".", "");
   return szCLSID + version + "}";
   }
//---------------------------------------------------------------------------
// create a CClassFactory object
//---------------------------------------------------------------------------
__declspec(dllexport) HRESULT STDMETHODCALLTYPE DllGetClassObject
   (REFCLSID rclsid, REFIID riid, LPVOID *ppReturn)
   {
   *ppReturn = NULL;

   // create a CClassFactory object and check it for validity

   CClassFactory *pClassFactory = new CClassFactory(rclsid);
   if(pClassFactory == NULL)
      return E_OUTOFMEMORY;

   // get the QueryInterface return for our return value
   HRESULT hResult = pClassFactory->QueryInterface(riid, ppReturn);

   // call Release to decement the ref count - creating the object set it to one
   // and QueryInterface incremented it - since its being used externally (not by
   // us), we only want the ref count to be 1
   pClassFactory->Release();

   // return the result from QueryInterface
   return hResult;
   }
//---------------------------------------------------------------------------
// Register this DLL by setting up our registry entries to point to
// our shell extension class.
//---------------------------------------------------------------------------
__declspec(dllexport) HRESULT STDMETHODCALLTYPE DllRegisterServer(void)
   {
   string key = getKey();
   HKEY hKey;
   DWORD unused;
   HRESULT hRes = SELFREG_E_CLASS;

   if(RegCreateKeyEx(HKEY_CLASSES_ROOT,
                     "CLSID",
                     0,
                     NULL,
                     REG_OPTION_NON_VOLATILE,
                     KEY_WRITE,
                     NULL,
                     &hKey,
                     &unused) == ERROR_SUCCESS)
      {
      if(RegCreateKey(hKey, getClsid().c_str(), &hKey) == ERROR_SUCCESS)
         {
         RegSetValueEx(hKey,
                         NULL,
                         0,
                         REG_SZ,
                         (BYTE*)key.c_str(),
                         15);

         if(RegCreateKey(hKey, "InProcServer32", &hKey) == ERROR_SUCCESS)
            {
            char lpFilename[MAX_PATH] = {0};
            GetModuleFileName(hInst, lpFilename, MAX_PATH);
            RegSetValueEx(hKey,
                          NULL,
                          0,
                          REG_SZ,
                          (BYTE*)lpFilename,
                          strlen(lpFilename)+1);

            RegSetValueEx(hKey,
                          "ThreadingModel",
                          0,
                          REG_SZ,
                          (BYTE*)"Apartment",
                          10);
            }
         }
      RegCloseKey(hKey);

      string fullKey = "*\\shellex\\ContextMenuHandlers\\" + key;
      if(RegCreateKeyEx(HKEY_CLASSES_ROOT,
                       fullKey.c_str(),
                       0,
                       NULL,
                       REG_OPTION_NON_VOLATILE,
                       KEY_WRITE,
                       NULL,
                       &hKey,
                       &unused) == ERROR_SUCCESS)
         {
         RegSetValueEx(hKey,
                       NULL,
                       0,
                       REG_SZ,
                       (BYTE*)getClsid().c_str(),
                       strlen(getClsid().c_str()) + 1);

         RegCloseKey(hKey);
         hRes = S_OK;
         }
      }

   SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, 0, 0);
   return hRes;
   }
//---------------------------------------------------------------------------
// UnRegister this DLL by cleaning up our registry entries
//---------------------------------------------------------------------------
__declspec(dllexport) HRESULT STDMETHODCALLTYPE DllUnregisterServer(void)
   {
   string key = getKey();

   HKEY hKey;
   DWORD keys = 0, size = 0;
   LONG res = ERROR_SUCCESS;
   char *szCLSIDkey = new char[7+strlen(getClsid().c_str())];
   wsprintf(szCLSIDkey, "CLSID\\%s", getClsid().c_str());

   if(RegOpenKeyEx(HKEY_CLASSES_ROOT,
                 "*\\shellex\\ContextMenuHandlers",
                 0,
                 KEY_ALL_ACCESS,
                 &hKey) == ERROR_SUCCESS)
      {
      res = RegQueryInfoKey(hKey,
                      NULL,
                      NULL,
                      NULL,
                      &keys,
                      &size,
                      NULL,
                      NULL,
                      NULL,
                      NULL,
                      NULL,
                      NULL);

      char *buf = new char[size+1];
      for(DWORD x = 0; x < keys; x++)
         {
         RegEnumKey(hKey,
                    x,
                    buf,
                    size+1);
         if(strcmpi(buf, key.c_str()) == 0)
            {
            res = RegDeleteKey(hKey, key.c_str());
            break;
            }
         }
      delete[] buf;
      RegCloseKey(hKey);

      if(RegOpenKeyEx(HKEY_CLASSES_ROOT,
                      szCLSIDkey,
                      0,
                      KEY_WRITE,
                      &hKey) == ERROR_SUCCESS)
         {
         res = RegDeleteKey(hKey, "InProcServer32");
         RegCloseKey(hKey);

         if(RegOpenKeyEx(HKEY_CLASSES_ROOT,
                         "CLSID",
                         0,
                         KEY_WRITE,
                         &hKey) == ERROR_SUCCESS)

            {
            res = RegDeleteKey(hKey, getClsid().c_str());
            RegCloseKey(hKey);
            }
         }
      }
   delete[] szCLSIDkey;

   SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, 0, 0);

   if(res == ERROR_SUCCESS)
      return S_OK;
   else
      return SELFREG_E_CLASS;
   }
//---------------------------------------------------------------------------
// Main entry point into the DLL - called by Windows.
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
   {
   Application->Initialize();
   switch(reason)
      {
      case DLL_PROCESS_ATTACH: hInst = hinst;
                               break;
      case DLL_PROCESS_DETACH: break;
      }

   const unsigned _MCW_EW = 0x037f;
   const unsigned _EM_INVALID = 0xffff;
   _controlfp(_MCW_EW, _EM_INVALID);

   return TRUE;
   }

