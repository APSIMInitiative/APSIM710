#ifdef __WIN32__
   #include <windows.h>
   #include <direct.h>
   #include <assert.h>
#else
   #include <dlfcn.h>
   #include <stdio.h>
#endif

#include <list>
#include <functional>
#include <General/platform.h>
#include <General/dll.h>
#include <General/path.h>
#include <ApsimShared/ApsimDirectories.h>
#include "Computation.h"
#include "Transport.h"

#ifdef MONO

#pragma comment(lib,"mono.lib")
MonoDomain * protocol::Computation::domain = NULL;

#else

 #ifdef __WIN32__
using namespace mscorlib;
ICorRuntimeHost* protocol::Computation::pHost = NULL;

 #endif
#endif

using namespace std;
using namespace protocol;

typedef EXPORT void ( STDCALL wrapperDll_t) (char* dllFileName);

// ------------------------------------------------------------------
//  Short description:
//     Callback routine that all components call when sending a message.

//  Notes:

//  Changes:
//    dph 10/5/2001

// ------------------------------------------------------------------
void EXPORT STDCALL messageCallback(const uintptr_t* dummy, Message* message)
   {
   Transport::getTransport().deliverMessage(message);
   }

CallbackType* callback = &messageCallback;

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
Computation::Computation(const string& name,
                         const string& fileName,
                         const string& componentInterfaceExecutable,
                         unsigned int componentId,
                         unsigned int parentId) throw (runtime_error)
   {
	   handle = NULL;
#ifndef MONO
 #ifdef __WIN32__
	   psa = NULL;
	   typePtr = NULL;
 #endif
#endif
	   // need to give the component to the transport layer.  Need a better
	   // way of doing this.
	   Transport::getTransport().addComponent(componentId, name, this);

	   if (loadComponent(fileName, componentInterfaceExecutable)) {
		 createInstance(fileName, componentId, parentId);
	   }
   }

// ------------------------------------------------------------------
//  Short description:
//    destructor

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
Computation::~Computation(void)
   {
   if (isOk())
      deleteInstance();
   unloadComponent();
#ifndef MONO
 #ifdef __WIN32__
   if (psa)
   {
		SafeArrayDestroy(psa);                                                                   // Destroy safearray
		VariantClear(&vtobj);
   }
#endif
#endif
   }

// ------------------------------------------------------------------
//  Short description:
//    call the CREATE entry point.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void Computation::createInstance(const std::string& filename,
                                 unsigned int componentId,
                                 unsigned int parentId)
   {
	if (isManaged)
	{
		CreateManagedInstance(filename, componentId, parentId);
	}
	else
	{
   static uintptr_t dummy = 0;
   (*createInstanceProc) (filename.c_str(),
                          &componentId,
                          &parentId,
                          &instanceNo,
                          &dummy,
                          callback);
	}
   }

// ------------------------------------------------------------------
//  Short description:
//    call the TERMINATE entry point

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void Computation::deleteInstance(void) const
   {
   try
      {
      (*deleteInstanceProc) (&instanceNo);
      }
   catch (...)
      {
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    Load the DLL and find pointers to all the entry points.
//    An exception is thrown if the dll cannot be loaded.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
bool Computation::loadComponent(const std::string& FileName,
                                std::string componentInterfaceExecutable) throw (std::runtime_error)
{
	string filename = FileName;
	if (!fileExists(filename))
	{
		// Win 7 64 bit computers can put dlls into a Program Files (x86) directory.
		// The AusFarm DLLs are located there so try changing the directory.
		To_lower(filename);
		replaceAll(filename, "program files", "Program Files (x86)"); 
	}

	executableFileName = filename;

	createInstanceProc = NULL;
	deleteInstanceProc = NULL;
	messageToLogicProc = NULL;
	isManaged = false;
	string componentInterface;

	if (componentInterfaceExecutable != "")
	{
		componentInterface = componentInterfaceExecutable;
		handle = loadDLL(componentInterface.c_str());
	}
	else
	{
	    if (!fileExists(filename))
			throw runtime_error(string("File \"" + filename + "\" does not exist"));
			
		CompilationMode dllMode = IsManaged(filename.c_str());

		if (dllMode == Invalid)
		{
			string msg = "File " + filename + " is not a valid component file!";
			throw runtime_error(msg);
		}

		else if (dllMode == CLR)
		{
			isManaged = true;
		    InitNETFrameworks();
		}

		else // "Native" component
		{
			handle = loadDLL(executableFileName);

			wrapperDll_t *wrapperDll;
			wrapperDll = (wrapperDll_t *) dllProcAddress(handle, "wrapperDLL");
			if (wrapperDll == NULL)
				throw std::runtime_error("Cannot find entry point 'wrapperDLL' in dll: " + executableFileName);

			// Go get the wrapperDll filename.
			char wrapperFileName[1024];
			(*wrapperDll)(&wrapperFileName[0]);
			componentInterface = wrapperFileName;

			if (componentInterface != "")
			{
				// This is a wrapped dll - it has no "entry points". Load the wrapper instead.
				closeDLL(handle);

#ifdef __WIN32__
				if (Str_i_Eq(fileTail(componentInterface), "piwrapper.dll"))
				{
					// AUSFarm dlls are treated differently - need to be loaded with the ausfarm 
					// infrastructure in the same working dir
					char oldwd[MAX_PATH];
					getcwd(oldwd, MAX_PATH);
					chdir(fileDirName(executableFileName).c_str());
					handle = LoadLibrary(componentInterface.c_str());
					if (handle == NULL)
				 {
					 // Get windows error message.
					 LPVOID lpMsgBuf;
					 FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
							NULL,
							GetLastError(),
							MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
							(LPTSTR) &lpMsgBuf,
							0,
							NULL
							);
					 string errorMessage = ("Cannot load DLL: " + componentInterface + ".\n  " + (LPTSTR) lpMsgBuf);
					 LocalFree( lpMsgBuf );
					 throw runtime_error(errorMessage);
				 }
					chdir(oldwd);
				}
				else 
				{
                    //check if the componentInterface already has a full path name. Only append the dll path if it is a short name or partially qualified name.
                    if (!((componentInterface.c_str()[0] == '\\') || (componentInterface.c_str()[0] == '/') || strchr(componentInterface.c_str(), ':')))
					    componentInterface = fileDirName(executableFileName) + "/" + componentInterface;
					handle = loadDLL(componentInterface.c_str());
				}
#else
				if (componentInterface[0] != '/')  // Adjust the path name if it's not fully qualified
					componentInterface = getExecutableDirectory() + "/" + componentInterface;
				handle = loadDLL(componentInterface.c_str());
#endif
			}
			else
			{
				// This is not a wrapped dll - it will provide entrypoints itself
			}
		}
	}

	if (!isManaged)
	{
#ifdef __WIN32__
#ifdef _MSC_VER
		createInstanceProc = (createInstanceProcType *) GetProcAddress((HMODULE)handle, "createInstance");
		deleteInstanceProc = (deleteInstanceProcType *) GetProcAddress((HMODULE)handle, "deleteInstance");
		messageToLogicProc = (messageToLogicProcType *) GetProcAddress((HMODULE)handle, "messageToLogic");
#else
		(FARPROC) createInstanceProc = GetProcAddress(handle, "createInstance");
		(FARPROC) deleteInstanceProc = GetProcAddress(handle, "deleteInstance");
		(FARPROC) messageToLogicProc = GetProcAddress(handle, "messageToLogic");
#endif
#else
		createInstanceProc = (createInstanceProcType *)dlsym(handle, "createInstance");
		deleteInstanceProc = (deleteInstanceProcType *)dlsym(handle, "deleteInstance");
		messageToLogicProc = (messageToLogicProcType *)dlsym(handle, "messageToLogic");
#endif

		if (createInstanceProc == NULL ||
			deleteInstanceProc == NULL ||
			messageToLogicProc == NULL)
		{
			string msg = "Not a valid APSIM DLL.  Missing 1 or more entry points.  DLL="
				+ filename + ", wrapper=" + componentInterface;
			throw runtime_error(msg);
		}
	}
#ifndef MONO
 #ifdef __WIN32__
	if (typePtr)
	{
		((_Type*)typePtr)->Release();
		typePtr = NULL;
	}

#endif
#endif

	return true;
}

// ------------------------------------------------------------------
//  Short description:
//    Unload the specified dll.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void Computation::unloadComponent(void)
   {
   if (handle)
	 {
	   closeDLL(handle);
	   handle = NULL;
	 }
   }

void Computation::InitNETFrameworks()
{
#ifdef MONO
	if (!domain)
	{
       const char* domain_name = "mono_apsim";
       // domain = mono_jit_init (domain_name);
       domain = mono_jit_init_version (domain_name, "v4.0.30319");
#if ! (MONO_MAJOR == 2 || (MONO_MAJOR == 3 & MONO_MINOR <= 8))
       mono_domain_set_config(domain, getApsimDirectory().c_str(), "mono_apsim.config");
#endif
	}
	return;
#else
 #ifdef __WIN32__
	if (pHost)
		return;

	// We could like statically to the mscoree dll, but there is a (slim) chance that someone
	// may want to run Apsim on a system where .NET frameworks have never been installed. 
	// With dynamic loading, that isn't a problem (unless they try using .NET components!)
	//
	// Note that we never call FreeLibrary. Once you've loaded the frameworks, you've got them
	// until your process exits...
	//
    mscoree = LoadLibrary("mscoree.dll");
	if (mscoree == NULL)
  	   throw runtime_error(".NET Frameworks are NOT installed!\n");

	//
	// Query 'HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\NET Framework Setup\NDP\v3.5\Install' DWORD value
	// See http://support.microsoft.com/kb/318785/ for more information on .NET runtime versioning information
	//
	HKEY key = NULL;
	DWORD lastError = 0;
//	lastError = RegOpenKeyEx(HKEY_LOCAL_MACHINE,TEXT("SOFTWARE\\Microsoft\\NET Framework Setup\\NDP\\v3.5"),0,KEY_QUERY_VALUE,&key);
//    if(lastError!=ERROR_SUCCESS) {
//		printf(TEXT("Error opening HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\NET Framework Setup\\NDP\\v3.5\n"));
//		return;
//	}
	lastError = RegOpenKeyEx(HKEY_LOCAL_MACHINE,TEXT("SOFTWARE\\Microsoft\\NET Framework Setup\\NDP\\v4\\Client"),0,KEY_QUERY_VALUE,&key);
	if(lastError!=ERROR_SUCCESS) {
		printf(TEXT("Error opening HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\NET Framework Setup\\NDP\\v4\\Client\n"));
		return;
	}

	DWORD type;
	BYTE data[4];
	DWORD len = sizeof(data);
	lastError = RegQueryValueEx(key,TEXT("Install"),NULL,&type,data,&len);

	if(lastError!=ERROR_SUCCESS) {
		RegCloseKey(key);
//		printf(TEXT("Error querying HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\NET Framework Setup\\NDP\\v3.5\\Install\n"));
		printf(TEXT("Error querying HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\NET Framework Setup\\NDP\\v4\\Client\\Install\n"));
		return;
	}

	RegCloseKey(key);

	// Was Install DWORD key value == 1 ??
	if(data[0]!=1)
	{
//		printf(TEXT(".NET Framework 3.5 is NOT installed\n"));
		printf(TEXT(".NET Framework 4 is NOT installed\n"));
		return;
	}

	// 
	// Load the runtime the 3.5 Runtime (CLR version 2.0)
	//
//	LPWSTR pszVer = L"v2.0.50727";  // .NET Fx 3.5 needs CLR 2.0
	LPWSTR pszVer = L"v4.0.30319";  // .NET Fx 4 needs CLR 4.0
	LPWSTR pszFlavor = L"wks";
	pHost = NULL;

    typedef HRESULT (STDCALL *CorBindToRuntimeExType)(LPCWSTR, LPCWSTR, DWORD, REFCLSID, REFIID, LPVOID FAR *);
    CorBindToRuntimeExType bindFunc;
	bindFunc = (CorBindToRuntimeExType)GetProcAddress(mscoree, "CorBindToRuntimeEx");
	if (bindFunc == NULL)
  	   throw runtime_error("Could not locate CorBindToRuntimeEx\n");

    HRESULT hr = bindFunc( pszVer,       
		pszFlavor,    
		STARTUP_LOADER_OPTIMIZATION_SINGLE_DOMAIN | STARTUP_CONCURRENT_GC, 
		CLSID_CorRuntimeHost, 
		IID_ICorRuntimeHost,
		(void **)&pHost);

	if (!SUCCEEDED(hr)) {
		printf(TEXT("CorBindToRuntimeEx failed 0x%x\n"),hr);
		return;
	}

	// _putts(TEXT("Loaded version 2.0.50727 of the CLR\n"));

	pHost->Start(); // Start the CLR

	return;
 #else
   throw runtime_error("Support for .NET components has not been compiled into this build of APSIM.");
 #endif
#endif
}

void Computation::CreateManagedInstance(const std::string& filename,
										unsigned int componentId,
										unsigned int parentId)

{
#ifdef MONO
	try 
	{
		MonoImageOpenStatus status;

		//MonoAssembly* assembly = mono_assembly_open (filename.c_str(), &status);
		MonoAssembly* assembly = mono_domain_assembly_open (domain, filename.c_str());
		if (!assembly)
			printf("Error\n");
		else
		{
			MonoImage *image = mono_assembly_get_image(assembly);   //component derived class in this assembly
			MonoClass *klass = mono_class_from_name (image, "CMPComp", "TGCComponent");

			/* allocate memory for the object */
			classInstance = mono_object_new (domain, klass);

			// "Pin" the class instance to keep the garbage collector from discarding it.
			uint64_t handle = mono_gchandle_new (classInstance, true);
			classInstance = mono_gchandle_get_target (handle);

			MonoMethod* ctor_method = GetMonoMethod(classInstance, "CMPComp.TGCComponent:.ctor");
	                handleMsgMethod = GetMonoMethod(classInstance, "CMPComp.TGCComponent:handleMessage");

			/* execute the component constructor that takes three arguments */
			unsigned compId = componentId;
			unsigned parent = parentId;
			void *args [3];
			args [0] = &compId;
			args [1] = &parent;
			void* fPtr = (void*)callback;
			args [2] = &fPtr;
			/* constructor methods return void, so we ignore the return value,
			* the constructed object is my_class_instance. */
			mono_runtime_invoke (ctor_method, classInstance, args, NULL);
		}
	}
	catch(exception& error) {
		throw runtime_error(error.what());
	}

#else
 #ifdef __WIN32__
	try {
		//
		// Get a pointer to the default domain in the CLR
		//
		_AppDomainPtr pDefaultDomain = NULL;
		IUnknownPtr   pAppDomainPunk = NULL;

		HRESULT hr = pHost->GetDefaultDomain(&pAppDomainPunk);
		assert(pAppDomainPunk); 

		hr = pAppDomainPunk->QueryInterface(__uuidof(_AppDomain),(void**) &pDefaultDomain);
		assert(pDefaultDomain);

		SAFEARRAY * l_pArray;
		l_pArray = SafeArrayCreateVectorEx(VT_VARIANT, 0, 3, NULL);
		long l_Index;
		VARIANTARG vals[3];
		vals[0].vt = VT_UINT;
		vals[0].ulVal = componentId;
		vals[1].vt = VT_UINT;
		vals[1].ulVal = parentId;
		vals[2].vt = VT_UINT;
		vals[2].byref = callback;

		l_Index = 0;
		SafeArrayPutElement (l_pArray, &l_Index, &vals[0]);
		l_Index = 1;
		SafeArrayPutElement (l_pArray, &l_Index, &vals[1]);
		l_Index = 2;
		SafeArrayPutElement (l_pArray, &l_Index, &vals[2]);

		std::wstring wide_filename(filename.length(), L' ');
		std::copy(filename.begin(), filename.end(), wide_filename.begin());

		pObjectHandle = pDefaultDomain->CreateInstanceFrom_3(
			wide_filename.c_str(),
			L"CMPComp.TGCComponent",
			false,
			BindingFlags_OptionalParamBinding,
			NULL,
			l_pArray,
			NULL,
			NULL,
			NULL);

		SafeArrayDestroy(l_pArray);
		psa = SafeArrayCreateVector(VT_VARIANT,0,1);                                // Create a safearray (length 1) for later re-use

		vtobj = pObjectHandle->Unwrap();                                            // Get an _Object (as variant) from the _ObjectHandle
   	    _ObjectPtr pObject; 
		vtobj.pdispVal->QueryInterface(__uuidof(_Object),(void**)&pObject);     // QI the variant for the Object iface for our component class
    	(void*)typePtr = (void*)pObject->GetType();
	}
	catch(_com_error& error) {
		throw runtime_error(std::string(error.Description()).c_str());
	}
 #endif
#endif
}

void Computation::messageToManagedLogic(Message* message) const
{
#ifdef MONO
    void *_args [1];
    _args[0] = &message;
    mono_runtime_invoke(handleMsgMethod, classInstance, _args, NULL);
#else
 #ifdef __WIN32__
	try {
		if (!typePtr)
		{
			_ObjectPtr pObject; 
			vtobj.pdispVal->QueryInterface(__uuidof(_Object),(void**)&pObject);     // QI the variant for the Object iface for our component class
			//_TypePtr pType = pObject->GetType();                                  // Get the _Type iface
			(void*)typePtr = (void*)pObject->GetType();
		}

		VARIANTARG val;
		long l_Index;
		val.vt = VT_UINT;
		val.byref = (void*)message;
		l_Index = 0;
		SafeArrayPutElement (psa, &l_Index, &val);

		/*pType*/ ((_Type*)typePtr)->InvokeMember_3("handleMessage",                // Invoke "handleMessage" method on pType
			BindingFlags_InvokeMethod,
			NULL,
			vtobj,
			psa );

	}
	catch(_com_error& error) {
		throw runtime_error(std::string(error.Description()).c_str());
	}
 #endif
#endif
}

#ifdef MONO
MonoMethod* Computation::GetMonoMethod(MonoObject* classInstance, const char* methodName)
{
	if (classInstance == NULL)
		return NULL;

	MonoClass* clas = mono_object_get_class(classInstance);
	if (clas == NULL)
		return NULL;

	MonoImage* image = mono_class_get_image(clas);
	if (image == NULL)
		return NULL;

	// Extract the namespace and component portions of the methodName string
	// A namespace and a component name must be provided as part of the string
	int nNSChars;
	int nCompChars;
	const char* namespaceEnd = strchr(methodName, '.');
	const char* componentEnd = strrchr(methodName, ':');

	if (namespaceEnd == NULL || componentEnd == NULL)
		return NULL;

	nNSChars = namespaceEnd - methodName;
	nCompChars = componentEnd - namespaceEnd - 1;

	char* namespaceStr = new char[nNSChars + 1];
	char* componentStr = new char[nCompChars + 1];
	strncpy(namespaceStr, methodName, nNSChars);
	namespaceStr[nNSChars] = '\0';
	strncpy(componentStr, methodName + nNSChars + 1, nCompChars);
	componentStr[nCompChars] = '\0';

	MonoMethodDesc* desc = mono_method_desc_new (methodName, false);

	MonoClass *klass = mono_class_from_name (image, namespaceStr, componentStr);
	if (klass == NULL)
		return NULL;

	// we may be looking for a virtual method; if so, we must first find the ancestor
	// which introducted it.
	MonoMethod *method = NULL;
	MonoClass *aKlass = klass;

	while (aKlass && !method )
	{
		method = mono_method_desc_search_in_class (desc, aKlass);
		if (!method)
			aKlass = mono_class_get_parent(aKlass);
	}

	mono_method_desc_free (desc);

	// If the method is virtual, get the derived version from the current class instance
	if (aKlass != klass && classInstance != NULL)
		method = mono_object_get_virtual_method (classInstance, method);

	return method;
}
#endif

CompilationMode IsManaged(const char * filename) {
	try {
		char data[4096];

		FILE* file = fopen(filename, "rb");
		if (!file)
			return Invalid;
		unsigned nRead = fread(data, 1, 4096, file);
		fclose(file);
		if (nRead != 4096)
			return Invalid;

#ifndef _WIN32
		// If we are running on Linux, the executable/so will start with the string 0x7f + 'ELF'
		// If the 5 byte is 1, it's a 32-bit image (2 indicates 64-bit)
		// If the 16th byte is 3, it's a shared object; 2 means it's an executable
		// If it's a Mono/.Net assembly, we continue to use the the "Windows" header

		// For now, if we're on Linux just see if it has an "ELF" header
		if (data[0] == 0x7f && data[1] == 'E' && data[2] == 'L' && data[3] == 'F')
			return Native;
#endif
		// Verify this is a executable/dll
		if (*(unsigned short*)&data[0] != 0x5a4d)
			return Invalid;

		// This will get the address for the WinNT header
		unsigned iWinNTHdr = *(unsigned*)&data[60];

		// Verify this is an NT address
		// At the file offset specified at offset 0x3c, is a 4-byte
		// signature that identifies the file as a PE format image file. This signature is “PE\0\0”
		if (*(unsigned*)&data[iWinNTHdr] != 0x00004550)
			return Invalid;

		unsigned iLightningAddr = iWinNTHdr + 24 + 208;
		unsigned iSum = 0;
		unsigned iTop = iLightningAddr + 8;

		for (unsigned i = iLightningAddr; i < iTop; ++i)
			iSum |= data[i];

		if (iSum == 0)
			return Native;
		else // This uses the CLR, but is it native or mixed? Look for native-style exports
		{
			unsigned int optionalHdrBase = iWinNTHdr + 24;
			// unsigned short magic = *(unsigned*)&data[optionalHdrBase];
			// bool is64bit = (magic & 0x200) != 0;
			// unsigned int exportTableAddr = *(unsigned*)&data[optionalHdrBase + 96];
			unsigned int exportTableSize = *(unsigned*)&data[optionalHdrBase + 100];
			if (exportTableSize > 0)
				return Mixed;
			else
				return CLR;
		}
	}
	catch(std::exception* e) {
		throw(e);
	}
}
