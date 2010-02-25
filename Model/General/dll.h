//---------------------------------------------------------------------------
#ifndef dllH
#define dllH

#include <string>
#include <stdexcept>
#include <General/platform.h>

// ------------------------------------------------------------------
// Loads a dll into memory and returns a handle to it.
// ------------------------------------------------------------------
void  EXPORT * loadDLL(const std::string& filename);

// ------------------------------------------------------------------
// Close the specified dll handle.
// ------------------------------------------------------------------
void EXPORT closeDLL(void* handle);

// ------------------------------------------------------------------
// Return the address of a function in the dll specified by dllHandle.
// ------------------------------------------------------------------
void EXPORT * dllProcAddress(void* dllHandle, const char* name);



#endif
