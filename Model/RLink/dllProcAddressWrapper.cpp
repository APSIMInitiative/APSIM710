#ifdef __WIN32__
#include <windows.h>

void * myDllProcAddress(void *h, const char *sym)
{
   return ((void *)GetProcAddress((HMODULE)h,sym));
}
#endif
