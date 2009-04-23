//---------------------------------------------------------------------------


#pragma hdrstop

#include "CropLibrary.h"

//---------------------------------------------------------------------------

#pragma package(smart_init)


extern "C" void _stdcall _export warningError (const int *user_or_internal,
                char *error_string)
{

}

//---------------------------------------------------------------------------

extern "C" void _stdcall _export fatal_error (const int *user_or_internal,
                char *error_string)
{

}
