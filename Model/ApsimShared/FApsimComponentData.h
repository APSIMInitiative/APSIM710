#ifndef FApsimComponentData_H
#define FApsimComponentData_H

class FString;

#include <General/platform.h>

extern "C" ApsimComponentData* EXPORT STDCALL newApsimComponentData(const char* xml,
                                                    unsigned xmlLength);
extern "C" void EXPORT STDCALL deleteApsimComponentData(ApsimComponentData* componentData);

extern "C" bool EXPORT STDCALL ApsimComponentData_getProperty
   (ApsimComponentData* componentData,
    const FString& propertyType,
    const FString& name,
    FString& value);
    
#endif
