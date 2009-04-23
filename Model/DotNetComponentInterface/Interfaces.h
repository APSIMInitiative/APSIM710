#pragma once
#include <string>
#include <vector>
#include <vcclr.h>
using namespace System;

using namespace System;
using namespace System::ComponentModel;

namespace ModelFramework {
ref class ApsimComponent;

class UnmanagedData	
	{
	public:
	    virtual ApsimComponent^ GetComponentInstance() = 0;
		virtual void pack(char* message) = 0;
		virtual void unpack(char* message) = 0;
		virtual unsigned memorySize() = 0;
		virtual String^ DDML() = 0;
	};
};