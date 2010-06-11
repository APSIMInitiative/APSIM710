using namespace System;
using namespace System::Collections::Generic;
using namespace System::Text;

#include "Instance.h"

Instance::Instance()
   {
   Children = gcnew NamedList<NamedItem^>();
   Parent = nullptr;
   }

