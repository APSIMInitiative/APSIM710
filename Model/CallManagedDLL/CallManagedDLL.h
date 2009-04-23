// CallManagedDLL.h

#pragma once

using namespace System;
using namespace System::Collections::Generic;

public ref class Instances
	{
	public:
		static int CreateClass(String^ dllFileName, String^ className);
		static Object^ at(unsigned index) {return objects[index];}
		static void CallMethod(unsigned Index, const char* methodName, Object^ argument);
	
	private: 
		static List<Object^>^ objects = gcnew List<Object^>();
		
	};
	