#include "stdafx.h"
#include "CallManagedDLL.h"


void CreateArray(double values[], int maxvalues, array<double>^ arr)
	{
	arr = gcnew array<double>(maxvalues);
	for (int i = 0; i != maxvalues; i++)
		arr[i] = values[i];
	}

void CopyArray(double values[], int maxvalues, array<double>^ arr)
	{
	for (int i = 0; i != arr->Length; i++)
		values[i] = arr[i];
	}

	
struct TimeType
   {
   int startday;
   int startsec;
   double startsecpart;
   int endday;
   int endsec;
   double endsecpart;
   double values[20];
   int numvalues;
   };

public ref class DotNetTimeType
   {
   public:
   int startday;
   int startsec;
   double startsecpart;
   int endday;
   int endsec;
   double endsecpart;
   array<double>^ values;
   int numvalues;
   };

extern "C" __declspec(dllexport) void __stdcall  CallMethod(int* ClassHandle, const char* MethodName, 
															TimeType* Time)
	{
	DotNetTimeType^ t = gcnew DotNetTimeType;
	t->startday = Time->startday;
	t->startsec = Time->startsec;
	t->startsecpart = Time->startsecpart;
	t->endday = Time->endday;
	t->endsec = Time->endsec;
	t->endsecpart = Time->endsecpart;
	CreateArray(Time->values, 20, t->values);
	t->numvalues = Time->numvalues;
		 
	Instances::CallMethod(*ClassHandle, MethodName, t);
	
	Time->startday = t->startday;
	Time->startsec = t->startsec;
	Time->startsecpart = t->startsecpart;
	Time->endday = t->endday;
	Time->endsec = t->endsec;
	Time->endsecpart = t->endsecpart;
	CopyArray(Time->values, 20, t->values);
	Time->numvalues = t->numvalues;
	}
	