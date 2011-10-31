#include "StdPlant.h"
#include "Output.h"
using namespace std;

Output::Output(const string &Name, const string &Units, const string &Description)
   {
   this->Name = Name;
   this->Units = Units;
   this->Description = Description;
   }

