#include "OutputVariable.h"
using namespace std;

OutputVariable::OutputVariable(const string &Name, const string &Units, const string &Description, float &Variable)
   : Output (Name,Units,Description)
   {
   this->Variable = (void*)&Variable;
   }

