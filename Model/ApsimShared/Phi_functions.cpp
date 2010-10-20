#include "Phi_functions.h"

#ifdef __WIN32__
   #include <direct.h>
#else
   #include <unistd.h>
#endif
using namespace std;

std::string ExtractFileExt(const char* a) {
  string returnString;

  string file = a;

  unsigned pos = file.find(".");
  if (pos != string::npos) {
    returnString = file.substr(pos); // includes the dot in the extension
  }
  return returnString;
}

std::string GetCurrentDirectory() {
  char buf[300];
  return getcwd(buf, 300);
}

void ShowMessage(std::string a) {
  cout << a;
  cout << "\n";
  cerr << a;
  cerr << "\n";
}
