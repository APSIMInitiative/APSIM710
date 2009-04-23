#include <string>
#include <stdexcept>

//---------------------------------------------------------------------------
#ifndef ApsimDirectoriesH
#define ApsimDirectoriesH

// ------------------------------------------------------------------
// This routine provides a way for APSIM applications to get the
// home directory.  Will throw a runtime error if the current
// Application is not in the apsim directory structure.
// ------------------------------------------------------------------
std::string getApsimDirectory(void);


#endif
