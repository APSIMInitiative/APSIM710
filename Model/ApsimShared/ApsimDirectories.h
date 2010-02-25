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
std::string EXPORT getApsimDirectory(void);

// ------------------------------------------------------------------
// This routine returns the directory where the
// current executable is located.
// ------------------------------------------------------------------
std::string EXPORT getExecutableDirectory(void);

std::string EXPORT getExecutableFileName(void);

#endif
