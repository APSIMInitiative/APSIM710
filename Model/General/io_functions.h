//---------------------------------------------------------------------------
#ifndef io_functionsH
#define io_functionsH

#include <string>
#include <vector>
#include <General/platform.h>

std::string EXPORT ExpandFileName(const char *);
bool EXPORT FileExists (const std::string &);
bool EXPORT DirectoryExists (const std::string &d);

//---------------------------------------------------------------------------
// Remove the path and extension from the specified file.
//---------------------------------------------------------------------------
void EXPORT RemovePathAndExtension(std::string& fileName);

//---------------------------------------------------------------------------
// Return the temporary directory.
//---------------------------------------------------------------------------
std::string EXPORT GetTempDir(void);

// ------------------------------------------------------------------
// Return a list of files/directories to caller.
// ------------------------------------------------------------------
void EXPORT getDirectoryListing(const std::string& directoryName,
                         const std::string& extension,
                         std::vector<std::string>& dirList,
                         unsigned int attribute,
                         bool fullPath = false);

// ------------------------------------------------------------------
// Remove invalid file name characters from the specified string e.g. / \ | *
// ------------------------------------------------------------------
void EXPORT removeInvalidFileNameChars(std::string& fileName);

#endif
