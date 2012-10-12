#include <../General/pch.h>
#include "io_functions.h"

#ifdef __WIN32__
    #include <windows.h>
    #include <io.h>
    #include <direct.h>
#else
    #include <sys/io.h>
#endif

#include <stdlib.h>
#include <sys/stat.h>
#include <General/path.h>
#include <General/stristr.h>
#include <iostream>

#include "boost/filesystem/path.hpp"
#include "boost/filesystem/operations.hpp"
namespace fs = boost::filesystem;

using namespace std;

// Drop-in replacements for vcl routines.

// converts the relative file name into a fully qualified path name.
std::string ExpandFileName(const char *s){
   fs::path p(s, fs::native);
   fs::path q = system_complete(p);
   return(q.native_file_string());
}

bool FileExists (const std::string &f) {
   fs::path p(f, fs::native);
   return (fs::exists(p));
}

bool DirectoryExists (const std::string &d) {
   fs::path p(d, fs::native);
   return (fs::exists(p) && fs::is_directory(p));
}

//---------------------------------------------------------------------------
// Remove the path and extension from the specified file.
//---------------------------------------------------------------------------
void RemovePathAndExtension(std::string& fileName)
   {
   boost::filesystem::path p(fileName, boost::filesystem::native);
   std::string name = p.leaf();
   size_t posExtension = name.find('.');
   if (posExtension != std::string::npos)
      name.erase(posExtension);
   fileName = name;
   }
//---------------------------------------------------------------------------
// Return the temporary directory.
//---------------------------------------------------------------------------
std::string GetTempDir(void)
   {
   string st = getenv("TEMP");
   return st;
   }
// ------------------------------------------------------------------
// Return a list of files/directories to caller.
// ------------------------------------------------------------------
#ifdef __WIN32__
#ifndef _MSC_VER
// Only used by SEGReport/borland.
void getDirectoryListing(const std::string& directoryName,
                         const std::string& extension,
                         std::vector<std::string>& dirList,
                         unsigned int attribute,
                         bool fullPath)
   {
   Path p;
   struct ffblk ffblk;
   int done;
   p.Set_directory (directoryName.c_str());
   p.Set_name (extension.c_str());
   done = findfirst(p.Get_path().c_str(), &ffblk, attribute);
   while (!done)
      {
      bool NormalFile = ((ffblk.ff_attrib & FA_DIREC) == 0);
      bool Keep = (strcmp(ffblk.ff_name, ".")!=0 &&
                   strcmp(ffblk.ff_name, "..") != 0);

      if (attribute == 0)
         Keep = Keep && NormalFile;
      else
         Keep = Keep && ((ffblk.ff_attrib & attribute) > 0);

      if (Keep)
         {
         Path p;
         if (fullPath)
            p.Set_directory (directoryName.c_str());
         p.Set_name(ffblk.ff_name);
         dirList.push_back (p.Get_path());
         }
      done = findnext (&ffblk);
      }
   findclose(&ffblk);
   }
#endif
#endif
// ------------------------------------------------------------------
// Remove invalid file name characters from the specified string e.g. / \ | *
// ------------------------------------------------------------------
void removeInvalidFileNameChars(string& fileName)
   {
   replaceAll(fileName, "\\", "_");
   replaceAll(fileName, "/", "_");
   replaceAll(fileName, ":", "_");
   replaceAll(fileName, "'", "_");
   replaceAll(fileName, "\"", "_");
   replaceAll(fileName, "*", "_");
   replaceAll(fileName, "?", "_");
   replaceAll(fileName, "<", "_");
   replaceAll(fileName, ">", "_");
   replaceAll(fileName, "|", "_");
   }

