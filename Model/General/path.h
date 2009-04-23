#ifndef PATH_H
#define PATH_H

#include <General/string_functions.h>
#include <General/platform.h>

// ------------------------------------------------------------------
//  Short description:
//    Class handling the manipulation of file paths.

//  Notes:
//		Terms :
//		        Drive     -> d:
//			Directory -> d:\bin
//			Name      -> test.con
//			Extension -> .con
//			Path      -> d:\bin\test.con

//  Changes:
//    DPH 17/11/94
//    DPH 13/5/1997 - reworked to use standard template library.

// ------------------------------------------------------------------
class EXPORT Path
{
   private:
      std::string Drive;
      std::string Directory;
      std::string Name;

   public:
      Path(void) {};
      Path(const std::string& File_path) {Set_path(File_path);};
      static char dirDelim(); //either '\' or '/' based on OS
      bool operator== (const Path& From) const;
      int operator< (const Path& From) const {return From.Get_full_path() > this->Get_full_path();};

      std::string Get_drive(void);
      std::string Get_directory(void);
      std::string Get_name(void);
      std::string Get_name_without_ext(void);
      std::string Get_extension(void);
      std::string Get_path(void);
      std::string Get_full_path (void) const;              // always returns a full absolute path.

      void Set_drive(const char* New_drive);
      void Set_directory(const char* New_directory);
      void Set_name(const char* New_name);
      void Set_extension(const char* New_extension);
      void Set_path(const std::string& New_path);

      bool Is_empty(void) const;
      bool Exists(void);
      void Change_directory(void);

      void Append_path (const char* Path); // append to current_working_dir (JW)
      std::string Back_up_directory (void);

      static Path getCurrentFolder(void);
      static Path getTempFolder(void);
};

// ------------------------------------------------------------------
//  Short description:
//     generic "for_each" function for removing directories from a list
//     paths and storing just the names in a given stl container.

//  Notes:

//  Changes:
//    DPH 28/10/97

// ------------------------------------------------------------------
template < class Container >
class remove_directory_and_copy : private std::unary_function <std::string, void >
   {
   private:
      Container& container;
   public:
      remove_directory_and_copy( Container& c )
         : container(c) { }
      void operator() (const std::string& x)
         {
         Path p(x.c_str());
         container.push_back (p.Get_name());
         }
    };

// ------------------------------------------------------------------
//  Short description:
//     generic "for_each" function for prepending a path to every item in a list
//     and storing in a given stl container.

//  Notes:

//  Changes:
//    DPH 28/10/97

// ------------------------------------------------------------------
template < class Container >
class prepend_directory_and_copy : private std::unary_function <std::string, void >
   {
   private:
      Container& container;
      std::string Directory;
   public:
      prepend_directory_and_copy(const char* directory, Container& c )
         : Directory(directory), container(c) { }
      void operator() (const std::string& x)
         {
         Path p (Directory.c_str());
         p.Append_path (x.c_str());
         container.push_back (p.Get_path());
         }
    };

// ------------------------------------------------------------------
//  Short description:
//     generic "for_each" function for removing directories and extensions
//     from a list of paths and storing just the names in a given stl container.

//  Notes:

//  Changes:
//    DPH 28/10/97
//    dph 12//2000 removed test for drive.

// ------------------------------------------------------------------
template < class Container >
class remove_directory_ext_and_copy : private std::unary_function <std::string, void >
   {
   private:
      Container& container;
   public:
      remove_directory_ext_and_copy( Container& c )
         : container(c) { }
      void operator() (const std::string& x)
         {
         Path p(x.c_str());
         container.push_back (p.Get_name_without_ext());
         }
    };

// Return the extension (anything after final ".") of a filename.
std::string EXPORT fileExtension(const std::string &filename);
// Return whether the extension is the same 
bool EXPORT fileExtensionEquals(const std::string &filename, const std::string &ext);
// Return the tail (anything after final "\") of a filename.
std::string EXPORT fileTail(const std::string &filename);
// Return the root (all of the characters in "filename" up to but not including the last "." ).
std::string EXPORT fileRoot(const std::string &filename);
// Return the directory that this file lives in
std::string EXPORT fileDirName(const std::string &filename);
// Return whether a file exists or not
bool EXPORT fileExists(const std::string &filename);
#endif

