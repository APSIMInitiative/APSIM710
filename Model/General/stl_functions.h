//---------------------------------------------------------------------------
#ifndef STL_functionsH 
#define STL_functionsH


#include <functional>
#include <general/string_functions.h>
#include <general/stristr.h>
#include <boost/lexical_cast.hpp>

using namespace std;


// ------------------------------------------------------------------
//  Short description:
//    locates an object in a container and returns the numerical
//    location within that container.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
template <class InputIterator, class T>
int location_number (InputIterator first, InputIterator last, const T& value)
   {
   int index = 0;
   while (first != last && value != *first)
      {
      ++first;
      index++;
      }
   if (first == last)
      return -1;
   else
      return index;
   }


// ------------------------------------------------------------------
//  Short description:
//    Find an item in stl container.  This function is a replacement for the
//    find function in algorithm.h.  The released function cannot handle
//    containers of pointers properly.

//  Notes:

//  Changes:
//    DPH 17/4/1997
//    dph 16/5/2001 removed indirection from value.

// ------------------------------------------------------------------
template <class InputIterator, class T>
InputIterator Pfind (InputIterator first, InputIterator last, const T& value)
{
    while (first != last && !(*(*first) == value))
        ++first;
    return first;
}

// ------------------------------------------------------------------
//  Short description:
//    Destroys all elements in a container.  Works with
//    containers of pointers.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
template <class CT, class T>
void Destroy_pointers (CT& container, const T )
   {
   while (!container.empty())
      {
		T Ptr = container.back();
      container.pop_back();
		delete Ptr;
		}
   }


template <class T>
class delete_this
{
  public:
     T operator() (T item)
     {
       delete item;
       return 0; // set item to NULL
     }
};


template <template <class A> class container_type, class T>
void delete_container(container_type<T> &sequence)
{
  transform(sequence.begin(),
            sequence.end(),
            sequence.begin(),
            delete_this<T>());
  sequence.clear();
}


template <class CT, class T>
class string2double_and_store : public std::unary_function<T, void>
   {
   private:
      CT& container;
   public:
      string2double_and_store (CT& c) : container(c) {}
      void operator() (T& arg)
         {container.push_back (atof(arg.c_str()));}
   };

template <class CT, class T>
class double2string_and_store : public std::unary_function<T, void>
   {
   private:
      CT& container;
      int NumDecPlaces;
   public:
      double2string_and_store (CT& c, int numdecplaces)
         : container(c), NumDecPlaces(numdecplaces) {}
      void operator() (T& arg)
         {container.push_back (ftoa(arg, NumDecPlaces));}
   };

// ------------------------------------------------------------------
//  Short description:
//    convert a container of strings to a container of doubles.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
template <class CT1, class CT2>
void String2double (CT1& source, CT2& dest)
   {
   dest.erase(dest.begin(), dest.end());
   string2double_and_store<CT2, std::string> convert(dest);
   std::for_each(source.begin(), source.end(), convert);
   }

// ------------------------------------------------------------------
//  Short description:
//    convert a container of doubles to a container of strings with the
//    specified number of decimal places.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
template <class CT1, class CT2>
void Double2string (CT1& source, CT2& dest, int NumDecPlaces = 5)
   {
   dest.erase(dest.begin(), dest.end());
   double2string_and_store<CT2, double> convert(dest, NumDecPlaces);
   std::for_each(source.begin(), source.end(), convert);
   }


template <class CT, class T>
class UniqueBackInserter : public std::unary_function<T, void>
   {
   private:
      CT& container;
   public:
      UniqueBackInserter (CT& c) : container(c) {}
      void operator() (T& arg)
         {
         if (std::find(container.begin(), container.end(), arg) == container.end())
            container.push_back(arg);
         }
   };

template <class CT, class T>
class Get_name_and_store_function
   {
   private:
      CT& Container;
   public:
      Get_name_and_store_function(CT& container)
         : Container (container)
         { }

      void operator () (T arg)
         {
         Container.push_back (arg.Get_name());
         };
   };

template <class CT, class T>
class Get_filename_and_store_function
   {
   private:
      CT& Container;
   public:
      Get_filename_and_store_function(CT& container)
         : Container (container)
         { }

      void operator () (T arg)
         {
         Container.push_back (arg.Get_filename());
         };
   };

template <class CT, class T>
class PGetNameFunction
   {
   private:
      CT& Container;
   public:
      PGetNameFunction(CT& container)
         : Container (container)
         { }

      void operator () (T* arg)
         {
         Container.push_back (arg->getName());
         };
   };

template <class T, class CT=std::vector<std::string> >
class GetName
   {
   private:
      CT& Container;
   public:
      GetName(std::vector<std::string>& container)
         : Container (container)
         { }

      void operator () (const T& arg)
         {
         Container.push_back(arg.getName());
         };
   };

template <class CT, class T>
class GetFilenameAndStoreFunction
   {
   private:
      CT& Container;
   public:
      GetFilenameAndStoreFunction(CT& container)
         : Container (container)
         { }

      void operator () (T arg)
         {
         Container.push_back (arg.GetFilename());
         };
   };

template <class T>
class Find_by_name_predicate
   {
   private:
      std::string Name;
   public:
      Find_by_name_predicate(const char* name)
         : Name(name)
         { }

      bool operator () (T& arg)
         {
         return (Str_i_Eq(arg.Get_name(), Name));
         };
   };

template <class T>
class Find_by_filename_predicate
   {
   private:
      std::string File_name;
   public:
      Find_by_filename_predicate(const char* file_name)
         : File_name(file_name)
         { }

      bool operator () (T& arg)
         {
         return (Str_i_Eq(arg.Get_filename(), File_name));
         };
   };

template <class T>
class CallbackFunction
   {
   public:
      virtual ~CallbackFunction(void) { };
      virtual void callback(T x) = 0;
   };
template <class T>
class ConstCallbackFunction
   {
   public:
      virtual ~ConstCallbackFunction(void) { };
      virtual void callback(T x) const = 0;
   };

template <class CT, class T>
class GetNameCallback : public CallbackFunction<T>
   {
   public:
      CT& C;
      GetNameCallback(CT& c) : C(c) { }

      virtual void callback(T t) {C.push_back(t.getName());}
   };
template <class CT, class T>
class PGetNameCallback : public CallbackFunction<T*>
   {
   public:
      CT& C;
      PGetNameCallback(CT& c) : C(c) { }

      virtual void callback(T* t) {C.push_back(t->getName());}
   };

template <class T>
class PEqualToName
   {
   private:
      std::string name;
   public:
      PEqualToName(const std::string& n)
         : name(n) {}

      bool operator () (T* arg)
      {return (Str_i_Eq(arg->getName(), name));};
   };
template <class T>
class EqualToName
   {
   private:
      std::string name;
   public:
      EqualToName(const std::string& n)
         : name(n) {}

      bool operator () (T& arg)
         {return (Str_i_Eq(arg.getName(), name));};
   };
template <class T>
class MatchName
   {
   private:
      std::string name;
   public:
      MatchName(const std::string& n)
         : name(n) {}

      bool operator () (T& arg)
         {return (Str_i_Eq(arg.name, name));};
   };
template <class T>
class PEqualToFileName
   {
   private:
      std::string filename;
   public:
      PEqualToFileName(const std::string& fn)
         : filename(fn) {}

      bool operator () (T* arg)
         {return (Str_i_Eq(arg->getFilename(), filename));};
   };
class PartialStringComparison
   {
   private:
      const string& st;
   public:
      PartialStringComparison(const std::string& s)
         : st(s) {}

      bool operator () (const std::string& arg)
         {
         return (stristr((char*) arg.c_str(), st.c_str()) != NULL);
         }
   };
class CaseInsensitiveStringComparison
   {
   private:
      const std::string& st;
   public:
      CaseInsensitiveStringComparison(const std::string& s)
         : st(s) {}

      bool operator () (const std::string& arg)
         {
         return Str_i_Eq(arg, st);
         }
   };

template <class T>
class EqualToFileName
   {
   private:
      std::string filename;
   public:
      EqualToFileName(const std::string& fn)
         : filename(fn) {}

      bool operator () (T& arg)
         {return (Str_i_Eq(arg.getFilename(), filename));};
   };

template <class CT, class T>
class MatchNameAndStore : public CallbackFunction<T*>
   {
   public:
      CT& C;
      const std::string& nameToMatch;
      MatchNameAndStore(const std::string& nametomatch, CT& c)
         : C(c), nameToMatch(nametomatch) { }

      virtual void callback(T& t)
         {
         if (Str_i_Eq(t.getName(), nameToMatch))
            C.push_back(t);
         }
   };

template <class CT, class T>
class PMatchNameAndStore : public CallbackFunction<T*>
   {
   public:
      CT& C;
      const std::string& nameToMatch;
      PMatchNameAndStore(const std::string& nametomatch, CT& c)
         : C(c), nameToMatch(nametomatch) { }

      virtual void callback(T* t)
         {
         if (Str_i_Eq(t->getName(), nameToMatch))
            C.push_back(t);
         }
   };
template <class CT, class T>
class PrefixCallback
   {
   public:
      CT& C;
      T&  prefix;
      PrefixCallback(CT& c, T& p) : C(c), prefix(p) { }

      void operator() (T t) {C.push_back(prefix + t);}
   };

template <class T>
struct Pless : public std::binary_function<T*, T*, bool>
   {
   bool operator() (const T* x, const T* y) const { return *x < *y; }
   };

template <class InputIterator, class Function, class Predicate>
Function for_each_if (InputIterator first, InputIterator last,
                      Function f, Predicate pred)
  {
  while (first != last)
     {
     if (pred(*first))
        f(*first);
     first++;
     }
  return f;
  }
template <class CT, class T>
class GetAttribute
   {
   public:
      CT& C;
      const string attributeName;
      GetAttribute(const std::string& attributename, CT& c)
         : attributeName(attributename), C(c) { }

      void operator()(T t)
         {
         C.push_back(t.getAttribute(attributeName));
         }
   };
//---------------------------------------------------------------------------
// predicate to create a vector of strings that has a particular extension.
//---------------------------------------------------------------------------
template <class CT>
class MatchPartialStringAndStore
   {
   private:
      CT& matchingFiles;
      const string extensionToMatch;
   public:
      MatchPartialStringAndStore(const string& extension, CT& matchingfiles)
         : extensionToMatch(extension), matchingFiles(matchingfiles) { }
      void operator() (const string& st)
         {
         if (stristr(st.c_str(), extensionToMatch.c_str()) != NULL)
            matchingFiles.push_back(st);
         }
   };

//---------------------------------------------------------------------------
// predicate to create a vector of strings that has a particular extension.
//---------------------------------------------------------------------------
template <class CT>
class FilterContainer
   {
   private:
      CT& matchingFiles;
      const CT& subStrings;
   public:
      FilterContainer(const CT& substrings, CT& matchingfiles)
         : subStrings(substrings), matchingFiles(matchingfiles) { }
      void operator() (const string& st)
         {
         for (typename CT::const_iterator i = subStrings.begin(); i != subStrings.end(); i++)
            {
            if (stristr(st.c_str(), i->c_str()) != NULL &&
                find(matchingFiles.begin(), matchingFiles.end(), st) == matchingFiles.end())
               matchingFiles.push_back(st);
            }
         }
   };
//---------------------------------------------------------------------------
// functor to remove all characters, following a delimiter, from a string
// and store in a return container.
//---------------------------------------------------------------------------
template <class CT>
class RemoveSuffix : public std::unary_function<std::string, void>
   {
   private:
      CT& container;
      std::string delimiter;
   public:
      RemoveSuffix(const std::string& d, CT& c)
         : delimiter(d), container(c) {}
      void operator() (const std::string& st)
         {
         unsigned posDelimiter = st.find(delimiter);
         if (posDelimiter != std::string::npos)
            container.push_back(st.substr(0, posDelimiter));
         else
            container.push_back(st);
         }
   };
//---------------------------------------------------------------------------
// functor to remove all characters, preceeding and including a delimiter, from a string
// and store in a return container.
//---------------------------------------------------------------------------
template <class CT>
class RemovePrefix : public std::unary_function<std::string, void>
   {
   private:
      CT& container;
      std::string delimiter;
   public:
      RemovePrefix(const std::string& d, CT& c)
         : delimiter(d), container(c) {}
      void operator() (const std::string& st)
         {
         unsigned posDelimiter = st.find(delimiter);
         if (posDelimiter != std::string::npos)
            container.push_back(st.substr(posDelimiter+1));
         else
            container.push_back(st);
         }
   };
//---------------------------------------------------------------------------
// functor to remove a specified substring from a string and store in
// a new container.
//---------------------------------------------------------------------------
template <class CT>
class RemoveSubStringAndStore : public std::unary_function<std::string, void>
   {
   private:
      CT& container;
      const std::string subString;
   public:
      RemoveSubStringAndStore(const std::string& subSt, CT& c)
         : subString(subSt), container(c) {}
      void operator() (const std::string& st)
         {
         char* posSubString = stristr(st.c_str(), subString.c_str());
         if (posSubString != NULL)
            {
            unsigned pos = posSubString - st.c_str();
            string newString = st;
            newString.erase(pos, subString.length());
            container.push_back(newString);
            }
         }
   };
//---------------------------------------------------------------------------
// This class can be used by a class to define an iterator to a map of values.
// Normally a map iterator returns a pair when deferenced. This iterator
// returns the value in pair.second.  See ApsimDataFile.cpp for an example
// of its use.
//---------------------------------------------------------------------------
template <class _Value, class _MapIterator>
struct MapSecondIterator
   {
   typedef _Value  value_type;
   typedef _Value& reference;
   typedef _Value* pointer;
   typedef MapSecondIterator<_Value, _MapIterator> _Self;

   MapSecondIterator(_MapIterator i) : iter(i) { }
   reference operator*() const
      {
      return iter->second;
      }

//   _STLP_DEFINE_ARROW_OPERATOR;

   _Self& operator++()
      {
      iter++;
      return *this;
      }
   _Self operator++(int)
      {
      _Self __tmp = *this;
      iter++;
      return __tmp;
      }

   _Self& operator--()
      {
      iter--;
      return *this;
      }
   _Self operator--(int)
      {
      _Self __tmp = *this;
      iter--;
      return __tmp;
      }

   bool operator !=(const _Self& rhs)
      {
      return (iter != rhs.iter);
      }

   private:
      _MapIterator iter;

   };

template < class container_type, class T>
void StringToContainer (const string& Numbers,
                        container_type& container)
   {
   container.erase (container.begin(), container.end());
   std::vector<std::string> string_container;
   splitIntoValues(Numbers, " ", string_container);
   std::vector<std::string>::iterator st;
   for (st = string_container.begin();
        st != string_container.end();
        st++)
      {
      T val;
      val = boost::lexical_cast<T> (*st);
      container.push_back(val);
      }
   }

template < class container_type>
bool vectorsAreDifferent(container_type& c1, container_type& c2)
   {
   if (c1.size() != c2.size())
      return true;
   else
      {
      typename container_type::iterator i1; i1 = c1.begin();
      typename container_type::iterator i2; i2 = c2.begin();
      while (i1 != c1.end())
         {
         if (*i1 != *i2)
            return true;
         i1++;
         i2++;
         }
      }
   return false;
   }

#endif
