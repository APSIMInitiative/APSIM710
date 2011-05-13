//---------------------------------------------------------------------------
#ifndef FStringH
#define FStringH

#include <General/platform.h>
#include <string.h>
#include <stdexcept>
#ifdef __WIN32__
   #include <memory.h>
#endif

// This enumeration tells the constructor of an alias how to determine
// the initial "real" length of the string.
enum StringType {CString, FORString, EmptyString};

// ------------------------------------------------------------------
//  Short description:
//     Encapsulates a case insensitive FORTRAN string.

//  Notes:

//  Changes:
//    DPH 10/10/2000

// ------------------------------------------------------------------
class FString
   {
   public:
      static const unsigned npos = (unsigned) -1;

      // default constructor that doesn't alias to anything.
      FString(void)
         {
         aliasTo((const char*) NULL, 0);
         }
      // constructor for an alias to a char*
/*      FString(char* t)
         {
         aliasTo(t, strlen(t));
         }
*/      // constructor for an alias to a char*
      FString(const char* t)
         {
         aliasTo(t, strlen(t));
         }

      // constructor for an alias to a FORTRAN string
      FString(char* t, unsigned int tLength, StringType stringType)
         {
         aliasTo(t, tLength);
         if (stringType == CString)
            realLen = (unsigned)strlen(t);
         else if (stringType == EmptyString)
            realLen = 0;
         else
            calcRealLength();
         }
      // constructor for an alias to a FORTRAN string
      FString(const char* t, unsigned int tLength, StringType stringType)
         {
         if (tLength == 0)
            tLength = (unsigned)strlen(t);
         aliasTo(t, tLength);
         if (stringType == CString)
            realLen = (unsigned)strlen(t);
         else if (stringType == EmptyString)
            realLen = 0;
		 else
            calcRealLength();
         }
      // copy constructor
      FString(const FString& rhs)
         {
         text = rhs.text;
         canModify = rhs.canModify;
         len = rhs.len;
         realLen = rhs.realLen;
         }

      // alias to the specified string
      void aliasTo(char* t, const unsigned int tLength)
         {
         text = t;
         len = tLength;
         realLen = len;
         canModify = true;
         }
      // alias to the specified string
      void aliasTo(const char* t, const unsigned int tLength)
         {
         text = (char*)t;
         len = tLength;
         realLen = len;
         canModify = false;
         }

      const char* f_str(void) const
         {
         return text;
         }
      bool operator== (const FString& rhs) const
         {
#ifdef __WIN32__
         return (length() == rhs.length() &&
                 _strnicmp(f_str(), rhs.f_str(), length()) == 0);
#else
		 return (length() == rhs.length() &&
                 strncasecmp(f_str(), rhs.f_str(), length()) == 0);
#endif
         }
      bool operator!= (const FString& rhs) const
         {return !(*this == rhs);}
      char operator[] (unsigned index) {return text[index];}
      FString& operator= (const FString& rhs)
         {
         if (canModify)
            {
            unsigned int rhsLength = rhs.length();
            if (len < rhsLength)
               error("String truncation.  FORTRAN string not long enough\n"
                     "to hold the string:\n", rhs);
            else
               {
               if (text != rhs.text)
                  memcpy(text, rhs.f_str(), rhsLength);
               memset(&text[rhsLength], ' ', len - rhsLength);
               realLen = rhsLength;
               }
            }
         else
            {
            text = rhs.text;
            len = rhs.len;
            realLen = rhs.realLen;
            canModify = rhs.canModify;
            }
         return *this;
         }
      FString& operator+ (const FString& rhs)
         {
         if (canModify)
            {
            unsigned int rhsLength = rhs.length();
            if (len - length() < rhsLength)
               error("String truncation.  FORTRAN string not long enough\n"
                     "to hold the string:\n", rhs);
            else
               {
               memcpy(&text[length()], rhs.text, rhsLength);
               memset(&text[length()+rhsLength], ' ', len - rhsLength-length());
               }
            }
         else
            error("Cannot modify const string: ", rhs);

         return *this;
         }
      unsigned int length(void) const
         {
         return realLen;
         }
      unsigned int maxLength(void) const
         {
         return len;
         }
      FString substr(unsigned int pos, unsigned int nchar = npos) const
         {
         if (nchar == npos)
            nchar = length() - pos;
         else if (pos+nchar > len)
            error("Invalid index into string: ", *this);
         return FString(&text[pos], nchar, FORString);
         }
      void insert(unsigned int pos, const char* st)
         {
         if (canModify)
            {
            unsigned numCharsToInsert = strlen(st);
            if (len-realLen < numCharsToInsert)
               error("String not long enough to hold the string: ", st);
            else
               {
               char buffer[500];
               strncpy(buffer, &text[pos], realLen-pos);
               strncpy(&text[pos], st, numCharsToInsert);
               strncpy(&text[pos+numCharsToInsert], buffer, realLen-pos);
               realLen += numCharsToInsert;
               }
            }
         else
            error("Cannot modify const string: ", text);
         }
      void erase(unsigned int pos, unsigned int nchars)
         {
         if (canModify)
            {
            strncpy(&text[pos], &text[pos+nchars], realLen-(pos+nchars));
	        memset(&text[realLen-nchars], ' ', nchars);
            realLen -= nchars;
            }
         else
            error("Cannot modify const string: ", text);
         }
      unsigned int find(const char* st, unsigned startPos = 0) const
         // Find the position of a substring. This version largely based
		 // on a published algorithm for strstr()
         {
	     unsigned stLength = strlen(st);
         for (unsigned int i = startPos; i <= realLen - stLength; i++)
            {
				if (text[i] == *st)
				{
					const char* ch1 = text + i;
					const char* ch2 = st;
					while (true)
					{
						if (*++ch2 == '\0')
							return i;
						else if (*++ch1 != *ch2)
							break;
					}
				}
           }
         return npos;
         }

   private:
      char* text;
      bool canModify;
      unsigned int len;
      unsigned int realLen;

      void calcRealLength(void)
         {
         for (realLen = len; realLen > 0 && text[realLen-1] == ' '; realLen--);
         }
      void error(const FString& msg1, const FString& msg2) const
         {
         char* buffer = new char[msg1.length() + msg2.length() + 1];
         strncpy(buffer, msg1.text, msg1.length());
         buffer[msg1.length()] = 0;
         strncat(buffer, msg2.text, msg2.length());
         buffer[msg1.length() + msg2.length()] = 0;

         throw std::runtime_error(buffer);
#if 0
         MessageBox(NULL, buffer, "Internal error in FString", MB_ICONERROR | MB_OK);
         delete [] buffer;
#endif
         }

   };

//inline std::ostream& operator<< (std::ostream& out, const FString& st)
//   {
//   out.write(st.text, st.realLen);
//   return out;
//   }

// ------------------------------------------------------------------
//  Short description:
//     Encapsulates a list of FORTRAN strings.

//  Notes:

//  Changes:
//    DPH 10/10/2000

// ------------------------------------------------------------------
class FStrings
   {
   public:
      FStrings(void) : maxNumElements(0), numElements(0), elementLength(0) { }
      FStrings(char* t,
               const unsigned int elementlength,
               const unsigned int maxNumElement,
               unsigned int numElement)
         : maxNumElements(maxNumElement),
           numElements(numElement),
           elementLength(elementlength)
           {
           st.aliasTo(t, elementlength*maxNumElements);
           }

      template <class CT>
      FStrings& operator= (const CT& strings)
         {
         unsigned pos = 0;
         numElements = 0;
         if (strings.size() > maxNumElements)
            throw std::runtime_error("Too many strings for FORTRAN string array");
         for (typename CT::const_iterator i = strings.begin();
                                 i != strings.end();
                                 i++)
            {
            FString rhs((*i).c_str());
            st.substr(pos, elementLength) = rhs;
            pos += elementLength;
            numElements++;
            }
         return *this;
         }

/*      template <class CT>
      void toC(CT& strings)
         {
         unsigned pos = 0;
         for (unsigned int i = 0; i < numElements; i++)
            {
            strings.push_back(st.substr(pos, elementLength).asString());
            pos += elementLength;
            }
         }
*/      FString getString(unsigned index) const
         {
         return st.substr(index * elementLength, elementLength);
         }
      void addString(FString& st)
         {
         getString(numElements) = st;
         numElements++;
         }

      unsigned getNumElements(void) const {return numElements;}
      unsigned getElementLength(void) const {return elementLength;}
      FString getSt(void) const {return st;}
   private:
      unsigned maxNumElements;
      unsigned numElements;
      unsigned elementLength;
      FString st;
   };

#endif
