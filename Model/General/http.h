//---------------------------------------------------------------------------
#ifndef GENERAL_HTTP_H
#define GENERAL_HTTP_H
#include <general/platform.h>
//---------------------------------------------------------------------------
// This tiny http class encapsulates libXMLs nanoHTTP interface.
//---------------------------------------------------------------------------
class EXPORT tHTTP
   {
   private:
      void *myContext;
      char *myContentType;
      std::string _ErrorMessage;

   public:
      tHTTP(void);
      ~tHTTP();

      std::string Get(const std::string& url);                        // get a URL, returning string
      bool Get(const std::string& filename, const std::string& url);   // get a URL, contents into file, true on success

      int responseCode(void);
      std::string responseText(void);
      std::string contentType(void);
      const std::string & ErrorMessage(void) { return(_ErrorMessage); };
   };

#endif
