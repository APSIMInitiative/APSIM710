#include <../General/pch.h>
#include <stdio.h>
#include <fcntl.h>
#include <libxml/parser.h>
#include <libxml/nanohttp.h>
#include <string>
#include "string_functions.h"
#include "http.h"

tHTTP::tHTTP(void) 
{
  myContext = NULL;
  myContentType = NULL;

  xmlNanoHTTPInit();
}

tHTTP::~tHTTP(void) 
{
  if (myContext) { xmlNanoHTTPClose(myContext);}
  if (myContentType) { xmlFree(myContentType);}

  xmlNanoHTTPCleanup();
}

// get a URL, contents into file, return true on success
bool tHTTP::Get(const std::string& filename, const std::string& url)
{
  if (myContext) {xmlNanoHTTPClose(myContext); myContext = NULL;}
  if (myContentType) {xmlFree(myContentType); myContentType = NULL;}
  _ErrorMessage = "";
  
  FILE *fp = fopen(filename.c_str(), "wb");
  if (fp == NULL) 
     {
     _ErrorMessage = "Cannot open file: " + filename;
     return false;
     }

  myContext = xmlNanoHTTPOpen(url.c_str(), &myContentType);
  if (myContext == NULL) 
     {
     fclose(fp);
     _ErrorMessage = "HTTP Open failure: " + url; 
     return false;
     }

  int buflen = 5*1024*1024, bytesRead;
  char *buffer = (char*)malloc(buflen);
  if (buffer == NULL) 
     {
     fclose(fp);
     _ErrorMessage = "malloc failure"; 
     return false;
     }

  while ((bytesRead = xmlNanoHTTPRead(myContext, buffer, buflen)) > 0)
     {
     fwrite(buffer, sizeof(char), bytesRead, fp);
     }

  fclose(fp);
  free(buffer);

  _ErrorMessage = responseText();
    
  return( xmlNanoHTTPReturnCode(myContext) == 200);
}

// get a URL, contents into file, return string. Caller should check return code for success.
std::string tHTTP::Get(const std::string& url)
{
  if (myContext) {xmlNanoHTTPClose(myContext); myContext = NULL;}
  if (myContentType) {xmlFree(myContentType); myContentType = NULL;}

  myContext = xmlNanoHTTPOpen(url.c_str(), &myContentType);
  if (myContext == NULL) return(std::string(""));

  std::string result;
  int buflen = 5*1024*1024, bytesRead;
  char *buffer = (char*)malloc(buflen);
  
  while ((bytesRead = xmlNanoHTTPRead(myContext, buffer, buflen)) > 0)
     {
     result.append(buffer, bytesRead);
     }  

  free(buffer);
  
  return(result);
}

int tHTTP::responseCode(void) 
{
  if (myContext) return xmlNanoHTTPReturnCode(myContext);
  return 0;
}

std::string tHTTP::responseText(void) 
{
  if (myContext) {
     int code = xmlNanoHTTPReturnCode(myContext);
     switch (code) {
       case 200: return ("200 OK");
       case 400: return ("400 Bad Request");
       case 401: return ("401 Unauthorised");
       case 403: return ("403 Forbidden");
       case 404: return ("404 Not Found");
       case 405: return ("405 Method Not Allowed");
       case 408: return ("408 Request Time Out");
       case 500: return ("500 Internal Server Error");
       case 501: return ("501 Not Implemented");
       case 503: return ("503 Service Unavailable");

       default:  return (itoa(code));
     } 
     /* notreached */
  }   
  return "";
}

std::string tHTTP::contentType(void) 
{
   return std::string(myContentType);
}
