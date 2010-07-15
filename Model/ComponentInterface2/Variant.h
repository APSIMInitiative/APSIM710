#ifndef VariantH
#define VariantH

#include <stdlib.h>
#include <string>
#include <vector>

#include <General/platform.h>

// ------------------------------------------------------------------
//  Short description:
//     Variant class for handling unspecified data types. These
//     structures are created at run time. Ugly as sin.
// ------------------------------------------------------------------
class EXPORT Variant 
      {
      private:
      public:
        char * bufStart;
        unsigned int bufLen;

        Variant();
        ~Variant();

        bool get(const std::string& name, std::string &value);
        bool get(const std::string& name, float &value);

        void pack(const std::string& name, const std::string &value);
        void pack(const std::string& name, std::vector<float> &value);
        void pack(const std::string& name, std::vector<std::string> &value);
        	
//        void * getRawData(void) {return (void*) bufStart;};
      };

class MessageData;

void EXPORT pack(MessageData& messageData, const Variant& data);
void EXPORT unpack(MessageData& messageData, Variant& data);
unsigned EXPORT memorySize(Variant& data) ;
std::string EXPORT DDML(const Variant& data);

#endif
