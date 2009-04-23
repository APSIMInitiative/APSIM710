#ifndef InterfacesH
#define InterfacesH

#include <string>
#include <vector>

class MessageData;
class ArraySpecifier;


class Convertable
   {
   public:
      virtual ~Convertable() { }
      virtual void from(const std::vector<std::string>& values) = 0;
   };


class Packable
   {
   public:
      virtual ~Packable() { }
      virtual unsigned memorySize() = 0;
      virtual void pack(MessageData& messageData) = 0;
      virtual void unpack(MessageData& messageData, const std::string& ddml) = 0;
      virtual std::string ddml() = 0;
   };

#endif

