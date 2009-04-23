#ifndef StructureConverterH
#define StructureConverterH
#include <string>
#include <general/platform.h>
class MessageData;

bool EXPORT unpackField(MessageData& messageData, const std::string& ddml, bool& Data, const std::string& Name);
bool EXPORT unpackField(MessageData& messageData, const std::string& ddml, int& Data, const std::string& Name);
bool EXPORT unpackField(MessageData& messageData, const std::string& ddml, float& Data, const std::string& Name);
bool EXPORT unpackField(MessageData& messageData, const std::string& ddml, double& Data, const std::string& Name);
bool EXPORT unpackField(MessageData& messageData, const std::string& ddml, std::string& Data, const std::string& Name);
bool EXPORT unpackField(MessageData& messageData, const std::string& ddml, char* Data, const std::string& Name);

#endif
