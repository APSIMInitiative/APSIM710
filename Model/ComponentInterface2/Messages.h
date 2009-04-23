#ifndef MessagesH
#define MessagesH
#include <general/platform.h>
#include <string>

class MessageData;

//------ Complete ------
struct CompleteType
   {
   int ackID;
   };

void EXPORT pack(MessageData& messageData, const CompleteType& data);
void EXPORT unpack(MessageData& messageData, CompleteType& data);
unsigned EXPORT memorySize(const CompleteType& data);
std::string EXPORT DDML(const CompleteType& data);

//------ ApsimGetQuery ------
struct ApsimGetQueryType
   {
   std::string name;
   };

void EXPORT pack(MessageData& messageData, const ApsimGetQueryType& data);
void EXPORT unpack(MessageData& messageData, ApsimGetQueryType& data);
unsigned EXPORT memorySize(const ApsimGetQueryType& data);
std::string EXPORT DDML(const ApsimGetQueryType& data);

//------ ApsimSetQuery ------
struct ApsimSetQueryType
   {
   std::string name;
   int replyToID;
   int replyID;
   int variant;
   };

void EXPORT pack(MessageData& messageData, const ApsimSetQueryType& data);
void EXPORT unpack(MessageData& messageData, ApsimSetQueryType& data);
unsigned EXPORT memorySize(const ApsimSetQueryType& data);
std::string EXPORT DDML(const ApsimSetQueryType& data);

//------ Error ------
struct ErrorType
   {
   bool isFatal;
   std::string msg;
   };

void EXPORT pack(MessageData& messageData, const ErrorType& data);
void EXPORT unpack(MessageData& messageData, ErrorType& data);
unsigned EXPORT memorySize(const ErrorType& data);
std::string EXPORT DDML(const ErrorType& data);

//------ Event ------
struct EventType
   {
   int ID;
   int publishedBy;
   std::string ddml;
   };

void EXPORT pack(MessageData& messageData, const EventType& data);
void EXPORT unpack(MessageData& messageData, EventType& data);
unsigned EXPORT memorySize(const EventType& data);
std::string EXPORT DDML(const EventType& data);

//------ GetValue ------
struct GetValueType
   {
   int ID;
   };

void EXPORT pack(MessageData& messageData, const GetValueType& data);
void EXPORT unpack(MessageData& messageData, GetValueType& data);
unsigned EXPORT memorySize(const GetValueType& data);
std::string EXPORT DDML(const GetValueType& data);

//------ Init1 ------
struct Init1Type
   {
   std::string sdml;
   std::string fqn;
   bool inStartup;
   };

void EXPORT pack(MessageData& messageData, const Init1Type& data);
void EXPORT unpack(MessageData& messageData, Init1Type& data);
unsigned EXPORT memorySize(const Init1Type& data);
std::string EXPORT DDML(const Init1Type& data);

//------ NotifySetValueSuccess ------
struct NotifySetValueSuccessType
   {
   int ID;
   bool success;
   };

void EXPORT pack(MessageData& messageData, const NotifySetValueSuccessType& data);
void EXPORT unpack(MessageData& messageData, NotifySetValueSuccessType& data);
unsigned EXPORT memorySize(const NotifySetValueSuccessType& data);
std::string EXPORT DDML(const NotifySetValueSuccessType& data);

//------ PublishEvent ------
struct PublishEventType
   {
   int ID;
   std::string ddml;
   };

void EXPORT pack(MessageData& messageData, const PublishEventType& data);
void EXPORT unpack(MessageData& messageData, PublishEventType& data);
unsigned EXPORT memorySize(const PublishEventType& data);
std::string EXPORT DDML(const PublishEventType& data);

//------ QueryInfo ------
struct QueryInfoType
   {
   std::string name;
   int kind;
   };

void EXPORT pack(MessageData& messageData, const QueryInfoType& data);
void EXPORT unpack(MessageData& messageData, QueryInfoType& data);
unsigned EXPORT memorySize(const QueryInfoType& data);
std::string EXPORT DDML(const QueryInfoType& data);

//------ Register ------
struct RegisterType
   {
   int kind;
   int ID;
   int destID;
   std::string name;
   std::string ddml;
   };

void EXPORT pack(MessageData& messageData, const RegisterType& data);
void EXPORT unpack(MessageData& messageData, RegisterType& data);
unsigned EXPORT memorySize(const RegisterType& data);
std::string EXPORT DDML(const RegisterType& data);

//------ DeRegister ------
struct DeRegisterType
   {
   int kind;
   int ID;
   };

void EXPORT pack(MessageData& messageData, const DeRegisterType& data);
void EXPORT unpack(MessageData& messageData, DeRegisterType& data);
unsigned EXPORT memorySize(const DeRegisterType& data);
std::string EXPORT DDML(const DeRegisterType& data);

//------ ReplyValue ------
struct ReplyValueType
   {
   int queryID;
   std::string ddml;
   };

void EXPORT pack(MessageData& messageData, const ReplyValueType& data);
void EXPORT unpack(MessageData& messageData, ReplyValueType& data);
unsigned EXPORT memorySize(const ReplyValueType& data);
std::string EXPORT DDML(const ReplyValueType& data);

//------ RequestComponentID ------
struct RequestComponentIDType
   {
   int replytoID;
   std::string name;
   };

void EXPORT pack(MessageData& messageData, const RequestComponentIDType& data);
void EXPORT unpack(MessageData& messageData, RequestComponentIDType& data);
unsigned EXPORT memorySize(const RequestComponentIDType& data);
std::string EXPORT DDML(const RequestComponentIDType& data);

//------ ReturnComponentID ------
struct ReturnComponentIDType
   {
   std::string fqdn;
   int ID;
   };

void EXPORT pack(MessageData& messageData, const ReturnComponentIDType& data);
void EXPORT unpack(MessageData& messageData, ReturnComponentIDType& data);
unsigned EXPORT memorySize(const ReturnComponentIDType& data);
std::string EXPORT DDML(const ReturnComponentIDType& data);

//------ RequestSetValue ------
struct RequestSetValueType
   {
   int ID;
   std::string ddml;
   };

void EXPORT pack(MessageData& messageData, const RequestSetValueType& data);
void EXPORT unpack(MessageData& messageData, RequestSetValueType& data);
unsigned EXPORT memorySize(const RequestSetValueType& data);
std::string EXPORT DDML(const RequestSetValueType& data);

//------ ReturnInfo ------
struct ReturnInfoType
   {
   int queryID;
   int compID;
   int ID;
   std::string name;
   std::string type;
   int kind;
   };

void EXPORT pack(MessageData& messageData, const ReturnInfoType& data);
void EXPORT unpack(MessageData& messageData, ReturnInfoType& data);
unsigned EXPORT memorySize(const ReturnInfoType& data);
std::string EXPORT DDML(const ReturnInfoType& data);

//------ ReturnValue ------
struct ReturnValueType
   {
   int compID;
   int ID;
   std::string ddml;
   };

void EXPORT pack(MessageData& messageData, const ReturnValueType& data);
void EXPORT unpack(MessageData& messageData, ReturnValueType& data);
unsigned EXPORT memorySize(const ReturnValueType& data);
std::string EXPORT DDML(const ReturnValueType& data);

//------ QueryValue ------
struct QueryValueType
   {
   int ID;
   int requestedByID;
   };

void EXPORT pack(MessageData& messageData, const QueryValueType& data);
void EXPORT unpack(MessageData& messageData, QueryValueType& data);
unsigned EXPORT memorySize(const QueryValueType& data);
std::string EXPORT DDML(const QueryValueType& data);

//------ QuerySetValue ------
struct QuerySetValueType
   {
   int ID;
   std::string ddml;
   };

void EXPORT pack(MessageData& messageData, const QuerySetValueType& data);
void EXPORT unpack(MessageData& messageData, QuerySetValueType& data);
unsigned EXPORT memorySize(const QuerySetValueType& data);
std::string EXPORT DDML(const QuerySetValueType& data);

#endif
