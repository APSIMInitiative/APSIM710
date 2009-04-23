#ifndef MessageTypesH
#define MessageTypesH
#include <ComponentInterface/MessageDataExt.h>
#include <General/platform.h>
#include <ApsimShared/FString.h>

#define max_array_size 100

namespace protocol {

//-------------------- CompleteType
struct CompleteType
   {
   int ackID;
   };
struct FCompleteType
   {
   int ackID;
   };
inline protocol::MessageData& operator<<(protocol::MessageData& messageData, const CompleteType& data)
   {
   messageData << data.ackID;
   return messageData;
   }
inline protocol::MessageData& operator>>(protocol::MessageData& messageData, CompleteType& data)
   {
   messageData >> data.ackID;
   return messageData;
   }
std::string EXPORT DDML(const CompleteType& value);
std::string EXPORT DDML(const FCompleteType& value);
inline unsigned int memorySize(const CompleteType& data)
   {
   return 0
      + protocol::memorySize(data.ackID)
;
   }
inline unsigned int memorySize(const FCompleteType& data)
   {
   unsigned size = 0;
      size += protocol::memorySize(data.ackID);
;
   return size;
   }
void InitComplete(CompleteType& data, const std::string& xml);

//-------------------- ApsimGetQueryType
struct ApsimGetQueryType
   {
   std::string name;
   };
struct FApsimGetQueryType
   {
   char name[max_array_size];
   };
inline protocol::MessageData& operator<<(protocol::MessageData& messageData, const ApsimGetQueryType& data)
   {
   messageData << data.name;
   return messageData;
   }
inline protocol::MessageData& operator>>(protocol::MessageData& messageData, ApsimGetQueryType& data)
   {
   messageData >> data.name;
   return messageData;
   }
std::string EXPORT DDML(const ApsimGetQueryType& value);
std::string EXPORT DDML(const FApsimGetQueryType& value);
inline unsigned int memorySize(const ApsimGetQueryType& data)
   {
   return 0
      + protocol::memorySize(data.name)
;
   }
inline unsigned int memorySize(const FApsimGetQueryType& data)
   {
   unsigned size = 0;
      size += protocol::memorySize(FString(data.name, max_array_size, FORString));
;
   return size;
   }
void InitApsimGetQuery(ApsimGetQueryType& data, const std::string& xml);

//-------------------- ApsimSetQueryType
struct ApsimSetQueryType
   {
   std::string name;
   int replyToID;
   int replyID;
   int variant;
   };
struct FApsimSetQueryType
   {
   char name[max_array_size];
   int replyToID;
   int replyID;
   int variant;
   };
inline protocol::MessageData& operator<<(protocol::MessageData& messageData, const ApsimSetQueryType& data)
   {
   messageData << data.name;
   messageData << data.replyToID;
   messageData << data.replyID;
   messageData << data.variant;
   return messageData;
   }
inline protocol::MessageData& operator>>(protocol::MessageData& messageData, ApsimSetQueryType& data)
   {
   messageData >> data.name;
   messageData >> data.replyToID;
   messageData >> data.replyID;
   messageData >> data.variant;
   return messageData;
   }
std::string EXPORT DDML(const ApsimSetQueryType& value);
std::string EXPORT DDML(const FApsimSetQueryType& value);
inline unsigned int memorySize(const ApsimSetQueryType& data)
   {
   return 0
      + protocol::memorySize(data.name)
      + protocol::memorySize(data.replyToID)
      + protocol::memorySize(data.replyID)
      + protocol::memorySize(data.variant)
;
   }
inline unsigned int memorySize(const FApsimSetQueryType& data)
   {
   unsigned size = 0;
      size += protocol::memorySize(FString(data.name, max_array_size, FORString));
      size += protocol::memorySize(data.replyToID);
      size += protocol::memorySize(data.replyID);
      size += protocol::memorySize(data.variant);
;
   return size;
   }
void InitApsimSetQuery(ApsimSetQueryType& data, const std::string& xml);

//-------------------- ErrorType
struct ErrorType
   {
   bool isFatal;
   std::string msg;
   };
struct FErrorType
   {
   bool isFatal;
   char msg[max_array_size];
   };
inline protocol::MessageData& operator<<(protocol::MessageData& messageData, const ErrorType& data)
   {
   messageData << data.isFatal;
   messageData << data.msg;
   return messageData;
   }
inline protocol::MessageData& operator>>(protocol::MessageData& messageData, ErrorType& data)
   {
   messageData >> data.isFatal;
   messageData >> data.msg;
   return messageData;
   }
std::string EXPORT DDML(const ErrorType& value);
std::string EXPORT DDML(const FErrorType& value);
inline unsigned int memorySize(const ErrorType& data)
   {
   return 0
      + protocol::memorySize(data.isFatal)
      + protocol::memorySize(data.msg)
;
   }
inline unsigned int memorySize(const FErrorType& data)
   {
   unsigned size = 0;
      size += protocol::memorySize(data.isFatal);
      size += protocol::memorySize(FString(data.msg, max_array_size, FORString));
;
   return size;
   }
void InitError(ErrorType& data, const std::string& xml);

//-------------------- EventType
struct EventType
   {
   int ID;
   int publishedBy;
   std::string ddml;
   };
struct FEventType
   {
   int ID;
   int publishedBy;
   char ddml[max_array_size];
   };
inline protocol::MessageData& operator<<(protocol::MessageData& messageData, const EventType& data)
   {
   messageData << data.ID;
   messageData << data.publishedBy;
   messageData << data.ddml;
   return messageData;
   }
inline protocol::MessageData& operator>>(protocol::MessageData& messageData, EventType& data)
   {
   messageData >> data.ID;
   messageData >> data.publishedBy;
   messageData >> data.ddml;
   return messageData;
   }
std::string EXPORT DDML(const EventType& value);
std::string EXPORT DDML(const FEventType& value);
inline unsigned int memorySize(const EventType& data)
   {
   return 0
      + protocol::memorySize(data.ID)
      + protocol::memorySize(data.publishedBy)
      + protocol::memorySize(data.ddml)
;
   }
inline unsigned int memorySize(const FEventType& data)
   {
   unsigned size = 0;
      size += protocol::memorySize(data.ID);
      size += protocol::memorySize(data.publishedBy);
      size += protocol::memorySize(FString(data.ddml, max_array_size, FORString));
;
   return size;
   }
void InitEvent(EventType& data, const std::string& xml);

//-------------------- GetValueType
struct GetValueType
   {
   int ID;
   };
struct FGetValueType
   {
   int ID;
   };
inline protocol::MessageData& operator<<(protocol::MessageData& messageData, const GetValueType& data)
   {
   messageData << data.ID;
   return messageData;
   }
inline protocol::MessageData& operator>>(protocol::MessageData& messageData, GetValueType& data)
   {
   messageData >> data.ID;
   return messageData;
   }
std::string EXPORT DDML(const GetValueType& value);
std::string EXPORT DDML(const FGetValueType& value);
inline unsigned int memorySize(const GetValueType& data)
   {
   return 0
      + protocol::memorySize(data.ID)
;
   }
inline unsigned int memorySize(const FGetValueType& data)
   {
   unsigned size = 0;
      size += protocol::memorySize(data.ID);
;
   return size;
   }
void InitGetValue(GetValueType& data, const std::string& xml);

//-------------------- Init1Type
struct Init1Type
   {
   std::string sdml;
   std::string fqn;
   bool inStartup;
   };
struct FInit1Type
   {
   char sdml[max_array_size];
   char fqn[max_array_size];
   bool inStartup;
   };
inline protocol::MessageData& operator<<(protocol::MessageData& messageData, const Init1Type& data)
   {
   messageData << data.sdml;
   messageData << data.fqn;
   messageData << data.inStartup;
   return messageData;
   }
inline protocol::MessageData& operator>>(protocol::MessageData& messageData, Init1Type& data)
   {
   messageData >> data.sdml;
   messageData >> data.fqn;
   messageData >> data.inStartup;
   return messageData;
   }
std::string EXPORT DDML(const Init1Type& value);
std::string EXPORT DDML(const FInit1Type& value);
inline unsigned int memorySize(const Init1Type& data)
   {
   return 0
      + protocol::memorySize(data.sdml)
      + protocol::memorySize(data.fqn)
      + protocol::memorySize(data.inStartup)
;
   }
inline unsigned int memorySize(const FInit1Type& data)
   {
   unsigned size = 0;
      size += protocol::memorySize(FString(data.sdml, max_array_size, FORString));
      size += protocol::memorySize(FString(data.fqn, max_array_size, FORString));
      size += protocol::memorySize(data.inStartup);
;
   return size;
   }
void InitInit1(Init1Type& data, const std::string& xml);

//-------------------- NotifySetValueSuccessType
struct NotifySetValueSuccessType
   {
   int ID;
   bool success;
   };
struct FNotifySetValueSuccessType
   {
   int ID;
   bool success;
   };
inline protocol::MessageData& operator<<(protocol::MessageData& messageData, const NotifySetValueSuccessType& data)
   {
   messageData << data.ID;
   messageData << data.success;
   return messageData;
   }
inline protocol::MessageData& operator>>(protocol::MessageData& messageData, NotifySetValueSuccessType& data)
   {
   messageData >> data.ID;
   messageData >> data.success;
   return messageData;
   }
std::string EXPORT DDML(const NotifySetValueSuccessType& value);
std::string EXPORT DDML(const FNotifySetValueSuccessType& value);
inline unsigned int memorySize(const NotifySetValueSuccessType& data)
   {
   return 0
      + protocol::memorySize(data.ID)
      + protocol::memorySize(data.success)
;
   }
inline unsigned int memorySize(const FNotifySetValueSuccessType& data)
   {
   unsigned size = 0;
      size += protocol::memorySize(data.ID);
      size += protocol::memorySize(data.success);
;
   return size;
   }
void InitNotifySetValueSuccess(NotifySetValueSuccessType& data, const std::string& xml);

//-------------------- PublishEventType
struct PublishEventType
   {
   int ID;
   std::string ddml;
   };
struct FPublishEventType
   {
   int ID;
   char ddml[max_array_size];
   };
inline protocol::MessageData& operator<<(protocol::MessageData& messageData, const PublishEventType& data)
   {
   messageData << data.ID;
   messageData << data.ddml;
   return messageData;
   }
inline protocol::MessageData& operator>>(protocol::MessageData& messageData, PublishEventType& data)
   {
   messageData >> data.ID;
   messageData >> data.ddml;
   return messageData;
   }
std::string EXPORT DDML(const PublishEventType& value);
std::string EXPORT DDML(const FPublishEventType& value);
inline unsigned int memorySize(const PublishEventType& data)
   {
   return 0
      + protocol::memorySize(data.ID)
      + protocol::memorySize(data.ddml)
;
   }
inline unsigned int memorySize(const FPublishEventType& data)
   {
   unsigned size = 0;
      size += protocol::memorySize(data.ID);
      size += protocol::memorySize(FString(data.ddml, max_array_size, FORString));
;
   return size;
   }
void InitPublishEvent(PublishEventType& data, const std::string& xml);

//-------------------- QueryInfoType
struct QueryInfoType
   {
   std::string name;
   int kind;
   };
struct FQueryInfoType
   {
   char name[max_array_size];
   int kind;
   };
inline protocol::MessageData& operator<<(protocol::MessageData& messageData, const QueryInfoType& data)
   {
   messageData << data.name;
   messageData << data.kind;
   return messageData;
   }
inline protocol::MessageData& operator>>(protocol::MessageData& messageData, QueryInfoType& data)
   {
   messageData >> data.name;
   messageData >> data.kind;
   return messageData;
   }
std::string EXPORT DDML(const QueryInfoType& value);
std::string EXPORT DDML(const FQueryInfoType& value);
inline unsigned int memorySize(const QueryInfoType& data)
   {
   return 0
      + protocol::memorySize(data.name)
      + protocol::memorySize(data.kind)
;
   }
inline unsigned int memorySize(const FQueryInfoType& data)
   {
   unsigned size = 0;
      size += protocol::memorySize(FString(data.name, max_array_size, FORString));
      size += protocol::memorySize(data.kind);
;
   return size;
   }
void InitQueryInfo(QueryInfoType& data, const std::string& xml);

//-------------------- RegisterType
struct RegisterType
   {
   int kind;
   int ID;
   int destID;
   std::string name;
   std::string ddml;
   };
struct FRegisterType
   {
   int kind;
   int ID;
   int destID;
   char name[max_array_size];
   char ddml[max_array_size];
   };
inline protocol::MessageData& operator<<(protocol::MessageData& messageData, const RegisterType& data)
   {
   messageData << data.kind;
   messageData << data.ID;
   messageData << data.destID;
   messageData << data.name;
   messageData << data.ddml;
   return messageData;
   }
inline protocol::MessageData& operator>>(protocol::MessageData& messageData, RegisterType& data)
   {
   messageData >> data.kind;
   messageData >> data.ID;
   messageData >> data.destID;
   messageData >> data.name;
   messageData >> data.ddml;
   return messageData;
   }
std::string EXPORT DDML(const RegisterType& value);
std::string EXPORT DDML(const FRegisterType& value);
inline unsigned int memorySize(const RegisterType& data)
   {
   return 0
      + protocol::memorySize(data.kind)
      + protocol::memorySize(data.ID)
      + protocol::memorySize(data.destID)
      + protocol::memorySize(data.name)
      + protocol::memorySize(data.ddml)
;
   }
inline unsigned int memorySize(const FRegisterType& data)
   {
   unsigned size = 0;
      size += protocol::memorySize(data.kind);
      size += protocol::memorySize(data.ID);
      size += protocol::memorySize(data.destID);
      size += protocol::memorySize(FString(data.name, max_array_size, FORString));
      size += protocol::memorySize(FString(data.ddml, max_array_size, FORString));
;
   return size;
   }
void InitRegister(RegisterType& data, const std::string& xml);

//-------------------- DeRegisterType
struct DeRegisterType
   {
   int kind;
   int ID;
   };
struct FDeRegisterType
   {
   int kind;
   int ID;
   };
inline protocol::MessageData& operator<<(protocol::MessageData& messageData, const DeRegisterType& data)
   {
   messageData << data.kind;
   messageData << data.ID;
   return messageData;
   }
inline protocol::MessageData& operator>>(protocol::MessageData& messageData, DeRegisterType& data)
   {
   messageData >> data.kind;
   messageData >> data.ID;
   return messageData;
   }
std::string EXPORT DDML(const DeRegisterType& value);
std::string EXPORT DDML(const FDeRegisterType& value);
inline unsigned int memorySize(const DeRegisterType& data)
   {
   return 0
      + protocol::memorySize(data.kind)
      + protocol::memorySize(data.ID)
;
   }
inline unsigned int memorySize(const FDeRegisterType& data)
   {
   unsigned size = 0;
      size += protocol::memorySize(data.kind);
      size += protocol::memorySize(data.ID);
;
   return size;
   }
void InitDeRegister(DeRegisterType& data, const std::string& xml);

//-------------------- ReplyValueType
struct ReplyValueType
   {
   int queryID;
   std::string ddml;
   };
struct FReplyValueType
   {
   int queryID;
   char ddml[max_array_size];
   };
inline protocol::MessageData& operator<<(protocol::MessageData& messageData, const ReplyValueType& data)
   {
   messageData << data.queryID;
   messageData << data.ddml;
   return messageData;
   }
inline protocol::MessageData& operator>>(protocol::MessageData& messageData, ReplyValueType& data)
   {
   messageData >> data.queryID;
   messageData >> data.ddml;
   return messageData;
   }
std::string EXPORT DDML(const ReplyValueType& value);
std::string EXPORT DDML(const FReplyValueType& value);
inline unsigned int memorySize(const ReplyValueType& data)
   {
   return 0
      + protocol::memorySize(data.queryID)
      + protocol::memorySize(data.ddml)
;
   }
inline unsigned int memorySize(const FReplyValueType& data)
   {
   unsigned size = 0;
      size += protocol::memorySize(data.queryID);
      size += protocol::memorySize(FString(data.ddml, max_array_size, FORString));
;
   return size;
   }
void InitReplyValue(ReplyValueType& data, const std::string& xml);

//-------------------- RequestComponentIDType
struct RequestComponentIDType
   {
   int replytoID;
   std::string name;
   };
struct FRequestComponentIDType
   {
   int replytoID;
   char name[max_array_size];
   };
inline protocol::MessageData& operator<<(protocol::MessageData& messageData, const RequestComponentIDType& data)
   {
   messageData << data.replytoID;
   messageData << data.name;
   return messageData;
   }
inline protocol::MessageData& operator>>(protocol::MessageData& messageData, RequestComponentIDType& data)
   {
   messageData >> data.replytoID;
   messageData >> data.name;
   return messageData;
   }
std::string EXPORT DDML(const RequestComponentIDType& value);
std::string EXPORT DDML(const FRequestComponentIDType& value);
inline unsigned int memorySize(const RequestComponentIDType& data)
   {
   return 0
      + protocol::memorySize(data.replytoID)
      + protocol::memorySize(data.name)
;
   }
inline unsigned int memorySize(const FRequestComponentIDType& data)
   {
   unsigned size = 0;
      size += protocol::memorySize(data.replytoID);
      size += protocol::memorySize(FString(data.name, max_array_size, FORString));
;
   return size;
   }
void InitRequestComponentID(RequestComponentIDType& data, const std::string& xml);

//-------------------- ReturnComponentIDType
struct ReturnComponentIDType
   {
   std::string fqdn;
   int ID;
   };
struct FReturnComponentIDType
   {
   char fqdn[max_array_size];
   int ID;
   };
inline protocol::MessageData& operator<<(protocol::MessageData& messageData, const ReturnComponentIDType& data)
   {
   messageData << data.fqdn;
   messageData << data.ID;
   return messageData;
   }
inline protocol::MessageData& operator>>(protocol::MessageData& messageData, ReturnComponentIDType& data)
   {
   messageData >> data.fqdn;
   messageData >> data.ID;
   return messageData;
   }
std::string EXPORT DDML(const ReturnComponentIDType& value);
std::string EXPORT DDML(const FReturnComponentIDType& value);
inline unsigned int memorySize(const ReturnComponentIDType& data)
   {
   return 0
      + protocol::memorySize(data.fqdn)
      + protocol::memorySize(data.ID)
;
   }
inline unsigned int memorySize(const FReturnComponentIDType& data)
   {
   unsigned size = 0;
      size += protocol::memorySize(FString(data.fqdn, max_array_size, FORString));
      size += protocol::memorySize(data.ID);
;
   return size;
   }
void InitReturnComponentID(ReturnComponentIDType& data, const std::string& xml);

//-------------------- RequestSetValueType
struct RequestSetValueType
   {
   int ID;
   std::string ddml;
   };
struct FRequestSetValueType
   {
   int ID;
   char ddml[max_array_size];
   };
inline protocol::MessageData& operator<<(protocol::MessageData& messageData, const RequestSetValueType& data)
   {
   messageData << data.ID;
   messageData << data.ddml;
   return messageData;
   }
inline protocol::MessageData& operator>>(protocol::MessageData& messageData, RequestSetValueType& data)
   {
   messageData >> data.ID;
   messageData >> data.ddml;
   return messageData;
   }
std::string EXPORT DDML(const RequestSetValueType& value);
std::string EXPORT DDML(const FRequestSetValueType& value);
inline unsigned int memorySize(const RequestSetValueType& data)
   {
   return 0
      + protocol::memorySize(data.ID)
      + protocol::memorySize(data.ddml)
;
   }
inline unsigned int memorySize(const FRequestSetValueType& data)
   {
   unsigned size = 0;
      size += protocol::memorySize(data.ID);
      size += protocol::memorySize(FString(data.ddml, max_array_size, FORString));
;
   return size;
   }
void InitRequestSetValue(RequestSetValueType& data, const std::string& xml);

//-------------------- ReturnInfoType
struct ReturnInfoType
   {
   int queryID;
   int compID;
   int ID;
   std::string name;
   std::string type;
   int kind;
   };
struct FReturnInfoType
   {
   int queryID;
   int compID;
   int ID;
   char name[max_array_size];
   char type[max_array_size];
   int kind;
   };
inline protocol::MessageData& operator<<(protocol::MessageData& messageData, const ReturnInfoType& data)
   {
   messageData << data.queryID;
   messageData << data.compID;
   messageData << data.ID;
   messageData << data.name;
   messageData << data.type;
   messageData << data.kind;
   return messageData;
   }
inline protocol::MessageData& operator>>(protocol::MessageData& messageData, ReturnInfoType& data)
   {
   messageData >> data.queryID;
   messageData >> data.compID;
   messageData >> data.ID;
   messageData >> data.name;
   messageData >> data.type;
   messageData >> data.kind;
   return messageData;
   }
std::string EXPORT DDML(const ReturnInfoType& value);
std::string EXPORT DDML(const FReturnInfoType& value);
inline unsigned int memorySize(const ReturnInfoType& data)
   {
   return 0
      + protocol::memorySize(data.queryID)
      + protocol::memorySize(data.compID)
      + protocol::memorySize(data.ID)
      + protocol::memorySize(data.name)
      + protocol::memorySize(data.type)
      + protocol::memorySize(data.kind)
;
   }
inline unsigned int memorySize(const FReturnInfoType& data)
   {
   unsigned size = 0;
      size += protocol::memorySize(data.queryID);
      size += protocol::memorySize(data.compID);
      size += protocol::memorySize(data.ID);
      size += protocol::memorySize(FString(data.name, max_array_size, FORString));
      size += protocol::memorySize(FString(data.type, max_array_size, FORString));
      size += protocol::memorySize(data.kind);
;
   return size;
   }
void InitReturnInfo(ReturnInfoType& data, const std::string& xml);

//-------------------- ReturnValueType
struct ReturnValueType
   {
   int compID;
   int ID;
   std::string ddml;
   };
struct FReturnValueType
   {
   int compID;
   int ID;
   char ddml[max_array_size];
   };
inline protocol::MessageData& operator<<(protocol::MessageData& messageData, const ReturnValueType& data)
   {
   messageData << data.compID;
   messageData << data.ID;
   messageData << data.ddml;
   return messageData;
   }
inline protocol::MessageData& operator>>(protocol::MessageData& messageData, ReturnValueType& data)
   {
   messageData >> data.compID;
   messageData >> data.ID;
   messageData >> data.ddml;
   return messageData;
   }
std::string EXPORT DDML(const ReturnValueType& value);
std::string EXPORT DDML(const FReturnValueType& value);
inline unsigned int memorySize(const ReturnValueType& data)
   {
   return 0
      + protocol::memorySize(data.compID)
      + protocol::memorySize(data.ID)
      + protocol::memorySize(data.ddml)
;
   }
inline unsigned int memorySize(const FReturnValueType& data)
   {
   unsigned size = 0;
      size += protocol::memorySize(data.compID);
      size += protocol::memorySize(data.ID);
      size += protocol::memorySize(FString(data.ddml, max_array_size, FORString));
;
   return size;
   }
void InitReturnValue(ReturnValueType& data, const std::string& xml);

//-------------------- QueryValueType
struct QueryValueType
   {
   int ID;
   int requestedByID;
   };
struct FQueryValueType
   {
   int ID;
   int requestedByID;
   };
inline protocol::MessageData& operator<<(protocol::MessageData& messageData, const QueryValueType& data)
   {
   messageData << data.ID;
   messageData << data.requestedByID;
   return messageData;
   }
inline protocol::MessageData& operator>>(protocol::MessageData& messageData, QueryValueType& data)
   {
   messageData >> data.ID;
   messageData >> data.requestedByID;
   return messageData;
   }
std::string EXPORT DDML(const QueryValueType& value);
std::string EXPORT DDML(const FQueryValueType& value);
inline unsigned int memorySize(const QueryValueType& data)
   {
   return 0
      + protocol::memorySize(data.ID)
      + protocol::memorySize(data.requestedByID)
;
   }
inline unsigned int memorySize(const FQueryValueType& data)
   {
   unsigned size = 0;
      size += protocol::memorySize(data.ID);
      size += protocol::memorySize(data.requestedByID);
;
   return size;
   }
void InitQueryValue(QueryValueType& data, const std::string& xml);

//-------------------- QuerySetValueType
struct QuerySetValueType
   {
   int ID;
   std::string ddml;
   };
struct FQuerySetValueType
   {
   int ID;
   char ddml[max_array_size];
   };
inline protocol::MessageData& operator<<(protocol::MessageData& messageData, const QuerySetValueType& data)
   {
   messageData << data.ID;
   messageData << data.ddml;
   return messageData;
   }
inline protocol::MessageData& operator>>(protocol::MessageData& messageData, QuerySetValueType& data)
   {
   messageData >> data.ID;
   messageData >> data.ddml;
   return messageData;
   }
std::string EXPORT DDML(const QuerySetValueType& value);
std::string EXPORT DDML(const FQuerySetValueType& value);
inline unsigned int memorySize(const QuerySetValueType& data)
   {
   return 0
      + protocol::memorySize(data.ID)
      + protocol::memorySize(data.ddml)
;
   }
inline unsigned int memorySize(const FQuerySetValueType& data)
   {
   unsigned size = 0;
      size += protocol::memorySize(data.ID);
      size += protocol::memorySize(FString(data.ddml, max_array_size, FORString));
;
   return size;
   }
void InitQuerySetValue(QuerySetValueType& data, const std::string& xml);

} // protocol

#endif
