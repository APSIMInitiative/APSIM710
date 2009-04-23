#include "Messages.h"
#include "BuiltIns.h"

   //------ Complete ------

   void EXPORT pack(MessageData& messageData, const CompleteType& data)
      {
      pack(messageData, data.ackID);
      }
   void EXPORT unpack(MessageData& messageData, CompleteType& data)
      {
      unpack(messageData, data.ackID);
      }
   unsigned EXPORT memorySize(const CompleteType& data)
      {
      return 0
              + ::memorySize(data.ackID)
              ;
      }
   std::string EXPORT DDML(const CompleteType& data)
      {return "<type name=\"Complete\">"
               "<field name=\"ackID\" kind=\"integer4\" />"
               "</type>";}

   //------ ApsimGetQuery ------

   void EXPORT pack(MessageData& messageData, const ApsimGetQueryType& data)
      {
      pack(messageData, data.name);
      }
   void EXPORT unpack(MessageData& messageData, ApsimGetQueryType& data)
      {
      unpack(messageData, data.name);
      }
   unsigned EXPORT memorySize(const ApsimGetQueryType& data)
      {
      return 0
              + ::memorySize(data.name)
              ;
      }
   std::string EXPORT DDML(const ApsimGetQueryType& data)
      {return "<type name=\"ApsimGetQuery\">"
               "<field name=\"name\" kind=\"string\" />"
               "</type>";}

   //------ ApsimSetQuery ------

   void EXPORT pack(MessageData& messageData, const ApsimSetQueryType& data)
      {
      pack(messageData, data.name);
      pack(messageData, data.replyToID);
      pack(messageData, data.replyID);
      pack(messageData, data.variant);
      }
   void EXPORT unpack(MessageData& messageData, ApsimSetQueryType& data)
      {
      unpack(messageData, data.name);
      unpack(messageData, data.replyToID);
      unpack(messageData, data.replyID);
      unpack(messageData, data.variant);
      }
   unsigned EXPORT memorySize(const ApsimSetQueryType& data)
      {
      return 0
              + ::memorySize(data.name)
              + ::memorySize(data.replyToID)
              + ::memorySize(data.replyID)
              + ::memorySize(data.variant)
              ;
      }
   std::string EXPORT DDML(const ApsimSetQueryType& data)
      {return "<type name=\"ApsimSetQuery\">"
               "<field name=\"name\" kind=\"string\" />"
               "<field name=\"replyToID\" kind=\"integer4\" />"
               "<field name=\"replyID\" kind=\"integer4\" />"
               "<field name=\"variant\" kind=\"integer4\" />"
               "</type>";}

   //------ Error ------

   void EXPORT pack(MessageData& messageData, const ErrorType& data)
      {
      pack(messageData, data.isFatal);
      pack(messageData, data.msg);
      }
   void EXPORT unpack(MessageData& messageData, ErrorType& data)
      {
      unpack(messageData, data.isFatal);
      unpack(messageData, data.msg);
      }
   unsigned EXPORT memorySize(const ErrorType& data)
      {
      return 0
              + ::memorySize(data.isFatal)
              + ::memorySize(data.msg)
              ;
      }
   std::string EXPORT DDML(const ErrorType& data)
      {return "<type name=\"Error\">"
               "<field name=\"fatal\" kind=\"boolean\" />"
               "<field name=\"message\" kind=\"string\" />"
               "</type>";}

   //------ Event ------

   void EXPORT pack(MessageData& messageData, const EventType& data)
      {
      pack(messageData, data.ID);
      pack(messageData, data.publishedBy);
      pack(messageData, data.ddml);
      }
   void EXPORT unpack(MessageData& messageData, EventType& data)
      {
      unpack(messageData, data.ID);
      unpack(messageData, data.publishedBy);
      unpack(messageData, data.ddml);
      }
   unsigned EXPORT memorySize(const EventType& data)
      {
      return 0
              + ::memorySize(data.ID)
              + ::memorySize(data.publishedBy)
              + ::memorySize(data.ddml)
              ;
      }
   std::string EXPORT DDML(const EventType& data)
      {return "<type name=\"Event\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"publishedBy\" kind=\"integer4\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";}

   //------ GetValue ------

   void EXPORT pack(MessageData& messageData, const GetValueType& data)
      {
      pack(messageData, data.ID);
      }
   void EXPORT unpack(MessageData& messageData, GetValueType& data)
      {
      unpack(messageData, data.ID);
      }
   unsigned EXPORT memorySize(const GetValueType& data)
      {
      return 0
              + ::memorySize(data.ID)
              ;
      }
   std::string EXPORT DDML(const GetValueType& data)
      {return "<type name=\"GetValue\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "</type>";}

   //------ Init1 ------

   void EXPORT pack(MessageData& messageData, const Init1Type& data)
      {
      pack(messageData, data.sdml);
      pack(messageData, data.fqn);
      pack(messageData, data.inStartup);
      }
   void EXPORT unpack(MessageData& messageData, Init1Type& data)
      {
      unpack(messageData, data.sdml);
      unpack(messageData, data.fqn);
      unpack(messageData, data.inStartup);
      }
   unsigned EXPORT memorySize(const Init1Type& data)
      {
      return 0
              + ::memorySize(data.sdml)
              + ::memorySize(data.fqn)
              + ::memorySize(data.inStartup)
              ;
      }
   std::string EXPORT DDML(const Init1Type& data)
      {return "<type name=\"Init1\">"
               "<field name=\"sdml\" kind=\"string\" />"
               "<field name=\"fqn\" kind=\"string\" />"
               "<field name=\"inStartup\" kind=\"boolean\" />"
               "</type>";}

   //------ NotifySetValueSuccess ------

   void EXPORT pack(MessageData& messageData, const NotifySetValueSuccessType& data)
      {
      pack(messageData, data.ID);
      pack(messageData, data.success);
      }
   void EXPORT unpack(MessageData& messageData, NotifySetValueSuccessType& data)
      {
      unpack(messageData, data.ID);
      unpack(messageData, data.success);
      }
   unsigned EXPORT memorySize(const NotifySetValueSuccessType& data)
      {
      return 0
              + ::memorySize(data.ID)
              + ::memorySize(data.success)
              ;
      }
   std::string EXPORT DDML(const NotifySetValueSuccessType& data)
      {return "<type name=\"NotifySetValueSuccess\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"success\" kind=\"boolean\" />"
               "</type>";}

   //------ PublishEvent ------

   void EXPORT pack(MessageData& messageData, const PublishEventType& data)
      {
      pack(messageData, data.ID);
      pack(messageData, data.ddml);
      }
   void EXPORT unpack(MessageData& messageData, PublishEventType& data)
      {
      unpack(messageData, data.ID);
      unpack(messageData, data.ddml);
      }
   unsigned EXPORT memorySize(const PublishEventType& data)
      {
      return 0
              + ::memorySize(data.ID)
              + ::memorySize(data.ddml)
              ;
      }
   std::string EXPORT DDML(const PublishEventType& data)
      {return "<type name=\"PublishEvent\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";}

   //------ QueryInfo ------

   void EXPORT pack(MessageData& messageData, const QueryInfoType& data)
      {
      pack(messageData, data.name);
      pack(messageData, data.kind);
      }
   void EXPORT unpack(MessageData& messageData, QueryInfoType& data)
      {
      unpack(messageData, data.name);
      unpack(messageData, data.kind);
      }
   unsigned EXPORT memorySize(const QueryInfoType& data)
      {
      return 0
              + ::memorySize(data.name)
              + ::memorySize(data.kind)
              ;
      }
   std::string EXPORT DDML(const QueryInfoType& data)
      {return "<type name=\"QueryInfo\">"
               "<field name=\"name\" kind=\"string\" />"
               "<field name=\"kind\" kind=\"integer4\" />"
               "</type>";}

   //------ Register ------

   void EXPORT pack(MessageData& messageData, const RegisterType& data)
      {
      pack(messageData, data.kind);
      pack(messageData, data.ID);
      pack(messageData, data.destID);
      pack(messageData, data.name);
      pack(messageData, data.ddml);
      }
   void EXPORT unpack(MessageData& messageData, RegisterType& data)
      {
      unpack(messageData, data.kind);
      unpack(messageData, data.ID);
      unpack(messageData, data.destID);
      unpack(messageData, data.name);
      unpack(messageData, data.ddml);
      }
   unsigned EXPORT memorySize(const RegisterType& data)
      {
      return 0
              + ::memorySize(data.kind)
              + ::memorySize(data.ID)
              + ::memorySize(data.destID)
              + ::memorySize(data.name)
              + ::memorySize(data.ddml)
              ;
      }
   std::string EXPORT DDML(const RegisterType& data)
      {return "<type name=\"Register\">"
               "<field name=\"kind\" kind=\"integer4\" />"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"destID\" kind=\"integer4\" />"
               "<field name=\"name\" kind=\"string\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";}

   //------ DeRegister ------

   void EXPORT pack(MessageData& messageData, const DeRegisterType& data)
      {
      pack(messageData, data.kind);
      pack(messageData, data.ID);
      }
   void EXPORT unpack(MessageData& messageData, DeRegisterType& data)
      {
      unpack(messageData, data.kind);
      unpack(messageData, data.ID);
      }
   unsigned EXPORT memorySize(const DeRegisterType& data)
      {
      return 0
              + ::memorySize(data.kind)
              + ::memorySize(data.ID)
              ;
      }
   std::string EXPORT DDML(const DeRegisterType& data)
      {return "<type name=\"DeRegister\">"
               "<field name=\"kind\" kind=\"integer4\" />"
               "<field name=\"ID\" kind=\"integer4\" />"
               "</type>";}

   //------ ReplyValue ------

   void EXPORT pack(MessageData& messageData, const ReplyValueType& data)
      {
      pack(messageData, data.queryID);
      pack(messageData, data.ddml);
      }
   void EXPORT unpack(MessageData& messageData, ReplyValueType& data)
      {
      unpack(messageData, data.queryID);
      unpack(messageData, data.ddml);
      }
   unsigned EXPORT memorySize(const ReplyValueType& data)
      {
      return 0
              + ::memorySize(data.queryID)
              + ::memorySize(data.ddml)
              ;
      }
   std::string EXPORT DDML(const ReplyValueType& data)
      {return "<type name=\"ReplyValue\">"
               "<field name=\"queryID\" kind=\"integer4\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";}

   //------ RequestComponentID ------

   void EXPORT pack(MessageData& messageData, const RequestComponentIDType& data)
      {
      pack(messageData, data.replytoID);
      pack(messageData, data.name);
      }
   void EXPORT unpack(MessageData& messageData, RequestComponentIDType& data)
      {
      unpack(messageData, data.replytoID);
      unpack(messageData, data.name);
      }
   unsigned EXPORT memorySize(const RequestComponentIDType& data)
      {
      return 0
              + ::memorySize(data.replytoID)
              + ::memorySize(data.name)
              ;
      }
   std::string EXPORT DDML(const RequestComponentIDType& data)
      {return "<type name=\"RequestComponentID\">"
               "<field name=\"replytoID\" kind=\"integer4\" />"
               "<field name=\"name\" kind=\"string\" />"
               "</type>";}

   //------ ReturnComponentID ------

   void EXPORT pack(MessageData& messageData, const ReturnComponentIDType& data)
      {
      pack(messageData, data.fqdn);
      pack(messageData, data.ID);
      }
   void EXPORT unpack(MessageData& messageData, ReturnComponentIDType& data)
      {
      unpack(messageData, data.fqdn);
      unpack(messageData, data.ID);
      }
   unsigned EXPORT memorySize(const ReturnComponentIDType& data)
      {
      return 0
              + ::memorySize(data.fqdn)
              + ::memorySize(data.ID)
              ;
      }
   std::string EXPORT DDML(const ReturnComponentIDType& data)
      {return "<type name=\"ReturnComponentID\">"
               "<field name=\"fqdn\" kind=\"string\" />"
               "<field name=\"ID\" kind=\"integer4\" />"
               "</type>";}

   //------ RequestSetValue ------

   void EXPORT pack(MessageData& messageData, const RequestSetValueType& data)
      {
      pack(messageData, data.ID);
      pack(messageData, data.ddml);
      }
   void EXPORT unpack(MessageData& messageData, RequestSetValueType& data)
      {
      unpack(messageData, data.ID);
      unpack(messageData, data.ddml);
      }
   unsigned EXPORT memorySize(const RequestSetValueType& data)
      {
      return 0
              + ::memorySize(data.ID)
              + ::memorySize(data.ddml)
              ;
      }
   std::string EXPORT DDML(const RequestSetValueType& data)
      {return "<type name=\"RequestSetValue\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";}

   //------ ReturnInfo ------

   void EXPORT pack(MessageData& messageData, const ReturnInfoType& data)
      {
      pack(messageData, data.queryID);
      pack(messageData, data.compID);
      pack(messageData, data.ID);
      pack(messageData, data.name);
      pack(messageData, data.type);
      pack(messageData, data.kind);
      }
   void EXPORT unpack(MessageData& messageData, ReturnInfoType& data)
      {
      unpack(messageData, data.queryID);
      unpack(messageData, data.compID);
      unpack(messageData, data.ID);
      unpack(messageData, data.name);
      unpack(messageData, data.type);
      unpack(messageData, data.kind);
      }
   unsigned EXPORT memorySize(const ReturnInfoType& data)
      {
      return 0
              + ::memorySize(data.queryID)
              + ::memorySize(data.compID)
              + ::memorySize(data.ID)
              + ::memorySize(data.name)
              + ::memorySize(data.type)
              + ::memorySize(data.kind)
              ;
      }
   std::string EXPORT DDML(const ReturnInfoType& data)
      {return "<type name=\"ReturnInfo\">"
               "<field name=\"queryID\" kind=\"integer4\" />"
               "<field name=\"compID\" kind=\"integer4\" />"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"name\" kind=\"string\" />"
               "<field name=\"type\" kind=\"string\" />"
               "<field name=\"kind\" kind=\"integer4\" />"
               "</type>";}

   //------ ReturnValue ------

   void EXPORT pack(MessageData& messageData, const ReturnValueType& data)
      {
      pack(messageData, data.compID);
      pack(messageData, data.ID);
      pack(messageData, data.ddml);
      }
   void EXPORT unpack(MessageData& messageData, ReturnValueType& data)
      {
      unpack(messageData, data.compID);
      unpack(messageData, data.ID);
      unpack(messageData, data.ddml);
      }
   unsigned EXPORT memorySize(const ReturnValueType& data)
      {
      return 0
              + ::memorySize(data.compID)
              + ::memorySize(data.ID)
              + ::memorySize(data.ddml)
              ;
      }
   std::string EXPORT DDML(const ReturnValueType& data)
      {return "<type name=\"ReturnValue\">"
               "<field name=\"compID\" kind=\"integer4\" />"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";}

   //------ QueryValue ------

   void EXPORT pack(MessageData& messageData, const QueryValueType& data)
      {
      pack(messageData, data.ID);
      pack(messageData, data.requestedByID);
      }
   void EXPORT unpack(MessageData& messageData, QueryValueType& data)
      {
      unpack(messageData, data.ID);
      unpack(messageData, data.requestedByID);
      }
   unsigned EXPORT memorySize(const QueryValueType& data)
      {
      return 0
              + ::memorySize(data.ID)
              + ::memorySize(data.requestedByID)
              ;
      }
   std::string EXPORT DDML(const QueryValueType& data)
      {return "<type name=\"QueryValue\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"requestedByID\" kind=\"integer4\" />"
               "</type>";}

   //------ QuerySetValue ------

   void EXPORT pack(MessageData& messageData, const QuerySetValueType& data)
      {
      pack(messageData, data.ID);
      pack(messageData, data.ddml);
      }
   void EXPORT unpack(MessageData& messageData, QuerySetValueType& data)
      {
      unpack(messageData, data.ID);
      unpack(messageData, data.ddml);
      }
   unsigned EXPORT memorySize(const QuerySetValueType& data)
      {
      return 0
              + ::memorySize(data.ID)
              + ::memorySize(data.ddml)
              ;
      }
   std::string EXPORT DDML(const QuerySetValueType& data)
      {return "<type name=\"QuerySetValue\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";}

