#include "MessageTypes.h"

#include <General/xml.h>

using namespace std;
namespace protocol {
void InitComplete(CompleteType& data, const std::string& xml)
   {
   XMLDocument doc(xml, XMLDocument::xmlContents);
   XMLNode::iterator childI = doc.documentElement();
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("ackID"));

   data.ackID = boost::lexical_cast<int> (childI->getValue());
   }

std::string EXPORT DDML(const CompleteType& value)
   {
   return "<type name=\"Complete\">"
               "<field name=\"ackID\" kind=\"integer4\" />"
               "</type>";
   }
std::string EXPORT DDML(const FCompleteType& value)
   {
   return "<type name=\"Complete\">"
               "<field name=\"ackID\" kind=\"integer4\" />"
               "</type>";
   }
}; // namespace protocol
namespace protocol {
void InitApsimGetQuery(ApsimGetQueryType& data, const std::string& xml)
   {
   XMLDocument doc(xml, XMLDocument::xmlContents);
   XMLNode::iterator childI = doc.documentElement();
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("name"));

   data.name = boost::lexical_cast<std::string> (childI->getValue());
   }

std::string EXPORT DDML(const ApsimGetQueryType& value)
   {
   return "<type name=\"ApsimGetQuery\">"
               "<field name=\"name\" kind=\"string\" />"
               "</type>";
   }
std::string EXPORT DDML(const FApsimGetQueryType& value)
   {
   return "<type name=\"ApsimGetQuery\">"
               "<field name=\"name\" kind=\"string\" />"
               "</type>";
   }
}; // namespace protocol
namespace protocol {
void InitApsimSetQuery(ApsimSetQueryType& data, const std::string& xml)
   {
   XMLDocument doc(xml, XMLDocument::xmlContents);
   XMLNode::iterator childI = doc.documentElement();
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("name"));

   data.name = boost::lexical_cast<std::string> (childI->getValue());
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("replyToID"));

   data.replyToID = boost::lexical_cast<int> (childI->getValue());
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("replyID"));

   data.replyID = boost::lexical_cast<int> (childI->getValue());
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("variant"));

   data.variant = boost::lexical_cast<int> (childI->getValue());
   }

std::string EXPORT DDML(const ApsimSetQueryType& value)
   {
   return "<type name=\"ApsimSetQuery\">"
               "<field name=\"name\" kind=\"string\" />"
               "<field name=\"replyToID\" kind=\"integer4\" />"
               "<field name=\"replyID\" kind=\"integer4\" />"
               "<field name=\"variant\" kind=\"integer4\" />"
               "</type>";
   }
std::string EXPORT DDML(const FApsimSetQueryType& value)
   {
   return "<type name=\"ApsimSetQuery\">"
               "<field name=\"name\" kind=\"string\" />"
               "<field name=\"replyToID\" kind=\"integer4\" />"
               "<field name=\"replyID\" kind=\"integer4\" />"
               "<field name=\"variant\" kind=\"integer4\" />"
               "</type>";
   }
}; // namespace protocol
namespace protocol {
void InitError(ErrorType& data, const std::string& xml)
   {
   XMLDocument doc(xml, XMLDocument::xmlContents);
   XMLNode::iterator childI = doc.documentElement();
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("isFatal"));

   data.isFatal = boost::lexical_cast<bool> (childI->getValue());
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("msg"));

   data.msg = boost::lexical_cast<std::string> (childI->getValue());
   }

std::string EXPORT DDML(const ErrorType& value)
   {
   return "<type name=\"Error\">"
               "<field name=\"fatal\" kind=\"boolean\" />"
               "<field name=\"message\" kind=\"string\" />"
               "</type>";
   }
std::string EXPORT DDML(const FErrorType& value)
   {
   return "<type name=\"Error\">"
               "<field name=\"fatal\" kind=\"boolean\" />"
               "<field name=\"message\" kind=\"string\" />"
               "</type>";
   }
}; // namespace protocol
namespace protocol {
void InitEvent(EventType& data, const std::string& xml)
   {
   XMLDocument doc(xml, XMLDocument::xmlContents);
   XMLNode::iterator childI = doc.documentElement();
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("ID"));

   data.ID = boost::lexical_cast<int> (childI->getValue());
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("publishedBy"));

   data.publishedBy = boost::lexical_cast<int> (childI->getValue());
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("ddml"));

   data.ddml = boost::lexical_cast<std::string> (childI->getValue());
   }

std::string EXPORT DDML(const EventType& value)
   {
   return "<type name=\"Event\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"publishedBy\" kind=\"integer4\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";
   }
std::string EXPORT DDML(const FEventType& value)
   {
   return "<type name=\"Event\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"publishedBy\" kind=\"integer4\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";
   }
}; // namespace protocol
namespace protocol {
void InitGetValue(GetValueType& data, const std::string& xml)
   {
   XMLDocument doc(xml, XMLDocument::xmlContents);
   XMLNode::iterator childI = doc.documentElement();
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("ID"));

   data.ID = boost::lexical_cast<int> (childI->getValue());
   }

std::string EXPORT DDML(const GetValueType& value)
   {
   return "<type name=\"GetValue\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "</type>";
   }
std::string EXPORT DDML(const FGetValueType& value)
   {
   return "<type name=\"GetValue\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "</type>";
   }
}; // namespace protocol
namespace protocol {
void InitInit1(Init1Type& data, const std::string& xml)
   {
   XMLDocument doc(xml, XMLDocument::xmlContents);
   XMLNode::iterator childI = doc.documentElement();
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("sdml"));

   data.sdml = boost::lexical_cast<std::string> (childI->getValue());
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("fqn"));

   data.fqn = boost::lexical_cast<std::string> (childI->getValue());
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("inStartup"));

   data.inStartup = boost::lexical_cast<bool> (childI->getValue());
   }

std::string EXPORT DDML(const Init1Type& value)
   {
   return "<type name=\"Init1\">"
               "<field name=\"sdml\" kind=\"string\" />"
               "<field name=\"fqn\" kind=\"string\" />"
               "<field name=\"inStartup\" kind=\"boolean\" />"
               "</type>";
   }
std::string EXPORT DDML(const FInit1Type& value)
   {
   return "<type name=\"Init1\">"
               "<field name=\"sdml\" kind=\"string\" />"
               "<field name=\"fqn\" kind=\"string\" />"
               "<field name=\"inStartup\" kind=\"boolean\" />"
               "</type>";
   }
}; // namespace protocol
namespace protocol {
void InitNotifySetValueSuccess(NotifySetValueSuccessType& data, const std::string& xml)
   {
   XMLDocument doc(xml, XMLDocument::xmlContents);
   XMLNode::iterator childI = doc.documentElement();
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("ID"));

   data.ID = boost::lexical_cast<int> (childI->getValue());
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("success"));

   data.success = boost::lexical_cast<bool> (childI->getValue());
   }

std::string EXPORT DDML(const NotifySetValueSuccessType& value)
   {
   return "<type name=\"NotifySetValueSuccess\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"success\" kind=\"boolean\" />"
               "</type>";
   }
std::string EXPORT DDML(const FNotifySetValueSuccessType& value)
   {
   return "<type name=\"NotifySetValueSuccess\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"success\" kind=\"boolean\" />"
               "</type>";
   }
}; // namespace protocol
namespace protocol {
void InitPublishEvent(PublishEventType& data, const std::string& xml)
   {
   XMLDocument doc(xml, XMLDocument::xmlContents);
   XMLNode::iterator childI = doc.documentElement();
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("ID"));

   data.ID = boost::lexical_cast<int> (childI->getValue());
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("ddml"));

   data.ddml = boost::lexical_cast<std::string> (childI->getValue());
   }

std::string EXPORT DDML(const PublishEventType& value)
   {
   return "<type name=\"PublishEvent\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";
   }
std::string EXPORT DDML(const FPublishEventType& value)
   {
   return "<type name=\"PublishEvent\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";
   }
}; // namespace protocol
namespace protocol {
void InitQueryInfo(QueryInfoType& data, const std::string& xml)
   {
   XMLDocument doc(xml, XMLDocument::xmlContents);
   XMLNode::iterator childI = doc.documentElement();
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("name"));

   data.name = boost::lexical_cast<std::string> (childI->getValue());
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("kind"));

   data.kind = boost::lexical_cast<int> (childI->getValue());
   }

std::string EXPORT DDML(const QueryInfoType& value)
   {
   return "<type name=\"QueryInfo\">"
               "<field name=\"name\" kind=\"string\" />"
               "<field name=\"kind\" kind=\"integer4\" />"
               "</type>";
   }
std::string EXPORT DDML(const FQueryInfoType& value)
   {
   return "<type name=\"QueryInfo\">"
               "<field name=\"name\" kind=\"string\" />"
               "<field name=\"kind\" kind=\"integer4\" />"
               "</type>";
   }
}; // namespace protocol
namespace protocol {
void InitRegister(RegisterType& data, const std::string& xml)
   {
   XMLDocument doc(xml, XMLDocument::xmlContents);
   XMLNode::iterator childI = doc.documentElement();
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("kind"));

   data.kind = boost::lexical_cast<int> (childI->getValue());
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("ID"));

   data.ID = boost::lexical_cast<int> (childI->getValue());
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("destID"));

   data.destID = boost::lexical_cast<int> (childI->getValue());
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("name"));

   data.name = boost::lexical_cast<std::string> (childI->getValue());
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("ddml"));

   data.ddml = boost::lexical_cast<std::string> (childI->getValue());
   }

std::string EXPORT DDML(const RegisterType& value)
   {
   return "<type name=\"Register\">"
               "<field name=\"kind\" kind=\"integer4\" />"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"destID\" kind=\"integer4\" />"
               "<field name=\"name\" kind=\"string\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";
   }
std::string EXPORT DDML(const FRegisterType& value)
   {
   return "<type name=\"Register\">"
               "<field name=\"kind\" kind=\"integer4\" />"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"destID\" kind=\"integer4\" />"
               "<field name=\"name\" kind=\"string\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";
   }
}; // namespace protocol
namespace protocol {
void InitDeRegister(DeRegisterType& data, const std::string& xml)
   {
   XMLDocument doc(xml, XMLDocument::xmlContents);
   XMLNode::iterator childI = doc.documentElement();
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("kind"));

   data.kind = boost::lexical_cast<int> (childI->getValue());
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("ID"));

   data.ID = boost::lexical_cast<int> (childI->getValue());
   }

std::string EXPORT DDML(const DeRegisterType& value)
   {
   return "<type name=\"DeRegister\">"
               "<field name=\"kind\" kind=\"integer4\" />"
               "<field name=\"ID\" kind=\"integer4\" />"
               "</type>";
   }
std::string EXPORT DDML(const FDeRegisterType& value)
   {
   return "<type name=\"DeRegister\">"
               "<field name=\"kind\" kind=\"integer4\" />"
               "<field name=\"ID\" kind=\"integer4\" />"
               "</type>";
   }
}; // namespace protocol
namespace protocol {
void InitReplyValue(ReplyValueType& data, const std::string& xml)
   {
   XMLDocument doc(xml, XMLDocument::xmlContents);
   XMLNode::iterator childI = doc.documentElement();
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("queryID"));

   data.queryID = boost::lexical_cast<int> (childI->getValue());
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("ddml"));

   data.ddml = boost::lexical_cast<std::string> (childI->getValue());
   }

std::string EXPORT DDML(const ReplyValueType& value)
   {
   return "<type name=\"ReplyValue\">"
               "<field name=\"queryID\" kind=\"integer4\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";
   }
std::string EXPORT DDML(const FReplyValueType& value)
   {
   return "<type name=\"ReplyValue\">"
               "<field name=\"queryID\" kind=\"integer4\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";
   }
}; // namespace protocol
namespace protocol {
void InitRequestComponentID(RequestComponentIDType& data, const std::string& xml)
   {
   XMLDocument doc(xml, XMLDocument::xmlContents);
   XMLNode::iterator childI = doc.documentElement();
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("replytoID"));

   data.replytoID = boost::lexical_cast<int> (childI->getValue());
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("name"));

   data.name = boost::lexical_cast<std::string> (childI->getValue());
   }

std::string EXPORT DDML(const RequestComponentIDType& value)
   {
   return "<type name=\"RequestComponentID\">"
               "<field name=\"replytoID\" kind=\"integer4\" />"
               "<field name=\"name\" kind=\"string\" />"
               "</type>";
   }
std::string EXPORT DDML(const FRequestComponentIDType& value)
   {
   return "<type name=\"RequestComponentID\">"
               "<field name=\"replytoID\" kind=\"integer4\" />"
               "<field name=\"name\" kind=\"string\" />"
               "</type>";
   }
}; // namespace protocol
namespace protocol {
void InitReturnComponentID(ReturnComponentIDType& data, const std::string& xml)
   {
   XMLDocument doc(xml, XMLDocument::xmlContents);
   XMLNode::iterator childI = doc.documentElement();
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("fqdn"));

   data.fqdn = boost::lexical_cast<std::string> (childI->getValue());
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("ID"));

   data.ID = boost::lexical_cast<int> (childI->getValue());
   }

std::string EXPORT DDML(const ReturnComponentIDType& value)
   {
   return "<type name=\"ReturnComponentID\">"
               "<field name=\"fqdn\" kind=\"string\" />"
               "<field name=\"ID\" kind=\"integer4\" />"
               "</type>";
   }
std::string EXPORT DDML(const FReturnComponentIDType& value)
   {
   return "<type name=\"ReturnComponentID\">"
               "<field name=\"fqdn\" kind=\"string\" />"
               "<field name=\"ID\" kind=\"integer4\" />"
               "</type>";
   }
}; // namespace protocol
namespace protocol {
void InitRequestSetValue(RequestSetValueType& data, const std::string& xml)
   {
   XMLDocument doc(xml, XMLDocument::xmlContents);
   XMLNode::iterator childI = doc.documentElement();
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("ID"));

   data.ID = boost::lexical_cast<int> (childI->getValue());
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("ddml"));

   data.ddml = boost::lexical_cast<std::string> (childI->getValue());
   }

std::string EXPORT DDML(const RequestSetValueType& value)
   {
   return "<type name=\"RequestSetValue\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";
   }
std::string EXPORT DDML(const FRequestSetValueType& value)
   {
   return "<type name=\"RequestSetValue\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";
   }
}; // namespace protocol
namespace protocol {
void InitReturnInfo(ReturnInfoType& data, const std::string& xml)
   {
   XMLDocument doc(xml, XMLDocument::xmlContents);
   XMLNode::iterator childI = doc.documentElement();
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("queryID"));

   data.queryID = boost::lexical_cast<int> (childI->getValue());
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("compID"));

   data.compID = boost::lexical_cast<int> (childI->getValue());
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("ID"));

   data.ID = boost::lexical_cast<int> (childI->getValue());
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("name"));

   data.name = boost::lexical_cast<std::string> (childI->getValue());
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("type"));

   data.type = boost::lexical_cast<std::string> (childI->getValue());
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("kind"));

   data.kind = boost::lexical_cast<int> (childI->getValue());
   }

std::string EXPORT DDML(const ReturnInfoType& value)
   {
   return "<type name=\"ReturnInfo\">"
               "<field name=\"queryID\" kind=\"integer4\" />"
               "<field name=\"compID\" kind=\"integer4\" />"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"name\" kind=\"string\" />"
               "<field name=\"type\" kind=\"string\" />"
               "<field name=\"kind\" kind=\"integer4\" />"
               "</type>";
   }
std::string EXPORT DDML(const FReturnInfoType& value)
   {
   return "<type name=\"ReturnInfo\">"
               "<field name=\"queryID\" kind=\"integer4\" />"
               "<field name=\"compID\" kind=\"integer4\" />"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"name\" kind=\"string\" />"
               "<field name=\"type\" kind=\"string\" />"
               "<field name=\"kind\" kind=\"integer4\" />"
               "</type>";
   }
}; // namespace protocol
namespace protocol {
void InitReturnValue(ReturnValueType& data, const std::string& xml)
   {
   XMLDocument doc(xml, XMLDocument::xmlContents);
   XMLNode::iterator childI = doc.documentElement();
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("compID"));

   data.compID = boost::lexical_cast<int> (childI->getValue());
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("ID"));

   data.ID = boost::lexical_cast<int> (childI->getValue());
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("ddml"));

   data.ddml = boost::lexical_cast<std::string> (childI->getValue());
   }

std::string EXPORT DDML(const ReturnValueType& value)
   {
   return "<type name=\"ReturnValue\">"
               "<field name=\"compID\" kind=\"integer4\" />"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";
   }
std::string EXPORT DDML(const FReturnValueType& value)
   {
   return "<type name=\"ReturnValue\">"
               "<field name=\"compID\" kind=\"integer4\" />"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";
   }
}; // namespace protocol
namespace protocol {
void InitQueryValue(QueryValueType& data, const std::string& xml)
   {
   XMLDocument doc(xml, XMLDocument::xmlContents);
   XMLNode::iterator childI = doc.documentElement();
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("ID"));

   data.ID = boost::lexical_cast<int> (childI->getValue());
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("requestedByID"));

   data.requestedByID = boost::lexical_cast<int> (childI->getValue());
   }

std::string EXPORT DDML(const QueryValueType& value)
   {
   return "<type name=\"QueryValue\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"requestedByID\" kind=\"integer4\" />"
               "</type>";
   }
std::string EXPORT DDML(const FQueryValueType& value)
   {
   return "<type name=\"QueryValue\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"requestedByID\" kind=\"integer4\" />"
               "</type>";
   }
}; // namespace protocol
namespace protocol {
void InitQuerySetValue(QuerySetValueType& data, const std::string& xml)
   {
   XMLDocument doc(xml, XMLDocument::xmlContents);
   XMLNode::iterator childI = doc.documentElement();
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("ID"));

   data.ID = boost::lexical_cast<int> (childI->getValue());
   childI = std::find_if(doc.documentElement().begin(),
                         doc.documentElement().end(),
                         EqualToName<XMLNode>("ddml"));

   data.ddml = boost::lexical_cast<std::string> (childI->getValue());
   }

std::string EXPORT DDML(const QuerySetValueType& value)
   {
   return "<type name=\"QuerySetValue\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";
   }
std::string EXPORT DDML(const FQuerySetValueType& value)
   {
   return "<type name=\"QuerySetValue\">"
               "<field name=\"ID\" kind=\"integer4\" />"
               "<field name=\"ddml\" kind=\"string\" />"
               "</type>";
   }
}; // namespace protocol
