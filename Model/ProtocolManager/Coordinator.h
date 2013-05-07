//---------------------------------------------------------------------------
#ifndef CoordinatorH
#define CoordinatorH

// ------------------------------------------------------------------
//  Short description:
//    This unit provides the functionality to manage a "system".
//    A "system" is a group of components.  It provides methods
//    to add/remove components, send messages and events between
//    components.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
class Coordinator : public protocol::Component
   {
   public:
      Coordinator(void);
      ~Coordinator(void);
      virtual std::string getType(void) {return "ProtocolManager";};

   private:
      typedef std::map<int, ComponentAlias*> Components;
      Components components;
      int sequencerID;
      int runningMessageID;

      int childComponentID;
      std::string  childComponentName;

      bool afterInit2;
      string title;
      int titleID;
      int componentsID;
      static vector<int> componentOrders;
      bool doTerminate;
      bool printReport;
      std::string parentName;
      std::stack<int> previousGetValueCompID;
      std::stack<int> previousGetValueRegID;
      std::stack<int> previousSetValueCompID;             //keep track of the requestSetValue caller  
      std::stack<int> previousSetValueRegID;
      std::set<std::string> variablesBeenPolledForSets;
      std::set<std::string> variablesBeenPolledForGets;

      virtual void doInit1(const protocol::Init1Data&);
      virtual void doInit2(void);
      virtual void doCommence(void);
      virtual void onReturnComponentIDMessage(protocol::ReturnComponentIDData& data);
      virtual void onRegisterMessage(unsigned int fromID, protocol::RegisterData& registerData);
      virtual void onDeregisterMessage(unsigned int fromID, protocol::DeregisterData& registerData);
      virtual void onPublishEventMessage(unsigned int fromID, protocol::PublishEventData& publishEventData);

      virtual void onTerminateSimulationMessage(void);
      virtual void onGetValueMessage(unsigned int fromID, protocol::GetValueData& getValueData);
      virtual void onReplyValueMessage(unsigned fromID, protocol::ReplyValueData replyValueData);
      virtual void onRequestComponentIDMessage(unsigned int fromID, protocol::RequestComponentIDData& data);
      virtual void onQueryInfoMessage(unsigned int fromID, unsigned int messageID, protocol::QueryInfoData& queryInfo);
      virtual void onRequestSetValueMessage(unsigned int fromID, protocol::RequestSetValueData& setValueData);
      virtual void onApsimChangeOrderData(unsigned int fromID, protocol::MessageData& messageData);
	  virtual void onQuerySetValueMessage(unsigned fromID, protocol::QuerySetValueData& querySetData, unsigned msgID);
      virtual void onReplySetValueSuccess(unsigned fromID, protocol::ReplySetValueSuccessData& replySetData, unsigned msgID);
	  
      virtual void respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);
      virtual void respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData);
      virtual bool respondToSet(unsigned int& fromID, protocol::QuerySetValueData& setValueData);
      virtual void notifyTermination(void);

      void addComponent(const std::string& name,
                        const std::string& executable,
                        const std::string& componentInterfaceExecutable,
                        const std::string& sdml,
						const std::string& compClass);

      // ------------------------------------------------------------------
      // Send queryValue messages to all subscribed components.
      // ------------------------------------------------------------------
      void sendQueryValueMessage(unsigned fromID, unsigned regID);

      //send the querySetValue message
      void sendQuerySetValueMessage(unsigned int from, unsigned int setterID, unsigned int to, unsigned int ID, protocol::RequestSetValueData& data);
      void sendApsimSetQueryMessage(unsigned int from, unsigned int to, const FString& regName, unsigned replyToID, unsigned replyID, protocol::Variant& variant);

      void reorderSubscriptions(vector<ApsimRegistration *>& subs);
      void readAllRegistrations(void);

      virtual void onApsimGetQuery(unsigned int fromID, protocol::ApsimGetQueryData& apsimGetQueryData);
      virtual void onError(protocol::ErrorData& errorData);
      void propogateEvent(unsigned int fromID, protocol::PublishEventData& publishEventData);

      void pollComponentsForGetVariable(int fromID, int destID, const string& variableName);


   };

inline const char *unQualifiedName(const char *szFQN);
inline char *qualifiedOwnerName(const char *szFQN, char *buf);

#endif
