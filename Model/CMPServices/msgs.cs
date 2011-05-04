using System;

namespace CMPServices
{
    //=========================================================================
    /// <summary>
    /// Container for the public constants describing message types.
    /// </summary>
    //=========================================================================
    public class Msgs
    {
        //=========================================================================
        /// <summary>
        /// Array of names of the different types of messages.
        /// <example>
        /// <code>
        /// string msgName = Msgs.msgTypes[Msgs.MSG_ACTIVATE - 1];
        /// </code>
        /// </example>
        /// </summary>
        //=========================================================================
        public static string[] msgTypes = new string[32] 
        {
        "ACTIVATE",
        "ADD",
        "ERROR",
        "COMMENCE",
        "COMPLETE",
        "DEACTIVATE",
        "DELETE",
        "DEREGISTER",
        "EVENT",
        "GETVALUE",
        "INIT1",
        "INIT2",
        "NOTIFYDELETE",
        "NOTIFYREG",
        "NOTIFYSET",
        "NOTIFYTERMINATION",
        "PAUSE",
        "PUBLISHEVENT",
        "QUERYINFO",
        "QUERYSET",
        "QUERYVALUE",
        "REGISTER",
        "REINSTATE",
        "REPLYSET",
        "REPLYVALUE",
        "REQUESTCOMPID",
        "REQUESTSET",
        "RESUME",
        "RETURNCOMPID",
        "RETURNINFO",
        "RETURNVALUE",
        "TERMINATE"
        };

        /// <summary>
        /// maximum fields in a message
        /// </summary>
        public const int MAX_FLDS = 6;

        //define the message numbers and also the fields in the msgs
        //=========================================================================
        /// <summary>
        /// Activate message ID
        /// </summary>
        public const int MSG_ACTIVATE = 1;
        /// <summary>
        /// Activate component ID
        /// </summary>
        public const int MSG_ACTIVATE_COMP = 1001;

        //=========================================================================
        /// <summary>
        /// Add message ID
        /// </summary>
        public const int MSG_ADD = 2;
        /// <summary>
        /// Add a component using this SDML
        /// </summary>
        public const int MSG_ADD_SDML = 2001;

        //=========================================================================
        /// <summary>
        /// Error message ID
        /// </summary>
        public const int MSG_ERROR = 3;
        /// <summary>
        /// Is this a fatal message field?
        /// </summary>
        public const int MSG_ERROR_FATAL = 3001;
        /// <summary>
        /// String field describing the error
        /// </summary>
        public const int MSG_ERROR_MESSAGE = 3002;

        //=========================================================================
        /// <summary>
        /// Commence message ID
        /// </summary>
        public const int MSG_COMMENCE = 4;

        //=========================================================================
        /// <summary>
        /// Complete message ID
        /// </summary>
        public const int MSG_COMPLETE = 5;
        /// <summary>
        /// Message ID being acknowledged
        /// </summary>
        public const int MSG_COMPLETE_ACKID = 5001;

        //=========================================================================
        /// <summary>
        /// Deactivate a component ID.
        /// </summary>
        public const int MSG_DEACTIVATE = 6;
        /// <summary>
        /// Component ID to deactivate.
        /// </summary>
        public const int MSG_DEACTIVATE_COMP = 6001;

        //=========================================================================
        /// <summary>
        /// Delete a component ID.
        /// </summary>
        public const int MSG_DELETE = 7;
        /// <summary>
        /// Component ID to delete.
        /// </summary>
        public const int MSG_DELETE_COMP = 7001;

        //=========================================================================
        /// <summary>
        /// Deregister message ID.
        /// </summary>
        public const int MSG_DEREGISTER = 8;
        /// <summary>
        /// Kind of entity to deregister
        /// </summary>
        public const int MSG_DEREGISTER_KIND = 8001;
        /// <summary>
        /// ID of the entity to deregister.
        /// </summary>
        public const int MSG_DEREGISTER_ID = 8002;

        //=========================================================================
        /// <summary>
        /// Event message ID.
        /// </summary>
        public const int MSG_EVENT = 9;
        /// <summary>
        /// Event ID.
        /// </summary>
        public const int MSG_EVENT_ID = 9001;
        /// <summary>
        /// Published by this component ID.
        /// </summary>
        public const int MSG_EVENT_PUBLISHEDBY = 9002;
        /// <summary>
        /// Event type
        /// </summary>
        public const int MSG_EVENT_TYPE = 9003;
        /// <summary>
        /// Event data/params
        /// </summary>
        public const int MSG_EVENT_PARAMS = 9004;

        //=========================================================================
        /// <summary>
        /// GetValue message ID.
        /// </summary>
        public const int MSG_GETVALUE = 10;
        /// <summary>
        /// ID of property to retrieve.
        /// </summary>
        public const int MSG_GETVALUE_ID = 10001;

        //=========================================================================
        /// <summary>
        /// Init1 message ID.
        /// </summary>
        public const int MSG_INIT1 = 11;
        /// <summary>
        /// SDML script for the component.
        /// </summary>
        public const int MSG_INIT1_SDML = 11001;
        /// <summary>
        /// FQN of the component.
        /// </summary>
        public const int MSG_INIT1_FQN = 11002;
        /// <summary>
        /// Issued during startup.
        /// </summary>
        public const int MSG_INIT1_INSTARTUP = 11003;

        //=========================================================================
        /// <summary>
        /// Init2 message ID.
        /// </summary>
        public const int MSG_INIT2 = 12;

        //=========================================================================
        /// <summary>
        /// NotifyDelete message ID.
        /// </summary>
        public const int MSG_NOTIFYDELETE = 13;

        //=========================================================================
        /// <summary>
        /// Notify registration ID.
        /// </summary>
        public const int MSG_NOTIFYREG = 14;
        /// <summary>
        /// Registration ID of the entity.
        /// </summary>
        public const int MSG_NOTIFYREG_REG = 14001;
        /// <summary>
        /// Kind of entity.
        /// </summary>
        public const int MSG_NOTIFYREG_KIND = 14002;
        /// <summary>
        /// Owner ID.
        /// </summary>
        public const int MSG_NOTIFYREG_OWNER = 14003;
        /// <summary>
        /// ID of the entity.
        /// </summary>
        public const int MSG_NOTIFYREG_ID = 14004;
        /// <summary>
        /// Name of the entity.
        /// </summary>
        public const int MSG_NOTIFYREG_NAME = 14005;
        /// <summary>
        /// Type of registration.
        /// </summary>
        public const int MSG_NOTIFYREG_TYPE = 14006;

        //=========================================================================
        /// <summary>
        /// NotifySet message ID.
        /// </summary>
        public const int MSG_NOTIFYSET = 15;
        /// <summary>
        /// ID of the entity.
        /// </summary>
        public const int MSG_NOTIFYSET_ID = 15001;
        /// <summary>
        /// Successfully set.
        /// </summary>
        public const int MSG_NOTIFYSET_SUCCESS = 15002;

        //=========================================================================
        /// <summary>
        /// NotifyTermination message ID.
        /// </summary>
        public const int MSG_NOTIFYTERMINATION = 16;

        //=========================================================================
        /// <summary>
        /// Pause message ID.
        /// </summary>
        public const int MSG_PAUSE = 17;

        //=========================================================================
        /// <summary>
        /// PublishEvent message ID.
        /// </summary>
        public const int MSG_PUBLISHEVENT = 18;
        /// <summary>
        /// Published event ID.
        /// </summary>
        public const int MSG_PUBLISHEVENT_ID = 18001;
        /// <summary>
        /// Published DDML type.
        /// </summary>
        public const int MSG_PUBLISHEVENT_TYPE = 18002;
        /// <summary>
        /// Published event data/parameters
        /// </summary>
        public const int MSG_PUBLISHEVENT_PARAMS = 18003;

        //=========================================================================
        /// <summary>
        /// QueryInfo message ID.
        /// </summary>
        public const int MSG_QUERYINFO = 19;
        /// <summary>
        /// Name of entity to query.
        /// </summary>
        public const int MSG_QUERYINFO_NAME = 19001;
        /// <summary>
        /// Kind of entity to query.
        /// </summary>
        public const int MSG_QUERYINFO_KIND = 19002;

        //=========================================================================
        /// <summary>
        /// QuerySet message ID.
        /// </summary>
        public const int MSG_QUERYSET = 20;
        /// <summary>
        /// ID of entity to set.
        /// </summary>
        public const int MSG_QUERYSET_ID = 20001;
        /// <summary>
        /// Type of entity to set.
        /// </summary>
        public const int MSG_QUERYSET_TYPE = 20002;
        /// <summary>
        /// Value
        /// </summary>
        public const int MSG_QUERYSET_VALUE = 20003;

        //=========================================================================
        /// <summary>
        /// QueryValue message ID.
        /// </summary>
        public const int MSG_QUERYVALUE = 21;
        /// <summary>
        /// ID of entity to query.
        /// </summary>
        public const int MSG_QUERYVALUE_ID = 21001;
        /// <summary>
        /// Component ID of requester
        /// </summary>
        public const int MSG_QUERYVALUE_REQBY = 21002;

        //=========================================================================
        /// <summary>
        /// Register message ID.
        /// </summary>
        public const int MSG_REGISTER = 22;
        /// <summary>
        /// Kind of entity to register.
        /// </summary>
        public const int MSG_REGISTER_KIND = 22001;
        /// <summary>
        /// ID of the entity.
        /// </summary>
        public const int MSG_REGISTER_ID = 22002;
        /// <summary>
        /// Destination component ID.
        /// </summary>
        public const int MSG_REGISTER_DESTID = 22003;
        /// <summary>
        /// Name of the entity to register.
        /// </summary>
        public const int MSG_REGISTER_NAME = 22004;
        /// <summary>
        /// DDML type of the entity.
        /// </summary>
        public const int MSG_REGISTER_TYPE = 22005;

        //=========================================================================
        /// <summary>
        /// Reinstate message ID.
        /// </summary>
        public const int MSG_REINSTATE = 23;
        /// <summary>
        /// SDML script of the component.
        /// </summary>
        public const int MSG_REINSTATE_SDML = 23001;

        //=========================================================================
        /// <summary>
        /// ReplySet message ID.
        /// </summary>
        public const int MSG_REPLYSET = 24;
        /// <summary>
        /// Requested by.
        /// </summary>
        public const int MSG_REPLYSET_REQID = 24001;
        /// <summary>
        /// Success.
        /// </summary>
        public const int MSG_REPLYSET_OK = 24002;

        //=========================================================================
        /// <summary>
        /// ReplyValue message ID.
        /// </summary>
        public const int MSG_REPLYVALUE = 25;
        /// <summary>
        /// Query message ID.
        /// </summary>
        public const int MSG_REPLYVALUE_QUERYID = 25001;
        /// <summary>
        /// DDML type of the value.
        /// </summary>
        public const int MSG_REPLYVALUE_TYPE = 25002;
        /// <summary>
        /// Value.
        /// </summary>
        public const int MSG_REPLYVALUE_VALUE = 25003;

        //=========================================================================
        /// <summary>
        /// RequestComponentID message ID.
        /// </summary>
        public const int MSG_REQUESTCOMPID = 26;
        /// <summary>
        /// ID of the owner of the component.
        /// </summary>
        public const int MSG_REQUESTCOMPID_OWNER = 26001;
        /// <summary>
        /// Name of the component.
        /// </summary>
        public const int MSG_REQUESTCOMPID_NAME = 26002;

        //=========================================================================
        /// <summary>
        /// RequestSet message ID.
        /// </summary>
        public const int MSG_REQUESTSET = 27;
        /// <summary>
        /// ID of the property to set.
        /// </summary>
        public const int MSG_REQUESTSET_ID = 27001;
        /// <summary>
        /// DDML type of the property.
        /// </summary>
        public const int MSG_REQUESTSET_TYPE = 27002;
        /// <summary>
        /// Value.
        /// </summary>
        public const int MSG_REQUESTSET_VALUE = 27003;

        //=========================================================================
        /// <summary>
        /// Resume message ID.
        /// </summary>
        public const int MSG_RESUME = 28;

        //=========================================================================
        /// <summary>
        /// ReturnComponentID message ID.
        /// </summary>
        public const int MSG_RETURNCOMPID = 29;
        /// <summary>
        /// Component name.
        /// </summary>
        public const int MSG_RETURNCOMPID_FQN = 29001;
        /// <summary>
        /// Component ID.
        /// </summary>
        public const int MSG_RETURNCOMPID_ID = 29002;

        //=========================================================================
        /// <summary>
        /// ReturnInfo message ID.
        /// </summary>
        public const int MSG_RETURNINFO = 30;
        /// <summary>
        /// Message ID of the query.
        /// </summary>
        public const int MSG_RETURNINFO_QUERYID = 30001;
        /// <summary>
        /// Component ID.
        /// </summary>
        public const int MSG_RETURNINFO_COMPID = 30002;
        /// <summary>
        /// Entity ID.
        /// </summary>
        public const int MSG_RETURNINFO_ID = 30003;
        /// <summary>
        /// Name of the entity.
        /// </summary>
        public const int MSG_RETURNINFO_NAME = 30004;
        /// <summary>
        /// Entity DDML type.
        /// </summary>
        public const int MSG_RETURNINFO_TYPE = 30005;
        /// <summary>
        /// Kind of the entity.
        /// </summary>
        public const int MSG_RETURNINFO_KIND = 30006;

        //=========================================================================
        /// <summary>
        /// ReturnValue message ID.
        /// </summary>
        public const int MSG_RETURNVALUE = 31;
        /// <summary>
        /// Component ID.
        /// </summary>
        public const int MSG_RETURNVALUE_COMPID = 31001;
        /// <summary>
        /// Property ID.
        /// </summary>
        public const int MSG_RETURNVALUE_ID = 31002;
        /// <summary>
        /// DDML type of the value.
        /// </summary>
        public const int MSG_RETURNVALUE_TYPE = 31003;
        /// <summary>
        /// Value.
        /// </summary>
        public const int MSG_RETURNVALUE_VALUE = 31004;

        //=========================================================================
        /// <summary>
        /// Terminate message ID.
        /// </summary>
        public const int MSG_TERMINATE = 32;

    }
}
