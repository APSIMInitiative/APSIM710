using System;

namespace CMPServices
{
    //============================================================================
    /// <summary>
    /// Contains many constants used throughout the CMP system.
    /// </summary>
    //============================================================================
    public class TypeSpec
    {
        /// <summary>
        /// Entity kind. Driver type entity.
        /// </summary>
        public const int KIND_DRIVER = 1;
        /// <summary>
        /// Entity kind. Owned property.
        /// </summary>
        public const int KIND_OWNED = 2;
        /// <summary>
        /// Entity kind. Owned and readable property.
        /// </summary>
        public const int KIND_OWNED_R = 2;
        /// <summary>
        /// Entity kind. Owned and writeable.
        /// </summary>
        public const int KIND_OWNED_W = 3;
        /// <summary>
        /// Entity kind. Owned and readable and writeable.
        /// </summary>
        public const int KIND_OWNED_RW = 4;
        /// <summary>
        /// Entity kind. Published event.
        /// </summary>
        public const int KIND_PUBLISHEDEVENT = 5;
        /// <summary>
        /// Entity kind. Subscribed event.
        /// </summary>
        public const int KIND_SUBSCRIBEDEVENT = 6;
        /// <summary>
        /// Entity kind. Non system Component.
        /// </summary>
        public const int KIND_COMPONENT = 7;
        /// <summary>
        /// Entity kind. System component.
        /// </summary>
        public const int KIND_SYSTEM = 8;
        /// <summary>
        /// Entity kind. A Setter property.
        /// </summary>
        public const int KIND_REQUESTSET = 9;

        /// <summary>
        /// Event id for the published error event.
        /// </summary>
        public const int EVTERROR = 0;
        /// <summary>
        /// Subscribing event error id
        /// </summary>
        public const int EVTERROR_S = 1;

        /// <summary>
        /// Has no connection limit.
        /// </summary>
        public const int NO_MAXCONN = -1;

        /// <summary>
        /// Empty DDML type XML. 
        /// <example><code><![CDATA[<type/>]]></code></example>
        /// </summary>
        public const String TYPEEMPTY = "<type/>";
        /// <summary>
        /// "&lt;type"
        /// </summary>
        public const String TYPEDDML = "<type";


        /* Special types used by the default properties */
        /* used by driver and published events */
        /// <summary>
        /// DDML representation of the connection array.
        /// </summary>
        public const String TYPECONNECTED = "<type array=\"T\">" +
                                                  "<element>" +
                                                     "<field name=\"name\" kind=\"string\"/>" +
                                                     "<field name=\"connects\" kind=\"string\" array=\"T\"/>" +
                                                  "</element>" +
                                               "</type>";
        /* used by subscribed events */
        /// <summary>
        /// DDML representation of the subscribed events array used by the Sequencer.
        /// </summary>
        public const String TYPESUBEVENTS = "<type array=\"T\">" +
                                                  "<element>" +
                                                     "<field name=\"name\" kind=\"string\"/>" +
                                                     "<field name=\"include\" kind=\"boolean\"/>" +
                                                     "<field name=\"order\" kind=\"integer4\"/>" +
                                                  "</element>" +
                                               "</type>";
        //============================================================================
        /// <summary>
        /// DDML constant for the error type.
        /// <example>
        /// <code>
        /// <![CDATA[
        /// <type>
        ///   <field name="fatal" kind="boolean"/>
        ///   <field name="message" kind="string"/>
        /// </type>
        /// ]]>
        /// </code>
        /// </example>
        /// </summary>
        //============================================================================
        public const string typeERROR = "<type><field name=\"fatal\" kind=\"boolean\"/><field name=\"message\" kind=\"string\"/></type>";


    }

}
