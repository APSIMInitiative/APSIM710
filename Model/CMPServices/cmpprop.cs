using System;
using System.Xml;
using CMPServices;

namespace CMPServices
{
   //===========================================================================
   /// <summary>
   /// Used for properties of a TCMPComponent or EditInfo class/>.
   /// </summary>
   //===========================================================================
    public class TCompProperty
    {
        /// <summary>
        /// True if readable.
        /// </summary>
        public Boolean bRead;
        /// <summary>
        /// True if writeable.
        /// </summary>
        public Boolean bWrite;
        /// <summary>
        /// True if this is an Init property of a component.
        /// </summary>
        public Boolean bInit;
        /// <summary>
        /// The TTypedValue attribute of this TCompProperty. Access the DDML type and
        /// any internal value using this attribute.
        /// </summary>
        public TInitValue InitValue;
        /// <summary>
        /// The name of this property.
        /// </summary>
        public String Name;
        /// <summary>
        /// Short description
        /// </summary>
        public string sDescr;         
        /// <summary>
        /// Full description
        /// </summary>
        public string sFullDescr;     

        //=======================================================================
        /// <summary>
        /// Creates a component property from the XML description of a property.
        /// </summary>
        /// <param name="sXML">XML for the property containing the DDML type. e.g. 
        /// &lt;property name=&quot;name&quot; access=&quot;read&quot; init=&quot;F&quot;&gt;
        ///   &lt;type kind=&quot;string&quot;&gt;&lt;defval&gt;&lt;/defval&gt;
        ///   &lt;/type&gt;
        /// &lt;/property&gt;
        /// </param>
        //=======================================================================
        public TCompProperty(String sXML)
        {
            TSDMLParser parser;
            String access;
            XmlNode anode;

            //create a parser. (not very nice creating a second parser but....)
            parser = new TSDMLParser(sXML);
            anode = parser.firstElementChild(parser.rootNode(), "type");
            InitValue = new TInitValue(parser, anode, "");

            bRead = false;
            bWrite = false;
            bInit = false;
            sDescr = "";
            sFullDescr = "";

            Name = parser.getAttrValue(parser.rootNode(), "name");
            InitValue.Name = Name;
            sDescr = parser.getAttrValue(parser.rootNode(), "descr");
            access = parser.getAttrValue(parser.rootNode(), "access");
            if (access == "both")
            {
                bRead = true;
                bWrite = true;
            }
            if ((access == "read") || (access.Length == 0))
                bRead = true;
            if (access == "write")
                bWrite = true;
            if (parser.getAttrValue(parser.rootNode(), "init") == "T")
            {
                bInit = true;
                initialiseMembers(InitValue);
            }
            anode = parser.firstElementChild(parser.rootNode(), "description");
            if (anode != null)
            {
                sFullDescr = parser.getText(anode);
            }
        }
        //=======================================================================
        /// <summary>
        /// Construct a TCompProperty using a TTypedValue object. It will be 
        /// non readable, non writeable, and not an init property. It's name will
        /// be ""
        /// </summary>
        /// <param name="typedValue">Typed Value to use as the blueprint</param>
        //=======================================================================
        public TCompProperty(TTypedValue typedValue)
        {
            bRead = false;
            bWrite = false;
            bInit = false;
            InitValue = new TInitValue(typedValue);
            Name = "";
            sDescr = "";
            sFullDescr = "";
        }
        //=======================================================================
        /// <summary>
        /// Copy constructor
        /// </summary>
        /// <param name="bluePrintValue"></param>
        //=======================================================================
        public TCompProperty(TCompProperty bluePrintValue)
        {
            bRead = bluePrintValue.bRead;
            bWrite = bluePrintValue.bWrite;
            bInit = bluePrintValue.bInit;
            InitValue = new TInitValue(bluePrintValue.InitValue);
            sDescr = bluePrintValue.sDescr;
            sFullDescr = bluePrintValue.sFullDescr;
            //what about sub field descrs??
        }
        //===========================================================================
        /// <summary>
        /// Initialise all the members of the TInitValue. Recurse into fields.
        /// </summary>
        /// <param name="item">The TInitValue to initialise</param>
        //===========================================================================
        protected void initialiseMembers(TInitValue item)
        {
            if (item.isScalar() && (item.getDefault() != null))
            {
                item.copyFrom(item.getDefault());
            }
            else
            {
                for (uint i = 1; i <= item.count(); i++) //for each member of the item
                {
                    TTypedValue child = item.item(i);
                    initialiseMembers((TInitValue)child);
                }
            }
        }
    }
   //===========================================================================
   /// <summary>
   /// Describes a driving variable of a TCMPComponent.
   /// </summary>
   //===========================================================================
   public class TCompDriver
   {
      private int FMinConn;
      private int FMaxConn;
      private int FConnCount;
      private String FName;
      /// <summary>
      /// Short description
      /// </summary>
      public string sDescr;
      /// <summary>
      /// Full description
      /// </summary>
      public string sFullDescr;
      /// <summary>
      /// The TTypedValue attribute of this TCompDriver. Access the DDML type and
      /// any internal value using this attribute.
      /// </summary>
      public TDDMLValue initType;
      //=======================================================================
      /// <summary>
      /// Creates a driving property object using the XML describing the property.
      /// </summary>
      /// <param name="sXML">XML for the driving property containing the DDML type.</param>
      //=======================================================================
       public TCompDriver(String sXML)
       {
           TSDMLParser parser;
           XmlNode anode;
           String sBuf;

           sDescr = "";
           sFullDescr = "";

           parser = new TSDMLParser(sXML);
           anode = parser.firstElementChild(parser.rootNode(), "type");
           initType = new TDDMLValue(parser, anode, "");

           FName = parser.getAttrValue(parser.rootNode(), "name");
           initType.Name = FName;
           sDescr = parser.getAttrValue(parser.rootNode(), "descr");

           FMinConn = 1;  // Default
           sBuf = parser.getAttrValue(parser.rootNode(), "minsrc");
           if (sBuf.Trim().Length > 0)
           {
               FMinConn = Convert.ToInt32(sBuf);
           }
           FMaxConn = 1;  // Default
           sBuf = parser.getAttrValue(parser.rootNode(), "maxsrc");
           if (sBuf.Trim().Length > 0)
           {
               FMaxConn = Convert.ToInt32(sBuf);
           }
           anode = parser.firstElementChild(parser.rootNode(), "description");
           if (anode != null)
           {
               sFullDescr = parser.getText(anode);
           }
       }
       //===========================================================================
       /// <summary>
       /// Copy constructor
       /// </summary>
       /// <param name="bluePrintValue"></param>
       //===========================================================================
       public TCompDriver(TCompDriver bluePrintValue)
       {
           initType = new TDDMLValue(bluePrintValue.initType);
           FMinConn = bluePrintValue.MinConn;
           FMaxConn = bluePrintValue.MaxConn;
           FConnCount = bluePrintValue.ConnCount;
           sDescr = bluePrintValue.sDescr;
           sFullDescr = bluePrintValue.sFullDescr;
       }
       //===========================================================================
       /// <summary>
       /// Name of the driving property.
       /// </summary>
       //===========================================================================
       public string Name
       {
           get { return FName; }
           set { FName = value; }
       }
       //===========================================================================
       /// <summary>
       /// The count of minimum number of connections.
       /// </summary>
       //===========================================================================
       public int MinConn
       {
           get { return FMinConn; }
           set { FMinConn = value; }
       }
       //===========================================================================
       /// <summary>
       /// Count of maximum number of connections.
       /// </summary>
       //===========================================================================
       public int MaxConn
       {
           get { return FMaxConn; }
           set { FMaxConn = value; }
       }
       //===========================================================================
       /// <summary>
       /// Count of connections.
       /// </summary>
       //===========================================================================
       public int ConnCount
       {
           get { return FConnCount; }
           set { FConnCount = value; }
       }
   }

   //===========================================================================
   /// <summary>
   /// This is the object that contains a component event of a TCMPComponent.
   /// </summary>
   //===========================================================================
   public class TCompEvent
   {
       private Boolean bIsEmpty;
       /// <summary>
       /// Kind of event. published/subscribed
       /// </summary>
       public String sKind;
       /// <summary>
       /// Include in the simulation sequence
       /// </summary>
       public Boolean bIncSequence;
       /// <summary>
       /// Sequencing order for subscribed events.
       /// </summary>
       public uint order;
       /// <summary>
       /// The TTypedValue attribute of this TCompEvent. Access the DDML type and
       /// any internal value using this attribute.
       /// </summary>
       public TDDMLValue initType;
       /// <summary>
       /// Name of this event.
       /// </summary>
       public String Name;
       /// <summary>
       /// Short description
       /// </summary>
       public string sDescr;
       /// <summary>
       /// Full description
       /// </summary>
       public string sFullDescr;
       //=======================================================================
       /// <summary>
       /// Construct a component event object using XML description.
       /// </summary>
       /// <param name="sXML">XML description that contains the DDML of the type.</param>
       //=======================================================================
       public TCompEvent(String sXML)
       {
           TXMLParser parser;
           XmlNode anode;
           String typeText;

           parser = new TXMLParser(sXML);
           sDescr = "";
           sFullDescr = "";

           //build a DDML type from the values in the event description
           typeText = "<type>";
           anode = parser.firstElementChild(parser.rootNode(), "field");
           bIsEmpty = (anode == null);
           while (anode != null)
           {
               typeText = typeText + parser.docToString(anode);
               anode = parser.nextElementSibling(anode, "field");
           }
           typeText = typeText + "</type>";

           initType = new TDDMLValue(typeText, "");     //create as a DDML type
           Name = parser.getAttrValue(parser.rootNode(), "name"); //set this object's attributes
           sKind = parser.getAttrValue(parser.rootNode(), "kind");
           sDescr = parser.getAttrValue(parser.rootNode(), "descr");

           if (sKind.Length < 1)
           {
               sKind = "published";        // Default when not specified
           }
           order = 0;                     //init to an invalid value
           anode = parser.firstElementChild(parser.rootNode(), "description");
           if (anode != null)
           {
               sFullDescr = parser.getText(anode);
           }
       }
       //=======================================================================
       /// <summary>
       /// Copy constructor
       /// </summary>
       /// <param name="bluePrintValue"></param>
       //=======================================================================
       public TCompEvent(TCompEvent bluePrintValue)
       {
           initType = new TDDMLValue(bluePrintValue.initType);
           bIsEmpty = bluePrintValue.isEmpty;
           sKind = bluePrintValue.sKind;
           bIncSequence = bluePrintValue.bIncSequence;
           order = bluePrintValue.order;
           sDescr = bluePrintValue.sDescr;
           sFullDescr = bluePrintValue.sFullDescr;
       }
       //=======================================================================
       /// <summary>
       /// Indicates the type of the event.
       /// <returns>True if this event is an empty type.</returns>
       /// </summary>
       public Boolean isEmpty
       {
           get { return bIsEmpty; }
           set { bIsEmpty = value; }
       }
   }
}
