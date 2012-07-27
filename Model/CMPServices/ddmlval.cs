using System;
using System.Text;
using System.Xml;

namespace CMPServices
{
   //============================================================================
   /// <summary>
   /// A DDML value class that is a structured type specialised from <see cref="TTypedValue">TTypedValue</see> .
   /// Manipulates the DDML specified by the CMP specification.
   /// </summary>
   /// <example>
   /// <code>
   /// <![CDATA[
   /// <type name="a_record">
   ///    <field name="height"       kind="double"   unit="mm"   />
   ///    <field name="apples"       kind="integer2" array="T"   />
   ///    <field name="conductivity" kind="single"   unit="dS/m" />
   /// </type>
   ///      ... OR
   /// <type array="T">
   ///   <element kind="double" array="T" unit="oC" />
   /// </type>
   /// ]]>
   /// </code>
   /// </example>
   /// <seealso cref="TTypedValue">TTypedValue Class</seealso>
   //============================================================================
	public class TDDMLValue : TTypedValue
	{
       //============================================================================
       /// <summary>
       /// Constructs a DDML value using an XML description.
       /// </summary>
       /// <param name="sXML">XML text description.</param>
       /// <param name="sBaseType">Set the base type of this object.</param>
       //============================================================================
        public TDDMLValue(String sXML, String sBaseType) : base (sXML, sBaseType)
        {
           //required in this derived class
           buildType(sXML);                   //calls suitable virtual functions
        }
        //============================================================================
        /// <summary>
        /// Construct this object using the parser already created in the parent. Also
        /// use the dom node, baseNode to be the root node of the document for this
        /// new typed value. Can also specify the base type using szBaseType.
        /// </summary>
        /// <param name="parentParser">The parent's parser.</param>
        /// <param name="baseNode">DOMNode to use as the root node.</param>
        /// <param name="sBaseType">Used to set the base type.</param>
        //============================================================================
        public TDDMLValue(TSDMLParser parentParser, XmlNode baseNode, String sBaseType)
                  : base (parentParser, baseNode, sBaseType)
        {
           //required in this derived class
           buildType(parentParser, baseNode);  //calls suitable virtual functions
        }
        //============================================================================
        /// <summary>
        /// Creates a scalar of this aBaseType with sName.
        /// </summary>
        /// <param name="sName">Name of the scalar.</param>
        /// <param name="aBaseType">Base type of this scalar.</param>
        //============================================================================
        public TDDMLValue(String sName, TBaseType aBaseType) : base (sName, aBaseType)
        {
           //required in this derived class
           //create a scalar type of TTypedValue
           constructScalar(sName, aBaseType); //calls suitable virtual functions
        }

        //============================================================================
        /// <summary>
        /// Creates a one dimensional array of scalar items.
        /// </summary>
        /// <param name="sArrayName">Name of this array.</param>
        /// <param name="aBaseType">Set the base type of this array.</param>
        /// <param name="iNoElements">Create it with this number of elements.</param>
        //============================================================================
        public TDDMLValue(String sArrayName, TBaseType aBaseType, int iNoElements)
                  : base (sArrayName, aBaseType, iNoElements)
        {
           //required in this derived class
           //add array elements which are scalars
           addScalar("", aBaseType);     //calls suitable virtual function
           setElementCount((uint)iNoElements);
        }

        //============================================================================
        /// <summary>
        /// Creates a one dimensional array of arbitrary items.
        /// </summary>
        /// <param name="sArrayName">Name of this array.</param>
        /// <param name="baseValue">Use as the base type of the array elements.</param>
        /// <param name="iNoElements">Create it with this number of elements.</param>
        //============================================================================
        public TDDMLValue(String sArrayName, TTypedValue baseValue, int iNoElements)
                  : base (sArrayName, baseValue, iNoElements)
        {
            newMember( baseValue );
            setElementCount((uint)iNoElements);
        }
        //============================================================================
        /// <summary>
        /// Copy constructor. This constructor makes a copy of the source's structure.
        /// For specialised child classes, this constructor should be overriden.
        /// </summary>
        /// <param name="typedValue">Source TTypedValue</param>
        /// <seealso cref="TTypedValue">TTypedValue Class</seealso>
        //============================================================================
        public TDDMLValue(TTypedValue typedValue)
                  : base (typedValue)
        {
           //required in this derived class
           initTypeCopy(typedValue);  //calls suitable virtual functions
        }
        //============================================================================
        /// <summary>
        /// Adds a scalar to an array or record.
        /// </summary>
        /// <param name="sName">Name of the scalar.</param>
        /// <param name="aType">The basic type for this scalar.</param>
        /// <returns>A ref to the newly created scalar.</returns>
        //============================================================================
        public override TTypedValue addScalar(String sName, TBaseType aType)
        {
           TDDMLValue newScalar;

           TTypedValue result = null;

           if (FIsArray || FIsRecord) {
              newScalar = new TDDMLValue(sName, aType);
              addMember(newScalar);
              result = newScalar;
           }

           return result;
        }
        //============================================================================
        /// <summary>
        /// Builds the list of children typed values.
        /// </summary>
        //============================================================================
        protected override void getFldElemList()
        {
           TDDMLValue newMember;
           XmlNode memberNode;

           //builds the child list using the parent's parser and just shifts the
           //parser's topElement domnode respectively.
           memberNode = parser.firstMember(parser.rootNode());
           while (memberNode != null) {
              newMember = new TDDMLValue(parser, memberNode, "");
              FMembers.Add(newMember);    //add to the list of children

              memberNode = parser.nextMember(memberNode);
           }
        }
        //============================================================================
        /// <summary>
        /// Gets the XML representation of a <see cref="TTypedValue"/> or descendant.
        /// </summary>
        /// <param name="value">The typed value to be formatted.</param>
        /// <param name="startIndent">Formatting indentation space count. -1 = no indentation</param>
        /// <param name="tab">Number of spaces in each tab</param>
        /// <returns>The string containing the XML text.</returns>
        //============================================================================
        public override String getText(TTypedValue value, int startIndent, int tab)
        {
           int nextIndent;
           String sIndent;

           if (startIndent > -1) {
              nextIndent = startIndent;
              sIndent = new String(' ', nextIndent);
           }
           else {
              nextIndent = -1;
              sIndent = "";
           }

           StringBuilder sbuf = new StringBuilder(sIndent);
           sbuf.Append("<type name=\"");
           sbuf.Append(value.Name);
           sbuf.Append("\" ");
           sbuf.Append(writeFieldInfo(value, nextIndent, tab));
           if (!value.isScalar())
           {
               sbuf.Append(sIndent);
               sbuf.Append("</type>");
           }

           return sbuf.ToString();
        }

        //============================================================================
        /// <summary>
        /// Formats the xml text for the 'field' or 'element' description of the typed
        /// value stored as a TTypedValue.
        /// </summary>
        /// <param name="attrInfo"></param>
        /// <param name="indent"></param>
        /// <param name="tab">Number of spaces in each tab</param>
        /// <returns>Buffer containing the text</returns>
        /// N.Herrmann Apr 2002
        //============================================================================
        protected override String writeFieldInfo(TTypedValue attrInfo, int indent, int tab)
        {
            String elementType;
            uint i;
            int nextIndent;
            int oneIndent;
            String CR = "";
            TTypedValue firstElement;
            String sIndent = "";

            //determine how much to indent this description
            oneIndent = 0;
            nextIndent = -1;
            if (indent > -1)
            {
                CR = Environment.NewLine;
                oneIndent = tab;
                nextIndent = indent + oneIndent;
                sIndent = new String(' ', nextIndent);   //begin at this level
            }

            StringBuilder xml = new StringBuilder("");
            if (attrInfo.baseType() != TBaseType.ITYPE_DEF)
                xml.Append(" kind=\"" + attrInfo.typeName() + "\"");
            if (attrInfo.isArray())
                xml.Append(" array=\"T\"");

            if ((attrInfo.units().Length > 0) && (attrInfo.units()[0] != '-'))
            {
                xml.Append(" unit=\"" + attrInfo.units() + "\"");
            }

            //now nest into the fields/elements for this DDML type
            if (!attrInfo.isScalar())
            {
                xml.Append(">" + CR);
                if (attrInfo.isArray() && (attrInfo.baseType() == TBaseType.ITYPE_DEF))  //an array will only show the first child for DDML
                {
                    if (attrInfo.count() == 0)
                        firstElement = attrInfo.item(0);
                    else
                        firstElement = attrInfo.item(1);
                    elementType = "element";
                    xml.Append(sIndent + "<" + elementType);
                    xml.Append(writeFieldInfo(firstElement, nextIndent, oneIndent));
                    if (!firstElement.isScalar())
                        xml.Append(sIndent + "</" + elementType + ">" + CR);
                }
                else if (attrInfo.isRecord())
                {
                    for (i = 1; i <= attrInfo.count(); i++)         //for each child field
                    {
                        elementType = "field";
                        xml.Append(sIndent + "<" + elementType + " name=\"" + attrInfo.item(i).Name + "\"");
                        xml.Append(writeFieldInfo(attrInfo.item(i), nextIndent, oneIndent));
                        if (!attrInfo.item(i).isScalar())
                            xml.Append(sIndent + "</" + elementType + ">" + CR);
                    }
                }
            }
            else
                xml.Append("/>" + CR);

            return xml.ToString();
        }

        //============================================================================
        /// <summary>
        /// Uses the copy constructor to make a clone of a typedvalue's structure.
        /// It is then added as a member to an array or record.
        /// this virtual function is expected to be overriden so that new members are
        /// of the child classes' type.
        /// </summary>
        /// <param name="bluePrintValue">Use this TTypedValue as the blue print.</param>
        // N.Herrmann Apr 2002
        //============================================================================
        public override void newMember(TTypedValue bluePrintValue)
        {
           TDDMLValue newElement;

           newElement = new TDDMLValue(bluePrintValue); //calls copy constructor
           addMember(newElement);  //add the copy
        }
        //============================================================================
        /// <summary>
        /// Retrieve the DDML XML description of this object.
        /// </summary>
        /// <returns>Returns the unformatted XML</returns>
        // N.Herrmann Sep 2010
        //============================================================================
        public String asDDML()
        {
            return getText(this, -1, 0);
        }
	}
}





