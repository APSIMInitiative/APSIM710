using System;
using System.Text;
using System.Xml;

namespace CMPServices
{
    //===========================================================================
    /// <summary>
    /// A class that contains a structured type specialised from <see cref="TTypedValue">TTypedValue</see> .
    /// Similar to a DDML type but contains max, min and default values. Usually contained 
    /// in a TBaseComp object and used in a component SDML XML text.
    /// </summary>
    /// <example>This is an XML representaion.
    /// <code>
    /// <![CDATA[<type kind="double" unit="oC">
    ///   <defval>0.0</defval>
    ///   <maxval>9.9E307</maxval>
    ///   <minval>-9.9E307</minval>
    /// </type>]]>
    /// </code>
    /// </example>
    //===========================================================================
    public class TInitValue : TTypedValue
    {
        /// <summary>
        /// 127
        /// </summary>
        public const int VERYLARGE_I1 = 127;
        /// <summary>
        /// 32767
        /// </summary>
        public const int VERYLARGE_I2 = 32767;
        /// <summary>
        /// 2147483646
        /// </summary>
        public const int VERYLARGE_I4 = 2147483646;
        /// <summary>
        /// 9223372036854775806
        /// </summary>
        public const Int64 VERYLARGE_I8 = 9223372036854775806;
        /// <summary>
        /// 3.402823466e+38F
        /// </summary>
        public const float VERYLARGE_S = 3.402823466e+38F;
        /// <summary>
        /// +9.9E307
        /// </summary>
        public const double VERYLARGE_D_POS = float.MaxValue;
        /// <summary>
        /// -9.9E307
        /// </summary>
        public const double VERYLARGE_D_NEG = -1 * float.MaxValue;

        /// <summary>
        /// -128
        /// </summary>
        public const int NO_RANGE_I1 = -128;
        /// <summary>
        /// -32768
        /// </summary>
        public const int NO_RANGE_I2 = -32768;
        /// <summary>
        /// -2147483647
        /// </summary>
        public const int NO_RANGE_I4 = -2147483647;
        /// <summary>
        /// -9223372036854775807
        /// </summary>
        public const Int64 NO_RANGE_I8 = Int64.MinValue;
        /// <summary>
        /// -3.402823466e+38F
        /// </summary>
        public const float NO_RANGE_S = float.MinValue;
        /// <summary>
        /// -9.9999E307
        /// </summary>
        public const double NO_RANGE_D = double.MinValue;
        /// <summary>
        /// Stores the maximum value.
        /// </summary>
        private TDDMLValue FMax;
        /// <summary>
        /// Stores the minimum value.
        /// </summary>
        private TDDMLValue FMin;
        /// <summary>
        /// Stores the default value.
        /// </summary>
        private TDDMLValue FDefault;
        /// <summary>
        /// Description field. Exposed using setDescr() and getDescr()
        /// </summary>
        private string FDescr;

        //============================================================================
        /// <summary>
        /// Constructs a typed value using an XML description.
        /// </summary>
        /// <param name="sXML">XML text description.</param>
        /// <param name="sBaseType">Set the base type of this object.</param>
        //============================================================================
        public TInitValue(String sXML, String sBaseType)
            : base(sXML, sBaseType)
        {
            //FMax = null;
            //FMin = null;
            FDefault = null;
            FDescr = "";

            //required in this derived class
            buildType(sXML);                   //calls suitable virtual functions

            if (isArray())                        // default array length is zero
                setElementCount(0);
        }

        //============================================================================
        /// <summary>
        /// Construct this object using the parser already created in the parent. Also
        /// use the dom node, baseNode to be the root node of the document for this
        /// new typed value. Can also specify the base type using sBaseType.
        /// </summary>
        /// <param name="parentParser">The parent's parser.</param>
        /// <param name="baseNode">DOM node to use as the root node.</param>
        /// <param name="sBaseType">Used to set the base type.</param>
        //============================================================================
        public TInitValue(TSDMLParser parentParser, XmlNode baseNode, String sBaseType)
            : base(parentParser, baseNode, sBaseType)
        {
            //FMax = null;
            //FMin = null;
            FDefault = null;
            FDescr = "";

            //required in this derived class
            buildType(parentParser, baseNode);  //calls suitable virtual functions

            if (isArray())                        // default array length is zero
                setElementCount(0);
        }

        //============================================================================
        /// <summary>
        /// Creates a scalar of this iBaseType with sName.
        /// </summary>
        /// <param name="sName">Name of the scalar.</param>
        /// <param name="aBaseType">Base type of this scalar.</param>
        //============================================================================
        public TInitValue(String sName, TBaseType aBaseType)
            : base(sName, aBaseType)
        {
            //FMax = null;
            //FMin = null;
            FDefault = null;
            FDescr = "";
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
        public TInitValue(String sArrayName, TBaseType aBaseType, int iNoElements)
            : base(sArrayName, aBaseType, iNoElements)
        {
            //FMax = null;
            //FMin = null;
            FDefault = null;
            FDescr = "";
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
        public TInitValue(String sArrayName, TTypedValue baseValue, int iNoElements)
            : base(sArrayName, baseValue, iNoElements)
        {
            //FMax = null;
            //FMin = null;
            FDefault = null;
            FDescr = "";
            newMember(baseValue);
            setElementCount((uint)iNoElements);
        }
        //============================================================================
        /// <summary>
        /// Copy constructor. This constructor makes a copy of the source's structure.
        /// For specialised child classes, this constructor should be overriden.
        /// </summary>
        /// <param name="typedValue">TTypedValue to use as the source.</param>
        //============================================================================
        public TInitValue(TTypedValue typedValue)
            : base(typedValue)
        {
            //FMax = null;
            //FMin = null;
            FDefault = null;
            FDescr = "";
            //required in this derived class
            initTypeCopy(typedValue);  //calls suitable virtual functions

            TInitValue other = (TInitValue)typedValue;
            if (other != null)
            {
                if (other.FDefault != null)
                {
                    if (FDefault == null)
                        FDefault = new TDDMLValue("defval", FBaseType);
                    FDefault.copyFrom(other.FDefault);
                }
                if (other.FMin != null)
                {
                    if (FMin == null)
                        FMin = new TDDMLValue("minval", FBaseType);
                    FMin.copyFrom(other.FMin);
                }
                if (other.FMax != null)
                {
                    if (FMax == null)
                        FMax = new TDDMLValue("maxval", FBaseType);
                    FMax.copyFrom(other.FMax);
                }
            }
        }

        //============================================================================
        /// <summary>
        /// Default constructor
        /// </summary>
        //============================================================================
        public TInitValue()
            : base("", "")
        {
            //FMax = null;
            //FMin = null;
            FDefault = null;
            FDescr = "";
        }

        //============================================================================
        /// <summary>
        /// Create a scalar item of this type.
        /// </summary>
        //============================================================================
        protected override void createScalar()
        {
            base.createScalar();   //allocates memory for the type
            makeDefaultAndRange();
        }

        //============================================================================
        /// <summary>
        /// Finds children nodes in the XML doc and creates member items of this type.
        /// </summary>
        //============================================================================
        protected override void getFldElemList()
        {
            TInitValue newMember;
            XmlNode memberNode;
            string buf;

            //builds the child list using the parent's parser and just shifts the
            //parser's topElement domnode respectively.
            memberNode = parser.firstMember(parser.rootNode());
            while (memberNode != null)
            {
                newMember = new TInitValue(parser, memberNode, "");
                buf = parser.getAttrValue(memberNode, "descr");
                newMember.setDescr(buf, 512);
                FMembers.Add(newMember);    //add to the list of children

                memberNode = parser.nextMember(memberNode);
            }

            if (FIsArray && (FBaseType != TTypedValue.TBaseType.ITYPE_DEF))
                makeDefaultAndRange();

        }

        //============================================================================
        /// <summary>
        /// Gets the value of this XML element and stores it in the matching typed value.
        /// </summary>
        /// <param name="sAttrName">Name of the element.</param>
        /// <param name="aValue">The typed value to be set with the value found.</param>
        //============================================================================
        protected void parseRangeValue(String sAttrName, TTypedValue aValue)
        {
            XmlNode anode;
            String sAttrValue;

            if (parser != null)
            {
                anode = parser.firstElementChild(parser.rootNode(), sAttrName);
                if (anode != null)
                {
                    sAttrValue = parser.getText(anode);
                    aValue.setValue(sAttrValue);
                }
            }
        }

        //============================================================================
        /// <summary>
        /// Set the default value and range based on the scalar type.
        /// </summary>
        //============================================================================
        protected void makeDefaultAndRange()
        {
            if (FBaseType != TTypedValue.TBaseType.ITYPE_DEF)
            {                         // Default value is automatically set
                FDefault = new TDDMLValue("default", FBaseType);   //   to zero/FALSE/null string
                parseRangeValue("defval", FDefault);
            }

            if ((FBaseType < TTypedValue.TBaseType.ITYPE_INT1) || (FBaseType > TTypedValue.TBaseType.ITYPE_DOUBLE))
            {  //Only numeric types have max & min values
                FMax = null;
                FMin = null;
            }
            else
            {
                FMax = new TDDMLValue("maxval", FBaseType);
                FMin = new TDDMLValue("minval", FBaseType);
                switch (FBaseType)
                {
                    case TTypedValue.TBaseType.ITYPE_INT1:
                        {
                            FMax.setValue(+VERYLARGE_I1);
                            FMin.setValue(-VERYLARGE_I1);
                        } break;
                    case TTypedValue.TBaseType.ITYPE_INT2:
                        {
                            FMax.setValue(+VERYLARGE_I2);
                            FMin.setValue(-VERYLARGE_I2);
                        } break;
                    case TTypedValue.TBaseType.ITYPE_INT4:
                        {
                            FMax.setValue(+VERYLARGE_I4);
                            FMin.setValue(-VERYLARGE_I4);
                        } break;
                    case TTypedValue.TBaseType.ITYPE_INT8:
                        {
                            FMax.setValue(+VERYLARGE_I8);
                            FMin.setValue(-VERYLARGE_I8);
                        } break;
                    case TTypedValue.TBaseType.ITYPE_SINGLE:
                        {
                            FMax.setValue(+1.0 * VERYLARGE_S);
                            FMin.setValue(-1.0 * VERYLARGE_S);
                        } break;
                    case TTypedValue.TBaseType.ITYPE_DOUBLE:
                        {
                            FMax.setValue(VERYLARGE_D_POS);
                            FMin.setValue(VERYLARGE_D_NEG);
                        } break;
                }
                parseRangeValue("maxval", FMax);
                parseRangeValue("minval", FMin);
            }

        }

        //============================================================================
        /// <summary>
        /// Gets the typed value as an XML description. 
        /// </summary>
        /// <param name="value">The typed value to describe as xml.</param>
        /// <param name="startIndent">Formatting indentation start.</param>
        /// <param name="tab">Number of spaces in each tab</param>
        /// <returns>The string with the XML result.</returns>
        //============================================================================
        public override String getText(TTypedValue value, int startIndent, int tab)
        {
            int nextIndent;
            String sIndent;

            if (startIndent > -1)
            {
                nextIndent = startIndent + tab;
                sIndent = new String(' ', startIndent);
            }
            else
            {
                nextIndent = -1;
                sIndent = "";
            }

            StringBuilder sbuf = new StringBuilder(sIndent);
            sbuf.Append("<type name=\"");
            sbuf.Append(value.Name);
            sbuf.Append("\" ");
            sbuf.Append(sIndent);
            sbuf.Append(writeFieldInfo(value, nextIndent, tab));
            sbuf.Append(sIndent);
            sbuf.Append("</type>");

            return sbuf.ToString();
        }

        //============================================================================
        /// <summary>
        /// Formats the xml text for the 'field' or 'element' description of the typed   
        /// value stored as a TPropertyInfo or TDriverInfo.                              
        /// </summary>
        /// <param name="attrInfo">The TypedValue attribute.</param>
        /// <param name="indent">Indentation in chars.</param>
        /// <param name="tab">Number of spaces in each tab</param>
        /// <returns>XML text for the field</returns>
        //============================================================================
        protected override String writeFieldInfo(TTypedValue attrInfo, int indent, int tab)
        {
            uint i;
            uint iFirst;
            int oneIndent;
            int nextIndent;
            int startIndent;
            String CR = "";

            //determine how much to indent this description
            oneIndent = 0;
            startIndent = 0;
            if (indent > -1)
            {
                oneIndent = tab;
                startIndent = indent;
                CR = "\r\n";
            }
            nextIndent = indent;
            String sIndent = new String(' ', startIndent + oneIndent);

            StringBuilder xml = new StringBuilder("");
            xml.Append(" kind=\"" + attrInfo.typeName() + "\"");
            if (attrInfo.isArray())
                xml.Append(" array=\"T\"");

            if ((attrInfo.units().Length > 0) && (attrInfo.units()[0] != '-'))
                xml.Append(" unit=\"" + attrInfo.units() + "\"");

            xml.Append(">" + CR);

            //if a scalar or array of scalars then write the defval|minval|maxval elements
            if (attrInfo.isScalar() || (attrInfo.isArray() && attrInfo.baseType() == TTypedValue.TBaseType.ITYPE_DEF))
            {
                TInitValue initInfo = new TInitValue(attrInfo);
                if (initInfo != null)
                {
                    if (initInfo.getDefault() != null)
                        xml.Append(sIndent + "<defval>" + initInfo.getDefault().asEscapedString() + "</defval>" + CR);
                    if (initInfo.getMin() != null)
                        xml.Append(sIndent + "<minval>" + initInfo.getMin().asEscapedString() + "</minval>" + CR);
                    if (initInfo.getMax() != null)
                        xml.Append(sIndent + "<maxval>" + initInfo.getMax().asEscapedString() + "</maxval>" + CR);
                }
            }

            //now nest into the fields/elements
            sIndent = new String(' ', startIndent);
            if (!attrInfo.isScalar())
            {
                // Special handling for non-scalar arrays of length 0, to ensure type definition is output
                iFirst = (uint)((attrInfo.isArray() && attrInfo.count() == 0) ? 0 : 1);
                for (i = iFirst; i <= attrInfo.count(); i++)
                {  //for each child field
                    if (attrInfo.isArray() && attrInfo.baseType() == TTypedValue.TBaseType.ITYPE_DEF)
                    {
                        xml.Append(sIndent + "<element");
                        xml.Append(writeFieldInfo(attrInfo.item(i), nextIndent, oneIndent));
                        xml.Append(sIndent + "</element>" + CR);
                    }
                    else if (attrInfo.isRecord())
                    {
                        xml.Append(sIndent + "<field name=\"" + attrInfo.item(i).Name + "\"");
                        xml.Append(writeFieldInfo(attrInfo.item(i), nextIndent, oneIndent));
                        xml.Append(sIndent + "</field>" + CR);
                    }
                }
            }

            return xml.ToString();
        }

        //============================================================================
        /// <summary>
        /// Uses the copy constructor to make a clone of a typedvalue's structure.
        /// It is then added as a member to an array or record.
        /// this virtual function is expected to be overriden so that new members are
        /// of the child classes' type.
        /// </summary>
        /// <param name="bluePrintValue">Use this TTYpedValue as the blue print.</param>
        //============================================================================
        public override void newMember(TTypedValue bluePrintValue)
        {
            TInitValue newElement;

            newElement = new TInitValue(bluePrintValue); //calls copy constructor
            addMember(newElement);  //add the copy

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
            TInitValue newScalar;

            TTypedValue result = null;

            if (FIsArray || FIsRecord)
            {
                newScalar = new TInitValue(sName, aType);

                if (isArray())
                {
                    // Arrays pass their default value and valid range down to their elements
                    if (newScalar.getDefault() != null)
                        newScalar.getDefault().copyFrom(FDefault);
                    if (newScalar.getMin() != null)
                        newScalar.getMin().copyFrom(FMin);
                    if (newScalar.getMax() != null)
                        newScalar.getMax().copyFrom(FMax);
                }

                addMember(newScalar);
                result = newScalar;
            }
            return result;
        }

        //============================================================================
        /// <summary>
        /// Get the maximum value.
        /// </summary>
        /// <returns>Ref to FMax.</returns>
        //============================================================================
        public TTypedValue getMax()
        {
            return FMax;
        }

        //============================================================================
        /// <summary>
        /// Get the minimum value.
        /// </summary>
        /// <returns>Ref to FMin.</returns>
        //============================================================================
        public TTypedValue getMin()
        {
            return FMin;
        }

        //============================================================================
        /// <summary>
        /// Get the default value.
        /// </summary>
        /// <returns>Ref to FDefault</returns>
        //============================================================================
        public TTypedValue getDefault()
        {
            return FDefault;
        }

        //============================================================================
        /// <summary>
        /// Set the default value.
        /// </summary>
        /// <param name="aValue">Scalar value to use.</param>
        /// <returns>True if successful.</returns>
        //============================================================================
        public bool setDefault(int aValue)
        {
            Boolean result = false;

            try
            {
                if (FDefault != null)
                {
                    FDefault.setValue(aValue);
                    result = true;
                }
            }
            catch (TypeMisMatchException)
            {
                result = false;
            }

            return result;
        }

        //============================================================================
        /// <summary>
        /// Set the default value.
        /// </summary>
        /// <param name="aValue">Scalar value to use.</param>
        /// <returns>True if successful.</returns>
        //============================================================================
        public bool setDefault(double aValue)
        {
            Boolean result = false;

            try
            {
                if (FDefault != null)
                {
                    FDefault.setValue(aValue);
                    result = true;
                }
            }
            catch (TypeMisMatchException)
            {
                result = false;
            }

            return result;

        }

        //============================================================================
        /// <summary>
        /// Set the default value.
        /// </summary>
        /// <param name="aValue">New value.</param>
        /// <returns>True if successful.</returns>
        //============================================================================
        public bool setDefault(bool aValue)
        {
            Boolean result = false;

            try
            {
                if (FDefault != null)
                {
                    FDefault.setValue(aValue);
                    result = true;
                }
            }
            catch (TypeMisMatchException)
            {
                result = false;
            }

            return result;

        }

        //============================================================================
        /// <summary>
        /// Set the default value.
        /// </summary>
        /// <param name="aValue">New string value.</param>
        /// <returns>True if successful.</returns>
        //============================================================================
        public bool setDefault(String aValue)
        {
            Boolean result = false;

            try
            {
                if (FDefault != null)
                {
                    FDefault.setValue(aValue);
                    result = true;
                }
            }
            catch (TypeMisMatchException)
            {
                result = false;
            }

            return result;

        }

        //============================================================================
        /// <summary>
        /// Is current value the default?
        /// </summary>
        /// <returns>True if the current value is the default.</returns>
        //============================================================================
        public bool bIsDefault()
        {
            Boolean result;
            if (isScalar())
                result = ((getDefault() != null) && equals(getDefault()));
            else if (isArray())
                result = (count() == 0); // zero-length array is default
            else
            {  // record
                result = true;
                for (uint idx = 1; idx <= count(); ++idx)
                {
                    TInitValue anItem = new TInitValue(item(idx));
                    if (anItem != null)
                        result &= anItem.bIsDefault();
                }
            }
            return result;
        }

        //============================================================================
        /// <summary>
        /// Sets the maximum value.
        /// </summary>
        /// <param name="aValue">New maximum value.</param>
        /// <returns>True if successful.</returns>
        //============================================================================
        public bool setMax(int aValue)
        {
            Boolean result = false;

            try
            {
                if (FMax != null)
                {
                    FMax.setValue(aValue);
                    result = true;
                }
            }
            catch (TypeMisMatchException)
            {
                result = false;
            }

            return result;

        }

        //============================================================================
        /// <summary>
        /// Set the maximum value.
        /// </summary>
        /// <param name="aValue">New maximum value.</param>
        /// <returns>True if successful.</returns>
        //============================================================================
        public bool setMax(double aValue)
        {
            Boolean result = false;

            try
            {
                if (FMax != null)
                {
                    FMax.setValue(aValue);
                    result = true;
                }
            }
            catch (TypeMisMatchException)
            {
                result = false;
            }

            return result;

        }

        //============================================================================
        /// <summary>
        /// Set the minimum value.
        /// </summary>
        /// <param name="aValue">The new minimum value.</param>
        /// <returns>True if successful.</returns>
        // N.Herrmann Apr 2002
        //============================================================================
        public bool setMin(int aValue)
        {
            Boolean result = false;

            try
            {
                if (FMin != null)
                {
                    FMin.setValue(aValue);
                    result = true;
                }
            }
            catch (TypeMisMatchException)
            {
                result = false;
            }

            return result;
        }

        //============================================================================
        /// <summary>
        /// Set the minimum value.
        /// </summary>
        /// <param name="aValue">The new minimum value.</param>
        /// <returns>True if successful.</returns>
        // N.Herrmann Apr 2002
        //============================================================================
        public bool setMin(Double aValue)
        {
            Boolean result = false;

            try
            {
                if (FMin != null)
                {
                    FMin.setValue(aValue);
                    result = true;
                }
            }
            catch (TypeMisMatchException)
            {
                result = false;
            }

            return result;

        }
        //============================================================================
        /// <summary>
        /// Get the description text for this initvalue
        /// </summary>
        /// <returns>The description text.</returns>
        //============================================================================
        public string getDescr()
        {
            return FDescr;
        }
        //============================================================================
        /// <summary>
        /// Store the description for this InitValue
        /// </summary>
        /// <param name="value">The text of the description</param>
        /// <param name="maxSize">Limit the length of the description</param>
        //============================================================================
        public void setDescr(string value, uint maxSize)
        {
            FDescr = value;

            //remove cr lf characters and " chars
            FDescr.Replace('\n', ' ');
            FDescr.Replace('\r', ' ');
            FDescr.Replace("\"", "'");

            //limit the length of the description
            if (FDescr.Length > maxSize)
            {
                FDescr.Remove((int)maxSize - 3);
                FDescr += "...";
            }
        }
    }
}
