using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using System.Collections;

namespace CMPServices
{
    //=========================================================================
    /// <summary>
    /// Interface class for the SDML init section and the user's initialisation dialog
    /// in the CMP component. Allows easy read/write access to the init section. Includes
    /// knowledge of the component's description for when the init section passed here
    /// may be incomplete.
    /// </summary>
    /// <example>
    /// This example shows how you would use this class in an initialisation dialog.
    /// <code>
    /// public void init(string sdml, string compDescr)
    /// {
    ///     EditInfo = new TEditorInfo(compDescr);  //create using the component description
    ///
    ///     EditInfo.setXML(sdml); //init with the SDML init section
    ///     double dVal = 0;
    ///     if (EditInfo.readValue("day_degree", ref dVal))
    ///         textBox1.Text = dVal.ToString();
    ///     
    ///     //add the array data to the grid
    ///     string[] row = new string[2];
    ///     int count = EditInfo.arrayLength("array");
    ///     for (int i = 1; i &lt;= count; i++)
    ///     {
    ///         EditInfo.readElement("array", i, ref dVal);
    ///         row[0] = i.ToString();
    ///         row[1] = dVal.ToString();
    ///         dataGridView1.Rows.Add(row);
    ///     }
    /// }
    /// </code>
    /// </example>
    //=========================================================================
    public class TEditorInfo
    {
        //=========================================================================
        /// <summary>
        /// Internal list of TInitValues that will be used for temporary storage.
        /// </summary>
        //=========================================================================
        private List<TInitValue> FValues;
        //=========================================================================
        /// <summary>
        /// Utility class for interfacing the component description and the 
        /// init section with the component's dialog.
        /// </summary>
        /// <param name="sDescription">Component description.</param>
        //=========================================================================
        public TEditorInfo(string sDescription)
        {
            FValues = new List<TInitValue>();
            InitList(sDescription);
        }
        //=========================================================================
        /// <summary>
        /// Remove all entries from the list.
        /// </summary>
        //=========================================================================
        protected void Clear()
        {
            FValues.Clear();
        }
        //=========================================================================
        /// <summary>
        /// Initialise the internal list of properties from the
        /// component description.
        /// </summary>
        /// <param name="sDescription">Component description</param>
        //=========================================================================
        private void InitList(string sDescription)
        {
            string sProperty;
            Clear();
            TComponentDescrParser DescParser = new TComponentDescrParser(sDescription);
            sProperty = DescParser.firstProperty();
            while (sProperty.Length != 0)
            {
                TCompProperty newProperty = new TCompProperty(sProperty);
                if (newProperty.bInit)
                    addValue(newProperty.InitValue); //add the member property of the newProperty

                sProperty = DescParser.nextProperty();
            }
        }
        //=========================================================================
        /// <summary>
        /// Add a component property to the internal list.
        /// </summary>
        /// <param name="propInit">Property item.</param>
        /// <returns>The count of items in the internal list of properties.</returns>
        //=========================================================================
        protected int addValue(TInitValue propInit)
        {
            FValues.Add(propInit);
            return FValues.Count;
        }
        //=========================================================================
        /// <summary>
        /// Find the init property and return the typedvalue.
        /// </summary>
        /// <param name="initName">Name of the init property.</param>
        /// <param name="xValue">The typed value found. null if not found.</param>
        /// <returns>True if found.</returns>
        //=========================================================================
        public bool readValue(string initName, ref TTypedValue xValue)
        {
            TInitValue aValue = null;

            bool result = (findValue(initName, out aValue) >= 0);
            if (result)
                xValue = aValue;

            return result;
        }
        //=========================================================================
        /// <summary>
        /// Find the property and return the Init value.
        /// </summary>
        /// <param name="initName">Name of the init property.</param>
        /// <param name="aValue">The Init value found. null if not found.</param>
        /// <returns>True if found.</returns>
        //=========================================================================
        public bool readValue(string initName, ref TInitValue aValue)
        {
            return (findValue(initName, out aValue) >= 0);
        }
        //=========================================================================
        /// <summary>
        /// Read the property's value.
        /// </summary>
        /// <param name="initName">Name of the init property.</param>
        /// <param name="value">The value found.</param>
        /// <returns>True if found.</returns>
        //=========================================================================
        public bool readValue(string initName, ref double value)
        {
            TInitValue aValue;

            bool result = (findValue(initName, out aValue) >= 0);
            if (result)
                value = aValue.asDouble();
            
            return result;
        }
        //=========================================================================
        /// <summary>
        /// Read the property's value.
        /// </summary>
        /// <param name="initName">Name of the init property.</param>
        /// <param name="value">The value found.</param>
        /// <returns>True if found.</returns>
        //=========================================================================
        public bool readValue(string initName, ref int value)
        {
            TInitValue aValue;

            bool result = (findValue(initName, out aValue) >= 0);
            if (result)
                value = aValue.asInt();

            return result;
        }
        //=========================================================================
        /// <summary>
        /// Read the property's value.
        /// </summary>
        /// <param name="initName">Name of the init property.</param>
        /// <param name="value">The value found.</param>
        /// <returns>True if found.</returns>
        //=========================================================================
        public bool readValue(string initName, ref bool value)
        {
            TInitValue aValue;

            bool result = (findValue(initName, out aValue) >= 0);
            if (result)
                value = aValue.asBool();

            return result;
        }
        //=========================================================================
        /// <summary>
        /// Read the property's value.
        /// </summary>
        /// <param name="initName">Name of the init property.</param>
        /// <param name="value">The value found.</param>
        /// <returns>True if found.</returns>
        //=========================================================================
        public bool readValue(string initName, ref string value)
        {
            TInitValue aValue;

            bool result = (findValue(initName, out aValue) >= 0);
            if (result)
                value = aValue.asStr();

            return result;
        }
        //=========================================================================
        /// <summary>
        /// Reads the double value for the init property.
        /// </summary>
        /// <param name="initName">Name of the init property.</param>
        /// <returns>The scalar value of the property. Returns 0 if not found.</returns>
        //=========================================================================
        public double readDouble(string initName)
        {
            double result = 0.0;
            readValue( initName, ref result );
            return result;
        }
        //=========================================================================
        /// <summary>
        /// Reads the integer value for the init property.
        /// </summary>
        /// <param name="initName">Name of the init property.</param>
        /// <returns>The scalar value of the property. Returns 0 if not found.</returns>
        //=========================================================================
        public int readInteger(string initName)
        {
            int result = 0;
            readValue(initName, ref result);
            return result;
        }
        //=========================================================================
        /// <summary>
        /// Reads the boolean value for the init property.
        /// </summary>
        /// <param name="initName">Name of the init property.</param>
        /// <returns>The scalar value of the property. Returns False if not found.</returns>
        //=========================================================================
        public bool readBoolean(string initName)
        {
            bool result = false;
            readValue(initName, ref result);
            return result;
        }
        //=========================================================================
        /// <summary>
        /// Reads the string value for the init property.
        /// </summary>
        /// <param name="initName">Name of the init property.</param>
        /// <returns>The scalar value of the property. Returns empty string if not found.</returns>
        //=========================================================================
        public string readString(string initName)
        {
            string result = "";
            readValue(initName, ref result);
            return result;
        }
        //=========================================================================
        /// <summary>
        /// Find the property by name.
        /// </summary>
        /// <param name="initName">The name of the init property.</param>
        /// <param name="aValue">The init value found. Returns null if not found.</param>
        /// <returns>The index in the internal property values list.</returns>
        //=========================================================================
        private int findValue(string initName, out TInitValue aValue)
        {
            aValue = null;
            int result = FValues.Count - 1;
            while ((result >= 0) && (aValue == null))
            {
                if (FValues[result].Name == initName)
                    aValue = FValues[result];
                else
                    result--;
            }
            return result;
        }
        //=========================================================================
        /// <summary>
        /// Get the whole init section as &lt;initsection&gt;&lt;/initsection&gt; .
        /// </summary>
        /// <returns>The SDML init section.</returns>
        //=========================================================================
        public string asSDML()
        {
            TSDMLValue sdmlVal = new TSDMLValue("<type/>", "");
            StringBuilder sdml = new StringBuilder();
            sdml.Insert(0, "<initsection>");
            for (int i = 0; i < FValues.Count; i++)
            {
                sdml.Append(sdmlVal.getText(FValues[i], 2, 2));
            }
            sdml.Append("</initsection>");

            return sdml.ToString();
        }
        //=========================================================================
        /// <summary>
        /// Initialise the component's properties from the SDML init section.
        /// </summary>
        /// <param name="sdml">The SDML for the init section.</param>
        //=========================================================================
        public void setXML(string sdml)
        {
            XmlNode anode;

            TSDMLParser parser = new TSDMLParser(sdml);

            anode = parser.firstElementChild(parser.rootNode(), "init");
            while (anode != null)
            {
                TSDMLValue sdmlVal = new TSDMLValue(parser, anode, "");
                writeValue(sdmlVal.Name, sdmlVal);

                anode = parser.nextElementSibling(anode, "init");
            }

        }
        //=========================================================================
        /// <summary>
        /// Write the typed value to the property.
        /// </summary>
        /// <param name="initName">Name of the destination property.</param>
        /// <param name="xValue">The typed value to write.</param>
        //=========================================================================
        public void writeValue(string initName, TTypedValue xValue)
        {
            TInitValue newValue;

            findValue(initName, out newValue);
            if (newValue != null)
                writeData(xValue, newValue);
            else
            {
                newValue = new TInitValue(xValue);
                newValue.Name = initName;
                addValue(newValue);
                writeData(xValue, newValue);
            }
        }
        //=========================================================================
        /// <summary>
        /// Write the Init value to the property.
        /// </summary>
        /// <param name="initName">Name of the destination property.</param>
        /// <param name="aValue">The Init value to write.</param>
        //=========================================================================
        public void writeValue(string initName, ref TInitValue aValue)
        {
            TInitValue newValue;

            findValue(initName, out newValue);
            if (newValue != null)
                newValue.setValue(aValue);
            else
            {
                aValue.Name = initName;
                addValue(aValue);
            }
        }
        //=========================================================================
        /// <summary>
        /// Writes the srcValue to destValue.
        /// </summary>
        /// <param name="srcValue">The source value.</param>
        /// <param name="destValue">The destination item.</param>
        //=========================================================================
        private void writeData(TTypedValue srcValue, TTypedValue destValue)
        {
            uint Idx;

            if (destValue.isScalar())
            {
                if (srcValue != null)
                    destValue.setValue(srcValue);
                else if ((destValue.baseType() >= TTypedValue.TBaseType.ITYPE_INT1) && (destValue.baseType() <= TTypedValue.TBaseType.ITYPE_DOUBLE))
                    destValue.setValue(0);
                else
                    destValue.setValue("");
            }
            else if (destValue.isArray())
            {
                if (srcValue == null)
                    destValue.setElementCount(0);
                else
                {
                    destValue.setElementCount(srcValue.count());
                    for (Idx = 1; Idx <= srcValue.count(); Idx++)
                        writeData(srcValue.item(Idx), destValue.item(Idx));
                }
            }
            else // record
            {
                for (Idx = 1; Idx <= destValue.count(); Idx++)
                    writeData(srcValue.member(destValue.item(Idx).Name), destValue.item(Idx));
            }
        }
        //=========================================================================
        /// <summary>
        /// Write a double to the values list.
        /// </summary>
        /// <param name="initName">Name of the init.</param>
        /// <param name="value">The scalar value.</param>
        //=========================================================================
        public void writeValue(string initName, double value)
        {
            TInitValue newValue;

            findValue(initName, out newValue);
            if (newValue != null)
            {
                newValue.setValue(value);
            }
            else
            {
                newValue = new TInitValue(initName, TTypedValue.TBaseType.ITYPE_DOUBLE);
                newValue.setValue(value);
                addValue(newValue);
            }
        }
        //=========================================================================
        /// <summary>
        /// Write an integer to the values list.
        /// </summary>
        /// <param name="initName">Name of the init.</param>
        /// <param name="value">The scalar value.</param>
        //=========================================================================
        public void writeValue(string initName, int value)
        {
            TInitValue newValue;

            findValue(initName, out newValue);
            if (newValue != null)
            {
                newValue.setValue(value);
            }
            else
            {
                newValue = new TInitValue(initName, TTypedValue.TBaseType.ITYPE_INT4);
                newValue.setValue(value);
                addValue(newValue);
            }
        }
        //=========================================================================
        /// <summary>
        /// Write a boolean to the values list.
        /// </summary>
        /// <param name="initName">Name of the init.</param>
        /// <param name="value">The scalar value.</param>
        //=========================================================================
        public void writeValue(string initName, bool value)
        {
            TInitValue newValue;

            findValue(initName, out newValue);
            if (newValue != null)
            {
                newValue.setValue(value);
            }
            else
            {
                newValue = new TInitValue(initName, TTypedValue.TBaseType.ITYPE_BOOL);
                newValue.setValue(value);
                addValue(newValue);
            }
        }
        //=========================================================================
        /// <summary>
        /// Write a string to the values list.
        /// </summary>
        /// <param name="initName">Name of the init.</param>
        /// <param name="value">The scalar value.</param>
        //=========================================================================
        public void writeValue(string initName, string value)
        {
            TInitValue newValue;

            findValue(initName, out newValue);
            if (newValue != null)
            {
                newValue.setValue(value);
            }
            else
            {
                newValue = new TInitValue(initName, TTypedValue.TBaseType.ITYPE_STR);
                newValue.setValue(value);
                addValue(newValue);
            }
        }
        //=========================================================================
        /// <summary>
        /// Find the element of an array to write to. Resize the array if required.
        /// </summary>
        /// <param name="initName">Name of the init.</param>
        /// <param name="idx">Array item index.</param>
        /// <param name="baseType">The base type of the item.</param>
        /// <param name="elemValue">The typed value of the array item.</param>
        /// <returns>The array item.</returns>
        //=========================================================================
        private void findElementWrite(string initName, int idx, TTypedValue.TBaseType baseType, ref TInitValue elemValue)
        {
            TInitValue aValue = null;

            if (idx < 1)
                throw (new ArrayIndexException("Attempt to write out of range of array value " + initName));

            findValue(initName, out aValue);                                       // Try to locate the named value         

            if (aValue == null)                                                        // First time this array has been        
            {                                                                        //   referred to - create it             
                aValue = new TInitValue(initName, baseType, idx);
                addValue(aValue);
            }

            if (!aValue.isArray())
                throw (new TypeMisMatchException("Attempt to write array to non-array value " + initName));

            if (idx > aValue.count())
                aValue.setElementCount((uint)idx);

            elemValue = (TInitValue)aValue.item((uint)idx);
        }
        //=========================================================================
        /// <summary>
        /// Write to an array at element idx.
        /// </summary>
        /// <param name="initName">Name of the array init.</param>
        /// <param name="idx">Item in the array.</param>
        /// <param name="value">The scalar value.</param>
        //=========================================================================
        public void writeElement(string initName, int idx, double value)
        {
            TInitValue elemValue = null;
            findElementWrite(initName, idx, TTypedValue.TBaseType.ITYPE_DOUBLE, ref elemValue);
            elemValue.setValue( value );
        }
        //=========================================================================
        /// <summary>
        /// Write to an array at element idx.
        /// </summary>
        /// <param name="initName">Name of the array init.</param>
        /// <param name="idx">Item in the array.</param>
        /// <param name="value">The scalar value.</param>
        //=========================================================================
        public void writeElement(string initName, int idx, int value)
        {
            TInitValue elemValue = null;
            findElementWrite(initName, idx, TTypedValue.TBaseType.ITYPE_INT4, ref elemValue);
            elemValue.setValue(value);
        }
        //=========================================================================
        /// <summary>
        /// Write to an array at element idx.
        /// </summary>
        /// <param name="initName">Name of the array init.</param>
        /// <param name="idx">Item in the array.</param>
        /// <param name="value">The scalar value.</param>
        //=========================================================================
        public void writeElement(string initName, int idx, bool value)
        {
            TInitValue elemValue = null;
            findElementWrite(initName, idx, TTypedValue.TBaseType.ITYPE_BOOL, ref elemValue);
            elemValue.setValue(value);
        }
        //=========================================================================
        /// <summary>
        /// Write to an array at element idx.
        /// </summary>
        /// <param name="initName">Name of the array init.</param>
        /// <param name="idx">Item in the array.</param>
        /// <param name="value">The scalar value.</param>
        //=========================================================================
        public void writeElement(string initName, int idx, string value)
        {
            TInitValue elemValue = null;
            findElementWrite(initName, idx, TTypedValue.TBaseType.ITYPE_STR, ref elemValue);
            elemValue.setValue(value);
        }
        //=========================================================================
        /// <summary>
        /// Return the array length of the init property.
        /// </summary>
        /// <param name="initName">Name of the init.</param>
        /// <returns>The array length. Returns 0 if the property is not found.
        /// Throws exception if this is not an array.</returns>
        //=========================================================================
        public int arrayLength(string initName)
        {
            TInitValue aValue;
            int result;

            findValue(initName, out aValue);
            if (aValue == null)
                result = 0;
            else if (aValue.isArray())
                result = (int)aValue.count();
            else
                throw (new TypeMisMatchException("Attempt to read array from non-array value " + initName));

            return result;
        }
        //=========================================================================
        /// <summary>
        /// Find the init property item in the array.
        /// </summary>
        /// <param name="initName">Name of the init array.</param>
        /// <param name="idx">Array item index.</param>
        /// <returns>The typed value found at the index.</returns>
        //=========================================================================
        private TTypedValue findElementRead(string initName, int idx)
        {
            TTypedValue aValue = null;
            TTypedValue result;
            readValue(initName, ref aValue);

            if (aValue == null)
                result = null;
            else
            {
                if (!aValue.isArray())
                    throw (new TypeMisMatchException("Attempt to read array from non-array value " + initName));
                if ((idx < 1) || (idx > aValue.count()))
                    throw (new ArrayIndexException("Attempt to read out of range of array value " + initName));
                result = aValue.item((uint)idx);
            }
            return result;
        }
        //=========================================================================
        /// <summary>
        /// Read the scalar value in the array at the specified index.
        /// </summary>
        /// <param name="initName">Name of the array init property.</param>
        /// <param name="idx">Item index in the array.</param>
        /// <param name="value">The scalar value found at the index. Returns 0 if not found.</param>
        //=========================================================================
        public void readElement(string initName, int idx, ref double value)
        {
            TTypedValue aValue = null;

            aValue = findElementRead(initName, idx);
            if (aValue != null)
                value = aValue.asDouble();
            else
                value = 0.0;
        }
        //=========================================================================
        /// <summary>
        /// Read the scalar value in the array at the specified index.
        /// </summary>
        /// <param name="initName">Name of the array init property.</param>
        /// <param name="idx">Item index in the array.</param>
        /// <param name="value">The scalar value found at the index. Returns 0 if not found.</param>
        //=========================================================================
        public void readElement(string initName, int idx, ref int value)
        {
            TTypedValue aValue = null;

            aValue = findElementRead(initName, idx);
            if (aValue != null)
                value = aValue.asInt();
            else
                value = 0;
        }
        //=========================================================================
        /// <summary>
        /// Read the scalar value in the array at the specified index.
        /// </summary>
        /// <param name="initName">Name of the array init property.</param>
        /// <param name="idx">Item index in the array.</param>
        /// <param name="value">The value found at the index. Returns false if not found.</param>
        //=========================================================================
        public void readElement(string initName, int idx, ref bool value)
        {
            TTypedValue aValue = null;

            aValue = findElementRead(initName, idx);
            if (aValue != null)
                value = aValue.asBool();
            else
                value = false;
        }
        //=========================================================================
        /// <summary>
        /// Read the scalar value in the array at the specified index.
        /// </summary>
        /// <param name="initName">Name of the array init property.</param>
        /// <param name="idx">Item index in the array.</param>
        /// <param name="value">The scalar value found at the index. Returns empty string if not found.</param>
        //=========================================================================
        public void readElement(string initName, int idx, ref string value)
        {
            TTypedValue aValue = null;

            aValue = findElementRead(initName, idx);
            if (aValue != null)
                value = aValue.asStr();
            else
                value = "";
        }

    }
}
