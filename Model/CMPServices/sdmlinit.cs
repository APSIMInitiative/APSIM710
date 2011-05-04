using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;

namespace CMPServices
{
    //============================================================================
    /// <summary>
    /// Parses and contains the initialisation section of a component.
    /// Initialised with this xml:
    /// <pre>&lt;initsection&gt;
    ///   &lt;init name="sequence" array="T"&gt;
    ///   ...
    ///   &lt;/init&gt;
    /// &lt;/initsection&gt;</pre>
    /// </summary>
    //============================================================================
    public class TSDMLCompInit : TXMLParser
    {
        /// <summary>
        /// Name of init element in xml code
        /// </summary>
        protected string initElement;        
        //============================================================================
        /// <summary>
        /// Count the number of inits.
        /// </summary>
        //============================================================================
        protected void getInits()
        {
            XmlNode anode;

            FInitCount = 0;

            anode = firstElementChild(rootNode(), initElement);
            while (anode != null)
            {
                FInitCount++;
                anode = nextElementSibling(anode, initElement);
            }
        }
        /// <summary>
        /// Number of initialisation values
        /// </summary>
        protected int FInitCount;            
        //============================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="xml"></param>
        //============================================================================
        public TSDMLCompInit(string xml) :
            base (xml)
        {
            initElement = "init";
            getInits();             //count the number of inits
        }
        //============================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <returns>Returns the count of init items.</returns>
        //============================================================================
        public int initCount()
        {
            return FInitCount;
        }
        //============================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="idx">1-x</param>
        /// <returns>Returns the name value of the TSDMLValue.</returns>
        //============================================================================
        public string initName(uint idx)
        {
            XmlNode anode;
            uint count = 0;

            string name = "";
            anode = firstElementChild(rootNode(), initElement);
            while ((idx > count) && (anode != null))
            {
                count++;                                              //an init found
                if (count == idx)
                {                                   //if this is the index to be found
                    name = getAttrValue(anode, "name");
                }
                else
                    anode = nextElementSibling(anode, initElement);
            }
            return name;
        }
        //============================================================================
        /// <summary>
        /// Returns the init by index. Format: &lt;init&gt; ... &lt;/init&gt;
        /// </summary>
        /// <param name="idx">Index of the init to return. 1 -> x</param>
        /// <returns></returns>
        //============================================================================
        string initText(uint idx)
        {
            XmlNode anode;

            string buf = "";
            uint count = 0;
            anode = firstElementChild(rootNode(), initElement);
            while ((idx > count) && (anode != null))
            {
                count++;
                if (count == idx)
                {
                    buf = docToString(anode);
                }
                else
                    anode = nextElementSibling(anode, initElement);
            }

            return buf;
        }
        //============================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="initName">Name of the init property.</param>
        /// <returns>The string containing the init</returns>
        //============================================================================
        public string initTextByName(string initName)
        {
            bool found = false;
            XmlNode anode;

            string buf = "";
            anode = firstElementChild(rootNode(), initElement);
            while (!found && (anode != null) )
            {
                if (getAttrValue(anode, "name") == initName)
                {
                    found = true;
                    buf = docToString(anode);
                }
                else
                    anode = nextElementSibling(anode, initElement);
            }
            return buf;
        }

        /* names of the special SDML arrays (inits) found in the <initsection> of a component */
        /// <summary>
        /// Name of the published event array
        /// </summary>
        /// <returns></returns>
        public string pubEventArrayName()
        {
            return "published_events";
        }
        /// <summary>
        /// Name of the subscribed event array
        /// </summary>
        /// <returns></returns>
        public string subEventArrayName()
        {
            return "subscribed_events";
        }
        /// <summary>
        /// Name of the driver connection array
        /// </summary>
        /// <returns></returns>
        public string driverArrayName()
        {
            return "driver_connections";
        }
    }
}
