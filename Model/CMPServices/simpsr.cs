using System;
using System.Xml;

namespace CMPServices
{
    //===========================================================================
    /// <summary>
    /// Parser for parsing a simulation XML document. A simulation document 
    /// follows the same structure as a system component so this is a 
    /// specialised TCompParser class.
    /// Simulation descriptions follow this structure:<br/>
    /// &lt;documentroot&gt;<br/>
    ///   &lt;sdmlversion&gt;&lt;sdmlversion&gt;<br/>
    ///   &lt;component&gt; &lt;/component&gt;<br/>
    ///   &lt;system&gt;<br/>
    ///     &lt;component&gt; &lt;/component&gt;<br/>
    ///   &lt;/system&gt;<br/>
    /// &lt;/documentroot&gt;<br/>	
    /// </summary>
    //===========================================================================
    public class TSimulationParser : TCompParser
    {
        //===========================================================================
        /// <summary>
        /// Create a simulation parser using the XML document.
        /// </summary>
        /// <param name="sXml">SDML simulation in XML</param>
        //===========================================================================
        public TSimulationParser(String sXml)
            : base(sXml)
        {
            getDescription();
        }
        //=======================================================================
        /// <summary>
        /// Get the descriptive fields (name and sdmlversion).
        /// </summary>
        //=======================================================================
        private void getDescription()
        {
            XmlNode anode;

            anode = topElement;
            if (anode != null)
            {
                FName = getAttrValue(anode, "name");
                anode = firstElementChild(topElement, "sdmlversion");
                if (anode != null)
                    FVersion = getText(anode);
            }
        }

    }
}
