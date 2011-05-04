using System;
using System.Xml;
using CMPServices;

namespace CMPServices
{
	/// <summary>
	/// Parses the component description returned from a component getDescription() function.
	/// </summary>
	public class TComponentDescrParser : TXMLParser
	{
       private String FClassname;
       private String FExecutable;
       private String FVersion;
       private String FAuthor;
       private Boolean FIsSystem;
       //=======================================================================
       /// <summary>
       /// Read the description from the XML file.
       /// </summary>
       //=======================================================================
       protected void getDescription()
       {
           XmlNode anode;

           anode = firstElementChild(topElement, "executable");
           if (anode != null)
               FExecutable = getText(anode);

           anode = firstElementChild(topElement, "class");
           if (anode != null)
               FClassname = getText(anode);

           anode = firstElementChild(topElement, "version");
           if (anode != null)
               FVersion = getText(anode);

           anode = firstElementChild(topElement, "author");
           if (anode != null)
               FAuthor = getText(anode);

           anode = firstElementChild(topElement, "system");
           if (anode != null)
               FIsSystem = true;   //this component can be a system
           else
               FIsSystem = false;
       }
       //=======================================================================
       /// <summary>
       /// Creates a parser from an XML string.
       /// </summary>
       /// <param name="sdml">The SDML component description.</param>
       //=======================================================================
		 public TComponentDescrParser(String sdml) : base (sdml)
		 {
           getDescription();
		 }
       //=======================================================================
       // Properties
       //=======================================================================
       /// <summary>
       /// Class name of the component.
       /// </summary>
       public String ClassName
       {
           get { return FClassname; }
       }
       /// <summary>
       /// Full path to the component dll.
       /// </summary>
       public String Executable
       {
           get { return FExecutable; }
       }
       /// <summary>
       /// Version number of the component.
       /// </summary>
       public String Version
       {
           get { return FVersion; }
       }
       /// <summary>
       /// Author name.
       /// </summary>
       public String Author
       {
           get { return FAuthor; }
       }
       /// <summary>
       /// True if it can be a system.
       /// </summary>
       public Boolean isSystem
       {
           get { return FIsSystem; }
       }
       //=======================================================================
       /// <summary>
       /// Find the first property. Sets the currNode.
       /// </summary>
       /// <returns>The property SDML.</returns>
       //=======================================================================
       public String firstProperty()
       {
           XmlNode anode;
           String result = "";

           anode = firstElementChild(topElement, "property");
           if (anode != null)
               result = docToString(anode);

           return result;
       }
       //=======================================================================
       /// <summary>
       /// Gets the next property. Relies on this being called immediately after a call
       /// firstProperty() which sets the currNode. If something else calls a method
       /// which sets the currNode between calls, the outcome is unpredictable.
       /// </summary>
       /// <returns>The SDML description of this property.</returns>
       // May be better to have a class ptr to a property node. Or use some indexing.
       //=======================================================================
       public String nextProperty()
       {
           XmlNode anode;
           String result = "";

           anode = nextElementSibling(currNode, "property");
           if (anode != null)
               result = docToString(anode);

           return result;
       }
       //=======================================================================
       /// <summary>
       /// Find the first driving property. Sets the currNode.
       /// </summary>
       /// <returns>The SDML description of the driver.</returns>
       //=======================================================================
       public String firstDriver()
       {
           XmlNode anode;
           String result = "";

           anode = firstElementChild(topElement, "driver");
           if (anode != null)
               result = docToString(anode);

           return result;
       }
       //=======================================================================
       /// <summary>
       /// Find the next driver from currNode.
       /// </summary>
       /// <returns>The SDML description of the driver.</returns>
       //=======================================================================
       public String nextDriver()
       {
           XmlNode anode;
           String result = "";

           anode = nextElementSibling(currNode, "driver");
           if (anode != null)
               result = docToString(anode);

           return result;
       }
       //=======================================================================
       /// <summary>
       /// Find the first event. Sets currNode.
       /// </summary>
       /// <returns>The SDML description of the event.</returns>
       //=======================================================================
       public String firstEvent()
       {
           XmlNode anode;
           String result = "";

           anode = firstElementChild(topElement, "event");
           if (anode != null)
               result = docToString(anode);

           return result;
       }
       //=======================================================================
       /// <summary>
       /// Find the next event from currNode.
       /// </summary>
       /// <returns>The SDML description of the event.</returns>
       //=======================================================================
       public String nextEvent()
       {
           XmlNode anode;
           String result = "";

           anode = nextElementSibling(currNode, "event");
           if (anode != null)
               result = docToString(anode);

           return result;
       }

	}
}
