using System;
using System.Collections.Generic;
using System.Text;   

namespace CMPServices
{
   //==============================================================================
   /// <summary>
   /// A property item that belongs to a script.
   /// </summary>
   //==============================================================================
    internal class TScriptProperty
    {
        /// <summary>
        /// Name of the property.
        /// </summary>
        public string name;
        /// <summary>
        /// DDML type of the property.
        /// </summary>
        public string ddmlType;     // ddml type spec of this type
        /// <summary>
        /// SDML type of this property.
        /// </summary>
        public string sdmlType;     // the sdml describing this type

        /// <summary>
        /// store (cache) the datablock of this value
        /// </summary>
        public Byte[] datablock;
    }
   //==============================================================================
   /// <summary>
   /// The basic script item details. 
   /// </summary>
   //==============================================================================
    internal class TScriptSpec
    {
        /// <summary>
        /// ID of the script
        /// </summary>
        public string scriptName;
        /// <summary>
        /// Text of the script
        /// </summary>
        public string scriptText;
        /// <summary>
        /// A change has occurred. Reset to false after the scriptText is built.
        /// </summary>
        public bool propertyChanged;
        /// <summary>
        /// Property list.
        /// </summary>
        public List<TScriptProperty> propertyList;
    }
   //==============================================================================
   /// <summary>
   /// This object is responsible for storing and updating the initscript
   /// for a component. May contain more than one script.
   ///
   /// I have employed a caching mechanism for the script so that the user can
   /// decide the most efficient use of this container. The field <i>runningRebuild</i>
   /// is used to set if the script is rebuilt every time a property is added.
   /// Options: Rebuild when every property is added is very useful if the script is
   ///           built, remains static and is accessed many times.
   ///           The non runningBuild is useful if many properties are added and
   ///           few accesses to the script are made. Because the script will only
   ///           be built when it is retrieved.
   /// </summary>
   //==============================================================================
   internal class TInitScriptManager
   {
       private List<TScriptSpec> scriptList;
      /// <summary>
      /// Determines when script texts are updated.
      /// true = always update the script when a property changes
      /// </summary>
      protected bool FRunningRebuild;      
      /// <summary>
      /// Construct a script manager.
      /// </summary>
      public TInitScriptManager(bool runningRebuild)
      {
         FRunningRebuild = runningRebuild;
         scriptList = new List<TScriptSpec>();
      }
      //==============================================================================
      /// <summary>
      /// Case insensitive search of the script list.
      /// </summary>
      /// <param name="sScriptName">Name of the script to search for.</param>
      /// <returns>Index of the script in the scriptList. Returns -1 if not found.</returns>
      // N.Herrmann Feb 2007
      //==============================================================================
      protected int getScriptIndex(string sScriptName) 
      {
         TScriptSpec script;
         int index = -1;

         int i = 0;
         while ( (index < 0) && (i < scriptList.Count) ) 
         {
            script = scriptList[i];
            if (script.scriptName.ToLower().Equals(sScriptName.ToLower()))    //case insensitive
            {     
               index = i;
            }
            else 
            {
               i++;
            }
         }
         return index;
      }

      //==============================================================================
      /// <summary>
      /// Returns the index to the script property from the search (case insensitive).
      /// </summary>
      /// <param name="script">The script to seearch.</param>
      /// <param name="sPropertyName">Property name to find.</param>
      /// <returns>The index in the propertyList array of the property. Returns
      /// -1 if not found</returns>
      // N.Herrmann Feb 2007
      //==============================================================================
      protected int getProperty(TScriptSpec script, string sPropertyName) 
      {
         TScriptProperty prop;
         int index = -1;

         int i = 0;
         while ( (index < 0) && (i < script.propertyList.Count) ) 
         {
            prop = script.propertyList[i];
            if ( prop.name.ToLower().Equals(sPropertyName.ToLower()) )     //case insensitive 
            {
               index = i;
            }
            else 
            {
               i++;
            }
         }
         return index;
      }
      //==============================================================================
      /// <summary>
      /// Build the script from all the properties
      /// </summary>
      /// <param name="sScriptName">Name of the script.</param>
      /// <returns>The lenght in characters of the script.</returns>
      // N.Herrmann Feb 2007
      //==============================================================================
      protected int buildScript(string sScriptName) 
      {
         int length = 0;

         int index = getScriptIndex(sScriptName);
         if (index >= 0) 
         {
            length = buildScript(index);
         }
         return length;
      }
      //==============================================================================
      /// <summary>
      /// Constructs the script and stores it in the scriptText field of the script item.
      /// </summary>
      /// <param name="index">Index of the script in the scriptList.</param>
      /// <returns>Length in characters of the script.</returns>
      // N.Herrmann Feb 2007
      //==============================================================================
      protected int buildScript(int index)
      {
         TScriptProperty prop;
         StringBuilder buf;
         int i;
         int length = 0;

         if (index >= 0) 
         {
            TScriptSpec script = scriptList[index];
            buf = new StringBuilder("<initsection>\n");
            //now for each property
            i = 0;
            while (i < script.propertyList.Count ) 
            {
               prop = script.propertyList[i];
               buf.Append(prop.sdmlType + "\n");
               i++;
            }
            buf.Append("</initsection>");
            script.scriptText = buf.ToString();
            script.propertyChanged = false;
            length = script.scriptText.Length;
            scriptList[index] = script;
         }
         
         return length;
      }
      //==============================================================================
      /// <summary>
      /// Creates a new script container. If one by this name already exists, then
      /// the existing one is removed.
      /// </summary>
      /// <param name="sScriptName">Name of the new script.</param>
      // N.Herrmann Feb 2007
      //==============================================================================
      public void createInitScript(string sScriptName) 
      {
         int index;
         TScriptSpec script;
      
         index = getScriptIndex(sScriptName);
         if (index >= 0) 
         {
            scriptList.RemoveAt(index);
         }
         script = new TScriptSpec();
         script.scriptName = sScriptName;
         script.scriptText = "";
         script.propertyList = new List<TScriptProperty>();
         scriptList.Add(script);
      }
      //==============================================================================
      /// <summary>
      /// Deletes an init script container.
      /// </summary>
      /// <param name="sScriptName">Name of the script to delete.</param>
      /// <returns>Returns the count of scripts remaining in this object.</returns>
      // N.Herrmann Feb 2007
      //==============================================================================
      public int deleteInitScript(string sScriptName) 
      {
         int index = getScriptIndex(sScriptName);
         if (index >= 0) 
         {
            scriptList.RemoveAt(index);
         }
         return scriptList.Count;
      }
      //==============================================================================
      /// <summary>
      /// Get the length of the script in characters. Ensures that the script is
      /// up to date before getting it's length.
      /// </summary>
      /// <param name="sScriptName">Name of the script.</param>
      /// <param name="lLength">Returns the length in characters.</param>
      // N.Herrmann Feb 2007
      //==============================================================================
      public void initScriptLength(string sScriptName, ref int lLength)
      {
         TScriptSpec script;

         lLength = 0;
         int index = getScriptIndex(sScriptName);
         if (index >= 0) 
         {
            if (!FRunningRebuild) 
            {
               script = scriptList[index];
               if (script.propertyChanged == true) 
               {
                  lLength = buildScript(index);
               }
            }
         }
      }
      //==============================================================================
      /// <summary>
      /// Get the text of the init script.
      /// </summary>
      /// <param name="sScriptName">Name of the script.</param>
      /// <param name="sScriptText">Returns the script text.</param>
      // N.Herrmann Feb 2007
      //==============================================================================
      public void textFromInitScript(string sScriptName, ref string sScriptText) 
      {
         TScriptSpec script;

         sScriptText = "";
         int index = getScriptIndex(sScriptName);
         if (index >= 0) 
         {
            if (!FRunningRebuild) 
            {
               script = scriptList[index];
               if (script.propertyChanged == true) 
               {
                  buildScript(index);
               }
               sScriptText = (scriptList[index]).scriptText; //always assume there is space for this string
            }
         }
      }

      //==============================================================================
      /// <summary>
      /// Convert an init script into the relevant properties.
      /// </summary>
      /// <param name="sScriptName">Name of the script.</param>
      /// <param name="sScriptText">XML text of the init script
      /// &lt;initsection&gt;
      ///      &lt;init..... /&gt;
      ///   &lt;/initsection&gt;
      ///</param>
      // N.Herrmann Feb 2007
      //==============================================================================
      public void textToInitScript(string sScriptName, string sScriptText) 
      {
         uint initCount;
         string buf;
         uint i;
         TDDMLValue ddmlVal;
         TSDMLValue sdmlVal;
         Byte [] data;
         TInitParser initSection;

         int index = getScriptIndex(sScriptName);
         if (index < 0) 
         {
            createInitScript(sScriptName);
         }

         scriptList[index].scriptText = sScriptText;   //set the text string for the script
         scriptList[index].propertyList.Clear();     //remove all the properties
         //now get all the sdml properties from the initsection
         initSection = new TInitParser(sScriptText);                //parse the <initsection>
         ddmlVal = new TDDMLValue("<empty/>", "defined");           //used for getting the ddml type
         initCount = initSection.initCount();
         for (i = 1; i <= initCount; i++) 
         {                                                          //for each init value
            buf = initSection.initText(i);                          //the sdml xml
            sdmlVal = new TSDMLValue(buf, "");                      //new sdml type
            data = new Byte[sdmlVal.sizeBytes()];
            sdmlVal.getData(ref data);                              //get the datablock
            //add this property to the script. (a little inneficient here)
            valueToInitScript(sScriptName, sdmlVal.Name, ddmlVal.getText(sdmlVal, 0, 2), data);
         }
      }
   
      //==============================================================================
      /// <summary>
      /// Add the property. If it exists then overwrite it.
      /// </summary>
      /// <param name="sScriptName">Name of the script.</param>
      /// <param name="sPropertyName">Name of the property.</param>
      /// <param name="sTypeDDML">DDML type</param>
      /// <param name="pValueData">Data block.</param>
      // N.Herrmann Feb 2007
      //==============================================================================
      public void valueToInitScript(string sScriptName, string sPropertyName, string sTypeDDML, Byte [] pValueData) 
      {
         TScriptProperty prop;
         TDDMLValue ddmlVal;
         TSDMLValue sdml;
         uint blocksize;
         int i;

         int index = getScriptIndex(sScriptName);
         if (index >= 0) 
         {
            prop = new TScriptProperty();
            
            prop.ddmlType = sTypeDDML;                            //store a copy of the type descr
            prop.name = sPropertyName;
            ddmlVal = new TDDMLValue(sTypeDDML, "");
            ddmlVal.setData(pValueData, pValueData.Length, 0);    //store the ddml value
            sdml = new TSDMLValue("<empty/>", "defined");         //use this object for conversion
            prop.sdmlType = sdml.getText(ddmlVal, 2, 2);          //store the sdml value description
            blocksize = ddmlVal.sizeBytes();                      //now I know the size of the type
            prop.datablock = new Byte[blocksize];                 //allocate mem
            for (i = 0; i < blocksize; i++) 
            {
               prop.datablock[i] = pValueData[i];                 //copy the bytes
            }

            //add/overwrite the property values
            //search firstly for the property item that may exist            
            int propIndex = getProperty(scriptList[index], sPropertyName);
            if (propIndex < 0) 
            {
               (scriptList[index]).propertyList.Add(prop); //and add it to the list
            }
            else 
            {
               (scriptList[index]).propertyList[index] = prop;
            }

            if (FRunningRebuild) 
            {
               buildScript(index);                                //update the scriptText
            }
            else 
            {
               (scriptList[index]).propertyChanged = true;
            }
         }
      }
      //==============================================================================
      /// <summary>
      /// Retrieve the property by name (case insensitive).
      /// </summary>
      /// <param name="sScriptName">Name of script.</param>
      /// <param name="sPropertyName">Name of the property.</param>
      /// <param name="sTypeDDML">Returned DDML type string.</param>
      /// <param name="pValueData">Returned byte array.</param>
      // N.Herrmann Feb 2007
      //==============================================================================
      public void valueFromInitScript(string sScriptName, string sPropertyName, ref string sTypeDDML, ref Byte [] pValueData) 
      {
         int index = getScriptIndex(sScriptName);
         if (index >= 0) 
         {
            int propIndex = getProperty(scriptList[index], sPropertyName);
            if (propIndex >= 0) 
            {
               sTypeDDML = ((scriptList[index]).propertyList[propIndex]).ddmlType;
               pValueData = ((scriptList[index]).propertyList[propIndex]).datablock; //may need resizing ??
            }
         }
      }
	}
}
