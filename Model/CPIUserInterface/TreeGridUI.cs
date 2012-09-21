using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;
using System.Xml;
using System.IO;

using Controllers;
using CSGeneral;
using CMPServices;
using ApsimFile;
namespace CPIUserInterface
{
    //=========================================================================
    /// <summary>
    /// Used as a generic interface for CPI components. Either native code or managed code.
    /// This TreeGridUI class is used to show a hierarchical tree grid view of the component's
    /// initialisation section. When possible, the component's description is used to populate the 
    /// grid and the component initial values then fill the values.
    /// </summary>
    //=========================================================================
    public partial class TreeGridUI : CPIBaseView
    {
        private List<TTypedValue> typedvals;        //list of properties from the init section
        //=====================================================================
        /// <summary>
        /// A tree grid UI that allows the initialisation of a list of 
        /// SDML values as found in the initsection of a CPI component.
        /// </summary>
        //=====================================================================
        public TreeGridUI() : base ()
        {
            InitializeComponent();
            typedvals = new List<TTypedValue>();
            base.HelpText = "Initialisation values"; //triggers TreeGridUI_Load()
        }
        //=====================================================================
        /// <summary>
        /// Saves the current property values to Data.
        /// </summary>
        //=====================================================================
        public override void OnSave()
        {
            StoreControls();
            String newXML = WriteInitsectionXml();

            //now store the new xml by replacing the old xmlnode in Data
            XmlNode initSection = XmlHelper.Find(Data, "initsection");
            if (initSection != null)
                Data.RemoveChild(initSection);
            XmlDocument doc = new XmlDocument();
            doc.LoadXml(newXML);
            Data.AppendChild(Data.OwnerDocument.ImportNode(doc.DocumentElement, true));
        }
        //=====================================================================
        /// <summary>
        /// Write the TTypedValues to an xml string.
        /// </summary>
        /// <returns>The init section string</returns>
        //=====================================================================
        protected override String WriteInitsectionXml()
        {
            StringBuilder newXML = new StringBuilder();
            newXML.Append("<initsection>");

            TSDMLValue sdmlWriter = new TSDMLValue("<init/>", "");

            if (propertyList.Count > 0)             //if using the full component description
            {
                for (int i = 0; i < propertyList.Count; i++)
                {
                    if (propertyList[i].bInit == true)
                        newXML.Append(sdmlWriter.getText(propertyList[i].InitValue, 0, 2));
                }
            }
            else
            {
                for (int i = 0; i < typedvals.Count; i++)
                {
                    newXML.Append(sdmlWriter.getText(typedvals[i], 0, 2));
                }
            }

            newXML.Append("</initsection>");
            return newXML.ToString();
        }
        //=====================================================================
        /// <summary>
        /// Store the forms controls into the list of TTypedValues.
        /// </summary>
        //=====================================================================
        private void StoreControls()
        {
            //in this case the tree of init values is stored from within the control
            //code is only required here for additional controls or validation
        }
        public override void OnRefresh()
        {
            base.OnRefresh();
        }
        //=====================================================================
        /// <summary>
        /// When the UI is created and loaded
        /// </summary>
        //=====================================================================
        protected override void OnLoad()
        {
            //fill the propertyList with init properties
            if (InitFromComponentDescription())
            {
                ReadInitSection();  //reads all the init values and sets the property values

                this.afTreeViewColumns1.reloadTreeEvent += new AFTreeViewColumns.reloadTree(afTreeViewColumns1_reloadTreeEvent);
                this.afTreeViewColumns1.saveChangesEvent += new AFTreeViewColumns.onDataChange(afTreeViewColumns1_saveChangesEvent);

                ListView.ColumnHeaderCollection lvColumns = afTreeViewColumns1.Columns;

                lvColumns.Clear();

                ColumnHeader ch1 = new ColumnHeader();
                ch1.Name = "Variable";
                ch1.Text = "Variable";
                ch1.Width = 150;
                lvColumns.Add(ch1);

                ColumnHeader ch2 = new ColumnHeader();
                ch2.Name = "Value";
                ch2.Text = "Value";
                ch2.Width = 150;
                lvColumns.Add(ch2);

                ColumnHeader ch3 = new ColumnHeader();
                ch3.Name = "Type";
                ch3.Text = "Type";
                ch3.Width = 50;
                lvColumns.Add(ch3);

                ColumnHeader ch4 = new ColumnHeader();
                ch4.Name = "Unit";
                ch4.Text = "Unit";
                ch4.Width = 50;
                lvColumns.Add(ch4);

                ColumnHeader ch5 = new ColumnHeader();
                ch5.Name = "default";
                ch5.Text = "default";
                ch5.Width = 50;
                lvColumns.Add(ch5);

                ColumnHeader ch6 = new ColumnHeader();
                ch6.Name = "min";
                ch6.Text = "min";
                ch6.Width = 50;
                lvColumns.Add(ch6);

                ColumnHeader ch7 = new ColumnHeader();
                ch7.Name = "max";
                ch7.Text = "max";
                ch7.Width = 50;
                lvColumns.Add(ch7);
            }
        }
        //=======================================================================
        /// <summary>
        /// Read the init section from the script and set the property values.
        /// The tree also gets populated.
        /// </summary>
        //=======================================================================
        private void ReadInitSection()
        {
            //Fill the property fields
            XmlNode initSection = XmlHelper.Find(Data, "initsection");
            //get all the init section values
            String initXML = "";
            if (initSection != null)
                initXML = initSection.OuterXml;

            InitFromInitSection(initXML);
        }
        //=======================================================================
        /// <summary>
        /// Initialise the lists of properties with values from the init section
        /// SDML. And reload the tree with the values.
        /// </summary>
        /// <param name="initXML">The init section which is <code><initsection>...</initsection></code></param>
        //=======================================================================
        private Boolean InitFromInitSection(String initXML)
        {
            if (initXML.Length > 0)
            {
                typedvals.Clear();
                TInitParser initPsr = new TInitParser(initXML);

                for (int i = 1; i <= initPsr.initCount(); i++)
                {
                    String initText = initPsr.initText((uint)i);
                    TSDMLValue sdmlinit = new TSDMLValue(initText, "");
                    typedvals.Add(sdmlinit);
                }
            }
            //if the component description is valid then use it.
            if (propertyList.Count > 0)
            {
                foreach (TTypedValue value in typedvals)    //for every init section value
                {
                    //find the property in the component description list
                    int i = 0;
                    TCompProperty prop = propertyList[i];
                    while ((prop != null) && (i < propertyList.Count))
                    {
                        if (value.Name.ToLower() == prop.Name.ToLower())
                        {
                            prop.InitValue.setValue(value); //set the property's value
                        }
                        i++;
                        if (i < propertyList.Count)
                            prop = propertyList[i];
                    }
                }
            }
            if (propertyList.Count > 0)
                populateTreeModel(propertyList);            //can populate with the full list
            else
                populateTreeModel();                        //use init section only

            return (initXML.Length > 0) || (propertyList.Count > 0);
        }
        //=======================================================================
        /// <summary>
        /// CPI components also include some properties that may not need to be shown to the user.
        /// </summary>
        /// <param name="propName">Name of the property</param>
        /// <returns>True if the property should be shown.</returns>
        //=======================================================================
        private Boolean makePropertyVisible(String propName)
        {
            Boolean bPropVisible = true;
            //check if this variable should be hidden
            if ((propName == STRSUBEVENT_ARRAY))
                bPropVisible = false;
            if ((propName == STRPUBEVENT_ARRAY))
                bPropVisible = false;
            if ((propName == STRDRIVER_ARRAY))
                bPropVisible = false;

            return bPropVisible;
        }
        //=======================================================================
        /// <summary>
        /// Populate the tree based on the list of typed values found only in
        /// the init section for the component.
        /// </summary>
        private void populateTreeModel()
        {
            afTreeViewColumns1.SuspendLayout();
            afTreeViewColumns1.TreeView.Nodes.Clear();

            foreach (TTypedValue prop in typedvals)
            {
                if (makePropertyVisible(prop.Name) == true)
                {
                    TreeNode trNode2 = new TreeNode();
                    afTreeViewColumns1.TreeView.Nodes.Add(trNode2);
                    addTreeModelNode(trNode2, prop.Name, prop);
                }
            }
            afTreeViewColumns1.ResumeLayout();
        }
        //=======================================================================
        /// <summary>
        /// Populate the tree based on the TCompProperty list gained from the
        /// component description.
        /// </summary>
        private void populateTreeModel(List<TCompProperty> compProperties)
        {
            afTreeViewColumns1.SuspendLayout();
            afTreeViewColumns1.TreeView.Nodes.Clear();

            foreach (TCompProperty prop in compProperties)
            {
                if (prop.bInit == true)
                {
                    if (makePropertyVisible(prop.Name) == true)
                    {
                        TreeNode trNode2 = new TreeNode();
                        afTreeViewColumns1.TreeView.Nodes.Add(trNode2);
                        addTreeModelNode(trNode2, prop.InitValue.Name, prop.InitValue);
                    }
                }
            }
            afTreeViewColumns1.ResumeLayout();
        }
        //=======================================================================
        private void addTreeModelNode(TreeNode parentNode, String name, TTypedValue typedValue)
        {
            uint i = 1;
            uint j = 1;

            parentNode.Name = name;
            parentNode.Text = name;
            parentNode.Tag = new TAFTreeViewColumnTag(typedValue);

            if ((typedValue.isArray()) || (typedValue.isRecord()))
            {
                uint iCount = typedValue.count();
                while (i <= iCount)
                {
                    TTypedValue typedValueChild = typedValue.item(i);

                    if (typedValueChild != null)
                    {
                        TreeNode trNode2 = new TreeNode();
                        parentNode.Nodes.Add(trNode2);
                        string sVarName = j.ToString();
                        if (typedValue.isArray())
                            sVarName = "[" + sVarName + "]";
                        j++;
                        if (typedValueChild.Name.Length > 0)
                        {
                            sVarName = typedValueChild.Name;
                        }
                        addTreeModelNode(trNode2, sVarName, typedValueChild);
                    }
                    i++;
                }
            }
        }
        //=======================================================================
        private void afTreeViewColumns1_saveChangesEvent()
        {
            this.afTreeViewColumns1.Focus();
        }
        //=======================================================================
        /// <summary>
        /// Called from the tree when arrays are resized.
        /// Using the selected node, just recreate it's child nodes.
        /// </summary>
        //=======================================================================
        private void afTreeViewColumns1_reloadTreeEvent()
        {
            if (afTreeViewColumns1.TreeView.SelectedNode != null)
            {
                TAFTreeViewColumnTag changedValue = (TAFTreeViewColumnTag)afTreeViewColumns1.TreeView.SelectedNode.Tag;
                afTreeViewColumns1.TreeView.BeginUpdate();
                afTreeViewColumns1.TreeView.SelectedNode.Nodes.Clear();
                addTreeModelNode(afTreeViewColumns1.TreeView.SelectedNode, changedValue.TypedValue.Name, changedValue.TypedValue);
                afTreeViewColumns1.TreeView.SelectedNode.Expand();
                afTreeViewColumns1.TreeView.EndUpdate();
            }
        }
        //=========================================================================
        /// <summary>
        /// Opens the component's initialising dialog.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        //=========================================================================
        private void button1_Click(object sender, EventArgs e)
        {
            String sInitSDML = WriteInitsectionXml(); //with no subscribed events in the SDML
            if (Initialise(FDllFileName, ref sInitSDML))
                InitFromInitSection(sInitSDML);
        }
        //=========================================================================
        /// <summary>
        /// Open help file based on the dll component name.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        //=========================================================================
        private void button2_Click(object sender, EventArgs e)
        {
            String helpFileName = Path.ChangeExtension(FDllFileName, "chm");
            if (File.Exists(helpFileName))
            {
                openHelp(helpFileName);
            }
            else {
                helpFileName = Path.ChangeExtension(FDllFileName, "html");
                if (File.Exists(helpFileName))
                {
                    openHelp(helpFileName);
                }
                else
                    MessageBox.Show("Cannot find help file " + helpFileName);
            }
        }
	} 
}

    