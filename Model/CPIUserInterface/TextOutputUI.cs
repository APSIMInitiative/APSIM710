using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Data;
using System.Globalization;
using System.IO;
using System.Text;
using System.Windows.Forms;
using System.Xml;
using ApsimFile;
using CMPServices;
using Controllers;
using CSGeneral;
using Graph;

namespace CPIUserInterface
{
    public partial class TextOutputUI : CPIBaseView
    {
        private const int OUTPUTTAB = 0;
        private StringCollection ComponentNames = new StringCollection();
        private StringCollection ComponentTypes = new StringCollection();
        private List<TTypedValue> typedvals;
        private string FileName;

        public delegate void ColumnClickEvent(string ColumnName);
        public event ColumnClickEvent OnColumnClickEvent;
        ////====================================================================
        /// <summary>
        /// This UI is designed for the TextOutput component. Specifically the
        /// .net version. It creates files in apsim format so the charting
        /// system can use them.
        /// N.Herrmann May 2012
        /// </summary>
        ////====================================================================
        public TextOutputUI()
        {
            InitializeComponent();

            //some standard date formats
            String[] formats = new String[5]{ "dd/MM/yyyy",
                                        "dd/MMM/yyyy",
                                        "dd/MM/yyyy",
                                        "d/M/yy",
                                        "yyyy/MM/dd"};
            typedvals = new List<TTypedValue>();
            comboBox1.SelectedIndex = 3;

            //ensure that the current sep is used so APSIMInputFile copes nicely with reading
            CultureInfo ci = CultureInfo.CurrentCulture;
            String delim = ci.DateTimeFormat.DateSeparator;
            comboBox2.Items.Clear();
            for (int i = 0; i < formats.Length; i++)
            {
                formats[i] = formats[i].Replace("-", delim);
                formats[i] = formats[i].Replace("/", delim);
                comboBox2.Items.Add(formats[i]);
            }

            //Configure the treeview that contains the variables for a component
            this.afTreeViewColumns1.reloadTreeEvent += new AFTreeViewColumns.reloadTree(afTreeViewColumns1_reloadTreeEvent);
            this.afTreeViewColumns1.saveChangesEvent += new AFTreeViewColumns.onDataChange(afTreeViewColumns1_saveChangesEvent);
            this.afTreeViewColumns1.getColValueEvent += new AFTreeViewColumns.onGetColValue(afTreeViewColumns1_getColVal);

            ListView.ColumnHeaderCollection lvColumns = afTreeViewColumns1.Columns;

            lvColumns.Clear();

            ColumnHeader ch1 = new ColumnHeader();
            ch1.Name = "Variable";
            ch1.Text = "Variable";
            ch1.Width = 150;
            lvColumns.Add(ch1);

            ColumnHeader ch2 = new ColumnHeader();
            ch2.Name = "Type";
            ch2.Text = "Type";
            ch2.Width = 50;
            lvColumns.Add(ch2);

            ColumnHeader ch3 = new ColumnHeader();
            ch3.Name = "Unit";
            ch3.Text = "Unit";
            ch3.Width = 50;
            lvColumns.Add(ch3);

            ColumnHeader ch4 = new ColumnHeader();
            ch4.Name = "Description";
            ch4.Text = "Description";
            ch4.Width = 300;
            lvColumns.Add(ch4);

            afTreeViewColumns1.AllowEdit = false;
        }
        ////====================================================================
        /// <summary>
        /// Load the component details each time it is selected.
        /// </summary>
        ////====================================================================
        protected override void OnLoad()
        {
            InitFromComponentDescription(); //fills the propertyList with init properties
            base.HelpText = " TextOutput";

            ReadInitSection();

        }
        ////====================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        ////====================================================================
        private void TextOutUI_Load(object sender, EventArgs e)
        {
        }
        ////======================================================================
        /// <summary>
        /// Read the init section from the script and set the property values.
        /// The tree also gets populated.
        /// </summary>
        ////======================================================================
        private void ReadInitSection()
        {
            // Fill the property fields
            XmlNode initSection = XmlHelper.Find(Data, "initsection");
            // get all the init section values
            String initXML = "";
            if (initSection != null)
                initXML = initSection.OuterXml;

            InitFromInitSection(initXML);
        }
        ////======================================================================
        /// <summary>
        /// Initialise the lists of properties with values from the init section
        /// SDML. 
        /// </summary>
        /// <param name="initXML">The init section which is <code><initsection>...</initsection></code></param>
        ////======================================================================
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
            // if the component description is valid then use it.
            if (propertyList.Count > 0)
            {
                foreach (TTypedValue value in typedvals)    // for every init section value
                {
                    // find the property in the component description list
                    int i = 0;
                    TCompProperty prop = propertyList[i];
                    while ((prop != null) && (i < propertyList.Count))
                    {
                        if (value.Name.ToLower() == prop.Name.ToLower())
                        {
                            prop.InitValue.setValue(value); // set the property's value
                        }
                        i++;
                        if (i < propertyList.Count)
                            prop = propertyList[i];
                    }
                }
            }
            return (initXML.Length > 0) || (propertyList.Count > 0);
        }
        ////====================================================================
        /// <summary>
        /// Save the changes on the form.
        /// </summary>
        ////====================================================================
        public override void OnSave()
        {
            StoreControls();
            String newXML = WriteInitsectionXml();

            // now store the new xml by replacing the old xmlnode in Data
            XmlNode initSection = XmlHelper.Find(Data, "initsection");
            if (initSection != null)
                Data.RemoveChild(initSection);
            XmlDocument doc = new XmlDocument();
            doc.LoadXml(newXML);
            Data.AppendChild(Data.OwnerDocument.ImportNode(doc.DocumentElement, true));
        }
        ////====================================================================
        /// <summary>
        /// Store the forms controls into the list of TTypedValues.
        /// </summary>
        ////====================================================================
        private void StoreControls()
        {
            string[] VariableNames = DataTableUtility.GetColumnAsStrings((DataTable)Grid.DataSource, Grid.Columns[0].Name);
            string[] AliasNames = DataTableUtility.GetColumnAsStrings((DataTable)Grid.DataSource, Grid.Columns[1].Name);
            string[] Aggreg = DataTableUtility.GetColumnAsStrings((DataTable)Grid.DataSource, Grid.Columns[2].Name);
            string[] DecPlaces = DataTableUtility.GetColumnAsStrings((DataTable)Grid.DataSource, Grid.Columns[3].Name);

            string[] EventNames = DataTableUtility.GetColumnAsStrings((DataTable)EventGrid.DataSource, EventGrid.Columns[0].Name);

            uint count;
            int i = 0;
            while (i < propertyList.Count)
            {
                if (propertyList[i].InitValue.Name == "apsim_format")
                    propertyList[i].InitValue.setValue(true);

                if (propertyList[i].InitValue.Name == "filename")
                    propertyList[i].InitValue.setValue(FileName);
                
                if (propertyList[i].InitValue.Name == "interval")
                    propertyList[i].InitValue.setValue(Convert.ToInt32(textBox1.Text));

                if (propertyList[i].InitValue.Name == "intervalunit")
                    propertyList[i].InitValue.setValue(comboBox1.Text);

                if (propertyList[i].InitValue.Name == "dateformat")
                {
                    propertyList[i].InitValue.setValue(comboBox2.Text);
                }

                if (propertyList[i].InitValue.Name == "outputs")
                {
                    TTypedValue sdmlinit = propertyList[i].InitValue;
                    sdmlinit.setElementCount(0);
                    count = 0;
                    // loop through each row and add it to the outputs list
                    for (int v = 0; v < VariableNames.Length; v++)
                    {
                        if (VariableNames[v].Length > 0)
                        {
                            count++;
                            sdmlinit.setElementCount(sdmlinit.count() + 1);
                            sdmlinit.item(count).member("varname").setValue(VariableNames[v]);
                            String alias = AliasNames[v].Replace(' ', '_');
                            if (alias.Length < 1)
                            {
                                alias = VariableNames[v];
                            }
                            sdmlinit.item(count).member("alias").setValue(alias);
                            sdmlinit.item(count).member("aggreg").setValue(Aggreg[v]);
                            sdmlinit.item(count).member("decplaces").setValue(DecPlaces[v]);
                        }
                    }
                }
                if (propertyList[i].InitValue.Name == "outputfrequency")
                {
                    TTypedValue sdmlinit = propertyList[i].InitValue;
                    count = 0;
                    sdmlinit.setElementCount(0);
                    if (EventNames.Length == 0)
                    {
                        count = 1;
                        sdmlinit.setElementCount(1);
                        sdmlinit.item(count).setValue("post");  // add the default post event (overrides update_outputs)
                    }
                    else
                    {
                        // loop through each row and add it to the outputfrequency array
                        for (int v = 0; v < EventNames.Length; v++)
                        {
                            if (EventNames[v].Length > 0)
                            {
                                count++;
                                sdmlinit.setElementCount(sdmlinit.count() + 1);
                                sdmlinit.item(count).setValue(EventNames[v]);
                            }
                        }
                    }
                }
                i++;
            }
        }
        ////====================================================================
        /// <summary>
        /// Refresh the variable and events grid
        /// </summary>
        ////====================================================================
        public override void OnRefresh()
        {
            base.OnRefresh();

            // get the filename from the simulation
            ApsimFile.Component outputfileComponent = Controller.ApsimData.Find(NodePath);
            FileName = ComponentUtility.CalcFileName(outputfileComponent, "_");
            String aVersion = Configuration.Instance.ApsimVersion();
            String Title = "ApsimVersion = " + aVersion + Environment.NewLine + "Title = " + FileName;

            int i = 0;
            while (i < propertyList.Count)
            {
                if (propertyList[i].InitValue.Name == "title")
                {
                    propertyList[i].InitValue.setValue(Title);
                }
                if (propertyList[i].InitValue.Name == "interval")
                {
                    textBox1.Text = propertyList[i].InitValue.asInt().ToString();
                }
                if (propertyList[i].InitValue.Name == "intervalunit")
                {
                    comboBox1.SelectedIndex = comboBox1.Items.IndexOf(propertyList[i].InitValue.asStr());
                    if (comboBox1.SelectedIndex < 0)
                    {
                        comboBox1.SelectedIndex = comboBox1.Items.IndexOf("day");
                    }
                }
                if (propertyList[i].InitValue.Name == "dateformat")
                {
                    // ensure that the current sep is used so APSIMInputFile copes nicely with reading
                    CultureInfo ci = CultureInfo.CurrentCulture;
                    String delim = ci.DateTimeFormat.DateSeparator;
                    String format = propertyList[i].InitValue.asStr();
                    format = format.Replace("-", delim);
                    format = format.Replace("/", delim);
                    comboBox2.Text = format;
                }
                if (comboBox2.Text.Length < 1)
                {
                    comboBox2.SelectedIndex = 0;
                }
                i++;
            }
           
            PopulateEventsGrid();       // Restore the chosen events into the events grid
            PopulateVariablesGrid();    // Restore the chosen variables into the variables grid

            // We want to find the component that is a child of our paddock.
            ApsimFile.Component Paddock = Controller.ApsimData.Find(NodePath).FindContainingPaddock();
            GetSiblingComponents(Paddock, ref ComponentNames, ref ComponentTypes);

            // populate the events tab
            PopulateEventComponentFilter(); //also triggers PopulateEventsListView() 

            // populate the variable tab
            PopulateComponentFilter();      //also triggers PopulateVariableListView()

            if (tabControl1.SelectedIndex == OUTPUTTAB)
            {
                ReloadOutputFile();
            }
        }
        ////====================================================================
        /// <summary>
        /// Restore the chosen variables into the variables grid
        /// </summary>
        ////====================================================================
        private void PopulateVariablesGrid()
        {  
            DataTable Table = new DataTable();
            // if (XmlHelper.Type(Data).ToLower() == "variables")
            {
                // GridLabel.Text = "Output file columns:";
                DictionaryLabel.Text = "Variables to drag onto grid:";
                Table.Columns.Clear();
                Table.Columns.Add("Variable name", System.Type.GetType("System.String"));
                Table.Columns.Add("Alias", System.Type.GetType("System.String"));
                Table.Columns.Add("Aggreg.", System.Type.GetType("System.String"));
                Table.Columns.Add("Dec. Places", System.Type.GetType("System.String"));
            }

            // Fill data table.
            int i = 0;
            while (i < propertyList.Count)
            {
                if (propertyList[i].InitValue.Name == "outputs")
                {
                    for (int v = 1; v <= propertyList[i].InitValue.count(); v++)
                    {
                        TTypedValue typedVal = propertyList[i].InitValue.item((uint)v);
                        DataRow NewRow = Table.NewRow();
                        NewRow[0] = typedVal.member("varname").asStr();
                        NewRow[1] = typedVal.member("alias").asStr();
                        NewRow[2] = typedVal.member("aggreg").asStr();
                        NewRow[3] = typedVal.member("decplaces").asStr();
                        Table.Rows.Add(NewRow);
                    }
                    i = propertyList.Count; // terminate loop
                }
                i++;
            }
            // set the autosizemode back to none before I set the column widths (avoids failure)
            Grid.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.None;
            Grid.Columns.Clear();
            Grid.DataSource = Table;     // Give data table to grid.
            Grid.Columns[2].Width = 60;
            Grid.Columns[3].Width = 75;
            Grid.Columns[0].AutoSizeMode = DataGridViewAutoSizeColumnMode.Fill;
            Grid.Columns[0].AutoSizeMode = DataGridViewAutoSizeColumnMode.None;
            Grid.Columns[0].DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft;
            DataGridViewColumn thirdColumn = Grid.Columns[2];
            thirdColumn.ToolTipText = "max, min, average, total";
            DataGridViewColumn firstColumn = Grid.Columns[0];
            firstColumn.ToolTipText = "Ctrl+Up, Ctrl+Down to reorder";
            Grid.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill;
        }
        ////====================================================================
        /// <summary>
        /// Restore the chosen events into the events grid
        /// </summary>
        ////====================================================================
        private void PopulateEventsGrid()
        {
            DataTable Table = new DataTable();
            Table.Columns.Clear();
            Table.Columns.Add("Event name", System.Type.GetType("System.String"));

            // Fill data table.
            int i = 0;
            while (i < propertyList.Count)
            {
                if (propertyList[i].InitValue.Name == "outputfrequency")
                {
                    for (uint v = 1; v <= propertyList[i].InitValue.count(); v++)
                    {
                        TTypedValue typedVal = propertyList[i].InitValue.item(v);
                        DataRow NewRow = Table.NewRow();
                        NewRow[0] = typedVal.asStr();
                        Table.Rows.Add(NewRow);
                    }
                    i = propertyList.Count; //terminate loop
                }
                i++;
            }
            // set the autosizemode back to none before I set the column widths (avoids failure)
            Grid.Columns[0].AutoSizeMode = DataGridViewAutoSizeColumnMode.None;
            EventGrid.Columns.Clear();
            EventGrid.DataSource = Table;     // Give data table to grid.

            Grid.Columns[0].AutoSizeMode = DataGridViewAutoSizeColumnMode.Fill;
            Grid.Columns[0].DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft;
        }
        ////====================================================================
        /// <summary>
        /// Populate the component filter drop down
        /// </summary>
        ////====================================================================
        private void PopulateComponentFilter()
        {
			ComponentFilter.Items.Clear(); 
            foreach (string ComponentName in ComponentNames)
            {
                ComponentFilter.Items.Add(ComponentName);
            }

            if (ComponentFilter.Items.Count > 0)
            {
                ComponentFilter.SelectedIndex = 0;
			} 
        }
        ////====================================================================
        /// <summary>
        /// Populate the variable list view box
        /// </summary>
        ////====================================================================
        private void PopulateVariableListView()
        {
            if (ComponentFilter.SelectedIndex >= 0 & ComponentFilter.SelectedIndex < ComponentNames.Count)
            {
                Cursor.Current = Cursors.WaitCursor;

                string ComponentType = ComponentTypes[ComponentFilter.SelectedIndex];
                string ComponentName = ComponentNames[ComponentFilter.SelectedIndex];
                string PropertyGroup = XmlHelper.Type(Data);
                // e.g. variables or events
                AddVariablesToListView(ComponentName, ComponentType, PropertyGroup);

                Cursor.Current = Cursors.Default;
            }
        }
        ////====================================================================
        /// <summary>
        /// 
        /// </summary>
        ////====================================================================
        private void PopulateEventComponentFilter()
        {
            ComponentEventsFilter.Items.Clear();
            foreach (string ComponentName in ComponentNames)
            {
                ComponentEventsFilter.Items.Add(ComponentName);
            }

            if (ComponentEventsFilter.Items.Count > 0)
            {
                ComponentEventsFilter.SelectedIndex = 0;
            }
        }
        ////====================================================================
        /// <summary>
        /// Fill the listview with event names
        /// </summary>
        ////====================================================================
        private void PopulateEventsListView()
        {
            if (ComponentEventsFilter.SelectedIndex >= 0 & ComponentEventsFilter.SelectedIndex < ComponentNames.Count)
            {
                Cursor.Current = Cursors.WaitCursor;
                EventsListView.BeginUpdate();
                EventsListView.Groups.Clear();
                EventsListView.Items.Clear();

                string ComponentType = ComponentTypes[ComponentEventsFilter.SelectedIndex];
                string ComponentName = ComponentNames[ComponentEventsFilter.SelectedIndex];
                string PropertyGroup = XmlHelper.Type(Data);
                // e.g. variables or events
                AddEventsToListView(ComponentName, ComponentType, PropertyGroup);

                EventsListView.EndUpdate();
                EventsListView.Columns[0].AutoResize(ColumnHeaderAutoResizeStyle.ColumnContent);
                Cursor.Current = Cursors.Default;
            }
        }
        ////====================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="ComponentName"></param>
        /// <param name="ComponentType"></param>
        /// <param name="PropertyGroup"></param>
        ////====================================================================
        private void AddVariablesToListView(string ComponentName, string ComponentType, string PropertyGroup)
        {
            List<TCompProperty> varList = null;
            varList = Types.Instance.VariableList(ComponentType);

            //add to the treeview
            afTreeViewColumns1.StopLayout();
            afTreeViewColumns1.TreeView.Nodes.Clear();

            foreach (TCompProperty Variable in varList)
            {
                // add the descriptions - AusFarm and APSIM components store this differently
                if (Variable.sDescr.Length > 1)
                {
                    Variable.InitValue.setDescr(Variable.sDescr, 255);
                }

                TreeNode trNode2 = new TreeNode();
                afTreeViewColumns1.TreeView.Nodes.Add(trNode2);
                addTreeModelNode(trNode2, Variable.InitValue.Name, Variable.InitValue);
            }
            afTreeViewColumns1.RestartLayout();
        }
        ////=====================================================================
        /// <summary>
        /// Add component events to the listview.
        /// </summary>
        /// <param name="ComponentName"></param>
        /// <param name="ComponentType"></param>
        /// <param name="PropertyGroup"></param>
        ////=====================================================================
        private void AddEventsToListView(string ComponentName, string ComponentType, string PropertyGroup)
        {
            List<Types.MetaDataInfo> ModelInfo = null;
            ModelInfo = Types.Instance.Events(ComponentType);
           
            string GroupName = ComponentName;
            if (string.IsNullOrEmpty(GroupName))
            {
                GroupName = ComponentName + " " + PropertyGroup;
            }
            ListViewGroup NewGroup = new ListViewGroup(GroupName);

            foreach (Types.MetaDataInfo Variable in ModelInfo)
            {
                EventsListView.Groups.Add(NewGroup);
                ListViewItem ListItem = new ListViewItem(Variable.Name);
                ListItem.Group = NewGroup;
                ListItem.SubItems.Add(Variable.Description);
                EventsListView.Items.Add(ListItem);
            }
        }

        ////=====================================================================
        /// <summary>
        /// Return a list of sibling component names and types for the specified data component 
        /// </summary>
        /// <param name="paddock"></param>
        /// <param name="componentNames"></param>
        /// <param name="componentTypes"></param>
        ////=====================================================================
        private static void GetSiblingComponents(ApsimFile.Component paddock, ref StringCollection componentNames, ref StringCollection componentTypes)
        {
            componentNames.Clear();
            componentTypes.Clear();
            if ((paddock != null))
            {
                if ((paddock.Parent != null) && (paddock.Parent.Parent != null))
                {
                    GetSiblingComponents(paddock.Parent, ref componentNames, ref componentTypes);
                }
                foreach (ApsimFile.Component Sibling in paddock.ChildNodes)
                {
                    if ((Sibling.Type.ToLower() != "simulation") && (Sibling.Type.ToLower() != "graph"))
                    {
                        componentNames.Add(Sibling.Name);
                        componentTypes.Add(Sibling.Type);
                    }
                }
            }
        }
        ////=====================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void ComponentFilter_SelectedIndexChanged(object sender, EventArgs e)
        {
            PopulateVariableListView();
        }
        ////=====================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        ////=====================================================================
        private void ComponentEventsFilter_SelectedIndexChanged(object sender, EventArgs e)
        {
            PopulateEventsListView();
        }
        ////=====================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        ////=====================================================================
        private void Grid_DragEnter(object sender, DragEventArgs e)
        {
            e.Effect = DragDropEffects.Copy;
        }
        ////=====================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        ////=====================================================================
        private void Grid_DragOver(object sender, DragEventArgs e)
        {
            e.Effect = DragDropEffects.Copy;
        }
        ////=====================================================================
        /// <summary>
        /// User has dropped a variable onto the variable grid
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        ////=====================================================================
        private void Grid_DragDrop(object sender, DragEventArgs e)
        {
            TreeNode srcNode = (TreeNode)e.Data.GetData(typeof(TreeNode));
            if (srcNode != null)
            {
                int decplaces = 0;
                TTypedValue item = ((TAFTreeViewColumnTag)(srcNode.Tag)).TypedValue;
                if (item.isScalar() && ((item.baseType() == TTypedValue.TBaseType.ITYPE_DOUBLE) || (item.baseType() == TTypedValue.TBaseType.ITYPE_SINGLE)))
                    decplaces = 2;
                String varText = getParentPath(srcNode, srcNode.Text);
                AddVariableToGrid(varText, srcNode.Text, "", decplaces);
            }
        }
        ////=====================================================================
        /// <summary>
        /// Builds the path string for a variable in the tree.
        /// For example: plant2stock:herbage[1]:dm
        /// </summary>
        /// <param name="node">The lowest tree node</param>
        /// <param name="varText">The text for the node</param>
        /// <returns>The path with the parent text added</returns>
        ////=====================================================================
        private String getParentPath(TreeNode node, String varText)
        {
            String fullPath = varText;
            if (node.Parent != null)
            {
                if (node.Parent.Tag != null)
                {
                    TTypedValue parent = ((TAFTreeViewColumnTag)(node.Parent.Tag)).TypedValue;
                    if (parent.isRecord())
                    {
                        fullPath = node.Parent.Text + ":" + varText;
                    }
                    else
                    {
                        if (node.Text.Contains("["))
                        {
                            fullPath = node.Parent.Text + varText;
                        }
                        else
                        {
                            fullPath = node.Parent.Text + "." + varText;
                        }
                    }
                    fullPath = getParentPath(node.Parent, fullPath);
                }
            }
            return fullPath;
        }
        ////=====================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="name"></param>
        /// <param name="alias"></param>
        /// <param name="aggreg"></param>
        /// <param name="decplaces"></param>
        ////=====================================================================
        private void AddVariableToGrid(String name, String alias, String aggreg, int decplaces)
        {
            DataTable Table = (DataTable)Grid.DataSource;

            // Go look for a blank cell.
            int Row = 0;
            for (Row = 0; Row <= Table.Rows.Count - 1; Row++)
            {
                /*if ( Information.IsDBNull(Table.Rows[Row][0]) || (string.IsNullOrEmpty(Table.Rows[Row][0])) )
                {
                    break; // TODO: might not be correct. Was : Exit For
                }*/
            }
            if (Row == Table.Rows.Count)
            {
                DataRow NewRow = ((DataTable)Grid.DataSource).NewRow();
                NewRow[0] = name;
                NewRow[1] = alias;
                NewRow[2] = aggreg;
                NewRow[3] = decplaces.ToString();
                Table.Rows.Add(NewRow);
            }
            else
            {
                Table.Rows[Row][0] = name;
            }

            Grid.Columns[2].Width = 60;
            Grid.Columns[3].Width = 60;
            Grid.Columns[0].DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft;
        }
        ////=====================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="VariableNames"></param>
        ////=====================================================================
        private void AddVariablesToGrid(ListView.SelectedListViewItemCollection VariableNames)
        {
            // UserChange = false;
            DataTable Table = (DataTable)Grid.DataSource;

            foreach (ListViewItem SelectedItem in VariableNames)
            {
                // Go look for a blank cell.
                int Row = 0;
                for (Row = 0; Row <= Table.Rows.Count - 1; Row++)
                {
                    /*if ( Information.IsDBNull(Table.Rows[Row][0]) || (string.IsNullOrEmpty(Table.Rows[Row][0])) )
                    {
                        break; // TODO: might not be correct. Was : Exit For
                    }*/
                }
                if (Row == Table.Rows.Count)
                {
                    DataRow NewRow = ((DataTable)Grid.DataSource).NewRow();
                    NewRow[0] = SelectedItem.Text;
                    NewRow[1] = SelectedItem.Text;
                    NewRow[2] = "";
                    NewRow[3] = "0";
                    Table.Rows.Add(NewRow);
                }
                else
                {
                    Table.Rows[Row][0] = SelectedItem.Text;
                }
            }
            // Grid.PopulateGrid();
            Grid.Columns[2].Width = 60;
            Grid.Columns[3].Width = 60;
            // Grid.Columns[0].AutoSizeMode = DataGridViewAutoSizeColumnMode.Fill;
            // Grid.Columns[0].AutoSizeMode = DataGridViewAutoSizeColumnMode.None;
            Grid.Columns[0].DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft;
            // UserChange = true;
        }
        ////=====================================================================
        /// <summary>
        /// When changing tabs - reload the data view.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        ////=====================================================================
        private void tabControl1_Selected(object sender, TabControlEventArgs e)
        {
            if (e.TabPageIndex == OUTPUTTAB)
            {
                ReloadOutputFile();
            }
        }
        ////=====================================================================
        /// <summary>
        /// Reloads the output file into the datagrid
        /// </summary>
        ////=====================================================================
        private void ReloadOutputFile()
        {
            Cursor.Current = Cursors.WaitCursor;
            try
            {
                label2.Text = Path.GetFullPath(FileName);
                if (File.Exists(Path.GetFullPath(FileName)))
                {
                    try
                    {
                        if (chkTextView.Checked)    // if view the text file in the richedit
                        {
                            FileContentsBox.Clear();
                            DataGrid.Visible = false;
                            FileContentsBox.Visible = true;
                            FileStream stream = new FileStream(Path.GetFullPath(FileName), FileMode.Open, FileAccess.Read, FileShare.ReadWrite);
                            StreamReader outfile = new StreamReader(stream);
                            FileContentsBox.AppendText(outfile.ReadToEnd());
                            outfile.Close();
                            labelLines.Text = FileContentsBox.Lines.Length.ToString() + " lines";
                        }
                        else
                        {
                            DataGrid.Visible = true;
                            FileContentsBox.Visible = false;
                            DataGrid.DataSource = null;

                            DataProcessor Processor = new DataProcessor();
                            // List<string> DefaultFileNames = new List<string>();
                            // UIUtility.OutputFileUtility.GetOutputFiles(Controller, Controller.Selection, DefaultFileNames);
                            // Processor.DefaultOutputFileNames = DefaultFileNames;

                            XmlDocument Doc = new XmlDocument();
                            Doc.LoadXml("<GDApsimFileReader name=\"ApsimFileReader\">" +
                                          "<FileName>" + Path.GetFullPath(FileName) + "</FileName>" +
                                        "</GDApsimFileReader>");
                            // Doc.LoadXml(Controller.ApsimData.Find(NodePath).FullXML());
                            DataGrid.DataSource = Processor.Go(Doc.DocumentElement, NodePath);
                            foreach (DataGridViewColumn Col in DataGrid.Columns)
                                Col.SortMode = DataGridViewColumnSortMode.NotSortable;
                            labelLines.Text = DataGrid.RowCount.ToString() + " lines";
                            DataGrid.Columns[0].Frozen = true;
                        }
                    }
                    catch (Exception)
                    {
                    }
                }
            }
            finally
            {
                Cursor.Current = Cursors.Default;
            }
        }
        ////=====================================================================
        /// <summary>
        /// Respond to key clicks on the grid. Del, Ctrl+Up, Ctrl+Down
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        ////=====================================================================
        private void Grid_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Delete)
            {
                OnDelete(sender, e);
            }
            if (e.KeyCode == Keys.Up && e.Modifiers == Keys.Control)
            {
                 DataGridViewRow dr = Grid.SelectedRows[0]; // single row selection
                 int idx = Grid.SelectedRows[0].Index;
                 int newrow = idx - 1;
                 if (newrow >= 0)
                 {
                     string sTmp = "";
                     for (int iTmp = 0; iTmp <= Grid.Columns.Count - 1; iTmp++)
                     {
                         sTmp = Grid.Rows[idx].Cells[iTmp].Value.ToString();
                         Grid.Rows[idx].Cells[iTmp].Value = Grid.Rows[newrow].Cells[iTmp].Value;
                         Grid.Rows[newrow].Cells[iTmp].Value = sTmp;
                     }
                     Grid.FirstDisplayedScrollingRowIndex = newrow;
                     Grid.CurrentRow.Selected = false;
                     Grid.Rows[newrow].Selected = true;
                 }
                 e.Handled = true;
            }
            if (e.KeyCode == Keys.Down && e.Modifiers == Keys.Control)
            {
                DataGridViewRow dr = Grid.SelectedRows[0]; // single row selection
                int idx = Grid.SelectedRows[0].Index;
                int newrow = idx + 1;
                if (newrow < Grid.Rows.Count - 1)
                {
                    string sTmp = "";
                    for (int iTmp = 0; iTmp <= Grid.Columns.Count - 1; iTmp++)
                    {
                        sTmp = Grid.Rows[newrow].Cells[iTmp].Value.ToString();
                        Grid.Rows[newrow].Cells[iTmp].Value = Grid.Rows[idx].Cells[iTmp].Value;
                        Grid.Rows[idx].Cells[iTmp].Value = sTmp;
                    }
                    Grid.FirstDisplayedScrollingRowIndex = newrow;
                    Grid.CurrentRow.Selected = false;
                    Grid.Rows[newrow].Selected = true;
                }
                e.Handled = true;
            }
        }
        ////=====================================================================
        /// <summary>
        /// Delete the current row
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        ////=====================================================================
        private void OnDelete(object sender, KeyEventArgs e)
        {
            if (!((DataGridView)sender).IsCurrentCellInEditMode)
            {
                // delete the whole row
                foreach (DataGridViewRow dr in ((DataGridView)sender).SelectedRows)
                {
                    if (dr.Cells[0].Value.ToString() != null) //Cells[0] - primary key
                    {
                        dr.Cells[0].Value = ""; // Grid.Rows.Remove(dr);
                    }
                }
            }
        }
        ////=====================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        ////=====================================================================
        private void EventGrid_DragDrop(object sender, DragEventArgs e)
        {
            AddEventsToGrid(EventsListView.SelectedItems);
        }
        ////=====================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        ////=====================================================================
        private void EventGrid_DragEnter(object sender, DragEventArgs e)
        {
            e.Effect = DragDropEffects.Copy;
        }
        ////=====================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        ////=====================================================================
        private void EventGrid_DragOver(object sender, DragEventArgs e)
        {
            e.Effect = DragDropEffects.Copy;
        }
        ////=====================================================================
        /// <summary>
        /// Respond to key clicks on the Events grid
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        ////=====================================================================
        private void EventGrid_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Delete)
            {
                OnDelete(sender, e);
            }
        }
        ////=====================================================================
        /// <summary>
        /// Add the event names to the grid
        /// </summary>
        /// <param name="eventNames"></param>
        ////=====================================================================
        private void AddEventsToGrid(ListView.SelectedListViewItemCollection eventNames)
        {
            // UserChange = false;
            DataTable Table = (DataTable)EventGrid.DataSource;

            foreach (ListViewItem SelectedItem in eventNames)
            {
                int Row = 0;
                for (Row = 0; Row <= Table.Rows.Count - 1; Row++)
                {
                    /*if ( Information.IsDBNull(Table.Rows[Row][0]) || (string.IsNullOrEmpty(Table.Rows[Row][0])) )
                    {
                        break; // TODO: might not be correct. Was : Exit For
                    }*/
                }
                if (Row == Table.Rows.Count)
                {
                    DataRow NewRow = ((DataTable)EventGrid.DataSource).NewRow();
                    NewRow[0] = SelectedItem.Text;
                    Table.Rows.Add(NewRow);
                }
                else
                {
                    Table.Rows[Row][0] = SelectedItem.Text;
                }
            }
            EventGrid.Columns[0].Width = 360;
            EventGrid.Columns[0].AutoSizeMode = DataGridViewAutoSizeColumnMode.Fill;
            EventGrid.Columns[0].DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft;
        }
        ////=====================================================================
        /// <summary>
        /// User is trying to initiate a drag - allow drag operation
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        ////=====================================================================
        private void EventsListView_ItemDrag(object sender, ItemDragEventArgs e)
        {
            EventsListView.DoDragDrop("xx", DragDropEffects.All);
        }
        ////=====================================================================
        /// <summary>
        /// Called from the tree when arrays are resized.
        /// Using the selected node, just recreate it's child nodes.
        /// </summary>
        ////=====================================================================
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
        ////=====================================================================
        /// <summary>
        /// 
        /// </summary>
        ////=====================================================================
        private void afTreeViewColumns1_saveChangesEvent()
        {
            this.afTreeViewColumns1.Focus();
        }
        ////=====================================================================
        /// <summary>
        /// Event handler for the control that fills the correct column with 
        /// the specified data.
        /// </summary>
        /// <param name="item">The tree item chosen</param>
        /// <param name="col">The column in the grid to be filled</param>
        /// <returns>The string value for the cell in the grid</returns>
        ////=====================================================================
        private String afTreeViewColumns1_getColVal(TAFTreeViewColumnTag item, int col)
        {
            switch (col)
            {
                case 0: return item.Variable;
                case 1: return item.Type;
                case 2: return item.Unit;
                case 3: return item.Descr;
                default: return "";
            }
        }
        ////=======================================================================
        /// <summary>
        /// Standard function for adding a node to the treelist. Will recurse
        /// down through the value if it is a record or array.
        /// </summary>
        /// <param name="parentNode"></param>
        /// <param name="name"></param>
        /// <param name="typedValue"></param>
        ////=====================================================================
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
                if (typedValue.isArray() && (iCount < 1))
                {
                    typedValue.setElementCount(1);
                    iCount++;
                }
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

        private void DataGrid_CellMouseClick(object sender, DataGridViewCellMouseEventArgs e)
        {
            if (e.ColumnIndex >= 0 && e.RowIndex == -1 && OnColumnClickEvent != null && e.Button == MouseButtons.Left && DataGrid.Columns.Count > 0)
                OnColumnClickEvent.Invoke(DataGrid.Columns[e.ColumnIndex].HeaderText);
        }
        ////=====================================================================
        /// <summary>
        /// User chooses text view or table view of the file
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        ////=====================================================================
        private void chkTextView_CheckStateChanged(object sender, EventArgs e)
        {
            ReloadOutputFile();
        }
    }

}
