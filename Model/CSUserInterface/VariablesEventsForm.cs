using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using Controllers;
using System.Collections.Specialized;
using CSGeneral;

namespace CSUserInterface
{
    public partial class VariablesEventsForm : Form
    {
        private BaseController Controller;
        private ApsimFile.Component Selected;
        private StringCollection ComponentNames = new StringCollection();
        private StringCollection ComponentTypes = new StringCollection();
        bool InOnLoad = false;

        public VariablesEventsForm(BaseController Cntr)
        {
            InitializeComponent();
            Controller = Cntr;
        }

        private void OnLoad(object sender, EventArgs e)
        {
            if (!InOnLoad)
            {
                InOnLoad = true;
                Selected = Controller.ApsimData.Find(Controller.SelectedPath);

                ApsimFile.Component Paddock = Selected.FindContainingPaddock();
                GetSiblingComponents(Paddock, ref ComponentNames, ref ComponentTypes);

                PopulateComponentFilter();
                PopulateVariableListView();
                InOnLoad = false;
            }
        }

        private void PopulateComponentFilter()
        {
            // ----------------------------------------
            // Populate the component filter drop down
            // ----------------------------------------
            ComponentFilter.Items.Clear();
            foreach (string ComponentName in ComponentNames)
                ComponentFilter.Items.Add(ComponentName);
        
            if (ComponentFilter.Items.Count > 0)
                ComponentFilter.SelectedIndex = 0;
        }


        private void PopulateVariableListView()
        {
            // ----------------------------------------------
            // Populate the variable list view box
            // ----------------------------------------------

            if ((ComponentFilter.SelectedIndex >= 0) && (ComponentFilter.SelectedIndex < ComponentNames.Count))
            {
                System.Windows.Forms.Cursor.Current = Cursors.WaitCursor;
                VariableListView.BeginUpdate();
                VariableListView.Groups.Clear();
                VariableListView.Items.Clear();

                string ComponentType = ComponentTypes[ComponentFilter.SelectedIndex];
                string ComponentName = ComponentNames[ComponentFilter.SelectedIndex];
                string PropertyGroup = "Variables";
                if (EventsCheckBox.Checked)
                    PropertyGroup = "Events";
                
                AddVariablesToListView(ComponentName, ComponentType, PropertyGroup);

                VariableListView.EndUpdate();
                VariableListView.Columns[0].AutoResize(ColumnHeaderAutoResizeStyle.ColumnContent);
                System.Windows.Forms.Cursor.Current = Cursors.Default;
            }
        }

        private void AddVariablesToListView(string ComponentName, string ComponentType, string PropertyGroup)
        {
            List<Types.MetaDataInfo> ModelInfo = null;
            if (PropertyGroup == "Variables")
            {
                ModelInfo = Types.Instance.Variables(ComponentType);
            }
            else
            {
                ModelInfo = Types.Instance.Events(ComponentType);
            }

            string GroupName = ComponentName;
            if (string.IsNullOrEmpty(GroupName))
            {
                GroupName = ComponentName + " " + PropertyGroup;
            }
            ListViewGroup NewGroup = new ListViewGroup(GroupName);

            foreach (Types.MetaDataInfo Variable in ModelInfo)
            {
                VariableListView.Groups.Add(NewGroup);
                ListViewItem ListItem = new ListViewItem(Variable.Name);
                ListItem.Group = NewGroup;
                if (Variable.IsArray)
                {
                    ListItem.SubItems.Add("Yes");
                }
                else
                {
                    ListItem.SubItems.Add("No");
                }
                ListItem.SubItems.Add(Variable.Units);
                ListItem.SubItems.Add(Variable.Description);
                VariableListView.Items.Add(ListItem);
            }
        }

        // --------------------------------------------------
        // Return a list of sibling component names and types
        // for the specified data component
        // --------------------------------------------------
        private static void GetSiblingComponents(ApsimFile.Component Paddock, ref StringCollection ComponentNames, ref StringCollection ComponentTypes)
        {
            ComponentNames.Clear();
            ComponentTypes.Clear();
            if ((Paddock != null))
            {
                if ((Paddock.Parent != null) && (Paddock.Parent.Parent != null))
                {
                    GetSiblingComponents(Paddock.Parent, ref ComponentNames, ref ComponentTypes);
                }
                foreach (ApsimFile.Component Sibling in Paddock.ChildNodes)
                {
                    if ((Sibling.Type.ToLower() != "simulation") && (Sibling.Type.ToLower() != "graph"))
                    {
                        ComponentNames.Add(Sibling.Name);
                        ComponentTypes.Add(Sibling.Type);
                    }
                }
            }
        }

        private void ComponentFilter_TextChanged(System.Object sender, System.EventArgs e)
        {
            PopulateVariableListView();
        }

        private void EventsCheckBox_CheckedChanged(object sender, EventArgs e)
        {
            PopulateVariableListView();
        }
    }
}
