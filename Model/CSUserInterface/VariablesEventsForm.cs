using System;
using System.IO;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Xml;
using System.Windows.Forms;
using Controllers;
using System.Collections.Specialized;
using CSGeneral;
using ApsimFile;
using System.CodeDom.Compiler;
using System.Runtime.InteropServices;
using System.Reflection;

namespace CSUserInterface
{
    /// <summary>
    /// A container class that lists a components exported variables, and subscribed events.
    /// </summary>
	class ComponentVE
        {
        public string name;
        public string type;
        public string manager2script;
        public ComponentVE(ApsimFile.Component c)
		{
			name = c.Name;
			type = c.Type.ToLower(); 
			XmlDocument doc = new XmlDocument();
			doc.LoadXml(c.FullXMLNoShortCuts());
            manager2script = XmlHelper.Value(doc.FirstChild, "text");
		}

        public List<Types.MetaDataInfo> ModelInfo(string what) {
			List<Types.MetaDataInfo> result = null;
			if (what.ToLower() == "variables")
				result = Types.Instance.Variables(type);
			else if (what.ToLower() == "events")
				result = Types.Instance.Events(type);
            try
            {
                if (type.ToLower() == "manager2" && what.ToLower() == "variables")
                    result.AddRange(getDynamicVariables(CompileTextToAssembly(manager2script)));
                else if (type.ToLower() == "manager2" && what.ToLower() == "events")
                    result.AddRange(getDynamicEvents(CompileTextToAssembly(manager2script)));
            }
            catch (Exception) { /* nothing - probably incomplete */ }

            return(result);
        }
        private List<Types.MetaDataInfo> getDynamicVariables(Assembly assembly) 
        {
            List<Types.MetaDataInfo> result = new List<Types.MetaDataInfo>();
            foreach( Type t in assembly.GetTypes())
                foreach (FieldInfo property in t.GetFields(BindingFlags.FlattenHierarchy | BindingFlags.Instance | BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic))
                {
                    Types.MetaDataInfo m = new Types.MetaDataInfo();
                    foreach (Object Attr in property.GetCustomAttributes(false))
                    {
                        if (Attr.GetType().Name == "Output")
                            m.Name = property.Name;
                        if (Attr.GetType().Name == "Units")
                            m.Units = Attr.ToString();
                        if (Attr.GetType().Name == "Description")
                            m.Description = Attr.ToString();
                        //m.IsArray = false; FIXME
                    }
                    if (m.Name != null) 
                       result.Add(m);
                }
            return(result);
        }

        private List<Types.MetaDataInfo> getDynamicEvents(Assembly assembly)
        {
            List<Types.MetaDataInfo> result = new List<Types.MetaDataInfo>();
            foreach (Type t in assembly.GetTypes())
                foreach (MethodInfo method in t.GetMethods(BindingFlags.FlattenHierarchy | BindingFlags.Instance | BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic))
                {
                    Types.MetaDataInfo m = new Types.MetaDataInfo();
                    foreach (Object Attr in method.GetCustomAttributes(false))
                    {
                        if (Attr.GetType().Name == "EventHandler")
                            m.Name = method.Name ;
                        if (Attr.GetType().Name == "Description")
                            m.Description = Attr.ToString();
                    }
                    if (m.Name != null)
                    {
                        if (m.Name.Substring(0, 2).ToLower() == "on")
                            m.Name = m.Name.Substring(2);
                        result.Add(m);
                    }
                }
            return (result);
        }

        // Quick and dirty compile to probe for metadata. 
        private Assembly CompileTextToAssembly(string script)
        {
            bool VB = script.IndexOf("Imports System") != -1;
            string Language = CodeDomProvider.GetLanguageFromExtension(".cs");
            if (VB)
                Language = CodeDomProvider.GetLanguageFromExtension(".vb");

            CodeDomProvider Provider = CodeDomProvider.CreateProvider(Language);
            CompilerParameters Params = new CompilerParameters();
            Params.GenerateInMemory = true;      //Assembly is created in memory
            Params.TempFiles = new TempFileCollection(Path.GetTempPath(), false);
            Params.TreatWarningsAsErrors = false;
            Params.WarningLevel = 2;
            Params.ReferencedAssemblies.Add("System.dll");
            Params.ReferencedAssemblies.Add("System.Xml.dll");
            Params.ReferencedAssemblies.Add("System.Core.dll");
            Params.ReferencedAssemblies.Add("System.Data.Linq.dll");
            Params.ReferencedAssemblies.Add(Path.Combine(Configuration.ApsimBinDirectory(), "CSDotNetComponentInterface.dll"));
            Params.ReferencedAssemblies.Add(Path.Combine(Configuration.ApsimBinDirectory(), "DotNetProxies.dll"));
            Params.ReferencedAssemblies.Add(Path.Combine(Configuration.ApsimBinDirectory(), "CMPServices.dll"));
            Params.ReferencedAssemblies.Add(Path.Combine(Configuration.ApsimBinDirectory(), "CSGeneral.dll"));
            Params.TempFiles = new TempFileCollection(".");
            Params.TempFiles.KeepFiles = false;

            CompilerResults results = Provider.CompileAssemblyFromSource(Params, new string [] {script} );

            return results.CompiledAssembly;
        }
        // --------------------------------------------------
        // Return a list of sibling component names and types
        // for the specified data component
        // --------------------------------------------------
        static public void GetVisibleComponents(ApsimFile.Component component, ref List<ComponentVE> Components)
        {
            Components.Clear();
            List<ApsimFile.Component> heirarchy = new List<ApsimFile.Component>();
            ApsimFile.Component parent = component.Parent;
            while (parent != null)
            {
                if (parent.Type == "area" || parent.Type == "simulation")
                    heirarchy.Insert(0, parent);
                parent = parent.Parent;
            }
            foreach (ApsimFile.Component paddock in heirarchy)
                GetSiblingComponents(paddock, ref Components);
        }

        static private void GetSiblingComponents(ApsimFile.Component component, ref List<ComponentVE> Components)
        {
            foreach (ApsimFile.Component Sibling in component.ChildNodes)
            {
                if (Types.Instance.Dlls(Sibling.Type).Count() > 0)
                    Components.Add(new ComponentVE(Sibling));
                if (Sibling.Type == "folder")
                    GetSiblingComponents(Sibling, ref Components);
            }
        }

    }
    public partial class VariablesEventsForm : Form
    {
        private BaseController Controller;
        private ApsimFile.Component Selected;
        private List<ComponentVE> MyComponents = new List<ComponentVE>();
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

                ComponentVE.GetVisibleComponents(Selected, ref MyComponents);

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
            foreach (ComponentVE component in MyComponents)
                ComponentFilter.Items.Add(component.name);
        
            if (ComponentFilter.Items.Count > 0)
                ComponentFilter.SelectedIndex = 0;
        }


        private void PopulateVariableListView()
        {
            // ----------------------------------------------
            // Populate the variable list view box
            // ----------------------------------------------

            if ((ComponentFilter.SelectedIndex >= 0) && (ComponentFilter.SelectedIndex < MyComponents.Count))
            {
                System.Windows.Forms.Cursor.Current = Cursors.WaitCursor;
                VariableListView.BeginUpdate();
                VariableListView.Groups.Clear();
                VariableListView.Items.Clear();

                VariableListView.Columns.Clear();
                if (EventsCheckBox.Checked)
                {
                    AddThingsToListView(MyComponents[ComponentFilter.SelectedIndex], "Events");
                    ColumnHeader1.Text = "Event name";
                    VariableListView.Columns.Add(ColumnHeader1);
                    VariableListView.Columns.Add(ColumnHeader3);
                }
                else
                {
                    AddThingsToListView(MyComponents[ComponentFilter.SelectedIndex], "Variables");
                    ColumnHeader1.Text = "Variable name";
                    VariableListView.Columns.Add(ColumnHeader1);
                    VariableListView.Columns.Add(ColumnHeader4);
                    VariableListView.Columns.Add(ColumnHeader2);
                    VariableListView.Columns.Add(ColumnHeader3);

                }
                VariableListView.EndUpdate();
                VariableListView.Columns[0].AutoResize(ColumnHeaderAutoResizeStyle.ColumnContent);
                System.Windows.Forms.Cursor.Current = Cursors.Default;
            }
        }

		private void AddThingsToListView(ComponentVE c, string what)
        {
			string GroupName = c.name;
            if (string.IsNullOrEmpty(GroupName))
            {
                GroupName = c.name + what;
            }
            ListViewGroup NewGroup = new ListViewGroup(GroupName);
            VariableListView.Groups.Add(NewGroup);

            StringCollection hidden = new StringCollection();
            hidden.AddRange(new string[] { "active", "author", "name", "state", "type", "version" });
            foreach (Types.MetaDataInfo thing in c.ModelInfo(what))
            {
                if (hidden.Contains(thing.Name.ToLower()))
                    continue;
                ListViewItem ListItem = new ListViewItem(thing.Name);
                ListItem.Group = NewGroup;
                if (what == "Variables")
                {
                    if (thing.IsArray)
                    {
                        ListItem.SubItems.Add("Yes");
                    }
                    else
                    {
                        ListItem.SubItems.Add("No");
                    }
                    ListItem.SubItems.Add(thing.Units);
                }
                ListItem.SubItems.Add(thing.Description);
                VariableListView.Items.Add(ListItem);
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
