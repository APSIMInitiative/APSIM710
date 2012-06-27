using Microsoft.VisualBasic;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Data;
using System.Drawing;
using System.Diagnostics;
using System.Windows.Forms;


using System.Collections.Specialized;
using System.IO;
using System.Reflection;
using System.Xml;

using ApsimFile;
using CSGeneral;
namespace Controllers
{


    public class BaseController
    {
        // Simple base class for a user interface manager

        private StringCollection MySelectedData = new StringCollection();
        private XmlNode ActionFile;
        private ExplorerUI MyExplorer;
        private Form MyMainForm;
        private ImageList LargeIcons = null;
        private ImageList MediumIcons = null;
        private ImageList SmallIcons = null;
        private List<int> ImageIndexes = null;
        private List<string> ImageFileNames = null;
        private bool MyFactorialMode = false;

        private string MySelectedFactorialPath = "";

        public ApsimFile.ApsimFile ApsimData;
        public delegate void NotifyEventHandler();
        public delegate void SelectionChangedHandler(StringCollection OldSelections, StringCollection NewSelections);
        public delegate void FactorialSelectionChangedHandler(string OldSelection, string NewSelection);

        public event NotifyEventHandler SelectionChangingEvent;
        // Fired when the current selection is about to change.
        public event SelectionChangedHandler SelectionChangedEvent;
        // Fired when the current selection has changed.
        public event FactorialSelectionChangedHandler FactorialSelectionChangedEvent;
        // Fired when the current selection has changed.


        public BaseController(Form MainForm, string SectionName, bool MainController)
        {
            // -----------------------
            // constructor
            // -----------------------
            MyMainForm = MainForm;

            ApsimFile.Configuration.Instance.ApplicationName = SectionName;
            ApsimData = new ApsimFile.ApsimFile();

            // Setup the image lists.
            LargeIcons = new ImageList();
            MediumIcons = new ImageList();
            SmallIcons = new ImageList();
            LargeIcons.ImageSize = new Size(32, 32);
            MediumIcons.ImageSize = new Size(24, 24);
            SmallIcons.ImageSize = new Size(16, 16);
            LargeIcons.Tag = "LargeIcon";
            MediumIcons.Tag = "MediumIcon";
            SmallIcons.Tag = "SmallIcon";
            ImageFileNames = new List<string>();
            ImageIndexes = new List<int>();

            // Setup the types
            foreach (string FileName in Types.Instance.SmallIconFileNames())
            {
                LoadIcon(FileName, ref SmallIcons);
            }
            foreach (string FileName in Types.Instance.LargeIconFileNames())
            {
                LoadIcon(FileName, ref LargeIcons);
            }

            // Setup the actions file and load all images specified by it.
            string ActionFileName = Configuration.Instance.Setting("ActionFile");
            if (!string.IsNullOrEmpty(ActionFileName))
            {
                XmlDocument ActionDoc = new XmlDocument();
                ActionDoc.Load(ActionFileName);
                ActionFile = ActionDoc.DocumentElement;
                XmlNode ActionsNode = XmlHelper.Find(ActionDoc.DocumentElement, "Actions");
                List<string> SmallIconFileNames = XmlHelper.ValuesRecursive(ActionsNode, "SmallIcon");
                foreach (string IconFileName in SmallIconFileNames)
                {
                    LoadIcon(IconFileName, ref SmallIcons);
                }
                List<string> MediumIconFileNames = XmlHelper.ValuesRecursive(ActionsNode, "MediumIcon");
                foreach (string IconFileName in MediumIconFileNames)
                {
                    LoadIcon(IconFileName, ref MediumIcons);
                }

            }

            if (MainController)
            {
                ApsimData.FileNameChanged += OnFileNameChanged;
            }
        }
        public BaseView CreateUI(string ComponentType)
        {
            // -------------------------------------
            // Create a User interface form for the
            // specified type.
            // -------------------------------------
            string UIType = Types.Instance.MetaData(ComponentType, "UItype");
            if (!string.IsNullOrEmpty(UIType))
            {
                return (BaseView)CreateClass(UIType);
            }
            return null;
        }
        public Form MainForm
        {
            get { return MyMainForm; }
        }
        public ExplorerUI Explorer
        {
            get { return (MyExplorer); }
            set { MyExplorer = value; }
        }
        public bool FactorialMode
        {
            get { return (MyFactorialMode); }
            set
            {
                MyFactorialMode = value;
                if (MyFactorialMode && (ApsimData.FactorComponent == null))
                {
                    ApsimData.CreateFactorComponent();
                    //should trigger an onRefresh to theinterface
                }

                MyExplorer.RefreshDisplayMode();

            }
        }

        #region "Image handling"
        private void LoadIcon(string ImageFileName, ref ImageList Icons)
        {
            // Load the specified icon into the specified imagelist. The filename
            // of the icon is stored in the tag field of the bitmap so that we
            // can look for it later.
            ImageFileName = Configuration.RemoveMacros(ImageFileName);
            if (File.Exists(ImageFileName) && (ImageFileNames.IndexOf(ImageFileName) == -1))
            {
                Bitmap Icon = new Bitmap(ImageFileName);
                Icons.Images.Add(Icon);

                ImageFileNames.Add(ImageFileName);
                ImageIndexes.Add(Icons.Images.Count - 1);
            }
        }
        public ImageList ImageList(string ImageType)
        {
            // Return an image list to caller. ImageType can be SmallIcon,
            // MediumIcon or LargeIcon.
            if (ImageType == "SmallIcon")
            {
                return SmallIcons;
            }
            else if (ImageType == "MediumIcon")
            {
                return MediumIcons;
            }
            else
            {
                return LargeIcons;
            }
        }
        public int ImageIndex(string TypeName, string ImageType)
        {
            string ImageFileName = Types.Instance.MetaData(TypeName, ImageType);
            return ConvertImageFileNameToIndex(ImageFileName, ImageType);
        }
        public int ConvertImageFileNameToIndex(string ImageFileName, string ImageType)
        {
            ImageFileName = Configuration.RemoveMacros(ImageFileName);
            for (int I = 0; I <= ImageFileNames.Count - 1; I++)
            {
                if (ImageFileNames[I] == ImageFileName)
                {
                    return ImageIndexes[I];
                }
            }
            return -1;
        }

        #endregion

        #region "File handling methods"
        public bool FileSaveAfterPrompt()
        {
            // --------------------------------------------------------
            // Often called at program exit to optionally prompt the
            // user to save the current data if something has changed.
            // --------------------------------------------------------
            Explorer.SaveCurrentView();
            if (ApsimData.IsDirty)
            {
                DialogResult DoSave = MessageBox.Show("The current file has changed. Do you want to save it before proceeding?", "Save?", MessageBoxButtons.YesNoCancel, MessageBoxIcon.Question);
                switch (DoSave)
                {
                    case DialogResult.Yes:
                        // Save the file
                        if (ApsimData.FileName.Contains("Untitled"))
                        {
                            SaveFileDialog Dialog = new SaveFileDialog();
                            Dialog.Filter = Configuration.Instance.Setting("DialogFilter");
                            Dialog.DefaultExt = Configuration.Instance.Setting("DefaultExtension");
                            Dialog.AddExtension = true;
                            Dialog.OverwritePrompt = true;
                            if (Dialog.ShowDialog() == DialogResult.OK)
                            {
                                Explorer.SaveCurrentView();
                                ApsimData.SaveAs(Dialog.FileName);
                                RefreshToolStrips();
                            }
                        }
                        else
                        {
                            ApsimData.Save();
                        }

                        break;
                    case DialogResult.No:
                        break;
                    // Do not save

                    case DialogResult.Cancel:
                        // Cancel pressed.
                        return false;
                }
            }
            return true;
        }
        public void LoadPreviousFile()
        {
            foreach (string PreviousFileName in Configuration.Instance.GetFrequentList())
            {
                if (File.Exists(PreviousFileName))
                {
                    ApsimData.OpenFile(PreviousFileName);
                    break; // TODO: might not be correct. Was : Exit For
                }
            }
        }
        private void OnFileNameChanged(string FileName)
        {
            if (FileName.ToLower() != "untitled")
            {
                Configuration.Instance.AddFileToFrequentList(FileName);
            }
            ClearRunStrip();
            RefreshToolStrips();
        }
        private void ClearRunStrip()
        {
            Control[] RunPanels = MainForm.Controls.Find("RunToolStrip", true);
            if (RunPanels.Length == 1)
            {
                ToolStrip Strip = (System.Windows.Forms.ToolStrip)RunPanels[0];
                ToolStripButton ErrorsButton = (ToolStripButton)Strip.Items["ErrorsButton"];
                ToolStripLabel PercentLabel = (ToolStripLabel)Strip.Items["PercentLabel"];
                ToolStripProgressBar ProgressBar = (ToolStripProgressBar)Strip.Items["RunProgress"];

                ErrorsButton.Visible = false;
                PercentLabel.Text = "";
                ProgressBar.Value = 0;
            }
        }
        public string ToRelativePath(string FileName)
        {
            string st = Configuration.AddMacros(FileName);
            string DName = Path.GetDirectoryName(ApsimData.FileName);
            if (!string.IsNullOrEmpty(DName))
            {
                return st.Replace(DName + Path.DirectorySeparatorChar, "");
            }
            else
            {
                return FileName;
            }
        }
        public string ToAbsolute(string FileName)
        {
            if ((FileName == null))
            {
                return FileName;
            }
            else
            {
                string st = Configuration.RemoveMacros(FileName);
                if (!string.IsNullOrEmpty(Path.GetDirectoryName(ApsimData.FileName)))
                {
                    string CWD = Directory.GetCurrentDirectory();
                    Directory.SetCurrentDirectory(Path.GetDirectoryName(ApsimData.FileName));
                    st = Path.GetFullPath(st);
                    Directory.SetCurrentDirectory(CWD);
                }
                return st;
            }
        }

        #endregion

        #region "Node selection methods. NB: Paths are delimited with '/' characters"
        public StringCollection SelectedPaths
        {

            get
            {
                // --------------------------------------------------------
                // Provide readwrite access to the current selections.
                // The strings returned contain data paths to all
                // selected nodes.
                // --------------------------------------------------------
                //temporary collection used to store selected paths

                StringCollection ReturnValues = new StringCollection();
                //if the selected path is nothing, then set it to the root component in the .apsim file.
                if (MySelectedData == null)
                {
                    ReturnValues.Add(ApsimData.RootComponent.FullPath);
                }
                else
                {
                    //store selected paths in temporary collection
                    foreach (string FullPath in MySelectedData)
                    {
                        ReturnValues.Add(FullPath);
                    }
                }
                return ReturnValues;
            }


            set
            {
                StringCollection OldSelections = new StringCollection();
                //temporary collection used to store old selected paths

                //store old selected paths in temporary collection
                foreach (string Selection in MySelectedData)
                {
                    OldSelections.Add(Selection);
                }

                // Are the new selected paths different from the old selected paths?
                //quick test (based on number of paths)
                bool Different = OldSelections.Count != value.Count;
                //exhaustive test 
                if (!Different)
                {
                    for (int i = 0; i <= OldSelections.Count - 1; i++)
                    {
                        //if Different is set to true once in the loop, make sure Different is permanently set to true.
                        Different = Different || (OldSelections[i] != value[i]);
                        //does each path in the old selected paths, match their new equivalent in the new selected paths  
                    }
                }

                // If they are different
                if (Different)
                {
                    if (SelectionChangingEvent != null)
                    {
                        SelectionChangingEvent();
                    }
                    MySelectedData = value;
                    //set "the controllers" selected paths to the new selected paths.
                    if (SelectionChangedEvent != null)
                    {
                        SelectionChangedEvent(OldSelections, MySelectedData);
                    }
                    //calls OnSelectionChanged() -> in DataTree.vb and in ExplorerUI.vb 
                    RefreshToolStrips();
                    //IMPORTANT 'See first sub in Action Region below. This region is where all the toolstrips get rebinded after the selection is changed. This first sub is what starts all this rebinding.
                }
            }
        }

        public string SelectedPath
        {
            get
            {
                if ((MySelectedData.Count == 1))
                {
                    return MySelectedData[0];
                }
                else
                {
                    throw new Exception("Too many nodes selected - expected 1");
                }
            }
            set
            {
                StringCollection NewSelections = new StringCollection();
                if (!string.IsNullOrEmpty(value))
                {
                    NewSelections.Add(value);
                }
                SelectedPaths = NewSelections;
            }
        }
        public ApsimFile.Component Selection
        {
            //return the corresponding component to the node that was selected.
            get { return ApsimData.Find(SelectedPath); }
        }
        #endregion
        #region "Factorial Selection Methods"
        public string SelectedFactorialPath
        {
            get { return MySelectedFactorialPath; }

            set
            {
                if (MySelectedFactorialPath != value)
                {
                    string oldSelection = MySelectedFactorialPath;
                    MySelectedFactorialPath = value;
                    if (FactorialSelectionChangedEvent != null)
                    {
                        FactorialSelectionChangedEvent(oldSelection, MySelectedFactorialPath);
                    }
                    //calls OnSelectionChanged() -> in DataTree.vb and in ExplorerUI.vb 
                    //RefreshToolStrips()     'IMPORTANT 'See first sub in Action Region below. This region is where all the toolstrips get rebinded after the selection is changed. This first sub is what starts all this rebinding.
                }
            }
        }
        public ApsimFile.Component FactorialSelection
        {
            //return the corresponding component to the node that was selected.
            //Return FindFactorialComponent(SelectedFactorialPath)
            get { return ApsimData.Find(SelectedFactorialPath); }
        }
        public ApsimFile.Component FindFactorialComponent(string path)
        {
            int pos = path.IndexOf(ApsimFile.Component.Delimiter);
            if ((pos == -1))
            {
                return ApsimData.FactorComponent;
            }

            string adjPath = path.Substring(pos + 1);
            return ApsimData.FactorComponent.Find(adjPath);
            //return the corresponding component to the node that was selected.
        }
        #endregion

        #region "Action methods"


        // --------------------------------------------------------------
        // This method creates a context menu for a UI then returns it.
        // The difference between this and ProvideToolStrip() is this does not add itself to the ToolStrips local variable (which is required for RefreshToolStrips).
        // The context menus belonging to the UI's don't need the same enabling/disabling of actions depening on the current node
        // that is selected, like the MainToolBar and the DataTree context menu do. They don't need the RefreshToolStrips()
        // This is because if a different node is selected the UI changes and hence the context menu is no longer relavent for the new UI.
        // -------------------------------------------------------------

        public ContextMenuStrip CreateUIContextMenu(string ToolStripName)
        {
            ContextMenuStrip Strip = new ContextMenuStrip();
            Strip.ItemClicked += ActionOnClick;
            //add an event handler to the entire strip NOT to individual items.
            Strip.Name = ToolStripName;
            XmlNode ToolStripDescriptor = XmlHelper.Find(ActionFile, ToolStripName);
            if ((ToolStripDescriptor != null))
            {
                bool ImageAboveText = false;
                ImageAboveText = (XmlHelper.Attribute(ToolStripDescriptor, "ImageAboveText") == "yes");
                PopulateToolStrip(Strip, ToolStripDescriptor, ImageAboveText);
            }
            return Strip;
        }


        // ------------------------------------------------------------------------
        // These action methods populate and implement the Context Menu and MainToolBar
        // ToolStrip in the application. All actions are defined in an actions.xml
        // file. Each type in types.xml has zero or more actions in it. When the
        // user clicks on a node in the tree, the context menu will contain those
        // actions referred to in the types.xml under the type the user clicked on.
        //
        // By "Context Menu" we mean the DataTree context menu. See section in actions.xml called <ContextMenu>.
        // Different types of nodes have different actions disabled. So the controller
        // stores the context menu and simply enables and disables the actions each
        // time a different node is selected in the datatree. RefreshToolStrips is
        // called to do this. See above in SelectedPaths() "Set" to see where RefreshToolStrips 
        // is called when a selection is made. So when the user selects the node by right mouse 
        // clicking on it, the context menu gets the correct actions enabled/disabled before the
        // context menu is displayed.
        //
        // The toolstrip across the top of MainForm works the exact same way. RefreshToolStrips
        // also is used to enable/disable the actions depending on which node is selected.
        // For example, the Run button is sometimes disabled.
        // ------------------------------------------------------------------------

        //this is the Controllers list of toolstrips. (eg. MainToolBar, ContextMenu)
        private List<ToolStrip> ToolStrips = new List<ToolStrip>();
        //see bottom of Actions.xml for a list of all Toolstrips in ApsimUI. Anything NOT inside the <Actions> tags is a toolstrip.

        //Refresh what actions are enabled/disabled depending on what node in the datatree is selected.
        public void RefreshToolStrips()
        {
            foreach (ToolStrip Strip in ToolStrips)
            {
                EnableActions(Strip);
                //does the enable/disable of items in toolstrip
                XmlNode ToolStripDescriptor = XmlHelper.Find(ActionFile, Strip.Name);
                //seach action.xml for the name of the strip (really only interested in MainToolbar because that is the only one with RecentFileList child but one day someone may use it in the context menu as well)
                RefreshRecentFileList(Strip, ToolStripDescriptor);
                //refresh the list of previously opened files as well
            }
        }
        private void EnableActions(ToolStrip Strip)
        {
            foreach (ToolStripItem ToolItem in Strip.Items)
            {
                if ((ToolItem.Tag != null))
                {
                    ToolItem.Enabled = IsActionAllowed(XmlHelper.Find(ActionFile, "/Folder/Actions/" + ToolItem.Tag.ToString()));
                }
                //Does this toolstrip item have a sub menu (such as "Open") 
                if (ToolItem is ToolStripDropDownItem)
                {
                    ToolStripDropDownItem DropDownItem = (ToolStripDropDownItem)ToolItem;
                    //recast item as a sub menu item
                    ToolItem.Enabled = IsActionAllowed(XmlHelper.Find(ActionFile, "/Folder/Actions/" + ToolItem.Tag.ToString()));
                    EnableActions(DropDownItem.DropDown);
                    //recursion call, to enable/disable actions in the sub menu.
                }
            }
        }
        private void RefreshRecentFileList(ToolStrip Strip, XmlNode ToolStripDescriptor)
        {
            int ItemIndex = 0;
            //Walk through the ToolStripDescriptor until you find a RecentFileList Child
            foreach (XmlNode ChildDescriptor in XmlHelper.ChildNodes(ToolStripDescriptor, ""))
            {
                //Step over any items
                if (XmlHelper.Type(ChildDescriptor) == "Item")
                {
                    ItemIndex = ItemIndex + 1;
                    //If you find one, Refresh 
                }
                else if (XmlHelper.Type(ChildDescriptor) == "RecentFileList")
                {
                    // strip off all old ones.
                    while (!(Strip.Items[Strip.Items.Count - 1] is ToolStripSeparator))
                    {
                        Strip.Items.RemoveAt(Strip.Items.Count - 1);
                    }
                    foreach (string FileName in Configuration.Instance.GetFrequentList())
                    {
                        ToolStripItem Item = Strip.Items.Add(FileName);
                        Item.ImageIndex = -1;
                        Item.Tag = "";
                        Item.ToolTipText = "Open this file";
                    }
                    //Step into any DropDownItems to look for RecentFileLists in submenu
                }
                else if ((XmlHelper.Type(ChildDescriptor) == "DropDownItem") && (Strip.Items[ItemIndex] is ToolStripDropDownItem))
                {
                    ToolStripDropDownItem DropDownButton = (ToolStripDropDownItem)Strip.Items[ItemIndex];
                    RefreshRecentFileList(DropDownButton.DropDown, ChildDescriptor);
                    //recursion call
                    ItemIndex = ItemIndex + 1;
                }
            }
        }

        //Create a new toolstrip but does not return it instead it adds it to the controllers list of toolstrips.  
        //(so it will have its items enabled/disabled depending whenever a different node in the datatree is selected) 
        //To see where toolstrips are created do a "Find All References" on "ProvideToolStrip"
        public void ProvideToolStrip(ToolStrip Strip, string ToolStripName)
        {
            // --------------------------------------------------------------
            // This method populates the specified context menu for the 
            // currently selected type. 
            // --------------------------------------------------------------
            Strip.Items.Clear();
            //get rid of the old items out of the toolstrip you are going to repopulate.
            Strip.ItemClicked += ActionOnClick;
            //add an event handler to the entire strip NOT to individual items.
            Strip.Name = ToolStripName;
            XmlNode ToolStripDescriptor = XmlHelper.Find(ActionFile, ToolStripName);
            if ((ToolStripDescriptor != null))
            {
                bool ImageAboveText = false;
                ImageAboveText = (XmlHelper.Attribute(ToolStripDescriptor, "ImageAboveText") == "yes");
                PopulateToolStrip(Strip, ToolStripDescriptor, ImageAboveText);
            }
            ToolStrips.Add(Strip);
            //add this newly created toolstrip to the Controllers list of toolstrips
        }
        //Removes it from the controllers list of toolstrips
        public void RemoveToolStrip(ToolStrip Strip)
        {
            ToolStrips.Remove(Strip);
        }
        private void PopulateToolStrip(ToolStrip Strip, XmlNode ToolStripDescriptor, bool ImageAboveText)
        {
            // --------------------------------------------------------------
            // This method populates the specified context menu given the
            // specified descriptor data. 
            // --------------------------------------------------------------

            foreach (XmlNode ChildDescriptor in XmlHelper.ChildNodes(ToolStripDescriptor, ""))
            {
                if (XmlHelper.Type(ChildDescriptor) == "ImageSize")
                {
                    Strip.ImageList = ImageList(ChildDescriptor.InnerText);

                }
                else if (XmlHelper.Type(ChildDescriptor) == "Item")
                {
                    ToolStripItem Item = CreateToolStripItem(Strip, ChildDescriptor.InnerText);
                    if (ImageAboveText)
                    {
                        Item.TextImageRelation = TextImageRelation.ImageAboveText;
                    }

                }
                else if (XmlHelper.Type(ChildDescriptor) == "DropDownItem")
                {
                    string DropDownActionName = XmlHelper.Attribute(ChildDescriptor, "action");
                    XmlNode Action = XmlHelper.Find(ActionFile, "/Folder/Actions/" + DropDownActionName);
                    ToolStripDropDownItem DropDownButton = null;
                    ToolStripDropDown DropDownStrip = null;
                    if (Strip.GetType().ToString() == "System.Windows.Forms.ToolStrip")
                    {
                        DropDownButton = new ToolStripDropDownButton();
                        DropDownStrip = new ToolStripDropDownMenu();
                    }
                    else
                    {
                        DropDownButton = new ToolStripMenuItem();
                        DropDownStrip = new ToolStripDropDownMenu();
                    }
                    DropDownButton.Text = XmlHelper.Value(Action, "text");
                    DropDownButton.ImageIndex = ImageIndexForAction(Action, Strip.ImageList.Tag.ToString());
                    DropDownButton.ImageScaling = ToolStripItemImageScaling.None;
                    DropDownButton.AutoToolTip = false;
                    DropDownButton.Tag = DropDownActionName;
                    DropDownButton.Enabled = IsActionAllowed(Action);
                    if (ImageAboveText)
                    {
                        DropDownButton.TextImageRelation = TextImageRelation.ImageAboveText;
                    }
                    DropDownStrip.ImageList = ImageList("SmallIcon");
                    DropDownStrip.ItemClicked += ActionOnClick;
                    //add a handler
                    PopulateToolStrip(DropDownStrip, ChildDescriptor, false);
                    //recursion call
                    DropDownButton.DropDown = DropDownStrip;
                    Strip.Items.Add(DropDownButton);

                }
                else if (XmlHelper.Type(ChildDescriptor) == "Separator")
                {
                    Strip.Items.Add(new ToolStripSeparator());

                }
                else if (XmlHelper.Type(ChildDescriptor) == "RecentFileList")
                {
                    foreach (string FileName in Configuration.Instance.GetFrequentList())
                    {
                        ToolStripItem Item = Strip.Items.Add(FileName);
                        Item.ImageIndex = -1;
                        Item.Tag = "";
                        Item.ToolTipText = "Open this file";
                        if (ImageAboveText)
                        {
                            Item.TextImageRelation = TextImageRelation.ImageAboveText;
                        }
                    }
                }
            }
        }
        private ToolStripItem CreateToolStripItem(ToolStrip Strip, string ActionName)
        {
            XmlNode Action = XmlHelper.Find(ActionFile, "/Folder/Actions/" + ActionName);
            if ((Action != null))
            {
                ToolStripItem Item = Strip.Items.Add(XmlHelper.Value(Action, "text"));
                Item.Name = XmlHelper.Value(Action, "text");
                Item.ToolTipText = XmlHelper.Value(Action, "description");
                Item.Tag = ActionName;
                Item.ImageIndex = ImageIndexForAction(Action, Strip.ImageList.Tag.ToString());
                Item.ImageScaling = ToolStripItemImageScaling.None;
                string ShortCut = XmlHelper.Value(Action, "shortcut");
                if (Item is ToolStripMenuItem && !string.IsNullOrEmpty(ShortCut))
                {
                    ((ToolStripMenuItem)Item).ShortcutKeys = (Keys)Keys.Parse(typeof(Keys), ShortCut);
                }
                Item.Enabled = IsActionAllowed(Action);
                return Item;
            }
            else
            {
                return null;
            }
        }
        public int ImageIndexForAction(XmlNode Node, string ImageType)
        {
            // The node passed in will be an action node i.e. <FileNew>. This method needs to 
            // find the child element as specified by ImageType (e.g. <SmallIcon>) and then
            // convert the icon filename into an index.
            if (Node != null)
            {
                string IconFileName = XmlHelper.Value(Node, ImageType);
                return ConvertImageFileNameToIndex(IconFileName, ImageType);
            }
            return -1;
        }
        private bool IsActionAllowed(XmlNode Action)
        {
            //need to pass the strip so you can get the parent, to see if toolstrip belongs to an apsim toolbox (needed for "InToolbox" test)
            bool Allowed = true;
            if ((Action != null))
            {
                foreach (XmlNode DisabledWhen in XmlHelper.ChildNodes(Action, "DisabledWhen"))
                {
                    string DisabledWhenFlag = DisabledWhen.InnerText;
                    if (DisabledWhenFlag == "ReadOnly" && ApsimData.IsReadOnly)
                    {
                        Allowed = false;
                    }
                    if (DisabledWhenFlag == "RootNode" && MySelectedData.Count > 0 && MySelectedData[0].LastIndexOf("/") == 0)
                    {
                        Allowed = false;
                    }
                    if (DisabledWhenFlag == "MultipleNodesSelected" && MySelectedData.Count > 1)
                    {
                        Allowed = false;
                    }
                    if (DisabledWhenFlag == "NothingLoaded" && ApsimData.FileName == "Untitled")
                    {
                        Allowed = false;
                    }
                    if (DisabledWhenFlag == "NotShortcut" && MySelectedData.Count == 1 && (Selection != null) && Selection.ShortCutTo == null)
                    {
                        Allowed = false;
                    }
                    if (DisabledWhenFlag == "Enabled" && MySelectedData.Count == 1 && (Selection != null) && Selection.Enabled)
                    {
                        Allowed = false;
                    }
                    if (DisabledWhenFlag == "Disabled" && MySelectedData.Count == 1 && (Selection != null) && !Selection.Enabled)
                    {
                        Allowed = false;
                    }
                    //check if the explorerUI for this controller is the explorerUI for the toolbox.
                    if (DisabledWhenFlag == "InToolbox" && (this.Explorer != null) && (this.Explorer.Name == "ToolboxExplorer"))
                    {
                        Allowed = false;
                    }
                    //I think this is to disable once you have clicked on it. 
                    //Eg. Once you click the "Run" button disable it, while it is running to stop them clicking on it again. Once it is finished you can enable it again.
                    if ((XmlHelper.Find(DisabledWhen, "call") != null))
                    {
                        List<object> Arguments = new List<object>();
                        Arguments.Add(this);
                        Allowed = !(bool)CSGeneral.CallDll.CallMethodOfClass(XmlHelper.Find(DisabledWhen, "call"), Arguments);
                    }
                }

                if ((MySelectedData != null) && Allowed && MySelectedData.Count == 1 && XmlHelper.ChildNodes(Action, "AppliesTo").Count > 0)
                {
                    Allowed = false;
                    if ((Selection != null))
                    {
                        string SelectedType = Selection.Type;
                        foreach (XmlNode AppliesTo in XmlHelper.ChildNodes(Action, "AppliesTo"))
                        {
                            if (AppliesTo.InnerText.ToLower() == SelectedType.ToLower())
                            {
                                Allowed = true;
                            }
                        }
                    }
                }
            }
            return Allowed;
        }

        private void ActionOnClick(object Sender, ToolStripItemClickedEventArgs E)
        {
            // --------------------------------------------------------------
            // When the user clicks on an action, this method is called.
            // Go perform whatever action is necessary.
            // --------------------------------------------------------------
            try
            {
                if ((E.ClickedItem.Tag != null))
                {
                    if (string.IsNullOrEmpty((string)E.ClickedItem.Tag) && FileSaveAfterPrompt())
                    {
                        //if the user clicks on one of the mru files listed under the open icon on the toolbar
                        //then it falls through here...
                        Explorer.CloseUI();
                        ApsimData.OpenFile(E.ClickedItem.Text);
                    }
                    else
                    {
                        InvokeAction(Sender, (string)E.ClickedItem.Tag);
                    }
                }
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        public void InvokeAction(object Sender, string ActionName)
        {
            //close the menu
            // For some reason, if we don't explicitly close the menus they remain open,
            // and on top of other windows.
            if (Sender is ContextMenuStrip)
            {
                ((ContextMenuStrip)Sender).Close();
            }
            else if (Sender is ToolStripDropDownMenu)
            {
                ToolStripDropDownMenu Menu = (ToolStripDropDownMenu)Sender;
                if ((Menu.OwnerItem != null) && (Menu.OwnerItem.Owner != null) && Menu.OwnerItem.Owner is ContextMenuStrip)
                {
                    ((ContextMenuStrip)Menu.OwnerItem.Owner).Close();
                }
                Menu.Close();
            }

            //call the method to carry out the action
            XmlNode ActionInvoke = XmlHelper.Find(ActionFile, "/Folder/Actions/" + ActionName + "/OnInvoke/Call");
            //look up the <class> and <method> for the action in the Actions.xml
            if ((ActionInvoke != null))
            {
                List<object> Arguments = new List<object>();
                Arguments.Add(this);
                //pass the Controller as a parameter to the method
                CallDll.CallMethodOfClass(ActionInvoke, Arguments);
                //parameters for this are an XmlNode (with a <class> and <method> tag) and a list of Objects (which are the paramters for the method specified in the XmlNode)
            }
        }
        public static object CreateClass(string ClassToCall)
        {
            // --------------------------------------------------------------
            // Call a static/shared method of a class to perform the 
            // necessary action.
            // --------------------------------------------------------------
            try
            {
                int PosPeriod = ClassToCall.IndexOf(".");
                if (PosPeriod == -1)
                {
                    throw new Exception("No namespace specified in action: " + ClassToCall);
                }
                string NameSpaceName = ClassToCall.Substring(0, PosPeriod);
                Type t = null;
                foreach (Assembly Assemb in AppDomain.CurrentDomain.GetAssemblies())
                {
                    if (NameSpaceName.ToLower() == Assemb.GetName().Name.ToLower())
                    {
                        t = Assemb.GetType(ClassToCall, true, true);
                    }
                }
                if ((t == null))
                {
                    throw new Exception("Cannot find type: " + ClassToCall);
                }
                return Activator.CreateInstance(t);

            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.GetBaseException().Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation);
            }
            return null;
        }

        #endregion

    }
}
