
using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.ComponentModel;
using System.ComponentModel.Design;
using System.Data;
using System.Drawing;
using System.Drawing.Design;
using System.IO;
using System.Reflection;
using System.Text;
using System.Windows.Forms;
using System.Xml;

using Steema.TeeChart;

using ApsimFile;
using Controllers;
using CSGeneral;
using CSUserInterface;
using VBUserInterface;  //Grid.vb
using UIUtility;
using UIBits;           //InputDialog


namespace GraphUserInterface
   {
   public partial class ChartPageUI : BaseView
      {
      private DataProcessor DataProcessor;
      private ArrowRenderer Arrow = new ArrowRenderer(10, 1f, true);
      enum PageMode { Normal, Arrow, Design };
      PageMode Mode;
      Greatis.FormDesigner.Toolbox Toolbox = new Greatis.FormDesigner.Toolbox();

      public ChartPageUI()
         {
         InitializeComponent();
         IMenuCommandService mcs = (IMenuCommandService)FormDesigner.DesignerHost.GetService(typeof(IMenuCommandService));
         mcs.AddVerb(new DesignerVerb("Delete", new EventHandler(OnRemoveMenuItem), StandardCommands.VerbFirst));
         mcs.AddVerb(new DesignerVerb("Rename", new EventHandler(OnRenameMenuItem), StandardCommands.VerbLast));
         mcs.AddVerb(new DesignerVerb("Exit edit mode", new EventHandler(OnNormalClick), StandardCommands.VertSpaceConcatenate));

         Toolbox.ToolboxCtrl.Designer = FormDesigner;
         Toolbox.ToolboxCtrl.AddToolboxItem(CreateToolboxItem(typeof(ApsimFileReaderUI), "ApsimFileReader"), "Data");
         Toolbox.ToolboxCtrl.AddToolboxItem(CreateToolboxItem(typeof(ApsimFileReaderUI), "XmlFileReader"), "Data");
         Toolbox.ToolboxCtrl.AddToolboxItem(CreateToolboxItem(typeof(FieldListUI), "Probability"), "Data");
         Toolbox.ToolboxCtrl.AddToolboxItem(CreateToolboxItem(typeof(FilterUI), "Filter"), "Data");
         Toolbox.ToolboxCtrl.AddToolboxItem(CreateToolboxItem(typeof(EmptyGraphUI), "Cumulative"), "Data");
         Toolbox.ToolboxCtrl.AddToolboxItem(CreateToolboxItem(typeof(EmptyGraphUI), "Depth"), "Data");
         Toolbox.ToolboxCtrl.AddToolboxItem(CreateToolboxItem(typeof(FieldListUI), "Diff"), "Data");
         Toolbox.ToolboxCtrl.AddToolboxItem(CreateToolboxItem(typeof(FrequencyUI), "Frequency"), "Data");
         Toolbox.ToolboxCtrl.AddToolboxItem(CreateToolboxItem(typeof(FieldListUI), "KWTest"), "Data");
         Toolbox.ToolboxCtrl.AddToolboxItem(CreateToolboxItem(typeof(RegressionUI), "Regression"), "Data");
         Toolbox.ToolboxCtrl.AddToolboxItem(CreateToolboxItem(typeof(PredObsUI), "PredObs"), "Data");
         Toolbox.ToolboxCtrl.AddToolboxItem(CreateToolboxItem(typeof(StatsUI), "Stats"), "Data");
         Toolbox.ToolboxCtrl.AddToolboxItem(CreateToolboxItem(typeof(SOIUI), "SOIData"), "Data");
         Toolbox.ToolboxCtrl.AddToolboxItem(CreateToolboxItem(typeof(RecordFilterUI), "RecordFilter"), "Data");
         Toolbox.ToolboxCtrl.AddToolboxItem(CreateToolboxItem(typeof(REMSUI), "REMS"), "Data");
         Toolbox.ToolboxCtrl.AddToolboxItem(CreateToolboxItem(typeof(EmptyGraphUI), "Joiner"), "Data");
         Toolbox.ToolboxCtrl.AddToolboxItem(CreateToolboxItem(typeof(FieldListUI), "SeriesSplitter"), "Data");
         Toolbox.ToolboxCtrl.AddToolboxItem(CreateToolboxItem(typeof(XYPickerUI), "XY"), "Visual");
         Toolbox.ToolboxCtrl.AddToolboxItem(CreateToolboxItem(typeof(ChartUI), "Chart"), "Visual");
         Toolbox.ToolboxCtrl.AddToolboxItem(CreateToolboxItem(typeof(DataGridUI), "DataGrid"), "Visual");
         Toolbox.TopMost = true;
         Toolbox.ToolboxCtrl.AutoSize = true;
         Toolbox.AutoSize = true;
         Toolbox.FormBorderStyle = FormBorderStyle.SizableToolWindow;
         Toolbox.Height = 500;
         Toolbox.Width = 120;
         }
      private ToolboxItem CreateToolboxItem(Type t, string DisplayName)
         {
         ToolboxItem Item = new ToolboxItem(t);
         Item.DisplayName = DisplayName;
         return Item;
         }
      public DataProcessor Processor
         {
         get { return DataProcessor; }
         set { DataProcessor = value; }
         }

      protected override void OnLoad()
         {
         Mode = PageMode.Normal;

         // some paths (e.g. in ApsimFileReaders) are relative to the 
         // directory where the .apsim is stored. Make sure the 
         // current directory is set appropriately.
         if (Controller.ApsimData.FileName.IndexOf('\\') != -1)
            Directory.SetCurrentDirectory(Path.GetDirectoryName(Controller.ApsimData.FileName));

         // Try and populate an outputfile  child object with filenames from our parent's siblings.
         ApsimFile.Component Parent = Controller.ApsimData.Find(NodePath + "/..");
         XmlNode OutputFileChild = XmlHelper.Find(Data, "outputfile");
         if (Parent != null && OutputFileChild != null)
            {
            string CSVFileNames = UIUtility.OutputFileUtility.GetCSVListOfOutputFiles(Controller);
            string[] OutputFileNames = CSVFileNames.Split(',');
            List<string> Files = new List<string>();
            foreach (string File in OutputFileNames)
               Files.Add(File);
            XmlHelper.SetValues(OutputFileChild, "FileName", Files);
            }
         // Position ourselves
         if (XmlHelper.Attribute(Data, "Left") != "")
            Left = Convert.ToInt32(XmlHelper.Attribute(Data, "Left"));
         if (XmlHelper.Attribute(Data, "Top") != "")
            Top = Convert.ToInt32(XmlHelper.Attribute(Data, "Top"));
         if (XmlHelper.Attribute(Data, "Width") != "")
            Width = Convert.ToInt32(XmlHelper.Attribute(Data, "Width"));
         if (XmlHelper.Attribute(Data, "Height") != "")
            Height = Convert.ToInt32(XmlHelper.Attribute(Data, "Height"));

         // Go create all child controls.
         Controls.Clear();
         foreach (XmlNode Child in XmlHelper.ChildNodes(Data, ""))
            Add(Child);
         }
      private void Add(XmlNode NewComponent)
         {
         BaseView View = null;

         string UIType = Types.Instance.MetaData(XmlHelper.Type(NewComponent), "uitype");
         if (UIType != "")
            View = (BaseView)BaseController.CreateClass(UIType);

         if (View != null)
            {
            View.Name = XmlHelper.Name(NewComponent);

            View.Parent = this;
            View.ViewChanged += OnViewChanged;
            View.OnLoad(Controller, XmlHelper.FullPath(NewComponent), NewComponent.OuterXml);

            if (XmlHelper.Attribute(NewComponent, "Left") != "")
               View.Left = Convert.ToInt32(XmlHelper.Attribute(NewComponent, "Left"));
            if (XmlHelper.Attribute(NewComponent, "Top") != "")
               View.Top = Convert.ToInt32(XmlHelper.Attribute(NewComponent, "Top"));
            if (XmlHelper.Attribute(NewComponent, "Width") != "")
               View.Width = Convert.ToInt32(XmlHelper.Attribute(NewComponent, "Width"));
            if (XmlHelper.Attribute(NewComponent, "Height") != "")
               View.Height = Convert.ToInt32(XmlHelper.Attribute(NewComponent, "Height"));
            View.Visible = (XmlHelper.Attribute(NewComponent, "Visible").ToLower() != "no");
            if (!Processor.FromApsimReport)
               Processor.Add(NewComponent.OuterXml);
            }
         }

      public override void OnRefresh()
         {
         // -----------------------------------------------
         // Called when it's time to refresh the canvas and
         // everything on it.
         // -----------------------------------------------
         foreach (BaseView View in Controls)
            View.OnRefresh();
         }
      public override void OnSave()
         {
         // -----------------------------------------------
         // Called when it's time to save everything back
         // to XML
         // -----------------------------------------------
         OnNormalClick(null, null);
         if (Data != null)
            {
            XmlHelper.SetAttribute(Data, "Left", Left.ToString());
            XmlHelper.SetAttribute(Data, "Top", Top.ToString());
            XmlHelper.SetAttribute(Data, "Width", Width.ToString());
            XmlHelper.SetAttribute(Data, "Height", Height.ToString());
            XmlDocument Doc = new XmlDocument();
            int MinimumTop = 1000;
            foreach (Control ViewControl in Controls)
               {
               if (ViewControl is BaseView)
                  {
                  BaseView View = (BaseView)ViewControl;
                  XmlNode ChildNode = XmlHelper.Find(Data, View.Name);
                  Doc.LoadXml(View.GetData());
                  ChildNode.InnerXml = Doc.DocumentElement.InnerXml;
                  XmlHelper.SetAttribute(ChildNode, "Left", View.Left.ToString());
                  XmlHelper.SetAttribute(ChildNode, "Top", View.Top.ToString());
                  XmlHelper.SetAttribute(ChildNode, "Width", View.Width.ToString());
                  XmlHelper.SetAttribute(ChildNode, "Height", View.Height.ToString());
                  MinimumTop = Math.Min(MinimumTop, View.Top);
                  }
               }

            // There seems to be a bug in studio where if the user scrolls vertically down
            // the page while in edit mode and then exits edit mode, the Top positions of
            // all controls end up being negative. The code below simply corrects the problem
            // by correcting all control's Top location.
            if (MinimumTop < 0)
               {
               foreach (Control ViewControl in Controls)
                  {
                  if (ViewControl is BaseView)
                     {
                     BaseView View = (BaseView)ViewControl;
                     ViewControl.Top = ViewControl.Top - MinimumTop + 10;  // an extra 10 pixels down.
                     XmlNode ChildNode = XmlHelper.Find(Data, ViewControl.Name);
                     XmlHelper.SetAttribute(ChildNode, "Top", ViewControl.Top.ToString());
                     }
                  }
               }
            }
         }
      public override void OnClose()
         {
         OnNormalClick(null, null);
         foreach (BaseView View in Controls)
            {
            View.OnClose();
            }
         }
      private void OnViewChanged(BaseView ChangedView)
         {
         DataProcessor.Set(ChangedView.GetData());
         // We call onsave so that our 'Data' is completely up to date with
         // respect to our children.
         OnSave();
         OnRefresh();

         // Now we re-publish the event so that our parent container knows
         // that something has changed.
         PublishViewChanged();
         }

      #region ContextMenu methods
      private void OnMenuOpening(object sender, CancelEventArgs e)
         {
         // -----------------------------------------------------
         // User has opened the source menu - populate it.
         // -----------------------------------------------------

         Control ControlUnderMouse = GetChildAtPoint(PointToClient(Cursor.Position));

         PopupMenu.Items.Clear();
         if (Mode == PageMode.Normal)
            {
            if (ControlUnderMouse != null && ControlUnderMouse is ChartUI)
               {
               ToolStripMenuItem ChartPropertiesMenuItem = (ToolStripMenuItem)PopupMenu.Items.Add("Chart properties");
               ChartPropertiesMenuItem.Click += OnChartPropertiesClick;
               ChartPropertiesMenuItem.Tag = ControlUnderMouse;
               PopupMenu.Items.Add(new ToolStripSeparator());
               }
            ToolStripMenuItem ArrowMenuItem = (ToolStripMenuItem)PopupMenu.Items.Add("Connections");
            ArrowMenuItem.Click += OnArrowClick;
            ToolStripMenuItem EditMenuItem = (ToolStripMenuItem)PopupMenu.Items.Add("Edit");
            EditMenuItem.Click += OnEditClick;
            ToolStripMenuItem ViewErrorsMenuItem = (ToolStripMenuItem)PopupMenu.Items.Add("View all error messages");
            ViewErrorsMenuItem.Click += OnViewErrorsClick;
            }
         else if (Mode == PageMode.Arrow)
            {
            if (ControlUnderMouse != null)
               {
               XmlNode ControlData = XmlHelper.Find(Data, ControlUnderMouse.Name);
               string[] DataSetNames = Processor.DataSetNames();
               foreach (string DataSetName in DataSetNames)
                  {
                  if (DataSetName != ControlUnderMouse.Name)
                     {
                     ToolStripMenuItem MenuItem = (ToolStripMenuItem)PopupMenu.Items.Add("Link to " + DataSetName);
                     MenuItem.Checked = (XmlHelper.ChildByTypeAndValue(ControlData, "source", DataSetName) != null);
                     MenuItem.Click += OnAddLinkMenuItemClick;
                     MenuItem.Tag = DataSetName + " " + ControlUnderMouse.Name;
                     }
                  }
               foreach (string SourceName in XmlHelper.Values(ControlData, "source"))
                  {
                  if (Array.IndexOf(DataSetNames, SourceName) == -1)
                     ControlData.RemoveChild(XmlHelper.ChildByTypeAndValue(ControlData, "source", SourceName));
                  }
               }
            PopupMenu.Items.Add(new ToolStripSeparator());
            ToolStripMenuItem ExitConnectionsMenuItem = (ToolStripMenuItem)PopupMenu.Items.Add("Exit connections mode");
            ExitConnectionsMenuItem.Click += OnNormalClick;
            }
         e.Cancel = false;
         }
      private void OnControlAdded(object sender, ComponentEventArgs e)
         {
         // -----------------------------------------------------
         // User has inserted a new component
         // -----------------------------------------------------
         BaseView NewControl = (BaseView)e.Component;
         if (NewControl.Name != "")
            {
            string NewTypeName = Toolbox.ToolboxCtrl.SelectedItem.DisplayName;
            XmlNode NewChild = Data.AppendChild(Data.OwnerDocument.CreateElement(NewTypeName));
            XmlHelper.EnsureNodeIsUnique(NewChild);
            if (NewTypeName.ToLower() != "page")
               Processor.Add(NewChild.OuterXml);

            NewControl.Parent = this;
            NewControl.Name = XmlHelper.Name(NewChild);
            NewControl.OnLoad(Controller, XmlHelper.FullPath(NewChild), NewChild.OuterXml);
            NewControl.OnRefresh();
            NewControl.ViewChanged += OnViewChanged;
            }
         }
      private void OnRemoveMenuItem(object sender, EventArgs e)
         {
         // -----------------------------------------------------
         // User has clicked on remove menu item.
         // -----------------------------------------------------
         ISelectionService ss = (ISelectionService)FormDesigner.DesignerHost.GetService(typeof(ISelectionService));
         Control ComponentToRemove = (Control)ss.PrimarySelection;
         if (MessageBox.Show("Are you sure you want to remove " + ComponentToRemove.Name,
                             "Are you sure", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == DialogResult.Yes)
            {
            ss.SetSelectedComponents(null);
            string ComponentNameToRemove = ComponentToRemove.Name;
            Data.RemoveChild(XmlHelper.Find(Data, ComponentNameToRemove));
            ComponentToRemove.Parent.Controls.Remove(ComponentToRemove);
            Processor.Erase(ComponentNameToRemove);
            OnSave();
            PublishViewChanged();
            Invalidate();
            OnEditClick(null, null);
            }
         }
      private void OnRenameMenuItem(object sender, EventArgs e)
         {
         // -----------------------------------------------------
         // User has clicked on remove menu item.
         // -----------------------------------------------------
         ISelectionService Selections = (ISelectionService)FormDesigner.DesignerHost.GetService(typeof(ISelectionService));
         if (Selections.SelectionCount == 1)
            {
            BaseView ComponentToRename = (BaseView)Selections.PrimarySelection;
            string OldName = ComponentToRename.Name;
            string NewName = UIBits.InputDialog.InputBox("Enter new name:",
                                                  "Rename", OldName, false);
            if (NewName != "" && NewName != OldName)
               {
               XmlNode RenamedChild = XmlHelper.Find(Data, OldName);
               XmlHelper.SetName(RenamedChild, NewName);
               XmlHelper.EnsureNodeIsUnique(RenamedChild);
               NewName = XmlHelper.Name(RenamedChild);
               ComponentToRename.Name = NewName;
               ComponentToRename.Text = NewName;
               Processor.Rename(OldName, NewName);
               OnSave();
               PublishViewChanged();
               ComponentToRename.OnLoad(Controller, ComponentToRename.NodePath, RenamedChild.OuterXml);
               ComponentToRename.OnRefresh();
               OnEditClick(null, null);
               }
            }
         }
      private void OnNormalClick(object sender, EventArgs e)
         {
         if (FormDesigner.Active)
            {
            IComponentChangeService iccs = (IComponentChangeService)FormDesigner.DesignerHost.GetService(typeof(IComponentChangeService));
            iccs.ComponentAdded -= OnControlAdded;
            FormDesigner.Active = false;
            OnSave();
            PublishViewChanged();
            }
         Mode = PageMode.Normal;

         Toolbox.Visible = false;
         Invalidate();
         }

      private Control FindWindow(Control ctrl, string ControlName)
         {
         if (ctrl.Text == ControlName)
            return ctrl;
         foreach (Control Child in ctrl.Controls)
            {
            Control F = FindWindow(Child, ControlName);
            if (F != null)
               return F;
            }
         return null;
         }
      private void OnArrowClick(object sender, EventArgs e)
         {
         FormDesigner.Active = false;
         Mode = PageMode.Arrow;
         Invalidate();
         }
      private void OnEditClick(object sender, EventArgs e)
         {
         FormDesigner.Active = true;
         Toolbox.Visible = true;
         Mode = PageMode.Design;
         IComponentChangeService iccs = (IComponentChangeService)FormDesigner.DesignerHost.GetService(typeof(IComponentChangeService));
         iccs.ComponentAdded += OnControlAdded;
         }
      private void OnChartPropertiesClick(object sender, EventArgs e)
         {
         ToolStripItem MenuItem = (ToolStripItem)sender;
         ChartUI ChartToEdit = (ChartUI)MenuItem.Tag;
         ChartToEdit.ChartEdit();
         }
      private void OnAddLinkMenuItemClick(object sender, EventArgs e)
         {
         // -----------------------------------------------------
         // User has clicked on an add link menu item.
         // -----------------------------------------------------
         ToolStripMenuItem MenuItem = (ToolStripMenuItem)sender;
         string[] TagNames = MenuItem.Tag.ToString().Split(' ');
         string SourceName = TagNames[0];
         string ThisControlName = TagNames[1];

         XmlNode ChildData = XmlHelper.Find(Data, ThisControlName);
         if (MenuItem.Checked)
            ChildData.RemoveChild(XmlHelper.ChildByTypeAndValue(ChildData, "source", SourceName));
         else
            {
            XmlNode NewSourceNode = XmlHelper.CreateNode(ChildData.OwnerDocument, "source", "");
            NewSourceNode.InnerText = SourceName;
            ChildData.AppendChild(NewSourceNode);
            }

         // we need to give the new xml to the child control again.
         Control[] ThisControl = Controls.Find(ThisControlName, false);
         if (ThisControl.Length == 1)
            {
            BaseView Cntl = (BaseView)ThisControl[0];
            Cntl.OnLoad(Controller, NodePath, ChildData.OuterXml);
            }

         // give the new xml to the data processor and then refresh everything.
         Processor.Set(ChildData.OuterXml);
         Invalidate();
         OnRefresh();
         PublishViewChanged();
         OnArrowClick(null, null);
         }
      private void OnViewErrorsClick(object sender, EventArgs e)
         {
         string Errors = Processor.ErrorMessage();
         if (Errors != "")
            MessageBox.Show(Errors, "Errors", MessageBoxButtons.OK, MessageBoxIcon.Error);
         else
            MessageBox.Show("No errors found", "", MessageBoxButtons.OK, MessageBoxIcon.Information);
         }

      #endregion

      #region Arrow stuff
      private void OnCanvasPaint(object sender, PaintEventArgs e)
         {
         //e.Graphics.DrawRectangle(SystemPens.ControlLightLight, Bounds);
         //e.Graphics.FillRectangle(SystemBrushes.ControlDark, Bounds /*e.ClipRectangle*/);
         //XmlNode GraphDataNode = Controller.XmlNode.Find(NodePath);
         //if (GraphDataNode.Attribute("EditMode") == "On")
         if (Data != null && Mode == PageMode.Arrow)
            DrawArrows(e);
         }
      private void DrawArrows(PaintEventArgs e)
         {
         foreach (XmlNode Child in XmlHelper.ChildNodes(Data, ""))
            foreach (XmlNode Source in XmlHelper.ChildNodes(Child, "source"))
               {
               if (Source.InnerText != "")
                  {
                  Control[] FromCtrl = Controls.Find(Source.InnerText, true);
                  Control[] ToCtrl = Controls.Find(XmlHelper.Name(Child), true);
                  if (FromCtrl.Length == 1 && ToCtrl.Length == 1)
                     DrawArrowBetweenControls(FromCtrl[0], ToCtrl[0], e);
                  }
               }

         }
      private void DrawArrowBetweenControls(Control FromCtrl, Control ToCtrl, PaintEventArgs e)
         {
         float x1, y1, x2, y2;
         if (FromCtrl.Top <= ToCtrl.Top)
            {
            y1 = FromCtrl.Bottom;
            y2 = ToCtrl.Top;
            if (ToCtrl.Left > FromCtrl.Right)
               {
               x1 = FromCtrl.Right;
               x2 = ToCtrl.Left;
               }
            else if (ToCtrl.Left > FromCtrl.Left)
               {
               x1 = FromCtrl.Left;
               x2 = ToCtrl.Left;
               }
            else
               {
               x1 = FromCtrl.Left;
               x2 = ToCtrl.Right;
               }

            }
         else
            {
            y1 = FromCtrl.Top;
            y2 = ToCtrl.Bottom;
            if (ToCtrl.Left > FromCtrl.Right)
               {
               x1 = FromCtrl.Right;
               x2 = ToCtrl.Left;
               }
            else if (ToCtrl.Left > FromCtrl.Left)
               {
               x1 = FromCtrl.Left;
               x2 = ToCtrl.Left;
               }
            else
               {
               x1 = FromCtrl.Left;
               x2 = ToCtrl.Right;
               }
            }
         if (!FromCtrl.Visible)
            {
            x1 = 1;
            y1 = 1;
            }
         Arrow.DrawArrow(e.Graphics, Pens.Blue, Brushes.LightBlue, x1, y1, x2, y2);
         }
      #endregion


      }
   }

