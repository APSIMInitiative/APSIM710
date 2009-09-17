
using System;
using System.Collections;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using System.Xml;

using ApsimFile;
using Controllers;
using CSGeneral;
using UIUtility;    //GridUtility.cs



namespace CSUserInterface
   {
   public class InitWaterUI : BaseView
      {
      private System.Windows.Forms.Panel panel1;
      internal System.Windows.Forms.RadioButton LayeredRadio;
      internal System.Windows.Forms.GroupBox GroupBox;
      internal System.Windows.Forms.Panel DepthWetSoilPanel;
      internal System.Windows.Forms.Label Label3;
      internal System.Windows.Forms.TextBox DepthEdit;
      internal System.Windows.Forms.Panel PercentPanel;
      internal System.Windows.Forms.RadioButton EvenlyDistributedRadio;
      internal System.Windows.Forms.RadioButton FilledFromTopRadio;
      internal System.Windows.Forms.Label Label2;
      internal System.Windows.Forms.TextBox PAWCEdit;
      internal System.Windows.Forms.Label Label1;
      internal System.Windows.Forms.NumericUpDown PercentEdit;
      internal System.Windows.Forms.RadioButton DepthWetSoilRadio;
      internal System.Windows.Forms.RadioButton PercentRadio;
      private System.Windows.Forms.Splitter splitter1;
      private System.ComponentModel.IContainer components = null;
      private Soil SoilData;
      private InitWater InitialWater = null;
      private WaterChartControl WaterChartControl;
      private FarPoint.Win.Spread.FpSpread Grid;
      private FarPoint.Win.Spread.SheetView WaterGrid;
      private Label label4;
      private ComboBox RelativeToCombo;
      private bool UserChange = true;


      // -------------------
      // constructor.
      // -------------------
      public InitWaterUI()
         {
         // This call is required by the Windows Form Designer.
         InitializeComponent();
         }

      // ------------------------------------
      // Clean up any resources being used.
      // ------------------------------------
      protected override void Dispose(bool disposing)
         {
         if (disposing)
            {
            if (components != null)
               {
               components.Dispose();
               }
            }
         base.Dispose(disposing);
         }

      #region Designer generated code
      /// <summary>
      /// Required method for Designer support - do not modify
      /// the contents of this method with the code editor.
      /// </summary>
      private void InitializeComponent()
         {
         FarPoint.Win.Spread.TipAppearance tipAppearance1 = new FarPoint.Win.Spread.TipAppearance();
         FarPoint.Win.Spread.CellType.NumberCellType numberCellType1 = new FarPoint.Win.Spread.CellType.NumberCellType();
         this.panel1 = new System.Windows.Forms.Panel();
         this.Grid = new FarPoint.Win.Spread.FpSpread();
         this.WaterGrid = new FarPoint.Win.Spread.SheetView();
         this.LayeredRadio = new System.Windows.Forms.RadioButton();
         this.GroupBox = new System.Windows.Forms.GroupBox();
         this.RelativeToCombo = new System.Windows.Forms.ComboBox();
         this.label4 = new System.Windows.Forms.Label();
         this.PercentPanel = new System.Windows.Forms.Panel();
         this.EvenlyDistributedRadio = new System.Windows.Forms.RadioButton();
         this.FilledFromTopRadio = new System.Windows.Forms.RadioButton();
         this.Label2 = new System.Windows.Forms.Label();
         this.PAWCEdit = new System.Windows.Forms.TextBox();
         this.Label1 = new System.Windows.Forms.Label();
         this.PercentEdit = new System.Windows.Forms.NumericUpDown();
         this.DepthWetSoilPanel = new System.Windows.Forms.Panel();
         this.Label3 = new System.Windows.Forms.Label();
         this.DepthEdit = new System.Windows.Forms.TextBox();
         this.DepthWetSoilRadio = new System.Windows.Forms.RadioButton();
         this.PercentRadio = new System.Windows.Forms.RadioButton();
         this.splitter1 = new System.Windows.Forms.Splitter();
         this.WaterChartControl = new CSUserInterface.WaterChartControl();
         this.panel1.SuspendLayout();
         ((System.ComponentModel.ISupportInitialize)(this.Grid)).BeginInit();
         ((System.ComponentModel.ISupportInitialize)(this.WaterGrid)).BeginInit();
         this.GroupBox.SuspendLayout();
         this.PercentPanel.SuspendLayout();
         ((System.ComponentModel.ISupportInitialize)(this.PercentEdit)).BeginInit();
         this.DepthWetSoilPanel.SuspendLayout();
         this.SuspendLayout();
         // 
         // panel1
         // 
         this.panel1.Controls.Add(this.Grid);
         this.panel1.Controls.Add(this.LayeredRadio);
         this.panel1.Controls.Add(this.GroupBox);
         this.panel1.Controls.Add(this.DepthWetSoilRadio);
         this.panel1.Controls.Add(this.PercentRadio);
         this.panel1.Dock = System.Windows.Forms.DockStyle.Left;
         this.panel1.Location = new System.Drawing.Point(0, 40);
         this.panel1.Name = "panel1";
         this.panel1.Size = new System.Drawing.Size(296, 677);
         this.panel1.TabIndex = 2;
         // 
         // Grid
         // 
         this.Grid.AccessibleDescription = "Grid, Sheet1, Row 0, Column 0, ";
         this.Grid.BackColor = System.Drawing.SystemColors.Control;
         this.Grid.EditModeReplace = true;
         this.Grid.HorizontalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.AsNeeded;
         this.Grid.Location = new System.Drawing.Point(66, 219);
         this.Grid.Name = "Grid";
         this.Grid.Sheets.AddRange(new FarPoint.Win.Spread.SheetView[] {
            this.WaterGrid});
         this.Grid.Size = new System.Drawing.Size(148, 289);
         this.Grid.TabIndex = 30;
         tipAppearance1.BackColor = System.Drawing.SystemColors.Info;
         tipAppearance1.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
         tipAppearance1.ForeColor = System.Drawing.SystemColors.InfoText;
         this.Grid.TextTipAppearance = tipAppearance1;
         this.Grid.VerticalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.AsNeeded;
         // 
         // WaterGrid
         // 
         this.WaterGrid.Reset();
         // Formulas and custom names must be loaded with R1C1 reference style
         this.WaterGrid.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.R1C1;
         this.WaterGrid.ColumnCount = 2;
         this.WaterGrid.ColumnHeader.RowCount = 2;
         this.WaterGrid.AutoUpdateNotes = true;
         this.WaterGrid.ColumnHeader.Cells.Get(0, 0).Value = "Depth";
         this.WaterGrid.ColumnHeader.Cells.Get(0, 1).Value = "SW";
         this.WaterGrid.ColumnHeader.Cells.Get(1, 0).Value = "(cm)";
         this.WaterGrid.ColumnHeader.Cells.Get(1, 1).Value = "(%)";
         this.WaterGrid.Columns.Get(0).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Right;
         this.WaterGrid.Columns.Get(0).Label = "(cm)";
         this.WaterGrid.Columns.Get(0).Locked = false;
         numberCellType1.DecimalPlaces = 2;
         this.WaterGrid.Columns.Get(1).CellType = numberCellType1;
         this.WaterGrid.Columns.Get(1).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Right;
         this.WaterGrid.Columns.Get(1).Label = "(%)";
         this.WaterGrid.DefaultStyle.BackColor = System.Drawing.Color.White;
         this.WaterGrid.DefaultStyle.Locked = false;
         this.WaterGrid.DefaultStyle.Parent = "DataAreaDefault";
         this.WaterGrid.RowHeader.Columns.Default.Resizable = false;
         this.WaterGrid.RowHeader.Visible = false;
         this.WaterGrid.SheetName = "Sheet1";
         this.WaterGrid.CellChanged += new FarPoint.Win.Spread.SheetViewEventHandler(this.WaterGrid_CellChanged);
         this.WaterGrid.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.A1;
         // 
         // LayeredRadio
         // 
         this.LayeredRadio.Location = new System.Drawing.Point(8, 56);
         this.LayeredRadio.Name = "LayeredRadio";
         this.LayeredRadio.Size = new System.Drawing.Size(293, 21);
         this.LayeredRadio.TabIndex = 28;
         this.LayeredRadio.Text = "Specify water as layered values";
         this.LayeredRadio.CheckedChanged += new System.EventHandler(this.LayeredRadio_CheckedChanged);
         // 
         // GroupBox
         // 
         this.GroupBox.Controls.Add(this.RelativeToCombo);
         this.GroupBox.Controls.Add(this.label4);
         this.GroupBox.Controls.Add(this.PercentPanel);
         this.GroupBox.Controls.Add(this.DepthWetSoilPanel);
         this.GroupBox.Location = new System.Drawing.Point(8, 80);
         this.GroupBox.Name = "GroupBox";
         this.GroupBox.Size = new System.Drawing.Size(272, 118);
         this.GroupBox.TabIndex = 27;
         this.GroupBox.TabStop = false;
         this.GroupBox.Text = "Properties";
         // 
         // RelativeToCombo
         // 
         this.RelativeToCombo.FormattingEnabled = true;
         this.RelativeToCombo.Location = new System.Drawing.Point(78, 94);
         this.RelativeToCombo.Name = "RelativeToCombo";
         this.RelativeToCombo.Size = new System.Drawing.Size(168, 21);
         this.RelativeToCombo.TabIndex = 18;
         this.RelativeToCombo.TextChanged += new System.EventHandler(this.RelativeToCombo_TextChanged);
         // 
         // label4
         // 
         this.label4.AutoSize = true;
         this.label4.Location = new System.Drawing.Point(11, 97);
         this.label4.Name = "label4";
         this.label4.Size = new System.Drawing.Size(61, 13);
         this.label4.TabIndex = 17;
         this.label4.Text = "Relative to:";
         // 
         // PercentPanel
         // 
         this.PercentPanel.Controls.Add(this.EvenlyDistributedRadio);
         this.PercentPanel.Controls.Add(this.FilledFromTopRadio);
         this.PercentPanel.Controls.Add(this.Label2);
         this.PercentPanel.Controls.Add(this.PAWCEdit);
         this.PercentPanel.Controls.Add(this.Label1);
         this.PercentPanel.Controls.Add(this.PercentEdit);
         this.PercentPanel.Location = new System.Drawing.Point(8, 19);
         this.PercentPanel.Name = "PercentPanel";
         this.PercentPanel.Size = new System.Drawing.Size(256, 69);
         this.PercentPanel.TabIndex = 15;
         // 
         // EvenlyDistributedRadio
         // 
         this.EvenlyDistributedRadio.Location = new System.Drawing.Point(136, 40);
         this.EvenlyDistributedRadio.Name = "EvenlyDistributedRadio";
         this.EvenlyDistributedRadio.Size = new System.Drawing.Size(113, 20);
         this.EvenlyDistributedRadio.TabIndex = 16;
         this.EvenlyDistributedRadio.Text = "Evenly distributed";
         this.EvenlyDistributedRadio.CheckedChanged += new System.EventHandler(this.DistributionRadioChanged);
         // 
         // FilledFromTopRadio
         // 
         this.FilledFromTopRadio.Checked = true;
         this.FilledFromTopRadio.Location = new System.Drawing.Point(13, 42);
         this.FilledFromTopRadio.Name = "FilledFromTopRadio";
         this.FilledFromTopRadio.Size = new System.Drawing.Size(127, 20);
         this.FilledFromTopRadio.TabIndex = 15;
         this.FilledFromTopRadio.TabStop = true;
         this.FilledFromTopRadio.Text = "Filled from top";
         this.FilledFromTopRadio.CheckedChanged += new System.EventHandler(this.DistributionRadioChanged);
         // 
         // Label2
         // 
         this.Label2.AutoSize = true;
         this.Label2.Location = new System.Drawing.Point(167, 10);
         this.Label2.Name = "Label2";
         this.Label2.Size = new System.Drawing.Size(52, 13);
         this.Label2.TabIndex = 14;
         this.Label2.Text = "mm water";
         // 
         // PAWCEdit
         // 
         this.PAWCEdit.Location = new System.Drawing.Point(107, 7);
         this.PAWCEdit.Name = "PAWCEdit";
         this.PAWCEdit.Size = new System.Drawing.Size(53, 20);
         this.PAWCEdit.TabIndex = 13;
         this.PAWCEdit.TextChanged += new System.EventHandler(this.PAWCEdit_TextChanged);
         // 
         // Label1
         // 
         this.Label1.AutoSize = true;
         this.Label1.Location = new System.Drawing.Point(67, 10);
         this.Label1.Name = "Label1";
         this.Label1.Size = new System.Drawing.Size(15, 13);
         this.Label1.TabIndex = 12;
         this.Label1.Text = "%";
         // 
         // PercentEdit
         // 
         this.PercentEdit.Location = new System.Drawing.Point(13, 7);
         this.PercentEdit.Name = "PercentEdit";
         this.PercentEdit.Size = new System.Drawing.Size(47, 20);
         this.PercentEdit.TabIndex = 11;
         this.PercentEdit.Value = new decimal(new int[] {
            100,
            0,
            0,
            0});
         this.PercentEdit.ValueChanged += new System.EventHandler(this.PercentEdit_ValueChanged);
         this.PercentEdit.KeyUp += new System.Windows.Forms.KeyEventHandler(this.PercentEdit_KeyUp);
         // 
         // DepthWetSoilPanel
         // 
         this.DepthWetSoilPanel.Controls.Add(this.Label3);
         this.DepthWetSoilPanel.Controls.Add(this.DepthEdit);
         this.DepthWetSoilPanel.Location = new System.Drawing.Point(6, 19);
         this.DepthWetSoilPanel.Name = "DepthWetSoilPanel";
         this.DepthWetSoilPanel.Size = new System.Drawing.Size(200, 34);
         this.DepthWetSoilPanel.TabIndex = 16;
         // 
         // Label3
         // 
         this.Label3.AutoSize = true;
         this.Label3.Location = new System.Drawing.Point(67, 7);
         this.Label3.Name = "Label3";
         this.Label3.Size = new System.Drawing.Size(39, 13);
         this.Label3.TabIndex = 14;
         this.Label3.Text = "cm soil";
         // 
         // DepthEdit
         // 
         this.DepthEdit.Location = new System.Drawing.Point(7, 7);
         this.DepthEdit.Name = "DepthEdit";
         this.DepthEdit.Size = new System.Drawing.Size(53, 20);
         this.DepthEdit.TabIndex = 13;
         this.DepthEdit.TextChanged += new System.EventHandler(this.DepthEdit_TextChanged);
         // 
         // DepthWetSoilRadio
         // 
         this.DepthWetSoilRadio.Location = new System.Drawing.Point(8, 32);
         this.DepthWetSoilRadio.Name = "DepthWetSoilRadio";
         this.DepthWetSoilRadio.Size = new System.Drawing.Size(293, 20);
         this.DepthWetSoilRadio.TabIndex = 26;
         this.DepthWetSoilRadio.Text = "Specify water as a depth of wet soil";
         this.DepthWetSoilRadio.CheckedChanged += new System.EventHandler(this.DepthWetSoilRadio_CheckedChanged);
         // 
         // PercentRadio
         // 
         this.PercentRadio.Location = new System.Drawing.Point(8, 8);
         this.PercentRadio.Name = "PercentRadio";
         this.PercentRadio.Size = new System.Drawing.Size(300, 21);
         this.PercentRadio.TabIndex = 25;
         this.PercentRadio.Text = "Specify a fraction of maximum available water";
         this.PercentRadio.CheckedChanged += new System.EventHandler(this.PercentRadio_CheckedChanged);
         // 
         // splitter1
         // 
         this.splitter1.Location = new System.Drawing.Point(296, 40);
         this.splitter1.Name = "splitter1";
         this.splitter1.Size = new System.Drawing.Size(3, 677);
         this.splitter1.TabIndex = 21;
         this.splitter1.TabStop = false;
         // 
         // WaterChartControl
         // 
         this.WaterChartControl.Dock = System.Windows.Forms.DockStyle.Fill;
         this.WaterChartControl.LinkedSoil = null;
         this.WaterChartControl.Location = new System.Drawing.Point(299, 40);
         this.WaterChartControl.Name = "WaterChartControl";
         this.WaterChartControl.Size = new System.Drawing.Size(444, 677);
         this.WaterChartControl.TabIndex = 22;
         // 
         // InitWaterUI
         // 
         this.Controls.Add(this.WaterChartControl);
         this.Controls.Add(this.splitter1);
         this.Controls.Add(this.panel1);
         this.Name = "InitWaterUI";
         this.Size = new System.Drawing.Size(743, 717);
         this.Controls.SetChildIndex(this.panel1, 0);
         this.Controls.SetChildIndex(this.splitter1, 0);
         this.Controls.SetChildIndex(this.WaterChartControl, 0);
         this.panel1.ResumeLayout(false);
         ((System.ComponentModel.ISupportInitialize)(this.Grid)).EndInit();
         ((System.ComponentModel.ISupportInitialize)(this.WaterGrid)).EndInit();
         this.GroupBox.ResumeLayout(false);
         this.GroupBox.PerformLayout();
         this.PercentPanel.ResumeLayout(false);
         this.PercentPanel.PerformLayout();
         ((System.ComponentModel.ISupportInitialize)(this.PercentEdit)).EndInit();
         this.DepthWetSoilPanel.ResumeLayout(false);
         this.DepthWetSoilPanel.PerformLayout();
         this.ResumeLayout(false);

         }
      #endregion

      // -----------------------
      // Refresh the form
      // -----------------------
      override public void OnRefresh()
         {
         WaterChartControl.OnWaterChange -= OnInitWaterChangedByDrag;

         HelpText = "There are multiple ways of initialising soil water. Select a method by clicking one of the options below "
                + " and then filling in the details.";

         ApsimFile.Component SoilNode = Controller.ApsimData.Find(NodePath);
         if (SoilNode != null && SoilNode.Parent != null)
            {
            SoilNode = SoilNode.Parent;
            XmlDocument Doc = new XmlDocument();
            Doc.LoadXml(SoilNode.Contents);
            SoilData = new Soil(Doc.DocumentElement);
            InitialWater = new InitWater(Data, SoilData);

            FarPoint.Win.Spread.InputMap InputMap = Grid.GetInputMap(FarPoint.Win.Spread.InputMapMode.WhenAncestorOfFocused);
            InputMap.Put(new FarPoint.Win.Spread.Keystroke(Keys.Delete, Keys.None),
                            FarPoint.Win.Spread.SpreadActions.ClipboardCut);
            InputMap.Put(new FarPoint.Win.Spread.Keystroke(Keys.Enter, Keys.None),
                            FarPoint.Win.Spread.SpreadActions.MoveToNextRow);

            WaterChartControl.LinkedSoil = SoilData;
            WaterChartControl.LinkedSoilWater = InitialWater;
            RelativeToCombo.Items.Add("ll15");
            RelativeToCombo.Items.AddRange(SoilData.Crops);

            PopulateControls();
            }
         WaterChartControl.OnWaterChange += OnInitWaterChangedByDrag;
         }


      // -------------------------------------
      // Populate all controls from the soil
      // -------------------------------------
      private void PopulateControls()
         {
         UserChange = false;
         PercentRadio.Checked = (InitialWater.Method == InitWater.MethodType.Percent);
         DepthWetSoilRadio.Checked = (InitialWater.Method == InitWater.MethodType.DepthWetSoil);
         LayeredRadio.Checked = (InitialWater.Method == InitWater.MethodType.Layered);
         PercentPanel.Visible = PercentRadio.Checked;
         DepthWetSoilPanel.Visible = DepthWetSoilRadio.Checked;
         GroupBox.Visible = !LayeredRadio.Checked;
         RelativeToCombo.Visible = (PercentRadio.Checked || DepthWetSoilRadio.Checked);
         RelativeToCombo.Text = InitialWater.RelativeTo;

         if (InitialWater.Method == InitWater.MethodType.Percent)
            {
            PercentEdit.Text = InitialWater.Percent.ToString("f0");
            FilledFromTopRadio.Checked = InitialWater.FilledFromTop;
            EvenlyDistributedRadio.Checked = !InitialWater.FilledFromTop;
            UpdatePAWCBox();
            }
         else if (InitialWater.Method == InitWater.MethodType.DepthWetSoil)
            {
            int DepthCM = InitialWater.DepthWetSoil / 10;
            DepthEdit.Text = DepthCM.ToString();
            }
         PopulateGrid();
         UserChange = true;
         }


      // ------------------------------
      // Save the contents of the grid.
      // ------------------------------
      private void SaveControls()
         {
         if (PercentRadio.Checked)
            {
            InitialWater.SetUsingPercent(Convert.ToInt32(PercentEdit.Text), FilledFromTopRadio.Checked);
            InitialWater.RelativeTo = RelativeToCombo.Text;
            }
         else if (DepthWetSoilRadio.Checked)
            {
            InitialWater.SetUsingDepthWetSoil(Convert.ToInt32(DepthEdit.Text));
            InitialWater.RelativeTo = RelativeToCombo.Text;
            }
         else
            {
            int NumLayers = GridUtility.FindFirstBlankCell(WaterGrid, 0);
            double[] sw = GridUtility.GetColumnAsDoubles(WaterGrid, 1, NumLayers);
            sw = MathUtility.Divide_Value(sw, 100);
            InitialWater.SetUsingLayered(sw);
            }
         }


      // ----------------------------
      // Update the PAWC edit box.
      // ----------------------------
      private void UpdatePAWCBox()
         {
         UserChange = false;
         double[] pawc;
         if (RelativeToCombo.Text == "ll15")
            pawc = SoilData.PAWC();
         else
            pawc = SoilData.PAWC(RelativeToCombo.Text);

         double Proportion = Convert.ToInt32(PercentEdit.Value) / 100.0;
         double AmountWater = MathUtility.Sum(pawc) * Proportion;
         PAWCEdit.Text = AmountWater.ToString("f0");    // This will call PopulateGrid
         UserChange = true;
         }

      // -------------------------------------
      // Populate water grid from the data
      // -------------------------------------
      private void PopulateGrid()
         {
         UserChange = false;
         WaterGrid.ClearRange(0, 0, WaterGrid.RowCount, WaterGrid.ColumnCount, true);
         WaterGrid.Columns[0].Locked = !LayeredRadio.Checked;
         WaterGrid.Columns[1].Locked = !LayeredRadio.Checked;
         if (LayeredRadio.Checked)
            WaterGrid.DefaultStyle.BackColor = SystemColors.Window;
         else
            WaterGrid.DefaultStyle.BackColor = SystemColors.Control;

         GridUtility.SetColumnAsStrings(WaterGrid, 0, SoilComponentUtility.ToDepthStrings(InitialWater.Thickness));
         GridUtility.SetColumnAsDoubles(WaterGrid, 1, MathUtility.Multiply_Value(InitialWater.SW, 100));

         WaterChartControl.RefreshView();
         UserChange = true;
         }


      // -----------------------------------------------------
      // User has changed the contents of a cell - update row.
      // -----------------------------------------------------
      private void WaterGrid_CellChanged(object sender, FarPoint.Win.Spread.SheetViewEventArgs e)
         {
         if (UserChange)
            {
            if (e.Column == 0)
               {
               // user changed depths
               int NumLayers = GridUtility.FindFirstBlankCell(WaterGrid, 0);
               InitialWater.Thickness = SoilComponentUtility.ToThickness(GridUtility.GetColumnAsStrings(WaterGrid, 0, NumLayers));
               }
            else
               SaveControls();

            WaterChartControl.RefreshView();
            }
         }


      // ----------------------------------------------
      // User has changed the value of the percent radio button
      // ----------------------------------------------
      private void PercentRadio_CheckedChanged(object sender, System.EventArgs e)
         {
         if (PercentRadio.Checked && UserChange)
            {
            if (InitialWater.Method != InitWater.MethodType.Percent)
               InitialWater.SetUsingPercent(100, true);
            PopulateControls();
            }
         }


      // -------------------------------------------------------------
      // User has changed the value of the depth wet soil radio button
      // -------------------------------------------------------------
      private void DepthWetSoilRadio_CheckedChanged(object sender, System.EventArgs e)
         {
         if (DepthWetSoilRadio.Checked && UserChange)
            {
            if (InitialWater.Method != InitWater.MethodType.DepthWetSoil)
               InitialWater.SetUsingDepthWetSoil(100);
            PopulateControls();
            }
         }


      // -------------------------------------------------------------
      // User has changed the value of the layered radio button
      // -------------------------------------------------------------
      private void LayeredRadio_CheckedChanged(object sender, System.EventArgs e)
         {
         if (LayeredRadio.Checked && UserChange)
            {
            if (InitialWater.Method != InitWater.MethodType.Layered)
               {
               InitialWater.Thickness = SoilData.Thickness;
               InitialWater.SetUsingLayered(SoilData.LL15);
               }
            PopulateControls();
            }
         }


      // -------------------------------------------------
      // User has changed the percent value using Up|Down
      // -------------------------------------------------
      private void PercentEdit_ValueChanged(object sender, System.EventArgs e)
         {
         if (UserChange)
            {
            UserChange = false;
            int Percent = Convert.ToInt32(PercentEdit.Value);
            InitialWater.SetUsingPercent(Percent, FilledFromTopRadio.Checked);
            UpdatePAWCBox();
            PopulateGrid();
            UserChange = true;
            }
         }


      // --------------------------------------------
      //User has changed the percent value by typing.
      // --------------------------------------------
      private void PercentEdit_KeyUp(object sender, System.Windows.Forms.KeyEventArgs e)
         {
         PercentEdit_ValueChanged(sender, null);
         }


      // ------------------------------
      // User has typed in a pawc value
      // ------------------------------
      private void PAWCEdit_TextChanged(object sender, System.EventArgs e)
         {
         if (UserChange)
            {
            UserChange = false;
            double[] pawc;
            if (RelativeToCombo.Text == "ll15")
               pawc = SoilData.PAWC();
            else
               pawc = SoilData.PAWC(RelativeToCombo.Text);
            double TotalPAWC = MathUtility.Sum(pawc);
            int Percent = 0;
            if (PAWCEdit.Text != "")
               Percent = Convert.ToInt32(Convert.ToDouble(PAWCEdit.Text) / TotalPAWC * 100);
            Percent = Math.Min(Percent, 100);
            Percent = Math.Max(Percent, 0);
            PercentEdit.Value = Percent;
            InitialWater.SetUsingPercent(Percent, FilledFromTopRadio.Checked);
            PopulateGrid();
            UserChange = true;
            }
         }


      // -----------------------------------------
      // User has typed in a depth wet soil value
      // -----------------------------------------
      private void DepthEdit_TextChanged(object sender, System.EventArgs e)
         {
         if (UserChange)
            {
            UserChange = false;
            int Depth = 0;
            if (DepthEdit.Text != "")
               Depth = Convert.ToInt32(Convert.ToInt32(DepthEdit.Text)) * 10;
            InitialWater.SetUsingDepthWetSoil(Depth);
            PopulateGrid();
            UserChange = true;
            }
         }

      // -----------------------------------------
      // User has ticked either the fill from top radio
      // or the evenly dist. radio.
      // -----------------------------------------
      private void DistributionRadioChanged(object sender, System.EventArgs e)
         {
         if (UserChange)
            {
            UserChange = false;
            int Percent = Convert.ToInt32(PercentEdit.Value);
            InitialWater.SetUsingPercent(Percent, FilledFromTopRadio.Checked);
            PopulateGrid();
            UserChange = true;
            }
         }

      private void RelativeToCombo_TextChanged(object sender, EventArgs e)
         {
         if (UserChange)
            {
            UserChange = false;
            InitialWater.RelativeTo = RelativeToCombo.Text;
            PopulateGrid();
            UpdatePAWCBox();
            UserChange = true;
            }
         }
      private void OnInitWaterChangedByDrag(int LayerNumber, double NewValue)
         {
         // User has dragged an initwater point, update our grid.
         LayeredRadio.Checked = true;
         WaterGrid.Cells[LayerNumber, 1].Text = NewValue.ToString("f2");
         }

      }
   }

