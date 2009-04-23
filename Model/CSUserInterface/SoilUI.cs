
using System;
using System.Collections;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Windows.Forms;

using FarPoint.Win.Spread;

using ApsimFile;
using Controllers;
using CSGeneral;
using UIUtility;        //GridUtility.cs
using UIBits;           //InputDialog


namespace CSUserInterface
	{

	// -------------------------------------------
	// A user interface for soil stuff.
	// -------------------------------------------
	public class SoilUI : BaseView
		{
		private System.ComponentModel.IContainer components = null;
        private Soil MySoil;
        private static int NUMBER_OF_STATIC_COLS = 7;
		private FarPoint.Win.Spread.FpSpread Grid;
		private FarPoint.Win.Spread.SheetView General;
		private FarPoint.Win.Spread.SheetView SoilProfile;
		private FarPoint.Win.Spread.SheetView APSIM;
		private FarPoint.Win.Spread.SheetView Phosphorus;
        private FarPoint.Win.Spread.SheetView Water;
        private TMGDevelopment.Windows.Forms.PrintForm PrintForm;
		private System.Windows.Forms.OpenFileDialog OpenAttachmentDialog;
		private bool UserChange = true;
        private FarPoint.Win.Spread.SheetView PhotoAttachSheet;
        private CSUserInterface.WaterChartControl WaterChartControl;
		private string AttachmentFileName;
        
		
		public SoilUI()
			{
			// This call is required by the Windows Form Designer.
			InitializeComponent();
			}
		protected override void Dispose( bool disposing )
			{
			if( disposing )
				{
				if (File.Exists(AttachmentFileName))
					File.Delete(AttachmentFileName);

				if (components != null) 
					{
					components.Dispose();
					}
				}
			base.Dispose( disposing );
			}

		#region Designer generated code
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
        this.components = new System.ComponentModel.Container();
        FarPoint.Win.Spread.TipAppearance tipAppearance1 = new FarPoint.Win.Spread.TipAppearance();
        FarPoint.Win.Spread.CellType.TextCellType textCellType1 = new FarPoint.Win.Spread.CellType.TextCellType();
        FarPoint.Win.Spread.CellType.TextCellType textCellType2 = new FarPoint.Win.Spread.CellType.TextCellType();
        FarPoint.Win.BevelBorder bevelBorder1 = new FarPoint.Win.BevelBorder(FarPoint.Win.BevelBorderType.Raised);
        FarPoint.Win.BevelBorder bevelBorder2 = new FarPoint.Win.BevelBorder(FarPoint.Win.BevelBorderType.Raised);
        FarPoint.Win.Spread.CellType.ComboBoxCellType comboBoxCellType1 = new FarPoint.Win.Spread.CellType.ComboBoxCellType();
        FarPoint.Win.Spread.CellType.TextCellType textCellType3 = new FarPoint.Win.Spread.CellType.TextCellType();
        FarPoint.Win.Spread.CellType.TextCellType textCellType4 = new FarPoint.Win.Spread.CellType.TextCellType();
        FarPoint.Win.Spread.CellType.TextCellType textCellType5 = new FarPoint.Win.Spread.CellType.TextCellType();
        FarPoint.Win.Spread.CellType.TextCellType textCellType6 = new FarPoint.Win.Spread.CellType.TextCellType();
        FarPoint.Win.Spread.CellType.ButtonCellType buttonCellType1 = new FarPoint.Win.Spread.CellType.ButtonCellType();
        FarPoint.Win.Spread.CellType.ButtonCellType buttonCellType2 = new FarPoint.Win.Spread.CellType.ButtonCellType();
        FarPoint.Win.Spread.CellType.ButtonCellType buttonCellType3 = new FarPoint.Win.Spread.CellType.ButtonCellType();
        FarPoint.Win.Spread.CellType.ImageCellType imageCellType1 = new FarPoint.Win.Spread.CellType.ImageCellType();
        this.Grid = new FarPoint.Win.Spread.FpSpread();
        this.General = new FarPoint.Win.Spread.SheetView();
        this.Water = new FarPoint.Win.Spread.SheetView();
        this.SoilProfile = new FarPoint.Win.Spread.SheetView();
        this.APSIM = new FarPoint.Win.Spread.SheetView();
        this.Phosphorus = new FarPoint.Win.Spread.SheetView();
        this.PhotoAttachSheet = new FarPoint.Win.Spread.SheetView();
        this.PrintForm = new TMGDevelopment.Windows.Forms.PrintForm(this.components);
        this.OpenAttachmentDialog = new System.Windows.Forms.OpenFileDialog();
        this.WaterChartControl = new CSUserInterface.WaterChartControl();
        ((System.ComponentModel.ISupportInitialize)(this.Grid)).BeginInit();
        ((System.ComponentModel.ISupportInitialize)(this.General)).BeginInit();
        ((System.ComponentModel.ISupportInitialize)(this.Water)).BeginInit();
        ((System.ComponentModel.ISupportInitialize)(this.SoilProfile)).BeginInit();
        ((System.ComponentModel.ISupportInitialize)(this.APSIM)).BeginInit();
        ((System.ComponentModel.ISupportInitialize)(this.Phosphorus)).BeginInit();
        ((System.ComponentModel.ISupportInitialize)(this.PhotoAttachSheet)).BeginInit();
        this.SuspendLayout();


        // 
        // Grid
        // 
        this.Grid.AccessibleDescription = "Grid, Water, Row 0, Column 0, ";
        this.Grid.AllowDragDrop = true;
        this.Grid.Dock = System.Windows.Forms.DockStyle.Top;
        this.Grid.EditModeReplace = true;
        this.Grid.HorizontalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.AsNeeded;
        this.Grid.Location = new System.Drawing.Point(0, 18);
        this.Grid.Name = "Grid";
        this.Grid.SelectionBlockOptions = ((FarPoint.Win.Spread.SelectionBlockOptions)(((FarPoint.Win.Spread.SelectionBlockOptions.Cells | FarPoint.Win.Spread.SelectionBlockOptions.Rows)
                    | FarPoint.Win.Spread.SelectionBlockOptions.Sheet)));
        this.Grid.Sheets.AddRange(new FarPoint.Win.Spread.SheetView[] {
            this.General,
            this.Water,
            this.SoilProfile,
            this.APSIM,
            this.Phosphorus,
            this.PhotoAttachSheet});
        this.Grid.Size = new System.Drawing.Size(906, 292);
        this.Grid.TabIndex = 12;
        this.Grid.TabStrip.ButtonPolicy = FarPoint.Win.Spread.TabStripButtonPolicy.AsNeeded;
        this.Grid.TabStripPolicy = FarPoint.Win.Spread.TabStripPolicy.Always;
        this.Grid.TabStripRatio = 0.512295081967213;
        tipAppearance1.BackColor = System.Drawing.SystemColors.Info;
        tipAppearance1.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
        tipAppearance1.ForeColor = System.Drawing.SystemColors.InfoText;
        this.Grid.TextTipAppearance = tipAppearance1;
        this.Grid.TextTipPolicy = FarPoint.Win.Spread.TextTipPolicy.Floating;
        this.Grid.SetViewportLeftColumn(0, 1);
        this.Grid.SetActiveViewport(0, -1);
        this.Grid.SetViewportLeftColumn(1, 0, 7);
        this.Grid.SetActiveViewport(1, 0, -1);
        this.Grid.ButtonClicked += new FarPoint.Win.Spread.EditorNotifyEventHandler(this.OnAttachGridButtonClicked);
        this.Grid.TextTipFetch += new FarPoint.Win.Spread.TextTipFetchEventHandler(this.OnGetTextTip);
        this.Grid.ActiveSheetIndex = 1;
        this.Grid.ActiveSheetChanged += new EventHandler(this.OnActiveSheetChanged);
        // 
        // General
        // 
        this.General.Reset();
        // Formulas and custom names must be loaded with R1C1 reference style
        this.General.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.R1C1;
        this.General.ColumnCount = 2;
        this.General.RowCount = 13;
        this.General.AutoUpdateNotes = true;
        this.General.Cells.Get(0, 0).Value = "State: ";
        this.General.Cells.Get(1, 0).Value = "Region: ";
        this.General.Cells.Get(2, 0).Value = "Nearest Town: ";
        this.General.Cells.Get(3, 0).Value = "Site: ";
        this.General.Cells.Get(4, 0).Value = "Name: ";
        this.General.Cells.Get(4, 1).Locked = true;
        this.General.Cells.Get(5, 0).Value = "APSoil Number: ";
        this.General.Cells.Get(6, 0).Value = "Classification: ";
        this.General.Cells.Get(7, 0).Value = "Latitude (WGS84): ";
        this.General.Cells.Get(8, 0).Value = "Longitude (WGS84): ";
        this.General.Cells.Get(9, 0).Value = "Location accuracy: ";
        this.General.Cells.Get(10, 0).Value = "Natural Vegetation: ";
        this.General.Cells.Get(11, 0).Value = "Data source: ";
        textCellType1.MaxLength = 500;
        textCellType1.Multiline = true;
        textCellType1.WordWrap = true;
        this.General.Cells.Get(11, 1).CellType = textCellType1;
        this.General.Cells.Get(12, 0).Value = "Comments: ";
        textCellType2.MaxLength = 500;
        textCellType2.Multiline = true;
        textCellType2.WordWrap = true;
        this.General.Cells.Get(12, 1).CellType = textCellType2;
        this.General.ColumnHeader.Visible = false;
        this.General.Columns.Get(0).Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
        this.General.Columns.Get(0).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Right;
        this.General.Columns.Get(0).Locked = false;
        this.General.Columns.Get(0).Width = 139F;
        this.General.Columns.Get(1).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Left;
        this.General.Columns.Get(1).Width = 550F;
        this.General.FrozenColumnCount = 1;
        this.General.RestrictColumns = true;
        this.General.RowHeader.Columns.Default.Resizable = false;
        this.General.RowHeader.Visible = false;
        this.General.Rows.Get(11).Height = 41F;
        this.General.Rows.Get(12).Height = 105F;
        this.General.SheetName = "General";
        this.General.CellChanged += new FarPoint.Win.Spread.SheetViewEventHandler(this.OnGeneralCellChanged);
        this.General.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.A1;
        // 
        // Water
        // 
        this.Water.Reset();
        // Formulas and custom names must be loaded with R1C1 reference style
        this.Water.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.R1C1;
        this.Water.ColumnCount = 7;
        this.Water.ColumnHeader.RowCount = 3;
        this.Water.RowCount = 100;
        this.Water.AutoUpdateNotes = true;
        this.Water.ColumnHeader.Cells.Get(0, 0).Border = bevelBorder1;
        this.Water.ColumnHeader.Cells.Get(0, 0).ColumnSpan = 7;
        this.Water.ColumnHeader.Cells.Get(0, 0).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Center;
        this.Water.ColumnHeader.Cells.Get(0, 0).Value = "Soil properties";
        this.Water.ColumnHeader.Cells.Get(0, 6).VerticalAlignment = FarPoint.Win.Spread.CellVerticalAlignment.General;
        this.Water.ColumnHeader.Cells.Get(1, 0).Border = bevelBorder2;
        this.Water.ColumnHeader.Cells.Get(1, 0).Value = "Depth";
        this.Water.ColumnHeader.Cells.Get(1, 1).Value = "BD";
        this.Water.ColumnHeader.Cells.Get(1, 2).Value = "% Rocks";
        this.Water.ColumnHeader.Cells.Get(1, 3).Value = "SAT";
        this.Water.ColumnHeader.Cells.Get(1, 4).Value = " DUL";
        this.Water.ColumnHeader.Cells.Get(1, 5).Value = "AirDry";
        this.Water.ColumnHeader.Cells.Get(1, 6).Value = "LL15";
        this.Water.ColumnHeader.Cells.Get(2, 0).Value = "(cm)";
        this.Water.ColumnHeader.Cells.Get(2, 1).Value = " (g/cc)";
        this.Water.ColumnHeader.Cells.Get(2, 2).Value = "(%)";
        this.Water.ColumnHeader.Cells.Get(2, 3).Value = "(mm/mm)";
        this.Water.ColumnHeader.Cells.Get(2, 4).Value = "(mm/mm)";
        this.Water.ColumnHeader.Cells.Get(2, 5).Value = "(mm/mm)";
        this.Water.ColumnHeader.Cells.Get(2, 6).Value = "(mm/mm)";
        this.Water.Columns.Get(0).Label = "(cm)";
        this.Water.Columns.Get(0).Width = 53F;
        this.Water.Columns.Get(1).Label = " (g/cc)";
        this.Water.Columns.Get(1).Width = 45F;
        this.Water.Columns.Get(2).Label = "(%)";
        this.Water.Columns.Get(2).Width = 52F;
        this.Water.Columns.Get(3).Label = "(mm/mm)";
        this.Water.Columns.Get(3).Width = 55F;
        this.Water.Columns.Get(4).Label = "(mm/mm)";
        this.Water.Columns.Get(4).Width = 56F;
        this.Water.Columns.Get(5).Label = "(mm/mm)";
        this.Water.Columns.Get(5).Width = 56F;
        this.Water.Columns.Get(6).Label = "(mm/mm)";
        this.Water.Columns.Get(6).Width = 56F;
        this.Water.FrozenColumnCount = 7;
        this.Water.FrozenTrailingRowCount = 1;
        this.Water.RowHeader.Columns.Default.Resizable = false;
        this.Water.RowHeader.Visible = false;
        this.Water.SheetName = "Water";
        this.Water.CellChanged += new FarPoint.Win.Spread.SheetViewEventHandler(this.OnWaterCellChanged);
        this.Water.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.A1;
        // 
        // SoilProfile
        // 
        this.SoilProfile.Reset();
        // Formulas and custom names must be loaded with R1C1 reference style
        this.SoilProfile.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.R1C1;
        this.SoilProfile.ColumnCount = 23;
        this.SoilProfile.ColumnHeader.RowCount = 2;
        this.SoilProfile.RowCount = 100;
        this.SoilProfile.AutoUpdateNotes = true;
        this.SoilProfile.ColumnHeader.AutoText = FarPoint.Win.Spread.HeaderAutoText.Blank;
        this.SoilProfile.ColumnHeader.Cells.Get(0, 0).Value = "Depth";
        this.SoilProfile.ColumnHeader.Cells.Get(0, 1).Value = "Texture";
        this.SoilProfile.ColumnHeader.Cells.Get(0, 2).Value = "SWCon";
        this.SoilProfile.ColumnHeader.Cells.Get(0, 3).Value = "MWCon";
        this.SoilProfile.ColumnHeader.Cells.Get(0, 4).Value = "FBiom";
        this.SoilProfile.ColumnHeader.Cells.Get(0, 5).Value = "Finert";
        this.SoilProfile.ColumnHeader.Cells.Get(0, 6).Value = "KS";
        this.SoilProfile.ColumnHeader.Cells.Get(0, 7).Value = "OC";
        this.SoilProfile.ColumnHeader.Cells.Get(0, 8).Value = "EC";
        this.SoilProfile.ColumnHeader.Cells.Get(0, 9).Value = "pH";
        this.SoilProfile.ColumnHeader.Cells.Get(0, 10).Value = "Cl";
        this.SoilProfile.ColumnHeader.Cells.Get(0, 11).Value = "Boron";
        this.SoilProfile.ColumnHeader.Cells.Get(0, 12).Value = "CEC";
        this.SoilProfile.ColumnHeader.Cells.Get(0, 13).Value = "Ca";
        this.SoilProfile.ColumnHeader.Cells.Get(0, 14).Value = "Mg";
        this.SoilProfile.ColumnHeader.Cells.Get(0, 15).Value = "Na";
        this.SoilProfile.ColumnHeader.Cells.Get(0, 16).Value = "K";
        this.SoilProfile.ColumnHeader.Cells.Get(0, 17).Value = "ESP";
        this.SoilProfile.ColumnHeader.Cells.Get(0, 18).Value = "Mn";
        this.SoilProfile.ColumnHeader.Cells.Get(0, 19).Value = "Al";
        this.SoilProfile.ColumnHeader.Cells.Get(0, 20).Value = "Particle size";
        this.SoilProfile.ColumnHeader.Cells.Get(0, 21).Value = "Particle size";
        this.SoilProfile.ColumnHeader.Cells.Get(0, 22).Value = "Particle size";
        this.SoilProfile.ColumnHeader.Cells.Get(1, 0).Value = "(cm)";
        this.SoilProfile.ColumnHeader.Cells.Get(1, 2).Value = "(0-1)";
        this.SoilProfile.ColumnHeader.Cells.Get(1, 3).Value = "(0-1)";
        this.SoilProfile.ColumnHeader.Cells.Get(1, 4).Value = "(0-1)";
        this.SoilProfile.ColumnHeader.Cells.Get(1, 5).Value = "(0-1)";
        this.SoilProfile.ColumnHeader.Cells.Get(1, 6).Value = "(mm/day)";
        this.SoilProfile.ColumnHeader.Cells.Get(1, 7).Value = "(%)";
        this.SoilProfile.ColumnHeader.Cells.Get(1, 8).Value = "(mS/cm)";
        this.SoilProfile.ColumnHeader.Cells.Get(1, 9).Locked = false;
        this.SoilProfile.ColumnHeader.Cells.Get(1, 9).Value = "(water)";
        this.SoilProfile.ColumnHeader.Cells.Get(1, 10).Value = "(mg/kg)";
        this.SoilProfile.ColumnHeader.Cells.Get(1, 11).Value = "(mg/kg)";
        this.SoilProfile.ColumnHeader.Cells.Get(1, 12).Value = "(cmol+/kg)";
        this.SoilProfile.ColumnHeader.Cells.Get(1, 13).Value = "(cmol+/kg)";
        this.SoilProfile.ColumnHeader.Cells.Get(1, 14).Value = "(cmol+/kg)";
        this.SoilProfile.ColumnHeader.Cells.Get(1, 15).Value = "(cmol+/kg)";
        this.SoilProfile.ColumnHeader.Cells.Get(1, 16).Value = "(cmol+/kg)";
        this.SoilProfile.ColumnHeader.Cells.Get(1, 17).Value = "(%)";
        this.SoilProfile.ColumnHeader.Cells.Get(1, 18).Value = "(mg/kg)";
        this.SoilProfile.ColumnHeader.Cells.Get(1, 19).Value = "(meq/100g)";
        this.SoilProfile.ColumnHeader.Cells.Get(1, 20).Value = "sand (%)";
        this.SoilProfile.ColumnHeader.Cells.Get(1, 21).Value = "silt (%)";
        this.SoilProfile.ColumnHeader.Cells.Get(1, 22).Value = "clay (%)";
        this.SoilProfile.Columns.Get(19).Label = "(meq/100g)";
        this.SoilProfile.Columns.Get(19).Width = 72F;
        this.SoilProfile.Columns.Get(20).Label = "sand (%)";
        this.SoilProfile.Columns.Get(20).Width = 79F;
        this.SoilProfile.Columns.Get(21).Label = "silt (%)";
        this.SoilProfile.Columns.Get(21).Width = 79F;
        this.SoilProfile.Columns.Get(22).Label = "clay (%)";
        this.SoilProfile.Columns.Get(22).Width = 79F;
        this.SoilProfile.Protect = false;
        this.SoilProfile.RowHeader.Columns.Default.Resizable = false;
        this.SoilProfile.SheetName = "Soil profile";
        this.SoilProfile.CellChanged += new FarPoint.Win.Spread.SheetViewEventHandler(this.OnProfileCellChanged);
        this.SoilProfile.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.A1;
        // 
        // APSIM
        // 
        this.APSIM.Reset();
        // Formulas and custom names must be loaded with R1C1 reference style
        this.APSIM.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.R1C1;
        this.APSIM.ColumnCount = 2;
        this.APSIM.RowCount = 24;
        this.APSIM.ActiveColumnIndex = 1;
        this.APSIM.ActiveRowIndex = 7;
        this.APSIM.AutoUpdateNotes = true;
        this.APSIM.Cells.Get(0, 0).Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
        this.APSIM.Cells.Get(0, 0).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Right;
        this.APSIM.Cells.Get(0, 0).Value = "Evaporation";
        comboBoxCellType1.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
        comboBoxCellType1.Items = new string[] {
        "1 value for U and Cona",
        "Summer/Winter values for U and Cona"};
        this.APSIM.Cells.Get(0, 1).CellType = comboBoxCellType1;
        this.APSIM.Cells.Get(0, 1).Locked = false;
        this.APSIM.Cells.Get(0, 1).Value = "1 value for U and Cona";
        this.APSIM.Cells.Get(1, 0).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Right;
        this.APSIM.Cells.Get(1, 0).Value = "U";
        this.APSIM.Cells.Get(2, 0).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Right;
        this.APSIM.Cells.Get(2, 0).Value = "Cona";
        this.APSIM.Cells.Get(3, 0).ForeColor = System.Drawing.Color.LightSteelBlue;
        this.APSIM.Cells.Get(3, 0).Value = "SummerU";
        this.APSIM.Cells.Get(4, 0).ForeColor = System.Drawing.Color.LightSteelBlue;
        this.APSIM.Cells.Get(4, 0).Value = "WinterU";
        this.APSIM.Cells.Get(5, 0).ForeColor = System.Drawing.Color.LightSteelBlue;
        this.APSIM.Cells.Get(5, 0).Value = "SummerCona";
        this.APSIM.Cells.Get(6, 0).ForeColor = System.Drawing.Color.LightSteelBlue;
        this.APSIM.Cells.Get(6, 0).Value = "WinterCona";
        this.APSIM.Cells.Get(7, 0).ForeColor = System.Drawing.Color.LightSteelBlue;
        this.APSIM.Cells.Get(7, 0).Value = "SummerDate";
        this.APSIM.Cells.Get(7, 1).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Right;
        this.APSIM.Cells.Get(8, 0).ForeColor = System.Drawing.Color.LightSteelBlue;
        this.APSIM.Cells.Get(8, 0).Value = "WinterDate";
        this.APSIM.Cells.Get(8, 1).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Right;
        this.APSIM.Cells.Get(9, 0).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Right;
        this.APSIM.Cells.Get(9, 0).Value = "Salb";
        this.APSIM.Cells.Get(10, 0).Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
        this.APSIM.Cells.Get(10, 0).Value = "Unsaturated Flow";
        this.APSIM.Cells.Get(10, 1).Locked = true;
        this.APSIM.Cells.Get(11, 0).Value = "DiffusConst";
        this.APSIM.Cells.Get(12, 0).Value = "DiffusSlope";
        this.APSIM.Cells.Get(13, 0).Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
        this.APSIM.Cells.Get(13, 0).Value = "Runoff";
        this.APSIM.Cells.Get(13, 1).Locked = true;
        this.APSIM.Cells.Get(14, 0).Value = "CN2Bare";
        this.APSIM.Cells.Get(15, 0).Value = "CNRed";
        this.APSIM.Cells.Get(16, 0).Value = "CNCov";
        this.APSIM.Cells.Get(17, 0).Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
        this.APSIM.Cells.Get(17, 0).Value = "Organic Matter";
        this.APSIM.Cells.Get(17, 1).Locked = true;
        this.APSIM.Cells.Get(18, 0).Value = "RootCN";
        this.APSIM.Cells.Get(19, 0).Value = "RootWt";
        this.APSIM.Cells.Get(20, 0).Value = "SoilCN";
        this.APSIM.Cells.Get(21, 0).Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
        this.APSIM.Cells.Get(21, 0).Value = "Erosion";
        this.APSIM.Cells.Get(21, 1).Locked = true;
        this.APSIM.Cells.Get(22, 0).Value = "EnrACoeff";
        this.APSIM.Cells.Get(23, 0).Value = "EnrBCoeff";
        this.APSIM.ColumnHeader.Visible = false;
        this.APSIM.Columns.Get(0).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Right;
        this.APSIM.Columns.Get(0).Locked = true;
        this.APSIM.Columns.Get(0).Width = 101F;
        this.APSIM.Columns.Get(1).Width = 213F;
        this.APSIM.RowHeader.Columns.Default.Resizable = false;
        this.APSIM.SheetName = "APSIM";
        this.APSIM.CellChanged += new FarPoint.Win.Spread.SheetViewEventHandler(this.OnAPSIMCellChanged);
        this.APSIM.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.A1;
        // 
        // Phosphorus
        // 
        this.Phosphorus.Reset();
        // Formulas and custom names must be loaded with R1C1 reference style
        this.Phosphorus.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.R1C1;
        this.Phosphorus.ColumnCount = 5;
        this.Phosphorus.ColumnHeader.RowCount = 2;
        this.Phosphorus.RowCount = 100;
        this.Phosphorus.ActiveColumnIndex = 4;
        this.Phosphorus.AutoUpdateNotes = true;
        this.Phosphorus.ColumnHeader.AutoText = FarPoint.Win.Spread.HeaderAutoText.Blank;
        this.Phosphorus.ColumnHeader.Cells.Get(0, 0).Value = "Depth";
        this.Phosphorus.ColumnHeader.Cells.Get(0, 1).Value = "Labile P";
        this.Phosphorus.ColumnHeader.Cells.Get(0, 2).Value = "Banded P";
        this.Phosphorus.ColumnHeader.Cells.Get(0, 3).Value = "Rock P";
        this.Phosphorus.ColumnHeader.Cells.Get(0, 4).Value = "Sorption";
        this.Phosphorus.ColumnHeader.Cells.Get(1, 0).Value = "(cm)";
        this.Phosphorus.ColumnHeader.Cells.Get(1, 1).Value = "(mg/kg)";
        this.Phosphorus.ColumnHeader.Cells.Get(1, 2).Value = "(kg/ha)";
        this.Phosphorus.ColumnHeader.Cells.Get(1, 3).Value = "(kg/ha)";
        this.Phosphorus.Columns.Get(0).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Right;
        this.Phosphorus.Columns.Get(0).Label = "(cm)";
        this.Phosphorus.Columns.Get(0).Width = 70F;
        this.Phosphorus.Columns.Get(1).CellType = textCellType3;
        this.Phosphorus.Columns.Get(1).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Right;
        this.Phosphorus.Columns.Get(1).Label = "(mg/kg)";
        this.Phosphorus.Columns.Get(1).Width = 71F;
        this.Phosphorus.Columns.Get(2).CellType = textCellType4;
        this.Phosphorus.Columns.Get(2).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Right;
        this.Phosphorus.Columns.Get(2).Label = "(kg/ha)";
        this.Phosphorus.Columns.Get(3).CellType = textCellType5;
        this.Phosphorus.Columns.Get(3).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Right;
        this.Phosphorus.Columns.Get(3).Label = "(kg/ha)";
        this.Phosphorus.Columns.Get(4).CellType = textCellType6;
        this.Phosphorus.Columns.Get(4).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Right;
        this.Phosphorus.RowHeader.Columns.Default.Resizable = false;
        this.Phosphorus.Rows.Get(14).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Right;
        this.Phosphorus.SheetName = "Phosphorus";
        this.Phosphorus.CellChanged += new FarPoint.Win.Spread.SheetViewEventHandler(this.OnPhosphorusCellChanged);
        this.Phosphorus.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.A1;
        // 
        // PhotoAttachSheet
        // 
        this.PhotoAttachSheet.Reset();
        // Formulas and custom names must be loaded with R1C1 reference style
        this.PhotoAttachSheet.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.R1C1;
        this.PhotoAttachSheet.ColumnCount = 4;
        this.PhotoAttachSheet.ColumnHeader.RowCount = 0;
        this.PhotoAttachSheet.RowCount = 2;
        this.PhotoAttachSheet.RowHeader.ColumnCount = 0;
        this.PhotoAttachSheet.ActiveRowIndex = 1;
        this.PhotoAttachSheet.AutoUpdateNotes = true;
        buttonCellType1.Text = "Load";
        this.PhotoAttachSheet.Cells.Get(0, 0).CellType = buttonCellType1;
        buttonCellType2.Text = "Remove";
        this.PhotoAttachSheet.Cells.Get(0, 1).CellType = buttonCellType2;
        buttonCellType3.Text = "Show";
        this.PhotoAttachSheet.Cells.Get(0, 2).CellType = buttonCellType3;
        this.PhotoAttachSheet.Cells.Get(0, 3).Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
        this.PhotoAttachSheet.Cells.Get(0, 3).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Center;
        this.PhotoAttachSheet.Cells.Get(0, 3).Locked = true;
        this.PhotoAttachSheet.Cells.Get(0, 3).VerticalAlignment = FarPoint.Win.Spread.CellVerticalAlignment.Center;
        imageCellType1.Style = FarPoint.Win.RenderStyle.StretchAndScale;
        imageCellType1.TransparencyColor = System.Drawing.Color.Empty;
        imageCellType1.TransparencyTolerance = 0;
        this.PhotoAttachSheet.Cells.Get(1, 0).CellType = imageCellType1;
        this.PhotoAttachSheet.Cells.Get(1, 0).ColumnSpan = 4;
        this.PhotoAttachSheet.Cells.Get(1, 0).Locked = true;
        this.PhotoAttachSheet.Columns.Get(0).Locked = false;
        this.PhotoAttachSheet.Columns.Get(1).Locked = false;
        this.PhotoAttachSheet.Columns.Get(2).Locked = false;
        this.PhotoAttachSheet.Columns.Get(3).Locked = false;
        this.PhotoAttachSheet.Columns.Get(3).Width = 357F;
        this.PhotoAttachSheet.HorizontalGridLine = new FarPoint.Win.Spread.GridLine(FarPoint.Win.Spread.GridLineType.Flat, System.Drawing.Color.LightGray, System.Drawing.SystemColors.ControlLightLight, System.Drawing.SystemColors.ControlDark, 0);
        this.PhotoAttachSheet.RowHeader.Columns.Default.Resizable = false;
        this.PhotoAttachSheet.Rows.Get(0).Height = 38F;
        this.PhotoAttachSheet.Rows.Get(1).Height = 392F;
        this.PhotoAttachSheet.SheetName = "Photo/Attach";
        this.PhotoAttachSheet.VerticalGridLine = new FarPoint.Win.Spread.GridLine(FarPoint.Win.Spread.GridLineType.Flat, System.Drawing.Color.LightGray, System.Drawing.SystemColors.ControlLightLight, System.Drawing.SystemColors.ControlDark, 0);
        this.PhotoAttachSheet.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.A1;
        // 
        // PrintForm
        // 
        this.PrintForm.AutoFit = TMGDevelopment.Windows.Forms.PageElement.Body;
        this.PrintForm.BodyContainer = this;
        this.PrintForm.CenterStyle = TMGDevelopment.Windows.Forms.CenterStyle.None;
        this.PrintForm.PreDraw += new TMGDevelopment.Windows.Forms.PreDrawEventHandler(this.OnPreDraw);
        // 
        // OpenAttachmentDialog
        // 
        this.OpenAttachmentDialog.Filter = "All files|*.*";
        this.OpenAttachmentDialog.RestoreDirectory = true;
        // 
        // WaterChartControl
        // 
        this.WaterChartControl.Dock = System.Windows.Forms.DockStyle.Fill;
        this.WaterChartControl.LinkedSoil = null;
        this.WaterChartControl.Location = new System.Drawing.Point(0, 310);
        this.WaterChartControl.Name = "WaterChartControl";
        this.WaterChartControl.Size = new System.Drawing.Size(906, 452);
        this.WaterChartControl.TabIndex = 16;
        // 
        // SoilUI
        // 
        this.Controls.Add(this.WaterChartControl);
        this.Controls.Add(this.Grid);
        this.Name = "SoilUI";
        this.Size = new System.Drawing.Size(906, 762);
        this.Controls.SetChildIndex(this.Grid, 0);
        this.Controls.SetChildIndex(this.WaterChartControl, 0);
        ((System.ComponentModel.ISupportInitialize)(this.Grid)).EndInit();
        ((System.ComponentModel.ISupportInitialize)(this.General)).EndInit();
        ((System.ComponentModel.ISupportInitialize)(this.Water)).EndInit();
        ((System.ComponentModel.ISupportInitialize)(this.SoilProfile)).EndInit();
        ((System.ComponentModel.ISupportInitialize)(this.APSIM)).EndInit();
        ((System.ComponentModel.ISupportInitialize)(this.Phosphorus)).EndInit();
        ((System.ComponentModel.ISupportInitialize)(this.PhotoAttachSheet)).EndInit();
        this.ResumeLayout(false);
		}
		#endregion



        protected override void OnLoad()
            {
            FarPoint.Win.Spread.InputMap InputMap = Grid.GetInputMap(FarPoint.Win.Spread.InputMapMode.WhenAncestorOfFocused);
            InputMap.Put(new FarPoint.Win.Spread.Keystroke(Keys.Delete, Keys.None),
                        FarPoint.Win.Spread.SpreadActions.ClipboardCut);
            InputMap.Put(new FarPoint.Win.Spread.Keystroke(Keys.Enter, Keys.None),
                        FarPoint.Win.Spread.SpreadActions.MoveToNextRow);
            this.PopupMenuChange();     //set the Grid context menu for the first time.
        }

		override public void OnRefresh()
			{
			MySoil = new Soil(Data);

			HelpText = "";
			WaterChartControl.LinkedSoil = MySoil;
			PopulateGeneralGrid();
			PopulateAttachGrid();
			PopulateWaterGrid();
			PopulateProfileGrid();
			PopulateAPSIMGrid();
			PopulatePhosphorusGrid();
			SetupCellNotes();
			OperationMode mode = OperationMode.Normal;
			
			if (Controller.ApsimData.IsReadOnly)
				mode = OperationMode.ReadOnly;
				
			General.OperationMode = mode;
			Water.OperationMode = mode;
			SoilProfile.OperationMode = mode;
			APSIM.OperationMode = mode;
			Phosphorus.OperationMode = mode;

            }

        private void OnActiveSheetChanged(object sender, System.EventArgs e)
            {
            this.PopupMenuChange();     //change the Grid context menu
            }


        #region PopupMenu
        //Change the Grid context menu depening on which sheet you are looking at.
        private void PopupMenuChange()
        {
            if (Grid.ActiveSheetIndex == 1)      // Water sheet is currently open   
            {
                // use the BaseController to populate the context menu using the actions.xml file. 
                // "ContextMenu_SoilUI_Management" is the name of the xml section in the actions.xml file.

                Grid.ContextMenuStrip = Controller.CreateUIContextMenu("ContextMenu_SoilUI_Management");
            }
            else if (Grid.ActiveSheetIndex == 2) // Profile sheet is currently open  
            {
                Grid.ContextMenuStrip = Controller.CreateUIContextMenu("ContextMenu_SoilUI_PH");
            }
            else if ((Grid.ActiveSheetIndex != 1) & (Grid.ActiveSheetIndex != 2))   // Any other sheet is currently open
            {
                // clear the context menu if the active sheet is not the Water or Profile sheet. 
                Grid.ContextMenuStrip = null;
            }
        }
        # endregion

        #region General Grid
        private void PopulateGeneralGrid()
			{
            UserChange = false;
			General.ClearRange(0, 1, General.RowCount, 1, true);
            General.Cells[0, 1].Value = MySoil.State;
            General.Cells[1, 1].Value = MySoil.Region;
            General.Cells[2, 1].Value = MySoil.NearestTown;
            General.Cells[3, 1].Value = MySoil.Site;
			General.Cells[4, 1].Value = MySoil.Name;
            General.Cells[5, 1].Value = MySoil.ApsoilNumber;
            General.Cells[6, 1].Value = MySoil.Classification;
            General.Cells[7, 1].Value = MySoil.Latitude;
            General.Cells[8, 1].Value = MySoil.Longitude;
            General.Cells[9, 1].Value = MySoil.LocationAccuracy;
            General.Cells[10, 1].Value = MySoil.NaturalVegetation;
			General.Cells[11, 1].Value = MySoil.DataSource;
			General.Cells[12, 1].Value = MySoil.Comment;
			UserChange = true;
			}
		private void SaveGeneralGrid()
			{
            MySoil.State = GridUtility.GetCellAsString(General, 1, 0);
            MySoil.Region = GridUtility.GetCellAsString(General, 1, 1);
            MySoil.NearestTown = GridUtility.GetCellAsString(General, 1, 2);
            MySoil.Site = GridUtility.GetCellAsString(General, 1, 3);
            MySoil.ApsoilNumber = GridUtility.GetCellAsString(General, 1, 5);
            MySoil.Classification = GridUtility.GetCellAsString(General, 1, 6);
            MySoil.Latitude = Convert.ToDouble(GridUtility.GetCellAsString(General, 1, 7));
            MySoil.Longitude = Convert.ToDouble(GridUtility.GetCellAsString(General, 1, 8));
            MySoil.LocationAccuracy = GridUtility.GetCellAsString(General, 1, 9);
            MySoil.NaturalVegetation = GridUtility.GetCellAsString(General, 1, 10);
			MySoil.DataSource = GridUtility.GetCellAsString(General, 1, 11);
			MySoil.Comment = GridUtility.GetCellAsString(General, 1, 12);
			}
        private void OnGeneralCellChanged(object sender, FarPoint.Win.Spread.SheetViewEventArgs e)
            {
            if (UserChange)
                {
                UserChange = false;
                try
                    {
                    SaveGeneralGrid();
                    if (e.Row == 3)
                        PopulateWaterGrid();
                    }
                catch (Exception err)
                    {
                    MessageBox.Show(err.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                    }

                UserChange = true;
                }
            }
        #endregion

        #region Water Grid
        private void PopulateWaterGrid()
			{
			UserChange = false;
            Water.ClearRange(0, 0, Water.RowCount, Water.ColumnCount, false);
			GridUtility.SetColumnAsStrings(Water, 0, SoilComponentUtility.ToDepthStrings(MySoil.Thickness));
			GridUtility.SetColumnAsDoubles(Water, 1, MySoil.BD);
            GridUtility.SetColumnAsDoubles(Water, 2, MySoil.Rocks);
			GridUtility.SetColumnAsDoubles(Water, 3, MySoil.SAT);
			GridUtility.SetColumnAsDoubles(Water, 4, MySoil.DUL);
			GridUtility.SetColumnAsDoubles(Water, 5, MySoil.Airdry);
			GridUtility.SetColumnAsDoubles(Water, 6, MySoil.LL15);

			// Make sure we have the right number of crop columns.
			string[] CropNames = MySoil.Crops;
			int RequiredNumberOfColumns = CropNames.Length*4 + NUMBER_OF_STATIC_COLS;
			Water.ColumnCount = RequiredNumberOfColumns;

			// Fill all crop columns with numbers.
			for (int CropNumber = 0; CropNumber != CropNames.Length; CropNumber++)
				{
				int CropCol = NUMBER_OF_STATIC_COLS + CropNumber*4;
				string CropName = CropNames[CropNumber];
				Color CropColor = Color.GreenYellow;
				if (CropNumber / 2.0 != CropNumber / 2)
					CropColor = Color.PaleGreen;
				Color PredCropColor = Color.Pink;
				if (CropNumber / 2.0 != CropNumber / 2)
					PredCropColor = Color.LightPink;

				// setup the LL column.
				bool Predicted = MySoil.CropIsPredicted(CropName);
                if (Predicted)
                    {
                    Water.ColumnHeader.Cells[0, CropCol].Text = "Predicted " + CropName;
                    FarPoint.Win.Spread.CellType.NumberCellType PredLLFormatter = new FarPoint.Win.Spread.CellType.NumberCellType();
                    PredLLFormatter.DecimalPlaces = 2;
                    Water.Columns[CropCol].CellType = PredLLFormatter;
                    }
                else
                    {
                    Water.ColumnHeader.Cells[0, CropCol].Text = CropName;
                    Water.Columns[CropCol].CellType = null;
                    }
				Water.ColumnHeader.Cells[0, CropCol].ColumnSpan = 4;
				Water.ColumnHeader.Cells[0, CropCol].HorizontalAlignment = CellHorizontalAlignment.Center;
				Water.ColumnHeader.Cells[1, CropCol].Text = "LL";
				Water.ColumnHeader.Cells[2, CropCol].Text = "mm/mm";
				GridUtility.SetColumnAsDoubles(Water, CropCol, MySoil.LL(CropName));

				if (Predicted)
					Water.Columns[CropCol].BackColor = PredCropColor;
				else
					Water.Columns[CropCol].BackColor = CropColor;
            Water.Columns[CropCol].Locked = Predicted;
            Water.Columns[CropCol].Width = 60;

				// setup the PAWC column.
				Water.ColumnHeader.Cells[1,CropCol+1].Text = "PAWC";
				Water.ColumnHeader.Cells[2,CropCol+1].Text = "(mm)";
				Water.Columns[CropCol+1].Locked = true;
				if (Predicted)
					Water.Columns[CropCol+1].BackColor = PredCropColor;
				else
					Water.Columns[CropCol+1].BackColor = CropColor;
				Water.Columns[CropCol+1].Width = 45;
				RefreshPAWCColumn(CropCol+1);
				FarPoint.Win.Spread.CellType.NumberCellType PAWCFormatter = new FarPoint.Win.Spread.CellType.NumberCellType();
				PAWCFormatter.DecimalPlaces = 1;
				Water.Columns[CropCol+1].CellType = PAWCFormatter;

				// setup the KL column
				Water.ColumnHeader.Cells[1, CropCol+2].Text = "KL";
				Water.ColumnHeader.Cells[2, CropCol+2].Text = " ";
				GridUtility.SetColumnAsDoubles(Water, CropCol+2, MySoil.KL(CropName));
				if (Predicted)
					Water.Columns[CropCol+2].BackColor = PredCropColor;
				else
					Water.Columns[CropCol+2].BackColor = CropColor;
            Water.Columns[CropCol + 2].Locked = Predicted;
            Water.Columns[CropCol + 2].Width = 45;

				// setup the XF column
				Water.ColumnHeader.Cells[1, CropCol+3].Text = "XF";
				Water.ColumnHeader.Cells[2, CropCol+3].Text = " ";
				GridUtility.SetColumnAsDoubles(Water, CropCol+3, MySoil.XF(CropName));
				if (Predicted)
					Water.Columns[CropCol+3].BackColor = PredCropColor;
				else
					Water.Columns[CropCol+3].BackColor = CropColor;
            Water.Columns[CropCol + 3].Locked = Predicted;
            Water.Columns[CropCol + 3].Width = 45;
				}

			AddSummaryRow();
			UserChange = true;
			}
		private void SaveWaterGrid()
			{	
			for (int col = 0; col != Water.ColumnCount; col++)
				SaveWaterGridColumn(col);
			}
		private void SaveWaterGridColumn(int ColumnIndex)
			{
			if (ColumnIndex == 0)
				AddSummaryRow();
			int NumLayers = GridUtility.FindFirstBlankCell(Water, 0);
			switch (ColumnIndex)
				{
				case 0: MySoil.Thickness = SoilComponentUtility.ToThickness(GridUtility.GetColumnAsStrings(Water, 0, NumLayers)); break;
				case 1: MySoil.BD = GridUtility.GetColumnAsDoubles(Water, 1, NumLayers); break;
                case 2: MySoil.Rocks = GridUtility.GetColumnAsDoubles(Water, 2, NumLayers); break;
                case 3: MySoil.SAT = GridUtility.GetColumnAsDoubles(Water, 3, NumLayers); break;
                case 4: MySoil.DUL = GridUtility.GetColumnAsDoubles(Water, 4, NumLayers); RefreshPAWCColumns();  break;
				case 5: MySoil.Airdry = GridUtility.GetColumnAsDoubles(Water, 5, NumLayers); break;
				case 6: MySoil.LL15 = GridUtility.GetColumnAsDoubles(Water, 6, NumLayers); break;
				default:
					{
					int CropCol = (ColumnIndex - NUMBER_OF_STATIC_COLS) / 4 * 4 + NUMBER_OF_STATIC_COLS;
					string CropName = Water.ColumnHeader.Cells[0, CropCol].Text;
					if (!CropName.StartsWith("Predicted "))
						{
						string[] CropNames = MySoil.Crops;
						// Save all crop columns
						double[] ll = GridUtility.GetColumnAsDoubles(Water, CropCol, NumLayers);
						double[] kl = GridUtility.GetColumnAsDoubles(Water, CropCol+2, NumLayers);
						double[] xf = GridUtility.GetColumnAsDoubles(Water, CropCol+3, NumLayers);
						MySoil.SetCrop(CropName, ll, kl, xf); 
						RefreshPAWCColumn(CropCol+1);
						}
					break;
					}
				}

                        
			WaterChartControl.RefreshView();
			}
        private void RefreshPAWCColumns()
            {
            for (int col = 0; col != Water.ColumnCount; col++)
                if (Water.ColumnHeader.Cells[1, col].Text == "PAWC")
                    RefreshPAWCColumn(col);
            }
        private void RefreshPAWCColumn(int ColumnIndex)
            {
            string CropName = Water.ColumnHeader.Cells[0, ColumnIndex - 1].Text.Replace("Predicted ", "");
            double[] paw = MySoil.PAWC(CropName);
            GridUtility.SetColumnAsDoubles(Water, ColumnIndex, paw);
            Water.Columns[ColumnIndex].Locked = true;
            AddSummaryRow();
            }
        private void AddSummaryRow()
            {
            int NumDepths = GridUtility.FindFirstBlankCell(Water, 0);
            if (NumDepths > 0)
                {
                int SummaryRow = Water.RowCount - 1;

                // set the blank row.
                Water.Cells[SummaryRow, 0, SummaryRow, Water.ColumnCount - 1].Formula = "";
                Water.Cells[SummaryRow, 0, SummaryRow, Water.ColumnCount - 1].Text = "";
                Water.Rows[SummaryRow].BackColor = Color.White;

                // set the summary row.
                Water.Cells[SummaryRow, 0].Text = "Totals";
                for (int col = NUMBER_OF_STATIC_COLS + 1; col < Water.ColumnCount; col += 4)
                    {
                    int ColForFormula = col + 1;
                    int EndRowForFormula = SummaryRow - 1;
                    string SumFormula = "SUM(R1C" + ColForFormula.ToString() + ":R" + EndRowForFormula.ToString() + "C" + ColForFormula.ToString() + ")";
                    Water.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.R1C1;
                    Water.Cells[SummaryRow, col].Formula = SumFormula;
                    }
                Water.Rows[SummaryRow].BackColor = Color.Yellow;
                }
            }
        private void OnWaterCellChanged(object sender, FarPoint.Win.Spread.SheetViewEventArgs e)
            {
            if (UserChange)
                {
                UserChange = false;
                SaveWaterGridColumn(e.Column);
                UserChange = true;
                }
            }
        #endregion

        #region Profile Grid
        private void PopulateProfileGrid()
			{
			UserChange = false;
			SoilProfile.ClearRange(0, 0, SoilProfile.RowCount, SoilProfile.ColumnCount, true);
			GridUtility.SetColumnAsStrings(SoilProfile, 0, SoilComponentUtility.ToDepthStrings(MySoil.Thickness));
            GridUtility.SetColumnAsStrings(SoilProfile, 1, MySoil.Texture);
            GridUtility.SetColumnAsDoubles(SoilProfile, 2, MySoil.SWCON);
			GridUtility.SetColumnAsDoubles(SoilProfile, 3, MySoil.MWCON);
			GridUtility.SetColumnAsDoubles(SoilProfile, 4, MySoil.FBIOM);
			GridUtility.SetColumnAsDoubles(SoilProfile, 5, MySoil.FINERT);
            GridUtility.SetColumnAsDoubles(SoilProfile, 6, MySoil.KS);
            GridUtility.SetColumnAsDoubles(SoilProfile, 7, MySoil.OC);
			GridUtility.SetColumnAsDoubles(SoilProfile, 8, MySoil.EC);
			GridUtility.SetColumnAsDoubles(SoilProfile, 10, MySoil.CL);
			GridUtility.SetColumnAsDoubles(SoilProfile, 11, MySoil.Boron);
			GridUtility.SetColumnAsDoubles(SoilProfile, 12, MySoil.CEC);
			GridUtility.SetColumnAsDoubles(SoilProfile, 13, MySoil.Ca);
			GridUtility.SetColumnAsDoubles(SoilProfile, 14, MySoil.Mg);
			GridUtility.SetColumnAsDoubles(SoilProfile, 15, MySoil.Na);
			GridUtility.SetColumnAsDoubles(SoilProfile, 16, MySoil.K);
			GridUtility.SetColumnAsDoubles(SoilProfile, 17, MySoil.ESP);
            GridUtility.SetColumnAsDoubles(SoilProfile, 18, MySoil.Mn);
            GridUtility.SetColumnAsDoubles(SoilProfile, 19, MySoil.Al);
            GridUtility.SetColumnAsDoubles(SoilProfile, 20, MySoil.ParticleSizeSand);
			GridUtility.SetColumnAsDoubles(SoilProfile, 21, MySoil.ParticleSizeSilt);
			GridUtility.SetColumnAsDoubles(SoilProfile, 22, MySoil.ParticleSizeClay);
            if (MySoil.PHStoredAsWater())
                {
                SoilProfile.ColumnHeader.Cells[1, 9].Text = "(water)";
                GridUtility.SetColumnAsDoubles(SoilProfile, 9, MySoil.PH);
                }
            else
                {
                SoilProfile.ColumnHeader.Cells[1, 9].Text = "(CaCl)";
                GridUtility.SetColumnAsDoubles(SoilProfile, 9, MySoil.PHCaCl);
                UserChange = true;
                }
			}
		private void SaveProfileGrid(int ColumnIndex)
			{
			int NumLayers = GridUtility.FindFirstBlankCell(SoilProfile, 0);
			switch (ColumnIndex)
				{
                case 1: MySoil.Texture = GridUtility.GetColumnAsStrings(SoilProfile, 1, NumLayers); break;
                case 2: MySoil.SWCON = GridUtility.GetColumnAsDoubles(SoilProfile, 2, NumLayers); break;
				case 3: MySoil.MWCON  = GridUtility.GetColumnAsDoubles(SoilProfile, 3, NumLayers); break;
				case 4: MySoil.FBIOM  = GridUtility.GetColumnAsDoubles(SoilProfile, 4, NumLayers);  break;
				case 5: MySoil.FINERT = GridUtility.GetColumnAsDoubles(SoilProfile, 5, NumLayers);  break;
				case 6: MySoil.KS     = GridUtility.GetColumnAsDoubles(SoilProfile, 6, NumLayers);  break;
                case 7: MySoil.OC = GridUtility.GetColumnAsDoubles(SoilProfile, 7, NumLayers); break;
                case 8: MySoil.EC = GridUtility.GetColumnAsDoubles(SoilProfile, 8, NumLayers); break;
                case 9: if (SoilProfile.ColumnHeader.Cells[1, 9].Text == "(water)")
                            MySoil.PH = GridUtility.GetColumnAsDoubles(SoilProfile, 9, NumLayers);  
                        else
                            MySoil.PHCaCl = GridUtility.GetColumnAsDoubles(SoilProfile, 9, NumLayers);
                        break;
				case 10: MySoil.CL     = GridUtility.GetColumnAsDoubles(SoilProfile, 10, NumLayers);  break;
				case 11: MySoil.Boron     = GridUtility.GetColumnAsDoubles(SoilProfile, 11, NumLayers);  break;
				case 12: MySoil.CEC    = GridUtility.GetColumnAsDoubles(SoilProfile, 12, NumLayers);  break;
				case 13: MySoil.Ca     = GridUtility.GetColumnAsDoubles(SoilProfile, 13, NumLayers);  break;
				case 14: MySoil.Mg     = GridUtility.GetColumnAsDoubles(SoilProfile, 14, NumLayers); break;
				case 15: MySoil.Na     = GridUtility.GetColumnAsDoubles(SoilProfile, 15, NumLayers); break;
				case 16: MySoil.K      = GridUtility.GetColumnAsDoubles(SoilProfile, 16, NumLayers); break;
				case 17: MySoil.ESP    = GridUtility.GetColumnAsDoubles(SoilProfile, 17, NumLayers); break;
                case 18: MySoil.Mn = GridUtility.GetColumnAsDoubles(SoilProfile, 18, NumLayers); break;
                case 19: MySoil.Al = GridUtility.GetColumnAsDoubles(SoilProfile, 19, NumLayers); break;
                case 20: MySoil.ParticleSizeSand = GridUtility.GetColumnAsDoubles(SoilProfile, 20, NumLayers); break;
				case 21: MySoil.ParticleSizeSilt = GridUtility.GetColumnAsDoubles(SoilProfile, 21, NumLayers); break;
				case 22: MySoil.ParticleSizeClay = GridUtility.GetColumnAsDoubles(SoilProfile, 22, NumLayers); break;
				}
            }
        private void OnProfileCellChanged(object sender, FarPoint.Win.Spread.SheetViewEventArgs e)
            {
            if (UserChange)
                {
                UserChange = false;
                SaveProfileGrid(e.Column);
                UserChange = true;
                }
            }
        #endregion

        #region APSIM Grid
        private void PopulateAPSIMGrid()
			{
			UserChange = false;
			APSIM.ClearRange(0, 1, APSIM.RowCount, 1, true);

            FarPoint.Win.Spread.CellType.ComboBoxCellType SummerWinterCombo = (FarPoint.Win.Spread.CellType.ComboBoxCellType) APSIM.Cells[0, 1].CellType;
            if (MySoil.SummerCona != MathUtility.MissingValue)
                APSIM.Cells[0, 1].Text = SummerWinterCombo.Items[1];
            else
                APSIM.Cells[0, 1].Text = SummerWinterCombo.Items[0];

            SetupConaU();

			GridUtility.SetCellAsDouble(APSIM, 1, 9, MySoil.Salb);
			GridUtility.SetCellAsDouble(APSIM, 1, 11, MySoil.DiffusConst);
			GridUtility.SetCellAsDouble(APSIM, 1, 12, MySoil.DiffusSlope);
			GridUtility.SetCellAsDouble(APSIM, 1, 14, MySoil.CN2Bare);
			GridUtility.SetCellAsDouble(APSIM, 1, 15, MySoil.CNRed);
			GridUtility.SetCellAsDouble(APSIM, 1, 16, MySoil.CNCov);
			GridUtility.SetCellAsDouble(APSIM, 1, 18, MySoil.RootCN);
			GridUtility.SetCellAsDouble(APSIM, 1, 19, MySoil.RootWT);
			GridUtility.SetCellAsDouble(APSIM, 1, 20, MySoil.SoilCN);
			GridUtility.SetCellAsDouble(APSIM, 1, 22, MySoil.EnrACoeff);
			GridUtility.SetCellAsDouble(APSIM, 1, 23, MySoil.EnrBCoeff);
			UserChange = true;
			
			}
        private void SetupConaU()
            {
            FarPoint.Win.Spread.CellType.ComboBoxCellType SummerWinterCombo = (FarPoint.Win.Spread.CellType.ComboBoxCellType) APSIM.Cells[0, 1].CellType;
            if (APSIM.Cells[0, 1].Text == SummerWinterCombo.Items[1])
                {
                APSIM.Cells[1, 0].ForeColor = Color.LightSteelBlue;
                APSIM.Cells[2, 0].ForeColor = Color.LightSteelBlue;
                APSIM.Cells[3, 0].ForeColor = Color.Black;
                APSIM.Cells[4, 0].ForeColor = Color.Black;
                APSIM.Cells[5, 0].ForeColor = Color.Black;
                APSIM.Cells[6, 0].ForeColor = Color.Black;
                APSIM.Cells[7, 0].ForeColor = Color.Black;
                APSIM.Cells[8, 0].ForeColor = Color.Black;
                APSIM.Cells[1, 1].Locked = true;
                APSIM.Cells[2, 1].Locked = true;
                APSIM.Cells[3, 1].Locked = false;
                APSIM.Cells[4, 1].Locked = false;
                APSIM.Cells[5, 1].Locked = false;
                APSIM.Cells[6, 1].Locked = false;
                APSIM.Cells[7, 1].Locked = false;
                APSIM.Cells[8, 1].Locked = false;
                APSIM.Cells[1, 1].Text = "";
                APSIM.Cells[2, 1].Text = "";
                GridUtility.SetCellAsDouble(APSIM, 1, 3, MySoil.SummerU);
                GridUtility.SetCellAsDouble(APSIM, 1, 4, MySoil.WinterU);
                GridUtility.SetCellAsDouble(APSIM, 1, 5, MySoil.SummerCona);
                GridUtility.SetCellAsDouble(APSIM, 1, 6, MySoil.WinterCona);
                APSIM.Cells[7, 1].Text = MySoil.SummerDate;
                APSIM.Cells[8, 1].Text = MySoil.WinterDate;
                }
            else
                {
                APSIM.Cells[1, 0].ForeColor = Color.Black;
                APSIM.Cells[2, 0].ForeColor = Color.Black;
                APSIM.Cells[3, 0].ForeColor = Color.LightSteelBlue;
                APSIM.Cells[4, 0].ForeColor = Color.LightSteelBlue;
                APSIM.Cells[5, 0].ForeColor = Color.LightSteelBlue;
                APSIM.Cells[6, 0].ForeColor = Color.LightSteelBlue;
                APSIM.Cells[7, 0].ForeColor = Color.LightSteelBlue;
                APSIM.Cells[8, 0].ForeColor = Color.LightSteelBlue;
                APSIM.Cells[1, 1].Locked = false;
                APSIM.Cells[2, 1].Locked = false;
                APSIM.Cells[3, 1].Locked = true;
                APSIM.Cells[4, 1].Locked = true;
                APSIM.Cells[5, 1].Locked = true;
                APSIM.Cells[6, 1].Locked = true;
                APSIM.Cells[7, 1].Locked = true;
                APSIM.Cells[8, 1].Locked = true;
                GridUtility.SetCellAsDouble(APSIM, 1, 1, MySoil.U);
                GridUtility.SetCellAsDouble(APSIM, 1, 2, MySoil.Cona);
                APSIM.Cells[3, 1].Text = "";
                APSIM.Cells[4, 1].Text = "";
                APSIM.Cells[5, 1].Text = "";
                APSIM.Cells[6, 1].Text = "";
                APSIM.Cells[7, 1].Text = "";
                APSIM.Cells[8, 1].Text = "";
                }
            }
		private void SaveAPSIMGrid()
			{
            FarPoint.Win.Spread.CellType.ComboBoxCellType SummerWinterCombo = (FarPoint.Win.Spread.CellType.ComboBoxCellType)APSIM.Cells[0, 1].CellType;
            if (APSIM.Cells[0, 1].Text == SummerWinterCombo.Items[0])
                MySoil.SetUCona(GridUtility.GetCellAsDouble(APSIM, 1, 1), GridUtility.GetCellAsDouble(APSIM, 1, 2));
            else
                MySoil.SetSummerWinterUCona(GridUtility.GetCellAsDouble(APSIM, 1, 3), GridUtility.GetCellAsDouble(APSIM, 1, 4),
                                            GridUtility.GetCellAsDouble(APSIM, 1, 5), GridUtility.GetCellAsDouble(APSIM, 1, 6),
                                            APSIM.Cells[7, 1].Text, APSIM.Cells[8, 1].Text);
                
			MySoil.Salb = GridUtility.GetCellAsDouble(APSIM, 1, 9);
			MySoil.DiffusConst = GridUtility.GetCellAsDouble(APSIM, 1, 11);
			MySoil.DiffusSlope = GridUtility.GetCellAsDouble(APSIM, 1, 12);
			MySoil.CN2Bare = GridUtility.GetCellAsDouble(APSIM, 1, 14);
			MySoil.CNRed = GridUtility.GetCellAsDouble(APSIM, 1, 15);
			MySoil.CNCov = GridUtility.GetCellAsDouble(APSIM, 1, 16);
			MySoil.RootCN = GridUtility.GetCellAsDouble(APSIM, 1, 18);
			MySoil.RootWT = GridUtility.GetCellAsDouble(APSIM, 1, 19);
			MySoil.SoilCN = GridUtility.GetCellAsDouble(APSIM, 1, 20);
			MySoil.EnrACoeff = GridUtility.GetCellAsDouble(APSIM, 1, 22);
			MySoil.EnrBCoeff = GridUtility.GetCellAsDouble(APSIM, 1, 23);
            }
        private void OnAPSIMCellChanged(object sender, FarPoint.Win.Spread.SheetViewEventArgs e)
            {
            if (UserChange)
                {
                UserChange = false;
                if (e.Row == 0)
                    SetupConaU();
                SaveAPSIMGrid();
                UserChange = true;
                }
            }
        #endregion

        #region Phosphorus Grid
        private void PopulatePhosphorusGrid()
			{
			UserChange = false;
			Phosphorus.ClearRange(0, 0, Phosphorus.RowCount, Phosphorus.ColumnCount, true);
			GridUtility.SetColumnAsStrings(Phosphorus, 0, SoilComponentUtility.ToDepthStrings(MySoil.Thickness));
			GridUtility.SetColumnAsDoubles(Phosphorus, 1, MySoil.LabileP);
			GridUtility.SetColumnAsDoubles(Phosphorus, 2, MySoil.BandedP);
			GridUtility.SetColumnAsDoubles(Phosphorus, 3, MySoil.RockP);
			GridUtility.SetColumnAsDoubles(Phosphorus, 4, MySoil.Sorption);

			int NumLayers = GridUtility.FindFirstBlankCell(Phosphorus, 0);
			int FirstStaticRow = NumLayers + 1;

			Phosphorus.Cells[FirstStaticRow,0].Value = "Root CP:";
			GridUtility.SetCellAsDouble(Phosphorus, 1, FirstStaticRow, MySoil.RootCP);

			Phosphorus.Cells[FirstStaticRow+1,0].Value = "RateDissolRock:";
			GridUtility.SetCellAsDouble(Phosphorus, 1, FirstStaticRow+1, MySoil.RateDissolRock);

            Phosphorus.Cells[FirstStaticRow+2, 0].Value = "RateLossAvail:";
            GridUtility.SetCellAsDouble(Phosphorus, 1, FirstStaticRow+2, MySoil.RateLossAvail);
            
            UserChange = true;
			}
		private void SavePhosphorusGrid()
			{
			int NumLayers = GridUtility.FindFirstBlankCell(Phosphorus, 0);
			MySoil.LabileP  = GridUtility.GetColumnAsDoubles(Phosphorus, 1, NumLayers);
			MySoil.BandedP  = GridUtility.GetColumnAsDoubles(Phosphorus, 2, NumLayers);
			MySoil.RockP  = GridUtility.GetColumnAsDoubles(Phosphorus, 3, NumLayers);
			MySoil.Sorption  = GridUtility.GetColumnAsDoubles(Phosphorus, 4, NumLayers);

			int FirstStaticRow = NumLayers + 1;
			MySoil.RootCP = GridUtility.GetCellAsDouble(Phosphorus, 1, FirstStaticRow);
			MySoil.RateDissolRock = GridUtility.GetCellAsDouble(Phosphorus, 1, FirstStaticRow+1);
            MySoil.RateLossAvail = GridUtility.GetCellAsDouble(Phosphorus, 1, FirstStaticRow+2);
            }
        private void OnPhosphorusCellChanged(object sender, FarPoint.Win.Spread.SheetViewEventArgs e)
            {
            if (UserChange)
                {
                UserChange = false;
                SavePhosphorusGrid();
                UserChange = true;
                }
            }
        #endregion

        #region Attachment Grid
        private void PopulateAttachGrid()
            {
            PhotoAttachSheet.Cells[0, 3].Text = MySoil.AttachmentFileName;
            string AttachmentExt = Path.GetExtension(PhotoAttachSheet.Cells[0, 3].Text).ToLower();
            FarPoint.Win.Spread.CellType.ImageCellType Image = (FarPoint.Win.Spread.CellType.ImageCellType)
                                                               PhotoAttachSheet.Cells[1, 0].CellType;

            if (AttachmentExt == ".jpg" || AttachmentExt == ".gif" || AttachmentExt == ".bmp" ||
                AttachmentExt == ".png")
                {
                PhotoAttachSheet.Cells[1, 0].Value = MySoil.Attachment;
                }
            else
                PhotoAttachSheet.Cells[1, 0].Value = "";
            }
        private void OnAttachGridButtonClicked(object sender, FarPoint.Win.Spread.EditorNotifyEventArgs e)
            {
            if (File.Exists(AttachmentFileName))
                File.Delete(AttachmentFileName);

            if (e.Column == 0)
                {
                // User has clicked on load.
                if (OpenAttachmentDialog.ShowDialog() == DialogResult.OK)
                    {
                    if (File.Exists(AttachmentFileName))
                        File.Delete(AttachmentFileName);

                    MySoil.AttachmentFileName = OpenAttachmentDialog.FileName;
                    PopulateAttachGrid();
                    }
                }
            else if (e.Column == 1)
                {
                // user has clicked on delete
                MySoil.AttachmentFileName = "";
                PopulateAttachGrid();
                }
            else if (e.Column == 2)
                {
                // user has clicked on show.
                AttachmentFileName = MySoil.CreateAttachment();
                System.Diagnostics.Process.Start(AttachmentFileName);
                }
            }
        #endregion

		
		private void SetupCellNotes()
			{
			foreach (Soil.Note note in MySoil.GetNotes())
				{
				if (note.GridName == "General")
					{
					General.Cells[note.Row, note.Col].Note = note.Text;
					General.Cells[note.Row, note.Col].BackColor = Color.LightGoldenrodYellow; 
					}
				else if (note.GridName == "Water")
					{
					Water.Cells[note.Row, note.Col].Note = note.Text;
					Water.Cells[note.Row, note.Col].BackColor = Color.LightGoldenrodYellow; 
					}
				else if (note.GridName == "Soil profile")
					{	
					SoilProfile.Cells[note.Row, note.Col].Note = note.Text;
					SoilProfile.Cells[note.Row, note.Col].BackColor = Color.LightGoldenrodYellow; 
					}
				else if (note.GridName == "APSIM")
					{
					APSIM.Cells[note.Row, note.Col].Note = note.Text;
					APSIM.Cells[note.Row, note.Col].BackColor = Color.LightGoldenrodYellow; 
					}

				else if (note.GridName == "Phosphorus")
					{
					Phosphorus.Cells[note.Row, note.Col].Note = note.Text;
					Phosphorus.Cells[note.Row, note.Col].BackColor = Color.LightGoldenrodYellow; 
					}
				}
			}

		private void AddNote()
			{
			string NoteText = UIBits.InputDialog.InputBox("Enter text:", "New note", "", false);
			if (NoteText != "")
				{
				FarPoint.Win.Spread.Model.CellRange Range = Grid.ActiveSheet.GetSelection(0);
				for (int Row = Range.Row; Row < Range.Row + Range.RowCount; Row++)
					for (int Col = Range.Column; Col < Range.Column + Range.ColumnCount; Col++)
						MySoil.AddNote(Grid.ActiveSheet.SheetName, Col, Row, NoteText);
				}
			}

		private void DeleteNote()
			{
			if (Grid.ActiveSheet.SelectionCount > 0)
				{
				FarPoint.Win.Spread.Model.CellRange Range = Grid.ActiveSheet.GetSelection(0);
				for (int Row = Range.Row; Row < Range.Row + Range.RowCount; Row++)
					for (int Col = Range.Column; Col < Range.Column + Range.ColumnCount; Col++)
						MySoil.DeleteNote(Grid.ActiveSheet.SheetName, Col, Row);
				Grid.ActiveSheet.Cells[Range.Row, Range.Column, Range.Row+Range.RowCount-1, Range.Column+Range.ColumnCount-1].Note = "";
				Grid.ActiveSheet.Cells[Range.Row, Range.Column, Range.Row+Range.RowCount-1, Range.Column+Range.ColumnCount-1].BackColor = Color.White;
				}
            }

        #region Printing Methods
        private int CurrentStartCol;
        private int SlotHeight;
        private int NumSlots = 3;
        private int NumFixedCols = 6;
        private int NumFreeColsToPrint = 8;
        private int MarginBetweenSlots = 0;
        public void OnBeginPrint(object sender, System.Drawing.Printing.PrintEventArgs e)
            {
            CurrentStartCol = NumFixedCols;
            SlotHeight = 0;
            }
        public void OnPrintPage(object sender, System.Drawing.Printing.PrintPageEventArgs e)
            {
            if (SlotHeight == 0)
                SlotHeight = CalcSlotHeight(e);

            e.Graphics.Clip = new Region(e.MarginBounds);
            e.HasMorePages = true;
            for (int slot = 0; slot < NumSlots && e.HasMorePages; slot++)
                {
                Rectangle r = new Rectangle(e.MarginBounds.Left, e.MarginBounds.Top + slot * (SlotHeight + MarginBetweenSlots),
                                            e.MarginBounds.Right - e.MarginBounds.Left,
                                            SlotHeight);
                if (CurrentStartCol >= Water.ColumnCount)
                    {
                    // print the graph.
                    PrintForm.PrintControl(e.Graphics, r, WaterChartControl.WaterChart, 1.0F);

                    e.HasMorePages = false;
                    }
                else
                    {
                    PrintForm.PrintControl(e.Graphics, r, Grid, 1.0F);
                    CurrentStartCol += NumFreeColsToPrint;
                    }
                }
            e.Graphics.ResetClip();
            }

        private int CalcSlotHeight(System.Drawing.Printing.PrintPageEventArgs e)
            {
            Water.PrintInfo.ColStart = 0;
            Water.PrintInfo.ColEnd = 6;
            Water.PrintInfo.RowStart = 0;
            Water.PrintInfo.RowEnd = GridUtility.FindFirstBlankCell(Water, 0) - 1;
            Water.PrintInfo.PrintType = PrintType.CellRange;
            Rectangle r = new Rectangle(e.MarginBounds.Left, e.MarginBounds.Top,
                                        e.MarginBounds.Width, 200);
            do
                {
                r.Height = r.Height + 10;
                }
            while (Grid.GetOwnerPrintPageCount(e.Graphics, r, 1) > 1);
            return r.Height;
            }

        public void OnPreDraw(object sender, TMGDevelopment.Windows.Forms.PreDrawEventArgs e)
            {
            if (e.Control == Grid)
                {
                Water.PrintInfo.ColStart = CurrentStartCol;
                Water.PrintInfo.ColEnd = CurrentStartCol + NumFreeColsToPrint - 1;
                Water.PrintInfo.PrintType = PrintType.CellRange;
                Grid.OwnerPrintDraw(e.Graphics, e.Bounds, 1, 1);
                e.OwnerDrawn = true;
                }
            else if (e.Control == WaterChartControl.WaterChart)
                {
                Bitmap b = new Bitmap(e.Bounds.Width, e.Bounds.Height);
                WaterChartControl.WaterChart.DrawToBitmap(b, e.Bounds);
                e.Graphics.DrawImage(b, e.Bounds);
                e.OwnerDrawn = true;
                }
            }
        #endregion

        private void OnGetTextTip(object sender, FarPoint.Win.Spread.TextTipFetchEventArgs e)
            {
            e.ShowTip = false;
            if (e.View.ActiveSheetIndex == 1)
                {
                if (e.Row < 3)
                    {
                    if (e.Column >= NUMBER_OF_STATIC_COLS)
                        {
                        if (Water.ColumnHeader.Cells[1, e.Column].Text == "LL")
                            e.TipText = Water.ColumnHeader.Cells[0, e.Column].Text + ": Crop lower limit (mm water/mm soil)";
                        else if (Water.ColumnHeader.Cells[1, e.Column].Text == "PAWC")
                            e.TipText = Water.ColumnHeader.Cells[0, e.Column-1].Text + ": Calculated plant available water capacity (mm water/mm soil)";
                        else if (Water.ColumnHeader.Cells[1, e.Column].Text == "KL")
                            e.TipText = Water.ColumnHeader.Cells[0, e.Column-2].Text + ": Water extraction parameter (0-1)";
                        else if (Water.ColumnHeader.Cells[1, e.Column].Text == "XF")
                            e.TipText = Water.ColumnHeader.Cells[0, e.Column-3].Text + ": Root exploration parameter (0-1)";
                        }
                    else
                        {
                        switch (e.Column)
                            {
                            case (0): e.TipText = "Depth of layer (cm)"; break;
                            case (1): e.TipText = "Bulk density (gm dry soil/cc moist soil)"; break;
                            case (2): e.TipText = "Rocks (%)"; break;
                            case (3): e.TipText = "Saturation (mm water/mm soil)"; break;
                            case (4): e.TipText = "Drained upper limit (mm water/mm soil)"; break;
                            case (5): e.TipText = "Air dry (mm water/mm soil)"; break;
                            case (6): e.TipText = "Lower limit 15 bar (mm water/mm soil)"; break;
                            }
                        }
                    e.ShowTip = true;
                    }
                }
            else if (e.View.ActiveSheetIndex == 2)
                {
                if (e.Row < 2)
                    {
                    switch (e.Column)
                        {
                        case (0): e.TipText = "Depth of layer (cm)"; break;
                        case (1): e.TipText = "Texture of soil"; break;
                        case (2): e.TipText = "The proportion of soil water above dul that drains in one day (0-1)"; break;
                        case (3): e.TipText = "A value of 0 indicates the layer is considered impermeable to ‘cascading' flow, hence water cascading down the soil profile will reach this layer and begin to back-up towards the surface. Drainage is still allowed through this layer unless ‘swcon' is also set to zero. (0-1)"; break;
                        case (4): e.TipText = "Microbe fraction of humic pool (0-1)"; break;
                        case (5): e.TipText = "Inert fraction of humic pool (0-1)"; break;
                        case (6): e.TipText = "Saturated conductivity (mm/day) - optional"; break;
                        case (7): e.TipText = "Soil Organic Carbon [Method = Walkley Black] (%)"; break;
                        case (8): e.TipText = "Electrical conductivity (mS/cm or dS/m)"; break;
                        case (9): e.TipText = "pH (Water or Calcium chloride)"; break;
                        case (10): e.TipText = "Cloride (mg/kg)"; break;
                        case (11): e.TipText = "Boron (mg/kg)"; break;
                        case (12): e.TipText = "Cec (meq/100g or cmol+/kg)"; break;
                        case (13): e.TipText = "Ca (meq/100g or cmol+/kg)"; break;
                        case (14): e.TipText = "Mg (meq/100g or cmol+/kg)"; break;
                        case (15): e.TipText = "Na (meq/100g or cmol+/kg)"; break;
                        case (16): e.TipText = "K (meq/100g or cmol+/kg)"; break;
                        case (17): e.TipText = "ESP (%)"; break;
                        case (18): e.TipText = "Particle size sand (%)"; break;
                        case (19): e.TipText = "Particle size silt (%)"; break;
                        case (20): e.TipText = "Particle size clay (%)"; break;
                        }
                    e.ShowTip = true;
                    }
                }
            else if (e.View.ActiveSheetIndex == 3)
                {
                if (e.Column == 0)
                    {
                    switch (e.Row)
                        {
                        case (0): e.TipText = "There are 2 ways of specifying U and Cona - single values for each or summer and winter values for each"; e.ShowTip = true; break;
                        case (1): e.TipText = "U (as in CERES) is the amount of cumulative evaporation, since soil wetting, before soil supply becomes limiting - stage 1"; e.ShowTip = true; break;
                        case (2): e.TipText = "Stage 2: Soil evaporation is a fraction of the square root of time since the end of first stage evaporation, using the regression coefficient CONA"; e.ShowTip = true; break;
                        case (3): e.TipText = "U value to use in Summer"; e.ShowTip = true; break;
                        case (4): e.TipText = "U value to use in Winter"; e.ShowTip = true; break;
                        case (5): e.TipText = "Cona value to use in Summer"; e.ShowTip = true; break;
                        case (6): e.TipText = "Cona value to use in Winter"; e.ShowTip = true; break;
                        case (7): e.TipText = "The start date of summer (e.g. 15-sep)"; e.ShowTip = true; break;
                        case (8): e.TipText = "The start date of winter (e.g. 15-apr)"; e.ShowTip = true; break;
                        case (9): e.TipText = "Bare soil albedo"; e.ShowTip = true; break;
                        case (11):
                        case (12): e.TipText = "Diffusivity = diffus_const x exp(diffus_slope x thet_av)"; e.ShowTip = true; break;
                        case (14): e.TipText = "Runoff curve number for BARE soil at AMC2"; e.ShowTip = true; break;
                        case (15): e.TipText = "Reduction in CN2_bare for 'cn_cov' increase in cover"; e.ShowTip = true; break;
                        case (16): e.TipText = "Frac. cover for 'cn_red' reduction in cover & max. cover for reduction"; e.ShowTip = true; break;
                        case (18): e.TipText = "C:N ratio of initial root residues"; e.ShowTip = true; break;
                        case (19): e.TipText = "Initial weight of root FOM in soil profile (kg/ha)"; e.ShowTip = true; break;
                        case (20): e.TipText = "C:N ratio of the soil"; e.ShowTip = true; break;
                        case (22):
                        case (23): e.TipText = "enrichment ratio ENR = enr_a_coeff * (1000.0 * soil_loss)**(-1.0 * enr_b_coeff)"; e.ShowTip = true; break;
                        }
                    e.ShowTip = true;
                    }
                }
            }


		 }

	}

