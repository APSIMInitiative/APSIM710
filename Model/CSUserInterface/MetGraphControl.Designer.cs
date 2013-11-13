namespace CSUserInterface
{
    partial class MetGraphControl
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(MetGraphControl));
            this.TabImages = new System.Windows.Forms.ImageList(this.components);
            this.TabControl = new System.Windows.Forms.TabControl();
            this.TabPage1 = new System.Windows.Forms.TabPage();
            this.YearPanel = new System.Windows.Forms.Panel();
            this.Label5 = new System.Windows.Forms.Label();
            this.Label4 = new System.Windows.Forms.Label();
            this.YearStartBox = new System.Windows.Forms.NumericUpDown();
            this.NumYearsBox = new System.Windows.Forms.NumericUpDown();
            this.Label3 = new System.Windows.Forms.Label();
            this.ContentsBox = new System.Windows.Forms.TextBox();
            this.TabPage2 = new System.Windows.Forms.TabPage();
            this.Label1 = new System.Windows.Forms.Label();
            this.RainfallLabel = new System.Windows.Forms.Label();
            this.RainfallChart = new Steema.TeeChart.TChart();
            this.RainfallBar = new Steema.TeeChart.Styles.Bar();
            this.TabPage3 = new System.Windows.Forms.TabPage();
            this.MonthlyRainfallChart = new Steema.TeeChart.TChart();
            this.MonthlyRainfallBar = new Steema.TeeChart.Styles.Bar();
            this.MonthlyEvaporationLine = new Steema.TeeChart.Styles.Line();
            this.TabPage5 = new System.Windows.Forms.TabPage();
            this.TemperatureChart = new Steema.TeeChart.TChart();
            this.MaximumTemperatureLine = new Steema.TeeChart.Styles.Line();
            this.MinimumTemperatureLine = new Steema.TeeChart.Styles.Line();
            this.TabPage4 = new System.Windows.Forms.TabPage();
            this.RadiationChart = new Steema.TeeChart.TChart();
            this.RainfallBar2 = new Steema.TeeChart.Styles.Bar();
            this.RadiationLine = new Steema.TeeChart.Styles.Line();
            this.MaximumRadiationLine = new Steema.TeeChart.Styles.Line();
            this.TabControl.SuspendLayout();
            this.TabPage1.SuspendLayout();
            this.YearPanel.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.YearStartBox)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.NumYearsBox)).BeginInit();
            this.TabPage2.SuspendLayout();
            this.TabPage3.SuspendLayout();
            this.TabPage5.SuspendLayout();
            this.TabPage4.SuspendLayout();
            this.SuspendLayout();
            // 
            // MyHelpLabel
            // 
            this.MyHelpLabel.Size = new System.Drawing.Size(1015, 16);
            // 
            // TabImages
            // 
            this.TabImages.ImageStream = ((System.Windows.Forms.ImageListStreamer)(resources.GetObject("TabImages.ImageStream")));
            this.TabImages.TransparentColor = System.Drawing.Color.Transparent;
            this.TabImages.Images.SetKeyName(0, "text.png");
            this.TabImages.Images.SetKeyName(1, "drink_blue.png");
            this.TabImages.Images.SetKeyName(2, "thermometer.png");
            this.TabImages.Images.SetKeyName(3, "sunglasses.png");
            // 
            // TabControl
            // 
            this.TabControl.Controls.Add(this.TabPage1);
            this.TabControl.Controls.Add(this.TabPage2);
            this.TabControl.Controls.Add(this.TabPage3);
            this.TabControl.Controls.Add(this.TabPage5);
            this.TabControl.Controls.Add(this.TabPage4);
            this.TabControl.Dock = System.Windows.Forms.DockStyle.Fill;
            this.TabControl.ImageList = this.TabImages;
            this.TabControl.Location = new System.Drawing.Point(0, 16);
            this.TabControl.Name = "TabControl";
            this.TabControl.SelectedIndex = 0;
            this.TabControl.Size = new System.Drawing.Size(1015, 708);
            this.TabControl.TabIndex = 15;
            this.TabControl.SelectedIndexChanged += new System.EventHandler(this.TabControl_SelectedIndexChanged);
            // 
            // TabPage1
            // 
            this.TabPage1.Controls.Add(this.YearPanel);
            this.TabPage1.Controls.Add(this.ContentsBox);
            this.TabPage1.ImageIndex = 0;
            this.TabPage1.Location = new System.Drawing.Point(4, 23);
            this.TabPage1.Name = "TabPage1";
            this.TabPage1.Padding = new System.Windows.Forms.Padding(3);
            this.TabPage1.Size = new System.Drawing.Size(1007, 681);
            this.TabPage1.TabIndex = 0;
            this.TabPage1.Text = "Raw data";
            this.TabPage1.UseVisualStyleBackColor = true;
            // 
            // YearPanel
            // 
            this.YearPanel.BackColor = System.Drawing.Color.White;
            this.YearPanel.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.YearPanel.Controls.Add(this.Label5);
            this.YearPanel.Controls.Add(this.Label4);
            this.YearPanel.Controls.Add(this.YearStartBox);
            this.YearPanel.Controls.Add(this.NumYearsBox);
            this.YearPanel.Controls.Add(this.Label3);
            this.YearPanel.Location = new System.Drawing.Point(6, 26);
            this.YearPanel.Name = "YearPanel";
            this.YearPanel.Size = new System.Drawing.Size(295, 31);
            this.YearPanel.TabIndex = 19;
            // 
            // Label5
            // 
            this.Label5.AutoSize = true;
            this.Label5.Location = new System.Drawing.Point(130, 7);
            this.Label5.Name = "Label5";
            this.Label5.Size = new System.Drawing.Size(34, 13);
            this.Label5.TabIndex = 20;
            this.Label5.Text = "Show";
            // 
            // Label4
            // 
            this.Label4.AutoSize = true;
            this.Label4.Location = new System.Drawing.Point(3, 7);
            this.Label4.Name = "Label4";
            this.Label4.Size = new System.Drawing.Size(32, 13);
            this.Label4.TabIndex = 19;
            this.Label4.Text = "Year:";
            // 
            // YearStartBox
            // 
            this.YearStartBox.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.YearStartBox.CausesValidation = false;
            this.YearStartBox.Font = new System.Drawing.Font("Microsoft Sans Serif", 9F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.YearStartBox.Location = new System.Drawing.Point(56, 3);
            this.YearStartBox.Maximum = new decimal(new int[] {
            2200,
            0,
            0,
            0});
            this.YearStartBox.Minimum = new decimal(new int[] {
            1800,
            0,
            0,
            0});
            this.YearStartBox.Name = "YearStartBox";
            this.YearStartBox.Size = new System.Drawing.Size(64, 21);
            this.YearStartBox.TabIndex = 16;
            this.YearStartBox.Value = new decimal(new int[] {
            2000,
            0,
            0,
            0});
            // 
            // NumYearsBox
            // 
            this.NumYearsBox.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.NumYearsBox.CausesValidation = false;
            this.NumYearsBox.Font = new System.Drawing.Font("Microsoft Sans Serif", 9F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.NumYearsBox.Location = new System.Drawing.Point(183, 3);
            this.NumYearsBox.Maximum = new decimal(new int[] {
            200,
            0,
            0,
            0});
            this.NumYearsBox.Minimum = new decimal(new int[] {
            1,
            0,
            0,
            0});
            this.NumYearsBox.Name = "NumYearsBox";
            this.NumYearsBox.Size = new System.Drawing.Size(49, 21);
            this.NumYearsBox.TabIndex = 17;
            this.NumYearsBox.Value = new decimal(new int[] {
            1,
            0,
            0,
            0});
            // 
            // Label3
            // 
            this.Label3.AutoSize = true;
            this.Label3.Location = new System.Drawing.Point(238, 7);
            this.Label3.Name = "Label3";
            this.Label3.Size = new System.Drawing.Size(38, 13);
            this.Label3.TabIndex = 18;
            this.Label3.Text = "year(s)";
            // 
            // ContentsBox
            // 
            this.ContentsBox.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.ContentsBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.ContentsBox.Font = new System.Drawing.Font("Courier New", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.ContentsBox.Location = new System.Drawing.Point(3, 3);
            this.ContentsBox.Multiline = true;
            this.ContentsBox.Name = "ContentsBox";
            this.ContentsBox.ReadOnly = true;
            this.ContentsBox.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
            this.ContentsBox.Size = new System.Drawing.Size(1001, 675);
            this.ContentsBox.TabIndex = 0;
            this.ContentsBox.WordWrap = false;
            // 
            // TabPage2
            // 
            this.TabPage2.Controls.Add(this.Label1);
            this.TabPage2.Controls.Add(this.RainfallLabel);
            this.TabPage2.Controls.Add(this.RainfallChart);
            this.TabPage2.ImageIndex = 1;
            this.TabPage2.Location = new System.Drawing.Point(4, 23);
            this.TabPage2.Name = "TabPage2";
            this.TabPage2.Padding = new System.Windows.Forms.Padding(3);
            this.TabPage2.Size = new System.Drawing.Size(1007, 681);
            this.TabPage2.TabIndex = 1;
            this.TabPage2.Text = "Rainfall chart";
            this.TabPage2.UseVisualStyleBackColor = true;
            // 
            // Label1
            // 
            this.Label1.AutoSize = true;
            this.Label1.Location = new System.Drawing.Point(303, 11);
            this.Label1.Name = "Label1";
            this.Label1.Size = new System.Drawing.Size(72, 13);
            this.Label1.TabIndex = 16;
            this.Label1.Text = "Total Rainfall:";
            // 
            // RainfallLabel
            // 
            this.RainfallLabel.AutoSize = true;
            this.RainfallLabel.Location = new System.Drawing.Point(381, 11);
            this.RainfallLabel.Name = "RainfallLabel";
            this.RainfallLabel.Size = new System.Drawing.Size(19, 13);
            this.RainfallLabel.TabIndex = 15;
            this.RainfallLabel.Text = "L1";
            // 
            // RainfallChart
            // 
            // 
            // 
            // 
            this.RainfallChart.Aspect.View3D = false;
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            this.RainfallChart.Axes.Bottom.AxisPen.Width = 1;
            // 
            // 
            // 
            this.RainfallChart.Axes.Bottom.Grid.Visible = false;
            this.RainfallChart.Axes.Bottom.Increment = 30D;
            // 
            // 
            // 
            // 
            // 
            // 
            this.RainfallChart.Axes.Bottom.Labels.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RainfallChart.Axes.Bottom.Labels.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RainfallChart.Axes.Bottom.Labels.Bevel.StringColorOne = "FFFFFFFF";
            this.RainfallChart.Axes.Bottom.Labels.Bevel.StringColorTwo = "FF808080";
            this.RainfallChart.Axes.Bottom.Labels.CustomSize = 40;
            this.RainfallChart.Axes.Bottom.Labels.DateTimeFormat = "MMM yyyy";
            // 
            // 
            // 
            this.RainfallChart.Axes.Bottom.Labels.Font.Size = 11;
            this.RainfallChart.Axes.Bottom.Labels.Font.SizeFloat = 11F;
            this.RainfallChart.Axes.Bottom.MaximumOffset = 2;
            // 
            // 
            // 
            this.RainfallChart.Axes.Bottom.MinorTicks.Visible = false;
            // 
            // 
            // 
            this.RainfallChart.Axes.Bottom.Ticks.Length = 0;
            // 
            // 
            // 
            this.RainfallChart.Axes.Bottom.TicksInner.Length = 5;
            // 
            // 
            // 
            // 
            // 
            // 
            this.RainfallChart.Axes.Bottom.Title.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RainfallChart.Axes.Bottom.Title.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RainfallChart.Axes.Bottom.Title.Bevel.StringColorOne = "FFFFFFFF";
            this.RainfallChart.Axes.Bottom.Title.Bevel.StringColorTwo = "FF808080";
            this.RainfallChart.Axes.Bottom.Title.Caption = "Date";
            // 
            // 
            // 
            this.RainfallChart.Axes.Bottom.Title.Font.Size = 11;
            this.RainfallChart.Axes.Bottom.Title.Font.SizeFloat = 11F;
            this.RainfallChart.Axes.Bottom.Title.Lines = new string[] {
        "Date"};
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            this.RainfallChart.Axes.Depth.Labels.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RainfallChart.Axes.Depth.Labels.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RainfallChart.Axes.Depth.Labels.Bevel.StringColorOne = "FFFFFFFF";
            this.RainfallChart.Axes.Depth.Labels.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.RainfallChart.Axes.Depth.Title.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RainfallChart.Axes.Depth.Title.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RainfallChart.Axes.Depth.Title.Bevel.StringColorOne = "FFFFFFFF";
            this.RainfallChart.Axes.Depth.Title.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            this.RainfallChart.Axes.DepthTop.Labels.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RainfallChart.Axes.DepthTop.Labels.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RainfallChart.Axes.DepthTop.Labels.Bevel.StringColorOne = "FFFFFFFF";
            this.RainfallChart.Axes.DepthTop.Labels.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.RainfallChart.Axes.DepthTop.Title.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RainfallChart.Axes.DepthTop.Title.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RainfallChart.Axes.DepthTop.Title.Bevel.StringColorOne = "FFFFFFFF";
            this.RainfallChart.Axes.DepthTop.Title.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.RainfallChart.Axes.Left.AxisPen.Width = 1;
            // 
            // 
            // 
            this.RainfallChart.Axes.Left.Grid.Visible = false;
            // 
            // 
            // 
            // 
            // 
            // 
            this.RainfallChart.Axes.Left.Labels.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RainfallChart.Axes.Left.Labels.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RainfallChart.Axes.Left.Labels.Bevel.StringColorOne = "FFFFFFFF";
            this.RainfallChart.Axes.Left.Labels.Bevel.StringColorTwo = "FF808080";
            this.RainfallChart.Axes.Left.Labels.CustomSize = 40;
            // 
            // 
            // 
            this.RainfallChart.Axes.Left.Labels.Font.Size = 11;
            this.RainfallChart.Axes.Left.Labels.Font.SizeFloat = 11F;
            this.RainfallChart.Axes.Left.Labels.Separation = 20;
            // 
            // 
            // 
            this.RainfallChart.Axes.Left.MinorTicks.Visible = false;
            // 
            // 
            // 
            this.RainfallChart.Axes.Left.Ticks.Length = 0;
            // 
            // 
            // 
            this.RainfallChart.Axes.Left.TicksInner.Length = 5;
            // 
            // 
            // 
            // 
            // 
            // 
            this.RainfallChart.Axes.Left.Title.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RainfallChart.Axes.Left.Title.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RainfallChart.Axes.Left.Title.Bevel.StringColorOne = "FFFFFFFF";
            this.RainfallChart.Axes.Left.Title.Bevel.StringColorTwo = "FF808080";
            this.RainfallChart.Axes.Left.Title.Caption = "Rainfall (mm)";
            // 
            // 
            // 
            this.RainfallChart.Axes.Left.Title.Font.Size = 11;
            this.RainfallChart.Axes.Left.Title.Font.SizeFloat = 11F;
            this.RainfallChart.Axes.Left.Title.Lines = new string[] {
        "Rainfall (mm)"};
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            this.RainfallChart.Axes.Right.Labels.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RainfallChart.Axes.Right.Labels.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RainfallChart.Axes.Right.Labels.Bevel.StringColorOne = "FFFFFFFF";
            this.RainfallChart.Axes.Right.Labels.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.RainfallChart.Axes.Right.Title.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RainfallChart.Axes.Right.Title.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RainfallChart.Axes.Right.Title.Bevel.StringColorOne = "FFFFFFFF";
            this.RainfallChart.Axes.Right.Title.Bevel.StringColorTwo = "FF808080";
            this.RainfallChart.Axes.Right.Visible = false;
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            this.RainfallChart.Axes.Top.Labels.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RainfallChart.Axes.Top.Labels.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RainfallChart.Axes.Top.Labels.Bevel.StringColorOne = "FFFFFFFF";
            this.RainfallChart.Axes.Top.Labels.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.RainfallChart.Axes.Top.Title.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RainfallChart.Axes.Top.Title.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RainfallChart.Axes.Top.Title.Bevel.StringColorOne = "FFFFFFFF";
            this.RainfallChart.Axes.Top.Title.Bevel.StringColorTwo = "FF808080";
            this.RainfallChart.Axes.Top.Visible = false;
            this.RainfallChart.BackColor = System.Drawing.Color.Transparent;
            this.RainfallChart.Cursor = System.Windows.Forms.Cursors.Default;
            this.RainfallChart.Dock = System.Windows.Forms.DockStyle.Fill;
            // 
            // 
            // 
            // 
            // 
            // 
            this.RainfallChart.Footer.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RainfallChart.Footer.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RainfallChart.Footer.Bevel.StringColorOne = "FFFFFFFF";
            this.RainfallChart.Footer.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.RainfallChart.Header.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RainfallChart.Header.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RainfallChart.Header.Bevel.StringColorOne = "FFFFFFFF";
            this.RainfallChart.Header.Bevel.StringColorTwo = "FF808080";
            this.RainfallChart.Header.Lines = new string[] {
        ""};
            // 
            // 
            // 
            // 
            // 
            // 
            this.RainfallChart.Legend.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RainfallChart.Legend.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RainfallChart.Legend.Bevel.StringColorOne = "FFFFFFFF";
            this.RainfallChart.Legend.Bevel.StringColorTwo = "FF808080";
            this.RainfallChart.Legend.LegendStyle = Steema.TeeChart.LegendStyles.Series;
            // 
            // 
            // 
            this.RainfallChart.Legend.Pen.Visible = false;
            // 
            // 
            // 
            this.RainfallChart.Legend.Shadow.Visible = false;
            // 
            // 
            // 
            // 
            // 
            // 
            this.RainfallChart.Legend.Title.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RainfallChart.Legend.Title.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RainfallChart.Legend.Title.Bevel.StringColorOne = "FFFFFFFF";
            this.RainfallChart.Legend.Title.Bevel.StringColorTwo = "FF808080";
            this.RainfallChart.Legend.Visible = false;
            this.RainfallChart.Location = new System.Drawing.Point(3, 3);
            this.RainfallChart.Name = "RainfallChart";
            // 
            // 
            // 
            // 
            // 
            // 
            this.RainfallChart.Panel.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RainfallChart.Panel.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RainfallChart.Panel.Bevel.Outer = Steema.TeeChart.Drawing.BevelStyles.None;
            this.RainfallChart.Panel.Bevel.StringColorOne = "FFFFFFFF";
            this.RainfallChart.Panel.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            this.RainfallChart.Panel.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            // 
            // 
            // 
            this.RainfallChart.Panel.ImageBevel.Width = 1;
            this.RainfallChart.Panel.MarginBottom = 1D;
            this.RainfallChart.Panel.MarginLeft = 1D;
            this.RainfallChart.Series.Add(this.RainfallBar);
            this.RainfallChart.Size = new System.Drawing.Size(1001, 675);
            // 
            // 
            // 
            // 
            // 
            // 
            this.RainfallChart.SubFooter.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RainfallChart.SubFooter.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RainfallChart.SubFooter.Bevel.StringColorOne = "FFFFFFFF";
            this.RainfallChart.SubFooter.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.RainfallChart.SubHeader.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RainfallChart.SubHeader.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RainfallChart.SubHeader.Bevel.StringColorOne = "FFFFFFFF";
            this.RainfallChart.SubHeader.Bevel.StringColorTwo = "FF808080";
            this.RainfallChart.TabIndex = 14;
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            this.RainfallChart.Walls.Back.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RainfallChart.Walls.Back.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RainfallChart.Walls.Back.Bevel.StringColorOne = "FFFFFFFF";
            this.RainfallChart.Walls.Back.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.RainfallChart.Walls.Bottom.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RainfallChart.Walls.Bottom.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RainfallChart.Walls.Bottom.Bevel.StringColorOne = "FFFFFFFF";
            this.RainfallChart.Walls.Bottom.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.RainfallChart.Walls.Left.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RainfallChart.Walls.Left.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RainfallChart.Walls.Left.Bevel.StringColorOne = "FFFFFFFF";
            this.RainfallChart.Walls.Left.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.RainfallChart.Walls.Right.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RainfallChart.Walls.Right.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RainfallChart.Walls.Right.Bevel.StringColorOne = "FFFFFFFF";
            this.RainfallChart.Walls.Right.Bevel.StringColorTwo = "FF808080";
            this.RainfallChart.Walls.Visible = false;
            // 
            // RainfallBar
            // 
            // 
            // 
            // 
            this.RainfallBar.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(192)))), ((int)(((byte)(192)))));
            this.RainfallBar.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(192)))), ((int)(((byte)(192)))));
            this.RainfallBar.ColorEach = false;
            // 
            // 
            // 
            // 
            // 
            // 
            this.RainfallBar.Marks.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RainfallBar.Marks.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RainfallBar.Marks.Bevel.StringColorOne = "FFFFFFFF";
            this.RainfallBar.Marks.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.RainfallBar.Marks.Symbol.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RainfallBar.Marks.Symbol.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RainfallBar.Marks.Symbol.Bevel.StringColorOne = "FFFFFFFF";
            this.RainfallBar.Marks.Symbol.Bevel.StringColorTwo = "FF808080";
            this.RainfallBar.Marks.Visible = false;
            // 
            // 
            // 
            this.RainfallBar.Pen.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(115)))), ((int)(((byte)(115)))));
            this.RainfallBar.Title = "Rainfall";
            // 
            // 
            // 
            this.RainfallBar.XValues.DataMember = "X";
            this.RainfallBar.XValues.DateTime = true;
            this.RainfallBar.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.Ascending;
            // 
            // 
            // 
            this.RainfallBar.YValues.DataMember = "Bar";
            // 
            // TabPage3
            // 
            this.TabPage3.Controls.Add(this.MonthlyRainfallChart);
            this.TabPage3.ImageIndex = 1;
            this.TabPage3.Location = new System.Drawing.Point(4, 23);
            this.TabPage3.Name = "TabPage3";
            this.TabPage3.Padding = new System.Windows.Forms.Padding(3);
            this.TabPage3.Size = new System.Drawing.Size(1007, 681);
            this.TabPage3.TabIndex = 2;
            this.TabPage3.Text = "Monthly rainfall chart";
            this.TabPage3.UseVisualStyleBackColor = true;
            // 
            // MonthlyRainfallChart
            // 
            // 
            // 
            // 
            this.MonthlyRainfallChart.Aspect.View3D = false;
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            this.MonthlyRainfallChart.Axes.Bottom.AxisPen.Width = 1;
            // 
            // 
            // 
            this.MonthlyRainfallChart.Axes.Bottom.Grid.Visible = false;
            this.MonthlyRainfallChart.Axes.Bottom.Increment = 30D;
            // 
            // 
            // 
            // 
            // 
            // 
            this.MonthlyRainfallChart.Axes.Bottom.Labels.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.MonthlyRainfallChart.Axes.Bottom.Labels.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.MonthlyRainfallChart.Axes.Bottom.Labels.Bevel.StringColorOne = "FFFFFFFF";
            this.MonthlyRainfallChart.Axes.Bottom.Labels.Bevel.StringColorTwo = "FF808080";
            this.MonthlyRainfallChart.Axes.Bottom.Labels.CustomSize = 40;
            this.MonthlyRainfallChart.Axes.Bottom.Labels.DateTimeFormat = "MMM yyyy";
            // 
            // 
            // 
            this.MonthlyRainfallChart.Axes.Bottom.Labels.Font.Size = 11;
            this.MonthlyRainfallChart.Axes.Bottom.Labels.Font.SizeFloat = 11F;
            this.MonthlyRainfallChart.Axes.Bottom.MaximumOffset = 2;
            // 
            // 
            // 
            this.MonthlyRainfallChart.Axes.Bottom.MinorTicks.Visible = false;
            // 
            // 
            // 
            this.MonthlyRainfallChart.Axes.Bottom.Ticks.Length = 0;
            // 
            // 
            // 
            this.MonthlyRainfallChart.Axes.Bottom.TicksInner.Length = 5;
            // 
            // 
            // 
            // 
            // 
            // 
            this.MonthlyRainfallChart.Axes.Bottom.Title.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.MonthlyRainfallChart.Axes.Bottom.Title.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.MonthlyRainfallChart.Axes.Bottom.Title.Bevel.StringColorOne = "FFFFFFFF";
            this.MonthlyRainfallChart.Axes.Bottom.Title.Bevel.StringColorTwo = "FF808080";
            this.MonthlyRainfallChart.Axes.Bottom.Title.Caption = "Date";
            // 
            // 
            // 
            this.MonthlyRainfallChart.Axes.Bottom.Title.Font.Size = 11;
            this.MonthlyRainfallChart.Axes.Bottom.Title.Font.SizeFloat = 11F;
            this.MonthlyRainfallChart.Axes.Bottom.Title.Lines = new string[] {
        "Date"};
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            this.MonthlyRainfallChart.Axes.Depth.Labels.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.MonthlyRainfallChart.Axes.Depth.Labels.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.MonthlyRainfallChart.Axes.Depth.Labels.Bevel.StringColorOne = "FFFFFFFF";
            this.MonthlyRainfallChart.Axes.Depth.Labels.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.MonthlyRainfallChart.Axes.Depth.Title.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.MonthlyRainfallChart.Axes.Depth.Title.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.MonthlyRainfallChart.Axes.Depth.Title.Bevel.StringColorOne = "FFFFFFFF";
            this.MonthlyRainfallChart.Axes.Depth.Title.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            this.MonthlyRainfallChart.Axes.DepthTop.Labels.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.MonthlyRainfallChart.Axes.DepthTop.Labels.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.MonthlyRainfallChart.Axes.DepthTop.Labels.Bevel.StringColorOne = "FFFFFFFF";
            this.MonthlyRainfallChart.Axes.DepthTop.Labels.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.MonthlyRainfallChart.Axes.DepthTop.Title.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.MonthlyRainfallChart.Axes.DepthTop.Title.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.MonthlyRainfallChart.Axes.DepthTop.Title.Bevel.StringColorOne = "FFFFFFFF";
            this.MonthlyRainfallChart.Axes.DepthTop.Title.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.MonthlyRainfallChart.Axes.Left.AxisPen.Width = 1;
            // 
            // 
            // 
            this.MonthlyRainfallChart.Axes.Left.Grid.Visible = false;
            // 
            // 
            // 
            // 
            // 
            // 
            this.MonthlyRainfallChart.Axes.Left.Labels.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.MonthlyRainfallChart.Axes.Left.Labels.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.MonthlyRainfallChart.Axes.Left.Labels.Bevel.StringColorOne = "FFFFFFFF";
            this.MonthlyRainfallChart.Axes.Left.Labels.Bevel.StringColorTwo = "FF808080";
            this.MonthlyRainfallChart.Axes.Left.Labels.CustomSize = 40;
            // 
            // 
            // 
            this.MonthlyRainfallChart.Axes.Left.Labels.Font.Size = 11;
            this.MonthlyRainfallChart.Axes.Left.Labels.Font.SizeFloat = 11F;
            this.MonthlyRainfallChart.Axes.Left.Labels.Separation = 20;
            // 
            // 
            // 
            this.MonthlyRainfallChart.Axes.Left.MinorTicks.Visible = false;
            // 
            // 
            // 
            this.MonthlyRainfallChart.Axes.Left.Ticks.Length = 0;
            // 
            // 
            // 
            this.MonthlyRainfallChart.Axes.Left.TicksInner.Length = 5;
            // 
            // 
            // 
            // 
            // 
            // 
            this.MonthlyRainfallChart.Axes.Left.Title.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.MonthlyRainfallChart.Axes.Left.Title.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.MonthlyRainfallChart.Axes.Left.Title.Bevel.StringColorOne = "FFFFFFFF";
            this.MonthlyRainfallChart.Axes.Left.Title.Bevel.StringColorTwo = "FF808080";
            this.MonthlyRainfallChart.Axes.Left.Title.Caption = "Rainfall and Evaporation (mm)";
            // 
            // 
            // 
            this.MonthlyRainfallChart.Axes.Left.Title.Font.Size = 11;
            this.MonthlyRainfallChart.Axes.Left.Title.Font.SizeFloat = 11F;
            this.MonthlyRainfallChart.Axes.Left.Title.Lines = new string[] {
        "Rainfall and Evaporation (mm)"};
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            this.MonthlyRainfallChart.Axes.Right.Labels.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.MonthlyRainfallChart.Axes.Right.Labels.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.MonthlyRainfallChart.Axes.Right.Labels.Bevel.StringColorOne = "FFFFFFFF";
            this.MonthlyRainfallChart.Axes.Right.Labels.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.MonthlyRainfallChart.Axes.Right.Title.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.MonthlyRainfallChart.Axes.Right.Title.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.MonthlyRainfallChart.Axes.Right.Title.Bevel.StringColorOne = "FFFFFFFF";
            this.MonthlyRainfallChart.Axes.Right.Title.Bevel.StringColorTwo = "FF808080";
            this.MonthlyRainfallChart.Axes.Right.Visible = false;
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            this.MonthlyRainfallChart.Axes.Top.Labels.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.MonthlyRainfallChart.Axes.Top.Labels.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.MonthlyRainfallChart.Axes.Top.Labels.Bevel.StringColorOne = "FFFFFFFF";
            this.MonthlyRainfallChart.Axes.Top.Labels.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.MonthlyRainfallChart.Axes.Top.Title.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.MonthlyRainfallChart.Axes.Top.Title.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.MonthlyRainfallChart.Axes.Top.Title.Bevel.StringColorOne = "FFFFFFFF";
            this.MonthlyRainfallChart.Axes.Top.Title.Bevel.StringColorTwo = "FF808080";
            this.MonthlyRainfallChart.Axes.Top.Visible = false;
            this.MonthlyRainfallChart.BackColor = System.Drawing.Color.Transparent;
            this.MonthlyRainfallChart.Cursor = System.Windows.Forms.Cursors.Default;
            this.MonthlyRainfallChart.Dock = System.Windows.Forms.DockStyle.Fill;
            // 
            // 
            // 
            // 
            // 
            // 
            this.MonthlyRainfallChart.Footer.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.MonthlyRainfallChart.Footer.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.MonthlyRainfallChart.Footer.Bevel.StringColorOne = "FFFFFFFF";
            this.MonthlyRainfallChart.Footer.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.MonthlyRainfallChart.Header.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.MonthlyRainfallChart.Header.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.MonthlyRainfallChart.Header.Bevel.StringColorOne = "FFFFFFFF";
            this.MonthlyRainfallChart.Header.Bevel.StringColorTwo = "FF808080";
            this.MonthlyRainfallChart.Header.Lines = new string[] {
        ""};
            // 
            // 
            // 
            this.MonthlyRainfallChart.Legend.Alignment = Steema.TeeChart.LegendAlignments.Bottom;
            // 
            // 
            // 
            this.MonthlyRainfallChart.Legend.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.MonthlyRainfallChart.Legend.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.MonthlyRainfallChart.Legend.Bevel.StringColorOne = "FFFFFFFF";
            this.MonthlyRainfallChart.Legend.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            this.MonthlyRainfallChart.Legend.Font.Size = 11;
            this.MonthlyRainfallChart.Legend.Font.SizeFloat = 11F;
            this.MonthlyRainfallChart.Legend.LegendStyle = Steema.TeeChart.LegendStyles.Series;
            // 
            // 
            // 
            this.MonthlyRainfallChart.Legend.Pen.Visible = false;
            // 
            // 
            // 
            this.MonthlyRainfallChart.Legend.Shadow.Visible = false;
            // 
            // 
            // 
            // 
            // 
            // 
            this.MonthlyRainfallChart.Legend.Title.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.MonthlyRainfallChart.Legend.Title.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.MonthlyRainfallChart.Legend.Title.Bevel.StringColorOne = "FFFFFFFF";
            this.MonthlyRainfallChart.Legend.Title.Bevel.StringColorTwo = "FF808080";
            this.MonthlyRainfallChart.Location = new System.Drawing.Point(3, 3);
            this.MonthlyRainfallChart.Name = "MonthlyRainfallChart";
            // 
            // 
            // 
            // 
            // 
            // 
            this.MonthlyRainfallChart.Panel.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.MonthlyRainfallChart.Panel.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.MonthlyRainfallChart.Panel.Bevel.Outer = Steema.TeeChart.Drawing.BevelStyles.None;
            this.MonthlyRainfallChart.Panel.Bevel.StringColorOne = "FFFFFFFF";
            this.MonthlyRainfallChart.Panel.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            this.MonthlyRainfallChart.Panel.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            // 
            // 
            // 
            this.MonthlyRainfallChart.Panel.ImageBevel.Width = 1;
            this.MonthlyRainfallChart.Panel.MarginBottom = 1D;
            this.MonthlyRainfallChart.Panel.MarginLeft = 1D;
            this.MonthlyRainfallChart.Series.Add(this.MonthlyRainfallBar);
            this.MonthlyRainfallChart.Series.Add(this.MonthlyEvaporationLine);
            this.MonthlyRainfallChart.Size = new System.Drawing.Size(1001, 675);
            // 
            // 
            // 
            // 
            // 
            // 
            this.MonthlyRainfallChart.SubFooter.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.MonthlyRainfallChart.SubFooter.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.MonthlyRainfallChart.SubFooter.Bevel.StringColorOne = "FFFFFFFF";
            this.MonthlyRainfallChart.SubFooter.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.MonthlyRainfallChart.SubHeader.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.MonthlyRainfallChart.SubHeader.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.MonthlyRainfallChart.SubHeader.Bevel.StringColorOne = "FFFFFFFF";
            this.MonthlyRainfallChart.SubHeader.Bevel.StringColorTwo = "FF808080";
            this.MonthlyRainfallChart.TabIndex = 15;
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            this.MonthlyRainfallChart.Walls.Back.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.MonthlyRainfallChart.Walls.Back.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.MonthlyRainfallChart.Walls.Back.Bevel.StringColorOne = "FFFFFFFF";
            this.MonthlyRainfallChart.Walls.Back.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.MonthlyRainfallChart.Walls.Bottom.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.MonthlyRainfallChart.Walls.Bottom.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.MonthlyRainfallChart.Walls.Bottom.Bevel.StringColorOne = "FFFFFFFF";
            this.MonthlyRainfallChart.Walls.Bottom.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.MonthlyRainfallChart.Walls.Left.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.MonthlyRainfallChart.Walls.Left.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.MonthlyRainfallChart.Walls.Left.Bevel.StringColorOne = "FFFFFFFF";
            this.MonthlyRainfallChart.Walls.Left.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.MonthlyRainfallChart.Walls.Right.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.MonthlyRainfallChart.Walls.Right.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.MonthlyRainfallChart.Walls.Right.Bevel.StringColorOne = "FFFFFFFF";
            this.MonthlyRainfallChart.Walls.Right.Bevel.StringColorTwo = "FF808080";
            this.MonthlyRainfallChart.Walls.Visible = false;
            // 
            // MonthlyRainfallBar
            // 
            // 
            // 
            // 
            this.MonthlyRainfallBar.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(192)))), ((int)(((byte)(192)))));
            this.MonthlyRainfallBar.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(192)))), ((int)(((byte)(192)))));
            this.MonthlyRainfallBar.ColorEach = false;
            // 
            // 
            // 
            // 
            // 
            // 
            this.MonthlyRainfallBar.Marks.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.MonthlyRainfallBar.Marks.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.MonthlyRainfallBar.Marks.Bevel.StringColorOne = "FFFFFFFF";
            this.MonthlyRainfallBar.Marks.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.MonthlyRainfallBar.Marks.Symbol.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.MonthlyRainfallBar.Marks.Symbol.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.MonthlyRainfallBar.Marks.Symbol.Bevel.StringColorOne = "FFFFFFFF";
            this.MonthlyRainfallBar.Marks.Symbol.Bevel.StringColorTwo = "FF808080";
            this.MonthlyRainfallBar.Marks.Visible = false;
            // 
            // 
            // 
            this.MonthlyRainfallBar.Pen.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(115)))), ((int)(((byte)(115)))));
            this.MonthlyRainfallBar.Title = "Rainfall";
            // 
            // 
            // 
            this.MonthlyRainfallBar.XValues.DataMember = "X";
            this.MonthlyRainfallBar.XValues.DateTime = true;
            this.MonthlyRainfallBar.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.Ascending;
            // 
            // 
            // 
            this.MonthlyRainfallBar.YValues.DataMember = "Bar";
            // 
            // MonthlyEvaporationLine
            // 
            // 
            // 
            // 
            this.MonthlyEvaporationLine.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(68)))), ((int)(((byte)(102)))), ((int)(((byte)(163)))));
            this.MonthlyEvaporationLine.Color = System.Drawing.Color.FromArgb(((int)(((byte)(68)))), ((int)(((byte)(102)))), ((int)(((byte)(163)))));
            this.MonthlyEvaporationLine.ColorEach = false;
            // 
            // 
            // 
            this.MonthlyEvaporationLine.LinePen.Color = System.Drawing.Color.FromArgb(((int)(((byte)(41)))), ((int)(((byte)(61)))), ((int)(((byte)(98)))));
            // 
            // 
            // 
            // 
            // 
            // 
            this.MonthlyEvaporationLine.Pointer.Brush.Color = System.Drawing.Color.Red;
            this.MonthlyEvaporationLine.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle;
            this.MonthlyEvaporationLine.Title = "Evaporation";
            // 
            // 
            // 
            this.MonthlyEvaporationLine.XValues.DataMember = "X";
            this.MonthlyEvaporationLine.XValues.DateTime = true;
            this.MonthlyEvaporationLine.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.Ascending;
            // 
            // 
            // 
            this.MonthlyEvaporationLine.YValues.DataMember = "Y";
            // 
            // TabPage5
            // 
            this.TabPage5.Controls.Add(this.TemperatureChart);
            this.TabPage5.ImageIndex = 2;
            this.TabPage5.Location = new System.Drawing.Point(4, 23);
            this.TabPage5.Name = "TabPage5";
            this.TabPage5.Size = new System.Drawing.Size(1007, 681);
            this.TabPage5.TabIndex = 4;
            this.TabPage5.Text = "Temperature chart";
            this.TabPage5.UseVisualStyleBackColor = true;
            // 
            // TemperatureChart
            // 
            // 
            // 
            // 
            this.TemperatureChart.Aspect.View3D = false;
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            this.TemperatureChart.Axes.Bottom.AxisPen.Width = 1;
            // 
            // 
            // 
            this.TemperatureChart.Axes.Bottom.Grid.Visible = false;
            this.TemperatureChart.Axes.Bottom.Increment = 30D;
            // 
            // 
            // 
            // 
            // 
            // 
            this.TemperatureChart.Axes.Bottom.Labels.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.TemperatureChart.Axes.Bottom.Labels.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.TemperatureChart.Axes.Bottom.Labels.Bevel.StringColorOne = "FFFFFFFF";
            this.TemperatureChart.Axes.Bottom.Labels.Bevel.StringColorTwo = "FF808080";
            this.TemperatureChart.Axes.Bottom.Labels.DateTimeFormat = "MMM yyyy";
            // 
            // 
            // 
            this.TemperatureChart.Axes.Bottom.Labels.Font.Size = 11;
            this.TemperatureChart.Axes.Bottom.Labels.Font.SizeFloat = 11F;
            // 
            // 
            // 
            this.TemperatureChart.Axes.Bottom.MinorTicks.Visible = false;
            // 
            // 
            // 
            this.TemperatureChart.Axes.Bottom.Ticks.Length = 0;
            // 
            // 
            // 
            this.TemperatureChart.Axes.Bottom.TicksInner.Length = 5;
            // 
            // 
            // 
            // 
            // 
            // 
            this.TemperatureChart.Axes.Bottom.Title.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.TemperatureChart.Axes.Bottom.Title.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.TemperatureChart.Axes.Bottom.Title.Bevel.StringColorOne = "FFFFFFFF";
            this.TemperatureChart.Axes.Bottom.Title.Bevel.StringColorTwo = "FF808080";
            this.TemperatureChart.Axes.Bottom.Title.Caption = "Date";
            // 
            // 
            // 
            this.TemperatureChart.Axes.Bottom.Title.Font.Size = 11;
            this.TemperatureChart.Axes.Bottom.Title.Font.SizeFloat = 11F;
            this.TemperatureChart.Axes.Bottom.Title.Lines = new string[] {
        "Date"};
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            this.TemperatureChart.Axes.Depth.Labels.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.TemperatureChart.Axes.Depth.Labels.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.TemperatureChart.Axes.Depth.Labels.Bevel.StringColorOne = "FFFFFFFF";
            this.TemperatureChart.Axes.Depth.Labels.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.TemperatureChart.Axes.Depth.Title.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.TemperatureChart.Axes.Depth.Title.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.TemperatureChart.Axes.Depth.Title.Bevel.StringColorOne = "FFFFFFFF";
            this.TemperatureChart.Axes.Depth.Title.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            this.TemperatureChart.Axes.DepthTop.Labels.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.TemperatureChart.Axes.DepthTop.Labels.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.TemperatureChart.Axes.DepthTop.Labels.Bevel.StringColorOne = "FFFFFFFF";
            this.TemperatureChart.Axes.DepthTop.Labels.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.TemperatureChart.Axes.DepthTop.Title.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.TemperatureChart.Axes.DepthTop.Title.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.TemperatureChart.Axes.DepthTop.Title.Bevel.StringColorOne = "FFFFFFFF";
            this.TemperatureChart.Axes.DepthTop.Title.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.TemperatureChart.Axes.Left.AxisPen.Width = 1;
            // 
            // 
            // 
            this.TemperatureChart.Axes.Left.Grid.Visible = false;
            // 
            // 
            // 
            // 
            // 
            // 
            this.TemperatureChart.Axes.Left.Labels.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.TemperatureChart.Axes.Left.Labels.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.TemperatureChart.Axes.Left.Labels.Bevel.StringColorOne = "FFFFFFFF";
            this.TemperatureChart.Axes.Left.Labels.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            this.TemperatureChart.Axes.Left.Labels.Font.Size = 11;
            this.TemperatureChart.Axes.Left.Labels.Font.SizeFloat = 11F;
            this.TemperatureChart.Axes.Left.Labels.Separation = 20;
            // 
            // 
            // 
            this.TemperatureChart.Axes.Left.MinorTicks.Visible = false;
            // 
            // 
            // 
            this.TemperatureChart.Axes.Left.Ticks.Length = 0;
            // 
            // 
            // 
            this.TemperatureChart.Axes.Left.TicksInner.Length = 5;
            // 
            // 
            // 
            // 
            // 
            // 
            this.TemperatureChart.Axes.Left.Title.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.TemperatureChart.Axes.Left.Title.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.TemperatureChart.Axes.Left.Title.Bevel.StringColorOne = "FFFFFFFF";
            this.TemperatureChart.Axes.Left.Title.Bevel.StringColorTwo = "FF808080";
            this.TemperatureChart.Axes.Left.Title.Caption = "Temperature (oC)";
            // 
            // 
            // 
            this.TemperatureChart.Axes.Left.Title.Font.Size = 11;
            this.TemperatureChart.Axes.Left.Title.Font.SizeFloat = 11F;
            this.TemperatureChart.Axes.Left.Title.Lines = new string[] {
        "Temperature (oC)"};
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            this.TemperatureChart.Axes.Right.Labels.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.TemperatureChart.Axes.Right.Labels.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.TemperatureChart.Axes.Right.Labels.Bevel.StringColorOne = "FFFFFFFF";
            this.TemperatureChart.Axes.Right.Labels.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.TemperatureChart.Axes.Right.Title.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.TemperatureChart.Axes.Right.Title.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.TemperatureChart.Axes.Right.Title.Bevel.StringColorOne = "FFFFFFFF";
            this.TemperatureChart.Axes.Right.Title.Bevel.StringColorTwo = "FF808080";
            this.TemperatureChart.Axes.Right.Visible = false;
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            this.TemperatureChart.Axes.Top.Labels.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.TemperatureChart.Axes.Top.Labels.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.TemperatureChart.Axes.Top.Labels.Bevel.StringColorOne = "FFFFFFFF";
            this.TemperatureChart.Axes.Top.Labels.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.TemperatureChart.Axes.Top.Title.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.TemperatureChart.Axes.Top.Title.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.TemperatureChart.Axes.Top.Title.Bevel.StringColorOne = "FFFFFFFF";
            this.TemperatureChart.Axes.Top.Title.Bevel.StringColorTwo = "FF808080";
            this.TemperatureChart.Axes.Top.Visible = false;
            this.TemperatureChart.BackColor = System.Drawing.Color.Transparent;
            this.TemperatureChart.Cursor = System.Windows.Forms.Cursors.Default;
            this.TemperatureChart.Dock = System.Windows.Forms.DockStyle.Fill;
            // 
            // 
            // 
            // 
            // 
            // 
            this.TemperatureChart.Footer.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.TemperatureChart.Footer.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.TemperatureChart.Footer.Bevel.StringColorOne = "FFFFFFFF";
            this.TemperatureChart.Footer.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.TemperatureChart.Header.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.TemperatureChart.Header.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.TemperatureChart.Header.Bevel.StringColorOne = "FFFFFFFF";
            this.TemperatureChart.Header.Bevel.StringColorTwo = "FF808080";
            this.TemperatureChart.Header.Lines = new string[] {
        ""};
            // 
            // 
            // 
            this.TemperatureChart.Legend.Alignment = Steema.TeeChart.LegendAlignments.Bottom;
            // 
            // 
            // 
            this.TemperatureChart.Legend.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.TemperatureChart.Legend.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.TemperatureChart.Legend.Bevel.StringColorOne = "FFFFFFFF";
            this.TemperatureChart.Legend.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            this.TemperatureChart.Legend.Font.Size = 11;
            this.TemperatureChart.Legend.Font.SizeFloat = 11F;
            this.TemperatureChart.Legend.LegendStyle = Steema.TeeChart.LegendStyles.Series;
            // 
            // 
            // 
            this.TemperatureChart.Legend.Pen.Visible = false;
            // 
            // 
            // 
            this.TemperatureChart.Legend.Shadow.Visible = false;
            // 
            // 
            // 
            // 
            // 
            // 
            this.TemperatureChart.Legend.Title.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.TemperatureChart.Legend.Title.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.TemperatureChart.Legend.Title.Bevel.StringColorOne = "FFFFFFFF";
            this.TemperatureChart.Legend.Title.Bevel.StringColorTwo = "FF808080";
            this.TemperatureChart.Location = new System.Drawing.Point(0, 0);
            this.TemperatureChart.Name = "TemperatureChart";
            // 
            // 
            // 
            // 
            // 
            // 
            this.TemperatureChart.Panel.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.TemperatureChart.Panel.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.TemperatureChart.Panel.Bevel.Outer = Steema.TeeChart.Drawing.BevelStyles.None;
            this.TemperatureChart.Panel.Bevel.StringColorOne = "FFFFFFFF";
            this.TemperatureChart.Panel.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            this.TemperatureChart.Panel.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            // 
            // 
            // 
            this.TemperatureChart.Panel.ImageBevel.Width = 1;
            this.TemperatureChart.Series.Add(this.MaximumTemperatureLine);
            this.TemperatureChart.Series.Add(this.MinimumTemperatureLine);
            this.TemperatureChart.Size = new System.Drawing.Size(1007, 681);
            // 
            // 
            // 
            // 
            // 
            // 
            this.TemperatureChart.SubFooter.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.TemperatureChart.SubFooter.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.TemperatureChart.SubFooter.Bevel.StringColorOne = "FFFFFFFF";
            this.TemperatureChart.SubFooter.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.TemperatureChart.SubHeader.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.TemperatureChart.SubHeader.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.TemperatureChart.SubHeader.Bevel.StringColorOne = "FFFFFFFF";
            this.TemperatureChart.SubHeader.Bevel.StringColorTwo = "FF808080";
            this.TemperatureChart.TabIndex = 16;
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            this.TemperatureChart.Walls.Back.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.TemperatureChart.Walls.Back.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.TemperatureChart.Walls.Back.Bevel.StringColorOne = "FFFFFFFF";
            this.TemperatureChart.Walls.Back.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.TemperatureChart.Walls.Bottom.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.TemperatureChart.Walls.Bottom.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.TemperatureChart.Walls.Bottom.Bevel.StringColorOne = "FFFFFFFF";
            this.TemperatureChart.Walls.Bottom.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.TemperatureChart.Walls.Left.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.TemperatureChart.Walls.Left.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.TemperatureChart.Walls.Left.Bevel.StringColorOne = "FFFFFFFF";
            this.TemperatureChart.Walls.Left.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.TemperatureChart.Walls.Right.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.TemperatureChart.Walls.Right.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.TemperatureChart.Walls.Right.Bevel.StringColorOne = "FFFFFFFF";
            this.TemperatureChart.Walls.Right.Bevel.StringColorTwo = "FF808080";
            this.TemperatureChart.Walls.Visible = false;
            // 
            // MaximumTemperatureLine
            // 
            // 
            // 
            // 
            this.MaximumTemperatureLine.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(68)))), ((int)(((byte)(102)))), ((int)(((byte)(163)))));
            this.MaximumTemperatureLine.Color = System.Drawing.Color.FromArgb(((int)(((byte)(68)))), ((int)(((byte)(102)))), ((int)(((byte)(163)))));
            this.MaximumTemperatureLine.ColorEach = false;
            // 
            // 
            // 
            this.MaximumTemperatureLine.LinePen.Color = System.Drawing.Color.FromArgb(((int)(((byte)(41)))), ((int)(((byte)(61)))), ((int)(((byte)(98)))));
            // 
            // 
            // 
            // 
            // 
            // 
            this.MaximumTemperatureLine.Pointer.Brush.Color = System.Drawing.Color.Red;
            this.MaximumTemperatureLine.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle;
            this.MaximumTemperatureLine.Title = "Maximum temperature";
            // 
            // 
            // 
            this.MaximumTemperatureLine.XValues.DataMember = "X";
            this.MaximumTemperatureLine.XValues.DateTime = true;
            this.MaximumTemperatureLine.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.Ascending;
            // 
            // 
            // 
            this.MaximumTemperatureLine.YValues.DataMember = "Y";
            // 
            // MinimumTemperatureLine
            // 
            // 
            // 
            // 
            this.MinimumTemperatureLine.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(243)))), ((int)(((byte)(156)))), ((int)(((byte)(53)))));
            this.MinimumTemperatureLine.Color = System.Drawing.Color.FromArgb(((int)(((byte)(243)))), ((int)(((byte)(156)))), ((int)(((byte)(53)))));
            this.MinimumTemperatureLine.ColorEach = false;
            // 
            // 
            // 
            this.MinimumTemperatureLine.LinePen.Color = System.Drawing.Color.FromArgb(((int)(((byte)(146)))), ((int)(((byte)(94)))), ((int)(((byte)(32)))));
            // 
            // 
            // 
            // 
            // 
            // 
            this.MinimumTemperatureLine.Pointer.Brush.Color = System.Drawing.Color.Green;
            this.MinimumTemperatureLine.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle;
            this.MinimumTemperatureLine.Title = "Minimum temperature";
            // 
            // 
            // 
            this.MinimumTemperatureLine.XValues.DataMember = "X";
            this.MinimumTemperatureLine.XValues.DateTime = true;
            this.MinimumTemperatureLine.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.Ascending;
            // 
            // 
            // 
            this.MinimumTemperatureLine.YValues.DataMember = "Y";
            // 
            // TabPage4
            // 
            this.TabPage4.Controls.Add(this.RadiationChart);
            this.TabPage4.ImageIndex = 3;
            this.TabPage4.Location = new System.Drawing.Point(4, 23);
            this.TabPage4.Name = "TabPage4";
            this.TabPage4.Size = new System.Drawing.Size(1007, 681);
            this.TabPage4.TabIndex = 3;
            this.TabPage4.Text = "Radiation chart";
            this.TabPage4.UseVisualStyleBackColor = true;
            // 
            // RadiationChart
            // 
            // 
            // 
            // 
            this.RadiationChart.Aspect.View3D = false;
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            this.RadiationChart.Axes.Bottom.AxisPen.Width = 1;
            // 
            // 
            // 
            this.RadiationChart.Axes.Bottom.Grid.Visible = false;
            this.RadiationChart.Axes.Bottom.Increment = 30D;
            // 
            // 
            // 
            // 
            // 
            // 
            this.RadiationChart.Axes.Bottom.Labels.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RadiationChart.Axes.Bottom.Labels.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RadiationChart.Axes.Bottom.Labels.Bevel.StringColorOne = "FFFFFFFF";
            this.RadiationChart.Axes.Bottom.Labels.Bevel.StringColorTwo = "FF808080";
            this.RadiationChart.Axes.Bottom.Labels.DateTimeFormat = "MMM yyyy";
            // 
            // 
            // 
            this.RadiationChart.Axes.Bottom.Labels.Font.Size = 11;
            this.RadiationChart.Axes.Bottom.Labels.Font.SizeFloat = 11F;
            // 
            // 
            // 
            this.RadiationChart.Axes.Bottom.MinorTicks.Visible = false;
            // 
            // 
            // 
            this.RadiationChart.Axes.Bottom.Ticks.Length = 0;
            // 
            // 
            // 
            this.RadiationChart.Axes.Bottom.TicksInner.Length = 5;
            // 
            // 
            // 
            // 
            // 
            // 
            this.RadiationChart.Axes.Bottom.Title.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RadiationChart.Axes.Bottom.Title.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RadiationChart.Axes.Bottom.Title.Bevel.StringColorOne = "FFFFFFFF";
            this.RadiationChart.Axes.Bottom.Title.Bevel.StringColorTwo = "FF808080";
            this.RadiationChart.Axes.Bottom.Title.Caption = "Date";
            // 
            // 
            // 
            this.RadiationChart.Axes.Bottom.Title.Font.Size = 11;
            this.RadiationChart.Axes.Bottom.Title.Font.SizeFloat = 11F;
            this.RadiationChart.Axes.Bottom.Title.Lines = new string[] {
        "Date"};
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            this.RadiationChart.Axes.Depth.Labels.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RadiationChart.Axes.Depth.Labels.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RadiationChart.Axes.Depth.Labels.Bevel.StringColorOne = "FFFFFFFF";
            this.RadiationChart.Axes.Depth.Labels.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.RadiationChart.Axes.Depth.Title.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RadiationChart.Axes.Depth.Title.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RadiationChart.Axes.Depth.Title.Bevel.StringColorOne = "FFFFFFFF";
            this.RadiationChart.Axes.Depth.Title.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            this.RadiationChart.Axes.DepthTop.Labels.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RadiationChart.Axes.DepthTop.Labels.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RadiationChart.Axes.DepthTop.Labels.Bevel.StringColorOne = "FFFFFFFF";
            this.RadiationChart.Axes.DepthTop.Labels.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.RadiationChart.Axes.DepthTop.Title.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RadiationChart.Axes.DepthTop.Title.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RadiationChart.Axes.DepthTop.Title.Bevel.StringColorOne = "FFFFFFFF";
            this.RadiationChart.Axes.DepthTop.Title.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.RadiationChart.Axes.Left.AxisPen.Width = 1;
            // 
            // 
            // 
            this.RadiationChart.Axes.Left.Grid.Visible = false;
            // 
            // 
            // 
            // 
            // 
            // 
            this.RadiationChart.Axes.Left.Labels.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RadiationChart.Axes.Left.Labels.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RadiationChart.Axes.Left.Labels.Bevel.StringColorOne = "FFFFFFFF";
            this.RadiationChart.Axes.Left.Labels.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            this.RadiationChart.Axes.Left.Labels.Font.Size = 11;
            this.RadiationChart.Axes.Left.Labels.Font.SizeFloat = 11F;
            this.RadiationChart.Axes.Left.Labels.Separation = 20;
            // 
            // 
            // 
            this.RadiationChart.Axes.Left.MinorTicks.Visible = false;
            // 
            // 
            // 
            this.RadiationChart.Axes.Left.Ticks.Length = 0;
            // 
            // 
            // 
            this.RadiationChart.Axes.Left.TicksInner.Length = 5;
            // 
            // 
            // 
            // 
            // 
            // 
            this.RadiationChart.Axes.Left.Title.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RadiationChart.Axes.Left.Title.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RadiationChart.Axes.Left.Title.Bevel.StringColorOne = "FFFFFFFF";
            this.RadiationChart.Axes.Left.Title.Bevel.StringColorTwo = "FF808080";
            this.RadiationChart.Axes.Left.Title.Caption = "Rainfall (mm)";
            // 
            // 
            // 
            this.RadiationChart.Axes.Left.Title.Font.Size = 11;
            this.RadiationChart.Axes.Left.Title.Font.SizeFloat = 11F;
            this.RadiationChart.Axes.Left.Title.Lines = new string[] {
        "Rainfall (mm)"};
            // 
            // 
            // 
            // 
            // 
            // 
            this.RadiationChart.Axes.Right.Grid.Visible = false;
            // 
            // 
            // 
            // 
            // 
            // 
            this.RadiationChart.Axes.Right.Labels.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RadiationChart.Axes.Right.Labels.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RadiationChart.Axes.Right.Labels.Bevel.StringColorOne = "FFFFFFFF";
            this.RadiationChart.Axes.Right.Labels.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            this.RadiationChart.Axes.Right.Labels.Font.Size = 11;
            this.RadiationChart.Axes.Right.Labels.Font.SizeFloat = 11F;
            // 
            // 
            // 
            this.RadiationChart.Axes.Right.MinorTicks.Visible = false;
            // 
            // 
            // 
            this.RadiationChart.Axes.Right.Ticks.Length = 0;
            // 
            // 
            // 
            this.RadiationChart.Axes.Right.TicksInner.Length = 5;
            // 
            // 
            // 
            // 
            // 
            // 
            this.RadiationChart.Axes.Right.Title.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RadiationChart.Axes.Right.Title.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RadiationChart.Axes.Right.Title.Bevel.StringColorOne = "FFFFFFFF";
            this.RadiationChart.Axes.Right.Title.Bevel.StringColorTwo = "FF808080";
            this.RadiationChart.Axes.Right.Title.Caption = "Radiation (mJ/m2)";
            // 
            // 
            // 
            this.RadiationChart.Axes.Right.Title.Font.Size = 11;
            this.RadiationChart.Axes.Right.Title.Font.SizeFloat = 11F;
            this.RadiationChart.Axes.Right.Title.Lines = new string[] {
        "Radiation (mJ/m2)"};
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            this.RadiationChart.Axes.Top.Labels.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RadiationChart.Axes.Top.Labels.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RadiationChart.Axes.Top.Labels.Bevel.StringColorOne = "FFFFFFFF";
            this.RadiationChart.Axes.Top.Labels.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.RadiationChart.Axes.Top.Title.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RadiationChart.Axes.Top.Title.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RadiationChart.Axes.Top.Title.Bevel.StringColorOne = "FFFFFFFF";
            this.RadiationChart.Axes.Top.Title.Bevel.StringColorTwo = "FF808080";
            this.RadiationChart.Axes.Top.Visible = false;
            this.RadiationChart.BackColor = System.Drawing.Color.Transparent;
            this.RadiationChart.Cursor = System.Windows.Forms.Cursors.Default;
            this.RadiationChart.Dock = System.Windows.Forms.DockStyle.Fill;
            // 
            // 
            // 
            // 
            // 
            // 
            this.RadiationChart.Footer.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RadiationChart.Footer.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RadiationChart.Footer.Bevel.StringColorOne = "FFFFFFFF";
            this.RadiationChart.Footer.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.RadiationChart.Header.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RadiationChart.Header.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RadiationChart.Header.Bevel.StringColorOne = "FFFFFFFF";
            this.RadiationChart.Header.Bevel.StringColorTwo = "FF808080";
            this.RadiationChart.Header.Lines = new string[] {
        ""};
            // 
            // 
            // 
            this.RadiationChart.Legend.Alignment = Steema.TeeChart.LegendAlignments.Bottom;
            // 
            // 
            // 
            this.RadiationChart.Legend.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RadiationChart.Legend.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RadiationChart.Legend.Bevel.StringColorOne = "FFFFFFFF";
            this.RadiationChart.Legend.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            this.RadiationChart.Legend.Font.Size = 11;
            this.RadiationChart.Legend.Font.SizeFloat = 11F;
            this.RadiationChart.Legend.LegendStyle = Steema.TeeChart.LegendStyles.Series;
            // 
            // 
            // 
            this.RadiationChart.Legend.Pen.Visible = false;
            this.RadiationChart.Legend.ResizeChart = false;
            // 
            // 
            // 
            this.RadiationChart.Legend.Shadow.Visible = false;
            // 
            // 
            // 
            // 
            // 
            // 
            this.RadiationChart.Legend.Title.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RadiationChart.Legend.Title.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RadiationChart.Legend.Title.Bevel.StringColorOne = "FFFFFFFF";
            this.RadiationChart.Legend.Title.Bevel.StringColorTwo = "FF808080";
            this.RadiationChart.Location = new System.Drawing.Point(0, 0);
            this.RadiationChart.Name = "RadiationChart";
            // 
            // 
            // 
            // 
            // 
            // 
            this.RadiationChart.Panel.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RadiationChart.Panel.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RadiationChart.Panel.Bevel.Outer = Steema.TeeChart.Drawing.BevelStyles.None;
            this.RadiationChart.Panel.Bevel.StringColorOne = "FFFFFFFF";
            this.RadiationChart.Panel.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            this.RadiationChart.Panel.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            // 
            // 
            // 
            this.RadiationChart.Panel.ImageBevel.Width = 1;
            this.RadiationChart.Series.Add(this.RainfallBar2);
            this.RadiationChart.Series.Add(this.RadiationLine);
            this.RadiationChart.Series.Add(this.MaximumRadiationLine);
            this.RadiationChart.Size = new System.Drawing.Size(1007, 681);
            // 
            // 
            // 
            // 
            // 
            // 
            this.RadiationChart.SubFooter.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RadiationChart.SubFooter.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RadiationChart.SubFooter.Bevel.StringColorOne = "FFFFFFFF";
            this.RadiationChart.SubFooter.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.RadiationChart.SubHeader.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RadiationChart.SubHeader.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RadiationChart.SubHeader.Bevel.StringColorOne = "FFFFFFFF";
            this.RadiationChart.SubHeader.Bevel.StringColorTwo = "FF808080";
            this.RadiationChart.TabIndex = 16;
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            // 
            this.RadiationChart.Walls.Back.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RadiationChart.Walls.Back.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RadiationChart.Walls.Back.Bevel.StringColorOne = "FFFFFFFF";
            this.RadiationChart.Walls.Back.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.RadiationChart.Walls.Bottom.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RadiationChart.Walls.Bottom.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RadiationChart.Walls.Bottom.Bevel.StringColorOne = "FFFFFFFF";
            this.RadiationChart.Walls.Bottom.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.RadiationChart.Walls.Left.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RadiationChart.Walls.Left.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RadiationChart.Walls.Left.Bevel.StringColorOne = "FFFFFFFF";
            this.RadiationChart.Walls.Left.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.RadiationChart.Walls.Right.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RadiationChart.Walls.Right.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RadiationChart.Walls.Right.Bevel.StringColorOne = "FFFFFFFF";
            this.RadiationChart.Walls.Right.Bevel.StringColorTwo = "FF808080";
            this.RadiationChart.Walls.Visible = false;
            // 
            // RainfallBar2
            // 
            // 
            // 
            // 
            this.RainfallBar2.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(192)))), ((int)(((byte)(192)))));
            this.RainfallBar2.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(192)))), ((int)(((byte)(192)))));
            this.RainfallBar2.ColorEach = false;
            // 
            // 
            // 
            // 
            // 
            // 
            this.RainfallBar2.Marks.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RainfallBar2.Marks.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RainfallBar2.Marks.Bevel.StringColorOne = "FFFFFFFF";
            this.RainfallBar2.Marks.Bevel.StringColorTwo = "FF808080";
            // 
            // 
            // 
            // 
            // 
            // 
            this.RainfallBar2.Marks.Symbol.Bevel.ColorOne = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.RainfallBar2.Marks.Symbol.Bevel.ColorTwo = System.Drawing.Color.FromArgb(((int)(((byte)(128)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            this.RainfallBar2.Marks.Symbol.Bevel.StringColorOne = "FFFFFFFF";
            this.RainfallBar2.Marks.Symbol.Bevel.StringColorTwo = "FF808080";
            this.RainfallBar2.Marks.Visible = false;
            // 
            // 
            // 
            this.RainfallBar2.Pen.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(115)))), ((int)(((byte)(115)))));
            this.RainfallBar2.Title = "Rainfall";
            // 
            // 
            // 
            this.RainfallBar2.XValues.DataMember = "X";
            this.RainfallBar2.XValues.DateTime = true;
            this.RainfallBar2.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.Ascending;
            // 
            // 
            // 
            this.RainfallBar2.YValues.DataMember = "Bar";
            // 
            // RadiationLine
            // 
            // 
            // 
            // 
            this.RadiationLine.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(68)))), ((int)(((byte)(102)))), ((int)(((byte)(163)))));
            this.RadiationLine.Color = System.Drawing.Color.FromArgb(((int)(((byte)(68)))), ((int)(((byte)(102)))), ((int)(((byte)(163)))));
            this.RadiationLine.ColorEach = false;
            // 
            // 
            // 
            this.RadiationLine.LinePen.Color = System.Drawing.Color.FromArgb(((int)(((byte)(41)))), ((int)(((byte)(61)))), ((int)(((byte)(98)))));
            // 
            // 
            // 
            // 
            // 
            // 
            this.RadiationLine.Pointer.Brush.Color = System.Drawing.Color.Green;
            this.RadiationLine.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle;
            this.RadiationLine.Title = "Radiation";
            this.RadiationLine.VertAxis = Steema.TeeChart.Styles.VerticalAxis.Right;
            // 
            // 
            // 
            this.RadiationLine.XValues.DataMember = "X";
            this.RadiationLine.XValues.DateTime = true;
            this.RadiationLine.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.Ascending;
            // 
            // 
            // 
            this.RadiationLine.YValues.DataMember = "Y";
            // 
            // MaximumRadiationLine
            // 
            // 
            // 
            // 
            this.MaximumRadiationLine.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(243)))), ((int)(((byte)(156)))), ((int)(((byte)(53)))));
            this.MaximumRadiationLine.Color = System.Drawing.Color.FromArgb(((int)(((byte)(243)))), ((int)(((byte)(156)))), ((int)(((byte)(53)))));
            this.MaximumRadiationLine.ColorEach = false;
            // 
            // 
            // 
            this.MaximumRadiationLine.LinePen.Color = System.Drawing.Color.FromArgb(((int)(((byte)(146)))), ((int)(((byte)(94)))), ((int)(((byte)(32)))));
            // 
            // 
            // 
            // 
            // 
            // 
            this.MaximumRadiationLine.Pointer.Brush.Color = System.Drawing.Color.Green;
            this.MaximumRadiationLine.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle;
            this.MaximumRadiationLine.Title = "Maximum radiation";
            this.MaximumRadiationLine.VertAxis = Steema.TeeChart.Styles.VerticalAxis.Right;
            // 
            // 
            // 
            this.MaximumRadiationLine.XValues.DataMember = "X";
            this.MaximumRadiationLine.XValues.DateTime = true;
            this.MaximumRadiationLine.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.Ascending;
            // 
            // 
            // 
            this.MaximumRadiationLine.YValues.DataMember = "Y";
            // 
            // MetGraphControl
            // 
            this.Controls.Add(this.TabControl);
            this.Name = "MetGraphControl";
            this.Size = new System.Drawing.Size(1015, 724);
            this.Controls.SetChildIndex(this.MyHelpLabel, 0);
            this.Controls.SetChildIndex(this.TabControl, 0);
            this.TabControl.ResumeLayout(false);
            this.TabPage1.ResumeLayout(false);
            this.TabPage1.PerformLayout();
            this.YearPanel.ResumeLayout(false);
            this.YearPanel.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.YearStartBox)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.NumYearsBox)).EndInit();
            this.TabPage2.ResumeLayout(false);
            this.TabPage2.PerformLayout();
            this.TabPage3.ResumeLayout(false);
            this.TabPage5.ResumeLayout(false);
            this.TabPage4.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.ImageList TabImages;
        private System.Windows.Forms.TabControl TabControl;
        private System.Windows.Forms.TabPage TabPage1;
        private System.Windows.Forms.TextBox ContentsBox;
        private System.Windows.Forms.TabPage TabPage2;
        private Steema.TeeChart.TChart RainfallChart;
        private System.Windows.Forms.TabPage TabPage3;
        private System.Windows.Forms.TabPage TabPage5;
        private System.Windows.Forms.TabPage TabPage4;
        private Steema.TeeChart.Styles.Bar RainfallBar;
        private Steema.TeeChart.TChart MonthlyRainfallChart;
        private Steema.TeeChart.TChart TemperatureChart;
        private Steema.TeeChart.TChart RadiationChart;
        private Steema.TeeChart.Styles.Bar RainfallBar2;
        private Steema.TeeChart.Styles.Line MaximumTemperatureLine;
        private Steema.TeeChart.Styles.Line MinimumTemperatureLine;
        private Steema.TeeChart.Styles.Line RadiationLine;
        private Steema.TeeChart.Styles.Line MaximumRadiationLine;
        private Steema.TeeChart.Styles.Bar MonthlyRainfallBar;
        private Steema.TeeChart.Styles.Line MonthlyEvaporationLine;
        private System.Windows.Forms.NumericUpDown YearStartBox;
        private System.Windows.Forms.Label Label1;
        private System.Windows.Forms.Label RainfallLabel;
        private System.Windows.Forms.Label Label3;
        private System.Windows.Forms.NumericUpDown NumYearsBox;
        private System.Windows.Forms.Panel YearPanel;
        private System.Windows.Forms.Label Label4;
        private System.Windows.Forms.Label Label5;

    }
}

