namespace GraphDataUserInterface
   {
   partial class PlotUI
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
         this.YMenuStrip = new System.Windows.Forms.ContextMenuStrip(this.components);
         this.RightAxis = new System.Windows.Forms.ToolStripMenuItem();
         this.CumulativeMenu = new System.Windows.Forms.ToolStripMenuItem();
         this.DeleteYMenu = new System.Windows.Forms.ToolStripMenuItem();
         this.DeleteAllYMenu = new System.Windows.Forms.ToolStripMenuItem();
         this.XMenuStrip = new System.Windows.Forms.ContextMenuStrip(this.components);
         this.TopAxisMenu = new System.Windows.Forms.ToolStripMenuItem();
         this.CumulativeX = new System.Windows.Forms.ToolStripMenuItem();
         this.DeleteXMenu = new System.Windows.Forms.ToolStripMenuItem();
         this.DeleteAllXMenu = new System.Windows.Forms.ToolStripMenuItem();
         this.SpecificColourCheckBox = new System.Windows.Forms.CheckBox();
         this.ColourButton = new Steema.TeeChart.ButtonColor();
         this.X = new System.Windows.Forms.ListBox();
         this.Y = new System.Windows.Forms.ListBox();
         this.label5 = new System.Windows.Forms.Label();
         this.PointCombo = new System.Windows.Forms.ComboBox();
         this.label4 = new System.Windows.Forms.Label();
         this.TypeCombo = new System.Windows.Forms.ComboBox();
         this.YLabel = new System.Windows.Forms.Label();
         this.XLabel = new System.Windows.Forms.Label();
         this.MainPanel.SuspendLayout();
         this.YMenuStrip.SuspendLayout();
         this.XMenuStrip.SuspendLayout();
         this.SuspendLayout();
         // 
         // MainPanel
         // 
         this.MainPanel.AutoSize = false;
         this.MainPanel.Controls.Add(this.SpecificColourCheckBox);
         this.MainPanel.Controls.Add(this.ColourButton);
         this.MainPanel.Controls.Add(this.X);
         this.MainPanel.Controls.Add(this.Y);
         this.MainPanel.Controls.Add(this.label5);
         this.MainPanel.Controls.Add(this.PointCombo);
         this.MainPanel.Controls.Add(this.label4);
         this.MainPanel.Controls.Add(this.TypeCombo);
         this.MainPanel.Controls.Add(this.YLabel);
         this.MainPanel.Controls.Add(this.XLabel);
         this.MainPanel.Location = new System.Drawing.Point(0, 16);
         this.MainPanel.Size = new System.Drawing.Size(620, 185);
         // 
         // MyHelpLabel
         // 
         this.MyHelpLabel.Size = new System.Drawing.Size(620, 16);
         this.MyHelpLabel.Text = "   dd";
         // 
         // YMenuStrip
         // 
         this.YMenuStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.RightAxis,
            this.CumulativeMenu,
            this.DeleteYMenu,
            this.DeleteAllYMenu});
         this.YMenuStrip.Name = "YMenuStrip";
         this.YMenuStrip.Size = new System.Drawing.Size(162, 92);
         // 
         // RightAxis
         // 
         this.RightAxis.Name = "RightAxis";
         this.RightAxis.Size = new System.Drawing.Size(161, 22);
         this.RightAxis.Text = "Right Hand Axis";
         this.RightAxis.Click += new System.EventHandler(this.OnRightAxisClick);
         // 
         // CumulativeMenu
         // 
         this.CumulativeMenu.Name = "CumulativeMenu";
         this.CumulativeMenu.Size = new System.Drawing.Size(161, 22);
         this.CumulativeMenu.Text = "Cumulative";
         this.CumulativeMenu.Click += new System.EventHandler(this.OnCumulativeClick);
         // 
         // DeleteYMenu
         // 
         this.DeleteYMenu.Name = "DeleteYMenu";
         this.DeleteYMenu.Size = new System.Drawing.Size(161, 22);
         this.DeleteYMenu.Text = "Delete";
         this.DeleteYMenu.Click += new System.EventHandler(this.OnDeleteYMenuClick);
         // 
         // DeleteAllYMenu
         // 
         this.DeleteAllYMenu.Name = "DeleteAllYMenu";
         this.DeleteAllYMenu.Size = new System.Drawing.Size(161, 22);
         this.DeleteAllYMenu.Text = "Delete All";
         this.DeleteAllYMenu.Click += new System.EventHandler(this.OnDeleteAllYMenuClick);
         // 
         // XMenuStrip
         // 
         this.XMenuStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.TopAxisMenu,
            this.CumulativeX,
            this.DeleteXMenu,
            this.DeleteAllXMenu});
         this.XMenuStrip.Name = "YMenuStrip";
         this.XMenuStrip.Size = new System.Drawing.Size(139, 92);
         // 
         // TopAxisMenu
         // 
         this.TopAxisMenu.Name = "TopAxisMenu";
         this.TopAxisMenu.Size = new System.Drawing.Size(138, 22);
         this.TopAxisMenu.Text = "Top Axis";
         this.TopAxisMenu.Click += new System.EventHandler(this.OnTopAxisMenuClick);
         // 
         // CumulativeX
         // 
         this.CumulativeX.Name = "CumulativeX";
         this.CumulativeX.Size = new System.Drawing.Size(138, 22);
         this.CumulativeX.Text = "Cumulative";
         this.CumulativeX.Click += new System.EventHandler(this.OnCumulativeXClick);
         // 
         // DeleteXMenu
         // 
         this.DeleteXMenu.Name = "DeleteXMenu";
         this.DeleteXMenu.Size = new System.Drawing.Size(138, 22);
         this.DeleteXMenu.Text = "Delete";
         this.DeleteXMenu.Click += new System.EventHandler(this.OnDeleteXMenuClick);
         // 
         // DeleteAllXMenu
         // 
         this.DeleteAllXMenu.Name = "DeleteAllXMenu";
         this.DeleteAllXMenu.Size = new System.Drawing.Size(138, 22);
         this.DeleteAllXMenu.Text = "Delete All";
         this.DeleteAllXMenu.Click += new System.EventHandler(this.OnDeleteAllXMenuClick);
         // 
         // SpecificColourCheckBox
         // 
         this.SpecificColourCheckBox.AutoSize = true;
         this.SpecificColourCheckBox.Location = new System.Drawing.Point(392, 133);
         this.SpecificColourCheckBox.Name = "SpecificColourCheckBox";
         this.SpecificColourCheckBox.Size = new System.Drawing.Size(102, 17);
         this.SpecificColourCheckBox.TabIndex = 44;
         this.SpecificColourCheckBox.Text = "Specific colour?";
         this.SpecificColourCheckBox.UseVisualStyleBackColor = true;
         this.SpecificColourCheckBox.CheckedChanged += new System.EventHandler(this.OnSpecificColourChange);
         // 
         // ColourButton
         // 
         this.ColourButton.Color = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(192)))), ((int)(((byte)(192)))));
         this.ColourButton.FlatStyle = System.Windows.Forms.FlatStyle.Standard;
         this.ColourButton.Location = new System.Drawing.Point(411, 150);
         this.ColourButton.Name = "ColourButton";
         this.ColourButton.Size = new System.Drawing.Size(102, 23);
         this.ColourButton.TabIndex = 43;
         this.ColourButton.Text = "Plot colour";
         this.ColourButton.UseVisualStyleBackColor = true;
         this.ColourButton.Visible = false;
         this.ColourButton.Click += new System.EventHandler(this.OnColourClick);
         // 
         // X
         // 
         this.X.ContextMenuStrip = this.XMenuStrip;
         this.X.FormattingEnabled = true;
         this.X.Location = new System.Drawing.Point(70, 39);
         this.X.Name = "X";
         this.X.Size = new System.Drawing.Size(140, 134);
         this.X.TabIndex = 42;
         this.X.KeyDown += new System.Windows.Forms.KeyEventHandler(this.OnXKeyDown);
         this.X.Click += new System.EventHandler(this.OnXClick);
         // 
         // Y
         // 
         this.Y.ContextMenuStrip = this.YMenuStrip;
         this.Y.FormattingEnabled = true;
         this.Y.Location = new System.Drawing.Point(229, 39);
         this.Y.Name = "Y";
         this.Y.Size = new System.Drawing.Size(140, 134);
         this.Y.TabIndex = 41;
         this.Y.KeyDown += new System.Windows.Forms.KeyEventHandler(this.OnYKeyDown);
         this.Y.Click += new System.EventHandler(this.OnYClick);
         // 
         // label5
         // 
         this.label5.AutoSize = true;
         this.label5.Location = new System.Drawing.Point(425, 75);
         this.label5.Name = "label5";
         this.label5.Size = new System.Drawing.Size(54, 13);
         this.label5.TabIndex = 40;
         this.label5.Text = "Point type";
         this.label5.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
         // 
         // PointCombo
         // 
         this.PointCombo.FormattingEnabled = true;
         this.PointCombo.Items.AddRange(new object[] {
            "None",
            "Circle",
            "Cross",
            "Diagonal cross",
            "Diamond",
            "Down triangle",
            "Left triangle",
            "Rectangle",
            "Right triangle",
            "Small dot",
            "Triangle"});
         this.PointCombo.Location = new System.Drawing.Point(392, 94);
         this.PointCombo.Name = "PointCombo";
         this.PointCombo.Size = new System.Drawing.Size(121, 21);
         this.PointCombo.TabIndex = 39;
         this.PointCombo.SelectedValueChanged += new System.EventHandler(this.OnPointChanged);
         // 
         // label4
         // 
         this.label4.AutoSize = true;
         this.label4.Location = new System.Drawing.Point(436, 18);
         this.label4.Name = "label4";
         this.label4.Size = new System.Drawing.Size(31, 13);
         this.label4.TabIndex = 38;
         this.label4.Text = "Type";
         this.label4.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
         // 
         // TypeCombo
         // 
         this.TypeCombo.AutoCompleteMode = System.Windows.Forms.AutoCompleteMode.Suggest;
         this.TypeCombo.AutoCompleteSource = System.Windows.Forms.AutoCompleteSource.ListItems;
         this.TypeCombo.FormattingEnabled = true;
         this.TypeCombo.Items.AddRange(new object[] {
            "No line",
            "Solid line",
            "Dash line",
            "Dashdot line",
            "Dashdotdot line",
            "Dot line",
            "Bar",
            "Box"});
         this.TypeCombo.Location = new System.Drawing.Point(392, 39);
         this.TypeCombo.Name = "TypeCombo";
         this.TypeCombo.Size = new System.Drawing.Size(121, 21);
         this.TypeCombo.TabIndex = 37;
         this.TypeCombo.SelectedValueChanged += new System.EventHandler(this.OnTypeChanged);
         // 
         // YLabel
         // 
         this.YLabel.AutoSize = true;
         this.YLabel.Location = new System.Drawing.Point(226, 21);
         this.YLabel.Name = "YLabel";
         this.YLabel.Size = new System.Drawing.Size(59, 13);
         this.YLabel.TabIndex = 36;
         this.YLabel.Text = "Y variables";
         // 
         // XLabel
         // 
         this.XLabel.AutoSize = true;
         this.XLabel.Location = new System.Drawing.Point(67, 21);
         this.XLabel.Name = "XLabel";
         this.XLabel.Size = new System.Drawing.Size(59, 13);
         this.XLabel.TabIndex = 35;
         this.XLabel.Text = "X variables";
         // 
         // PlotUI
         // 
         this.HelpText = "   dd";
         this.Name = "PlotUI";
         this.Size = new System.Drawing.Size(620, 400);
         this.Controls.SetChildIndex(this.MyHelpLabel, 0);
         this.Controls.SetChildIndex(this.MainPanel, 0);
         this.MainPanel.ResumeLayout(false);
         this.MainPanel.PerformLayout();
         this.YMenuStrip.ResumeLayout(false);
         this.XMenuStrip.ResumeLayout(false);
         this.ResumeLayout(false);

         }

      #endregion

      private System.Windows.Forms.ContextMenuStrip YMenuStrip;
      private System.Windows.Forms.ToolStripMenuItem CumulativeMenu;
      private System.Windows.Forms.ToolStripMenuItem DeleteYMenu;
      private System.Windows.Forms.ContextMenuStrip XMenuStrip;
      private System.Windows.Forms.ToolStripMenuItem TopAxisMenu;
      private System.Windows.Forms.ToolStripMenuItem DeleteXMenu;
      private System.Windows.Forms.ToolStripMenuItem DeleteAllXMenu;
      private System.Windows.Forms.ToolStripMenuItem DeleteAllYMenu;
      private System.Windows.Forms.ToolStripMenuItem RightAxis;
      private System.Windows.Forms.CheckBox SpecificColourCheckBox;
      private Steema.TeeChart.ButtonColor ColourButton;
      private System.Windows.Forms.ListBox X;
      private System.Windows.Forms.ListBox Y;
      private System.Windows.Forms.Label label5;
      private System.Windows.Forms.ComboBox PointCombo;
      private System.Windows.Forms.Label label4;
      private System.Windows.Forms.ComboBox TypeCombo;
      private System.Windows.Forms.Label YLabel;
      private System.Windows.Forms.Label XLabel;
      private System.Windows.Forms.ToolStripMenuItem CumulativeX;
      }
   }
