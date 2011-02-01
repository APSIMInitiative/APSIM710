namespace Graph
   {
   partial class AddSeriesForm
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
         this.TabControl = new System.Windows.Forms.TabControl();
         this.StandardTab = new System.Windows.Forms.TabPage();
         this.ExtendedTab = new System.Windows.Forms.TabPage();
         this.FinancialTab = new System.Windows.Forms.TabPage();
         this.OtherTab = new System.Windows.Forms.TabPage();
         this.StatsTab = new System.Windows.Forms.TabPage();
         this.tabPage3 = new System.Windows.Forms.TabPage();
         this.DataSourcePanel = new System.Windows.Forms.Panel();
         this.MainPanel = new System.Windows.Forms.Panel();
         this.splitter1 = new System.Windows.Forms.Splitter();
         this.OkButton = new System.Windows.Forms.Button();
         this.CancelBut = new System.Windows.Forms.Button();
         this.AddAnotherButton = new System.Windows.Forms.Button();
         this.GroupBox = new System.Windows.Forms.GroupBox();
         this.TopRadio = new System.Windows.Forms.RadioButton();
         this.BottomRadio = new System.Windows.Forms.RadioButton();
         this.groupBox1 = new System.Windows.Forms.GroupBox();
         this.CumulativeCheckBox = new System.Windows.Forms.CheckBox();
         this.RightRadio = new System.Windows.Forms.RadioButton();
         this.LeftRadio = new System.Windows.Forms.RadioButton();
         this.TabControl.SuspendLayout();
         this.MainPanel.SuspendLayout();
         this.GroupBox.SuspendLayout();
         this.groupBox1.SuspendLayout();
         this.SuspendLayout();
         // 
         // TabControl
         // 
         this.TabControl.Controls.Add(this.StandardTab);
         this.TabControl.Controls.Add(this.ExtendedTab);
         this.TabControl.Controls.Add(this.FinancialTab);
         this.TabControl.Controls.Add(this.OtherTab);
         this.TabControl.Controls.Add(this.StatsTab);
         this.TabControl.Controls.Add(this.tabPage3);
         this.TabControl.Dock = System.Windows.Forms.DockStyle.Top;
         this.TabControl.Location = new System.Drawing.Point(0, 0);
         this.TabControl.Name = "TabControl";
         this.TabControl.SelectedIndex = 0;
         this.TabControl.Size = new System.Drawing.Size(426, 273);
         this.TabControl.TabIndex = 0;
         // 
         // StandardTab
         // 
         this.StandardTab.Location = new System.Drawing.Point(4, 22);
         this.StandardTab.Name = "StandardTab";
         this.StandardTab.Size = new System.Drawing.Size(418, 247);
         this.StandardTab.TabIndex = 0;
         this.StandardTab.Text = "Standard";
         this.StandardTab.UseVisualStyleBackColor = true;
         // 
         // ExtendedTab
         // 
         this.ExtendedTab.Location = new System.Drawing.Point(4, 22);
         this.ExtendedTab.Name = "ExtendedTab";
         this.ExtendedTab.Size = new System.Drawing.Size(418, 247);
         this.ExtendedTab.TabIndex = 1;
         this.ExtendedTab.Text = "Extended";
         this.ExtendedTab.UseVisualStyleBackColor = true;
         // 
         // FinancialTab
         // 
         this.FinancialTab.Location = new System.Drawing.Point(4, 22);
         this.FinancialTab.Name = "FinancialTab";
         this.FinancialTab.Size = new System.Drawing.Size(418, 247);
         this.FinancialTab.TabIndex = 2;
         this.FinancialTab.Text = "Financial";
         this.FinancialTab.UseVisualStyleBackColor = true;
         // 
         // OtherTab
         // 
         this.OtherTab.Location = new System.Drawing.Point(4, 22);
         this.OtherTab.Name = "OtherTab";
         this.OtherTab.Size = new System.Drawing.Size(418, 247);
         this.OtherTab.TabIndex = 3;
         this.OtherTab.Text = "Other";
         this.OtherTab.UseVisualStyleBackColor = true;
         // 
         // StatsTab
         // 
         this.StatsTab.Location = new System.Drawing.Point(4, 22);
         this.StatsTab.Name = "StatsTab";
         this.StatsTab.Size = new System.Drawing.Size(418, 247);
         this.StatsTab.TabIndex = 4;
         this.StatsTab.Text = "Stats";
         this.StatsTab.UseVisualStyleBackColor = true;
         // 
         // tabPage3
         // 
         this.tabPage3.Location = new System.Drawing.Point(4, 22);
         this.tabPage3.Name = "tabPage3";
         this.tabPage3.Size = new System.Drawing.Size(418, 247);
         this.tabPage3.TabIndex = 5;
         this.tabPage3.Text = "3D";
         this.tabPage3.UseVisualStyleBackColor = true;
         // 
         // DataSourcePanel
         // 
         this.DataSourcePanel.Dock = System.Windows.Forms.DockStyle.Fill;
         this.DataSourcePanel.Location = new System.Drawing.Point(0, 276);
         this.DataSourcePanel.Name = "DataSourcePanel";
         this.DataSourcePanel.Size = new System.Drawing.Size(426, 249);
         this.DataSourcePanel.TabIndex = 2;
         // 
         // MainPanel
         // 
         this.MainPanel.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                     | System.Windows.Forms.AnchorStyles.Left)
                     | System.Windows.Forms.AnchorStyles.Right)));
         this.MainPanel.Controls.Add(this.DataSourcePanel);
         this.MainPanel.Controls.Add(this.splitter1);
         this.MainPanel.Controls.Add(this.TabControl);
         this.MainPanel.Location = new System.Drawing.Point(0, 0);
         this.MainPanel.Name = "MainPanel";
         this.MainPanel.Size = new System.Drawing.Size(426, 525);
         this.MainPanel.TabIndex = 0;
         // 
         // splitter1
         // 
         this.splitter1.Dock = System.Windows.Forms.DockStyle.Top;
         this.splitter1.Location = new System.Drawing.Point(0, 273);
         this.splitter1.Name = "splitter1";
         this.splitter1.Size = new System.Drawing.Size(426, 3);
         this.splitter1.TabIndex = 1;
         this.splitter1.TabStop = false;
         // 
         // OkButton
         // 
         this.OkButton.DialogResult = System.Windows.Forms.DialogResult.OK;
         this.OkButton.Location = new System.Drawing.Point(442, 20);
         this.OkButton.Name = "OkButton";
         this.OkButton.Size = new System.Drawing.Size(75, 23);
         this.OkButton.TabIndex = 1;
         this.OkButton.Text = "Ok";
         this.OkButton.UseVisualStyleBackColor = true;
         // 
         // CancelButton
         // 
         this.CancelBut.DialogResult = System.Windows.Forms.DialogResult.Cancel;
         this.CancelBut.Location = new System.Drawing.Point(442, 49);
         this.CancelBut.Name = "CancelButton";
         this.CancelBut.Size = new System.Drawing.Size(75, 23);
         this.CancelBut.TabIndex = 2;
         this.CancelBut.Text = "Cancel";
         this.CancelBut.UseVisualStyleBackColor = true;
         this.CancelBut.Click += new System.EventHandler(this.OnCancel);
         // 
         // AddAnotherButton
         // 
         this.AddAnotherButton.Location = new System.Drawing.Point(442, 103);
         this.AddAnotherButton.Name = "AddAnotherButton";
         this.AddAnotherButton.Size = new System.Drawing.Size(75, 23);
         this.AddAnotherButton.TabIndex = 3;
         this.AddAnotherButton.Text = "Add another";
         this.AddAnotherButton.UseVisualStyleBackColor = true;
         this.AddAnotherButton.Click += new System.EventHandler(this.OnAddAnother);
         // 
         // GroupBox
         // 
         this.GroupBox.Controls.Add(this.TopRadio);
         this.GroupBox.Controls.Add(this.BottomRadio);
         this.GroupBox.Location = new System.Drawing.Point(432, 273);
         this.GroupBox.Name = "GroupBox";
         this.GroupBox.Size = new System.Drawing.Size(95, 69);
         this.GroupBox.TabIndex = 6;
         this.GroupBox.TabStop = false;
         this.GroupBox.Text = "X";
         // 
         // TopRadio
         // 
         this.TopRadio.AutoSize = true;
         this.TopRadio.Location = new System.Drawing.Point(6, 43);
         this.TopRadio.Name = "TopRadio";
         this.TopRadio.Size = new System.Drawing.Size(44, 17);
         this.TopRadio.TabIndex = 1;
         this.TopRadio.Text = "Top";
         this.TopRadio.UseVisualStyleBackColor = true;
         // 
         // BottomRadio
         // 
         this.BottomRadio.AutoSize = true;
         this.BottomRadio.Checked = true;
         this.BottomRadio.Location = new System.Drawing.Point(7, 20);
         this.BottomRadio.Name = "BottomRadio";
         this.BottomRadio.Size = new System.Drawing.Size(58, 17);
         this.BottomRadio.TabIndex = 0;
         this.BottomRadio.TabStop = true;
         this.BottomRadio.Text = "Bottom";
         this.BottomRadio.UseVisualStyleBackColor = true;
         // 
         // groupBox1
         // 
         this.groupBox1.Controls.Add(this.CumulativeCheckBox);
         this.groupBox1.Controls.Add(this.RightRadio);
         this.groupBox1.Controls.Add(this.LeftRadio);
         this.groupBox1.Location = new System.Drawing.Point(432, 348);
         this.groupBox1.Name = "groupBox1";
         this.groupBox1.Size = new System.Drawing.Size(95, 89);
         this.groupBox1.TabIndex = 7;
         this.groupBox1.TabStop = false;
         this.groupBox1.Text = "Y";
         // 
         // CumulativeCheckBox
         // 
         this.CumulativeCheckBox.AutoSize = true;
         this.CumulativeCheckBox.Location = new System.Drawing.Point(6, 66);
         this.CumulativeCheckBox.Name = "CumulativeCheckBox";
         this.CumulativeCheckBox.Size = new System.Drawing.Size(78, 17);
         this.CumulativeCheckBox.TabIndex = 2;
         this.CumulativeCheckBox.Text = "Cumulative";
         this.CumulativeCheckBox.UseVisualStyleBackColor = true;
         // 
         // RightRadio
         // 
         this.RightRadio.AutoSize = true;
         this.RightRadio.Location = new System.Drawing.Point(6, 43);
         this.RightRadio.Name = "RightRadio";
         this.RightRadio.Size = new System.Drawing.Size(50, 17);
         this.RightRadio.TabIndex = 1;
         this.RightRadio.Text = "Right";
         this.RightRadio.UseVisualStyleBackColor = true;
         // 
         // LeftRadio
         // 
         this.LeftRadio.AutoSize = true;
         this.LeftRadio.Checked = true;
         this.LeftRadio.Location = new System.Drawing.Point(7, 20);
         this.LeftRadio.Name = "LeftRadio";
         this.LeftRadio.Size = new System.Drawing.Size(43, 17);
         this.LeftRadio.TabIndex = 0;
         this.LeftRadio.TabStop = true;
         this.LeftRadio.Text = "Left";
         this.LeftRadio.UseVisualStyleBackColor = true;
         // 
         // AddSeriesForm
         // 
         this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
         this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
         this.ClientSize = new System.Drawing.Size(530, 531);
         this.Controls.Add(this.groupBox1);
         this.Controls.Add(this.GroupBox);
         this.Controls.Add(this.OkButton);
         this.Controls.Add(this.CancelBut);
         this.Controls.Add(this.AddAnotherButton);
         this.Controls.Add(this.MainPanel);
         this.Name = "AddSeriesForm";
         this.Text = "GraphUIAddSeriesForm";
         this.Load += new System.EventHandler(this.OnLoad);
         this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.OnClosing);
         this.TabControl.ResumeLayout(false);
         this.MainPanel.ResumeLayout(false);
         this.GroupBox.ResumeLayout(false);
         this.GroupBox.PerformLayout();
         this.groupBox1.ResumeLayout(false);
         this.groupBox1.PerformLayout();
         this.ResumeLayout(false);

         }

      #endregion

      private System.Windows.Forms.TabControl TabControl;
      private System.Windows.Forms.TabPage StandardTab;
      private System.Windows.Forms.TabPage ExtendedTab;
      private System.Windows.Forms.TabPage FinancialTab;
      private System.Windows.Forms.TabPage OtherTab;
      private System.Windows.Forms.TabPage StatsTab;
      private System.Windows.Forms.TabPage tabPage3;
      private System.Windows.Forms.Panel DataSourcePanel;
      private System.Windows.Forms.Panel MainPanel;
      private System.Windows.Forms.Button AddAnotherButton;
      private System.Windows.Forms.Button CancelBut;
      private System.Windows.Forms.Button OkButton;
      private System.Windows.Forms.Splitter splitter1;
      private System.Windows.Forms.GroupBox GroupBox;
      private System.Windows.Forms.RadioButton TopRadio;
      private System.Windows.Forms.RadioButton BottomRadio;
      private System.Windows.Forms.GroupBox groupBox1;
      private System.Windows.Forms.CheckBox CumulativeCheckBox;
      private System.Windows.Forms.RadioButton RightRadio;
      private System.Windows.Forms.RadioButton LeftRadio;
      }
   }