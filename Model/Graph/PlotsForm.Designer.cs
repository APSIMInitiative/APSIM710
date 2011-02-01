namespace Graph
   {
   partial class PlotsForm
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
         this.PlotListContextMenu = new System.Windows.Forms.ContextMenuStrip(this.components);
         this.DeleteMenu = new System.Windows.Forms.ToolStripMenuItem();
         this.splitter2 = new System.Windows.Forms.Splitter();
         this.tabControl1 = new System.Windows.Forms.TabControl();
         this.Standard = new System.Windows.Forms.TabPage();
         this.Extended = new System.Windows.Forms.TabPage();
         this.Financial = new System.Windows.Forms.TabPage();
         this.Other = new System.Windows.Forms.TabPage();
         this.DetailsPanel = new System.Windows.Forms.Panel();
         this.label1 = new System.Windows.Forms.Label();
         this.DataList = new System.Windows.Forms.ComboBox();
         this.Y4Label = new System.Windows.Forms.Label();
         this.Y4 = new System.Windows.Forms.ListBox();
         this.Y3 = new System.Windows.Forms.ListBox();
         this.Y3Label = new System.Windows.Forms.Label();
         this.Y2 = new System.Windows.Forms.ListBox();
         this.Y2Label = new System.Windows.Forms.Label();
         this.Y1 = new System.Windows.Forms.ListBox();
         this.Y1Label = new System.Windows.Forms.Label();
         this.X = new System.Windows.Forms.ListBox();
         this.XLabel = new System.Windows.Forms.Label();
         this.DataGrid = new System.Windows.Forms.DataGridView();
         this.splitter3 = new System.Windows.Forms.Splitter();
         this.panel1 = new System.Windows.Forms.Panel();
         this.CancelBut = new System.Windows.Forms.Button();
         this.OkButton = new System.Windows.Forms.Button();
         this.PlotListContextMenu.SuspendLayout();
         this.tabControl1.SuspendLayout();
         this.DetailsPanel.SuspendLayout();
         ((System.ComponentModel.ISupportInitialize)(this.DataGrid)).BeginInit();
         this.panel1.SuspendLayout();
         this.SuspendLayout();
         // 
         // PlotListContextMenu
         // 
         this.PlotListContextMenu.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.DeleteMenu});
         this.PlotListContextMenu.Name = "PlotListContextMenu";
         this.PlotListContextMenu.Size = new System.Drawing.Size(132, 26);
         // 
         // DeleteMenu
         // 
         this.DeleteMenu.Name = "DeleteMenu";
         this.DeleteMenu.ShortcutKeys = System.Windows.Forms.Keys.Delete;
         this.DeleteMenu.Size = new System.Drawing.Size(131, 22);
         this.DeleteMenu.Text = "Delete";
         this.DeleteMenu.Click += new System.EventHandler(this.OnDeleteVariable);
         // 
         // splitter2
         // 
         this.splitter2.Dock = System.Windows.Forms.DockStyle.Top;
         this.splitter2.Location = new System.Drawing.Point(0, 169);
         this.splitter2.Name = "splitter2";
         this.splitter2.Size = new System.Drawing.Size(630, 3);
         this.splitter2.TabIndex = 4;
         this.splitter2.TabStop = false;
         // 
         // tabControl1
         // 
         this.tabControl1.Controls.Add(this.Standard);
         this.tabControl1.Controls.Add(this.Extended);
         this.tabControl1.Controls.Add(this.Financial);
         this.tabControl1.Controls.Add(this.Other);
         this.tabControl1.Dock = System.Windows.Forms.DockStyle.Top;
         this.tabControl1.Location = new System.Drawing.Point(0, 0);
         this.tabControl1.Name = "tabControl1";
         this.tabControl1.SelectedIndex = 0;
         this.tabControl1.Size = new System.Drawing.Size(630, 169);
         this.tabControl1.TabIndex = 5;
         // 
         // Standard
         // 
         this.Standard.Location = new System.Drawing.Point(4, 22);
         this.Standard.Name = "Standard";
         this.Standard.Padding = new System.Windows.Forms.Padding(3);
         this.Standard.Size = new System.Drawing.Size(622, 143);
         this.Standard.TabIndex = 0;
         this.Standard.Text = "Standard";
         this.Standard.UseVisualStyleBackColor = true;
         // 
         // Extended
         // 
         this.Extended.Location = new System.Drawing.Point(4, 22);
         this.Extended.Name = "Extended";
         this.Extended.Padding = new System.Windows.Forms.Padding(3);
         this.Extended.Size = new System.Drawing.Size(622, 143);
         this.Extended.TabIndex = 1;
         this.Extended.Text = "Extended";
         this.Extended.UseVisualStyleBackColor = true;
         // 
         // Financial
         // 
         this.Financial.Location = new System.Drawing.Point(4, 22);
         this.Financial.Name = "Financial";
         this.Financial.Size = new System.Drawing.Size(622, 143);
         this.Financial.TabIndex = 3;
         this.Financial.Text = "Financial";
         this.Financial.UseVisualStyleBackColor = true;
         // 
         // Other
         // 
         this.Other.Location = new System.Drawing.Point(4, 22);
         this.Other.Name = "Other";
         this.Other.Size = new System.Drawing.Size(622, 143);
         this.Other.TabIndex = 2;
         this.Other.Text = "Other";
         this.Other.UseVisualStyleBackColor = true;
         // 
         // DetailsPanel
         // 
         this.DetailsPanel.Controls.Add(this.label1);
         this.DetailsPanel.Controls.Add(this.DataList);
         this.DetailsPanel.Controls.Add(this.Y4Label);
         this.DetailsPanel.Controls.Add(this.Y4);
         this.DetailsPanel.Controls.Add(this.Y3);
         this.DetailsPanel.Controls.Add(this.Y3Label);
         this.DetailsPanel.Controls.Add(this.Y2);
         this.DetailsPanel.Controls.Add(this.Y2Label);
         this.DetailsPanel.Controls.Add(this.Y1);
         this.DetailsPanel.Controls.Add(this.Y1Label);
         this.DetailsPanel.Controls.Add(this.X);
         this.DetailsPanel.Controls.Add(this.XLabel);
         this.DetailsPanel.Dock = System.Windows.Forms.DockStyle.Top;
         this.DetailsPanel.Location = new System.Drawing.Point(0, 172);
         this.DetailsPanel.Name = "DetailsPanel";
         this.DetailsPanel.Size = new System.Drawing.Size(630, 166);
         this.DetailsPanel.TabIndex = 6;
         // 
         // label1
         // 
         this.label1.AutoSize = true;
         this.label1.Location = new System.Drawing.Point(4, 142);
         this.label1.Name = "label1";
         this.label1.Size = new System.Drawing.Size(33, 13);
         this.label1.TabIndex = 54;
         this.label1.Text = "Data:";
         // 
         // DataList
         // 
         this.DataList.FormattingEnabled = true;
         this.DataList.Location = new System.Drawing.Point(43, 139);
         this.DataList.Name = "DataList";
         this.DataList.Size = new System.Drawing.Size(579, 21);
         this.DataList.TabIndex = 53;
         this.DataList.TextChanged += new System.EventHandler(this.OnDataListChanged);
         // 
         // Y4Label
         // 
         this.Y4Label.AutoSize = true;
         this.Y4Label.Location = new System.Drawing.Point(501, 6);
         this.Y4Label.Name = "Y4Label";
         this.Y4Label.Size = new System.Drawing.Size(20, 13);
         this.Y4Label.TabIndex = 52;
         this.Y4Label.Text = "Y4";
         // 
         // Y4
         // 
         this.Y4.ContextMenuStrip = this.PlotListContextMenu;
         this.Y4.FormattingEnabled = true;
         this.Y4.Location = new System.Drawing.Point(504, 22);
         this.Y4.Name = "Y4";
         this.Y4.Size = new System.Drawing.Size(118, 108);
         this.Y4.TabIndex = 51;
         // 
         // Y3
         // 
         this.Y3.ContextMenuStrip = this.PlotListContextMenu;
         this.Y3.FormattingEnabled = true;
         this.Y3.Location = new System.Drawing.Point(379, 22);
         this.Y3.Name = "Y3";
         this.Y3.Size = new System.Drawing.Size(118, 108);
         this.Y3.TabIndex = 50;
         // 
         // Y3Label
         // 
         this.Y3Label.AutoSize = true;
         this.Y3Label.Location = new System.Drawing.Point(379, 6);
         this.Y3Label.Name = "Y3Label";
         this.Y3Label.Size = new System.Drawing.Size(20, 13);
         this.Y3Label.TabIndex = 49;
         this.Y3Label.Text = "Y3";
         // 
         // Y2
         // 
         this.Y2.ContextMenuStrip = this.PlotListContextMenu;
         this.Y2.FormattingEnabled = true;
         this.Y2.Location = new System.Drawing.Point(255, 22);
         this.Y2.Name = "Y2";
         this.Y2.Size = new System.Drawing.Size(118, 108);
         this.Y2.TabIndex = 48;
         // 
         // Y2Label
         // 
         this.Y2Label.AutoSize = true;
         this.Y2Label.Location = new System.Drawing.Point(255, 6);
         this.Y2Label.Name = "Y2Label";
         this.Y2Label.Size = new System.Drawing.Size(20, 13);
         this.Y2Label.TabIndex = 47;
         this.Y2Label.Text = "Y2";
         // 
         // Y1
         // 
         this.Y1.ContextMenuStrip = this.PlotListContextMenu;
         this.Y1.FormattingEnabled = true;
         this.Y1.Location = new System.Drawing.Point(131, 22);
         this.Y1.Name = "Y1";
         this.Y1.Size = new System.Drawing.Size(118, 108);
         this.Y1.TabIndex = 46;
         this.Y1.Click += new System.EventHandler(this.OnXYListClick);
         // 
         // Y1Label
         // 
         this.Y1Label.AutoSize = true;
         this.Y1Label.Location = new System.Drawing.Point(128, 6);
         this.Y1Label.Name = "Y1Label";
         this.Y1Label.Size = new System.Drawing.Size(20, 13);
         this.Y1Label.TabIndex = 45;
         this.Y1Label.Text = "Y1";
         // 
         // X
         // 
         this.X.ContextMenuStrip = this.PlotListContextMenu;
         this.X.FormattingEnabled = true;
         this.X.Location = new System.Drawing.Point(6, 22);
         this.X.Name = "X";
         this.X.Size = new System.Drawing.Size(119, 108);
         this.X.TabIndex = 44;
         this.X.Click += new System.EventHandler(this.OnXYListClick);
         // 
         // XLabel
         // 
         this.XLabel.AutoSize = true;
         this.XLabel.Location = new System.Drawing.Point(6, 6);
         this.XLabel.Name = "XLabel";
         this.XLabel.Size = new System.Drawing.Size(14, 13);
         this.XLabel.TabIndex = 43;
         this.XLabel.Text = "X";
         // 
         // DataGrid
         // 
         this.DataGrid.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
         this.DataGrid.Dock = System.Windows.Forms.DockStyle.Fill;
         this.DataGrid.Location = new System.Drawing.Point(0, 338);
         this.DataGrid.Name = "DataGrid";
         this.DataGrid.Size = new System.Drawing.Size(630, 188);
         this.DataGrid.TabIndex = 7;
         this.DataGrid.CellMouseClick += new System.Windows.Forms.DataGridViewCellMouseEventHandler(this.OnDataGridCellMouseClick);
         // 
         // splitter3
         // 
         this.splitter3.Dock = System.Windows.Forms.DockStyle.Top;
         this.splitter3.Location = new System.Drawing.Point(0, 338);
         this.splitter3.Name = "splitter3";
         this.splitter3.Size = new System.Drawing.Size(630, 3);
         this.splitter3.TabIndex = 8;
         this.splitter3.TabStop = false;
         // 
         // panel1
         // 
         this.panel1.Controls.Add(this.CancelBut);
         this.panel1.Controls.Add(this.OkButton);
         this.panel1.Dock = System.Windows.Forms.DockStyle.Right;
         this.panel1.Location = new System.Drawing.Point(630, 0);
         this.panel1.Name = "panel1";
         this.panel1.Size = new System.Drawing.Size(98, 526);
         this.panel1.TabIndex = 55;
         // 
         // CancelBut
         // 
         this.CancelBut.DialogResult = System.Windows.Forms.DialogResult.Cancel;
         this.CancelBut.Location = new System.Drawing.Point(11, 52);
         this.CancelBut.Name = "CancelBut";
         this.CancelBut.Size = new System.Drawing.Size(75, 23);
         this.CancelBut.TabIndex = 1;
         this.CancelBut.Text = "Cancel";
         this.CancelBut.UseVisualStyleBackColor = true;
         this.CancelBut.Click += new System.EventHandler(this.OnCancelClick);
         // 
         // OkButton
         // 
         this.OkButton.DialogResult = System.Windows.Forms.DialogResult.OK;
         this.OkButton.Location = new System.Drawing.Point(11, 22);
         this.OkButton.Name = "OkButton";
         this.OkButton.Size = new System.Drawing.Size(75, 23);
         this.OkButton.TabIndex = 0;
         this.OkButton.Text = "Ok";
         this.OkButton.UseVisualStyleBackColor = true;
         this.OkButton.Click += new System.EventHandler(this.OnOkClick);
         // 
         // PlotsForm
         // 
         this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
         this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
         this.ClientSize = new System.Drawing.Size(728, 526);
         this.Controls.Add(this.splitter3);
         this.Controls.Add(this.DataGrid);
         this.Controls.Add(this.DetailsPanel);
         this.Controls.Add(this.splitter2);
         this.Controls.Add(this.tabControl1);
         this.Controls.Add(this.panel1);
         this.Name = "PlotsForm";
         this.Text = "Plot details";
         this.PlotListContextMenu.ResumeLayout(false);
         this.tabControl1.ResumeLayout(false);
         this.DetailsPanel.ResumeLayout(false);
         this.DetailsPanel.PerformLayout();
         ((System.ComponentModel.ISupportInitialize)(this.DataGrid)).EndInit();
         this.panel1.ResumeLayout(false);
         this.ResumeLayout(false);

         }

      #endregion

      private System.Windows.Forms.ContextMenuStrip PlotListContextMenu;
      private System.Windows.Forms.ToolStripMenuItem DeleteMenu;
      private System.Windows.Forms.Splitter splitter2;
      private System.Windows.Forms.TabControl tabControl1;
      private System.Windows.Forms.TabPage Standard;
      private System.Windows.Forms.TabPage Extended;
      private System.Windows.Forms.TabPage Other;
      private System.Windows.Forms.Panel DetailsPanel;
      private System.Windows.Forms.DataGridView DataGrid;
      private System.Windows.Forms.Splitter splitter3;
      private System.Windows.Forms.Label Y4Label;
      private System.Windows.Forms.ListBox Y4;
      private System.Windows.Forms.ListBox Y3;
      private System.Windows.Forms.Label Y3Label;
      private System.Windows.Forms.ListBox Y2;
      private System.Windows.Forms.Label Y2Label;
      private System.Windows.Forms.ListBox Y1;
      private System.Windows.Forms.Label Y1Label;
      private System.Windows.Forms.ListBox X;
      private System.Windows.Forms.Label XLabel;
      private System.Windows.Forms.TabPage Financial;
      private System.Windows.Forms.Label label1;
      private System.Windows.Forms.ComboBox DataList;
      private System.Windows.Forms.Panel panel1;
      private System.Windows.Forms.Button CancelBut;
      private System.Windows.Forms.Button OkButton;

      }
   }