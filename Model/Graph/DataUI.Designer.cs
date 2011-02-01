namespace Graph
   {
   partial class DataUI : Controllers.BaseView
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
         this.DataGrid = new System.Windows.Forms.DataGridView();
         this.MainPanel = new System.Windows.Forms.Panel();
         this.Properties = new VBUserInterface.GenericUI();
         this.splitter1 = new System.Windows.Forms.Splitter();
         ((System.ComponentModel.ISupportInitialize)(this.DataGrid)).BeginInit();
         this.SuspendLayout();
         // 
         // DataGrid
         // 
         this.DataGrid.AllowUserToAddRows = false;
         this.DataGrid.AllowUserToDeleteRows = false;
         this.DataGrid.ClipboardCopyMode = System.Windows.Forms.DataGridViewClipboardCopyMode.EnableAlwaysIncludeHeaderText;
         this.DataGrid.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
         this.DataGrid.Cursor = System.Windows.Forms.Cursors.Hand;
         this.DataGrid.Dock = System.Windows.Forms.DockStyle.Fill;
         this.DataGrid.Location = new System.Drawing.Point(0, 151);
         this.DataGrid.Name = "DataGrid";
         this.DataGrid.ReadOnly = true;
         this.DataGrid.RowHeadersWidth = 16;
         this.DataGrid.RowTemplate.Resizable = System.Windows.Forms.DataGridViewTriState.True;
         this.DataGrid.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.CellSelect;
         this.DataGrid.ShowEditingIcon = false;
         this.DataGrid.ShowRowErrors = false;
         this.DataGrid.Size = new System.Drawing.Size(655, 337);
         this.DataGrid.TabIndex = 2;
         this.DataGrid.CellMouseClick += new System.Windows.Forms.DataGridViewCellMouseEventHandler(this.OnCellMouseClick);
         // 
         // MainPanel
         // 
         this.MainPanel.AutoSize = true;
         this.MainPanel.Dock = System.Windows.Forms.DockStyle.Top;
         this.MainPanel.Location = new System.Drawing.Point(0, 16);
         this.MainPanel.Name = "MainPanel";
         this.MainPanel.Size = new System.Drawing.Size(655, 0);
         this.MainPanel.TabIndex = 6;
         // 
         // Properties
         // 
         this.Properties.AutoScroll = true;
         this.Properties.BackColor = System.Drawing.SystemColors.Window;
         this.Properties.Dock = System.Windows.Forms.DockStyle.Top;
         this.Properties.HelpText = "";
         this.Properties.Location = new System.Drawing.Point(0, 16);
         this.Properties.Name = "Properties";
         this.Properties.Size = new System.Drawing.Size(655, 135);
         this.Properties.TabIndex = 7;
         // 
         // splitter1
         // 
         this.splitter1.Dock = System.Windows.Forms.DockStyle.Top;
         this.splitter1.Location = new System.Drawing.Point(0, 151);
         this.splitter1.Name = "splitter1";
         this.splitter1.Size = new System.Drawing.Size(655, 3);
         this.splitter1.TabIndex = 8;
         this.splitter1.TabStop = false;
         // 
         // DataUI
         // 
         this.Controls.Add(this.splitter1);
         this.Controls.Add(this.DataGrid);
         this.Controls.Add(this.Properties);
         this.Controls.Add(this.MainPanel);
         this.Name = "DataUI";
         this.Size = new System.Drawing.Size(655, 488);
         this.Controls.SetChildIndex(this.MyHelpLabel, 0);
         this.Controls.SetChildIndex(this.MainPanel, 0);
         this.Controls.SetChildIndex(this.Properties, 0);
         this.Controls.SetChildIndex(this.DataGrid, 0);
         this.Controls.SetChildIndex(this.splitter1, 0);
         ((System.ComponentModel.ISupportInitialize)(this.DataGrid)).EndInit();
         this.ResumeLayout(false);
         this.PerformLayout();

         }

      #endregion

      protected System.Windows.Forms.Panel MainPanel;
      public System.Windows.Forms.DataGridView DataGrid;
      private VBUserInterface.GenericUI Properties;
      private System.Windows.Forms.Splitter splitter1;
      }
   }
