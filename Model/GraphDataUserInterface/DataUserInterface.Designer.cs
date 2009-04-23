namespace GraphDataUserInterface
   {
   partial class DataUserInterface
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
         this.DataGrid.Location = new System.Drawing.Point(0, 16);
         this.DataGrid.Name = "DataGrid";
         this.DataGrid.ReadOnly = true;
         this.DataGrid.RowHeadersVisible = false;
         this.DataGrid.RowHeadersWidth = 16;
         this.DataGrid.RowTemplate.Resizable = System.Windows.Forms.DataGridViewTriState.True;
         this.DataGrid.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.CellSelect;
         this.DataGrid.ShowEditingIcon = false;
         this.DataGrid.ShowRowErrors = false;
         this.DataGrid.Size = new System.Drawing.Size(655, 472);
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
         // DataUserInterface
         // 
         this.Controls.Add(this.DataGrid);
         this.Controls.Add(this.MainPanel);
         this.Name = "DataUserInterface";
         this.Size = new System.Drawing.Size(655, 488);
         this.Controls.SetChildIndex(this.MyHelpLabel, 0);
         this.Controls.SetChildIndex(this.MainPanel, 0);
         this.Controls.SetChildIndex(this.DataGrid, 0);
         ((System.ComponentModel.ISupportInitialize)(this.DataGrid)).EndInit();
         this.ResumeLayout(false);
         this.PerformLayout();

         }

      #endregion

      protected System.Windows.Forms.Panel MainPanel;
      public System.Windows.Forms.DataGridView DataGrid;
      }
   }
