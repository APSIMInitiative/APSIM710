namespace CSUserInterface
{
    partial class AreaUI
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
            this.ListView = new System.Windows.Forms.ListView();
            this.ColumnHeader1 = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
            this.ListViewContextMenu = new System.Windows.Forms.ContextMenu();
            this.MenuItem1 = new System.Windows.Forms.MenuItem();
            this.OpenFileDialog = new System.Windows.Forms.OpenFileDialog();
            this.panelArea = new System.Windows.Forms.Panel();
            this.label1 = new System.Windows.Forms.Label();
            this.textBoxArea = new System.Windows.Forms.TextBox();
            this.panelArea.SuspendLayout();
            this.SuspendLayout();
            // 
            // MyHelpLabel
            // 
            this.MyHelpLabel.Size = new System.Drawing.Size(940, 16);
            // 
            // ListView
            // 
            this.ListView.Alignment = System.Windows.Forms.ListViewAlignment.Default;
            this.ListView.AllowDrop = true;
            this.ListView.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.ListView.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.ColumnHeader1});
            this.ListView.ContextMenu = this.ListViewContextMenu;
            this.ListView.Dock = System.Windows.Forms.DockStyle.Fill;
            this.ListView.Location = new System.Drawing.Point(0, 66);
            this.ListView.MultiSelect = false;
            this.ListView.Name = "ListView";
            this.ListView.Size = new System.Drawing.Size(940, 519);
            this.ListView.TabIndex = 0;
            this.ListView.UseCompatibleStateImageBehavior = false;
            this.ListView.ItemDrag += new System.Windows.Forms.ItemDragEventHandler(this.ListView_ItemDrag);
            this.ListView.DragDrop += new System.Windows.Forms.DragEventHandler(this.ListView_DragDrop);
            this.ListView.DragEnter += new System.Windows.Forms.DragEventHandler(this.ListView_DragEnter);
            this.ListView.DragOver += new System.Windows.Forms.DragEventHandler(this.ListView_DragOver);
            this.ListView.DoubleClick += new System.EventHandler(this.ListView_DoubleClick);
            this.ListView.KeyDown += new System.Windows.Forms.KeyEventHandler(this.ListView_KeyDown);
            // 
            // ColumnHeader1
            // 
            this.ColumnHeader1.Width = -2;
            // 
            // ListViewContextMenu
            // 
            this.ListViewContextMenu.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.MenuItem1});
            // 
            // MenuItem1
            // 
            this.MenuItem1.Index = 0;
            this.MenuItem1.Text = "Load picture";
            this.MenuItem1.Click += new System.EventHandler(this.MenuItem1_Click);
            // 
            // OpenFileDialog
            // 
            this.OpenFileDialog.DefaultExt = "jpg";
            this.OpenFileDialog.Filter = "\"JPG files|*.jpg|BMP files|*.bmp|All files|*.*";
            this.OpenFileDialog.RestoreDirectory = true;
            this.OpenFileDialog.Title = "Select a picture to load";
            // 
            // panelArea
            // 
            this.panelArea.Controls.Add(this.label1);
            this.panelArea.Controls.Add(this.textBoxArea);
            this.panelArea.Dock = System.Windows.Forms.DockStyle.Top;
            this.panelArea.Location = new System.Drawing.Point(0, 16);
            this.panelArea.MaximumSize = new System.Drawing.Size(0, 50);
            this.panelArea.Name = "panelArea";
            this.panelArea.Size = new System.Drawing.Size(940, 50);
            this.panelArea.TabIndex = 2;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(12, 19);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(98, 13);
            this.label1.TabIndex = 1;
            this.label1.Text = "Paddock area (ha):";
            // 
            // textBoxArea
            // 
            this.textBoxArea.Location = new System.Drawing.Point(116, 16);
            this.textBoxArea.Name = "textBoxArea";
            this.textBoxArea.Size = new System.Drawing.Size(67, 20);
            this.textBoxArea.TabIndex = 0;
            // 
            // AreaUI
            // 
            this.Controls.Add(this.ListView);
            this.Controls.Add(this.panelArea);
            this.Name = "AreaUI";
            this.Size = new System.Drawing.Size(940, 585);
            this.Controls.SetChildIndex(this.MyHelpLabel, 0);
            this.Controls.SetChildIndex(this.panelArea, 0);
            this.Controls.SetChildIndex(this.ListView, 0);
            this.panelArea.ResumeLayout(false);
            this.panelArea.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.ListView ListView;
        private System.Windows.Forms.ContextMenu ListViewContextMenu;
        private System.Windows.Forms.MenuItem MenuItem1;
        private System.Windows.Forms.OpenFileDialog OpenFileDialog;
        private System.Windows.Forms.ColumnHeader ColumnHeader1;
        private System.Windows.Forms.Panel panelArea;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.TextBox textBoxArea;
    }
}
