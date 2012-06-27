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
            this.ListView.AutoArrange = false;
            this.ListView.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.ListView.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.ColumnHeader1});
            this.ListView.ContextMenu = this.ListViewContextMenu;
            this.ListView.Dock = System.Windows.Forms.DockStyle.Fill;
            this.ListView.Location = new System.Drawing.Point(0, 16);
            this.ListView.MultiSelect = false;
            this.ListView.Name = "ListView";
            this.ListView.Size = new System.Drawing.Size(940, 569);
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
            // AreaUI
            // 
            this.Controls.Add(this.ListView);
            this.Name = "AreaUI";
            this.Size = new System.Drawing.Size(940, 585);
            this.Controls.SetChildIndex(this.MyHelpLabel, 0);
            this.Controls.SetChildIndex(this.ListView, 0);
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.ListView ListView;
        private System.Windows.Forms.ContextMenu ListViewContextMenu;
        private System.Windows.Forms.MenuItem MenuItem1;
        private System.Windows.Forms.OpenFileDialog OpenFileDialog;
        private System.Windows.Forms.ColumnHeader ColumnHeader1;
    }
}
