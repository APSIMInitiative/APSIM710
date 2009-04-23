namespace GraphUserInterface
    {
    partial class ChartPageUI
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
            this.PopupMenu = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.FormDesigner = new Greatis.FormDesigner.Designer();
            this.SuspendLayout();
            // 
            // PopupMenu
            // 
            this.PopupMenu.Name = "PopupMenu";
            this.PopupMenu.Size = new System.Drawing.Size(61, 4);
            this.PopupMenu.Opening += new System.ComponentModel.CancelEventHandler(this.OnMenuOpening);
            // 
            // FormDesigner
            // 
            this.FormDesigner.Active = false;
            this.FormDesigner.DesignedComponents = null;
            this.FormDesigner.DesignedForm = this;
            this.FormDesigner.FormTreasury = null;
            this.FormDesigner.GridSize = new System.Drawing.Size(8, 8);
            this.FormDesigner.LogFile = null;
            // 
            // ChartPageUI
            // 
            this.AutoScroll = false;
            this.BackColor = System.Drawing.SystemColors.Window;
            this.ContextMenuStrip = this.PopupMenu;
            this.Name = "ChartPageUI";
            this.Size = new System.Drawing.Size(345, 150);
            this.Paint += new System.Windows.Forms.PaintEventHandler(this.OnCanvasPaint);
            this.ResumeLayout(false);

            }

        #endregion

        private System.Windows.Forms.ContextMenuStrip PopupMenu;
        private Greatis.FormDesigner.Designer FormDesigner;
        }
    }
