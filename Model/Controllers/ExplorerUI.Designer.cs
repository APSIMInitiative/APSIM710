namespace Controllers
{
    partial class ExplorerUI
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
            this.Splitter = new System.Windows.Forms.Splitter();
            this.pnlDisplayArea = new System.Windows.Forms.Panel();
            this.pnlHost = new System.Windows.Forms.Panel();
            this.Panel1 = new System.Windows.Forms.Panel();
            this.Splitter1 = new System.Windows.Forms.Splitter();
            this.pnlPlaceHolder = new System.Windows.Forms.Panel();
            this.Title = new System.Windows.Forms.Panel();
            this.Label1 = new System.Windows.Forms.Label();
            this.UIPanel = new System.Windows.Forms.Panel();
            this.Label2 = new System.Windows.Forms.Label();
            this.FactorTree = new Controllers.FactorTree(this.components);
            this.DataTree = new Controllers.DataTree();
            this.pnlDisplayArea.SuspendLayout();
            this.pnlHost.SuspendLayout();
            this.pnlPlaceHolder.SuspendLayout();
            this.Title.SuspendLayout();
            this.SuspendLayout();
            //
            //Splitter
            //
            this.Splitter.BackColor = System.Drawing.SystemColors.ControlLight;
            this.Splitter.Location = new System.Drawing.Point(225, 0);
            this.Splitter.Name = "Splitter";
            this.Splitter.Size = new System.Drawing.Size(5, 828);
            this.Splitter.TabIndex = 4;
            this.Splitter.TabStop = false;
            //
            //pnlDisplayArea
            //
            this.pnlDisplayArea.Controls.Add(this.pnlHost);
            this.pnlDisplayArea.Controls.Add(this.UIPanel);
            this.pnlDisplayArea.Dock = System.Windows.Forms.DockStyle.Fill;
            this.pnlDisplayArea.Location = new System.Drawing.Point(230, 0);
            this.pnlDisplayArea.Name = "pnlDisplayArea";
            this.pnlDisplayArea.Size = new System.Drawing.Size(790, 828);
            this.pnlDisplayArea.TabIndex = 7;
            //
            //pnlHost
            //
            this.pnlHost.BackColor = System.Drawing.SystemColors.Window;
            this.pnlHost.Controls.Add(this.Panel1);
            this.pnlHost.Controls.Add(this.Splitter1);
            this.pnlHost.Controls.Add(this.pnlPlaceHolder);
            this.pnlHost.Controls.Add(this.Title);
            this.pnlHost.Dock = System.Windows.Forms.DockStyle.Left;
            this.pnlHost.Location = new System.Drawing.Point(0, 0);
            this.pnlHost.Name = "pnlHost";
            this.pnlHost.Size = new System.Drawing.Size(451, 828);
            this.pnlHost.TabIndex = 6;
            //
            //Panel1
            //
            this.Panel1.BackColor = System.Drawing.SystemColors.Window;
            this.Panel1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.Panel1.ForeColor = System.Drawing.SystemColors.ControlText;
            this.Panel1.Location = new System.Drawing.Point(204, 20);
            this.Panel1.Name = "Panel1";
            this.Panel1.Padding = new System.Windows.Forms.Padding(4);
            this.Panel1.Size = new System.Drawing.Size(247, 808);
            this.Panel1.TabIndex = 8;
            //
            //Splitter1
            //
            this.Splitter1.BackColor = System.Drawing.SystemColors.ControlLight;
            this.Splitter1.Location = new System.Drawing.Point(199, 20);
            this.Splitter1.Name = "Splitter1";
            this.Splitter1.Size = new System.Drawing.Size(5, 808);
            this.Splitter1.TabIndex = 5;
            this.Splitter1.TabStop = false;
            //
            //pnlPlaceHolder
            //
            this.pnlPlaceHolder.BackColor = System.Drawing.SystemColors.Window;
            this.pnlPlaceHolder.Controls.Add(this.FactorTree);
            this.pnlPlaceHolder.Dock = System.Windows.Forms.DockStyle.Left;
            this.pnlPlaceHolder.ForeColor = System.Drawing.SystemColors.ActiveCaptionText;
            this.pnlPlaceHolder.Location = new System.Drawing.Point(0, 20);
            this.pnlPlaceHolder.Name = "pnlPlaceHolder";
            this.pnlPlaceHolder.Padding = new System.Windows.Forms.Padding(4);
            this.pnlPlaceHolder.Size = new System.Drawing.Size(199, 808);
            this.pnlPlaceHolder.TabIndex = 9;
            //
            //Title
            //
            this.Title.BackColor = System.Drawing.SystemColors.ControlDarkDark;
            this.Title.Controls.Add(this.Label1);
            this.Title.Dock = System.Windows.Forms.DockStyle.Top;
            this.Title.ForeColor = System.Drawing.SystemColors.ControlText;
            this.Title.Location = new System.Drawing.Point(0, 0);
            this.Title.Name = "Title";
            this.Title.Size = new System.Drawing.Size(451, 20);
            this.Title.TabIndex = 6;
            //
            //Label1
            //
            this.Label1.AutoSize = true;
            this.Label1.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75f, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, System.Convert.ToByte(0));
            this.Label1.ForeColor = System.Drawing.SystemColors.ActiveCaptionText;
            this.Label1.Location = new System.Drawing.Point(4, 1);
            this.Label1.Name = "Label1";
            this.Label1.Size = new System.Drawing.Size(141, 16);
            this.Label1.TabIndex = 0;
            this.Label1.Text = "Factorial Configuration";
            //
            //UIPanel
            //
            this.UIPanel.BackColor = System.Drawing.SystemColors.Window;
            this.UIPanel.Location = new System.Drawing.Point(469, 3);
            this.UIPanel.Name = "UIPanel";
            this.UIPanel.Size = new System.Drawing.Size(302, 825);
            this.UIPanel.TabIndex = 5;
            //
            //Label2
            //
            this.Label2.AutoSize = true;
            this.Label2.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75f, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, System.Convert.ToByte(0));
            this.Label2.Location = new System.Drawing.Point(4, 1);
            this.Label2.Name = "Label2";
            this.Label2.Size = new System.Drawing.Size(141, 16);
            this.Label2.TabIndex = 0;
            this.Label2.Text = "Factorial Configuration";
            //
            //FactorTree
            //
            this.FactorTree.AllowDrop = true;
            this.FactorTree.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.FactorTree.Dock = System.Windows.Forms.DockStyle.Fill;
            this.FactorTree.HideSelection = false;
            this.FactorTree.Location = new System.Drawing.Point(4, 4);
            this.FactorTree.Name = "FactorTree";
            this.FactorTree.Size = new System.Drawing.Size(191, 800);
            this.FactorTree.TabIndex = 0;
            //
            //DataTree
            //
            this.DataTree.AllowDrop = true;
            this.DataTree.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.DataTree.Dock = System.Windows.Forms.DockStyle.Left;
            this.DataTree.Location = new System.Drawing.Point(0, 0);
            this.DataTree.Name = "DataTree";
            this.DataTree.Size = new System.Drawing.Size(225, 828);
            this.DataTree.TabIndex = 3;
            //
            //ExplorerUI
            //
            this.Controls.Add(this.pnlDisplayArea);
            this.Controls.Add(this.Splitter);
            this.Controls.Add(this.DataTree);
            this.Name = "ExplorerUI";
            this.Size = new System.Drawing.Size(1020, 828);
            this.pnlDisplayArea.ResumeLayout(false);
            this.pnlHost.ResumeLayout(false);
            this.pnlPlaceHolder.ResumeLayout(false);
            this.Title.ResumeLayout(false);
            this.Title.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Panel pnlHost;
        private System.Windows.Forms.Splitter Splitter1;
        private System.Windows.Forms.Panel Title;
        private System.Windows.Forms.Label Label1;
        private System.Windows.Forms.Panel Panel1;
        private System.Windows.Forms.Label Label2;
        private System.Windows.Forms.Panel pnlPlaceHolder;
        private System.Windows.Forms.Panel pnlDisplayArea;
        private System.Windows.Forms.Splitter Splitter;
        private System.Windows.Forms.Panel UIPanel;
        internal Controllers.DataTree DataTree;
        internal Controllers.FactorTree FactorTree;
    }
}

