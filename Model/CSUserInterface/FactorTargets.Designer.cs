namespace CSUserInterface
{
    partial class FactorTargets
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

        #region Component Designer generated code

        /// <summary> 
        /// Required method for Designer support - do not modify 
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.panel1 = new System.Windows.Forms.Panel();
            this.label1 = new System.Windows.Forms.Label();
            this.TargetList = new System.Windows.Forms.ListBox();
            this.panel1.SuspendLayout();
            this.SuspendLayout();
            // 
            // MyHelpLabel
            // 
            this.MyHelpLabel.Location = new System.Drawing.Point(0, 25);
            this.MyHelpLabel.Size = new System.Drawing.Size(207, 16);
            // 
            // panel1
            // 
            this.panel1.Controls.Add(this.label1);
            this.panel1.Dock = System.Windows.Forms.DockStyle.Top;
            this.panel1.Location = new System.Drawing.Point(0, 0);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(207, 25);
            this.panel1.TabIndex = 14;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(0, 0);
            this.label1.Margin = new System.Windows.Forms.Padding(0);
            this.label1.Name = "label1";
            this.label1.Padding = new System.Windows.Forms.Padding(4);
            this.label1.Size = new System.Drawing.Size(84, 21);
            this.label1.TabIndex = 14;
            this.label1.Text = "Factor Targets";
            // 
            // TargetList
            // 
            this.TargetList.AllowDrop = true;
            this.TargetList.Dock = System.Windows.Forms.DockStyle.Fill;
            this.TargetList.FormattingEnabled = true;
            this.TargetList.Location = new System.Drawing.Point(0, 25);
            this.TargetList.Name = "TargetList";
            this.TargetList.SelectionMode = System.Windows.Forms.SelectionMode.MultiExtended;
            this.TargetList.Size = new System.Drawing.Size(207, 212);
            this.TargetList.TabIndex = 15;
            this.TargetList.SelectedIndexChanged += new System.EventHandler(this.TargetList_SelectedIndexChanged);
            this.TargetList.DragDrop += new System.Windows.Forms.DragEventHandler(this.TargetList_DragDrop);
            this.TargetList.DragEnter += new System.Windows.Forms.DragEventHandler(this.TargetList_DragEnter);
            this.TargetList.KeyDown += new System.Windows.Forms.KeyEventHandler(this.TargetList_KeyDown);
            // 
            // FactorTargets
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.BackColor = System.Drawing.SystemColors.Window;
            this.Controls.Add(this.TargetList);
            this.Controls.Add(this.panel1);
            this.Margin = new System.Windows.Forms.Padding(0);
            this.Name = "FactorTargets";
            this.Size = new System.Drawing.Size(207, 239);
            this.Controls.SetChildIndex(this.panel1, 0);
            this.Controls.SetChildIndex(this.TargetList, 0);
            this.Controls.SetChildIndex(this.MyHelpLabel, 0);
            this.panel1.ResumeLayout(false);
            this.panel1.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.ListBox TargetList;
    }
}
