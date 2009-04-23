namespace GraphUserInterface
    {
    partial class EmptyGraphUI
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
            this.GroupBox = new System.Windows.Forms.GroupBox();
            this.SuspendLayout();
            // 
            // GroupBox
            // 
            this.GroupBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.GroupBox.Location = new System.Drawing.Point(0, 0);
            this.GroupBox.Name = "GroupBox";
            this.GroupBox.Size = new System.Drawing.Size(114, 36);
            this.GroupBox.TabIndex = 2;
            this.GroupBox.TabStop = false;
            this.GroupBox.Text = "GroupBox";
            // 
            // EmptyGraphUI
            // 
            this.Controls.Add(this.GroupBox);
            this.Name = "EmptyGraphUI";
            this.Size = new System.Drawing.Size(114, 36);
            this.Controls.SetChildIndex(this.GroupBox, 0);
            this.ResumeLayout(false);

            }

        #endregion

        private System.Windows.Forms.GroupBox GroupBox;
        }
    }
