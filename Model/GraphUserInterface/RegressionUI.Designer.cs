namespace GraphUserInterface
    {
    partial class RegressionUI
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
            this.label2 = new System.Windows.Forms.Label();
            this.label1 = new System.Windows.Forms.Label();
            this.YDropDown = new System.Windows.Forms.ComboBox();
            this.XDropDown = new System.Windows.Forms.ComboBox();
            this.GroupBox.SuspendLayout();
            this.SuspendLayout();
            // 
            // GroupBox
            // 
            this.GroupBox.Controls.Add(this.label2);
            this.GroupBox.Controls.Add(this.label1);
            this.GroupBox.Controls.Add(this.YDropDown);
            this.GroupBox.Controls.Add(this.XDropDown);
            this.GroupBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.GroupBox.Location = new System.Drawing.Point(0, 18);
            this.GroupBox.Name = "GroupBox";
            this.GroupBox.Size = new System.Drawing.Size(100, 123);
            this.GroupBox.TabIndex = 2;
            this.GroupBox.TabStop = false;
            this.GroupBox.Text = "GroupBox";
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(6, 74);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(14, 13);
            this.label2.TabIndex = 3;
            this.label2.Text = "Y";
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(6, 25);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(14, 13);
            this.label1.TabIndex = 2;
            this.label1.Text = "X";
            // 
            // YDropDown
            // 
            this.YDropDown.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.YDropDown.FormattingEnabled = true;
            this.YDropDown.Location = new System.Drawing.Point(9, 90);
            this.YDropDown.Name = "YDropDown";
            this.YDropDown.Size = new System.Drawing.Size(85, 21);
            this.YDropDown.TabIndex = 1;
            // 
            // XDropDown
            // 
            this.XDropDown.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.XDropDown.FormattingEnabled = true;
            this.XDropDown.Location = new System.Drawing.Point(9, 41);
            this.XDropDown.Name = "XDropDown";
            this.XDropDown.Size = new System.Drawing.Size(85, 21);
            this.XDropDown.TabIndex = 0;
            // 
            // RegressionUI
            // 
            this.Controls.Add(this.GroupBox);
            this.Name = "RegressionUI";
            this.Size = new System.Drawing.Size(100, 141);
            this.Controls.SetChildIndex(this.GroupBox, 0);
            this.GroupBox.ResumeLayout(false);
            this.GroupBox.PerformLayout();
            this.ResumeLayout(false);

            }

        #endregion

        private System.Windows.Forms.GroupBox GroupBox;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.ComboBox YDropDown;
        private System.Windows.Forms.ComboBox XDropDown;
        }
    }
