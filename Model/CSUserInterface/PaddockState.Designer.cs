namespace CSUserInterface
{
    partial class PaddockState
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
            this.cboState = new System.Windows.Forms.ComboBox();
            this.chkPaddock = new System.Windows.Forms.CheckBox();
            this.SuspendLayout();
            // 
            // cboState
            // 
            this.cboState.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.cboState.FormattingEnabled = true;
            this.cboState.Location = new System.Drawing.Point(97, 1);
            this.cboState.Name = "cboState";
            this.cboState.Size = new System.Drawing.Size(152, 21);
            this.cboState.TabIndex = 1;
            // 
            // chkPaddock
            // 
            this.chkPaddock.AutoSize = true;
            this.chkPaddock.Location = new System.Drawing.Point(11, 3);
            this.chkPaddock.Name = "chkPaddock";
            this.chkPaddock.Size = new System.Drawing.Size(80, 17);
            this.chkPaddock.TabIndex = 2;
            this.chkPaddock.Text = "checkBox1";
            this.chkPaddock.UseVisualStyleBackColor = true;
            // 
            // PaddockState
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.BackColor = System.Drawing.SystemColors.Window;
            this.Controls.Add(this.chkPaddock);
            this.Controls.Add(this.cboState);
            this.Name = "PaddockState";
            this.Size = new System.Drawing.Size(252, 23);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        public System.Windows.Forms.ComboBox cboState;
        public System.Windows.Forms.CheckBox chkPaddock;
    }
}
