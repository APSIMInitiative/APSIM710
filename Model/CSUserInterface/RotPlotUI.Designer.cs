namespace CSUserInterface
{
    partial class RotPlotUI
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
            this.txtFile = new System.Windows.Forms.TextBox();
            this.SuspendLayout();
            // 
            // txtFile
            // 
            this.txtFile.Location = new System.Drawing.Point(20, 19);
            this.txtFile.Name = "txtFile";
            this.txtFile.Size = new System.Drawing.Size(100, 20);
            this.txtFile.TabIndex = 2;
            // 
            // RotPlotUI
            // 
            this.Controls.Add(this.txtFile);
            this.Name = "RotPlotUI";
            this.Controls.SetChildIndex(this.txtFile, 0);
            this.Controls.SetChildIndex(this.MyHelpLabel, 0);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.TextBox txtFile;
    }
}
