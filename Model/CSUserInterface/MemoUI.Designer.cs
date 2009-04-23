namespace CSUserInterface
{
    partial class MemoUI
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
        this.components = new System.ComponentModel.Container();
        System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(MemoUI));
        this.TextBox = new System.Windows.Forms.TextBox();
        this.SuspendLayout();
        // 
        // MyHelpLabel
        // 
        this.MyHelpLabel.Size = new System.Drawing.Size(762, 16);
        // 
        // TextBox
        // 
        this.TextBox.BorderStyle = System.Windows.Forms.BorderStyle.None;
        this.TextBox.Dock = System.Windows.Forms.DockStyle.Fill;
        this.TextBox.Location = new System.Drawing.Point(0, 16);
        this.TextBox.Multiline = true;
        this.TextBox.Name = "TextBox";
        this.TextBox.Size = new System.Drawing.Size(762, 549);
        this.TextBox.TabIndex = 2;
        // 
        // MemoUI
        // 
        this.Controls.Add(this.TextBox);
        this.Name = "MemoUI";
        this.Size = new System.Drawing.Size(762, 565);
        this.Controls.SetChildIndex(this.MyHelpLabel, 0);
        this.Controls.SetChildIndex(this.TextBox, 0);
        this.ResumeLayout(false);
        this.PerformLayout();

        }

        #endregion

       private System.Windows.Forms.TextBox TextBox;

     }
}
