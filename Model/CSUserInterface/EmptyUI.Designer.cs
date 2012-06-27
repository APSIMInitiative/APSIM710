namespace CSUserInterface
{
    partial class EmptyUI
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(EmptyUI));
            this.Panel2 = new System.Windows.Forms.Panel();
            this.Panel1 = new System.Windows.Forms.Panel();
            this.GroupBox = new System.Windows.Forms.GroupBox();
            this.DocumentationLink = new System.Windows.Forms.LinkLabel();
            this.Label1 = new System.Windows.Forms.Label();
            this.MainLabel = new System.Windows.Forms.Label();
            this.PictureBox = new System.Windows.Forms.PictureBox();
            this.Panel2.SuspendLayout();
            this.Panel1.SuspendLayout();
            this.GroupBox.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)this.PictureBox).BeginInit();
            this.SuspendLayout();
            //
            //MyHelpLabel
            //
            this.MyHelpLabel.Size = new System.Drawing.Size(753, 16);
            //
            //Panel2
            //
            this.Panel2.Controls.Add(this.Panel1);
            this.Panel2.Controls.Add(this.PictureBox);
            this.Panel2.Dock = System.Windows.Forms.DockStyle.Fill;
            this.Panel2.Location = new System.Drawing.Point(0, 16);
            this.Panel2.Name = "Panel2";
            this.Panel2.Size = new System.Drawing.Size(753, 793);
            this.Panel2.TabIndex = 8;
            //
            //Panel1
            //
            this.Panel1.Controls.Add(this.GroupBox);
            this.Panel1.Controls.Add(this.Label1);
            this.Panel1.Controls.Add(this.MainLabel);
            this.Panel1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.Panel1.Location = new System.Drawing.Point(255, 0);
            this.Panel1.Name = "Panel1";
            this.Panel1.Size = new System.Drawing.Size(498, 793);
            this.Panel1.TabIndex = 7;
            //
            //GroupBox
            //
            this.GroupBox.Controls.Add(this.DocumentationLink);
            this.GroupBox.Location = new System.Drawing.Point(32, 211);
            this.GroupBox.Name = "GroupBox";
            this.GroupBox.Size = new System.Drawing.Size(330, 288);
            this.GroupBox.TabIndex = 9;
            this.GroupBox.TabStop = false;
            //
            //DocumentationLink
            //
            this.DocumentationLink.Location = new System.Drawing.Point(6, 25);
            this.DocumentationLink.Name = "DocumentationLink";
            this.DocumentationLink.Size = new System.Drawing.Size(208, 24);
            this.DocumentationLink.TabIndex = 8;
            this.DocumentationLink.TabStop = true;
            this.DocumentationLink.Text = "See Module Documentation for details.";
            //
            //Label1
            //
            this.Label1.Anchor = (System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left) | System.Windows.Forms.AnchorStyles.Right);
            this.Label1.Location = new System.Drawing.Point(16, 64);
            this.Label1.Name = "Label1";
            this.Label1.Size = new System.Drawing.Size(435, 88);
            this.Label1.TabIndex = 8;
            this.Label1.Text = "This component does not require extra user input. ";
            //
            //MainLabel
            //
            this.MainLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 14.25f, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, System.Convert.ToByte(0));
            this.MainLabel.Location = new System.Drawing.Point(16, 24);
            this.MainLabel.Name = "MainLabel";
            this.MainLabel.Size = new System.Drawing.Size(208, 22);
            this.MainLabel.TabIndex = 6;
            this.MainLabel.Text = "Crop type";
            //
            //PictureBox
            //
            this.PictureBox.Dock = System.Windows.Forms.DockStyle.Left;
            this.PictureBox.Image = (System.Drawing.Image)resources.GetObject("PictureBox.Image");
            this.PictureBox.Location = new System.Drawing.Point(0, 0);
            this.PictureBox.Name = "PictureBox";
            this.PictureBox.Size = new System.Drawing.Size(255, 793);
            this.PictureBox.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize;
            this.PictureBox.TabIndex = 1;
            this.PictureBox.TabStop = false;
            //
            //EmptyUI
            //
            this.Controls.Add(this.Panel2);
            this.Name = "EmptyUI";
            this.Size = new System.Drawing.Size(753, 809);
            this.Controls.SetChildIndex(this.MyHelpLabel, 0);
            this.Controls.SetChildIndex(this.Panel2, 0);
            this.Panel2.ResumeLayout(false);
            this.Panel2.PerformLayout();
            this.Panel1.ResumeLayout(false);
            this.GroupBox.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)this.PictureBox).EndInit();
            this.ResumeLayout(false);

        }

        #endregion

		private System.Windows.Forms.Panel Panel2;
		private System.Windows.Forms.PictureBox PictureBox;
		private System.Windows.Forms.Panel Panel1;
		private System.Windows.Forms.Label Label1;
		private System.Windows.Forms.GroupBox GroupBox;
		private System.Windows.Forms.LinkLabel DocumentationLink;
		private System.Windows.Forms.Label MainLabel;

    }
}
