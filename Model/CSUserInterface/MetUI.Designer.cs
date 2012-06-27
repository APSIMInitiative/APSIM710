namespace CSUserInterface
{
    partial class MetUI
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(MetUI));
            this.ImageList = new System.Windows.Forms.ImageList(this.components);
            this.OpenFileDialog = new System.Windows.Forms.OpenFileDialog();
            this.btnBrowse = new System.Windows.Forms.Button();
            this.MetGraphControl1 = new CSUserInterface.MetGraphControl();
            this.SuspendLayout();
            //
            //MyHelpLabel
            //
            this.MyHelpLabel.Size = new System.Drawing.Size(764, 16);
            //
            //ImageList
            //
            this.ImageList.ImageStream = (System.Windows.Forms.ImageListStreamer)resources.GetObject("ImageList.ImageStream");
            this.ImageList.TransparentColor = System.Drawing.Color.Transparent;
            this.ImageList.Images.SetKeyName(0, "");
            //
            //OpenFileDialog
            //
            this.OpenFileDialog.Filter = "met file (*.met)|*.met|All files(*.*)|*.*";
            this.OpenFileDialog.RestoreDirectory = true;
            //
            //btnBrowse
            //
            this.btnBrowse.BackColor = System.Drawing.SystemColors.Info;
            this.btnBrowse.FlatStyle = System.Windows.Forms.FlatStyle.Popup;
            this.btnBrowse.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnBrowse.ImageIndex = 0;
            this.btnBrowse.ImageList = this.ImageList;
            this.btnBrowse.Location = new System.Drawing.Point(0, 20);
            this.btnBrowse.Name = "btnBrowse";
            this.btnBrowse.Size = new System.Drawing.Size(88, 29);
            this.btnBrowse.TabIndex = 13;
            this.btnBrowse.Text = "Browse ...";
            this.btnBrowse.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnBrowse.UseVisualStyleBackColor = false;
            this.btnBrowse.Paint += btnBrowse_Paint;
            this.btnBrowse.Click += btnBrowse_Click;
            //
            //MetGraphControl1
            //
            this.MetGraphControl1.Anchor = (System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) | System.Windows.Forms.AnchorStyles.Left) | System.Windows.Forms.AnchorStyles.Right);
            this.MetGraphControl1.AutoScroll = true;
            this.MetGraphControl1.BackColor = System.Drawing.SystemColors.Control;
            this.MetGraphControl1.HelpText = "";
            this.MetGraphControl1.Location = new System.Drawing.Point(0, 50);
            this.MetGraphControl1.Name = "MetGraphControl1";
            this.MetGraphControl1.Size = new System.Drawing.Size(764, 481);
            this.MetGraphControl1.TabIndex = 14;
            //
            //MetUI
            //
            this.Controls.Add(this.btnBrowse);
            this.Controls.Add(this.MetGraphControl1);
            this.Name = "MetUI";
            this.Size = new System.Drawing.Size(764, 526);
            this.Controls.SetChildIndex(this.MyHelpLabel, 0);
            this.Controls.SetChildIndex(this.MetGraphControl1, 0);
            this.Controls.SetChildIndex(this.btnBrowse, 0);
            this.ResumeLayout(false);

        }

        #endregion

		private System.Windows.Forms.OpenFileDialog OpenFileDialog;
		private System.Windows.Forms.ImageList ImageList;
		private MetGraphControl MetGraphControl1;
		private System.Windows.Forms.Button btnBrowse;
    }
}

