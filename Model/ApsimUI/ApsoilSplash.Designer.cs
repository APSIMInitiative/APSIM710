namespace APSIMUI
{
    partial class ApsoilSplash
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(ApsoilSplash));
            this.OkButton = new System.Windows.Forms.Button();
            this.PictureBoxTop = new System.Windows.Forms.PictureBox();
            ((System.ComponentModel.ISupportInitialize)(this.PictureBoxTop)).BeginInit();
            this.SuspendLayout();
            // 
            // OkButton
            // 
            this.OkButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.OkButton.AutoSize = true;
            this.OkButton.DialogResult = System.Windows.Forms.DialogResult.OK;
            this.OkButton.Location = new System.Drawing.Point(545, 12);
            this.OkButton.Name = "OkButton";
            this.OkButton.Size = new System.Drawing.Size(75, 23);
            this.OkButton.TabIndex = 18;
            this.OkButton.Text = "Ok";
            this.OkButton.UseVisualStyleBackColor = true;
            // 
            // PictureBoxTop
            // 
            this.PictureBoxTop.BackColor = System.Drawing.Color.White;
            this.PictureBoxTop.Image = ((System.Drawing.Image)(resources.GetObject("PictureBoxTop.Image")));
            this.PictureBoxTop.Location = new System.Drawing.Point(1, 1);
            this.PictureBoxTop.Name = "PictureBoxTop";
            this.PictureBoxTop.Size = new System.Drawing.Size(631, 600);
            this.PictureBoxTop.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize;
            this.PictureBoxTop.TabIndex = 17;
            this.PictureBoxTop.TabStop = false;
            // 
            // ApsoilSplash
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(632, 602);
            this.Controls.Add(this.OkButton);
            this.Controls.Add(this.PictureBoxTop);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle;
            this.Name = "ApsoilSplash";
            this.Text = "ApsoilSplash";
            ((System.ComponentModel.ISupportInitialize)(this.PictureBoxTop)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        internal System.Windows.Forms.Button OkButton;
        internal System.Windows.Forms.PictureBox PictureBoxTop;
    }
}
