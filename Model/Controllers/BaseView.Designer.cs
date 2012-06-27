namespace Controllers
{
    partial class BaseView
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
            this.MyHelpLabel = new System.Windows.Forms.Label();
            this.SuspendLayout();
            //
            //MyHelpLabel
            //
            this.MyHelpLabel.BackColor = System.Drawing.SystemColors.ActiveCaption;
            this.MyHelpLabel.Dock = System.Windows.Forms.DockStyle.Top;
            this.MyHelpLabel.Font = new System.Drawing.Font("Tahoma", 9.75f, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, System.Convert.ToByte(0));
            this.MyHelpLabel.ForeColor = System.Drawing.SystemColors.ActiveCaptionText;
            this.MyHelpLabel.Location = new System.Drawing.Point(0, 0);
            this.MyHelpLabel.Name = "MyHelpLabel";
            this.MyHelpLabel.Size = new System.Drawing.Size(655, 40);
            this.MyHelpLabel.TabIndex = 1;
            this.MyHelpLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.MyHelpLabel.Visible = false;
            //
            //BaseView
            //
            this.AutoScroll = true;
            this.BackColor = System.Drawing.SystemColors.Window;
            this.Controls.Add(this.MyHelpLabel);
            this.Name = "BaseView";
            this.Size = new System.Drawing.Size(655, 541);
            this.ResumeLayout(false);

        }

        #endregion

        protected System.Windows.Forms.Label MyHelpLabel;

    }
}

