namespace GraphUserInterface
    {
    partial class SplitScreenPageUI
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
            this.Page1 = new GraphUserInterface.ChartPageUI();
            this.splitter1 = new System.Windows.Forms.Splitter();
            this.Page2 = new GraphUserInterface.ChartPageUI();
            this.SuspendLayout();
            // 
            // Page1
            // 
            this.Page1.Dock = System.Windows.Forms.DockStyle.Top;
            this.Page1.Location = new System.Drawing.Point(0, 31);
            this.Page1.Name = "Page1";
            this.Page1.Processor = null;
            this.Page1.Size = new System.Drawing.Size(655, 191);
            this.Page1.TabIndex = 3;
            // 
            // splitter1
            // 
            this.splitter1.Dock = System.Windows.Forms.DockStyle.Top;
            this.splitter1.Location = new System.Drawing.Point(0, 222);
            this.splitter1.Name = "splitter1";
            this.splitter1.Size = new System.Drawing.Size(655, 3);
            this.splitter1.TabIndex = 4;
            this.splitter1.TabStop = false;
            // 
            // Page2
            // 
            this.Page2.Dock = System.Windows.Forms.DockStyle.Fill;
            this.Page2.Location = new System.Drawing.Point(0, 225);
            this.Page2.Name = "Page2";
            this.Page2.Processor = null;
            this.Page2.Size = new System.Drawing.Size(655, 316);
            this.Page2.TabIndex = 5;
            // 
            // SplitScreenPageUI
            // 
            this.Controls.Add(this.Page2);
            this.Controls.Add(this.splitter1);
            this.Controls.Add(this.Page1);
            this.Name = "SplitScreenPageUI";
            this.Controls.SetChildIndex(this.Page1, 0);
            this.Controls.SetChildIndex(this.splitter1, 0);
            this.Controls.SetChildIndex(this.Page2, 0);
            this.ResumeLayout(false);
            this.PerformLayout();

            }

        #endregion

        private ChartPageUI Page1;
        private System.Windows.Forms.Splitter splitter1;
        private ChartPageUI Page2;
        }
    }
