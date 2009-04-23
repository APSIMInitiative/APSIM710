namespace GraphUserInterface
    {
    partial class ApsimReportDataForm
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
            this.ChartPage = new GraphUserInterface.ChartPageUI();
            this.label1 = new System.Windows.Forms.Label();
            this.SuspendLayout();
            // 
            // ChartPage
            // 
            this.ChartPage.AutoScroll = true;
            this.ChartPage.BackColor = System.Drawing.SystemColors.Window;
            this.ChartPage.Dock = System.Windows.Forms.DockStyle.Fill;
            this.ChartPage.HelpText = "";
            this.ChartPage.Location = new System.Drawing.Point(0, 23);
            this.ChartPage.Name = "ChartPage";
            this.ChartPage.Processor = null;
            this.ChartPage.Size = new System.Drawing.Size(679, 479);
            this.ChartPage.TabIndex = 0;
            // 
            // label1
            // 
            this.label1.BackColor = System.Drawing.SystemColors.Highlight;
            this.label1.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
            this.label1.Dock = System.Windows.Forms.DockStyle.Top;
            this.label1.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label1.ForeColor = System.Drawing.SystemColors.HighlightText;
            this.label1.Location = new System.Drawing.Point(0, 0);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(679, 23);
            this.label1.TabIndex = 1;
            this.label1.Text = "Right click on the page below to access a popup menu";
            // 
            // ApsimReportDataForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(679, 502);
            this.Controls.Add(this.ChartPage);
            this.Controls.Add(this.label1);
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "ApsimReportDataForm";
            this.ShowIcon = false;
            this.ShowInTaskbar = false;
            this.Text = "Data page";
            this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.OnFormClosing);
            this.ResumeLayout(false);

            }

        #endregion

        private ChartPageUI ChartPage;
        private System.Windows.Forms.Label label1;
        }
    }