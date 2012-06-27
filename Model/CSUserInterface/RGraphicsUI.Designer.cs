namespace CSUserInterface
{
    partial class RGraphicsUI
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
            this.TabControl = new System.Windows.Forms.TabControl();
            this.ImagePage = new System.Windows.Forms.TabPage();
            this.ScriptPage = new System.Windows.Forms.TabPage();
            this.PictureBox = new System.Windows.Forms.PictureBox();
            this.Console = new System.Windows.Forms.TabPage();
            this.TabControl.SuspendLayout();
            this.ImagePage.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)this.PictureBox).BeginInit();
            this.SuspendLayout();
            //
            //MyHelpLabel
            //
            this.MyHelpLabel.Size = new System.Drawing.Size(1022, 16);
            //
            //TabControl
            //
            this.TabControl.Controls.Add(this.ImagePage);
            this.TabControl.Controls.Add(this.ScriptPage);
            this.TabControl.Controls.Add(this.Console);
            this.TabControl.Dock = System.Windows.Forms.DockStyle.Fill;
            this.TabControl.Location = new System.Drawing.Point(0, 16);
            this.TabControl.Name = "TabControl";
            this.TabControl.SelectedIndex = 0;
            this.TabControl.Size = new System.Drawing.Size(1022, 800);
            this.TabControl.TabIndex = 3;
            this.TabControl.Selected += onTabSelected;

            //
            //ImagePage
            //
            this.ImagePage.Controls.Add(this.PictureBox);
            this.ImagePage.Location = new System.Drawing.Point(4, 22);
            this.ImagePage.Name = "ImagePage";
            this.ImagePage.Size = new System.Drawing.Size(1014, 774);
            this.ImagePage.TabIndex = 0;
            this.ImagePage.Text = "Graph";
            this.ImagePage.UseVisualStyleBackColor = true;
            //
            //ScriptPage
            //
            this.ScriptPage.Location = new System.Drawing.Point(4, 22);
            this.ScriptPage.Name = "ScriptPage";
            this.ScriptPage.Size = new System.Drawing.Size(1014, 774);
            this.ScriptPage.TabIndex = 1;
            this.ScriptPage.Text = "Script";
            this.ScriptPage.UseVisualStyleBackColor = true;
            //
            //PictureBox
            //
            this.PictureBox.Dock = System.Windows.Forms.DockStyle.Left;
            this.PictureBox.Location = new System.Drawing.Point(0, 0);
            this.PictureBox.Name = "PictureBox";
            this.PictureBox.Size = new System.Drawing.Size(255, 774);
            this.PictureBox.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize;
            this.PictureBox.TabIndex = 2;
            this.PictureBox.TabStop = false;
            //
            //ConsoleBox
            //
            this.ConsoleBox = new System.Windows.Forms.TextBox();
            this.ConsoleBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.ConsoleBox.Font = new System.Drawing.Font("Courier New", 12);
            this.ConsoleBox.WordWrap = false;
            this.ConsoleBox.Multiline = true;
            this.ConsoleBox.ScrollBars = System.Windows.Forms.ScrollBars.Both;
            //
            //Console
            //
            this.Console.Location = new System.Drawing.Point(4, 22);
            this.Console.Name = "Console";
            this.Console.Padding = new System.Windows.Forms.Padding(3);
            this.Console.Size = new System.Drawing.Size(1014, 774);
            this.Console.TabIndex = 2;
            this.Console.Text = "Console";
            this.Console.UseVisualStyleBackColor = true;
            this.Console.Controls.Add(ConsoleBox);
            //
            //RGraphicsUI
            //
            this.Controls.Add(this.TabControl);
            this.Name = "RGraphicsUI";
            this.Size = new System.Drawing.Size(1022, 816);
            this.Controls.SetChildIndex(this.MyHelpLabel, 0);
            this.Controls.SetChildIndex(this.TabControl, 0);
            this.TabControl.ResumeLayout(false);
            this.ImagePage.ResumeLayout(false);
            this.ImagePage.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)this.PictureBox).EndInit();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.PictureBox PictureBox;
        private System.Windows.Forms.TabPage ScriptPage;
        private System.Windows.Forms.TabPage Console;
        private System.Windows.Forms.TextBox ConsoleBox;
        private System.Windows.Forms.TabControl TabControl;
        private System.Windows.Forms.TabPage ImagePage;
        private QWhale.Editor.SyntaxEdit ScriptBox;
    }
}
