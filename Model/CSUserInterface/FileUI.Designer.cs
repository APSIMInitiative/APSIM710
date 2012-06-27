namespace CSUserInterface
{
    partial class FileUI
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(FileUI));
            this.ImageList = new System.Windows.Forms.ImageList(this.components);
            this.FileContentsBox = new System.Windows.Forms.RichTextBox();
            this.OpenFileDialog = new System.Windows.Forms.OpenFileDialog();
            this.BrowseButton = new System.Windows.Forms.ToolStripButton();
            this.ToolStripLabel1 = new System.Windows.Forms.ToolStripLabel();
            this.SearchTextBox = new System.Windows.Forms.ToolStripTextBox();
            this.SearchButton = new System.Windows.Forms.ToolStripButton();
            this.ToolStrip1 = new System.Windows.Forms.ToolStrip();
            this.GotoErrorButton = new System.Windows.Forms.ToolStripButton();
            this.GotoWarningButton = new System.Windows.Forms.ToolStripButton();
            this.Timer = new System.Windows.Forms.Timer(this.components);
            this.ToolStrip1.SuspendLayout();
            this.SuspendLayout();
            // 
            // MyHelpLabel
            // 
            this.MyHelpLabel.Size = new System.Drawing.Size(794, 16);
            // 
            // ImageList
            // 
            this.ImageList.ImageStream = ((System.Windows.Forms.ImageListStreamer)(resources.GetObject("ImageList.ImageStream")));
            this.ImageList.TransparentColor = System.Drawing.Color.Transparent;
            this.ImageList.Images.SetKeyName(0, "");
            this.ImageList.Images.SetKeyName(1, "");
            this.ImageList.Images.SetKeyName(2, "");
            this.ImageList.Images.SetKeyName(3, "");
            // 
            // FileContentsBox
            // 
            this.FileContentsBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.FileContentsBox.Font = new System.Drawing.Font("Courier New", 9F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.FileContentsBox.Location = new System.Drawing.Point(0, 41);
            this.FileContentsBox.Name = "FileContentsBox";
            this.FileContentsBox.ReadOnly = true;
            this.FileContentsBox.Size = new System.Drawing.Size(794, 427);
            this.FileContentsBox.TabIndex = 3;
            this.FileContentsBox.Text = "";
            this.FileContentsBox.WordWrap = false;
            this.FileContentsBox.KeyDown += new System.Windows.Forms.KeyEventHandler(this.FileContentsBox_KeyDown);
            // 
            // OpenFileDialog
            // 
            this.OpenFileDialog.Filter = "All files|*.*";
            this.OpenFileDialog.RestoreDirectory = true;
            // 
            // BrowseButton
            // 
            this.BrowseButton.Image = ((System.Drawing.Image)(resources.GetObject("BrowseButton.Image")));
            this.BrowseButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.BrowseButton.Name = "BrowseButton";
            this.BrowseButton.Size = new System.Drawing.Size(62, 22);
            this.BrowseButton.Text = "Browse";
            this.BrowseButton.ToolTipText = "Browse for files";
            this.BrowseButton.Click += new System.EventHandler(this.OnBrowseClick);
            // 
            // ToolStripLabel1
            // 
            this.ToolStripLabel1.Name = "ToolStripLabel1";
            this.ToolStripLabel1.Size = new System.Drawing.Size(44, 22);
            this.ToolStripLabel1.Text = "Search:";
            // 
            // SearchTextBox
            // 
            this.SearchTextBox.Name = "SearchTextBox";
            this.SearchTextBox.Size = new System.Drawing.Size(100, 25);
            this.SearchTextBox.KeyDown += new System.Windows.Forms.KeyEventHandler(this.OnSearchBoxKeyDown);
            // 
            // SearchButton
            // 
            this.SearchButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.SearchButton.Image = ((System.Drawing.Image)(resources.GetObject("SearchButton.Image")));
            this.SearchButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.SearchButton.Name = "SearchButton";
            this.SearchButton.Size = new System.Drawing.Size(23, 22);
            this.SearchButton.Text = "ToolStripButton1";
            this.SearchButton.ToolTipText = "Search the file for this text";
            this.SearchButton.Click += new System.EventHandler(this.SearchButton_Click);
            // 
            // ToolStrip1
            // 
            this.ToolStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.BrowseButton,
            this.ToolStripLabel1,
            this.SearchTextBox,
            this.SearchButton,
            this.GotoErrorButton,
            this.GotoWarningButton});
            this.ToolStrip1.Location = new System.Drawing.Point(0, 16);
            this.ToolStrip1.Name = "ToolStrip1";
            this.ToolStrip1.Size = new System.Drawing.Size(794, 25);
            this.ToolStrip1.TabIndex = 16;
            this.ToolStrip1.Text = "ToolStrip1";
            // 
            // GotoErrorButton
            // 
            this.GotoErrorButton.Image = ((System.Drawing.Image)(resources.GetObject("GotoErrorButton.Image")));
            this.GotoErrorButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.GotoErrorButton.Name = "GotoErrorButton";
            this.GotoErrorButton.Size = new System.Drawing.Size(102, 22);
            this.GotoErrorButton.Text = "Goto next error";
            this.GotoErrorButton.Click += new System.EventHandler(this.OnGotoErrorClick);
            // 
            // GotoWarningButton
            // 
            this.GotoWarningButton.Image = ((System.Drawing.Image)(resources.GetObject("GotoWarningButton.Image")));
            this.GotoWarningButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.GotoWarningButton.Name = "GotoWarningButton";
            this.GotoWarningButton.Size = new System.Drawing.Size(116, 22);
            this.GotoWarningButton.Text = "Goto next warning";
            this.GotoWarningButton.Click += new System.EventHandler(this.OnGotoWarningClick);
            // 
            // Timer
            // 
            this.Timer.Interval = 1000;
            this.Timer.Tick += new System.EventHandler(this.OnTimerTick);
            // 
            // FileUI
            // 
            this.Controls.Add(this.FileContentsBox);
            this.Controls.Add(this.ToolStrip1);
            this.Name = "FileUI";
            this.Size = new System.Drawing.Size(794, 468);
            this.Controls.SetChildIndex(this.MyHelpLabel, 0);
            this.Controls.SetChildIndex(this.ToolStrip1, 0);
            this.Controls.SetChildIndex(this.FileContentsBox, 0);
            this.ToolStrip1.ResumeLayout(false);
            this.ToolStrip1.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.ToolStripButton BrowseButton;
        private System.Windows.Forms.ToolStripLabel ToolStripLabel1;
        private System.Windows.Forms.ToolStripTextBox SearchTextBox;
        private System.Windows.Forms.ToolStripButton SearchButton;
        private System.Windows.Forms.ToolStrip ToolStrip1;
        private System.Windows.Forms.ToolStripButton GotoErrorButton;
        private System.Windows.Forms.ToolStripButton GotoWarningButton;
        private System.Windows.Forms.Timer Timer;
		private System.Windows.Forms.OpenFileDialog OpenFileDialog;
		private System.Windows.Forms.ImageList ImageList;
		private System.Windows.Forms.RichTextBox FileContentsBox;


    }
}
