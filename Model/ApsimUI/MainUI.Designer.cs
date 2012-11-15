namespace APSIMUI
{
    partial class MainUI
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(MainUI));
            this.RunButton = new System.Windows.Forms.ToolStripButton();
            this.ToolBoxPanelToolBar = new System.Windows.Forms.ToolBar();
            this.ToolboxSplitter = new System.Windows.Forms.Splitter();
            this.SimulationToolStrip = new System.Windows.Forms.ToolStrip();
            this.RunToolStrip = new System.Windows.Forms.ToolStrip();
            this.StopButton = new System.Windows.Forms.ToolStripButton();
            this.RunProgress = new System.Windows.Forms.ToolStripProgressBar();
            this.PercentLabel = new System.Windows.Forms.ToolStripLabel();
            this.ErrorsButton = new System.Windows.Forms.ToolStripButton();
            this.ContextMenuStrip1 = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.ToolStripMenuItem1 = new System.Windows.Forms.ToolStripMenuItem();
            this.ProgressLabel = new System.Windows.Forms.ToolStripStatusLabel();
            this.ProgressBar = new System.Windows.Forms.ToolStripProgressBar();
            this.ToolboxClose = new System.Windows.Forms.Button();
            this.Label6 = new System.Windows.Forms.Label();
            this.ToolBoxToolBarPanel = new System.Windows.Forms.Panel();
            this.Label5 = new System.Windows.Forms.Label();
            this.ToolboxButtonClose = new System.Windows.Forms.Button();
            this.ToolBoxPanel = new System.Windows.Forms.Panel();
            this.ToolBoxesToolStrip = new System.Windows.Forms.ToolStrip();
            this.StatusStrip1 = new System.Windows.Forms.StatusStrip();
            this.SimulationContainer = new System.Windows.Forms.ToolStripContainer();
            this.SimulationExplorer = new Controllers.ExplorerUI();
            this.RunToolStrip.SuspendLayout();
            this.ContextMenuStrip1.SuspendLayout();
            this.ToolBoxToolBarPanel.SuspendLayout();
            this.ToolBoxPanel.SuspendLayout();
            this.StatusStrip1.SuspendLayout();
            this.SimulationContainer.BottomToolStripPanel.SuspendLayout();
            this.SimulationContainer.ContentPanel.SuspendLayout();
            this.SimulationContainer.TopToolStripPanel.SuspendLayout();
            this.SimulationContainer.SuspendLayout();
            this.SuspendLayout();
            // 
            // RunButton
            // 
            this.RunButton.Image = ((System.Drawing.Image)(resources.GetObject("RunButton.Image")));
            this.RunButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None;
            this.RunButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.RunButton.Name = "RunButton";
            this.RunButton.Size = new System.Drawing.Size(30, 41);
            this.RunButton.Text = "Run";
            this.RunButton.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText;
            this.RunButton.ToolTipText = "Run APSIM";
            this.RunButton.Click += new System.EventHandler(this.OnRunButtonClick);
            // 
            // ToolBoxPanelToolBar
            // 
            this.ToolBoxPanelToolBar.Appearance = System.Windows.Forms.ToolBarAppearance.Flat;
            this.ToolBoxPanelToolBar.Divider = false;
            this.ToolBoxPanelToolBar.Dock = System.Windows.Forms.DockStyle.Fill;
            this.ToolBoxPanelToolBar.DropDownArrows = true;
            this.ToolBoxPanelToolBar.Location = new System.Drawing.Point(0, 0);
            this.ToolBoxPanelToolBar.Name = "ToolBoxPanelToolBar";
            this.ToolBoxPanelToolBar.ShowToolTips = true;
            this.ToolBoxPanelToolBar.Size = new System.Drawing.Size(754, 26);
            this.ToolBoxPanelToolBar.TabIndex = 17;
            this.ToolBoxPanelToolBar.TextAlign = System.Windows.Forms.ToolBarTextAlign.Right;
            // 
            // ToolboxSplitter
            // 
            this.ToolboxSplitter.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.ToolboxSplitter.Location = new System.Drawing.Point(0, 338);
            this.ToolboxSplitter.Name = "ToolboxSplitter";
            this.ToolboxSplitter.Size = new System.Drawing.Size(754, 3);
            this.ToolboxSplitter.TabIndex = 25;
            this.ToolboxSplitter.TabStop = false;
            this.ToolboxSplitter.Visible = false;
            // 
            // SimulationToolStrip
            // 
            this.SimulationToolStrip.Dock = System.Windows.Forms.DockStyle.None;
            this.SimulationToolStrip.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden;
            this.SimulationToolStrip.Location = new System.Drawing.Point(3, 0);
            this.SimulationToolStrip.MinimumSize = new System.Drawing.Size(100, 44);
            this.SimulationToolStrip.Name = "SimulationToolStrip";
            this.SimulationToolStrip.Size = new System.Drawing.Size(102, 44);
            this.SimulationToolStrip.TabIndex = 1;
            // 
            // RunToolStrip
            // 
            this.RunToolStrip.Dock = System.Windows.Forms.DockStyle.None;
            this.RunToolStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.RunButton,
            this.StopButton,
            this.RunProgress,
            this.PercentLabel,
            this.ErrorsButton});
            this.RunToolStrip.Location = new System.Drawing.Point(105, 0);
            this.RunToolStrip.Name = "RunToolStrip";
            this.RunToolStrip.Size = new System.Drawing.Size(243, 44);
            this.RunToolStrip.TabIndex = 2;
            // 
            // StopButton
            // 
            this.StopButton.Enabled = false;
            this.StopButton.Image = ((System.Drawing.Image)(resources.GetObject("StopButton.Image")));
            this.StopButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None;
            this.StopButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.StopButton.Name = "StopButton";
            this.StopButton.Size = new System.Drawing.Size(33, 41);
            this.StopButton.Text = "Stop";
            this.StopButton.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText;
            this.StopButton.ToolTipText = "Stop APSIM";
            this.StopButton.Click += new System.EventHandler(this.OnStopClick);
            // 
            // RunProgress
            // 
            this.RunProgress.AutoSize = false;
            this.RunProgress.Name = "RunProgress";
            this.RunProgress.Size = new System.Drawing.Size(100, 24);
            this.RunProgress.Step = 1;
            this.RunProgress.Style = System.Windows.Forms.ProgressBarStyle.Continuous;
            // 
            // PercentLabel
            // 
            this.PercentLabel.Name = "PercentLabel";
            this.PercentLabel.Size = new System.Drawing.Size(0, 41);
            // 
            // ErrorsButton
            // 
            this.ErrorsButton.Image = ((System.Drawing.Image)(resources.GetObject("ErrorsButton.Image")));
            this.ErrorsButton.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None;
            this.ErrorsButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.ErrorsButton.Name = "ErrorsButton";
            this.ErrorsButton.Size = new System.Drawing.Size(71, 41);
            this.ErrorsButton.Text = "Errors found";
            this.ErrorsButton.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText;
            this.ErrorsButton.Visible = false;
            this.ErrorsButton.Click += new System.EventHandler(this.OnErrorsClick);
            // 
            // ContextMenuStrip1
            // 
            this.ContextMenuStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.ToolStripMenuItem1});
            this.ContextMenuStrip1.Name = "ContextMenuStrip1";
            this.ContextMenuStrip1.Size = new System.Drawing.Size(184, 26);
            // 
            // ToolStripMenuItem1
            // 
            this.ToolStripMenuItem1.Name = "ToolStripMenuItem1";
            this.ToolStripMenuItem1.Size = new System.Drawing.Size(183, 22);
            this.ToolStripMenuItem1.Text = "ToolStripMenuItem1";
            // 
            // ProgressLabel
            // 
            this.ProgressLabel.Name = "ProgressLabel";
            this.ProgressLabel.Size = new System.Drawing.Size(0, 17);
            // 
            // ProgressBar
            // 
            this.ProgressBar.Name = "ProgressBar";
            this.ProgressBar.Size = new System.Drawing.Size(100, 16);
            // 
            // ToolboxClose
            // 
            this.ToolboxClose.Dock = System.Windows.Forms.DockStyle.Right;
            this.ToolboxClose.Location = new System.Drawing.Point(731, 0);
            this.ToolboxClose.Name = "ToolboxClose";
            this.ToolboxClose.Size = new System.Drawing.Size(23, 24);
            this.ToolboxClose.TabIndex = 23;
            this.ToolboxClose.Text = "X";
            this.ToolboxClose.UseVisualStyleBackColor = true;
            this.ToolboxClose.Click += new System.EventHandler(this.HideToolBoxWindow);
            // 
            // Label6
            // 
            this.Label6.AutoSize = true;
            this.Label6.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.Label6.ForeColor = System.Drawing.SystemColors.HighlightText;
            this.Label6.Location = new System.Drawing.Point(3, 3);
            this.Label6.Name = "Label6";
            this.Label6.Size = new System.Drawing.Size(58, 16);
            this.Label6.TabIndex = 22;
            this.Label6.Text = "Toolbox";
            // 
            // ToolBoxToolBarPanel
            // 
            this.ToolBoxToolBarPanel.BackColor = System.Drawing.SystemColors.Highlight;
            this.ToolBoxToolBarPanel.Controls.Add(this.ToolboxClose);
            this.ToolBoxToolBarPanel.Controls.Add(this.Label6);
            this.ToolBoxToolBarPanel.Controls.Add(this.Label5);
            this.ToolBoxToolBarPanel.Controls.Add(this.ToolboxButtonClose);
            this.ToolBoxToolBarPanel.Controls.Add(this.ToolBoxPanelToolBar);
            this.ToolBoxToolBarPanel.Dock = System.Windows.Forms.DockStyle.Top;
            this.ToolBoxToolBarPanel.Location = new System.Drawing.Point(0, 0);
            this.ToolBoxToolBarPanel.Name = "ToolBoxToolBarPanel";
            this.ToolBoxToolBarPanel.Size = new System.Drawing.Size(754, 24);
            this.ToolBoxToolBarPanel.TabIndex = 19;
            // 
            // Label5
            // 
            this.Label5.AutoSize = true;
            this.Label5.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.Label5.ForeColor = System.Drawing.SystemColors.HotTrack;
            this.Label5.Location = new System.Drawing.Point(3, 7);
            this.Label5.Name = "Label5";
            this.Label5.Size = new System.Drawing.Size(58, 16);
            this.Label5.TabIndex = 22;
            this.Label5.Text = "Toolbox";
            // 
            // ToolboxButtonClose
            // 
            this.ToolboxButtonClose.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.ToolboxButtonClose.BackColor = System.Drawing.Color.Transparent;
            this.ToolboxButtonClose.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("ToolboxButtonClose.BackgroundImage")));
            this.ToolboxButtonClose.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.ToolboxButtonClose.Location = new System.Drawing.Point(1104, -1);
            this.ToolboxButtonClose.Name = "ToolboxButtonClose";
            this.ToolboxButtonClose.Size = new System.Drawing.Size(24, 24);
            this.ToolboxButtonClose.TabIndex = 20;
            this.ToolboxButtonClose.TabStop = false;
            this.ToolboxButtonClose.UseVisualStyleBackColor = false;
            // 
            // ToolBoxPanel
            // 
            this.ToolBoxPanel.Controls.Add(this.ToolBoxToolBarPanel);
            this.ToolBoxPanel.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.ToolBoxPanel.Location = new System.Drawing.Point(0, 341);
            this.ToolBoxPanel.Name = "ToolBoxPanel";
            this.ToolBoxPanel.Size = new System.Drawing.Size(754, 104);
            this.ToolBoxPanel.TabIndex = 12;
            this.ToolBoxPanel.Visible = false;
            // 
            // ToolBoxesToolStrip
            // 
            this.ToolBoxesToolStrip.Dock = System.Windows.Forms.DockStyle.None;
            this.ToolBoxesToolStrip.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden;
            this.ToolBoxesToolStrip.LayoutStyle = System.Windows.Forms.ToolStripLayoutStyle.HorizontalStackWithOverflow;
            this.ToolBoxesToolStrip.Location = new System.Drawing.Point(3, 0);
            this.ToolBoxesToolStrip.MinimumSize = new System.Drawing.Size(100, 44);
            this.ToolBoxesToolStrip.Name = "ToolBoxesToolStrip";
            this.ToolBoxesToolStrip.ShowItemToolTips = false;
            this.ToolBoxesToolStrip.Size = new System.Drawing.Size(100, 44);
            this.ToolBoxesToolStrip.TabIndex = 2;
            // 
            // StatusStrip1
            // 
            this.StatusStrip1.Dock = System.Windows.Forms.DockStyle.None;
            this.StatusStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.ProgressBar,
            this.ProgressLabel});
            this.StatusStrip1.Location = new System.Drawing.Point(0, 0);
            this.StatusStrip1.Name = "StatusStrip1";
            this.StatusStrip1.Size = new System.Drawing.Size(119, 22);
            this.StatusStrip1.TabIndex = 3;
            this.StatusStrip1.Visible = false;
            // 
            // SimulationContainer
            // 
            // 
            // SimulationContainer.BottomToolStripPanel
            // 
            this.SimulationContainer.BottomToolStripPanel.BackColor = System.Drawing.SystemColors.ControlLight;
            this.SimulationContainer.BottomToolStripPanel.Controls.Add(this.StatusStrip1);
            this.SimulationContainer.BottomToolStripPanel.Controls.Add(this.ToolBoxesToolStrip);
            this.SimulationContainer.BottomToolStripPanel.MinimumSize = new System.Drawing.Size(100, 44);
            // 
            // SimulationContainer.ContentPanel
            // 
            this.SimulationContainer.ContentPanel.AutoScroll = true;
            this.SimulationContainer.ContentPanel.BackColor = System.Drawing.SystemColors.Window;
            this.SimulationContainer.ContentPanel.Controls.Add(this.SimulationExplorer);
            this.SimulationContainer.ContentPanel.Controls.Add(this.ToolboxSplitter);
            this.SimulationContainer.ContentPanel.Controls.Add(this.ToolBoxPanel);
            this.SimulationContainer.ContentPanel.Size = new System.Drawing.Size(754, 445);
            this.SimulationContainer.Dock = System.Windows.Forms.DockStyle.Fill;
            this.SimulationContainer.Location = new System.Drawing.Point(0, 0);
            this.SimulationContainer.Name = "SimulationContainer";
            this.SimulationContainer.Size = new System.Drawing.Size(754, 533);
            this.SimulationContainer.TabIndex = 5;
            this.SimulationContainer.Text = "ToolStripContainer1";
            // 
            // SimulationContainer.TopToolStripPanel
            // 
            this.SimulationContainer.TopToolStripPanel.BackColor = System.Drawing.SystemColors.ControlLight;
            this.SimulationContainer.TopToolStripPanel.Controls.Add(this.SimulationToolStrip);
            this.SimulationContainer.TopToolStripPanel.Controls.Add(this.RunToolStrip);
            this.SimulationContainer.TopToolStripPanel.MinimumSize = new System.Drawing.Size(100, 44);
            // 
            // SimulationExplorer
            // 
            this.SimulationExplorer.Dock = System.Windows.Forms.DockStyle.Fill;
            this.SimulationExplorer.Location = new System.Drawing.Point(0, 0);
            this.SimulationExplorer.Name = "SimulationExplorer";
            this.SimulationExplorer.Size = new System.Drawing.Size(754, 338);
            this.SimulationExplorer.TabIndex = 36;
            // 
            // MainUI
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(754, 533);
            this.Controls.Add(this.SimulationContainer);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "MainUI";
            this.StartPosition = System.Windows.Forms.FormStartPosition.Manual;
            this.Text = "MainUI";
            this.RunToolStrip.ResumeLayout(false);
            this.RunToolStrip.PerformLayout();
            this.ContextMenuStrip1.ResumeLayout(false);
            this.ToolBoxToolBarPanel.ResumeLayout(false);
            this.ToolBoxToolBarPanel.PerformLayout();
            this.ToolBoxPanel.ResumeLayout(false);
            this.StatusStrip1.ResumeLayout(false);
            this.StatusStrip1.PerformLayout();
            this.SimulationContainer.BottomToolStripPanel.ResumeLayout(false);
            this.SimulationContainer.BottomToolStripPanel.PerformLayout();
            this.SimulationContainer.ContentPanel.ResumeLayout(false);
            this.SimulationContainer.TopToolStripPanel.ResumeLayout(false);
            this.SimulationContainer.TopToolStripPanel.PerformLayout();
            this.SimulationContainer.ResumeLayout(false);
            this.SimulationContainer.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        internal System.Windows.Forms.ToolStripButton RunButton;
        internal System.Windows.Forms.ToolBar ToolBoxPanelToolBar;
        internal System.Windows.Forms.ToolStrip SimulationToolStrip;
        internal System.Windows.Forms.ToolStrip RunToolStrip;
        internal System.Windows.Forms.ToolStripButton StopButton;
        internal System.Windows.Forms.ToolStripProgressBar RunProgress;
        internal System.Windows.Forms.ToolStripLabel PercentLabel;
        internal System.Windows.Forms.ToolStripButton ErrorsButton;
        internal System.Windows.Forms.Splitter ToolboxSplitter;
        internal System.Windows.Forms.ContextMenuStrip ContextMenuStrip1;
        internal System.Windows.Forms.ToolStripMenuItem ToolStripMenuItem1;
        internal System.Windows.Forms.ToolStripStatusLabel ProgressLabel;
        internal System.Windows.Forms.ToolStripProgressBar ProgressBar;
        internal System.Windows.Forms.Button ToolboxClose;
        internal System.Windows.Forms.Label Label6;
        internal System.Windows.Forms.Panel ToolBoxToolBarPanel;
        internal System.Windows.Forms.Label Label5;
        internal System.Windows.Forms.Button ToolboxButtonClose;
        internal System.Windows.Forms.Panel ToolBoxPanel;
        internal System.Windows.Forms.ToolStrip ToolBoxesToolStrip;
        internal System.Windows.Forms.StatusStrip StatusStrip1;
        internal System.Windows.Forms.ToolStripContainer SimulationContainer;
        internal Controllers.ExplorerUI SimulationExplorer;
    }
}