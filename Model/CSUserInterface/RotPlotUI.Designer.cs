namespace CSUserInterface
{
    partial class RotPlotUI
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
            this.splitContainer1 = new System.Windows.Forms.SplitContainer();
            this.panel1 = new System.Windows.Forms.Panel();
            this.GraphDisplay = new CSUserInterface.GraphDisplayObject();
            this.contextMenuStrip1 = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.splitContainer2 = new System.Windows.Forms.SplitContainer();
            this.pnlFlowLayout = new System.Windows.Forms.FlowLayoutPanel();
            this.panel5 = new System.Windows.Forms.Panel();
            this.label7 = new System.Windows.Forms.Label();
            this.label4 = new System.Windows.Forms.Label();
            this.pnlHints = new System.Windows.Forms.Panel();
            this.pnlNodeProperties = new System.Windows.Forms.Panel();
            this.lblInvalidName = new System.Windows.Forms.Label();
            this.lblColour = new System.Windows.Forms.Label();
            this.label1 = new System.Windows.Forms.Label();
            this.txtDesc = new System.Windows.Forms.TextBox();
            this.txtName = new System.Windows.Forms.TextBox();
            this.label5 = new System.Windows.Forms.Label();
            this.label6 = new System.Windows.Forms.Label();
            this.pnlArcProperties = new System.Windows.Forms.SplitContainer();
            this.txtRules = new System.Windows.Forms.TextBox();
            this.panel2 = new System.Windows.Forms.Panel();
            this.label2 = new System.Windows.Forms.Label();
            this.txtActions = new System.Windows.Forms.TextBox();
            this.panel4 = new System.Windows.Forms.Panel();
            this.label3 = new System.Windows.Forms.Label();
            this.lstHints = new System.Windows.Forms.ListBox();
            this.panel3 = new System.Windows.Forms.Panel();
            this.lblStatus = new System.Windows.Forms.Label();
            this.dlgColour = new System.Windows.Forms.ColorDialog();
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer1)).BeginInit();
            this.splitContainer1.Panel1.SuspendLayout();
            this.splitContainer1.Panel2.SuspendLayout();
            this.splitContainer1.SuspendLayout();
            this.panel1.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer2)).BeginInit();
            this.splitContainer2.Panel1.SuspendLayout();
            this.splitContainer2.Panel2.SuspendLayout();
            this.splitContainer2.SuspendLayout();
            this.panel5.SuspendLayout();
            this.pnlHints.SuspendLayout();
            this.pnlNodeProperties.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.pnlArcProperties)).BeginInit();
            this.pnlArcProperties.Panel1.SuspendLayout();
            this.pnlArcProperties.Panel2.SuspendLayout();
            this.pnlArcProperties.SuspendLayout();
            this.panel2.SuspendLayout();
            this.panel4.SuspendLayout();
            this.panel3.SuspendLayout();
            this.SuspendLayout();
            // 
            // MyHelpLabel
            // 
            this.MyHelpLabel.Text = "Rotation Setup";
            this.MyHelpLabel.Visible = true;
            // 
            // splitContainer1
            // 
            this.splitContainer1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.splitContainer1.Location = new System.Drawing.Point(0, 16);
            this.splitContainer1.Name = "splitContainer1";
            this.splitContainer1.Orientation = System.Windows.Forms.Orientation.Horizontal;
            // 
            // splitContainer1.Panel1
            // 
            this.splitContainer1.Panel1.Controls.Add(this.panel1);
            // 
            // splitContainer1.Panel2
            // 
            this.splitContainer1.Panel2.Controls.Add(this.splitContainer2);
            this.splitContainer1.Size = new System.Drawing.Size(655, 375);
            this.splitContainer1.SplitterDistance = 231;
            this.splitContainer1.TabIndex = 33;
            // 
            // panel1
            // 
            this.panel1.AutoScroll = true;
            this.panel1.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.panel1.Controls.Add(this.GraphDisplay);
            this.panel1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.panel1.Location = new System.Drawing.Point(0, 0);
            this.panel1.Margin = new System.Windows.Forms.Padding(10);
            this.panel1.Name = "panel1";
            this.panel1.Padding = new System.Windows.Forms.Padding(8);
            this.panel1.Size = new System.Drawing.Size(655, 231);
            this.panel1.TabIndex = 33;
            // 
            // GraphDisplay
            // 
            this.GraphDisplay.BackColor = System.Drawing.SystemColors.Window;
            this.GraphDisplay.ContextMenuStrip = this.contextMenuStrip1;
            this.GraphDisplay.Location = new System.Drawing.Point(2, -1);
            this.GraphDisplay.Margin = new System.Windows.Forms.Padding(10);
            this.GraphDisplay.Name = "GraphDisplay";
            this.GraphDisplay.SelectedObject = null;
            this.GraphDisplay.Size = new System.Drawing.Size(0, 0);
            this.GraphDisplay.TabIndex = 12;
            // 
            // contextMenuStrip1
            // 
            this.contextMenuStrip1.Name = "contextMenuStrip1";
            this.contextMenuStrip1.Size = new System.Drawing.Size(61, 4);
            // 
            // splitContainer2
            // 
            this.splitContainer2.Dock = System.Windows.Forms.DockStyle.Fill;
            this.splitContainer2.FixedPanel = System.Windows.Forms.FixedPanel.Panel1;
            this.splitContainer2.Location = new System.Drawing.Point(0, 0);
            this.splitContainer2.Name = "splitContainer2";
            // 
            // splitContainer2.Panel1
            // 
            this.splitContainer2.Panel1.Controls.Add(this.pnlFlowLayout);
            this.splitContainer2.Panel1.Controls.Add(this.panel5);
            // 
            // splitContainer2.Panel2
            // 
            this.splitContainer2.Panel2.Controls.Add(this.pnlHints);
            this.splitContainer2.Panel2.Controls.Add(this.panel3);
            this.splitContainer2.Size = new System.Drawing.Size(655, 140);
            this.splitContainer2.SplitterDistance = 269;
            this.splitContainer2.TabIndex = 15;
            // 
            // pnlFlowLayout
            // 
            this.pnlFlowLayout.AutoScroll = true;
            this.pnlFlowLayout.Dock = System.Windows.Forms.DockStyle.Fill;
            this.pnlFlowLayout.Location = new System.Drawing.Point(0, 19);
            this.pnlFlowLayout.Name = "pnlFlowLayout";
            this.pnlFlowLayout.Size = new System.Drawing.Size(269, 121);
            this.pnlFlowLayout.TabIndex = 6;
            // 
            // panel5
            // 
            this.panel5.BackColor = System.Drawing.SystemColors.Highlight;
            this.panel5.Controls.Add(this.label7);
            this.panel5.Controls.Add(this.label4);
            this.panel5.Dock = System.Windows.Forms.DockStyle.Top;
            this.panel5.Location = new System.Drawing.Point(0, 0);
            this.panel5.Name = "panel5";
            this.panel5.Size = new System.Drawing.Size(269, 19);
            this.panel5.TabIndex = 5;
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.ForeColor = System.Drawing.SystemColors.HighlightText;
            this.label7.Location = new System.Drawing.Point(99, 1);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(59, 13);
            this.label7.TabIndex = 14;
            this.label7.Text = "Initial State";
            this.label7.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.ForeColor = System.Drawing.SystemColors.HighlightText;
            this.label4.Location = new System.Drawing.Point(3, 1);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(50, 13);
            this.label4.TabIndex = 13;
            this.label4.Text = "Paddock";
            this.label4.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // pnlHints
            // 
            this.pnlHints.Controls.Add(this.pnlNodeProperties);
            this.pnlHints.Controls.Add(this.pnlArcProperties);
            this.pnlHints.Controls.Add(this.lstHints);
            this.pnlHints.Location = new System.Drawing.Point(0, 25);
            this.pnlHints.Name = "pnlHints";
            this.pnlHints.Padding = new System.Windows.Forms.Padding(3);
            this.pnlHints.Size = new System.Drawing.Size(407, 100);
            this.pnlHints.TabIndex = 0;
            // 
            // pnlNodeProperties
            // 
            this.pnlNodeProperties.Controls.Add(this.lblInvalidName);
            this.pnlNodeProperties.Controls.Add(this.lblColour);
            this.pnlNodeProperties.Controls.Add(this.label1);
            this.pnlNodeProperties.Controls.Add(this.txtDesc);
            this.pnlNodeProperties.Controls.Add(this.txtName);
            this.pnlNodeProperties.Controls.Add(this.label5);
            this.pnlNodeProperties.Controls.Add(this.label6);
            this.pnlNodeProperties.Location = new System.Drawing.Point(103, 6);
            this.pnlNodeProperties.Name = "pnlNodeProperties";
            this.pnlNodeProperties.Size = new System.Drawing.Size(219, 91);
            this.pnlNodeProperties.TabIndex = 18;
            this.pnlNodeProperties.Visible = false;
            // 
            // lblInvalidName
            // 
            this.lblInvalidName.AutoSize = true;
            this.lblInvalidName.ForeColor = System.Drawing.Color.Red;
            this.lblInvalidName.Location = new System.Drawing.Point(177, 13);
            this.lblInvalidName.Name = "lblInvalidName";
            this.lblInvalidName.Size = new System.Drawing.Size(38, 13);
            this.lblInvalidName.TabIndex = 17;
            this.lblInvalidName.Text = "Name:";
            this.lblInvalidName.Visible = false;
            // 
            // lblColour
            // 
            this.lblColour.BackColor = System.Drawing.SystemColors.Info;
            this.lblColour.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.lblColour.Location = new System.Drawing.Point(71, 64);
            this.lblColour.Name = "lblColour";
            this.lblColour.Size = new System.Drawing.Size(100, 19);
            this.lblColour.TabIndex = 16;
            this.lblColour.Click += new System.EventHandler(this.label2_Click);
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(8, 66);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(40, 13);
            this.label1.TabIndex = 15;
            this.label1.Text = "Colour:";
            // 
            // txtDesc
            // 
            this.txtDesc.Location = new System.Drawing.Point(71, 36);
            this.txtDesc.Name = "txtDesc";
            this.txtDesc.Size = new System.Drawing.Size(100, 20);
            this.txtDesc.TabIndex = 14;
            this.txtDesc.TextChanged += new System.EventHandler(this.txtDesc_TextChanged);
            // 
            // txtName
            // 
            this.txtName.Location = new System.Drawing.Point(71, 9);
            this.txtName.Name = "txtName";
            this.txtName.Size = new System.Drawing.Size(100, 20);
            this.txtName.TabIndex = 13;
            this.txtName.TextChanged += new System.EventHandler(this.txtName_TextChanged);
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.Location = new System.Drawing.Point(8, 39);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(63, 13);
            this.label5.TabIndex = 12;
            this.label5.Text = "Description:";
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Location = new System.Drawing.Point(8, 12);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(38, 13);
            this.label6.TabIndex = 11;
            this.label6.Text = "Name:";
            // 
            // pnlArcProperties
            // 
            this.pnlArcProperties.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.pnlArcProperties.Location = new System.Drawing.Point(327, 7);
            this.pnlArcProperties.Margin = new System.Windows.Forms.Padding(0);
            this.pnlArcProperties.Name = "pnlArcProperties";
            this.pnlArcProperties.Orientation = System.Windows.Forms.Orientation.Horizontal;
            // 
            // pnlArcProperties.Panel1
            // 
            this.pnlArcProperties.Panel1.Controls.Add(this.txtRules);
            this.pnlArcProperties.Panel1.Controls.Add(this.panel2);
            // 
            // pnlArcProperties.Panel2
            // 
            this.pnlArcProperties.Panel2.Controls.Add(this.txtActions);
            this.pnlArcProperties.Panel2.Controls.Add(this.panel4);
            this.pnlArcProperties.Panel2.Padding = new System.Windows.Forms.Padding(1);
            this.pnlArcProperties.Size = new System.Drawing.Size(88, 115);
            this.pnlArcProperties.SplitterDistance = 57;
            this.pnlArcProperties.TabIndex = 17;
            this.pnlArcProperties.Visible = false;
            // 
            // txtRules
            // 
            this.txtRules.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.txtRules.Dock = System.Windows.Forms.DockStyle.Fill;
            this.txtRules.Location = new System.Drawing.Point(0, 19);
            this.txtRules.Multiline = true;
            this.txtRules.Name = "txtRules";
            this.txtRules.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
            this.txtRules.Size = new System.Drawing.Size(86, 36);
            this.txtRules.TabIndex = 4;
            this.txtRules.WordWrap = false;
            this.txtRules.TextChanged += new System.EventHandler(this.txtRules_TextChanged);
            // 
            // panel2
            // 
            this.panel2.BackColor = System.Drawing.SystemColors.Control;
            this.panel2.ContextMenuStrip = this.contextMenuStrip1;
            this.panel2.Controls.Add(this.label2);
            this.panel2.Dock = System.Windows.Forms.DockStyle.Top;
            this.panel2.Location = new System.Drawing.Point(0, 0);
            this.panel2.Name = "panel2";
            this.panel2.Size = new System.Drawing.Size(86, 19);
            this.panel2.TabIndex = 3;
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Dock = System.Windows.Forms.DockStyle.Fill;
            this.label2.ForeColor = System.Drawing.SystemColors.ControlText;
            this.label2.Location = new System.Drawing.Point(0, 0);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(34, 13);
            this.label2.TabIndex = 13;
            this.label2.Text = "Rules";
            this.label2.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // txtActions
            // 
            this.txtActions.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.txtActions.Dock = System.Windows.Forms.DockStyle.Fill;
            this.txtActions.Location = new System.Drawing.Point(1, 20);
            this.txtActions.Multiline = true;
            this.txtActions.Name = "txtActions";
            this.txtActions.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
            this.txtActions.Size = new System.Drawing.Size(84, 31);
            this.txtActions.TabIndex = 5;
            this.txtActions.WordWrap = false;
            this.txtActions.TextChanged += new System.EventHandler(this.txtActions_TextChanged);
            // 
            // panel4
            // 
            this.panel4.BackColor = System.Drawing.SystemColors.Control;
            this.panel4.Controls.Add(this.label3);
            this.panel4.Dock = System.Windows.Forms.DockStyle.Top;
            this.panel4.Location = new System.Drawing.Point(1, 1);
            this.panel4.Name = "panel4";
            this.panel4.Size = new System.Drawing.Size(84, 19);
            this.panel4.TabIndex = 4;
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Dock = System.Windows.Forms.DockStyle.Fill;
            this.label3.ForeColor = System.Drawing.SystemColors.ControlText;
            this.label3.Location = new System.Drawing.Point(0, 0);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(42, 13);
            this.label3.TabIndex = 13;
            this.label3.Text = "Actions";
            this.label3.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // lstHints
            // 
            this.lstHints.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.lstHints.FormattingEnabled = true;
            this.lstHints.Items.AddRange(new object[] {
            "<left-click>: select a node or arc.",
            "<right-click>:  shows a context-sensitive  menu.",
            "",
            "Once a node/arc is selected, it can be dragged to a new position.",
            "",
            "Nodes are created by right-clicking on a blank area.",
            "",
            "Arcs are created by firstly selecting a source node, then right-",
            "clicking over a target node."});
            this.lstHints.Location = new System.Drawing.Point(6, 9);
            this.lstHints.Margin = new System.Windows.Forms.Padding(10);
            this.lstHints.Name = "lstHints";
            this.lstHints.SelectionMode = System.Windows.Forms.SelectionMode.None;
            this.lstHints.Size = new System.Drawing.Size(241, 39);
            this.lstHints.TabIndex = 15;
            // 
            // panel3
            // 
            this.panel3.BackColor = System.Drawing.SystemColors.Highlight;
            this.panel3.Controls.Add(this.lblStatus);
            this.panel3.Dock = System.Windows.Forms.DockStyle.Top;
            this.panel3.Location = new System.Drawing.Point(0, 0);
            this.panel3.Name = "panel3";
            this.panel3.Size = new System.Drawing.Size(382, 19);
            this.panel3.TabIndex = 3;
            // 
            // lblStatus
            // 
            this.lblStatus.AutoSize = true;
            this.lblStatus.ForeColor = System.Drawing.SystemColors.HighlightText;
            this.lblStatus.Location = new System.Drawing.Point(3, 1);
            this.lblStatus.Name = "lblStatus";
            this.lblStatus.Size = new System.Drawing.Size(92, 13);
            this.lblStatus.TabIndex = 13;
            this.lblStatus.Text = "No Item Selected.";
            this.lblStatus.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // RotPlotUI
            // 
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Inherit;
            this.Controls.Add(this.splitContainer1);
            this.HelpText = "Rotation Setup";
            this.Name = "RotPlotUI";
            this.Size = new System.Drawing.Size(655, 391);
            this.Controls.SetChildIndex(this.MyHelpLabel, 0);
            this.Controls.SetChildIndex(this.splitContainer1, 0);
            this.splitContainer1.Panel1.ResumeLayout(false);
            this.splitContainer1.Panel2.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer1)).EndInit();
            this.splitContainer1.ResumeLayout(false);
            this.panel1.ResumeLayout(false);
            this.splitContainer2.Panel1.ResumeLayout(false);
            this.splitContainer2.Panel2.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer2)).EndInit();
            this.splitContainer2.ResumeLayout(false);
            this.panel5.ResumeLayout(false);
            this.panel5.PerformLayout();
            this.pnlHints.ResumeLayout(false);
            this.pnlNodeProperties.ResumeLayout(false);
            this.pnlNodeProperties.PerformLayout();
            this.pnlArcProperties.Panel1.ResumeLayout(false);
            this.pnlArcProperties.Panel1.PerformLayout();
            this.pnlArcProperties.Panel2.ResumeLayout(false);
            this.pnlArcProperties.Panel2.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.pnlArcProperties)).EndInit();
            this.pnlArcProperties.ResumeLayout(false);
            this.panel2.ResumeLayout(false);
            this.panel2.PerformLayout();
            this.panel4.ResumeLayout(false);
            this.panel4.PerformLayout();
            this.panel3.ResumeLayout(false);
            this.panel3.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.SplitContainer splitContainer1;
        private System.Windows.Forms.Panel panel1;
        private GraphDisplayObject GraphDisplay;
        private System.Windows.Forms.ColorDialog dlgColour;
        private System.Windows.Forms.ContextMenuStrip contextMenuStrip1;
        private System.Windows.Forms.SplitContainer splitContainer2;
        private System.Windows.Forms.Panel panel3;
        private System.Windows.Forms.Label lblStatus;
        private System.Windows.Forms.Panel pnlHints;
        private System.Windows.Forms.Panel pnlNodeProperties;
        private System.Windows.Forms.Label lblInvalidName;
        private System.Windows.Forms.Label lblColour;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.TextBox txtDesc;
        private System.Windows.Forms.TextBox txtName;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.SplitContainer pnlArcProperties;
        private System.Windows.Forms.TextBox txtRules;
        private System.Windows.Forms.Panel panel2;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.TextBox txtActions;
        private System.Windows.Forms.Panel panel4;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.ListBox lstHints;
        private System.Windows.Forms.Panel panel5;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.FlowLayoutPanel pnlFlowLayout;
        private System.Windows.Forms.Label label7;
    }
}
