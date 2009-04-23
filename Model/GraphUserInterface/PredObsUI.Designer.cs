namespace GraphUserInterface
    {
    partial class PredObsUI
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
            this.GroupBox = new System.Windows.Forms.GroupBox();
            this.label1 = new System.Windows.Forms.Label();
            this.FieldList = new System.Windows.Forms.CheckedListBox();
            this.GroupBox.SuspendLayout();
            this.SuspendLayout();
            // 
            // GroupBox
            // 
            this.GroupBox.Controls.Add(this.label1);
            this.GroupBox.Controls.Add(this.FieldList);
            this.GroupBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.GroupBox.Location = new System.Drawing.Point(0, 18);
            this.GroupBox.Name = "GroupBox";
            this.GroupBox.Size = new System.Drawing.Size(185, 149);
            this.GroupBox.TabIndex = 2;
            this.GroupBox.TabStop = false;
            this.GroupBox.Text = "Predicted / Observed";
            // 
            // label1
            // 
            this.label1.Location = new System.Drawing.Point(6, 27);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(193, 31);
            this.label1.TabIndex = 3;
            this.label1.Text = "Select field names to use to link the observed data to the predicted data.";
            // 
            // FieldList
            // 
            this.FieldList.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.FieldList.CheckOnClick = true;
            this.FieldList.FormattingEnabled = true;
            this.FieldList.Location = new System.Drawing.Point(6, 61);
            this.FieldList.Name = "FieldList";
            this.FieldList.Size = new System.Drawing.Size(173, 79);
            this.FieldList.TabIndex = 2;
            this.FieldList.ItemCheck += new System.Windows.Forms.ItemCheckEventHandler(this.OnFieldListChanged);
            // 
            // PredObsUI
            // 
            this.BackColor = System.Drawing.SystemColors.Window;
            this.Controls.Add(this.GroupBox);
            this.Name = "PredObsUI";
            this.Size = new System.Drawing.Size(185, 167);
            this.Controls.SetChildIndex(this.GroupBox, 0);
            this.GroupBox.ResumeLayout(false);
            this.ResumeLayout(false);

            }

        #endregion

        private System.Windows.Forms.GroupBox GroupBox;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.CheckedListBox FieldList;

        }
    }
