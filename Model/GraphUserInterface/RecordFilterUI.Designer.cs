namespace GraphUserInterface
    {
    partial class RecordFilterUI
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
            this.LastRecordCheck = new System.Windows.Forms.CheckBox();
            this.RecordNumberEdit = new System.Windows.Forms.TextBox();
            this.FirstRecordCheck = new System.Windows.Forms.CheckBox();
            this.label1 = new System.Windows.Forms.Label();
            this.GroupBox.SuspendLayout();
            this.SuspendLayout();
            // 
            // GroupBox
            // 
            this.GroupBox.Controls.Add(this.LastRecordCheck);
            this.GroupBox.Controls.Add(this.RecordNumberEdit);
            this.GroupBox.Controls.Add(this.FirstRecordCheck);
            this.GroupBox.Controls.Add(this.label1);
            this.GroupBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.GroupBox.Location = new System.Drawing.Point(0, 18);
            this.GroupBox.Name = "GroupBox";
            this.GroupBox.Size = new System.Drawing.Size(98, 115);
            this.GroupBox.TabIndex = 6;
            this.GroupBox.TabStop = false;
            this.GroupBox.Text = "GroupBox";
            // 
            // LastRecordCheck
            // 
            this.LastRecordCheck.AutoSize = true;
            this.LastRecordCheck.Location = new System.Drawing.Point(6, 44);
            this.LastRecordCheck.Name = "LastRecordCheck";
            this.LastRecordCheck.Size = new System.Drawing.Size(79, 17);
            this.LastRecordCheck.TabIndex = 7;
            this.LastRecordCheck.Text = "Last record";
            this.LastRecordCheck.UseVisualStyleBackColor = true;
            // 
            // RecordNumberEdit
            // 
            this.RecordNumberEdit.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.RecordNumberEdit.Location = new System.Drawing.Point(6, 82);
            this.RecordNumberEdit.Name = "RecordNumberEdit";
            this.RecordNumberEdit.Size = new System.Drawing.Size(86, 20);
            this.RecordNumberEdit.TabIndex = 9;
            // 
            // FirstRecordCheck
            // 
            this.FirstRecordCheck.AutoSize = true;
            this.FirstRecordCheck.Location = new System.Drawing.Point(7, 21);
            this.FirstRecordCheck.Name = "FirstRecordCheck";
            this.FirstRecordCheck.Size = new System.Drawing.Size(78, 17);
            this.FirstRecordCheck.TabIndex = 6;
            this.FirstRecordCheck.Text = "First record";
            this.FirstRecordCheck.UseVisualStyleBackColor = true;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(3, 66);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(83, 13);
            this.label1.TabIndex = 8;
            this.label1.Text = "Record number:";
            // 
            // RecordFilterUI
            // 
            this.Controls.Add(this.GroupBox);
            this.Name = "RecordFilterUI";
            this.Size = new System.Drawing.Size(98, 133);
            this.Controls.SetChildIndex(this.GroupBox, 0);
            this.GroupBox.ResumeLayout(false);
            this.GroupBox.PerformLayout();
            this.ResumeLayout(false);

            }

        #endregion

        private System.Windows.Forms.GroupBox GroupBox;
        private System.Windows.Forms.CheckBox LastRecordCheck;
        private System.Windows.Forms.TextBox RecordNumberEdit;
        private System.Windows.Forms.CheckBox FirstRecordCheck;
        private System.Windows.Forms.Label label1;

        }
    }
