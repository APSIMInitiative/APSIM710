namespace CPIUserInterface
{
    partial class TreeGridUI
    {
        #region Component Designer generated code

        /// <summary> 
        /// Required method for Designer support - do not modify 
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            this.afTreeViewColumns1 = new CPIUserInterface.AFTreeViewColumns();
            this.button2 = new System.Windows.Forms.Button();
            this.btnEdit = new System.Windows.Forms.Button();
            this.toolTip1 = new System.Windows.Forms.ToolTip(this.components);
            this.SuspendLayout();
            // 
            // MyHelpLabel
            // 
            this.MyHelpLabel.Size = new System.Drawing.Size(655, 18);
            // 
            // afTreeViewColumns1
            // 
            this.afTreeViewColumns1.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.afTreeViewColumns1.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(165)))), ((int)(((byte)(172)))), ((int)(((byte)(178)))));
            this.afTreeViewColumns1.Location = new System.Drawing.Point(3, 19);
            this.afTreeViewColumns1.Name = "afTreeViewColumns1";
            this.afTreeViewColumns1.Padding = new System.Windows.Forms.Padding(1);
            this.afTreeViewColumns1.Size = new System.Drawing.Size(652, 260);
            this.afTreeViewColumns1.TabIndex = 2;
            // 
            // button2
            // 
            this.button2.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.button2.Location = new System.Drawing.Point(611, 0);
            this.button2.Name = "button2";
            this.button2.Size = new System.Drawing.Size(41, 18);
            this.button2.TabIndex = 8;
            this.button2.Text = "Help";
            this.toolTip1.SetToolTip(this.button2, "View help for this component");
            this.button2.UseVisualStyleBackColor = true;
            this.button2.Click += new System.EventHandler(this.button2_Click);
            // 
            // btnEdit
            // 
            this.btnEdit.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.btnEdit.Location = new System.Drawing.Point(564, 0);
            this.btnEdit.Name = "btnEdit";
            this.btnEdit.Size = new System.Drawing.Size(41, 18);
            this.btnEdit.TabIndex = 9;
            this.btnEdit.Text = "Edit...";
            this.toolTip1.SetToolTip(this.btnEdit, "Open the custom editing dialog");
            this.btnEdit.UseVisualStyleBackColor = true;
            this.btnEdit.Click += new System.EventHandler(this.btnEdit_Click);
            // 
            // TreeGridUI
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.btnEdit);
            this.Controls.Add(this.button2);
            this.Controls.Add(this.afTreeViewColumns1);
            this.Name = "TreeGridUI";
            this.Size = new System.Drawing.Size(655, 282);
            this.Load += new System.EventHandler(this.TreeGridUI_Load);
            this.Controls.SetChildIndex(this.MyHelpLabel, 0);
            this.Controls.SetChildIndex(this.afTreeViewColumns1, 0);
            this.Controls.SetChildIndex(this.button2, 0);
            this.Controls.SetChildIndex(this.btnEdit, 0);
            this.ResumeLayout(false);

        }

        #endregion

        private AFTreeViewColumns afTreeViewColumns1;
        private System.Windows.Forms.Button button2;
        private System.Windows.Forms.Button btnEdit;
        private System.Windows.Forms.ToolTip toolTip1;
        private System.ComponentModel.IContainer components;
    }
}
