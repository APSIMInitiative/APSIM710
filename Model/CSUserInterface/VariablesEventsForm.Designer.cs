namespace CSUserInterface
{
    partial class VariablesEventsForm
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
            this.FilterPanel = new System.Windows.Forms.Panel();
            this.EventsCheckBox = new System.Windows.Forms.CheckBox();
            this.VariableListView = new System.Windows.Forms.ListView();
            this.ColumnHeader1 = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
            this.ColumnHeader4 = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
            this.ColumnHeader2 = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
            this.ColumnHeader3 = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
            this.Label1 = new System.Windows.Forms.Label();
            this.ComponentFilter = new System.Windows.Forms.ComboBox();
            this.FilterPanel.SuspendLayout();
            this.SuspendLayout();
            // 
            // FilterPanel
            // 
            this.FilterPanel.Controls.Add(this.EventsCheckBox);
            this.FilterPanel.Controls.Add(this.VariableListView);
            this.FilterPanel.Controls.Add(this.Label1);
            this.FilterPanel.Controls.Add(this.ComponentFilter);
            this.FilterPanel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.FilterPanel.Location = new System.Drawing.Point(0, 0);
            this.FilterPanel.Name = "FilterPanel";
            this.FilterPanel.Size = new System.Drawing.Size(450, 732);
            this.FilterPanel.TabIndex = 19;
            // 
            // EventsCheckBox
            // 
            this.EventsCheckBox.AutoSize = true;
            this.EventsCheckBox.Location = new System.Drawing.Point(262, 15);
            this.EventsCheckBox.Name = "EventsCheckBox";
            this.EventsCheckBox.Size = new System.Drawing.Size(95, 17);
            this.EventsCheckBox.TabIndex = 21;
            this.EventsCheckBox.Text = "Show Events?";
            this.EventsCheckBox.UseVisualStyleBackColor = true;
            this.EventsCheckBox.CheckedChanged += new System.EventHandler(this.EventsCheckBox_CheckedChanged);
            // 
            // VariableListView
            // 
            this.VariableListView.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.VariableListView.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.ColumnHeader1,
            this.ColumnHeader4,
            this.ColumnHeader2,
            this.ColumnHeader3});
            this.VariableListView.FullRowSelect = true;
            this.VariableListView.Location = new System.Drawing.Point(11, 39);
            this.VariableListView.Name = "VariableListView";
            this.VariableListView.Size = new System.Drawing.Size(427, 679);
            this.VariableListView.Sorting = System.Windows.Forms.SortOrder.Ascending;
            this.VariableListView.TabIndex = 20;
            this.VariableListView.UseCompatibleStateImageBehavior = false;
            this.VariableListView.View = System.Windows.Forms.View.Details;
            // 
            // ColumnHeader1
            // 
            this.ColumnHeader1.Text = "Variable name";
            this.ColumnHeader1.Width = 201;
            // 
            // ColumnHeader4
            // 
            this.ColumnHeader4.Text = "Array?";
            this.ColumnHeader4.Width = 45;
            // 
            // ColumnHeader2
            // 
            this.ColumnHeader2.Text = "Units";
            // 
            // ColumnHeader3
            // 
            this.ColumnHeader3.Text = "Description";
            this.ColumnHeader3.Width = 437;
            // 
            // Label1
            // 
            this.Label1.AutoSize = true;
            this.Label1.Location = new System.Drawing.Point(9, 15);
            this.Label1.Name = "Label1";
            this.Label1.Size = new System.Drawing.Size(86, 13);
            this.Label1.TabIndex = 19;
            this.Label1.Text = "Component filter:";
            // 
            // ComponentFilter
            // 
            this.ComponentFilter.FormattingEnabled = true;
            this.ComponentFilter.Location = new System.Drawing.Point(101, 12);
            this.ComponentFilter.Name = "ComponentFilter";
            this.ComponentFilter.Size = new System.Drawing.Size(155, 21);
            this.ComponentFilter.TabIndex = 18;
            this.ComponentFilter.TextChanged += new System.EventHandler(this.ComponentFilter_TextChanged);
            // 
            // VariablesEventsForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(450, 732);
            this.Controls.Add(this.FilterPanel);
            this.Name = "VariablesEventsForm";
            this.Text = "VariablesEventsForm";
            this.Load += new System.EventHandler(this.OnLoad);
            this.FilterPanel.ResumeLayout(false);
            this.FilterPanel.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Panel FilterPanel;
        private System.Windows.Forms.ListView VariableListView;
        private System.Windows.Forms.ColumnHeader ColumnHeader1;
        private System.Windows.Forms.ColumnHeader ColumnHeader4;
        private System.Windows.Forms.ColumnHeader ColumnHeader2;
        private System.Windows.Forms.ColumnHeader ColumnHeader3;
        private System.Windows.Forms.Label Label1;
        private System.Windows.Forms.ComboBox ComponentFilter;
        private System.Windows.Forms.CheckBox EventsCheckBox;
    }
}