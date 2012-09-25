using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;

namespace CPIUserInterface
{
    public partial class AFTreeViewColumns : UserControl
    {
        //=====================================================================
        /// <summary>
        /// public delegate for change of data
        /// </summary>
        public delegate void onDataChange();

        /// <summary>
        /// public event which occurs when the data changes
        /// </summary>
        public event onDataChange saveChangesEvent;
        /// <summary>
        /// 
        /// </summary>
        public delegate void reloadTree();
        /// <summary>
        /// 
        /// </summary>
        public event reloadTree reloadTreeEvent;

        private string _editColumn;

        //=====================================================================
        /// <summary>
        /// Event for retrieving the data for a column. This allows for the 
        /// flexible definition and use of this treelist control.
        /// Implement in the host class.
        /// </summary>
        /// <param name="item">The tag item for the node</param>
        /// <param name="col">The column number 0-n</param>
        /// <returns>The string for the cell</returns>
        public delegate String onGetColValue(TAFTreeViewColumnTag item, int col);
        public event onGetColValue getColValueEvent = null;

        public Boolean AllowEdit = true;
        private Boolean NoRedraw;
        //=====================================================================
        /// <summary>
        /// 
        /// </summary>
        public AFTreeViewColumns()
        {
            NoRedraw = false;
            InitializeComponent();
            _editColumn = "value";
            richTextBox1.Visible = false;

            this.BackColor = System.Windows.Forms.VisualStyles.VisualStyleInformation.TextControlBorder;
            this.Padding = new Padding(1);
        }
        //=====================================================================
        /// <summary>
        /// Get the internal treeview component
        /// </summary>
        public TreeView TreeView
        {
            get
            {
                return this.treeView1;
            }
        }
        public ListView ListView
        {
            get
            {
                return this.listView1;
            }
        }
        //=====================================================================
        /// <summary>
        /// get the columns from the listview component
        /// </summary>
        //=====================================================================
        public ListView.ColumnHeaderCollection Columns
        {
            get
            {
                return this.listView1.Columns;
            }
        }
        //=====================================================================
        /// <summary>
        /// handle the listview column click event
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void listView1_ColumnClick(object sender, ColumnClickEventArgs e)
        {
            this.treeView1.Focus();
        }
        //=====================================================================
        /// <summary>
        /// handle the treeview click event
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void treeView1_Click(object sender, EventArgs e)
        {
            Point p = this.treeView1.PointToClient(Control.MousePosition);
            TreeNode tn = this.treeView1.GetNodeAt(p);
            if (tn != null)
                this.treeView1.SelectedNode = tn;
        }
        //=====================================================================
        /// <summary>
        /// handle the listview column width changed event
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void listView1_ColumnWidthChanged(object sender, ColumnWidthChangedEventArgs e)
        {
            this.treeView1.Focus();
            this.treeView1.Invalidate();
        }
        //=====================================================================
        /// <summary>
        /// On resize of the listview columns - resize the tree node
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void listView1_ColumnWidthChanging(object sender, ColumnWidthChangingEventArgs e)
        {
            this.treeView1.Focus();
            this.treeView1.Invalidate();
        }
        /// <summary>
        /// Stop the redraw for the list and tree
        /// </summary>
        public void StopLayout()
        {
            this.TreeView.BeginUpdate();
            this.listView1.SuspendLayout();
            NoRedraw = true;
        }
        /// <summary>
        /// Enable the redraw for the list and the tree
        /// </summary>
        public void RestartLayout()
        {
            this.listView1.ResumeLayout();
            this.TreeView.EndUpdate();
            NoRedraw = false;
        }
        //=====================================================================
        /// <summary>
        /// on draw of the tree node - populate the listview columns
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void treeView1_DrawNode(object sender, DrawTreeNodeEventArgs e)
        {
            if (!NoRedraw)
            {
                e.DrawDefault = true;

                Rectangle rect = e.Bounds;

                if ((e.State & TreeNodeStates.Selected) != 0)
                {
                    if ((e.State & TreeNodeStates.Focused) != 0)
                        e.Graphics.FillRectangle(SystemBrushes.Highlight, rect);
                    else
                        e.Graphics.FillRectangle(SystemBrushes.Control, rect);
                }
                else
                    e.Graphics.FillRectangle(Brushes.White, rect);

                e.Graphics.DrawRectangle(SystemPens.Control, rect);

                for (int intColumn = 1; intColumn < this.listView1.Columns.Count; intColumn++)
                {
                    rect.Offset(this.listView1.Columns[intColumn - 1].Width, 0);
                    rect.Width = this.listView1.Columns[intColumn].Width;

                    e.Graphics.DrawRectangle(SystemPens.Control, rect);

                    string strColumnText;

                    TAFTreeViewColumnTag objTag = e.Node.Tag as TAFTreeViewColumnTag;

                    if (objTag != null)
                        if (getColValueEvent != null)   //if the host class has implemented a handler
                            strColumnText = getColValueEvent(objTag, intColumn);
                        else
                            strColumnText = objTag.getValueAtColumn(intColumn);
                    else
                        strColumnText = intColumn + " " + e.Node.Text; // dummy

                    TextFormatFlags flags = TextFormatFlags.EndEllipsis;
                    switch (this.listView1.Columns[intColumn].TextAlign)
                    {
                        case HorizontalAlignment.Center:
                            flags |= TextFormatFlags.HorizontalCenter;
                            break;
                        case HorizontalAlignment.Left:
                            flags |= TextFormatFlags.Left;
                            break;
                        case HorizontalAlignment.Right:
                            flags |= TextFormatFlags.Right;
                            break;
                        default:
                            break;
                    }

                    rect.Y++;
                    if ((e.State & TreeNodeStates.Selected) != 0 &&
                        (e.State & TreeNodeStates.Focused) != 0)
                        TextRenderer.DrawText(e.Graphics, strColumnText, treeView1.Font, rect, SystemColors.HighlightText, flags);
                    else
                        TextRenderer.DrawText(e.Graphics, strColumnText, treeView1.Font, rect, treeView1.ForeColor, treeView1.BackColor, flags);
                    rect.Y--;
                }
            }
        }
        //=====================================================================
        /// <summary>
        /// treeview double click event handler
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void treeView1_DoubleClick(object sender, EventArgs e)
        {
            if (sender is TreeView)
            {
                displayEditor();
            }
        }
        //=====================================================================
        /// <summary>
        /// display the text editor to update the list view control
        /// </summary>
        private void displayEditor()
        {
            if (AllowEdit)
            {
                TreeNode treeNode = this.treeView1.SelectedNode;
                if (treeNode != null)
                {
                    TAFTreeViewColumnTag o = treeNode.Tag as TAFTreeViewColumnTag;

                    if ((!o.TypedValue.isArray()) && (!o.TypedValue.isRecord()))
                    {
                        int iValColumnWidth = 150;
                        int spos = 0;
                        int epos = this.listView1.Columns[0].Width;
                        int i = 0;
                        bool lFound = false;
                        while ((i < this.listView1.Columns.Count) && (!lFound))
                        {
                            if (this.listView1.Columns[i].Text.ToLower().Equals(_editColumn))
                            {
                                lFound = true;
                                iValColumnWidth = this.listView1.Columns[i].Width;
                            }
                            else
                            {
                                spos = epos;
                                epos += this.listView1.Columns[i].Width;
                            }
                            i++;
                        }

                        richTextBox1.Size = new System.Drawing.Size(iValColumnWidth, 20);
                        richTextBox1.Location = new System.Drawing.Point(spos + 3, treeNode.Bounds.Y + treeNode.Bounds.Height + 3);
                        richTextBox1.Text = o.Value;
                        richTextBox1.Show();
                        richTextBox1.SelectAll();
                        richTextBox1.Focus();
                    }
                }
            }
        }
        //=====================================================================
        /// <summary>
        /// save changes to tree node
        /// </summary>
        private void saveChanges()
        {
            TreeNode selectedNode = this.treeView1.SelectedNode;
            if (selectedNode != null)
            {
                TAFTreeViewColumnTag objTAF = selectedNode.Tag as TAFTreeViewColumnTag;

                ListView.SelectedListViewItemCollection jj = this.listView1.SelectedItems;

                if (!objTAF.Value.Equals(richTextBox1.Text))
                {
                    richTextBox1.Text = objTAF.updateValue(richTextBox1.Text);
                    if (saveChangesEvent != null)
                    {
                        saveChangesEvent();
                    }
                }
            }
        }
        //=====================================================================
        /// <summary>
        /// richtextbox on leave handler
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void richTextBox1_Leave(object sender, EventArgs e)
        {
            saveChanges();
            richTextBox1.Visible = false;

            if ((this.treeView1.SelectedNode != null) && (this.treeView1.SelectedNode.Tag != null))
            {
                TAFTreeViewColumnTag objTag = this.treeView1.SelectedNode.Tag as TAFTreeViewColumnTag;
                objTag.Value = richTextBox1.Text;

            }
            this.treeView1.Refresh();
        }
        //=====================================================================
        /// <summary>
        /// richtextbox key down handler
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void richTextBox1_KeyDown(object sender, KeyEventArgs e)
        {
            // save changes on "enter"
            if (e.KeyCode == Keys.Enter)
            {
                saveChanges();
                richTextBox1.Visible = false;
                if (this.treeView1.SelectedNode.NextNode != null)
                {
                    this.treeView1.SelectedNode = this.treeView1.SelectedNode.NextNode;
                }
            }
        }
        //=====================================================================
        /// <summary>
        /// treeview key down handler
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void treeView1_KeyDown(object sender, KeyEventArgs e)
        {
            // display text editor when "enter" or F2 is pressed
            if ( (e.KeyCode == Keys.Enter) || (e.KeyCode == Keys.F2) )
            {
                if (sender is TreeView)
                {
                    displayEditor();
                }
            }
        }
        //=====================================================================
        private void treeView1_AfterSelect(object sender, TreeViewEventArgs e)
        {
            if (this.treeView1.SelectedNode != null)
            {
                this.treeView1.SelectedNode.BackColor = Color.Empty;
                this.treeView1.SelectedNode.ForeColor = Color.Empty;
                Rectangle r = this.treeView1.SelectedNode.Bounds;
            }
        }
        //=====================================================================
        private void treeView1_MouseClick(object sender, MouseEventArgs e)
        {
            foreach (ToolStripItem contextItem in contextMenuStrip1.Items)
            {
                contextItem.Enabled = false;
                contextItem.Visible = false;
            }

            if (e.Button == MouseButtons.Right)
            {
                if ((treeView1.SelectedNode != null) && (treeView1.SelectedNode.Tag != null) && (treeView1.SelectedNode.Tag is TAFTreeViewColumnTag))
                {
                    TAFTreeViewColumnTag treeViewColTag = this.treeView1.SelectedNode.Tag as TAFTreeViewColumnTag;
                    TAFTreeViewColumnTag treeParentTag = null;
                    if (this.treeView1.SelectedNode.Parent != null)
                    {
                        treeParentTag = this.treeView1.SelectedNode.Parent.Tag as TAFTreeViewColumnTag;
                    }

                    //delete option
                    deleteToolStripMenuItem1.Enabled = (treeParentTag != null) && (treeParentTag.TypedValue.count() > 0) && (treeParentTag.TypedValue.isArray());
                    deleteToolStripMenuItem1.Visible = (treeParentTag != null) && (treeParentTag.TypedValue.isArray());
                    //add options
                    if (treeViewColTag.TypedValue.isArray()) 
                    {
                        addToolStripMenuItem.Enabled = true;
                        addToolStripMenuItem.Visible = true;
                    }
                }
            }
        }
        //=====================================================================
        private void addToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if ((treeView1.SelectedNode != null) && (treeView1.SelectedNode.Tag != null) && (treeView1.SelectedNode.Tag is TAFTreeViewColumnTag))
            {
                TAFTreeViewColumnTag treeViewColTag = this.treeView1.SelectedNode.Tag as TAFTreeViewColumnTag;

                if (treeViewColTag.TypedValue.isArray() )
                {
                    treeViewColTag.TypedValue.setElementCount(treeViewColTag.TypedValue.count() + 1);

                    TreeNode currentTreeNode = treeView1.SelectedNode;

                    if (reloadTreeEvent != null)
                    {
                        reloadTreeEvent();      //expects as a minimum a redraw of the selected node
                    }
                    selectNode(treeView1.Nodes, currentTreeNode);
                }
            }
        }
        //=====================================================================
        private void selectNode(TreeNodeCollection parentNode, TreeNode node)
        {
            treeView1.Select();
            foreach (TreeNode treeNode in parentNode)
            {
                if (treeNode.Text.Equals(node.Text))
                {
                    treeView1.SelectedNode = treeNode;

                    if (node.IsExpanded)
                    {
                        treeView1.SelectedNode.Expand();
                    }
                }
                else
                {
                    selectNode(treeNode.Nodes, node);
                }
            }
        }
        //=====================================================================
        private void deleteToolStripMenuItem1_Click(object sender, EventArgs e)
        {
            if ((treeView1.SelectedNode != null) && (treeView1.SelectedNode.Tag != null) && (treeView1.SelectedNode.Tag is TAFTreeViewColumnTag))
            {
                TAFTreeViewColumnTag treeViewColTag = this.treeView1.SelectedNode.Tag as TAFTreeViewColumnTag;

                TreeNode parentNode = treeView1.SelectedNode.Parent;
                
                if ((parentNode != null) && (parentNode.Tag is TAFTreeViewColumnTag))
                {
                    TAFTreeViewColumnTag parentTreeViewColTag = parentNode.Tag as TAFTreeViewColumnTag;
                    parentTreeViewColTag.TypedValue.deleteElement(this.treeView1.SelectedNode.Index + 1);
                    
                    if (reloadTreeEvent != null)
                    {
                        selectNode(treeView1.Nodes, parentNode);    //this means that is the parent only is being refilled it will work correctly
                        reloadTreeEvent();
                    }

                    selectNode(treeView1.Nodes, parentNode);
                }
            }
        }

        private void treeView1_ItemDrag(object sender, ItemDragEventArgs e)
        {
                TreeView.DoDragDrop(e.Item, DragDropEffects.All);
        }
    }
}
