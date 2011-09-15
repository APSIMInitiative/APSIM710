using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Steema.TeeChart;
using Steema.TeeChart.Styles;

namespace Graph
{
    public partial class GraphPropertyForm : Form
    {
        private GraphUI2 Chart;

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="C"></param>
        public GraphPropertyForm(GraphUI2 C)
        {
            InitializeComponent();
            Chart = C;
        }

        /// <summary>
        /// Form has been loaded.
        /// </summary>
        private void OnLoad(object sender, EventArgs e)
        {
            RefreshAll();
            Tree.Nodes[0].Expand();
            Tree.Nodes[0].Nodes[0].Expand();
        }

        private void RefreshAll()
        {
            string SeriesTitleSelected = "";
            if (Tree.SelectedNode != null && Tree.SelectedNode.Parent == Tree.Nodes[0].Nodes[0])
                SeriesTitleSelected = Tree.SelectedNode.Text;

            // Put all series in tree.
            Tree.Nodes[0].Nodes[0].Nodes.Clear();
            foreach (Series S in Chart.Chart.Series)
            {
                TreeNode SeriesNode = Tree.Nodes[0].Nodes[0].Nodes.Add(S.Title);
                SeriesNode.Name = S.Title;
                SeriesNode.ContextMenuStrip = SeriesContextMenu;
                SeriesNode.ToolTipText = S.Description;
            }

            if (SeriesTitleSelected != "")
            {
                TreeNode[] Node = Tree.Nodes[0].Nodes[0].Nodes.Find(SeriesTitleSelected, false);
                if (Node.Length == 1)
                    Tree.SelectedNode = Node[0];

            }
        }

        /// <summary>
        /// User has selected a node in the tree - display an editor for it.
        /// </summary>
        private void OnTreeNodeSelect(object sender, TreeViewEventArgs e)
        {
            PropertyPanel.Controls.Clear();

            Form f;
            if (e.Node.Text == "Chart")
                f = new Steema.TeeChart.Editors.GeneralEditor(Chart.Chart.Chart, PropertyPanel);
            else if (e.Node.Text == "Panel")
                f = new Steema.TeeChart.Editors.PanelEditor(Chart.Chart.Panel, PropertyPanel);
            else if (e.Node.Text == "Left")
                f = new Steema.TeeChart.Editors.AxisEditor(Chart.Chart.Axes.Left, PropertyPanel);
            else if (e.Node.Text == "Top")
                f = new Steema.TeeChart.Editors.AxisEditor(Chart.Chart.Axes.Top, PropertyPanel);
            else if (e.Node.Text == "Right")
                f = new Steema.TeeChart.Editors.AxisEditor(Chart.Chart.Axes.Right, PropertyPanel);
            else if (e.Node.Text == "Bottom")
                f = new Steema.TeeChart.Editors.AxisEditor(Chart.Chart.Axes.Bottom, PropertyPanel);
            else if (e.Node.Text == "Titles")
                f = new Steema.TeeChart.Editors.TitleEditor(Chart.Chart.Chart, PropertyPanel);
            else if (e.Node.Text == "Left wall")
                f = new Steema.TeeChart.Editors.WallEditor(Chart.Chart.Walls.Left, PropertyPanel);
            else if (e.Node.Text == "Right wall")
                f = new Steema.TeeChart.Editors.WallEditor(Chart.Chart.Walls.Right, PropertyPanel);
            else if (e.Node.Text == "Back wall")
                f = new Steema.TeeChart.Editors.WallEditor(Chart.Chart.Walls.Back, PropertyPanel);
            else if (e.Node.Text == "Bottom wall")
                f = new Steema.TeeChart.Editors.WallEditor(Chart.Chart.Walls.Bottom, PropertyPanel);
            else if (e.Node.Text == "Legend")
                f = new Steema.TeeChart.Editors.LegendEditor(Chart.Chart.Chart, PropertyPanel);
            else if (e.Node.Text == "3D")
                f = new Steema.TeeChart.Editors.AspectEditor(Chart.Chart.Chart, PropertyPanel);
            //         else if (e.Node.Text == "Tools")
            //            f = new Steema.TeeChart.Editors.Tools.ToolsEditor(Chart.Chart.Chart, PropertyPanel);
            else if (e.Node.Text == "Themes")
                f = new Steema.TeeChart.Editors.ThemeEditor(Chart.Chart.Chart, PropertyPanel);
            else if (e.Node.Parent == Tree.Nodes[0].Nodes[0])
            {
                // series node.
                Series S = Chart.Chart.Series.WithTitle(e.Node.Text);
                f = new Steema.TeeChart.Editors.SeriesEditor(S, PropertyPanel);
                TabControl TabControl1 = (TabControl)f.Controls[0];
                TabControl1.TabPages.RemoveByKey("TabSource");
            }
            else
                f = null;

            if (f != null)
            {
                f.Parent = PropertyPanel;
                f.Dock = DockStyle.Fill;
                f.Show();
            }
        }

        private void OnAddSeriesMenu(object sender, EventArgs e)
        {
            PlotsForm F = new PlotsForm(Chart);
            if (F.ShowDialog() == DialogResult.OK)
            {
                foreach (Series S in F.GetSeries())
                {
                    S.CheckDataSource();
                    Chart.Chart.Series.Add(S);

                }
                Chart.OnSave();
                Chart.OnRefresh();
                RefreshAll();
            }
        }

        private void OnDeleteSeriesMenu(object sender, EventArgs e)
        {
            Series S = Chart.Chart.Series.WithTitle(Tree.SelectedNode.Text);
            if (S != null)
            {
                Chart.Chart.Series.Remove(S);
                RefreshAll();
            }
        }

        private void OnRenameSeriesMenu(object sender, EventArgs e)
        {
            Tree.SelectedNode.BeginEdit();
        }

        private void OnXYDataSeriesMenu(object sender, EventArgs e)
        {
            Series S = Chart.Chart.Series[Tree.SelectedNode.Index];
            int SeriesIndex = Chart.Chart.Series.IndexOf(S);

            PlotsForm F = new PlotsForm(Chart);
            F.SetSeries(S);
            if (F.ShowDialog() == DialogResult.OK)
            {
                List<Series> Series = F.GetSeries();
                if (Series.Count == 1)
                {
                    Chart.Chart.Series.Remove(S);
                    S = Chart.Chart.Series.Add(Series[0]);
                    Chart.Chart.Series.MoveTo(S, SeriesIndex);
                    bool SavedDirtyFlag = Chart.UserHasChangedProperties;
                    Chart.UserHasChangedProperties = true;
                    Chart.OnSave();
                    Chart.OnRefresh();
                    RefreshAll();
                    Chart.UserHasChangedProperties = SavedDirtyFlag;
                }
            }
        }

        private void Tree_BeforeLabelEdit(object sender, NodeLabelEditEventArgs e)
        {
            e.CancelEdit = e.Node.Parent != Tree.Nodes[0].Nodes[0];
        }

        private void Tree_AfterLabelEdit(object sender, NodeLabelEditEventArgs e)
        {
            Series S = Chart.Chart.Series[Tree.SelectedNode.Index];
            if (S != null && e.Label != null)
            {
                S.Title = e.Label;
                Chart.OnSave();
                Chart.OnRefresh();
            }
        }

        private void OnEnabledMenu(object sender, EventArgs e)
        {
            Series S = Chart.Chart.Series[Tree.SelectedNode.Index];
            S.Active = EnabledMenu.Checked;
        }

        private void OnMoveDown(object sender, EventArgs e)
        {
            if (Tree.SelectedNode.Index + 2 <= Tree.SelectedNode.Parent.Nodes.Count)
            {
                Series S = Chart.Chart.Series[Tree.SelectedNode.Index];
                Chart.Chart.Series.MoveTo(S, Tree.SelectedNode.Index + 2);
                Chart.OnSave();
                Chart.OnRefresh();
                RefreshAll();
            }
        }

        private void OnMoveUp(object sender, EventArgs e)
        {
            if (Tree.SelectedNode.Index - 1 >= 0)
            {
                Series S = Chart.Chart.Series[Tree.SelectedNode.Index];
                Chart.Chart.Series.MoveTo(S, Tree.SelectedNode.Index - 1);
                Chart.OnSave();
                Chart.OnRefresh();
                RefreshAll();
            }
        }

        private void OnKeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Down && e.Modifiers == Keys.Control)
                OnMoveDown(null, null);
            else if (e.KeyCode == Keys.Up && e.Modifiers == Keys.Control)
                OnMoveUp(null, null);
            else if (e.KeyCode == Keys.Delete)
                OnDeleteSeriesMenu(null, null);
            else if (e.KeyCode == Keys.F2)
                OnRenameSeriesMenu(null, null);
        }





    }
}
