using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Xml;
using CSGeneral;
using ApsimFile;


namespace CSUserInterface
{
    public partial class Factorial : Controllers.BaseView
    {
        public Factorial()
        {
            InitializeComponent();
        }
        public override void OnRefresh()
        {
            treeSims.Nodes.Clear();
            FactorBuilder builder = new FactorBuilder();
            List<string> SimsToRun = new List<string>();
            ApsimFile.ApsimFile.ExpandSimsToRun(Controller.ApsimData.RootComponent, ref SimsToRun);

            foreach (string SimulationPath in SimsToRun)
            {
                int iCount = 0;
                List<FactorItem> items = builder.BuildFactorItems(Controller.ApsimData.FactorComponent, SimulationPath);
                foreach (FactorItem item in items)
                {
                    iCount += item.CalcCount();
                }
                ApsimFile.Component tmpComp = Controller.ApsimData.Find(SimulationPath);
                if (tmpComp != null)
                {
                    TreeNode treeNode = treeSims.Nodes.Add(tmpComp.Name + " (" + iCount.ToString() + ")");
                    treeNode.ImageIndex = 0;
                    treeNode.SelectedImageIndex = 0;
                    if (items.Count > 0)
                        AddFactorsToTreeNode(items[0], treeNode);
                }
            }
        }
        public void AddFactorsToTreeNode(FactorItem factor, TreeNode parentNode)
        {
            if (factor != null)
            {
                TreeNode node = parentNode.Nodes.Add(factor.getDesc() + " (" + factor.getCount() + ")");
                node.ImageIndex = 1;
                node.SelectedImageIndex = 1;
                AddFactorsToTreeNode(factor.NextItem, parentNode);
            }
        }

        public override void OnSave()
        {
        }


    }
}
