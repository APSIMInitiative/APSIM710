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
//outputfile - use constants to add factorial settings
//use xml, insert before existing??
//uses first '=' to separate name and value
// <Constants> starts this section
// <test>value</test> is a line
//If constant is present, it ignores the Title

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
            int iTotalCount = 0;
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
                iTotalCount += iCount;
                txtTotalSims.Text = iTotalCount.ToString();
            }

            XmlNode varNode = Data.SelectSingleNode("//settings");
            string s = "";
            if(varNode != null)
                s = XmlHelper.Attribute(varNode, "fn");
            if (s == "1")
                radDesc.Checked = true;
            else
                radCount.Checked = true;

            s = "";
            if (varNode != null)
                s = XmlHelper.Attribute(varNode, "tl");
            if (s == "1")
                radMultiple.Checked = true;
            else
                radSingle.Checked = true;
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
            Data.RemoveAll();

            XmlNode node = Data.AppendChild(Data.OwnerDocument.CreateElement("settings"));
            if (radDesc.Checked)
            {
                XmlHelper.SetAttribute(node, "fn", "1");
            }
            if (radMultiple.Checked)
            {
                XmlHelper.SetAttribute(node, "tl", "1");
            }
        }


    }
}
