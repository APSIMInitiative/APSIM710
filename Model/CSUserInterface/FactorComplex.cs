using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Xml;
using CSGeneral;

namespace CSUserInterface
{
    public partial class FactorComplex : Controllers.BaseView
    {
        public FactorComplex()
        {
            InitializeComponent();
        }

        public override void OnRefresh()
        {
            DisplayXMLValues();
        }
        private void DisplayXMLValues()
        {
            treeView1.Nodes.Clear();
            //find vars node
            XmlNode varsNode = Data.SelectSingleNode("vars");
            if (varsNode != null)
            {
                foreach (XmlNode node in varsNode.ChildNodes)
                {
                    AddTreeNode(null, node);
                }
            }
        }
        private void AddTreeNode(TreeNode parentNode, XmlNode xmlnode)
        {
            string sText = XmlHelper.Name(xmlnode);
            TreeNode treeNode = null;

            if (parentNode == null)
            {
                treeNode = treeView1.Nodes.Add(sText);
            }
            else
            {
                treeNode = parentNode.Nodes.Add(sText);
                parentNode.Expand();
            }

            bool bEnabled = XmlHelper.Attribute(xmlnode, "enabled") == "yes";
            treeNode.Checked = bEnabled;
            bool bInherited = XmlHelper.Attribute(xmlnode, "inherited") == "yes";
            if (bInherited)
            {
                treeNode.ForeColor = Color.Gray;
            }
            else
            {
                
                //Font tmpFont = new System.Drawing.Font(treeView1.Font.FontFamily, treeView1.Font.Size, FontStyle.Bold);
                //treeNode.NodeFont = tmpFont;
            }
            foreach (XmlNode node in xmlnode.ChildNodes)
            {
                AddTreeNode(treeNode, node);
            }
        }


    }
}
