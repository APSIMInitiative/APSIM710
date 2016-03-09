using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.IO;
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
            double iTotalCount = 0;
            foreach (string SimulationPath in SimsToRun)
            {
                double iCount = 0;
                List<FactorItem> items = builder.BuildFactorItems(Controller.ApsimData.FactorComponent, SimulationPath);
                foreach (FactorItem item in items)
                {
                    iCount += item.CalcCount();
                }
                int tmpCounter = 0;
                ApsimFile.Component tmpComp = Controller.ApsimData.Find(SimulationPath);
                if (tmpComp != null)
                {
                    TreeNode treeNode = treeSims.Nodes.Add(tmpComp.Name + " (" + iCount.ToString() + ")");
                    treeNode.ImageIndex = 0;
                    treeNode.SelectedImageIndex = 0;
                    List<String> factorials = ApsimFile.Factor.CalcFactorialList(Controller.ApsimData, SimulationPath);
                    AddFactorsToTreeNode(treeNode, factorials);
                    tmpCounter += factorials.Count;
                }
                iTotalCount += tmpCounter;
                txtTotalSims.Text = iTotalCount.ToString();
            }
            //double tmp = treeSims.Nodes[0].Nodes.Count;
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
        public void AddFactorsToTreeNode(TreeNode parentNode, List<string> factorials)
        {
            foreach(string factorial in factorials)
            {
                TreeNode node = parentNode.Nodes.Add(factorial);
                node.ImageIndex = 1;
                node.SelectedImageIndex = 1;
            }
        }

        public override void OnSave()
        {
            Data.RemoveAll();

            //JKB 1/7/13
            //When Run is pressed, Data hasn't been updated as OnLoad (where Data is refreshed from the text) is only called when it is made visible
            //Therefore changes made to the xml to trigger 'active' for FactorialMode aren't reflected in Data properly
            XmlHelper.SetValue(Data, "active", Controller.FactorialMode ? "1" : "0");

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

        private void btnGenerate_Click(object sender, EventArgs e)
        {
            MakeSims();
        }
        private void MakeSims()
        {
            System.Collections.Specialized.StringCollection PathsToConvert = Controller.SelectedPaths;
            List<String> SimsToConvert = new List<String>();
            foreach (String SimulationPath in PathsToConvert)
                ApsimFile.ApsimFile.ExpandSimsToRun(Controller.ApsimData.Find(SimulationPath), ref SimsToConvert);
            
            String UserMsg = "No simulations selected!";
            if (SimsToConvert.Count > 0)
            {
                FolderBrowserDialog FolderChooser = new FolderBrowserDialog();
                FolderChooser.Description = "Select the directory in which to save the .sim files";
                FolderChooser.SelectedPath = Directory.GetCurrentDirectory();
                if (FolderChooser.ShowDialog() == DialogResult.OK)
                {
                    try
                    {
                        List<string> SimFiles = ApsimFile.Factor.CreateSimFiles(Controller.ApsimData, SimsToConvert.ToArray(), FolderChooser.SelectedPath);
                        if (SimFiles.Count == 1)
                            UserMsg = "1 Simulation created.";
                        else
                        {
                            UserMsg = SimFiles.Count + " Simulations created.";
                        }
                    }
                    catch (Exception err)
                    {
                        MessageBox.Show("Unexpected Error while generating .sim files:\n " + err.Message, "Error generating .sim file", MessageBoxButtons.OK, MessageBoxIcon.Error);
                    }
                }
            }
            MessageBox.Show(UserMsg, "Create .SIM", MessageBoxButtons.OK, MessageBoxIcon.Information);
        }


    }
}
