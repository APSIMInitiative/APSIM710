using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;
using System.Collections.Specialized;
using CSGeneral;
using System.Xml;

namespace CSUserInterface
{
    public partial class FactorTargets : Controllers.BaseView
    {

        public FactorTargets()
        {
            InitializeComponent();
        }
        public override void OnRefresh()
        {
            TargetList.Items.Clear();
            XmlNode varNode = Data.SelectSingleNode("//targets");
            if (varNode != null)
            {
                foreach (XmlNode target in varNode.ChildNodes)
                {
                    TargetList.Items.Add(target.InnerText);
                }
            }
        }
        public void AddTargets(StringCollection targetPaths)
        {
           XmlNode targetsNode = Data.SelectSingleNode("//targets");
           if (targetsNode == null)
              targetsNode = Data.AppendChild(Data.OwnerDocument.CreateElement("targets"));

           foreach (string item in targetPaths)
           {
              XmlNode varNode = targetsNode.AppendChild(Data.OwnerDocument.CreateElement("Target"));
              varNode.InnerText = item;
           }
        }
        public override void OnSave()
        {
           XmlNode targetsNode = Data.SelectSingleNode("//targets");
           if (targetsNode == null)
              targetsNode = Data.AppendChild(Data.OwnerDocument.CreateElement("targets"));
           targetsNode.RemoveAll();

           foreach (string item in TargetList.Items)
           {
              XmlNode varNode = targetsNode.AppendChild(Data.OwnerDocument.CreateElement("Target"));
              varNode.InnerText = item;
           }
        }
        private void TargetList_DragDrop(object sender, DragEventArgs e)
        {
            if (e.Data.GetDataPresent(typeof(string)))
            {
                StringCollection sPaths = Controller.SelectedPaths;
                string FullXML = (string)e.Data.GetData(DataFormats.Text);
                foreach (string path in sPaths)
                    TargetList.Items.Add(path);
            }
        }

        private void TargetList_DragEnter(object sender, DragEventArgs e)
        {
            if (e.Data.GetDataPresent(typeof(string)))
            {
                e.Effect = DragDropEffects.Copy;
            }
            else
            {
                e.Effect = DragDropEffects.None;
            }
        }

        private void TargetList_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Delete)
            {
                //delete selected items
                for (int i = TargetList.SelectedIndices.Count - 1; i >= 0; --i)
                {
                    TargetList.Items.RemoveAt(TargetList.SelectedIndices[i]);
                }
                e.Handled = true;
            }
        }

        private void TargetList_SelectedIndexChanged(object sender, EventArgs e)
        {
            StringCollection paths = new StringCollection();
            foreach (string path in TargetList.SelectedItems)
            {
                paths.Add(path);
            }
            Controller.SelectedPaths = paths;
        }
    }
}
