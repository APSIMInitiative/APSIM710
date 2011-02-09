using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using ApsimFile;
using System.Xml;
using CSGeneral;
using System.IO;

namespace CSUserInterface
   {
   public partial class PlugInsUI : Controllers.BaseView
      {
      public PlugInsUI()
         {
         InitializeComponent();
         }

      protected override void OnLoad()
         {
         base.OnLoad();

         PlugInListBox.Items.Clear();
         foreach (XmlNode Child in XmlHelper.ChildNodes(Data, "PlugIn"))
            {
            string FileName = Configuration.RemoveMacros(Child.InnerText);
            bool Enabled = XmlHelper.Attribute(Child, "enabled") == "yes";
            ListViewItem Item = new ListViewItem(Path.GetFileNameWithoutExtension(FileName));
            Item.Checked = Enabled;
            Item.SubItems.Add(FileName);
            PlugInListBox.Items.Add(Item);
            }

         if (PlugInListBox.Items.Count == 0)
            {
            // Load in default plugins.
            foreach (KeyValuePair<string, bool> PlugIn in PlugIns.AllPlugIns)
               {
               string FileName = PlugIn.Key;
               bool Enabled = PlugIn.Value;
               ListViewItem Item = new ListViewItem(Path.GetFileNameWithoutExtension(FileName));
               Item.Checked = Enabled;
               Item.SubItems.Add(FileName);
               PlugInListBox.Items.Add(Item);
               }
            }
         }


      public override void OnSave()
         {
         base.OnSave();

         Data.RemoveAll();
         foreach (ListViewItem item in PlugInListBox.Items)
            {
            string FileName = item.SubItems[1].Text;
            bool Enabled = item.Checked;
            XmlNode PlugInNode = Data.OwnerDocument.CreateElement("PlugIn");
            Data.AppendChild(PlugInNode);
            PlugInNode.InnerText = Configuration.AddMacros(FileName);
            if (Enabled)
               XmlHelper.SetAttribute(PlugInNode, "enabled", "yes");
            else
               XmlHelper.SetAttribute(PlugInNode, "enabled", "no");
            }
         }

      private void OnAddPlugInClick(object sender, LinkLabelLinkClickedEventArgs e)
         {
         // -------------------------------------------- 
         // user has clicked add plugin button 
         // --------------------------------------------
         if (OpenPlugInDialog.ShowDialog() == DialogResult.OK)
            {
            string FileName = OpenPlugInDialog.FileName;
            bool Enabled = true;
            ListViewItem Item = new ListViewItem(Path.GetFileNameWithoutExtension(FileName));
            Item.Checked = Enabled;
            Item.SubItems.Add(FileName);
            PlugInListBox.Items.Add(Item);
            }
         }

      private void OnRemovePlugInClick(object sender, LinkLabelLinkClickedEventArgs e)
         {
         // ----------------------------------- 
         // User has clicked remove plugin button 
         // ----------------------------------- 
         foreach (ListViewItem Item in PlugInListBox.SelectedItems)
            PlugInListBox.Items.Remove(Item);
         }
      }
   }
