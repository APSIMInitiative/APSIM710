using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Xml;

using Controllers;
using CSGeneral;


namespace CPIUserInterface
{
    public partial class ScriptUI : BaseView
    {
        public ScriptUI()
        {
            InitializeComponent();
        }
        protected override void OnLoad()
        {
            base.HelpText = "Manager script";
        }

        private void ScriptUI_Load(object sender, EventArgs e)
        {
            //Fill the property fields
            XmlNode UINode = XmlHelper.Find(Data, "ui");
            if (UINode != null)
            {
                //log file
                //log on/off
            }
            //fill the richedit with the script
            richTextBox1.Text = "!Sample script";
        }
        public override void OnSave()
        {
                        
        }
        public override void OnRefresh()
        {
            base.OnRefresh();
        }
    }
}
