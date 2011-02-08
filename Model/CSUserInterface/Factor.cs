using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Xml;
using CSGeneral;
using System.Collections.Specialized;

namespace CSUserInterface
{
    public partial class Factor : Controllers.BaseView
    {
        public Factor()
        {
            InitializeComponent();
        }
        protected override void OnLoad()
        {
            FactorTargets.OnLoad(Controller, NodePath, Data.OuterXml);
        }
        public override void OnRefresh()
        {
            FactorTargets.OnRefresh();
            //ApsimFile.Component comp = Controller.ApsimData.Find(NodePath);
            
        }
        public override void OnSave()
        {
            FactorTargets.OnSave();
            XmlDocument doc = new XmlDocument();
            doc.LoadXml(FactorTargets.GetData());
            Data.InnerXml = doc.DocumentElement.InnerXml;
        }


    }
}
