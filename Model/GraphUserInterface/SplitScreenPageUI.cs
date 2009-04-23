
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
using UIUtility;


namespace GraphUserInterface
    {
    public partial class SplitScreenPageUI : BaseView
        {
        private DataProcessor DataProcessor;

        public SplitScreenPageUI()
            {
            InitializeComponent();
            }
        ~SplitScreenPageUI()
            {
            if (DataProcessor != null)
                {
                DataProcessor.Shutdown();
                DataProcessor = null;
                }
            }

        protected override void OnLoad()
            {
            DataProcessor = new DataProcessor();
            List<XmlNode> Pages = XmlHelper.ChildNodes(Data, "page");
            if (Pages.Count >= 1)
                {
                Page1.Processor = DataProcessor;
                Page1.OnLoad(Controller, NodePath, Pages[0].OuterXml);
                Page1.ViewChanged += OnPage1Changed;
                }
            if (Pages.Count == 2)
                {
                Page2.Processor = DataProcessor;
                Page2.OnLoad(Controller, NodePath, Pages[1].OuterXml);
                Page2.ViewChanged += OnPage1Changed;
                }
            }
        public override void OnRefresh()
            {
            // -----------------------------------------------
            // Called when it's time to refresh the canvas and
            // everything on it.
            // -----------------------------------------------
            base.OnRefresh();

            if (XmlHelper.ChildNodes(Data, "page").Count >= 1)
                Page1.OnRefresh();
            if (XmlHelper.ChildNodes(Data, "page").Count == 2)
                Page2.OnRefresh();
            }
        public override void OnSave()
            {
            // -----------------------------------------------
            // Called when it's time to save everything back
            // to XML
            // -----------------------------------------------
            base.OnSave();
            Page1.OnSave();
            Page2.OnSave();
            Data.InnerXml = Page1.GetData() + Page2.GetData();
            }
        private void OnPage1Changed(BaseView ChangedNode)
            {
            Data.InnerXml = Page1.GetData() + Page2.GetData();
            Page2.OnRefresh();
            }
        private void OnPage2Changed(BaseView ChangedNode)
            {
            Data.InnerXml = Page1.GetData() + Page2.GetData();
            Page1.OnRefresh();
            }
        public override void OnClose()
            {
            Page1.OnClose();
            Page2.OnClose();
            }

        
        
        }
    }

