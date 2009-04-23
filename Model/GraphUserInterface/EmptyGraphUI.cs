
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

using Controllers;


namespace GraphUserInterface
    {
    public partial class EmptyGraphUI : BaseView
        {
        public EmptyGraphUI()
            {
            InitializeComponent();
            }

        public override void OnRefresh()
            {
            // -----------------------------------------------
            // Called when it's time to refresh
            // -----------------------------------------------
            base.OnRefresh();
            GroupBox.Text = Name;
            }

        }
    }

