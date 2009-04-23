
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

using FarPoint.Win.Spread;

using ApsimFile;
using Controllers;
using UIUtility;  //GridUtility.cs


namespace CSUserInterface
    {
    public partial class ProfileUI : BaseView
        {
        private ApsimFile.Soil MySoil = null;
        //private string ComponentType;
        //private APSIMData FieldInfo;
        public ProfileUI()
            {
            InitializeComponent();
            }

        protected override void OnLoad()
            {
            FarPoint.Win.Spread.InputMap InputMap = Grid.GetInputMap(FarPoint.Win.Spread.InputMapMode.WhenAncestorOfFocused);
            InputMap.Put(new FarPoint.Win.Spread.Keystroke(Keys.Delete, Keys.None),
                        FarPoint.Win.Spread.SpreadActions.ClipboardCut);
            InputMap.Put(new FarPoint.Win.Spread.Keystroke(Keys.Enter, Keys.None),
                        FarPoint.Win.Spread.SpreadActions.MoveToNextRow);
            }
        public override void OnClose()
            {
            }
		override public void OnRefresh()
			{
            //APSIMData Component = Controller.ApsimData.Find(NodePath);
            //if (Component != null && Component.Parent != null)
            //    {
            //    ComponentType = Component.Type;
            //    APSIMData TypeInfo = Controller.GetComponentTypeInfo(ComponentType);
            //    if (TypeInfo != null && TypeInfo.Child("fields") != null)
            //        {
            //        FieldInfo = TypeInfo.Child("fields");
            //        MySoil = new Soil(Component.Parent);
            //        PopulateProfileGrid();
            //        OperationMode mode = OperationMode.Normal;
            //        if (Controller.ApsimData.IsReadOnly)
            //            mode = OperationMode.ReadOnly;
        				
            //        SoilProfile.OperationMode = mode;
            //        }
            //    }
            }

        private void PopulateProfileGrid()
            {
            //UserChange = false;
            //APSIMData[] Fields = FieldInfo.get_Children(null);

            //SoilProfile.RowCount = MySoil.Thickness.Length;
            //SoilProfile.ColCount = Fields.Length + 1;
            //SoilProfile.ClearRange(0, 0, SoilProfile.RowCount, SoilProfile.ColumnCount, true);

            //GridUtility.SetColumnAsStrings(SoilProfile, 0, Soils.SoilComponentUtility.ToDepthStrings(MySoil.Thickness));
            //for (int Col = 1; Col != SoilProfile.ColumnCount; Col++)
            //    {
            //    SoilProfile.
            //    GridUtility.SetColumnAsStrings(SoilProfile, Col, MySoil.Values(Fields[Col-1]));
            //    }
            //GridUtility.SetColumnAsStrings(SoilProfile, 0, Soils.SoilComponentUtility.ToDepthStrings(MySoil.Thickness));
            //GridUtility.SetColumnAsStrings(SoilProfile, 1, MySoil.Texture);
            //GridUtility.SetColumnAsDoubles(SoilProfile, 2, MySoil.SWCON);
            //GridUtility.SetColumnAsDoubles(SoilProfile, 3, MySoil.MWCON);
            //GridUtility.SetColumnAsDoubles(SoilProfile, 4, MySoil.FBIOM);
            //GridUtility.SetColumnAsDoubles(SoilProfile, 5, MySoil.FINERT);
            //GridUtility.SetColumnAsDoubles(SoilProfile, 6, MySoil.KS);
            //GridUtility.SetColumnAsDoubles(SoilProfile, 7, MySoil.OC);
            //GridUtility.SetColumnAsDoubles(SoilProfile, 8, MySoil.EC);
            //GridUtility.SetColumnAsDoubles(SoilProfile, 10, MySoil.CL);
            //GridUtility.SetColumnAsDoubles(SoilProfile, 11, MySoil.Boron);
            //GridUtility.SetColumnAsDoubles(SoilProfile, 12, MySoil.CEC);
            //GridUtility.SetColumnAsDoubles(SoilProfile, 13, MySoil.Ca);
            //GridUtility.SetColumnAsDoubles(SoilProfile, 14, MySoil.Mg);
            //GridUtility.SetColumnAsDoubles(SoilProfile, 15, MySoil.Na);
            //GridUtility.SetColumnAsDoubles(SoilProfile, 16, MySoil.K);
            //GridUtility.SetColumnAsDoubles(SoilProfile, 17, MySoil.ESP);
            //GridUtility.SetColumnAsDoubles(SoilProfile, 18, MySoil.ParticleSizeSand);
            //GridUtility.SetColumnAsDoubles(SoilProfile, 19, MySoil.ParticleSizeSilt);
            //GridUtility.SetColumnAsDoubles(SoilProfile, 20, MySoil.ParticleSizeClay);
            //if (MySoil.PHStoredAsWater())
            //    {
            //    SoilProfile.ColumnHeader.Cells[1, 9].Text = "(water)";
            //    GridUtility.SetColumnAsDoubles(SoilProfile, 9, MySoil.PH);
            //    }
            //else
            //    {
            //    SoilProfile.ColumnHeader.Cells[1, 9].Text = "(CaCl)";
            //    GridUtility.SetColumnAsDoubles(SoilProfile, 9, MySoil.PHCaCl);
            //    UserChange = true;
            //    }
            }
        private void SaveProfileGrid(int ColumnIndex)
            {
            int NumLayers = GridUtility.FindFirstBlankCell(SoilProfile, 0);
            switch (ColumnIndex)
                {
            case 1: MySoil.Texture = GridUtility.GetColumnAsStrings(SoilProfile, 1, NumLayers); break;
            case 2: MySoil.SWCON = GridUtility.GetColumnAsDoubles(SoilProfile, 2, NumLayers); break;
            case 3: MySoil.MWCON = GridUtility.GetColumnAsDoubles(SoilProfile, 3, NumLayers); break;
            case 4: MySoil.FBIOM = GridUtility.GetColumnAsDoubles(SoilProfile, 4, NumLayers); break;
            case 5: MySoil.FINERT = GridUtility.GetColumnAsDoubles(SoilProfile, 5, NumLayers); break;
            case 6: MySoil.KS = GridUtility.GetColumnAsDoubles(SoilProfile, 6, NumLayers); break;
            case 7: MySoil.OC = GridUtility.GetColumnAsDoubles(SoilProfile, 7, NumLayers); break;
            case 8: MySoil.EC = GridUtility.GetColumnAsDoubles(SoilProfile, 8, NumLayers); break;
            case 9: if (SoilProfile.ColumnHeader.Cells[1, 9].Text == "(water)")
                MySoil.PH = GridUtility.GetColumnAsDoubles(SoilProfile, 9, NumLayers);
            else
                MySoil.PHCaCl = GridUtility.GetColumnAsDoubles(SoilProfile, 9, NumLayers);
            break;
            case 10: MySoil.CL = GridUtility.GetColumnAsDoubles(SoilProfile, 10, NumLayers); break;
            case 11: MySoil.Boron = GridUtility.GetColumnAsDoubles(SoilProfile, 11, NumLayers); break;
            case 12: MySoil.CEC = GridUtility.GetColumnAsDoubles(SoilProfile, 12, NumLayers); break;
            case 13: MySoil.Ca = GridUtility.GetColumnAsDoubles(SoilProfile, 13, NumLayers); break;
            case 14: MySoil.Mg = GridUtility.GetColumnAsDoubles(SoilProfile, 14, NumLayers); break;
            case 15: MySoil.Na = GridUtility.GetColumnAsDoubles(SoilProfile, 15, NumLayers); break;
            case 16: MySoil.K = GridUtility.GetColumnAsDoubles(SoilProfile, 16, NumLayers); break;
            case 17: MySoil.ESP = GridUtility.GetColumnAsDoubles(SoilProfile, 17, NumLayers); break;
            case 18: MySoil.ParticleSizeSand = GridUtility.GetColumnAsDoubles(SoilProfile, 18, NumLayers); break;
            case 19: MySoil.ParticleSizeSilt = GridUtility.GetColumnAsDoubles(SoilProfile, 19, NumLayers); break;
            case 20: MySoil.ParticleSizeClay = GridUtility.GetColumnAsDoubles(SoilProfile, 20, NumLayers); break;
                }
            }
        private void OnProfileCellChanged(object sender, FarPoint.Win.Spread.SheetViewEventArgs e)
            {
            //if (UserChange)
            //    {
            //    UserChange = false;
            //    SaveProfileGrid(e.Column);
            //    UserChange = true;
            //    }
            }

        }
    }

