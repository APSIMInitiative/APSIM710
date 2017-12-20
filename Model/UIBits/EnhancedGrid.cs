using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace UIBits
{
    using Microsoft.VisualBasic;
    using System;
    using System.Collections;
    using System.Collections.Generic;
    using System.Data;
    using System.Diagnostics;
    using CSGeneral;
    public class MyDataGridViewComboBoxCell : DataGridViewComboBoxCell
    {
        public bool ValueInserted;
        public MyDataGridViewComboBoxCell()
        {
            FlatStyle = FlatStyle.Flat;
            MaxDropDownItems = 12;
        }
        protected override bool SetValue(int rowIndex, object value)
        {
            InsertValueIntoList(value);
            return base.SetValue(rowIndex, value);
        }
        public void InsertValueIntoList(object value)
        {
            if (value != null && value.ToString() != "" && !Items.Contains(value))
            {
                if (ValueInserted)
                    Items[0] = value;
                else
                    Items.Insert(0, value);
                ValueInserted = true;
            }
        }
    }


    public partial class EnhancedGrid : DataGridView
    {
        public delegate void TableColumnChangedDelegate(List<string> ColumnName);
        public delegate void AddingNewRowDelegate(DataGridViewRow NewRow);
        private DataTable _DataSourceTable = null;
        private bool InRefresh;
        private List<string> ChangedColumnNames = new List<string>();
        private List<Point> SavedSelections = new List<Point>();
        public ContextMenuStrip PopupMenu = new ContextMenuStrip();



        /// <summary>
        /// Whenever a column in the DataSourceTable is modified, this event will be
        /// triggered.
        /// </summary>
        public event TableColumnChangedDelegate TableColumnChangedEvent;

        /// <summary>
        /// Whenever a new row is to be added, this event will be triggered allowing
        /// the subscriber to add combo box cell types etc before the row gets 
        /// added to the Rows collection.
        /// </summary>
        public event AddingNewRowDelegate AddingNewRowEvent;

        /// <summary>
        /// Constructor
        /// </summary>
        public EnhancedGrid()
        {
            InitializeComponent();
            CellDoubleClick += new DataGridViewCellEventHandler(OnCellDoubleClick);
            CellClick += new DataGridViewCellEventHandler(OnCellClick);
            CellValidating += new DataGridViewCellValidatingEventHandler(OnCellValidating);
            DataError += new DataGridViewDataErrorEventHandler(OnDataError);
            EditingControlShowing += new DataGridViewEditingControlShowingEventHandler(OnEditingControlShowing);
            RowHeadersVisible = false;

            // Populate the context menu.
            ToolStripMenuItem CopyItem = (ToolStripMenuItem)PopupMenu.Items.Add("Copy");
            CopyItem.ShortcutKeys = Keys.Control | Keys.C;
            CopyItem.Click += new EventHandler(OnCopy);

            ToolStripMenuItem PasteItem = (ToolStripMenuItem)PopupMenu.Items.Add("Paste");
            PasteItem.ShortcutKeys = Keys.Control | Keys.V;
            PasteItem.Click += new EventHandler(OnPaste);

            ToolStripMenuItem DeleteItem = (ToolStripMenuItem)PopupMenu.Items.Add("Delete");
            DeleteItem.ShortcutKeys = Keys.Delete;
            DeleteItem.Click += new EventHandler(OnDelete);

            PopupMenu.Items.Add(new ToolStripSeparator());

            ToolStripMenuItem MoveDownItem = (ToolStripMenuItem)PopupMenu.Items.Add("Move down");
            MoveDownItem.ShortcutKeys = Keys.Control | Keys.Down;
            MoveDownItem.Click += new EventHandler(OnMoveDown);

            ToolStripMenuItem MoveUpItem = (ToolStripMenuItem)PopupMenu.Items.Add("Move up");
            MoveUpItem.ShortcutKeys = Keys.Control | Keys.Up;
            MoveUpItem.Click += new EventHandler(OnMoveUp);
            ContextMenuStrip = PopupMenu;
        }

        /// <summary>
        /// The edit control (e.g. combo box) is about to be shown. If we added a temporary item
        /// to the combo control (so that the grid doesn't reject a value) then remove it now so
        /// that the user doesn't see it. It will be added in later (OnCellValidating) when the
        /// user has finished editing.
        /// </summary>
        private void OnEditingControlShowing(object Sender, DataGridViewEditingControlShowingEventArgs e)
        {
            ContextMenuStrip = null;
            if (CurrentCell is MyDataGridViewComboBoxCell && EditingControl is DataGridViewComboBoxEditingControl)
            {
                DataGridViewComboBoxEditingControl Combo = (DataGridViewComboBoxEditingControl)EditingControl;
                Combo.DropDownStyle = ComboBoxStyle.DropDown;
                Combo.AutoCompleteMode = AutoCompleteMode.None;
                Combo.KeyDown -= new KeyEventHandler(OnComboKeyDown);
                Combo.KeyDown += new KeyEventHandler(OnComboKeyDown);
                Combo.Leave -= new EventHandler(OnComboLeave);
                Combo.Leave += new EventHandler(OnComboLeave);
                MyDataGridViewComboBoxCell MyCombo = (MyDataGridViewComboBoxCell)CurrentCell;
                if (MyCombo.ValueInserted)
                {
                    string SavedText = Combo.Text;
                    Combo.Items.RemoveAt(0);
                    Combo.Text = SavedText;
                }
            }
        }

        /// <summary>
        /// Need to make sure that when the user stops editing a cell, a context menu is still
        /// attached to both the cell and the grid. The ContextMenu is removed in the 
        /// OnEditingControlShowing method above, so we need to add it back in here.
        /// </summary>
        protected override void OnCellEndEdit(DataGridViewCellEventArgs e)
        {
            base.OnCellEndEdit(e);
            CurrentCell.ContextMenuStrip = PopupMenu;
            ContextMenuStrip = PopupMenu;
        }

        /// <summary>
        /// Need to stop the occasional error message.
        /// </summary>
        void OnDataError(object sender, DataGridViewDataErrorEventArgs e)
        {
            e.Cancel = false;
        }

        /// <summary>
        /// Need to trap the case when the user clicks away on the ComboBox.
        /// </summary>
        void OnComboLeave(object sender, EventArgs e)
        {
            DataGridViewComboBoxEditingControl Combo = (DataGridViewComboBoxEditingControl)EditingControl;
            if (Combo != null && CurrentCell.Value.ToString() != Combo.Text)
            {
                string SavedValue = Combo.Text;
                Combo.Leave -= new EventHandler(OnComboLeave);
                CurrentCell.Value = SavedValue;
                Combo.Leave += new EventHandler(OnComboLeave);
            }
        }

        /// <summary>
        /// We need to stop the Tab key. If the user has a combo box dropped down and types a value into
        /// the edit part of the combo box and then presses TAB, a nasty exception occurs. This method
        /// stops that happening.
        /// 
        /// UPDATE 14-9-12 JF
        /// Changed this to a try-catch as the problem occurs with other keys. see task 1586.
        /// </summary>
        protected override bool ProcessDialogKey(Keys keyData)
        {
            try
            {
                if (keyData == Keys.Tab)
                    return true;
                else
                    return base.ProcessDialogKey(keyData);
            }
            catch (Exception)
            {
                return true;
            }
        }

        /// <summary>
        /// Need to trap the ENTER key otherwise the combo drop down doesn't close.
        /// </summary>
        void OnComboKeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Enter)
            {
                DataGridViewComboBoxEditingControl Combo = (DataGridViewComboBoxEditingControl)EditingControl;
                if (Combo != null)
                {
                    // An EndEdit will trigger a Leave event on the ComboBox. We don't want to trap that
                    // so disable the event.
                    Combo.Leave -= new EventHandler(OnComboLeave);

                    string SavedValue = Combo.Text;
                    EndEdit();
                    CurrentCell.Value = SavedValue;
                    this.ProcessEnterKey(Keys.Enter);
                }
            }
        }


        /// <summary>
        /// This property should be used instead of the DataSource property.
        /// </summary>
        public DataTable DataSourceTable
        {
            get
            {
                return _DataSourceTable;
            }
            set
            {
                _DataSourceTable = value;
                PopulateGrid();
            }
        }

        /// <summary>
        /// Fill the grid with data from the DataSourceTable
        /// </summary>
        public void PopulateGrid()
        {
            if (DataSourceTable != null)
            {
                InRefresh = true;

                ContextMenuStrip = PopupMenu;

                // Make sure we have the right number of columns.
                ColumnCount = Math.Max(DataSourceTable.Columns.Count, 1);

                // Populate the grid headers.
                for (int Col = 0; Col < DataSourceTable.Columns.Count; Col++)
                {
                    Columns[Col].HeaderText = DataSourceTable.Columns[Col].ColumnName;
                    Columns[Col].DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight;
                    Columns[Col].SortMode = DataGridViewColumnSortMode.NotSortable;
                }

                // Populate the grid cells with new rows.
                Rows.Clear();
                for (int Row = 0; Row < DataSourceTable.Rows.Count; Row++)
                {
                    DataGridViewRow NewRow = new DataGridViewRow();
                    for (int Col = 0; Col < DataSourceTable.Columns.Count; Col++)
                    {
                        NewRow.Cells.Add(new DataGridViewTextBoxCell());
                        NewRow.Cells[Col].Value = DataSourceTable.Rows[Row][Col];
                        NewRow.ContextMenuStrip = PopupMenu;
                    }

                    if (AddingNewRowEvent != null)
                        AddingNewRowEvent.Invoke(NewRow);

                    Rows.Add(NewRow);
                }

                InRefresh = false;
            }
            else
                Columns.Clear();
        }

        public void EndEditMode()
        {
            if (IsCurrentCellInEditMode)
            {
                if (EditingControl != null)
                    CurrentCell.DetachEditingControl();

                if (IsCurrentCellInEditMode)
                    EndEdit();
            }
        }
        /// <summary>
        /// This method will create a combo drop down, with the specified ComboItems, in the specified cell.
        /// </summary>
        public void CreateComboInCell(DataGridViewCell Cell, string[] ComboItems)
        {
            MyDataGridViewComboBoxCell Combo = new MyDataGridViewComboBoxCell();
            Combo.Items.AddRange(ComboItems);
            ReplaceCell(Cell, Combo);
        }

        /// <summary>
        /// This method will create a date drop downin the specified cell.
        /// </summary>
        public void CreateDateDropDownInCell(DataGridViewCell Cell)
        {
            CalendarCell DateEditor = new CalendarCell();
            ReplaceCell(Cell, DateEditor);
        }

        /// <summary>
        /// This method will create a date drop downin the specified cell.
        /// </summary>
        public void CreateButtonInCell(DataGridViewCell Cell)
        {
            DataGridViewButtonCell Button = new DataGridViewButtonCell();
            ReplaceCell(Cell, Button);
        }

        /// <summary>
        /// Replace the specified OldCell with the specified NewCell
        /// </summary>
        private static void ReplaceCell(DataGridViewCell OldCell, DataGridViewCell NewCell)
        {
            int i = OldCell.OwningRow.Cells.IndexOf(OldCell);

            object OldValue = OldCell.Value;
            OldCell.OwningRow.Cells.Insert(i, NewCell);
            OldCell.OwningRow.Cells[i].Value = OldValue;
            OldCell.OwningRow.Cells.RemoveAt(i + 1);
        }

        /// <summary>
        /// Validate the cell contents to make sure it is ok.
        /// </summary>
        void OnCellValidating(object sender, DataGridViewCellValidatingEventArgs e)
        {
            //if (!InRefresh && e.FormattedValue.ToString() != "" && 
            //    DataSourceTable != null && DataSourceTable.Columns[e.ColumnIndex].DataType == typeof(double))
            //   {
            //   double Value;
            //   e.Cancel = !double.TryParse(e.FormattedValue.ToString(), out Value);
            //   if (e.Cancel)
            //      MessageBox.Show("Cannot convert " + e.FormattedValue + " to a real number.");
            //   }
        }

        /// <summary>
        /// We need to trap the end of an edit and write the data back to the 
        /// table.
        /// </summary>
        protected override void OnCellValueChanged(DataGridViewCellEventArgs e)
        {
            base.OnCellValueChanged(e);
            if (DataSourceTable != null && !InRefresh && e.RowIndex != -1 && e.ColumnIndex != -1)
            {
                try
                {
                    int Col = e.ColumnIndex;
                    int Row = e.RowIndex;

                    if (Row > DataSourceTable.Rows.Count - 1)
                        DataSourceTable.Rows.Add(DataSourceTable.NewRow());

                    // Make sure this row has our popup menu.
                    if (Rows[e.RowIndex].ContextMenuStrip == null)
                        Rows[e.RowIndex].ContextMenuStrip = PopupMenu;
                    ContextMenuStrip = PopupMenu;

                    if (DataSourceTable.Columns[Col].DataType == typeof(double) &&
                        !MathUtility.IsNumerical(Rows[Row].Cells[Col].Value.ToString()))
                    {
                        // Turn the column into a string column.

                        // Capture state of existing column and remove it from DataTable.
                        string ColumnName = DataSourceTable.Columns[Col].ColumnName;
                        string[] Values = GridUtility.GetColumnAsStringsUsingCellFormat(this, Col);
                        int Ordinal = DataSourceTable.Columns[Col].Ordinal;
                        DataSourceTable.Columns.RemoveAt(Col);

                        // Create a new column of string type.
                        DataColumn NewColumn = DataSourceTable.Columns.Add(ColumnName, typeof(string));
                        NewColumn.SetOrdinal(Ordinal);
                        DataTableUtility.AddColumn(DataSourceTable, ColumnName, Values);
                    }
                    DataSourceTable.Rows[Row][Col] = Rows[Row].Cells[Col].Value;
                    ChangedColumnNames.Add(DataSourceTable.Columns[Col].ColumnName);

                    if (TableColumnChangedEvent != null)
                        TableColumnChangedEvent.Invoke(ChangedColumnNames);
                    ChangedColumnNames.Clear();
                }
                catch (Exception)
                { }  // This can happen when the user puts a text value into a numeric column.
            }
        }

        /// <summary>
        /// We need to trap the on click to put the cell into edit mode and drop
        /// down the combo box if it is a combo cell.
        /// </summary>
        void OnCellClick(object sender, DataGridViewCellEventArgs e)
        {
            if (CurrentCell is DataGridViewComboBoxCell)
            {
                BeginEdit(false);
                if (EditingControl is DataGridViewComboBoxEditingControl)
                {
                    DataGridViewComboBoxEditingControl combo = (DataGridViewComboBoxEditingControl)EditingControl;
                    combo.DroppedDown = true;
                }
            }
        }

        /// <summary>
        /// User has double clicked a cell. Enter edit mode.
        /// </summary>
        void OnCellDoubleClick(object sender, DataGridViewCellEventArgs e)
        {
            BeginEdit(false);
        }

        /// <summary>
        /// User has hit delete. Clear contents of selected cells.
        /// </summary>
        private void OnDelete(object sender, EventArgs e)
        {
            try
            {
                if (!IsCurrentCellInEditMode)
                {
                    InRefresh = true;

                    List<string> ColumnsChanged = new List<string>();
                    SaveSelection();

                    // Make the values in the grid & table null.
                    foreach (Point Cell in SavedSelections)
                    {
                        int Col = Cell.X;
                        int Row = Cell.Y;
                        if (Row < Rows.Count - 1)
                        {
                            Rows[Row].Cells[Col].Value = null;
                            if (DataSourceTable != null)
                                DataSourceTable.Rows[Row][Col] = Rows[Row].Cells[Col].Value;
                            if (!ColumnsChanged.Contains(Columns[Col].HeaderText))
                                ColumnsChanged.Add(Columns[Col].HeaderText);
                        }
                    }
                    InRefresh = false;

                    // Now invoke the table column changed event.
                    if (TableColumnChangedEvent != null)
                        TableColumnChangedEvent.Invoke(ColumnsChanged);
                }
            }
            catch (Exception err)
            {
                MessageBox.Show(err.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        /// <summary>
        /// User has hit Copy. Copy contents of selected cells to the clipboard
        /// </summary>
        private void OnCopy(object sender, EventArgs e)
        {
            try
            {
                // Add the selection to the clipboard.
                Clipboard.SetDataObject(GetClipboardContent());
            }
            catch (Exception err)
            {
                MessageBox.Show(err.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        /// <summary>
        /// User has hit paste. Paste the contents of the clipboard into the grid.
        /// </summary>
        private void OnPaste(object sender, EventArgs e)
        {
            try
            {
                List<string> ColumnsChanged = Paste(Clipboard.GetText(), CurrentCell, true);
                if (ColumnsChanged.Count > 0 && TableColumnChangedEvent != null)
                    TableColumnChangedEvent.Invoke(ColumnsChanged);
            }
            catch (Exception err)
            {
                MessageBox.Show(err.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        /// <summary>
        /// Internal method to paste the specified contents into the specified cell of the grid. 
        /// DataTable event is not invoked.
        /// </summary>
        private List<string> Paste(string Contents, DataGridViewCell CellToPasteInto, bool stripTrailer)
        {
            InRefresh = true;
            List<string> ColumnsChanged = new List<string>();
            try
            {
                string[] Lines = Contents.Split('\n');
                int Row = CellToPasteInto.RowIndex;
                int ColBase = CellToPasteInto.ColumnIndex;
                bool addingAllowed = AllowUserToAddRows;  
                AllowUserToAddRows = false; // Having this turned on mucks up the programmatic addition of new rows. They get inserted at the wrong spot.
                DataGridViewCell Cell;
                try
                {
                    for (int iLine = 0; iLine < Lines.Length; iLine++)
                    {
                        string Line = Lines[iLine];
                        if (stripTrailer && Line.Length == 0 && iLine == Lines.Length - 1) // If we have an empty string at the end (thank you, Excel), ignore it.
                            break;
                        string[] LineBits = Line.Split('\t');
                        for (int i = 0; i < LineBits.GetLength(0); ++i)
                        {
                            LineBits[i] = LineBits[i].Replace("\r", "");
                            int Col = ColBase + i;
                            if (Col < ColumnCount)
                            {
                                while (Row >= RowCount)
                                    RowCount++;
                                Cell = this[Col, Row];
                                if (!Cell.ReadOnly)
                                {
                                    if (Cell.Value == null || Cell.Value.ToString() != LineBits[i])
                                    {
                                        // Add value to grid.
                                        if (LineBits[i] == "")
                                            Cell.Value = DBNull.Value;
                                        else
                                            Cell.Value = Convert.ChangeType(LineBits[i], Cell.ValueType);

                                        // Add value to DataTable.
                                        if (DataSourceTable != null)
                                        {
                                            if (Row >= DataSourceTable.Rows.Count)
                                                DataSourceTable.Rows.Add(DataSourceTable.NewRow());
                                            DataSourceTable.Rows[Row][Col] = Cell.Value;
                                        }

                                        // Store column name for later invoking of TableColumnChanged event.
                                        if (!ColumnsChanged.Contains(Columns[Col].HeaderText))
                                            ColumnsChanged.Add(Columns[Col].HeaderText);

                                    }
                                }
                            }
                            else
                                break;
                        }
                        Row++;
                    }
                }
                finally
                {
                    AllowUserToAddRows = addingAllowed; // Restore the original value
                }
            }
            catch (Exception err)
            {
                MessageBox.Show(err.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
            ContextMenuStrip = PopupMenu;
            InRefresh = false;
            return ColumnsChanged;
        }

        /// <summary>
        /// User has hit move down. Move contents of selected cells down one row.
        /// </summary>
        private void OnMoveDown(object sender, EventArgs e)
        {
            try
            {
                // Save the current selection
                SaveSelection();

                // Work out the dimensions of the current selection.
                int StartRow, StartCol, EndRow, EndCol;
                CalcStartEndColAndRow(out StartRow, out StartCol, out EndRow, out EndCol);

                if (EndRow < RowCount - 1)
                {
                    // Get the contents of the current selection
                    string ContentsToMoveDown = GetClipboardContent().GetText();
                    // no readonly cells now in source selection
                    for (int row = StartRow; row <= EndRow; row++)
                    {
                        for (int Col = StartCol; Col <= EndCol; Col++)
                            Rows[row].Cells[Col].ReadOnly = false;
                    }

                    // Get the contents of the row below the current selection.
                    ClearSelection();
                    for (int Col = StartCol; Col <= EndCol; Col++)
                        Rows[EndRow + 1].Cells[Col].Selected = true;
                    string ContentsToPutInStartRow = GetClipboardContent().GetText();

                    // Paste the contents into the row below.
                    List<string> ColumnsChanged = Paste(ContentsToMoveDown, this.Rows[StartRow + 1].Cells[StartCol], false);

                    // Paste the bit we replaced into the start row.
                    Paste(ContentsToPutInStartRow, this.Rows[StartRow].Cells[StartCol], false);

                    // Now invoke the table column changed event.
                    if (TableColumnChangedEvent != null)
                        TableColumnChangedEvent.Invoke(ColumnsChanged);

                    // Restore the selection but down a row.
                    RestoreSelection(1);
                }
            }
            catch (Exception err)
            {
                MessageBox.Show(err.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        /// <summary>
        /// User has hit move up. Move contents of selected cells up one row.
        /// </summary>
        private void OnMoveUp(object sender, EventArgs e)
        {
            try
            {
                // Save the current selection
                SaveSelection();

                // Work out the dimensions of the current selection.
                int StartRow, StartCol, EndRow, EndCol;
                CalcStartEndColAndRow(out StartRow, out StartCol, out EndRow, out EndCol);

                if (StartRow > 0)
                {
                    // Get the contents of the current selection
                    string ContentsToMoveUp = GetClipboardContent().GetText();
                    // no readonly cells now in source selection
                    for (int row = StartRow; row <= EndRow; row++)
                    {
                        for (int Col = StartCol; Col <= EndCol; Col++)
                            Rows[row].Cells[Col].ReadOnly = false;
                    }

                    // Get the contents of the row above the current selection.
                    ClearSelection();
                    for (int Col = StartCol; Col <= EndCol; Col++)
                        Rows[StartRow - 1].Cells[Col].Selected = true;
                    string ContentsToPutInEndRow = GetClipboardContent().GetText();

                    // Paste the contents into the row below.
                    List<string> ColumnsChanged = Paste(ContentsToMoveUp, this.Rows[StartRow - 1].Cells[StartCol], false);

                    // Paste the bit we replaced into the end row.
                    Paste(ContentsToPutInEndRow, this.Rows[EndRow].Cells[StartCol], false);

                    // Now invoke the table column changed event.
                    if (TableColumnChangedEvent != null)
                        TableColumnChangedEvent.Invoke(ColumnsChanged);

                    // Restore the selection but up a row.
                    RestoreSelection(-1);
                }
            }
            catch (Exception err)
            {
                MessageBox.Show(err.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        /// <summary>
        /// Calculate the start and end column and row for the current selection.
        /// </summary>
        private void CalcStartEndColAndRow(out int StartRow, out int StartCol, out int EndRow, out int EndCol)
        {
            // Get the start and end row and column.
            StartRow = 10000;
            StartCol = 10000;
            EndRow = -1;
            EndCol = -1;
            for (int i = 0; i < SelectedCells.Count; i++)
            {
                int Row = SelectedCells[i].RowIndex;
                int Col = SelectedCells[i].ColumnIndex;
                StartRow = Math.Min(StartRow, Row);
                EndRow = Math.Max(EndRow, Row);
                StartCol = Math.Min(StartCol, Col);
                EndCol = Math.Max(EndCol, Col);
            }
        }

        /// <summary>
        /// Save the current cell selections
        /// </summary>
        public void SaveSelection()
        {
            SavedSelections.Clear();
            foreach (DataGridViewCell Cell in SelectedCells)
                SavedSelections.Add(new Point(Cell.ColumnIndex, Cell.RowIndex));
        }

        /// <summary>
        /// Restore from a previously saved cell selections
        /// </summary>
        public void RestoreSelection(int YModifier)
        {
            ClearSelection();
            foreach (Point Cell in SavedSelections)
            {
                if (Cell.Y < Rows.Count && Cell.X < Columns.Count)
                    Rows[Cell.Y + YModifier].Cells[Cell.X].Selected = true;
            }
        }

        /// <summary>
        /// Remove blank rows from the bottom of the grid.
        /// </summary>
        public void RemoveBlankRows()
        {
            do
            {
                int Row = RowCount - 1;
                foreach (DataGridViewCell Cell in Rows[Row].Cells)
                {
                    if (Cell.Value != null)
                        return;
                }
                Rows.RemoveAt(Row);
            }
            while (RowCount > 0);
        }

        /// <summary>
        /// Create and fill a datatable from the grid.
        /// </summary>
        /// <returns></returns>
        public DataTable ToTable()
        {
            DataTable Table = new DataTable();

            for (int Col = 0; Col < Columns.Count; Col++)
            {
                string ColumnName = Columns[Col].HeaderText.Replace("\r\n", " ");
                string[] Values = GridUtility.GetColumnAsStrings(this, Col);
                if (MathUtility.IsNumerical(Values))
                    DataTableUtility.AddColumn(Table, ColumnName, MathUtility.StringsToDoubles(Values));
                else
                    DataTableUtility.AddColumn(Table, ColumnName, Values);
            }

            return Table;
        }

    }
}
