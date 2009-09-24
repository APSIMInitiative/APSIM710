<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class ScriptUI
    Inherits Controllers.BaseView

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing AndAlso components IsNot Nothing Then
            components.Dispose()
        End If
        MyBase.Dispose(disposing)
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(ScriptUI))
        Me.TabControl = New System.Windows.Forms.TabControl
        Me.Properties = New System.Windows.Forms.TabPage
        Me.Script = New System.Windows.Forms.TabPage
        Me.TextBox = New QWhale.Editor.SyntaxEdit(Me.components)
        Me.CsParser = New QWhale.Syntax.Parsers.CsParser
        Me.VbParser = New QWhale.Syntax.Parsers.VbParser
        Me.GenericUI = New VBUserInterface.GenericUI
        Me.ToolStrip1 = New System.Windows.Forms.ToolStrip
        Me.FindReplaceButton = New System.Windows.Forms.ToolStripButton
        Me.TabControl.SuspendLayout()
        Me.Properties.SuspendLayout()
        Me.Script.SuspendLayout()
        Me.ToolStrip1.SuspendLayout()
        Me.SuspendLayout()
        '
        'TabControl
        '
        Me.TabControl.Controls.Add(Me.Properties)
        Me.TabControl.Controls.Add(Me.Script)
        Me.TabControl.Dock = System.Windows.Forms.DockStyle.Fill
        Me.TabControl.Location = New System.Drawing.Point(0, 16)
        Me.TabControl.Name = "TabControl"
        Me.TabControl.SelectedIndex = 0
        Me.TabControl.Size = New System.Drawing.Size(655, 525)
        Me.TabControl.TabIndex = 2
        '
        'Properties
        '
        Me.Properties.Controls.Add(Me.GenericUI)
        Me.Properties.Location = New System.Drawing.Point(4, 22)
        Me.Properties.Name = "Properties"
        Me.Properties.Padding = New System.Windows.Forms.Padding(3)
        Me.Properties.Size = New System.Drawing.Size(647, 499)
        Me.Properties.TabIndex = 0
        Me.Properties.Text = "Properties"
        Me.Properties.UseVisualStyleBackColor = True
        '
        'Script
        '
        Me.Script.Controls.Add(Me.TextBox)
        Me.Script.Controls.Add(Me.ToolStrip1)
        Me.Script.Location = New System.Drawing.Point(4, 22)
        Me.Script.Name = "Script"
        Me.Script.Padding = New System.Windows.Forms.Padding(3)
        Me.Script.Size = New System.Drawing.Size(647, 499)
        Me.Script.TabIndex = 1
        Me.Script.Text = "Script"
        Me.Script.UseVisualStyleBackColor = True
        '
        'TextBox
        '
        Me.TextBox.BackColor = System.Drawing.SystemColors.Window
        Me.TextBox.Cursor = System.Windows.Forms.Cursors.IBeam
        Me.TextBox.Dock = System.Windows.Forms.DockStyle.Fill
        Me.TextBox.Font = New System.Drawing.Font("Courier New", 10.0!)
        Me.TextBox.Gutter.Options = CType((((QWhale.Editor.GutterOptions.PaintLineNumbers Or QWhale.Editor.GutterOptions.PaintLinesOnGutter) _
                    Or QWhale.Editor.GutterOptions.PaintBookMarks) _
                    Or QWhale.Editor.GutterOptions.PaintLineModificators), QWhale.Editor.GutterOptions)
        Me.TextBox.Location = New System.Drawing.Point(3, 28)
        Me.TextBox.Name = "TextBox"
        Me.TextBox.Size = New System.Drawing.Size(641, 468)
        Me.TextBox.TabIndex = 0
        Me.TextBox.Text = ""
        '
        'CsParser
        '
        Me.CsParser.DefaultState = 0
        Me.CsParser.Options = CType(((((((QWhale.Syntax.SyntaxOptions.Outline Or QWhale.Syntax.SyntaxOptions.SmartIndent) _
                    Or QWhale.Syntax.SyntaxOptions.CodeCompletion) _
                    Or QWhale.Syntax.SyntaxOptions.SyntaxErrors) _
                    Or QWhale.Syntax.SyntaxOptions.AutoComplete) _
                    Or QWhale.Syntax.SyntaxOptions.FormatCase) _
                    Or QWhale.Syntax.SyntaxOptions.FormatSpaces), QWhale.Syntax.SyntaxOptions)
        Me.CsParser.XmlScheme = resources.GetString("CsParser.XmlScheme")
        '
        'VbParser
        '
        Me.VbParser.DefaultState = 0
        Me.VbParser.Options = CType(((((((((QWhale.Syntax.SyntaxOptions.Outline Or QWhale.Syntax.SyntaxOptions.SmartIndent) _
                    Or QWhale.Syntax.SyntaxOptions.CodeCompletion) _
                    Or QWhale.Syntax.SyntaxOptions.SyntaxErrors) _
                    Or QWhale.Syntax.SyntaxOptions.ReparseOnLineChange) _
                    Or QWhale.Syntax.SyntaxOptions.AutoComplete) _
                    Or QWhale.Syntax.SyntaxOptions.FormatCase) _
                    Or QWhale.Syntax.SyntaxOptions.FormatSpaces) _
                    Or QWhale.Syntax.SyntaxOptions.CodeCompletionTabs), QWhale.Syntax.SyntaxOptions)
        Me.VbParser.XmlScheme = resources.GetString("VbParser.XmlScheme")
        '
        'GenericUI
        '
        Me.GenericUI.AutoScroll = True
        Me.GenericUI.BackColor = System.Drawing.SystemColors.Window
        Me.GenericUI.Dock = System.Windows.Forms.DockStyle.Fill
        Me.GenericUI.HelpText = ""
        Me.GenericUI.Location = New System.Drawing.Point(3, 3)
        Me.GenericUI.Name = "GenericUI"
        Me.GenericUI.Size = New System.Drawing.Size(641, 493)
        Me.GenericUI.TabIndex = 0
        '
        'ToolStrip1
        '
        Me.ToolStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.FindReplaceButton})
        Me.ToolStrip1.Location = New System.Drawing.Point(3, 3)
        Me.ToolStrip1.Name = "ToolStrip1"
        Me.ToolStrip1.Size = New System.Drawing.Size(641, 25)
        Me.ToolStrip1.TabIndex = 1
        Me.ToolStrip1.Text = "ToolStrip1"
        '
        'FindReplaceButton
        '
        Me.FindReplaceButton.Image = CType(resources.GetObject("FindReplaceButton.Image"), System.Drawing.Image)
        Me.FindReplaceButton.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.FindReplaceButton.Name = "FindReplaceButton"
        Me.FindReplaceButton.Size = New System.Drawing.Size(89, 22)
        Me.FindReplaceButton.Text = "Find/Replace"
        '
        'ScriptUI
        '
        Me.Controls.Add(Me.TabControl)
        Me.Name = "ScriptUI"
        Me.Controls.SetChildIndex(Me.MyHelpLabel, 0)
        Me.Controls.SetChildIndex(Me.TabControl, 0)
        Me.TabControl.ResumeLayout(False)
        Me.Properties.ResumeLayout(False)
        Me.Script.ResumeLayout(False)
        Me.Script.PerformLayout()
        Me.ToolStrip1.ResumeLayout(False)
        Me.ToolStrip1.PerformLayout()
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents TabControl As System.Windows.Forms.TabControl
    Friend WithEvents Properties As System.Windows.Forms.TabPage
    Friend WithEvents Script As System.Windows.Forms.TabPage
    Friend WithEvents GenericUI As VBUserInterface.GenericUI
    Friend WithEvents TextBox As QWhale.Editor.SyntaxEdit
    Friend WithEvents CsParser As QWhale.Syntax.Parsers.CsParser
    Friend WithEvents VbParser As QWhale.Syntax.Parsers.VbParser
    Friend WithEvents ToolStrip1 As System.Windows.Forms.ToolStrip
    Friend WithEvents FindReplaceButton As System.Windows.Forms.ToolStripButton

End Class
