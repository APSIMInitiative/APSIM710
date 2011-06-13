Imports System
Imports System.IO
Imports System.Collections.Specialized


Public Class BaseView
    ' ----------------------------------
    ' Base class for all user interfaces
    ' It has data and knowledge of a
    ' user interface.
    ' ----------------------------------   
    Inherits System.Windows.Forms.UserControl

   Protected _Controller As BaseController
    Protected MyNodePath As String
    Protected Data As Xml.XmlNode

    Delegate Sub NotifyEventHandler(ByVal NodeChanged As BaseView)
    Public Event ViewChanged As NotifyEventHandler



#Region " Windows Form Designer generated code "
    Public Sub New()
        MyBase.New()
        InitializeComponent()
        HelpText = ""
    End Sub

    Protected Overloads Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing Then
            If Not (components Is Nothing) Then
                components.Dispose()
            End If
        End If
        MyBase.Dispose(disposing)
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer
    Protected WithEvents MyHelpLabel As System.Windows.Forms.Label

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
      Me.MyHelpLabel = New System.Windows.Forms.Label
      Me.SuspendLayout()
      '
      'MyHelpLabel
      '
      Me.MyHelpLabel.BackColor = System.Drawing.SystemColors.ActiveCaption
      Me.MyHelpLabel.Dock = System.Windows.Forms.DockStyle.Top
      Me.MyHelpLabel.Font = New System.Drawing.Font("Tahoma", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
      Me.MyHelpLabel.ForeColor = System.Drawing.SystemColors.ActiveCaptionText
      Me.MyHelpLabel.Location = New System.Drawing.Point(0, 0)
      Me.MyHelpLabel.Name = "MyHelpLabel"
      Me.MyHelpLabel.Size = New System.Drawing.Size(655, 40)
      Me.MyHelpLabel.TabIndex = 1
      Me.MyHelpLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
      Me.MyHelpLabel.Visible = False
      '
      'BaseView
      '
      Me.AutoScroll = True
      Me.BackColor = System.Drawing.SystemColors.Window
      Me.Controls.Add(Me.MyHelpLabel)
      Me.Name = "BaseView"
      Me.Size = New System.Drawing.Size(655, 541)
      Me.ResumeLayout(False)

   End Sub
#End Region

    Public Overloads Sub OnLoad(ByVal Controller As BaseController, _
                                ByVal NodePath As String, _
                                ByVal Contents As String)
      _Controller = Controller
        MyNodePath = NodePath

        Dim Doc As Xml.XmlDocument = New Xml.XmlDocument
        Doc.LoadXml(Contents)
        Data = Doc.DocumentElement
        OnLoad()
    End Sub
    Public Function GetData() As String
        If IsNothing(Data) Then
            Return ""
        Else
            Return Data.OuterXml
        End If
    End Function
    Public ReadOnly Property NodePath() As String
        Get
            Return MyNodePath
        End Get
    End Property

   Public ReadOnly Property Controller() As BaseController
      Get
         Return _Controller
      End Get
   End Property


    Protected Overridable Overloads Sub OnLoad()
        ' ---------------------------------------------
        ' An overridable method that is called whenever
        ' the object has just been loaded.
        ' ---------------------------------------------
    End Sub

    Public Overridable Sub OnRefresh()
        ' ---------------------------------------------
        ' An overridable method that is called whenever
        ' the object needs to be refreshed.
        ' ---------------------------------------------
    End Sub

    Public Overridable Sub OnSave()
        ' ---------------------------------------------
        ' An overridable method that is called whenever
        ' data should be saved back to the APSIMData 
        ' instance.
        ' ---------------------------------------------
    End Sub
    Public Overridable Function OnDropData(ByVal SourcePaths As StringCollection, ByVal FullXML As String) As Boolean
        ' ---------------------------------------------
        ' An overridable method that is called whenever
        ' a Component tree node in the Factorial Tree
        ' has data dropped on it. 
        ' default behaviour is to return false and let
        ' the FactorTree look after the logic
        ' The Factor View needs the event to add Targets 
        ' automatically when the first component is dropped
        ' ---------------------------------------------
        Return False
    End Function

    Protected Sub PublishViewChanged()
        ' ---------------------------------------------
        ' This is used by the graph system.
        ' ---------------------------------------------
        RaiseEvent ViewChanged(Me)
    End Sub

    Public Overridable Sub OnClose()
        ' ---------------------------------------------
        ' An overridable method that is called whenever
        ' the object is being destroyed.
        ' ---------------------------------------------
    End Sub

    Public Property HelpText() As String
        ' ---------------------------------------------
        ' Provide access to the help label of this ui
        ' ---------------------------------------------
        Get
            Return MyHelpLabel.Text
        End Get
        Set(ByVal Value As String)
            MyHelpLabel.Text = Value
            MyHelpLabel.Visible = Value <> ""
            Dim s As New Drawing.Size(MyHelpLabel.Size.Width, 100)
            MyHelpLabel.Height = MyHelpLabel.GetPreferredSize(s).Height
        End Set
    End Property

    Public Overridable Sub PrintPage(ByVal MarginBounds As Rectangle, ByVal Graphs As Graphics)

    End Sub

    Public Overridable Sub Export(ByVal FileName As String)

    End Sub

End Class
