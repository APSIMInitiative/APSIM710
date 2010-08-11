

Imports System.Collections.Specialized
Imports System.io
Imports System.Reflection
Imports System.Windows.Forms
Imports System.Xml

Imports ApsimFile
Imports CSGeneral


Public Class BaseController
    ' Simple base class for a user interface manager

    Private MySelectedData As New StringCollection
    Private ActionFile As XmlNode
    Private MyExplorer As ExplorerUI
    Private MyMainForm As Form
    Private LargeIcons As ImageList = Nothing
    Private MediumIcons As ImageList = Nothing
    Private SmallIcons As ImageList = Nothing
    Private ImageIndexes As List(Of Integer) = Nothing
    Private ImageFileNames As List(Of String) = Nothing
    Public ApsimData As ApsimFile.ApsimFile

    Delegate Sub NotifyEventHandler()
    Delegate Sub SelectionChangedHandler(ByVal OldSelections As StringCollection, ByVal NewSelections As StringCollection)

    Public Event BeforeSaveEvent As NotifyEventHandler              ' Fired immediately before data is saved.
    Public Event AfterSaveEvent As NotifyEventHandler               ' Fired immediately after data is saved.
    Public Event SelectionChangingEvent As NotifyEventHandler       ' Fired when the current selection is about to change.
    Public Event SelectionChangedEvent As SelectionChangedHandler   ' Fired when the current selection has changed.


    Sub New(ByVal MainForm As Form, ByVal SectionName As String, ByVal MainController As Boolean)
        ' -----------------------
        ' constructor
        ' -----------------------
        MyMainForm = MainForm

        ApsimFile.Configuration.Instance.ApplicationName = SectionName
        ApsimData = New ApsimFile.ApsimFile()

        ' Setup the image lists.
        LargeIcons = New ImageList()
        MediumIcons = New ImageList()
        SmallIcons = New ImageList()
        LargeIcons.ImageSize = New Size(32, 32)
        MediumIcons.ImageSize = New Size(24, 24)
        SmallIcons.ImageSize = New Size(16, 16)
        LargeIcons.Tag = "LargeIcon"
        MediumIcons.Tag = "MediumIcon"
        SmallIcons.Tag = "SmallIcon"
        ImageFileNames = New List(Of String)
        ImageIndexes = New List(Of Integer)

        ' Setup the types
        For Each FileName As String In Types.Instance.SmallIconFileNames
            LoadIcon(FileName, SmallIcons)
        Next
        For Each FileName As String In Types.Instance.LargeIconFileNames
            LoadIcon(FileName, LargeIcons)
        Next

        ' Setup the actions file and load all images specified by it.
        Dim ActionFileName As String = Configuration.Instance.Setting("ActionFile")
        If ActionFileName <> "" Then
            Dim ActionDoc As New XmlDocument()
            ActionDoc.Load(ActionFileName)
            ActionFile = ActionDoc.DocumentElement
            Dim ActionsNode As XmlNode = XmlHelper.Find(ActionDoc.DocumentElement, "Actions")
            Dim SmallIconFileNames As List(Of String) = XmlHelper.ValuesRecursive(ActionsNode, "SmallIcon")
            For Each IconFileName As String In SmallIconFileNames
                LoadIcon(IconFileName, SmallIcons)
            Next
            Dim MediumIconFileNames As List(Of String) = XmlHelper.ValuesRecursive(ActionsNode, "MediumIcon")
            For Each IconFileName As String In MediumIconFileNames
                LoadIcon(IconFileName, MediumIcons)
            Next

        End If

        If MainController Then
            AddHandler ApsimData.FileNameChanged, AddressOf OnFileNameChanged
        End If
    End Sub
    Public Function CreateUI(ByVal ComponentType As String) As BaseView
        ' -------------------------------------
        ' Create a User interface form for the
        ' specified type.
        ' -------------------------------------
        Dim UIType As String = Types.Instance.MetaData(ComponentType, "UItype")
        If UIType <> "" Then
            Return CreateClass(UIType)
        End If
        Return Nothing
    End Function
    Public ReadOnly Property MainForm() As Form
        Get
            Return MyMainForm
        End Get
    End Property
    Public Property Explorer() As ExplorerUI
        Get
            Return (MyExplorer)
        End Get
        Set(ByVal value As ExplorerUI)
            MyExplorer = value
        End Set
    End Property

#Region "Image handling"
    Private Sub LoadIcon(ByVal ImageFileName As String, ByRef Icons As ImageList)
        ' Load the specified icon into the specified imagelist. The filename
        ' of the icon is stored in the tag field of the bitmap so that we
        ' can look for it later.
        ImageFileName = Configuration.RemoveMacros(ImageFileName)
        If File.Exists(ImageFileName) And ImageFileNames.IndexOf(ImageFileName) = -1 Then
            Dim Icon As New Bitmap(ImageFileName)
            Icons.Images.Add(Icon)

            ImageFileNames.Add(ImageFileName)
            ImageIndexes.Add(Icons.Images.Count - 1)
        End If
    End Sub
    Public Function ImageList(ByVal ImageType As String) As ImageList
        ' Return an image list to caller. ImageType can be SmallIcon,
        ' MediumIcon or LargeIcon.
        If ImageType = "SmallIcon" Then
            Return SmallIcons
        ElseIf ImageType = "MediumIcon" Then
            Return MediumIcons
        Else
            Return LargeIcons
        End If
    End Function
    Public Function ImageIndex(ByVal TypeName As String, ByVal ImageType As String) As Integer
        Dim ImageFileName As String = Types.Instance.MetaData(TypeName, ImageType)
        Return ConvertImageFileNameToIndex(ImageFileName, ImageType)
    End Function
    Public Function ConvertImageFileNameToIndex(ByVal ImageFileName As String, ByVal ImageType As String) As Integer
        ImageFileName = Configuration.RemoveMacros(ImageFileName)
        For I As Integer = 0 To ImageFileNames.Count - 1
            If ImageFileNames(I) = ImageFileName Then
                Return ImageIndexes(I)
            End If
        Next
        Return -1
    End Function

#End Region

#Region "File handling methods"
    Public Function FileSaveAfterPrompt() As Boolean
        ' --------------------------------------------------------
        ' Often called at program exit to optionally prompt the
        ' user to save the current data if something has changed.
        ' --------------------------------------------------------
        Explorer.SaveCurrentView()
        If ApsimData.IsDirty Then
            Dim DoSave As Integer = MessageBox.Show("The current file has changed. Do you want to save it before proceeding?", _
                                                    "Save?", MessageBoxButtons.YesNoCancel, MessageBoxIcon.Question)
            Select Case DoSave
                Case DialogResult.Yes
                    ' Save the file
                    If ApsimData.FileName.Contains("Untitled") Then
                        Dim Dialog As New SaveFileDialog
                        Dialog.Filter = Configuration.Instance.Setting("DialogFilter")
                        Dialog.DefaultExt = Configuration.Instance.Setting("DefaultExtension")
                        Dialog.AddExtension = True
                        Dialog.OverwritePrompt = True
                        If Dialog.ShowDialog = DialogResult.OK Then
                            Explorer.SaveCurrentView()
                            ApsimData.SaveAs(Dialog.FileName)
                            RefreshToolStrips()
                        End If
                    Else
                        ApsimData.Save()
                    End If

                Case DialogResult.No
                    ' Do not save

                Case DialogResult.Cancel
                    ' Cancel pressed.
                    Return False
            End Select
        End If
        Return True
    End Function
    Public Sub LoadPreviousFile()
        For Each PreviousFileName As String In Configuration.Instance.GetFrequentList()
            If File.Exists(PreviousFileName) Then
                ApsimData.OpenFile(PreviousFileName)
                Exit For
            End If
        Next
    End Sub
    Private Sub OnFileNameChanged(ByVal FileName As String)
        If FileName.ToLower <> "untitled" Then
            Configuration.Instance.AddFileToFrequentList(FileName)
        End If
    End Sub
    Public Function ToRelativePath(ByVal FileName As String) As String
        Dim st As String = Configuration.AddMacros(FileName)
        Dim DName As String = Path.GetDirectoryName(ApsimData.FileName)
        If DName <> "" Then
            Return st.Replace(DName + "\", "")
        Else
            Return FileName
        End If
    End Function
    Public Function ToAbsolute(ByVal FileName As String) As String
        If IsNothing(FileName) Then
            Return FileName
        Else
            Dim st As String = Configuration.RemoveMacros(FileName)
            If Path.GetDirectoryName(ApsimData.FileName) <> "" Then
                Dim CWD = Directory.GetCurrentDirectory()
                Directory.SetCurrentDirectory(Path.GetDirectoryName(ApsimData.FileName))
                st = Path.GetFullPath(st)
                Directory.SetCurrentDirectory(CWD)
            End If
            Return st
        End If
    End Function

#End Region

#Region "Node selection methods. NB: Paths are delimited with '/' characters"
    Public Property SelectedPaths() As StringCollection

        Get
            ' --------------------------------------------------------
            ' Provide readwrite access to the current selections.
            ' The strings returned contain data paths to all
            ' selected nodes.
            ' --------------------------------------------------------
            Dim ReturnValues As New StringCollection        'temporary collection used to store selected paths

            'if the selected path is nothing, then set it to the root component in the .apsim file.
            If (IsNothing(MySelectedData)) Then
                Dim temp As New StringCollection
                temp.Add(ApsimData.RootComponent.FullPath)
                SelectedPaths = temp
            End If

            For Each FullPath As String In MySelectedData   'store selected paths in temporary collection
                ReturnValues.Add(FullPath)
            Next

            Return ReturnValues
        End Get

        Set(ByVal Paths As StringCollection)

            Dim OldSelections As New StringCollection       'temporary collection used to store old selected paths

            For Each Selection As String In MySelectedData  'store old selected paths in temporary collection
                OldSelections.Add(Selection)
            Next

            ' Are the new selected paths different from the old selected paths?
            'quick test (based on number of paths)
            Dim Different As Boolean = OldSelections.Count <> Paths.Count
            'exhaustive test 
            If Not Different Then
                For i As Integer = 0 To OldSelections.Count - 1
                    'if Different is set to true once in the loop, make sure Different is permanently set to true.
                    Different = Different Or (OldSelections(i) <> Paths(i))     'does each path in the old selected paths, match their new equivalent in the new selected paths  
                Next
            End If

            ' If they are different
            If Different Then
                RaiseEvent SelectionChangingEvent()
                MySelectedData = Paths              'set "the controllers" selected paths to the new selected paths.
                RaiseEvent SelectionChangedEvent(OldSelections, MySelectedData)     'calls OnSelectionChanged() -> in DataTree.vb and in ExplorerUI.vb 
                RefreshToolStrips()     'IMPORTANT 'See first sub in Action Region below. This region is where all the toolstrips get rebinded after the selection is changed. This first sub is what starts all this rebinding.
            End If
        End Set

    End Property
    Public Property SelectedPath() As String
        Get
            If (MySelectedData.Count = 1) Then
                Return MySelectedData(0)
            Else
                Throw New Exception("Too many nodes selected - expected 1")
            End If
        End Get
        Set(ByVal value As String)
            Dim NewSelections As New StringCollection
            If value <> "" Then
                NewSelections.Add(value)
            End If
            SelectedPaths = NewSelections
        End Set
    End Property
    Public ReadOnly Property Selection() As ApsimFile.Component
        Get
            Return ApsimData.Find(SelectedPath) 'return the corresponding component to the node that was selected.
        End Get
    End Property
#End Region

#Region "Action methods"


    ' --------------------------------------------------------------
    ' This method creates a context menu for a UI then returns it.
    ' The difference between this and ProvideToolStrip() is this does not add itself to the ToolStrips local variable (which is required for RefreshToolStrips).
    ' The context menus belonging to the UI's don't need the same enabling/disabling of actions depening on the current node
    ' that is selected, like the MainToolBar and the DataTree context menu do. They don't need the RefreshToolStrips()
    ' This is because if a different node is selected the UI changes and hence the context menu is no longer relavent for the new UI.
    ' -------------------------------------------------------------

    Public Function CreateUIContextMenu(ByVal ToolStripName As String) As ContextMenuStrip
        Dim Strip As ContextMenuStrip = New ContextMenuStrip
        AddHandler Strip.ItemClicked, AddressOf ActionOnClick           'add an event handler to the entire strip NOT to individual items.
        Strip.Name = ToolStripName
        Dim ToolStripDescriptor As XmlNode = XmlHelper.Find(ActionFile, ToolStripName)
        If Not IsNothing(ToolStripDescriptor) Then
            Dim ImageAboveText As Boolean = False
            ImageAboveText = (XmlHelper.Attribute(ToolStripDescriptor, "ImageAboveText") = "yes")
            PopulateToolStrip(Strip, ToolStripDescriptor, ImageAboveText)
        End If
        Return Strip
    End Function


    ' ------------------------------------------------------------------------
    ' These action methods populate and implement the Context Menu and MainToolBar
    ' ToolStrip in the application. All actions are defined in an actions.xml
    ' file. Each type in types.xml has zero or more actions in it. When the
    ' user clicks on a node in the tree, the context menu will contain those
    ' actions referred to in the types.xml under the type the user clicked on.
    '
    ' By "Context Menu" we mean the DataTree context menu. See section in actions.xml called <ContextMenu>.
    ' Different types of nodes have different actions disabled. So the controller
    ' stores the context menu and simply enables and disables the actions each
    ' time a different node is selected in the datatree. RefreshToolStrips is
    ' called to do this. See above in SelectedPaths() "Set" to see where RefreshToolStrips 
    ' is called when a selection is made. So when the user selects the node by right mouse 
    ' clicking on it, the context menu gets the correct actions enabled/disabled before the
    ' context menu is displayed.
    '
    ' The toolstrip across the top of MainForm works the exact same way. RefreshToolStrips
    ' also is used to enable/disable the actions depending on which node is selected.
    ' For example, the Run button is sometimes disabled.
    ' ------------------------------------------------------------------------

    Private ToolStrips As New List(Of ToolStrip)            'this is the Controllers list of toolstrips. (eg. MainToolBar, ContextMenu)
    'see bottom of Actions.xml for a list of all Toolstrips in ApsimUI. Anything NOT inside the <Actions> tags is a toolstrip.

    'Refresh what actions are enabled/disabled depending on what node in the datatree is selected.
    Public Sub RefreshToolStrips()
        For Each Strip As ToolStrip In ToolStrips
            EnableActions(Strip)                                'does the enable/disable of items in toolstrip
            Dim ToolStripDescriptor As XmlNode = XmlHelper.Find(ActionFile, Strip.Name)     'seach action.xml for the name of the strip (really only interested in MainToolbar because that is the only one with RecentFileList child but one day someone may use it in the context menu as well)
            RefreshRecentFileList(Strip, ToolStripDescriptor)   'refresh the list of previously opened files as well
        Next
    End Sub
    Private Sub EnableActions(ByVal Strip As ToolStrip)
        For Each ToolItem As ToolStripItem In Strip.Items
            If Not IsNothing(ToolItem.Tag) Then
                ToolItem.Enabled = IsActionAllowed(XmlHelper.Find(ActionFile, "/Folder/Actions/" + ToolItem.Tag.ToString))
            End If
            'Does this toolstrip item have a sub menu (such as "Open") 
            If TypeOf ToolItem Is ToolStripDropDownItem Then
                Dim DropDownItem As ToolStripDropDownItem = ToolItem    'recast item as a sub menu item
                ToolItem.Enabled = IsActionAllowed(XmlHelper.Find(ActionFile, "/Folder/Actions/" + ToolItem.Tag.ToString))
                EnableActions(DropDownItem.DropDown)                    'recursion call, to enable/disable actions in the sub menu.
            End If
        Next
    End Sub
    Private Sub RefreshRecentFileList(ByVal Strip As ToolStrip, ByVal ToolStripDescriptor As XmlNode)
        Dim ItemIndex As Integer = 0
        'Walk through the ToolStripDescriptor until you find a RecentFileList Child
        For Each ChildDescriptor As XmlNode In XmlHelper.ChildNodes(ToolStripDescriptor, "")
            If XmlHelper.Type(ChildDescriptor) = "Item" Then                     'Step over any items
                ItemIndex = ItemIndex + 1
            ElseIf XmlHelper.Type(ChildDescriptor) = "RecentFileList" Then       'If you find one, Refresh 
                ' strip off all old ones.
                While Strip.Items.Count > 2
                    Strip.Items.RemoveAt(Strip.Items.Count - 1)
                End While
                For Each FileName As String In Configuration.Instance.GetFrequentList()
                    Dim Item As ToolStripItem = Strip.Items.Add(FileName)
                    Item.ImageIndex = -1
                    Item.Tag = ""
                    Item.ToolTipText = "Open this file"
                Next                                                            'Step into any DropDownItems to look for RecentFileLists in submenu
            ElseIf XmlHelper.Type(ChildDescriptor) = "DropDownItem" And _
                   TypeOf Strip.Items(ItemIndex) Is ToolStripDropDownItem Then
                Dim DropDownButton As ToolStripDropDownItem = Strip.Items(ItemIndex)
                RefreshRecentFileList(DropDownButton.DropDown, ChildDescriptor)      'recursion call
                ItemIndex = ItemIndex + 1
            End If
        Next
    End Sub

    'Create a new toolstrip but does not return it instead it adds it to the controllers list of toolstrips.  
    '(so it will have its items enabled/disabled depending whenever a different node in the datatree is selected) 
    Public Sub ProvideToolStrip(ByVal Strip As ToolStrip, ByVal ToolStripName As String)    'To see where toolstrips are created do a "Find All References" on "ProvideToolStrip"
        ' --------------------------------------------------------------
        ' This method populates the specified context menu for the 
        ' currently selected type. 
        ' --------------------------------------------------------------
        Strip.Items.Clear()                                             'get rid of the old items out of the toolstrip you are going to repopulate.
        AddHandler Strip.ItemClicked, AddressOf ActionOnClick           'add an event handler to the entire strip NOT to individual items.
        Strip.Name = ToolStripName
        Dim ToolStripDescriptor As XmlNode = XmlHelper.Find(ActionFile, ToolStripName)
        If Not IsNothing(ToolStripDescriptor) Then
            Dim ImageAboveText As Boolean = False
            ImageAboveText = (XmlHelper.Attribute(ToolStripDescriptor, "ImageAboveText") = "yes")
            PopulateToolStrip(Strip, ToolStripDescriptor, ImageAboveText)
        End If
        ToolStrips.Add(Strip)   'add this newly created toolstrip to the Controllers list of toolstrips
    End Sub
    'Removes it from the controllers list of toolstrips
    Public Sub RemoveToolStrip(ByVal Strip As ToolStrip)
        ToolStrips.Remove(Strip)
    End Sub
    Private Sub PopulateToolStrip(ByVal Strip As ToolStrip, ByVal ToolStripDescriptor As XmlNode, ByVal ImageAboveText As Boolean)
        ' --------------------------------------------------------------
        ' This method populates the specified context menu given the
        ' specified descriptor data. 
        ' --------------------------------------------------------------
        For Each ChildDescriptor As XmlNode In XmlHelper.ChildNodes(ToolStripDescriptor, "")

            If XmlHelper.Type(ChildDescriptor) = "ImageSize" Then
                Strip.ImageList = ImageList(ChildDescriptor.InnerText)

            ElseIf XmlHelper.Type(ChildDescriptor) = "Item" Then
                Dim Item As ToolStripItem = CreateToolStripItem(Strip, ChildDescriptor.InnerText)
                If ImageAboveText Then
                    Item.TextImageRelation = TextImageRelation.ImageAboveText
                End If

            ElseIf XmlHelper.Type(ChildDescriptor) = "DropDownItem" Then
                Dim DropDownActionName As String = XmlHelper.Attribute(ChildDescriptor, "action")
                Dim Action As XmlNode = XmlHelper.Find(ActionFile, "/Folder/Actions/" + DropDownActionName)
                Dim DropDownButton As ToolStripDropDownItem
                Dim DropDownStrip As ToolStripDropDown
                If Strip.GetType().ToString = "System.Windows.Forms.ToolStrip" Then
                    DropDownButton = New ToolStripDropDownButton
                    DropDownStrip = New ToolStripDropDownMenu
                Else
                    DropDownButton = New ToolStripMenuItem
                    DropDownStrip = New ToolStripDropDownMenu
                End If
                DropDownButton.Text = XmlHelper.Value(Action, "text")
                DropDownButton.ImageIndex = ImageIndexForAction(Action, Strip.ImageList.Tag.ToString)
                DropDownButton.ImageScaling = ToolStripItemImageScaling.None
                DropDownButton.AutoToolTip = False
                DropDownButton.Tag = DropDownActionName
                DropDownButton.Enabled = IsActionAllowed(Action)
                If ImageAboveText Then
                    DropDownButton.TextImageRelation = TextImageRelation.ImageAboveText
                End If
                DropDownStrip.ImageList = ImageList("SmallIcon")
                AddHandler DropDownStrip.ItemClicked, AddressOf ActionOnClick   'add a handler
                PopulateToolStrip(DropDownStrip, ChildDescriptor, False)        'recursion call
                DropDownButton.DropDown = DropDownStrip
                Strip.Items.Add(DropDownButton)

            ElseIf XmlHelper.Type(ChildDescriptor) = "Separator" Then
                Strip.Items.Add(New ToolStripSeparator)

            ElseIf XmlHelper.Type(ChildDescriptor) = "RecentFileList" Then
                For Each FileName As String In Configuration.Instance.GetFrequentList()
                    Dim Item As ToolStripItem = Strip.Items.Add(FileName)
                    Item.ImageIndex = -1
                    Item.Tag = ""
                    Item.ToolTipText = "Open this file"
                    If ImageAboveText Then
                        Item.TextImageRelation = TextImageRelation.ImageAboveText
                    End If
                Next
            End If
        Next
    End Sub
    Private Function CreateToolStripItem(ByVal Strip As ToolStrip, ByVal ActionName As String) As ToolStripItem
        Dim Action As XmlNode = XmlHelper.Find(ActionFile, "/Folder/Actions/" + ActionName)
        If Not IsNothing(Action) Then
            Dim Item As ToolStripItem = Strip.Items.Add(XmlHelper.Value(Action, "text"))
            Item.Name = XmlHelper.Value(Action, "text")
            Item.ToolTipText = XmlHelper.Value(Action, "description")
            Item.Tag = ActionName
            Item.ImageIndex = ImageIndexForAction(Action, Strip.ImageList.Tag.ToString)
            Item.ImageScaling = ToolStripItemImageScaling.None
            Dim ShortCut As String = XmlHelper.Value(Action, "shortcut")
            If TypeOf Item Is ToolStripMenuItem AndAlso ShortCut <> "" Then
                CType(Item, ToolStripMenuItem).ShortcutKeys = Keys.Parse(GetType(Keys), ShortCut)
            End If
            Item.Enabled = IsActionAllowed(Action)
            Return Item
        Else
            Return Nothing
        End If
    End Function
    Public Function ImageIndexForAction(ByVal Node As XmlNode, ByVal ImageType As String) As Integer
        ' The node passed in will be an action node i.e. <FileNew>. This method needs to 
        ' find the child element as specified by ImageType (e.g. <SmallIcon>) and then
        ' convert the icon filename into an index.
        If Node IsNot Nothing Then
            Dim IconFileName As String = XmlHelper.Value(Node, ImageType)
            Return ConvertImageFileNameToIndex(IconFileName, ImageType)
        End If
        Return -1
    End Function
    Private Function IsActionAllowed(ByVal Action As XmlNode) As Boolean
        'need to pass the strip so you can get the parent, to see if toolstrip belongs to an apsim toolbox (needed for "InToolbox" test)
        Dim Allowed As Boolean = True
        If Not IsNothing(Action) Then
            For Each DisabledWhen As XmlNode In XmlHelper.ChildNodes(Action, "DisabledWhen")
                Dim DisabledWhenFlag As String = DisabledWhen.InnerText
                If DisabledWhenFlag = "ReadOnly" AndAlso ApsimData.IsReadOnly Then
                    Allowed = False
                End If
                If DisabledWhenFlag = "RootNode" AndAlso MySelectedData.Count > 0 AndAlso MySelectedData(0).LastIndexOf("/") = 0 Then
                    Allowed = False
                End If
                If DisabledWhenFlag = "MultipleNodesSelected" AndAlso MySelectedData.Count > 1 Then
                    Allowed = False
                End If
                If DisabledWhenFlag = "NothingLoaded" AndAlso ApsimData.FileName = "Untitled" Then
                    Allowed = False
                End If
                If DisabledWhenFlag = "NotShortcut" AndAlso MySelectedData.Count = 1 AndAlso Selection.ShortCutTo Is Nothing Then
                    Allowed = False
                End If
                If DisabledWhenFlag = "Enabled" AndAlso MySelectedData.Count = 1 AndAlso Selection.Enabled Then
                    Allowed = False
                End If
                If DisabledWhenFlag = "Disabled" AndAlso MySelectedData.Count = 1 AndAlso Not Selection.Enabled Then
                    Allowed = False
                End If
                If DisabledWhenFlag = "InToolbox" AndAlso Not IsNothing(Me.Explorer) AndAlso (Me.Explorer().Name = "ToolboxExplorer") Then 'check if the explorerUI for this controller is the explorerUI for the toolbox.
                    Allowed = False
                End If
                'I think this is to disable once you have clicked on it. 
                'Eg. Once you click the "Run" button disable it, while it is running to stop them clicking on it again. Once it is finished you can enable it again.
                If Not IsNothing(XmlHelper.Find(DisabledWhen, "call")) Then
                    Dim Arguments As New List(Of Object)
                    Arguments.Add(Me)
                    Allowed = Not CSGeneral.CallDll.CallMethodOfClass(XmlHelper.Find(DisabledWhen, "call"), Arguments)
                End If
            Next

            If Allowed And MySelectedData.Count = 1 And XmlHelper.ChildNodes(Action, "AppliesTo").Count > 0 Then
                Allowed = False
                Dim SelectedType As String = Selection.Type
                For Each AppliesTo As XmlNode In XmlHelper.ChildNodes(Action, "AppliesTo")
                    If AppliesTo.InnerText.ToLower = SelectedType.ToLower Then
                        Allowed = True
                    End If
                Next
            End If
        End If
        Return Allowed
    End Function

    Private Sub ActionOnClick(ByVal Sender As Object, ByVal E As ToolStripItemClickedEventArgs)
        ' --------------------------------------------------------------
        ' When the user clicks on an action, this method is called.
        ' Go perform whatever action is necessary.
        ' --------------------------------------------------------------
        Try
            If Not IsNothing(E.ClickedItem.Tag) Then
                If E.ClickedItem.Tag = "" AndAlso FileSaveAfterPrompt() Then
                    Explorer.CloseUI()
                    ApsimData.OpenFile(E.ClickedItem.Text)
                Else
                    InvokeAction(Sender, E.ClickedItem.Tag)
                End If
            End If
        Catch ex As Exception
            MessageBox.Show(ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
        End Try
    End Sub
    Public Sub InvokeAction(ByVal Sender As Object, ByVal ActionName As String)

        'close the menu
        ' For some reason, if we don't explicitly close the menus they remain open,
        ' and on top of other windows.
        If TypeOf Sender Is ContextMenuStrip Then
            CType(Sender, ContextMenuStrip).Close()
        ElseIf TypeOf Sender Is ToolStripDropDownMenu Then
            Dim Menu As ToolStripDropDownMenu = CType(Sender, ToolStripDropDownMenu)
            If Not IsNothing(Menu.OwnerItem) AndAlso Not IsNothing(Menu.OwnerItem.Owner) _
                   AndAlso TypeOf Menu.OwnerItem.Owner Is ContextMenuStrip Then
                CType(Menu.OwnerItem.Owner, ContextMenuStrip).Close()
            End If
            Menu.Close()
        End If

        'call the method to carry out the action
        Dim ActionInvoke As XmlNode = XmlHelper.Find(ActionFile, "/Folder/Actions/" + ActionName + "/OnInvoke/Call")    'look up the <class> and <method> for the action in the Actions.xml
        If Not IsNothing(ActionInvoke) Then
            Dim Arguments As New List(Of Object)
            Arguments.Add(Me)                   'pass the Controller as a parameter to the method
            CallDll.CallMethodOfClass(ActionInvoke, Arguments)      'parameters for this are an XmlNode (with a <class> and <method> tag) and a list of Objects (which are the paramters for the method specified in the XmlNode)
        End If
    End Sub
    Public Shared Function CreateClass(ByVal ClassToCall As String) As Object
        ' --------------------------------------------------------------
        ' Call a static/shared method of a class to perform the 
        ' necessary action.
        ' --------------------------------------------------------------
        Try
            Dim PosPeriod As Integer = ClassToCall.IndexOf(".")
            If PosPeriod = -1 Then
                Throw New Exception("No namespace specified in action: " + ClassToCall)
            End If
            Dim NameSpaceName As String = ClassToCall.Substring(0, PosPeriod)
            Dim t As Type = Nothing
            For Each Assemb As Assembly In AppDomain.CurrentDomain.GetAssemblies
                If NameSpaceName.ToLower() = Assemb.GetName().Name.ToLower() Then
                    t = Assemb.GetType(ClassToCall, True, True)
                End If
            Next
            If IsNothing(t) Then
                Throw New Exception("Cannot find type: " + ClassToCall)
            End If
            Return Activator.CreateInstance(t)

        Catch ex As Exception
            MessageBox.Show(ex.GetBaseException().Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        End Try
        Return Nothing
    End Function

#End Region

End Class
