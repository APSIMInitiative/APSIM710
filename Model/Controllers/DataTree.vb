Imports System.Xml
Imports System.Collections.Specialized
Imports System.Windows.Forms
Imports System.Drawing
Imports CSGeneral
Imports ApsimFile


Public Class DataTree
    Inherits TreeView       'DataTree inherits from TreeView NOT from BaseView, but it still uses BaseController as its go between to the Model.
    'ApsimUI only has 2 Views, BaseView and TreeView. TreeView is the parent of the DataTree, and BaseView is the parent of every other UI in ApsimUI.
    'DataTree inherits from TreeView NOT from BaseView, but it still uses BaseController as its go between to the Model.
    'ApsimUI only has 2 Views, BaseView and TreeView. TreeView is the parent of the DataTree, and BaseView is the parent of every other UI in ApsimUI.

    ' ---------------------------------------------------
    ' Tree control for visualising an ApsimFile
    ' ---------------------------------------------------

    Private PopupMenu As New System.Windows.Forms.ContextMenuStrip                              'create a new Context Menu for the page [right click anywhere on the page]
    Private WithEvents PopupMenuRightDrag As New System.Windows.Forms.ContextMenuStrip          'create a new Context Menu for a drag using the right mouse button. See TreeView Drag Events below

    Private Controller As BaseController
    Private FirstTimeRename As Boolean = False
    Private EnableNodeSelection As Boolean = True
    Private DisplayNodeTypeInTree As Boolean = False


    Public Sub OnLoad(ByVal Controller As BaseController)
        ' ---------------------------------------------------
        ' Set ourselves up.
        ' ---------------------------------------------------

        AddHandler Controller.ApsimData.ComponentChangedEvent, AddressOf OnRefresh
        AddHandler Controller.ApsimData.FileNameChanged, AddressOf OnFileNameChanged
        AddHandler Controller.SelectionChangedEvent, AddressOf OnSelectionChanged

        PathSeparator = "/"
        ImageList = Controller.ImageList("SmallIcon")

        Me.ShowNodeToolTips = True
        Me.Controller = Controller
        Me.ContextMenuStrip = PopupMenu                        'tell the datatree that its context menu is the popup menu

        'PopupMenu.TopLevel = False                              'tell the popup menu that it belongs to a parent control.
        'PopupMenu.Parent = Me                                   'tell the popup menu that it belongs to the datatree.

        Controller.ProvideToolStrip(PopupMenu, "ContextMenu")   'initialise Context Menu for the page using the controller [popup when you right click anywhere on the page]

        'initialise Context Menu for the right mouse button drag [popup when you drop the node you were dragging with the right mouse]
        PopupMenuRightDrag.Items.Add("Copy Here")
        PopupMenuRightDrag.Items.Add("Move Here")
        PopupMenuRightDrag.Items.Add("Create Link Here")
        PopupMenuRightDrag.Items.Add(New ToolStripSeparator)
        PopupMenuRightDrag.Items.Add("Cancel")

        DisplayNodeTypeInTree = Configuration.Instance.Setting("DisplayNodeTypeInTree") = "Yes"
    End Sub
    Private Function GetNodeFromPath(ByVal ChildPath As String) As TreeNode
        ' --------------------------------------------------
        ' Returns a tree node given a fullly delimited path.
        ' --------------------------------------------------
        Dim name As String
        Dim Path As String = ChildPath.Substring(1)
        Dim CurrentNode As TreeNode = Nothing
        Do Until Path = ""
            Dim PosDelimiter As Integer = Path.IndexOf(PathSeparator)
            If PosDelimiter <> -1 Then
                name = Path.Substring(0, PosDelimiter)
                Path = Path.Substring(PosDelimiter + 1)
            Else
                name = Path
                Path = ""
            End If

            Dim ChildNode As TreeNode = Nothing
            If CurrentNode Is Nothing Then
                If Nodes.Count = 0 Then
                    Return Nothing
                Else
                    ChildNode = Nodes(0)
                End If
            Else
                For Each ChildNode In CurrentNode.Nodes
                    If ChildNode.Text.ToLower() = name.ToLower() Then
                        Exit For
                    End If
                Next
            End If
            CurrentNode = ChildNode
            If Not IsNothing(CurrentNode) Then
                If CurrentNode.Text.ToLower() <> name.ToLower() Then
                    CurrentNode = Nothing
                End If
            End If
            If IsNothing(CurrentNode) Then
                Exit Do
            End If
        Loop

        Return CurrentNode
    End Function
    Private Function GetPathFromNode(ByVal Node As TreeNode)
        Return PathSeparator + Node.FullPath    'just put an extra "/" in front of the node path, so "root/child" becomes "/root/child" (this is needed because our "Selected Path" root starts with a /, whereas the inbuilt node full path property does not start with a / at the root)
    End Function

    Private Sub OnFileNameChanged(ByVal FileName As String)
        ' ---------------------------------------------------------
        ' The file name has changed so expand all folder nodes.
        ' ---------------------------------------------------------
        If Nodes.Count = 1 Then
            Dim RootNode As TreeNode = Nodes(0)
            CollapseAll()
            RootNode.Expand()
            If RootNode.Nodes.Count = 1 Then
                If RootNode.Nodes(0).Tag.ToString.ToLower = "simulation" Then
                    ' Count the number of paddocks.
                    Dim NumPaddocks As Integer = 0
                    For Each Child As TreeNode In RootNode.Nodes(0).Nodes
                        If Child.Tag = "area" Then
                            NumPaddocks = NumPaddocks + 1
                        End If
                    Next
                    If NumPaddocks = 1 Then
                        For Each Child As TreeNode In RootNode.Nodes(0).Nodes
                            Child.Expand()
                        Next
                    End If
                End If
                RootNode.Nodes(0).Expand()
            End If
            Controller.SelectedPath = Controller.ApsimData.RootComponent.FullPath
        End If
    End Sub
    Private Sub ExpandAllFolders(ByVal Node As TreeNode)
        If Node.Tag.ToString.ToLower = "folder" Then
            Dim ThereAreSubFolders As Boolean = False
            For Each Child As TreeNode In Node.Nodes
                If Child.Tag.ToString.ToLower = "folder" Then
                    ThereAreSubFolders = True
                    Exit For
                End If
            Next
            If ThereAreSubFolders Then
                Node.Expand()
                For Each Child As TreeNode In Node.Nodes
                    ExpandAllFolders(Child)
                Next
            End If
        End If
    End Sub




#Region "Refresh methods"

    Private Overloads Sub OnRefresh(ByVal Comp As ApsimFile.Component)
        ' ----------------------------------------------
        ' Do a refresh from the specified Comp down
        ' ----------------------------------------------
        Windows.Forms.Cursor.Current = Cursors.WaitCursor       'set the cursor object (usually an arrow) to the wait cursor (usually an hourglass)
        BeginUpdate()                                           'inbuilt tree function, it disables redrawing of the tree

        Try

            'If (the tree has no nodes) OR (Comp [the component parameter this sub was passed] is Null)
            If (Nodes.Count = 0) Or (Comp Is Nothing) Then
                Nodes.Clear()                                                                   'get rid of all the nodes in the tree 
                Dim RootNode As TreeNode = Nodes.Add(Controller.ApsimData.RootComponent.Name)   'create the root node from the root component and add it to the tree.
                RefreshNodeAndChildren(RootNode, Controller.ApsimData.RootComponent)            'refresh the tree from the root node down.

                'Get the node you want to refresh 
            Else

                Dim NodeToRefresh As TreeNode = GetNodeFromPath(Comp.FullPath)                  'get the corresponding node for the component this sub was passed
                If IsNothing(NodeToRefresh) Then                                                'if you have switched from one toolbox to another toolbox, then even though the components exist to do the refresh, the corresponding nodes do not yet exist because this OnRefresh is supposed to provide them. So GetNodeFromPath will return Nothing.
                    RefreshNodeAndChildren(Nodes(0), Controller.ApsimData.RootComponent)                                     'refresh the tree from this node down.
                Else
                    RefreshNodeAndChildren(NodeToRefresh, Comp)                                     'refresh the tree from this node down.
                End If

            End If
        Catch ex As Exception
            EndUpdate()                                                                         'inbuilt tree function, reinables redrawing of the tree
            Windows.Forms.Cursor.Current = Cursors.Default                                      'set the cursor object back to the default windows cursor (usually an arrow)
            Throw
        End Try


        'If multiple nodes are selected set the tree's selected node to the first one.

        'NB. Windows inbuilt Tree control does not allow you to select more then one node.
        '   So we had to create our own code to manually highlight the nodes that are selected. (manually changed the background/foreground properties of each node in the tree) (see ColourNode sub in the 'Selection methods' Region) 
        '   This is also why we needed to create the SelectedPaths property in the Base Control, instead of just using the tree controls inbuilt SelectedNode property.

        If (Controller.SelectedPaths.Count > 0) Then
            EnableNodeSelection = False                                                         'don't let the user click any other nodes while this code executes
            SelectedNode = GetNodeFromPath(Controller.SelectedPaths(0))                         'set tree's selected node property to the first item in the SelectedPaths. The tree control complains if you don't have something set as the SelectedNode, but we don't use it.
            EnableNodeSelection = True                                                          'let the user click on other nodes again

        End If


        EndUpdate()                                             'inbuilt tree function, reinables redrawing of the tree
        Windows.Forms.Cursor.Current = Cursors.Default          'set the cursor object back to the default windows cursor (usually an arrow)
    End Sub
    Private Sub RefreshNodeAndChildren(ByVal Node As TreeNode, ByVal Comp As ApsimFile.Component)
        ' --------------------------------------------------
        ' Recursively refresh the specified treenode and its
        ' child nodes in the tree.
        ' --------------------------------------------------

        ' Refresh the specified node first.
        Node.Text = Comp.Name
        Node.ImageIndex = Controller.ImageIndex(Comp.Type, "SmallIcon")
        Node.SelectedImageIndex = Node.ImageIndex
        Node.Tag = Comp.Type
        If DisplayNodeTypeInTree Then
            Node.ToolTipText = Comp.Type
        Else
            Node.ToolTipText = Comp.Description
        End If
        If Not IsNothing(Comp.ShortCutTo) Then
            Node.ToolTipText = "Linked to " + Comp.ShortCutTo.FullPath
            If Not Comp.Enabled Then
                Node.ToolTipText = "Disabled: " + Node.ToolTipText
            End If
        End If
        If Not Comp.Enabled Then
            Node.ToolTipText = "Disabled" + Node.ToolTipText
        End If
        ColourNode(Node)
        ' Go refresh all children.
        Dim ChildIndex As Integer = 0
        For Each Child As ApsimFile.Component In Comp.ChildNodes
            If Child.Type <> "factorial" Then
                Dim ChildTreeNode As TreeNode
                If ChildIndex < Node.Nodes.Count Then
                    ChildTreeNode = Node.Nodes(ChildIndex)
                Else
                    ChildTreeNode = Node.Nodes.Add(Child.Name)
                End If
                RefreshNodeAndChildren(ChildTreeNode, Child)
                ChildIndex = ChildIndex + 1
            End If
        Next
        While Node.Nodes.Count > ChildIndex
            Node.Nodes.Remove(Node.Nodes(Node.Nodes.Count - 1))
        End While
    End Sub
#End Region




#Region "Selection methods"

    Private PreviousNode As TreeNode
    Private Sub OnSelectionChanged(ByVal OldSelections As StringCollection, ByVal NewSelections As StringCollection)
        ' -----------------------------------------------------------------
        ' Selection has changed - update tree.
        ' -----------------------------------------------------------------

        'NB. Windows inbuilt Tree control does not allow you to select more then one node.
        '   So we had to create our own code to manually highlight the nodes that are selected. (manually changed the background/foreground properties of each node in the tree) (see the ColourNode sub below) 
        '   This is also why we needed to create the SelectedPaths property in the Base Control, instead of just using the tree controls inbuilt SelectedNode property.


        EnableNodeSelection = False     'don't let the user click any other nodes while this code executes

        'Change the colour of all the old selected nodes to the "unselected" colours
        For Each NodePath As String In OldSelections
            Dim Node As TreeNode = GetNodeFromPath(NodePath)    'get the node that the old selected path points to
            If Not IsNothing(Node) Then
                ColourNode(Node)                                'change the colour of the unselected node to the unselected colours.
            End If

        Next

        'Change the colour of all the new selected nodes to the "selected" colours
        For Each NodePath As String In NewSelections

         Dim Node As TreeNode = GetNodeFromPath(NodePath)    'get the node that the new selected path points to.
         If Not IsNothing(Node) Then
            SelectedNode = Node                                 'set the Tree's selected node to the node specified in the new selected path (just used to trigger the AfterSelect event, which is handled by OnTreeSelectionChanged() subroutine below this subroutine) (nb. we REDO this for EVERY node in NewSelections. We have to do this one node at a time because the Tree does not allow you to select more then one node) 
            ColourNode(Node)                                    'change the colour of the new selected node to the selected colours.
            Node.EnsureVisible()                                'use inbuilt tree node function that expands the tree to make sure the node specified is visible in the tree.  
         End If
      Next

        EnableNodeSelection = True      'let the user click on other nodes again
    End Sub
    Private Sub OnTreeSelectionChanged(ByVal Sender As Object, ByVal e As TreeViewEventArgs) Handles Me.AfterSelect
        If EnableNodeSelection Then
            Controller.SelectedPath = GetPathFromNode(e.Node)
        End If
    End Sub

#End Region

    Private LinkFont As Font = New System.Drawing.Font(Me.Font.FontFamily, Me.Font.Size, FontStyle.Underline)
    Private UnLinkFont As Font = New System.Drawing.Font(Me.Font.FontFamily, Me.Font.Size, FontStyle.Regular)
    Private Sub ColourNode(ByVal Node As TreeNode)


        'NB. Windows inbuilt Tree control does not allow you to select more then one node.
        '   So we had to create our own code to manually highlight the nodes that are selected. (manually changed the background/foreground properties of each node in the tree) (see the code below) 
        '   This is also why we needed to create the SelectedPaths property in the Base Control, instead of just using the tree controls inbuilt SelectedNode property.

        'If the node is linked to another node  
        If Node.ToolTipText.IndexOf("Linked to") = 0 Then       'nb. ToolTipText is the text that appears when you hover the mouse over the node. IndexOf just returns the index of the first occurance of one string in another.
            Node.ForeColor = Color.Blue                         'colour to blue
            Node.NodeFont = LinkFont                            'font to underlined (see LinkFont variable declared just above this ColourNode function)
            Node.BackColor = BackColor                          'back colour to default back colour for the tree

            'If the node is disabled 

        ElseIf Node.ToolTipText.IndexOf("Disabled") = 0 Then
            Node.ForeColor = SystemColors.InactiveCaptionText   'colour to the default colour for a windows system disabled element 
            Node.BackColor = SystemColors.InactiveCaption       'back colour to the default back colour for a disabled item in windows 

            'If it's just a normal node
        Else
            Node.ForeColor = Color.Black                        'colour to black
            Node.BackColor = BackColor                          'back colour to default back colour for the tree
            Node.NodeFont = UnLinkFont                          'font to regular (see UnLinkFont variable declared just above this ColourNode function)


        End If

        'If the node is a selected node
        If Controller.SelectedPaths.IndexOf(GetPathFromNode(Node)) <> -1 Then       'this IndexOf is for a string collection NOT a string. So it it checks every string in the collection for an exact match with the search string. If it finds one it returns that strings index in the collection.
            Node.ForeColor = SystemColors.HighlightText         'colour to the default colour for a selected item in windows
            Node.BackColor = SystemColors.Highlight             'back colour to the default back colour for a selected item in windows

        End If
    End Sub




#Region "Rename methods"        'This is a Rename done by doing 2 seperate clicks (See "Left Click Only" code in TreeView_MouseDown). NOT by a right mouse click then selecting rename, this is handled by Rename() sub in BaseAction.vb   



    'event handlers for a Node.BeginEdit()

    Private Sub OnBeforeEdit(ByVal sender As Object, ByVal e As System.Windows.Forms.NodeLabelEditEventArgs) Handles Me.BeforeLabelEdit
        ' ---------------------------------------------------
        ' User is about to start editing a tree node.
        ' We must disable the popup menu because if the user
        ' hits DELETE while editing the node, the ACTION
        ' will trigger, deleting the whole node rather than
        ' the bit of text on the node caption.
        ' ---------------------------------------------------
        'PopupMenu.Enabled = False
        Me.ContextMenuStrip.Enabled = False

    End Sub
    Private Sub OnAfterEdit(ByVal sender As Object, ByVal e As System.Windows.Forms.NodeLabelEditEventArgs) Handles Me.AfterLabelEdit
        ' ---------------------------------------------------
        ' User has just finished editing the label of a node.
        ' ---------------------------------------------------

        If Not FirstTimeRename Then
            If Not IsNothing(e.Label) Then

                If (e.Label.Length > 0) Then 'Check user typed something in. So you are not trying to rename it to a blank.

                    If Not (CSGeneral.Utility.CheckForInvalidChars(e.Label)) Then

                        ' Firstly empty the current selections.
                        Controller.SelectedPath = ""

                        ' Change the data
                        Dim Comp As ApsimFile.Component = Controller.ApsimData.Find(GetPathFromNode(e.Node))
                        Comp.Name = e.Label

                        ' Now tell the base controller about the new selections.
                        Controller.SelectedPath = Comp.FullPath

                    Else

                        MessageBox.Show("You can not use characters such as < > / \ ' "" ` : ? | * & = ! in the name")
                        e.CancelEdit = True     'cancel the edit event.

                    End If

                Else
                    e.CancelEdit = True     'cancel the edit event.
                End If
            End If
            LabelEdit = False
        End If
        FirstTimeRename = False
        'PopupMenu.Enabled = True
        Me.ContextMenuStrip.Enabled = True
    End Sub

#End Region




#Region "Drag / Drop methods"       'These events handle a drag with both the left mouse button and the right mouse button

    'Global variables for Drag/Drop methods
    Protected PathsBeingDragged As StringCollection     'used to store the paths for all the components that have been selected in the drag
    Private isRightBtnDrag As Boolean = False           'is the drag event a drag using the right mouse button


    Private Sub TreeView_ItemDrag(ByVal sender As Object, ByVal e As System.Windows.Forms.ItemDragEventArgs) Handles Me.ItemDrag
        ' -----------------------------------------------------------------
        ' User has initiated a drag on a node - the full xml of the node
        ' is stored as the data associated with the drag event args.
        ' -----------------------------------------------------------------

        'If what is being dragged is not already in the controller as a selected item.
        If Controller.SelectedPaths.IndexOf(GetPathFromNode(e.Item)) = -1 Then
            SelectedNode = e.Item                                                       'add it to the base controller (by setting the selected node of the tree to the dragged item. This then fires the selection changed event for the tree which I think is handled by the base controller. This will add the dragged items to the base controller)
        End If

        'Work out the xml of what you are dragging.
        Dim FullXML As String = ""                                                  'used to store the xml of ALL the components that have been selected in the drag  'reset it to nothing, ready for recreation.
        For Each SelectedPath As String In Controller.SelectedPaths                 'get the full xml of all the selected nodes that are getting dragged
            Dim Comp As ApsimFile.Component = Controller.ApsimData.Find(SelectedPath)   'get the component for this particular selected node (using it's path)
            FullXML = FullXML + Comp.FullXML                                            'get the xml for the component and add it to the xml of previous selected nodes
        Next
        PathsBeingDragged = Controller.SelectedPaths                                'store the paths of ALL the nodes that are being dragged in a global variable, so it can be used by other drag events.

        'Raise the other DragDropEvents
        DoDragDrop(FullXML, DragDropEffects.Copy Or DragDropEffects.Move Or DragDropEffects.Link)
        'parameters: (Store xml of what you are dragging in "data" Drag Event Argument), (allowable types of left mouse drags [Drag Drop Effects are of type FlagsAttribute, which allow bitwise operators AND and OR]). 

    End Sub

    Private Sub TreeView_DragOver(ByVal sender As Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles Me.DragOver
        ' --------------------------------------------------
        ' User has dragged a node over us - allow drop?
        ' --------------------------------------------------

        'Make sure you are actually dragging something
        If e.Data.GetDataPresent(GetType(System.String)) Then   'check the "data" Drag Event Argument

            'If the mouse is currently dragging over a node and not over blank area
            Dim pt As Point = PointToClient(New Point(e.X, e.Y))    'get the drop location
            Dim DestinationNode As TreeNode = GetNodeAt(pt)         'find the node closest to the drop location
            If Not IsNothing(DestinationNode) Then

                'Work out the type of left drag this is (copy, move, create link/shortcut), and store it in the "Effect" Drag Event Argument
                Dim FullXML As String = e.Data.GetData(DataFormats.Text)
                Dim DropComp As ApsimFile.Component = Controller.ApsimData.Find(GetPathFromNode(DestinationNode))   'get the corresponding component for the destination node.
                If DropComp.AllowAdd(FullXML) Then                      'if allowed to drop this node onto this destination node
                    If Not IsNothing(PathsBeingDragged) AndAlso PathsBeingDragged.Count > 0 AndAlso (Control.ModifierKeys And Keys.Shift) = Keys.Shift Then
                        e.Effect = DragDropEffects.Move                     'these DragDropEffects are just the changes to the mouse icon when you hover over a node whilst dragging
                    ElseIf Not IsNothing(PathsBeingDragged) AndAlso PathsBeingDragged.Count > 0 And (Control.ModifierKeys And Keys.Alt) = Keys.Alt Then
                        e.Effect = DragDropEffects.Link
                    Else
                        e.Effect = DragDropEffects.Copy
                    End If
                Else                                                    'if NOT allowed to drop this node onto this destination node
                    e.Effect = DragDropEffects.None                         'display circle with line through it symbol
                End If

                'If this is a right mouse drag, set the global variable flag
                If (e.KeyState = 2) Then                                'you have to do this test in "DragOver" event because the DragEventArgs disappear once the "DragDrop" event occurs [even thought it has a DragEventArgs parameter]
                    isRightBtnDrag = True                                   'set the flag to true
                End If

            End If
        End If
    End Sub


    Private Sub TreeView_DragDrop(ByVal sender As Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles Me.DragDrop
        ' --------------------------------------------------
        ' User has released mouse button during a drag.
        ' Accept the dragged node.
        ' --------------------------------------------------

        'Get the Destination Node
        Dim pt As Point = CType(sender, TreeView).PointToClient(New Point(e.X, e.Y))    'get the drop location
        Dim DestinationNode As TreeNode = CType(sender, TreeView).GetNodeAt(pt)         'find the node closest to the drop location
        Controller.SelectedPath = GetPathFromNode(DestinationNode)                      'set the selected path for the controller to the path for the destination node

        'Get the xml of what was dragged.
        Dim FullXML As String = e.Data.GetData(DataFormats.Text)                        'it was put into the "data" Drag Event Argument in the DoDragDrop call (in TreeViewDragItem event handler)  


        'Drag using the right mouse button
        If isRightBtnDrag Then

            'Display the popup menu to let the user select the drag action

            PopupMenuRightDrag.Tag = FullXML                                                'use this as a dodgy way of passing the xml of what was dragged to the "Right Drag Context Menu" ItemClicked event.  
            PopupMenuRightDrag.Show(Me, pt)                                                 'display the context menu at the place user dropped the node(s) at
            isRightBtnDrag = False                                                          'reset the flag back to false 

        Else

            'Drag using the left mouse button

            'depending on the type of left mouse drag, do the corresponding drag action.
            Select Case e.Effect                                                             'use the "Effect" Drag Event Argument to get the type of left mouse drag
                Case DragDropEffects.Copy                                                           'these DragDropEffects are just the changes to the mouse icon when you hover over a node whilst dragging
                    DragCopy(FullXML)
                Case DragDropEffects.Move
                    DragMove(FullXML)
                Case DragDropEffects.Link
                    DragLink()
                Case DragDropEffects.None                                                           'for when the user drags to somewhere that does not allow a drag to.
                    DragCancel()
                Case Else                                                                           'should not needed this, but just put it in to prevent errors.
                    DragCancel()
            End Select

        End If
    End Sub

    ' "Right Drag Context Menu" Click Event.

    Private Sub PopupMenuRightDrag_ItemClicked(ByVal sender As Object, ByVal e As ToolStripItemClickedEventArgs) Handles PopupMenuRightDrag.ItemClicked

        'Get what was dragged. 
        Dim FullXML As String = CType(PopupMenuRightDrag.Tag, String)   'it was put into the Tag property of the right mouse click popup menu in the "DragDrop" event (just before the right click popup menu is displayed) 


        'Get what option in popup menu was clicked, and do the drag action
        Select Case e.ClickedItem.Text                                  'what did the user select from the Context Menu of a right drag.
            Case "Copy Here"
                DragCopy(FullXML)
            Case "Move Here"
                DragMove(FullXML)
            Case "Create Link Here"
                DragLink()
            Case "Cancel"
                DragCancel()
            Case Else                                                   'should not needed this, but just put it in to prevent errors.
                DragCancel()
        End Select


    End Sub

    'Drag Actions

    'copy
    Private Sub DragCopy(ByVal FullXML As String)
        Controller.Selection.Add(FullXML)                       'add the dragged nodes to the destination node (destination node is stored as the current selection in the controller) 
    End Sub
    'move
    Private Sub DragMove(ByVal FullXML As String)
        Controller.Selection.Add(FullXML)                       'add the dragged nodes to the destination node (destination node is stored as the current selection in the controller) 
        If Not IsNothing(PathsBeingDragged) Then                'when you drag from datatree in simulation to datatree in a toolbox the PathsBeingDragged disappear and it causes an error. I have done this to avoid the error but moving does not really make sense in this situation anyway. Instead just copying makes more sense.
            For Each DraggedPath As String In PathsBeingDragged     'delete all the dragged nodes from their original path.
                Dim Comp As ApsimFile.Component = Controller.ApsimData.Find(DraggedPath)
                If Not IsNothing(Comp) Then
                    Comp.Parent.Delete(Comp)
                End If
            Next
        End If
    End Sub
    'create link
    Private Sub DragLink()
        If Not IsNothing(PathsBeingDragged) Then                'when you drag from datatree in simulation to datatree in a toolbox the PathsBeingDragged disappear and it causes an error. I have done this to avoid the error but creating a link does not really make sense in this situation anywhere. Instead just doing nothing makes more sense.
            For Each DraggedPath As String In PathsBeingDragged     'add all the dragged node as a link to the destination node
                Dim Comp As ApsimFile.Component = Controller.ApsimData.Find(DraggedPath)
                If Not IsNothing(Comp) Then
                    Controller.Selection.AddShortCut(Comp)              'add one of the dragged nodes as a link to the destination node (destination node is stored as the current selection in the controller)
                End If
            Next
        End If
    End Sub
    'cancel
    Private Sub DragCancel()
        If Not IsNothing(PathsBeingDragged) Then                'when you drag from datatree in simulation to datatree in a toolbox the PathsBeingDragged disappear and it causes an error. I have done this to avoid the error.
            PathsBeingDragged.Clear()                               'clear the variable storing the dragged nodes 
        End If
    End Sub

#End Region




#Region "Mouse events"

    Private Sub TreeView_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Me.MouseDown
        ' ---------------------------------------------------------------
        ' If the user right clicks on a node and that node isn't already
        ' selected, then go select it.
        ' ---------------------------------------------------------------


        'Initialise variables

        Dim ClickedNode As TreeNode = GetNodeAt(e.Location)                         'get the node in the datatree that was clicked on.
        If IsNothing(ClickedNode) Then                                              'if the button was pressed on a non node area of the datatree
            Return                                                                      'do nothing.
        End If

        If (e.X < ClickedNode.Bounds.Left - 20) Then                                'allow for an image 20 pixels to the left of the text of a node
            Return
        End If

      'Don't let the user click any other nodes while this code executes

      EnableNodeSelection = False

        Dim RightClick As Boolean = (e.Button = Windows.Forms.MouseButtons.Right)   'is it a right mouse button click
        Dim Control As Boolean = (ModifierKeys = Keys.Control)                      'is control button on keyboard pressed 
        Dim Shift As Boolean = (ModifierKeys = Keys.Shift)                          'is shift button on keyboard pressed

        Dim SelectedPaths As StringCollection = Controller.SelectedPaths            'get the selected paths from the controller. (stores more then one nodes path because the user can hold down the control key and select more then one node)




        'Click with a Control button OR Right Click         'TODO: this may execute for CTRL + Centre Mouse Click too, you may want to specifically exclude this later.

        If Control Then

            'check to see if the clicked node has been clicked on already 
            If SelectedPaths.IndexOf(GetPathFromNode(ClickedNode)) = -1 Then            'this IndexOf is for a string "collection" NOT a string. So it it checks every string "in the collection" for an exact match with the search string. If it finds one it returns that strings index in the collection
                SelectedPaths.Add(GetPathFromNode(ClickedNode))                             'if not then add it's path to the list of selected paths
                PreviousNode = ClickedNode                                                  'store clicked node as the previously clicked on node (used for Shift button)
            End If



            'Click with a Shift button

        ElseIf Shift Then

            'if (user has previously clicked on a node with the shift key down) AND (clicked node has the same parent as the previous node [they are siblings]) 
            If Not IsNothing(PreviousNode) AndAlso PreviousNode.Parent.Equals(ClickedNode.Parent) Then

                Dim FirstIndex As Integer = PreviousNode.Index                              'set to the index of the previously clicked on node
                Dim LastIndex As Integer = ClickedNode.Index                                'set to the index of the clicked node
                If FirstIndex > LastIndex Then                                              'if they clicked lower sibling before higher sibling, then rearrange the order they were clicked.

                    Dim TempIndex As Integer = LastIndex
                    LastIndex = FirstIndex
                    FirstIndex = TempIndex
                End If

                SelectedPaths.Clear()                                                       'get rid of old selected paths
                For i As Integer = FirstIndex To LastIndex                                  'add the paths for all the node's between the first index and the last index, to the list of selected paths 
                    SelectedPaths.Add(GetPathFromNode(PreviousNode.Parent.Nodes(i)))
                Next

                PreviousNode = ClickedNode                                                  'store clicked node as the previously clicked on node (incase Shift button is used on the next click as well)

            End If


            '(Do right and left click test after the multiple select tests of Control and Shift, so that Control and Shift can work for both Left or Right mouse clicks)

            'Right Click Only           

        ElseIf RightClick Then

            SelectedPaths.Clear()                                                       'get rid of existing selected paths
            SelectedPaths.Add(GetPathFromNode(ClickedNode))                             'add new path to selected paths
            PreviousNode = ClickedNode                                                  'store clicked node as the previously clicked on node (used for Shift button)


            'Left Click Only

        ElseIf Not IsNothing(ClickedNode) Then      'if user clicked on a node and not a blank area

            'if the user clicked again on the same thing that was already selected -> then do a Rename.

            'click on same thing that was already selected.
            If Not Controller.ApsimData.IsReadOnly _
                    AndAlso SelectedPaths.Count = 1 _
                    AndAlso SelectedPaths(0) = GetPathFromNode(ClickedNode) _
                    AndAlso ClickedNode.Level > 0 Then

                'if not readonly, 
                'and user has clicked once before, 
                'and what they clicked this time is the same as what they clicked last time, 
                'and they have clicked on a node that is lower down then the root node [can't rename the root node] 

                LabelEdit = True                                                                'set the tree's label edit property  to true, allowing all the nodes on the tree to have their labels edited. (needs to be set to true for Node.BeginEdit() to work)  
                FirstTimeRename = True
                ClickedNode.BeginEdit()                                                         'call the inbuilt tree node function that allows the user to edit the nodes label. (see OnBeforeEdit and OnAfterEdit sub for what happens before and after the user edits the label)
            EnableNodeSelection = True
                Exit Sub

                'if they clicked on something different to what was already selected -> change the selection

            Else

                SelectedPaths.Clear()                                                           'get rid of existing selected paths
                SelectedPaths.Add(GetPathFromNode(ClickedNode))                                 'add new path to selected paths
                PreviousNode = ClickedNode                                                      'store clicked node as the previously clicked on node (used for Shift button)

            End If
        End If


        'Finish off

        Controller.SelectedPaths = SelectedPaths                                    'update the selected paths "in the controller"


        'Let the user click on other nodes again

        EnableNodeSelection = True

    End Sub

#End Region

    Private Sub InitializeComponent()
        Me.SuspendLayout()
        '
        'DataTree
        '
        Me.ForeColor = System.Drawing.Color.Goldenrod
        Me.HideSelection = False
        Me.ResumeLayout(False)

    End Sub


End Class
