Imports System.Xml
Imports System.Collections.Specialized
Imports System.Windows.Forms
Imports System.Drawing

Public Class FactorTree
    Inherits TreeView

    Private Controller As BaseController
    Private EnableNodeSelection As Boolean = True

    Private PopupMenu As New System.Windows.Forms.ContextMenuStrip                              'create a new Context Menu for the page [right click anywhere on the page]
    Private WithEvents PopupMenuRightDrag As New System.Windows.Forms.ContextMenuStrip          'create a new Context Menu for a drag using the right mouse button. See TreeView Drag Events below


    Public Sub OnLoad(ByVal Controller As BaseController)
        ' ---------------------------------------------------
        ' Set ourselves up.
        ' ---------------------------------------------------

        AddHandler Controller.ApsimData.ComponentChangedEvent, AddressOf OnRefresh
        '        AddHandler Controller.ApsimData.FileNameChanged, AddressOf OnFileNameChanged
        AddHandler Controller.FactorialSelectionChangedEvent, AddressOf OnSelectionChanged

        PathSeparator = "/"
        ImageList = Controller.ImageList("SmallIcon")

        Me.ShowNodeToolTips = True
        Me.Controller = Controller
        Me.ContextMenuStrip = PopupMenu                        'tell the datatree that its context menu is the popup menu
        Controller.ProvideToolStrip(PopupMenu, "ContextMenu")   'initialise Context Menu for the page using the controller [popup when you right click anywhere on the page]

        'initialise Context Menu for the right mouse button drag [popup when you drop the node you were dragging with the right mouse]
        PopupMenuRightDrag.Items.Add("Copy Here")
        PopupMenuRightDrag.Items.Add("Move Here")
        PopupMenuRightDrag.Items.Add("Create Link Here")
        PopupMenuRightDrag.Items.Add(New ToolStripSeparator)
        PopupMenuRightDrag.Items.Add("Cancel")

    End Sub

#Region "Refresh methods"
    Private Sub OnRefresh(ByVal Comp As ApsimFile.Component)
        ' ----------------------------------------------
        ' Do a refresh from the specified Comp down
        ' ----------------------------------------------
        Windows.Forms.Cursor.Current = Cursors.WaitCursor       'set the cursor object (usually an arrow) to the wait cursor (usually an hourglass)
        BeginUpdate()                                           'inbuilt tree function, it disables redrawing of the tree

        Try
            If Controller.ApsimData.FactorComponent Is Nothing Then
                Nodes.Clear()                                                                   'get rid of all the nodes in the tree 
            Else
                'If (the tree has no nodes) OR (Comp [the component parameter this sub was passed] is Null)
                If (Nodes.Count = 0) Or (Comp Is Nothing) Then
                    Nodes.Clear()                                                                   'get rid of all the nodes in the tree 
                    Dim RootNode As TreeNode = Nodes.Add(Controller.ApsimData.FactorComponent.Name)   'create the root node from the root component and add it to the tree.
                    RefreshNodeAndChildren(RootNode, Controller.ApsimData.FactorComponent)            'refresh the tree from the root node down.
                Else
                    Dim NodeToRefresh As TreeNode = GetNodeFromPath(Comp.FullPath)                  'get the corresponding node for the component this sub was passed
                    'return the corresponding component to the node that was selected.
                    If IsNothing(NodeToRefresh) Then                                                'if you have switched from one toolbox to another toolbox, then even though the components exist to do the refresh, the corresponding nodes do not yet exist because this OnRefresh is supposed to provide them. So GetNodeFromPath will return Nothing.
                        RefreshNodeAndChildren(Nodes(0), Controller.ApsimData.FactorComponent)                                     'refresh the tree from this node down.
                    Else
                        RefreshNodeAndChildren(NodeToRefresh, Comp)                                     'refresh the tree from this node down.
                    End If
                End If
            End If
        Catch ex As Exception
            EndUpdate()                                                                         'inbuilt tree function, reinables redrawing of the tree
            Windows.Forms.Cursor.Current = Cursors.Default                                      'set the cursor object back to the default windows cursor (usually an arrow)
            Throw
        End Try

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
        Node.ToolTipText = Comp.Description

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
            Dim ChildTreeNode As TreeNode
            If ChildIndex < Node.Nodes.Count Then
                ChildTreeNode = Node.Nodes(ChildIndex)
            Else
                ChildTreeNode = Node.Nodes.Add(Child.Name)
            End If
            RefreshNodeAndChildren(ChildTreeNode, Child)
            ChildIndex = ChildIndex + 1
        Next
        While Node.Nodes.Count > ChildIndex
            Node.Nodes.Remove(Node.Nodes(Node.Nodes.Count - 1))
        End While
    End Sub
    Private Function GetNodeFromPath(ByVal ChildPath As String) As TreeNode
        ' --------------------------------------------------
        ' Returns a tree node given a fullly delimited path.
        ' --------------------------------------------------
        If ChildPath = "" Then
            Return Nothing
        End If
        'reomve RootComponent namfrom path (first part?)
        Dim name As String = ""
        Dim Path As String = ChildPath.Substring(1)
        Dim PosDelimiter As Integer = Path.IndexOf(PathSeparator)
        'Path will have the RootComponent in it which is not shown in the Factorial Tree
        If PosDelimiter <> -1 Then
            Path = Path.Substring(PosDelimiter + 1)
        End If

        Dim CurrentNode As TreeNode = Nothing
        Do Until Path = ""
            PosDelimiter = Path.IndexOf(PathSeparator)
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
        Return PathSeparator + Controller.ApsimData.RootComponent.Name + PathSeparator + Node.FullPath    'just put an extra "/" in front of the node path, so "root/child" becomes "/root/child" (this is needed because our "Selected Path" root starts with a /, whereas the inbuilt node full path property does not start with a / at the root)
        'Return Node.FullPath    'DO NOT put an extra "/" in front of the node path, so "root/child" becomes "/root/child" (this is needed because our "Selected Path" root starts with a /, whereas the inbuilt node full path property does not start with a / at the root)
    End Function
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
#End Region

#Region "Selection Methods"
    Private Sub TreeView_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Me.MouseDown
        ' ---------------------------------------------------------------
        ' If the user right clicks on a node and that node isn't already
        ' selected, then go select it.
        ' ---------------------------------------------------------------

        'Don't let the user click any other nodes while this code executes

        EnableNodeSelection = False

        'Initialise variables
        Dim ClickedNode As TreeNode = GetNodeAt(e.Location)                         'get the node in the datatree that was clicked on.
        If IsNothing(ClickedNode) Then                                              'if the button was pressed on a non node area of the datatree
            Return                                                                      'do nothing.
        End If

        If (e.X < ClickedNode.Bounds.Left - 20) Then                                'allow for an image 20 pixels to the left of the text of a node
            Return
        End If

        Dim LeftClick As Boolean = (e.Button = Windows.Forms.MouseButtons.Left)   'is it a right mouse button click
        Dim SelectedPath As String = GetPathFromNode(ClickedNode)

        'Right Click Only           
        If LeftClick Then
            'Left Click Only
            'if user clicked on a node and not a blank area (was tested for null earlier)
            'if the user clicked again on the same thing that was already selected -> then do a Rename.
            'click on same thing that was already selected.
            If Not Controller.ApsimData.IsReadOnly _
                    AndAlso SelectedPath = Controller.SelectedFactorialPath _
                    AndAlso ClickedNode.Level > 0 Then

                'if not readonly, 
                'and user has clicked once before, 
                'and what they clicked this time is the same as what they clicked last time, 
                'and they have clicked on a node that is lower down then the root node [can't rename the root node] 

                LabelEdit = True         'set the tree's label edit property  to true, allowing all the nodes on the tree to have their labels edited. (needs to be set to true for Node.BeginEdit() to work)  
                ClickedNode.BeginEdit()  'call the inbuilt tree node function that allows the user to edit the nodes label. (see OnBeforeEdit and OnAfterEdit sub for what happens before and after the user edits the label)
                Exit Sub
            End If
        End If

        'if they clicked on something different to what was already selected -> change the selection
        'Finish off

        Controller.SelectedFactorialPath = SelectedPath   'update the selected paths "in the controller"

        'Let the user click on other nodes again
        EnableNodeSelection = True

    End Sub
    Private Sub OnSelectionChanged(ByVal OldSelection As String, ByVal NewSelection As String)
        ' -----------------------------------------------------------------
        ' Selection has changed - update tree.
        ' -----------------------------------------------------------------
        EnableNodeSelection = False     'don't let the user click any other nodes while this code executes

        'Change the colour of all the old selected nodes to the "unselected" colours
        Dim Node As TreeNode = GetNodeFromPath(OldSelection)    'get the node that the old selected path points to
        If Not IsNothing(Node) Then
            ColourNode(Node)                                'change the colour of the unselected node to the unselected colours.
        End If

        'Change the colour of all the new selected nodes to the "selected" colours
        'get the node that the new selected path points to.
        SelectedNode = GetNodeFromPath(NewSelection)                'set the Tree's selected node to the node specified in the new selected path (just used to trigger the AfterSelect event, which is handled by OnTreeSelectionChanged() subroutine below this subroutine) (nb. we REDO this for EVERY node in NewSelections. We have to do this one node at a time because the Tree does not allow you to select more then one node) 
        ColourNode(SelectedNode)                                    'change the colour of the new selected node to the selected colours.
        SelectedNode.EnsureVisible()                                'use inbuilt tree node function that expands the tree to make sure the node specified is visible in the tree.  

        EnableNodeSelection = True      'let the user click on other nodes again
    End Sub
#End Region

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
        'Me.ContextMenuStrip.Enabled = False

    End Sub
    Private Sub OnAfterEdit(ByVal sender As Object, ByVal e As System.Windows.Forms.NodeLabelEditEventArgs) Handles Me.AfterLabelEdit
        ' ---------------------------------------------------
        ' User has just finished editing the label of a node.
        ' ---------------------------------------------------
        If Not IsNothing(e.Label) Then

            If (e.Label.Length > 0) Then 'Check user typed something in. So you are not trying to rename it to a blank.

                If Not (CSGeneral.Utility.CheckForInvalidChars(e.Label)) Then

                    ' Firstly empty the current selections.
                    Controller.SelectedFactorialPath = ""

                    ' Change the data
                    Dim Comp As ApsimFile.Component = Controller.ApsimData.Find(GetPathFromNode(e.Node))
                    Comp.Name = e.Label

                    ' Now tell the base controller about the new selections.
                    Controller.SelectedFactorialPath = Comp.FullPath

                Else

                    MessageBox.Show("You can not use characters such as < > / \ ' "" ` : ? | * & = ! in the name")
                    e.CancelEdit = True     'cancel the edit event.

                End If

            Else
                e.CancelEdit = True     'cancel the edit event.
            End If
        End If
        LabelEdit = False
        'Me.ContextMenuStrip.Enabled = True
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

        If (Controller.SelectedFactorialPath <> GetPathFromNode(e.Item)) Then
            SelectedNode = e.Item                                                       'add it to the base controller (by setting the selected node of the tree to the dragged item. This then fires the selection changed event for the tree which I think is handled by the base controller. This will add the dragged items to the base controller)
        End If

        'Work out the xml of what you are dragging.
        Dim FullXML As String = ""                                                  'used to store the xml of ALL the components that have been selected in the drag  'reset it to nothing, ready for recreation.
        Dim Comp As ApsimFile.Component = Controller.ApsimData.Find(Controller.SelectedFactorialPath)   'get the component for this particular selected node (using it's path)
        FullXML = FullXML + Comp.FullXML                                            'get the xml for the component and add it to the xml of previous selected nodes
        'PathsBeingDragged = Controller.SelectedPaths                                'store the paths of ALL the nodes that are being dragged in a global variable, so it can be used by other drag events.

        'Raise the other DragDropEvents
        DoDragDrop(FullXML, DragDropEffects.Copy)
        'parameters: (Store xml of what you are dragging in "data" Drag Event Argument), (allowable types of left mouse drags [Drag Drop Effects are of type FlagsAttribute, which allow bitwise operators AND and OR]). 

    End Sub

    Private Sub TreeView_DragOver(ByVal sender As Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles Me.DragOver
        ' --------------------------------------------------
        ' User has dragged a node over us - allow drop?
        ' ------------------------<sor<sorghum />ghum />--------------------------

        'Make sure you are actually dragging something
        If e.Data.GetDataPresent(GetType(System.String)) Then   'check the "data" Drag Event Argument

            'If the mouse is currently dragging over a node and not over blank area
            Dim pt As Point = PointToClient(New Point(e.X, e.Y))    'get the drop location
            Dim DestinationNode As TreeNode = GetNodeAt(pt)         'find the node closest to the drop location
            If Not IsNothing(DestinationNode) Then

                'Work out the type of left drag this is (copy, move, create link/shortcut), and store it in the "Effect" Drag Event Argument
                Dim FullXML As String = e.Data.GetData(DataFormats.Text)
                Dim DropComp As ApsimFile.Component = Controller.ApsimData.Find(GetPathFromNode(DestinationNode))   'get the corresponding component for the destination node.
                'If DropComp.AllowAdd(FullXML) Then                      'if allowed to drop this node onto this destination node

                PathsBeingDragged = Controller.SelectedPaths
                If Not IsNothing(PathsBeingDragged) AndAlso PathsBeingDragged.Count > 0 Then
                    e.Effect = DragDropEffects.Copy
                End If
                'Else                                                    'if NOT allowed to drop this node onto this destination node
                'e.Effect = DragDropEffects.None                         'display circle with line through it symbol
                'End If
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
        Controller.SelectedFactorialPath = GetPathFromNode(DestinationNode)                      'set the selected path for the controller to the path for the destination node

        'Get the xml of what was dragged.
        Dim FullXML As String = e.Data.GetData(DataFormats.Text)                        'it was put into the "data" Drag Event Argument in the DoDragDrop call (in TreeViewDragItem event handler)  

        'Drag using the left mouse button

        'depending on the type of left mouse drag, do the corresponding drag action.
        Select Case e.Effect                                                             'use the "Effect" Drag Event Argument to get the type of left mouse drag
            Case DragDropEffects.Copy                                                           'these DragDropEffects are just the changes to the mouse icon when you hover over a node whilst dragging
                DragCopy(FullXML)
            Case Else                                                                           'should not needed this, but just put it in to prevent errors.
                DragCancel()
        End Select
    End Sub
    'Drag Actions

    'copy
    Private Sub DragCopy(ByVal FullXML As String)
        Controller.FactorialSelection.Add(FullXML)                       'add the dragged nodes to the destination node (destination node is stored as the current selection in the controller) 
    End Sub
    'cancel
    Private Sub DragCancel()
        If Not IsNothing(PathsBeingDragged) Then                'when you drag from datatree in simulation to datatree in a toolbox the PathsBeingDragged disappear and it causes an error. I have done this to avoid the error.
            PathsBeingDragged.Clear()                               'clear the variable storing the dragged nodes 
        End If
    End Sub
#End Region

End Class
