<%@ Page Language="C#" AutoEventWireup="true" CodeBehind="Bob.aspx.cs" Inherits="BobWeb.WebForm1" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml">
<head runat="server">
    <title></title>
</head>
<body>
    <form id="form1" runat="server">
    <div>
    
       <h1>
          APSIM Build system</h1>
    
    </div>
    <p style="margin-left: 0px; margin-bottom: 19px">
       <asp:Button ID="UploadButton" runat="server" onclick="UploadButton_Click" 
          Text="Upload job" />
    &nbsp;&nbsp;&nbsp;&nbsp; Number of rows:
       <asp:TextBox ID="NumRowsTextBox" runat="server" AutoPostBack="True" 
          ontextchanged="NumRowsTextBox_TextChanged">100</asp:TextBox>
&nbsp;&nbsp;
       <asp:CheckBox ID="Passes" runat="server" 
          oncheckedchanged="Passes_CheckedChanged" Text="Only show Passes" 
          AutoPostBack="True" />
    </p>
    <p>
       <asp:GridView ID="GridView" runat="server" 
          CellPadding="4" ForeColor="#333333" 
          GridLines="None" AutoGenerateColumns="False">
          <AlternatingRowStyle BackColor="White" ForeColor="#284775" />
          <Columns>
             <asp:HyperLinkField DataNavigateUrlFields="ID" 
                DataNavigateUrlFormatString="http://bob.apsim.info/BobWeb/DeleteJob.aspx?id={0}" 
                HeaderText="Action" Text="Delete" />
             <asp:BoundField DataField="UserName" HeaderText="User" />
             <asp:HyperLinkField DataNavigateUrlFields="PatchFileName" 
                DataNavigateUrlFormatString="http://bob.apsim.info/files/upload/{0}.zip" 
                DataTextField="PatchFileNameShort" HeaderText="Patch file" />
             <asp:BoundField DataField="Description" HeaderText="Description" />
             <asp:HyperLinkField DataNavigateUrlFields="BugID" 
                DataNavigateUrlFormatString="http://www.apsim.info/BugTracker/edit_bug.aspx?id={0}" 
                DataTextField="BugID" DataTextFormatString="T{0}" HeaderText="Task" />
             <asp:BoundField DataField="Status" HeaderText="Status" HtmlEncode="False" />
             <asp:BoundField DataField="UploadTime" HeaderText="Upload Time" 
                Visible="False" />
             <asp:BoundField DataField="StartTime" HeaderText="Start Time" 
                DataFormatString="{0:dd/MMM/yyyy h:mm tt}" />
             <asp:BoundField DataField="Duration" HeaderText="Duration" />
             <asp:HyperLinkField DataNavigateUrlFields="RevisionNumber" 
                DataNavigateUrlFormatString="http://apsrunet.apsim.info/websvn/revision.php?repname=apsim&amp;path=%2Ftrunk%2F&amp;rev={0}" 
                DataTextField="RevisionNumber" HeaderText="Revision" 
                DataTextFormatString="R{0}" />
             <asp:BoundField DataField="DetailsFileName" HeaderText="Links" 
                HtmlEncode="False" />
          </Columns>
          <EditRowStyle BackColor="#999999" />
          <FooterStyle BackColor="#5D7B9D" Font-Bold="True" ForeColor="White" />
          <HeaderStyle BackColor="#5D7B9D" Font-Bold="True" ForeColor="White" />
          <PagerStyle BackColor="#284775" ForeColor="White" HorizontalAlign="Center" />
          <RowStyle BackColor="#F7F6F3" ForeColor="#333333" VerticalAlign="Top" />
          <SelectedRowStyle BackColor="#E2DED6" Font-Bold="True" ForeColor="#333333" />
          <SortedAscendingCellStyle BackColor="#E9E7E2" />
          <SortedAscendingHeaderStyle BackColor="#506C8C" />
          <SortedDescendingCellStyle BackColor="#FFFDF8" />
          <SortedDescendingHeaderStyle BackColor="#6F8DAE" />
       </asp:GridView>
    </p>
    <p>
       &nbsp;</p>
    <p>
       &nbsp;</p>
    <p>
       &nbsp;</p>
    </form>
</body>
</html>
