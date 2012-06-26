<%@ Page Language="C#" AutoEventWireup="true" CodeBehind="Upload.aspx.cs" Inherits="BobWeb.Upload" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml">
<head runat="server">
    <title></title>
    <style type="text/css">
       #Text1
       {
          width: 195px;
          height: 24px;
       }
    </style>
</head>
<body>
    <form id="form1" runat="server">
    <div>
       User name:&nbsp;
       <asp:TextBox ID="UserNameTextBox" runat="server"></asp:TextBox>
&nbsp;&nbsp; Password:
       <asp:TextBox ID="PasswordTextBox" runat="server"></asp:TextBox>
       <asp:Label ID="InvalidLabel" runat="server" Font-Bold="True" ForeColor="Red" 
          Text="Invalid user name or password." Visible="False"></asp:Label>
    </div>
    <p>
       Task ID:&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
       <asp:DropDownList ID="BugList" runat="server" 
          DataSourceID="BugTrackerDataSource" DataTextField="Description" 
          DataValueField="Description" Height="30px" Width="462px">
       </asp:DropDownList>
    </p>
    <p>
       Description: 
       <asp:TextBox ID="DescriptionTextBox" runat="server" Width="463px"></asp:TextBox>
       <asp:Label ID="DescriptionLabel" runat="server" Font-Bold="True" 
          ForeColor="Red" Text="Missing a description." Visible="False"></asp:Label>
    </p>
    <p>
       Patch file to upload:&nbsp;&nbsp;
       <asp:FileUpload ID="FileUpload" runat="server" Width="404px" />
       <asp:Label ID="PatchLabel" runat="server" Font-Bold="True" ForeColor="Red" 
          Text="No patch file specified." Visible="False"></asp:Label>
    </p>
    <p>
       <asp:CheckBox ID="CheckBox" runat="server" Checked="True" 
          Text="If uploaded patch runs clean, do a commit?" />
    </p>
    <p>
       <asp:Button ID="UploadButton" runat="server" onclick="UploadButton_Click" 
          Text="Upload patch" />
    </p>
    <asp:SqlDataSource ID="BugTrackerDataSource" runat="server" 
       ConnectionString="<%$ ConnectionStrings:BugTrackerConnectionString %>" 
       SelectCommand="SELECT CONVERT (VARCHAR, bg_id) + ' - ' + bg_short_desc AS Description, bg_id FROM bugs WHERE (bg_status &lt;&gt; @bg_status) ORDER BY bg_id DESC">
       <SelectParameters>
          <asp:Parameter DefaultValue="5" Name="bg_status" Type="Int32" />
       </SelectParameters>
    </asp:SqlDataSource>
    </form>
</body>
</html>
