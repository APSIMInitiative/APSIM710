using System;
using System.Runtime.InteropServices;

namespace DCC.Install
{
///
/// Summary description for ExecuteDLLFunction.
///
public class ExecuteDLLFunction
{
   [DllImport("kernel32")]
   public extern static int LoadLibrary(string lpLibFileName);
   [DllImport("kernel32")]
   public extern static bool FreeLibrary(int hLibModule);
   [DllImport("kernel32", CharSet=CharSet.Ansi)]
   public extern static int GetProcAddress(int hModule, string lpProcName);

   [DllImport("user32", EntryPoint="CallWindowProc")]
   public static extern int CallWindowProcA(int lpPrevWndFunc, int hwnd, int MSG, int wParam, int lParam);

   private string _dllfile;
   private int _dll;

public string dllfile
{
get { return _dllfile; }
set { _dllfile = value; }
}

public ExecuteDLLFunction(string DLLFile)
{
_dllfile = DLLFile;
_dll = LoadLibrary(_dllfile);
}

~ExecuteDLLFunction()
{
FreeLibrary(_dll);
}

public int Execute(string strFunc)
{
int funcaddr;

if (_dll != 0)
{

funcaddr = GetProcAddress(_dll, strFunc);

if (funcaddr != 0)
{
return CallWindowProcA(funcaddr, 0, 0, 0, 0);
}
else
{
return -2;
}
}
else
{
return -1;
}
}
}
}