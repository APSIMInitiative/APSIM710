using System;
using System.ComponentModel;
using System.Threading;
using Gtk;

namespace ApsimHPC
{
    class MainClass
    {
        public static MainWindow win;

        public static void Main(string[] args)
        {
            Application.Init();
            win = new MainWindow();
            win.Show();
            Application.Run();
        }
    }
}
