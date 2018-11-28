using System;
using Gtk;

namespace ApsimHPC
{
    class MainClass
    {
        public static MainWindow win;
        public static void Main(string[] args)
        {
            Application.Init();
            JobExecutor executor = new JobExecutor();
            win = new MainWindow(executor);
            win.Show();
            Application.Run();
        }
    }
}
