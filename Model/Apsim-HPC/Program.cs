using System;
using Gtk;

namespace ApsimHPC
{
	class MainClass
	{
	    public static MainWindow win;
		public static void Main (string[] args)
		{
            Console.WriteLine("Beefore init");
			Application.Init ();
            Console.WriteLine("Before jobex");
            JobExecutor executor = new JobExecutor();
			win = new MainWindow (executor);
            Console.WriteLine("Before win.show");
            win.Show ();
            Console.WriteLine("Before run");
            Application.Run ();
		}
	}
}
