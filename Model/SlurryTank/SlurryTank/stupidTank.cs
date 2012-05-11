using System;
using ModelFramework;

    public class StupidTank
    {
        double amount = 0.0;

        [Param]
        double SulphS = 0;
        [Output]
        [Units("kg")]
        public double ENH3 = 0;
        [Input]
        double DM;

        public void addSlurry() //! makes amount 10 times the DM - not sensible in reality!
        {
            amount += 10.0 * DM;
        }

        [EventHandler]
        public void OnInitialised()
        {
            Console.Out.WriteLine("Entered stupidtank OnInitialised");
            SulphS = 1;
            Console.Out.WriteLine(SulphS);
            Console.Out.WriteLine("Exited stupidtank OnInitialised");

        }

        [EventHandler]
        public void OnPrepare() //!called at start of day 
        {
            Console.Out.WriteLine("Entered stupidtank OnPrepare");
            SulphS += 1;
            Console.Out.Write("SulphS = ");
            Console.Out.WriteLine(SulphS);
            Console.Out.WriteLine("Exited stupidtank OnPrepare");
        }

        [EventHandler]
        public void OnProcess()     //called to run daily processes
        {
            ENH3 += 1.0;
            addSlurry();
            Console.Out.WriteLine("Entered stupidtank OnProcess");
            Console.Out.Write("ENH3 = ");
            Console.Out.WriteLine(ENH3);
            Console.Out.Write("DM = ");
            Console.Out.WriteLine(DM);
            Console.Out.Write("amount = ");
            Console.Out.WriteLine(amount);
            Console.Out.WriteLine("Exited stupidtank OnProcess");
        }

}
