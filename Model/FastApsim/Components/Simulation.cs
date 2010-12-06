using System;
using System.Collections.Generic;
using System.Text;

[CanHaveChildren]
public class Simulation
   {
   public delegate void NullDelegate();
   public event NullDelegate Tick;
   public event NullDelegate Prepare;
   public event NullDelegate Process;
   public event NullDelegate Post;
   public event NullDelegate Report;
   public event NullDelegate Initialised;

   public void InvokeTick()
      {
      Tick.Invoke();
      if (Prepare != null)
         Prepare.Invoke();
      if (Process != null)
         Process.Invoke();
      if (Post != null)
         Post.Invoke();
      if (Report != null)
         Report.Invoke();
      }
   public void InvokeInitialised()
      {
      if (Initialised != null)
         Initialised.Invoke();
      }

   }

[CanHaveChildren]
public class Area
   {

   }

