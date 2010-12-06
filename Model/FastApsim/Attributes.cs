using System;
using System.Collections.Generic;
using System.Text;


   [AttributeUsage(AttributeTargets.Class)]
   public class CanHaveChildren : System.Attribute
	   {}

   [AttributeUsage(AttributeTargets.Field | AttributeTargets.Property)]
   public class Input : System.Attribute
   { }

   [AttributeUsage(AttributeTargets.Field | AttributeTargets.Property)]
   public class Param : System.Attribute
      {
      public string Alias = null;
      public Param()
         {
         }
      public Param(string ParamName)
         {
         Alias = ParamName;
         }
      }

   [AttributeUsage(AttributeTargets.Field | AttributeTargets.Property)]
   public class Output : System.Attribute
      {
      public Output()
         {
         }

      // Deprecated.
      public Output(string Text)
         {
         }
      }

   [AttributeUsage(AttributeTargets.Method)]
   public class EventHandler : System.Attribute
   { }

   [AttributeUsage(AttributeTargets.Field)]
   public class Ref : System.Attribute
      {
      public string Name;
      public Ref(string N)
         {
         Name = N;
         }
      }

   [AttributeUsage(AttributeTargets.Field)]
   public class RefOptional : System.Attribute
      {
      public string Name;
      public RefOptional(string N)
         {
         Name = N;
         }
      }

   [AttributeUsage(AttributeTargets.Class)]
   public class Description : System.Attribute
      {
      public Description(string Text)
         {
         }
      }

   [AttributeUsage(AttributeTargets.Field | AttributeTargets.Property)]
   public class Units : System.Attribute
      {
      public Units(string text)
         {
         }
      }

   [AttributeUsage(AttributeTargets.Event)]
   public class Event : System.Attribute
      {
      }
