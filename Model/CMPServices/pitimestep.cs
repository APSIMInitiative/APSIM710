using System;

namespace CMPServices
{
   //============================================================================
	/// <summary>
	/// Summary description for TTimeStep.
	/// TTimeStep encapsulates the time step property used in the Common Modeling Protocol.
	/// </summary>
	//============================================================================
	public class TTimeStep
	{
      private TTimeValue start;    //start of timestep
      private TTimeValue finish;   //end of timestep

      //============================================================================
      /// <summary>
      /// This constant is the DDML type of the timestep property.
      ///<example>
      ///<code>
      ///<![CDATA[
      ///<type name="timestep">
      ///  <field name="startDay" kind="integer4" unit="d"></field>
      ///  <field name="startSec" kind="integer4" unit="s"></field>
      ///  <field name="startSecPart" kind="double" unit="s"></field>
      ///  <field name="endDay" kind="integer4" unit="d"></field>
      ///  <field name="endSec" kind="integer4" unit="s"></field>
      ///  <field name="endSecPart" kind="double" unit="s"></field>
      ///</type>
      /// ]]>
      ///</code>
      ///</example>
      /// </summary>
      //============================================================================
      public const String typeTIMESTEP = "<type name=\"timestep\">"
                             + "<field name=\"startDay\"     kind=\"integer4\" unit=\"d\" />"
                             + "<field name=\"startSec\"     kind=\"integer4\" unit=\"s\" />"
                             + "<field name=\"startSecPart\" kind=\"double\"   unit=\"s\" />"
                             + "<field name=\"endDay\"       kind=\"integer4\" unit=\"d\" />"
                             + "<field name=\"endSec\"       kind=\"integer4\" unit=\"s\" />"
                             + "<field name=\"endSecPart\"   kind=\"double\"   unit=\"s\" />"
                             + "</type>";


		//============================================================================
      /// <summary>
      /// Default constructor
      /// </summary>
      //============================================================================
        public TTimeStep()
        {
            start = new TTimeValue();
            finish = new TTimeValue();
        }
      //============================================================================
      /// <summary>
      /// Initialises this timestep with the field values from the typed value.
      /// </summary>
      /// <param name="timeValue">A TTypedValue that has the correct timestep structure.</param>
      //============================================================================
      public void Set(TTypedValue timeValue)
      {
         start.Set(timeValue.member(1).asInt(), (uint)timeValue.member(2).asInt(), timeValue.member(3).asDouble());
         finish.Set(timeValue.member(4).asInt(), (uint)timeValue.member(5).asInt(), timeValue.member(6).asDouble());
      }
      //============================================================================
      /// <summary>
      /// Sets the fields of the typed value with the timestep values.
      /// </summary>
      /// <param name="timeValue">Ref to the typed value to be set.</param>
      /// <returns>A ref to the typed value that has been set.</returns>
      //============================================================================
      public TTypedValue getTypedValue(ref TTypedValue timeValue)
      {
         timeValue.member(1).setValue((int)start.getDay());
         timeValue.member(2).setValue((int)start.getSec());
         timeValue.member(3).setValue((double)start.getSecPart());

         timeValue.member(4).setValue((int)finish.getDay());
         timeValue.member(5).setValue((int)finish.getSec());
         timeValue.member(6).setValue((double)finish.getSecPart());

         return timeValue;
      }
      //============================================================================
      /// <summary>
      /// Ref to the start time object.
      /// </summary>
      /// <returns></returns>
      //============================================================================
      public TTimeValue getStart() 
      {
         return start;
      }
      //============================================================================
      /// <summary>
      /// Ref to the finish time object.
      /// </summary>
      /// <returns></returns>
      //============================================================================
      public TTimeValue getFinish() 
      {
         return finish;
      }
      //============================================================================
      /// <summary>
      /// Set the start time using a TTimeValue
      /// </summary>
      /// <param name="init"></param>
      //============================================================================
      public void setStart(TTimeValue init) 
      {
         start.Set(init);
      }
      //============================================================================
      /// <summary>
      /// Set the finish time using a TTimeValue
      /// </summary>
      /// <param name="init"></param>
      //============================================================================
      public void setFinish(TTimeValue init)
      {
         finish.Set(init);
      }
	}
}
