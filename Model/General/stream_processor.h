//---------------------------------------------------------------------------
#ifndef stream_processorH
#define stream_processorH

#include <string>
#include <list>
class Stream_processor;
// ------------------------------------------------------------------
//  Short description:
//     Base class for line processor

//  Notes:

//  Changes:
//    DPH 8/10/97

// ------------------------------------------------------------------
class Line_processor
   {
   protected:
      char Macro_char;
      virtual bool Get_macro_value (const char* Macro_name, std::string& Macro_value) {return true;};

   public:
      Line_processor(void);
      virtual bool Replace_macros (std::string& Line);

   friend class Stream_processor;
   };

// ------------------------------------------------------------------
//  Short description:
//     this class takes an input stream and writes the contents to
//     an output stream.  For each line, it calls the specified line
//     processor to give it the opportunity to change the line or
//     to exclude from the out stream.

//  Notes:

//  Changes:
//    DPH 8/10/97

// ------------------------------------------------------------------
class Stream_processor
   {
   public:
      Stream_processor (void);

      void Add_processor (Line_processor* Processor)
         {Processors.push_back (Processor);}

      void Go (std::istream& In_stream,
               std::ostream& Out_stream);

   private:
      std::list<Line_processor*> Processors;
   };
#endif
