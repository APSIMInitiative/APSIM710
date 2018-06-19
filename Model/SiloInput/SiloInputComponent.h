//---------------------------------------------------------------------------
#ifndef SiloInputComponentH
#define SiloInputComponentH
// ------------------------------------------------------------------
// Encapsulates the APSIM SILO INPUT module
// ------------------------------------------------------------------
class SiloInputComponent : public InputComponent
   {
   public:
      SiloInputComponent(void);
      ~SiloInputComponent(void);

   protected:
      virtual void openInputFile(void);
      virtual void doInit2(void);
   };
#endif
