//---------------------------------------------------------------------------
#ifndef SimulationH
#define SimulationH

namespace protocol {
// ------------------------------------------------------------------
//  Short description:
//     Encapsulates a top-level run.  It is passed a configuration
//     and a simulation and sets the simulation running.  Exceptions
//     are thrown when an error occurs.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
class Simulation : public ISimulation
   {
   public:
      Simulation(void);
      ~Simulation(void);

      // go execute the simulation
      void go(const std::string& simFilename);

      // set a hook into the message stream.
      void setMessageHook(IMessageHook* hook);

   private:
      IComputation* masterPM;
      ApsimSimulationFile* data;

      // initialise the simulation
      virtual void init(const std::string& sdml);

      // commence the simulation
      virtual void commence(void);

      // terminate the simulation
      virtual void term (void);

      void logError(const char* simFilename, const char* msg);

      // resolve any includes in the specified sdml.
      void resolveIncludes(string& sdml);
   };

} // namespace protocol
#endif
