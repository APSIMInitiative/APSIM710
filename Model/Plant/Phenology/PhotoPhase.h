#ifndef PhotoPhaseH
#define PhotoPhaseH

class Environment;

class PhotoPhase : public Phase
   {
   protected:
      interpolationFunction stressFunction;
      interpolationFunction photo_tt;       // Growing degree days to complete phase as a function of cum vd
      float photoperiod;
      float twilight;
      string cutoff;

      virtual float stress();

   public:
      void read();
      PhotoPhase(ScienceAPI& scienceAPI, plantInterface& p, const std::string& StartStageName, const std::string& EndStageName)
         : Phase (scienceAPI, p, StartStageName, EndStageName) {}
      void updateTTTargets();
      virtual string description();
   };


#endif

