#ifndef IndividualLeafH
#define IndividualLeafH

// Abstract class for leaf objects
class IndividualLeaf
   {
   public:
   IndividualLeaf(ScienceAPI& API, plantInterface& plant);
   virtual ~IndividualLeaf() {};

   private:
      plantInterface& Plant;
      ScienceAPI& scienceAPI;
      float population;

   };

#endif

