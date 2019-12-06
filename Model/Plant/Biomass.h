#ifndef BiomassH 
#define BiomassH
class Biomass
   {
   public:
      Biomass();
      Biomass(float sDM, float sN, float sP, float nsDM);
      Biomass(const Biomass& From);
      virtual ~Biomass() {}
      virtual void Clear();
      virtual float DM() const {return StructuralDM()+NonStructuralDM();}
      virtual float N()  const {return privateN;}
      virtual float P()  const {return privateP;}

      virtual float StructuralDM() const {return (fabs(privateStructuralDM) > 10e-20) ? privateStructuralDM : 0.0f ;}
      virtual float NonStructuralDM() const {return (fabs(privateNonStructuralDM) > 10e-20) ? privateNonStructuralDM : 0.0f ;}


      virtual void AddStructuralDM(float amount)  ;
      virtual void AddN(float amount)   ;
      virtual void AddP(float amount)   ;

      virtual void AddNonStructuralDM(float amount)  ;

      virtual void SetStructuralDM(float amount)  {privateStructuralDM = amount;};
      virtual void SetN(float amount)   {privateN = amount;};
      virtual void SetP(float amount)   {privateP = amount;};

      virtual void SetNonStructuralDM(float amount)  {privateNonStructuralDM = amount;   CheckBounds();};

      float Pconc() {return (float)divide(P(),DM(),0.0);};
      float Nconc() {return (float)divide(N(),DM(),0.0);};
	  float NconcPercent() {return DM() > 1.0E-4 ? (float)(divide(N(),DM(),0.0)*fract2pcnt) : 0.0f;};
	  float PconcPercent() {return DM() > 1.0E-4 ? (float)(divide(P(),DM(),0.0)*fract2pcnt) : 0.0f;};

      Biomass operator + (const Biomass& rhs);
      Biomass operator - (const Biomass& rhs);
      Biomass operator * (float Fraction);
      virtual Biomass& operator = (const Biomass& rhs);

   protected:
      virtual void CheckBounds() { }

   private:
      float privateStructuralDM;
      float privateNonStructuralDM;
      float privateN;
      float privateP;
};

#endif
