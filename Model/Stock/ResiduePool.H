
// Modification log
// 6 Aug 97 J. Hargreaves  Implementation

#ifndef ResiduePool_H
#define ResiduePool_H


class ResiduePartType
{
   public:
      ResiduePartType(void);
      virtual ~ResiduePartType(void);							// destructor

      ResiduePartType(float carbohydrateInit, float celluloseInit, float ligninInit);
      ResiduePartType(const ResiduePartType &ResiduePartType);
      const ResiduePartType &operator=(const float value);
      const ResiduePartType &operator=(const ResiduePartType &other);

      friend std::ostream &operator<<(std::ostream &, const ResiduePartType &);

      ResiduePartType operator+ (const float y) ;
      ResiduePartType operator+ (const ResiduePartType &y) const;
      ResiduePartType operator- (const float y) ;
      ResiduePartType operator- (const ResiduePartType &y) const ;
      ResiduePartType operator* (const ResiduePartType &y) const ;
      ResiduePartType operator* (const float y) const;
      ResiduePartType operator/ (const ResiduePartType &divisor)  const;
      ResiduePartType operator/ (const float divisor)  const;

      float divide (float dividend, float divisor, float default_value) const;
      ResiduePartType divide (const ResiduePartType &dividend, const ResiduePartType &divisor, double default_value);
      ResiduePartType multiply (const ResiduePartType &y);
      void setValue(float carbohydrateValue, float celluloseValue, float ligninValue);
      float total(void) const;
      virtual void display(std::ostream &os) const ;


      float carbohydrate;
      float cellulose;
      float lignin;

   private:
};

class ResiduePool
{
	public:												// member functions
		ResiduePool(void);			// default constructor
            ResiduePool(ResiduePartType standing, ResiduePartType lying);
		virtual ~ResiduePool(void);							// destructor
		ResiduePool(const ResiduePool &ResiduePool); 						// copy constructor
		const ResiduePool &operator=(const ResiduePool &other);		// Assigment operator

            ResiduePool(float standingInit, float lyingInit);
            const ResiduePool &operator=(const float value);
            friend std::ostream &operator<<(std::ostream &, const ResiduePool &);

            ResiduePool operator+ (const ResiduePool &y) const;
            ResiduePool operator- (const ResiduePool &y) const;
            ResiduePool operator* (const ResiduePool &y) const;
            ResiduePool operator* (const float y) const;
            ResiduePool operator/ (const ResiduePool &divisor)  const;
            ResiduePool operator/ (const float divisor)  const;
            ResiduePool divide (const ResiduePool &dividend, const ResiduePool &divisor, double default_value);

            float divide (float dividend, float divisor, float default_value) const;

////            void setValue(float standingCarbohydrate, float standingCellulose, float standingLignin, float lyingCarbohydrate, float lyingCellulose, float lyingLignin);
            void setValue(ResiduePartType standing, ResiduePartType lying);
		float total(void) const;  	// Query

         virtual void display(ostream& os = cout) const;	// display function

         ResiduePartType standing;
         ResiduePartType lying;
         std::string name;

	private:
};


#endif
