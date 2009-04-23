
// Modification log
// 6 Aug 97 J. Hargreaves  Implementation

#ifndef SeedPool_H
#define SeedPool_H

#ifndef __CSTRING_H
#include <cstring.h>
#endif

#ifndef __IOSTREAM_H
#include <iostream.h>
#endif

class SeedPartType
{
   friend ostream &operator<<(ostream &, const SeedPartType &);
   public:
      SeedPartType();
      virtual ~SeedPartType();                                         // destructor
      SeedPartType(float mealInit, float oilInit);
      SeedPartType(const SeedPartType &SeedPartType);                                  // copy constructor
      const SeedPartType &operator=(const SeedPartType &other);       // Assigment operator
      const SeedPartType &operator=(const float &y);       // Assigment operator

      SeedPartType operator- (const SeedPartType &y) const;  // Command
      SeedPartType operator+ (const SeedPartType &y) const;  // Command
      SeedPartType operator* (const SeedPartType &y) const;  // Command
      SeedPartType operator* (const float &y) const;  // Command
      SeedPartType operator/ (const SeedPartType &y) const;  // Command
      SeedPartType operator/ (const float &y) const;  // Command
      SeedPartType multiply (const SeedPartType &y);  // Command
      SeedPartType divide (SeedPartType &dividend, SeedPartType &divisor, float default_value);  // Command
      float divide (const float dividend, const float divisor, float default_value) const;  // Command

      void setValue(float meal, float oil);
      float total() const;    // Query

      float meal;
      float oil;

   private:
};

class SeedPool
{
   friend ostream &operator<<(ostream &, const SeedPool &);
      public:                                                                       // member functions
            SeedPool();                  // default constructor
            SeedPool(float greenmeal, float greenoil, float senescedmeal, float senescedoil);
            virtual ~SeedPool();                                       // destructor
            SeedPool(const SeedPool &SeedPool);                                  // copy constructor
            const SeedPool &operator=(const SeedPool &other);         // Assigment operator
            const SeedPool &operator=(const float &y);         // Assigment operator

            SeedPool operator- (const SeedPool &y) const;  // Command
            SeedPool operator+ (const SeedPool &y) const;  // Command
            SeedPool operator* (const SeedPool &y) const;  // Command
            SeedPool operator* (const float &y) const;  // Command
            SeedPool operator/ (const SeedPool &y) const;  // Command
            SeedPool operator/ (const float &y) const;  // Command
            SeedPool multiply (const SeedPool &y);  // Command
            void setValue(float greenmeal, float greenoil, float senescedmeal, float senescedoil);
            void setValue(SeedPartType &green, SeedPartType &senesced);
            float total() const;    // Query

            virtual void display(ostream &os = cout) const; // display function



         SeedPartType green;
         SeedPartType senesced;
      private:
};

#endif
