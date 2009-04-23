
// Modification log
// 6 Aug 97 J. Hargreaves  Implementation

#ifndef PlantPool_H
#define PlantPool_H

#ifndef __CSTRING_H
#include <cstring.h>
#endif

#ifndef __IOSTREAM_H
#include <iostream.h>
#endif

class PlantPartType
{
   friend ostream &operator<<(ostream &, const PlantPartType &);
   public:
      PlantPartType(void);
      virtual ~PlantPartType(void);                                         // destructor
      PlantPartType(float leafInit, float stemInit, float podInit);
      PlantPartType(const PlantPartType &PlantPartType);                                  // copy constructor
      const PlantPartType &operator=(const PlantPartType &other);       // Assigment operator
      const PlantPartType &operator=(const float &y);       // Assigment operator

      PlantPartType operator- (const PlantPartType &y) const;  // Command
      PlantPartType operator+ (const PlantPartType &y) const;  // Command
      PlantPartType operator* (const PlantPartType &y) const;  // Command
      PlantPartType operator* (const float &y) const;  // Command
      PlantPartType operator/ (const PlantPartType &y) const;  // Command
      PlantPartType operator/ (const float &y) const;  // Command
      PlantPartType multiply (const PlantPartType &y);  // Command
      PlantPartType divide (PlantPartType &dividend, PlantPartType &divisor, float default_value);  // Command
      float divide (const float dividend, const float divisor, float default_value) const;  // Command

      void setValue(float leaf, float stem, float pod);
      float total(void) const;    // Query

      float leaf;
      float stem;
      float pod;

   private:
};

class PlantPool
{
   friend ostream &operator<<(ostream &, const PlantPool &);
      public:                                                                       // member functions
            PlantPool(void);                  // default constructor
            PlantPool(float greenLeaf, float greenStem, float greenPod, float senescedLeaf, float senescedStem, float senescedPod);
            virtual ~PlantPool(void);                                       // destructor
            PlantPool(const PlantPool &PlantPool);                                  // copy constructor
            const PlantPool &operator=(const PlantPool &other);         // Assigment operator
            const PlantPool &operator=(const float &y);         // Assigment operator

            PlantPool operator- (const PlantPool &y) const;  // Command
            PlantPool operator+ (const PlantPool &y) const;  // Command
            PlantPool operator* (const PlantPool &y) const;  // Command
            PlantPool operator* (const float &y) const;  // Command
            PlantPool operator/ (const PlantPool &y) const;  // Command
            PlantPool operator/ (const float &y) const;  // Command
            PlantPool multiply (const PlantPool &y);  // Command
            void setValue(float greenLeaf, float greenStem, float greenPod, float senescedLeaf, float senescedStem, float senescedPod);
            void setValue(PlantPartType &green, PlantPartType &senesced);
            float total(void) const;    // Query

            virtual void display(ostream &os = cout) const; // display function



         PlantPartType green;
         PlantPartType senesced;
      private:
};

#endif
