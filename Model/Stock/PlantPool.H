
// Modification log
// 6 Aug 97 J. Hargreaves  Implementation

#ifndef PlantPool_H
#define PlantPool_H

class PlantPartType
{
   public:
      PlantPartType(void);
      virtual ~PlantPartType(void);                                         // destructor
      friend std::ostream &operator<<(std::ostream &, const PlantPartType &);
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
      public:                                                                       // member functions
            PlantPool(void);                  // default constructor
            PlantPool(float greenLeaf, float greenStem, float greenPod, float senescedLeaf, float senescedStem, float senescedPod);
            virtual ~PlantPool(void);                                       // destructor
            PlantPool(const PlantPool &PlantPool);                                  // copy constructor
            const PlantPool &operator=(const PlantPool &other);         // Assigment operator
            const PlantPool &operator=(const float &y);         // Assigment operator
            friend std::ostream &operator<<(std::ostream &, const PlantPool &);

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

            virtual void display(std::ostream &os = std::cout) const; // display function

         PlantPartType green;
         PlantPartType senesced;
      private:
};

#endif
