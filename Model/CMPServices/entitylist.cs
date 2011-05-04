using System;
using System.Collections;
using System.Collections.Generic;

namespace CMPServices
{
   //============================================================================
   /// <summary>
   /// This class is used for property and event id records that are contained in lists.
   /// </summary>
   //============================================================================
   public class TIDSpec       
   {
      /// <summary>
      /// Owning component
      /// </summary>
      public uint compID;     
      /// <summary>
      /// Property or event to identify
      /// </summary>
      public uint itemID;    
      /// <summary>
      /// FQName of the component.
      /// </summary>
      public String name;    
      /// <summary>
      /// Type of entity
      /// </summary>
      public int kind;       
      /// <summary>
      /// The DDML description
      /// </summary>
      public String sType;  
      /// <summary>
      /// Used when the connection matches the rule (e.g. nearest)
      /// </summary>
      public bool matchesRule;    
   } 
   //==============================================================================
	/// <summary>
	/// TEntityList is used primarily as an container for an array of component
	/// objects. The add() method captures various fields describing the entity.
	/// </summary>
	//==============================================================================
    public class TEntityList
	{
        private List <TIDSpec> infoList;
      //==============================================================================
      /// <summary>
      /// Default constructor
      /// </summary>
      //==============================================================================
		public TEntityList()
		{
            infoList = new List<TIDSpec>();
		}
      //==============================================================================
      /// <summary>
      /// Add an entity to the list.
      /// </summary>
      /// <param name="compID">Owning component</param>
      /// <param name="regID">Property or event to identify</param>
      /// <param name="sName">FQName of the component.</param>
      /// <param name="kind">Type of entity</param>
      /// <param name="sType">The DDML description</param>
      /// <param name="matchesRule">True is this entity matches the connect rule.</param>
      /// <returns>The entity that has been added to the internal list.</returns>
      //==============================================================================
      public TIDSpec add(uint compID, uint regID, String sName, int kind,  String sType, bool matchesRule)
      {
         TIDSpec entity = new TIDSpec();
         entity.compID = compID;
         entity.itemID = regID;
         entity.name = sName;
         entity.kind = kind;
         entity.sType = sType;
         entity.matchesRule = matchesRule;

         infoList.Add(entity);
         return entity;
      }
      //==============================================================================
      /// <summary>
      /// Get the count of list items.
      /// </summary>
      /// <returns>The count of list items.</returns>
      //==============================================================================
      public int count()
      {
         return infoList.Count;
      }
      //==============================================================================
      /// <summary>
      /// Empty the complete list of items.
      /// </summary>
      //==============================================================================
      public void clear()
      {
         infoList.Clear();
      }
      //==============================================================================
      /// <summary>
      /// Delete the selected item.
      /// </summary>
      /// <param name="index">The index in the list. 1 - x.</param>
      //==============================================================================
      public void erase(int index)
      {
         infoList.RemoveAt(index - 1);
      }
      //==============================================================================
      /// <summary>
      /// Get the TEntityRef item at the list position.
      /// </summary>
      /// <param name="index">The index in the list. 1 - x.</param>
      /// <returns>The TIDSpec object.</returns>
      //==============================================================================
      public TIDSpec getEntity(int index)
      {
         if ( (index > 0) && (index <= infoList.Count) )
            return infoList[index - 1];
         else
            return null;
      }
        //==============================================================================
        /// <summary>
        /// Assign the entity at the index in the list with the new entity.
        /// </summary>
        /// <param name="entity"></param>
        /// <param name="index"></param>
        //==============================================================================
        public void storeEntity(TIDSpec entity, int index)
        {
            if ((index > 0) && (index <= infoList.Count))
                infoList[index - 1] = entity;

        }

	}
}
