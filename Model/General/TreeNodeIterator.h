#ifndef TreeNodeIteratorH
#define TreeNodeIteratorH

// turn of the warnings about "Functions containing for are not expanded inline.
#pragma warn -inl

// ------------------------------------------------------------------
// This template provides a tree iterator.
//   It requires of type T the following methods:
//      T* getNextSibling(void) const;
// ------------------------------------------------------------------
template <class T>
struct TreeNodeIterator
   {
   typedef ptrdiff_t   difference_type;
   typedef T value_type;
   typedef T *pointer;
   typedef T &reference;
   typedef std::forward_iterator_tag  iterator_category;

   TreeNodeIterator(const T n)
      : node(n)
      {}
   reference operator* ()
      {return node;}
   pointer operator-> ()
      {return &node;}

   TreeNodeIterator& operator++()
      {
      node = node.getNextSibling();
      return *this;
      }
   TreeNodeIterator operator++(int)
      {
      TreeNodeIterator tmp(*this);
      node = node.getNextSibling();
      return tmp;
      }
   bool operator==(const TreeNodeIterator& rhs) const
      {
      return node == rhs.node;
      }
   bool operator!=(const TreeNodeIterator& rhs) const
      {
      return node != rhs.node;
      }

   private:
      T node;
   };


// ------------------------------------------------------------------
//  Short description:
//     This template provides a aliasing tree iterator.
//     T should be an instance of TreeNodeIterator and Alias can be anything.
//     It requires of type T the following methods:
//        T& operator++()
//        T operator++(int)
//        bool operator==(const T& rhs) const
//        bool operator!=(const T& rhs) const
//        T& firstChild(void) const
//     It requires of type AliasT the following methods:
//        AliasT::AliasT(T&);

//  Changes:
//    DPH 19/11/2001
// ------------------------------------------------------------------
template <class T, class AliasT>
struct TreeNodeAliasIterator
   {
   typedef ptrdiff_t   difference_type;
   typedef AliasT value_type;
   typedef AliasT *pointer;
   typedef AliasT &reference;

   TreeNodeAliasIterator(const T& n)
      : node(n), alias(*node)
      {}
   value_type operator*() const
      {return alias;}
   pointer operator-> ()
      {return &alias;}

   TreeNodeAliasIterator& operator++()
      {
      node++;
      alias = AliasT(*node);
      return *this;
      }
   TreeNodeAliasIterator operator++(int)
      {
      TreeNodeAliasIterator tmp(*this);
      node++;
      alias = AliasT(*node);
      return tmp;
      }
   bool operator==(const TreeNodeAliasIterator& rhs) const
      {
      return node == rhs.node;
      }
   bool operator!=(const TreeNodeAliasIterator& rhs) const
      {
      return node != rhs.node;
      }
   TreeNodeAliasIterator firstChild(void) const
      {
      return node.firstChild();
      }

   private:
      T node;
      AliasT alias;
   };

// restore the warnings about "Functions containing for are not expanded inline.
#pragma warn .inl

#endif

