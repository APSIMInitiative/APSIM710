

using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.IO;
using System.Xml;
using System.Text;

using CSGeneral;

namespace ApsimFile
   {

   /// <summary>
   /// This class implements a macro language  e.g.
   /// Supports nested foreach loops and nested if blocks.
   ///
   /// [foreach sim in simulation]
   /// [foreach s in sim.soil]
   /// [foreach scrop in s.crop]
   /// [foreach simcrop in sim.crop]
   /// [if [simcrop.name] = [scrop.name]]
   ///   layers = [foreach l in simcrop.layer] [l.dlayer] [endfor]
   ///   Soil curve number = [s.soilwat2.cn2]
   /// [endif]
   /// 
   /// [if   ]
   /// 
   /// [elseif   ]
   ///   [if   ]
   ///   
   ///   [elseif  ]
   ///   
   ///   [else]
   ///   
   ///   [endif]
   /// [elseif   ]
   /// 
   /// [else]
   /// 
   /// [endif]
   /// [endfor]
   /// [endfor]
   /// [endfor]
   /// [endfor]

   /// </summary>
   public class Macro
      {
      // -----------------------
      //	constructor
      // -----------------------
      public Macro()
         {

         }
      // ------------------------------------------------------------------
      // Go generate all files, putting all files in the specified OutputDirectory.
      // This method returns a list of filenames that were generated.
      // ------------------------------------------------------------------
      public string Go(XmlNode MacroValues, string MacroContents)
         {
         string Contents = MacroContents;
         ParseIncludes(ref Contents);
         ParseComments(ref Contents);

         Contents = ParseForEach(MacroValues, Contents);
         ReplaceGlobalMacros(ref Contents, MacroValues);
         ParseIF(ref Contents);
         ParseToLower(ref Contents);
         return Contents;
         }
      // ------------------------------------------------------------------
      // Go generate all files, putting all files in the specified OutputDirectory.
      // This method returns a list of filenames that were generated.
      // ------------------------------------------------------------------
      public StringCollection Go(XmlNode MacroValues, string MacroContents, string OutputDirectory, bool AppendToFile)
         {
         string Contents = Go(MacroValues, MacroContents);
         Contents = ReplaceHTMLSymbols(Contents);
         return WriteStringToFiles(Contents, OutputDirectory, AppendToFile);
         }
      // ------------------------------------------------------------------
      // Parse and remove all foreach macros from specified string.
      // Contents is the full text to parse.
      //
      // ------------------------------------------------------------------
      public string ParseForEach(XmlNode MacroValues, string Contents)
         {
         StringCollection AliasNames = new StringCollection();
         XmlNode[] AliasNodes = new XmlNode[100];
         AliasNames.Add(XmlHelper.Type(MacroValues).ToLower());
         AliasNodes[0] = MacroValues;
         return ParseForEach(Contents, MacroValues, AliasNames, AliasNodes);
         }


      // ------------------------------------------------------------------
      // Parse and remove all foreach macros from specified string.
      // Contents is the full text to parse.
      //
      // ------------------------------------------------------------------
      string ParseForEach(string Contents,
            XmlNode ValuesNode,
         StringCollection AliasNames,
            XmlNode[] AliasNodes)
         {
         // Locate the next foreach macro.
         int PosForEach = FindForEachMacro(Contents, 0);
         while (PosForEach != -1)
            {
            // ok found a foreach - now parse it to get the variable alias before the 'in' keyword
            // and the variable name after the 'in' keyword.
            // e.g. [foreach scrop in s.crop]
            //			scrop is the alias and s.crop is the variable name.
            string ForEachAlias, NodeName, NodeType;
            int PosAfterForEach;
            ParseForEachMacro(Contents, PosForEach, out ForEachAlias, out NodeName, out NodeType, out PosAfterForEach);

            // find the matching endfor macro.
            int PosEndForEach = FindMatchingEndFor(Contents, PosAfterForEach);

            // parse the endfor macro
            int PosAfterEndFor = ParseEndForMacro(Contents, PosEndForEach);

            // get the bit of text before the foreach macro
            string PreForEachText = Contents.Substring(0, AdjustStartPos(ref Contents, PosForEach));

            // get the contents of the foreach body
            string ForEachText = Contents.Substring(PosAfterForEach, AdjustStartPos(ref Contents, PosEndForEach) - PosAfterForEach);

            // get the bit of text after the endfor macro
            string PostForEachText = Contents.Substring(PosAfterEndFor);

            // resolve node name to get a XmlNode node.
            XmlNode MacroNode = ResolveNode(NodeName, AliasNames, AliasNodes);

            // Loop through all matching child nodes and duplicate the foreach body for each child.
            StringBuilder Body = new StringBuilder();
            List<XmlNode> ChildNodes = XmlHelper.ChildNodes(MacroNode, NodeType);
            foreach (XmlNode Child in ChildNodes)
               {
               AliasNames.Add(ForEachAlias.ToLower());
               AliasNodes[AliasNames.Count - 1] = Child;

               // recurse back and create a new for each body.
               string NewForEachBody = ParseForEach(ForEachText, ValuesNode, AliasNames, AliasNodes);

               // Replace any macros in this new text.
               ReplaceLocalMacros(ref NewForEachBody, AliasNames, AliasNodes);

               // Remove local alias'
               AliasNames.Remove(ForEachAlias.ToLower());

               Body.Append(NewForEachBody);
               }

            ForEachText = Body.ToString();

            Contents = PreForEachText + ForEachText + PostForEachText;

            // locate next for_each
            PosForEach = FindForEachMacro(Contents, 0);
            }
         return Contents;
         }
      // ------------------------------------------------------------------
      // Adjust the start position of a macro. This routine will remove
      // unwanted spaces on the front of the macro if the macro has
      // nothing before it on the line.
      // ------------------------------------------------------------------
      int AdjustStartPos(ref string Contents, int PosStartOfMacro)
         {
         if (PosStartOfMacro > 0)
            {
            int Pos = PosStartOfMacro - 1;
            while (Pos > 0 && Contents[Pos] == ' ')
               Pos--;

            if (Contents[Pos] == '\n')
                return Pos + 1;
            else
                if (Pos == 0)
                    return 0;
                else
                    return PosStartOfMacro;
            }
         else
            return 0;
         }
      // ------------------------------------------------------------------
      // Adjust the start position of a macro. This routine will remove
      // unwanted spaces on the front of the macro if the macro has
      // nothing before it on the line.
      // Same as the version above, but for StringBuilder instead of string
      // ------------------------------------------------------------------
      int AdjustStartPos(ref StringBuilder Contents, int PosStartOfMacro)
      {
          if (PosStartOfMacro > 0)
          {
              int Pos = PosStartOfMacro - 1;
              while (Pos > 0 && Contents[Pos] == ' ')
                  Pos--;

              if (Contents[Pos] == '\n')
                  return Pos + 1;
              else
                  if (Pos == 0)
                      return 0;
                  else
                      return PosStartOfMacro;
          }
          else
              return 0;
      }
      // ------------------------------------------------------------------
      /// <summary>
      /// Adjust the ending of a macro line to remove '      \r\n' or \n
      /// This routine will remove
      /// unwanted spaces and a carriage return on end of the macro
      /// if there is nothing else between the end of the macro and
      /// the end of the line.
      /// </summary>
      /// <param name="Contents"></param>
      /// <param name="PosMacro">Position of ']'</param>
      /// <returns>The first char position after ']' when no whitespace found.
      /// Otherwise it returns the char after '\n'</returns>
      // ------------------------------------------------------------------
      int AdjustEndPos(ref string Contents, int PosMacro)
      {
          int PosEndOfMacro = PosMacro;
          if (Contents[PosMacro] != ']')
              PosEndOfMacro = Contents.IndexOf(']', PosMacro);
          PosEndOfMacro++;

          int Pos = PosEndOfMacro;
          //while spaces or CRLF in the line then increment position
          while (Pos < Contents.Length)
          {
              if (Contents[Pos] == ' ' || Contents[Pos] == '\r' || Contents[Pos] == '\n')
              {
                  if (Contents[Pos] == '\n')    //eoln
                      return Pos + 1;
                  Pos++;
              }
              else
                  return PosEndOfMacro;   //found something else so terminate and return
          }
          return PosEndOfMacro;
      }
      // ------------------------------------------------------------------
      // Adjust the end position of a macro. This routine will remove
      // unwanted spaces and a carriage return on end of the macro
      // if there is nothing else between the end of the macro and
      // the end of the line.
      // Same as the version above, but for StringBuilder instead of string
      // ------------------------------------------------------------------
      int AdjustEndPos(ref StringBuilder Contents, int PosMacro)
      {
          int PosEndOfMacro = PosMacro;
          if (Contents[PosMacro] != ']')
              PosEndOfMacro = Contents.IndexOf("]", PosMacro, false);
          PosEndOfMacro++;
          int Pos = PosEndOfMacro;
          //while spaces or CRLF in the line then increment position
          while (Pos < Contents.Length)
          {
              if (Contents[Pos] == ' ' || Contents[Pos] == '\r' || Contents[Pos] == '\n')
              {
                  if (Contents[Pos] == '\n')    //eoln
                      return Pos + 1;
                  Pos++;
              }
              else
                  return PosEndOfMacro;   //found something else so terminate and return
          }
          return PosEndOfMacro;
      }
      //---------------------------------------------------------------
      // Find the start of a foreach macro in the specified contents.
      //---------------------------------------------------------------
      int FindForEachMacro(string Contents, int StartPos)
         {
             return Contents.IndexOf("[foreach ", StartPos, StringComparison.Ordinal);
         }
      // -------------------------------------------------
      // Parses a string like: [foreach simulation.soil as s]
      //		Returns:
      //			ForEachAlias = s
      //			NodeName = simulation
      //			NodeType = soil
      // -------------------------------------------------
      void ParseForEachMacro(string Contents, int PosForEach, out string ForEachAlias,
         out string NodeName, out string NodeType, out int PosAfterForEach)
         {
             PosAfterForEach = Contents.IndexOf("]", PosForEach, StringComparison.Ordinal);
         if (PosAfterForEach == -1)
            throw new Exception("Expected a ']' character while trying to parse a foreach macro");
         string Macro = Contents.Substring(PosForEach + 1, PosAfterForEach - PosForEach - 1);

         char[] delimiters = { ' ' };
         string[] words = Macro.Split(delimiters, 4);

         if (words[0] != "foreach")
            throw new Exception("Expected a 'foreach' keyword while trying to parse a foreach macro");
         if (words[1] == "")
            throw new Exception("Expected a variable name after the  'foreach' keyword while trying to parse a foreach macro");

         // parse the node name and type
         int PosPeriod = words[1].IndexOf('.');
         if (PosPeriod == -1)
            throw new Exception("Invalid variable name in foreach macro: " + words[1]);
         NodeName = words[1].Substring(0, PosPeriod);
         NodeType = words[1].Substring(PosPeriod + 1);

         // Parse the alias if it exists.
         if (words.Length == 4)
            {
            if (words[2] != "as")
               throw new Exception("Expected an 'as' keyword while trying to parse a foreach macro. Got a '" + words[2] + "' instead.");
            if (words[3] == "")
               throw new Exception("Expected an alias after an 'as' keyword while trying to parse a foreach macro");
            ForEachAlias = words[3];
            }
         else if (words.Length == 2)
            ForEachAlias = NodeType;
         else
            throw new Exception("Invalid foreach macro at : " + Contents);
         PosAfterForEach = AdjustEndPos(ref Contents, PosAfterForEach);
         }
      //---------------------------------------------------------------
      // Parse the endfor macro and return the position to just after
      // the macro.
      //---------------------------------------------------------------
      int ParseEndForMacro(string Contents, int PosEndForEach)
         {
         return AdjustEndPos(ref Contents, PosEndForEach);
         }
      //---------------------------------------------------------------
      // Find the matching endfor for the specified foreach.
      //---------------------------------------------------------------
      int FindMatchingEndFor(string Contents, int PosForEachBody)
         {
         // There may be nested loops - therefore
         // need to count #for_each and #endfor statements until
         // count = 0
         int PosEndFor = -1;
         int CurrentPos = PosForEachBody;
         int Count = 1;
         while (Count > 0)
            {
            int PosForEach = FindForEachMacro(Contents, CurrentPos);
            PosEndFor = Contents.IndexOf("[endfor]", CurrentPos, StringComparison.Ordinal);
            if (PosForEach != -1 && PosForEach < PosEndFor)
               {
               Count++;
               CurrentPos = PosForEach + 1;
               }
            else
               {
               Count--;
               CurrentPos = PosEndFor;
               }
            CurrentPos++;
            }
         if (PosEndFor == -1)
            throw new Exception("Missing an [endfor] macro");

         return PosEndFor;
         }
      //---------------------------------------------------------------
      // Resolve the specified alias into an XmlNode node.
      //---------------------------------------------------------------
      XmlNode ResolveNode(string Alias, StringCollection AliasNames, XmlNode[] AliasNodes)
         {
         int PosAlias = AliasNames.IndexOf(Alias.ToLower());
         if (PosAlias == -1)
            throw new Exception("Invalid alias specified in foreach macro: " + Alias);

         return AliasNodes[PosAlias];
         }
      //---------------------------------------------------------------
      // Replace all macros in the specified Contents.
      //---------------------------------------------------------------
      void ReplaceLocalMacros(ref string Contents, StringCollection AliasNames, XmlNode[] AliasNodes)
         {
         char[] delimiters = { '.' };

         int PosStartMacro = Contents.IndexOf('[');
         while (PosStartMacro != -1)
            {
            int PosEndMacro = Contents.IndexOf(']', PosStartMacro);
            string Macro = Contents.Substring(PosStartMacro + 1, PosEndMacro - PosStartMacro - 1);
            string[] words = Macro.Split(delimiters, 2);
            if (words.Length == 2)
               {
               int PosAlias = AliasNames.IndexOf(words[0].ToLower());
               if (PosAlias != -1)
                  {
                  XmlNode node = AliasNodes[PosAlias];
                  string Value = GetValueFromNode(node, words[1]);
                  if (Value != null)
                     {
                     Contents = Contents.Remove(PosStartMacro, Macro.Length + 2);
                     Contents = Contents.Insert(PosStartMacro, Value);
                     }
                  }
               }
            PosStartMacro = Contents.IndexOf('[', PosStartMacro + 1);
            }
         }

      //---------------------------------------------------------------
      // Replace global macros in the specified Contents.
      //---------------------------------------------------------------
      void ReplaceGlobalMacros(ref string Contents, XmlNode Values)
         {
         int PosStartMacro = Contents.IndexOf('[');
         while (PosStartMacro != -1)
            {
            int PosEndMacro = Contents.IndexOf(']', PosStartMacro);
            string Macro = Contents.Substring(PosStartMacro + 1, PosEndMacro - PosStartMacro - 1);
            int PosPeriod = Macro.IndexOf(".");
            if (PosPeriod != -1)
               {
               string MacroFirstBit = Macro.Substring(0, PosPeriod);
               if (MacroFirstBit == Values.Name || MacroFirstBit == XmlHelper.Type(Values))
                  Macro = Macro.Substring(PosPeriod + 1);
               }

            if (Macro != "")
               {
               string Value = GetValueFromNode(Values, Macro);
               if (Value != null)
                  {
                  Contents = Contents.Remove(PosStartMacro, PosEndMacro - PosStartMacro - 1 + 2);
                  Contents = Contents.Insert(PosStartMacro, Value);
                  }
               }
            PosStartMacro = Contents.IndexOf('[', PosStartMacro + 1);
            }
         }
      //---------------------------------------------------------------
      // Return a attribute value or child value from the specified child node
      //---------------------------------------------------------------
      string GetValueFromNode(XmlNode Child, string Macro)
         {
         int PosLastPeriod = Macro.LastIndexOf('.');
         string FormatString = "";
         if (PosLastPeriod != -1)
            {
            string ChildName = Macro.Substring(0, PosLastPeriod);
            Macro = Macro.Substring(PosLastPeriod + 1);
            int NumDecPlaces;
            if (int.TryParse(Macro, out NumDecPlaces))
               {
               FormatString = "f" + NumDecPlaces.ToString();
               Macro = ChildName;
               }
            else
               {
               ChildName = ChildName.Replace(".", "\\");
               if (ChildName == "") return null;
               XmlNode NewChild = XmlHelper.Find(Child, ChildName);
               if (NewChild != null)
                  Child = NewChild;
               else if (ChildName != "" && ChildName.ToLower() != XmlHelper.Type(Child).ToLower())
                  return null;   // Invalid child name
               }
            }

         string Value;
         if (Macro == "name")
            Value = XmlHelper.Name(Child);
         else if (Macro == "xmltype")
            Value = XmlHelper.Type(Child);
         else if (XmlHelper.Attribute(Child, Macro) != "")
            Value = XmlHelper.Attribute(Child, Macro);
         else if (Macro == "xml")
            Value = Child.OuterXml;
         else if (Macro == "innerxml")
            Value = Child.InnerXml;
         else
            {
            if (XmlHelper.Find(Child, Macro) != null)
               Value = XmlHelper.Value(Child, Macro);
            else
               return null;
            }

         if (FormatString != "")
            Value = Convert.ToDouble(Value).ToString(FormatString);

         return Value;
         }
        //==================================================================================
        /// <summary>
        /// Parse the IF statements and return the modified string.
        /// </summary>
        /// <param name="Contents"></param>
        void ParseIF(ref String Contents)
        {
            int startBlock, endBlock;
            Contents = ParseEachIF(Contents, 0, out startBlock, out endBlock);
        }
        //==================================================================================
        /// <summary>
        /// Recursive parsing of IF blocks. Supports multiple elseif levels.
        /// </summary>
        /// <param name="Contents">Block of macro script to parse</param>
        /// <param name="origin">Starting point in the script</param>
        /// <param name="start"></param>
        /// <param name="end">The end char in Contents parsed</param>
        /// <returns>The modified IF block. Will return "" if no sub IF block is found in Contents.</returns>
        String ParseEachIF(string Contents, int origin, out int start, out int end)
        {
            string SubIfBlock;
            int startSubIf, endSubIf;

            StringBuilder ParsedResult = new StringBuilder();
            start = 0;
            end = start;
            String IfBlock;
            int PosIF = FindIFMacro(Contents, origin);
            while ((PosIF != -1) && (PosIF < Contents.Length))
            {
                IfBlock = "";
                int PosEndMacro = FindMatchingCloseBracket(ref Contents, PosIF + 1);    //finds ']'
                int PosEndIF = FindMatchingEndBlock(Contents, PosEndMacro, "[endif]");
                if (PosEndIF != -1)
                {
                    //evaluate the condition 
                    int CondStart = Contents.IndexOf(" ", PosIF + 1);
                    int CondEnd = PosEndMacro - 1;
                    PosEndMacro = AdjustEndPos(ref Contents, PosEndMacro); //now move to first useful char after ]
                    Boolean ok = EvaluateIf(Contents.Substring(CondStart, CondEnd - CondStart + 1));
                    if (ok) //use IF True section
                    {
                        //this section is between IF - ELSEIF or IF - ELSE or IF - ENDIF
                        String sWholeIfBlock = Contents.Substring(PosEndMacro, PosEndIF - PosEndMacro);
                        int PosTerminateIF = findIFTerminator(sWholeIfBlock);
                        PosTerminateIF = AdjustStartPos(ref sWholeIfBlock, PosTerminateIF);
                        IfBlock = sWholeIfBlock.Substring(0, PosTerminateIF);
                    }
                    else
                    {
                        int PosEndPrevMacro = PosEndMacro;
                        //look for each elseif and evaluate them until success
                        int PosElseIF = FindElseIFMacro(Contents.Substring(PosEndPrevMacro, PosEndIF - PosEndPrevMacro));
                        Boolean elseIfSuccess = false;
                        while ((PosElseIF != -1) && !elseIfSuccess)
                        {
                            CondStart = Contents.IndexOf(" ", PosEndPrevMacro + 1 + PosElseIF);
                            int PosEndElseIFMacro = FindMatchingCloseBracket(ref Contents, CondStart + 1);
                            CondEnd = PosEndElseIFMacro - 1;
                            PosEndElseIFMacro = AdjustEndPos(ref Contents, PosEndElseIFMacro); //now move to first useful char after ]
                            String sWholeIfBlock = Contents.Substring(PosEndElseIFMacro, PosEndIF - PosEndElseIFMacro);
                            ok = EvaluateIf(Contents.Substring(CondStart, CondEnd - CondStart + 1));
                            if (ok)   //if condition is ok then
                            {
                                int PosTerminateElseIF = findIFTerminator(sWholeIfBlock);
                                PosTerminateElseIF = AdjustStartPos(ref sWholeIfBlock, PosTerminateElseIF);
                                IfBlock = sWholeIfBlock.Substring(0, PosTerminateElseIF);
                                elseIfSuccess = true;   //terminate here
                            }
                            else
                            {
                                PosElseIF = FindElseIFMacro(sWholeIfBlock); //get next [elseif
                                PosEndPrevMacro = PosEndElseIFMacro;
                            }
                        }
                        //otherwise fall back to the Else (terminated by endif)
                        if (!elseIfSuccess)
                        {
                            int PosElse = FindMatchingEndBlock(Contents, PosEndMacro, "[else]");
                            if ((PosElse != -1) && (PosElse < PosEndIF))
                            {
                                int PosEndElseMacro = FindMatchingCloseBracket(ref Contents, PosElse + 1);
                                PosEndElseMacro = AdjustEndPos(ref Contents, PosEndElseMacro); //now move to first useful char after ]
                                IfBlock = Contents.Substring(PosEndElseMacro, AdjustStartPos(ref Contents, PosEndIF) - PosEndElseMacro);
                            }
                        }
                    }

                    start = PosIF;
                    end = FindMatchingCloseBracket(ref Contents, PosEndIF + 1);
                    end = AdjustEndPos(ref Contents, end); //now move to first useful char after ]
                    string pre = Contents.Substring(origin, AdjustStartPos(ref Contents, PosIF) - origin); //store prepend if there is one
                    SubIfBlock = ParseEachIF(IfBlock, 0, out startSubIf, out endSubIf);                       //recurse
                    if (SubIfBlock.Length > 0)
                    {
                        ParsedResult.Append(pre + SubIfBlock);
                    }
                    else
                        ParsedResult.Append(pre + IfBlock);

                    origin = end;
                    PosIF = FindIFMacro(Contents, origin);
                }
                else
                    PosIF = -1;
            }
            //append any remaining script
            if (origin < Contents.Length)
                 ParsedResult.Append(Contents.Substring(end, Contents.Length - end));

            return (ParsedResult.ToString());
        }
      //============================================================= 
      int FindIFMacro(string Contents, int StartPos)
      {
          return Contents.IndexOf("[if ", StartPos, StringComparison.Ordinal);
      }
      //===============================================================================
      /// <summary>
      /// Find the next [elseif macro in this block. 
      /// </summary>
      /// <param name="Contents"></param>
      /// <returns>The position of the macro</returns>
      int FindElseIFMacro(string Contents)
      {
          int PosElseIF = FindMatchingEndBlock(Contents, 0, "[elseif");

          return PosElseIF;
      }
      //===============================================================================
      /// <summary>
      /// Finds the terminating macro for an IF, ELSEIF, ELSE 
      /// from within the Contents of an IF block
      /// </summary>
      /// <param name="Contents">A block of code from within an [if] [endif]</param>
      /// <returns>The index in this substring</returns>
      private int findIFTerminator(String Contents)
      {
          int PosEndIF = FindMatchingEndBlock(Contents, 0, "[endif]");
          int PosElse = FindMatchingEndBlock(Contents, 0, "[else]");
          int PosElseIF = FindMatchingEndBlock(Contents, 0, "[elseif");

          if (PosElse == -1)
              PosElse = Contents.Length;
          if (PosElseIF == -1)
              PosElseIF = Contents.Length;
          if (PosEndIF == -1)
              PosEndIF = Contents.Length;

          int PosEndBlock = Math.Min(Math.Min(PosElseIF, PosElse), PosEndIF);

          return PosEndBlock;
      }
      //==================================================================================
      /// <summary>
      /// Find terminator for [if, [elseif that is of type [elseif, [else], [endif]
      /// </summary>
      /// <param name="Contents">Source string</param>
      /// <param name="PosEndMacro">Position at end of [if ...]</param>
      /// <param name="Ending">[elseif], [else], [endif]</param>
      /// <returns>The position of the end of the code block up to the '[' in [endif</returns>
      private int FindMatchingEndBlock(string Contents, int PosEndMacro, string Ending)
      {
          int CurrentPos = PosEndMacro;
          int PosEndBlock = -1;
          int PosIF = FindIFMacro(Contents, CurrentPos); //determine if next level starts
          int PosNextEnd = Contents.IndexOf(Ending, CurrentPos, StringComparison.Ordinal);
          while ((PosIF != -1) && (PosIF < PosNextEnd)) //if [IF found then recurse
          {   //needs some validation of matching if-endif
              PosEndBlock = FindMatchingEndBlock(Contents, FindMatchingCloseBracket(ref Contents, PosIF + 1), "[endif]");
              CurrentPos = FindMatchingCloseBracket(ref Contents, PosEndBlock + 1);
              PosIF = FindIFMacro(Contents, CurrentPos); //another IF macro?
              PosNextEnd = Contents.IndexOf(Ending, CurrentPos, StringComparison.Ordinal);
          }
          PosEndBlock = PosNextEnd; 

          return PosEndBlock;
      }
      //---------------------------------------------------------------
      // Find the matching close bracket starting from the specified
      // position 
      //---------------------------------------------------------------
      int FindMatchingCloseBracket(ref string Contents, int Pos)
         {
         int Count = 1;
         while (Pos < Contents.Length && Count != 0)
            {
            if (Contents[Pos] == '[')
               Count++;
            if (Contents[Pos] == ']')
               Count--;
            Pos++;
            }
         if (Count != 0)
            throw new Exception("Badly formed if statement: " + Contents.Substring(Pos));
         return Pos - 1;
         }

      //---------------------------------------------------------------
      // Find the matching close bracket starting from the specified
      // position 
      // Same as the above, but for StringBuilder instead of string
      //---------------------------------------------------------------
      int FindMatchingCloseBracket(ref StringBuilder Contents, int Pos)
      {
          int Count = 1;
          while (Pos < Contents.Length && Count != 0)
          {
              if (Contents[Pos] == '[')
                  Count++;
              if (Contents[Pos] == ']')
                  Count--;
              Pos++;
          }
          if (Count != 0)
              throw new Exception("Badly formed if statement: " + Contents.ToString(Pos, 25));
          return Pos - 1;
      }

      //---------------------------------------------------------------
      // Parse all comment statements and remove.
      //---------------------------------------------------------------
      void ParseComments(ref string Contents)
         {
             int PosComment = Contents.IndexOf("[comment]", StringComparison.Ordinal);
         while (PosComment != -1)
            {
            PosComment = AdjustStartPos(ref Contents, PosComment);
            int PosEndComment = Contents.IndexOf("[endcomment]", PosComment + 1, StringComparison.Ordinal);
            if (PosEndComment == -1)
               throw new Exception("Cannot find matching [endcomment] macro");
            PosEndComment = AdjustEndPos(ref Contents, PosEndComment);
            Contents = Contents.Remove(PosComment, PosEndComment - PosComment);
            PosComment = Contents.IndexOf("[comment]", PosComment + 1, StringComparison.Ordinal);
            }
         }
      void ParseToLower(ref string Contents)
         {
         const string opentext = "[tolower]";
         const string closetext = "[endtolower]";

         int Pos = Contents.IndexOf(opentext, StringComparison.Ordinal);
         while (Pos != -1)
            {
            int PosEnd = Contents.IndexOf(closetext, Pos, StringComparison.Ordinal);
            if (PosEnd == -1)
               throw new Exception("Cannot find matching [endtolower] macro");

			int n = PosEnd - Pos - opentext.Length;

			string text = Contents.Substring(Pos + opentext.Length, n);

            Contents =  Contents.Substring(0, Pos ) + 
					     text.ToLower() +
						 Contents.Substring(PosEnd + closetext.Length, Contents.Length - (PosEnd + closetext.Length));
            Pos = Contents.IndexOf(opentext, Pos, StringComparison.Ordinal);
            }
         }

		//---------------------------------------------------------------
      // Parse all comment statements and remove.
      //---------------------------------------------------------------
      void ParseIncludes(ref string Contents)
         {
         const string opentext = "[include]";
         const string closetext = "[endinclude]";

         int PosInclude = Contents.IndexOf(opentext, StringComparison.Ordinal);
         while (PosInclude != -1)
            {
            PosInclude = AdjustStartPos(ref Contents, PosInclude);
            int PosEndInclude = Contents.IndexOf(closetext, PosInclude, StringComparison.Ordinal);
            if (PosEndInclude == -1)
               throw new Exception("Cannot find matching [endinclude] macro");
            PosEndInclude = AdjustEndPos(ref Contents, PosEndInclude);
            string filename = Contents.Substring(PosInclude + opentext.Length, PosEndInclude - PosInclude - opentext.Length - closetext.Length - 2);
            filename = Configuration.RemoveMacros(filename);
            string oldtext = Contents.Substring(PosInclude, PosEndInclude - PosInclude);
            if (!File.Exists(filename))
               throw new Exception("file not found: " + filename);
            StreamReader sr = new StreamReader(filename);
            string newtext = sr.ReadToEnd();

            Contents = Contents.Replace(oldtext, newtext);
            PosInclude = Contents.IndexOf("[include]", PosInclude, StringComparison.Ordinal);
            }
         }
      //---------------------------------------------------------------
      // Evaluate the specified IF macro. Return true if it equates
      // to true.
      //---------------------------------------------------------------
      bool EvaluateIf(string IfMacro)
         {
         StringCollection s = StringManip.SplitStringHonouringQuotes(IfMacro, " ");

         if (s.Count == 0)
            return false;
         if (s.Count == 1 && s[0] == "\"\"")
            return false;
         if (s.Count == 1)
            return (s[0].IndexOf('[') == -1 || s[0].IndexOf(']') == -1);
         if (s.Count == 2)
             return true;
         //			if (s.Count != 3)
         //				throw new Exception("Badly formatted if statement: " + IfMacro);
         char[] operators = { '<', '>' };
         string lhs = s[0];
         string op = s[1];
         string rhs = s[2];
         lhs.Trim();
         rhs.Trim();
         if (op == "=")
            {
            if (rhs == "\"\"")
               return (lhs.IndexOf('[') != -1 && lhs.IndexOf(']') != -1);
            return StringManip.StringsAreEqual(lhs, rhs);
            }
         else if (op == "<>")
            return !StringManip.StringsAreEqual(lhs, rhs);
         else if (op == "!=")
            return !StringManip.StringsAreEqual(lhs, rhs);
         else if (op.IndexOfAny(operators) == -1)
            return (s.Count >= 1);


         else
            {
            double lhsValue, rhsValue;
            try
               {
               lhsValue = Convert.ToDouble(lhs);
               rhsValue = Convert.ToDouble(rhs);
               }
            catch (Exception)
               {
               return false;
               }
            if (op == "<")
               return (lhsValue < rhsValue);
            else if (op == "<=")
               return (lhsValue <= rhsValue);
            else if (op == ">")
               return (lhsValue > rhsValue);
            else if (op == ">=")
               return (lhsValue >= rhsValue);
            else
               throw new Exception("Unknown if macro operator: " + op);
            }

         }
      //---------------------------------------------------------------
      // Write specified contents to files.
      //---------------------------------------------------------------
      StringCollection WriteStringToFiles(string Contents, string OutputDirectory, bool AppendToFile)
         {
         StringCollection FileNamesCreated = new StringCollection();
         const string FileMacro = "[file";
         int PosFile = Contents.IndexOf(FileMacro, StringComparison.Ordinal);
         while (PosFile != -1)
            {
            PosFile += FileMacro.Length;
            int PosEndFile = Contents.IndexOf(']', PosFile);
            string Filename = Contents.Substring(PosFile, PosEndFile - PosFile);
            Filename = Filename.Trim();
            if (OutputDirectory != "")
               Filename = OutputDirectory + "/" + Filename;

            int PosStartFileBody = AdjustEndPos(ref Contents, PosFile);
            int PosEndFileBody = Contents.IndexOf("[endfile]", PosStartFileBody, StringComparison.Ordinal);
            if (PosEndFileBody == -1)
               throw new Exception("Cannot find a matching [endfile] keyword for file " + Filename);

            string FileContents = Contents.Substring(PosStartFileBody, PosEndFileBody - PosStartFileBody);
					
            // Dump the file text into the given file name
            StreamWriter o = new StreamWriter(Filename, AppendToFile);
            o.Write(FileContents);
            o.Close();
            FileNamesCreated.Add(Path.GetFullPath(Filename));

            PosFile = Contents.IndexOf(FileMacro, PosEndFileBody, StringComparison.Ordinal);
            }
         return FileNamesCreated;
         }

      //---------------------------------------------------------------
      // Replaces HTML symbols with the correct symbols.  For example 
      // &amp; becomes &
      //---------------------------------------------------------------
      string ReplaceHTMLSymbols(string Contents)
         {
         Contents = Contents.Replace("&amp;", "&");
         Contents = Contents.Replace("&lt;", "<");
         Contents = Contents.Replace("&gt;", ">");
         Contents = Contents.Replace("&quot;", "\"");
         Contents = Contents.Replace("&nbsp;", " ");
         return Contents;
         }

      }

   // Extend the StringBuilder class to provide an IndexOf function.
   // Copied from http://stackoverflow.com/questions/1359948/why-doesnt-stringbuilder-have-indexof-method
   public static class StringBuilderExtensions
   {
       /// <summary>
       /// Returns the index of the start of the contents in a StringBuilder
       /// </summary>        
       /// <param name="value">The string to find</param>
       /// <param name="startIndex">The starting index.</param>
       /// <param name="ignoreCase">if set to <c>true</c> it will ignore case</param>
       /// <returns></returns>
       public static int IndexOf(this StringBuilder sb, string value, int startIndex, bool ignoreCase)
       {
           int index;
           int length = value.Length;
           int maxSearchLength = (sb.Length - length) + 1;

           if (ignoreCase)
           {
               for (int i = startIndex; i < maxSearchLength; ++i)
               {
                   if (Char.ToLower(sb[i]) == Char.ToLower(value[0]))
                   {
                       index = 1;
                       while ((index < length) && (Char.ToLower(sb[i + index]) == Char.ToLower(value[index])))
                           ++index;

                       if (index == length)
                           return i;
                   }
               }

               return -1;
           }

           for (int i = startIndex; i < maxSearchLength; ++i)
           {
               if (sb[i] == value[0])
               {
                   index = 1;
                   while ((index < length) && (sb[i + index] == value[index]))
                       ++index;

                   if (index == length)
                       return i;
               }
           }

           return -1;
       }
   }

   }

