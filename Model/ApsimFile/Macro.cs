

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
   ///
   /// [foreach sim in simulation]
   /// [foreach s in sim.soil]
   /// [foreach scrop in s.crop]
   /// [foreach simcrop in sim.crop]
   /// [if [simcrop.name] = [scrop.name]]
   ///   layers = [foreach l in simcrop.layer] [l.dlayer] [endfor]
   ///   Soil curve number = [s.soilwat2.cn2]
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
         ParseIf(ref Contents);
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
                  return PosStartOfMacro;
          }
          else
              return 0;
      }
      // ------------------------------------------------------------------
      // Adjust the end position of a macro. This routine will remove
      // unwanted spaces and a carriage return on end of the macro
      // if there is nothing else between the end of the macro and
      // the end of the line.
      // ------------------------------------------------------------------
      int AdjustEndPos(ref string Contents, int PosMacro)
         {
         int PosEndOfMacro = PosMacro;
         if (Contents[PosMacro] != ']')
            PosEndOfMacro = Contents.IndexOf(']', PosMacro);
         PosEndOfMacro++;
         int Pos = PosEndOfMacro;
         while (Pos < Contents.Length && (Contents[Pos] == ' ' || Contents[Pos] == '\r'))
            Pos++;

         if (Pos < Contents.Length && Contents[Pos] == '\n')
            return Pos + 1;
         else
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
          while (Pos < Contents.Length && (Contents[Pos] == ' ' || Contents[Pos] == '\r'))
              Pos++;

          if (Pos < Contents.Length && Contents[Pos] == '\n')
              return Pos + 1;
          else
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

      //---------------------------------------------------------------
      // Parse all if statements.
      //---------------------------------------------------------------
      void ParseIf(ref string Contents)
      {
          // This routine used "Remove", which can be slow with large string objects
          // To speed this up, we can use a StringBuilder object, for which these
          // operations are a lot less expensive. We copy our contents string
          // to a new StringBuilder at the start, then copy the StringBuilder back
          // to the string at the end.
          int PosElseIf = 0;
          int PosElse = 0;
          int PosCondition = Contents.IndexOf("[if", StringComparison.Ordinal);
          if (PosCondition != -1)
          {
              StringBuilder Cnts = new StringBuilder(Contents);
              while (PosCondition != -1 && PosCondition != Cnts.Length)
              {
                  int PosEndMacro = FindMatchingCloseBracket(ref Cnts, PosCondition + 1);
                  int PosEndIf = Cnts.IndexOf("[endif]", PosCondition + 1, false);
                  int PosNextElse = Cnts.IndexOf("[else]", PosCondition + 1, false);
                  int PosNextElseIf = Cnts.IndexOf("[elseif", PosCondition + 1, false);
                  if (PosNextElse == -1)
                      PosNextElse = Cnts.Length;
                  if (PosNextElseIf == -1)
                      PosNextElseIf = Cnts.Length;

                  int PosIf;
                  int PosEndBlock = Math.Min(Math.Min(PosNextElseIf, PosNextElse), PosEndIf);
                  if (PosEndBlock != -1)
                  {
                      bool ok;
                      if (PosCondition == PosElse && PosCondition != PosElseIf)
                          ok = true;

                      else
                      {
                          int PosSpace = Cnts.IndexOf(" ", PosCondition, false);

                          ok = EvaluateIf(Cnts.ToString(PosSpace, PosEndMacro - PosSpace));
                      }
                      if (ok)
                      {
                          PosEndBlock = AdjustStartPos(ref Cnts, PosEndBlock);
                          PosEndIf = AdjustEndPos(ref Cnts, PosEndIf);

                          // remove everything from the end of block to after the endif.
                          Cnts.Remove(PosEndBlock, PosEndIf - PosEndBlock);

                          // remove the condition line.
                          PosEndMacro = AdjustEndPos(ref Cnts, PosEndMacro);
                          PosCondition = AdjustStartPos(ref Cnts, PosCondition);
                          Cnts.Remove(PosCondition, PosEndMacro - PosCondition);
                      }
                      else
                      {
                          // remove everything from start of condition down to end of block.
                          PosCondition = AdjustStartPos(ref Cnts, PosCondition);
                          if (PosEndBlock == PosEndIf)
                              PosEndBlock = AdjustEndPos(ref Cnts, PosEndBlock);
                          else
                              PosEndBlock = AdjustStartPos(ref Cnts, PosEndBlock);
                          Cnts.Remove(PosCondition, PosEndBlock - PosCondition);
                      }
                      PosIf = Cnts.IndexOf("[if", PosCondition, false);
                  }
                  else
                      PosIf = Cnts.IndexOf("[if", PosCondition + 1, false);

                  PosElse = Cnts.IndexOf("[else]", PosCondition, false);
                  PosElseIf = Cnts.IndexOf("[elseif", PosCondition, false);
                  if (PosIf == -1)
                      PosIf = Cnts.Length;
                  if (PosElse == -1)
                      PosElse = Cnts.Length;
                  if (PosElseIf == -1)
                      PosElseIf = Cnts.Length;

                  PosCondition = Math.Min(Math.Min(PosIf, PosElse), PosElseIf);
              }
              Contents = Cnts.ToString();
          }
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

