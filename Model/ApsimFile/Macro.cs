

using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.IO;
using System.Xml;

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
            string PreForEachText = Contents.Substring(0, AdjustStartPos(Contents, PosForEach));

            // get the contents of the foreach body
            string ForEachText = Contents.Substring(PosAfterForEach, AdjustStartPos(Contents, PosEndForEach) - PosAfterForEach);

            // get the bit of text after the endfor macro
            string PostForEachText = Contents.Substring(PosAfterEndFor);

            // resolve node name to get a XmlNode node.
            XmlNode MacroNode = ResolveNode(NodeName, AliasNames, AliasNodes);

            // Loop through all matching child nodes and duplicate the foreach body for each child.
            string Body = "";
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

               Body += NewForEachBody;
               }

            ForEachText = Body;

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
      int AdjustStartPos(string Contents, int PosStartOfMacro)
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
      int AdjustEndPos(string Contents, int PosMacro)
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
      //---------------------------------------------------------------
      // Find the start of a foreach macro in the specified contents.
      //---------------------------------------------------------------
      int FindForEachMacro(string Contents, int StartPos)
         {
         return Contents.IndexOf("[foreach ", StartPos);
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
         PosAfterForEach = Contents.IndexOf("]", PosForEach);
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
         PosAfterForEach = AdjustEndPos(Contents, PosAfterForEach);
         }
      //---------------------------------------------------------------
      // Parse the endfor macro and return the position to just after
      // the macro.
      //---------------------------------------------------------------
      int ParseEndForMacro(string Contents, int PosEndForEach)
         {
         return AdjustEndPos(Contents, PosEndForEach);
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
            PosEndFor = Contents.IndexOf("[endfor]", CurrentPos);
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
         int PosElseIf = 0;
         int PosElse = 0;
         int PosCondition = Contents.IndexOf("[if");
         while (PosCondition != -1 && PosCondition != Contents.Length)
            {
            int PosEndMacro = FindMatchingCloseBracket(ref Contents, PosCondition + 1);
            int PosEndIf = Contents.IndexOf("[endif]", PosCondition + 1);
            int PosNextElse = Contents.IndexOf("[else]", PosCondition + 1);
            int PosNextElseIf = Contents.IndexOf("[elseif", PosCondition + 1);
            if (PosNextElse == -1)
               PosNextElse = Contents.Length;
            if (PosNextElseIf == -1)
               PosNextElseIf = Contents.Length;

            int PosIf;
            int PosEndBlock = Math.Min(Math.Min(PosNextElseIf, PosNextElse), PosEndIf);
            if (PosEndBlock != -1)
               {
               bool ok;
               if (PosCondition == PosElse && PosCondition != PosElseIf)
                  ok = true;

               else
                  {
                  int PosSpace = Contents.IndexOf(' ', PosCondition);

                  ok = EvaluateIf(Contents.Substring(PosSpace, PosEndMacro - PosSpace));
                  }
               if (ok)
                  {
                  PosEndBlock = AdjustStartPos(Contents, PosEndBlock);
                  PosEndIf = AdjustEndPos(Contents, PosEndIf);

                  // remove everything from the end of block to after the endif.
                  Contents = Contents.Remove(PosEndBlock, PosEndIf - PosEndBlock);

                  // remove the condition line.
                  PosEndMacro = AdjustEndPos(Contents, PosEndMacro);
                  PosCondition = AdjustStartPos(Contents, PosCondition);
                  Contents = Contents.Remove(PosCondition, PosEndMacro - PosCondition);
                  }
               else
                  {
                  // remove everything from start of condition down to end of block.
                  PosCondition = AdjustStartPos(Contents, PosCondition);
                  if (PosEndBlock == PosEndIf)
                     PosEndBlock = AdjustEndPos(Contents, PosEndBlock);
                  else
                     PosEndBlock = AdjustStartPos(Contents, PosEndBlock);
                  Contents = Contents.Remove(PosCondition, PosEndBlock - PosCondition);
                  }
               PosIf = Contents.IndexOf("[if");
               }
            else
               PosIf = Contents.IndexOf("[if", PosCondition+1);
            
            PosElse = Contents.IndexOf("[else]");
            PosElseIf = Contents.IndexOf("[elseif");
            if (PosIf == -1)
               PosIf = Contents.Length;
            if (PosElse == -1)
               PosElse = Contents.Length;
            if (PosElseIf == -1)
               PosElseIf = Contents.Length;

            PosCondition = Math.Min(Math.Min(PosIf, PosElse), PosElseIf);
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
      // Parse all comment statements and remove.
      //---------------------------------------------------------------
      void ParseComments(ref string Contents)
         {
         int PosComment = Contents.IndexOf("[comment]");
         while (PosComment != -1)
            {
            PosComment = AdjustStartPos(Contents, PosComment);
            int PosEndComment = Contents.IndexOf("[endcomment]");
            if (PosEndComment == -1)
               throw new Exception("Cannot find matching [endcomment] macro");
            PosEndComment = AdjustEndPos(Contents, PosEndComment);
            Contents = Contents.Remove(PosComment, PosEndComment - PosComment);
            PosComment = Contents.IndexOf("[comment]");
            }
         }
      void ParseToLower(ref string Contents)
         {
         const string opentext = "[tolower]";
         const string closetext = "[endtolower]";

		 int Pos = Contents.IndexOf(opentext);
         while (Pos != -1)
            {
            Pos = AdjustStartPos(Contents, Pos);
            int PosEnd = Contents.IndexOf(closetext);
            if (PosEnd == -1)
               throw new Exception("Cannot find matching [endtolower] macro");

			PosEnd = AdjustStartPos(Contents, PosEnd);
			int n = PosEnd - Pos - opentext.Length;

			string text = Contents.Substring(Pos + opentext.Length, n);

            Contents =  Contents.Substring(0, Pos ) + 
					     text.ToLower() +
						 Contents.Substring(PosEnd + closetext.Length, Contents.Length - (PosEnd + closetext.Length));
            Pos = Contents.IndexOf(opentext);
            }
         }

		//---------------------------------------------------------------
      // Parse all comment statements and remove.
      //---------------------------------------------------------------
      void ParseIncludes(ref string Contents)
         {
         const string opentext = "[include]";
         const string closetext = "[endinclude]";

         int PosInclude = Contents.IndexOf(opentext);
         while (PosInclude != -1)
            {
            PosInclude = AdjustStartPos(Contents, PosInclude);
            int PosEndInclude = Contents.IndexOf(closetext);
            if (PosEndInclude == -1)
               throw new Exception("Cannot find matching [endinclude] macro");
            PosEndInclude = AdjustEndPos(Contents, PosEndInclude);
            string filename = Contents.Substring(PosInclude + opentext.Length, PosEndInclude - PosInclude - opentext.Length - closetext.Length - 2);
            filename = Configuration.RemoveMacros(filename);
            string oldtext = Contents.Substring(PosInclude, PosEndInclude - PosInclude);
            if (!File.Exists(filename))
               throw new Exception("file not found: " + filename);
            StreamReader sr = new StreamReader(filename);
            string newtext = sr.ReadToEnd();

            Contents = Contents.Replace(oldtext, newtext);
            PosInclude = Contents.IndexOf("[include]");
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
         int PosFile = Contents.IndexOf(FileMacro);
         while (PosFile != -1)
            {
            PosFile += FileMacro.Length;
            int PosEndFile = Contents.IndexOf(']', PosFile);
            string Filename = Contents.Substring(PosFile, PosEndFile - PosFile);
            Filename = Filename.Trim();
            if (OutputDirectory != "")
               Filename = OutputDirectory + "/" + Filename;

            int PosStartFileBody = AdjustEndPos(Contents, PosFile);
            int PosEndFileBody = Contents.IndexOf("[endfile]", PosStartFileBody);
            if (PosEndFileBody == -1)
               throw new Exception("Cannot find a matching [endfile] keyword for file " + Filename);

            string FileContents = Contents.Substring(PosStartFileBody, PosEndFileBody - PosStartFileBody);
					
            // Dump the file text into the given file name
            StreamWriter o = new StreamWriter(Filename, AppendToFile);
            o.Write(FileContents);
            o.Close();
            FileNamesCreated.Add(Path.GetFullPath(Filename));

            PosFile = Contents.IndexOf(FileMacro, PosEndFileBody);
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
   }

