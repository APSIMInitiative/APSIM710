namespace XmlUnit {
    using NUnit.Framework;
    using System.IO;
    
    public class XmlAssertion : Assert {
        public static void XmlEquals(TextReader controlTextReader, TextReader testTextReader) {
            XmlEquals(new XmlDiff(controlTextReader, testTextReader));
        }

        public static void XmlEquals(string controlText, string testText) {
           XmlEquals(new XmlDiff(controlText, testText));
        }

        public static void XmlEquals(XmlInput controlInput, XmlInput testInput) {
           XmlEquals(new XmlDiff(controlInput, testInput));
        }        
        
        public static void XmlIdentical(TextReader controlTextReader, TextReader testTextReader) {
           XmlIdentical(new XmlDiff(controlTextReader, testTextReader));
        }        

        public static void XmlIdentical(string controlText, string testText) {
           XmlIdentical(new XmlDiff(controlText, testText));
        }        
        
        public static void XmlIdentical(XmlInput controlInput, XmlInput testInput) {
           XmlIdentical(new XmlDiff(controlInput, testInput));
        }        
        
        public static void XmlEquals(XmlDiff xmlDiff) {
           XmlEquals(xmlDiff, true);
        }
        
        public static void XmlNotEquals(XmlDiff xmlDiff) {
           XmlEquals(xmlDiff, false);
        }

        private static void XmlEquals(XmlDiff xmlDiff, bool equalOrNot) {
            DiffResult diffResult = xmlDiff.Compare();
            if (equalOrNot) {
              NUnit.Framework.Assert.IsTrue(diffResult.Equal, diffResult.StringValue);
            } else {
              NUnit.Framework.Assert.IsFalse(diffResult.Equal, diffResult.StringValue);
            }
        }
        
        public static void XmlIdentical(XmlDiff xmlDiff) {
           XmlIdentical(xmlDiff, true);
        }
        
        public static void XmlNotIdentical(XmlDiff xmlDiff) {
           XmlIdentical(xmlDiff, false);
        }
        
        private static void XmlIdentical(XmlDiff xmlDiff, bool identicalOrNot) {
            DiffResult diffResult = xmlDiff.Compare();
            if (identicalOrNot) {
              NUnit.Framework.Assert.IsTrue(diffResult.Identical, xmlDiff.OptionalDescription);
            } else {
              NUnit.Framework.Assert.IsFalse(diffResult.Identical, xmlDiff.OptionalDescription);
            }
        }
        
        public static void XmlValid(string someXml) {
           XmlValid(new XmlInput(someXml));
        }
        
        public static void XmlValid(string someXml, string baseURI) {
           XmlValid(new XmlInput(someXml, baseURI));
        }
        
        public static void XmlValid(TextReader reader) {
           XmlValid(new XmlInput(reader));
        }
        
        public static void XmlValid(TextReader reader, string baseURI) {
           XmlValid(new XmlInput(reader, baseURI));
        }
        
        public static void XmlValid(XmlInput xmlInput) {
            Validator validator = new Validator(xmlInput);
           XmlValid(validator);
        }
        
        public static void XmlValid(Validator validator) {
          NUnit.Framework.Assert.IsTrue(validator.IsValid, validator.ValidationMessage);
        }
        
        public static void XPathExists(string anXPathExpression, string inXml) {
           XPathExists(anXPathExpression, new XmlInput(inXml));
        }
        
        public static void XPathExists(string anXPathExpression, TextReader inXml) {
           XPathExists(anXPathExpression, new XmlInput(inXml));
        }
        
        public static void XPathExists(string anXPathExpression, XmlInput inXml) {
            XPath xpath = new XPath(anXPathExpression);
            NUnit.Framework.Assert.AreEqual(true, xpath.XPathExists(inXml));
        }
        
        public static void XPathEvaluatesTo(string anXPathExpression, string inXml, 
                                                  string expectedValue) {
           XPathEvaluatesTo(anXPathExpression, new XmlInput(inXml), expectedValue);
        }
        
        public static void XPathEvaluatesTo(string anXPathExpression, TextReader inXml, 
                                                  string expectedValue) {
           XPathEvaluatesTo(anXPathExpression, new XmlInput(inXml), expectedValue);
        }
                                                  
        public static void XPathEvaluatesTo(string anXPathExpression, XmlInput inXml, 
                                                  string expectedValue) {
            XPath xpath = new XPath(anXPathExpression);
            NUnit.Framework.Assert.AreEqual(expectedValue, xpath.EvaluateXPath(inXml));
        }
        
        public static void XslTransformResults(string xslTransform, string xmlToTransform, string expectedResult) {
        	XslTransformResults(new XmlInput(xslTransform), new XmlInput(xmlToTransform), new XmlInput(expectedResult));
        }
        
        public static void XslTransformResults(XmlInput xslTransform, XmlInput xmlToTransform, XmlInput expectedResult) {
        	Xslt xslt = new Xslt(xslTransform);
        	XmlOutput output = xslt.Transform(xmlToTransform);
        	XmlEquals(expectedResult, output.AsXml());
        }
    }
}
