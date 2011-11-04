using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NUnit.Framework;
using DiffDiff;

namespace Test
{
    [TestFixture]
    public class Test
    {

        [Test]
        public void testAbsMethod()
        {
            object output;

            Differ.CompareMethod icm = new Differ.AbsCompare(1);

            Assert.False(icm.match(20m, 19m, out output));
            Assert.AreEqual("-1", output);

            Assert.True(icm.match(20m, 19.5m, out output));
            Assert.AreEqual("-0.5", output);

            Assert.False(icm.match(21m, 22m, out output));
            Assert.AreEqual("1", output);

            Assert.True(icm.match(21m, 21.5m, out output));
            Assert.AreEqual("0.5", output);

            icm = new Differ.AbsCompare(0.01m);

            Assert.False(icm.match(2554321.01m, 2554321m, out output));
            Assert.AreEqual("-0.01", output);

            Assert.True(icm.match(2554321.01m, 2554321.001m, out output));
            Assert.AreEqual("-0.009", output);

            Assert.False(icm.match(22m, "23", out output));
            Assert.AreEqual("22 = 23", output);

            Assert.True(icm.match("Some string", "Some string", out output));
            Assert.AreEqual("Some string = Some string", output);

            Assert.False(icm.match("Some string", "Some string!", out output));
            Assert.AreEqual("Some string = Some string!", output);
        }

        [Test]
        public void testPctMethod()
        {
            object output;

            Differ.CompareMethod icm = new Differ.AbsCompare(1);

            Assert.False(icm.match(20m, 19m, out output));
            Assert.AreEqual("-1", output);

            Assert.True(icm.match(20m, 19.000000001m, out output));
            Assert.AreEqual("-0.999999999", output);

            Assert.False(icm.match(20m, 21m, out output));
            Assert.AreEqual("1", output);

            Assert.True(icm.match("Some string", "Some string", out output));
            Assert.AreEqual("Some string = Some string", output);

            Assert.False(icm.match("Some string", "Some string!", out output));
            Assert.AreEqual("Some string = Some string!", output);
        }

        [Test]
        public void testDPMethod()
        {
            object output;

            Differ.CompareMethod icm = new Differ.DPCompare(1);

            Assert.False(icm.match(20m, 19m, out output));
            Assert.AreEqual("-1.0", output);

            Assert.True(icm.match(20.14m, 20.18m, out output));
            Assert.AreEqual("0.0", output);

            Assert.False(icm.match(20.14m, 20.20m, out output));
            Assert.AreEqual("0.1", output);

            icm = new Differ.DPCompare(-1);

            Assert.False(icm.match(20m, 19m, out output));
            Assert.AreEqual("-1.0", output);

            Assert.True(icm.match(20.14m, 20.18m, out output));
            Assert.AreEqual("0.0", output);

            Assert.False(icm.match(20.14m, 20.20m, out output));
            Assert.AreEqual("0.1", output);

            Assert.True(icm.match(20.104m, 20.10m, out output));
            Assert.AreEqual("0.00", output);

            Assert.False(icm.match(20.114m, 20.10m, out output));
            Assert.AreEqual("-0.01", output);

            Assert.True(icm.match("Some string", "Some string", out output));
            Assert.AreEqual("Some string = Some string", output);

            Assert.False(icm.match("Some string", "Some string!", out output));
            Assert.AreEqual("Some string = Some string!", output);
        }



    }
}
