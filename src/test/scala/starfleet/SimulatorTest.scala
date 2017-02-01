package starfleet

import java.io.{PrintWriter, StringWriter}

import org.scalatest.{FunSpec, Matchers}

import scala.io.Source

class SimulatorTest extends FunSpec with Matchers {
  (1 to 5) foreach { n =>
    it(s"should have matching output for example $n") {
      val root = s"/example$n"
      val field = Field.load(Source.fromInputStream(this.getClass.getResourceAsStream(s"$root/field")).mkString)
      val script = Source.fromInputStream(this.getClass.getResourceAsStream(s"$root/script")).getLines.toList
      val expected = Source.fromInputStream(this.getClass.getResourceAsStream(s"$root/output")).getLines.toList

      val sw = new StringWriter
      val pw = new PrintWriter(sw)
      Simulator.run(field, script, pw)
      pw.close()

      val actual = Source.fromString(sw.toString).getLines.toList

      actual.zip(expected).zipWithIndex foreach { case ((a, e), n) =>
          s"$n: $a" shouldBe s"$n: $e"
      }

      actual.size shouldBe expected.size
    }
  }
}
