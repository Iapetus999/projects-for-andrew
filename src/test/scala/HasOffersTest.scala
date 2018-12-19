import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable

object HasOffersTest {
  val epsilon = 'ε'
}

sealed trait Nucleotide

case object A extends Nucleotide

case object C extends Nucleotide

case object G extends Nucleotide

case object T extends Nucleotide

case object `ε` extends Nucleotide

class HasOffersTest extends FlatSpec with Matchers {

  import HasOffersTest._


  it should "work" in {
    val testSequence: List[Nucleotide] = List(C, G)
    val streamer = testSequence.toStream

    val nukeStream: Stream[Nucleotide] = List(A, C, G, T, A, C, C, G, T, ε).toStream


    val t = findSequence(testSequence, nukeStream, 2, 3)

    System.out.println(s"t=$t")

    formatResults(t, testSequence)

  }

  "the sample test" should "work" in {

    val testStream: Stream[Nucleotide] = List(A, A, G, T, A, C, G, T, G, C, A, G, T, G, A, G, T, A, G, T, A, G, A, C, C, T, G, A, C, G, T, A, G, A, C, C, G, A, T, A, T, A, A, G, T, A, G, C, T, A, ε).toStream
    val testSequence = List(A, G, T, A)
    val t = findSequence(testSequence, testStream, 5, 7)

    formatResults(t, testSequence)
  }


  "empty stream" should "return empty" in {

    val testStream: Stream[Nucleotide] = List(ε).toStream
    val testSequence = List(A, G, T, A)
    val t = findSequence(testSequence, testStream, 5, 7)

    val results = formatResults(t, testSequence)
    results should be("")
  }

  "find nothing" should "return empty" in {
    val testStream: Stream[Nucleotide] = List(A, A, A, A, A, A, ε).toStream
    val testSequence = List(G)
    val t = findSequence(testSequence, testStream, 5, 7)

    val results = formatResults(t, testSequence)
    results should be("")
  }

  "stream is item" should "return 1 item" in {
    val testStream: Stream[Nucleotide] = List(A, ε).toStream
    val testSequence = List(A)
    val t = findSequence(testSequence, testStream, 5, 7)

    val results = formatResults(t, testSequence)
    results should be(" A \n")
  }

  "stream all items" should "return all item" in {
    val testStream: Stream[Nucleotide] = List(A, A, A, A, A, ε).toStream
    val testSequence = List(A)
    val t = findSequence(testSequence, testStream, 5, 7)

    val results = formatResults(t, testSequence)
    results should be(
      " A AAAA\nA A AAA\nAA A AA\nAAA A A\nAAAA A \n"
    )
  }

  "many overlaps" should "return all overlaps" in {
    val testStream: Stream[Nucleotide] = List(A, A, A, A, A,A, A, A, A, A, ε).toStream
    val testSequence = List(A,A)
    val t = findSequence(testSequence, testStream, 1, 1)

    val results = formatResults(t, testSequence)
    results should be(
      " AA A\nA AA A\nA AA A\nA AA A\nA AA A\nA AA A\nA AA A\nA AA A\nA AA \n"
    )
  }

  def findSequence(seq: Seq[Nucleotide], stream: Stream[Nucleotide], countBefore: Int, countAfter: Int): List[NSequence] = {
    var going = true
    var lastX = List[Nucleotide]()
    var iteration = 0
    val iter = stream.iterator
    var sequences = List[NSequence]()

    while (going && iter.hasNext) {
      val nuc = iter.next()
      if (nuc != `ε`) {
        if (nuc == seq.head) {
          sequences = NSequence(lastX.reverse) :: sequences
        }

        sequences = sequences map {
          case ns if ns.isComplete ⇒ ns
          case ns if ns.isMatched ⇒ ns.copy(after = nuc :: ns.after, isComplete = ns.after.length + 1 >= countAfter)
          case ns if nuc == seq(ns.current) ⇒
            if (ns.current + 1 == seq.length)
              ns.copy(isMatched = true)
            else
              ns.copy(current = ns.current + 1)
          case ns ⇒ ns.copy(isFailed = true)
        } filterNot {
          _.isFailed
        }

        if (countBefore > 0) {
          lastX = if (iteration < countBefore) nuc :: lastX
          else nuc :: lastX take countBefore
        }

        iteration = iteration + 1
      }
      else going = false
    }
    sequences.filter(ns ⇒ ns.isMatched).map(ns ⇒ ns.copy(after = ns.after.reverse)).reverse
  }

  def formatResults(res: List[NSequence], seq: Seq[Nucleotide]): String = {
    val sb = new StringBuilder()
    res.foreach(ns ⇒ {
      ns.before.foreach(nuc ⇒ sb.append(nuc))
      sb.append(' ')
      seq.foreach(nuc ⇒ sb.append(nuc))
      sb.append(' ')
      ns.after.foreach(nuc ⇒ sb.append(nuc))
      sb.append('\n')
    })

    val result = sb.toString()
    System.out.println(result)
    result
  }

}

class Streamer {
  def next(s: Stream[Nucleotide]): Nucleotide = {
    s.head
  }
}

case class NSequence(
                      before: Seq[Nucleotide],
                      after: List[Nucleotide] = List(),
                      current: Int = 0,
                      isMatched: Boolean = false, // Are we still matching?
                      isComplete: Boolean = false, // Do we have the after seq complete?
                      isFailed: Boolean = false,
                    )
