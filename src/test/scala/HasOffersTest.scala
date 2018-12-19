import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random



class HasOffersTest extends FlatSpec with Matchers {

  import SequenceFinder._

  "simple example" should "work" in {
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
    val testStream: Stream[Nucleotide] = List(A, A, A, A, A, A, A, A, A, A, ε).toStream
    val testSequence = List(A, A)
    val t = findSequence(testSequence, testStream, 1, 1)

    val results = formatResults(t, testSequence)
    results should be(
      " AA A\nA AA A\nA AA A\nA AA A\nA AA A\nA AA A\nA AA A\nA AA A\nA AA \n"
    )
  }

  "extreme test" should "not error out" in {
    val size = 100000
    var count = 0
    val bigStream = Stream.continually(Random.nextInt(4) match {
      case 0 => A
      case 1 => C
      case 2 => G
      case 3 => T
    }) take size append Stream(ε)

    count = 0
    val testSequence = Stream.continually(Random.nextInt(4) match {
      case 0 => A
      case 1 => C
      case 2 => G
      case 3 => T
    }) take 4
    val t = findSequence(testSequence, bigStream, 20, 20)

    val results = formatResults(t, testSequence)
    System.out.println(s"Big Test:\n$results")


  }
}



