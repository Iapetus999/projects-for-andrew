import scala.util.Try

object SequenceFinder {
  /**
    * Note that the requirement was for a function nucleotide next(stream *s) but I decided to use Scala's Stream implementation.
    *
    * @param seq         The sequence of nucleotides we are searching for ex: AGCT
    * @param stream      A continuous stream of Nucleotides ending with ε ex: AGGTACACCATTAGε
    * @param countBefore the 'x' count of nucleotides before the target
    * @param countAfter  the 'y' count of nucleotides after the target
    * @return a list of matches. There's a separate function to pretty-print them. Last item will be None if it's non-inifinite
    */
  def findSequence(seq: Seq[Nucleotide], stream: Stream[Nucleotide], countBefore: Int, countAfter: Int): Stream[Option[NSequence]] = {
    var lastX = List[Nucleotide]()
    val iter = stream.iterator
    var candidates = List[NSequence]()

    // should speed up access if it's an array
    val seqArr = seq.toArray

    // Using this as an ID for the candidate
    var counter = 0L

    Stream.continually[Option[NSequence]] {
      Try(iter
        .takeWhile(_ != ε)
        .foreach { nuc =>
          if (nuc == seqArr.head) {
            // We'll start matching this sequence.
            candidates = NSequence(before = lastX.reverse, start = counter) :: candidates
          }
          counter = counter + 1
          // Walk through all candidate sequences and see which ones need to be updated.
          candidates = candidates map {
            case ns if ns.isComplete ⇒ ns // ignore
            case ns if ns.isMatched ⇒ ns.copy(after = nuc :: ns.after, isComplete = ns.after.length + 1 >= countAfter) // either now complete or we're looking for the after nucs
            case ns if nuc == seqArr(ns.current) ⇒
              if (ns.current + 1 == seqArr.length)
                ns.copy(isMatched = true) // We have a winner!
              else
                ns.copy(current = ns.current + 1) // Next nuc checks out, keep matching
            case ns ⇒ ns.copy(isFailed = true) // no match :(
          } filterNot {
            _.isFailed // drop the unmatched sequences
          }

          // Keep track of the x "before" nucs
          if (countBefore > 0) {
            lastX = nuc :: lastX take countBefore
          }
          // Break the inner loop to output a candidate
          if (candidates.exists(candidate => candidate.isComplete)) throw new Exception("Found One")
        })

      Option(
        candidates.collectFirst {
          case ns@NSequence(_, _, _, _, true, false, _) =>
            // These are "complete" matches
            candidates = candidates.filterNot(n => n.start == ns.start)
            ns.copy(after = ns.after.reverse)
        } getOrElse {
          candidates.filter(ns ⇒ ns.isMatched).reverse.collectFirst {
            case ns@NSequence(_, _, _, true, false, false, _) =>
              // If there are no more complete matches, this means we're at the end of the stream. Will choose one match on each iteration to throw into the stream
              candidates = candidates.filterNot(n => n.start == ns.start)
              ns.copy(after = ns.after.reverse)
          }.orNull
        }
      )
    }
  }

  /**
    * Produces results like "AGCT AAB GTCA" for each item in the list
    *
    * @param res raw results
    * @param seq the sequence we searched for
    * @return formatted string
    */
  def dumpStream(stream: Stream[Option[NSequence]], seq: Seq[Nucleotide]): String = {
    val sb = new StringBuilder()
    stream.takeWhile(_.isDefined).foreach(n => {
      formatResult(seq, sb, n.get)
      System.out.println(sb.toString())
    })
    sb.toString()
  }

  def formatResult(seq: Seq[Nucleotide], sb: StringBuilder, ns: NSequence): StringBuilder = {
    ns.before.foreach(nuc ⇒ sb.append(nuc))
    sb.append(' ')
    seq.foreach(nuc ⇒ sb.append(nuc))
    sb.append(' ')
    ns.after.foreach(nuc ⇒ sb.append(nuc))
    sb.append('\n')
  }
}


/**
  * Our immutable data object
  *
  * @param before     All the nucleotides before the match up to x
  * @param after      All the nucleotides after the match up to y
  * @param current    Index of item we're looking for
  * @param isMatched  True if we've found the whole match
  * @param isComplete True if we've completed the after list
  * @param isFailed   There is no match
  * @param start      A unique ID for manipulating lists of NSequence
  */
case class NSequence(
                      before: Seq[Nucleotide],
                      after: List[Nucleotide] = List(),
                      current: Int = 0,
                      isMatched: Boolean = false, // Are we still matching?
                      isComplete: Boolean = false, // Do we have the after seq complete?
                      isFailed: Boolean = false,
                      start: Long
                    )

