
object SequenceFinder {
  /**
    * Note that the requirement was for a function nucleotide next(stream *s) but I decided to use Scala's Stream implementation.
    * @param seq The sequence of nucleotides we are searching for ex: AGCT
    * @param stream A continuous stream of Nucleotides ending with ε ex: AGGTACACCATTAGε
    * @param countBefore the 'x' count of nucleotides before the target
    * @param countAfter the 'y' count of nucleotides after the target
    * @return a list of matches. There's a separate function to pretty-print them
    */
  def findSequence(seq: Seq[Nucleotide], stream: Stream[Nucleotide], countBefore: Int, countAfter: Int): List[NSequence] = {
    var lastX = List[Nucleotide]()
    val iter = stream.iterator
    var candidates = List[NSequence]()

    // should speed up access if it's an array
    val seqArr = seq.toArray

    iter
      .takeWhile(_ != ε)
      .foreach { nuc =>
        if (nuc == seqArr.head) {
          // We'll start matching this sequence.
          candidates = NSequence(lastX.reverse) :: candidates
        }

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
      }

    // At the end, only look at what was actually matched. Their "after" list needs to be reversed and then the whole list needs to be reversed.
    candidates
      .filter(ns ⇒ ns.isMatched)
      .map(ns ⇒ ns.copy(after = ns.after.reverse))
      .reverse
  }

  /**
    * Produces results like "AGCT AAB GTCA" for each item in the list
    * @param res raw results
    * @param seq the sequence we searched for
    * @return formatted string
    */
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

/**
  * Our immutable data object
  * @param before All the nucleotides before the match up to x
  * @param after All the nucleotides after the match up to y
  * @param current Index of item we're looking for
  * @param isMatched True if we've found the whole match
  * @param isComplete True if we've completed the after list
  * @param isFailed There is no match
  */
case class NSequence(
                      before: Seq[Nucleotide],
                      after: List[Nucleotide] = List(),
                      current: Int = 0,
                      isMatched: Boolean = false, // Are we still matching?
                      isComplete: Boolean = false, // Do we have the after seq complete?
                      isFailed: Boolean = false,
                    )

