/**
  * Example of how I might conform with the spec by creating the MyStream wrapper and using the standalone 'next' function
  */
object Streamer {
  def next(s: MyStream[Nucleotide]): Nucleotide = {
    if (s.iter.hasNext) s.iter.next() else ε
  }
}

class MyStream[A](stream: Stream[A]) {
  def iter: Iterator[A] = stream.iterator
}