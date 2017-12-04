package day04

object Passphrase {

  def countUnique(passphrases: Seq[String]): Int = passphrases.count(unique)

  def unique(passphrase: String): Boolean = unique(words(passphrase))

  def countWithoutAnagrams(passphrases: Seq[String]): Int =
    passphrases.count(withoutAnagrams)

  def withoutAnagrams(passphrase: String): Boolean =
    unique(words(passphrase).map(Anagram))

  private def words(passphrase: String) = passphrase.split(' ')

  private def unique[T](ts: Seq[T]): Boolean = ts.toSet.size == ts.size

  case class Anagram(word: String) {

    private lazy val letters = word.groupBy(c => c).mapValues(_.length)

    def eq(o: Anagram): Boolean = letters == o.letters

    override def hashCode(): Int = letters.hashCode()

    override def equals(other: scala.Any): Boolean = other match {
      case a: Anagram => eq(a)
      case _          => false
    }

  }

}
