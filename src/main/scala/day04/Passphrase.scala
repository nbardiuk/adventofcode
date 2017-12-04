package day04

object Passphrase {

  def countUnique(passphrases: Seq[String]): Int = passphrases.count(unique)

  def unique(passphrase: String): Boolean = unique(words(passphrase))

  def countWithoutAnagrams(passphrases: Seq[String]): Int = passphrases.count(withoutAnagrams)

  def withoutAnagrams(passphrase: String): Boolean = unique(words(passphrase).map(anagramId))

  private def anagramId(word: String) = word.groupBy(c => c)

  private def words(passphrase: String) = passphrase.split(' ')

  private def unique[T](ts: Seq[T]): Boolean = ts.toSet.size == ts.size

}
