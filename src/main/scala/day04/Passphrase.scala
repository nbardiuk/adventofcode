package day04

object Passphrase {
  def countValid(passphrases: Seq[String]): Int = passphrases.count(valid)

  def valid(passphrase: String): Boolean = {
    val words = passphrase.split(' ')
    words.toSet.size == words.length
  }

}
