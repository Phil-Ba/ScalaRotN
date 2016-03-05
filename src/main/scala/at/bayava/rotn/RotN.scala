package at.bayava.rotn

/**
	* Created by philba on 2/20/16.
	*/
class RotN private(private val alphabet: RotNAlphabet) {

	def rotN(input: String): String = {
		val sb = new StringBuilder
		for (c <- input) {
			sb += alphabet >> c
		}
		sb.mkString
	}

	def deRotN(input: String): String = {
		val sb = new StringBuilder
		for (c <- input) {
			sb += alphabet << c
		}
		sb.mkString
	}

}

object RotN {

	/**
		*
		* @param n
		* @return
		*/
	def apply(n: Int): RotN = {
		new RotN(RotNAlphabet(n))
	}

}
