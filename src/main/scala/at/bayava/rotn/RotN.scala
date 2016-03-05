package at.bayava.rotn

/**
	* Class for en/decoding string using character rotation
	*
	* @param alphabet the alphabet to use for en/decoding
	* @author by philba on 2/20/16.
	*/
class RotN private(private val alphabet: RotNAlphabet) {

	/**
		* Encodes a string by rotating
		*
		* @param input the string to encode
		* @return the encoded string
		*/
	def rotN(input: String): String = {
		val sb = new StringBuilder
		for (c <- input) {
			sb += alphabet >> c
		}
		sb.mkString
	}

	/**
		* Decodes a string by rotating backwards
		*
		* @param input the string to decode
		* @return the decoded string
		*/
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
		* Creates a RotN for en/decoding
		*
		* @param n the amount of characters which should be used for en/decoding
		* @return a RotN which rotates the given amount of characters
		*/
	def apply(n: Int): RotN = {
		new RotN(RotNAlphabet(n))
	}

}
