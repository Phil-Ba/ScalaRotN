package at.bayava.rotn

/**
	* Class for encoding an decoding characters according to a particular alphabet
	*
	* @param n the number of characters this alphabet shifts for en/decoding
	* @author Created by philba on 2/19/16.
	*/
class RotNAlphabet private(private val n: Int) {

	/**
		* "Decodes" a character by unshifting it
		*
		* @param c character to be decoded
		* @return the decoded character
		*/
	def <<(c: Char): Char = RotNAlphabet.valueForChar(c) - n match {
		case idx: Int if idx >= 0 => RotNAlphabet.charForValue(idx, c.isUpper)
		case idx: Int if idx < 0 => RotNAlphabet.charForValue(RotNAlphabet.size + idx, c.isUpper)
	}

	/**
		* "Encodes" a character by shifting it
		*
		* @param c charcter to be encoded
		* @return the encoded character
		*/
	def >>(c: Char): Char = RotNAlphabet.charForValue((RotNAlphabet.valueForChar(c) + n) % RotNAlphabet.size, c
		.isUpper)

}

object RotNAlphabet {

	val letters: Seq[Char] = Seq('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q',
		'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'ä', 'ö', 'ü')

	/**
		* The number of letters in the alphabet
		*/
	val size = letters.size

	/**
		* creates a new alphabet for en/decoding
		*
		* @param n the number of characters the alphabet should shift for de/encoding
		* @return a new alphabet instance
		*/
	def apply(n: Int): RotNAlphabet = {
		val optimzedShiftSize: Int = n % size
		new RotNAlphabet(optimzedShiftSize)
	}


	/**
		* Finds the position of a character in the alphabet
		*
		* @param c the charcter to find
		* @return the position of the character in the alphabet
		*/
	def valueForChar(c: Char): Int = {
		val idx = c match {
			case c: Char if c.isUpper => letters.indexOf(c.toLower)
			case c: Char => letters.indexOf(c)
		}
		require(idx > -1, s"Unknown character '$c'!")
		idx
	}

	/**
		* Return a character given its position in the alphabet
		*
		* @param idx     the position of the character in the alphabet
		* @param toUpper should the returned character be uppercased
		* @return the character at the given positionn the aalphabet
		**/
	def charForValue(idx: Int, toUpper: Boolean = false): Char = {
		require(idx <= size, s"The given index '$idx' is outside of the alphabet(max $size)!")
		toUpper match {
			case true => letters(idx).toUpper
			case false => letters(idx)
		}
	}

}
