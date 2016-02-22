/**
	* Created by philba on 2/19/16.
	*/
class RotNAlphabet private(private val n: Int) {

	def <<(c: Char): Char = RotNAlphabet.valueForChar(c) - n match {
		case idx: Int if idx >= 0 => RotNAlphabet.charForValue(idx, c.isUpper)
		case idx: Int if idx < 0 => RotNAlphabet.charForValue(idx, c.isUpper)
	}

	def >>(c: Char): Char = RotNAlphabet.charForValue(RotNAlphabet.valueForChar(c) + n % RotNAlphabet.size, c.isUpper)

}

object RotNAlphabet {

	val letters: Seq[Char] = Seq('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q',
		'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'ä', 'ö', 'ü')

	val size = letters.size

	def apply(n: Int): RotNAlphabet = {
		new RotNAlphabet(n % size)
	}


	val lowerValue: PartialFunction[Char, Int] = {
		case c: Char if c.isLower => letters.indexOf(c)
	}

	val upperValue: PartialFunction[Char, Int] = {
		case c: Char if c.isUpper => letters.indexOf(c.toLower)
	}

	def valueForChar(c: Char): Int = {
		val idx = lowerValue.applyOrElse(c, upperValue)
		require(idx > -1)
		idx
	}

	def charForValue(idx: Int, toUpper: Boolean = false): Char = toUpper match {
		case true => letters(idx).toUpper
		case false => letters(idx)
	}

}
