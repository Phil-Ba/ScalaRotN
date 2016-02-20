/**
	* Created by philba on 2/19/16.
	*/
object Alphabet {

	val lower: Seq[Char] = Seq('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q',
		'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'ä', 'ö', 'ü')

	val upper: Seq[Char] = Seq('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q',
		'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'Ä', 'Ö', 'Ü')

	val lowerValue: PartialFunction[Char, Int] = {
		case c: Char if c.isLower => lower.indexOf(c) + 1
	}

	val upperValue: PartialFunction[Char, Int] = {
		case c: Char if c.isUpper => upper.indexOf(c) + 1
	}

	def valueForChar(c: Char): Int = lowerValue.applyOrElse(c, upperValue)

}
