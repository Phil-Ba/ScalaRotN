package at.bayava.rotn

import org.scalatest.FunSpec
import org.scalatest.prop.TableDrivenPropertyChecks._

/**
	* Created by philba on 2/20/16.
	*/
class RotNAlphabetTest extends FunSpec {

	val lowerLetters = Table(("letter", "expectedValue"),
		('a', 0),
		('b', 1),
		('i', 8),
		('ü', 28)
	)

	val upperLetters = Table(("letter", "expectedValue"),
		('A', 0),
		('B', 1),
		('I', 8),
		('Ü', 28)
	)

	describe("testLowerValue") {

		it("should return the expected values for lower letters") {
			forAll(lowerLetters) { (letter, expectedValue) =>
				assert(RotNAlphabet.lowerValue.apply(letter) == expectedValue)
			}
		}

		it("should throw an error for upper letters") {
			forAll(upperLetters) { (letter, expectedValue) =>
				intercept[MatchError] {
					RotNAlphabet.lowerValue.apply(letter)
				}
			}
		}

		it("should throw an error for non-letters") {
			intercept[MatchError] {
				RotNAlphabet.lowerValue.apply('/')
			}
		}

	}

	describe("testUpperValue") {
		it("should return the expected values for upper letters") {
			forAll(upperLetters) { (letter, expectedValue) =>
				assert(RotNAlphabet.upperValue.apply(letter) == expectedValue)
			}
		}

		it("should throw an error for lower letters") {
			forAll(lowerLetters) { (letter, expectedValue) =>
				intercept[MatchError] {
					RotNAlphabet.upperValue.apply(letter)
				}
			}
		}

		it("should throw an error for non-letters") {
			intercept[MatchError] {
				RotNAlphabet.upperValue.apply('/')
			}
		}
	}

	describe("valueForChar") {
		it("should return the right value for lower letters") {
			forAll(lowerLetters) { (letter, expectedValue) =>
				assert(RotNAlphabet.valueForChar(letter) == expectedValue)
			}
		}

		it("should return the right value for upper letters") {
			forAll(upperLetters) { (letter, expectedValue) =>
				assert(RotNAlphabet.valueForChar(letter) == expectedValue)
			}
		}

		it("should throw an error for non-letters") {
			intercept[MatchError] {
				RotNAlphabet.valueForChar('/')
			}
		}
	}

	describe(">> should") {

		describe("for a rotation of 5") {
			val rot5Results = Table(("letter", "shiftResult"),
				('a', 'f'),
				('b', 'g'),
				('c', 'h'),
				('h', 'm'),
				('h', 'm'),
				('i', 'n'),
				('z', 'b'),
				('ä', 'c'),
				('ö', 'd'),
				('ü', 'e')
			)
			val rot5 = RotNAlphabet(5)

			it("correctly shift lower case letters") {
				forAll(rot5Results) { (letter, shiftResult) =>
					assert((rot5 >> letter) == shiftResult)
				}
			}

			it("correctly shift upper case letters") {
				forAll(rot5Results) { (letter, shiftResult) =>
					assert((rot5 >> letter.toUpper) == shiftResult.toUpper)
				}
			}

		}
	}

}
