import org.scalatest.FunSpec
import org.scalatest.prop.TableDrivenPropertyChecks._

/**
	* Created by philba on 2/20/16.
	*/
class AlphabetTest extends FunSpec {

	val lowerLetters = Table(("letter", "expectedValue"),
		('a', 1),
		('b', 2),
		('i', 9),
		('ü', 29)
	)

	val upperLetters = Table(("letter", "expectedValue"),
		('A', 1),
		('B', 2),
		('I', 9),
		('Ü', 29)
	)

	describe("testLowerValue") {

		it("should return the expected values for lower letters") {
			forAll(lowerLetters) { (letter, expectedValue) =>
				assert(Alphabet.lowerValue.apply(letter) == expectedValue)
			}
		}

		it("should throw an error for upper letters") {
			forAll(upperLetters) { (letter, expectedValue) =>
				intercept[MatchError] {
					Alphabet.lowerValue.apply(letter)
				}
			}
		}

	}

	describe("testUpperValue") {
		it("should return the expected values for upper letters") {
			forAll(upperLetters) { (letter, expectedValue) =>
				assert(Alphabet.upperValue.apply(letter) == expectedValue)
			}
		}

		it("should throw an error for lower letters") {
			forAll(lowerLetters) { (letter, expectedValue) =>
				intercept[MatchError] {
					Alphabet.upperValue.apply(letter)
				}
			}
		}
	}

}
