package at.bayava.rotn

import org.scalatest.FunSpec

/**
	* Created by philba on 3/3/16.
	*/
class RotNTest extends FunSpec {

	val rot1 = RotN(1)
	val rot5 = RotN(5)
	val rot28 = RotN(28)
	val rotAll = RotN(RotNAlphabet.size)

	val startString1: String = "abcdefg"
	val startString2: String = "xyzäöüa"

	describe("Rot1") {

		val resultString1: String = "bcdefgh"
		val resultString2: String = "yzäöüab"

		it("should shift letters exactly one character") {
			assert(rot1.rotN(startString1) == resultString1)
			assert(rot1.rotN(startString2) == resultString2)
		}

		it("should unshift letters exactly one character") {
			assert(rot1.deRotN(resultString1) == startString1)
			assert(rot1.deRotN(resultString2) == startString2)
		}
	}

	describe("Rot5") {

		val resultString1: String = "fghijkl"
		val resultString2: String = "üabcdef"

		it("should shift letters exactly one character") {
			assert(rot5.rotN(startString1) == resultString1)
			assert(rot5.rotN(startString2) == resultString2)
		}

		it("should unshift letters exactly one character") {
			assert(rot5.deRotN(resultString1) == startString1)
			assert(rot5.deRotN(resultString2) == startString2)
		}
	}

	describe("Rot28") {

		val resultString1: String = "üabcdef"
		val resultString2: String = "wxyzäöü"

		it("should shift letters exactly one character") {
			assert(rot28.rotN(startString1) == resultString1)
			assert(rot28.rotN(startString2) == resultString2)
		}

		it("should unshift letters exactly one character") {
			assert(rot28.deRotN(resultString1) == startString1)
			assert(rot28.deRotN(resultString2) == startString2)
		}
	}

	describe("If you rotate with the size of the whole alphabet") {

		val startString: String = "xcväölüpadsifüpwe"

		it("shift should produce the input string") {
			assert(rotAll.rotN(startString) == startString)
		}

		it("unshift should produce the input string") {
			assert(rotAll.deRotN(startString) == startString)
		}
	}

}
