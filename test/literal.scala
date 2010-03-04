/*
 * Test cases with literal Range specifications.
 *
 * Copyright (c) 2006-2010 Spotify AB
 *
 * @author Tommie Gannert
 * @date 2010-02-22
 */

object LiteralTest {
	def main(args: Array[String]) {
		testUntil()
		testTo()
		testUntilBy()
		testToBy()
	}

	def testUntil() {
		for (i <- 0 until 10000) {
			println("" + i)
		}
	}

	def testTo() {
		for (i <- 0 to 10000) {
			println("" + i)
		}
	}

	def testUntilBy() {
		for (i <- 0 until 10000 by 2) {
			println("" + i)
		}
	}

	def testToBy() {
		for (i <- 0 to 10000 by 2) {
			println("" + i)
		}
	}
}
