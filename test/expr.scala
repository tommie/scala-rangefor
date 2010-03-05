/*
 * Test cases with expression Range specifications.
 *
 * Copyright (c) 2006-2010 Spotify AB
 *
 * @author Tommie Gannert
 * @date 2010-03-05
 */

object ExprTest {
	def main(args: Array[String]) {
		testUntil()
		testTo()
		testUntilBy()
		testToBy()
	}

	def testUntil() {
		for (i <- 0+1 until 10000+1) {
			println("" + i)
		}
	}

	def testTo() {
		for (i <- 0+1 to 10000+1) {
			println("" + i)
		}
	}

	def testUntilBy() {
		for (i <- 0+1 until 10000+1 by 2+1) {
			println("" + i)
		}
	}

	def testToBy() {
		for (i <- 0+1 to 10000+1 by 2+1) {
			println("" + i)
		}
	}
}
