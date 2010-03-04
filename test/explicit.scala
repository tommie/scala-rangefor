/*
 * Test cases with an explicit Range object.
 *
 * Copyright (c) 2006-2010 Spotify AB
 *
 * @author Tommie Gannert
 * @date 2010-02-22
 */

object ExplicitTest {
	def main(args: Array[String]) {
		testExplicit()
	}

	def testExplicit() {
		for (i <- new Range(0, 10000, 1)) {
			println("" + i)
		}
	}
}
