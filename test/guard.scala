/*
 * Test cases with simple guards.
 *
 * Copyright (c) 2006-2010 Spotify AB
 *
 * @author Tommie Gannert
 * @date 2010-02-22
 */

object GuardTest {
	def main(args: Array[String]) {
		testSingle()
	}

	def testSingle() {
		for (i <- 0 to 10000 if (i % 2 == 0)) {
			println("" + i)
		}
	}
}
