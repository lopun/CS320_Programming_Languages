import Util._

object Main extends Homework01 {
    /* TODO Implement 10 missing functions */

     def dollar2won(dollar: Int): Int = if (dollar < 0) error("dollar has to be non-negative value!") else dollar * 1100

     def volumeOfCuboid(a: Int, b: Int, c: Int): Int = if (a < 0 || b < 0 || c < 0) error("all of them(a,b,c) have to be non-negative value.") else a * b * c

     def isEven(num: Int): Boolean = num % 2 == 0

     def isOdd(num: Int): Boolean = num % 2 != 0 

     def gcd(a: Int, b: Int): Int = {
	 	if (a < 0 || b < 0)
			error("a and b must be non-negative value.")

		else {
			if (b == 0) a
        	else gcd(b, a%b)
		}
      }

     def lcm(a: Int, b: Int): Int = if (a < 0 || b < 0) error("both a and b have to be non-negative value.") else (a * b) / gcd(a, b)

     def numOfHomework(course: COURSE): Int = course match {
       case CS320(quiz, homework) => homework
       case CS330(projects, homework) => homework
       case CS311(homework) => homework
	   case _ => error("course must be one of CS320 / CS330 / CS311 lectures.")
     }
     
     def hasProjects(course: COURSE): Boolean = course match {
        case CS330(projects, homework) => if (projects >= 2) true else false
        case CS320(quiz, homework) => false
		case CS311(homework) => false
     }

     def namePets(pets: List[String]): List[String] = pets.map((pet: String) => pet match {
        case "dog" => "happy"
        case "cat" => "smart"
        case "pig" => "pinky"
        case other => other
     })

     def giveName(oldName: String, newName: String): List[String] => List[String] =  {

     	def innerNames(names: List[String]): List[String] = names.map((name: String) => { if (name == oldName) newName else name })
     	innerNames

     }

     def ownTests(): Unit = {
       println("Test 1 : dollar2won, testcase: 2, 5")
       test(dollar2won(2), 2200)
       test(dollar2won(5), 5500)
	   testExc(dollar2won(-1), "non-negative")

       println("Test 2 : volumeOfCuboid, testcase: (1,2,3), (2,4,5)")
       test(volumeOfCuboid(1,2,3), 6)
       test(volumeOfCuboid(2,4,5), 40)
	   testExc(volumeOfCuboid(-1,-2,3), "non-negative")

       println("Test 3 : isEven, testcase: 1, 2")
       test(isEven(1), false)
       test(isEven(2), true)

       println("Test 4 : isOdd, testcase: 1, 2")
       test(isOdd(1), true)
       test(isOdd(2), false)
	   

       println("Test 5 : gcd, testcase: (6,9), (12, 20)")
       test(gcd(6, 9), 3)
       test(gcd(12, 20), 4)
	   testExc(gcd(-1, -2), "non-negative")

       println("Test 6 : lcm, testcase: (3,4), (12, 20)")
       test(lcm(3, 4), 12)
       test(lcm(12, 20), 60)
	   testExc(lcm(-1, -2), "non-negative")

       println("Test 7 : numOfHomework, testcase: CS320(2, 5), CS330(5, 6),CS311(5)")
       test(numOfHomework(CS320(2, 5)), 5)
       test(numOfHomework(CS330(5, 6)), 6)
       test(numOfHomework(CS311(5)), 5)

       println("Test 8 : hasProjects, testcase: CS320(2, 5), CS330(5, 6), CS330(1, 6), CS311(5)")
       test(hasProjects(CS320(2, 5)), false)
       test(hasProjects(CS330(5, 6)), true)
       test(hasProjects(CS330(1, 6)), false)
       test(hasProjects(CS311(5)), false)

       println("Test 9 : namePets, testcase: List('dog', 'baby', 'kitty'), List('cat', 'baby', 'trevi')")
       test(namePets(List("dog", "baby", "kitty")), List("happy", "baby", "kitty"))
       test(namePets(List("cat", "baby", "trevi")), List("smart", "baby", "trevi"))

       println("Test 10 : giveName, testcase : oldName - 'cat' / newName -  'kitty' / testList - List('cat', 'baby', 'trevi')")
       test(giveName("cat", "kitty")(List("cat", "baby", "trevi")), List("kitty", "baby", "trevi"))
     } 
}
