package arrayandstrings

import org.scalatest.Matchers._
import org.scalatest.WordSpec

class HashtableSpec extends WordSpec {
  "arrayandstrings.Hashtable" should {
    "get size 0 after creation" in {
      new Hashtable[String, Int]().length shouldBe 0
    }

    "get size 1 after addition and 0 after removal" in {
      val hashtable = new Hashtable[String, Int]()
      hashtable("1") = 1
      hashtable.length shouldBe 1
      hashtable.remove("1")
      hashtable.length shouldBe 0
    }

    "have all elements after resize and downsize correctly" in {
      val hashtable = new Hashtable[Int, String]()
      val desiredLength = 50
      for (i <- 1 to desiredLength) {
        hashtable(i) = i.toString
        hashtable.length shouldBe i
        hashtable.occupiedSlots shouldBe i
      }
      hashtable.length shouldBe desiredLength
      hashtable.array.length shouldBe 80
      for(i <- 1 to desiredLength) {
        hashtable(i) shouldBe i.toString
        hashtable.remove(i)
        hashtable.length shouldBe (desiredLength - i)
        hashtable.occupiedSlots shouldBe desiredLength - i
      }
      hashtable.array.length shouldBe 10
    }

    "get size 1 when add 1 two times" in {
      val hashtable = new Hashtable[Int, String]()
      hashtable(1) = "1"
      hashtable(1) = "1"
      hashtable.length shouldBe 1
    }

    "update one key should store latest change" in {
      val ht = new Hashtable[Int, String]()
      ht(1) = "1"
      ht(1) = "2"
      ht(1) shouldBe "2"
    }

    "remove non existent key" in {
      val ht = new Hashtable[Int, String]()
      ht(2) = "2"
      ht.remove(1)
      ht.length shouldBe 1
      ht.remove(2)
      ht.length shouldBe 0
      ht.remove(2)
      ht.length shouldBe 0
    }
  }
}
