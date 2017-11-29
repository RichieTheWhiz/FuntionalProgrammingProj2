//Lang List
val chinese: List[String] = List("ling", "yi", "er", "san", "si", "wu", "liu", "qi", "ba", "jiu", "shi")
val english: List[String] = List("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")

//The Almighty Function Go, that does math and weeds out irrelevants
def go(list : List[String]): Unit = {
  val translation : List[Int] = trans(list)
  println("Translation: " + print(translation, " "))
  println("Addition: " + print(translation, " + ") + " = " + add(translation) + " \n")
  println("Multiplication: " + print(translation, " * ") + " = " + multi(translation) + " ")
}

//Mathmatical Operations : Addition and Multiplication
def add(list : List[Int]): Int = {
  list.foldLeft(0)(_ + _)
}

def multi(list : List[Int]): Int = {
  list.foldLeft(1)(_*_)
}


//Translating Function
def trans(Clist : List[String]): List[Int] = {
  Clist match {
    case Nil => Nil
    case head :: tail =>
      chinese.contains(head) match {
        case true => chinese.indexOf(head) :: trans(tail)
        case false => english.contains(head) match {
            case true => english.indexOf(head) :: trans(tail)
            case false => trans(tail)
          }
      }
  }
}


//Professor Dehlinger likes to mention helpers this one is to assist the print
def print(list : List[Int], etc : String): String = {
list match {
  case Nil => ""
  case head :: tail =>
    tail match {
      case Nil => head + ""
      case _ => head + etc + print(tail, etc)
    }
}
}

go(List("yi", "nine", "six", "ba"))
go(List("yi", "josh", "three", "si"))
go(List("richie", "is" ,"2", "1", "now"))
go(List("ling", "si", "jiu", "six"))