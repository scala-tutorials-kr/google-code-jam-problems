object T9Spelling {

  val l = List(("2", "abc"), ("3", "def"), ("4", "ghi")
    , ("5", "jkl"), ("6", "mno"), ("7", "pqrs"), ("8", "tuv"), ("9", "wxyz")
    , ("0", " "))

  val t9: Map[Char, String] = l.flatMap(p => {
    p._2.toCharArray.zip(Stream from 1).map(t => (t._1, p._1 * t._2))
  }).toMap


  def main(args: Array[String]): Unit = {
    val fileNames = ("C-small-practice", "C-large-practice")
    val fileName = fileNames._2

    val iterator: Iterator[String] = FileIO.readFile(fileName +".in")
    val noOfCase: Int = iterator.next().toInt

    val inputs: List[(Int, String)] = (1 to noOfCase).map((_, iterator.next())).toList
    val outputs: List[String] = inputs.map(p => s"Case #${p._1}: ${makeTString(p._2)}")

    FileIO.writeFile(outputs, fileName + ".out")
  }

  def makeTString(word: String): String = {
    val outs = word.toCharArray.map(c => t9(c)).toList

    val zeros = outs.zip(outs.tail).map(p => if (p._1.charAt(0) == p._2.charAt(0)) " " else "")

    outs.zipAll(zeros, "", "").map(p => p._1 + p._2).reduce(_+_)
  }
}



import java.io.{BufferedWriter, File, FileWriter}

object FileIO {
  def readFile(fileName: String): Iterator[String] =
    scala.io.Source.fromFile(fileName).getLines()

  def writeFile(contents: List[String], fileName: String): Unit = {
    val bw = new BufferedWriter(new FileWriter(new File(fileName)))

    for (l <- contents)
      bw.write(l + "\n")

    bw.close()
  }
}
