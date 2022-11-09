import scala.io.StdIn.readLine

object Apps extends App {
  val str = "Hello, Scala!"
  //a
  println(str.reverse)
  println(str.toLowerCase)
  println(str.replace("!", ""))
  println(str.concat(" and goodbye python!"))

  //b
  val yearSalaryRead = readLine("Введите годовой доход: ").toFloat
  val bonusPercentRead = readLine("Введите процент премии: ").toFloat
  val compensationRead = readLine("Введите компенсацию питания: ").toFloat

  def monthlySalary(yearSalary: Float = 0, bonusPercent: Float = 0, compensation: Float = 0) = {
    (yearSalary + yearSalary * bonusPercent + compensation) * 0.87 / 12
  }

  println("Ежемесячный оклад сотрудника после вычета налогов = " + monthlySalary(yearSalaryRead, bonusPercentRead, compensationRead))


  //c
  val salaryStaff = List(100, 150, 200, 80, 120, 75)

  def meanSalaryStaff(salaryStaff: List[Int]): Int = {
    val meanSalary: Float = salaryStaff.sum / salaryStaff.length
    val employeeSalary = monthlySalary(yearSalaryRead, bonusPercentRead, compensationRead)
    val deviationOfSalary = (((employeeSalary * 100) / meanSalary) - 100).toInt
    val result = deviationOfSalary match {
      case a if a > 0 => "+" + deviationOfSalary
      case a if a == 0 => "0" + deviationOfSalary
      case a if a > 0 => "-" + deviationOfSalary
    }
    println(s"Отклонение от средней зарплаты: $result%")
    deviationOfSalary
  }
  meanSalaryStaff(salaryStaff)

  //d
  def correctSalary(): List[Int] = {
    var nSalaryStaff = List[Int]()
    val employeeSalary = monthlySalary()
    println("Введите сумму премии/штрафа (при штрафе добавьте знак'-' перед числом:")
    val correct = readLine().toInt
    val sumSalary = employeeSalary + correct
    println(sumSalary)
    nSalaryStaff = salaryStaff :+ sumSalary.toInt
    val max = nSalaryStaff.max
    val min = nSalaryStaff.min
    println(s"Самая высокая зарплата: $max")
    println(s"Самая низкая зарплата: $min")
    nSalaryStaff
  }

}
