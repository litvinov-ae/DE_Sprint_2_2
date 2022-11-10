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

  val monthlySalaryWorker = monthlySalary(yearSalaryRead, bonusPercentRead, compensationRead)
  println("Ежемесячный оклад сотрудника после вычета налогов = " + monthlySalaryWorker)


  //c
  var salaryWorkers = List(100, 150, 200, 80, 120, 75)

  def meanSalaryWorkers(salaryStaff: List[Int]): Int = {
    val meanSalary: Float = salaryStaff.sum / salaryStaff.length
    val deviationOfSalary = (((monthlySalaryWorker * 100) / meanSalary) - 100).toInt
    val result = deviationOfSalary match {
      case a if a > 0 => "+" + deviationOfSalary
      case a if a == 0 => "0" + deviationOfSalary
      case a if a > 0 => "-" + deviationOfSalary
    }
    println(s"Отклонение от средней зарплаты: $result%")
    deviationOfSalary
  }

    meanSalaryWorkers(salaryWorkers)

  //d
  def correctSalary() = {
    println("Введите сумму премии/штрафа (при штрафе добавьте знак'-' перед числом: ")
    val correctedSalary: Double = monthlySalaryWorker + readLine().toFloat
    salaryWorkers = salaryWorkers :+ correctedSalary.toInt
    val minSalary = salaryWorkers.min
    val maxSalary = salaryWorkers.max
    println(s"Самая низкая зарплата: $minSalary")
    println(s"Самая высокая зарплата: $maxSalary")
  }

  correctSalary()

  //e
  def addWorker(salaryForNewWorker: Int) = {
    salaryWorkers = salaryWorkers :+ salaryForNewWorker
    salaryWorkers = salaryWorkers.sorted
    println(salaryWorkers)
  }

  addWorker(350)
  addWorker(90)


  //f
  addWorker(130)

  def getNumberWorker(salaryWorker: Int) = {
    salaryWorkers.indexOf(salaryWorker)
  }

  println("Позиция искомого сотрудника: " + getNumberWorker(130))

  //g
  var middleSalaryStaff = List[Int]()

  def getMiddleWorkers(minBorder: Int, maxBorder: Int) {
    for (worker <- salaryWorkers) {
      if ((worker > minBorder) && (worker <= maxBorder))
        println(s"Cотрудник уровня middle № ${salaryWorkers.indexOf(worker)}")
    }
  }

  getMiddleWorkers(99, 150)


  //h
  def salaryIndexing(indexing: Float) {
    salaryWorkers = salaryWorkers.map(x => (x + (x * (indexing / 100))).toInt)
  }

  salaryIndexing(7)
  print(s"Новая зарплата сотрудников после индексации: $salaryWorkers")
}