import scala.util
import scala.util.Random


// GAURAV KUNTE
// PL FALL 2018


object Qsort {

  def createArray(N: Int) : Array[Int] = {

    val randomNumber = scala.util.Random

    var i : Int = 0;

    // This is for randomly generating an array of integers where the numbers will be repetitive
    //val myArray = (for (i <- 0 until N-1) yield randomNumber.nextInt(N)).toArray


    // Randomly generating unique integers and storing them in an array
    val myArray = scala.util.Random.shuffle(1 to N).toArray

    return myArray

  }

  def swap(a: Array[Int], i: Int, j: Int): Any = {

    if (i == j){

      return
    }

    var temp: Int = 0

    temp = a(i)
    a(i) = a(j)
    a(j) = temp
  }

  def bubbleSort(a: Array[Int], low: Int, high: Int): Any = {
    
    if (low >= high){
      return
    }

    var i, j: Int = 0

    for (i <- low to high){

      for (j <- i+1 to high){

        if(a(i) > a(j)){

          swap(a, i, j)
        }
      }
    }
    
  }

  def partition(a: Array[Int], low: Int, high: Int): Int = {

    var pivot, middle, i: Int = 0

    pivot = a(high)
    middle = low

    for (i <- low to high){

      if (a(i) < pivot){

        swap(a, i, middle)
        middle = middle + 1

      }
    }
    swap(a, high, middle)

    return middle
  }

  def quickSort(a: Array[Int], low: Int, high: Int): Unit ={

    if ((high - low) < 10){

      bubbleSort(a, low, high)

    }

    else {

      var middle: Int = 0
      middle = partition(a, low, high)
      if (low < middle){

        quickSort(a, low, middle - 1)

      }

      if (middle < high){

        quickSort(a, middle + 1, high)

      }
    }

  }

  def printArray(a: Array[Int], N: Int) = {

    a.foreach(n => print(n + " "))
    println()
    println()
  }

  def verifyArray(a: Array[Int], N: Int): Any = {

    var i: Int = 0

    for(i <- 0 to N - 2){

      if (a(i) > a(i+1)){

        println(s"FAILED: a[$i] = $a(i), a($i+1) = $a(i+1)")
        println()
        return
      }
    }
    println("Result Verified")

  }

  def main(args: Array[String]): Unit = {

    var N: Int = 0;
    N = args(0).toInt

    assert(N > 1)

    var a:Array[Int] = new Array[Int](N);


    a = createArray(N)
    print("Initial Array is: ")
    printArray(a, N)

    quickSort(a, 0, N-1)

    print("Array after using quick sort is: ")
    printArray(a, N)

    verifyArray(a, N)

  }
}
