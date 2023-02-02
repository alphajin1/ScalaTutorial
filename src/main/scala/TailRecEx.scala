import scala.annotation.tailrec

object TailRecEx {
  def sum1(list: List[Int]): Int = list match {
    // 머리 재귀 ex1
    // 리스트는 다음과 같이 생성 됨
    // List( 1, 2, 3 ) == 1 :: ( 2 :: ( 3 :: Nil )))
    // ref) https://programmingfbf7290.tistory.com/entry/16%EC%9E%A5-%EB%A6%AC%EC%8A%A4%ED%8A%B8
    case Nil => 0
    case t :: tail => t + sum1(tail)
  }

  def sum2(list: List[Int]): Int = {
    // 머리 재귀 ex2
    if (list.isEmpty) 0
    else list.head + sum2(list.tail)
  }

  def sum3(list: List[Int], acc: Int): Int = {
    // 중간 값을 가지는 꼬리 재귀 ex1
    if (list.isEmpty) acc
    else sum3(list.tail, list.head + acc)
  }

  def sum4(list: List[Int], acc: Int): Int = list match {
    // 중간 값을 가지는 꼬리 재귀 ex2
    // 중간 값을 저장했기 때문에 stack over flow가 발생하지 않는다.
    case Nil => acc
    case h :: tail => sum4(tail, h + acc)
  }

  def tailrecSum(l: List[Int]): Int = {
    // sum3, sum4 를 이용하여 entry 함수 형태로 변환
    // 재귀 함수 앞에 붙히면 스칼라 컴파일러에 꼬리 재귀가 있으니 최적화하라고 알려준다. (몇가지 제약사항이 있다. )
    // https://knight76.tistory.com/entry/scala-%EA%BC%AC%EB%A6%AC-%EC%9E%AC%EA%B7%80tail-recursion%EC%99%80-tailrec
    @tailrec
    def sum5(list: List[Int], acc: Int): Int = list match {
      case Nil => acc
      case x :: tail => sum5(tail, acc + x)
    }
    sum5(l, 0)
  }

  def main(args: Array[String]): Unit = {
    println(sum1((1 to 2).toList))
    println(sum2((1 to 2).toList))
    // stack over flow
    // println(sum2((1 to 1000000).toList))

    println(sum3(((1 to 2).toList), 0))
    println(sum4(((1 to 2).toList), 0))
    println(sum4((1 to 1000000).toList, 0))

    println(tailrecSum((1 to 1000000).toList))

  }
}
