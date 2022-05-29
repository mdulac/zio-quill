package io.getquill

import io.getquill.context.mirror.{ MirrorSession, Row }
import io.getquill.util.PrintMac

import scala.reflect.ClassTag

//package io.getquill
//
//import io.getquill.util.PrintMac
//
//object MyTest2 {
//  val ctx = new MirrorContext(PostgresDialect, SnakeCase)
//  import ctx._
//
//  case class Person(id: Int, name: String)
//  case class Address(owner: Int, street: String)
//
//  def main(args: Array[String]): Unit = {
//    PrintMac(run {
//      for {
//        p <- query[Person]
//        a <- query[Address].leftJoin(a => a.owner == p.id)
//      } yield (p, a)
//    })
//  }
//}

object FutureTests {
  val ctx = new MirrorContext(PostgresDialect, Literal) {

    // Make the option decoder more like JDBC where there are no Option elements
    implicit override def optionDecoder[T](implicit d: Decoder[T]): Decoder[Option[T]] =
      MirrorDecoder((index: Index, row: ResultRow, session: Session) =>
        if (row.nullAt(index))
          None
        else
          Some(d(index, row, session)))

    override def decoder[T: ClassTag]: Decoder[T] =
      MirrorDecoder((index: Index, row: ResultRow, session: Session) => {
        val cls = implicitly[ClassTag[T]].runtimeClass
        if (cls.isPrimitive && row.nullAt(index))
          0.asInstanceOf[T]
        else if (row.nullAt(index))
          null.asInstanceOf[T]
        else
          row[T](index)
      })
  }
  import ctx._

  //case class Person(id: Int, name: Option[String], age: Int)

  //  object SimpleCase {
  //    case class Person(id: Int, name: String)
  //    case class Address(owner: Int, street: String)
  //
  //    def exec(): Unit = {
  //      PrintMac(run {
  //        for {
  //          p <- query[Address]
  //          a <- query[Person].leftJoin(a => a.id == p.owner)
  //        } yield (p, a)
  //      })
  //    }
  //  }

  object MultiNestedOptionProduct {
    case class InnerName(title: Int, last: String) extends Embedded
    case class Name(first: String, last: Option[InnerName]) extends Embedded
    case class Address(owner: Int, street: String)
    case class Person(id: Int, name: Option[Name])

    def exec() = {
      val result =
        run {
          for {
            p <- query[Address]
            a <- query[Person].leftJoin(a => a.id == p.owner)
          } yield (p, a)
        }

      compare(
        result.extractor(Row(123, "St", 123, null, null, null), MirrorSession.default),
        (Address(123, "St"), Some(Person(123, None)))
      )
      compare(
        result.extractor(Row(123, "St", 123, "Joe", null, null), MirrorSession.default),
        (Address(123, "St"), Some(Person(123, Some(Name("Joe", None)))))
      )
      compare(
        result.extractor(Row(123, "St", 123, null, 1337, null), MirrorSession.default),
        (Address(123, "St"), Some(Person(123, Some(Name(null, Some(InnerName(1337, null)))))))
      )
    }
  }

  object MultiNestedOptionPrimitive {
    case class Name(first: String, last: Option[Int]) extends Embedded
    case class Address(owner: Int, street: String)
    case class Person(id: Int, name: Option[Name], age: Int)

    def exec() = {
      val result =
        run {
          for {
            p <- query[Address]
            a <- query[Person].leftJoin(a => a.id == p.owner)
          } yield (p, a)
        }

      compare(
        result.extractor(Row(123, "St", 123, "Joe", null, null), MirrorSession.default),
        (Address(123, "St"), Some(Person(123, Some(Name("Joe", None)), 0)))
      )
      compare(
        result.extractor(Row(123, "St", 123, null, 1337, null), MirrorSession.default),
        (Address(123, "St"), Some(Person(123, Some(Name(null, Some(1337))), 0)))
      )
    }
  }

  //  object NestedOption {
  //    // last name is an integer for the encoding example even thought it doesn't make sense
  //    case class Name(first: String, last: Int) extends Embedded
  //    // NOTE: nullCheck(name) does not need to try to decode a Name object and check if it is a None,
  //    //       all it needs to do is to check the name.first, name.last elements for nullity.
  //    //       nullCheck(person) does not need to try to decode a Name object to check Name
  //    //       for nullity, all it needs to do is to decode the Columns of Name.
  //    //       Option(Name(all null)) => None
  //    //       Option(Name(one not-null)) => Some(Name(...))
  //    // This does not apply to non-null products whose rows will be decoded anyway e.g.
  //    //       Address(all null) => Address(0, null)
  //    //
  //    // Given:
  //    // Row(a.owner, a.street, p.id, p.name.first, p.name.last, p.age)
  //    // Row(123    , "St"    , 123 , null, null, 44) => (Address(123, "st"), Some(Person(123, None, 44)))
  //    // Row(123    , "St"    , null , null, null, null) => (Address(123, "st"), None)
  //    // Row(123    , "St"    , 123 , null, null, null) => (Address(123, "st"), Some(Person(123, None, 0)))
  //    // Row(123    , "St"    , 123 , "Joe", null, null) => (Address(123, "st"), Some(Person(123, Name("Joe", 0), 0)))
  //    // Row(123    , "St"    , 123 , null, 1337, null) => (Address(123, "st"), Some(Person(123, Name(null, 1337), 0)))
  //
  //    // check if there's a join on a null?
  //
  //    case class Person(id: Int, name: Option[Name], age: Int)
  //    case class Address(owner: Int, street: String)
  //
  //    def exec(): Unit = {
  //      val result =
  //        run {
  //          for {
  //            p <- query[Address]
  //            a <- query[Person].leftJoin(a => a.id == p.owner)
  //          } yield (p, a)
  //        }
  //
  //      // Given:
  //      // Row(a.owner, a.street, p.id, p.name.first, p.name.last, p.age)
  //
  //      // Row(123    , "St"    , 123 , null, null, 44) => (Address(123, "st"), Some(Person(123, None, 44)))
  //      compare(
  //        result.extractor(Row(123, "St", 123, null, null, 44), MirrorSession.default),
  //        (Address(123, "St"), Some(Person(123, None, 44)))
  //      )
  //      // Row(123    , "St"    , null , null, null, null) => (Address(123, "st"), None)
  //      compare(
  //        result.extractor(Row(123, "St", null, null, null, null), MirrorSession.default),
  //        (Address(123, "St"), None)
  //      )
  //      // TODO check that regular mirror decoder throws an exception here trying to convert 0 to null?
  //      //      actually regular mirror decoder will throw if anything is null
  //      // Row(123    , "St"    , 123 , null, null, null) => (Address(123, "st"), Some(Person(123, None, 0)))
  //      compare(
  //        result.extractor(Row(123, "St", 123, null, null, null), MirrorSession.default),
  //        (Address(123, "St"), Some(Person(123, None, 0)))
  //      )
  //      // Row(123    , "St"    , 123 , "Joe", null, null) => (Address(123, "st"), Some(Person(123, Name("Joe", 0), 0)))
  //      compare(
  //        result.extractor(Row(123, "St", 123, "Joe", null, null), MirrorSession.default),
  //        (Address(123, "St"), Some(Person(123, Some(Name("Joe", 0)), 0)))
  //      )
  //      // Row(123    , "St"    , 123 , null, 1337, null) => (Address(123, "st"), Some(Person(123, Name(null, 1337), 0)))
  //      compare(
  //        result.extractor(Row(123, "St", 123, null, 1337, null), MirrorSession.default),
  //        (Address(123, "St"), Some(Person(123, Some(Name(null, 1337)), 0)))
  //      )
  //    }
  //  }

  def compare(a: Any, b: Any) = {
    println(s"$a == $b -> ${a == b}")
  }

  def main(args: Array[String]): Unit = {
    //NestedOption.exec()
    MultiNestedOptionPrimitive.exec()
    MultiNestedOptionProduct.exec()
  }
}