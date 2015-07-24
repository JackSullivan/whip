package so.modernized.whip.psl

import edu.umd.cs.psl.database.ReadOnlyDatabase
import edu.umd.cs.psl.model.argument._
import edu.umd.cs.psl.model.function.ExternalFunction
import edu.umd.cs.psl.model.predicate.PredicateFactory
import cc.factorie.app.strings.editDistance

import scala.reflect.ClassTag

import so.modernized.whip.util.union._

object PSLDSL {

  type PslType = union[String]#or[Int]#or[Double]#or[UniqueID]

  object FunctionConversions {

    private def extract[T : prove[PslType]#containsType](gt: GroundTerm): T = gt match {
      case intTerm: IntegerAttribute => intTerm.getValue.toInt.asInstanceOf[T]
      case dblTerm: DoubleAttribute => dblTerm.getValue.toDouble.asInstanceOf[T]
      case strTerm: StringAttribute => strTerm.getValue.asInstanceOf[T]
      case idTerm: UniqueID => idTerm.asInstanceOf[T]
    }

    implicit def fn1ToPslFn[A: prove[PslType]#containsType](fn: A => Double): ExternalFunction = new ExternalFunction {

      def getValue(db: ReadOnlyDatabase, args: GroundTerm*) = fn(extract[A](args(0)))

      val getArity = 1
      val getArgumentTypes = Array(ArgumentType.String)
    }

    implicit def fn2ToPslFn[A: prove[PslType]#containsType, B: prove[PslType]#containsType](fn: (A, B) => Double): ExternalFunction = new ExternalFunction {

      def getValue(db: ReadOnlyDatabase, args: GroundTerm*) = fn(extract[A](args(0)), extract[B](args(1)))

      val getArity = 1
      val getArgumentTypes = Array(ArgumentType.String)
    }

    implicit def fn3ToPslFn[A : prove[PslType]#containsType, B : prove[PslType]#containsType, C : prove[PslType]#containsType](fn: (A, B, C) => Double): ExternalFunction = new ExternalFunction {

      def getValue(db: ReadOnlyDatabase, args: GroundTerm*) = fn(extract[A](args(0)), extract[B](args(1)), extract[C](args(2)))

      val getArity = 1
      val getArgumentTypes = Array(ArgumentType.String)
    }
  }

  private def argType[A : prove[PslType]#containsType](implicit ct:ClassTag[A]) = ct match {
    case ClassTag.Int => ArgumentType.Integer
    case ClassTag.Double => ArgumentType.Double
    case c if c.runtimeClass == classOf[String]  => ArgumentType.String
  }

  def R[A : prove[PslType]#containsType, B : prove[PslType]#containsType](pred:String) =
    PredicateFactory.getFactory.createStandardPredicate(pred, argType[A], argType[B])

  def f(pred:String, ext:ExternalFunction) =
    PredicateFactory.getFactory.createFunctionalPredicate(pred, ext)

}

object Foo {
  import PSLDSL._

  R[UniqueID, UniqueID]("Network")
  R[UniqueID, String]("Name")
  R[UniqueID, UniqueID]("Knows")
  R[UniqueID, UniqueID]("SamePerson")

  import FunctionConversions._

  f("SameName", {(s1:String, s2:String) => editDistance(s1,s2).toDouble})

}
