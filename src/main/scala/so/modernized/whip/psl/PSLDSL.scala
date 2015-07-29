package so.modernized.whip.psl

import edu.umd.cs.psl.database.loading.Inserter
import edu.umd.cs.psl.database.{DataStore, ReadOnlyDatabase}
import edu.umd.cs.psl.model.kernel.predicateconstraint.{DomainRangeConstraintType, DomainRangeConstraintKernel, SymmetryConstraintKernel}

//import scala.collection.JavaConverters._
import scala.reflect.runtime.universe._
import edu.umd.cs.psl.database.Partition
import edu.umd.cs.psl.database.rdbms.{RDBMSUniqueIntID, RDBMSUniqueStringID}
import edu.umd.cs.psl.model.Model
import edu.umd.cs.psl.model.argument._
import edu.umd.cs.psl.model.atom.QueryAtom
import edu.umd.cs.psl.model.formula._
import edu.umd.cs.psl.model.function.ExternalFunction
import edu.umd.cs.psl.model.kernel.rule.{CompatibilityRuleKernel, ConstraintRuleKernel}
import edu.umd.cs.psl.model.predicate.{SpecialPredicate, PredicateFactory}
//import edu.umd.cs.psl.model.set.term.{SetUnion, SetTerm, FormulaSetTerm, VariableSetTerm}

import so.modernized.whip.util.union._

object PSLDSL {

  type PslType = union[String]#or[Int]#or[Double]#or[UniqueID]
  type IdType = union[Int]#or[String]

  object FunctionConversions {

    def extract[T : prove[PslType]#containsType](gt: GroundTerm): T = gt match {
      case intTerm: IntegerAttribute => intTerm.getValue.asInstanceOf[T]
      case dblTerm: DoubleAttribute => dblTerm.getValue.asInstanceOf[T]
      case strTerm: StringAttribute => strTerm.getValue.asInstanceOf[T]
      case idTerm: UniqueID => idTerm.asInstanceOf[T]
    }


    implicit def fn1ToPslFn[A: prove[PslType]#containsType : TypeTag](fn: A => Double): ExternalFunction = new ExternalFunction {

      def getValue(db: ReadOnlyDatabase, args: GroundTerm*) = fn(extract[A](args(0)))

      val getArity = 1
      val getArgumentTypes = Array(argType[A])
    }

    implicit def fn2ToPslFn[A: prove[PslType]#containsType : TypeTag, B: prove[PslType]#containsType : TypeTag](fn: (A, B) => Double): ExternalFunction = new ExternalFunction {

      def getValue(db: ReadOnlyDatabase, args: GroundTerm*) = fn(extract[A](args(0)), extract[B](args(1)))

      val getArity = 2
      val getArgumentTypes = Array(argType[A], argType[B])
    }

    implicit def fn3ToPslFn[A : prove[PslType]#containsType : TypeTag, B : prove[PslType]#containsType : TypeTag, C : prove[PslType]#containsType : TypeTag](fn: (A, B, C) => Double): ExternalFunction = new ExternalFunction {

      def getValue(db: ReadOnlyDatabase, args: GroundTerm*) = fn(extract[A](args(0)), extract[B](args(1)), extract[C](args(2)))

      val getArity = 3
      val getArgumentTypes = Array(argType[A], argType[B], argType[C])
    }
  }

  implicit def pslTypeToGroundTerm[T : prove[PslType]#containsType](t:T):GroundTerm = t match {
    case i:Int => new IntegerAttribute(i)
    case d:Double => new DoubleAttribute(d)
    case s:String => new StringAttribute(s)
    case id:UniqueID => id
  }

  implicit class FormulaMethods(val f1:Formula) extends AnyVal{

    def &(f2:Formula) = new Conjunction(f1, f2)
    def |(f2:Formula) = new Disjunction(f1, f2)
    def >>(f2:Formula) = new Rule(f1, f2)
    def unary_~ = new Negation(f1)
    def where(weight:Double, isSquared:Boolean = false)(implicit m:Model) {m.addKernel(new CompatibilityRuleKernel(f1, weight, isSquared))}
  }

  def constraint(r:Formula)(implicit m:Model) {m addKernel new ConstraintRuleKernel(r)}

  implicit class VarStringContext(val sc:StringContext) extends AnyVal {
    def v(args:Any*) = new Variable(sc.parts.mkString)
  }

  implicit class VariableMethods(val v1:Variable) extends AnyVal {
    def ^(v2:Variable) = new QueryAtom(SpecialPredicate.NonSymmetric, v1, v2)
    def !(v2:Variable) = new QueryAtom(SpecialPredicate.NotEqual, v1, v2)
    def =:=(v2:Variable) = new QueryAtom(SpecialPredicate.Equal, v1, v2)
  }

  private def argType[A : prove[PslType]#containsType](implicit aTpe:TypeTag[A]) = typeOf[A] match {
    case i if i <:< typeOf[Int]      => ArgumentType.Integer
    case d if d <:< typeOf[Double]   => ArgumentType.Double
    case s if s <:< typeOf[String]   => ArgumentType.String
    case u if u <:< typeOf[UniqueID] => ArgumentType.UniqueID
  }

  def id[ID : prove[IdType]#containsType](idType:ID):UniqueID = idType match {
    case sId:String => new RDBMSUniqueStringID(sId)
    case iId:Int => new RDBMSUniqueIntID(iId)
  }

  trait Symmetry {
    this: R[_,_] =>
     model addKernel new SymmetryConstraintKernel(pred)
  }

  trait Functional {this: PslType =>}
  trait PartialFunctional {this: PslType =>}

  object R {

    protected trait Inversion {
      this:R[_,_] =>

      abstract override def apply(ts:Term*) = new QueryAtom(pred, ts.reverse:_*)
    }
  }

  case class R[A : prove[PslType]#containsType, B :prove[PslType]#containsType](predName:String)(implicit ds:DataStore, m:Model, aTpe:TypeTag[A], bTpe:TypeTag[B]) {
    protected[PSLDSL] val pred = PredicateFactory.getFactory.createStandardPredicate(predName, argType[A], argType[B])
    protected val model = m

    ds.registerPredicate(pred)

    typeOf[A] match {
      case a if a <:< typeOf[Functional] => model addKernel new DomainRangeConstraintKernel(pred, DomainRangeConstraintType.Functional)
      case a if a <:< typeOf[PartialFunctional] => model addKernel new DomainRangeConstraintKernel(pred, DomainRangeConstraintType.PartialFunctional)
      case otw => ()
    }

    typeOf[B] match {
      case b if b <:< typeOf[Functional] => model addKernel new DomainRangeConstraintKernel(pred, DomainRangeConstraintType.InverseFunctional)
      case b if b <:< typeOf[PartialFunctional] => model addKernel new DomainRangeConstraintKernel(pred, DomainRangeConstraintType.PartialInverseFunctional)
      case otw => ()
    }



    def apply(ts:Term*) = new QueryAtom(pred, ts:_*)

    private var inserter:Inserter = null
    private var lastPart:Partition = null
    def load(part:Partition)(a:A, b:B): Unit = {
      if(inserter==null || part != lastPart) {
        inserter = ds.getInserter(pred, part)
        lastPart = part
      }
      inserter.insert(a.asInstanceOf[AnyRef],b.asInstanceOf[AnyRef])
    }

    private var labeledInserter:Inserter = null
    private var labeledLastPart:Partition = null
    def loadLabeled[A1 : prove[PslType]#containsType, B1 : prove[PslType]#containsType](part:Partition)(a:A1, b:B1, conf:Double): Unit = {
      if(labeledInserter==null || part != labeledLastPart) {
        labeledInserter = ds.getInserter(pred, part)
        labeledLastPart = part
      }
      labeledInserter.insertValue(conf, a.asInstanceOf[AnyRef],b.asInstanceOf[AnyRef])
    }

    import R._
    def inverse = if (this.isInstanceOf[Symmetry]) {
      new R[A,B](predName + "__inverse") with Symmetry with Inversion
    } else {
      new R[A,B](predName + "__inverse") with Inversion
    }
  }

  implicit def rToPred(r:R[_,_]) = r.pred
  implicit def rTravToPredTrav(rs:Traversable[R[_,_]]) = rs map rToPred

  case class f(predName:String, ext:ExternalFunction) {
    private val pred = PredicateFactory.getFactory.createFunctionalPredicate(predName,ext)
    def apply(ts:Term*) = new QueryAtom(pred, ts:_*)
  }

  /*
  object SetComparisonOps {
    private var auxVarIdx = 0
    private def nextAuxId = {
      val res = auxVarIdx
      auxVarIdx += 1
      res
    }

    private def auxVariable = new Variable("aux__" + nextAuxId)

    def set(v:Variable, path:Seq[R[_,_]]=Seq.empty) = {
      if (path.isEmpty) {
        new VariableSetTerm(v, ArgumentType.UniqueID)
      } else {
        var v1 = v
        var v2 = auxVariable
        val iter = path.iterator
        val formula = path.map { pred =>
          val qa = pred(v1, v2)
          v1 = v2
          v2 = auxVariable
          qa
        }.reduce[Formula](_ & _)
        new FormulaSetTerm(formula, v2, Set(v).asJava)
      }
    }

    implicit class SetTermMethods(st1:SetTerm) extends AnyVal {
      def +(st2:SetTerm) = new SetUnion(st1, st2)

      def setEquality(st2:SetTerm)(implicit ds:DataStore, m:Model) = {
        val predName:String = "setEquality__" + nextAuxId
        val (variables, argTypes) = st2.getAnchorVariables(st1.getAnchorVariables(new VariableTypeMap)).asScala.toSeq.sortBy(_._1.getName).unzip
        val auxPred = PredicateFactory.getFactory.createStandardPredicate(predName, argTypes:_*)
        ds.registerPredicate(auxPred)
        // todo actually setting this up needs access to some other predicate from somewhere
        //m.addKernel(new SetDefinitionKernel(auxPred, st1, st2, variables, , new AggregateSetEquality))
        auxPred(variables)
      }
    }
  }
  */

}

