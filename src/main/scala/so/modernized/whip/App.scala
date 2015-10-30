package

object Simulation extends App {

  import scala.util.Random
  import cc.factorie.directed._
  import cc.factorie.variable._
  import cc.factorie.infer._

  object FloorWetFamily extends DirectedFamily3[BooleanVar, BooleanVar, BooleanVar] {

    self =>
    def logpr(wet: BooleanValue, rains: BooleanValue, springer: BooleanValue): Double = {
      math.log(pr(wet, rains, springer))
    }

    def pr(wet: BooleanValue, rains: BooleanValue, springer: BooleanValue): Double = {
      if ((rains.booleanValue || springer.booleanValue) && wet.booleanValue) 0.95
      else 0.05
    }

    def sampledValue(rains: BooleanValue, springer: BooleanValue)(implicit random: scala.util.Random): BooleanValue = BooleanValue(true)

    //maths.nextGamma(alpha, beta)(random)

    case class Factor(override val _1: BooleanVar, override val _2: BooleanVar, override val _3: BooleanVar) extends super.Factor(_1, _2, _3) {
      def pr(wet: BooleanValue, rains: BooleanValue, springer: BooleanValue): Double = self.pr(wet, rains, springer)

      def sampledValue(rains: BooleanValue, springer: BooleanValue)(implicit random: scala.util.Random): BooleanValue = self.sampledValue(rains, springer)
    }

    def newFactor(_1: BooleanVar, _2: BooleanVar, _3: BooleanVar) = Factor(_1, _2, _3)
  }


  object RainsFamily extends DirectedFamily1[BooleanVar] {

    self =>
    def logpr(rains: BooleanValue): Double = {
      math.log(pr(rains))
    }

    def pr(rains: BooleanValue): Double = {
      if (rains.booleanValue) 0.15
      else 0.75
    }

    def sampledValue()(implicit random: scala.util.Random): BooleanValue = BooleanValue(random.nextBoolean())

    case class Factor(override val _1: BooleanVar) extends super.Factor(_1) {
      def pr(rains: BooleanValue): Double = self.pr(rains)

      def sampledValue(implicit random: Random): BooleanValue = self.sampledValue()
    }

    def newFactor(_1: BooleanVar) = Factor(_1)
  }


  object SpringerFamily extends DirectedFamily2[BooleanVar,BooleanVar] {

    self =>
    def logpr(springer: BooleanValue, rains: BooleanValue): Double = {
      math.log(pr(springer, rains))
    }

    def pr(springer: BooleanValue, rains: BooleanValue): Double = {
      if (springer.booleanValue && rains.booleanValue) 0.05
      if (springer.booleanValue && !rains.booleanValue) 0.95
      else 0.5

    }

    def sampledValue(rains: BooleanValue)(implicit random: scala.util.Random): BooleanValue = BooleanValue(true)

    case class Factor(override val _1: BooleanVar, override val _2: BooleanVar) extends super.Factor(_1, _2) {
      def pr(springer: BooleanValue, rains: BooleanValue): Double = self.pr(springer,rains)

      def sampledValue(rains: BooleanValue)(implicit random: scala.util.Random): BooleanValue = self.sampledValue(rains)
    }

    def newFactor(_1: BooleanVar, _2: BooleanVar) = Factor(_1, _2)
  }

  val rainsVar = new BooleanVariable(false)
  val springerVar = new BooleanVariable(false)
  val floorWetVar = new BooleanVariable

  implicit val directedModel = DirectedModel()
  directedModel += FloorWetFamily.newFactor(floorWetVar, rainsVar, springerVar)
  directedModel += RainsFamily.newFactor(rainsVar)
  directedModel += SpringerFamily.newFactor(springerVar,rainsVar)

  // ?
  //How to calculate the likelihood that it is raining while the floor is wet ???
  // ?

  println(directedModel.allFactors)
  println("childFactors of rainsVar: " + directedModel.getChildFactors(rainsVar))
  println("childFactors of floorWetVar: " + directedModel.getChildFactors(floorWetVar))
  println("childFactors of springerVar: " + directedModel.getChildFactors(springerVar))
}