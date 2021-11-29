package urbanevolution

import scala.util.Random

object RunUrbanEvolution extends App {

  implicit val rng: Random = new Random

  val model = UrbanEvolution(
    syntheticCities = 30,
    syntheticHierarchy = 1.0,
    syntheticMaxPop =  100000.0,
    finalTime = 50,
    seed = rng.nextInt(),
    growthRate = 0.0,
    innovationWeight = 0.005,
    gravityDecay = 0.5,
    innovationDecay = 0.3,
    mutationRate = 0.4,
    newInnovationHierarchy = 0.5,
    earlyAdoptersRate = 0.2,
    utilityStd = 1.0,
    utilityDistribution = "log-normal" // "normal"
  )

  val result = UrbanEvolution.run(model)

  println("Average diversity = "+result.averageDiversity)
  println("Average utility = "+result.averageUtility)
  println("Average innovation = "+result.averageInnovation)
  val finalHierarchy = Statistics.slope(result.populations.getCol(50).flatValues)._1
  println("Final pop hierarchy = "+finalHierarchy)
  println("Delta hierarchy pop = "+(finalHierarchy - Statistics.slope(result.populations.getCol(0).flatValues)._1))
  println("Average gravity flow = "+result.averageGravityFlow)

}
