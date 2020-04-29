
package urbanevolution


import org.openmole.spatialdata.model.urbandynamics._
import org.openmole.spatialdata.utils.math.Statistics

import scala.util.Random


case class UrbanEvolution(
                          model: Innovation
                         ) {

  def run(): (Double,Double,Double,Double) = {
    val result = Innovation.run(model)
    val finalHierarchy = Statistics.slope(result.macroResult.simulatedPopulation.getCol(model.dates.length).flatValues)._1
    (result.averageDiversity,result.averageUtility,result.averageInnovation,finalHierarchy)
  }

}

object UrbanEvolution {

  def apply(
            gravityDecay: Double,
            innovationDecay: Double,
            mutationRate: Double,
            newInnovationHierarchy: Double,
            earlyAdoptersRate: Double,
            utilityStd: Double,
            utilityDistribution: String,
            syntheticCities: Int,
            syntheticHierarchy: Double,
            syntheticMaxPop: Double,
            finalTime: Int,
            growthRate: Double,
            innovationWeight: Double,
            seed: Int
           ): UrbanEvolution = {

    implicit val rng: Random = new Random(seed)

    UrbanEvolution(
      Innovation(syntheticCities, syntheticHierarchy, syntheticMaxPop, finalTime, seed, growthRate, innovationWeight, gravityDecay,
        innovationDecay, mutationRate, newInnovationHierarchy, earlyAdoptersRate, utilityStd, utilityDistribution)
    )

  }

}
