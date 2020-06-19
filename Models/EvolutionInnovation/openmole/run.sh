
for i in `seq 1 10`
do
  echo $i
  /home/ubuntu/ComplexSystems/OpenMole/openmole-11.0-SNAPSHOT/openmole/openmole --mem 15G --password-file omlpwd -p /home/ubuntu/ComplexSystems/UrbanEvolution/Models/EvolutionInnovation/target/scala-2.12/urbanevolution_2.12-0.1-SNAPSHOT.jar --script Exploration.oms 
done

