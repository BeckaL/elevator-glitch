This is a scala / SBT project.
 
The eventual aim is to model how different lift algorithms affect people's waiting time.

It's also got an ASCII animation front end to visualise a simulation (and because that's about as front end as I get)

## Running the programme to see a random visualisation

`sbt run`

## Running the tests

`sbt test`

## Still to do

* Add in multiple lifts functionality (possibly with concurrency)
* Add in timer stats to evaluate a simulation
* Implement different algorithms to test by extending ScenarioCriteria
