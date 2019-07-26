This is a scala programme with an SBT structure to model how different lift algorithms affect people's waiting time.

The model's assumptions are as follows:

* Time proceeds in a 'tick based' fashion, rather than continuously
* People will wait for a lift until it comes (rather than getting bored and taking the stairs)
* Lifts take 1 tick to move up or down one floor, and it takes 1 tick for 0 - 5 people to enter or leave the lift, and 2 ticks for 5 or more people.

## Running the programme 

`sbt compile`
`sbt run`

## Running the tests

`sbt test`

