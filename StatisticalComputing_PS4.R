# Noah Bardash
# Problem Set 4
# Monty Hall: Let's Make a Deal 2

# Getting Started

# function monty accepts input of door choice, car location
monty <- function(door_choice, car_location) {
  # if the two inputs point to the same door, record a win, otherwise record a loss
  if(door_choice == car_location) {win <- TRUE}
  else {win <- FALSE}
  return(win)
}
# test cases
monty(1, 1)
monty(1, 2)
monty(sample(1:3, 1), sample(1:3, 1))

# 1

# create S4 door class with three slots:
# numeric chosenDoor (will later be cast as int) - location of door choice
# numeric carDoor (will later be cast as int) - location of car
# logical switch - boolean representing whether or not you might switch doors
setClass(Class="door",
         representation = representation(
           chosenDoor = "numeric",
           carDoor = "numeric",
           switch = "logical"
         ),
         prototype = prototype(
           chosenDoor = c(),
           carDoor = c(),
           switch = c()
         )
)

# validity function ensures door values are between 1 & 3 (inclusive)
setValidity("door", function(object){
  if(object@chosenDoor < 1 | object@chosenDoor > 3){return("invalid chosen door")}
  if(object@carDoor < 1 | object@carDoor > 3){return("invalid car door")}
  #if(object@switch != TRUE & object@switch != FALSE){return("invalid switch entry")}
}
)

# initialize door
setMethod("initialize", "door", function(.Object, ...) {
  value = callNextMethod()
  validObject(value)
  return(value)
})

# test door creation
game1 <- new("door", chosenDoor=as.integer(1), carDoor=as.integer(3), switch=FALSE)
game2 <- new("door", chosenDoor=as.integer(1), carDoor=as.integer(1), switch=FALSE)
game3 <- new("door", chosenDoor=as.integer(1), carDoor=as.integer(1), switch=TRUE)

# 2


# instantiate winner variable, null
winner <- NULL

# create generic PlayGame method
setGeneric("PlayGame", function(object="door") {
  standardGeneric("PlayGame")
})

# create door PlayGame method
setMethod("PlayGame", "door", function(object){
  firstChosenDoor = sample(3, 1) # randomly choose initial door
  object@carDoor = sample(3, 1) # randomly choose car door, overwrite value from creation of door
  # if switch input is false, set the door's chosen door to the random value set two lines above
  if(object@switch == FALSE){
    object@chosenDoor = firstChosenDoor
  }
  # otherwise (if switch input is true)     car is 1, first choice is 3. remove 1. rmDO is now (2,3)
  else{
    # determine which door to open & remove from play
    openDoorOptions = c(1,2,3)[-object@carDoor] # cannot reveal door hiding the car
    if(object@carDoor != firstChosenDoor){ # if initial player's choice is not car door...
      openDoorOptions = openDoorOptions[-which(openDoorOptions==firstChosenDoor)] # cannot reveal door initially chosen
    }
    if(length(openDoorOptions) == 1){openDoor = openDoorOptions} # if only 1 door remains after above conditions, open that door
    else{openDoor = sample((openDoorOptions), 1)} # if not, randomly choose door to open from the remaining (2) doors
    doorsRemaining = c(1,2,3)[-openDoor]
    object@chosenDoor = sample(doorsRemaining, 1)
  }
  # if car door is the same as final chosen door, set winner to TRUE in global environment
  if(object@chosenDoor == object@carDoor){winner <<- TRUE}
  # otherwise winner is FALSE, return winner
  else{winner <<- FALSE} 
  return(winner)
})

PlayGame(game2)
PlayGame(game3)

# Simulation

# play a game with no door switching
noSwitchGame = new("door", chosenDoor=as.integer(1), carDoor=as.integer(1), switch=FALSE)
noSwitchResults = replicate(1000, noSwitchGame) # replicate scenario setup 1000 times
X<-sapply(noSwitchResults, PlayGame) # call PlayGame method on each of the 1000 games
table(X) # display TRUEs (wins) and FALSEs (losses) in a table

# play a game with potential door switching
SwitchGame = new("door", chosenDoor=as.integer(1), carDoor=as.integer(1), switch=TRUE)
SwitchResults = replicate(1000, SwitchGame) # replicate scenario setup 1000 times
Y<-sapply(SwitchResults, PlayGame) # call PlayGame method on each of the 1000 games
table(Y) # display TRUEs (wins) and FALSEs (losses) in a table

