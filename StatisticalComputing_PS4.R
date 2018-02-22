# Noah Bardash
# Problem Set 4
# Monty Hall: Let's Make a Deal 2

# Getting Started

monty <- function(door_choice, car_location) {
  if(door_choice == car_location) {win <- TRUE}
  else {win <- FALSE}
  return(win)
}
monty(1, 1)
monty(1, 2)
monty(sample(1:3, 1), sample(1:3, 1))

# 1

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

setValidity("door", function(object){
  if(object@chosenDoor < 1 | object@chosenDoor > 3){return("invalid chosen door")}
  if(object@carDoor < 1 | object@carDoor > 3){return("invalid car door")}
  if(object@switch != TRUE & object@switch != FALSE){return("invalid switch entry")}
}
)

setMethod("initialize", "door", function(.Object, ...) {
  value = callNextMethod()
  validObject(value)
  return(value)
})

game1 <- new("door", chosenDoor=as.integer(1), carDoor=as.integer(3), switch=FALSE)
game2 <- new("door", chosenDoor=as.integer(1), carDoor=as.integer(1), switch=FALSE)



# 2

setGeneric("PlayGame", function(object="door") {
  standardGeneric("PlayGame")
})
setMethod("PlayGame", "door", function(object){
  firstChosenDoor = sample(1:3, 1)
  object@carDoor = sample(1:3, 1)
  if(object@switch == FALSE){
    object@chosenDoor = firstChosenDoor
    if(object@chosenDoor == object@carDoor){winner = TRUE}
    else{winner = FALSE}
  }
  else{
    
  }
  if(winner == TRUE){return("Congrats! You win.")}
  else{return("Better luck next time!")}
})


PlayGame(game2)

#setValidity("door", function(game){
# winner = FALSE
# 
#}
#)
