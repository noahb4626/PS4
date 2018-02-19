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
new("door")
#setValidity("door", function(game){
 # winner = FALSE
 # if(game@chosenDoor == game@carDoor){winner = TRUE}
#  else{winner = FALSE}
 # return(winner)
 # if(game@chosenDoor == game@carDoor){return("Congrats! You win.")}
 # else{return("Better luck next time!")}
}
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

new("door", chosenDoor=as.integer(1), carDoor=as.integer(3), switch=FALSE)


new("door", 1, 1, FALSE)
