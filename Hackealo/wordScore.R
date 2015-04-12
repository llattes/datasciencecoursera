## El "puntaje" de una palabra es la suma del puntaje de cada letra.
## El "puntaje" de una letra es la posición en donde se encuentra en una palabra,
## multiplicada por su posición en el abecedario. 
## Por ejemplo, el puntaje de "acb" es (1*1) + (2*3) + (3*2). 
## ¿Cuál es la suma de las puntuaciones de las permutaciones de "EfuinDm"?

calculateScore <- function(word) {
  # Convert the String to a Character Vector.
  vectorifiedWord <- substring(word, seq(1, nchar(word), 1), seq(1, nchar(word), 1))
  # Obtain all permutations of the vectorified word with no repeats. Returns Matrix.
  wordPermutations <- permutations(n = length(vectorifiedWord), r = length(vectorifiedWord), v = vectorifiedWord)
  permutationsTotal <- 0
  for (i in 1:dim(wordPermutations)[1]) {
    permutationScore <- 0
    for (j in 1:dim(wordPermutations)[2]) {
      # Find the position of the given letter in the letters/LETTERS arrays.
      nonCaps <- match(x = wordPermutations[i, j], table = letters, nomatch = 0)
      caps <- match(x = wordPermutations[i, j], table = LETTERS, nomatch = 0)
      letterPosition <- nonCaps + caps
      permutationScore <- permutationScore + (j * letterPosition)
    }
    permutationsTotal <- permutationsTotal + permutationScore
  }
  permutationsTotal
}

# score <- calculateScore("EfuinDm")
