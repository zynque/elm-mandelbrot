module Mandelbrot exposing (mandel)


plus : (Float, Float) -> (Float, Float) -> (Float, Float)
plus (a, b) (c, d) = (a + c, b + d)

mult : (Float, Float) -> (Float, Float) -> (Float, Float)
mult (a, b) (c, d) = ((a*c) - (b*d), (a*d) + (b*c))

magnitudeSquared : (Float, Float) -> Float
magnitudeSquared (a, b) = (a * a) + (b * b)

iteratorFor c = \z -> plus (mult z z) c

bailCondition p = magnitudeSquared p > 4.0

repeat : Int -> Int -> (a -> a) -> (a -> Bool) -> a -> (a, Int)
repeat attemptsSoFar attemptsRemaining f bail value =
  if (attemptsRemaining < 1) then (value, attemptsSoFar)
  else if (bail value) then (value, attemptsSoFar)
  else repeat (attemptsSoFar + 1) (attemptsRemaining - 1) f bail (f value)

mandel : (Float, Float) -> (Float, Int)
mandel point =
  let iterator = iteratorFor point
      (final, iterations) = repeat 0 500 iterator bailCondition (0.0,0.0)
  in (magnitudeSquared final, iterations)
