{- |
= BNF

@
template := template-element template

template-element := plain-text
                    {{ expression }}
                    {{ expression | filter }}
                    {% tag %}

value := \/-?[1-9][0-9]*(.[0-9]*)?\/ # number
         \/"[A-z_]+[A-z_0-9]*"\/ # string
         true
         false
         nil
         array

variable := variable-name
            variable.variable-name

variable-name := \/[A-z]+\/

tag := # comment
       comment
       endcomment

       # control flow
       if expression
       endif
       unless exression
       endunless
       elsif expression
       else
       case expression
       when expression
       endcase

       # iteration
       for variable-name in expresson
       endfor
       break
       continue
       cycle expression-list
       cycle expression: expression-list
       tablerow variable-name in expression

       # raw
       raw
       endraw

       # variable
       assign variable-name = expression
       capture variable-name
       endcapture
       increment variable-name
       decrement variable-name

       # filter
       expression | filter

expression := value
              variable

              # binary operator
              expression < expression
              expression <= expression
              expression > expression
              expression >= expression
              expression == expression
              expression != expression
              expression && expression
              expression || expression
              expression contains expression

              # array
              expression[expression]
              expression array-parameters
              (expression..expression)

array := expression-without-array-without-parenthesis, array

expression-without-raw-array := expression \ array

array-parameters := limit:exression array-parameters
                    offset:expression array-parameters
                    reversed array-parameters

filter := abs
          append expression
@
-}
module Text.Liquoh
  (module Expression) where

import Text.Liquoh.Interpreter.Expression as Expression
  ( Expression, evaluate, number, string, bool, nil, array, (.<.), (.<=.), (.>.), (.>=.), (.==.), (./=.), (.&&.)
  , (.||.), at
  )
