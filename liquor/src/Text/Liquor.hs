{- |
= BNF

@
Statement := Statement-element Statement

Statement-element := Plain-text
                    {{ Expression }} # output
                    {{ Expression | Filter }}
                    {% break %}
                    {% continue %}
                    Comment
                    If-template
                    Case-template
                    For-template
                    Assign-template

Statement-break := Statement-break-element Statement-break

Statement-break-element := Statement-element
                          break

Value := \/-?[1-9][0-9]*(.[0-9]*)?\/ # number
         \/"[A-z_]+[A-z_0-9]*"\/ # string
         true
         false
         nil
         array

Variable := Variable-name
            Variable.Variable-name

Variable-name := \/[A-z]+\/

Comment := {% comment %} \/.*\/ {% endcomment %}

If-template := {% if Expression %} Statement {% endif %}
               {% if Expression %} Statement Else-list {% endif %}

Else-list := {% else %} Statement
             {% elsif Expression %} Statement Else-list

Unless-template := {% unless Expression %} Statement {% endunless %}

Case-template := {% case Expression %} When-list {% endcase %}

When-list := {% when Expression %} Statement When-list

For-template := {% for Variable-name in Expression %} Statement {% endfor %}

Assign-template := {% assign Variable-name = Expression %}

--tag := cycle Expression-list
--       cycle Expression: Expression-list
--
--       tablerow Variable-name in Expression
--
--       # raw
--       raw
--       endraw
--
--       capture Variable-name
--       endcapture
--
--       increment Variable-name
--       decrement Variable-name

Term := Value
        Variable
        Expression[Expression]
        Expression array-parameters
        (Expression..Expression)

Expression := Operand: Term
              Operator:
                Infix Left or
                Infix Left and
                Infix Non ==, !=
                Infix Non <, <=, >, >=, contains
                Infix Left ,

array-parameters := limit:Expression array-parameters
                    offset:Expression array-parameters
                    reversed array-parameters

filter := abs
          append Expression
@
-}
module Text.Liquor
  ( module Export
  , parseAndInterpret
  ) where

import Text.Liquor.Common as Export
import Text.Liquor.Interpreter as Export (ShopifyTemplate, interpret)
import Text.Liquor.Parser as Export (parse)

import qualified Data.Text as Text

parseAndInterpret :: Context -> Text.Text -> Result Text.Text
parseAndInterpret context input = (parse input :: Either Text.Text ShopifyTemplate) >>= interpret context
