{- |
= BNF

@
Template := Template-element Template

Template-element := Plain-text
                    {{ Expression }} # output
                    {{ Expression | Filter }}
                    {% break %}
                    {% continue %}
                    Comment
                    If-template
                    Case-template
                    For-template
                    Assign-template

Template-break := Template-break-element Template-break

Template-break-element := Template-element
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

If-template := {% if Expression %} Template {% endif %}
               {% if Expression %} Template Else-list {% endif %}

Else-list := {% else %} Template
             {% elsif Expression %} Template Else-list

Unless-template := {% unless Expression %} Template {% endunless %}

Case-template := {% case Expression %} When-list {% endcase %}

When-list := {% when Expression %} Template When-list

For-template := {% for Variable-name in Expression %} Template {% endfor %}

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
module Text.Liquoh
  ( module Export
  , parseAndInterpret
  ) where

import Text.Liquoh.Common
import Text.Liquoh.Parser as Export (parse)
import Text.Liquoh.Interpreter as Export (ShopifyTemplate, interpret)

import qualified Data.Text as Text

parseAndInterpret :: Context -> Text.Text -> Result Text.Text
parseAndInterpret context input = (parse input :: Either Text.Text [ShopifyTemplate]) >>= interpret context
