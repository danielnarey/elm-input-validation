module InputValidation exposing (..)

{-| ## Type-validation for user input from text, numeric, and custom fields

This library provides a consistent way of handling type-validation for user
input. The type of input expected is specified when constructing an input
element in the Elm program's view, and the corresponding reader function is
called when program updates in response to input. If the input does not match
the expected type, or the reader function called does not correspond to the type
expected, an error results.

# Value Representation
@docs TypedInput

# Capturing Input
@docs captureOnInput

# Reading Input with Type Validation
@docs readStringInput, readIntInput, readFloatInput, readBoolInput
@docs readCustomInput

# Reading Input without Type Validation
@docs readInputAsString

-}

import Json.Decode as Json exposing (Decoder)
import Html exposing (Attribute)
import Html.Events as Events
import Result


-- VALUE REPRESENTATION

{-| Represents a JavaScript value with a type specification.

`BoolInput` requires an expression that takes a string and returns a `Bool`.
`CustomInput` requires a `Json`
[`Decoder`](http://package.elm-lang.org/packages/elm-lang/core/latest/Json-Decode#Decoder).
`Fail` captures no input and may be used for error handling.
-}
type TypedInput a
  = StringInput Json.Value
  | IntInput Json.Value
  | FloatInput Json.Value
  | BoolInput (String -> Bool) Json.Value
  | CustomInput (Decoder a) Json.Value
  | Fail


-- CAPTURING INPUT

{-| Capture input from a field as `TypedInput`

    type Msg
      = Name (TypedInput a)
    ...

    input [ type' "text", captureOnInput Name StringInput ] []

-}
captureOnInput : (TypedInput a -> msg) -> (Json.Value -> TypedInput a) -> Attribute msg
captureOnInput captureKey inputKey =
  Json.value
    |> Json.at ["target", "value"]
    |> Json.map (\v -> inputKey v)
    |> Json.map captureKey
    |> Events.on "input"


-- READING INPUT WITH TYPE VALIDATION

{-| Decode a `StringInput` value as a string; return an error message if the
decoder fails or if the argument is a type other than `StringInput`.

-}
readStringInput : TypedInput a -> Result String String
readStringInput typedInput =
  case typedInput of
    StringInput jsonValue ->
      jsonValue
        |> Json.decodeValue Json.string

    _ ->
      Err "Expecting `StringInput`"


{-| Decode an `IntInput` value as an string, then attempt to convert the string
to an `Int`; return an error message if the string decoder fails, if type
conversion fails, or if the argument is a type other than `IntInput`.

-}
readIntInput : TypedInput a -> Result String Int
readIntInput typedInput =
  case typedInput of
    IntInput jsonValue ->
      jsonValue
        |> Json.decodeValue Json.string
        |> Result.andThen String.toInt

    _ ->
      Err "Expecting `IntInput`"


{-| Decode a `FloatInput` value as a string, then attempt to convert the string
to a `Float`; return an error message if the string decoder fails, if type
conversion fails, or if the argument is a type other than `FloatInput`.

-}
readFloatInput : TypedInput a -> Result String Float
readFloatInput typedInput =
  case typedInput of
    FloatInput jsonValue ->
      jsonValue
        |> Json.decodeValue Json.string
        |> Result.andThen String.toFloat

    _ ->
      Err "Expecting `FloatInput`"


{-| Decode a `BoolInput` value as a string, then pass the string to a custom
expression that will return a `Bool`; return an error message if the string
decoder fails or if the argument is a type other than `BoolInput`.

-}
readBoolInput : TypedInput a -> Result String Bool
readBoolInput typedInput =
  case typedInput of
    BoolInput expression jsonValue ->
      jsonValue
        |> Json.decodeValue Json.string
        |> Result.map expression


    _ ->
      Err "Expecting `BoolInput`"


{-| Decode a `CustomInput` value with a custom `Json`
[`Decoder`](http://package.elm-lang.org/packages/elm-lang/core/latest/Json-Decode#Decoder);
return an error message if the decoder fails or if the argument is a type other
than `CustomInput`.
-}
readCustomInput : TypedInput a -> Result String a
readCustomInput typedInput =
  case typedInput of
    CustomInput decoder jsonValue ->
      jsonValue
        |> Json.decodeValue decoder

    _ ->
      Err "Expecting `CustomInput`"


-- READING INPUT WITHOUT TYPE VALIDATION

{-| Decode any `TypedInput` value as a string, or return an error message if the
decoder fails. Useful for debugging.

-}
readInputAsString : TypedInput a -> Result String String
readInputAsString typedInput =
  case typedInput of
    StringInput jsonValue ->
      jsonValue
        |> Json.decodeValue Json.string

    IntInput jsonValue ->
      jsonValue
        |> Json.decodeValue Json.string

    FloatInput jsonValue ->
      jsonValue
        |> Json.decodeValue Json.string

    BoolInput expression jsonValue ->
      jsonValue
        |> Json.decodeValue Json.string

    CustomInput decoder jsonValue ->
      jsonValue
        |> Json.decodeValue Json.string

    Fail ->
      Err "Something went wrong: Trying to read `Fail` as input"
