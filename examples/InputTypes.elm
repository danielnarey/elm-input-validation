import InputValidation as Validate exposing (TypedInput(..))
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as Json
import List


main =
  Html.beginnerProgram
    { model = userInput
    , view = view
    , update = update
    }


-- MODEL

type alias UserInput =
  { name : Maybe String
  , age : Maybe Int
  , pi : Maybe Float
  , answer : Maybe Bool
  , chars : Maybe (List Char)
  }

userInput =
  { name = Nothing
  , age = Nothing
  , pi = Nothing
  , answer = Nothing
  , chars = Nothing
  }


-- UPDATE

type CapturedValue a
  = Name (TypedInput a)
  | Age (TypedInput a)
  | Pi (TypedInput a)
  | Answer (TypedInput a)
  | Chars (TypedInput a)


update : CapturedValue (List Char) -> UserInput -> UserInput
update capturedValue userInput =
  case capturedValue of
    Name value ->
      { userInput
      | name =
          value
            |> Validate.readStringInput
            |> Result.toMaybe
      }

    Age value ->
      { userInput
      | age =
          value
            |> Validate.readIntInput
            |> Result.toMaybe
      }

    Pi value ->
      { userInput
      | pi =
          value
            |> Validate.readFloatInput
            |> Result.toMaybe
      }

    Answer value ->
      { userInput
      | answer =
          value
            |> Validate.readBoolInput
            |> Result.toMaybe
      }

    Chars value ->
      { userInput
      | chars =
          value
            |> Validate.readCustomInput
            |> Result.toMaybe
      }


-- VIEW

view : UserInput -> Html (CapturedValue (List Char))
view userInput =
  let
    row1 =
      [ "What is your name?"
        |> Html.text
      , StringInput
        |> inputField Name
      , userInput.name
        |> Maybe.map (\s -> "\"" ++ s ++ "\"")
        |> Maybe.withDefault "Expecting a `String`"
        |> Html.text
      ]

    row2 =
      [ "How old are you?"
        |> Html.text
      , IntInput
        |> inputField Age
      , userInput.age
        |> Maybe.map toString
        |> Maybe.withDefault "Expecting an `Int`"
        |> Html.text
      ]

    row3 =
      [ "Enter the first few digits of Pi:"
        |> Html.text
      , FloatInput
        |> inputField Pi
      , userInput.pi
        |> Maybe.map toString
        |> Maybe.withDefault "Expecting a `Float`"
        |> Html.text
      ]

    row4 =
      [ "Type the word PLATYPUS:"
        |> Html.text
      , BoolInput (\s -> String.toLower s == "platypus")
        |> inputField Answer
      , userInput.answer
        |> Maybe.map (\b -> if b then "Correct" else "Incorrect")
        |> Maybe.withDefault "Expecting a `Bool` result"
        |> Html.text
      ]

    row5 =
      [ "Type some characters!"
        |> Html.text
      , CustomInput (Json.string |> Json.map String.toList)
        |> inputField Chars
      , userInput.chars
        |> Maybe.map (\list -> list |> List.intersperse ',' |> String.fromList)
        |> Maybe.map (\s -> "[" ++ s ++ "]")
        |> Maybe.withDefault "Expecting a `Char` sequence"
        |> Html.text
      ]


  in
    [ row1
    , row2
    , row3
    , row4
    , row5
    ]
      |> List.map tableRow
      |> Html.table []


tableRow : List (Html msg) -> Html msg
tableRow elementList =
  elementList
    |> List.map (\elem -> Html.td [] [elem])
    |> Html.tr []


inputField : (TypedInput a -> CapturedValue a) -> (Json.Value -> TypedInput a) -> Html (CapturedValue a)
inputField captureKey inputKey =
  []
    |> Html.input
      [ Attr.type_ "text"
      , inputKey
        |> Validate.captureOnInput captureKey
      ]
