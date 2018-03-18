import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

main = 
    Html.beginnerProgram { model = model, view = view, update = update }

-- main = 
--     Html.program { init = init, model = model, view = view, update = update }



-- -- Init

-- init: (Model, Cmd Msg)
-- init = 
--     let 
--         model = 
--             {

-- MODEL

type alias Model = 
    { name: String
    , password: String
    , passwordAgain: String
    }

model: Model
model = 
    Model "" "" "" 


-- UPDATE

type Msg 
    = Name String
    | Password String
    | PasswordAgain String
        

update : Msg -> Model -> Model
update msg model =
    case msg of
        Name newName -> 
            { model | name = newName }
        Password newPassword -> 
            { model | password = newPassword }
        PasswordAgain newPasswordAgain -> 
            { model | passwordAgain = newPasswordAgain }
            
-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ input [ type_ "text", placeholder "Name", onInput Name ] []
    , input [ type_ "password", placeholder "Password", onInput Password ] []
    , input [ type_ "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
    , viewValidation model
    ]

viewValidation : Model -> Html msg
viewValidation model =
  let
    (color, message) =
      if model.password == model.passwordAgain then
        ("green", "OK")
      else
        ("red", "Passwords do not match!")
  in
    div [ style [("color", color)] ] [ text message ]
