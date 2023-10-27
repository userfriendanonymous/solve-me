module View where

import Prelude

import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Expr as Expr
import Flame (Html, QuerySelector(..), Subscription, (:>))
import Flame.Application.NoEffects as FAN
import Flame.Html.Attribute (onChange, onInput, onClick)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE

data Stage
    = Loading
    | Correct
    | Incorrect
    | Invalid

derive instance Eq Stage

type Model =
    { expr :: Either Stage Expr.Value
    , answer :: String
    , score :: Int
    }

data Message
    -- = NewExpr Int
    = SetExpr Expr.Value
    | SetAnswer String
    | SubmitAnswer

newExpr :: Int -> Array (Aff (Maybe Message))
newExpr diff =
    [ do
        expr <- liftEffect (Expr.random diff)
        delay $ Milliseconds 500.0
        pure $ Just $ SetExpr expr
    ]

init :: Tuple Model (Array (Aff (Maybe Message)))
init =
    { expr : Left Loading
    , answer : ""
    , score : 0
    } :> newExpr 2

update :: Model -> Message -> Tuple Model (Array (Aff (Maybe Message)))
update model = case _ of
    SetExpr value -> model { expr = Right value } :> []
    SetAnswer value -> model { answer = value } :> []
    SubmitAnswer -> case model.expr of
        Right expr ->
            let
                stage = case Int.fromString model.answer of
                    Just answer ->
                        if Expr.solve expr == answer
                        then Correct
                        else Incorrect
                    Nothing -> Invalid
            in model
                { answer = ""
                , expr = Left stage
                , score = model.score + if stage == Correct then 1 else 0
                } :> newExpr 2
        _ -> model :> []

view :: Model -> Html Message
view model = HE.div_
    [ HE.div
        [ HA.style
            [ "background-color" :> "black"
            , "color" :> "white"
            , "height" :> "100vh"
            , "padding" :> "20px"
            , "box-sizing" :> "border-box"
            , "display" :> "flex"
            , "flex-direction" :> "column"
            , "font-size" :> "20px"
            , "font-family" :> "'Josefin Sans', sans-serif"
            ]
        ]
        [ HE.div
            [ HA.style
                [ "font-weight" :> "bold"
                ]
            ]
            [ "Score: " <> show model.score ]
        , HE.div
            [ HA.style
                [ "display" :> "flex"
                , "flex-grow" :> "1"
                , "align-items" :> "center"
                , "justify-content" :> "center"
                ]
            ]
            [ case model.expr of
                Right expr -> HE.div
                    [ HA.style
                        [ "display" :> "flex"
                        , "flex-direction" :> "column"
                        , "align-items" :> "center"
                        , "gap" :> "20px"
                        ]
                    ]
                    [ HE.div
                        [ HA.style
                            [ "display" :> "flex"
                            , "align-items" :> "center"
                            , "gap" :> "15px" ]
                        ]
                        [ HE.div
                            [ HA.style
                                [ "font-size" :> "30px"
                                , "text-align" :> "center"
                                ]
                            ]
                            [ show expr <> " = " ]
                        , HE.input
                            [ HA.placeholder "?"
                            , onInput (\event -> SetAnswer event)
                            , HA.style
                                [ "all" :> "unset"
                                , "height" :> "20px"
                                , "background" :> "white"
                                , "color" :> "black"
                                , "width" :> "100px"
                                , "padding" :> "10px"
                                ]
                            ]
                        ]
                    , HE.button
                        [ onClick SubmitAnswer
                        , HA.style
                            [ "all" :> "unset"
                            , "background" :> "white"
                            , "color" :> "black"
                            , "padding" :> "20px"
                            , "cursor" :> "pointer" 
                            ]
                        ]
                        "Submit"
                    ]
                Left stage -> HE.div
                    [ HA.style
                        [ "font-size" :> "40px"
                        , "font-weight" :> "900"
                        ]
                    ]
                    [ case stage of
                        Loading -> HE.div_ [ "Loading..." ]
                        Correct -> HE.div_ [ "Correct!" ]
                        Incorrect -> HE.div_ [ "Incorrect!" ]
                        Invalid -> HE.div_ [ "Invalid input" ]
                    ]
            ]
        ]
    ]

subscribe :: Array (Subscription Message)
subscribe = []
