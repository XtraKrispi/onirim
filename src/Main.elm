module Main exposing (main)

import Browser
import Card exposing (Card(..), Suit(..), Symbol(..), cardBack)
import Css
    exposing
        ( absolute
        , alignItems
        , backgroundColor
        , border2
        , borderBox
        , borderRadius
        , boxSizing
        , center
        , column
        , cursor
        , dashed
        , deg
        , displayFlex
        , fixed
        , flexDirection
        , fontFamily
        , fontSize
        , height
        , hover
        , int
        , justifyContent
        , left
        , margin
        , marginLeft
        , num
        , opacity
        , padding
        , pct
        , pointer
        , position
        , px
        , rem
        , rotateZ
        , sansSerif
        , spaceBetween
        , top
        , transform
        , transforms
        , translateY
        , vh
        , vw
        , width
        , zIndex
        , zero
        )
import Css.Global
import Css.Transitions exposing (transition)
import Deck exposing (Deck, draw)
import Html as H
import Html.Styled as Html exposing (Attribute, Html, toUnstyled)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import List.Extra as List
import Random


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type GamePhase
    = Setup (Deck Card)
    | Playing GameModel
    | GameOver GameEndState


type GameEndState
    = PlayerWon
    | PlayerLost


type NightmareSubPhase
    = WaitingForSelection
    | DrawnFive (List Card)


type TurnPhase
    = PlayOrDiscard (Maybe { index : Int, card : Card })
    | Prophecy
    | FillHand
    | DrawnDoorCard { doorCard : Card, keyCard : Card }
    | DrawnDoorCardNoKey Card
    | DrawnNightmareCard
    | DrawnNightmareWaitingToDiscardKey
    | ShuffleLimboPile


turnPhaseToString : TurnPhase -> String
turnPhaseToString turnPhase =
    case turnPhase of
        PlayOrDiscard _ ->
            "Playing/Discarding"

        FillHand ->
            "Filling Hand"

        DrawnDoorCard _ ->
            "Drew a door card"

        DrawnDoorCardNoKey _ ->
            "Drew a door card but didn't have a key"

        DrawnNightmareCard ->
            "Drew a nightmare card"

        DrawnNightmareWaitingToDiscardKey ->
            "Need to discard a key"

        ShuffleLimboPile ->
            "Shuffling Limbo Pile"

        Prophecy ->
            "Triggered a Prophecy"


type alias GameModel =
    { deck : Deck Card
    , hand : List Card
    , limboPile : List Card
    , discardPile : List Card
    , labyrinth : List Card
    , unlockedDoors : List Card
    , turnPhase : TurnPhase
    }


type alias Model =
    { seed : Random.Seed
    , phase : GamePhase
    , modalContent : Maybe (Html Msg)
    }


init : Int -> ( Model, Cmd Msg )
init initialSeed =
    let
        ( initialDeck, seed ) =
            Deck.shuffle
                (Random.initialSeed initialSeed)
                (Deck.new Card.cards)
    in
    ( { seed = seed
      , phase = Setup initialDeck
      , modalContent = Nothing
      }
    , Cmd.none
    )


type SetupMsg
    = StartNewGame


type PlayingMsg
    = ClickCard { index : Int, card : Card }
    | PlayCard
    | DiscardCard
    | Draw
    | UseKeyOnDoorCard
    | DoNotUseKeyOnDoorCard
    | DrawnNightmareNeedToDiscardKey
    | DrawnNightmareNeedToPlaceDoorIntoLimbo
    | DrawnNightmareDiscardAKey { index : Int, keyCard : Card }
    | DrawnNightmarePlaceDoorIntoLimbo { index : Int, doorCard : Card }
    | DrawnNightmareRevealAndDiscardTopFive
    | DrawnNightmareDiscardEntireHand


type Msg
    = SetupMessage SetupMsg
    | PlayingMessage PlayingMsg


drawUntilOnlyLocation :
    { hand : List Card
    , toMixBackIn : List Card
    , deck : Deck Card
    }
    ->
        { hand : List Card
        , toMixBackIn : List Card
        , deck : Deck Card
        }
drawUntilOnlyLocation input =
    if List.length input.hand == 5 then
        input

    else
        let
            { cards, deck } =
                draw (5 - List.length input.hand) input.deck

            cardsToReplace =
                cards
                    |> List.filter
                        (\c ->
                            case c of
                                Location _ ->
                                    False

                                _ ->
                                    True
                        )

            cardsToKeep =
                cards
                    |> List.filter
                        (\c ->
                            case c of
                                Location _ ->
                                    True

                                _ ->
                                    False
                        )
        in
        drawUntilOnlyLocation
            { hand = cardsToKeep ++ input.hand
            , toMixBackIn = cardsToReplace ++ input.toMixBackIn
            , deck = deck
            }


updateSetupMessage : SetupMsg -> Model -> Deck Card -> ( Model, Cmd Msg )
updateSetupMessage msg model cardDeck =
    case msg of
        StartNewGame ->
            let
                { hand, toMixBackIn, deck } =
                    drawUntilOnlyLocation
                        { hand = []
                        , toMixBackIn = []
                        , deck = cardDeck
                        }

                ( newDeck, newSeed ) =
                    deck
                        |> Deck.add toMixBackIn
                        |> Deck.shuffle model.seed
            in
            ( { model
                | seed = newSeed
                , phase =
                    Playing
                        { hand = hand
                        , deck = newDeck
                        , limboPile = []
                        , discardPile = []
                        , labyrinth = []
                        , unlockedDoors = []
                        , turnPhase = PlayOrDiscard Nothing
                        }
              }
            , Cmd.none
            )


updatePlayingMessage : PlayingMsg -> Model -> GameModel -> ( Model, Cmd Msg )
updatePlayingMessage msg model gameModel =
    case msg of
        ClickCard d ->
            case gameModel.turnPhase of
                PlayOrDiscard Nothing ->
                    ( { model
                        | phase =
                            Playing
                                { gameModel | turnPhase = PlayOrDiscard (Just d) }
                        , modalContent =
                            d.card
                                |> playOrDiscardView
                                |> Html.map PlayingMessage
                                |> Just
                      }
                    , Cmd.none
                    )

                DrawnNightmareWaitingToDiscardKey ->
                    if Card.isKey d.card then
                        ( { model
                            | phase =
                                Playing
                                    { gameModel
                                        | turnPhase = FillHand
                                        , hand = List.removeAt d.index gameModel.hand
                                        , discardPile = d.card :: gameModel.discardPile
                                    }
                          }
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        PlayCard ->
            case gameModel.turnPhase of
                PlayOrDiscard (Just { index, card }) ->
                    let
                        handWithoutCard =
                            List.removeAt index gameModel.hand

                        { newLabyrinth, newHand, newPhase } =
                            gameModel.labyrinth
                                |> List.head
                                |> Maybe.map
                                    (\c ->
                                        case ( c, card ) of
                                            ( Location l1, Location l2 ) ->
                                                if l1.symbol /= l2.symbol then
                                                    { newLabyrinth = card :: gameModel.labyrinth
                                                    , newHand = handWithoutCard
                                                    , newPhase = FillHand
                                                    }

                                                else
                                                    { newLabyrinth = gameModel.labyrinth
                                                    , newHand = gameModel.hand
                                                    , newPhase = PlayOrDiscard Nothing
                                                    }

                                            _ ->
                                                { newLabyrinth = gameModel.labyrinth
                                                , newHand = gameModel.hand
                                                , newPhase = PlayOrDiscard Nothing
                                                }
                                    )
                                |> Maybe.withDefault
                                    { newLabyrinth = [ card ]
                                    , newHand = handWithoutCard
                                    , newPhase = FillHand
                                    }
                    in
                    ( { model
                        | phase =
                            Playing
                                { gameModel
                                    | labyrinth = newLabyrinth
                                    , hand = newHand
                                    , turnPhase = newPhase
                                }
                        , modalContent = Nothing
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        DiscardCard ->
            case gameModel.turnPhase of
                PlayOrDiscard (Just { index, card }) ->
                    ( { model
                        | phase =
                            Playing
                                { gameModel
                                    | hand = List.removeAt index gameModel.hand
                                    , discardPile = card :: gameModel.discardPile
                                    , turnPhase =
                                        case card of
                                            Location { symbol } ->
                                                case symbol of
                                                    Key ->
                                                        Prophecy

                                                    _ ->
                                                        FillHand

                                            _ ->
                                                FillHand
                                }
                        , modalContent = Nothing
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        Draw ->
            case gameModel.turnPhase of
                FillHand ->
                    if List.length gameModel.hand == 5 then
                        let
                            nextPhase =
                                if List.length gameModel.limboPile > 0 then
                                    ShuffleLimboPile

                                else
                                    PlayOrDiscard Nothing
                        in
                        ( { model | phase = Playing { gameModel | turnPhase = nextPhase } }
                        , Cmd.none
                        )

                    else
                        let
                            { cards, deck } =
                                Deck.draw 1 gameModel.deck

                            ( newModel, newCmd ) =
                                case List.head cards of
                                    -- If we try to draw a card and none
                                    -- are left, we've lost
                                    Nothing ->
                                        ( { model | phase = GameOver PlayerLost }, Cmd.none )

                                    -- Drawing a location card just goes
                                    -- into our hand
                                    Just ((Location _) as card) ->
                                        ( { model
                                            | phase =
                                                Playing
                                                    { gameModel
                                                        | deck = deck
                                                        , hand = card :: gameModel.hand
                                                    }
                                          }
                                        , Cmd.none
                                        )

                                    -- Drawing a door card means that
                                    -- if we have a key, we can (choice)
                                    -- use it to unlock the door,
                                    -- otherwise it goes into Limbo to be reshuffled
                                    Just ((Door { suit }) as card) ->
                                        let
                                            matchingKey =
                                                List.find
                                                    (\c ->
                                                        let
                                                            info =
                                                                Card.basicInformation c
                                                        in
                                                        info.symbol == Just Key && info.suit == Just suit
                                                    )
                                                    gameModel.hand
                                        in
                                        case matchingKey of
                                            Just keyCard ->
                                                ( { model
                                                    | phase =
                                                        Playing
                                                            { gameModel
                                                                | turnPhase = DrawnDoorCard { doorCard = card, keyCard = keyCard }
                                                                , deck = deck
                                                            }
                                                    , modalContent =
                                                        Just <|
                                                            Html.map PlayingMessage
                                                                (Html.div []
                                                                    [ Html.span [] [ Html.text "You drew a door card" ]
                                                                    , Card.view card
                                                                    , Html.span [] [ Html.text "Do you want to use a key?" ]
                                                                    , Html.div []
                                                                        [ Html.button [ onClick UseKeyOnDoorCard ] [ Html.text "Yes" ]
                                                                        , Html.button [ onClick DoNotUseKeyOnDoorCard ] [ Html.text "No" ]
                                                                        ]
                                                                    ]
                                                                )
                                                  }
                                                , Cmd.none
                                                )

                                            Nothing ->
                                                ( { model
                                                    | phase =
                                                        Playing
                                                            { gameModel
                                                                | turnPhase = DrawnDoorCardNoKey card
                                                                , limboPile = card :: gameModel.limboPile
                                                                , deck = deck
                                                            }
                                                  }
                                                , Cmd.none
                                                )

                                    -- Nightmare cards bring 4 options for the
                                    -- player:
                                    -- 1) Discard a key
                                    -- 2) Place a gained door into Limbo
                                    -- 3) Reveal the top 5 cards, and discard
                                    --    everything BUT door/nightmare cards
                                    --    and put those into Limbo
                                    -- 4) Discard the entire hand and
                                    --    redraw like at the beginning of the game
                                    Just Nightmare ->
                                        let
                                            hasKey =
                                                gameModel.hand
                                                    |> List.any (\c -> (Card.basicInformation c).symbol == Just Key)

                                            hasAnyDoors =
                                                not (List.isEmpty gameModel.unlockedDoors)

                                            choices =
                                                [ Html.button
                                                    [ Attributes.disabled (not hasKey)
                                                    , onClick DrawnNightmareNeedToDiscardKey
                                                    ]
                                                    [ Html.text
                                                        "Discard a key"
                                                    ]
                                                , Html.button [ Attributes.disabled (not hasAnyDoors) ] [ Html.text "Toss an unlocked door in Limbo" ]
                                                , Html.button [] [ Html.text "Discard the top five cards, throwing away Location cards" ]
                                                , Html.button [] [ Html.text "Discard your entire hand and redraw" ]
                                                ]
                                        in
                                        ( { model
                                            | phase =
                                                Playing
                                                    { gameModel
                                                        | turnPhase = DrawnNightmareCard
                                                        , deck = deck
                                                    }
                                            , modalContent =
                                                Just
                                                    (Html.map PlayingMessage
                                                        (Html.div []
                                                            [ Html.h2 [] [ Html.text "You drew a Nightmare card, you have a choice to make" ]
                                                            , Html.div [] (List.map identity choices)
                                                            ]
                                                        )
                                                    )
                                          }
                                        , Cmd.none
                                        )
                        in
                        ( newModel, newCmd )

                _ ->
                    ( model, Cmd.none )

        UseKeyOnDoorCard ->
            case gameModel.turnPhase of
                DrawnDoorCard { doorCard, keyCard } ->
                    ( { model
                        | phase =
                            Playing
                                { gameModel
                                    | turnPhase = FillHand
                                    , hand = List.remove keyCard gameModel.hand
                                    , discardPile = keyCard :: gameModel.discardPile
                                    , unlockedDoors = doorCard :: gameModel.unlockedDoors
                                }
                        , modalContent = Nothing
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        DoNotUseKeyOnDoorCard ->
            case gameModel.turnPhase of
                DrawnDoorCard { doorCard } ->
                    ( { model
                        | phase =
                            Playing
                                { gameModel
                                    | turnPhase = FillHand
                                    , limboPile = doorCard :: gameModel.limboPile
                                }
                        , modalContent = Nothing
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        DrawnNightmareNeedToDiscardKey ->
            ( { model
                | phase =
                    Playing
                        { gameModel
                            | turnPhase = DrawnNightmareWaitingToDiscardKey
                        }
                , modalContent = Nothing
              }
            , Cmd.none
            )

        DrawnNightmareNeedToPlaceDoorIntoLimbo ->
            -- TODO: Do this
            ( model, Cmd.none )

        DrawnNightmareDiscardAKey { index, keyCard } ->
            case gameModel.turnPhase of
                DrawnNightmareCard ->
                    ( { model
                        | phase =
                            Playing
                                { gameModel
                                    | turnPhase = FillHand
                                    , hand = List.removeAt index gameModel.hand
                                    , discardPile =
                                        Nightmare
                                            :: keyCard
                                            :: gameModel.discardPile
                                }
                        , modalContent = Nothing
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        DrawnNightmarePlaceDoorIntoLimbo { index, doorCard } ->
            case gameModel.turnPhase of
                DrawnNightmareCard ->
                    ( { model
                        | phase =
                            Playing
                                { gameModel
                                    | turnPhase = FillHand
                                    , limboPile = doorCard :: gameModel.limboPile
                                    , discardPile = Nightmare :: gameModel.discardPile
                                    , unlockedDoors = List.removeAt index gameModel.unlockedDoors
                                }
                        , modalContent = Nothing
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        DrawnNightmareRevealAndDiscardTopFive ->
            case gameModel.turnPhase of
                DrawnNightmareCard ->
                    ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DrawnNightmareDiscardEntireHand ->
            case gameModel.turnPhase of
                DrawnNightmareCard ->
                    ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.phase, msg ) of
        ( Setup cardDeck, SetupMessage sMsg ) ->
            updateSetupMessage sMsg model cardDeck

        ( Playing gameModel, PlayingMessage gMsg ) ->
            updatePlayingMessage gMsg model gameModel

        ( Setup _, _ ) ->
            ( model, Cmd.none )

        ( Playing _, _ ) ->
            ( model, Cmd.none )

        ( GameOver _, _ ) ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


playOrDiscardView : Card -> Html PlayingMsg
playOrDiscardView card =
    Html.div []
        [ Card.view card
        , Html.div []
            [ Html.button [ onClick PlayCard ] [ Html.text "Play" ]
            , Html.button [ onClick DiscardCard ] [ Html.text "Discard" ]
            ]
        ]


modal : Maybe (Html msg) -> Html msg
modal mContent =
    let
        outerDivStyles =
            [ position fixed
            , top zero
            , left zero
            , backgroundColor (Css.rgb 0 0 0)
            , displayFlex
            , alignItems center
            , justifyContent center
            , transition [ Css.Transitions.opacity 100 ]
            ]

        innerDivStyles =
            [ transition [ Css.Transitions.transform 200 ]
            , backgroundColor (Css.rgb 255 255 255)
            , padding (rem 2)
            , borderRadius (rem 1)
            , zIndex (int 1001)
            ]
    in
    case mContent of
        Just content ->
            Html.div
                [ css
                    ([ height (vh 100)
                     , width (vw 100)
                     , zIndex (int 1000)
                     , opacity (num 0.9)
                     ]
                        ++ outerDivStyles
                    )
                ]
                [ Html.div
                    [ css
                        (transform (translateY zero)
                            :: innerDivStyles
                        )
                    ]
                    [ content
                    ]
                ]

        Nothing ->
            Html.div
                [ css
                    ([ height (vh 0)
                     , width (vw 0)
                     , zIndex (int 0)
                     , opacity (num 0)
                     ]
                        ++ outerDivStyles
                    )
                ]
                [ Html.div
                    [ css
                        (transform (translateY (vh -100))
                            :: innerDivStyles
                        )
                    ]
                    []
                ]


overlappingCard : List (Attribute msg) -> Int -> Card -> Html msg
overlappingCard attrs idx card =
    Html.div
        (css
            [ height (px 200)
            , width (px 150)
            , marginLeft
                (if idx == 0 then
                    rem 0

                 else
                    rem -3
                )
            , zIndex (int 1)
            , transition [ Css.Transitions.transform 300 ]
            ]
            :: attrs
        )
        [ Card.view card ]


setupView : Model -> Html SetupMsg
setupView _ =
    Html.div
        [ css
            [ displayFlex
            , alignItems center
            , justifyContent center
            , height (pct 100)
            ]
        ]
        [ Html.div
            [ css
                [ displayFlex
                , flexDirection column
                , alignItems center
                ]
            ]
            [ Html.div [ css [ fontSize (rem 4) ] ]
                [ Html.text "Onirim" ]
            , Html.div []
                [ Html.button [ onClick StartNewGame ]
                    [ Html.text "Start a new game" ]
                ]
            ]
        ]


playingView : GameModel -> Html PlayingMsg
playingView model =
    Html.div
        [ css
            [ displayFlex
            , flexDirection column
            , alignItems center
            , justifyContent spaceBetween
            , height (pct 100)
            , width (pct 100)
            ]
        ]
        [ Html.div
            [ css
                [ position absolute
                , top zero
                , left zero
                , padding (rem 1)
                ]
            ]
            [ Html.text ("Phase: " ++ turnPhaseToString model.turnPhase) ]
        , Html.div
            [ css
                [ height (px 200)
                , width (px 150)
                , cursor pointer
                ]
            , onClick Draw
            ]
            [ cardBack ]
        , Html.div
            [ css
                [ border2 (px 2) dashed
                , height (px 200)
                , width (pct 100)
                , padding (rem 1)
                , displayFlex
                ]
            ]
            (List.indexedMap (overlappingCard []) (List.reverse model.labyrinth))
        , Html.div
            [ css [ displayFlex, height (px 200) ] ]
            (List.indexedMap
                (\i c ->
                    overlappingCard
                        [ css
                            (case ( model.turnPhase, (Card.basicInformation c).symbol ) of
                                ( DrawnNightmareWaitingToDiscardKey, Just Key ) ->
                                    [ hover [ zIndex (int 2) ]
                                    , transforms
                                        [ translateY (pct -20)
                                        , rotateZ (deg 10)
                                        ]
                                    ]

                                _ ->
                                    [ hover
                                        [ zIndex (int 2)
                                        , transforms
                                            [ translateY (pct -20)
                                            , rotateZ (deg 10)
                                            ]
                                        ]
                                    ]
                            )
                        , onClick (ClickCard { index = i, card = c })
                        ]
                        i
                        c
                )
                model.hand
            )
        ]


view : Model -> H.Html Msg
view model =
    toUnstyled <|
        Html.div
            [ css
                [ height (vh 100)
                , width (vw 100)
                , padding (rem 3)
                , boxSizing borderBox
                ]
            ]
            [ Css.Global.global
                [ Css.Global.selector "body" [ margin zero ]
                , Css.Global.everything [ fontFamily sansSerif ]
                ]
            , case model.phase of
                Setup _ ->
                    Html.map SetupMessage <| setupView model

                Playing gameModel ->
                    Html.map PlayingMessage <| playingView gameModel

                GameOver PlayerLost ->
                    Html.div [] [ Html.text "Game Over - You Lost" ]

                GameOver PlayerWon ->
                    Html.div [] [ Html.text "Game Over - You Won!" ]
            , modal model.modalContent
            ]
