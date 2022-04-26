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
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import List.Extra as List
import Process
import Random
import Task


main : Program () Model Msg
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


type TurnPhase
    = PlayOrDiscard (Maybe { index : Int, card : Card })
    | Prophecy
    | FillHand
    | DrawnDoorCard { doorCard : Card, keyCard : Card }
    | DrawnDoorCardNoKey
    | DrawnNightmareCard
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

        DrawnDoorCardNoKey ->
            "Drew a door card but didn't have a key"

        DrawnNightmareCard ->
            "Drew a nightmare card"

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


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( initialDeck, seed ) =
            Deck.shuffle (Random.initialSeed 0) (Deck.new Card.cards)
    in
    ( { seed = seed
      , phase = Setup initialDeck
      , modalContent = Nothing
      }
    , Cmd.none
    )


type Msg
    = StartNewGame
    | WaitingForPlayerCardDecision { index : Int, card : Card }
    | PlayCard
    | DiscardCard
    | Draw
    | RevertToFillHand
    | UseKeyOnDoorCard
    | DoNotUseKeyOnDoorCard


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartNewGame ->
            case model.phase of
                Setup cardDeck ->
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

                _ ->
                    ( model, Cmd.none )

        WaitingForPlayerCardDecision d ->
            case model.phase of
                Playing gameModel ->
                    case gameModel.turnPhase of
                        PlayOrDiscard Nothing ->
                            ( { model
                                | phase =
                                    Playing
                                        { gameModel | turnPhase = PlayOrDiscard (Just d) }
                                , modalContent =
                                    Just (playOrDiscardView d.card)
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DiscardCard ->
            case model.phase of
                Playing gameModel ->
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

                _ ->
                    ( model, Cmd.none )

        PlayCard ->
            case model.phase of
                Playing gameModel ->
                    case gameModel.turnPhase of
                        PlayOrDiscard (Just { index, card }) ->
                            let
                                handWithoutCard =
                                    List.removeAt index gameModel.hand

                                { newLabyrinth, newHand } =
                                    gameModel.labyrinth
                                        |> List.head
                                        |> Maybe.map
                                            (\c ->
                                                case ( c, card ) of
                                                    ( Location l1, Location l2 ) ->
                                                        if l1.symbol /= l2.symbol then
                                                            { newLabyrinth = card :: gameModel.labyrinth
                                                            , newHand = handWithoutCard
                                                            }

                                                        else
                                                            { newLabyrinth = gameModel.labyrinth
                                                            , newHand = gameModel.hand
                                                            }

                                                    _ ->
                                                        { newLabyrinth = gameModel.labyrinth
                                                        , newHand = gameModel.hand
                                                        }
                                            )
                                        |> Maybe.withDefault
                                            { newLabyrinth = [ card ]
                                            , newHand = handWithoutCard
                                            }
                            in
                            ( { model
                                | phase =
                                    Playing
                                        { gameModel
                                            | labyrinth = newLabyrinth
                                            , hand = newHand
                                            , turnPhase = FillHand
                                        }
                                , modalContent = Nothing
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Draw ->
            case model.phase of
                Playing gameModel ->
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
                                        case cards of
                                            -- If we try to draw a card and none
                                            -- are left, we've lost
                                            [] ->
                                                ( { model | phase = GameOver PlayerLost }, Cmd.none )

                                            -- Drawing a location card just goes
                                            -- into our hand
                                            [ (Location _) as card ] ->
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
                                            [ (Door { suit }) as card ] ->
                                                case
                                                    List.find
                                                        (\c ->
                                                            (Card.basicInformation c).symbol
                                                                == Just Key
                                                                && (Card.basicInformation c).suit
                                                                == Just suit
                                                        )
                                                        gameModel.hand
                                                of
                                                    Just keyCard ->
                                                        ( { model
                                                            | phase =
                                                                Playing
                                                                    { gameModel
                                                                        | turnPhase = DrawnDoorCard { doorCard = card, keyCard = keyCard }
                                                                        , deck = deck
                                                                    }
                                                            , modalContent =
                                                                Just
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
                                                                        | turnPhase = DrawnDoorCardNoKey
                                                                        , limboPile = card :: gameModel.limboPile
                                                                        , deck = deck
                                                                    }
                                                          }
                                                        , Task.perform (always RevertToFillHand) (Process.sleep 2000)
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
                                            [ Nightmare ] ->
                                                ( { model
                                                    | phase =
                                                        Playing
                                                            { gameModel
                                                                | turnPhase = DrawnNightmareCard
                                                                , deck = deck
                                                            }
                                                  }
                                                , Cmd.none
                                                )

                                            _ ->
                                                ( model, Cmd.none )

                                    -- TODO: Need to draw one card and resolve
                                in
                                ( newModel, newCmd )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        RevertToFillHand ->
            case model.phase of
                Playing gameModel ->
                    ( { model | phase = Playing { gameModel | turnPhase = FillHand } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UseKeyOnDoorCard ->
            case model.phase of
                Playing gameModel ->
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

                _ ->
                    ( model, Cmd.none )

        DoNotUseKeyOnDoorCard ->
            case model.phase of
                Playing gameModel ->
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

                _ ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


playOrDiscardView : Card -> Html Msg
playOrDiscardView card =
    Html.div []
        [ Card.view card
        , Html.div []
            [ Html.button [ onClick PlayCard ] [ Html.text "Play" ]
            , Html.button [ onClick DiscardCard ] [ Html.text "Discard" ]
            ]
        ]


modal : Maybe (Html Msg) -> Html Msg
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


overlappingCard : List (Attribute Msg) -> Int -> Card -> Html Msg
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


setupView : Model -> Html Msg
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


playingView : GameModel -> Html Msg
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
                            [ hover
                                [ zIndex (int 2)
                                , transforms
                                    [ translateY (pct -20)
                                    , rotateZ (deg 10)
                                    ]
                                ]
                            ]
                        , onClick (WaitingForPlayerCardDecision { index = i, card = c })
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
                    setupView model

                Playing gameModel ->
                    playingView gameModel

                GameOver _ ->
                    Debug.todo "branch 'GameOver' not implemented"
            , modal model.modalContent
            ]
