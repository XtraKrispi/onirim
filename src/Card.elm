module Card exposing (..)

import Css
    exposing
        ( alignItems
        , backgroundColor
        , border
        , borderBox
        , borderColor
        , borderRadius
        , borderStyle
        , boxShadow5
        , boxSizing
        , center
        , color
        , displayFlex
        , height
        , justifyContent
        , padding
        , pct
        , px
        , rem
        , solid
        , width
        , zero
        )
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Svg.Styled as Svg exposing (Svg)
import Svg.Styled.Attributes as SAttr


type Symbol
    = Key
    | Sun
    | Moon


type Suit
    = Observatory
    | Aquarium
    | Garden
    | Library


type Card
    = Door { suit : Suit }
    | Location { suit : Suit, symbol : Symbol }
    | Nightmare


basicInformation : Card -> { suit : Maybe Suit, symbol : Maybe Symbol }
basicInformation card =
    case card of
        Door { suit } ->
            { suit = Just suit, symbol = Nothing }

        Location { suit, symbol } ->
            { suit = Just suit, symbol = Just symbol }

        Nightmare ->
            { suit = Nothing, symbol = Nothing }


cards : List Card
cards =
    List.repeat 9 (Location { suit = Observatory, symbol = Key })
        ++ List.repeat 4 (Location { suit = Observatory, symbol = Moon })
        ++ List.repeat 3 (Location { suit = Observatory, symbol = Sun })
        ++ List.repeat 8 (Location { suit = Aquarium, symbol = Key })
        ++ List.repeat 4 (Location { suit = Aquarium, symbol = Moon })
        ++ List.repeat 3 (Location { suit = Aquarium, symbol = Sun })
        ++ List.repeat 7 (Location { suit = Garden, symbol = Key })
        ++ List.repeat 4 (Location { suit = Garden, symbol = Moon })
        ++ List.repeat 3 (Location { suit = Garden, symbol = Sun })
        ++ List.repeat 6 (Location { suit = Library, symbol = Key })
        ++ List.repeat 4 (Location { suit = Library, symbol = Moon })
        ++ List.repeat 3 (Location { suit = Library, symbol = Sun })
        ++ List.repeat 2 (Door { suit = Observatory })
        ++ List.repeat 2 (Door { suit = Aquarium })
        ++ List.repeat 2 (Door { suit = Garden })
        ++ List.repeat 2 (Door { suit = Library })
        ++ List.repeat 10 Nightmare


suitColor suit =
    case suit of
        Observatory ->
            Css.rgb 255 0 0

        Aquarium ->
            Css.rgb 0 0 255

        Garden ->
            Css.rgb 0 255 0

        Library ->
            Css.rgb 210 180 140


cardBack : Html msg
cardBack =
    Html.div
        [ css
            [ width (pct 100)
            , height (pct 100)
            , backgroundColor (Css.rgb 255 255 255)
            , border (px 2)
            , borderColor (Css.rgb 0 0 0)
            , borderStyle solid
            , borderRadius (rem 0.5)
            , displayFlex
            , alignItems center
            , justifyContent center
            ]
        ]
        [ Html.text "Back" ]


isKey : Card -> Bool
isKey card =
    (basicInformation card).symbol == Just Key


view : Card -> Html msg
view c =
    case c of
        Door { suit } ->
            Html.div
                [ css
                    [ width (pct 100)
                    , height (pct 100)
                    , backgroundColor (suitColor suit)
                    , border (px 2)
                    , borderColor (Css.rgb 0 0 0)
                    , borderStyle solid
                    , borderRadius (rem 0.5)
                    , padding (rem 1)
                    , boxSizing borderBox
                    ]
                ]
                [ Html.text "Door" ]

        Location { suit, symbol } ->
            Html.div
                [ css
                    [ width (pct 100)
                    , height (pct 100)
                    , backgroundColor (suitColor suit)
                    , border (px 2)
                    , borderColor (Css.rgb 0 0 0)
                    , borderStyle solid
                    , borderRadius (rem 0.5)
                    , padding (rem 1)
                    , boxSizing borderBox
                    ]
                ]
                [ Html.div
                    [ css
                        [ width (rem 2)
                        , height (rem 2)
                        , displayFlex
                        , padding (rem 0.5)
                        , backgroundColor (Css.rgb 255 255 255)
                        , borderRadius (pct 50)
                        , boxShadow5 zero zero (px 5) (px 10) (Css.rgb 255 255 255)
                        ]
                    ]
                    [ case symbol of
                        Key ->
                            key

                        Moon ->
                            moon

                        Sun ->
                            sun
                    ]
                ]

        Nightmare ->
            Html.div
                [ css
                    [ width (pct 100)
                    , height (pct 100)
                    , backgroundColor (Css.rgb 0 0 0)
                    , border (px 2)
                    , borderColor (Css.rgb 255 255 255)
                    , borderStyle solid
                    , borderRadius (rem 0.5)
                    , padding (rem 1)
                    , boxSizing borderBox
                    , color (Css.rgb 255 255 255)
                    ]
                ]
                [ Html.text "Nightmare" ]


key : Svg msg
key =
    Svg.svg
        [ SAttr.version "1.1"
        , SAttr.viewBox "0 0 86.707 41.89"
        ]
        [ Svg.g [ SAttr.transform "translate(-41.458 -53.394)" ]
            [ Svg.g [ SAttr.transform "translate(-29.307 8.5776)" ]
                [ Svg.ellipse
                    [ SAttr.cx "136.71"
                    , SAttr.cy "65.762"
                    , SAttr.rx "17.334"
                    , SAttr.ry "17.513"
                    , SAttr.fill "none"
                    , SAttr.opacity ".98"
                    , SAttr.stroke "#000"
                    , SAttr.strokeWidth "6.865"
                    ]
                    []
                , Svg.g [ SAttr.fillRule "evenodd" ]
                    [ Svg.rect
                        [ SAttr.x "70.765"
                        , SAttr.y "60.043"
                        , SAttr.width "50.751"
                        , SAttr.height "7.5054"
                        , SAttr.strokeWidth ".35001"
                        ]
                        []
                    , Svg.rect
                        [ SAttr.x "70.765"
                        , SAttr.y "67.549"
                        , SAttr.width "6.7906"
                        , SAttr.height "10.722"
                        , SAttr.strokeWidth ".26458"
                        ]
                        []
                    , Svg.rect
                        [ SAttr.x "86.848"
                        , SAttr.y "67.191"
                        , SAttr.width "6.0758"
                        , SAttr.height "8.5776"
                        , SAttr.strokeWidth ".22229"
                        ]
                        []
                    ]
                ]
            ]
        ]


moon : Svg msg
moon =
    Svg.svg
        [ SAttr.version "1.1"
        , SAttr.viewBox "0 0 45.324 45.764"
        ]
        [ Svg.g [ SAttr.transform "translate(-77.285 -61.916)" ]
            [ Svg.path
                [ SAttr.d "m115.74 67.619a23.467 23.467 0 0 0-9.3205-5.7036 23.467 23.467 0 0 1-5.282 25.085 23.467 23.467 0 0 1-23.849 5.7099 23.467 23.467 0 0 0 5.2637 8.0963 23.467 23.467 0 0 0 33.187-3.6e-4 23.467 23.467 0 0 0 1e-5 -33.187z"
                , SAttr.opacity ".98"
                , SAttr.strokeWidth "0"
                ]
                []
            ]
        ]


sun : Svg msg
sun =
    Svg.svg
        [ SAttr.version "1.1"
        , SAttr.viewBox "0 0 45.324 45.764"
        ]
        [ Svg.g [ SAttr.transform "translate(-77.285 -61.916)" ]
            [ Svg.g [ SAttr.strokeWidth "0" ]
                [ Svg.path
                    [ SAttr.d "m99.967 71.678a12.391 12.954 0 0 0-12.391 12.954 12.391 12.954 0 0 0 12.391 12.954 12.391 12.954 0 0 0 12.39-12.954 12.391 12.954 0 0 0-12.39-12.954zm-0.04909 4.4204a8.2115 8.5847 0 0 1 8.2114 8.585 8.2115 8.5847 0 0 1-8.2114 8.5845 8.2115 8.5847 0 0 1-8.2114-8.5845 8.2115 8.5847 0 0 1 8.2114-8.585z"
                    ]
                    []
                , Svg.path
                    [ SAttr.d "m95.734 76.528a5.8151 7.4135 0 0 0 1.152-0.15009 8.6169 5.4029 66.86 0 1-4.4503-6.5474 8.6169 5.4029 66.86 0 1 0.63195-7.2998 5.8151 7.4135 0 0 0-3.1491 6.5835 5.8151 7.4135 0 0 0 5.8154 7.4138z"
                    ]
                    []
                , Svg.path
                    [ SAttr.d "m102.76 76.58a5.8151 7.4135 20 0 0 1.1339 0.25297 8.6169 5.4029 86.86 0 1-1.9426-7.6746 8.6169 5.4029 86.86 0 1 3.0905-6.6434 5.8151 7.4135 20 0 0-5.2109 5.1094 5.8151 7.4135 20 0 0 2.929 8.9557z"
                    ]
                    []
                , Svg.path
                    [ SAttr.d "m107.68 76.877a5.5387 7.2016 26.555 0 0 0.59187 1.0183 4.8413 8.8975 22.95 0 1 2.8881-7.8667 4.8413 8.8975 22.95 0 1 5.5647-3.4242 5.5387 7.2016 26.555 0 0-6.0998 0.62307 5.5387 7.2016 26.555 0 0-2.945 9.6496z"
                    ]
                    []
                , Svg.path
                    [ SAttr.d "m110.32 83.869a5.7976 6.9235 75.978 0 0 0.32388 1.1084 5.2751 8.2174 52.322 0 1 5.2841-5.5196 5.2751 8.2174 52.322 0 1 6.7957-0.64524 5.7976 6.9235 75.978 0 0-6.5458-1.958 5.7976 6.9235 75.978 0 0-5.8578 7.0145z"
                    ]
                    []
                , Svg.path
                    [ SAttr.d "m108.06 88.729a6.9235 5.7976 25.978 0 0-0.46442 1.0573 8.2174 5.2751 2.3223 0 1 7.5958-0.83176 8.2174 5.2751 2.3223 0 1 5.6205 3.8739 6.9235 5.7976 25.978 0 0-3.7558-5.7075 6.9235 5.7976 25.978 0 0-8.9961 1.6081z"
                    ]
                    []
                , Svg.path
                    [ SAttr.d "m104.01 93.128a6.9235 5.7976 55.978 0 0-0.93085 0.68343 8.2174 5.2751 32.322 0 1 6.994 3.0776 8.2174 5.2751 32.322 0 1 2.9306 6.1651 6.9235 5.7976 55.978 0 0-0.39885-6.8208 6.9235 5.7976 55.978 0 0-8.5949-3.1054z"
                    ]
                    []
                , Svg.path
                    [ SAttr.d "m97.453 94.148a6.9235 5.7976 85.978 0 0-1.1479 0.12644 8.2174 5.2751 62.322 0 1 4.5182 6.1623 8.2174 5.2751 62.322 0 1-0.54462 6.8045 6.9235 5.7976 85.978 0 0 3.065-6.1064 6.9235 5.7976 85.978 0 0-5.8907-6.9868z"
                    ]
                    []
                , Svg.path
                    [ SAttr.d "m92.915 92.337a5.7976 6.9235 35.978 0 0-0.96059-0.64097 5.2751 8.2174 12.322 0 1-0.49986 7.6248 5.2751 8.2174 12.322 0 1-4.791 4.8624 5.7976 6.9235 35.978 0 0 6.273-2.7076 5.7976 6.9235 35.978 0 0-0.02151-9.1387z"
                    ]
                    []
                , Svg.path
                    [ SAttr.d "m90.549 87.721a5.7976 6.9235 85.978 0 0-0.12645-1.1479 5.2751 8.2174 62.322 0 1-6.1623 4.5182 5.2751 8.2174 62.322 0 1-6.8045-0.54462 5.7976 6.9235 85.978 0 0 6.1064 3.065 5.7976 6.9235 85.978 0 0 6.9868-5.8907z"
                    ]
                    []
                , Svg.path
                    [ SAttr.d "m91.444 79.979a7.2275 6.0089 36.892 0 0 0.8541-0.84382 8.7304 5.3721 19.796 0 1-7.9437-1.8362 8.7304 5.3721 19.796 0 1-4.2346-5.6234 7.2275 6.0089 36.892 0 0 1.688 6.7206 7.2275 6.0089 36.892 0 0 9.6362 1.5829z"
                    ]
                    []
                ]
            ]
        ]
