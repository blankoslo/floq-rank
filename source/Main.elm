module Main exposing (..)

import Date exposing (Date)
import Date.Format as Date
import Dict exposing (Dict)
import Html exposing (div, span, strong, text)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List
import Maybe.Extra as Maybe
import Random.Pcg exposing (Seed, step)
import Task exposing (Task)
import Ui
import Ui.App
import Ui.Button
import Ui.Checkbox
import Ui.Chooser
import Ui.Container
import Uuid exposing (Uuid)


type alias User =
    { id : String
    , name : String
    , profiles : Dict String Profile
    }


type alias Profile =
    { nMatches : Int
    , nMatchesWon : Int
    , nMatchesLost : Int
    , rating : Float
    }


type Result
    = Win
    | Loss
    | Tie


type alias Matchup =
    { gameId : Uuid
    , user1Id : String
    , user2Id : String
    , result : Result
    , createdAt : Date
    }


type alias Game =
    { id : Uuid
    , title : String
    }


type alias Model =
    { app : Ui.App.Model
    , gameChooser : Ui.Chooser.Model
    , user1Chooser : Ui.Chooser.Model
    , user2Chooser : Ui.Chooser.Model
    , user1Checkbox : Ui.Checkbox.Model
    , tieCheckbox : Ui.Checkbox.Model
    , user2Checkbox : Ui.Checkbox.Model
    , users : Dict String User
    , games : Dict String Game
    , matchups : List Matchup
    , apiToken : String
    , apiHost : String
    }


type Msg
    = NoOp
    | Init (Result.Result Http.Error ( List FloqEmployee, List Game, List Matchup ))
    | App Ui.App.Msg
    | GameChooser Ui.Chooser.Msg
    | User1Chooser Ui.Chooser.Msg
    | User2Chooser Ui.Chooser.Msg
    | User1Checkbox Ui.Checkbox.Msg
    | TieCheckbox Ui.Checkbox.Msg
    | User2Checkbox Ui.Checkbox.Msg
    | SubmitMatchup (Date -> Matchup)
    | AddMatchup Matchup


mkUser : List Game -> String -> String -> ( String, User )
mkUser games name id =
    let
        mkProfile gameId =
            ( Uuid.toString gameId, Profile 0 0 0 1000 )

        profiles =
            Dict.fromList <| (List.map mkProfile (List.map .id games))
    in
        ( id, User id name profiles )


mkGame : String -> Uuid -> ( String, Game )
mkGame title uuid =
    ( Uuid.toString uuid, Game uuid title )


type Rating
    = Rating Float


type Expected
    = Expected Float



-- Formulas taken from https://no.wikipedia.org/wiki/Elo-rating


expected : ( Rating, Rating ) -> ( Expected, Expected )
expected ( Rating ra, Rating rb ) =
    let
        ea =
            1 / (1 + 10 ^ ((rb - ra) / 400))

        eb =
            1 / (1 + 10 ^ ((ra - rb) / 400))
    in
        ( Expected ea, Expected eb )


kFactor : Float
kFactor =
    32


newRating : Rating -> Expected -> Expected -> Rating
newRating (Rating ra) (Expected ea) (Expected result) =
    let
        newRa =
            ra + kFactor * (result - ea)
    in
        Rating newRa


generateUuids : Seed -> Int -> List Uuid -> List Uuid
generateUuids seed n uuids =
    let
        ( newUuid, newSeed ) =
            step Uuid.uuidGenerator seed
    in
        generateUuids newSeed (n - 1) (newUuid :: uuids)



-- defaultGames : Dict String Game
-- defaultGames = Dict.fromList <| List.filterMap identity
--                  [ Maybe.map (mkGame "FIFA 2016")
--                      (Uuid.fromString "133d20d1-11c9-4693-b4af-7496ddb30e0e")
--                  , Maybe.map (mkGame "Sjakk")
--                      (Uuid.fromString "233d20d1-11c9-4693-b4af-7496ddb30e0e")
--                  ]
-- defaultUsers : Dict String User
-- defaultUsers = Dict.fromList
--                  [ mkUser "Arne" "50a34028-528b-4a70-8231-54103d940ee3"
--                  , mkUser "Bob" "2db45eb9-d55d-4f03-b1a9-a25699abe169"
--                  , mkUser "Cato" "f5b46616-3a0b-4520-9cf5-a6389fbfdb0b"
--                  , mkUser "David" "6f39c213-e19e-4d2a-884d-9a68f959b6f2"
--                  , mkUser "Espen" "41d42d9f-c47c-448c-a7ac-846bc6b00cd6"
--                  ]


type alias FloqEmployee =
    { id : Int
    , firstName : String
    , lastName : String
    }


employeesDecoder : Decode.Decoder (List FloqEmployee)
employeesDecoder =
    let
        user =
            Decode.map3 FloqEmployee
                (Decode.field "id" Decode.int)
                (Decode.field "first_name" Decode.string)
                (Decode.field "last_name" Decode.string)
    in
        Decode.list user


fetchEmployees : String -> String -> Http.Request (List FloqEmployee)
fetchEmployees apiToken apiHost =
    let
        authHeader =
            Http.header "Authorization" ("Bearer " ++ apiToken)
    in
        Http.request
            { method = "GET"
            , headers = [ authHeader ]
            , url = (apiHost ++ "/employees")
            , body = Http.emptyBody
            , expect = Http.expectJson employeesDecoder
            , timeout = Nothing
            , withCredentials = False
            }


gamesDecoder : Decode.Decoder (List Game)
gamesDecoder =
    let
        gameOrNothing a b =
            Maybe.map (\x -> Game x b) a

        game =
            Decode.map2 gameOrNothing
                (Decode.map Uuid.fromString (Decode.field "id" Decode.string))
                (Decode.field "title" Decode.string)
    in
        Decode.map (List.filterMap identity) (Decode.list game)


fetchGames : String -> String -> Http.Request (List Game)
fetchGames apiToken apiHost =
    let
        authHeader =
            Http.header "Authorization" ("Bearer " ++ apiToken)
    in
        Http.request
            { method = "GET"
            , headers = [ authHeader ]
            , url = (apiHost ++ "/ranked_games")
            , body = Http.emptyBody
            , expect = Http.expectJson gamesDecoder
            , timeout = Nothing
            , withCredentials = False
            }


matchupDecoder : Decode.Decoder (Maybe Matchup)
matchupDecoder =
    let
        matchupOrNothing a b c d e =
            Maybe.map2 (\ma me -> Matchup ma b c d me) a e

        toResult x =
            case x of
                "WIN" ->
                    Win

                "LOSS" ->
                    Loss

                _ ->
                    Tie
    in
        Decode.map5 matchupOrNothing
            (Decode.map Uuid.fromString (Decode.field "game_id" Decode.string))
            (Decode.field "user1_id" Decode.int |> Decode.map toString)
            (Decode.field "user2_id" Decode.int |> Decode.map toString)
            (Decode.map toResult (Decode.field "matchup_result" Decode.string))
            (Decode.map (Date.fromString >> Result.toMaybe) (Decode.field "created_at" Decode.string))


matchupsDecoder : Decode.Decoder (List Matchup)
matchupsDecoder =
    Decode.map (List.filterMap identity) (Decode.list matchupDecoder)


fetchMatchups : String -> String -> Http.Request (List Matchup)
fetchMatchups apiToken apiHost =
    let
        authHeader =
            Http.header "Authorization" ("Bearer " ++ apiToken)
    in
        Http.request
            { method = "GET"
            , headers = [ authHeader ]
            , url = (apiHost ++ "/ranked_matchups")
            , body = Http.emptyBody
            , expect = Http.expectJson matchupsDecoder
            , timeout = Nothing
            , withCredentials = False
            }


fetchAll : String -> String -> Task Http.Error ( List FloqEmployee, List Game, List Matchup )
fetchAll t h =
    Task.map3 (,,)
        (Http.toTask <| fetchEmployees t h)
        (Http.toTask <| fetchGames t h)
        (Http.toTask <| fetchMatchups t h)


matchupEncoder : Matchup -> Encode.Value
matchupEncoder matchup =
    let
        toResultString x =
            case x of
                Win ->
                    "WIN"

                Loss ->
                    "LOSS"

                Tie ->
                    "TIE"
    in
        Encode.object
            [ ( "game_id", Encode.string <| Uuid.toString matchup.gameId )
            , ( "user1_id", Encode.int <| Maybe.withDefault 0 <| Result.toMaybe <| String.toInt matchup.user1Id )
            , ( "user2_id", Encode.int <| Maybe.withDefault 0 <| Result.toMaybe <| String.toInt matchup.user2Id )
            , ( "matchup_result", Encode.string <| toResultString matchup.result )
            ]


postMatchup : String -> String -> Matchup -> Http.Request (Maybe Matchup)
postMatchup apiToken apiHost matchup =
    let
        authHeader =
            Http.header "Authorization" ("Bearer " ++ apiToken)

        preferHeader =
            Http.header "Prefer" "return=representation"
    in
        Http.request
            { method = "POST"
            , headers = [ authHeader, preferHeader ]
            , url = (apiHost ++ "/ranked_matchups")
            , body = Http.jsonBody (matchupEncoder matchup)
            , expect = Http.expectJson matchupDecoder
            , timeout = Nothing
            , withCredentials = False
            }


type alias Flags =
    { apiToken : String
    , apiHost : String
    }


init : Flags -> Model
init flags =
    { app = Ui.App.init
    , gameChooser =
        Ui.Chooser.init [] "" ""
            |> \x -> { x | closeOnSelect = True, searchable = True }
    , user1Chooser =
        Ui.Chooser.init [] "" ""
            |> \x -> { x | closeOnSelect = True, searchable = True }
    , user2Chooser =
        Ui.Chooser.init [] "" ""
            |> \x -> { x | closeOnSelect = True, searchable = True }
    , user1Checkbox =
        Ui.Checkbox.init True
            |> \x -> { x | readonly = True }
    , tieCheckbox = Ui.Checkbox.init False
    , user2Checkbox = Ui.Checkbox.init False
    , users = Dict.empty
    , games = Dict.empty
    , matchups = []
    , apiToken = flags.apiToken
    , apiHost = flags.apiHost
    }


userToItem : User -> Ui.Chooser.Item
userToItem user =
    Ui.Chooser.Item user.name user.id


gameToItem : Game -> Ui.Chooser.Item
gameToItem game =
    Ui.Chooser.Item game.title (Uuid.toString game.id)


addMatchup : Matchup -> Model -> Model
addMatchup matchup model =
    let
        updateProfile result newRating =
            Maybe.map
                (\x ->
                    { x
                        | nMatches = x.nMatches + 1
                        , nMatchesWon =
                            if result == Win then
                                x.nMatchesWon + 1
                            else
                                x.nMatchesWon
                        , nMatchesLost =
                            if result == Loss then
                                x.nMatchesLost + 1
                            else
                                x.nMatchesLost
                        , rating = newRating
                    }
                )

        updateUser result (Rating newRating) =
            Maybe.map (\x -> { x | profiles = Dict.update (Uuid.toString matchup.gameId) (updateProfile result newRating) x.profiles })

        getOppositeResult x =
            case x of
                Win ->
                    Loss

                Loss ->
                    Win

                _ ->
                    Tie

        getExpected x =
            case x of
                Win ->
                    Expected 1

                Loss ->
                    Expected 0

                Tie ->
                    Expected 0.5

        oldU1Rating =
            Dict.get matchup.user1Id model.users
                |> Maybe.andThen (.profiles >> Dict.get (Uuid.toString matchup.gameId))
                |> Maybe.map (.rating)
                |> Maybe.withDefault 1000

        oldU2Rating =
            Dict.get matchup.user2Id model.users
                |> Maybe.andThen (.profiles >> Dict.get (Uuid.toString matchup.gameId))
                |> Maybe.map (.rating)
                |> Maybe.withDefault 1000

        ( u1Result, u2Result ) =
            ( matchup.result, getOppositeResult (matchup.result) )

        ( ra, rb ) =
            ( Rating oldU1Rating, Rating oldU2Rating )

        ( ea, eb ) =
            expected ( ra, rb )

        ( u1Rating, u2Rating ) =
            ( newRating ra ea (getExpected u1Result), newRating rb eb (getExpected u2Result) )
    in
        { model
            | matchups = (matchup :: model.matchups)
            , users =
                Dict.update matchup.user1Id (updateUser u1Result u1Rating) model.users
                    |> Dict.update matchup.user2Id (updateUser u2Result u2Rating)
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Init initialData ->
            let
                employeeToUser games employee =
                    mkUser games (employee.firstName ++ " " ++ employee.lastName) (toString employee.id)

                ( users, games, matchups ) =
                    case initialData of
                        Ok ( us, gs, ms ) ->
                            let
                                games =
                                    List.map (\x -> ( Uuid.toString x.id, x )) gs |> Dict.fromList

                                users =
                                    List.map (employeeToUser (Dict.values games)) us |> Dict.fromList
                            in
                                ( users, games, ms )

                        Err _ ->
                            ( Dict.empty, Dict.empty, [] )

                initialModel =
                    { model
                        | users = users
                        , games = games
                        , gameChooser =
                            Ui.Chooser.updateData (List.map gameToItem <| Dict.values games) model.gameChooser
                                |> Ui.Chooser.selectFirst
                                |> Tuple.first
                        , user1Chooser = Ui.Chooser.updateData (List.map userToItem <| Dict.values users) model.user1Chooser
                        , user2Chooser = Ui.Chooser.updateData (List.map userToItem <| Dict.values users) model.user2Chooser
                    }
            in
                ( List.foldl addMatchup initialModel matchups
                , Cmd.none
                )

        App act ->
            let
                ( app, effect ) =
                    Ui.App.update act model.app
            in
                ( { model | app = app }, Cmd.map App effect )

        GameChooser msg ->
            ( { model
                | gameChooser = Ui.Chooser.update msg model.gameChooser |> Tuple.first
              }
            , Cmd.none
            )

        User1Chooser msg ->
            let
                user1Chooser =
                    Ui.Chooser.update msg model.user1Chooser
                        |> Tuple.first

                selectedUser =
                    Maybe.withDefault "" (Ui.Chooser.getFirstSelected user1Chooser)

                selectableUsers =
                    Dict.values model.users
                        |> List.filter (.id >> ((/=) selectedUser))
                        |> List.map userToItem
            in
                ( { model
                    | user1Chooser = user1Chooser
                    , user2Chooser = Ui.Chooser.updateData selectableUsers model.user2Chooser
                  }
                , Cmd.none
                )

        User2Chooser msg ->
            let
                user2Chooser =
                    Ui.Chooser.update msg model.user2Chooser
                        |> Tuple.first

                selectedUser =
                    Maybe.withDefault "" (Ui.Chooser.getFirstSelected user2Chooser)

                selectableUsers =
                    Dict.values model.users
                        |> List.filter (.id >> ((/=) selectedUser))
                        |> List.map userToItem
            in
                ( { model
                    | user2Chooser = user2Chooser
                    , user1Chooser = Ui.Chooser.updateData selectableUsers model.user1Chooser
                  }
                , Cmd.none
                )

        User1Checkbox msg ->
            let
                user1Checkbox =
                    Ui.Checkbox.update msg model.user1Checkbox |> Tuple.first

                user2Checkbox =
                    Ui.Checkbox.setValue ((.value >> not) user1Checkbox) model.user2Checkbox

                tieCheckbox =
                    Ui.Checkbox.setValue ((.value >> not) user1Checkbox) model.tieCheckbox
            in
                ( { model
                    | user1Checkbox = { user1Checkbox | readonly = .value user1Checkbox }
                    , user2Checkbox = { user2Checkbox | readonly = .value user2Checkbox }
                    , tieCheckbox = { tieCheckbox | readonly = .value tieCheckbox }
                  }
                , Cmd.none
                )

        TieCheckbox msg ->
            let
                tieCheckbox =
                    Ui.Checkbox.update msg model.tieCheckbox |> Tuple.first

                user1Checkbox =
                    Ui.Checkbox.setValue ((.value >> not) tieCheckbox) model.user1Checkbox

                user2Checkbox =
                    Ui.Checkbox.setValue ((.value >> not) tieCheckbox) model.user2Checkbox
            in
                ( { model
                    | tieCheckbox = { tieCheckbox | readonly = .value tieCheckbox }
                    , user1Checkbox = { user1Checkbox | readonly = .value user1Checkbox }
                    , user2Checkbox = { user2Checkbox | readonly = .value user2Checkbox }
                  }
                , Cmd.none
                )

        User2Checkbox msg ->
            let
                user2Checkbox =
                    Ui.Checkbox.update msg model.user2Checkbox |> Tuple.first

                user1Checkbox =
                    Ui.Checkbox.setValue ((.value >> not) user2Checkbox) model.user1Checkbox

                tieCheckbox =
                    Ui.Checkbox.setValue ((.value >> not) user2Checkbox) model.tieCheckbox
            in
                ( { model
                    | user1Checkbox = { user1Checkbox | readonly = .value user1Checkbox }
                    , user2Checkbox = { user2Checkbox | readonly = .value user2Checkbox }
                    , tieCheckbox = { tieCheckbox | readonly = .value tieCheckbox }
                  }
                , Cmd.none
                )

        SubmitMatchup f ->
            let
                postMatchupCmd =
                    Task.map f Date.now
                        |> Task.andThen (\x -> (Http.toTask (postMatchup model.apiToken model.apiHost x)))
                        |> Task.attempt (Result.withDefault Nothing >> Maybe.map AddMatchup >> Maybe.withDefault NoOp)
            in
                ( model, postMatchupCmd )

        AddMatchup matchup ->
            ( addMatchup matchup model, Cmd.none )


rankingsRowView : ( Int, String, Profile ) -> Html.Html Msg
rankingsRowView ( rank, name, profile ) =
    let
        rankStr =
            toString rank

        nMatchesStr =
            toString profile.nMatches

        nMatchesWonStr =
            toString profile.nMatchesWon

        nMatchesLostStr =
            toString profile.nMatchesLost

        nMatchesTiedStr =
            toString (profile.nMatches - (profile.nMatchesWon + profile.nMatchesLost))

        ratingStr =
            toString <| round profile.rating
    in
        Html.tr
            []
            [ Html.td [] [ text rankStr ]
            , Html.td [] [ text name ]
            , Html.td [] [ text nMatchesStr ]
            , Html.td [] [ text nMatchesWonStr ]
            , Html.td [] [ text nMatchesLostStr ]
            , Html.td [] [ text nMatchesTiedStr ]
            , Html.td [] [ text ratingStr ]
            ]


rankingsView : Model -> Html.Html Msg
rankingsView model =
    let
        gameId =
            Maybe.withDefault "" (Ui.Chooser.getFirstSelected model.gameChooser)

        users =
            Dict.values model.users
                |> List.filter (Dict.member gameId << .profiles)
                |> List.filter (.profiles >> Dict.get gameId >> Maybe.map .nMatches >> Maybe.withDefault 0 >> (<) 0)
                |> List.sortBy (.profiles >> Dict.get gameId >> Maybe.map .rating >> Maybe.withDefault 0)
                |> List.reverse

        ranks =
            List.range 1 (List.length users)

        names =
            List.map .name users

        profiles =
            List.filterMap (Dict.get gameId << .profiles) users

        header =
            Html.thead
                []
                [ Html.tr
                    []
                    [ Html.th [] [ text "Rank" ]
                    , Html.th [] [ text "Name" ]
                    , Html.th [] [ text "Played" ]
                    , Html.th [] [ text "Won" ]
                    , Html.th [] [ text "Lost" ]
                    , Html.th [] [ text "Tied" ]
                    , Html.th [] [ text "Rating" ]
                    ]
                ]

        rows =
            if (List.length profiles > 0) then
                (header :: (List.map rankingsRowView (List.zip3 ranks names profiles)))
            else
                []
    in
        Ui.App.view
            App
            model.app
            [ Html.table [] rows ]


matchupRowView : ( String, String, Result, Date ) -> Html.Html Msg
matchupRowView ( name1, name2, result, createdAt ) =
    let
        name1Elem =
            case result of
                Loss ->
                    text name1

                _ ->
                    strong [] [ text name1 ]

        name2Elem =
            case result of
                Win ->
                    text name2

                _ ->
                    strong [] [ text name2 ]

        createdAtStr =
            text <| Date.formatISO8601 createdAt
    in
        Html.tr
            []
            [ Html.td [] [ name1Elem ]
            , Html.td [] [ name2Elem ]
            , Html.td [] [ createdAtStr ]
            ]


getName : Model -> String -> String
getName model x =
    Maybe.withDefault "" (Maybe.map .name (Dict.get x model.users))


matchupsView : Model -> Html.Html Msg
matchupsView model =
    let
        gameId =
            Maybe.withDefault "" (Ui.Chooser.getFirstSelected model.gameChooser)

        limit =
            10

        matchups =
            model.matchups
                |> List.filter (.gameId >> Uuid.toString >> ((==) gameId))
                |> List.take (limit + 1)

        names1 =
            List.map (.user1Id >> getName model) matchups

        names2 =
            List.map (.user2Id >> getName model) matchups

        results =
            List.map .result matchups

        createdAts =
            List.map .createdAt matchups

        header =
            Html.thead
                []
                [ Html.tr
                    []
                    [ Html.th [] [ text "P1" ]
                    , Html.th [] [ text "P2" ]
                    , Html.th [] [ text "Date" ]
                    ]
                ]

        rows =
            if (List.length results > 0) then
                (header :: List.map matchupRowView (List.zip4 names1 names2 results createdAts))
            else
                []
    in
        Ui.App.view
            App
            model.app
            [ Html.table [] (List.take limit rows)
            , if List.length rows > limit then
                text "..."
              else
                text ""
            ]


getResult : ( Bool, Bool, Bool ) -> Result
getResult x =
    case x of
        ( True, False, False ) ->
            Win

        ( False, False, True ) ->
            Loss

        _ ->
            Tie


view : Model -> Html.Html Msg
view model =
    let
        gameId =
            Ui.Chooser.getFirstSelected model.gameChooser
                |> Maybe.map Uuid.fromString
                |> Maybe.join

        user1Id =
            Ui.Chooser.getFirstSelected model.user1Chooser

        user2Id =
            Ui.Chooser.getFirstSelected model.user2Chooser

        result =
            getResult ( .value model.user1Checkbox, .value model.tieCheckbox, .value model.user2Checkbox )

        matchup =
            Maybe.map3 (\a b c -> SubmitMatchup (\date -> Matchup a b c result date)) gameId user1Id user2Id

        verb =
            case result of
                Win ->
                    "wins"

                Tie ->
                    "ties"

                Loss ->
                    "loses"

        toMatchupStr a b =
            "" ++ (getName model a) ++ " " ++ verb ++ " against " ++ (getName model b) ++ "!"

        matchupStr =
            Maybe.map2 toMatchupStr user1Id user2Id
    in
        Ui.App.view
            App
            model.app
            [ Ui.Container.column
                []
                [ Ui.title [] [ text "Blank-Rank" ]
                , Ui.Container.column
                    []
                    [ Html.h3 [] [ text "Select a game" ]
                    , Ui.Container.row
                        []
                        [ Html.map GameChooser (Ui.Chooser.view model.gameChooser)
                        ]
                    ]
                , Ui.Container.column
                    []
                    [ Html.h3 [] [ text "Submit a result" ]
                    , Ui.Container.row
                        []
                        [ Html.map User1Chooser (Ui.Chooser.view model.user1Chooser)
                        , Html.map User1Checkbox (Ui.Checkbox.viewRadio model.user1Checkbox)
                        , Html.map TieCheckbox (Ui.Checkbox.viewRadio model.tieCheckbox)
                        , Html.map User2Checkbox (Ui.Checkbox.viewRadio model.user2Checkbox)
                        , Html.map User2Chooser (Ui.Chooser.view model.user2Chooser)
                        ]
                    , Ui.Container.row
                        []
                        (List.filterMap identity
                            [ Maybe.map (Ui.Button.primary "Submit") matchup
                            , Just <| Ui.Container.columnCenter [] [ text <| Maybe.withDefault "" matchupStr ]
                            ]
                        )
                    ]
                , Ui.Container.column [] [ rankingsView model ]
                , Ui.Container.column [] [ matchupsView model ]
                ]
            , Ui.Container.row
                []
                [ text "" ]
            ]


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = \x -> ( init x, Task.attempt Init (fetchAll x.apiToken x.apiHost) )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
