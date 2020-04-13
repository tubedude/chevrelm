module Main exposing (main)

import Browser
import Html exposing (Html, a, button, code, div, h1, hr, input, p, pre, span, text)
import Html.Attributes exposing (class, href, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D
import Json.Encode as E
import RemoteData



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { keyCreation : KeyCreation
    , privKey : PrivKey
    , pubKey : RemoteData.WebData PubKey
    , unlockInfo : UnlockInfo
    }


type alias PrivKey =
    { gpgPrivKey : RemoteData.WebData String
    , gpgPrivKeyPass : String
    , gpgPrivSaveToDisk : Bool
    }


type alias ValidPrivKey =
    { gpgPrivKey : String
    , gpgPrivKeyPass : String
    , gpgPrivSaveToDisk : Bool
    }


type alias KeyCreation =
    { identifier : String
    , password : String
    , bits : Int
    }


type alias UnlockInfo =
    { fingerprint : String
    , unlockStatus : RemoteData.WebData String
    , password : String
    }


type alias PubKey =
    { fingerprint : String
    , pubKey : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, Cmd.none )


initModel : Model
initModel =
    Model initKeyCreation initPrivKey RemoteData.NotAsked initUnlockInfo


initKeyCreation : KeyCreation
initKeyCreation =
    KeyCreation "" "" 4096


initPrivKey : PrivKey
initPrivKey =
    PrivKey RemoteData.NotAsked "" False


initUnlockInfo : UnlockInfo
initUnlockInfo =
    UnlockInfo "" RemoteData.NotAsked ""



-- UPDATE


type Msg
    = EnteredPrivKey String
    | EnteredPrivKeyPass String
    | EnteredBits String
    | SubmitKey
    | CreatePrivKey
    | PrivKeyCreated (RemoteData.WebData String)
    | AddCreationPassword String
    | AddCreationIdentifier String
    | UnlockKey
    | UpdateFingerprint String
    | GotPubKey (RemoteData.WebData PubKey)
    | GotUnlockInfo (RemoteData.WebData String)
    | UpdateUnlockPassword String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EnteredBits str ->
            let
                oldKeyCriation =
                    model.keyCreation

                newKeyCreation =
                    { oldKeyCriation | bits = Maybe.withDefault model.keyCreation.bits (String.toInt str) }
            in
            ( { model | keyCreation = newKeyCreation }, Cmd.none )

        AddCreationPassword str ->
            let
                keyCreation =
                    model.keyCreation

                newKeyCreation =
                    { keyCreation | password = str }
            in
            ( { model | keyCreation = newKeyCreation }, Cmd.none )

        AddCreationIdentifier str ->
            let
                keyCreation =
                    model.keyCreation

                newKeyCreation =
                    { keyCreation | identifier = str }
            in
            ( { model | keyCreation = newKeyCreation }, Cmd.none )

        CreatePrivKey ->
            ( model, createKey model.keyCreation )

        PrivKeyCreated privkey ->
            let
                newKey =
                    PrivKey privkey model.keyCreation.password False
            in
            ( { model | privKey = newKey }, Cmd.none )

        EnteredPrivKey str ->
            let
                newprivKey =
                    PrivKey (RemoteData.Success str) model.privKey.gpgPrivKeyPass model.privKey.gpgPrivSaveToDisk
            in
            ( { model | privKey = newprivKey }, Cmd.none )

        EnteredPrivKeyPass str ->
            let
                newprivKey =
                    PrivKey model.privKey.gpgPrivKey str model.privKey.gpgPrivSaveToDisk
            in
            ( { model | privKey = newprivKey }, Cmd.none )

        SubmitKey ->
            case model.privKey.gpgPrivKey of
                RemoteData.Success key ->
                    let
                        newKey =
                            ValidPrivKey key model.privKey.gpgPrivKeyPass True
                    in
                    ( model, savePrivKey newKey )

                _ ->
                    ( model, Cmd.none )

        UnlockKey ->
            ( model, unlockKey model.unlockInfo )

        UpdateFingerprint str ->
            let
                oldUnlockInfo =
                    model.unlockInfo

                newUnlockInfo =
                    { oldUnlockInfo | fingerprint = str }

                newModel =
                    { model | unlockInfo = newUnlockInfo }
            in
            ( newModel, Cmd.none )

        GotPubKey resp ->
            case resp of
                RemoteData.Success key ->
                    let
                        oldUnlockInfo =
                            model.unlockInfo

                        newUnlockInfo =
                            { oldUnlockInfo | fingerprint = key.fingerprint, password = model.keyCreation.password }
                    in
                    ( { model | unlockInfo = newUnlockInfo, pubKey = resp }
                    , Cmd.none
                    )

                _ ->
                    ( { model | pubKey = resp }, Cmd.none )

        GotUnlockInfo resp ->
            let
                oldUnlockInfo =
                    model.unlockInfo

                newUnlockInfo =
                    { oldUnlockInfo | unlockStatus = resp }

                newModel =
                    { model | unlockInfo = newUnlockInfo }
            in
            ( newModel, Cmd.none )

        UpdateUnlockPassword str ->
            let
                oldUnlockInfo =
                    model.unlockInfo

                newUnlockInfo =
                    { oldUnlockInfo | password = str }
            in
            ( { model | unlockInfo = newUnlockInfo }, Cmd.none )


createKey : KeyCreation -> Cmd Msg
createKey params =
    Http.post
        { url = "/remoteSigner/gpg/generateKey"
        , body = Http.jsonBody (encodeKeyCreation params)
        , expect = Http.expectString (RemoteData.fromResult >> PrivKeyCreated)
        }


encodeKeyCreation : KeyCreation -> E.Value
encodeKeyCreation params =
    E.object
        [ ( "Identifier", E.string params.identifier )
        , ( "Password", E.string params.password )
        , ( "Bits", E.int params.bits )
        ]


savePrivKey : ValidPrivKey -> Cmd Msg
savePrivKey privKey =
    Http.post
        { url = "/keyRing/addPrivateKey"
        , body = Http.jsonBody (buildSavePrivKeyBody privKey)
        , expect = Http.expectJson (RemoteData.fromResult >> GotPubKey) decodePubKey
        }


unlockKey : UnlockInfo -> Cmd Msg
unlockKey unlockInfo =
    Http.post
        { url = "/remoteSigner/gpg/unlockKey"
        , body = Http.jsonBody (buildUnlockBody unlockInfo)
        , expect = Http.expectString (RemoteData.fromResult >> GotUnlockInfo)
        }


buildUnlockBody : UnlockInfo -> E.Value
buildUnlockBody unLockInfo =
    E.object
        [ ( "FingerPrint", E.string unLockInfo.fingerprint )
        , ( "Password", E.string unLockInfo.password )
        ]


decodePubKey : D.Decoder PubKey
decodePubKey =
    D.map2 PubKey
        (D.field "FingerPrint" D.string)
        (D.field "PublicKey" D.string)


buildSavePrivKeyBody : ValidPrivKey -> E.Value
buildSavePrivKeyBody privKey =
    E.object
        [ ( "EncryptedPrivateKey", E.string privKey.gpgPrivKey )
        , ( "Password", E.string privKey.gpgPrivKeyPass )
        , ( "SaveToDisk", E.bool privKey.gpgPrivSaveToDisk )
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    Html.main_ [ class "container-fluid" ]
        [ Html.section [ class "container row", id "header" ]
            [ h1 [ class "" ] [ text "Chevron WEB interface" ]
            , Html.h6 [ class "" ] [ text "This is just a test" ]
            ]
        , Html.section [ id "keyCreation", class "container row" ]
            [ Html.h4 [ class "" ] [ text "Create key" ]
            , p [] [ text "Not necessary if you have your own key. Just remember this is not really safe to create a key online. If this is a production key you should create it in a local machine." ]
            , div [ class "form-control" ]
                [ Html.label [] [ text "Name and email: " ]
                , viewInput "text" "FirstName LastName first.last@example.com" model.keyCreation.identifier AddCreationIdentifier
                ]
            , div [ class "form-control" ]
                [ Html.label [] [ text "Key Password: " ]
                , viewInput "text" "password" model.keyCreation.password AddCreationPassword
                ]
            , div [ class "form-control" ]
                [ Html.label [] [ text "Bits: " ]
                , input [ type_ "text", placeholder "4096", value (String.fromInt model.keyCreation.bits), onInput EnteredBits ] []
                ]
            , button [ onClick CreatePrivKey ] [ text "Create Private Key" ]
            , displayCreationMsg model
            , hr [] []
            ]
        , Html.section [ id "privKey", class "container row" ]
            [ Html.h4 [ class "" ] [ text "Insert key" ]
            , Html.legend [] [ text "Private Key section" ]
            , Html.fieldset []
                [ displayPrivKey model
                ]
            , hr [] []
            ]
        , Html.section [ id "unlockKey", class "container row" ]
            [ Html.h4 [ class "" ] [ text "Unlock key" ]
            , Html.legend [] [ text "Private Key section" ]
            , Html.fieldset []
                [ displayUnlockStatus model.unlockInfo
                , viewInput "text" "Fingerprint" model.unlockInfo.fingerprint UpdateFingerprint
                , viewInput "text" "Password" model.unlockInfo.password UpdateUnlockPassword
                , button [ onClick UnlockKey, class "button" ] [ text "Unlock key" ]
                ]
            ]
        , Html.footer [ class "footer" ]
            [ Html.section [ class "container row" ]
                [ p []
                    [ text "Proudly made with "
                    , a [ href "https://elm-lang.org/" ] [ text "Elm" ]
                    , text "."
                    ]
                ]
            ]
        ]


displayCreationMsg : Model -> Html Msg
displayCreationMsg model =
    case model.privKey.gpgPrivKey of
        RemoteData.NotAsked ->
            div []
                [ p [] [ text "You may enter below a key you generated elsewhere." ]
                ]

        RemoteData.Loading ->
            div []
                [ p [] [ text "Hold on. Generating key." ]
                ]

        RemoteData.Failure err ->
            let
                errMsg =
                    case err of
                        Http.NetworkError ->
                            "Network Error"

                        Http.BadStatus int ->
                            "Bad Status: " ++ String.fromInt int

                        Http.Timeout ->
                            "Timeout"

                        Http.BadUrl str ->
                            "BadUrl: " ++ str

                        Http.BadBody str ->
                            "BadBody: " ++ str
            in
            div []
                [ p [] [ text "What a shame. There was nome \"Network\" error. Why don't we try again?" ]
                , pre [] [ code [] [ text errMsg ] ]
                ]

        RemoteData.Success _ ->
            div []
                [ p [] [ text "Success!! Check your brand new key below" ]
                ]


displayPrivKey : Model -> Html Msg
displayPrivKey model =
    case model.privKey.gpgPrivKey of
        RemoteData.NotAsked ->
            div []
                [ keyInput "" model.privKey.gpgPrivKeyPass
                ]

        RemoteData.Loading ->
            div [] []

        RemoteData.Failure _ ->
            div [] [ keyInput "" "" ]

        RemoteData.Success key ->
            div [] [ keyInput key model.privKey.gpgPrivKeyPass ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


keyInput : String -> String -> Html Msg
keyInput val pass =
    div []
        [ Html.textarea [ placeholder "copy and past your key", value val, onInput EnteredPrivKey ] []
        , viewInput "text" "password" pass EnteredPrivKeyPass
        , button [ onClick SubmitKey ] [ text "Submit Private key" ]
        ]


displayUnlockStatus : UnlockInfo -> Html Msg
displayUnlockStatus unlockInfo =
    case unlockInfo.unlockStatus of
        RemoteData.NotAsked ->
            Html.span [] [ p [] [ text "Don't know if it is unlocked" ] ]

        RemoteData.Loading ->
            Html.span [] [ p [] [ text "Unlocking" ] ]

        RemoteData.Failure _ ->
            Html.span [] [ p [] [ text "Network issue" ] ]

        RemoteData.Success str ->
            Html.span [] [ p [] [ text (String.concat [ "Unlock is: ", str ]) ] ]
