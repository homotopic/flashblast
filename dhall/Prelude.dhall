{ MultiClozeSpec = { phrases : List Text, images : List Text }
, VideoSource =
    < LocalVideo : Text
    | YouTubeDL : { url : Text, out : Text, format : Text }
    >
, ExcerptSpec =
    { source : VideoSource
    , subs : Text
    , clipf : Text → Text
    , audiof : Text → Text
    , framef : Text → Text
    }
, VariField = < Empty | Raw : Text | Images : List Text | Audio : Text >
, MinimalReversedSpec = { from : VariField, to : VariField }
, BasicReversedSpec =
    { from : VariField
    , from_extra : VariField
    , to : VariField
    , to_extra : VariField
    }
, Forvo = { locale : Text, apiKey : Text }
, Spec =
    < Pronunciation :
        { multis : List MultiClozeSpec
        , audiof : Text → Text
        , forvo : Optional Forvo
        }
    | Excerpt : List ExcerptSpec
    | MinimalReversed : List MinimalReversedSpec
    | BasicReversed : List BasicReversedSpec
    >
, Part = { outfile : Text, spec : Spec }
, defaultDeckLayout =
    λ(l : Text) →
    λ(p : List Part) →
      { mapKey = l
      , mapValue =
        { resourceDirs =
          { images = "${l}/images", audio = "${l}/audio", video = "${l}/video" }
        , exportDirs =
          { notes = "out/decks/${l}/notes"
          , clips = "out/decks/${l}/clips"
          , audio = "out/decks/${l}/audio"
          , images = "out/decks/${l}/images"
          }
        , parts = p
        }
      }
}
