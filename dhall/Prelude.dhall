let MultiClozeSpec = { phrases : List Text, images : List Text }

let VideoSource =
      < LocalVideo : Text
      | YouTubeDL : { url : Text, out : Text, format : Text }
      >

let ExcerptSpec =
      { source : VideoSource
      , subs : Text
      , clipf : Text → Text
      , audiof : Text → Text
      , framef : Text → Text
      }

let VariField = < Empty | Raw : Text | Images : List Text | Audio : Text >

let MinimalReversedSpec = { from : VariField, to : VariField }

let BasicReversedSpec =
      { from : VariField
      , from_extra : VariField
      , to : VariField
      , to_extra : VariField
      }

let Forvo = { locale : Text, apiKey : Text }

let Spec =
      < Pronunciation :
          { multis : List MultiClozeSpec
          , audiof : Text → Text
          , forvo : Optional Forvo
          }
      | Excerpt : List ExcerptSpec
      | MinimalReversed : List MinimalReversedSpec
      | BasicReversed : List BasicReversedSpec
      >

let Part = { outfile : Text, spec : Spec }

let defaultDeckLayout =
      λ(l : Text) →
      λ(p : List Part) →
        { mapKey = l
        , mapValue =
          { resourceDirs =
            { images = "${l}/images"
            , audio = "${l}/audio"
            , video = "${l}/video"
            }
          , exportDirs =
            { notes = "out/decks/${l}/notes"
            , clips = "out/decks/${l}/clips"
            , audio = "out/decks/${l}/audio"
            , images = "out/decks/${l}/images"
            }
          , parts = p
          }
        }

let defaultYouTubeDLSpec =
      λ(t : Text) →
      λ(u : Text) →
      λ(s : Text) →
        { source =
            VideoSource.YouTubeDL
              { url = u, out = "${t}.mp4", format = "mp4" }
        , subs = s
        , clipf = λ(n : Text) → "${t}-${n}.mkv"
        , audiof = λ(n : Text) → "${t}-${n}.mp3"
        , framef = λ(n : Text) → "${t}-${n}.bmp"
        }

in  { MultiClozeSpec
    , VideoSource
    , ExcerptSpec
    , VariField
    , MinimalReversedSpec
    , BasicReversedSpec
    , Forvo
    , Spec
    , Part
    , defaultDeckLayout
    , defaultYouTubeDLSpec
    }
