let VF = < Blank | RawText : Text | Image : Text | Audio : Text | Video : Text >

let VFC = < Single : VF | Multi : List VF >

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

let MinimalReversedSpec = { front : VF, back : VF }

let BasicReversedSpec =
      { front : VF, front_extra : VFC, back : VF, back_extra : VFC }

let Forvo = { locale : Text }

let PronunciationSpec =
      { multis : List MultiClozeSpec
      , audiof : Text → Text
      , forvo : Optional Forvo
      }

let Spec =
      < Pronunciation : List PronunciationSpec
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
            VideoSource.YouTubeDL { url = u, out = "${t}.mp4", format = "mp4" }
        , subs = s
        , clipf = λ(n : Text) → "${t}-${n}.mkv"
        , audiof = λ(n : Text) → "${t}-${n}.mp3"
        , framef = λ(n : Text) → "${t}-${n}.bmp"
        }

let defaultLocalMkv =
      λ(t : Text) →
      λ(s : Text) →
        { source =
            VideoSource.LocalVideo { "${t}.mkv" }
        , subs = s
        , clipf = λ(n : Text) → "${t}-${n}.mkv"
        , audiof = λ(n : Text) → "${t}-${n}.mp3"
        , framef = λ(n : Text) → "${t}-${n}.bmp"
        }

in  { MultiClozeSpec
    , VideoSource
    , ExcerptSpec
    , VF
    , MinimalReversedSpec
    , BasicReversedSpec
    , Forvo
    , Spec
    , Part
    , defaultDeckLayout
    , defaultYouTubeDLSpec
    , defaultLocalMkv
    }
