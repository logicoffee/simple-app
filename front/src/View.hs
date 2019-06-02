{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module View where

import           Common.Types.Todo
import           Effect
import           Miso.Html
import           Miso.String

render :: Model -> View Action
render Model{..} = div_ []
    [ input_ [type_ "text", value_ $ toMisoString $ pTask draftCreate]
    , input_ [type_ "date", value_ $ toMisoString $ show $ pDeadline draftCreate]
    , button_ [onClick CreateTodo] []
    , table_ []
        [ thead_ [] [tr_ []
            [ td_ [] [text "Task"]
            , td_ [] [text "Deadline"]]]
        , tbody_ [] $ Prelude.map todoListItem todos]
    ]


todoListItem :: Todo -> View Action
todoListItem Todo{..} = tr_ []
    [ td_ [] [text $ toMisoString task]
    , td_ [] [text $ toMisoString $ show deadline]]
