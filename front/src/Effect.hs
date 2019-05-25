module Effect where

import           API
import           Common.Types.Todo
import           Miso.Effect

data Model = Model
    { todos       :: [Todo] -- Todo全体
    , draftCreate :: Todo'  -- 新規作成中のTodo
    } deriving(Eq)

data Action = FetchTodos
            | DisplayAll [Todo]
            | UpdateDraftCreate Todo'
            | CreateTodo
            | FinishTodo TodoID

updateModel :: Action -> Model -> Effect Action Model
updateModel FetchTodos model              = model <# fmap DisplayAll fetchAllTodos
updateModel (DisplayAll tds) model        = noEff $ model { todos = tds }
updateModel (UpdateDraftCreate td') model = noEff $ model { draftCreate = td' }
updateModel CreateTodo model = model <# do
    td <- createTodo $ draftCreate model
    let tds = todos model
    return $ DisplayAll $ td : tds
updateModel (FinishTodo tid) model = model <# do
    deleteTodo tid
    return FetchTodos
