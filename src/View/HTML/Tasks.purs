module View.HTML.Tasks (render) where

import Prelude

import Data.Array (length)
import Data.Foldable (for_)
import Data.UUID (toString)
import Model.Task
import Text.Smolder.HTML
import Text.Smolder.HTML.Attributes (lang, href, rel, type', for, id, value, action, method, name)
import Text.Smolder.Markup ((!), text)
import Text.Smolder.Renderer.String as Smolder

renderTask :: Task -> Html Unit
renderTask task = tr $ do
  td $ text task.name
  td $ text $ show task.status
  td $ text (show task.priority)
  td $ form ! action ("/task/" <> toString task.id) ! method "POST" $ do
    input ! type' "hidden" ! name "_method" ! value "delete"
    input ! type' "submit" ! value "Delete"

render :: Array Task -> String
render tasks = Smolder.render doc
  where doc = html ! lang "en" $ do
          head $ link ! href "/static/css/tasks.css" ! rel "stylesheet"
          body $ do
                   h1 $ text "Your Tasks"
                   if length tasks > 0 then
                      table $ do
                        thead $ tr $ do
                          th $ text "Task Name"
                          th $ text "Status"
                          th $ text "Priority"
                          th $ text ""
                        tbody $ for_ tasks renderTask
                     else p $ text "You have no tasks."
                   h1 $ text "Create a new task"
                   form ! action "/tasks" ! method "POST" $ do
                     label ! for "name" $ text "Task Name: "
                     input ! name "name" ! type' "text" ! id "name"
                     br
                     label ! for "priority" $ text "Priority: "
                     select ! name "priority" ! id "priority" $ do
                       option ! value "low" $ text "Low"
                       option ! value "medium" $ text "Medium"
                       option ! value "high" $ text "High"
                     br
                     input ! type' "submit" ! value "Create"
