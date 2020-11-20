module View.HTML.Tasks (render) where

import Data.Foldable (for_)
import Data.UUID (toString)
import Prelude (($), Unit, discard, show)
import Model.Task
import Text.Smolder.HTML
import Text.Smolder.HTML.Attributes (lang, href, rel, type', for, id, value, action, method, name)
import Text.Smolder.Markup ((!), text)
import Text.Smolder.Renderer.String as Smolder

renderTask :: Task -> Html Unit
renderTask task = tr $ do
  td $ text (toString task.id)
  td $ text task.name
  td $ text (show task.priority)

render :: Array Task -> String
render tasks = Smolder.render doc
  where doc = html ! lang "en" $ do
          head $ link ! href "/static/css/tasks.css" ! rel "stylesheet"
          body $ do
            h1 $ text "Your Tasks"
            table $ do
              thead $ tr $ do
                th $ text "ID"
                th $ text "Task Name"
                th $ text "Priority"
              tbody $ for_ tasks renderTask
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
