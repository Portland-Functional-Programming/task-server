module View.Tasks where

import Data.Foldable (for_)
import Prelude (($), Unit, discard, show)
import Task
import Text.Smolder.HTML
import Text.Smolder.HTML.Attributes (lang, href, rel, type', for, id, value, action, method, name)
import Text.Smolder.Markup ((!), text)
import Text.Smolder.Renderer.String (render)

renderTask :: Task -> Html Unit
renderTask task = tr $ do
  td $ text task.name
  td $ text (show task.priority)

renderTasks :: Array Task -> String
renderTasks tasks = render doc
  where doc = html ! lang "en" $ do
          head $ link ! href "/static/css/tasks.css" ! rel "stylesheet"
          body $ do
            h1 $ text "Your Tasks"
            table $ do
              thead $ tr $ do
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
