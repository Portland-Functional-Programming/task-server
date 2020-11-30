module Controller.Home (get) where

import Prelude

import HTTPure (Request, Response)
import Effect.Aff.Class (class MonadAff)
import HTTPure as HTTPure
import Text.Smolder.Renderer.String as Smolder
import Text.Smolder.Markup ((!), text)
import Text.Smolder.HTML.Attributes (action, href, lang, method, name, rel, type', value)
import Text.Smolder.HTML (body, a, form, head, html, input, link)

get :: forall m. MonadAff m => Request -> m Response
get _ = HTTPure.ok $ Smolder.render $
        html ! lang "en" $ do
          head $ link ! href "/static/css/tasks.css" ! rel "stylesheet"
          body $ a ! href "/login" $ text "Log In."
            -- form ! action "/login" ! method "POST" $ do
          --   input ! type' "text" ! name "username"
          --   input ! type' "password" ! name "password"
          --   input ! type' "submit" ! value "Log In"
