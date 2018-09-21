module I3 where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.Common (null, trim)
import Effect.Aff (Aff, makeAff)
import Effect.Class (liftEffect)
import Node.Buffer (toString) as Buf
import Node.ChildProcess (defaultExecOptions, defaultSpawnOptions, exec, spawn)
import Node.Encoding (Encoding(..))

execCommand :: String → Aff String
execCommand command =
  makeAff (\f -> exec command defaultExecOptions (_.stdout >>> Right >>> f) *> mempty)
  >>= (liftEffect <<< Buf.toString UTF8)

getWindowById :: String → Aff (Maybe String)
getWindowById title =
  (\s -> if (null s) then Nothing else Just s) <$> execCommand ("findWindowByTitle " <> title)

switchToWindow :: String -> Aff Unit
switchToWindow id =
  void $
    execCommand ("i3-msg '[con_id=\"" <> (trim id) <> "\"] focus'")

createWindow :: String → String → Aff Unit
createWindow projName projPath =
  liftEffect $
    void $
      spawn "termite" ["-t", projName, "-d",  projPath] $
        defaultSpawnOptions { detached = true }
