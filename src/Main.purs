module Main where

import Prelude

import Data.Argonaut.Core (stringify) as Json
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (toUnfoldable)
import Data.Either (Either, either)
import Data.Functor (mapFlipped)
import Data.List.Types (List(..), (:))
import Data.Maybe (fromMaybe, maybe)
import Data.String.Yarn (unlines)
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Class.Console (errorShow, log)
import Foreign.Object (Object)
import Foreign.Object (insert, keys) as Obj
import Node.Encoding (Encoding(..))
import Node.FS.Sync (exists, readTextFile, writeTextFile)
import Node.Path (basename)
import Node.Process (argv, cwd, lookupEnv)

configFile :: Effect String
configFile =
  lookupEnv "XDG_CONFIG_HOME"
  >>=
    (maybe
      (((_ <> "/projects.json") <<< fromMaybe "") <$> lookupEnv "HOME")
      (pure <<< (_ <> "/projects.json")))

addToProjects :: String → Effect Unit
addToProjects dir = loadConfig >>= (map (Obj.insert projName dir) >>> either errorShow (encodeJson >>> Json.stringify >>> writeConfig))
  where
    projName = basename dir
    writeConfig str = configFile >>= flip (writeTextFile UTF8) str

loadConfig :: Effect (Either String (Object String))
loadConfig = configFile >>= \cf ->
  ifM (exists cf)
   ((decodeJson <=< jsonParser) <$> readTextFile UTF8 cf)
   (pure (pure mempty))

main :: Effect Unit
main =
  argv >>= (toUnfoldable >>> case _ of
    Nil -> errorShow "Wow!! Not even node? Drop a line as to how you invoked this"
    (_ : programName : "add" : Nil) -> cwd >>= addToProjects
    (_ : programName : "add" : xs) ->
      cwd <#> ((_ <> "/") >>> (<>) >>> mapFlipped xs)
      >>= (traverse_ addToProjects)
    (_ : programName : "ls" : _) -> lsProjects
    _ -> errorShow $ "Usage : project add [dir] | ls "
  )

lsProjects :: Effect Unit
lsProjects = loadConfig >>= (either identity (Obj.keys >>> unlines) >>> log)
