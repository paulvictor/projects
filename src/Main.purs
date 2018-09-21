module Main where

import Prelude

import Data.Argonaut.Core (stringify) as Json
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (toUnfoldable)
import Data.Either (Either(..), either, note)
import Data.Functor (mapFlipped)
import Data.List.Types (List(..), (:))
import Data.Maybe (fromMaybe, maybe)
import Data.String.Yarn (unlines)
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff, Canceler, launchAff_, makeAff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (errorShow, log)
import Foreign.Object (Object)
import Foreign.Object (insert, keys, lookup) as Obj
import I3 (createWindow, getWindowById, switchToWindow)
import Node.ChildProcess (defaultSpawnOptions, spawn, stdin, stdout)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (exists, readTextFile, writeTextFile)
import Node.Path (basename)
import Node.Process (argv, cwd, exit, lookupEnv)
import Node.Stream (Readable, end, writeString)

foreign import data Readline :: Type
foreign import _rlOnlyReadable :: ∀ r . Readable r -> Effect Readline
foreign import _getLine :: Readline -> Canceler -> EffectFnAff String

getLine :: Readline → Aff String
getLine rl = fromEffectFnAff $ _getLine rl mempty

askProcess :: String -> Array String -> String -> Aff String
askProcess cmd args q = do
  child <-
    liftEffect $ spawn cmd args (defaultSpawnOptions { detached = true })
  sendLine (stdin child) q *>
  closeStream (stdin child) *>
  liftEffect (_rlOnlyReadable (stdout child)) >>= getLine
  where
  closeStream w = makeAff (\f -> end w (f (Right unit)) *> mempty)
  sendLine w s = makeAff (\f -> writeString w UTF8 s (f (Right unit)) *> mempty)

configFile :: Effect String
configFile =
  lookupEnv "XDG_CONFIG_HOME"
  >>=
    (maybe
      (((_ <> "/projects.json") <<< fromMaybe "") <$> lookupEnv "HOME")
      (pure <<< (_ <> "/projects.json")))

addToProjects :: String → Effect Unit
addToProjects dir =
  map (Obj.insert projName dir) <$> loadConfig >>=
  either errorShow (encodeJson >>> Json.stringify >>> writeConfig)
  where
    projName = basename dir
    writeConfig str = configFile >>= flip (writeTextFile UTF8) str

loadConfig :: Effect (Either String (Object String))
loadConfig = configFile >>= \cf ->
  ifM (exists cf)
   ((decodeJson <=< jsonParser) <$> readTextFile UTF8 cf)
   (pure (pure mempty))

goto :: String → Aff Unit
goto projName =
  getWindowById projName
  >>= maybe
    (liftEffect (findProjDirectory projName)
      >>= either
        errorShow
        (createWindow projName))
    switchToWindow

findProjDirectory :: String → Effect (Either String String)
findProjDirectory projName =
  (identity >=>
    (note "Unable to find project" <<< Obj.lookup projName)
  ) <$> loadConfig

main :: Effect Unit
main =
  argv >>= (toUnfoldable >>> case _ of
    (_ : programName : "add" : Nil) -> cwd >>= addToProjects
    (_ : programName : "add" : xs) ->
      cwd <#> ((_ <> "/") >>> (<>) >>> mapFlipped xs)
      >>= (traverse_ addToProjects)
    (_ : programName : "ls" : _) -> (unlines <$> listProjects) >>= log
    (_ : programName : "choose" : _) -> do
       prjs <- listProjects
       launchAff_ ((userChoice prjs >>= goto) *> (liftEffect (exit 0)))
    _ -> errorShow $ "Usage : project add [dir] | ls "
  )
  where
  listProjects = loadConfig <#> either mempty Obj.keys
  userChoice = unlines >>> askProcess "rofi" ["-dmenu"]
