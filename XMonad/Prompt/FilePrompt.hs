{-# LANGUAGE UnicodeSyntax #-}
module XMonad.Prompt.FilePrompt 
  ( FPConf (..)
  , filePrompt
  , kdeOpen
  , gnomeOpen
  , mimeOpen
  , customizeOpen
  , openWith
  ) where

import XMonad              (X, spawn, MonadIO)
import XMonad.Core         (io)
import XMonad.Prompt       (XPrompt (..), mkXPrompt, XPConfig, getNextCompletion)
import Control.Monad       ((>>=), (>>), liftM)
import Control.Arrow       (second)
import Control.Applicative ((<$>))
import Data.List           (sort, span, isPrefixOf)
import Data.Maybe          (fromMaybe)
import Directory           (getDirectoryContents)
import System.FilePath     (pathSeparator, splitFileName, (</>), takeExtensions)
import System.Directory    (getHomeDirectory, doesDirectoryExist, doesFileExist)
import System.Process      (proc, createProcess, CreateProcess (std_out, cwd), StdStream (CreatePipe), waitForProcess)
import System.Exit         (ExitCode (ExitSuccess))
import System.IO           (hGetLine)

--------------------------------------------------------------------------------
-- First, some function to open files at in the end.

{- Directly specify a command to open a file. The command must contain `%f' as a placeholder for the filename -}
openWith ∷ MonadIO m ⇒ String → String → m ()
openWith program filename = snd `liftM` fixHome run filename
  where 
    run filename' = spawn $ replace "%f" filename' program

{- Extend a generic file opener by custom opener specified my extension, MIME string and binarity of encoding. -}
customizeOpen ∷ MonadIO m ⇒
  (Maybe String → String → Bool → Maybe (String → m ())) →
  (String → m ()) →
  String → m ()
customizeOpen customization fallback filename = do
  Just (mime, binary) ← snd `liftM` fixHome fileInfo filename
  let open = fromMaybe fallback (customization extension mime binary)
      extension = ifthenelse null (const Nothing) (Just . tail) . takeExtensions $ filename
  open filename

{- Specify programs to open files, by MIME -}
mimeOpen ∷ MonadIO m ⇒ [(String, String)] → String → String → String → m ()
mimeOpen mimeToProgram textViewer binaryViewer filename = do
  Just (mime, binary) ← fileInfo filename
  let program       = fromMaybe defaultViewer $ lookup filename mimeToProgram
      defaultViewer = if binary then binaryViewer else textViewer
  openWith program filename

{- Open files the KDE4 way. -}
kdeOpen ∷ MonadIO m ⇒ String → m ()
kdeOpen = openWith "kioclient exec file:'%f'" 

{- Open files the Gnome way. -}
gnomeOpen ∷ MonadIO m ⇒ String → m ()
gnomeOpen = openWith "gnome-open '%f'"

--------------------------------------------------------------------------------
-- The definition of the file prompt.

newtype FPConf = FPConf { open ∷ String → X () }

data FilePrompt = FilePrompt

instance XPrompt FilePrompt where
  showXPrompt FilePrompt = "Open: "
  nextCompletion FilePrompt = getNextCompletion
  commandToComplete FilePrompt = id

{- This file promt lets you open files from the xmonad prompt. -}
filePrompt ∷ FPConf → XPConfig → X ()
filePrompt fileConf xpConf =
  mkXPrompt FilePrompt xpConf completePath' (open fileConf)

{- Completes a pathname -}
completePath ∷ String → IO [String]
completePath pathname = 
  ifM (doesFileExist pathname) (return []) $
    ifM (doesDirectoryExist pathname) (completeDir pathname) $
      uncurry completePrefix $ splitFileName pathname
  where
    filterHidden = filter ((/=) '.' . head)
    completeDir pathname =
      sequence . fmap (addTrailingSlashes pathname) =<<
        fmap (pathname </>) . filterHidden <$>
          getDirectoryContents pathname
    completePrefix dirname prefix = ifM (doesDirectoryExist dirname) completeChildren (return [])
      where
        completeChildren =
          sequence . fmap (addTrailingSlashes dirname) =<<
            addDirnames . filterPrefix . filterHidden <$>
              getDirectoryContents dirname
        filterPrefix     = filter (prefix `isPrefixOf`)
        addDirnames      = fmap (dirname </>)
    addTrailingSlashes dirname filename = do
      isDir ← doesDirectoryExist (dirname </> filename)
      return $ if isDir then filename ++ "/" else filename

{- Completes a pathname wrt. to the user's home. -}
completePath' ∷ String → IO [String]
completePath' filename = uncurry fmap . second sort <$> fixHome completePath filename

--------------------------------------------------------------------------------
-- Some utilities, more or less heavily missing from haskell's standard library.

ifM ∷ Monad m ⇒ m Bool → m a → m a → m a
ifM t a b = t >>= \x -> if x then a else b

ifthenelse ∷ (a → Bool) → (a → b) → (a → b) → a → b
ifthenelse test dann sonst = \x → if test x then dann x else sonst x

replace ∷ Eq a ⇒ [a] → [a] → [a] → [a]
replace pattern snippet = aux
  where
    aux [] = []
    aux xs@(x:xs') =
      if pattern `isPrefixOf` xs
         then (snippet ++) . aux $ drop (length pattern) xs
         else x : aux xs'

fileInfo ∷ MonadIO m ⇒ String → m (Maybe (String, Bool)) -- returns (MIME, binary)
fileInfo filename = snd `liftM` fixHome (io . aux) filename
  where
    parseOutput = second (== "; charset=binary") . span (/= ';')
    aux filename = flip catch (const $ return Nothing) $ do
      home ← getHomeDirectory
      let cp = (proc "file" ["-bi", filename]) { std_out = CreatePipe, cwd = Just home }
      (_, Just hout, _, ph) ← createProcess cp
      ifM ((ExitSuccess ==) <$> waitForProcess ph)
         (Just . parseOutput <$> hGetLine hout) 
         (return Nothing)

{- Handle relative paths with respect to the user's home directory. -}  
fixHome ∷ MonadIO m ⇒
  (String → m a) →        -- original function
  String →                -- argument
  m (String → String, a)  -- returns a function to undo the relative directory handling and the result of the original function
fixHome f "" = do
  home ← io getHomeDirectory
  let post = drop (length home + 1)
  (,) post `liftM` f home
fixHome f ('~':'/':filename) = do
  home ← io getHomeDirectory
  let post = ("~" ++) . drop (length home)
  (,) post `liftM` f (home </> filename)
fixHome f filename@('/':_) =
  (,) id `liftM` f filename
fixHome f filename = do
  home ← io getHomeDirectory
  let post = drop (length home + 1)
  (,) post `liftM` f (home </> filename)

