{-# LANGUAGE UnicodeSyntax #-}
module XMonad.Prompt.FilePrompt 
  ( FPConf (..)
  , filePrompt
  , kdeOpen
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
import System.Process      (readProcess)
import System.FilePath     (pathSeparator, splitFileName, (</>))
import System.Directory    (getHomeDirectory, doesDirectoryExist, doesFileExist)

--------------------------------------------------------------------------------
-- First, some function to open files at in the end.

{- Directly specify a command to open a file. The command must contain `%f' as a placeholder for the filename -}
openWith ∷ MonadIO m ⇒ String → String → m ()
-- openWith programStr filename = snd `liftM` fixHome (spawn . replace "%f" filename) filename
openWith program filename = snd `liftM` fixHome run filename
  where run filename' = spawn (replace "%f" filename' program)

{- Extend a generic file opener by custom opener specified my extension, MIME string and binarity of encoding. -}
customizeOpen ∷ MonadIO m ⇒
  (Maybe String → String → Bool → Maybe (String → m ())) -- map extension, if any, the MIME type and the binarity to maybe an X to open a file
  → (String → m ())                                      -- fallback to open files
  → String → m ()
customizeOpen custom_open fallback pathname = do
  (mime, binary) ← fileInfo pathname
  let filename  = snd . splitFileName $ pathname
      extension = if '.' `elem` filename
                    then Just . reverse . takeWhile (/= '.') . reverse $ filename
                    else Nothing
      open = fromMaybe fallback $ custom_open extension mime binary
  open pathname

{- Specify programs to open files, by MIME -}
mimeOpen ∷ MonadIO m ⇒ [(String, String)] → String → String → String → m ()
mimeOpen mimeToProgram textViewer binaryViewer filename = do
  (mime, binary) ← fileInfo filename
  let program       = fromMaybe defaultViewer $ lookup filename mimeToProgram
      defaultViewer = if binary then binaryViewer else textViewer
  openWith program filename

{- Open files the KDE4 way. -}
kdeOpen ∷ MonadIO m ⇒ String → m ()
kdeOpen = openWith "kioclient exec file:'%f'" 

--------------------------------------------------------------------------------
-- The definition of the file prompt.

data FPConf = FPConf { open ∷ String → X () }

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

completePath' ∷ String → IO [String]
completePath' filename = appendOriginal . uncurry fmap . second sort <$> fixHome completePath filename
  where appendOriginal = if filename /= "" then (filename :) else id

--------------------------------------------------------------------------------
-- Some utilities, more or less heavily missing from the haskell standard API.

ifM t a b = t >>= \x -> if x then a else b

replace ∷ Eq a ⇒ [a] → [a] → [a] → [a]
replace pattern snippet = aux
  where
    aux [] = []
    aux xs@(x:xs') =
      if pattern `isPrefixOf` xs
         then (snippet ++) . aux $ drop (length pattern) xs
         else x : aux xs'

fileInfo ∷ MonadIO m ⇒ String → m (String, Bool) -- returns (MIME, binary)
fileInfo filename = snd `liftM` fixHome aux filename
  where
    aux filename = io $ readOutput <$> readProcess "file" ["-bi", filename] ""
    readOutput = second (== "; charset=binary\n") . span (/= ';')

{- Handle relative paths with respect to the user's home directory. -}  
fixHome ∷ MonadIO m ⇒
  (String → m a)            -- original function
  → String                  -- argument
  → m (String → String, a)  -- returns a function to undo the relative directory handling and the result of the original function
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


