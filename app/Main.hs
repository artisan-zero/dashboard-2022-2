{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

---------------------------------------------------------------------
-- App             : Linux Dashboard
-- Author          : Willian A. Salles
---------------------------------------------------------------------
-- Dashboard para visualização de informações do
-- sistema operacional Linux.

-- App criado para a disciplina de Sistemas Operacionais 
-- da UTFPR, 2022-2.

-- Projeto em estágio inicial.
--  Visualizações:
--   - Numero de processos ativos
--   - Uso de memoria
---------------------------------------------------------------------
module Main where

import Prelude                   hiding (error)
import Commands


import System.IO.Error

import Data.Text                        ( pack   )
import Text.Printf                      ( printf )

import Control.Concurrent               ( threadDelay )
import Control.Monad                    ( void )
import Control.Monad.Trans.Except


import GI.Gtk                           ( Window(..)
                                        , Box(..)
                                        , Label(..)
                                        , Button(..)
                                        , Orientation(..)
                                        )
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple

import Pipes



---------------------Data----------------------------

data State = State
  { processCount :: Either Error ProcessCount
  , memoryUsage :: Either Error MemoryUsage
  }

data Event
  = Closed
  | UpdateProcess (Either Error ProcessCount)
  | UpdateMemory (Either Error MemoryUsage)


----------------- MVC -------------------------------
view' :: State -> AppView Window Event
view' State {..} =
  bin
    Window
    [ #title := "Dashboard"
    , on #deleteEvent (const (True, Closed))
    , #widthRequest := 50
    , #heightRequest := 900
    ]
  $ container
      Box
      [ #orientation := OrientationVertical]
      [ BoxChild defaultBoxChildProperties $ button Closed processCount
      , BoxChild defaultBoxChildProperties $ button Closed memoryUsage
      ]


update' :: State -> Event -> Transition State Event
update' (state@State {..}) (UpdateProcess process)
  = Transition state { processCount = process } $ (return Nothing)
update' (state@State {..}) (UpdateMemory memory)
  = Transition state { memoryUsage = memory } $ (return Nothing)
update' _ Closed = Exit



-- App --
main :: IO ()
main = void $ run
  App { view         = view'
      , update       = update'
      , inputs       = [updateAll]
      , initialState = State (Left $ error "Nothing here yet") $ (Left $ error "Nothing here")
      }

-- Producers that run unix commands to update the state
updateAll :: Producer Event IO ()
updateAll = every (Select (yield =<< (lift $ UpdateMemory <$> (runExceptT getMemoryUsage)))
                   <> Select (yield =<< (lift $ UpdateProcess <$> (runExceptT getProcessCount)))
                   <> Select (lift $ threadDelay 2000000))
            <* updateAll


-- A class that can create a Widget Button from data
class ToButton a where
  button :: event -> a -> Widget event

instance (ToButton a, ToButton b) => ToButton (Either a b) where
  button event dunno =
    either (button event) (button event) dunno

instance ToButton ProcessCount where
  button event (ProcessCount count) =
    widget Button
    [ on #clicked event
    , #label := pack pcountLabel ]
    where
      pcountLabel
        = printf "Processes:\n %d" count

instance ToButton MemoryUsage where
  button event m =
    widget Button
    [ on #clicked event
    , #label := pack memLabel ]
    where
      memLabel
        = printf " Memory\nTotal: %s\n Free: %s\n  %.2f %%"
        (show . dtSize $ total m)
        (show . dtSize $ avail m)
        (100 * usedRate m)

instance ToButton Error where
  button event (Error str) =
    widget Button
    [ on #clicked event
    , #label := pack ("Error " ++ str) ]


-- A datatype and helper functions to represent the data size
data DataSize'
  = KB
  | MB
  | GB
  | TB
  deriving (Show, Enum)


newtype DataSize = DataSize (Float, DataSize')


dtSize :: Float -> DataSize
dtSize i = usageNaming $ DataSize (i, KB)


usageNaming :: DataSize -> DataSize
usageNaming (DataSize (x, TB)) = DataSize (x, TB)
usageNaming (DataSize (i, dsize))
  | i > 1024 = usageNaming $ DataSize (i / 1024, succ dsize)
  | otherwise = DataSize (i, dsize)


instance Show DataSize where
  show (DataSize (i, dsize)) = show (truncate' i 2) ++ " " ++ show dsize


truncate' :: Float -> Int -> Float
truncate' x n = (fromIntegral (floor (x * t))) / t
    where t = 10^n
