import System.Directory
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleWindows
import XMonad.Actions.WindowGo
import XMonad.Actions.GridSelect
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Util.EZConfig
import XMonad.Util.CustomKeys

import qualified XMonad.StackSet as S

main = xmonad $ conf
       `additionalKeysP`
       [ ("C-t C-d C-b", withFilePrompt "Pic: " bgFolder setDesktopBackground)
       , ("<Print>", withFilePrompt "Name: " screenshotFolder capToFile)

       , ("C-t C-c", spawn "clickchain")

       , ("C-t C-<Delete>", sudoSpawn "pm-suspend")
       , ("C-t <Delete>", sudoSpawn "pm-hibernate")

       , ("C-t p", spawn "dmenu_run")
       , ("C-t C-p", spawn "dmenu_run")
       , ("C-t <Return>", spawn "xterm")
       , ("C-t e", runOrRaise "emacs" (className =? "Emacs"))
       , ("C-t C-e", runOrRaise "emacs" (className =? "Emacs"))
       , ("C-t b", spawn "chromium --proxy-server=\"socks://localhost:9050\" --incognito")
       , ("C-t C-b", spawn "chromium --proxy-server=\"socks://localhost:9050\" --user-agent=\"Mozilla/5.0 (Windows NT 5.1) AppleWebKit/537.4 (KHTML, like Gecko) Chrome/22.0.1229.94 Safari/537.4\"")

       , ("C-t s", nextWS)
       , ("C-t C-s", nextWS)
       , ("C-t w", toggleWS) -- nextWS and prevWS are also valid
       , ("C-t C-w", toggleWS)
       , ("C-t C-t", windowSwap)
       , ("C-t t", windows S.swapDown)
       , ("C-t C-j", windows S.swapDown)
       , ("C-t j", windows S.focusDown)
       , ("C-t k", windows S.focusUp)
       , ("C-t C-k", windows S.swapUp)
       , ("C-t g", goToSelected defaultGSConfig)

       , ("C-t C-<Space>", sendMessage NextLayout)
       , ("C-t C-h", sendMessage Shrink)
       , ("C-t C-l", sendMessage Expand)
       ]
  where conf = defaultConfig { XMonad.startupHook = onStartup, modMask = mod4Mask }

---------- Config Options
bgFolder = "/home/inaimathi/pictures/backgrounds/"
screenshotFolder = "/home/inaimathi/pictures/screenshots/"

onStartup :: X ()
onStartup = do
  spawn "setxkbmap -layout us -option ctrl:nocaps"
  spawn "set-monitors"
  spawn "emacs"
  spawn "firefox"
  spawn "urxvt -e exit" -- for a feh workaround https://github.com/derf/feh/issues/162
  setDesktopBackground "edge-of-the-world.jpg"

---------- Helper Functions
setDesktopBackground :: MonadIO m => String -> m ()
setDesktopBackground pic = spawn $ concat ["feh --no-xinerama --bg-fill ", bgFolder, pic]

capToFile :: MonadIO m => String -> m ()
capToFile picName = spawn $ concat ["import ", screenshotFolder, picName]

sudoSpawn command = withPrompt "Password" $ run command
  where run command password = spawn $ concat ["echo ", password, " | sudo -S ", command]

---------- Utility
windowSwap = do
  windows S.focusDown
  windows S.swapUp

xpConf = defaultXPConfig { position = Top }

withPrompt prompt fn = inputPrompt xpConf prompt ?+ fn

withCompletingPrompt prompt completions fn =
  inputPromptWithCompl xpConf prompt comp ?+ fn
  where comp = mkComplFunFromList completions

withFilePrompt prompt directory fn = do
  files <- liftIO $ getDirectoryContents directory
  let fs = filter relevant files
      relevant f = '.' /= head f
  withCompletingPrompt prompt fs fn
