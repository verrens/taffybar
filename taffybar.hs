import System.Environment (lookupEnv)

import System.Taffybar
import System.Taffybar.Systray
import System.Taffybar.SimpleClock
import System.Taffybar.Widgets.PollingGraph
import System.Taffybar.Widgets.VerticalBar
import System.Information.CPU
import System.Taffybar.TaffyPager
import System.Information.Memory
import System.Taffybar.Widgets.PollingLabel
import System.Taffybar.NetMonitor
import System.Taffybar.DiskIOMonitor
import System.Taffybar.Pager
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.Battery

import Debug.Trace


main = do
  scr <- maybe 0 read <$> lookupEnv "TAFFY_SCREEN"
  defaultTaffybar defaultTaffybarConfig
    { monitorNumber = scr
    , barHeight = 20
    , widgetSpacing = 3
    , startWidgets = reverse leftWidgets
    , endWidgets = reverse $ if scr == 0 then rightWidgets else rightWidgetsAlt
    }
  where

  leftWidgets = [ pager ]
  rightWidgets = [ notify, tray, netWifi, netEth, disk, cpu, bat, clock ]
  rightWidgetsAlt = [ notify, clock ]
  
  clock = textClockNew Nothing "<span fgcolor='#ee9a00'>%H:%M:%S</span>" 1

  tray = systrayNew

  notify = notifyAreaNew defaultNotificationConfig
    { notificationMaxLength = 128
    , notificationMaxTimeout = 600
    }

  cpu = pollingGraphNew cpuCfg 1 cpuCallback where
    cpuCallback = do
      (_, systemLoad, totalLoad) <- cpuLoad
      return [ totalLoad, systemLoad ]
    cpuCfg = defaultGraphConfig
      { graphDataColors = [ (0, 1, 0, 1), (1, 0, 1, 0.5)]
      , graphLabel = Nothing -- Just "cpu"
      , graphDirection = RIGHT_TO_LEFT
      , graphBorderColor = (0.23, 0.23, 0.23)
      }

  disk = dioMonitorNew gCfg 1 "sda" where
    gCfg = defaultGraphConfig
      { graphDataColors = [ (0, 1, 0, 1), (1, 0, 1, 0.5)]
      , graphLabel = Nothing -- Just "cpu"
      , graphDirection = RIGHT_TO_LEFT
      , graphBorderColor = (0.23, 0.23, 0.23)
      }

  bat = batteryBarNew batCfg tmpl 5 where
    tmpl i
      -- | seconds i < 120 =
      --   "<span fgcolor='red'>$time$</span>"
      | charging i =
        "<span fgcolor='green'>$time$</span>"
      | p < 11 =
        "<span fgcolor='red'>$time$</span>"
      | p < 20 =
        "<span fgcolor='yellow'>$time$</span>"
      | otherwise =
        "<span fgcolor='gray70'>$time$</span>"
      where p = percent i
    batCfg = (defaultBarConfig barColor)
      { barBorderColor = (0.23, 0.23, 0.23)
      }
    barColor p
      | p < 0.11 = (1, 0, 0)
      | p < 0.20 = (1, 1, 0)
      | p < 0.90 = (0.7, 0.7, 0.7)
      | otherwise = (0, 1, 0)
  -- batText = textBatteryNew "$time$" 3


  netEth = netMonitorNew 1 "eth0"
  netWifi = netMonitorNew 1 "wlan0"


--   net = pollingGraphNew cfg 0.5 $ getNetInfo "eth0" where
--     cfg = defaultGraphConfig
--       { graphDataColors = [ (0, 1, 0, 1), (1, 0, 1, 0.5)]
--       , graphLabel = Nothing -- Just "cpu"
--       }

  mem = pollingGraphNew memCfg 1 memCallback where
    memCallback = do
      mi <- parseMeminfo
      return [memoryUsedRatio mi]
    memCfg = defaultGraphConfig
      { graphDataColors = [(1, 0, 0, 1)]
      , graphLabel = Just "mem"
      }
  pager = taffyPagerNew PagerConfig
    { activeWindow     = escape . shorten 128
    , activeLayout     = lay
    , activeWorkspace  = wspace "white" "gray40"
    , hiddenWorkspace  = wspace "gray80" ""
    , emptyWorkspace   = wspace "gray50" ""
    , visibleWorkspace = wspace "gray80" "gray20"
    , urgentWorkspace  = wspace "white" "gray60"
    , widgetSep        = ""
    } where
    lay l = [ ' ' , case l of
      "ResizableTall" -> '│'
      "Mirror ResizableTall" -> '─'
      "Tab" -> '═', ' ' ]
    wspace _ _ "NSP" = []
    wspace fg bg nm = colorize fg bg $ escape nm
