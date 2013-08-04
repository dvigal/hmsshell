{-# LINE 1 "System\Win32\Shell\ShellFunctions.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LINE 2 "System\Win32\Shell\ShellFunctions.hsc" #-}

module System.Win32.Shell.ShellFunctions where

import Foreign.Ptr
import System.Win32.Types
import System.Win32.Shell.NotifyIconData	


{-# LINE 10 "System\Win32\Shell\ShellFunctions.hsc" #-}

{-# LINE 11 "System\Win32\Shell\ShellFunctions.hsc" #-}

foreign import stdcall unsafe "Shell_NotifyIcon"
	c_Shell_NotifyIcon :: DWORD -> Ptr NotifyIconData -> IO Bool

shellNotifyIcon :: DWORD -> Ptr NotifyIconData -> IO Bool	
shellNotifyIcon dwMsg ptr | ptr == nullPtr = error "ptr cannot be null!"
						  | otherwise      = c_Shell_NotifyIcon dwMsg ptr