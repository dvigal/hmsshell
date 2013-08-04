{-# LANGUAGE ForeignFunctionInterface #-}

module System.Win32.Shell.ShellFunctions where

import Foreign.Ptr
import System.Win32.Types
import System.Win32.Shell.NotifyIconData	

#include "windows.h"
#include "shellapi.h"

foreign import stdcall unsafe "Shell_NotifyIcon"
	c_Shell_NotifyIcon :: DWORD -> Ptr NotifyIconData -> IO Bool

shellNotifyIcon :: DWORD -> Ptr NotifyIconData -> IO Bool	
shellNotifyIcon dwMsg ptr | ptr == nullPtr = error "ptr cannot be null!"
						  | otherwise      = c_Shell_NotifyIcon dwMsg ptr