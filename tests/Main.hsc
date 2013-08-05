module Main where

import Foreign.Storable (peek)
import Foreign.Ptr
import Foreign.Marshal.Alloc

import System.Win32.Types (nullPtr)

import System.Win32.Shell.NotifyIconData
import Test.HUnit

emptyNotifyIconDataTest = TestCase (do
		empty <- emptyNotifyIconData
		let hWnd = h_hWnd empty
		assertEqual "hWnd is null" nullPtr hWnd
	)

szTipSizeTest = TestCase (do
		empty <- emptyNotifyIconData
#ifdef UNICODE
		assertEqual "UNICODE. szTip size equal to" 128 $ length $ h_szTip empty
#else 
		assertEqual "NOT UNICODE. szTip size equal to" 64 $ length $ h_szTip empty
#endif						
	)

tests = TestList 
	[
	  TestLabel "emptyNotifyIconDataTest" emptyNotifyIconDataTest
	, TestLabel "szTipSizeTest" szTipSizeTest
	]

main = do
	runTestTT tests

