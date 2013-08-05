module Main where

import Foreign.Marshal.Alloc

import System.Win32.Types (nullPtr)

import System.Win32.Shell.NotifyIconData
import Test.HUnit

peekNotifyIconDataTest = TestCase (do
		ptr <- malloc::(Ptr NotifyIconData)
		notifyIconData < peek ptr
		let hWnd = h_hWnd notifyIconData
		assertEqual "hWnd is null" hWnd nullPtr
	)

tests = TestList [TestLabel "peekNotifyIconDataTest" peekNotifyIconDataTest]

main = do
	runTestTT tests

