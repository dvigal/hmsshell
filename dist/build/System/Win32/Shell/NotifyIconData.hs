{-# LINE 1 "System\Win32\Shell\NotifyIconData.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LINE 2 "System\Win32\Shell\NotifyIconData.hsc" #-}

module System.Win32.Shell.NotifyIconData 
	( NotifyIconData
	, createNotifyIcon0
	, createNotifyIcon1
	) where

import Graphics.Win32
import System.Win32.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal
import Foreign.Marshal.Array (peekArray, pokeArray)
import Data.Maybe
import Data.Char ( chr, ord )

import Guid (GUID, createGuid)


{-# LINE 23 "System\Win32\Shell\NotifyIconData.hsc" #-}

data NotifyIconData = NotifyIconData
	{ 	h_cbSize 				:: !DWORD
	 ,	h_hWnd 					:: !HWND
	 ,	h_uID   				:: !UINT
	 ,	h_uFlags 				:: !UINT
	 ,  h_uCallbackMessage		:: !UINT
	 ,  h_hIcon					:: HICON

{-# LINE 45 "System\Win32\Shell\NotifyIconData.hsc" #-}

{-# LINE 48 "System\Win32\Shell\NotifyIconData.hsc" #-}
	 ,	h_szTip		 			:: [CChar]	 	 

{-# LINE 50 "System\Win32\Shell\NotifyIconData.hsc" #-}
-------------------------------------------------	 

{-# LINE 54 "System\Win32\Shell\NotifyIconData.hsc" #-}

{-# LINE 57 "System\Win32\Shell\NotifyIconData.hsc" #-}
	} deriving (Show)



{-# LINE 61 "System\Win32\Shell\NotifyIconData.hsc" #-}

{-# LINE 62 "System\Win32\Shell\NotifyIconData.hsc" #-}

instance Storable NotifyIconData where
	alignment _ = 2
{-# LINE 65 "System\Win32\Shell\NotifyIconData.hsc" #-}
	sizeOf _ 	= ((88))
{-# LINE 66 "System\Win32\Shell\NotifyIconData.hsc" #-}
	peek ptr = do
		c_cbSize 				<- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 68 "System\Win32\Shell\NotifyIconData.hsc" #-}
		c_hWnd 			   		<- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) ptr
{-# LINE 69 "System\Win32\Shell\NotifyIconData.hsc" #-}
		c_uID 			   		<- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 70 "System\Win32\Shell\NotifyIconData.hsc" #-}
		c_uFlags 		   		<- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) ptr
{-# LINE 71 "System\Win32\Shell\NotifyIconData.hsc" #-}
		c_uCallbackMessage 		<- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr
{-# LINE 72 "System\Win32\Shell\NotifyIconData.hsc" #-}
		c_hIcon			   		<- ((\hsc_ptr -> peekByteOff hsc_ptr 20)) ptr
{-# LINE 73 "System\Win32\Shell\NotifyIconData.hsc" #-}

{-# LINE 85 "System\Win32\Shell\NotifyIconData.hsc" #-}
		let c_szTipOffset 		= ((24))
{-# LINE 86 "System\Win32\Shell\NotifyIconData.hsc" #-}
		c_szTip         		<- (peekArray 64 (ptr `plusPtr` c_szTipOffset))

{-# LINE 88 "System\Win32\Shell\NotifyIconData.hsc" #-}

{-# LINE 91 "System\Win32\Shell\NotifyIconData.hsc" #-}

{-# LINE 94 "System\Win32\Shell\NotifyIconData.hsc" #-}
		return NotifyIconData 
			{ 	h_cbSize  		   = c_cbSize
			 , 	h_hWnd  		   = c_hWnd
			 , 	h_uID 	  		   = c_uID
			 , 	h_uFlags 		   = c_uFlags
			 ,	h_uCallbackMessage = c_uCallbackMessage
			 ,	h_hIcon       	   = c_hIcon

{-# LINE 109 "System\Win32\Shell\NotifyIconData.hsc" #-}

{-# LINE 112 "System\Win32\Shell\NotifyIconData.hsc" #-}

{-# LINE 115 "System\Win32\Shell\NotifyIconData.hsc" #-}
			 , 	h_szTip        	   = c_szTip			 
			}

	poke ptr notifyIconData = do
		((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr $ h_cbSize notifyIconData
{-# LINE 120 "System\Win32\Shell\NotifyIconData.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr $ h_hWnd notifyIconData
{-# LINE 121 "System\Win32\Shell\NotifyIconData.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr $ h_uID notifyIconData
{-# LINE 122 "System\Win32\Shell\NotifyIconData.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 12)) ptr $ h_uFlags notifyIconData
{-# LINE 123 "System\Win32\Shell\NotifyIconData.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 16)) ptr $ h_uCallbackMessage notifyIconData
{-# LINE 124 "System\Win32\Shell\NotifyIconData.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 20)) ptr $ h_hIcon notifyIconData
{-# LINE 125 "System\Win32\Shell\NotifyIconData.hsc" #-}

{-# LINE 134 "System\Win32\Shell\NotifyIconData.hsc" #-}

{-# LINE 137 "System\Win32\Shell\NotifyIconData.hsc" #-}

{-# LINE 140 "System\Win32\Shell\NotifyIconData.hsc" #-}
		let h_szTipPtrOffset       = ptr `plusPtr` ((24))
{-# LINE 141 "System\Win32\Shell\NotifyIconData.hsc" #-}
		pokeArray h_szTipPtrOffset $ h_szTip notifyIconData		
		return ()

createNotifyIcon0 :: HWND -> UINT -> HICON -> UINT -> NotifyIconData
createNotifyIcon0 hWnd uID hIcon uFlags = createNotifyIcon1 hWnd uID hIcon (Just uFlags)  Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 

createNotifyIcon1 :: HWND -> UINT -> HICON -> Maybe UINT -> Maybe UINT -> Maybe DWORD -> Maybe DWORD -> Maybe String -> Maybe String -> Maybe DWORD -> Maybe String -> Maybe HICON -> Maybe GUID -> NotifyIconData

createNotifyIcon1 hWnd uID hIcon
			   uFlags 
			   uCallbackMessage
			   dwState
			   dwStateMask
			   szInfo
			   szInfoTitle
			   dwInfoFlags
			   szTip 
			   hBalloonIcon 
			   guidItem  = NotifyIconData
    {
    	h_cbSize = ((88))
{-# LINE 162 "System\Win32\Shell\NotifyIconData.hsc" #-}
     ,  h_hWnd = hWnd
     , 	h_uID = uID
     ,  h_uFlags = fromMaybe 0 uFlags
     ,  h_uCallbackMessage = fromMaybe 0 uCallbackMessage
     ,  h_hIcon = hIcon

{-# LINE 180 "System\Win32\Shell\NotifyIconData.hsc" #-}

{-# LINE 187 "System\Win32\Shell\NotifyIconData.hsc" #-}

{-# LINE 190 "System\Win32\Shell\NotifyIconData.hsc" #-}
	 ,  h_szTip = charsToCchars $ maybe' 64 szTip

{-# LINE 192 "System\Win32\Shell\NotifyIconData.hsc" #-}

{-# LINE 193 "System\Win32\Shell\NotifyIconData.hsc" #-}

{-# LINE 196 "System\Win32\Shell\NotifyIconData.hsc" #-}

{-# LINE 199 "System\Win32\Shell\NotifyIconData.hsc" #-}
	}
	where maybe' n src = maybe [] (\l -> take n l) src 				  				  		

charsToCchars :: String -> [CChar]
charsToCchars str = map (\ch -> castCharToCChar ch) str

ccharsToString :: [CChar] -> String
ccharsToString cchars = map (\ch -> castCCharToChar ch) cchars

wNUL :: CWchar
wNUL = 0

cWcharsToChars :: [CWchar] -> [Char]
charsToCWchars :: [Char] -> [CWchar]


{-# LINE 215 "System\Win32\Shell\NotifyIconData.hsc" #-}

-- On Windows, wchar_t is 16 bits wide and CWString uses the UTF-16 encoding.

-- coding errors generate Chars in the surrogate range
cWcharsToChars = map chr . fromUTF16 . map fromIntegral
 where
  fromUTF16 (c1:c2:wcs)
    | 0xd800 <= c1 && c1 <= 0xdbff && 0xdc00 <= c2 && c2 <= 0xdfff =
      ((c1 - 0xd800)*0x400 + (c2 - 0xdc00) + 0x10000) : fromUTF16 wcs
  fromUTF16 (c:wcs) = c : fromUTF16 wcs
  fromUTF16 [] = []

charsToCWchars = foldr utf16Char [] . map ord
 where
  utf16Char c wcs
    | c < 0x10000 = fromIntegral c : wcs
    | otherwise   = let c' = c - 0x10000 in
                    fromIntegral (c' `div` 0x400 + 0xd800) :
                    fromIntegral (c' `mod` 0x400 + 0xdc00) : wcs

