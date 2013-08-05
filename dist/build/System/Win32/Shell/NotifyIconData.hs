{-# LINE 1 "System\Win32\Shell\NotifyIconData.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LINE 2 "System\Win32\Shell\NotifyIconData.hsc" #-}

module System.Win32.Shell.NotifyIconData where

import Graphics.Win32
import System.Win32.Types
import System.Win32.Mem (zeroMemory)
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal
import Data.Maybe
import Data.Char ( chr, ord )

import Guid


{-# LINE 19 "System\Win32\Shell\NotifyIconData.hsc" #-}

data NotifyIconData = NotifyIconData
	{ 	h_cbSize 				:: !DWORD
	 ,	h_hWnd 					:: !HWND
	 ,	h_uID   				:: !UINT
	 ,	h_uFlags 				:: !UINT
	 ,  h_uCallbackMessage		:: !UINT
	 ,  h_hIcon					:: HICON

{-# LINE 41 "System\Win32\Shell\NotifyIconData.hsc" #-}

{-# LINE 44 "System\Win32\Shell\NotifyIconData.hsc" #-}
	 ,	h_szTip		 			:: [CChar]	 	 

{-# LINE 46 "System\Win32\Shell\NotifyIconData.hsc" #-}
-------------------------------------------------	 

{-# LINE 50 "System\Win32\Shell\NotifyIconData.hsc" #-}

{-# LINE 53 "System\Win32\Shell\NotifyIconData.hsc" #-}
	} deriving (Show)



{-# LINE 57 "System\Win32\Shell\NotifyIconData.hsc" #-}

{-# LINE 58 "System\Win32\Shell\NotifyIconData.hsc" #-}

instance Storable NotifyIconData where
	alignment _ = 2
{-# LINE 61 "System\Win32\Shell\NotifyIconData.hsc" #-}
	sizeOf _ 	= ((88))
{-# LINE 62 "System\Win32\Shell\NotifyIconData.hsc" #-}
	peek ptr = do
		c_cbSize 				<- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr
{-# LINE 64 "System\Win32\Shell\NotifyIconData.hsc" #-}
		c_hWnd 			   		<- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) ptr
{-# LINE 65 "System\Win32\Shell\NotifyIconData.hsc" #-}
		c_uID 			   		<- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr
{-# LINE 66 "System\Win32\Shell\NotifyIconData.hsc" #-}
		c_uFlags 		   		<- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) ptr
{-# LINE 67 "System\Win32\Shell\NotifyIconData.hsc" #-}
		c_uCallbackMessage 		<- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr
{-# LINE 68 "System\Win32\Shell\NotifyIconData.hsc" #-}
		c_hIcon			   		<- ((\hsc_ptr -> peekByteOff hsc_ptr 20)) ptr
{-# LINE 69 "System\Win32\Shell\NotifyIconData.hsc" #-}

{-# LINE 81 "System\Win32\Shell\NotifyIconData.hsc" #-}
		let c_szTipOffset 		= ((24))
{-# LINE 82 "System\Win32\Shell\NotifyIconData.hsc" #-}
		c_szTip         		<- (peekArray 64 (ptr `plusPtr` c_szTipOffset))

{-# LINE 84 "System\Win32\Shell\NotifyIconData.hsc" #-}

{-# LINE 87 "System\Win32\Shell\NotifyIconData.hsc" #-}

{-# LINE 90 "System\Win32\Shell\NotifyIconData.hsc" #-}
		return NotifyIconData 
			{ 	h_cbSize  		   = c_cbSize
			 , 	h_hWnd  		   = c_hWnd
			 , 	h_uID 	  		   = c_uID
			 , 	h_uFlags 		   = c_uFlags
			 ,	h_uCallbackMessage = c_uCallbackMessage
			 ,	h_hIcon       	   = c_hIcon

{-# LINE 105 "System\Win32\Shell\NotifyIconData.hsc" #-}

{-# LINE 108 "System\Win32\Shell\NotifyIconData.hsc" #-}

{-# LINE 111 "System\Win32\Shell\NotifyIconData.hsc" #-}
			 , 	h_szTip        	   = c_szTip			 
			}

	poke ptr notifyIconData = do
		((\hsc_ptr -> pokeByteOff hsc_ptr 0)) ptr $ h_cbSize notifyIconData
{-# LINE 116 "System\Win32\Shell\NotifyIconData.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 4)) ptr $ h_hWnd notifyIconData
{-# LINE 117 "System\Win32\Shell\NotifyIconData.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 8)) ptr $ h_uID notifyIconData
{-# LINE 118 "System\Win32\Shell\NotifyIconData.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 12)) ptr $ h_uFlags notifyIconData
{-# LINE 119 "System\Win32\Shell\NotifyIconData.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 16)) ptr $ h_uCallbackMessage notifyIconData
{-# LINE 120 "System\Win32\Shell\NotifyIconData.hsc" #-}
		((\hsc_ptr -> pokeByteOff hsc_ptr 20)) ptr $ h_hIcon notifyIconData
{-# LINE 121 "System\Win32\Shell\NotifyIconData.hsc" #-}

{-# LINE 130 "System\Win32\Shell\NotifyIconData.hsc" #-}

{-# LINE 133 "System\Win32\Shell\NotifyIconData.hsc" #-}

{-# LINE 136 "System\Win32\Shell\NotifyIconData.hsc" #-}
		let h_szTipPtrOffset       = ptr `plusPtr` ((24))
{-# LINE 137 "System\Win32\Shell\NotifyIconData.hsc" #-}
		pokeArray h_szTipPtrOffset $ h_szTip notifyIconData		
		return ()

emptyNotifyIconData :: IO NotifyIconData
emptyNotifyIconData = do
	ptr <- malloc::IO (Ptr NotifyIconData)
	let szt = (fromIntegral (88))::DWORD
{-# LINE 144 "System\Win32\Shell\NotifyIconData.hsc" #-}
	zeroMemory ptr szt
	peek ptr


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
{-# LINE 166 "System\Win32\Shell\NotifyIconData.hsc" #-}
     ,  h_hWnd = hWnd
     , 	h_uID = uID
     ,  h_uFlags = fromMaybe 0 uFlags
     ,  h_uCallbackMessage = fromMaybe 0 uCallbackMessage
     ,  h_hIcon = hIcon

{-# LINE 184 "System\Win32\Shell\NotifyIconData.hsc" #-}

{-# LINE 191 "System\Win32\Shell\NotifyIconData.hsc" #-}

{-# LINE 194 "System\Win32\Shell\NotifyIconData.hsc" #-}
	 ,  h_szTip = charsToCchars $ maybe' 64 szTip

{-# LINE 196 "System\Win32\Shell\NotifyIconData.hsc" #-}

{-# LINE 197 "System\Win32\Shell\NotifyIconData.hsc" #-}

{-# LINE 200 "System\Win32\Shell\NotifyIconData.hsc" #-}

{-# LINE 203 "System\Win32\Shell\NotifyIconData.hsc" #-}
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


{-# LINE 219 "System\Win32\Shell\NotifyIconData.hsc" #-}

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

