{-# LANGUAGE ForeignFunctionInterface #-}

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

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

data NotifyIconData = NotifyIconData
	{ 	h_cbSize 				:: !DWORD
	 ,	h_hWnd 					:: !HWND
	 ,	h_uID   				:: !UINT
	 ,	h_uFlags 				:: !UINT
	 ,  h_uCallbackMessage		:: !UINT
	 ,  h_hIcon					:: HICON
#if _WIN32_IE >= 0x0500
	 , 	h_dwState			    :: !DWORD
	 , 	h_dwStateMask			:: !DWORD
#ifdef UNICODE	 
	 , 	h_szInfo			    :: [CWchar]
	 ,	h_szInfoTitle			:: [CWchar]
#else
	 ,	h_szInfo  				:: [CChar]
	 ,	h_szInfoTitle			:: [CChar]
#endif	 	 
	 ,	h_union		 	    	:: !UINT
	 
	 , 	h_dwInfoFlags			:: !DWORD
#endif 
#ifdef UNICODE	 
	 ,  h_szTip					:: [CWchar]
#else
	 ,	h_szTip		 			:: [CChar]	 	 
#endif
-------------------------------------------------	 
#if _WIN32_WINNT >= 0x600	 
	 ,	h_hBalloonIcon			:: HICON	 
#endif
#if _WIN32_IE >= 0x600
  	 ,  h_guidItem	        	:: GUID
#endif 
	} deriving (Show)


#include "windows.h"
#include "shellapi.h"

instance Storable NotifyIconData where
	alignment _ = #{alignment NOTIFYICONDATA}
	sizeOf _ 	= (#size NOTIFYICONDATA)
	peek ptr = do
		c_cbSize 				<- (#peek NOTIFYICONDATA, cbSize) ptr
		c_hWnd 			   		<- (#peek NOTIFYICONDATA, hWnd) ptr
		c_uID 			   		<- (#peek NOTIFYICONDATA, uID) ptr
		c_uFlags 		   		<- (#peek NOTIFYICONDATA, uFlags) ptr
		c_uCallbackMessage 		<- (#peek NOTIFYICONDATA, uCallbackMessage) ptr
		c_hIcon			   		<- (#peek NOTIFYICONDATA, hIcon) ptr
#if _WIN32_IE >= 0x0500		
		let c_szTipOffset 		= (#offset NOTIFYICONDATA, szTip)
		c_szTip         		<- (peekArray 128 (ptr `plusPtr` c_szTipOffset))
		c_dwState		   		<- (#peek NOTIFYICONDATA, dwState) ptr
		c_dwStateMask	   		<- (#peek NOTIFYICONDATA, dwStateMask) ptr
		let c_szInfoOffset 		= (#offset NOTIFYICONDATA, szInfo)
		c_szInfo		   		<- peekArray 256 (ptr `plusPtr` c_szInfoOffset)
		let c_union 				= 0
		let c_szInfoTitleOffset = (#offset NOTIFYICONDATA, szInfoTitle)
		c_szInfoTitle	   	<- (peekArray 64 (ptr `plusPtr` c_szInfoTitleOffset))
		c_dwInfoFlags	   		<- (#peek NOTIFYICONDATA, dwInfoFlags) ptr
#else
		let c_szTipOffset 		= (#offset NOTIFYICONDATA, szTip)
		c_szTip         		<- (peekArray 64 (ptr `plusPtr` c_szTipOffset))
#endif
#if _WIN32_IE >= 0x600		
		c_guidItem 				<- (#peek NOTIFYICONDATA, guidItem) ptr		
#endif		
#if _WIN32_WINNT >= 0x600		
		c_hBalloonIcon 			<- (#peek NOTIFYICONDATA, hBalloonIcon) ptr
#endif		
		return NotifyIconData 
			{ 	h_cbSize  		   = c_cbSize
			 , 	h_hWnd  		   = c_hWnd
			 , 	h_uID 	  		   = c_uID
			 , 	h_uFlags 		   = c_uFlags
			 ,	h_uCallbackMessage = c_uCallbackMessage
			 ,	h_hIcon       	   = c_hIcon
#if _WIN32_IE >= 0x0500			 
			 , 	h_dwState 		   = c_dwState
			 , 	h_dwStateMask      = c_dwStateMask
			 , 	h_szInfo  		   = c_szInfo
			 , 	h_union 		   = c_union
			 , 	h_szInfoTitle  	   = c_szInfoTitle
			 ,  h_dwInfoFlags      = c_dwInfoFlags		 
#endif
#if _WIN32_IE >= 0x600
			 , 	h_guidItem   	   = c_guidItem
#endif
#if _WIN32_WINNT >= 0x600			 
			 ,  h_hBalloonIcon	   = c_hBalloonIcon
#endif			 
			 , 	h_szTip        	   = c_szTip			 
			}

	poke ptr notifyIconData = do
		(#poke NOTIFYICONDATA, cbSize) ptr $ h_cbSize notifyIconData
		(#poke NOTIFYICONDATA, hWnd) ptr $ h_hWnd notifyIconData
		(#poke NOTIFYICONDATA, uID) ptr $ h_uID notifyIconData
		(#poke NOTIFYICONDATA, uFlags) ptr $ h_uFlags notifyIconData
		(#poke NOTIFYICONDATA, uCallbackMessage) ptr $ h_uCallbackMessage notifyIconData
		(#poke NOTIFYICONDATA, hIcon) ptr $ h_hIcon notifyIconData
#if _WIN32_IE >= 0x0500
		(#poke NOTIFYICONDATA, dwState) ptr $ h_dwState notifyIconData
		(#poke NOTIFYICONDATA, dwStateMask) ptr $ h_dwStateMask notifyIconData
		let h_szInfoPtrOffset 	   = ptr `plusPtr` (#offset NOTIFYICONDATA, szInfo)
		pokeArray h_szInfoPtrOffset $ h_szInfo notifyIconData
		let h_szInfoTitlePtrOffset = ptr `plusPtr` (#offset NOTIFYICONDATA, szInfoTitle)
		pokeArray h_szInfoTitlePtrOffset $ h_szInfoTitle notifyIconData
		(#poke NOTIFYICONDATA, dwInfoFlags) ptr $ h_dwInfoFlags	notifyIconData
#endif
#if _WIN32_IE >= 0x600			
		(#poke NOTIFYICONDATA, guidItem) ptr $ h_guidItem notifyIconData
#endif	
#if _WIN32_WINNT >= 0x600		
		(#poke NOTIFYICONDATA, hBalloonIcon) ptr $ h_hBalloonIcon notifyIconData
#endif	
		let h_szTipPtrOffset       = ptr `plusPtr` (#offset NOTIFYICONDATA, szTip)
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
    	h_cbSize = (#size NOTIFYICONDATA)
     ,  h_hWnd = hWnd
     , 	h_uID = uID
     ,  h_uFlags = fromMaybe 0 uFlags
     ,  h_uCallbackMessage = fromMaybe 0 uCallbackMessage
     ,  h_hIcon = hIcon
#if _WIN32_IE >= 0x0500     
     ,  h_dwState = fromMaybe 0 dwState
     ,  h_dwStateMask = fromMaybe 0 dwStateMask
     ,  h_union = 4 -- TODO	
     ,  h_dwInfoFlags = fromMaybe 0 dwInfoFlags
#ifdef UNICODE     
     ,  h_szInfo = charsToCWchars $ maybe' 256 szInfo
     ,  h_szInfoTitle = charsToCWchars $ maybe' 64 szInfoTitle
#else 
	 ,  h_info = charsToCchars $ maybe' 256 szInfo
	 ,  h_szInfoTitle = charsToCchars $ maybe' 64 szInfoTitle
#endif	      
#endif 
#ifdef UNICODE
#if _WIN32_IE >= 0x0500    
     ,  h_szTip = charsToCWchars $ maybe' 128 szTip
#else      
     ,  h_szTip = charsToCWchars $ maybe' 64 szTip
#endif     
#else
#if _WIN32_IE >= 0x0500	
	 ,  h_szTip = charsToCchars $ maybe' 128 szTip
#else 
	 ,  h_szTip = charsToCchars $ maybe' 64 szTip
#endif	 
#endif	      
#if _WIN32_WINNT >= 0x600     
     ,  h_hBalloonIcon = fromMaybe nullPtr hBalloonIcon
#endif
#if _WIN32_IE >= 0x600     
     ,  h_guidItem = fromMaybe (createGuid 0 0 0 [0,0,0,0,0,0,0,0]) guidItem
#endif     
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

#ifdef mingw32_HOST_OS

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

#else /* !mingw32_HOST_OS */

cWcharsToChars xs  = map castCWcharToChar xs
charsToCWchars xs  = map castCharToCWchar xs

-- These conversions only make sense if __STDC_ISO_10646__ is defined
-- (meaning that wchar_t is ISO 10646, aka Unicode)

castCWcharToChar :: CWchar -> Char
castCWcharToChar ch = chr (fromIntegral ch )

castCharToCWchar :: Char -> CWchar
castCharToCWchar ch = fromIntegral (ord ch)

#endif /* !mingw32_HOST_OS */