import           Data.Word
import           Foreign.C.Types
import           Foreign.Ptr
import           System.Posix.Types

foreign import ccall "read"
  c_read :: CInt
         -> Ptr Word8
         -> CSize
         -> CSsize
