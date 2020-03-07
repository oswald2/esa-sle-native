module Data.SLE.ServiceInstanceID
  ()
where

import           RIO

import           Data.Primitive.PrimArray

import           Language.Asn.Types


rsp :: ObjectIdentifier
rsp = ObjectIdentifier $ primArrayFromList [1, 3, 112, 4, 3, 1, 2, 40]
cltu :: ObjectIdentifier
cltu = ObjectIdentifier $ primArrayFromList[1, 3, 112, 4, 3, 1, 2, 7]
spack :: ObjectIdentifier
spack = ObjectIdentifier $ primArrayFromList[1, 3, 112, 4, 3, 1, 2, 53]
rcf :: ObjectIdentifier
rcf = ObjectIdentifier $ primArrayFromList[1, 3, 112, 4, 3, 1, 2, 46]
tcva :: ObjectIdentifier
tcva = ObjectIdentifier $ primArrayFromList[1, 3, 112, 4, 3, 1, 2, 16]
rslFg :: ObjectIdentifier
rslFg = ObjectIdentifier $ primArrayFromList[1, 3, 112, 4, 3, 1, 2, 38]
raf :: ObjectIdentifier
raf = ObjectIdentifier $ primArrayFromList[1, 3, 112, 4, 3, 1, 2, 22]
fslFg :: ObjectIdentifier
fslFg = ObjectIdentifier $ primArrayFromList[1, 3, 112, 4, 3, 1, 2, 14]
fsp :: ObjectIdentifier
fsp = ObjectIdentifier $ primArrayFromList[1, 3, 112, 4, 3, 1, 2, 10]
sagr :: ObjectIdentifier
sagr = ObjectIdentifier $ primArrayFromList[1, 3, 112, 4, 3, 1, 2, 52]
rocf :: ObjectIdentifier
rocf = ObjectIdentifier $ primArrayFromList[1, 3, 112, 4, 3, 1, 2, 49]
tcf :: ObjectIdentifier
tcf = ObjectIdentifier $ primArrayFromList[1, 3, 112, 4, 3, 1, 2, 12]
rcfsh :: ObjectIdentifier
rcfsh = ObjectIdentifier $ primArrayFromList[1, 3, 112, 4, 3, 1, 2, 44]
