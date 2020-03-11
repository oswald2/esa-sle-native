module Data.SLE.ServiceInstanceID
  ( rsp
  , cltu
  , spack
  , rcf
  , tcva
  , rslFg
  , raf
  , fslFg
  , fsp
  , sagr
  , rocf
  , tcf
  , rcfsh
  , ServiceInstanceAttribute(..)
  , serviceInstanceAttribute
  , ServiceInstanceIdentifier(..)
  , serviceInstanceIdentifier
  )
where

import           RIO

import           Data.ASN1.OID
import           Data.ASN1.Types

import           Data.SLE.Common

rsp :: OID
rsp = [1, 3, 112, 4, 3, 1, 2, 40]

cltu :: OID
cltu = [1, 3, 112, 4, 3, 1, 2, 7]

spack :: OID
spack = [1, 3, 112, 4, 3, 1, 2, 53]

rcf :: OID
rcf = [1, 3, 112, 4, 3, 1, 2, 46]

tcva :: OID
tcva = [1, 3, 112, 4, 3, 1, 2, 16]

rslFg :: OID
rslFg = [1, 3, 112, 4, 3, 1, 2, 38]

raf :: OID
raf = [1, 3, 112, 4, 3, 1, 2, 22]

fslFg :: OID
fslFg = [1, 3, 112, 4, 3, 1, 2, 14]

fsp :: OID
fsp = [1, 3, 112, 4, 3, 1, 2, 10]

sagr :: OID
sagr = [1, 3, 112, 4, 3, 1, 2, 52]

rocf :: OID
rocf = [1, 3, 112, 4, 3, 1, 2, 49]

tcf :: OID
tcf = [1, 3, 112, 4, 3, 1, 2, 12]

rcfsh :: OID
rcfsh = [1, 3, 112, 4, 3, 1, 2, 44]


data ServiceInstanceAttribute = ServiceInstanceAttribute {
  _siAttrID :: OID
  , _siAttrValue :: Text
  }

serviceInstanceAttribute :: ServiceInstanceAttribute -> [ASN1]
serviceInstanceAttribute ServiceInstanceAttribute {..} =
  [ Start Set
  , Start Sequence
  , OID _siAttrID
  , visibleString _siAttrValue
  , End Sequence
  , End Set
  ]


newtype ServiceInstanceIdentifier = ServiceInstanceIdentifier {
  _siIDs :: [ServiceInstanceAttribute]
  }


serviceInstanceIdentifier :: ServiceInstanceIdentifier -> [ASN1]
serviceInstanceIdentifier ServiceInstanceIdentifier {..} = 
  Start Sequence : concatMap serviceInstanceAttribute _siIDs ++ [End Sequence]