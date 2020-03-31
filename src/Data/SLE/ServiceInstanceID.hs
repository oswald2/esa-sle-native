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
  , getServiceInstanceIdentifier
  , SerID(..)
  )
where

import           RIO
import           RIO.State
import           Control.Monad.Except
import           RIO.List
import           Data.ASN1.Types

import           Data.SLE.Common




rsp :: ASN1
rsp = OID [1, 3, 112, 4, 3, 1, 2, 40]

cltu :: ASN1
cltu = OID [1, 3, 112, 4, 3, 1, 2, 7]

spack :: ASN1
spack = OID [1, 3, 112, 4, 3, 1, 2, 53]

rcf :: ASN1
rcf = OID [1, 3, 112, 4, 3, 1, 2, 46]

tcva :: ASN1
tcva = OID [1, 3, 112, 4, 3, 1, 2, 16]

rslFg :: ASN1
rslFg = OID [1, 3, 112, 4, 3, 1, 2, 38]

raf :: ASN1
raf = OID [1, 3, 112, 4, 3, 1, 2, 22]

fslFg :: ASN1
fslFg = OID [1, 3, 112, 4, 3, 1, 2, 14]

fsp :: ASN1
fsp = OID [1, 3, 112, 4, 3, 1, 2, 10]

sagr :: ASN1
sagr = OID [1, 3, 112, 4, 3, 1, 2, 52]

rocf :: ASN1
rocf = OID [1, 3, 112, 4, 3, 1, 2, 49]

tcf :: ASN1
tcf = OID [1, 3, 112, 4, 3, 1, 2, 12]

rcfsh :: ASN1
rcfsh = OID [1, 3, 112, 4, 3, 1, 2, 44]


data SerID =
  RSP
  | FCLTU
  | SPACK
  | RCF
  | TCVA
  | RSLFG
  | RAF
  | FSLFG
  | FSP
  | SAGR
  | ROCF
  | TCF
  | RCFSH
  deriving (Eq, Ord, Enum, Show)

toOid :: SerID -> ASN1
toOid RSP   = rsp
toOid FCLTU = cltu
toOid SPACK = spack
toOid RCF   = rcf
toOid TCVA  = tcva
toOid RSLFG = rslFg
toOid RAF   = raf
toOid FSLFG = fslFg
toOid FSP   = fsp
toOid SAGR  = sagr
toOid ROCF  = rocf
toOid TCF   = tcf
toOid RCFSH = rcfsh

fromOid :: ASN1 -> Maybe SerID
fromOid (OID [_x1, _x2, _x3, _x4, _x5, _x6, _x7, x]) = 
  if | x == 40 -> Just RSP 
     | x == 7 -> Just FCLTU 
     | x == 53 -> Just SPACK 
     | x == 46 -> Just RCF 
     | x == 16 -> Just TCVA 
     | x == 38 -> Just RSLFG 
     | x == 22 -> Just RAF 
     | x == 14 -> Just FSLFG 
     | x == 10 -> Just FSP 
     | x == 52 -> Just SAGR 
     | x == 49 -> Just ROCF 
     | x == 12 -> Just TCF 
     | x == 44 -> Just RCFSH 
     | otherwise -> Nothing 
fromOid _ = Nothing  



data ServiceInstanceAttribute = ServiceInstanceAttribute {
  _siAttrID :: SerID
  , _siAttrValue :: Text
  }

serviceInstanceAttribute :: ServiceInstanceAttribute -> [ASN1]
serviceInstanceAttribute ServiceInstanceAttribute {..} =
  [ Start Set
  , Start Sequence
  , toOid _siAttrID
  , visibleString _siAttrValue
  , End Sequence
  , End Set
  ]

getServiceInstanceAttribute :: [ASN1] -> (Maybe ServiceInstanceAttribute, [ASN1])
getServiceInstanceAttribute (Start Set : Start Sequence : oid : str : End Sequence : End Set : rest) = 
  let sia = do ServiceInstanceAttribute <$> fromOid oid <*> getVisibleString str
  in 
  (sia, rest)
getServiceInstanceAttribute x = (Nothing, x)


newtype ServiceInstanceIdentifier = ServiceInstanceIdentifier {
  _siIDs :: [ServiceInstanceAttribute]
  }


serviceInstanceIdentifier :: ServiceInstanceIdentifier -> [ASN1]
serviceInstanceIdentifier ServiceInstanceIdentifier {..} =
  Start Sequence : concatMap serviceInstanceAttribute _siIDs <> [End Sequence]


getServiceInstanceIdentifier :: [ASN1] -> (Maybe ServiceInstanceIdentifier, [ASN1])
getServiceInstanceIdentifier full@(Start Sequence : sias) =
  loop sias []
  where 
    loop ls acc = 
      case getServiceInstanceAttribute ls of 
        (Nothing, End Sequence : rest) -> (Just (ServiceInstanceIdentifier (reverse acc)), rest) 
        (Nothing, _rest) -> (Nothing, full)
        (Just attr, rest) -> loop rest (attr : acc)
getServiceInstanceIdentifier full = (Nothing, full)


type Parser a = ExceptT Text (State [ASN1]) a


parseASN1 :: (ASN1 -> Bool) -> (ASN1 -> a) -> Parser a
parseASN1 p f = do 
  x <- get 
  case x of 
    (val : rest) -> if p val 
      then do 
        put rest 
        return (f val)
      else throwError "parseASN1: Predicate did not match"
    _ -> throwError "parseASN1: list empty, could not parse value"


parseStartSequence :: Parser () 
parseStartSequence = parseASN1 (== Start Sequence) (const ())

parseEndSequence :: Parser () 
parseEndSequence = parseASN1 (== End Sequence) (const ())

between :: Parser a -> Parser b -> Parser c -> Parser c 
between begin end bet = do 
  begin 
  val <- bet
  end 
  return val 

manyA :: Parser a -> Parser [a]
manyA p = loop []
  where 
    loop acc = do 
      action acc `catchError` const (return (reverse acc))
    
    action acc = do 
      val <- p 
      loop (val : acc)

parseSequence :: Parser e -> Parser [e]
parseSequence p = do 
  between parseStartSequence parseEndSequence (manyA p)


parseStartSet :: Parser () 
parseStartSet = parseASN1 (== Start Set) (const ())