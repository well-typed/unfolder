{- This file was auto-generated from kvstore.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.Kvstore (
        KeyValueService(..), CreateRequest(), CreateResponse(),
        DeleteRequest(), DeleteResponse(), RetrieveRequest(),
        RetrieveResponse(), UpdateRequest(), UpdateResponse()
    ) where
import qualified Data.ProtoLens.Runtime.Control.DeepSeq as Control.DeepSeq
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Prism as Data.ProtoLens.Prism
import qualified Data.ProtoLens.Runtime.Prelude as Prelude
import qualified Data.ProtoLens.Runtime.Data.Int as Data.Int
import qualified Data.ProtoLens.Runtime.Data.Monoid as Data.Monoid
import qualified Data.ProtoLens.Runtime.Data.Word as Data.Word
import qualified Data.ProtoLens.Runtime.Data.ProtoLens as Data.ProtoLens
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Bytes as Data.ProtoLens.Encoding.Bytes
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Growing as Data.ProtoLens.Encoding.Growing
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Parser.Unsafe as Data.ProtoLens.Encoding.Parser.Unsafe
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Wire as Data.ProtoLens.Encoding.Wire
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Field as Data.ProtoLens.Field
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Message.Enum as Data.ProtoLens.Message.Enum
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Service.Types as Data.ProtoLens.Service.Types
import qualified Data.ProtoLens.Runtime.Lens.Family2 as Lens.Family2
import qualified Data.ProtoLens.Runtime.Lens.Family2.Unchecked as Lens.Family2.Unchecked
import qualified Data.ProtoLens.Runtime.Data.Text as Data.Text
import qualified Data.ProtoLens.Runtime.Data.Map as Data.Map
import qualified Data.ProtoLens.Runtime.Data.ByteString as Data.ByteString
import qualified Data.ProtoLens.Runtime.Data.ByteString.Char8 as Data.ByteString.Char8
import qualified Data.ProtoLens.Runtime.Data.Text.Encoding as Data.Text.Encoding
import qualified Data.ProtoLens.Runtime.Data.Vector as Data.Vector
import qualified Data.ProtoLens.Runtime.Data.Vector.Generic as Data.Vector.Generic
import qualified Data.ProtoLens.Runtime.Data.Vector.Unboxed as Data.Vector.Unboxed
import qualified Data.ProtoLens.Runtime.Text.Read as Text.Read
{- | Fields :
     
         * 'Proto.Kvstore_Fields.key' @:: Lens' CreateRequest Data.ByteString.ByteString@
         * 'Proto.Kvstore_Fields.value' @:: Lens' CreateRequest Data.ByteString.ByteString@ -}
data CreateRequest
  = CreateRequest'_constructor {_CreateRequest'key :: !Data.ByteString.ByteString,
                                _CreateRequest'value :: !Data.ByteString.ByteString,
                                _CreateRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show CreateRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField CreateRequest "key" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CreateRequest'key (\ x__ y__ -> x__ {_CreateRequest'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField CreateRequest "value" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _CreateRequest'value
           (\ x__ y__ -> x__ {_CreateRequest'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message CreateRequest where
  messageName _ = Data.Text.pack "io.grpc.CreateRequest"
  packedMessageDescriptor _
    = "\n\
      \\rCreateRequest\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\fR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\fR\ENQvalue"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        key__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "key"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"key")) ::
              Data.ProtoLens.FieldDescriptor CreateRequest
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"value")) ::
              Data.ProtoLens.FieldDescriptor CreateRequest
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _CreateRequest'_unknownFields
        (\ x__ y__ -> x__ {_CreateRequest'_unknownFields = y__})
  defMessage
    = CreateRequest'_constructor
        {_CreateRequest'key = Data.ProtoLens.fieldDefault,
         _CreateRequest'value = Data.ProtoLens.fieldDefault,
         _CreateRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          CreateRequest -> Data.ProtoLens.Encoding.Bytes.Parser CreateRequest
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getBytes
                                             (Prelude.fromIntegral len))
                                       "key"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"key") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getBytes
                                             (Prelude.fromIntegral len))
                                       "value"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"value") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "CreateRequest"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"key") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((\ bs
                          -> (Data.Monoid.<>)
                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                  (Prelude.fromIntegral (Data.ByteString.length bs)))
                               (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         _v))
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"value") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                         ((\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData CreateRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_CreateRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_CreateRequest'key x__)
                (Control.DeepSeq.deepseq (_CreateRequest'value x__) ()))
{- | Fields :
      -}
data CreateResponse
  = CreateResponse'_constructor {_CreateResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show CreateResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Message CreateResponse where
  messageName _ = Data.Text.pack "io.grpc.CreateResponse"
  packedMessageDescriptor _
    = "\n\
      \\SOCreateResponse"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag = let in Data.Map.fromList []
  unknownFields
    = Lens.Family2.Unchecked.lens
        _CreateResponse'_unknownFields
        (\ x__ y__ -> x__ {_CreateResponse'_unknownFields = y__})
  defMessage
    = CreateResponse'_constructor {_CreateResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          CreateResponse
          -> Data.ProtoLens.Encoding.Bytes.Parser CreateResponse
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "CreateResponse"
  buildMessage
    = \ _x
        -> Data.ProtoLens.Encoding.Wire.buildFieldSet
             (Lens.Family2.view Data.ProtoLens.unknownFields _x)
instance Control.DeepSeq.NFData CreateResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq (_CreateResponse'_unknownFields x__) ()
{- | Fields :
     
         * 'Proto.Kvstore_Fields.key' @:: Lens' DeleteRequest Data.ByteString.ByteString@ -}
data DeleteRequest
  = DeleteRequest'_constructor {_DeleteRequest'key :: !Data.ByteString.ByteString,
                                _DeleteRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show DeleteRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField DeleteRequest "key" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _DeleteRequest'key (\ x__ y__ -> x__ {_DeleteRequest'key = y__}))
        Prelude.id
instance Data.ProtoLens.Message DeleteRequest where
  messageName _ = Data.Text.pack "io.grpc.DeleteRequest"
  packedMessageDescriptor _
    = "\n\
      \\rDeleteRequest\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\fR\ETXkey"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        key__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "key"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"key")) ::
              Data.ProtoLens.FieldDescriptor DeleteRequest
      in
        Data.Map.fromList [(Data.ProtoLens.Tag 1, key__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _DeleteRequest'_unknownFields
        (\ x__ y__ -> x__ {_DeleteRequest'_unknownFields = y__})
  defMessage
    = DeleteRequest'_constructor
        {_DeleteRequest'key = Data.ProtoLens.fieldDefault,
         _DeleteRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          DeleteRequest -> Data.ProtoLens.Encoding.Bytes.Parser DeleteRequest
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getBytes
                                             (Prelude.fromIntegral len))
                                       "key"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"key") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "DeleteRequest"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"key") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((\ bs
                          -> (Data.Monoid.<>)
                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                  (Prelude.fromIntegral (Data.ByteString.length bs)))
                               (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         _v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData DeleteRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_DeleteRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq (_DeleteRequest'key x__) ())
{- | Fields :
      -}
data DeleteResponse
  = DeleteResponse'_constructor {_DeleteResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show DeleteResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Message DeleteResponse where
  messageName _ = Data.Text.pack "io.grpc.DeleteResponse"
  packedMessageDescriptor _
    = "\n\
      \\SODeleteResponse"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag = let in Data.Map.fromList []
  unknownFields
    = Lens.Family2.Unchecked.lens
        _DeleteResponse'_unknownFields
        (\ x__ y__ -> x__ {_DeleteResponse'_unknownFields = y__})
  defMessage
    = DeleteResponse'_constructor {_DeleteResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          DeleteResponse
          -> Data.ProtoLens.Encoding.Bytes.Parser DeleteResponse
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "DeleteResponse"
  buildMessage
    = \ _x
        -> Data.ProtoLens.Encoding.Wire.buildFieldSet
             (Lens.Family2.view Data.ProtoLens.unknownFields _x)
instance Control.DeepSeq.NFData DeleteResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq (_DeleteResponse'_unknownFields x__) ()
{- | Fields :
     
         * 'Proto.Kvstore_Fields.key' @:: Lens' RetrieveRequest Data.ByteString.ByteString@ -}
data RetrieveRequest
  = RetrieveRequest'_constructor {_RetrieveRequest'key :: !Data.ByteString.ByteString,
                                  _RetrieveRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show RetrieveRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField RetrieveRequest "key" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _RetrieveRequest'key
           (\ x__ y__ -> x__ {_RetrieveRequest'key = y__}))
        Prelude.id
instance Data.ProtoLens.Message RetrieveRequest where
  messageName _ = Data.Text.pack "io.grpc.RetrieveRequest"
  packedMessageDescriptor _
    = "\n\
      \\SIRetrieveRequest\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\fR\ETXkey"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        key__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "key"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"key")) ::
              Data.ProtoLens.FieldDescriptor RetrieveRequest
      in
        Data.Map.fromList [(Data.ProtoLens.Tag 1, key__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _RetrieveRequest'_unknownFields
        (\ x__ y__ -> x__ {_RetrieveRequest'_unknownFields = y__})
  defMessage
    = RetrieveRequest'_constructor
        {_RetrieveRequest'key = Data.ProtoLens.fieldDefault,
         _RetrieveRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          RetrieveRequest
          -> Data.ProtoLens.Encoding.Bytes.Parser RetrieveRequest
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getBytes
                                             (Prelude.fromIntegral len))
                                       "key"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"key") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "RetrieveRequest"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"key") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((\ bs
                          -> (Data.Monoid.<>)
                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                  (Prelude.fromIntegral (Data.ByteString.length bs)))
                               (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         _v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData RetrieveRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_RetrieveRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq (_RetrieveRequest'key x__) ())
{- | Fields :
     
         * 'Proto.Kvstore_Fields.value' @:: Lens' RetrieveResponse Data.ByteString.ByteString@ -}
data RetrieveResponse
  = RetrieveResponse'_constructor {_RetrieveResponse'value :: !Data.ByteString.ByteString,
                                   _RetrieveResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show RetrieveResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField RetrieveResponse "value" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _RetrieveResponse'value
           (\ x__ y__ -> x__ {_RetrieveResponse'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message RetrieveResponse where
  messageName _ = Data.Text.pack "io.grpc.RetrieveResponse"
  packedMessageDescriptor _
    = "\n\
      \\DLERetrieveResponse\DC2\DC4\n\
      \\ENQvalue\CAN\SOH \SOH(\fR\ENQvalue"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"value")) ::
              Data.ProtoLens.FieldDescriptor RetrieveResponse
      in
        Data.Map.fromList [(Data.ProtoLens.Tag 1, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _RetrieveResponse'_unknownFields
        (\ x__ y__ -> x__ {_RetrieveResponse'_unknownFields = y__})
  defMessage
    = RetrieveResponse'_constructor
        {_RetrieveResponse'value = Data.ProtoLens.fieldDefault,
         _RetrieveResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          RetrieveResponse
          -> Data.ProtoLens.Encoding.Bytes.Parser RetrieveResponse
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getBytes
                                             (Prelude.fromIntegral len))
                                       "value"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"value") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "RetrieveResponse"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let
                _v = Lens.Family2.view (Data.ProtoLens.Field.field @"value") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((\ bs
                          -> (Data.Monoid.<>)
                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                  (Prelude.fromIntegral (Data.ByteString.length bs)))
                               (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         _v))
             (Data.ProtoLens.Encoding.Wire.buildFieldSet
                (Lens.Family2.view Data.ProtoLens.unknownFields _x))
instance Control.DeepSeq.NFData RetrieveResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_RetrieveResponse'_unknownFields x__)
             (Control.DeepSeq.deepseq (_RetrieveResponse'value x__) ())
{- | Fields :
     
         * 'Proto.Kvstore_Fields.key' @:: Lens' UpdateRequest Data.ByteString.ByteString@
         * 'Proto.Kvstore_Fields.value' @:: Lens' UpdateRequest Data.ByteString.ByteString@ -}
data UpdateRequest
  = UpdateRequest'_constructor {_UpdateRequest'key :: !Data.ByteString.ByteString,
                                _UpdateRequest'value :: !Data.ByteString.ByteString,
                                _UpdateRequest'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show UpdateRequest where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Field.HasField UpdateRequest "key" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _UpdateRequest'key (\ x__ y__ -> x__ {_UpdateRequest'key = y__}))
        Prelude.id
instance Data.ProtoLens.Field.HasField UpdateRequest "value" Data.ByteString.ByteString where
  fieldOf _
    = (Prelude..)
        (Lens.Family2.Unchecked.lens
           _UpdateRequest'value
           (\ x__ y__ -> x__ {_UpdateRequest'value = y__}))
        Prelude.id
instance Data.ProtoLens.Message UpdateRequest where
  messageName _ = Data.Text.pack "io.grpc.UpdateRequest"
  packedMessageDescriptor _
    = "\n\
      \\rUpdateRequest\DC2\DLE\n\
      \\ETXkey\CAN\SOH \SOH(\fR\ETXkey\DC2\DC4\n\
      \\ENQvalue\CAN\STX \SOH(\fR\ENQvalue"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag
    = let
        key__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "key"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"key")) ::
              Data.ProtoLens.FieldDescriptor UpdateRequest
        value__field_descriptor
          = Data.ProtoLens.FieldDescriptor
              "value"
              (Data.ProtoLens.ScalarField Data.ProtoLens.BytesField ::
                 Data.ProtoLens.FieldTypeDescriptor Data.ByteString.ByteString)
              (Data.ProtoLens.PlainField
                 Data.ProtoLens.Optional (Data.ProtoLens.Field.field @"value")) ::
              Data.ProtoLens.FieldDescriptor UpdateRequest
      in
        Data.Map.fromList
          [(Data.ProtoLens.Tag 1, key__field_descriptor),
           (Data.ProtoLens.Tag 2, value__field_descriptor)]
  unknownFields
    = Lens.Family2.Unchecked.lens
        _UpdateRequest'_unknownFields
        (\ x__ y__ -> x__ {_UpdateRequest'_unknownFields = y__})
  defMessage
    = UpdateRequest'_constructor
        {_UpdateRequest'key = Data.ProtoLens.fieldDefault,
         _UpdateRequest'value = Data.ProtoLens.fieldDefault,
         _UpdateRequest'_unknownFields = []}
  parseMessage
    = let
        loop ::
          UpdateRequest -> Data.ProtoLens.Encoding.Bytes.Parser UpdateRequest
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        10
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getBytes
                                             (Prelude.fromIntegral len))
                                       "key"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"key") y x)
                        18
                          -> do y <- (Data.ProtoLens.Encoding.Bytes.<?>)
                                       (do len <- Data.ProtoLens.Encoding.Bytes.getVarInt
                                           Data.ProtoLens.Encoding.Bytes.getBytes
                                             (Prelude.fromIntegral len))
                                       "value"
                                loop (Lens.Family2.set (Data.ProtoLens.Field.field @"value") y x)
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "UpdateRequest"
  buildMessage
    = \ _x
        -> (Data.Monoid.<>)
             (let _v = Lens.Family2.view (Data.ProtoLens.Field.field @"key") _x
              in
                if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                    Data.Monoid.mempty
                else
                    (Data.Monoid.<>)
                      (Data.ProtoLens.Encoding.Bytes.putVarInt 10)
                      ((\ bs
                          -> (Data.Monoid.<>)
                               (Data.ProtoLens.Encoding.Bytes.putVarInt
                                  (Prelude.fromIntegral (Data.ByteString.length bs)))
                               (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                         _v))
             ((Data.Monoid.<>)
                (let
                   _v = Lens.Family2.view (Data.ProtoLens.Field.field @"value") _x
                 in
                   if (Prelude.==) _v Data.ProtoLens.fieldDefault then
                       Data.Monoid.mempty
                   else
                       (Data.Monoid.<>)
                         (Data.ProtoLens.Encoding.Bytes.putVarInt 18)
                         ((\ bs
                             -> (Data.Monoid.<>)
                                  (Data.ProtoLens.Encoding.Bytes.putVarInt
                                     (Prelude.fromIntegral (Data.ByteString.length bs)))
                                  (Data.ProtoLens.Encoding.Bytes.putBytes bs))
                            _v))
                (Data.ProtoLens.Encoding.Wire.buildFieldSet
                   (Lens.Family2.view Data.ProtoLens.unknownFields _x)))
instance Control.DeepSeq.NFData UpdateRequest where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq
             (_UpdateRequest'_unknownFields x__)
             (Control.DeepSeq.deepseq
                (_UpdateRequest'key x__)
                (Control.DeepSeq.deepseq (_UpdateRequest'value x__) ()))
{- | Fields :
      -}
data UpdateResponse
  = UpdateResponse'_constructor {_UpdateResponse'_unknownFields :: !Data.ProtoLens.FieldSet}
  deriving stock (Prelude.Eq, Prelude.Ord)
instance Prelude.Show UpdateResponse where
  showsPrec _ __x __s
    = Prelude.showChar
        '{'
        (Prelude.showString
           (Data.ProtoLens.showMessageShort __x) (Prelude.showChar '}' __s))
instance Data.ProtoLens.Message UpdateResponse where
  messageName _ = Data.Text.pack "io.grpc.UpdateResponse"
  packedMessageDescriptor _
    = "\n\
      \\SOUpdateResponse"
  packedFileDescriptor _ = packedFileDescriptor
  fieldsByTag = let in Data.Map.fromList []
  unknownFields
    = Lens.Family2.Unchecked.lens
        _UpdateResponse'_unknownFields
        (\ x__ y__ -> x__ {_UpdateResponse'_unknownFields = y__})
  defMessage
    = UpdateResponse'_constructor {_UpdateResponse'_unknownFields = []}
  parseMessage
    = let
        loop ::
          UpdateResponse
          -> Data.ProtoLens.Encoding.Bytes.Parser UpdateResponse
        loop x
          = do end <- Data.ProtoLens.Encoding.Bytes.atEnd
               if end then
                   do (let missing = []
                       in
                         if Prelude.null missing then
                             Prelude.return ()
                         else
                             Prelude.fail
                               ((Prelude.++)
                                  "Missing required fields: "
                                  (Prelude.show (missing :: [Prelude.String]))))
                      Prelude.return
                        (Lens.Family2.over
                           Data.ProtoLens.unknownFields (\ !t -> Prelude.reverse t) x)
               else
                   do tag <- Data.ProtoLens.Encoding.Bytes.getVarInt
                      case tag of
                        wire
                          -> do !y <- Data.ProtoLens.Encoding.Wire.parseTaggedValueFromWire
                                        wire
                                loop
                                  (Lens.Family2.over
                                     Data.ProtoLens.unknownFields (\ !t -> (:) y t) x)
      in
        (Data.ProtoLens.Encoding.Bytes.<?>)
          (do loop Data.ProtoLens.defMessage) "UpdateResponse"
  buildMessage
    = \ _x
        -> Data.ProtoLens.Encoding.Wire.buildFieldSet
             (Lens.Family2.view Data.ProtoLens.unknownFields _x)
instance Control.DeepSeq.NFData UpdateResponse where
  rnf
    = \ x__
        -> Control.DeepSeq.deepseq (_UpdateResponse'_unknownFields x__) ()
data KeyValueService = KeyValueService {}
instance Data.ProtoLens.Service.Types.Service KeyValueService where
  type ServiceName KeyValueService = "KeyValueService"
  type ServicePackage KeyValueService = "io.grpc"
  type ServiceMethods KeyValueService = '["create",
                                          "delete",
                                          "retrieve",
                                          "update"]
  packedServiceDescriptor _
    = "\n\
      \\SIKeyValueService\DC29\n\
      \\ACKCreate\DC2\SYN.io.grpc.CreateRequest\SUB\ETB.io.grpc.CreateResponse\DC2?\n\
      \\bRetrieve\DC2\CAN.io.grpc.RetrieveRequest\SUB\EM.io.grpc.RetrieveResponse\DC29\n\
      \\ACKUpdate\DC2\SYN.io.grpc.UpdateRequest\SUB\ETB.io.grpc.UpdateResponse\DC29\n\
      \\ACKDelete\DC2\SYN.io.grpc.DeleteRequest\SUB\ETB.io.grpc.DeleteResponse"
instance Data.ProtoLens.Service.Types.HasMethodImpl KeyValueService "create" where
  type MethodName KeyValueService "create" = "Create"
  type MethodInput KeyValueService "create" = CreateRequest
  type MethodOutput KeyValueService "create" = CreateResponse
  type MethodStreamingType KeyValueService "create" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl KeyValueService "retrieve" where
  type MethodName KeyValueService "retrieve" = "Retrieve"
  type MethodInput KeyValueService "retrieve" = RetrieveRequest
  type MethodOutput KeyValueService "retrieve" = RetrieveResponse
  type MethodStreamingType KeyValueService "retrieve" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl KeyValueService "update" where
  type MethodName KeyValueService "update" = "Update"
  type MethodInput KeyValueService "update" = UpdateRequest
  type MethodOutput KeyValueService "update" = UpdateResponse
  type MethodStreamingType KeyValueService "update" = 'Data.ProtoLens.Service.Types.NonStreaming
instance Data.ProtoLens.Service.Types.HasMethodImpl KeyValueService "delete" where
  type MethodName KeyValueService "delete" = "Delete"
  type MethodInput KeyValueService "delete" = DeleteRequest
  type MethodOutput KeyValueService "delete" = DeleteResponse
  type MethodStreamingType KeyValueService "delete" = 'Data.ProtoLens.Service.Types.NonStreaming
packedFileDescriptor :: Data.ByteString.ByteString
packedFileDescriptor
  = "\n\
    \\rkvstore.proto\DC2\aio.grpc\"7\n\
    \\rCreateRequest\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\fR\ETXkey\DC2\DC4\n\
    \\ENQvalue\CAN\STX \SOH(\fR\ENQvalue\"\DLE\n\
    \\SOCreateResponse\"#\n\
    \\SIRetrieveRequest\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\fR\ETXkey\"(\n\
    \\DLERetrieveResponse\DC2\DC4\n\
    \\ENQvalue\CAN\SOH \SOH(\fR\ENQvalue\"7\n\
    \\rUpdateRequest\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\fR\ETXkey\DC2\DC4\n\
    \\ENQvalue\CAN\STX \SOH(\fR\ENQvalue\"\DLE\n\
    \\SOUpdateResponse\"!\n\
    \\rDeleteRequest\DC2\DLE\n\
    \\ETXkey\CAN\SOH \SOH(\fR\ETXkey\"\DLE\n\
    \\SODeleteResponse2\131\STX\n\
    \\SIKeyValueService\DC29\n\
    \\ACKCreate\DC2\SYN.io.grpc.CreateRequest\SUB\ETB.io.grpc.CreateResponse\DC2?\n\
    \\bRetrieve\DC2\CAN.io.grpc.RetrieveRequest\SUB\EM.io.grpc.RetrieveResponse\DC29\n\
    \\ACKUpdate\DC2\SYN.io.grpc.UpdateRequest\SUB\ETB.io.grpc.UpdateResponse\DC29\n\
    \\ACKDelete\DC2\SYN.io.grpc.DeleteRequest\SUB\ETB.io.grpc.DeleteResponseB%\n\
    \\SYNio.grpc.examples.protoB\tKeyValuesP\SOHJ\144\a\n\
    \\ACK\DC2\EOT\NUL\NUL+\SOH\n\
    \\b\n\
    \\SOH\f\DC2\ETX\NUL\NUL\DC2\n\
    \\b\n\
    \\SOH\STX\DC2\ETX\STX\NUL\DLE\n\
    \\b\n\
    \\SOH\b\DC2\ETX\ETX\NUL/\n\
    \\t\n\
    \\STX\b\SOH\DC2\ETX\ETX\NUL/\n\
    \\b\n\
    \\SOH\b\DC2\ETX\EOT\NUL\"\n\
    \\t\n\
    \\STX\b\n\
    \\DC2\ETX\EOT\NUL\"\n\
    \\b\n\
    \\SOH\b\DC2\ETX\ENQ\NUL*\n\
    \\t\n\
    \\STX\b\b\DC2\ETX\ENQ\NUL*\n\
    \\n\
    \\n\
    \\STX\EOT\NUL\DC2\EOT\a\NUL\n\
    \\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\NUL\SOH\DC2\ETX\a\b\NAK\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\NUL\DC2\ETX\b\STX\DLE\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ENQ\DC2\ETX\b\STX\a\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\SOH\DC2\ETX\b\b\v\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\NUL\ETX\DC2\ETX\b\SO\SI\n\
    \\v\n\
    \\EOT\EOT\NUL\STX\SOH\DC2\ETX\t\STX\DC2\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\ENQ\DC2\ETX\t\STX\a\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\SOH\DC2\ETX\t\b\r\n\
    \\f\n\
    \\ENQ\EOT\NUL\STX\SOH\ETX\DC2\ETX\t\DLE\DC1\n\
    \\n\
    \\n\
    \\STX\EOT\SOH\DC2\EOT\f\NUL\r\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\SOH\SOH\DC2\ETX\f\b\SYN\n\
    \\n\
    \\n\
    \\STX\EOT\STX\DC2\EOT\SI\NUL\DC1\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\STX\SOH\DC2\ETX\SI\b\ETB\n\
    \\v\n\
    \\EOT\EOT\STX\STX\NUL\DC2\ETX\DLE\STX\DLE\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ENQ\DC2\ETX\DLE\STX\a\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\SOH\DC2\ETX\DLE\b\v\n\
    \\f\n\
    \\ENQ\EOT\STX\STX\NUL\ETX\DC2\ETX\DLE\SO\SI\n\
    \\n\
    \\n\
    \\STX\EOT\ETX\DC2\EOT\DC3\NUL\NAK\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\ETX\SOH\DC2\ETX\DC3\b\CAN\n\
    \\v\n\
    \\EOT\EOT\ETX\STX\NUL\DC2\ETX\DC4\STX\DC2\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ENQ\DC2\ETX\DC4\STX\a\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\SOH\DC2\ETX\DC4\b\r\n\
    \\f\n\
    \\ENQ\EOT\ETX\STX\NUL\ETX\DC2\ETX\DC4\DLE\DC1\n\
    \\n\
    \\n\
    \\STX\EOT\EOT\DC2\EOT\ETB\NUL\SUB\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\EOT\SOH\DC2\ETX\ETB\b\NAK\n\
    \\v\n\
    \\EOT\EOT\EOT\STX\NUL\DC2\ETX\CAN\STX\DLE\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\ENQ\DC2\ETX\CAN\STX\a\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\SOH\DC2\ETX\CAN\b\v\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\NUL\ETX\DC2\ETX\CAN\SO\SI\n\
    \\v\n\
    \\EOT\EOT\EOT\STX\SOH\DC2\ETX\EM\STX\DC2\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\SOH\ENQ\DC2\ETX\EM\STX\a\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\SOH\SOH\DC2\ETX\EM\b\r\n\
    \\f\n\
    \\ENQ\EOT\EOT\STX\SOH\ETX\DC2\ETX\EM\DLE\DC1\n\
    \\n\
    \\n\
    \\STX\EOT\ENQ\DC2\EOT\FS\NUL\GS\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\ENQ\SOH\DC2\ETX\FS\b\SYN\n\
    \\n\
    \\n\
    \\STX\EOT\ACK\DC2\EOT\US\NUL!\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\ACK\SOH\DC2\ETX\US\b\NAK\n\
    \\v\n\
    \\EOT\EOT\ACK\STX\NUL\DC2\ETX \STX\DLE\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\ENQ\DC2\ETX \STX\a\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\SOH\DC2\ETX \b\v\n\
    \\f\n\
    \\ENQ\EOT\ACK\STX\NUL\ETX\DC2\ETX \SO\SI\n\
    \\n\
    \\n\
    \\STX\EOT\a\DC2\EOT#\NUL$\SOH\n\
    \\n\
    \\n\
    \\ETX\EOT\a\SOH\DC2\ETX#\b\SYN\n\
    \\n\
    \\n\
    \\STX\ACK\NUL\DC2\EOT&\NUL+\SOH\n\
    \\n\
    \\n\
    \\ETX\ACK\NUL\SOH\DC2\ETX&\b\ETB\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\NUL\DC2\ETX'\STX5\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\SOH\DC2\ETX'\ACK\f\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\STX\DC2\ETX'\r\SUB\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\NUL\ETX\DC2\ETX'%3\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\SOH\DC2\ETX(\STX;\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\SOH\DC2\ETX(\ACK\SO\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\STX\DC2\ETX(\SI\RS\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\SOH\ETX\DC2\ETX()9\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\STX\DC2\ETX)\STX5\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\STX\SOH\DC2\ETX)\ACK\f\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\STX\STX\DC2\ETX)\r\SUB\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\STX\ETX\DC2\ETX)%3\n\
    \\v\n\
    \\EOT\ACK\NUL\STX\ETX\DC2\ETX*\STX5\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\ETX\SOH\DC2\ETX*\ACK\f\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\ETX\STX\DC2\ETX*\r\SUB\n\
    \\f\n\
    \\ENQ\ACK\NUL\STX\ETX\ETX\DC2\ETX*%3b\ACKproto3"