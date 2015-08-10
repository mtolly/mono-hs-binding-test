{-# LANGUAGE LambdaCase #-}
module Mono where

import Foreign
import Foreign.C
import Control.Monad

#include <glib.h>
#include <mono/jit/jit.h>
#include <mono/metadata/assembly.h>

data MonoDomain_
{#pointer *MonoDomain -> `MonoDomain_' #}
data MonoAssembly_
{#pointer *MonoAssembly -> `MonoAssembly_' #}
data MonoImage_
{#pointer *MonoImage -> `MonoImage_' #}
data MonoClass_
{#pointer *MonoClass -> `MonoClass_' #}
data MonoObject_
{#pointer *MonoObject -> `MonoObject_' #}
data MonoMethod_
{#pointer *MonoMethod -> `MonoMethod_' #}
data MonoType_
{#pointer *MonoType -> `MonoType_' #}
data MonoGenericContext_
{#pointer *MonoGenericContext -> `MonoGenericContext_' #}
data MonoGenericParam_
{#pointer *MonoGenericParam -> `MonoGenericParam_' #}

{#enum MonoTypeEnum {} deriving (Eq, Ord, Show, Read) #}

type Iter = Ptr (Ptr ())

-- class.h

data MonoVTable_
{#pointer *MonoVTable -> `MonoVTable_' #}

data MonoClassField_
{#pointer *MonoClassField -> `MonoClassField_' #}
data MonoProperty_
{#pointer *MonoProperty -> `MonoProperty_' #}
data MonoEvent_
{#pointer *MonoEvent -> `MonoEvent_' #}

{#fun mono_class_get {`MonoImage', `Word32'} -> `MonoClass' #}
{#fun mono_class_get_full {`MonoImage', `Word32', `MonoGenericContext'} -> `MonoClass' #}
{#fun mono_class_init {`MonoClass'} -> `Bool' #}
{#fun mono_class_vtable {`MonoDomain', `MonoClass'} -> `MonoVTable' #}
{#fun mono_class_from_name {`MonoImage', `String', `String'} -> `MonoClass' #}
{#fun mono_class_from_name_case {`MonoImage', `String', `String'} -> `MonoClass' #}
{#fun mono_class_get_method_from_name_flags {`MonoClass', `String', `CInt', `CInt'} -> `MonoMethod' #}
{#fun mono_class_from_typeref {`MonoImage', `Word32'} -> `MonoClass' #}
-- TODO: mono_class_from_typeref_checked
{#fun mono_class_from_generic_parameter {`MonoGenericParam', `MonoImage', `Bool'} -> `MonoClass' #}
{#fun mono_class_inflate_generic_type {`MonoType', `MonoGenericContext'} -> `MonoType' #}
{#fun mono_class_inflate_generic_method {`MonoMethod', `MonoGenericContext'} -> `MonoMethod' #}
{#fun mono_get_inflated_method {`MonoMethod'} -> `MonoMethod' #}
{#fun mono_field_from_token {`MonoImage', `Word32', id `Ptr MonoClass', `MonoGenericContext'} -> `MonoClassField' #}
{#fun mono_bounded_array_class_get {`MonoClass', `Word32', `Bool'} -> `MonoClass' #}
{#fun mono_array_class_get {`MonoClass', `Word32'} -> `MonoClass' #}
{#fun mono_ptr_class_get {`MonoType'} -> `MonoClass' #}
{#fun mono_class_get_field {`MonoClass', `Word32'} -> `MonoClassField' #}
{#fun mono_class_get_field_from_name {`MonoClass', `String'} -> `MonoClassField' #}
{#fun mono_class_get_field_token {`MonoClassField'} -> `Word32' #}
{#fun mono_class_get_event_token {`MonoEvent'} -> `Word32' #}
{#fun mono_class_get_property_from_name {`MonoClass', `String'} -> `MonoProperty' #}
{#fun mono_class_get_property_token {`MonoProperty'} -> `Word32' #}
{#fun mono_array_element_size {`MonoClass'} -> `Int32' #}
{#fun mono_class_instance_size {`MonoClass'} -> `Int32' #}
{#fun mono_class_array_element_size {`MonoClass'} -> `Int32' #}
{#fun mono_class_data_size {`MonoClass'} -> `Int32' #}
{#fun mono_class_value_size {`MonoClass', `Word32'} -> `Int32' #}
{#fun mono_class_min_align {`MonoClass'} -> `Int32' #}
{#fun mono_class_from_mono_type {`MonoType'} -> `MonoClass' #}
{#fun mono_class_is_subclass_of {`MonoClass', `MonoClass', `Bool'} -> `Bool' #}
{#fun mono_class_is_assignable_from {`MonoClass', `MonoClass'} -> `Bool' #}
{#fun mono_ldtoken {`MonoImage', `Word32', id `Ptr MonoClass', `MonoGenericContext'} -> `Ptr ()' #}
{#fun mono_type_get_name {`MonoType'} -> `String' #}
{#fun mono_type_get_underlying_type {`MonoType'} -> `MonoType' #}

-- MonoClass accessors
{#fun mono_class_get_image {`MonoClass'} -> `MonoImage' #}
{#fun mono_class_get_element_class {`MonoClass'} -> `MonoClass' #}
{#fun mono_class_is_valuetype {`MonoClass'} -> `Bool' #}
{#fun mono_class_is_enum {`MonoClass'} -> `Bool' #}
{#fun mono_class_enum_basetype {`MonoClass'} -> `MonoType' #}
{#fun mono_class_get_parent {`MonoClass'} -> `MonoClass' #}
{#fun mono_class_get_nesting_type {`MonoClass'} -> `MonoClass' #}
{#fun mono_class_get_rank {`MonoClass'} -> `CInt' #}
{#fun mono_class_get_flags {`MonoClass'} -> `Word32' #}
{#fun mono_class_get_name {`MonoClass'} -> `String' #}
{#fun mono_class_get_namespace {`MonoClass'} -> `String' #}
{#fun mono_class_get_type {`MonoClass'} -> `MonoType' #}
{#fun mono_class_get_type_token {`MonoClass'} -> `Word32' #}
{#fun mono_class_get_byref_type {`MonoClass'} -> `MonoType' #}
{#fun mono_class_num_fields {`MonoClass'} -> `CInt' #}
{#fun mono_class_num_methods {`MonoClass'} -> `CInt' #}
{#fun mono_class_num_properties {`MonoClass'} -> `CInt' #}
{#fun mono_class_num_events {`MonoClass'} -> `CInt' #}
{#fun mono_class_get_fields {`MonoClass', id `Iter'} -> `MonoClassField' #}
{#fun mono_class_get_methods {`MonoClass', id `Iter'} -> `MonoMethod' #}
{#fun mono_class_get_properties {`MonoClass', id `Iter'} -> `MonoProperty' #}
{#fun mono_class_get_events {`MonoClass', id `Iter'} -> `MonoEvent' #}
{#fun mono_class_get_interfaces {`MonoClass', id `Iter'} -> `MonoClass' #}
{#fun mono_class_get_nested_types {`MonoClass', id `Iter'} -> `MonoClass' #}
{#fun mono_class_is_delegate {`MonoClass'} -> `Bool' #}
{#fun mono_class_implements_interface {`MonoClass', `MonoClass'} -> `Bool' #}

-- MonoClassField accessors
{#fun mono_field_get_name {`MonoClassField'} -> `String' #}
{#fun mono_field_get_type {`MonoClassField'} -> `MonoType' #}
{#fun mono_field_get_parent {`MonoClassField'} -> `MonoClass' #}
{#fun mono_field_get_flags {`MonoClassField'} -> `Word32' #}
{#fun mono_field_get_offset {`MonoClassField'} -> `Word32' #}
{#fun mono_field_get_data {`MonoClassField'} -> `CString' #}

-- MonoProperty accessors
{#fun mono_property_get_name {`MonoProperty'} -> `String' #}
{#fun mono_property_get_set_method {`MonoProperty'} -> `MonoMethod' #}
{#fun mono_property_get_get_method {`MonoProperty'} -> `MonoMethod' #}
{#fun mono_property_get_parent {`MonoProperty'} -> `MonoClass' #}
{#fun mono_property_get_flags {`MonoProperty'} -> `Word32' #}

-- MonoEvent accessors
{#fun mono_event_get_name {`MonoEvent'} -> `String' #}
{#fun mono_event_get_add_method {`MonoEvent'} -> `MonoMethod' #}
{#fun mono_event_get_remove_method {`MonoEvent'} -> `MonoMethod' #}
{#fun mono_event_get_raise_method {`MonoEvent'} -> `MonoMethod' #}
{#fun mono_event_get_parent {`MonoEvent'} -> `MonoClass' #}
{#fun mono_event_get_flags {`MonoEvent'} -> `Word32' #}

{#fun mono_class_get_method_from_name {`MonoClass', `String', `CInt'} -> `MonoMethod' #}
{#fun mono_class_name_from_token {`MonoImage', `Word32'} -> `String' #}
{#fun mono_method_can_access_field {`MonoMethod', `MonoClassField'} -> `Bool' #}
{#fun mono_method_can_access_method {`MonoMethod', `MonoMethod'} -> `Bool' #}

-- assorted

{#fun mono_jit_init {`String'} -> `MonoDomain' #}
{#fun mono_jit_cleanup {`MonoDomain'} -> `()' #}
{#fun mono_domain_assembly_open {`MonoDomain', `String'} -> `MonoAssembly' #}
{#fun mono_assembly_get_image {`MonoAssembly'} -> `MonoImage' #}
{#fun mono_object_new {`MonoDomain', `MonoClass'} -> `MonoObject' #}
{#fun mono_runtime_object_init {`MonoObject'} -> `()' #}
{#fun mono_method_get_name {`MonoMethod'} -> `String' #}
{#fun mono_field_get_value {`MonoObject', `MonoClassField', `Ptr ()'} -> `()' #}
{#fun mono_field_static_get_value {`MonoVTable', `MonoClassField', `Ptr ()'} -> `()' #}
{#fun mono_field_set_value {`MonoObject', `MonoClassField', `Ptr ()'} -> `()' #}
{#fun mono_field_get_value_object {`MonoDomain', `MonoClassField', `MonoObject'} -> `MonoObject' #}
{#fun mono_object_get_class {`MonoObject'} -> `MonoClass' #}
{#fun mono_type_get_type {`MonoType'} -> `MonoTypeEnum' #}
{#fun mono_domain_get {} -> `MonoDomain' #}

printClassField :: MonoClassField -> IO ()
printClassField mcf = do
  let p s f = f mcf >>= \x -> putStrLn $ s ++ ": " ++ show x
  p "name" mono_field_get_name
  p "type" mono_field_get_type
  p "parent" mono_field_get_parent
  p "flags" mono_field_get_flags
  p "offset" mono_field_get_offset
  p "data" mono_field_get_data
  putStrLn ""

iteration :: (Iter -> IO (Ptr a)) -> IO [Ptr a]
iteration f = alloca $ \iter -> do
  poke iter nullPtr
  let loop = do
        p <- f iter
        if p == nullPtr
          then return []
          else fmap (p :) loop
  loop

findField :: MonoClass -> String -> IO MonoClassField
findField c f = do
  fields <- iteration $ mono_class_get_fields c
  pairs <- forM fields $ \field -> do
    name <- mono_field_get_name field
    return (name, field)
  case lookup f pairs of
    Nothing -> error $ "no field named " ++ show f
    Just field -> return field

data Value
  = Object MonoObject
  | UInt32 Word32
  deriving (Eq, Ord, Show)

getField :: MonoObject -> String -> IO Value
getField obj f = do
  klass <- mono_object_get_class obj
  field <- findField klass f
  mono_field_get_type field >>= mono_type_get_type >>= \case
    MONO_TYPE_U4 -> alloca $ \p -> do
      mono_field_get_value obj field $ castPtr p
      fmap UInt32 $ peek p
    _ -> do
      dom <- mono_domain_get
      fmap Object $ mono_field_get_value_object dom field obj

setField :: MonoObject -> String -> Value -> IO ()
setField obj f v = do
  klass <- mono_object_get_class obj
  field <- findField klass f
  mono_field_get_type field >>= mono_type_get_type >>= \case
    MONO_TYPE_U4 -> case v of
      UInt32 u32 -> alloca $ \p -> do
        poke p u32
        mono_field_set_value obj field $ castPtr p
      _ -> wrongType
    _ -> case v of
      Object o -> mono_field_set_value obj field $ castPtr o
      _ -> wrongType
  where wrongType = error "setField: incorrect type"

check :: IO (Ptr a) -> IO (Ptr a)
check = throwIfNull "something"

monoMain :: IO ()
monoMain = do
  dom <- check $ mono_jit_init "myapp"
  asm <- check $ mono_domain_assembly_open dom "X360.dll"
  img <- check $ mono_assembly_get_image asm

  createSTFS <- check $ mono_class_from_name img "X360.STFS" "CreateSTFS"
  xsession <- check $ mono_object_new dom createSTFS
  mono_runtime_object_init xsession

  Object header <- getField xsession "HeaderData"
  setField header "TitleID" $ UInt32 0x45410914

  langs <- check $ mono_class_from_name img "X360.STFS" "Languages"
  fs <- iteration $ mono_class_get_fields langs
  mapM_ printClassField fs
  alloca $ \p -> do
    vtable <- mono_class_vtable dom langs
    mono_field_static_get_value vtable (fs !! 3) $ castPtr p
    i <- peek p :: IO Int32
    print i

  mono_jit_cleanup dom
