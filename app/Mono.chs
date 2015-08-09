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
data MonoClassField_
{#pointer *MonoClassField -> `MonoClassField_' #}
data MonoMethod_
{#pointer *MonoMethod -> `MonoMethod_' #}
data MonoType_
{#pointer *MonoType -> `MonoType_' #}

{#enum MonoTypeEnum {} deriving (Eq, Ord, Show, Read) #}

type Iter = Ptr (Ptr ())

{#fun mono_jit_init {`String'} -> `MonoDomain' #}
{#fun mono_jit_cleanup {`MonoDomain'} -> `()' #}
{#fun mono_domain_assembly_open {`MonoDomain', `String'} -> `MonoAssembly' #}
{#fun mono_assembly_get_image {`MonoAssembly'} -> `MonoImage' #}
{#fun mono_class_from_name {`MonoImage', `String', `String'} -> `MonoClass' #}
{#fun mono_object_new {`MonoDomain', `MonoClass'} -> `MonoObject' #}
{#fun mono_runtime_object_init {`MonoObject'} -> `()' #}
{#fun mono_class_num_fields {`MonoClass'} -> `Int' #}
{#fun mono_class_num_methods {`MonoClass'} -> `Int' #}
{#fun mono_class_get_fields {`MonoClass', id `Iter'} -> `MonoClassField' #}
{#fun mono_class_get_methods {`MonoClass', id `Iter'} -> `MonoMethod' #}
{#fun mono_field_get_name {`MonoClassField'} -> `String' #}
{#fun mono_method_get_name {`MonoMethod'} -> `String' #}
{#fun mono_field_get_value {`MonoObject', `MonoClassField', `Ptr ()'} -> `()' #}
{#fun mono_field_set_value {`MonoObject', `MonoClassField', `Ptr ()'} -> `()' #}
{#fun mono_field_get_value_object {`MonoDomain', `MonoClassField', `MonoObject'} -> `MonoObject' #}
{#fun mono_object_get_class {`MonoObject'} -> `MonoClass' #}
{#fun mono_class_get_name {`MonoClass'} -> `String' #}
{#fun mono_field_get_type {`MonoClassField'} -> `MonoType' #}
{#fun mono_type_get_type {`MonoType'} -> `MonoTypeEnum' #}
{#fun mono_domain_get {} -> `MonoDomain' #}

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

callField :: MonoObject -> String -> IO Value
callField obj f = do
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

  Object header <- callField xsession "HeaderData"
  UInt32 titleID <- callField header "TitleID"
  print titleID
  setField header "TitleID" $ UInt32 0x45410914
  UInt32 titleID2 <- callField header "TitleID"
  print titleID2

  mono_jit_cleanup dom
