{-# LANGUAGE LambdaCase #-}
module Mono where

import Foreign hiding (void)
import Foreign.C
import Control.Monad
import Control.Monad.Fix (fix)
import Control.Exception (bracket)
import Data.List (intercalate)

#include <glib.h>
#include <mono/jit/jit.h>
#include <mono/metadata/assembly.h>
#include <mono/metadata/debug-helpers.h>

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
data MonoMethodSignature_
{#pointer *MonoMethodSignature -> `MonoMethodSignature_' #}
data MonoString_
{#pointer *MonoString -> `MonoString_' #}

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

-- debug-helpers.h (without disassembly stuff)

data MonoMethodDesc_
{#pointer *MonoMethodDesc -> `MonoMethodDesc_' #}

{#fun mono_type_full_name {`MonoType'} -> `String' #}
{#fun mono_signature_get_desc {`MonoMethodSignature', `Bool'} -> `String' #}
{#fun mono_context_get_desc {`MonoGenericContext'} -> `String' #}

{#fun mono_method_desc_new {`String', `Bool'} -> `MonoMethodDesc' #}
{#fun mono_method_desc_from_method {`MonoMethod'} -> `MonoMethodDesc' #}
{#fun mono_method_desc_free {`MonoMethodDesc'} -> `()' #}
{#fun mono_method_desc_match {`MonoMethodDesc', `MonoMethod'} -> `Bool' #}
{#fun mono_method_desc_full_match {`MonoMethodDesc', `MonoMethod'} -> `Bool' #}
{#fun mono_method_desc_search_in_class {`MonoMethodDesc', `MonoClass'} -> `MonoMethod' #}
{#fun mono_method_desc_search_in_image {`MonoMethodDesc', `MonoImage'} -> `MonoMethod' #}

{#fun mono_method_full_name {`MonoMethod', `Bool'} -> `String' #}
{#fun mono_field_full_name {`MonoClassField'} -> `String' #}

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
{#fun mono_runtime_invoke {`MonoMethod', `Ptr ()', id `Ptr (Ptr ())', id `Ptr MonoObject'} -> `MonoObject' #}
{#fun mono_string_new {`MonoDomain', `String'} -> `MonoString' #}
{#fun mono_method_signature {`MonoMethod'} -> `MonoMethodSignature' #}
{#fun mono_signature_get_params {`MonoMethodSignature', id `Iter'} -> `MonoType' #}

iteration :: (Iter -> IO (Ptr a)) -> IO [Ptr a]
iteration f = alloca $ \iter -> do
  poke iter nullPtr
  fix $ \loop -> do
    p <- f iter
    if p == nullPtr
      then return []
      else fmap (p :) loop

check :: IO (Ptr a) -> IO (Ptr a)
check = throwIfNull "got a null pointer"

runtimeInvoke :: MonoMethod -> Ptr () -> [Ptr ()] -> IO (Either MonoObject MonoObject)
runtimeInvoke meth this params =
  withArray0 nullPtr params $ \ary -> do -- don't know if the null term is necessary
    alloca $ \pex -> do
      poke pex nullPtr
      ret <- mono_runtime_invoke meth this ary pex
      exception <- peek pex
      return $ if exception == nullPtr
        then Right ret
        else Left exception

data Thing
  = Object MonoObject
  | Int32 Int32
  | UInt32 Word32
  | String String
  deriving (Eq, Ord, Show)

getField :: MonoClassField -> MonoObject -> MonoDomain -> IO Thing
getField field obj dom = mono_field_get_type field >>= mono_type_get_type >>= \case
  MONO_TYPE_U4 -> alloca $ \p -> do
    mono_field_get_value obj field $ castPtr p
    fmap UInt32 $ peek p
  MONO_TYPE_I4 -> alloca $ \p -> do
    mono_field_get_value obj field $ castPtr p
    fmap Int32 $ peek p
  MONO_TYPE_STRING -> undefined
  _ -> fmap Object $ mono_field_get_value_object dom field obj

setField :: MonoClassField -> MonoObject -> Thing -> IO ()
setField field obj thing = mono_field_get_type field >>= mono_type_get_type >>= \case
  MONO_TYPE_U4 -> case thing of
    UInt32 w32 -> with w32 $ mono_field_set_value obj field . castPtr
    _ -> wrongType
  MONO_TYPE_I4 -> case thing of
    Int32 i32 -> with i32 $ mono_field_set_value obj field . castPtr
    _ -> wrongType
  MONO_TYPE_STRING -> undefined
  _ -> undefined
  where wrongType = error "setField: wrong type"

className :: MonoObject -> IO String
className obj = do
  klass <- mono_object_get_class    obj
  c     <- mono_class_get_name      klass
  ns    <- mono_class_get_namespace klass
  return $ ns ++ "." ++ c

thingClassName :: Thing -> IO String
thingClassName = \case
  Object o -> className o
  String _ -> return "string"
  UInt32 _ -> return "UInt32"
  Int32  _ -> return "Int32"

findMethod :: String -> MonoObject -> [Thing] -> MonoImage -> IO MonoMethod
findMethod name obj args img = do
  objClass <- className obj
  argClasses <- mapM thingClassName args
  let descString = objClass ++ ":" ++ name ++ "(" ++ intercalate ", " argClasses ++ ")"
      errstr = "No matching method for description: " ++ descString
  throwIfNull errstr
    $ bracket (check $ mono_method_desc_new descString True) mono_method_desc_free
    $ \desc -> mono_method_desc_search_in_image desc img

callMethod :: String -> MonoObject -> [Thing] -> MonoDomain -> MonoImage -> IO (Either MonoObject MonoObject)
callMethod name obj args dom img = do
  meth <- throwIfNull "No matching method found" $ findMethod name obj args img
  sig <- mono_method_signature meth
  paramTypes <- iteration $ mono_signature_get_params sig
  when (length paramTypes /= length args) $ error "callMethod: panic! arg length doesn't match"
  let withParam (ptype, thing) cont = case thing of
        Object o   -> cont' o
        String s   -> mono_string_new dom s >>= cont'
        UInt32 w32 -> with w32 cont'
        Int32  i32 -> with i32 cont'
        where cont' = cont . castPtr
  withMany withParam (zip paramTypes args) $ \ptrArgs -> do
    withArray0 nullPtr ptrArgs $ \ptrPtr -> do
      alloca $ \pex -> do
        poke pex nullPtr
        ret <- mono_runtime_invoke meth (castPtr obj) ptrPtr pex
        exception <- peek pex
        return $ if exception == nullPtr
          then Right ret
          else Left exception

monoMain :: IO ()
monoMain = bracket (check $ mono_jit_init "myapp") mono_jit_cleanup $ \dom -> do
  asm <- check $ mono_domain_assembly_open dom "X360.dll"
  img <- check $ mono_assembly_get_image asm

  let findField obj name = do
        klass <- throwIfNull "couldn't get class of object" $ mono_object_get_class obj
        throwIfNull ("couldn't find field " ++ name) $ mono_class_get_field_from_name klass name

      findMethod_ description = check
        $ bracket
          (check $ mono_method_desc_new description True)
          mono_method_desc_free
        $ \desc -> mono_method_desc_search_in_image desc img

      listMethods obj = do
        meths <- mono_object_get_class obj >>= iteration . mono_class_get_methods
        forM_ meths $ \m -> mono_method_full_name m True >>= putStrLn

      listFields obj = mono_object_get_class obj >>= listFieldsClass
      listFieldsClass klass = do
        fields <- iteration $ mono_class_get_fields klass
        forM_ fields $ \f -> mono_field_full_name f >>= putStrLn

      call desc obj args = do
        meth <- findMethod_ desc
        runtimeInvoke meth obj args >>= \case
          Left exc -> error $ ".NET method returned an exception: " ++ show exc
          Right ret -> return ret

      enumValue namespace classname ident = do
        klass <- check $ mono_class_from_name img namespace classname
        field <- check $ mono_class_get_field_from_name klass ident
        vtable <- check $ mono_class_vtable dom klass
        alloca $ \p -> do
          mono_field_static_get_value vtable field $ castPtr p
          peek p

  xsession <- do
    klass <- check $ mono_class_from_name img "X360.STFS" "CreateSTFS"
    xsession <- check $ mono_object_new dom klass
    mono_runtime_object_init xsession
    return xsession
  Object header <- findField xsession "HeaderData" >>= \f -> getField f xsession dom
  findField header "TitleID" >>= \f -> setField f header $ UInt32 0x45410914

  do
    klass <- check $ mono_class_from_name img "X360.STFS" "Languages"
    field <- mono_class_get_field_from_name klass "English"
    mono_field_get_type field >>= mono_type_get_type >>= print

  do  v <- enumValue "X360.STFS" "Languages" "English"
      Right _ <- callMethod "SetLanguage" header [Int32 v] dom img
      return ()
  Right _ <- callMethod "set_Publisher" header [String "Harmonix"] dom img
  Right _ <- callMethod "set_Title_Package" header [String "Rock Band 3"] dom img
  do  v <- enumValue "X360.STFS" "STFSType" "Type0"
      void $ with (v :: Int32) $ \p -> 
        call "X360.STFS.CreateSTFS:set_STFSType (X360.STFS.STFSType)" (castPtr xsession) [castPtr p]
  do  v <- enumValue "X360.STFS" "PackageType" "SavedGame"
      void $ with (v :: Int32) $ \p ->
        call "X360.STFS.HeaderData:set_ThisType (X360.STFS.PackageType)" (castPtr header) [castPtr p]
