/*
 * Copyright (c) 2014, 2020, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 *
 */

#include "precompiled.hpp"
#include "classfile/classFileStream.hpp"
#include "classfile/classListParser.hpp"
#include "classfile/classLoader.hpp"
#include "classfile/classLoaderData.inline.hpp"
#include "classfile/classLoaderDataGraph.hpp"
#include "classfile/classLoaderExt.hpp"
#include "classfile/dictionary.hpp"
#include "classfile/javaClasses.hpp"
#include "classfile/symbolTable.hpp"
#include "classfile/systemDictionary.hpp"
#include "classfile/systemDictionaryShared.hpp"
#include "classfile/verificationType.hpp"
#include "classfile/vmSymbols.hpp"
#include "logging/log.hpp"
#include "memory/allocation.hpp"
#include "memory/archiveUtils.hpp"
#include "memory/filemap.hpp"
#include "memory/heapShared.hpp"
#include "memory/metadataFactory.hpp"
#include "memory/metaspaceClosure.hpp"
#include "memory/oopFactory.hpp"
#include "memory/resourceArea.hpp"
#include "memory/universe.hpp"
#include "memory/dynamicArchive.hpp"
#include "oops/instanceKlass.hpp"
#include "oops/klass.inline.hpp"
#include "oops/objArrayOop.inline.hpp"
#include "oops/oop.inline.hpp"
#include "oops/typeArrayOop.inline.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/java.hpp"
#include "runtime/javaCalls.hpp"
#include "runtime/mutexLocker.hpp"
#include "utilities/hashtable.inline.hpp"
#include "utilities/resourceHash.hpp"
#include "utilities/stringUtils.hpp"


OopHandle SystemDictionaryShared::_shared_protection_domains;
OopHandle SystemDictionaryShared::_shared_jar_urls;
OopHandle SystemDictionaryShared::_shared_jar_manifests;
DEBUG_ONLY(bool SystemDictionaryShared::_no_class_loading_should_happen = false;)

class DumpTimeSharedClassInfo: public CHeapObj<mtClass> {
  bool                         _excluded;
public:
  struct DTLoaderConstraint {
    Symbol* _name;
    char _loader_type1;
    char _loader_type2;
    DTLoaderConstraint(Symbol* name, char l1, char l2) : _name(name), _loader_type1(l1), _loader_type2(l2) {
      _name->increment_refcount();
    }
    DTLoaderConstraint() : _name(NULL), _loader_type1('0'), _loader_type2('0') {}
    bool equals(const DTLoaderConstraint& t) {
      return t._name == _name &&
             ((t._loader_type1 == _loader_type1 && t._loader_type2 == _loader_type2) ||
              (t._loader_type2 == _loader_type1 && t._loader_type1 == _loader_type2));
    }
  };

  struct DTVerifierConstraint {
    Symbol* _name;
    Symbol* _from_name;
    DTVerifierConstraint() : _name(NULL), _from_name(NULL) {}
    DTVerifierConstraint(Symbol* n, Symbol* fn) : _name(n), _from_name(fn) {
      _name->increment_refcount();
      _from_name->increment_refcount();
    }
  };

  InstanceKlass*               _klass;
  bool                         _failed_verification;
  int                          _id;
  int                          _clsfile_size;
  int                          _clsfile_crc32;
  GrowableArray<DTVerifierConstraint>* _verifier_constraints;
  GrowableArray<char>*                 _verifier_constraint_flags;
  GrowableArray<DTLoaderConstraint>* _loader_constraints;

  DumpTimeSharedClassInfo() {
    _klass = NULL;
    _failed_verification = false;
    _id = -1;
    _clsfile_size = -1;
    _clsfile_crc32 = -1;
    _excluded = false;
    _verifier_constraints = NULL;
    _verifier_constraint_flags = NULL;
    _loader_constraints = NULL;
  }

  void add_verification_constraint(InstanceKlass* k, Symbol* name,
         Symbol* from_name, bool from_field_is_protected, bool from_is_array, bool from_is_object);
  void record_linking_constraint(Symbol* name, Handle loader1, Handle loader2);

  bool is_builtin() {
    return SystemDictionaryShared::is_builtin(_klass);
  }

  int num_verifier_constraints() {
    if (_verifier_constraint_flags != NULL) {
      return _verifier_constraint_flags->length();
    } else {
      return 0;
    }
  }

  int num_loader_constraints() {
    if (_loader_constraints != NULL) {
      return _loader_constraints->length();
    } else {
      return 0;
    }
  }

  void metaspace_pointers_do(MetaspaceClosure* it) {
    it->push(&_klass);
    if (_verifier_constraints != NULL) {
      for (int i = 0; i < _verifier_constraints->length(); i++) {
        DTVerifierConstraint* cons = _verifier_constraints->adr_at(i);
        it->push(&cons->_name);
        it->push(&cons->_from_name);
      }
    }
    if (_loader_constraints != NULL) {
      for (int i = 0; i < _loader_constraints->length(); i++) {
        DTLoaderConstraint* lc = _loader_constraints->adr_at(i);
        it->push(&lc->_name);
      }
    }
  }

  void set_excluded() {
    _excluded = true;
  }

  bool is_excluded() {
    // _klass may become NULL due to DynamicArchiveBuilder::set_to_null
    return _excluded || _failed_verification || _klass == NULL;
  }

  void set_failed_verification() {
    _failed_verification = true;
  }

  bool failed_verification() {
    return _failed_verification;
  }
};

inline unsigned DumpTimeSharedClassTable_hash(InstanceKlass* const& k) {
  if (DumpSharedSpaces) {
    // Deterministic archive contents
    uintx delta = k->name() - MetaspaceShared::symbol_rs_base();
    return primitive_hash<uintx>(delta);
  } else {
    // Deterministic archive is not possible because classes can be loaded
    // in multiple threads.
    return primitive_hash<InstanceKlass*>(k);
  }
}

class DumpTimeSharedClassTable: public ResourceHashtable<
  InstanceKlass*,
  DumpTimeSharedClassInfo,
  &DumpTimeSharedClassTable_hash,
  primitive_equals<InstanceKlass*>,
  15889, // prime number
  ResourceObj::C_HEAP>
{
  int _builtin_count;
  int _unregistered_count;
public:
  DumpTimeSharedClassInfo* find_or_allocate_info_for(InstanceKlass* k) {
    bool created = false;
    DumpTimeSharedClassInfo* p = put_if_absent(k, &created);
    if (created) {
      assert(!SystemDictionaryShared::no_class_loading_should_happen(),
             "no new classes can be loaded while dumping archive");
      p->_klass = k;
    } else {
      assert(p->_klass == k, "Sanity");
    }
    return p;
  }

  class CountClassByCategory : StackObj {
    DumpTimeSharedClassTable* _table;
  public:
    CountClassByCategory(DumpTimeSharedClassTable* table) : _table(table) {}
    bool do_entry(InstanceKlass* k, DumpTimeSharedClassInfo& info) {
      if (!info.is_excluded()) {
        if (info.is_builtin()) {
          ++ _table->_builtin_count;
        } else {
          ++ _table->_unregistered_count;
        }
      }
      return true; // keep on iterating
    }
  };

  void update_counts() {
    _builtin_count = 0;
    _unregistered_count = 0;
    CountClassByCategory counter(this);
    iterate(&counter);
  }

  int count_of(bool is_builtin) const {
    if (is_builtin) {
      return _builtin_count;
    } else {
      return _unregistered_count;
    }
  }
};

class RunTimeSharedClassInfo {
public:
  struct CrcInfo {
    int _clsfile_size;
    int _clsfile_crc32;
  };

  // This is different than  DumpTimeSharedClassInfo::DTVerifierConstraint. We use
  // u4 instead of Symbol* to save space on 64-bit CPU.
  struct RTVerifierConstraint {
    u4 _name;
    u4 _from_name;
    Symbol* name() { return (Symbol*)(SharedBaseAddress + _name);}
    Symbol* from_name() { return (Symbol*)(SharedBaseAddress + _from_name); }
  };

  struct RTLoaderConstraint {
    u4   _name;
    char _loader_type1;
    char _loader_type2;
    Symbol* constraint_name() {
      return (Symbol*)(SharedBaseAddress + _name);
    }
  };

  InstanceKlass* _klass;
  int _num_verifier_constraints;
  int _num_loader_constraints;

  // optional CrcInfo              _crc;  (only for UNREGISTERED classes)
  // optional RTLoaderConstraint   _loader_constraint_types[_num_loader_constraints]
  // optional RTVerifierConstraint _verifier_constraints[_num_verifier_constraints]
  // optional char                 _verifier_constraint_flags[_num_verifier_constraints]

private:
  static size_t header_size_size() {
    return sizeof(RunTimeSharedClassInfo);
  }
  static size_t crc_size(InstanceKlass* klass) {
    if (!SystemDictionaryShared::is_builtin(klass)) {
      return sizeof(CrcInfo);
    } else {
      return 0;
    }
  }
  static size_t verifier_constraints_size(int num_verifier_constraints) {
    return sizeof(RTVerifierConstraint) * num_verifier_constraints;
  }
  static size_t verifier_constraint_flags_size(int num_verifier_constraints) {
    return sizeof(char) * num_verifier_constraints;
  }
  static size_t loader_constraints_size(int num_loader_constraints) {
    return sizeof(RTLoaderConstraint) * num_loader_constraints;
  }

public:
  static size_t byte_size(InstanceKlass* klass, int num_verifier_constraints, int num_loader_constraints) {
    return header_size_size() +
           crc_size(klass) +
           loader_constraints_size(num_loader_constraints) +
           verifier_constraints_size(num_verifier_constraints) +
           verifier_constraint_flags_size(num_verifier_constraints);
  }

private:
  size_t crc_offset() const {
    return header_size_size();
  }

  size_t loader_constraints_offset() const  {
    return crc_offset() + crc_size(_klass);
  }
  size_t verifier_constraints_offset() const {
    return loader_constraints_offset() + loader_constraints_size(_num_loader_constraints);
  }
  size_t verifier_constraint_flags_offset() const {
    return verifier_constraints_offset() + verifier_constraints_size(_num_verifier_constraints);
  }

  void check_verifier_constraint_offset(int i) const {
    assert(0 <= i && i < _num_verifier_constraints, "sanity");
  }

  void check_loader_constraint_offset(int i) const {
    assert(0 <= i && i < _num_loader_constraints, "sanity");
  }

public:
  CrcInfo* crc() const {
    assert(crc_size(_klass) > 0, "must be");
    return (CrcInfo*)(address(this) + crc_offset());
  }
  RTVerifierConstraint* verifier_constraints() {
    assert(_num_verifier_constraints > 0, "sanity");
    return (RTVerifierConstraint*)(address(this) + verifier_constraints_offset());
  }
  RTVerifierConstraint* verifier_constraint_at(int i) {
    check_verifier_constraint_offset(i);
    return verifier_constraints() + i;
  }

  char* verifier_constraint_flags() {
    assert(_num_verifier_constraints > 0, "sanity");
    return (char*)(address(this) + verifier_constraint_flags_offset());
  }

  RTLoaderConstraint* loader_constraints() {
    assert(_num_loader_constraints > 0, "sanity");
    return (RTLoaderConstraint*)(address(this) + loader_constraints_offset());
  }

  RTLoaderConstraint* loader_constraint_at(int i) {
    check_loader_constraint_offset(i);
    return loader_constraints() + i;
  }

  static u4 object_delta_u4(Symbol* sym) {
    if (DynamicDumpSharedSpaces) {
      sym = DynamicArchive::original_to_target(sym);
    }
    return MetaspaceShared::object_delta_u4(sym);
  }

  void init(DumpTimeSharedClassInfo& info) {
    _klass = info._klass;
    if (!SystemDictionaryShared::is_builtin(_klass)) {
      CrcInfo* c = crc();
      c->_clsfile_size = info._clsfile_size;
      c->_clsfile_crc32 = info._clsfile_crc32;
    }
    _num_verifier_constraints = info.num_verifier_constraints();
    _num_loader_constraints   = info.num_loader_constraints();
    int i;
    if (_num_verifier_constraints > 0) {
      RTVerifierConstraint* vf_constraints = verifier_constraints();
      char* flags = verifier_constraint_flags();
      for (i = 0; i < _num_verifier_constraints; i++) {
        vf_constraints[i]._name      = object_delta_u4(info._verifier_constraints->at(i)._name);
        vf_constraints[i]._from_name = object_delta_u4(info._verifier_constraints->at(i)._from_name);
      }
      for (i = 0; i < _num_verifier_constraints; i++) {
        flags[i] = info._verifier_constraint_flags->at(i);
      }
    }

    if (_num_loader_constraints > 0) {
      RTLoaderConstraint* ld_constraints = loader_constraints();
      for (i = 0; i < _num_loader_constraints; i++) {
        ld_constraints[i]._name = object_delta_u4(info._loader_constraints->at(i)._name);
        ld_constraints[i]._loader_type1 = info._loader_constraints->at(i)._loader_type1;
        ld_constraints[i]._loader_type2 = info._loader_constraints->at(i)._loader_type2;
      }
    }
    if (DynamicDumpSharedSpaces) {
      _klass = DynamicArchive::original_to_target(info._klass);
    }
    ArchivePtrMarker::mark_pointer(&_klass);
  }

  bool matches(int clsfile_size, int clsfile_crc32) const {
    return crc()->_clsfile_size  == clsfile_size &&
           crc()->_clsfile_crc32 == clsfile_crc32;
  }

  char verifier_constraint_flag(int i) {
    check_verifier_constraint_offset(i);
    return verifier_constraint_flags()[i];
  }

private:
  // ArchiveCompactor::allocate() has reserved a pointer immediately before
  // archived InstanceKlasses. We can use this slot to do a quick
  // lookup of InstanceKlass* -> RunTimeSharedClassInfo* without
  // building a new hashtable.
  //
  //  info_pointer_addr(klass) --> 0x0100   RunTimeSharedClassInfo*
  //  InstanceKlass* klass     --> 0x0108   <C++ vtbl>
  //                               0x0110   fields from Klass ...
  static RunTimeSharedClassInfo** info_pointer_addr(InstanceKlass* klass) {
    return &((RunTimeSharedClassInfo**)klass)[-1];
  }

public:
  static RunTimeSharedClassInfo* get_for(InstanceKlass* klass) {
    return *info_pointer_addr(klass);
  }
  static void set_for(InstanceKlass* klass, RunTimeSharedClassInfo* record) {
    if (DynamicDumpSharedSpaces) {
      klass = DynamicArchive::original_to_buffer(klass);
      *info_pointer_addr(klass) = DynamicArchive::buffer_to_target(record);
    } else {
      *info_pointer_addr(klass) = record;
    }

    ArchivePtrMarker::mark_pointer(info_pointer_addr(klass));
  }

  // Used by RunTimeSharedDictionary to implement OffsetCompactHashtable::EQUALS
  static inline bool EQUALS(
       const RunTimeSharedClassInfo* value, Symbol* key, int len_unused) {
    return (value->_klass->name() == key);
  }
};

class RunTimeSharedDictionary : public OffsetCompactHashtable<
  Symbol*,
  const RunTimeSharedClassInfo*,
  RunTimeSharedClassInfo::EQUALS> {};

static DumpTimeSharedClassTable* _dumptime_table = NULL;
// SystemDictionaries in the base layer static archive
static RunTimeSharedDictionary _builtin_dictionary;
static RunTimeSharedDictionary _unregistered_dictionary;
// SystemDictionaries in the top layer dynamic archive
static RunTimeSharedDictionary _dynamic_builtin_dictionary;
static RunTimeSharedDictionary _dynamic_unregistered_dictionary;

oop SystemDictionaryShared::shared_protection_domain(int index) {
  return ((objArrayOop)_shared_protection_domains.resolve())->obj_at(index);
}

oop SystemDictionaryShared::shared_jar_url(int index) {
  return ((objArrayOop)_shared_jar_urls.resolve())->obj_at(index);
}

oop SystemDictionaryShared::shared_jar_manifest(int index) {
  return ((objArrayOop)_shared_jar_manifests.resolve())->obj_at(index);
}

Handle SystemDictionaryShared::get_shared_jar_manifest(int shared_path_index, TRAPS) {
  Handle manifest ;
  if (shared_jar_manifest(shared_path_index) == NULL) {
    SharedClassPathEntry* ent = FileMapInfo::shared_path(shared_path_index);
    long size = ent->manifest_size();
    if (size <= 0) {
      return Handle();
    }

    // ByteArrayInputStream bais = new ByteArrayInputStream(buf);
    const char* src = ent->manifest();
    assert(src != NULL, "No Manifest data");
    typeArrayOop buf = oopFactory::new_byteArray(size, CHECK_NH);
    typeArrayHandle bufhandle(THREAD, buf);
    ArrayAccess<>::arraycopy_from_native(reinterpret_cast<const jbyte*>(src),
                                         buf, typeArrayOopDesc::element_offset<jbyte>(0), size);

    Handle bais = JavaCalls::construct_new_instance(SystemDictionary::ByteArrayInputStream_klass(),
                      vmSymbols::byte_array_void_signature(),
                      bufhandle, CHECK_NH);

    // manifest = new Manifest(bais)
    manifest = JavaCalls::construct_new_instance(SystemDictionary::Jar_Manifest_klass(),
                      vmSymbols::input_stream_void_signature(),
                      bais, CHECK_NH);
    atomic_set_shared_jar_manifest(shared_path_index, manifest());
  }

  manifest = Handle(THREAD, shared_jar_manifest(shared_path_index));
  assert(manifest.not_null(), "sanity");
  return manifest;
}

Handle SystemDictionaryShared::get_shared_jar_url(int shared_path_index, TRAPS) {
  Handle url_h;
  if (shared_jar_url(shared_path_index) == NULL) {
    JavaValue result(T_OBJECT);
    const char* path = FileMapInfo::shared_path_name(shared_path_index);
    Handle path_string = java_lang_String::create_from_str(path, CHECK_(url_h));
    Klass* classLoaders_klass =
        SystemDictionary::jdk_internal_loader_ClassLoaders_klass();
    JavaCalls::call_static(&result, classLoaders_klass,
                           vmSymbols::toFileURL_name(),
                           vmSymbols::toFileURL_signature(),
                           path_string, CHECK_(url_h));

    atomic_set_shared_jar_url(shared_path_index, (oop)result.get_jobject());
  }

  url_h = Handle(THREAD, shared_jar_url(shared_path_index));
  assert(url_h.not_null(), "sanity");
  return url_h;
}

Handle SystemDictionaryShared::get_package_name(Symbol* class_name, TRAPS) {
  ResourceMark rm(THREAD);
  Handle pkgname_string;
  Symbol* pkg = ClassLoader::package_from_class_name(class_name);
  if (pkg != NULL) { // Package prefix found
    const char* pkgname = pkg->as_klass_external_name();
    pkgname_string = java_lang_String::create_from_str(pkgname,
                                                       CHECK_(pkgname_string));
  }
  return pkgname_string;
}

// Define Package for shared app classes from JAR file and also checks for
// package sealing (all done in Java code)
// See http://docs.oracle.com/javase/tutorial/deployment/jar/sealman.html
void SystemDictionaryShared::define_shared_package(Symbol*  class_name,
                                                   Handle class_loader,
                                                   Handle manifest,
                                                   Handle url,
                                                   TRAPS) {
  assert(SystemDictionary::is_system_class_loader(class_loader()), "unexpected class loader");
  // get_package_name() returns a NULL handle if the class is in unnamed package
  Handle pkgname_string = get_package_name(class_name, CHECK);
  if (pkgname_string.not_null()) {
    Klass* app_classLoader_klass = SystemDictionary::jdk_internal_loader_ClassLoaders_AppClassLoader_klass();
    JavaValue result(T_OBJECT);
    JavaCallArguments args(3);
    args.set_receiver(class_loader);
    args.push_oop(pkgname_string);
    args.push_oop(manifest);
    args.push_oop(url);
    JavaCalls::call_virtual(&result, app_classLoader_klass,
                            vmSymbols::defineOrCheckPackage_name(),
                            vmSymbols::defineOrCheckPackage_signature(),
                            &args,
                            CHECK);
  }
}

// Get the ProtectionDomain associated with the CodeSource from the classloader.
Handle SystemDictionaryShared::get_protection_domain_from_classloader(Handle class_loader,
                                                                      Handle url, TRAPS) {
  // CodeSource cs = new CodeSource(url, null);
  Handle cs = JavaCalls::construct_new_instance(SystemDictionary::CodeSource_klass(),
                  vmSymbols::url_code_signer_array_void_signature(),
                  url, Handle(), CHECK_NH);

  // protection_domain = SecureClassLoader.getProtectionDomain(cs);
  Klass* secureClassLoader_klass = SystemDictionary::SecureClassLoader_klass();
  JavaValue obj_result(T_OBJECT);
  JavaCalls::call_virtual(&obj_result, class_loader, secureClassLoader_klass,
                          vmSymbols::getProtectionDomain_name(),
                          vmSymbols::getProtectionDomain_signature(),
                          cs, CHECK_NH);
  return Handle(THREAD, (oop)obj_result.get_jobject());
}

// Returns the ProtectionDomain associated with the JAR file identified by the url.
Handle SystemDictionaryShared::get_shared_protection_domain(Handle class_loader,
                                                            int shared_path_index,
                                                            Handle url,
                                                            TRAPS) {
  Handle protection_domain;
  if (shared_protection_domain(shared_path_index) == NULL) {
    Handle pd = get_protection_domain_from_classloader(class_loader, url, THREAD);
    atomic_set_shared_protection_domain(shared_path_index, pd());
  }

  // Acquire from the cache because if another thread beats the current one to
  // set the shared protection_domain and the atomic_set fails, the current thread
  // needs to get the updated protection_domain from the cache.
  protection_domain = Handle(THREAD, shared_protection_domain(shared_path_index));
  assert(protection_domain.not_null(), "sanity");
  return protection_domain;
}

// Returns the ProtectionDomain associated with the moduleEntry.
Handle SystemDictionaryShared::get_shared_protection_domain(Handle class_loader,
                                                            ModuleEntry* mod, TRAPS) {
  ClassLoaderData *loader_data = mod->loader_data();
  if (mod->shared_protection_domain() == NULL) {
    Symbol* location = mod->location();
    if (location != NULL) {
      Handle location_string = java_lang_String::create_from_symbol(
                                     location, CHECK_NH);
      Handle url;
      JavaValue result(T_OBJECT);
      if (location->starts_with("jrt:/")) {
        url = JavaCalls::construct_new_instance(SystemDictionary::URL_klass(),
                                                vmSymbols::string_void_signature(),
                                                location_string, CHECK_NH);
      } else {
        Klass* classLoaders_klass =
          SystemDictionary::jdk_internal_loader_ClassLoaders_klass();
        JavaCalls::call_static(&result, classLoaders_klass, vmSymbols::toFileURL_name(),
                               vmSymbols::toFileURL_signature(),
                               location_string, CHECK_NH);
        url = Handle(THREAD, (oop)result.get_jobject());
      }

      Handle pd = get_protection_domain_from_classloader(class_loader, url,
                                                         CHECK_NH);
      mod->set_shared_protection_domain(loader_data, pd);
    }
  }

  Handle protection_domain(THREAD, mod->shared_protection_domain());
  assert(protection_domain.not_null(), "sanity");
  return protection_domain;
}

// Initializes the java.lang.Package and java.security.ProtectionDomain objects associated with
// the given InstanceKlass.
// Returns the ProtectionDomain for the InstanceKlass.
Handle SystemDictionaryShared::init_security_info(Handle class_loader, InstanceKlass* ik, PackageEntry* pkg_entry, TRAPS) {
  Handle pd;

  if (ik != NULL) {
    int index = ik->shared_classpath_index();
    assert(index >= 0, "Sanity");
    SharedClassPathEntry* ent = FileMapInfo::shared_path(index);
    Symbol* class_name = ik->name();

    if (ent->is_modules_image()) {
      // For shared app/platform classes originated from the run-time image:
      //   The ProtectionDomains are cached in the corresponding ModuleEntries
      //   for fast access by the VM.
      // all packages from module image are already created during VM bootstrap in
      // Modules::define_module().
      assert(pkg_entry != NULL, "archived class in module image cannot be from unnamed package");
      ModuleEntry* mod_entry = pkg_entry->module();
      pd = get_shared_protection_domain(class_loader, mod_entry, THREAD);
    } else {
      // For shared app/platform classes originated from JAR files on the class path:
      //   Each of the 3 SystemDictionaryShared::_shared_xxx arrays has the same length
      //   as the shared classpath table in the shared archive (see
      //   FileMap::_shared_path_table in filemap.hpp for details).
      //
      //   If a shared InstanceKlass k is loaded from the class path, let
      //
      //     index = k->shared_classpath_index():
      //
      //   FileMap::_shared_path_table[index] identifies the JAR file that contains k.
      //
      //   k's protection domain is:
      //
      //     ProtectionDomain pd = _shared_protection_domains[index];
      //
      //   and k's Package is initialized using
      //
      //     manifest = _shared_jar_manifests[index];
      //     url = _shared_jar_urls[index];
      //     define_shared_package(class_name, class_loader, manifest, url, CHECK_(pd));
      //
      //   Note that if an element of these 3 _shared_xxx arrays is NULL, it will be initialized by
      //   the corresponding SystemDictionaryShared::get_shared_xxx() function.
      Handle manifest = get_shared_jar_manifest(index, CHECK_(pd));
      Handle url = get_shared_jar_url(index, CHECK_(pd));
      define_shared_package(class_name, class_loader, manifest, url, CHECK_(pd));
      pd = get_shared_protection_domain(class_loader, index, url, CHECK_(pd));
    }
  }
  return pd;
}

bool SystemDictionaryShared::is_sharing_possible(ClassLoaderData* loader_data) {
  oop class_loader = loader_data->class_loader();
  return (class_loader == NULL ||
          SystemDictionary::is_system_class_loader(class_loader) ||
          SystemDictionary::is_platform_class_loader(class_loader));
}

// Currently AppCDS only archives classes from the run-time image, the
// -Xbootclasspath/a path, the class path, and the module path.
//
// Check if a shared class can be loaded by the specific classloader. Following
// are the "visible" archived classes for different classloaders.
//
// NULL classloader:
//   - see SystemDictionary::is_shared_class_visible()
// Platform classloader:
//   - Module class from runtime image. ModuleEntry must be defined in the
//     classloader.
// App classloader:
//   - Module Class from runtime image and module path. ModuleEntry must be defined in the
//     classloader.
//   - Class from -cp. The class must have no PackageEntry defined in any of the
//     boot/platform/app classloader, or must be in the unnamed module defined in the
//     AppClassLoader.
bool SystemDictionaryShared::is_shared_class_visible_for_classloader(
                                                     InstanceKlass* ik,
                                                     Handle class_loader,
                                                     Symbol* pkg_name,
                                                     PackageEntry* pkg_entry,
                                                     ModuleEntry* mod_entry,
                                                     TRAPS) {
  assert(class_loader.not_null(), "Class loader should not be NULL");
  assert(Universe::is_module_initialized(), "Module system is not initialized");
  ResourceMark rm(THREAD);

  int path_index = ik->shared_classpath_index();
  SharedClassPathEntry* ent =
            (SharedClassPathEntry*)FileMapInfo::shared_path(path_index);

  if (SystemDictionary::is_platform_class_loader(class_loader())) {
    assert(ent != NULL, "shared class for PlatformClassLoader should have valid SharedClassPathEntry");
    // The PlatformClassLoader can only load archived class originated from the
    // run-time image. The class' PackageEntry/ModuleEntry must be
    // defined by the PlatformClassLoader.
    if (mod_entry != NULL) {
      // PackageEntry/ModuleEntry is found in the classloader. Check if the
      // ModuleEntry's location agrees with the archived class' origination.
      if (ent->is_modules_image() && mod_entry->location()->starts_with("jrt:")) {
        return true; // Module class from the runtime image
      }
    }
  } else if (SystemDictionary::is_system_class_loader(class_loader())) {
    assert(ent != NULL, "shared class for system loader should have valid SharedClassPathEntry");
    if (pkg_name == NULL) {
      // The archived class is in the unnamed package. Currently, the boot image
      // does not contain any class in the unnamed package.
      assert(!ent->is_modules_image(), "Class in the unnamed package must be from the classpath");
      if (path_index >= ClassLoaderExt::app_class_paths_start_index()) {
        assert(path_index < ClassLoaderExt::app_module_paths_start_index(), "invalid path_index");
        return true;
      }
    } else {
      // Check if this is from a PackageEntry/ModuleEntry defined in the AppClassloader.
      if (pkg_entry == NULL) {
        // It's not guaranteed that the class is from the classpath if the
        // PackageEntry cannot be found from the AppClassloader. Need to check
        // the boot and platform classloader as well.
        ClassLoaderData* platform_loader_data =
          ClassLoaderData::class_loader_data_or_null(SystemDictionary::java_platform_loader()); // can be NULL during bootstrap
        if ((platform_loader_data == NULL ||
             ClassLoader::get_package_entry(pkg_name, platform_loader_data) == NULL) &&
             ClassLoader::get_package_entry(pkg_name, ClassLoaderData::the_null_class_loader_data()) == NULL) {
          // The PackageEntry is not defined in any of the boot/platform/app classloaders.
          // The archived class must from -cp path and not from the runtime image.
          if (!ent->is_modules_image() && path_index >= ClassLoaderExt::app_class_paths_start_index() &&
                                          path_index < ClassLoaderExt::app_module_paths_start_index()) {
            return true;
          }
        }
      } else if (mod_entry != NULL) {
        // The package/module is defined in the AppClassLoader. We support
        // archiving application module class from the runtime image or from
        // a named module from a module path.
        // Packages from the -cp path are in the unnamed_module.
        if (ent->is_modules_image() && mod_entry->location()->starts_with("jrt:")) {
          // shared module class from runtime image
          return true;
        } else if (pkg_entry->in_unnamed_module() && path_index >= ClassLoaderExt::app_class_paths_start_index() &&
            path_index < ClassLoaderExt::app_module_paths_start_index()) {
          // shared class from -cp
          DEBUG_ONLY( \
            ClassLoaderData* loader_data = class_loader_data(class_loader); \
            assert(mod_entry == loader_data->unnamed_module(), "the unnamed module is not defined in the classloader");)
          return true;
        } else {
          if(!pkg_entry->in_unnamed_module() &&
              (path_index >= ClassLoaderExt::app_module_paths_start_index())&&
              (path_index < FileMapInfo::get_number_of_shared_paths()) &&
              (strcmp(ent->name(), ClassLoader::skip_uri_protocol(mod_entry->location()->as_C_string())) == 0)) {
            // shared module class from module path
            return true;
          } else {
            assert(path_index < FileMapInfo::get_number_of_shared_paths(), "invalid path_index");
          }
        }
      }
    }
  } else {
    // TEMP: if a shared class can be found by a custom loader, consider it visible now.
    // FIXME: is this actually correct?
    return true;
  }
  return false;
}

bool SystemDictionaryShared::has_platform_or_app_classes() {
  if (FileMapInfo::current_info()->has_platform_or_app_classes()) {
    return true;
  }
  if (DynamicArchive::is_mapped() &&
      FileMapInfo::dynamic_info()->has_platform_or_app_classes()) {
    return true;
  }
  return false;
}

// The following stack shows how this code is reached:
//
//   [0] SystemDictionaryShared::find_or_load_shared_class()
//   [1] JVM_FindLoadedClass
//   [2] java.lang.ClassLoader.findLoadedClass0()
//   [3] java.lang.ClassLoader.findLoadedClass()
//   [4] jdk.internal.loader.BuiltinClassLoader.loadClassOrNull()
//   [5] jdk.internal.loader.BuiltinClassLoader.loadClass()
//   [6] jdk.internal.loader.ClassLoaders$AppClassLoader.loadClass(), or
//       jdk.internal.loader.ClassLoaders$PlatformClassLoader.loadClass()
//
// AppCDS supports fast class loading for these 2 built-in class loaders:
//    jdk.internal.loader.ClassLoaders$PlatformClassLoader
//    jdk.internal.loader.ClassLoaders$AppClassLoader
// with the following assumptions (based on the JDK core library source code):
//
// [a] these two loaders use the BuiltinClassLoader.loadClassOrNull() to
//     load the named class.
// [b] BuiltinClassLoader.loadClassOrNull() first calls findLoadedClass(name).
// [c] At this point, if we can find the named class inside the
//     shared_dictionary, we can perform further checks (see
//     is_shared_class_visible_for_classloader() to ensure that this class
//     was loaded by the same class loader during dump time.
//
// Given these assumptions, we intercept the findLoadedClass() call to invoke
// SystemDictionaryShared::find_or_load_shared_class() to load the shared class from
// the archive for the 2 built-in class loaders. This way,
// we can improve start-up because we avoid decoding the classfile,
// and avoid delegating to the parent loader.
//
// NOTE: there's a lot of assumption about the Java code. If any of that change, this
// needs to be redesigned.

InstanceKlass* SystemDictionaryShared::find_or_load_shared_class(
                 Symbol* name, Handle class_loader, TRAPS) {
  InstanceKlass* k = NULL;
  if (UseSharedSpaces) {
    if (!has_platform_or_app_classes()) {
      return NULL;
    }

    if (SystemDictionary::is_system_class_loader(class_loader()) ||
        SystemDictionary::is_platform_class_loader(class_loader())) {
      // Fix for 4474172; see evaluation for more details
      class_loader = Handle(
        THREAD, java_lang_ClassLoader::non_reflection_class_loader(class_loader()));
      ClassLoaderData *loader_data = register_loader(class_loader);
      Dictionary* dictionary = loader_data->dictionary();

      unsigned int d_hash = dictionary->compute_hash(name);

      bool DoObjectLock = true;
      if (is_parallelCapable(class_loader)) {
        DoObjectLock = false;
      }

      // Make sure we are synchronized on the class loader before we proceed
      //
      // Note: currently, find_or_load_shared_class is called only from
      // JVM_FindLoadedClass and used for PlatformClassLoader and AppClassLoader,
      // which are parallel-capable loaders, so this lock is NOT taken.
      Handle lockObject = compute_loader_lock_object(class_loader, THREAD);
      check_loader_lock_contention(lockObject, THREAD);
      ObjectLocker ol(lockObject, THREAD, DoObjectLock);

      {
        MutexLocker mu(THREAD, SystemDictionary_lock);
        InstanceKlass* check = find_class(d_hash, name, dictionary);
        if (check != NULL) {
          return check;
        }
      }

      k = load_shared_class_for_builtin_loader(name, class_loader, THREAD);
      if (k != NULL) {
        define_instance_class(k, CHECK_NULL);
      }
    }
  }
  return k;
}

PackageEntry* SystemDictionaryShared::get_package_entry_from_class_name(Handle class_loader, Symbol* class_name) {
  PackageEntry* pkg_entry = NULL;
  TempNewSymbol pkg_name = ClassLoader::package_from_class_name(class_name);
  if (pkg_name != NULL) {
    pkg_entry = class_loader_data(class_loader)->packages()->lookup_only(pkg_name);
  }
  return pkg_entry;
}

InstanceKlass* SystemDictionaryShared::load_shared_class_for_builtin_loader(
                 Symbol* class_name, Handle class_loader, TRAPS) {
  assert(UseSharedSpaces, "must be");
  InstanceKlass* ik = find_builtin_class(class_name);

  if (ik != NULL) {
    if ((ik->is_shared_app_class() &&
         SystemDictionary::is_system_class_loader(class_loader()))  ||
        (ik->is_shared_platform_class() &&
         SystemDictionary::is_platform_class_loader(class_loader()))) {
      PackageEntry* pkg_entry = get_package_entry_from_class_name(class_loader, class_name);
      Handle protection_domain =
        SystemDictionaryShared::init_security_info(class_loader, ik, pkg_entry, CHECK_NULL);
      return load_shared_class(ik, class_loader, protection_domain, NULL, pkg_entry, THREAD);
    }
  }
  return NULL;
}

void SystemDictionaryShared::allocate_shared_protection_domain_array(int size, TRAPS) {
  if (_shared_protection_domains.resolve() == NULL) {
    oop spd = oopFactory::new_objArray(
        SystemDictionary::ProtectionDomain_klass(), size, CHECK);
    _shared_protection_domains = OopHandle::create(spd);
  }
}

void SystemDictionaryShared::allocate_shared_jar_url_array(int size, TRAPS) {
  if (_shared_jar_urls.resolve() == NULL) {
    oop sju = oopFactory::new_objArray(
        SystemDictionary::URL_klass(), size, CHECK);
    _shared_jar_urls = OopHandle::create(sju);
  }
}

void SystemDictionaryShared::allocate_shared_jar_manifest_array(int size, TRAPS) {
  if (_shared_jar_manifests.resolve() == NULL) {
    oop sjm = oopFactory::new_objArray(
        SystemDictionary::Jar_Manifest_klass(), size, CHECK);
    _shared_jar_manifests = OopHandle::create(sjm);
  }
}

void SystemDictionaryShared::allocate_shared_data_arrays(int size, TRAPS) {
  allocate_shared_protection_domain_array(size, CHECK);
  allocate_shared_jar_url_array(size, CHECK);
  allocate_shared_jar_manifest_array(size, CHECK);
}

// This function is called for loading only UNREGISTERED classes
InstanceKlass* SystemDictionaryShared::lookup_from_stream(Symbol* class_name,
                                                          Handle class_loader,
                                                          Handle protection_domain,
                                                          const ClassFileStream* cfs,
                                                          TRAPS) {
  if (!UseSharedSpaces) {
    return NULL;
  }
  if (class_name == NULL) {  // don't do this for hidden and unsafe anonymous classes
    return NULL;
  }
  if (class_loader.is_null() ||
      SystemDictionary::is_system_class_loader(class_loader()) ||
      SystemDictionary::is_platform_class_loader(class_loader())) {
    // Do nothing for the BUILTIN loaders.
    return NULL;
  }

  const RunTimeSharedClassInfo* record = find_record(&_unregistered_dictionary, &_dynamic_unregistered_dictionary, class_name);
  if (record == NULL) {
    return NULL;
  }

  int clsfile_size  = cfs->length();
  int clsfile_crc32 = ClassLoader::crc32(0, (const char*)cfs->buffer(), cfs->length());

  if (!record->matches(clsfile_size, clsfile_crc32)) {
    return NULL;
  }

  return acquire_class_for_current_thread(record->_klass, class_loader,
                                          protection_domain, cfs,
                                          THREAD);
}

InstanceKlass* SystemDictionaryShared::acquire_class_for_current_thread(
                   InstanceKlass *ik,
                   Handle class_loader,
                   Handle protection_domain,
                   const ClassFileStream *cfs,
                   TRAPS) {
  ClassLoaderData* loader_data = ClassLoaderData::class_loader_data(class_loader());

  {
    MutexLocker mu(THREAD, SharedDictionary_lock);
    if (ik->class_loader_data() != NULL) {
      //    ik is already loaded (by this loader or by a different loader)
      // or ik is being loaded by a different thread (by this loader or by a different loader)
      return NULL;
    }

    // No other thread has acquired this yet, so give it to *this thread*
    ik->set_class_loader_data(loader_data);
  }

  // No longer holding SharedDictionary_lock
  // No need to lock, as <ik> can be held only by a single thread.
  loader_data->add_class(ik);

  // Get the package entry.
  PackageEntry* pkg_entry = get_package_entry_from_class_name(class_loader, ik->name());

  // Load and check super/interfaces, restore unsharable info
  InstanceKlass* shared_klass = load_shared_class(ik, class_loader, protection_domain,
                                                  cfs, pkg_entry, THREAD);
  if (shared_klass == NULL || HAS_PENDING_EXCEPTION) {
    // TODO: clean up <ik> so it can be used again
    return NULL;
  }

  return shared_klass;
}

static ResourceHashtable<
  Symbol*, bool,
  primitive_hash<Symbol*>,
  primitive_equals<Symbol*>,
  6661,                             // prime number
  ResourceObj::C_HEAP> _loaded_unregistered_classes;

bool SystemDictionaryShared::add_unregistered_class(InstanceKlass* k, TRAPS) {
  // We don't allow duplicated unregistered classes of the same name.
  assert(DumpSharedSpaces, "only when dumping");
  Symbol* name = k->name();
  bool created = false;
  _loaded_unregistered_classes.put_if_absent(name, true, &created);
  if (created) {
    MutexLocker mu_r(THREAD, Compile_lock); // add_to_hierarchy asserts this.
    SystemDictionary::add_to_hierarchy(k, CHECK_false);
  }
  return created;
}

// This function is called to resolve the super/interfaces of shared classes for
// non-built-in loaders. E.g., ChildClass in the below example
// where "super:" (and optionally "interface:") have been specified.
//
// java/lang/Object id: 0
// Interface   id: 2 super: 0 source: cust.jar
// ChildClass  id: 4 super: 0 interfaces: 2 source: cust.jar
InstanceKlass* SystemDictionaryShared::dump_time_resolve_super_or_fail(
    Symbol* child_name, Symbol* class_name, Handle class_loader,
    Handle protection_domain, bool is_superclass, TRAPS) {

  assert(DumpSharedSpaces, "only when dumping");

  ClassListParser* parser = ClassListParser::instance();
  if (parser == NULL) {
    // We're still loading the well-known classes, before the ClassListParser is created.
    return NULL;
  }
  if (child_name->equals(parser->current_class_name())) {
    // When this function is called, all the numbered super and interface types
    // must have already been loaded. Hence this function is never recursively called.
    if (is_superclass) {
      return parser->lookup_super_for_current_class(class_name);
    } else {
      return parser->lookup_interface_for_current_class(class_name);
    }
  } else {
    // The VM is not trying to resolve a super type of parser->current_class_name().
    // Instead, it's resolving an error class (because parser->current_class_name() has
    // failed parsing or verification). Don't do anything here.
    return NULL;
  }
}

DumpTimeSharedClassInfo* SystemDictionaryShared::find_or_allocate_info_for(InstanceKlass* k) {
  MutexLocker ml(DumpTimeTable_lock, Mutex::_no_safepoint_check_flag);
  if (_dumptime_table == NULL) {
    _dumptime_table = new (ResourceObj::C_HEAP, mtClass)DumpTimeSharedClassTable();
  }
  return _dumptime_table->find_or_allocate_info_for(k);
}

void SystemDictionaryShared::set_shared_class_misc_info(InstanceKlass* k, ClassFileStream* cfs) {
  Arguments::assert_is_dumping_archive();
  assert(!is_builtin(k), "must be unregistered class");
  DumpTimeSharedClassInfo* info = find_or_allocate_info_for(k);
  info->_clsfile_size  = cfs->length();
  info->_clsfile_crc32 = ClassLoader::crc32(0, (const char*)cfs->buffer(), cfs->length());
}

void SystemDictionaryShared::init_dumptime_info(InstanceKlass* k) {
  (void)find_or_allocate_info_for(k);
}

void SystemDictionaryShared::remove_dumptime_info(InstanceKlass* k) {
  MutexLocker ml(DumpTimeTable_lock, Mutex::_no_safepoint_check_flag);
  DumpTimeSharedClassInfo* p = _dumptime_table->get(k);
  if (p == NULL) {
    return;
  }
  if (p->_verifier_constraints != NULL) {
    for (int i = 0; i < p->_verifier_constraints->length(); i++) {
      DumpTimeSharedClassInfo::DTVerifierConstraint constraint = p->_verifier_constraints->at(i);
      if (constraint._name != NULL ) {
        constraint._name->decrement_refcount();
      }
      if (constraint._from_name != NULL ) {
        constraint._from_name->decrement_refcount();
      }
    }
    FREE_C_HEAP_ARRAY(DumpTimeSharedClassInfo::DTVerifierConstraint, p->_verifier_constraints);
    p->_verifier_constraints = NULL;
    FREE_C_HEAP_ARRAY(char, p->_verifier_constraint_flags);
    p->_verifier_constraint_flags = NULL;
  }
  if (p->_loader_constraints != NULL) {
    for (int i = 0; i < p->_loader_constraints->length(); i++) {
      DumpTimeSharedClassInfo::DTLoaderConstraint ld =  p->_loader_constraints->at(i);
      if (ld._name != NULL) {
        ld._name->decrement_refcount();
      }
    }
    FREE_C_HEAP_ARRAY(DumpTimeSharedClassInfo::DTLoaderConstraint, p->_loader_constraints);
    p->_loader_constraints = NULL;
  }
  _dumptime_table->remove(k);
}

bool SystemDictionaryShared::is_jfr_event_class(InstanceKlass *k) {
  while (k) {
    if (k->name()->equals("jdk/internal/event/Event")) {
      return true;
    }
    k = k->java_super();
  }
  return false;
}

void SystemDictionaryShared::warn_excluded(InstanceKlass* k, const char* reason) {
  ResourceMark rm;
  log_warning(cds)("Skipping %s: %s", k->name()->as_C_string(), reason);
}

bool SystemDictionaryShared::should_be_excluded(InstanceKlass* k) {
  if (k->is_hidden() || k->is_unsafe_anonymous()) {
    warn_excluded(k, "Hidden or Unsafe anonymous class");
    return true; // hidden and unsafe anonymous classes are not archived, skip
  }
  if (k->is_in_error_state()) {
    warn_excluded(k, "In error state");
    return true;
  }
  if (k->has_been_redefined()) {
    warn_excluded(k, "Has been redefined");
    return true;
  }
  if (k->shared_classpath_index() < 0 && is_builtin(k)) {
    // These are classes loaded from unsupported locations (such as those loaded by JVMTI native
    // agent during dump time).
    warn_excluded(k, "Unsupported location");
    return true;
  }
  if (k->signers() != NULL) {
    // We cannot include signed classes in the archive because the certificates
    // used during dump time may be different than those used during
    // runtime (due to expiration, etc).
    warn_excluded(k, "Signed JAR");
    return true;
  }
  if (is_jfr_event_class(k)) {
    // We cannot include JFR event classes because they need runtime-specific
    // instrumentation in order to work with -XX:FlightRecorderOptions=retransform=false.
    // There are only a small number of these classes, so it's not worthwhile to
    // support them and make CDS more complicated.
    warn_excluded(k, "JFR event class");
    return true;
  }
  if (k->init_state() < InstanceKlass::linked) {
    // In CDS dumping, we will attempt to link all classes. Those that fail to link will
    // be recorded in DumpTimeSharedClassInfo.
    Arguments::assert_is_dumping_archive();

    // TODO -- rethink how this can be handled.
    // We should try to link ik, however, we can't do it here because
    // 1. We are at VM exit
    // 2. linking a class may cause other classes to be loaded, which means
    //    a custom ClassLoader.loadClass() may be called, at a point where the
    //    class loader doesn't expect it.
    if (has_class_failed_verification(k)) {
      warn_excluded(k, "Failed verification");
    } else {
      warn_excluded(k, "Not linked");
    }
    return true;
  }
  if (k->major_version() < 50 /*JAVA_6_VERSION*/) {
    ResourceMark rm;
    log_warning(cds)("Pre JDK 6 class not supported by CDS: %u.%u %s",
                     k->major_version(),  k->minor_version(), k->name()->as_C_string());
    return true;
  }

  InstanceKlass* super = k->java_super();
  if (super != NULL && should_be_excluded(super)) {
    ResourceMark rm;
    log_warning(cds)("Skipping %s: super class %s is excluded", k->name()->as_C_string(), super->name()->as_C_string());
    return true;
  }

  Array<InstanceKlass*>* interfaces = k->local_interfaces();
  int len = interfaces->length();
  for (int i = 0; i < len; i++) {
    InstanceKlass* intf = interfaces->at(i);
    if (should_be_excluded(intf)) {
      log_warning(cds)("Skipping %s: interface %s is excluded", k->name()->as_C_string(), intf->name()->as_C_string());
      return true;
    }
  }

  return false;
}

// k is a class before relocating by ArchiveCompactor
void SystemDictionaryShared::validate_before_archiving(InstanceKlass* k) {
  ResourceMark rm;
  const char* name = k->name()->as_C_string();
  DumpTimeSharedClassInfo* info = _dumptime_table->get(k);
  assert(_no_class_loading_should_happen, "class loading must be disabled");
  guarantee(info != NULL, "Class %s must be entered into _dumptime_table", name);
  guarantee(!info->is_excluded(), "Should not attempt to archive excluded class %s", name);
  if (is_builtin(k)) {
    guarantee(!k->is_shared_unregistered_class(),
              "Class loader type must be set for BUILTIN class %s", name);
  } else {
    guarantee(k->is_shared_unregistered_class(),
              "Class loader type must not be set for UNREGISTERED class %s", name);
  }
}

class ExcludeDumpTimeSharedClasses : StackObj {
public:
  bool do_entry(InstanceKlass* k, DumpTimeSharedClassInfo& info) {
    if (SystemDictionaryShared::should_be_excluded(k)) {
      info.set_excluded();
    }
    return true; // keep on iterating
  }
};

void SystemDictionaryShared::check_excluded_classes() {
  ExcludeDumpTimeSharedClasses excl;
  _dumptime_table->iterate(&excl);
  _dumptime_table->update_counts();
}

bool SystemDictionaryShared::is_excluded_class(InstanceKlass* k) {
  assert(_no_class_loading_should_happen, "sanity");
  Arguments::assert_is_dumping_archive();
  return find_or_allocate_info_for(k)->is_excluded();
}

void SystemDictionaryShared::set_class_has_failed_verification(InstanceKlass* ik) {
  Arguments::assert_is_dumping_archive();
  find_or_allocate_info_for(ik)->set_failed_verification();
}

bool SystemDictionaryShared::has_class_failed_verification(InstanceKlass* ik) {
  Arguments::assert_is_dumping_archive();
  if (_dumptime_table == NULL) {
    assert(DynamicDumpSharedSpaces, "sanity");
    assert(ik->is_shared(), "must be a shared class in the static archive");
    return false;
  }
  DumpTimeSharedClassInfo* p = _dumptime_table->get(ik);
  return (p == NULL) ? false : p->failed_verification();
}

class IterateDumpTimeSharedClassTable : StackObj {
  MetaspaceClosure *_it;
public:
  IterateDumpTimeSharedClassTable(MetaspaceClosure* it) : _it(it) {}

  bool do_entry(InstanceKlass* k, DumpTimeSharedClassInfo& info) {
    if (!info.is_excluded()) {
      info.metaspace_pointers_do(_it);
    }
    return true; // keep on iterating
  }
};

void SystemDictionaryShared::dumptime_classes_do(class MetaspaceClosure* it) {
  IterateDumpTimeSharedClassTable iter(it);
  _dumptime_table->iterate(&iter);
}

bool SystemDictionaryShared::add_verification_constraint(InstanceKlass* k, Symbol* name,
         Symbol* from_name, bool from_field_is_protected, bool from_is_array, bool from_is_object) {
  Arguments::assert_is_dumping_archive();
  DumpTimeSharedClassInfo* info = find_or_allocate_info_for(k);
  info->add_verification_constraint(k, name, from_name, from_field_is_protected,
                                    from_is_array, from_is_object);

  if (DynamicDumpSharedSpaces) {
    // For dynamic dumping, we can resolve all the constraint classes for all class loaders during
    // the initial run prior to creating the archive before vm exit. We will also perform verification
    // check when running with the archive.
    return false;
  } else {
    if (is_builtin(k)) {
      // For builtin class loaders, we can try to complete the verification check at dump time,
      // because we can resolve all the constraint classes. We will also perform verification check
      // when running with the archive.
      return false;
    } else {
      // For non-builtin class loaders, we cannot complete the verification check at dump time,
      // because at dump time we don't know how to resolve classes for such loaders.
      return true;
    }
  }
}

void DumpTimeSharedClassInfo::add_verification_constraint(InstanceKlass* k, Symbol* name,
         Symbol* from_name, bool from_field_is_protected, bool from_is_array, bool from_is_object) {
  if (_verifier_constraints == NULL) {
    _verifier_constraints = new(ResourceObj::C_HEAP, mtClass) GrowableArray<DTVerifierConstraint>(4, true, mtClass);
  }
  if (_verifier_constraint_flags == NULL) {
    _verifier_constraint_flags = new(ResourceObj::C_HEAP, mtClass) GrowableArray<char>(4, true, mtClass);
  }
  GrowableArray<DTVerifierConstraint>* vc_array = _verifier_constraints;
  for (int i = 0; i < vc_array->length(); i++) {
    DTVerifierConstraint* p = vc_array->adr_at(i);
    if (name == p->_name && from_name == p->_from_name) {
      return;
    }
  }
  DTVerifierConstraint cons(name, from_name);
  vc_array->append(cons);

  GrowableArray<char>* vcflags_array = _verifier_constraint_flags;
  char c = 0;
  c |= from_field_is_protected ? SystemDictionaryShared::FROM_FIELD_IS_PROTECTED : 0;
  c |= from_is_array           ? SystemDictionaryShared::FROM_IS_ARRAY           : 0;
  c |= from_is_object          ? SystemDictionaryShared::FROM_IS_OBJECT          : 0;
  vcflags_array->append(c);

  if (log_is_enabled(Trace, cds, verification)) {
    ResourceMark rm;
    log_trace(cds, verification)("add_verification_constraint: %s: %s must be subclass of %s [0x%x]",
                                 k->external_name(), from_name->as_klass_external_name(),
                                 name->as_klass_external_name(), c);
  }
}

static char get_loader_type_by(oop  loader) {
  assert(SystemDictionary::is_builtin_class_loader(loader), "Must be built-in loader");
  if (SystemDictionary::is_boot_class_loader(loader)) {
    return (char)ClassLoader::BOOT_LOADER;
  } else if (SystemDictionary::is_platform_class_loader(loader)) {
    return (char)ClassLoader::PLATFORM_LOADER;
  } else {
    assert(SystemDictionary::is_system_class_loader(loader), "Class loader mismatch");
    return (char)ClassLoader::APP_LOADER;
  }
}

static oop get_class_loader_by(char type) {
  if (type == (char)ClassLoader::BOOT_LOADER) {
    return (oop)NULL;
  } else if (type == (char)ClassLoader::PLATFORM_LOADER) {
    return SystemDictionary::java_platform_loader();
  } else {
    assert (type == (char)ClassLoader::APP_LOADER, "Sanity");
    return SystemDictionary::java_system_loader();
  }
}

void DumpTimeSharedClassInfo::record_linking_constraint(Symbol* name, Handle loader1, Handle loader2) {
  assert(loader1 != loader2, "sanity");
  LogTarget(Info, class, loader, constraints) log;
  if (_loader_constraints == NULL) {
    _loader_constraints = new (ResourceObj::C_HEAP, mtClass) GrowableArray<DTLoaderConstraint>(4, true, mtClass);
  }
  char lt1 = get_loader_type_by(loader1());
  char lt2 = get_loader_type_by(loader2());
  DTLoaderConstraint lc(name, lt1, lt2);
  for (int i = 0; i < _loader_constraints->length(); i++) {
    DTLoaderConstraint dt = _loader_constraints->at(i);
    if (lc.equals(dt)) {
      if (log.is_enabled()) {
        ResourceMark rm;
        // Use loader[0]/loader[1] to be consistent with the logs in loaderConstraints.cpp
        log.print("[CDS record loader constraint for class: %s constraint_name: %s loader[0]: %s loader[1]: %s already added]",
                  _klass->external_name(), name->as_C_string(),
                  ClassLoaderData::class_loader_data(loader1())->loader_name_and_id(),
                  ClassLoaderData::class_loader_data(loader2())->loader_name_and_id());
      }
      return;
    }
  }
  _loader_constraints->append(lc);
  if (log.is_enabled()) {
    ResourceMark rm;
    // Use loader[0]/loader[1] to be consistent with the logs in loaderConstraints.cpp
    log.print("[CDS record loader constraint for class: %s constraint_name: %s loader[0]: %s loader[1]: %s total %d]",
              _klass->external_name(), name->as_C_string(),
              ClassLoaderData::class_loader_data(loader1())->loader_name_and_id(),
              ClassLoaderData::class_loader_data(loader2())->loader_name_and_id(),
              _loader_constraints->length());
  }
}

void SystemDictionaryShared::check_verification_constraints(InstanceKlass* klass,
                                                            TRAPS) {
  assert(!DumpSharedSpaces && UseSharedSpaces, "called at run time with CDS enabled only");
  RunTimeSharedClassInfo* record = RunTimeSharedClassInfo::get_for(klass);

  int length = record->_num_verifier_constraints;
  if (length > 0) {
    for (int i = 0; i < length; i++) {
      RunTimeSharedClassInfo::RTVerifierConstraint* vc = record->verifier_constraint_at(i);
      Symbol* name      = vc->name();
      Symbol* from_name = vc->from_name();
      char c            = record->verifier_constraint_flag(i);

      if (log_is_enabled(Trace, cds, verification)) {
        ResourceMark rm(THREAD);
        log_trace(cds, verification)("check_verification_constraint: %s: %s must be subclass of %s [0x%x]",
                                     klass->external_name(), from_name->as_klass_external_name(),
                                     name->as_klass_external_name(), c);
      }

      bool from_field_is_protected = (c & SystemDictionaryShared::FROM_FIELD_IS_PROTECTED) ? true : false;
      bool from_is_array           = (c & SystemDictionaryShared::FROM_IS_ARRAY)           ? true : false;
      bool from_is_object          = (c & SystemDictionaryShared::FROM_IS_OBJECT)          ? true : false;

      bool ok = VerificationType::resolve_and_check_assignability(klass, name,
         from_name, from_field_is_protected, from_is_array, from_is_object, CHECK);
      if (!ok) {
        ResourceMark rm(THREAD);
        stringStream ss;

        ss.print_cr("Bad type on operand stack");
        ss.print_cr("Exception Details:");
        ss.print_cr("  Location:\n    %s", klass->name()->as_C_string());
        ss.print_cr("  Reason:\n    Type '%s' is not assignable to '%s'",
                    from_name->as_quoted_ascii(), name->as_quoted_ascii());
        THROW_MSG(vmSymbols::java_lang_VerifyError(), ss.as_string());
      }
    }
  }
}

// Record class loader constraints that are checked inside
// InstanceKlass::link_class(), so that these can be checked quickly
// at runtime without laying out the vtable/itables.
void SystemDictionaryShared::record_linking_constraint(Symbol* name, InstanceKlass* klass,
                                                    Handle loader1, Handle loader2, TRAPS) {
  // A linking constraint check is executed when:
  //   - klass extends or implements type S
  //   - klass overrides method S.M(...) with X.M
  //     - If klass defines the method M, X is
  //       the same as klass.
  //     - If klass does not define the method M,
  //       X must be a supertype of klass and X.M is
  //       a default method defined by X.
  //   - loader1 = X->class_loader()
  //   - loader2 = S->class_loader()
  //   - loader1 != loader2
  //   - M's paramater(s) include an object type T
  // We require that
  //   - whenever loader1 and loader2 try to
  //     resolve the type T, they must always resolve to
  //     the same InstanceKlass.
  // NOTE: type T may or may not be currently resolved in
  // either of these two loaders. The check itself does not
  // try to resolve T.
  oop klass_loader = klass->class_loader();
  assert(klass_loader != NULL, "should not be called for boot loader");
  assert(loader1 != loader2, "must be");

  if (!is_system_class_loader(klass_loader) &&
      !is_platform_class_loader(klass_loader)) {
    // If klass is loaded by system/platform loaders, we can
    // guarantee that klass and S must be loaded by the same
    // respective loader between dump time and run time, and
    // the exact same check on (name, loader1, loader2) will
    // be executed. Hence, we can cache this check and execute
    // it at runtime without walking the vtable/itables.
    //
    // This cannot be guaranteed for classes loaded by other
    // loaders, so we bail.
    return;
  }

  if (THREAD->is_VM_thread()) {
    assert(DynamicDumpSharedSpaces, "must be");
    // We are re-laying out the vtable/itables of the *copy* of
    // a class during the final stage of dynamic dumping. The
    // linking constraints for this class has already been recorded.
    return;
  }
  Arguments::assert_is_dumping_archive();
  DumpTimeSharedClassInfo* info = find_or_allocate_info_for(klass);
  info->record_linking_constraint(name, loader1, loader2);
}

// returns true IFF there's no need to re-initialize the i/v-tables for klass for
// the purpose of checking class loader constraints.
bool SystemDictionaryShared::check_linking_constraints(InstanceKlass* klass, TRAPS) {
  assert(!DumpSharedSpaces && UseSharedSpaces, "called at run time with CDS enabled only");
  LogTarget(Info, class, loader, constraints) log;
  if (klass->is_shared_boot_class()) {
    // No class loader constraint check performed for boot classes.
    return true;
  }
  if (klass->is_shared_platform_class() || klass->is_shared_app_class()) {
    RunTimeSharedClassInfo* info = RunTimeSharedClassInfo::get_for(klass);
    assert(info != NULL, "Sanity");
    if (info->_num_loader_constraints > 0) {
      HandleMark hm;
      for (int i = 0; i < info->_num_loader_constraints; i++) {
        RunTimeSharedClassInfo::RTLoaderConstraint* lc = info->loader_constraint_at(i);
        Symbol* name = lc->constraint_name();
        Handle loader1(THREAD, get_class_loader_by(lc->_loader_type1));
        Handle loader2(THREAD, get_class_loader_by(lc->_loader_type2));
        if (log.is_enabled()) {
          ResourceMark rm(THREAD);
          log.print("[CDS add loader constraint for class %s symbol %s loader[0] %s loader[1] %s",
                    klass->external_name(), name->as_C_string(),
                    ClassLoaderData::class_loader_data(loader1())->loader_name_and_id(),
                    ClassLoaderData::class_loader_data(loader2())->loader_name_and_id());
        }
        if (!SystemDictionary::add_loader_constraint(name, klass, loader1, loader2, THREAD)) {
          // Loader constraint violation has been found. The caller
          // will re-layout the vtable/itables to produce the correct
          // exception.
          if (log.is_enabled()) {
            log.print(" failed]");
          }
          return false;
        }
        if (log.is_enabled()) {
            log.print(" succeeded]");
        }
      }
      return true; // for all recorded constraints added successully.
    }
  }
  if (log.is_enabled()) {
    ResourceMark rm(THREAD);
    log.print("[CDS has not recorded loader constraint for class %s]", klass->external_name());
  }
  return false;
}

class EstimateSizeForArchive : StackObj {
  size_t _shared_class_info_size;
  int _num_builtin_klasses;
  int _num_unregistered_klasses;

public:
  EstimateSizeForArchive() {
    _shared_class_info_size = 0;
    _num_builtin_klasses = 0;
    _num_unregistered_klasses = 0;
  }

  bool do_entry(InstanceKlass* k, DumpTimeSharedClassInfo& info) {
    if (!info.is_excluded()) {
      size_t byte_size = RunTimeSharedClassInfo::byte_size(info._klass, info.num_verifier_constraints(), info.num_loader_constraints());
      _shared_class_info_size += align_up(byte_size, BytesPerWord);
    }
    return true; // keep on iterating
  }

  size_t total() {
    return _shared_class_info_size;
  }
};

size_t SystemDictionaryShared::estimate_size_for_archive() {
  EstimateSizeForArchive est;
  _dumptime_table->iterate(&est);
  return est.total() +
    CompactHashtableWriter::estimate_size(_dumptime_table->count_of(true)) +
    CompactHashtableWriter::estimate_size(_dumptime_table->count_of(false));
}

class CopySharedClassInfoToArchive : StackObj {
  CompactHashtableWriter* _writer;
  bool _is_builtin;
public:
  CopySharedClassInfoToArchive(CompactHashtableWriter* writer,
                               bool is_builtin,
                               bool is_static_archive)
    : _writer(writer), _is_builtin(is_builtin) {}

  bool do_entry(InstanceKlass* k, DumpTimeSharedClassInfo& info) {
    if (!info.is_excluded() && info.is_builtin() == _is_builtin) {
      size_t byte_size = RunTimeSharedClassInfo::byte_size(info._klass, info.num_verifier_constraints(), info.num_loader_constraints());
      RunTimeSharedClassInfo* record;
      record = (RunTimeSharedClassInfo*)MetaspaceShared::read_only_space_alloc(byte_size);
      record->init(info);

      unsigned int hash;
      Symbol* name = info._klass->name();
      if (DynamicDumpSharedSpaces) {
        name = DynamicArchive::original_to_target(name);
      }
      hash = SystemDictionaryShared::hash_for_shared_dictionary(name);
      u4 delta;
      if (DynamicDumpSharedSpaces) {
        delta = MetaspaceShared::object_delta_u4(DynamicArchive::buffer_to_target(record));
      } else {
        delta = MetaspaceShared::object_delta_u4(record);
      }
      _writer->add(hash, delta);
      if (log_is_enabled(Trace, cds, hashtables)) {
        ResourceMark rm;
        log_trace(cds,hashtables)("%s dictionary: %s", (_is_builtin ? "builtin" : "unregistered"), info._klass->external_name());
      }

      // Save this for quick runtime lookup of InstanceKlass* -> RunTimeSharedClassInfo*
      RunTimeSharedClassInfo::set_for(info._klass, record);
    }
    return true; // keep on iterating
  }
};

void SystemDictionaryShared::write_dictionary(RunTimeSharedDictionary* dictionary,
                                              bool is_builtin,
                                              bool is_static_archive) {
  CompactHashtableStats stats;
  dictionary->reset();
  CompactHashtableWriter writer(_dumptime_table->count_of(is_builtin), &stats);
  CopySharedClassInfoToArchive copy(&writer, is_builtin, is_static_archive);
  _dumptime_table->iterate(&copy);
  writer.dump(dictionary, is_builtin ? "builtin dictionary" : "unregistered dictionary");
}

void SystemDictionaryShared::write_to_archive(bool is_static_archive) {
  if (is_static_archive) {
    write_dictionary(&_builtin_dictionary, true);
    write_dictionary(&_unregistered_dictionary, false);
  } else {
    write_dictionary(&_dynamic_builtin_dictionary, true);
    write_dictionary(&_dynamic_unregistered_dictionary, false);
  }
}

void SystemDictionaryShared::serialize_dictionary_headers(SerializeClosure* soc,
                                                          bool is_static_archive) {
  if (is_static_archive) {
    _builtin_dictionary.serialize_header(soc);
    _unregistered_dictionary.serialize_header(soc);
  } else {
    _dynamic_builtin_dictionary.serialize_header(soc);
    _dynamic_unregistered_dictionary.serialize_header(soc);
  }
}

void SystemDictionaryShared::serialize_well_known_klasses(SerializeClosure* soc) {
  for (int i = FIRST_WKID; i < WKID_LIMIT; i++) {
    soc->do_ptr((void**)&_well_known_klasses[i]);
  }
}

const RunTimeSharedClassInfo*
SystemDictionaryShared::find_record(RunTimeSharedDictionary* static_dict, RunTimeSharedDictionary* dynamic_dict, Symbol* name) {
  if (!UseSharedSpaces || !name->is_shared()) {
    // The names of all shared classes must also be a shared Symbol.
    return NULL;
  }

  unsigned int hash = SystemDictionaryShared::hash_for_shared_dictionary(name);
  const RunTimeSharedClassInfo* record = NULL;
  if (!MetaspaceShared::is_shared_dynamic(name)) {
    // The names of all shared classes in the static dict must also be in the
    // static archive
    record = static_dict->lookup(name, hash, 0);
  }

  if (record == NULL && DynamicArchive::is_mapped()) {
    record = dynamic_dict->lookup(name, hash, 0);
  }

  return record;
}

InstanceKlass* SystemDictionaryShared::find_builtin_class(Symbol* name) {
  const RunTimeSharedClassInfo* record = find_record(&_builtin_dictionary, &_dynamic_builtin_dictionary, name);
  if (record != NULL) {
    return record->_klass;
  } else {
    return NULL;
  }
}

void SystemDictionaryShared::update_shared_entry(InstanceKlass* k, int id) {
  assert(DumpSharedSpaces, "supported only when dumping");
  DumpTimeSharedClassInfo* info = find_or_allocate_info_for(k);
  info->_id = id;
}

class SharedDictionaryPrinter : StackObj {
  outputStream* _st;
  int _index;
public:
  SharedDictionaryPrinter(outputStream* st) : _st(st), _index(0) {}

  void do_value(const RunTimeSharedClassInfo* record) {
    ResourceMark rm;
    _st->print_cr("%4d:  %s", (_index++), record->_klass->external_name());
  }
};

void SystemDictionaryShared::print_on(outputStream* st) {
  if (UseSharedSpaces) {
    st->print_cr("Shared Dictionary");
    SharedDictionaryPrinter p(st);
    _builtin_dictionary.iterate(&p);
    _unregistered_dictionary.iterate(&p);
    if (DynamicArchive::is_mapped()) {
      _dynamic_builtin_dictionary.iterate(&p);
      _unregistered_dictionary.iterate(&p);
    }
  }
}

void SystemDictionaryShared::print_table_statistics(outputStream* st) {
  if (UseSharedSpaces) {
    _builtin_dictionary.print_table_statistics(st, "Builtin Shared Dictionary");
    _unregistered_dictionary.print_table_statistics(st, "Unregistered Shared Dictionary");
    if (DynamicArchive::is_mapped()) {
      _dynamic_builtin_dictionary.print_table_statistics(st, "Dynamic Builtin Shared Dictionary");
      _dynamic_unregistered_dictionary.print_table_statistics(st, "Unregistered Shared Dictionary");
    }
  }
}

bool SystemDictionaryShared::empty_dumptime_table() {
  if (_dumptime_table == NULL) {
    return true;
  }
  _dumptime_table->update_counts();
  if (_dumptime_table->count_of(true) == 0 && _dumptime_table->count_of(false) == 0){
    return true;
  }
  return false;
}

#if INCLUDE_CDS_JAVA_HEAP

class ArchivedMirrorPatcher {
  static void update(Klass* k) {
    if (k->has_raw_archived_mirror()) {
      oop m = HeapShared::materialize_archived_object(k->archived_java_mirror_raw_narrow());
      if (m != NULL) {
        java_lang_Class::update_archived_mirror_native_pointers(m);
      }
    }
  }

public:
  static void update_array_klasses(Klass* ak) {
    while (ak != NULL) {
      update(ak);
      ak = ArrayKlass::cast(ak)->higher_dimension();
    }
  }

  void do_value(const RunTimeSharedClassInfo* info) {
    InstanceKlass* ik = info->_klass;
    update(ik);
    update_array_klasses(ik->array_klasses());
  }
};

void SystemDictionaryShared::update_archived_mirror_native_pointers_for(RunTimeSharedDictionary* dict) {
  ArchivedMirrorPatcher patcher;
  dict->iterate(&patcher);
}

void SystemDictionaryShared::update_archived_mirror_native_pointers() {
  if (!HeapShared::open_archive_heap_region_mapped()) {
    return;
  }
  if (MetaspaceShared::relocation_delta() == 0) {
    return;
  }
  update_archived_mirror_native_pointers_for(&_builtin_dictionary);
  update_archived_mirror_native_pointers_for(&_unregistered_dictionary);

  for (int t = T_BOOLEAN; t <= T_LONG; t++) {
    Klass* k = Universe::typeArrayKlassObj((BasicType)t);
    ArchivedMirrorPatcher::update_array_klasses(k);
  }
}
#endif
