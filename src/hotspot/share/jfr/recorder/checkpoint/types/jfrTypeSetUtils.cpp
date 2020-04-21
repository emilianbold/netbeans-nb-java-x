/*
 * Copyright (c) 2017, 2020, Oracle and/or its affiliates. All rights reserved.
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
#include "jfr/recorder/checkpoint/types/jfrTypeSetUtils.hpp"
#include "oops/instanceKlass.hpp"
#include "oops/oop.inline.hpp"
#include "oops/symbol.hpp"

static JfrSymbolId::CStringEntry* bootstrap = NULL;

JfrSymbolId::JfrSymbolId() :
  _sym_table(new SymbolTable(this)),
  _cstring_table(new CStringTable(this)),
  _sym_list(NULL),
  _cstring_list(NULL),
  _sym_query(NULL),
  _cstring_query(NULL),
  _symbol_id_counter(1),
  _class_unload(false) {
  assert(_sym_table != NULL, "invariant");
  assert(_cstring_table != NULL, "invariant");
  bootstrap = new CStringEntry(0, (const char*)&BOOTSTRAP_LOADER_NAME);
  assert(bootstrap != NULL, "invariant");
  bootstrap->set_id(1);
  _cstring_list = bootstrap;
}

JfrSymbolId::~JfrSymbolId() {
  clear();
  delete _sym_table;
  delete _cstring_table;
  delete bootstrap;
}

void JfrSymbolId::clear() {
  assert(_sym_table != NULL, "invariant");
  if (_sym_table->has_entries()) {
    _sym_table->clear_entries();
  }
  assert(!_sym_table->has_entries(), "invariant");

  assert(_cstring_table != NULL, "invariant");
  if (_cstring_table->has_entries()) {
    _cstring_table->clear_entries();
  }
  assert(!_cstring_table->has_entries(), "invariant");

  _sym_list = NULL;
  _symbol_id_counter = 1;

  _sym_query = NULL;
  _cstring_query = NULL;

  assert(bootstrap != NULL, "invariant");
  bootstrap->reset();
  _cstring_list = bootstrap;
}

void JfrSymbolId::set_class_unload(bool class_unload) {
  _class_unload = class_unload;
}

void JfrSymbolId::on_link(const SymbolEntry* entry) {
  assert(entry != NULL, "invariant");
  const_cast<Symbol*>(entry->literal())->increment_refcount();
  assert(entry->id() == 0, "invariant");
  entry->set_id(++_symbol_id_counter);
  entry->set_list_next(_sym_list);
  _sym_list = entry;
}

bool JfrSymbolId::on_equals(uintptr_t hash, const SymbolEntry* entry) {
  assert(entry != NULL, "invariant");
  assert(entry->hash() == hash, "invariant");
  assert(_sym_query != NULL, "invariant");
  return _sym_query == entry->literal();
}

void JfrSymbolId::on_unlink(const SymbolEntry* entry) {
  assert(entry != NULL, "invariant");
  const_cast<Symbol*>(entry->literal())->decrement_refcount();
}

static const char* resource_to_cstring(const char* resource_str) {
  assert(resource_str != NULL, "invariant");
  const size_t length = strlen(resource_str);
  char* const c_string = JfrCHeapObj::new_array<char>(length + 1);
  assert(c_string != NULL, "invariant");
  strncpy(c_string, resource_str, length + 1);
  return c_string;
}

void JfrSymbolId::on_link(const CStringEntry* entry) {
  assert(entry != NULL, "invariant");
  assert(entry->id() == 0, "invariant");
  entry->set_id(++_symbol_id_counter);
  const_cast<CStringEntry*>(entry)->set_literal(resource_to_cstring(entry->literal()));
  entry->set_list_next(_cstring_list);
  _cstring_list = entry;
}

static bool string_compare(const char* query, const char* candidate) {
  assert(query != NULL, "invariant");
  assert(candidate != NULL, "invariant");
  const size_t length = strlen(query);
  return strncmp(query, candidate, length) == 0;
}

bool JfrSymbolId::on_equals(uintptr_t hash, const CStringEntry* entry) {
  assert(entry != NULL, "invariant");
  assert(entry->hash() == hash, "invariant");
  assert(_cstring_query != NULL, "invariant");
  return string_compare(_cstring_query, entry->literal());
}

void JfrSymbolId::on_unlink(const CStringEntry* entry) {
  assert(entry != NULL, "invariant");
  JfrCHeapObj::free(const_cast<char*>(entry->literal()), strlen(entry->literal() + 1));
}

traceid JfrSymbolId::bootstrap_name(bool leakp) {
  assert(bootstrap != NULL, "invariant");
  if (leakp) {
    bootstrap->set_leakp();
  }
  return 1;
}

traceid JfrSymbolId::mark(const Symbol* symbol, bool leakp) {
  assert(symbol != NULL, "invariant");
  return mark((uintptr_t)symbol->identity_hash(), symbol, leakp);
}

traceid JfrSymbolId::mark(uintptr_t hash, const Symbol* data, bool leakp) {
  assert(data != NULL, "invariant");
  assert(_sym_table != NULL, "invariant");
  _sym_query = data;
  const SymbolEntry& entry = _sym_table->lookup_put(hash, data);
  if (_class_unload) {
    entry.set_unloading();
  }
  if (leakp) {
    entry.set_leakp();
  }
  return entry.id();
}

traceid JfrSymbolId::mark(uintptr_t hash, const char* str, bool leakp) {
  assert(str != NULL, "invariant");
  assert(_cstring_table != NULL, "invariant");
  _cstring_query = str;
  const CStringEntry& entry = _cstring_table->lookup_put(hash, str);
  if (_class_unload) {
    entry.set_unloading();
  }
  if (leakp) {
    entry.set_leakp();
  }
  return entry.id();
}

/*
* jsr292 anonymous classes symbol is the external name +
* the identity_hashcode slash appended:
*   java.lang.invoke.LambdaForm$BMH/22626602
*
* caller needs ResourceMark
*/

uintptr_t JfrSymbolId::hidden_or_anon_klass_name_hash(const InstanceKlass* ik) {
  assert(ik != NULL, "invariant");
  assert(ik->is_unsafe_anonymous() || ik->is_hidden(), "invariant");
  const oop mirror = ik->java_mirror_no_keepalive();
  assert(mirror != NULL, "invariant");
  return (uintptr_t)mirror->identity_hash();
}

static const char* create_hidden_or_anon_klass_symbol(const InstanceKlass* ik, uintptr_t hash) {
  assert(ik != NULL, "invariant");
  assert(ik->is_unsafe_anonymous() || ik->is_hidden(), "invariant");
  assert(hash != 0, "invariant");
  char* hidden_or_anon_symbol = NULL;
  const oop mirror = ik->java_mirror_no_keepalive();
  assert(mirror != NULL, "invariant");
  char hash_buf[40];
  sprintf(hash_buf, "/" UINTX_FORMAT, hash);
  const size_t hash_len = strlen(hash_buf);
  const size_t result_len = ik->name()->utf8_length();
  hidden_or_anon_symbol = NEW_RESOURCE_ARRAY(char, result_len + hash_len + 1);
  ik->name()->as_klass_external_name(hidden_or_anon_symbol, (int)result_len + 1);
  assert(strlen(hidden_or_anon_symbol) == result_len, "invariant");
  strcpy(hidden_or_anon_symbol + result_len, hash_buf);
  assert(strlen(hidden_or_anon_symbol) == result_len + hash_len, "invariant");
  return hidden_or_anon_symbol;
}

bool JfrSymbolId::is_hidden_or_anon_klass(const Klass* k) {
  assert(k != NULL, "invariant");
  return k->is_instance_klass() &&
    (((const InstanceKlass*)k)->is_unsafe_anonymous() ||
     ((const InstanceKlass*)k)->is_hidden());
}

traceid JfrSymbolId::mark_hidden_or_anon_klass_name(const InstanceKlass* ik, bool leakp) {
  assert(ik != NULL, "invariant");
  assert(ik->is_unsafe_anonymous() || ik->is_hidden(), "invariant");
  const uintptr_t hash = hidden_or_anon_klass_name_hash(ik);
  const char* const hidden_or_anon_symbol = create_hidden_or_anon_klass_symbol(ik, hash);
  return mark(hash, hidden_or_anon_symbol, leakp);
}

traceid JfrSymbolId::mark(const Klass* k, bool leakp) {
  assert(k != NULL, "invariant");
  traceid symbol_id = 0;
  if (is_hidden_or_anon_klass(k)) {
    assert(k->is_instance_klass(), "invariant");
    symbol_id = mark_hidden_or_anon_klass_name((const InstanceKlass*)k, leakp);
  }
  if (0 == symbol_id) {
    Symbol* const sym = k->name();
    if (sym != NULL) {
      symbol_id = mark(sym, leakp);
    }
  }
  assert(symbol_id > 0, "a symbol handler must mark the symbol for writing");
  return symbol_id;
}

JfrArtifactSet::JfrArtifactSet(bool class_unload) : _symbol_id(new JfrSymbolId()),
                                                    _klass_list(NULL),
                                                    _total_count(0) {
  initialize(class_unload);
  assert(_klass_list != NULL, "invariant");
}

static const size_t initial_class_list_size = 200;

void JfrArtifactSet::initialize(bool class_unload, bool clear /* false */) {
  assert(_symbol_id != NULL, "invariant");
  if (clear) {
    _symbol_id->clear();
  }
  _symbol_id->set_class_unload(class_unload);
  _total_count = 0;
  // resource allocation
  _klass_list = new GrowableArray<const Klass*>(initial_class_list_size, false, mtTracing);
}

JfrArtifactSet::~JfrArtifactSet() {
  _symbol_id->clear();
  delete _symbol_id;
  // _klass_list will be cleared by a ResourceMark
}

traceid JfrArtifactSet::bootstrap_name(bool leakp) {
  return _symbol_id->bootstrap_name(leakp);
}

traceid JfrArtifactSet::mark_hidden_or_anon_klass_name(const Klass* klass, bool leakp) {
  assert(klass->is_instance_klass(), "invariant");
  return _symbol_id->mark_hidden_or_anon_klass_name((const InstanceKlass*)klass, leakp);
}

traceid JfrArtifactSet::mark(uintptr_t hash, const Symbol* sym, bool leakp) {
  return _symbol_id->mark(hash, sym, leakp);
}

traceid JfrArtifactSet::mark(const Klass* klass, bool leakp) {
  return _symbol_id->mark(klass, leakp);
}

traceid JfrArtifactSet::mark(const Symbol* symbol, bool leakp) {
  return _symbol_id->mark(symbol, leakp);
}

traceid JfrArtifactSet::mark(uintptr_t hash, const char* const str, bool leakp) {
  return _symbol_id->mark(hash, str, leakp);
}

bool JfrArtifactSet::has_klass_entries() const {
  return _klass_list->is_nonempty();
}

int JfrArtifactSet::entries() const {
  return _klass_list->length();
}

void JfrArtifactSet::register_klass(const Klass* k) {
  assert(k != NULL, "invariant");
  assert(_klass_list != NULL, "invariant");
  assert(_klass_list->find(k) == -1, "invariant");
  _klass_list->append(k);
}

size_t JfrArtifactSet::total_count() const {
  return _total_count;
}
