/*
 * Copyright (c) 2003, 2018, Oracle and/or its affiliates. All rights reserved.
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
 */

#include <string.h>
#include "jvmti.h"
#include "agent_common.h"
#include "jni_tools.h"
#include "jvmti_tools.h"

extern "C" {

/* ============================================================================= */

static jlong timeout = 0;

#define INFO_NONE       0x00
#define INFO_ALL        0xFF
#define INFO_OBJREF     0x01
#define INFO_STACKREF   0x02
#define INFO_HEAPROOT   0x04
#define INFO_HEAPOBJ    0x08

static unsigned int info = INFO_NONE;

#define DEBUGEE_CLASS_NAME      "nsk/jvmti/IterateOverReachableObjects/iterreachobj001"
#define ROOT_OBJECT_CLASS_NAME  "nsk/jvmti/IterateOverReachableObjects/iterreachobj001RootTestedClass"
#define ROOT_OBJECT_CLASS_SIG   "L" ROOT_OBJECT_CLASS_NAME ";"
#define CHAIN_OBJECT_CLASS_NAME "nsk/jvmti/IterateOverReachableObjects/iterreachobj001TestedClass"
#define CHAIN_OBJECT_CLASS_SIG  "L" CHAIN_OBJECT_CLASS_NAME ";"

#define OBJECT_FIELD_NAME               "object"
#define REACHABLE_CHAIN_FIELD_NAME      "reachableChain"
#define UNREACHABLE_CHAIN_FIELD_NAME    "unreachableChain"
#define TAIL_FIELD_NAME                 "tail"

#define DEFAULT_CHAIN_LENGTH 4

typedef struct ObjectDescStruct {
    jlong tag;
    jint found;
} ObjectDesc;

static int chainLength = 0;
static int objectsCount = 0;

static ObjectDesc* objectDescList = NULL;

static int fakeUserData = 0;
static int userDataError = 0;

/* ============================================================================= */

/** Obtain chain of tested objects and tag them recursively. */
static int getChainObjects(jvmtiEnv* jvmti, JNIEnv* jni, jobject firstObject,
                                    jfieldID firstField, const char firstFieldName[],
                                    jfieldID nextField, const char nextFieldName[],
                                    int count, ObjectDesc objectDescList[],
                                    jlong tag, int reachable) {

    int success = NSK_TRUE;
    jobject obj = NULL;
    jlong objTag = (reachable ? tag : -tag);

    if (count <= 0)
        return NSK_TRUE;

    count--;
    tag++;

    if (!NSK_JNI_VERIFY(jni, (obj =
            NSK_CPP_STUB3(GetObjectField, jni, firstObject, firstField)) != NULL)) {
        nsk_jvmti_setFailStatus();
        return NSK_FALSE;
    }

    objectDescList[count].tag = objTag;
    if (!NSK_JVMTI_VERIFY(
            NSK_CPP_STUB3(SetTag, jvmti, obj, objTag))) {
        nsk_jvmti_setFailStatus();
        return NSK_FALSE;
    }
    NSK_DISPLAY2("        tag=%-5ld object=0x%p\n", (long)objTag, (void*)obj);

    if (!getChainObjects(jvmti, jni, obj, nextField, nextFieldName,
                                nextField, nextFieldName,
                                count, objectDescList, tag, reachable)) {
        return NSK_FALSE;
    }

    NSK_TRACE(NSK_CPP_STUB2(DeleteLocalRef, jni, obj));
    return success;
}

/** Obtain all tested objects from debugee class and tag them recursively. */
static int getTestedObjects(jvmtiEnv* jvmti, JNIEnv* jni, int chainLength,
                                    int *objectsCount, ObjectDesc* *objectDescList) {
    jclass debugeeClass = NULL;
    jclass rootObjectClass = NULL;
    jclass chainObjectClass = NULL;

    jfieldID objectField = NULL;
    jfieldID reachableChainField = NULL;
    jfieldID unreachableChainField = NULL;
    jfieldID tailField = NULL;

    jobject rootObject = NULL;

    jlong rootObjectTag = 1;
    jlong chainObjectTag = 100;

    *objectsCount = 2 * chainLength;

    NSK_DISPLAY1("Allocate memory for objects list: %d objects\n", *objectsCount);
    if (!NSK_JVMTI_VERIFY(
            NSK_CPP_STUB3(Allocate, jvmti, (*objectsCount * sizeof(ObjectDesc)),
                                                    (unsigned char**)objectDescList))) {
        nsk_jvmti_setFailStatus();
        return NSK_FALSE;
    }
    NSK_DISPLAY1("  ... allocated array: 0x%p\n", (void*)objectDescList);

    {
        int k;
        for (k = 0; k < *objectsCount; k++) {
            (*objectDescList)[k].tag = 0;
            (*objectDescList)[k].found = 0;
        }
    }

    NSK_DISPLAY1("Find debugee class: %s\n", DEBUGEE_CLASS_NAME);
    if (!NSK_JNI_VERIFY(jni, (debugeeClass =
            NSK_CPP_STUB2(FindClass, jni, DEBUGEE_CLASS_NAME)) != NULL)) {
        nsk_jvmti_setFailStatus();
        return NSK_FALSE;
    }
    NSK_DISPLAY1("  ... found class: 0x%p\n", (void*)debugeeClass);

    NSK_DISPLAY1("Find root object class: %s\n", ROOT_OBJECT_CLASS_NAME);
    if (!NSK_JNI_VERIFY(jni, (rootObjectClass =
            NSK_CPP_STUB2(FindClass, jni, ROOT_OBJECT_CLASS_NAME)) != NULL)) {
        nsk_jvmti_setFailStatus();
        return NSK_FALSE;
    }
    NSK_DISPLAY1("  ... found class: 0x%p\n", (void*)rootObjectClass);

    NSK_DISPLAY1("Find chain object class: %s\n", CHAIN_OBJECT_CLASS_NAME);
    if (!NSK_JNI_VERIFY(jni, (chainObjectClass =
            NSK_CPP_STUB2(FindClass, jni, CHAIN_OBJECT_CLASS_NAME)) != NULL)) {
        nsk_jvmti_setFailStatus();
        return NSK_FALSE;
    }
    NSK_DISPLAY1("  ... found class: 0x%p\n", (void*)chainObjectClass);

    NSK_DISPLAY1("Find static field in debugee class: %s\n", OBJECT_FIELD_NAME);
    if (!NSK_JNI_VERIFY(jni, (objectField =
            NSK_CPP_STUB4(GetStaticFieldID, jni, debugeeClass,
                            OBJECT_FIELD_NAME, ROOT_OBJECT_CLASS_SIG)) != NULL)) {
        nsk_jvmti_setFailStatus();
        return NSK_FALSE;
    }
    NSK_DISPLAY1("  ... got fieldID: 0x%p\n", (void*)objectField);

    NSK_DISPLAY1("Find instance field in root object class: %s\n", REACHABLE_CHAIN_FIELD_NAME);
    if (!NSK_JNI_VERIFY(jni, (reachableChainField =
            NSK_CPP_STUB4(GetFieldID, jni, rootObjectClass,
                        REACHABLE_CHAIN_FIELD_NAME, CHAIN_OBJECT_CLASS_SIG)) != NULL)) {
        nsk_jvmti_setFailStatus();
        return NSK_FALSE;
    }
    NSK_DISPLAY1("  ... got fieldID: 0x%p\n", (void*)reachableChainField);

    NSK_DISPLAY1("Find instance field in root object class: %s\n", UNREACHABLE_CHAIN_FIELD_NAME);
    if (!NSK_JNI_VERIFY(jni, (unreachableChainField =
            NSK_CPP_STUB4(GetFieldID, jni, rootObjectClass,
                        UNREACHABLE_CHAIN_FIELD_NAME, CHAIN_OBJECT_CLASS_SIG)) != NULL)) {
        nsk_jvmti_setFailStatus();
        return NSK_FALSE;
    }
    NSK_DISPLAY1("  ... got fieldID: 0x%p\n", (void*)unreachableChainField);

    NSK_DISPLAY1("Find instance field in chain object class: %s\n", TAIL_FIELD_NAME);
    if (!NSK_JNI_VERIFY(jni, (tailField =
            NSK_CPP_STUB4(GetFieldID, jni, chainObjectClass,
                            TAIL_FIELD_NAME, CHAIN_OBJECT_CLASS_SIG)) != NULL)) {
        nsk_jvmti_setFailStatus();
        return NSK_FALSE;
    }
    NSK_DISPLAY1("  ... got fieldID: 0x%p\n", (void*)tailField);

    NSK_DISPLAY1("Get root object from static field: %s\n", OBJECT_FIELD_NAME);
    if (!NSK_JNI_VERIFY(jni, (rootObject =
            NSK_CPP_STUB3(GetStaticObjectField, jni, debugeeClass,
                                                    objectField)) != NULL)) {
        nsk_jvmti_setFailStatus();
        return NSK_FALSE;
    }
    NSK_DISPLAY1("  ... got object: 0x%p\n", (void*)rootObject);

    NSK_DISPLAY0("Obtain and tag chain objects:\n");

    NSK_DISPLAY1("    reachable objects chain: %d objects\n", chainLength);
    if (!getChainObjects(jvmti, jni, rootObject,
                                reachableChainField, REACHABLE_CHAIN_FIELD_NAME,
                                tailField, TAIL_FIELD_NAME,
                                chainLength, *objectDescList,
                                chainObjectTag, NSK_TRUE)) {
        nsk_jvmti_setFailStatus();
        return NSK_FALSE;
    }

    NSK_DISPLAY1("    unreachable objects chain: %d objects\n", chainLength);
    if (!getChainObjects(jvmti, jni, rootObject,
                                unreachableChainField, UNREACHABLE_CHAIN_FIELD_NAME,
                                tailField, TAIL_FIELD_NAME,
                                chainLength, *objectDescList + chainLength,
                                chainObjectTag, NSK_FALSE)) {
        nsk_jvmti_setFailStatus();
        return NSK_FALSE;
    }

    return NSK_TRUE;
}

/** Check if tagged objects were iterated. */
static int checkTestedObjects(jvmtiEnv* jvmti, JNIEnv* jni,
                                int chainLength, ObjectDesc objectDescList[]) {
    int success = NSK_TRUE;
    int i;

    NSK_DISPLAY0("Following tagged objects were iterated:\n");

    NSK_DISPLAY0("    reachable objects:\n");
    for (i = 0; i < chainLength; i++) {
        NSK_DISPLAY2("        tag=%-5ld found=%d times\n",
                        (long)objectDescList[i].tag, objectDescList[i].found);

        if (objectDescList[i].found <= 0) {
            NSK_COMPLAIN2("Reachable object was not iterated:\n"
                          "#   tag:      %ld\n"
                          "#   iterated: %d times\n",
                            (long)objectDescList[i].tag,
                            objectDescList[i].found);
            nsk_jvmti_setFailStatus();
        }
    }

    NSK_DISPLAY0("    unreachable objects:\n");
    for (i = 0; i < chainLength; i++) {
        NSK_DISPLAY2("        tag=%-5ld found=%d times\n",
                        (long)objectDescList[i + chainLength].tag,
                        objectDescList[i + chainLength].found);

        if (objectDescList[i + chainLength].found > 0) {
            NSK_COMPLAIN2("Unreachable object was iterated:\n"
                          "#   tag:      %ld\n"
                          "#   iterated: %d times\n",
                            (long)objectDescList[i + chainLength].tag,
                            objectDescList[i + chainLength].found);
            nsk_jvmti_setFailStatus();
        }
    }

    return NSK_TRUE;
}

/** Release references to the tested objects and free allocated memory. */
static int releaseTestedObjects(jvmtiEnv* jvmti, JNIEnv* jni, int chainLength,
                                                        ObjectDesc* objectDescList) {
    if (objectDescList != NULL) {
        NSK_DISPLAY1("Deallocate objects list: 0x%p\n", (void*)objectDescList);
        if (!NSK_JVMTI_VERIFY(
            NSK_CPP_STUB2(Deallocate, jvmti, (unsigned char*)objectDescList))) {
            nsk_jvmti_setFailStatus();
        }
    }

    return NSK_TRUE;
}

/* ============================================================================= */

/** heapRootCallback for heap iterator. */
jvmtiIterationControl JNICALL
heapRootCallback(jvmtiHeapRootKind root_kind,
                    jlong class_tag, jlong size, jlong* tag_ptr,
                    void* user_data) {

    if (info & INFO_HEAPROOT) {
        NSK_DISPLAY4("  heapRootCallback: root_kind=%2d class_tag=%-3ld size=%-3ld *tag_ptr=%-5ld\n",
                        (int)root_kind, (long)class_tag, (long)size,
                        (long)(tag_ptr == NULL ? (jlong)0 : *tag_ptr));
    }

    if (tag_ptr == NULL) {
        NSK_COMPLAIN4("NULL tag_ptr is passed to heapRootCallback:\n"
                      "#   tag_ptr:        0x%p\n"
                      "#   root_kind:      %d\n"
                      "#   class_tag:      %ld\n"
                      "#   size:           %ld\n",
                        (void*)tag_ptr,
                        (int)root_kind,
                        (long)class_tag,
                        (long)size);
        nsk_jvmti_setFailStatus();
    }

    if (tag_ptr != NULL && *tag_ptr != 0) {
        int found = 0;
        int i;

        for (i = 0; i < objectsCount; i++) {
            if (*tag_ptr == objectDescList[i].tag) {
                found++;
                objectDescList[i].found++;

                if (*tag_ptr < 0) {
                    NSK_COMPLAIN4("Unreachable tagged object passed to heapRootCallback:\n"
                                  "#   tag:            %ld\n"
                                  "#   root_kind:      %d\n"
                                  "#   class_tag:      %ld\n"
                                  "#   size:           %ld\n",
                                    (long)*tag_ptr,
                                    (int)root_kind,
                                    (long)class_tag,
                                    (long)size);
                    nsk_jvmti_setFailStatus();
                }

                if (*tag_ptr > 0) {
                    NSK_COMPLAIN4("Not root reachable tagged object passed to heapRootCallback:\n"
                                  "#   tag:            %ld\n"
                                  "#   root_kind:      %d\n"
                                  "#   class_tag:      %ld\n"
                                  "#   size:           %ld\n",
                                    (long)*tag_ptr,
                                    (int)root_kind,
                                    (long)class_tag,
                                    (long)size);
                    nsk_jvmti_setFailStatus();
                }
                break;
            }
        }

        if (found <= 0) {
            NSK_COMPLAIN4("Unknown tagged object passed to heapRootCallback:\n"
                          "#   tag:            %ld\n"
                          "#   root_kind:      %d\n"
                          "#   class_tag:      %ld\n"
                          "#   size:           %ld\n",
                            (long)*tag_ptr,
                            (int)root_kind,
                            (long)class_tag,
                            (long)size);
            nsk_jvmti_setFailStatus();
        }
    }

    if (user_data != &fakeUserData && !userDataError) {
       NSK_COMPLAIN2("Unexpected user_data is passed to heapRootCallback:\n"
                      "#   expected:       0x%p\n"
                      "#   actual:         0x%p\n",
                      user_data,
                      &fakeUserData);
        nsk_jvmti_setFailStatus();
        userDataError++;
    }

    return JVMTI_ITERATION_CONTINUE;
}

/** stackReferenceCallback for heap iterator. */
jvmtiIterationControl JNICALL
stackReferenceCallback(jvmtiHeapRootKind root_kind,
                            jlong class_tag, jlong size, jlong* tag_ptr,
                            jlong thread_tag, jint depth, jmethodID method,
                            jint slot, void* user_data) {

    if (info & INFO_STACKREF) {
        NSK_DISPLAY8("  stackReferenceCallback: root_kind=%1d class_tag=%-3ld size=%-3ld"
                        " *tag_ptr=%-5ld thread_tag=%-3ld depth=%3d method=0x%p slot=%d\n",
                        (int)root_kind, (long)class_tag, (long)size,
                        (long)(tag_ptr == NULL ? (jlong)0 : *tag_ptr),
                        (long)thread_tag, (int)depth, (void*)method, (int)slot);
    }

    if (tag_ptr == NULL) {
        NSK_COMPLAIN8("NULL tag_ptr is passed to stackReferenceCallback:\n"
                      "#   tag_ptr:        0x%p\n"
                      "#   root_kind:      %d\n"
                      "#   class_tag:      %ld\n"
                      "#   size:           %ld\n"
                      "#   thread_tag:     %ld\n"
                      "#   depth:          %d\n"
                      "#   method:         0x%p\n"
                      "#   slot:           %d\n",
                        (void*)tag_ptr,
                        (int)root_kind,
                        (long)class_tag,
                        (long)size,
                        (long)thread_tag,
                        (int)depth,
                        (void*)method,
                        (int)slot);
        nsk_jvmti_setFailStatus();
    }

    if (tag_ptr != NULL && *tag_ptr != 0) {
        int found = 0;
        int i;

        for (i = 0; i < objectsCount; i++) {
            if (*tag_ptr == objectDescList[i].tag) {
                found++;
                objectDescList[i].found++;

                if (*tag_ptr < 0) {
                    NSK_COMPLAIN8("Unreachable tagged object passed to stackReferenceCallback:\n"
                                  "#   tag:            %ld\n"
                                  "#   root_kind:      %d\n"
                                  "#   class_tag:      %ld\n"
                                  "#   size:           %ld\n"
                                  "#   thread_tag:     %ld\n"
                                  "#   depth:          %d\n"
                                  "#   method:         0x%p\n"
                                  "#   slot:           %d\n",
                                    (long)*tag_ptr,
                                    (int)root_kind,
                                    (long)class_tag,
                                    (long)size,
                                    (long)thread_tag,
                                    (int)depth,
                                    (void*)method,
                                    (int)slot);
                    nsk_jvmti_setFailStatus();
                }

                if (*tag_ptr > 0) {
                    NSK_COMPLAIN8("Not stack reachable tagged object passed to stackReferenceCallback:\n"
                                  "#   tag:            %ld\n"
                                  "#   root_kind:      %d\n"
                                  "#   class_tag:      %ld\n"
                                  "#   size:           %ld\n"
                                  "#   thread_tag:     %ld\n"
                                  "#   depth:          %d\n"
                                  "#   method:         0x%p\n"
                                  "#   slot:           %d\n",
                                    (long)*tag_ptr,
                                    (int)root_kind,
                                    (long)class_tag,
                                    (long)size,
                                    (long)thread_tag,
                                    (int)depth,
                                    (void*)method,
                                    (int)slot);
                    nsk_jvmti_setFailStatus();
                }
                break;
            }
        }

        if (found <= 0) {
            NSK_COMPLAIN8("Unknown tagged object passed to stackReferenceCallback:\n"
                          "#   tag:            %ld\n"
                          "#   root_kind:      %d\n"
                          "#   class_tag:      %ld\n"
                          "#   size:           %ld\n"
                          "#   thread_tag:     %ld\n"
                          "#   depth:          %d\n"
                          "#   method:         0x%p\n"
                          "#   slot:           %d\n",
                            (long)*tag_ptr,
                            (int)root_kind,
                            (long)class_tag,
                            (long)size,
                            (long)thread_tag,
                            (int)depth,
                            (void*)method,
                            (int)slot);
            nsk_jvmti_setFailStatus();
        }
    }

    if (user_data != &fakeUserData && !userDataError) {
       NSK_COMPLAIN2("Unexpected user_data is passed to stackReferenceCallback:\n"
                      "#   expected:       0x%p\n"
                      "#   actual:         0x%p\n",
                      user_data,
                      &fakeUserData);
        nsk_jvmti_setFailStatus();
        userDataError++;
    }

    return JVMTI_ITERATION_CONTINUE;
}

/** objectReferenceCallback for heap iterator. */
jvmtiIterationControl JNICALL
objectReferenceCallback(jvmtiObjectReferenceKind reference_kind,
                            jlong class_tag, jlong size, jlong* tag_ptr,
                            jlong refferrer_tag, jint refferrer_index,
                            void* user_data) {

    if (info & INFO_OBJREF) {
        NSK_DISPLAY6("  objectReferenceCallback: ref_kind=%1d class_tag=%-3ld size=%-3ld"
                        " *tag_ptr=%-5ld ref_tag=%-5ld ref_idx=%d\n",
                        (int)reference_kind, (long)class_tag, (long)size,
                        (long)(tag_ptr == NULL ? (jlong)0 : *tag_ptr),
                        (long)refferrer_tag, (int)refferrer_index);
    }

    if (tag_ptr == NULL) {
        NSK_COMPLAIN6("NULL tag_ptr is passed to objectReferenceCallback:\n"
                      "#   tag_ptr:        0x%p\n"
                      "#   reference_kind: %d\n"
                      "#   class_tag:      %ld\n"
                      "#   size:           %ld\n"
                      "#   refferrer_tag:  %ld\n"
                      "#   refferrer_idx:  %d\n",
                        (void*)tag_ptr,
                        (int)reference_kind,
                        (long)class_tag,
                        (long)size,
                        (long)refferrer_tag,
                        (int)refferrer_index);
        nsk_jvmti_setFailStatus();
    }

    if (tag_ptr != NULL && *tag_ptr != 0) {
        int found = 0;
        int i;

        for (i = 0; i < objectsCount; i++) {
            if (*tag_ptr == objectDescList[i].tag) {
                found++;
                objectDescList[i].found++;

                if (*tag_ptr < 0) {
                    NSK_COMPLAIN6("Unreachable tagged object is passed to objectReferenceCallback:\n"
                                  "#   tag:            %ld\n"
                                  "#   reference_kind: %d\n"
                                  "#   class_tag:      %ld\n"
                                  "#   size:           %ld\n"
                                  "#   refferrer_tag:  %ld\n"
                                  "#   refferrer_idx:  %d\n",
                                    (long)*tag_ptr,
                                    (int)reference_kind,
                                    (long)class_tag,
                                    (long)size,
                                    (long)refferrer_tag,
                                    (int)refferrer_index);
                    nsk_jvmti_setFailStatus();
                }
                break;
            }
        }

        if (found <= 0) {
            NSK_COMPLAIN6("Unknown tagged object is passed to ObjectReferenceCallback:\n"
                          "#   tag:            %ld\n"
                          "#   reference_kind: %d\n"
                          "#   class_tag:      %ld\n"
                          "#   size:           %ld\n"
                          "#   refferrer_tag:  %ld\n"
                          "#   refferrer_idx:  %d\n",
                            (long)*tag_ptr,
                            (int)reference_kind,
                            (long)class_tag,
                            (long)size,
                            (long)refferrer_tag,
                            (int)refferrer_index);
            nsk_jvmti_setFailStatus();
        }
    }

    if (user_data != &fakeUserData && !userDataError) {
       NSK_COMPLAIN2("Unexpected user_data is passed to ObjectReferenceCallback:\n"
                      "#   expected:       0x%p\n"
                      "#   actual:         0x%p\n",
                      user_data,
                      &fakeUserData);
        nsk_jvmti_setFailStatus();
        userDataError++;
    }

    if (reference_kind == JVMTI_REFERENCE_CLASS)
        return JVMTI_ITERATION_IGNORE;

    return JVMTI_ITERATION_CONTINUE;
}


/* ============================================================================= */

/** Agent algorithm. */
static void JNICALL
agentProc(jvmtiEnv* jvmti, JNIEnv* jni, void* arg) {
    NSK_DISPLAY0("Wait for tested objects created\n");
    if (!NSK_VERIFY(nsk_jvmti_waitForSync(timeout)))
        return;

    {
        NSK_DISPLAY0(">>> Obtain and tag tested objects from debugee class\n");
        {
            if (!NSK_VERIFY(getTestedObjects(jvmti, jni, chainLength, &objectsCount,
                                                                        &objectDescList)))
                return;
        }

        NSK_DISPLAY0(">>> Let debugee to clean links to unreachable objects\n");
        {
            if (!NSK_VERIFY(nsk_jvmti_resumeSync()))
                return;
            if (!NSK_VERIFY(nsk_jvmti_waitForSync(timeout)))
                return;
        }

        NSK_DISPLAY0(">>> Start iteration over reachable objects\n");
        {
            if (!NSK_JVMTI_VERIFY(
                    NSK_CPP_STUB5(IterateOverReachableObjects, jvmti,
                                  heapRootCallback, stackReferenceCallback,
                                  objectReferenceCallback, &fakeUserData))) {
                nsk_jvmti_setFailStatus();
                return;
            }
        }

        NSK_DISPLAY0(">>> Check if reachable objects were iterated:\n");
        {
            if (!checkTestedObjects(jvmti, jni, chainLength, objectDescList)) {
                nsk_jvmti_setFailStatus();
            }
        }

        NSK_DISPLAY0(">>> Clean used data\n");
        {
            if (!NSK_VERIFY(releaseTestedObjects(jvmti, jni, chainLength, objectDescList)))
                return;
        }
    }

    NSK_DISPLAY0("Let debugee to finish\n");
    if (!NSK_VERIFY(nsk_jvmti_resumeSync()))
        return;
}

/* ============================================================================= */

/** Agent library initialization. */
#ifdef STATIC_BUILD
JNIEXPORT jint JNICALL Agent_OnLoad_iterreachobj001(JavaVM *jvm, char *options, void *reserved) {
    return Agent_Initialize(jvm, options, reserved);
}
JNIEXPORT jint JNICALL Agent_OnAttach_iterreachobj001(JavaVM *jvm, char *options, void *reserved) {
    return Agent_Initialize(jvm, options, reserved);
}
JNIEXPORT jint JNI_OnLoad_iterreachobj001(JavaVM *jvm, char *options, void *reserved) {
    return JNI_VERSION_1_8;
}
#endif
jint Agent_Initialize(JavaVM *jvm, char *options, void *reserved) {
    jvmtiEnv* jvmti = NULL;

    if (!NSK_VERIFY(nsk_jvmti_parseOptions(options)))
        return JNI_ERR;

    timeout = nsk_jvmti_getWaitTime() * 60 * 1000;

    {
        const char* infoOpt = nsk_jvmti_findOptionValue("info");
        if (infoOpt != NULL) {
            if (strcmp(infoOpt, "none") == 0)
                info = INFO_NONE;
            else if (strcmp(infoOpt, "all") == 0)
                info = INFO_ALL;
            else if (strcmp(infoOpt, "objref") == 0)
                info = INFO_OBJREF;
            else if (strcmp(infoOpt, "stackref") == 0)
                info = INFO_STACKREF;
            else if (strcmp(infoOpt, "heaproot") == 0)
                info = INFO_HEAPROOT;
            else if (strcmp(infoOpt, "heapobj") == 0)
                info = INFO_HEAPOBJ;
            else {
                NSK_COMPLAIN1("Unknown option value: info=%s\n", infoOpt);
                return JNI_ERR;
            }
        }
    }

    chainLength = nsk_jvmti_findOptionIntValue("objects", DEFAULT_CHAIN_LENGTH);
    if (!NSK_VERIFY(chainLength > 0))
        return JNI_ERR;

    if (!NSK_VERIFY((jvmti =
            nsk_jvmti_createJVMTIEnv(jvm, reserved)) != NULL))
        return JNI_ERR;

    {
        jvmtiCapabilities caps;

        memset(&caps, 0, sizeof(caps));
        caps.can_tag_objects = 1;
        if (!NSK_JVMTI_VERIFY(
                NSK_CPP_STUB2(AddCapabilities, jvmti, &caps))) {
            return JNI_ERR;
        }
    }

    if (!NSK_VERIFY(nsk_jvmti_setAgentProc(agentProc, NULL)))
        return JNI_ERR;

    return JNI_OK;
}

/* ============================================================================= */

}
